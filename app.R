#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shinydashboard)
library(tidyverse)
library(jsonlite)
library(httr)
library(leaflet)


Base_URL <- "https://transloc-api-1-2.p.rapidapi.com"

appendURL <- function(.endpoint) {
  modify_url(Base_URL, path = .endpoint)
}

URLs <- 
  
  list(
    Arrivals = "/arrival-estimates.json",
    Routes = "/routes.json",
    Segments = "/segments.json",
    Stops = "/stops.json",
    Vehicles = "/vehicles.json"
  )

URLs <- 
  
  URLs %>% map(appendURL)


openApiGETFunction <- function(URL, Params) {
  
  GET(
    URL,
    add_headers(
      "x-rapidapi-host" = 'transloc-api-1-2.p.rapidapi.com',
      "x-rapidapi-key" = 'REDACTED_Insert_Your_Own_Key_Here'
    ),
    query = Params
  )
  
}

getAgencyData <- function() { 
  
  openApiGETFunction(
    appendURL("/agencies.json"),
    list(
      "callback" = "call"
    )
  ) %>%
  
  content("text") %>%
  
  fromJSON() %>%
  
  pluck("data") %>%
  
  as_tibble() %>%
  
  unnest(position) %>%
  
  rename_with(str_to_title) %>%
  
  rename(
    Agency_LAT = Lat,
    Agency_LNG = Lng
  )
}

agencyBounds <- function(.agency_data) {
  
  .agency_data %>% 
  
  unnest(Bounding_box) %>%
  
  rename_with(str_to_upper, c("lat", "lng")) %>%
  
  arrange(
    Agency_id,
    LAT,
    LNG
  ) %>%
  
  group_by(Agency_id) %>%
  
  mutate(
    Bound = if_else(row_number() == 1, "Lower", "Upper")
  ) %>%
  
  ungroup() %>%
  
  select(Agency_id, Bound, LAT, LNG) %>%
  
  pivot_wider(
    names_from = Bound,
    values_from = c(LAT, LNG),
    names_glue = "{Bound}_{.value}"
  )
}

finalizeAgencies <- function(.agency_data, .bounds) {
  
  .agency_data %>%
  
  select(-Bounding_box) %>%
  
  inner_join(
    .bounds
  ) %>%
  
  relocate(Agency_id)
}

Agencies <-
  
  getAgencyData()

Bounds <- 
  
  Agencies %>%
  
  agencyBounds()

Agencies <- 
  
  Agencies %>%
  
  finalizeAgencies(Bounds)
  
  

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(
    title = "Transporation Dashbaord",
    titleWidth = 270
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "intro", icon = icon("compass")),
      menuItem("Select Agencies", tabName = "get_agencies", icon = icon("building")),
      menuItem("Retrieve Data", tabName = "get_data", icon = icon("database")),
      menuItem("Agency Geometry", tabName = "agency_geo", icon = icon("map")),
      menuItem("Vehicle Locations", tabName = "vehicle_loc", icon = icon("car"))
    )
  ),
  dashboardBody(
    tabItems(
      
      tabItem(
        tabName = "intro",
        box(
          title = "Welcome to my Transportation Dashboard Project!",
          h5(
            "This dashboard heavily relies on the httr package to make GET http requests from the TransLoc OpenAPI."
          ),
          h5(
            "This API has real-time data on vehicles, their status, routes, stops, transporation agencies, and many other data points."
          ),
          h5(
            "My initial goal with this API was to pull the data, fit machine learning models for forecasting of arrival times, and potentially conduct some clustering on vehicle or driver types."
          ),
          h5(
            "After some intial exploratory data analysis (EDA), I realized the amount of missing data is quite large across many of the tables available, with little to no imformation to help impute these missing values."
          ),
          h5(
            "As well, there are not as many columns as I would have hoped for the purpose of feature engineering, or just to further help with the initial EDA."
          ),
          h5(
            "Therefore I have just left it as a dashboard to pull real-time data on a subset of agencies of your choice from the 'Select Agencies' tab, and to see the data yourself, or to see the transporatation agency locations and their jurisdiction (the 'Agency Geometry' tab) or vehicles on a Leaflet map (the 'Vehicle Locaions' tab)."
          )
        )
      ),
      
      # Agency Selection Tab
      tabItem(
        tabName = "get_agencies",
        column(4,
          selectInput(
            "agency_ids",
            label = "Select Agency ID's To Retrieve Data For",
            choices = sort(unique(str_to_title(paste0(Agencies$Agency_id, " - ", Agencies$Short_name)))),
            multiple = T
          ),
          actionButton("get_agencies_button", "Retrieve Agency ID's")
        ),
        box(
          dataTableOutput(
            "agencies_filtered_table"
          )
        )
      ),
      
      # Retrieve Data From API Tab
      tabItem(
        tabName = "get_data",
        box(
          title = "Retrieve Transportation Data from OpenAPI from Agencies Selected",
          actionButton(
            "get_data_button",
            "Retrieve Data From API"
          )
        ),
        tabBox(
          title = "Pick Table For Viewing",
          side = "right",
          tabPanel(
            "Arrivals",
            dataTableOutput(
              "arrivals"
            )
          ),
          tabPanel(
            "Vehicles",
            dataTableOutput(
              "vehicles"
            )
          ),
          tabPanel(
            "Stops",
            dataTableOutput(
              "stops"
            )
          ),
          tabPanel(
            "Routes",
            dataTableOutput(
              "routes"
            )
          )
        )
      ),
      
      # Plot Agency Locations and Geometry
      tabItem(
        tabName = "agency_geo",
        box(
          title = "Agency Locations and Their Corresponding Jurisdictions",
          leafletOutput(
            "agencies_leaflet"
          )
        )
      ),
      
      # Plot Vehicles Locations
      tabItem(
        tabName = "vehicle_loc",
        box(
          title = "Vehicle Locations and Their Speed",
          leafletOutput(
            "vehicles_leaflet"
          )
        )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  selected_agencies <- eventReactive(input$get_agencies_button, {
    
    input$agency_ids %>%
    
    str_extract_all("[0-9]") %>%
    
    map_chr(str_flatten)
  })
  
  Filtered_Agencies <- reactive({
    Agencies %>% filter(Agency_id %in% selected_agencies())
  })
  
  Flattened_Agencies <- reactive({
    selected_agencies() %>% str_flatten(",")
  })
  
  Query_Params <- reactive({
    
    list(
      Arrivals = list(
        "callback" = "call",
        "agencies" = Flattened_Agencies()
      ),
      Routes = list(
        "callback" = "call",
        "agencies" = Flattened_Agencies()
      ),
      Segments = list(
        "callback" = "call",
        "agencies" = Flattened_Agencies()
      ),
      Stops = list(
        "callback" = "call",
        "agencies" = Flattened_Agencies()
      ),
      Vehicles = list(
        "callback" = "call",
        "agencies" = Flattened_Agencies()
      )
    )
  })
  
  
  API_Data <- eventReactive(input$get_data_button, {
    map2(URLs, Query_Params(), openApiGETFunction) %>%
      
      map(~ content(., "text") %>% fromJSON() %>% pluck("data"))
  })
  
  Routes_IDs <- reactive( names(API_Data() %>% pluck("Routes")) )
  
  Vehicles_IDs <- reactive( names(API_Data() %>% pluck("Vehicles")) )
  
  Routes <- reactive({
    map2_dfr(
      API_Data() %>% pluck("Routes"), 
      Routes_IDs(), 
      ~ bind_cols(.x, .y) %>% 
        rename(Agency_ID = ...14) %>% 
        select(-c(agency_id))
      )
  })
  
  Vehicles <- reactive({
    API_Data() %>% 
      pluck("Vehicles") %>%
      map2_dfr( 
      Vehicles_IDs(), 
      bind_cols
      ) %>%
      rename(
        Agency_ID = ...15,
        Route_ID = route_id
        )
  })
  
  API_Data_New <- reactive(
    list(
      Arrivals = API_Data() %>% pluck("Arrivals"),
      Routes = Routes(),
      Segments = API_Data() %>% pluck("Segments"),
      Stops = API_Data() %>% pluck("Stops"),
      Vehicles = Vehicles()
    )
  )
  
  Arrivals <- reactive({ 
    API_Data_New() %>% 
      pluck("Arrivals") %>% 
      unnest(arrivals) %>% 
      rename_with(str_to_title) %>% 
      mutate(Arrival_at =  lubridate::ymd_hms(Arrival_at)) %>%
      setNames(
        c(
          "Rout_ID",
          "Vehicle_ID",
          "Estimated_Arrival",
          "Type",
          "Agency_ID",
          "Stop_ID"
        )
      )
  })

  Routes_Inter <- reactive({
    API_Data_New() %>%
      pluck("Routes") %>%
      pluck("segments") %>%
      map_chr(typeof) %>%
      bind_cols(Routes()) %>%
      rename(Type = ...1) %>%
      filter(Type != "list") %>%
      select(-Type) %>%
      unnest(segments)
  })
  
  Routes_Final <- reactive({
    Routes_Inter() %>%
      pluck("segments") %>%
      as_tibble() %>%
      bind_cols(Routes_Inter()) %>%
      select(-segments) %>%
      setNames(
        c(
          "Description",
          "Short_Name",
          "Route_ID",
          "Colour",
          "Segment_ID",
          "Segment_Dir",
          "Active",
          "Text_Colour",
          "Long_Name",
          "URL",
          "Hidden",
          "Type",
          "Stop_ID",
          "Agency_ID"
        )
      ) %>%
      relocate(contains("Segments"), .before = Active) %>%
      unnest(Stop_ID)
  })
  
  Segments <- reactive({ 
    API_Data_New() %>%
    pluck("Segments") %>%
    unlist() %>%
    tibble(
      Segment_ID = names(.),
      Segment = .
      )
  })
  
  Stops <- reactive({
    API_Data_New() %>%
    pluck("Stops") %>%
    unnest(agency_ids) %>%
    unnest(location) %>%
    unnest(routes) %>%
      setNames(
        c(
          "Code",
          "Description",
          "URL",
          "Station_ID",
          "Agency_ID",
          "Station_ID",
          "Location_Type",
          "Lat",
          "Lng",
          "Stop_ID",
          "Route_ID",
          "Name"
        )
      )
  })
  
  
  Vehicles_Inter <- reactive({
    Vehicles() %>%
      pluck("arrival_estimates") %>%
      map_chr(class) %>%
      bind_cols(Vehicles()) %>%
      rename(Type = ...1) %>%
      filter(Type != "list") %>%
      select(-Type)
  })
  
  Vehicles_Final <- reactive({
    Vehicles_Inter() %>%
      unnest(arrival_estimates) %>%
      unnest(location) %>%
      mutate(across(c(last_updated_on, arrival_at), lubridate::ymd_hms)) %>%
      select(-route_id) %>%
      rename_with(str_to_title) %>%
      setNames(
        c(
          "Standing_Capacity",
          "Description",
          "Seating_Capacity",
          "Last_Updated",
          "Call_Name",
          "Speed",
          "Vehicle_ID",
          "Segment_ID",
          "Occupancy_Level",
          "Route_ID",
          "Arrival_At",
          "Stop_ID",
          "Status",
          "Lat",
          "Lng",
          "Heading",
          "Agency_ID"
        )
      )
  })
  
  
  output$agencies_filtered_table <-
    
    renderDataTable(
      if(!input$get_agencies_button) {
        NULL
      }
    else {
      Filtered_Agencies()
      }
    )
  
  output$arrivals <- renderDataTable(
    Arrivals(),
    options = list(pageLength = 10)
  )
  
  output$vehicles <- renderDataTable(
    Vehicles_Final(),
    options = list(pageLength = 10)
  )
  
  output$stops <- renderDataTable(
    Stops(),
    options = list(pageLength = 10)
  )
  
  output$routes <- renderDataTable(
    Routes_Final(),
    options = list(pageLength = 10)
  )
  
  output$agencies_leaflet <- renderLeaflet(
    if(!input$get_agencies_button) {
      NULL
    }
    else {
    Filtered_Agencies() %>%
      
      mutate(
        Label = paste(Agency_id, Long_name, sep = " - "),
        Alt_Label = paste(
          sep = "<br/>",
          paste0("<b><a href='", Url, "'>", Long_name, "</a></b>"),
          paste0("Agency ID: ", Agency_id),
          paste0("Language: ", Language),
          paste0("Phone: ", Phone)
        ) %>%
          str_replace_all("='NA'>", "=''>")
      ) %>%
      
      leaflet() %>%
      
      addTiles() %>%
      
      addRectangles(~Upper_LNG, ~Upper_LAT, ~Lower_LNG, ~Lower_LAT, label = ~Label) %>%
      
      addMarkers(~Agency_LNG, ~Agency_LAT, popup = ~Alt_Label)
    }
  )
  
  output$vehicles_leaflet <- renderLeaflet(
    if(!input$get_data_button) {
      NULL
    }
    else {
    
    Vehicles_Final() %>%
      
      distinct(
        Vehicle_ID,
        Lat,
        Lng,
        Speed,
        Agency_ID
      ) %>%
      
      leaflet() %>%
      
      addTiles() %>%
      
      addMarkers(~Lng, ~Lat, popup = ~paste(sep = "<br/>", paste0("Vehicle ID: ", Vehicle_ID), paste0("Speed (MPH): ", Speed)))
    }
  )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
