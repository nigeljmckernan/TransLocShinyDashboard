# TransLoc OpenAPI Shiny App Dashboard

This is a Shiny App that heavily relies on the `httr`, `jsonlite`, and `tidyverse` collection of packages to pull real-time public transportation data from the [TransLoc OpenAPI](https://rapidapi.com/transloc/api/openapi-1-2/details).

There are 4 main tabs in this dashboard to interact with:

- Select Agencies
  - Select the Agencies that the rest of the tabs will pull data about
- Retrieve Data
  - The tab that actually conducts the `GET` requests from the TranLoc API. It pulls data from 5 different [endpoints](https://rapidapi.com/transloc/api/openapi-1-2/) (aside from Agencies):
    - Arrival Estimates
    - Segments
    - Vehicles
    - Stops
    - Routes
    
You can find the app [here](https://nigeljmckernan.shinyapps.io/TransportationDashboard/).
