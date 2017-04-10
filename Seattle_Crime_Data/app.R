
library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)

## Loading in data
spd <- read.csv("data/Seattle_Police_Department_Police_Report_Incident (3).csv")
spd_subset_keep <- c("Summarized.Offense.Description", "Date.Reported", 
                     "Longitude", "Latitude")
spd_subset <- spd[spd_subset_keep]
name <- sort(unique(spd_subset$Summarized.Offense.Description)) 

## Main UI
ui <- dashboardPage(
  dashboardHeader(
    title = "Seattle Police Incidents - 2017", titleWidth = 400 
  ),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(
        box(leafletOutput("map1"), width = 8),
        box(selectInput("name", "Select type of crime",
                        choices = spd_subset$Summarized.Offense.Description, 
                        selected=name[1]), width = 4)
        ),
    fluidRow(
      valueBoxOutput("totalCrimeBox")
    )
  )
)

## Server
server <- function(input, output, session){
  
  ## Outputs
  
  ### Maps
  output$map1 <- renderLeaflet({
    currentCrime <- subset(spd_subset, spd_subset$Summarized.Offense.Description == input$Controls)
    leaflet(spd_subset) %>% 
      addTiles() %>%
      fitBounds(~min(Longitude), ~min(Latitude), 
                ~max(Longitude), ~max(Latitude)) %>%
      addCircleMarkers(
        radius = 6,
        color = "red",
        stroke = FALSE,
        fillOpacity = 0.8,
        lng = spd_subset$Longitude,
        lat = spd_subset$Latitude,
        label = spd_subset$Summarized.Offense.Description)
        })
  
  
  ### Controls
 # output$Controls <- renderUI({
  #  selectInput("name", "Select type of crime",
   #                  name, 
    #                 selected=name[1]) 
  #  })
    
  ### Boxes
  output$totalCrimeBox <- renderValueBox(
    {valueBox(
      paste0(nrow(spd_subset)),"Total number of incidents", icon = icon("thumbs-down", lib = "glyphicon"), color = "red"
    )}
  )  
  }

# Run the application 
shinyApp(ui, server)

