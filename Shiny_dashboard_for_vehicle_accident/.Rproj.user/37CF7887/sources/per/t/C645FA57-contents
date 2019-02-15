#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#

library(shiny)
library(shinydashboard)
library(leaflet)
library(rgdal)
library(dplyr)
library(DT)
library(lubridate)
library(tidyr)

AccidentData <- read.csv(file = "Vehicle_Accident_Data.csv")
AccidentData <- tidyr::separate(data=AccidentData,
                      col=Location,
                      into=c("Latitude", "Longitude"),
                      sep=",",
                      remove=FALSE)
AccidentData$Latitude <- stringr::str_replace_all(AccidentData$Latitude, "[(]", "")
AccidentData$Longitude <- stringr::str_replace_all(AccidentData$Longitude, "[)]", "")

AccidentData$Latitude <- as.numeric(AccidentData$Latitude)
AccidentData$Longitude <- as.numeric(AccidentData$Longitude)

AccidentData <-  AccidentData[AccidentData$Latitude != 0 ,]

# converting date, time to date only to apply filter correctly
AccidentData$Crash.Date.Time <- mdy(sub(" .*","",AccidentData$Crash.Date.Time))

# merging all location fields to give one address
AccidentData <- unite(AccidentData,"Accident_location",c("Street.Prefix","Street.Name","Street.Suffix","Intersecting.Block.Number","Intersecting.Street.Prefix","Intersecting.Street.Name","Intersecting.Street.Suffix","Street.Description","Intersecting.Street.Description"))
AccidentData$Accident_location = sub("_", " ", AccidentData$Accident_location)

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Accidents Dashboard "),
  dashboardSidebar(
    dateRangeInput(inputId = "Date_Range",label = "Date Range",
                   start  = min(AccidentData$Crash.Date.Time),
                   end    = max(AccidentData$Crash.Date.Time)),
    radioButtons(inputId = "HIT_RUN",label = " Hit & Run ?",
                    choices = c(TRUE,FALSE),selected = FALSE),
    radioButtons(inputId = "Fatality",label = "Fatality ?",
                 choices = c(TRUE,FALSE),selected = FALSE)
  ),
  dashboardBody(
    fluidRow(box(width = 12, leafletOutput(outputId = "mymap"))),
    fluidRow(box(width = 12, dataTableOutput(outputId = "summary_table")))
  )
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   data_input <-  reactive({
     AccidentData %>%
     filter(Crash.Date.Time >= input$Date_Range[1]) %>%
     filter(Crash.Date.Time <= input$Date_Range[2]) %>%
       filter(Hit.And.Run == input$HIT_RUN) %>%
       filter(Fatality == input$Fatality)
       
     
   }) 
    
     output$mymap <- renderLeaflet({
       df <- data_input()
       
       m <- leaflet(data = df) %>%
         addTiles() %>%
         addMarkers(lng = ~Longitude,
                    lat = ~Latitude,
                    popup = paste("Date", df$Crash.Date.Time, "<br>",
                                  "Fatality:", df$Fatality, "<br>",
                                  "Address:",df$Accident_location))
       m
     })
     
     output$summary_table <- renderDataTable(data_input())
   
      }

# Run the application 
shinyApp(ui = ui, server = server)

