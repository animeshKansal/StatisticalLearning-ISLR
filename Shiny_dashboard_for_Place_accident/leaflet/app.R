#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#


#Specify the list of packages required for your project
list.of.packages <-
  c("shiny","shinydashboard","leaflet","rgdal","dplyr","DT","lubridate","tidyr","sqldf")

#Check for the packages that are NOT installed in the system
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

#Install the packages NOT installed in the system
if(length(new.packages)) install.packages(new.packages)


# Libraries
library(shiny)
library(shinydashboard)
library(leaflet)
library(rgdal)
library(dplyr)
library(DT)
library(lubridate)
library(tidyr)
library(sqldf)
library(leaflet.minicharts)

# Data load
raw <- read.delim(file = "Plane_Accidents.csv",sep = ";")

AccidentData <-  raw

# Add factor level
addNoLocation <- function(x){
  if(is.factor(x)) return(factor(x, levels=c(levels(x), "No Location")))
  return(x)
}

AccidentData <- as.data.frame(lapply(AccidentData, addNoLocation))

# Replace empty value with "No Location"
AccidentData$Location[AccidentData$Location == ""] <- "No Location"

AccidentData$Location <- tolower(AccidentData$Location) 
  
# Replacing Missing Longitude and Latitude values with aggregated average values based on 
# Location field since every data point has a location field.
Longitude_Table <- AccidentData %>%
  group_by(Location) %>%
  dplyr::summarize(Mean_Longitude = mean(Longitude, na.rm=TRUE))

Longitude_Table <- na.omit(Longitude_Table)

Longitude_Table <- Longitude_Table[!(Longitude_Table$Location == "no location"),]

Latitude_Table <- AccidentData %>%
  group_by(Location) %>%
  dplyr::summarize(Mean_Latitude = mean(Latitude, na.rm=TRUE))

Latitude_Table <- na.omit(Latitude_Table)

Latitude_Table <- Latitude_Table[!(Latitude_Table$Location == "no location"),]

# Putting Latitude, Longitude from Latitude and longitude table 
AccidentData_2 <- sqldf("select * from AccidentData left join Latitude_Table on Latitude_Table.Location = AccidentData.Location ")
AccidentData_2 <- sqldf("select * from AccidentData_2 left join Longitude_Table on Longitude_Table.Location = AccidentData_2.Location ")

AccidentData_3 <- AccidentData_2 

AccidentData_3$Latitude[is.na(AccidentData_3$Latitude)] <- AccidentData_3$Mean_Latitude[is.na(AccidentData_3$Latitude)]
AccidentData_3$Longitude[is.na(AccidentData_3$Longitude)] <- AccidentData_3$Mean_Longitude[is.na(AccidentData_3$Longitude)]

# Drop Meaningless columns
drops <- c("Mean_Longitude","Mean_Latitude","Location..35","Location..33","geo_point","Location")
AccidentData_3 <- AccidentData_3[ , !(names(AccidentData_3) %in% drops)]

# Remove NA latitude and longitude
AccidentData_3 <- AccidentData_3[!is.na(AccidentData_3$Latitude),]
AccidentData_3 <- AccidentData_3[!is.na(AccidentData_3$Longitude),]

AccidentData_3$Injury.Severity = gsub("\\s*\\([^\\)]+\\)","",as.character(AccidentData_3$Injury.Severity))

# Remove rows with value as "unavailable" in injury severity field
AccidentData_3 <- AccidentData_3[AccidentData_3$Injury.Severity!="Unavailable",]

#as date conversion
AccidentData_3$Event.Date <- as.Date(AccidentData_3$Event.Date) 

#substitute blank with Others
AccidentData_3$Broad.Phase.of.Flight <- sub("^$", "OTHER", AccidentData_3$Broad.Phase.of.Flight) 

set.seed(40)
sample_data <- sample_n(AccidentData_3,0.01*(nrow(AccidentData_3)))



ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Plane Accidents "),
  dashboardSidebar(
    dateRangeInput(inputId = "Date_Range",label = "Date Range",
                   start  = min(sample_data$Event.Date),
                   end    = max(sample_data$Event.Date)),
    sliderInput(inputId = "Sample_Size",label="Sample Size % (More than 5% can make your Rstudio Crash)",value = 1 ,min = 0,max = 100),
    radioButtons(inputId = "Fatality",label = "Crash Type",
                    choices = c("Non-Fatal","Fatal","Overall"),selected = "Overall")

  ),
  dashboardBody(
    fluidRow(box(width = 12, verbatimTextOutput("text1"))),
    fluidRow(box(width = 12, plotOutput(outputId = "bar_3"))),
    fluidRow(box(width = 12, leafletOutput(outputId = "mymap"))),
    fluidRow(box(width = 12, verbatimTextOutput("text3"))),
    fluidRow(box(width = 12, dataTableOutput(outputId = "summary_table"))),  
    fluidRow(box(width = 12, plotOutput(outputId = "bar"))),
    fluidRow(box(width = 12, verbatimTextOutput("text2"))),
    fluidRow(box(width = 12, plotOutput(outputId = "bar_2")))
    
      )
  
)


server <- function(input, output) {
   
  data_input <-  reactive({
    
    set.seed(40)
    
    sample_data <-
      sample_n(AccidentData_3, input$Sample_Size * 0.001 * (nrow(AccidentData_3)))
    
    if (input$Fatality == "Fatal") {
      sample_data %>%
        select(
          Event.Date,
          Total.Uninjured,
          Country,
          Total.Fatal.Injuries,
          Injury.Severity,
          Latitude,
          Longitude
        ) %>%
        filter(Event.Date >= input$Date_Range[1]) %>%
        filter(Event.Date <= input$Date_Range[2]) %>%
        filter(Injury.Severity != "Non-Fatal")  %>%
        filter(Injury.Severity != "Incident")

    }
    else{
      if(input$Fatality == "Overall"){

      sample_data %>%
        select(
          Event.Date,
          Total.Uninjured,
          Country,
          Total.Fatal.Injuries,
          Injury.Severity,
          Latitude,
          Longitude
        ) %>%
        filter(Event.Date >= input$Date_Range[1]) %>%
        filter(Event.Date <= input$Date_Range[2]) 
        }
    else {
      sample_data %>%
        select(
          Event.Date,
          Total.Uninjured,
          Country,
          Total.Fatal.Injuries,
          Injury.Severity,
          Latitude,
          Longitude
        ) %>%
        filter(Event.Date >= input$Date_Range[1]) %>%
        filter(Event.Date <= input$Date_Range[2]) %>%
        filter(Injury.Severity == "Non-Fatal" | Injury.Severity == "incident"  )
      
    }
    }
    
    
    
  }) 
    
     output$mymap <- renderLeaflet({
       df <- data_input()
       
       m <- leaflet(data = df) %>%
         addTiles() %>%
         addMarkers(
                    lng = ~Longitude,
                    lat = ~Latitude,
                    popup = paste("Date", df$Event.Date, "<br>"
                                  ))
       m
     })
     
     output$text1 <- renderText({paste("Note: Please scroll down to the end.\nObservation: Most accidents are Non-Fatal, also plotted on MAP using Filter")})
     
     
     
     DF_2 <- data.frame(rev(sort(round(prop.table(table(AccidentData_3$Broad.Phase.of.Flight))*100,2))))
     colnames(DF_2) <- c("Boarding Phases during Accident", "Percetage")
     output$summary_table <- renderDataTable(DF_2)

     output$bar <- renderPlot({

       
       barplot(sort(round(prop.table(table(AccidentData_3$Broad.Phase.of.Flight))*100,2)),horiz = TRUE,las=2,
               main="Boarding Phases during Accident | Bar plot")
       })
     
     output$text2 <- renderText({paste("Clearly number of accidents are decreasing over time but showing % of Accidents over time would have made it more clearer to state Air travel has become safer.")})
     
     output$bar_2 <- renderPlot({

       hist(as.numeric(gsub("-","",format(as.Date(raw$Event.Date),"%Y"))),breaks = 100,
        main = "Number of Accidents over time",xlab = "Years",ylab = "Frequency",xlim = c(1980,2020) )
       
     })
     
     output$text3 <- renderText({paste("Clearly most accidents are occured in landing phase.")})
     
     output$bar_3 <- renderPlot({
       
       
       barplot(sort(round(prop.table(table(AccidentData_3$Injury.Severity))*100,2)),
               main="Accidents by Injury Severity")
     })
     
}



# Run the application 
shinyApp(ui = ui, server = server)


