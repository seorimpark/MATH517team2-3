#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(Hmisc)
library(zoo)
library(ggplot2)
library(hrbrthemes) 
library(corrplot)
library(png)
library(imager)

















# unique latitudes:

# latitudes<- 



# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
    
    # Application title
    titlePanel("Land Cover Analysis"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sliderInput(
            "lat", "Select the latitude",
            min=25.25,
            max=49.25,
            step = 0.5,
            value= 28.75,
            ticks = FALSE,
            animate = FALSE
        ),
        sliderInput(
            "lon", "Select the longitude",
            min=-124.75,
            max=-66.75,
            step = 0.5,
            value= -70.25,
            ticks = FALSE,
            animate = FALSE
        )),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("myPlot")
        )
    ))


# Define server logic required to draw a histogram

server <- function(input, output) {
    
            output$myPlot <- renderPlot({
            distLat <- input$lat
            distLon <- input$lon
            if(distLat != 25.25 &&distLat != 49.25) {distLat<-distLat-0.05}
            if(distLon != -124.25 &&distLat != -66.75) {distLon<-distLon-0.05}
            print(getwd())
            
            image <- load.image("/Users/Mac/Desktop/SCV/SCV_project/MATH517team2-2/Wildfires and the land cover analysis/interactiveapp/Rplot_trials.jpeg")
            plot(image,axes=FALSE, frame.plot=TRUE)
  
           
       

        }

        )
            
           
  
    
      }
    

# Run the application 
shinyApp(ui = ui, server = server)
