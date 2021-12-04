
library(shiny)
library(shinyWidgets)
library(ggplot2)
library(maps)
library(dplyr)
library(zeallot)

# Read data
load("data_train_DF.RData")
data = data_train_DF

# Remove NA values from CNT and BA
data <- data[!is.na(data$CNT),]
data <- data[!is.na(data$BA),]

# Keep variables we need
data = data[,c("CNT", "BA", "lon", "lat", "area", "year", "month")]

data = data[data$CNT>0 & data$BA>0,]

spread <- function(sub) {
  n = nrow(sub)
  spread=0
  for (i in 1:n) {
    c(lon, lat, BA) %<-% sub[i, c("lon", "lat", "BA")]
    neighbours = sub[sub$lon>=lon-0.5 & sub$lon<=lon+0.5 & sub$lat>=lat-0.5 & sub$lat<=lat+0.5 & (sub$lon!=lon | sub$lat!=lat),]
    nspread = nrow(neighbours[neighbours$BA <= BA,])
    spread[i] = nspread
  }
  return(spread)
}

for (year in 2014:2015) {
  for (month in 3:9) {
    data[data$year==year & data$month==month, "spread"] = spread(data[data$year==year & data$month==month,])
  }
}


############# Application #############

ui <- fluidPage(
  
  # Application title
  titlePanel("Wildfires and meteorological factors"),
  
  sidebarLayout(
    sidebarPanel(
      helpText(style="text-align: justify;",
               "This app will help you find some meteorological factors that are often 
                     present during a wildfire. As this is an unusual event, we will first 
                     consider a subset of our data. We will only take the areas per month 
                     which have at least a certain number of wildfires and which have been 
                     burnt above a certain threshold. Please indicate these two values below 
                     using the sliders. Note that if both are too large, the subset will be
                     too small and problems will occur in the outputs.", 
               HTML("<br/>"),
               "As a result, a correlation heatmap (Pearson) will allow you to 
                     distinguish certain factors that could favor a wildfire. On the second 
                     tab, you will find a map of the US representing the areas considered 
                     in the subset, regardless of months and years."),
      sliderInput("year",
                  "Year:",
                  min = 1993,
                  max = 2015,
                  step = 1,
                  sep = "",
                  ticks=FALSE,
                  value = 1993),
      sliderInput("month",
                  "Month:",
                  min = 3,
                  max = 9,
                  step = 1,
                  ticks=FALSE,
                  value = 3),
      sliderTextInput(
        inputId    = "time",
        label      = "Month :",
        choices    = c(outer(substring(month.name[3:9], 1, 3), 1993:2015, paste)),
        selected   = "Sep 2015",
        animate    = TRUE,
        width      = "100%")
    ),
    
    mainPanel(
      # Show both plots
      tabsetPanel(
        tabPanel("US Map", br(), plotOutput("usMap"))
      )
    )
  )
)


server <- function(input, output) {
  
  output$usMap <- renderPlot({
    
    # Subset
    selection <- data[data$year == input$year & data$month == input$month,]
#    selection = selection[selection$spread ]
    
    # US map
    us <- map_data('state')
    
    # Plot areas
    ggplot() + 
      geom_map(aes(map_id=region), fill="white", color="black", size=0.15, map=us, data=us) +
      expand_limits(x = us$long, y = us$lat) +
      geom_point(data=selection, aes(x=lon, y=lat, size=area*2.7, colour=spread), shape=15) +
      scale_colour_gradient(low = "yellow", high="red") +
      scale_size_identity() + 
      theme(panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
            axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(),
            plot.title = element_text(size=20, hjust=0.5, vjust=2)) + 
      ggtitle('Areas considered in the subset') + 
      coord_fixed(1.3)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
