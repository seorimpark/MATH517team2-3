
library(shiny)
library(ggplot2)
library(tidyr)
library(dplyr)
library(corrplot)
library(maps)

# Read data
load("data_train_DF.RData")
data = data_train_DF

# Remove NA values from CNT and BA
data <- data[!is.na(data$CNT),]
data <- data[!is.na(data$BA),]

# Remove columns not related to weather
remove = c(paste("lc", seq(1:18), sep = ""), "altiMean", "altiSD")
data = data[, -which(names(data) %in% remove)]

# Renaming weather columns
data<-data%>% dplyr::rename(NSwind=clim1, WEwind=clim2, dew_temperature=clim3, temperature=clim4, 
                            potential_evaporation=clim5, solar_radiation= clim6, 
                            thermal_radiation=clim7, pressure=clim8, evaporation=clim9, 
                            precipitation=clim10)

# Wind into one component 
data <- data %>% mutate(Wspeed=(sqrt(NSwind^2+WEwind^2))) %>% select(-NSwind, -WEwind)

# Temperatures in Celsius
data$dew_temperature = data$dew_temperature - 273.15
data$temperature = data$temperature - 273.15

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
            sliderInput("CNT",
                        "Number of wildfires (CNT):",
                        min = 0,
                        max = 100,
                        step = 5,
                        value = 60),
            sliderInput("BA",
                    "Aggregated burnt area of wildfires in acres (BA):",
                      min = 0,
                      max = 10000,
                      step = 100,
                      value = 5000),
            textOutput("period")
            ),
        
            mainPanel(
              # Show both plots
              tabsetPanel(
                tabPanel("Correlation heatmap", plotOutput("corrPlot")),
                tabPanel("US Map", br(), plotOutput("usMap"))
                )
              )
        )
    )


server <- function(input, output) {
  
    output$period <- renderText({
      
      # Subset
      selection <- data[data$CNT >= input$CNT & data$BA >= input$BA,]
      range = range(selection$year)
      paste("Subset period:", range[1], "to", range[2])
      
    })

    output$corrPlot <- renderPlot({
      
      # Subset
      selection <- data[data$CNT >= input$CNT & data$BA >= input$BA,]
      
      # Correlations
      sel = cor(selection)
      
      # Plot
      corrplot(sel, method = "color", addCoef.col="black", tl.cex = 1, cl.cex = 1,
               number.cex = 1, number.digits = 2)
    
      }, width = 800, height = 800)
    
    
    output$usMap <- renderPlot({
      
      # Subset
      selection <- data[data$CNT >= input$CNT & data$BA >= input$BA,]
      
      # Finding considered areas
      loc = unique( selection[, c("lon", "lat", "area")] )
      us <- map_data('state')
      
      # Plot areas
      ggplot() + 
        geom_map(aes(map_id=region), fill="white", color="black", size=0.15, map=us, data=us) +
        expand_limits(x = us$long, y = us$lat) +
        geom_point(data=loc, aes(x=lon, y=lat, size=area*2.7), colour="red", shape=15) +
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
