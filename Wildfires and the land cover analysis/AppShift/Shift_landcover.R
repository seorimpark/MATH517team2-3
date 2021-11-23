library(shiny)
library(tidyverse)
library(Hmisc)
library(zoo)
library(ggplot2)
library(hrbrthemes) 
library(corrplot)
library(tidyr)
library(dplyr)
library(maps)


load("data_train_DF.RData")
data = data_train_DF


#removing NA
#data<-data_withNA %>% drop_na()
data_all<-data
data_all$Date <- as.yearmon(paste(data_all$year, data_all$month), "%Y %m")
data_all$Date<-as.Date(data_all$Date)
#data_all<-data_all[ , -which(names(data_all) %in% c("year","month"))]

filter_year<-function(data,year){
  if(year %in% unique(data$year)){
    data_filtered<-data %>% filter(year == year)
    data_filtered$Date<-as.yearmon(paste(data_filtered$year, data_filtered$month), "%Y %m")
    data_filtered$Date<-as.Date(data_filtered$Date)
    data_filtered<-data_filtered[ , -which(names(data_filtered) %in% c("year","month"))]
    return (data_filtered)}
  else return(data)}

data93<-filter_year(data,1993)



lats<-unique(data_all$lat)
lons<-unique(data_all$lon)
lcs<-1:17


min_lon=min(data93$lon)
max_lon=max(data93$lon)
min_lat=min(data93$lat)
max_lat=max(data93$lat)

names = c('cropland rainfed',
          'cropland rainfed herbaceous cover',
          'mosaic cropland',
          'mosaic natural vegetation',
          'tree broadleaved evergreen closed to open',
          'tree broadleaved deciduous closed to open',
          'tree needleleave evergreen closed to open',
          'tree needleleaved deciduous closed to open',
          'tree mixed',
          'mosaic tree and shrub',
          'shrubland',
          'grassland',
          'sparse vegetation',
          'tree cover flooded fresh or brakish water',
          'shrub or herbaceous cover flooded',
          'urban',
          'bare areas',
          'water')


# p <- data_all %>% 
#   mutate(`Major land cover`=factor(max.col(data_all %>% select(starts_with("lc"))), labels = names[1:18 %in% max.col(data_all  %>% select(starts_with("lc")))])) %>% 
#   filter(year==2015) %>% ggplot(aes(x=lon, y=lat, colour = `Major land cover`)) +
#   geom_point(shape=15, size=2.4) +
#   theme_minimal() + 
#   ggtitle('Major land cover in cells') + 
#   scale_color_manual(values = c("wheat","wheat3", "tan2","khaki4","springgreen3", "darkgreen", "seagreen4", "seagreen2", "yellowgreen", "yellow4", "chartreuse3", "sandybrown", "black", "steelblue1"))
# options(repr.plot.width=10, repr.plot.height=5)
# p






# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Land Cover Analysis"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      helpText(style="text-align: justify;",
               "This application allows you to visualize the change in distribution of the predominant land cover in the region over the years. Press Play for the animation.", 
               HTML("<br/>")," Note that the area proportions of all the land covers do not always sum to exactly 1 for each pixel and month since a few classes with quasi-0 proportion have been removed. "),
      
      sliderInput(
        "Year", "Select the year",
        min=1993,
        max=2015,
        step = 1,
        value= 1996,
        ticks = FALSE,
        animate = animationOptions(interval = 3000, loop = TRUE)
      )),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabPanel("Land cover Distribution", br(), plotOutput("distPlot"))
    
    
    )))

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    Year <- input$Year
    
    p <- data_all %>% 
      mutate(`Major land cover`=factor(max.col(data_all %>% select(starts_with("lc"))), labels = names[1:18 %in% max.col(data_all  %>% select(starts_with("lc")))])) %>% 
      filter(year==Year) %>% ggplot(aes(x=lon, y=lat, colour = `Major land cover`)) +
      geom_point(shape=15, size=2.4) +
      theme_minimal() + 
      ggtitle(paste('Major land cover in cells in '),Year) + 
      scale_color_manual(values = c("wheat","wheat3", "tan2","khaki4","springgreen3", "darkgreen", "seagreen4","seagreen3","lawngreen","greenyellow", "darkolivegreen","seagreen2", "yellowgreen", "yellow4", "chartreuse3", "sandybrown", "black", "steelblue1"))
    options(repr.plot.width=10, repr.plot.height=5)
    p
    
   
    
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)


