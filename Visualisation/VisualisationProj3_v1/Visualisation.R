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
library(shinyWidgets)

load("data_train_DF.RData")
#load("~/Desktop/Github/SCV/SCV_project/MATH517team2-3/VisualisationProj3/data_train_DF.RData")

data_withNA = data_train_DF


#removing NA
data<-data_withNA %>% drop_na()

data_all<-data
data_all$Date <- as.yearmon(paste(data_all$year, data_all$month), "%Y %m")
data_all$Date<-as.Date(data_all$Date)
#data_all<-data_all[ , -which(names(data_all) %in% c("year","month"))]


# Renaming weather columns
data_all<-data_all%>% dplyr::rename(NSwind=clim1, WEwind=clim2, dew_temperature=clim3, temperature=clim4, 
                            potential_evaporation=clim5, solar_radiation= clim6, 
                            thermal_radiation=clim7, pressure=clim8, evaporation=clim9, 
                            precipitation=clim10)

# Wind into one component 
data_all <- data_all %>% mutate(Wspeed=(sqrt(NSwind^2+WEwind^2))) %>% select(-NSwind, -WEwind)

# Temperatures in Celsius
data_all$dew_temperature = data_all$dew_temperature - 273.15
data_all$temperature = data_all$temperature - 273.15

filter_year<-function(data,year){
  if(year %in% unique(data$year)){
    data_filtered<-data %>% filter(year == year)
    data_filtered$Date<-as.yearmon(paste(data_filtered$year, data_filtered$month), "%Y %m")
    data_filtered$Date<-as.Date(data_filtered$Date)
    data_filtered<-data_filtered[ , -which(names(data_filtered) %in% c("year","month"))]
    return (data_filtered)}
  else return(data)}


filter_year_month<-function(data,year2,month2){
  if((year2 %in% unique(data$year)) && (month2 %in% unique(data$month))){
    data_filtered<-data %>% filter(year == year2)%>% filter(month == month2)
    data_filtered$Date<-as.yearmon(paste(data_filtered$year, data_filtered$month), "%Y %m")
    data_filtered$Date<-as.Date(data_filtered$Date)
    #data_filtered<-data_filtered[ , -which(names(data_filtered) %in% c("year","month"))]
    return (data_filtered)}
  else 
    return(data)}

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

Month_to_number<-function(m)
{
  if(m=="January"){return(1)}
  else if(m=="February"){return(2)}
  else if(m=="March"){return(3)}
  else if(m=="April"){return(4)}
  else if(m=="May"){return(5)}
  else if(m=="June"){return(6)}
  else if(m=="July"){return(7)}
  else if(m=="August"){return(8)}
  else if(m=="September"){return(9)}
  else if(m=="October"){return(10)}
  else if(m=="November"){return(11)}
  else if(m=="December"){return(12)}
}





# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Land Cover Analysis"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      helpText(style="text-align: justify;",
               "This application allows you to visualize the change of different features over the years. Press Play for the animation.", 
               HTML("<br/>")," (Additional text if needed) "),
      
      sliderInput(
        "Year", "Select the year",
        min=1993,
        max=2015,
        step = 1,
        value= 2005,
        ticks = FALSE,
        animate = animationOptions(interval = 3000, loop = TRUE)
      ),
      sliderTextInput("Month","Select the month" , 
                      choices = month.name[c(3: 9)],
                      selected = month.name[c(5)],
                      animate = animationOptions(interval = 3000, loop = TRUE), grid = FALSE, 
                      hide_min_max = FALSE, from_fixed = FALSE,
                      to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                      to_max = NULL, force_edges = FALSE, width = NULL, pre = NULL,
                      post = NULL, dragRange = TRUE)),
     
    
    
  
    # Show a plot of the generated distribution
    mainPanel(tabsetPanel(
      tabPanel("Fires", br(), plotOutput("Fires")),
      tabPanel("Temperatures", br(), plotOutput("Temp")))
      
    )))

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$Fires <- renderPlot({
    Year <- input$Year
    Year <-as.integer(Year)
    Month<-input$Month
    Month<-Month_to_number(Month)
    data_year<-filter_year_month(data_all,Year,Month)
    p2 <- (data_year %>% 
            mutate(`Major land cover`=factor(max.col(data_year %>% select(starts_with("lc"))), 
                                             labels = names[1:18 %in% max.col(data_year %>% select(starts_with("lc")))])) %>%
            select(c(CNT, lat, lon, `Major land cover`)) %>%
            group_by(.dots=c("lat", "lon")) %>% 
            mutate(CNTy=sum(CNT, na.rm=1))) %>%
      group_by(.dots=c("lat", "lon")) %>% mutate(CNTyMean=mean(CNTy)) %>% 
      arrange(desc(CNTyMean)) %>% distinct(lat, lon, CNTyMean, `Major land cover`) %>%
      
      # ggplot(aes(x=lon, y=lat, colour = `Major land cover`)) +
      # geom_point(shape=15, size=2.4) +
      # scale_color_manual(values = c("wheat","wheat3", "tan2","khaki4","springgreen3", "darkgreen", "seagreen4","seagreen3","lawngreen","greenyellow", "darkolivegreen","red", "yellowgreen", "yellow4", "chartreuse3", "sandybrown", "black", "steelblue1")) + 
      
      ggplot(aes(x=lon, y=lat)) +
      geom_point(shape=15, size=2.4)+ 
      geom_point(aes(size = CNTyMean), alpha=0.5, color="red", stroke = 0, shape = 16) +
      scale_size(breaks = c(0, 10, 50, 100), 
                 range = c(0, 15), 
                 name="num. of wildfires", 
                 labels = expression(0, 10, 50, 100)) + 
      theme_minimal() + 
      ggtitle('Mean number of wildfires in USA in 2000')
      p2
})
  
  
  output$Temp <- renderPlot({
    Year <- input$Year
    Month<-input$Month
    Month<-Month_to_number(Month)
    Year <-as.integer(Year)
    data_year<-filter_year_month(data_all,Year,Month)
    templot <- (data_year %>% 
             group_by(.dots=c("lat", "lon"))) %>%
     # group_by(.dots=c("lat", "lon")) %>% mutate(TEMPyMean=mean(data_year)) %>% 
      arrange(desc(temperature)) %>% distinct(lat, lon, temperature) %>%
      ggplot(aes(x=lon, y=lat)) +
      geom_point(shape=15, size=2.5)+ 
      geom_point(aes(color=cut(temperature, c(-20,-10, 0,10,20,30,40))), stroke = 0, shape = 16) +
      # scale_color_manual(name = "Temperature",
      #                    values = c("(-Inf,-20]" = "paleturquoise2",
      #                               "(-20,-10]" = "cadelblue2",
      #                               "(-10,0]" = "cornflowerblue",
      #                               "(0,10]" = "yellow",
      #                               "(10,20]" = "darkgoldenrod2",
      #                               "(20,30]" = "orange",
      #                               "(30, 40]" = "red",
      #                               "(40, +Inf]" = "red"),
      #                    labels = c("<-20","(-20,-10]", "(-10,0]","(0,10]","(10,20]","(20,30]","(30,40]"))+
    
      
    theme_minimal() + 
      ggtitle(paste('Temperature in USA in month',Month,"of year", Year))
    templot
    
    
    # scale_size(breaks = c(-20,-10, 0,10,20,30,40), 
    #            range = c(0, 15), 
    #            name="Temperature", 
    #            labels = expression(-20,-10, 0,10,20,30,40)) + 
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)


