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
library(tidyr)
library(dplyr)
library(maps)
library(shinyWidgets)
library(ggquiver)

#load("data_train_DF.RData")
my_model1<-load("../../Code/my_model1.rda")
#load("~/Desktop/Github/SCV/SCV_project/MATH517team2-3/VisualisationProj3/data_train_DF.RData")
data_state <- read.csv("BA_CNT_per_state.csv")
#data_withNA = data_train_DF
data_withNA = my_model1
#removing NA
data<-data_withNA #%>% drop_na() 

data_all<-data
data_all$Date <- as.yearmon(paste(data_all$year, data_all$month), "%Y %m")
data_all$Date<-as.Date(data_all$Date)
#data_all<-data_all[ , -which(names(data_all) %in% c("year","month"))]


# Renaming weather columns
data_all<-data_all%>% dplyr::rename(WEwind=clim1, NSwind=clim2, dew_temperature=clim3, temperature=clim4, 
                                    potential_evaporation=clim5, solar_radiation= clim6, 
                                    thermal_radiation=clim7, pressure=clim8, evaporation=clim9, 
                                    precipitation=clim10)

# Wind into one component 
#data_all <- data_all %>% mutate(Wspeed=(sqrt(NSwind^2+WEwind^2))) %>% select(-NSwind, -WEwind)

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

Month_to_number<-function(m){
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
               HTML("<br/>")),
      
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
                      post = NULL, dragRange = TRUE),
      radioButtons("Option1","Select the mode",
                   choices = c("None","CNT","BA"),
                   selected = "None"
      ),
    ),
    
    # Show a plot of the generated distribution
    mainPanel(tabsetPanel(
      #tabPanel("Fires", br(), plotOutput("Fires")),
      tabPanel("Wind direction", br(), plotOutput("wind")),
      tabPanel("Dew Temperatures", br(), plotOutput("dtemp")),
      tabPanel("Temperatures", br(), plotOutput("Temp")),
      tabPanel("Potential Evaporation", br(), plotOutput("pevap")),
      tabPanel("Evaporation", br(), plotOutput("evap")),
      tabPanel("Solar Radiation", br(), plotOutput("SR")),
      tabPanel("Thermal Radiation", br(), plotOutput("TR")),
      tabPanel("Pressure", br(), plotOutput("pressure")),
      tabPanel("Precipitation", br(), plotOutput("prec"))
    )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  output$Temp <- renderPlot({
    Year <- input$Year
    Month<-input$Month
    Month<-Month_to_number(Month)
    Year <-as.integer(Year)
    data_year<-filter_year_month(data_all,Year,Month)
    # group_by(.dots=c("lat", "lon")) %>% mutate(TEMPyMean=mean(data_year)) %>% 
    if (input$Option1=="None"){
      templot <- (data_year %>% 
                    group_by(.dots=c("lat", "lon"))) %>%
        arrange(desc(temperature)) %>% distinct(lat, lon, temperature) %>%
        ggplot(aes(x=lon, y=lat)) + 
        geom_point(shape=15, size=2.5) + 
        geom_point(aes(color=temperature), stroke = 1, shape = 16,size=2.8) +#(color=cut(temperature, c(-20,-10, 0,10,20,30,40))), stroke = 1, shape = 16,size=2.6) +
        scale_color_viridis_c(option="D")+
        #scale_color_manual(name = "Temperature",
        #                    values = c("(-Inf,-20]" = "paleturquoise2",
        #                               "(-20,-10]" = "blue",
        #                               "(-10,0]" = "cornflowerblue",
        #                               "(0,10]" = "yellow",
        #                               "(10,20]" = "darkgoldenrod2",
        #                               "(20,30]" = "orange",
        #                               "(30, 40]" = "red",
        #                               "(40, +Inf]" = "red"),
        #                    labels = c("<-20","(-20,-10]", "(-10,0]","(0,10]","(10,20]","(20,30]","(30,40]"))+
      labs(color="Temperature (Celsius)") +
      theme_minimal() + 
      ggtitle(paste('Temperature in USA in month',Month,"of year", Year))
      templot
    }
    
    else if (input$Option1=="CNT") {
      templot <- (data_year %>% 
                    group_by(.dots=c("lat", "lon"))) %>%
        arrange(desc(temperature)) %>% distinct(lat, lon, temperature,CNT) %>%
        ggplot(aes(x=lon, y=lat)) +
        geom_point(shape=15, size=2.5)+ #colour = "transparent")+ 
        geom_point(aes(color=temperature), stroke = 1, shape = 16,size=2.8) +#(color=cut(temperature, c(-20,-10, 0,10,20,30,40))), stroke = 1, shape = 16,size=2.6) +
        scale_color_viridis_c(option="D")+
        geom_point(aes(size = CNT), alpha=0.6, color="red", stroke = 0, shape = 16) +
        scale_size(breaks = c(0, 10, 50, 100), 
                   range = c(0, 15), 
                   name="num. of wildfires", 
                   labels = expression(0, 10, 50, 100)) + 
        labs(color="Temperature (Celsius)") + 
        theme_minimal() + 
        ggtitle(paste('Temperature in USA in month',Month,"of year", Year))
      templot
    }
    else{
      templot <- (data_year %>% 
                    group_by(.dots=c("lat", "lon"))) %>%
        arrange(desc(temperature)) %>% distinct(lat, lon, temperature,BA) %>%
        ggplot(aes(x=lon, y=lat)) +
        geom_point(shape=15, size=2.5)+ #colour = "transparent")+ 
        geom_point(aes(color=temperature), stroke = 1, shape = 16,size=2.8) +#(color=cut(temperature, c(-20,-10, 0,10,20,30,40))), stroke = 1, shape = 16,size=2.6) +
        scale_color_viridis_c(option="D")+
        geom_point(aes(size = log2(BA+1)), alpha=0.5, color="red", stroke = 0, shape = 16) +
        scale_size(breaks = c(0,5,10), 
                   range = c(0, 8), 
                   name="Burnt Area (Acres)", 
                   labels = expression(0, 2^5,2^10)) + 
        labs(color="Temperature (Celsius)")+
        theme_minimal() + 
        ggtitle(paste('Temperature in USA in month',Month,"of year", Year))
      templot
    }
  })
  
  output$dtemp <- renderPlot({
    Year <- input$Year
    Month<-input$Month
    Month<-Month_to_number(Month)
    Year <-as.integer(Year)
    data_year<-filter_year_month(data_all,Year,Month)
    # group_by(.dots=c("lat", "lon")) %>% mutate(TEMPyMean=mean(data_year)) %>% 
      if (input$Option1=="None"){
        templot <- (data_year %>% 
                      group_by(.dots=c("lat", "lon"))) %>%
          arrange(desc(dew_temperature)) %>% distinct(lat, lon, dew_temperature) %>%
          ggplot(aes(x=lon, y=lat)) + 
          geom_point(shape=15, size=2.5) + 
          geom_point(aes(color=dew_temperature), stroke = 1, shape = 16,size=2.8) +#(color=cut(temperature, c(-20,-10, 0,10,20,30,40))), stroke = 1, shape = 16,size=2.6) +
          scale_color_viridis_c(option="D")+
          #scale_color_manual(name = "Temperature",
          #                    values = c("(-Inf,-20]" = "paleturquoise2",
          #                               "(-20,-10]" = "blue",
          #                               "(-10,0]" = "cornflowerblue",
          #                               "(0,10]" = "yellow",
          #                               "(10,20]" = "darkgoldenrod2",
          #                               "(20,30]" = "orange",
          #                               "(30, 40]" = "red",
          #                               "(40, +Inf]" = "red"),
          #                    labels = c("<-20","(-20,-10]", "(-10,0]","(0,10]","(10,20]","(20,30]","(30,40]"))+
          labs(color="Temperature (Celsius)") +
          theme_minimal() + 
          ggtitle(paste('Dew temperature in USA in month',Month,"of year", Year))
        templot
      }
      
      else if (input$Option1=="CNT") {
        templot <- (data_year %>% 
                      group_by(.dots=c("lat", "lon"))) %>%
          arrange(desc(dew_temperature)) %>% distinct(lat, lon, dew_temperature,CNT) %>%
          ggplot(aes(x=lon, y=lat)) +
          geom_point(shape=15, size=2.5)+ #colour = "transparent")+ 
          geom_point(aes(color=dew_temperature), stroke = 1, shape = 16,size=2.8) +#(color=cut(temperature, c(-20,-10, 0,10,20,30,40))), stroke = 1, shape = 16,size=2.6) +
          scale_color_viridis_c(option="D")+
          geom_point(aes(size = CNT), alpha=0.6, color="red", stroke = 0, shape = 16) +
          scale_size(breaks = c(0, 10, 50, 100), 
                     range = c(0, 15), 
                     name="num. of wildfires", 
                     labels = expression(0, 10, 50, 100)) + 
          labs(color="Temperature (Celsius)") + 
          theme_minimal() + 
          ggtitle(paste('Dew temperature in USA in month',Month,"of year", Year))
        templot
      }
      else{
        templot <- (data_year %>% 
                      group_by(.dots=c("lat", "lon"))) %>%
          arrange(desc(dew_temperature)) %>% distinct(lat, lon, dew_temperature,BA) %>%
          ggplot(aes(x=lon, y=lat)) +
          geom_point(shape=15, size=2.5)+ #colour = "transparent")+ 
          geom_point(aes(color=dew_temperature), stroke = 1, shape = 16,size=2.8) +#(color=cut(temperature, c(-20,-10, 0,10,20,30,40))), stroke = 1, shape = 16,size=2.6) +
          scale_color_viridis_c(option="D")+
          geom_point(aes(size = log2(BA+1)), alpha=0.5, color="red", stroke = 0, shape = 16) +
          scale_size(breaks = c(0,5,10), 
                     range = c(0, 8), 
                     name="Burnt Area (Acres)", 
                     labels = expression(0, 2^5,2^10)) + 
          labs(color="Temperature (Celsius)")+
          theme_minimal() + 
          ggtitle(paste('Dew temperature in USA in month',Month,"of year", Year))
        templot
      }
  })
  
  output$wind <- renderPlot({
    Year <- input$Year
    Month<-input$Month
    Month<-Month_to_number(Month)
    Year <-as.integer(Year)
    data_year<-filter_year_month(data_all,Year,Month)
    data_year<-data_year[seq(1,nrow(data_year),2), ]
    if(input$Option1=="None"){
      templot <- (data_year %>% 
                    group_by(.dots=c("lat", "lon"))) %>%
        arrange(desc(WEwind)) %>% distinct(lat, lon, WEwind, NSwind) %>%
        ggplot(aes(x=lon, y=lat)) +
        geom_quiver(aes(u=WEwind,v=NSwind),vecsize=6,center=TRUE) +
        theme_minimal() + 
        ggtitle(paste('Direction of wind in USA in month',Month,"of year", Year))
      templot
    }
    else if (input$Option1=="CNT") {
      
      templot <- (data_year %>% 
                    group_by(.dots=c("lat", "lon"))) %>%
        # group_by(.dots=c("lat", "lon")) %>% mutate(TEMPyMean=mean(data_year)) %>% 
        arrange(desc(WEwind)) %>% distinct(lat, lon, WEwind, NSwind,CNT) %>%
        ggplot(aes(x=lon, y=lat)) +
        geom_quiver(aes(u=WEwind,v=NSwind),vecsize=4,center=TRUE) +
        geom_point(aes(size = CNT), alpha=0.6, color="red", stroke = 0, shape = 16) +
        scale_size(breaks = c(0, 10, 50, 100), 
                   range = c(0, 15), 
                   name="num. of wildfires", 
                   labels = expression(0, 10, 50, 100)) + 
        theme_minimal() + 
        ggtitle(paste('Direction of wind in USA in month',Month,"of year", Year))
      templot
    }
    else{
      templot <- (data_year %>% 
                    group_by(.dots=c("lat", "lon"))) %>%
        arrange(desc(WEwind)) %>% distinct(lat, lon, WEwind, NSwind,BA) %>%
        ggplot(aes(x=lon, y=lat)) +
        geom_quiver(aes(u=WEwind,v=NSwind),vecsize=4,center=TRUE) +
        geom_point(aes(size = log2(BA+1)), alpha=0.5, color="red", stroke = 0, shape = 16) +
        scale_size(breaks = c(0,5,10), 
                   range = c(0, 8), 
                   name="Burnt Area (Acres)", 
                   labels = expression(0, 2^5,2^10)) + 
        theme_minimal() + 
        ggtitle(paste('Direction of wind in USA in month',Month,"of year", Year))
      templot
    }
  })
  
  output$SR <- renderPlot({
    Year <- input$Year
    Month<-input$Month
    Month<-Month_to_number(Month)
    Year <-as.integer(Year)
    data_year<-filter_year_month(data_all,Year,Month)
    
    if(input$Option1=="None"){
      templot <- (data_year %>% 
                    group_by(.dots=c("lat", "lon"))) %>%
        # group_by(.dots=c("lat", "lon")) %>% mutate(TEMPyMean=mean(data_year)) %>% 
        arrange(desc(solar_radiation)) %>% distinct(lat, lon, solar_radiation) %>%
        ggplot(aes(x=lon, y=lat)) +
        geom_point(shape=15, size=2.5)+ 
        geom_point(aes(color=solar_radiation), stroke = 1, shape = 16,size=2.8)+ #c(10**7,1.4*10**7,1.8*10**7,2.2*10**7,2.4*10**7,2.8*10**7))), stroke = 1, shape = 16,size=2.6) +
        scale_color_viridis_c(option="D")+
        labs(color="Solar radiation (J/m2)")+
        theme_minimal() + 
        ggtitle(paste('Solar radiation in USA in month',Month,"of year", Year))
      templot
    }
    
    else if (input$Option1=="CNT") {
      templot <- (data_year %>% 
                    group_by(.dots=c("lat", "lon"))) %>%
        # group_by(.dots=c("lat", "lon")) %>% mutate(TEMPyMean=mean(data_year)) %>% 
        arrange(desc(solar_radiation)) %>% distinct(lat, lon,solar_radiation,CNT) %>%
        ggplot(aes(x=lon, y=lat)) +
        geom_point(shape=15, size=2.5)+ 
        geom_point(aes(color=solar_radiation), stroke = 1, shape = 16,size=2.8)+ #c(10**7,1.4*10**7,1.8*10**7,2.2*10**7,2.4*10**7,2.8*10**7))), stroke = 1, shape = 16,size=2.6) +
        scale_color_viridis_c(option="D")+
        geom_point(aes(size = CNT), alpha=0.6, color="red", stroke = 0, shape = 16) +
        scale_size(breaks = c(0, 10, 50, 100), 
                   range = c(0, 15), 
                   name="num. of wildfires", 
                   labels = expression(0, 10, 50, 100)) + 
        labs(color="Solar radiation (J/m2)")+
        theme_minimal() + 
        ggtitle(paste('Solar radiation in USA in month',Month,"of year", Year))
      templot
    }
    
    else{
      templot <- (data_year %>% 
                    group_by(.dots=c("lat", "lon"))) %>%
        arrange(desc(solar_radiation)) %>% distinct(lat, lon, solar_radiation,BA) %>%
        ggplot(aes(x=lon, y=lat)) +
        geom_point(shape=15, size=2.5)+ 
        geom_point(aes(color=solar_radiation), stroke = 1, shape = 16,size=2.8)+ #c(10**7,1.4*10**7,1.8*10**7,2.2*10**7,2.4*10**7,2.8*10**7))), stroke = 1, shape = 16,size=2.6) +
        scale_color_viridis_c(option="D")+
        geom_point(aes(size = log2(BA+1)), alpha=0.5, color="red", stroke = 0, shape = 16) +
        scale_size(breaks = c(0,5,10), 
                   range = c(0, 8), 
                   name="Burnt Area (Acres)", 
                   labels = expression(0, 2^5,2^10)) +
        labs(color="Solar radiation (J/m2)")+
        theme_minimal() + 
        ggtitle(paste('Solar radiation in USA in month',Month,"of year", Year))
      templot
    }
  })
  
  output$TR <- renderPlot({
    Year <- input$Year
    Month<-input$Month
    Month<-Month_to_number(Month)
    Year <-as.integer(Year)
    data_year<-filter_year_month(data_all,Year,Month)
    if (input$Option1=="None") {
      templot <- (data_year %>% 
                    group_by(.dots=c("lat", "lon"))) %>%
        # group_by(.dots=c("lat", "lon")) %>% mutate(TEMPyMean=mean(data_year)) %>% 
        arrange(desc(thermal_radiation)) %>% distinct(lat, lon, thermal_radiation) %>%
        ggplot(aes(x=lon, y=lat)) +
        geom_point(shape=15, size=2.5)+ 
        geom_point(aes(color=thermal_radiation),stroke = 1, shape = 16,size=2.8)+ #c(-2*10**7,-10**7,-8*10**6,-6*10**6,-4*10**6,-2*10**6,-10**5))), stroke = 1, shape = 16,size=2.6) +
          scale_color_viridis_c(option="D")+
        labs(color="Thermal radiation (J/m2)")+
        theme_minimal() + 
        ggtitle(paste('Thermal radiation in USA in month',Month,"of year", Year))
      templot
    }
    else if (input$Option1=="CNT") {
      
      templot <- (data_year %>% 
                    group_by(.dots=c("lat", "lon"))) %>%
        # group_by(.dots=c("lat", "lon")) %>% mutate(TEMPyMean=mean(data_year)) %>% 
        arrange(desc(thermal_radiation)) %>% distinct(lat, lon, thermal_radiation,CNT) %>%
        ggplot(aes(x=lon, y=lat)) +
        geom_point(shape=15, size=2.5)+ 
        geom_point(aes(color=thermal_radiation),stroke = 1, shape = 16,size=2.8)+ #c(-2*10**7,-10**7,-8*10**6,-6*10**6,-4*10**6,-2*10**6,-10**5))), stroke = 1, shape = 16,size=2.6) +
        scale_color_viridis_c(option="D")+
        geom_point(aes(size = CNT), alpha=0.6, color="red", stroke = 0, shape = 16) +
        scale_size(breaks = c(0, 10, 50, 100), 
                   range = c(0, 15), 
                   name="num. of wildfires", 
                   labels = expression(0, 10, 50, 100)) + 
        labs(color="Thermal radiation (J/m2)")+
        theme_minimal() + 
        ggtitle(paste('Thermal radiation in USA in month',Month,"of year", Year))
      templot
    }
    else{
      templot <- (data_year %>% 
                    group_by(.dots=c("lat", "lon"))) %>%
        arrange(desc(thermal_radiation)) %>% distinct(lat, lon, thermal_radiation,BA) %>%
        ggplot(aes(x=lon, y=lat)) +
        geom_point(shape=15, size=2.5)+ 
        geom_point(aes(color=thermal_radiation),stroke = 1, shape = 16,size=2.8)+ #c(-2*10**7,-10**7,-8*10**6,-6*10**6,-4*10**6,-2*10**6,-10**5))), stroke = 1, shape = 16,size=2.6) +
        scale_color_viridis_c(option="D")+
        geom_point(aes(size = log2(BA+1)), alpha=0.5, color="red", stroke = 0, shape = 16) +
        scale_size(breaks = c(0,5,10), 
                   range = c(0, 8), 
                   name="Burnt Area (Acres)", 
                   labels = expression(0, 2^5,2^10)) + 
        labs(color="Thermal radiation (J/m2)")+
        theme_minimal() + 
        ggtitle(paste('Thermal radiation in USA in month',Month,"of year", Year))
      templot
    }
    
  })
  output$pressure <- renderPlot({
    Year <- input$Year
    Month<-input$Month
    Month<-Month_to_number(Month)
    Year <-as.integer(Year)
    data_year<-filter_year_month(data_all,Year,Month)
    if(input$Option1=="None"){
      templot <- (data_year %>% 
                    group_by(.dots=c("lat", "lon"))) %>%
        # group_by(.dots=c("lat", "lon")) %>% mutate(TEMPyMean=mean(data_year)) %>% 
        arrange(desc(pressure)) %>% distinct(lat, lon, pressure) %>%
        ggplot(aes(x=lon, y=lat)) +
        geom_point(shape=15, size=2.5)+ 
        geom_point(aes(color=pressure), stroke = 1, shape = 16,size=2.8)+ #c(6*10**4,7*10**4,8*10**4,9*10**4,10**5,1.1*10**5,1.2*10**5))), stroke = 1, shape = 16,size=2.6) +
        scale_color_viridis_c(option="E")+
        labs(color="Surface pressure (Pa)")+
        theme_minimal() + 
        ggtitle(paste('Surface pressure in USA in month',Month,"of year", Year))
      templot
    }
    else if (input$Option1=="CNT") {
      
      templot <- (data_year %>% 
                    group_by(.dots=c("lat", "lon"))) %>%
        # group_by(.dots=c("lat", "lon")) %>% mutate(TEMPyMean=mean(data_year)) %>% 
        arrange(desc(pressure)) %>% distinct(lat, lon, pressure,CNT) %>%
        ggplot(aes(x=lon, y=lat)) +
        geom_point(shape=15, size=2.5)+ 
        geom_point(aes(color=pressure), stroke = 1, shape = 16,size=2.8)+ #c(6*10**4,7*10**4,8*10**4,9*10**4,10**5,1.1*10**5,1.2*10**5))), stroke = 1, shape = 16,size=2.6) +
        scale_color_viridis_c(option="E")+
        geom_point(aes(size = CNT), alpha=0.6, color="red", stroke = 0, shape = 16) +
        scale_size(breaks = c(0, 10, 50, 100), 
                   range = c(0, 15), 
                   name="num. of wildfires", 
                   labels = expression(0, 10, 50, 100)) + 
        labs(color="Surface pressure (Pa)")+
        theme_minimal() + 
        ggtitle(paste('Surface pressure in USA in month',Month,"of year", Year))
      templot
    }
    else{
      templot <- (data_year %>% 
                    group_by(.dots=c("lat", "lon"))) %>%
        arrange(desc(pressure)) %>% distinct(lat, lon, pressure,BA) %>%
        ggplot(aes(x=lon, y=lat)) +
        geom_point(shape=15, size=2.5)+ 
        geom_point(aes(color=pressure), stroke = 1, shape = 16,size=2.8)+ #c(6*10**4,7*10**4,8*10**4,9*10**4,10**5,1.1*10**5,1.2*10**5))), stroke = 1, shape = 16,size=2.6) +
        scale_color_viridis_c(option="E")+
        geom_point(aes(size = log2(BA+1)), alpha=0.5, color="red", stroke = 0, shape = 16) +
        scale_size(breaks = c(0,5,10), 
                   range = c(0, 8), 
                   name="Burnt Area (Acres)", 
                   labels = expression(0, 2^5,2^10)) + 
        labs(color="Surface pressure (Pa)")+
        theme_minimal() + 
        ggtitle(paste('Surface pressure in USA in month',Month,"of year", Year))
      templot
    }
    
  })
  
  output$pevap <- renderPlot({
    Year <- input$Year
    Month<-input$Month
    Month<-Month_to_number(Month)
    Year <-as.integer(Year)
    data_year<-filter_year_month(data_all,Year,Month)
    if(input$Option1=="None"){
      templot <- (data_year %>% 
                    group_by(.dots=c("lat", "lon"))) %>%
        # group_by(.dots=c("lat", "lon")) %>% mutate(TEMPyMean=mean(data_year)) %>% 
        arrange(desc(potential_evaporation)) %>% distinct(lat, lon, potential_evaporation) %>%
        ggplot(aes(x=lon, y=lat)) +
        geom_point(shape=15, size=2.5)+ 
        geom_point(aes(color=potential_evaporation), stroke = 1, shape = 16,size=2.8) + #, c(-5*10**(-2),-3*10**(-2),-2.5*10**(-2),-2*10**(-2),-1.5*10**(-2),-1*10**(-2),-5*10**(-3),-10**(-3),0.01))), stroke = 1, shape = 16,size=2.6) +
        scale_color_viridis_c(option="D",direction = -1)+
        labs(color="Evaporation (of water) (m)")+
        theme_minimal() + 
        ggtitle(paste('Potential evaporation of water in USA in month',Month,"of year", Year))
      templot
    }
    else if (input$Option1=="CNT") {
      
      templot <- (data_year %>% 
                    group_by(.dots=c("lat", "lon"))) %>%
        # group_by(.dots=c("lat", "lon")) %>% mutate(TEMPyMean=mean(data_year)) %>% 
        arrange(desc(potential_evaporation)) %>% distinct(lat, lon, potential_evaporation,CNT) %>%
        ggplot(aes(x=lon, y=lat)) +
        geom_point(shape=15, size=2.5)+ 
        geom_point(aes(color=potential_evaporation), stroke = 1, shape = 16,size=2.8) + #, c(-5*10**(-2),-3*10**(-2),-2.5*10**(-2),-2*10**(-2),-1.5*10**(-2),-1*10**(-2),-5*10**(-3),-10**(-3),0.01))), stroke = 1, shape = 16,size=2.6) +
        scale_color_viridis_c(option="D",direction = -1)+
        geom_point(aes(size = CNT), alpha=0.6, color="red", stroke = 0, shape = 16) +
        scale_size(breaks = c(0, 10, 50, 100), 
                   range = c(0, 15), 
                   name="num. of wildfires", 
                   labels = expression(0, 10, 50, 100)) + 
        labs(color="Evaporation (of water) (m)")+
        theme_minimal() + 
        ggtitle(paste('Potential evaporation of water in USA in month',Month,"of year", Year))
      templot
    }
    else{
      templot <- (data_year %>% 
                    group_by(.dots=c("lat", "lon"))) %>%
        arrange(desc(potential_evaporation)) %>% distinct(lat, lon, potential_evaporation,BA) %>%
        ggplot(aes(x=lon, y=lat)) +
        geom_point(shape=15, size=2.5)+ 
        geom_point(aes(color=potential_evaporation), stroke = 1, shape = 16,size=2.8) + #, c(-5*10**(-2),-3*10**(-2),-2.5*10**(-2),-2*10**(-2),-1.5*10**(-2),-1*10**(-2),-5*10**(-3),-10**(-3),0.01))), stroke = 1, shape = 16,size=2.6) +
        scale_color_viridis_c(option="D",direction = -1)+
        geom_point(aes(size = log2(BA+1)), alpha=0.5, color="red", stroke = 0, shape = 16) +
        scale_size(breaks = c(0,5,10), 
                   range = c(0, 8), 
                   name="Burnt Area (Acres)", 
                   labels = expression(0, 2^5,2^10)) + 
        labs(color="Evaporation (of water) (m)")+
        theme_minimal() + 
        ggtitle(paste('Potential evaporation of water in USA in month',Month,"of year", Year))
      templot
    }
  })
  
  output$evap <- renderPlot({
    Year <- input$Year
    Month<-input$Month
    Month<-Month_to_number(Month)
    Year <-as.integer(Year)
    data_year<-filter_year_month(data_all,Year,Month)
    if(input$Option1=="None"){
      templot <- (data_year %>% 
                    group_by(.dots=c("lat", "lon"))) %>%
        # group_by(.dots=c("lat", "lon")) %>% mutate(TEMPyMean=mean(data_year)) %>% 
        arrange(desc(evaporation)) %>% distinct(lat, lon, evaporation) %>%
        ggplot(aes(x=lon, y=lat)) +
        geom_point(shape=15, size=2.5)+ 
        geom_point(aes(color=evaporation), stroke = 1, shape = 16,size=2.8) +#, c(-7*10**(-3),-6*10**(-3),-5*10**(-3),-4*10**(-3),-3*10**(-3),-2*10**(-3),-10**(-3),0.1))), stroke = 1, shape = 16,size=2.6) +
        scale_color_viridis_c(option="D",direction = -1)+
        labs(color="Evaporation (of water) (m)")+
        theme_minimal() + 
        ggtitle(paste('Evaporation of water in USA in month',Month,"of year", Year))
      templot
    }
    else if (input$Option1=="CNT") {
      
      templot <- (data_year %>% 
                    group_by(.dots=c("lat", "lon"))) %>%
        # group_by(.dots=c("lat", "lon")) %>% mutate(TEMPyMean=mean(data_year)) %>% 
        arrange(desc(evaporation)) %>% distinct(lat, lon, evaporation,CNT) %>%
        ggplot(aes(x=lon, y=lat)) +
        geom_point(shape=15, size=2.5)+ 
        geom_point(aes(color=evaporation), stroke = 1, shape = 16,size=2.8) +#, c(-7*10**(-3),-6*10**(-3),-5*10**(-3),-4*10**(-3),-3*10**(-3),-2*10**(-3),-10**(-3),0.1))), stroke = 1, shape = 16,size=2.6) +
        scale_color_viridis_c(option="D",direction = -1)+
        geom_point(aes(size = CNT), alpha=0.6, color="red", stroke = 0, shape = 16) +
        scale_size(breaks = c(0, 10, 50, 100), 
                   range = c(0, 15), 
                   name="num. of wildfires", 
                   labels = expression(0, 10, 50, 100)) + 
        labs(color="Evaporation (of water) (m)")+
        theme_minimal() + 
        ggtitle(paste('Evaporation of water in USA in month',Month,"of year", Year))
      templot
    }
    else{
      templot <- (data_year %>% 
                    group_by(.dots=c("lat", "lon"))) %>%
        arrange(desc(evaporation)) %>% distinct(lat, lon, evaporation,BA) %>%
        ggplot(aes(x=lon, y=lat)) +
        geom_point(shape=15, size=2.5)+ 
        geom_point(aes(color=evaporation), stroke = 1, shape = 16,size=2.8) +#, c(-7*10**(-3),-6*10**(-3),-5*10**(-3),-4*10**(-3),-3*10**(-3),-2*10**(-3),-10**(-3),0.1))), stroke = 1, shape = 16,size=2.6) +
        scale_color_viridis_c(option="D",direction = -1)+
        geom_point(aes(size = log2(BA+1)), alpha=0.5, color="red", stroke = 0, shape = 16) +
        scale_size(breaks = c(0,5,10), 
                   range = c(0, 8), 
                   name="Burnt Area (Acres)", 
                   labels = expression(0, 2^5,2^10)) + 
        labs(color="Evaporation (of water) (m)")+
        theme_minimal() + 
        ggtitle(paste('Evaporation of water in USA in month',Month,"of year", Year))
      templot
    }
  })
  
  output$prec <- renderPlot({
    Year <- input$Year
    Month<-input$Month
    Month<-Month_to_number(Month)
    Year <-as.integer(Year)
    data_year<-filter_year_month(data_all,Year,Month)
    if(input$Option1=="None"){
      templot <- (data_year %>% 
                    group_by(.dots=c("lat", "lon"))) %>%
        # group_by(.dots=c("lat", "lon")) %>% mutate(TEMPyMean=mean(data_year)) %>% 
        arrange(desc(precipitation)) %>% distinct(lat, lon, precipitation) %>%
        ggplot(aes(x=lon, y=lat)) +
        geom_point(shape=15, size=2.5)+ 
        geom_point(aes(color=precipitation), stroke = 1, shape = 16,size=2.8)+#, c(0,10**-3,2*10**-3,3*10**-3,4*10**-3,5*10**-3,1))), stroke = 1, shape = 16,size=2.6) +
        scale_colour_viridis_c(option = "mako",direction = -1)+
        labs(color="Precipitation (m)")+
        theme_minimal() + 
        ggtitle(paste('Precipitation in USA in month',Month,"of year", Year))
      templot
    }
    else if (input$Option1=="CNT") {
      
      templot <- (data_year %>% 
                    group_by(.dots=c("lat", "lon"))) %>%
        # group_by(.dots=c("lat", "lon")) %>% mutate(TEMPyMean=mean(data_year)) %>% 
        arrange(desc(precipitation)) %>% distinct(lat, lon, precipitation,CNT) %>%
        ggplot(aes(x=lon, y=lat)) +
        geom_point(shape=15, size=2.5)+ 
        geom_point(aes(color=precipitation), stroke = 1, shape = 16,size=2.8)+#, c(0,10**-3,2*10**-3,3*10**-3,4*10**-3,5*10**-3,1))), stroke = 1, shape = 16,size=2.6) +
        scale_colour_viridis_c(option = "mako",direction = -1)+
        geom_point(aes(size = CNT), alpha=0.6, color="red", stroke = 0, shape = 16) +
        scale_size(breaks = c(0, 10, 50, 100), 
                   range = c(0, 15), 
                   name="num. of wildfires", 
                   labels = expression(0, 10, 50, 100)) + 
        labs(color="Precipitation (m)")+
        theme_minimal() + 
        ggtitle(paste('Precipitation in USA in month',Month,"of year", Year))
      templot
    }
    else{
      templot <- (data_year %>% 
                    group_by(.dots=c("lat", "lon"))) %>%
        arrange(desc(precipitation)) %>% distinct(lat, lon, precipitation,BA) %>%
        ggplot(aes(x=lon, y=lat)) +
        geom_point(shape=15, size=2.5)+ 
        geom_point(aes(color=precipitation), stroke = 1, shape = 16,size=2.8)+#, c(0,10**-3,2*10**-3,3*10**-3,4*10**-3,5*10**-3,1))), stroke = 1, shape = 16,size=2.6) +
        scale_colour_viridis_c(option = "mako",direction = -1)+
        geom_point(aes(size = log2(BA+1)), alpha=0.5, color="red", stroke = 0, shape = 16) +
        scale_size(breaks = c(0,5,10), 
                   range = c(0, 8), 
                   name="Burnt Area (Acres)", 
                   labels = expression(0, 2^5,2^10)) + 
        labs(color="Precipitation (m)")+
        theme_minimal() + 
        ggtitle(paste('Precipitation in USA in month',Month,"of year", Year))
      templot
    }

    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


