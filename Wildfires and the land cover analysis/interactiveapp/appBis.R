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

# loadind data 
load("data_train_DF.RData")
data_withNA = data_train_DF

#removing NA
data<-data_withNA %>% drop_na()

filter_year<-function(data,year){
  if(year %in% unique(data$year)){
    data_filtered<-data %>% filter(year == year)
    data_filtered$Date<-as.yearmon(paste(data_filtered$year, data_filtered$month), "%Y %m")
    data_filtered$Date<-as.Date(data_filtered$Date)
    data_filtered<-data_filtered[ , -which(names(data_filtered) %in% c("year","month"))]
    return (data_filtered)}
  else return(data)}
data93<-filter_year(data,1993)

data_all<-data
data_all$Date <- as.yearmon(paste(data_all$year, data_all$month), "%Y %m")
data_all$Date<-as.Date(data_all$Date)
data_all<-data_all[ , -which(names(data_all) %in% c("year","month"))]

lats<-unique(data_all$lat)
lons<-unique(data_all$lon)
lcs<-1:17
names_lc <- c('cropland rainfed',
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


min_lon=min(data93$lon)
max_lon=max(data93$lon)
min_lat=min(data93$lat)
max_lat=max(data93$lat)



# from one lon and lat cordinates, it gives you the values of lc you want if exits
# lc goes from 1 to 18
lc_from_place<-function(df, lon_,lat_, lc_)
{
  if (lon_ %in% lons && lat_ %in% lats && lc_ %in% lcs)
  {
    values<- df %>% filter(lat == lat_) %>% filter(lon == lon_)
    col_index<-5+lc_
    values<-values[, c(col_index,36)]
    return(values)
    
  }
  else{warning("wrong indices")}
}








    





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
      value= 40.75,
      ticks = FALSE,
      animate = FALSE
    ),
    sliderInput(
      "lon", "Select the longitude",
      min=-124.75,
      max=-66.75,
      step = 0.5,
      value= -90.25,
      ticks = FALSE,
      animate = FALSE
    )),
  
  # Show a plot of the generated distribution
  mainPanel("main panel",
            fluidRow(
              splitLayout(cellWidths = c("30%","35%" ,"35%"), plotOutput("myPlot0"),plotOutput("myPlot1"), plotOutput("myPlot2"))
            ))
))


# Define server logic required to draw a histogram

server <- function(input, output) {
  output$myPlot0 <- renderPlot({
    
    image<-load.image("/Users/Mac/Desktop/SCV/SCV_project/MATH517team2-2/Wildfires and the land cover analysis/interactiveapp/distribution.jpeg")
    plot(image,axes=FALSE, frame.plot=TRUE)
  })
  output$myPlot1 <- renderPlot({
    distLat <- input$lat
    distLon <- input$lon
    if(distLat != 25.25 &&distLat != 49.25) {distLat<-distLat-0.05}
    if(distLon != -124.25 &&distLat != -66.75) {distLon<-distLon-0.05}
    
    
    df <- NULL
    for(lcs_ in 1:17)
    {trial1<-lc_from_place(data_all, lons_,lats_, lcs_)
    if(nrow(trial1)!= 0){
      temp_df <- data.frame(x=trial1[,2], y=trial1[,1], col=rep(lcs_:lcs_, each=nrow(trial1)))
      df <- rbind(df,temp_df)} }
    if(!is.null(df)){
      p<-ggplot(df,aes(x=x,y=y,group=col,colour=factor(col))) + geom_line()+guides(colour=guide_legend(ncol=2))
      p
    }
    
   
  })
  output$myPlot2 <- renderPlot({
    distLat <- input$lat
    distLon <- input$lon
    if(distLat != 25.25 &&distLat != 49.25) {distLat<-distLat-0.05}
    if(distLon != -124.25 &&distLat != -66.75) {distLon<-distLon-0.05}
    
    
    plot(data93$lon, data93$lat,pch=9,xlab=" ",lwd=2,cex = .5, ylab="", xlim=c(min_lon,max_lon ), ylim=c(min_lat, max_lat))
    par(new=TRUE)
    plot(distLon, distLat, col="red",pch="O",xlab=" ",lwd=5, ylab="", xlim=c(min_lon,max_lon ), ylim=c(min_lat, max_lat))
    
    
    
  })
  
  
  
  
}


# Run the application 
shinyApp(ui = ui, server = server)