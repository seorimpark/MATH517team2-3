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



load("data_train_DF.RData")

# # This function, borrowed from http://www.r-bloggers.com/safe-loading-of-rdata-files/, load the Rdata into a new environment to avoid side effects
# LoadToEnvironment <- function(RData, env=new.env()) {
#   load(RData, env)
#   return(env)
# }

data = data_train_DF


#removing NA
#data<-data_withNA %>% drop_na()

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
lcs<-1:18



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
ui <- fluidPage(

    # Application title
    titlePanel("Land Cover Analysis"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            helpText(style="text-align: justify;",
                              "This application allows you to visualize the change in distribution of the different landcovers as a function of time according to the chosen location. Below are sliders to select the exact longitude and latitude investigated.", 
                              HTML("<br/>")," The landcovers are as follows: cropland rainfed (1), cropland rainfed herbaceous cover (2), mosaic cropland
(3), mosaic natural vegetation
(4), tree broadleaved evergreen closed to open
(5), tree broadleaved deciduous closed to open
(6), tree needleleave evergreen closed to open
(7), tree needleleaved deciduous closed to open
(8), tree mixed
(9), mosaic tree and shrub
(10), shrubland
(11), grassland
(12), sparse vegetation
(13), tree cover flooded fresh or brakish water
(14), shrub or herbaceous cover flooded
(15), urban
(16), bare areas
(17), water (18). Note that the area proportions of all the landcovers do not always sum to exactly 1 for each pixel and month since a few classes with quasi-0 proportion have been removed. These 18 predictors are therefore almost collinear. Note also that when close to the border, you can still see results displayed ( for example taking the coordinates 41.3 and -124.3). This is due to the definition of area and how the map grid was defined. Finally, note that if there is no plot, it means the latitude and longitude are not part of the US."),
            
                sliderInput(
                    "lat", "Select the latitude",
                    min=25.25,
                    max=49.25,
                    step = 0.5,
                    value= 40.75,
                    ticks = FALSE,
                    animate = animationOptions(interval = 3000, loop = TRUE)
                ),
                sliderInput(
                    "lon", "Select the longitude",
                    min=-124.75,
                    max=-66.75,
                    step = 0.5,
                    value= -90.25,
                    ticks = FALSE,
                    animate = animationOptions(interval = 3000, loop = TRUE)
                )),

        # Show a plot of the generated distribution
        mainPanel(tabsetPanel(
            tabPanel("US Map", br(), plotOutput("usMap")),
            tabPanel("Landcover Distribution", br(), plotOutput("distPlot"))
            
        )
    
)))

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        distLat <- input$lat
        distLon <- input$lon
        if(distLat != 25.25 &&distLat != 49.25) {distLat<-distLat-0.05}
        if(distLon != -124.25 &&distLat != -66.75) {distLon<-distLon+0.05}
        
        
        df <- NULL
        for(lcs_ in 1:18)
        {trial1<-lc_from_place(data_all, distLon,distLat, lcs_)
        if(nrow(trial1)!= 0){
            temp_df <- data.frame(x=trial1[,2], y=trial1[,1], col=rep(lcs_:lcs_, each=nrow(trial1)))
            df <- rbind(df,temp_df)} }
        if(!is.null(df)){
            p<-ggplot(df,aes(x=x,y=y,group=col,colour=factor(col))) + geom_line()+guides(colour=guide_legend(ncol=2))
            p+ ggtitle("Distribution of land covers across time") +
                xlab("Years") + ylab("Percentages of land covers")
        }
        
        
    })
    output$usMap <- renderPlot({
        
        
        distLat <- input$lat
        distLon <- input$lon
        if(distLat != 25.25 &&distLat != 49.25) {distLat<-distLat-0.05}
        if(distLon != -124.25 &&distLat != -66.75) {distLon<-distLon-0.05}
        #loc = unique( selection[, c("lon", "lat", "area")] )
        us <- map_data('state')
        
        # Plot areas
        ggplot() + 
            geom_map(aes(map_id=region), fill="white", color="black", size=0.15, map=us, data=us) +
            expand_limits(x = us$long, y = us$lat) +
            geom_point(data=NULL, aes(x=distLon, y=distLat), colour="red", shape=15) +
            scale_size_identity() + 
            theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()) +
            theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
                  axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(),
                  plot.title = element_text(size=20, hjust=0.5, vjust=2)) + 
            ggtitle('Exact location considered: ') + 
            coord_fixed(1.3)
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
