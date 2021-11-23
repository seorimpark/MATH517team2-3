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


# loadind data 
load("../Data/data_train_DF.RData")
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
data94<-filter_year(data,1994)
data95<-filter_year(data,1995)
data96<-filter_year(data,1996)
data97<-filter_year(data,1997)
data98<-filter_year(data,1998)
data99<-filter_year(data,1999)
data00<-filter_year(data,2000)
data01<-filter_year(data,2001)
data02<-filter_year(data,2002)
data03<-filter_year(data,2003)
data04<-filter_year(data,2004)
data05<-filter_year(data,2005)
data06<-filter_year(data,2006)
data07<-filter_year(data,2007)
data08<-filter_year(data,2008)
data09<-filter_year(data,2009)
data10<-filter_year(data,2010)
data11<-filter_year(data,2011)
data12<-filter_year(data,2012)
data13<-filter_year(data,2013)
data14<-filter_year(data,2014)
data15<-filter_year(data,2015)

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
    else{return(NA)}
}

#for(lats_ in lats ){
lats_ = lats[1]
#for(lons_ in lons){
lons_= lons[1]
df <- NULL
for(lcs_ in 1:17)
{trial1<-lc_from_place(data_all, lons_,lats_, lcs_)
temp_df <- data.frame(x=trial1[,2], y=trial1[,1], col=rep(lcs_:lcs_, each=nrow(trial1)))
df <- rbind(df,temp_df)} 
p=ggplot(df,aes(x=x,y=y,group=col,colour=factor(col))) +geom_line()
p=p+guides(colour=guide_legend(ncol=2))
p
#}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
