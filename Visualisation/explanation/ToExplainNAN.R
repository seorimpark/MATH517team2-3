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

load("~/Desktop/Github/SCV/SCV_project/MATH517team2-3/VisualisationProj3/data_train_DF.RData")

data_withNA = data_train_DF


#removing NA
data<-data_withNA 
data[is.na(data)] <- 0
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




Month=6
Year1=2004
Year2=2005
data_year_1<-filter_year_month(data_all,2004,6)
data_year_2<-filter_year_month(data_all,2005,6)
templot1<- (data_year_1 %>% 
              group_by(.dots=c("lat", "lon"))) %>%
  # group_by(.dots=c("lat", "lon")) %>% mutate(TEMPyMean=mean(data_year)) %>% 
  arrange(desc(temperature)) %>% distinct(lat, lon, temperature) %>%
  ggplot(aes(x=lon, y=lat)) +
  geom_point(shape=15, size=2.5)+ 
  geom_point(aes(color=cut(temperature, c(-20,-10, 0,10,20,30,40))), stroke = 0, shape = 16) +
 theme_minimal() + 
  ggtitle(paste('Temperature in USA in month',Month,"of year", Year1))

templot2<- (data_year_2 %>% 
              group_by(.dots=c("lat", "lon"))) %>%
  # group_by(.dots=c("lat", "lon")) %>% mutate(TEMPyMean=mean(data_year)) %>% 
  arrange(desc(temperature)) %>% distinct(lat, lon, temperature) %>%
  ggplot(aes(x=lon, y=lat)) +
  geom_point(shape=15, size=2.5)+ 
  geom_point(aes(color=cut(temperature, c(-20,-10, 0,10,20,30,40))), stroke = 0, shape = 16) +
  theme_minimal() + 
  ggtitle(paste('Temperature in USA in month',Month,"of year", Year2))

templot1
templot2



