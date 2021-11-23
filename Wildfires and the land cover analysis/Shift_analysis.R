library(tidyverse)
library(Hmisc)
library(zoo)
library(ggplot2)
library(hrbrthemes) 
library(corrplot)
library(dplyr)


load("../Data/data_train_DF.RData")

data = data_train_DF

filter_year<-function(data,y){
    data_filtered<-data %>% filter(year == y)
   return (data_filtered)
 }

#data93<-filter_year(data,1993)

# for( y in 1993:2015){
#   print(nrow(data %>% filter(year == y)))
# }

names<-1:18
df_major<-data93[ , which(names(data93) %in% c("lat","lon"))]
for( y in 1993:2015){
  df_temp<- data %>% 
  mutate(`Major land cover`=factor(max.col(data %>% select(starts_with("lc"))), labels = names[1:18 %in% max.col(data  %>% select(starts_with("lc")))])) %>% filter(year==y)
  #names(df_temp)[names(df_temp) == 'Major land cover'] <- y
  new_col_name<-paste("year",y)
  colnames(df_temp)[colnames(df_temp) == 'Major land cover'] <- new_col_name
  #colnames(df_temp$`Major land cover`)<-y
  df_major<-cbind(df_major,as.numeric(unlist(df_temp[38])))}


sapply(df_major, class)

N=nrow(df_major)
M=ncol(df_major)

NumberShifts <- rep(0, N)

#df_M<-data.frame(df_major)
for (i in 1:N){
  for (j in 3:M-1){
    if(df_major[i,j]!=df_major[i,j+1]){NumberShifts[i]=NumberShifts[i]+1}
  }
}

df_shifts<-cbind(data93[ , which(names(data93) %in% c("lat","lon"))],NumberShifts)

us <- map_data('state')
number_shifts<-1:5



p <- df_shifts %>% 
  #mutate(`Major land cover`=factor(max.col(data_all %>% select(starts_with("lc"))), labels = names[1:18 %in% max.col(data_all  %>% select(starts_with("lc")))])) %>% 
  #filter(year==Year) %>% 
  ggplot(aes(x=lon, y=lat, colour = as.factor(`NumberShifts`))) +
  geom_point(shape=15, size=2.4) +
  theme_minimal() + 
  ggtitle(paste('Number of landcover shift between 1993 and 2015 ')) + 
  #legend("Number of changes for the predominant landcover")+
  scale_color_manual(values = c( "wheat","orangered","sienna","gray0"))
options(repr.plot.width=10, repr.plot.height=5)
p
