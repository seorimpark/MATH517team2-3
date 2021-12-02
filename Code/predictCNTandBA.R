library(ggplot2)
library(tidyr)
library(dplyr)
library(corrplot)
library(maps)
library(tidyverse)
library(Hmisc)
library(zoo)
library(hrbrthemes) 
library(pscl)
library('plot.matrix')
library('psych')
library(bookdown)
library(quantreg)
library(latex2exp)
library(cowplot)
library(reshape2)
library(stargazer)
library(knitr)
library(stargazer)
load ("../Data/data_train_DF.RData")
df <- data_train_DF
#Remove the NA values from the data
train<-df[!is.na(df$CNT),] 
train<-df[!is.na(df$BA),]
test <- df[is.na(df$BA),]
test <- df[is.na(df$CNT),]
colnames(train)
m1<-zeroinfl(CNT~lc1+lc2+lc3+lc4+lc5+lc6+lc7+lc8+lc9+lc10+
               lc11+lc12+lc13+lc14+lc15+lc16+lc17+lc18+
               clim1+clim2+clim3+clim4+ clim5+ clim6+ clim7+clim8+
               clim10+ altiMean+ altiSD, data = train)
m2 <-zeroinfl(BA~lc1+lc2+lc3+lc4+lc5+lc6+lc7+lc8+lc9+lc10+
                lc11+lc12+lc13+lc14+lc15+lc16+lc17+lc18+
                clim1+clim2+clim3+clim4+ clim5+ clim6+ clim7+clim8+
                clim10+ altiMean+ altiSD, data = train)
#takes too much times to compute 
step(m1, direction = "backward")
#step(m2, direction = "backward", trace = FALSE)
#Need to think about something that makes it run faster 
#PREDICTION : 

#add the plot of the other file and we need to compute the accuracy of our model : 


#add the missing value 

#plot the distribution of the missing value 
