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
library(psych)
library(bookdown)
library(quantreg)
library(latex2exp)
library(cowplot)
library(reshape2)
library(stargazer)
library(knitr)
library(stargazer)
library(caret)
#Step 1 : load and process the data :
load ("../Data/data_train_DF.RData")
df <- data_train_DF
#Remove the NA values from the data
train<-df[!is.na(df$CNT),] 
train<-df[!is.na(df$BA),]
#Step 2 : Cross validation : 
set.seed(123)
#Creating training data as 70% of the dataset
random_sample <- createDataPartition (na.omit(train$CNT), p = 0.7, list = FALSE)
#Genrating training dataset form the random_sample
training <-train [random_sample,]
#Generating testing dataset from rows which are not in the training
testing <- train [-random_sample,]

#Step 3 : Build the model: 
model <- zeroinfl(CNT~ lc1+lc2+lc3+lc4+lc5+lc6+lc7+lc8+lc9+lc10+
                    lc11+lc12+lc13+lc14+lc15+lc16+lc17+lc18+ 
                    altiMean+ altiSD, data = training)
#Features selection : 
step (model, direction = "backward")
#Backward selection takes too much time to run 

#Prediction the test variable to check if our model is good
prediction <- predict(model, testing)
prediction
data.frame(R2 = R2(prediction, testing$CNT,na.rm = TRUE),
           RMSE = rmse(prediction, testing$CNT, na.rm=TRUE),
           MAE = mae(prediction, testing$CNT,na.rm =TRUE))
#The R2 is really bad and since we didn't manage to do features selection on the training set we are going to take an another small training set to run features selection : 
sp = sample (nrow(training), 10000)
train = training[sp,]
select <- zeroinfl(CNT~ lc1+lc2+lc3+lc4+lc5+lc6+lc7+lc8+lc9+lc10+
                   lc11+lc12+lc13+lc14+lc15+lc16+lc17+lc18+
                   clim1+clim2+clim3+clim4+ clim5+ clim6+ clim7+clim8+ clim9 +
                   clim10+ altiMean+ altiSD, data = train)
step(select, direction = "backward")
#removes only clim4
model <- zeroinfl(CNT~ lc1+lc2+lc3+lc4+lc5+lc6+lc7+lc8+lc9+lc10+
                    lc11+lc12+lc13+lc14+lc15+lc16+lc17+lc18+
                    clim1+clim2+clim3+ clim5+ clim6+ clim7+clim8+clim9+
                    clim10+ altiMean+ altiSD, data = training)
prediction <- predict(model, testing)
prediction
data.frame(R2 = R2(prediction, testing$CNT,na.rm = TRUE),
           RMSE = rmse(prediction, testing$CNT, na.rm=TRUE),
           MAE = mae(prediction, testing$CNT,na.rm =TRUE))
#Not big difference 

#Build a model for BA values : 
training$BA = as.integer(training$BA)
modelB <- zeroinfl(BA~CNT+lc1+lc2+lc3+lc4+lc5+lc6+lc7+lc8+lc9+lc10+
                     lc11+lc12+lc13+lc14+lc15+lc16+lc17+lc18+
                     clim1+clim2+clim3+ clim5+ clim6+ clim7+clim8+clim9+
                     clim10+ altiMean+ altiSD, data = training)
                   
prediction1 <-predict(modelB, testing)

data.frame(R2= R2(prediction1, testing$BA, na.rm = TRUE),
           RMSE = rmse(prediction1, testing$BA, na.rm = TRUE),
           MAE = mae(prediction1, testing$BA, na.rm = TRUE))
#Final goal : compute the missing the 
goalBA<- df[is.na(df$BA),]
nrow(df)
goalCNT<- df[is.na(df$CNT),]
nrow(goalCNT)
goalCNT$CNT <- predict()
goal






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
#takes too much time 
train2015 <- subset(df, year ==2015)
m2015<-zeroinfl(CNT~lc1+lc2+lc3+lc4+lc5+lc6+lc7+lc8+lc9+lc10+
               lc11+lc12+lc13+lc14+lc15+lc16+lc17+lc18+
               clim1+clim2+clim3+clim4+ clim5+ clim6+ clim7+clim8+
               clim10+ altiMean+ altiSD, data = train)
step(m2015, direction = "backward")
#do that for each year : 
m2014 <-subset(df, year ==2014)
m2014 <-zeroinfl(CNT~lc1+lc2+lc3+lc4+lc5+lc6+lc7+lc9+lc10+
                   lc11+lc12+lc13+lc14+lc15+lc16+lc17+lc18+
                   clim1+clim2+l)
train <- subset(df, year != 2015)

m1<-zeroinfl(CNT~lc1+lc2+lc3+lc4+lc5+lc6+lc7+lc8+lc9+lc10+lc11+lc12+lc13+lc14+lc15+lc16+lc17+lc18+, data = train)
#check if the actual model is better to a null null mode without predictors : 
mnull <- update(m1, .~1)
pchisq (2* (logLik(m1)-logLik(mnull)), df =18, lower.tail = FALSE)











#When trying to construct the model we have some trouble to run it because it takes so much time : 
train<-df[!is.na(df$CNT),] 
mLC <-zeroinfl(CNT~lc1+lc2+lc3+lc4+lc5+lc6+lc7+lc8+lc9+lc10+lc11+lc12+lc13
               +lc14+lc15+lc16+lc17+lc18, data = train ) 
summary(mLC)
step (mLC, direction = "backward")
 
mMETEO<- zeroinfl(CNT~clim1+ clim2+ clim3+ clim4+clim5+clim6+clim7+
                    clim8+clim9+clim10+altiMean+altiSD, data = train)
step (mMETEO, direction = "backward")












