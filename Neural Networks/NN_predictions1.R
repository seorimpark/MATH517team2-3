
library(keras)
library(dplyr)

# Read data
load("../Data/data_train_DF.RData")
data = data_train_DF

# Explore data
dim(data)
names(data)

# Remove NA values from CNT and BA
data <- data[!is.na(data$CNT),]
data <- data[!is.na(data$BA),]

# Renaming weather columns
data<-data%>% dplyr::rename(NSwind=clim1, WEwind=clim2, dew_temperature=clim3, temperature=clim4, 
         potential_evaporation=clim5, solar_radiation= clim6, 
         thermal_radiation=clim7, pressure=clim8, evaporation=clim9, 
         precipitation=clim10)

# Wind into one component 
data <- data %>% mutate(Wspeed=(sqrt(NSwind^2+WEwind^2))) %>% select(-NSwind, -WEwind)

# Temperatures in Celsius
data$dew_temperature = data$dew_temperature - 273.15
data$temperature = data$temperature - 273.15

# Overview
names(data)
sapply(data, range)
head(data)

#####################################################

train = data[data$year!=2015,]
test = data[data$year==2015,]

xtrain = as.matrix( train[, !colnames(train) %in% c("CNT", "BA")] )
ytrain = as.matrix( train[, colnames(train) %in% c("CNT", "BA")] )

xtest = as.matrix( test[, !colnames(test) %in% c("CNT", "BA")] )
ytest = as.matrix( test[, colnames(test) %in% c("CNT", "BA")] )

# Input size and dimensions
input_size = dim(xtrain)[2]
output_size = dim(ytrain)[2]
c(input_size, output_size)
c(dim(xtrain), dim(ytrain), dim(xtest), dim(ytest))

# Defining model
model <- keras_model_sequential() %>%
  layer_dense(units = 64, use_bias = TRUE,  activation = 'relu', input_shape = input_size) %>% 
  layer_dense(units = 256, use_bias = TRUE, activation = 'relu') %>%
  layer_dense(units = 1024, use_bias = TRUE, activation = 'relu') %>%
  layer_dense(units = 512, use_bias = TRUE, activation = 'relu') %>%
  layer_dense(units = 128, use_bias = TRUE, activation = 'relu') %>%
  layer_dense(units = output_size, use_bias = TRUE, activation = 'linear')

summary(model)

# Loss function and optimizer
model %>% compile(
  loss = "mse",
  optimizer = "adam",
  metrics = list("mae")
)

# Training
history <- model %>% fit(xtrain, ytrain, epochs=25, batch_size=256, 
                       validation_data=list(xtest, ytest))

# Plot history
plot(history)

#####################################################

k = 10
temp = xtest
temp[,"temperature"] = xtest[,"temperature"] + k

range(xtest[,"temperature"])
range(temp[,"temperature"])

# Prediction
prediction1 = model %>% predict(xtest)
prediction2 = model %>% predict(temp)
unique(temp==temp)
unique(xtest==xtest)
unique(temp==xtest)

apply(prediction1, 2, FUN=range)
apply(prediction2, 2, FUN=range)
apply(data[data$year==2015, colnames(data) %in% c("CNT", "BA")], 2, FUN=range)

apply(prediction1, 2, FUN=sum)
apply(prediction2, 2, FUN=sum)
apply(data[data$year==2015, colnames(data) %in% c("CNT", "BA")], 2, FUN=sum)

#####################################################

loc = data[data$CNT>60,]
c(lon, lat) %<-% loc[sample(nrow(loc),1), c("lon","lat")]
loc = data[data$lon==lon & data$lat==lat,]
loc_init = loc[loc$year==2015, !colnames(loc) %in% c("CNT", "BA")]
loc_pred = loc_init

for (year in 2016:2050) {
  next_year = loc_init
  next_year$year = year
  loc_pred = rbind(loc_pred, next_year)
}

loc_pred = loc_pred[loc_pred$year!=2015,]
dim(loc_pred)

loc_pred[1:14,1:8]

# Prediction
prediction = model %>% predict(as.matrix(loc_pred))

max(prediction[1])

