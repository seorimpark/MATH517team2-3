
library(ggplot2)
library(tidyr)
library(dplyr)
library(corrplot)
library(maps)

# Read data
load("../Data/data_train_DF.RData")
data = data_train_DF

# Explore data
dim(data)
names(data)

# Remove NA values from CNT and BA
data <- data[!is.na(data$CNT),]
data <- data[!is.na(data$BA),]

# Remove columns not related to weather
remove = c(paste("lc", seq(1:18), sep = ""), "altiMean", "altiSD")
data = data[, -which(names(data) %in% remove)]

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

# Parameters for sub-setting
lim_CNT = 60
lim_BA = 5000

# Subset and check
selection = data[data$CNT >= lim_CNT & data$BA >= lim_BA,]
dim(selection)
names(selection)
range(selection$year)
range(selection$month)

# Correlations
sel = cor(selection)

# Plot
dev.new(width=5, height=4)
corrplot(sel, method = "color", addCoef.col="black", tl.cex = 0.8, number.digits = 2, 
         number.cex = 0.6)

############# map #############

# Finding considered areas
loc = unique( selection[, c("lon", "lat", "area")] )
us <- map_data('state')

# Plot areas
ggplot() + 
  geom_map(aes(map_id=region), fill="white", color="black", size=0.15, map=us, data=us) +
  expand_limits(x = us$long, y = us$lat) +
  geom_point(data=loc, aes(x=lon, y=lat, size=area*2.7), colour="red", shape=15) +
  scale_size_identity() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        plot.title = element_text(size=20, hjust=0.5, vjust=2)) + 
  ggtitle('Areas considered in the subset') + 
  coord_fixed(1.3)
  

