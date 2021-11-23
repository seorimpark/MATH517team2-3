library(ggplot2)
library(readr)
library(tidyr)
library(dplyr)
library(corrplot)
library(ggcorrplot)
library(maps)
library(ggpubr)
library(pscl)
library(devtools)
## Read, understand and clean the Data : 



load("/Users/selimajaoua/Documents/GitHub/MATH517team2-2/Data/data_train_DF.RData")
df <- data_train_DF
df
colnames(df)
dim(df)
#Remove the NA values from the data
df<-df[!is.na(df$CNT),] 
df<-df[!is.na(df$BA),]
dim(df)
#we have 37 features 
## First :  land covers and meterological data correlation 
##Want to show that the meterological data have an impact on the land cover
remove = c("CNT","BA","lon","lat","area","year","month","altiMean","altiSD")
data =df[, -which(names(df)%in% remove)]
data

#library(FactoMineR)
#pca = PCA(data[,c(1:18)],scale.unit=T,graph = F)
#plot(pca, choix="var")
data<-data%>% dplyr::rename(CR=lc1,CRherb = lc2, MosaicC = lc3, MosaicVege = lc4,
                            TBE=lc5,TBD = lc6, TNE  = lc7, TND = lc8,
                            TreeMixed =lc9, MosaicTreeShrub= lc10, Shrub= lc11,
                            Grass = lc12, Svege = lc13,TreeFlood=lc14,ShrubFlood = lc15,
                            urban = lc16, BareAreas = lc17, Water = lc18,
                            NSwind=clim1, WEwind=clim2, 
                            dew_temperature=clim3, temperature=clim4, 
                            potential_evaporation=clim5, solar_radiation= clim6, 
                            thermal_radiation=clim7, pressure=clim8, evaporation=clim9, 
                            precipitation=clim10)

data
#  pairs(data)
FindCorForLC <- function (x,methode,threshold){
  #Want to show if y has an impact on x
  X = data[,x]
  correlations <- 0
  for (y in 1:10){
    Y = data[,18+y]
    #plot(X,Y,method = methode,las =1, xlabel = colnames(X),ylabel=colnames(Y) )
    c = cor(X,Y,method=methode)
    print(c)
    if (c > threshold){
      correlations = c(correlations, y)
    }
  }
  return(correlations)
}

corr1 = FindCorForLC(1,"pearson",0.6)
corr2 = FindCorForLC(2,"pearson",0.6)
corr3 = FindCorForLC(3,"pearson",0.6)
corr4 =FindCorForLC(4,"pearson",0.6)
corr5 =FindCorForLC(5,"pearson",0.6)
corr6 =FindCorForLC(6,"pearson",0.6)
corr7 =FindCorForLC(7,"pearson",0.6)
corr8 =FindCorForLC(8,"pearson",0.6)
corr9 =FindCorForLC(9,"pearson",0.6)
corr10 =FindCorForLC(10,"pearson",0.6)
corr11 =FindCorForLC(11,"pearson",0.6)
corr12 =FindCorForLC(12,"pearson",0.6)
corr13 =FindCorForLC(13,"pearson",0.6)
corr14 =FindCorForLC(14,"pearson",0.6)
corr15 =FindCorForLC(15,"pearson",0.6)
corr16 =FindCorForLC(16,"pearson",0.6)
corr17 =FindCorForLC(17,"pearson",0.6)
corr18 =FindCorForLC(18,"pearson",0.6)

#Remark : we do this because the matrix of correlation is too big to be computed by R
#we get that none of the variables are highly correlated between each other
#Now, notice that we want to compare two sets of variable and not each pair. To do so, we are goind to compute MANOVA
#Landcover <- data[,c(1:18)]
#M-eteo <- data[,c(19:28)]
#cca <- cancor(Landcover, Meteo)
#Highest correlation between pairs of linear combinations
#cca[["cor"]][1]
#Corresponding linear combination of the Land Cover Data
#cca[["xcoef"]][,1]
#cc_LC = as.matrix(Landcover)%*% cca[["xcoef"]][,1]
#Corresponding linear combination of the Meteorological Data
#cca[["ycoef"]][,1]
#cc_M  = as.matrix(Meteo)%*% cca[["ycoef"]][,1]
#the hightest correlation is 0.88..
#cca_data <- data%>%
 # mutate(cc1_lc = cc_LC,
 #         cc1_m =cc_M)
#cca_data%>%
 # ggplot(aes(x=cc1_lc , y= cc1_m))+
 # geom_point()
#cca_data <- cca_data%>%
#  mutate(cc1_lc = cc_LC,
#         cc1_m =cc_M,
#         year = df$year)
#cca_data%>%
#  ggplot(aes(x = year,y = cc1_lc,color = year))+
#  geom_boxplot(width = 0.5)+
#  geom_jitter(width =  0.15)+
#  theme(legend.position = "none")

#The lack of correlation that we observe in the first part can come from the fact that the corrolation can be non linear. 
CorMatrix <- function (LandCovers,MeteoDatas){
  X  = data[,LandCovers]
  Y  = data[,MeteoDatas]
  corr = cor(X,Y, method = "spearman")
  print(corr)
  corrplot(corr, method = "color")
}
corr1 = CorMatrix(1,c(1:10))
#To make the matrix of correlation small we are going to reorganise our variables
dataMod <- data

Rainfed = dataMod$CR +dataMod$CRherd
dataMod <- cbind(dataMod, t(Rainfed))
remove = c("CR","CRherb")
dataMod = dataMod[,-which(names(data)%in% remove)]

dataMod<- dataMod%>%
  add_column(Trees = t(dataMod$TBE +dataMod$TBD +dataMod$TNE + dataMod$TND+ dataMod$TreeMixed ))
remove = c("TBE", "TBD","TNE","TND","TreeMixed")
dataMod =df[, -which(names(df)%in% remove)]

dataMod<-dataMod%>%
  add_column(vegetation = t(dataMod$MosaicVege + dataMod$Svege))
remove = c("MosaicVege","Svege")
dataMod = df[, -which(names(df)%in% remove)]

dataMod<- dataMod%>% 
  add_column(FloadArea <-t(dataMod$TreeFlood + dataMod$ShrubFlood))
remove = c("TreeFlood","ShrubFlood")
dataMod = df[, -which(names(df)%in% remove)]

Matrix = CorMatrix()
 ##Now let's compute the probability that a neighboring area can spread wildfire based on the type of land cover
#(along with mf as wel)
hist = hist(df$CNT,breaks=seq(-0.5,max(df$CNT)+0.5,1))
check <- df%>% filter(CNT >100)
nrow(check)#we get 121 outliers. We are not going to ignore them but to see the distrubition 
df0 <-df%>% filter(CNT <100)
hist = hist(df0$CNT,breaks = seq(-0.5,max(df0$CNT)+0.5,1))
df1 <-df%>% filter(CNT<20)
hist = hist(df1$CNT, breaks = seq(-0.5, max(df1$CNT)+0.5, 1))
#1
ggplot ()+
  geom_point(data = df, aes(x = lc1, y= CNT))
#2
ggplot ()+
  geom_point(data = df, aes(x = lc2, y= CNT))
#3
ggplot ()+
  geom_point(data = df, aes(x = lc3, y= CNT))
#4
ggplot ()+
  geom_point(data = df, aes(x = lc4, y= CNT))
#5
ggplot ()+
  geom_point(data = df, aes(x = lc5, y= CNT))
#6
ggplot ()+
  geom_point(data = df, aes(x = lc6, y= CNT))
#7
ggplot ()+
  geom_point(data = df, aes(x = lc7, y= CNT))
#8
ggplot ()+
  geom_point(data = df, aes(x = lc8, y= CNT))
#9
ggplot ()+
  geom_point(data = df, aes(x = lc9, y= CNT))
#10
ggplot ()+
  geom_point(data = df, aes(x = lc10, y= CNT))
#11
ggplot ()+
  geom_point(data = df, aes(x = lc11, y= CNT))
#12
ggplot ()+
  geom_point(data = df, aes(x = lc12, y= CNT))
#13
ggplot ()+
  geom_point(data = df, aes(x = lc13, y= CNT))
#14
ggplot ()+
  geom_point(data = df, aes(x = lc14, y= CNT))
#15
ggplot ()+
  geom_point(data = df, aes(x = lc15, y= CNT))
#16
ggplot ()+
  geom_point(data = df, aes(x = lc16, y= CNT))
#17
ggplot ()+
  geom_point(data = df, aes(x = lc17, y= CNT))
#18
ggplot ()+
  geom_point(data = df, aes(x = lc18, y= CNT))

data1 <- within(df,{
  n
})
summary(df)

summary(m1<-zeroinfl(CNT~lc1+lc2+lc3+lc4+lc5+lc6+lc7+lc8+lc9+lc10+lc11+lc12+lc13+lc14+lc15+lc16+lc17+lc18, data = df))




neighboors <- function (df, lon1, lat1,area){
  n1 = c(lon1,lat1+0.5)
  n2 = c(lon1,lat1-0.5)
  n3 = c(lon1+0.5, lat1)
  n4 = c(lon1-0.5, lat1)
  neighboors = c()
  if(area ==1 ){
    neighboors <-rbind(n1,n2)
    neighboors <- rbind(neighboors,n3)
    neighboors<- rbind(neighboors,n4)
    return(neighboors)
  }
  if(area< 1){
    values1 <-df%>% filter(lat ==lat1+0.5)%>% filter(lon==lon1)
    values2 <-df%>% filter(lat ==lat1-0.5)%>% filter(lon==lon1)
    values3 <-df%>% filter(lat ==lat1)%>% filter(lon==lon1+0.5)
    values4 <-df%>% filter(lat ==lat1)%>% filter(lon==lon1-0.5)
    if(nrow(values1)>0){neighboors<-rbind(neighboors,n1)}
    if(nrow(values2)>0){neighboors<-rbind(neighboors,n2)}
    if(nrow(values3)>0){neighboors<-rbind(neighboors,n3)}
    if(nrow(values4)>0){neighboors<-rbind(neighboors,n4)}
    return(neighboors)
  }
}
#try function 
neighboors(df,-95.25,49.25,0.24) 
neighboors(df, -122.75, 48.75,1)

#Zero-inflated Poisson regression 
df <- within(df,{
  year <-factor(year)
  month <-factor(month)
})
summary(df)
summary(m1<- zeroinfl(formula = CNT~ lc1+lc2+lc3+lc4+lc5+lc6+lc7+lc8+lc9+lc10+lc11+lc12+lc13+lc14+lc15+lc16+lc17+lc18,data = df))
