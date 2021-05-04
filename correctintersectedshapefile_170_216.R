rm(list=ls())
require(rgdal)
require(cartography)
require(colorRamps)
require(spdep)
require(foreach)
require(easyR)
require(corrr)
require(rgeos)
require(corrplot)
require(plyr)
require(ineq)
library(RColorBrewer)
options(scipen=999)

# Read Data
data = read.csv("E:/Dropbox (Yale_FES)/Personal/Malaria Project/Data/Malaria Incidence Data/Malaria Confirmed Data Request_Koissi_170119.csv")

# Remove rows with Regional-level Data
data = data[-which(as.character(data$District) == as.character(data$Region)),]

# Create a variable that can be combined with the geocoding lookup table
data$geocode = paste(as.character(data$District),", ",as.character(data$Region),sep='')

# Read Geocoding lookup table and retain spatial ID and District-Region Name variables
geodata = read.csv('E:/Dropbox (Yale_FES)/Personal/Malaria Project/Data/Geocoding Data/dist_names_excl.csv')[,c(3,2)]

# Merge geocoding lookup table with the Data
geodata = merge(data,geodata,by.x="geocode",by.y="NAME")

# Calculate multiple indicidence variable by districts

# Total Incidence by districts between 2015 and 2018
TotalIncdnc = ddply(geodata,"SHP_ID",function(x)sum(as.numeric(colSums(x[,7:30],na.rm=T)),na.rm=T))
colnames(TotalIncdnc)[2] = "Total"

# Total Incidence by districts in 2015
TotalIncdnc_2015 = ddply(geodata,"SHP_ID",function(x)sum(as.numeric(colSums(x[which(x$Year==2015),7:30],na.rm=T)),na.rm=T))
colnames(TotalIncdnc_2015)[2] = "Total2015"

# Total Incidence by districts in 2016
TotalIncdnc_2016 = ddply(geodata,"SHP_ID",function(x)sum(as.numeric(colSums(x[which(x$Year==2016),7:30],na.rm=T)),na.rm=T))
colnames(TotalIncdnc_2016)[2] = "Total2016"

# Total Incidence by districts in 2017
TotalIncdnc_2017 = ddply(geodata,"SHP_ID",function(x)sum(as.numeric(colSums(x[which(x$Year==2017),7:30],na.rm=T)),na.rm=T))
colnames(TotalIncdnc_2017)[2] = "Total2017"

# Total Incidence by districts in 2018
TotalIncdnc_2018 = ddply(geodata,"SHP_ID",function(x)sum(as.numeric(colSums(x[which(x$Year==2018),7:30],na.rm=T)),na.rm=T))
colnames(TotalIncdnc_2018)[2] = "Total2018"

# Which Age Group has maximum incidence by district between 2015 and 2018
Agegrp_max = ddply(geodata,"SHP_ID",function(x)names(which.max(colSums(x[,7:30],na.rm=T))))
Agegrp_max[,2] = unlist(strsplit(Agegrp_max[,2],'Uncomplicated.Malaria.Tested.Positive'))
colnames(Agegrp_max)[2] = "AgegrpMaxInc"

# Which Age Group has minimum incidence by district between 2015 and 2018
Agegrp_min = ddply(geodata,"SHP_ID",function(x)names(which.min(colSums(x[,7:30],na.rm=T))))
Agegrp_min[,2] = unlist(strsplit(Agegrp_min[,2],'Uncomplicated.Malaria.Tested.Positive'))
colnames(Agegrp_min)[2] = "AgegrpMinInc"

# Which Age Group has maximum incidence by district between 2015 and 2018 regardless of sex
lt28days = rowSums(geodata[,c(7,19)],na.rm=T)
lt1year = rowSums(geodata[,c(8,20)],na.rm=T)
bw1_4 = rowSums(geodata[,c(9,21)],na.rm=T)
bw5_9 = rowSums(geodata[,c(10,22)],na.rm=T)
bw10_14 = rowSums(geodata[,c(11,23)],na.rm=T)
bw115_17 = rowSums(geodata[,c(12,24)],na.rm=T)
bw118_19 = rowSums(geodata[,c(13,25)],na.rm=T)
bw120_34 = rowSums(geodata[,c(14,26)],na.rm=T)
bw135_49 = rowSums(geodata[,c(15,27)],na.rm=T)
bw150_59 = rowSums(geodata[,c(16,28)],na.rm=T)
bw160_69 = rowSums(geodata[,c(17,29)],na.rm=T)
gt70 = rowSums(geodata[,c(18,30)],na.rm=T)

geodata_age = data.frame(geodata[,c(1:6,31)],lt28days,lt1year,bw1_4,bw5_9,bw10_14,bw115_17,bw118_19,bw120_34,bw135_49,bw150_59,bw160_69,gt70)
Agegrpnosex_max = ddply(geodata_age,"SHP_ID",function(x)names(which.max(colSums(x[,8:19],na.rm=T))))
colnames(Agegrpnosex_max)[2] = "AgegrpMaxInc_nosex"

# Which Age Group has minimum incidence by district between 2015 and 2018 regardless of sex

Agegrpnosex_min = ddply(geodata_age,"SHP_ID",function(x)names(which.min(colSums(x[,8:19],na.rm=T))))
colnames(Agegrpnosex_min)[2] = "AgegrpMinInc_nosex"

## Inequality in Distributions

Ent = ddply(geodata,"SHP_ID",function(x)entropy(colSums(x[,c(7:30)],na.rm=T),parameter=0.5,na.rm=T))

## Inequality in Distributions No Sex

Ent_nosex = ddply(geodata_age,"SHP_ID",function(x)entropy(colSums(x[,c(8:19)],na.rm=T),parameter=0.5,na.rm=T))



Agegrpnosex_sum = ddply(geodata_age,"SHP_ID",function(x)colSums(x[,8:19],na.rm=T))


## Plotting
# Read Shapefile

shp = readOGR('E:/Dropbox (Yale_FES)/Personal/Malaria Project/Data/Demographic Data','Ghana_Districts_170_216_census_built',verbose=FALSE)
shp = merge(shp,Agegrpnosex_sum,by.x="ID",by.y="SHP_ID")
colnames(shp@data)

shp$TotalIn = (rowSums(shp@data[,44:45],na.rm=T))
par(mfrow=c(1,2))
plot(shp$Urbanizati,shp$TotalIn,xlab="Urbanization",ylab="Total Incidence (2015-2018)",pch=20,cex=1.25)
est=round(cor(shp$Urbanizati,shp$TotalIn),2)
legend("topright",paste("r=",est,sep=""),bty="n")
plot(shp$Urbanizati,log(shp$bw120_34),xlab="Urbanization",ylab="Age 20-34 Incidence (2015-2018)",pch=20,cex=1.25)

est=round(cor(shp$Urbanizati,shp$bw120_34),2)
legend("topright",paste("r=",est,sep=""),bty="n")




