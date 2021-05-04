#Linear and nonlinear association between incidence rate and urbanization
rm(list=ls())
path<-"C:/Users/ZEF/Desktop/Related_ms2/Data"
setwd(path)
data<-read.csv("Epi_data2.csv")

library(tidyverse)
Med_cs<- setNames(aggregate (data$IncR, list(data$Region,data$District), median, na.rm = T), c("Region", "District", "Average_Inc"))


Med_cs<- setNames(aggregate (data$IncR, list(data$Region), median, na.rm = T), c("Region", "Med_Inc"))
Av_cs<- setNames(aggregate (data$IncR, list(data$Region), mean, na.rm = T), c("Region", "Mean_Inc"))
sd_cs<- setNames(aggregate (data$IncR, list(data$Region), sd, na.rm = T), c("Region", "sd_Inc"))
vn<-cbind(Med_cs, Av_cs, sd_cs)
vd<- merge(x = Med_cs,y = Av_cs, by.x = "Region", by.y = "Region")
vs<- merge(x = vd,y = sd_cs, by.x = "Region", by.y = "Region")
write.table(vs, "C:/Users/ZEF/Desktop/Related_ms2/Data/descriptive.csv",sep=",")

#dsn <- "C:/Users/ZEF/Desktop/Related_ms2/Data/Demographic Data/Joining Old Shapefile with New Shapefile"
require(rgdal)
#pshp <- readOGR(dsn=dsn,layer = "Map_of_Districts_216_census_points")
pshp <- readOGR("C:/Users/ZEF/Desktop/Related_ms2/Data/Demographic Data/Joining Old Shapefile with New Shapefile","Map_of_Districts_216_census_points")

#pshp<-readOGR('C:/Users/ZEF/Desktop/Related_ms2/Data/District Shapefile/Districts','Map_of_Districts_216')
#View(pshp@data)
pshp@data$NAME[pshp@data$NAME=="Accra Metropolis"]<- "Accra Metro"
pshp@data$NAME[pshp@data$NAME=="Adenta"]<-"Adentan"
pshp@data$NAME[pshp@data$NAME=="Afadzato South"]<-"Afadjato South"
pshp@data$NAME[pshp@data$NAME=="Afigya Kwabre"]<-"Afigya-Kwabre"
pshp@data$NAME[pshp@data$NAME=="Kwahu Afram Plains South"]<-"Afram Plains South"
pshp@data$NAME[pshp@data$NAME=="Agotime Ziope"]<-"Agortime-Ziope"
pshp@data$NAME[pshp@data$NAME=="Ahafo Ano North"]<-"Ahafo-Ano North"
pshp@data$NAME[pshp@data$NAME=="Ahafo Ano South"]<-"Ahafo-Ano South"
pshp@data$NAME[pshp@data$NAME=="Ajumako-Enyan-Esiam"]<-"Ajumako-Enyan-Essiam"
pshp@data$NAME[pshp@data$NAME=="Akwapem North"]<-"Akwapim North"
pshp@data$NAME[pshp@data$NAME=="Akwapem South"]<-"Akwapim South"
pshp@data$NAME[pshp@data$NAME=="Akyem Mansa"]<-"Akyemansa"
pshp@data$NAME[pshp@data$NAME=="Asante Akim Central Municipal"]<-"Asante Akim Central" 
pshp@data$NAME[pshp@data$NAME=="Asokore Mampong Municipal"]<-"Asokore-Mampong" 
pshp@data$NAME[pshp@data$NAME=="Atebubu Amantin"]<-"Atebubu-Amanten" 
pshp@data$NAME[pshp@data$NAME=="Atwima Kwanwoma"]<-"Atwima-Kwanwoma" 
pshp@data$NAME[pshp@data$NAME=="Awutu Senya East Municipal"]<-"Awutu Senya East"
pshp@data$NAME[pshp@data$NAME=="Bawku Municipal"]<-"Bawku"
pshp@data$NAME[pshp@data$NAME=="Bekwai Municipal"]<-"Bekwai"
pshp@data$NAME[pshp@data$NAME=="Birim Municipal" ]<-"Birim Central"
pshp@data$NAME[pshp@data$NAME=="Bolgatanga Municipal" ]<- "Bolgatanga"
pshp@data$NAME[pshp@data$NAME=="Bosomtwe /Atwima-Kwanwoma" ]<- "Bosomtwe"
pshp@data$NAME[pshp@data$NAME=="Cape Coast Metro" ]<- "Cape Coast"
pshp@data$NAME[pshp@data$NAME=="Bunkpurugu Yonyo" ]<-  "Bunkpurugu-Yunyoo"
pshp@data$NAME[pshp@data$NAME=="Asante-Mampong" ]<-  "Mampong Municipal"
pshp@data$NAME[pshp@data$NAME=="Effutu" ]<-  "Efutu"
pshp@data$NAME[pshp@data$NAME=="Ejisu Juaben"]<-  "Ejisu-Juaben"
pshp@data$NAME[pshp@data$NAME=="Ejura Sekye Dumase"]<- "Ejura-Sekyedumase"
pshp@data$NAME[pshp@data$NAME=="Ga Central Municipal" ]<- "Ga Central" 
pshp@data$NAME[pshp@data$NAME=="Garu Tempane" ]<- "Garu-Tempane" 
pshp@data$NAME[pshp@data$NAME=="Gonja Central"]<- "Central Gonja"
pshp@data$NAME[pshp@data$NAME=="Ho Municipal"]<- "Ho"
pshp@data$NAME[pshp@data$NAME=="Hohoe Municipal"]<- "Hohoe"
pshp@data$NAME[pshp@data$NAME=="Juabeso"]<- "Juaboso"
pshp@data$NAME[pshp@data$NAME=="Kasena Nankana East"]<- "Kasena-Nankana"
pshp@data$NAME[pshp@data$NAME=="Kasena Nankana West"]<- "Kasena-Nankana West"
pshp@data$NAME[pshp@data$NAME=="Keta Municipal"]<- "Keta"
pshp@data$NAME[pshp@data$NAME== "Komenda Edna Eguafo-Abirem"]<- "Komenda-Edna-Eguafo-Abirem"
pshp@data$NAME[pshp@data$NAME=="Kpando Municipal"]<- "Kpando"
pshp@data$NAME[pshp@data$NAME=="Kpone Katamanso"]<- "Kpone-Katamanso"
pshp@data$NAME[pshp@data$NAME=="Kumbumgu"]<- "Kumbungu"
pshp@data$NAME[pshp@data$NAME=="Kwabre"]<- "Kwabre East"
pshp@data$NAME[pshp@data$NAME=="Kwahu Afram Plains North"]<- "Kwahu North"
pshp@data$NAME[pshp@data$NAME=="La Dade Kotopon"]<- "La-Dade-Kotopon"
pshp@data$NAME[pshp@data$NAME=="La Nkwantanang Madina"]<- "La-Nkwantanang-Madina"
pshp@data$NAME[pshp@data$NAME=="Lambussie Karni"]<- "Lambussie-Karni"
pshp@data$NAME[pshp@data$NAME=="Lower Manya"]<- "Lower-Manya Krobo"
pshp@data$NAME[pshp@data$NAME=="Mampong Municipal"]<- "Asante-Mampong"
pshp@data$NAME[pshp@data$NAME=="Mamprugu Moagduri"]<- "Mamprugu-Moagduri"
pshp@data$NAME[pshp@data$NAME=="New Juaben Municipal"]<- "New Juaben"
pshp@data$NAME[pshp@data$NAME=="Nsawam Adoagyiri"]<- "Nsawam-Adoagyiri"
pshp@data$NAME[pshp@data$NAME=="Obuasi, Ashanti"]<- "Obuasi"
pshp@data$NAME[pshp@data$NAME=="Offinso Municipal"]<- "Offinso"
pshp@data$NAME[pshp@data$NAME=="Sagnerigu"]<- "Sagnarigu"
pshp@data$NAME[pshp@data$NAME=="Savelugu Nanton"]<- "Savelugu-Nanton"
pshp@data$NAME[pshp@data$NAME=="Sawla/Tuna/Kalba"]<- "Sawla-Tuna-Kalba" 
pshp@data$NAME[pshp@data$NAME=="Sefwi Akontombra"]<- "Sefwi-Akontombra"
pshp@data$NAME[pshp@data$NAME=="Sefwi Bibiani-Anhwiaso Bekwai"]<- "Bibiani-Anhwiaso-Bekwai"
pshp@data$NAME[pshp@data$NAME=="Sekondi Takoradi"]<- "Sekondi-Takoradi"
pshp@data$NAME[pshp@data$NAME== "Shai Osu Doku"]<- "Shai-Osudoku"
pshp@data$NAME[pshp@data$NAME=="Sissala  West"]<- "Sissala West"
pshp@data$NAME[pshp@data$NAME== "Suhum Municipal"]<- "Suhum"
pshp@data$NAME[pshp@data$NAME== "Tarkwa Nsuaem"]<- "Tarkwa-Nsuaem"
pshp@data$NAME[pshp@data$NAME== "Mamprusi East"]<- "East Mamprusi"
pshp@data$NAME[pshp@data$NAME== "Tamale North Sub Metro"]<- "Tamale"
pshp@data$NAME[pshp@data$NAME== "Tatale"]<- "Tatale-Sangule"
pshp@data$NAME[pshp@data$NAME== "Tema Metropolis"]<- "Tema"
pshp@data$NAME[pshp@data$NAME== "Twifo Ati-Morkwa"]<- "Twifo-Ati-Mokwa"
pshp@data$NAME[pshp@data$NAME== "Twifo Lower Denkyira"]<- "Twifo-Hemang Lower Denkyira"
pshp@data$NAME[pshp@data$NAME== "Suhum Municipal"]<- "Suhum"
pshp@data$NAME[pshp@data$NAME== "Upper Manya"]<- "Upper Manya-Krobo"
pshp@data$NAME[pshp@data$NAME== "Wa Municipal"]<- "Wa"
pshp@data$NAME[pshp@data$NAME== "Wassa Amenfi Central"]<- "Wassa-Amenfi Central"
pshp@data$NAME[pshp@data$NAME== "Yendi Municipal"]<- "Yendi"
pshp@data$NAME[pshp@data$NAME== "Yilo Krobo"]<- "Yilo-Krobo"
pshp@data$NAME[pshp@data$NAME== "Daffiama Bussie"]<- "Daffiama-Bussie-Issa"
pshp@data$NAME[pshp@data$NAME== "Kma"]<- "Kumasi Metro"
pshp@data$NAME[pshp@data$NAME== "Sekyere Afram Plains North"]<- "Sekyere Kumawu"
pshp@data$NAME[pshp@data$NAME== "Afigya Sekyere"]<- "Sekyere South"

which (unique(sort(pshp@data$NAME))!=unique(sort(Med_cs$District)))
setdiff(sort(unique(pshp@data$NAME)), unique(Med_cs$District))
setdiff(unique(Med_cs$District), unique(pshp@data$NAME))

pshp@data$NAME[pshp@data$NAME== "Abura / Asebu / Kwamankese"]<- "Abura-Asebu-Kwamankese" 
pshp@data$NAME[pshp@data$NAME== "Asikuma / Odoben / Brakwa"]<- "Asikuma-Odoben-Brakwa" 
pshp@data$NAME[pshp@data$NAME== "Bosomtwe /Atwima / Kwanwoma"]<- "Bosomtwe" 
pshp@data$NAME[pshp@data$NAME== "Komenda Edna Eguafo / Abirem"]<- "Komenda-Edna-Eguafo-Abirem" 
pshp@data$NAME[pshp@data$NAME== "Ledzokuku / Krowor"]<- "Ledzokuku-Krowor" 
pshp@data$NAME[pshp@data$NAME== "Prestea / Huni Valley"]<- "Prestea-Huni Valley" 
pshp@data$NAME[is.na(pshp@data$NAME)] <- "Obuasi"


pshp <- merge(x=pshp,y=Med_cs,by.x="NAME",by.y="District")
#plot(pshp@data$Average_Inc,ylab="Total Incidence")

ggplot(data=pshp@data, aes(x=Urbanizati, y=Average_Inc))+
  geom_point(aes(colour= Region), size = 1, alpha = .8)+
  guides (col = guide_legend(nrow = 10)) +
  geom_smooth()+
  labs(title = "Malaria incidence and urbanization",
       #subtitle = paste("", format(max(dt$date2), "%A, %B %e, %Y")),
        x = "Urbanization rate", y = "Mediane Incidence rate", 
       caption = "Data: DHIMS 2019| Graph: @koissi Savi") + 
  theme_minimal()

#library(ggpubr)
#MeaN_cs<-merge(x=MeaN_cs,y=pshp,by.x="District",by.y="NAME")
#ggscatter(MeaN_cs, x = MeaN_cs$Urbanizati, y = MeaN_cs$Average_Inc,na.rm=T,merge = F,
 #         add = "reg.line", conf.int = TRUE,
  #        cor.coef = TRUE, cor.method = "pearson",  xlab ="Urbanization rate", ylab="Incidence rate",
   #       caption = "Data: DHIMS 2019| Graph: @koissi Savi") 

modl<-lm(data=pshp@data, Average_Inc~Urbanizati)
summary(modl)

#correlations
require(psych)
print(corr.test(pshp@data$Average_Inc,pshp@data$Urbanizati),short=FALSE)
corr.test(pshp@data$Average_Inc,pshp@data$Urbanizati)
corr.test(pshp@data$Average_Inc,pshp@data$BuiltInten)
corr.test(pshp@data$Average_Inc,pshp@data$BuiltAr)

#Nonlinear relationship between urbanization and the incidence rate 

require(NNS)
v1<-NNS.dep(x=pshp@data$Average_Inc, y = pshp@data$Urbanizati,  print.map = TRUE)
y_p1<-replicate(1000, sample.int(length(pshp@data$Urbanizati)))# Create a permutation
nns.mc <- apply(y_p1, 2, function(g) NNS.dep(x=pshp@data$Average_Inc, pshp@data$Urbanizati[g]))# generate a new correlation measure
## Store results
cors <- unlist(lapply(nns.mc, "[[", 1))
deps <- unlist(lapply(nns.mc, "[[", 2))
## View results
hist(cors)
hist(deps)

## Left tailed correlation p-value
cor_p_value <- LPM(0, v1$Correlation, cors)
cor_p_value

## Right tailed correlation p-value
cor_p_value <- UPM(0, v1$Correlation, cors)
cor_p_value

## Confidence Intervals
## For 95th percentile VaR (both-tails) see [LPM.VaR] and [UPM.VaR]
## Lower CI
LPM.VaR(.975, 0, cors)
## Upper CI
UPM.VaR(.975, 0, cors)

v2<-NNS.dep(x=pshp@data$Average_Inc, y = pshp@data$BuiltInten,  print.map = TRUE)
y_p2<-replicate(1000, sample.int(length( pshp@data$BuiltInte)))# Create a permutation
nns.mc2 <- apply(y_p2, 2, function(g) NNS.dep(x=pshp@data$Average_Inc,  pshp@data$BuiltInte[g]))# generate a new correlation measure
## Store results
cors2 <- unlist(lapply(nns.mc2, "[[", 1))
deps2 <- unlist(lapply(nns.mc2, "[[", 2))
## View results
hist(cors2)
hist(deps2)

## Left tailed correlation p-value
cor_p_value2 <- LPM(0, v2$Correlation, cors)
cor_p_value2

## Right tailed correlation p-value
cor_p_value2 <- UPM(0, v2$Correlation, cors)
cor_p_value2

## Confidence Intervals
## For 95th percentile VaR (both-tails) see [LPM.VaR] and [UPM.VaR]
## Lower CI
LPM.VaR(.975, 0, cors2)
## Upper CI
UPM.VaR(.975, 0, cors2)

v3<-NNS.dep(x=pshp@data$Average_Inc, y = pshp@data$BuiltAr,  print.map = TRUE)
y_p3<-replicate(1000, sample.int(length(pshp@data$BuiltAr)))# Create a permutation
nns.mc3 <- apply(y_p3, 2, function(g) NNS.dep(x=pshp@data$Average_Inc,  pshp@data$BuiltAr[g]))# generate a new correlation measure
## Store results
cors3 <- unlist(lapply(nns.mc3, "[[", 1))
deps3 <- unlist(lapply(nns.mc3, "[[", 2))
## View results
hist(cors2)
hist(deps2)

## Left tailed correlation p-value
cor_p_value3 <- LPM(0, v$Correlation, cors)
cor_p_value3

## Right tailed correlation p-value
cor_p_value3 <- UPM(0, v3$Correlation, cors)
cor_p_value3

## Confidence Intervals
## For 95th percentile VaR (both-tails) see [LPM.VaR] and [UPM.VaR]
## Lower CI
LPM.VaR(.975, 0, cors3)
## Upper CI
UPM.VaR(.975, 0, cors3)


install.packages("devtools")
library(devtools)
install_github("ProcessMiner/nlcor",force = TRUE)
library(nlcor)
cor1<-nlcor(pshp@data$Average_Inc,pshp@data$Urbanizati)
cor2<-nlcor(pshp@data$Average_Inc,pshp@data$BuiltInten)
cor3<-nlcor(pshp@data$Average_Inc,pshp@data$BuiltAr, plt=TRUE)

