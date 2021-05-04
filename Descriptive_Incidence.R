rm(list=ls())
library(foreign)
dts<-read.dta("C:/Users/ZEF/Desktop/Related_ms2/Nice_Malaria_TV_DHS_2014.dta") # Read a stata file
View(dts)


rm(list=ls())
# computation of the incidence per thousand per population
path<-"C:/Users/ZEF/Desktop/Related_ms2/Data"
setwd(path)
data2<-read.csv("Epi_data2.csv")
path <- "C:/Users/ZEF/Desktop/MS3/WorlPop"
setwd(path)
data<-read.csv("Popu_15-18_gha.csv",sep=",", h=TRUE)
mydt_Y <- aggregate(Estimate ~ Year , data= data , FUN =sum )
summSta<-by(data2, data2$Region,summary)
write.table(summSta,"summSta.csv")
Inc <- rep(0,  nrow(data2))
for (i in 1:nrow(data2)) {
  for (j in 1:nrow(mydt_Y)) {
    if (data2$Year[i]  == mydt_Y$Year[j])
      Inc[i] <- round(data2$Clin_Caz[i] * 10000000/mydt_Y$Estimate[j], 10)
    
  }
  
}
data2$IncR<-Inc
head(data2, 5)

write.table(data2, "C:/Users/ZEF/Desktop/Related_ms2/Data/Epi_data2.csv",sep=",")




require(lubridate)
data2$days <- paste(data2$Month,"/", data2$Year)
data2$days<-parse_date_time(data2$days,orders=c( "bY"))
startdate <- as.Date("2014/12/31","%Y/%m/%d")
data2$date <- format(as.POSIXct(data2$days,format='%Y/%m/%d %UTC'),format='%Y/%m/%d')
data2$date2 <-  as.Date(data2$date,"%Y/%m/%d")
data2$NumDays  <- as.numeric(difftime(data2$date2,startdate ,units="days"))
library(tidyverse)
Med<- dplyr::arrange(setNames(aggregate (data2$IncR, list(data2$Region,data2$date2, data2$Age), median, na.rm=T), c("Region", "Date", "Age", "Med_Inc")), Date)
#MeaN<- MeaN%>% arrange(Age)
#View(MeaN)
Med_region<- dplyr::arrange(setNames(aggregate (data2$IncR, list(data2$Region, data2$days), median, na.rm=T), c("Region", "days", "Med_Inc")), days)
View(Med)

Med_dist<- setNames(aggregate (data$IncR, list(data$Region,data$District), median, na.rm=T), c("Region", "District",  "Med_Inc"))

write.csv(Med_dist, "In_Ans_ks.csv")



# Time series plotting
plot.new()
Med_Inc_ts<-ts(Med_region$Med_Inc, start = 2015, end = 2018, freq = 12)
Med_Inc_ts<-ts(Med_region$Med_Inc[Med_region$Region=="Western"], start = 2015, end = 2018, freq = 12)
Med_Inc_stl<-stl(Med_Inc_ts, s.window = "period")
plot(Med_Inc_stl, main = "Ghana")

attach(mtcars)
par(mfrow=c(2,2))
plot(wt,mpg, main="Scatterplot of wt vs. mpg")
plot(wt,disp, main="Scatterplot of wt vs disp")
hist(wt, main="Histogram of wt")
boxplot(wt, main="Boxplot of wt")








Ash<-Med_region%>%filter(Region=="Ashanti")
Ashwide<- Ash%>% spread(days, Med_Inc)

Ashw<-ts(Ashwide, frequency = 12, start=c(2015,1))
plot(Ashw)


p <- ggplot(data2, aes(x=IncR)) + 
  geom_density()
p+ geom_vline(aes(xintercept=mean(IncR),na.rm=T),
              color="blue", linetype="dashed", size=1)
tiff("descrip.tiff", units="in", width=8, height=5, res=800)
ab<-ggplot(Med, aes(x =Date , y = Med_Inc)) + 
  geom_line(aes(color = as.factor(Age))) + 
  #geom_smooth(alpha=.2, size=1) +
  #geom_line()+
  facet_wrap(~ Region) + 
  #scale_fill_manual(legend_title) + 
  # guides(fill=guide_legend("Age groups"))+
  labs(title = "A",
       #subtitle = paste("", format(max(MeaN$Days), "%A, %B %e, %Y")),
       x = "Year", y = "Incidence per 100 000 population"#,
       #caption = "Data: Source DHIMS 2018| Graph: @koissi Savi") +
    )+
  
  theme_minimal()
ab+theme(axis.text.x = element_text(angle = 90))
dev.off()
update_labels(ab, list(colour="Age groups"))
ggsave("Trend.png",dpi=800)

ggplot(data2, aes(x =NumDays , y = IncR, na.rm=TRUE)) + 
  geom_line(aes(color = sex, linetype= sex)) + 
  #geom_smooth(alpha=.2, size=1) +
  #geom_line()+
  facet_wrap(~ Region)+ 
  labs(title = "Malaria average incidence",
       #subtitle = paste("", format(max(MeaN$Days), "%A, %B %e, %Y")),
       x = "Number of Days", y = "Incidence rate", 
       caption = "Data: Source DHIMS 2018| Graph: @koissi Savi") + 
  theme_minimal()

MeaNY<- dplyr::arrange(setNames(aggregate (data2$IncR, list(data2$Region,data2$Year, data2$Age), mean, na.rm=T), c("Region", "Year", "Age", "Average_Inc")), Year)
dim(MeaNY)
png("Over_Y.png")
bar<-ggplot(data = MeaNY,  aes(fill=as.factor(Age), y=Average_Inc, x=Year, na.rm=TRUE)) + 
  geom_bar(position="dodge", stat="identity") +
  facet_wrap(~ Region)+ 
  theme(legend.title = "Age group") +
  #bar + coord_flip()+
  labs(title = "A",
       #subtitle = paste("", format(max(MeaN$Days), "%A, %B %e, %Y")),
       x = "Year", y = "Incidence rate"#, 
       #caption = "Data: Source DHIMS 2018| Graph: @koissi Savi") + 
  ) +
       theme_minimal()
print(bar)
bar+ labs(fill= "Age groups")
#dev.off()
ggsave("bar.png",dpi = 800 )
#bar + coord_polar()

MeaNYs<- dplyr::arrange(setNames(aggregate (data2$IncR, list(data2$Region,data2$Year, data2$sex), mean, na.rm=T), c("Region", "Year", "sex", "Average_Inc")), Year)

bas<-ggplot(data = MeaNYs,  aes(fill=sex, y=Average_Inc, x=Year, na.rm=TRUE)) + 
  geom_bar(position="dodge", stat="identity") +
  facet_wrap(~ Region) + 
  labs(title = "B",
       #subtitle = paste("", format(max(MeaN$Days), "%A, %B %e, %Y")),
       x = "Year", y = "Incidence rate"#, 
       #caption = "Data: Source DHIMS 2018| Graph: @koissi Savi") + 
  )+
   theme_minimal()

bas + labs(fill= "Gender") + coord_polar()
ggsave("gender.png", dpi=800)
dt %>%
  mutate(uniq_name = paste(Region, sex="f")) %>%
  group_by(uniq_name) %>%
  mutate(days_elapsed = date2-min(date2)) %>%
  ggplot(aes(x = days_elapsed, y = Incidence, group = uniq_name)) + 
  geom_line(size = 0.25, color = "gray20") + 
  scale_y_log10(labels = scales::label_number_si()) + 
  guides(color = FALSE) + 
  facet_wrap(~ Region, ncol = 5) + 
  labs(title = "Malaria incidence",
       subtitle = paste("", format(max(dt$date2), "%A, %B %e, %Y")),
       x = "Number of Days", y = "Incidence rate", 
       caption = "Data: Source DHIMS 2018| Graph: @koissi Savi") + 
  theme_minimal()


Max<-aggregate(data$Incidence, 
               list(data$geocode,
                    data$Year), max, na.rm=T)

Min<-aggregate(data$Incidence, 
               list(data$geocode,
                    data$Year), min, na.rm=T)

Mean<-aggregate(data$Incidence, 
                list(data$geocode,
                     data$Year), mean, na.rm=T)

MaxReg<-aggregate(data$Incidence, 
               list(data$Region,
                    data$Year), max, na.rm=T)

MinReg<-aggregate(data$Incidence, 
               list(data$Region,
                    data$Year), min, na.rm=T)

MeanReg<-aggregate(data$Incidence, 
                   list(data$Region,
                        data$Year), mean, na.rm=T)
dt<-dplyr::arrange(data, NumDays)
View(dt)
#library(tidyverse)


rate_rank<-data %>% filter (sex == "f",  Year>2014 & Year < 2019) %>% group_by(geocode) %>% summarize (mean_rate =
                                      mean(Incidence, na.rm=TRUE)) %>% mutate (rate_rank=rank (mean_rate))
rate_max_rank <- data %>% filter(sex=="f", Year==2017) %>% group_by(geocode) %>% summarize(Incidence_max =
                                      max (Incidence, na.rm=TRUE)) %>% mutate(Incidence_max_rank= rank(Incidence_max))

#dt  %>% filter( geocode %in% c("Abura-Asebu-Kwamankese, Central", "Accra Metro, Greater Accra", "Ada East, Greater Accra", "Ada West, Greater Accra", "Adaklu, Volta" , "Adansi North, Ashanti", "Adansi South, Ashanti", "Adentan, Greater Accra", "Afadjato South, Volta", "Afigya-Kwabre, Ashanti", "Afram Plains South, Eastern", "Agona East, Central", "Agona West, Central", "Agortime-Ziope, Volta", "Ahafo-Ano North, Ashanti", "Ahafo-Ano South, Ashanti", "Ahanta West, Western", "Ajumako-Enyan-Essiam, Central", "Akatsi North, Volta", "Akatsi South, Volta", "Akwapim North, Eastern", "Akwapim South, Eastern", "Akyemansa, Eastern", "Amansie Central, Ashanti" )) %>% group_by(Year, NumDays)  %>% slice(1)  %>%  left_join(rate_rank, by= "geocode")  %>% left_join (rate_max_rank, by= "geocode")  %>%ggplot(aes(x = NumDays, y= Incidence, group = Year)) + scale_color_manual( values= c("gray70", "firebrick"), labels = c("2015-2017", "2018")) +  scale_x_continuous(limits = c(1, 2000), breaks = c(1, seq(10, 2000, 30)), labels = as.character(c(1, seq(10, 2000, 30)))) + facet_wrap(~reorder(geocode, rate_rank, na.rm=TRUE), ncol = 4) + geom_line (size = 0.9) + guides (color = guide_legend (override.aes = list(size=3))) + labs(x= "Month of the Year", y="Incidence rate", color ="Year", title = "Overall malaria monthly incidence rate")



dt %>%
  mutate(uniq_name = paste(geocode, sex)) %>%
  filter( geocode %in% c("Abura-Asebu-Kwamankese, Central", "Accra Metro, Greater Accra", "Ada East, Greater Accra", "Ada West, Greater Accra", "Adaklu, Volta" , "Adansi North, Ashanti", "Adansi South, Ashanti", "Adentan, Greater Accra", "Afadjato South, Volta", "Afigya-Kwabre, Ashanti", "Afram Plains South, Eastern", "Agona East, Central", "Agona West, Central", "Agortime-Ziope, Volta", "Ahafo-Ano North, Ashanti", "Ahafo-Ano South, Ashanti", "Ahanta West, Western", "Ajumako-Enyan-Essiam, Central", "Akatsi North, Volta", "Akatsi South, Volta", "Akwapim North, Eastern", "Akwapim South, Eastern", "Akyemansa, Eastern", "Amansie Central, Ashanti" ))%>% 
  group_by(uniq_name) %>%
  mutate(days_elapsed = date2-min(date2)) %>%
  ggplot(aes(x = days_elapsed, y = Incidence, group = uniq_name)) + 
  geom_line(size = 0.25, color = "gray20") + 
  scale_y_log10(labels = scales::label_number_si()) + 
  guides(color = FALSE) + 
  facet_wrap(~ geocode, ncol = 5) + 
  labs(title = "Malaria incidence",
       subtitle = paste("", format(max(dt$date2), "%A, %B %e, %Y")),
       x = "Number of Days", y = "Incidence rate", 
       caption = "Data: The German Times | Graph: @koissi Savi") + 
  theme_minimal()


dt %>%
  mutate(uniq_name = paste(Region, sex="f")) %>%
   group_by(uniq_name) %>%
  mutate(days_elapsed = date2-min(date2)) %>%
  ggplot(aes(x = days_elapsed, y = Incidence, group = uniq_name)) + 
  geom_line(size = 0.25, color = "gray20") + 
  scale_y_log10(labels = scales::label_number_si()) + 
  guides(color = FALSE) + 
  facet_wrap(~ Region, ncol = 5) + 
  labs(title = "Malaria incidence",
       subtitle = paste("", format(max(dt$date2), "%A, %B %e, %Y")),
       x = "Number of Days", y = "Incidence rate", 
       caption = "Data: Source DHIMS 2018| Graph: @koissi Savi") + 
  theme_minimal()


MeaN_cs<- setNames(aggregate (data$Incidence, list(data$Region,data$District), mean, na.rm=T), c("Region", "District", "Average_Inc"))
sd_cs_d<-setNames(aggregate (data$Incidence, list(data$Region,data$Year), sd, na.rm=T), c("Region", "Year", "sd_Inc"))
MeaN_cs_d<- setNames(aggregate (data$Incidence, list(data$Region, data$Year), mean, na.rm=T), c("Region", "Year","Average_Inc"))
taB<-merge(x=MeaN_cs_d, y=sd_cs_d, by.x=c("Region","Year"), by.y=c("Region", "Year"))
View(taB)

sd_cs_d<-setNames(aggregate (data$Incidence, list(data$Region), sd, na.rm=T), c("Region", "sd_Inc"))
MeaN_cs_d<- setNames(aggregate (data$Incidence, list(data$Region), mean, na.rm=T), c("Region","Average_Inc"))
taB<-merge(x=MeaN_cs_d, y=sd_cs_d, by.x="Region", by.y="Region")
taB



#require(maps)
#world_map <- map_data("world")
#map_bj<- map_data("world", region = "Benin")
Max_cs<-setNames(aggregate (data$Incidence, list(data$Region,data$District), max, na.rm=T), c("Region", "District", "Max_Inc"))

Min_cs<-setNames(aggregate (data$Incidence, list(data$Region,data$District), min, na.rm=T), c("Region", "District", "Min_Inc"))

Med_cs<-setNames(aggregate (data2$IncR, list(data2$Region,data2$District), median, na.rm=T), c("Region", "District", "Med_Inc"))

m0_cs<-merge(x=Max_cs, y=Med_cs,by.x = c("Region","District"), by.y = c("Region","District") )
M_cs<-merge(x = m0_cs, y = Min_cs,by.x = c("Region","District"), by.y =c("Region","District") )
#M_cs<-M_cs[,-c(4,6)]
View(M_cs)



library(mgcv)
ub1 <- gam(Average_Inc ~ s(BuiltAr)+ s(BuiltInten) + s(Urbanizati),na.action=na.omit, data=pshp@data)
summary(ub1)
gam.check(ub1)
plot(ub1,scheme=2)


mk<-lm(Average_Inc ~log(BuiltInten) + vaccinations + ITN + Edu_men + Edu_wm + Healthcare + Toilet + Water , data=pshp@data)
summary(mk)


ggplot(data=subset(pshp@data, !is.na(Average_Inc)), aes(x=BuiltInten, y=Average_Inc))+
  geom_point(aes(colour= Region.x), size = 1, alpha = .8)+
  guides (col = guide_legend(nrow = 10)) +
  geom_smooth()+
  labs(title = "Malaria incidence and urbanization",
       #subtitle = paste("", format(max(dt$date2), "%A, %B %e, %Y")),
       x = "Build intensity", y = "Incidence rate", 
       caption = "Data: DHIMS 2019| Graph: @koissi Savi") + 
  theme_minimal()

modl1<-lm(pshp@data$Average_Inc~pshp@data$BuiltInten)
summary(modl1)

ggplot(data=subset(pshp@data, !is.na(Average_Inc)), aes(x=BuiltAr, y=Average_Inc))+
  geom_point(aes(colour= Region.x), size = 1, alpha = .8)+
  guides (col = guide_legend(nrow = 10)) +
  geom_smooth()+
  labs(title = "Malaria incidence and urbanization",
       #subtitle = paste("", format(max(dt$date2), "%A, %B %e, %Y")),
       x = "Log of Build areas", y = "Incidence rate", 
       caption = "Data: DHIMS 2019| Graph: @koissi Savi") + 
  theme_minimal()

modl2<-lm(pshp@data$Average_Inc~pshp@data$BuiltAr)
summary(modl2)

dftz<- as.data.frame(pshp@data[,-c(2:11,13:14,16:24,26:30,32)])
colnames(dftz)[colnames(dftz)=="NAME"]<-"District"
library(FactoMineR)
attach(dftz)
row.names(dftz)<-as.factor(District)
dftz<-dftz[,-1]
res.pca<-PCA(dftz, scale.unit=TRUE, ncp=2, graph=TRUE)
res.hcpc<-HCPC(res.pca, nb.clust=7)

library(factoextra)
fviz_dend(res.hcpc, 
          cex = 0.4,                     # Label size
          palette = "jco",               # Color palette see ?ggpubr::ggpar
          rect = TRUE, rect_fill = TRUE, # Add rectangle around groups
          rect_border = "jco",           # Rectangle color
          labels_track_height = 0.5      # Augment the room for labels
)

fviz_cluster(res.hcpc, geom = "point", main = "Factor map")

fviz_cluster(res.hcpc, 
             repel = TRUE,            # Avoid label overlapping
             show.clust.cent = TRUE, # Show cluster centers
             palette = "jco", labels="none",        # Color palette see ?ggpubr::ggpar
             ggtheme = theme_minimal(),
             main = "Factor map"
)

ggplot(res.hcpc$data.clust, aes(x=BuiltInten, y=Average_Inc))+
  geom_point(aes(colour= clust), size = 1, alpha = .8)+
  guides (col = guide_legend(nrow = 7)) +
  geom_smooth()+
  labs(title = "Malaria incidence and urbanization",
       #subtitle = paste("", format(max(dt$date2), "%A, %B %e, %Y")),
       x = "Build Intensity", y = "Incidence rate", 
       caption = "Data: DHIMS 2019| Graph: @koissi Savi") + 
  theme_minimal()

ggplot(res.hcpc$data.clust, aes(x=BuiltAr, y=Average_Inc))+
  geom_point(aes(colour= clust), size = 1, alpha = .8)+
  guides (col = guide_legend(nrow = 10)) +
  geom_smooth()+
  labs(title = "Malaria incidence and urbanization",
       #subtitle = paste("", format(max(dt$date2), "%A, %B %e, %Y")),
       x = "Build areas", y = "Incidence rate", 
       caption = "Data: DHIMS 2019| Graph: @koissi Savi") + 
  theme_minimal()

ggplot(res.hcpc$data.clust, aes(x=Urbanizati, y=Average_Inc))+
  geom_point(aes(colour= clust), size = 1, alpha = .8)+
  guides (col = guide_legend(nrow = 10)) +
  geom_smooth()+
  labs(title = "Malaria incidence and urbanization",
       #subtitle = paste("", format(max(dt$date2), "%A, %B %e, %Y")),
       x = "Urbanization rate", y = "Incidence rate", 
       caption = "Data: DHIMS 2019| Graph: @koissi Savi") + 
  theme_minimal()

#write.csv(res.hcpc$data.clust, "clust.csv")

# Let's now compute the Moran's I index and the Gini coefficient 
library(rgdal)
dshp<-readOGR("C:/Users/ZEF/Desktop/Related_ms2/Data/District Shapefile/Districts","Map_of_Districts_216") 
#View(dshp@data)
dshp@data$NAME[dshp@data$NAME=="Accra Metropolis"]<- "Accra Metro"
dshp@data$NAME[dshp@data$NAME=="Adenta"]<-"Adentan"
dshp@data$NAME[dshp@data$NAME=="Afadzato South"]<-"Afadjato South"
dshp@data$NAME[dshp@data$NAME=="Afigya Kwabre"]<-"Afigya-Kwabre"
dshp@data$NAME[dshp@data$NAME=="Kwahu Afram Plains South"]<-"Afram Plains South"
dshp@data$NAME[dshp@data$NAME=="Agotime Ziope"]<-"Agortime-Ziope"
dshp@data$NAME[dshp@data$NAME=="Ahafo Ano North"]<-"Ahafo-Ano North"
dshp@data$NAME[dshp@data$NAME=="Ahafo Ano South"]<-"Ahafo-Ano South"
dshp@data$NAME[dshp@data$NAME=="Ajumako-Enyan-Esiam"]<-"Ajumako-Enyan-Essiam"
dshp@data$NAME[dshp@data$NAME=="Akwapem North"]<-"Akwapim North"
dshp@data$NAME[dshp@data$NAME=="Akwapem South"]<-"Akwapim South"
dshp@data$NAME[dshp@data$NAME=="Akyem Mansa"]<-"Akyemansa"
dshp@data$NAME[dshp@data$NAME=="Asante Akim Central Municipal"]<-"Asante Akim Central" 
dshp@data$NAME[dshp@data$NAME=="Asokore Mampong Municipal"]<-"Asokore-Mampong" 
dshp@data$NAME[dshp@data$NAME=="Atebubu Amantin"]<-"Atebubu-Amanten" 
dshp@data$NAME[dshp@data$NAME=="Atwima Kwanwoma"]<-"Atwima-Kwanwoma" 
dshp@data$NAME[dshp@data$NAME=="Awutu Senya East Municipal"]<-"Awutu Senya East"
dshp@data$NAME[dshp@data$NAME=="Bawku Municipal"]<-"Bawku"
dshp@data$NAME[dshp@data$NAME=="Bekwai Municipal"]<-"Bekwai"
dshp@data$NAME[dshp@data$NAME=="Birim Municipal" ]<-"Birim Central"
dshp@data$NAME[dshp@data$NAME=="Bolgatanga Municipal" ]<- "Bolgatanga"
dshp@data$NAME[dshp@data$NAME=="Bosomtwe /Atwima-Kwanwoma" ]<- "Bosomtwe"
dshp@data$NAME[dshp@data$NAME=="Cape Coast Metro" ]<- "Cape Coast"
dshp@data$NAME[dshp@data$NAME=="Bunkpurugu Yonyo" ]<-  "Bunkpurugu-Yunyoo"
dshp@data$NAME[dshp@data$NAME=="Asante-Mampong" ]<-  "Mampong Municipal"
dshp@data$NAME[dshp@data$NAME=="Effutu" ]<-  "Efutu"
dshp@data$NAME[dshp@data$NAME=="Ejisu Juaben"]<-  "Ejisu-Juaben"
dshp@data$NAME[dshp@data$NAME=="Ejura Sekye Dumase"]<- "Ejura-Sekyedumase"
dshp@data$NAME[dshp@data$NAME=="Ga Central Municipal" ]<- "Ga Central" 
dshp@data$NAME[dshp@data$NAME=="Garu Tempane" ]<- "Garu-Tempane" 
dshp@data$NAME[dshp@data$NAME=="Gonja Central"]<- "Central Gonja"
dshp@data$NAME[dshp@data$NAME=="Ho Municipal"]<- "Ho"
dshp@data$NAME[dshp@data$NAME=="Hohoe Municipal"]<- "Hohoe"
dshp@data$NAME[dshp@data$NAME=="Juabeso"]<- "Juaboso"
dshp@data$NAME[dshp@data$NAME=="Kasena Nankana East"]<- "Kasena-Nankana"
dshp@data$NAME[dshp@data$NAME=="Kasena Nankana West"]<- "Kasena-Nankana West"
dshp@data$NAME[dshp@data$NAME=="Keta Municipal"]<- "Keta"
dshp@data$NAME[dshp@data$NAME== "Komenda Edna Eguafo-Abirem"]<- "Komenda-Edna-Eguafo-Abirem"
dshp@data$NAME[dshp@data$NAME=="Kpando Municipal"]<- "Kpando"
dshp@data$NAME[dshp@data$NAME=="Kpone Katamanso"]<- "Kpone-Katamanso"
dshp@data$NAME[dshp@data$NAME=="Kumbumgu"]<- "Kumbungu"
dshp@data$NAME[dshp@data$NAME=="Kwabre"]<- "Kwabre East"
dshp@data$NAME[dshp@data$NAME=="Kwahu Afram Plains North"]<- "Kwahu North"
dshp@data$NAME[dshp@data$NAME=="La Dade Kotopon"]<- "La-Dade-Kotopon"
dshp@data$NAME[dshp@data$NAME=="La Nkwantanang Madina"]<- "La-Nkwantanang-Madina"
dshp@data$NAME[dshp@data$NAME=="Lambussie Karni"]<- "Lambussie-Karni"
dshp@data$NAME[dshp@data$NAME=="Lower Manya"]<- "Lower-Manya Krobo"
dshp@data$NAME[dshp@data$NAME=="Mampong Municipal"]<- "Asante-Mampong"
dshp@data$NAME[dshp@data$NAME=="Mamprugu Moagduri"]<- "Mamprugu-Moagduri"
dshp@data$NAME[dshp@data$NAME=="New Juaben Municipal"]<- "New Juaben"
dshp@data$NAME[dshp@data$NAME=="Nsawam Adoagyiri"]<- "Nsawam-Adoagyiri"
dshp@data$NAME[dshp@data$NAME=="Obuasi, Ashanti"]<- "Obuasi"
dshp@data$NAME[dshp@data$NAME=="Offinso Municipal"]<- "Offinso"
dshp@data$NAME[dshp@data$NAME=="Sagnerigu"]<- "Sagnarigu"
dshp@data$NAME[dshp@data$NAME=="Savelugu Nanton"]<- "Savelugu-Nanton"
dshp@data$NAME[dshp@data$NAME=="Sawla/Tuna/Kalba"]<- "Sawla-Tuna-Kalba" 
dshp@data$NAME[dshp@data$NAME=="Sefwi Akontombra"]<- "Sefwi-Akontombra"
dshp@data$NAME[dshp@data$NAME=="Sefwi Bibiani-Anhwiaso Bekwai"]<- "Bibiani-Anhwiaso-Bekwai"
dshp@data$NAME[dshp@data$NAME=="Sekondi Takoradi"]<- "Sekondi-Takoradi"
dshp@data$NAME[dshp@data$NAME== "Shai Osu Doku"]<- "Shai-Osudoku"
dshp@data$NAME[dshp@data$NAME=="Sissala  West"]<- "Sissala West"
dshp@data$NAME[dshp@data$NAME== "Suhum Municipal"]<- "Suhum"
dshp@data$NAME[dshp@data$NAME== "Tarkwa Nsuaem"]<- "Tarkwa-Nsuaem"
dshp@data$NAME[dshp@data$NAME== "Mamprusi East"]<- "East Mamprusi"
dshp@data$NAME[dshp@data$NAME== "Tamale North Sub Metro"]<- "Tamale"
dshp@data$NAME[dshp@data$NAME== "Tatale"]<- "Tatale-Sangule"
dshp@data$NAME[dshp@data$NAME== "Tema Metropolis"]<- "Tema"
dshp@data$NAME[dshp@data$NAME== "Twifo Ati-Morkwa"]<- "Twifo-Ati-Mokwa"
dshp@data$NAME[dshp@data$NAME== "Twifo Lower Denkyira"]<- "Twifo-Hemang Lower Denkyira"
dshp@data$NAME[dshp@data$NAME== "Suhum Municipal"]<- "Suhum"
dshp@data$NAME[dshp@data$NAME== "Upper Manya"]<- "Upper Manya-Krobo"
dshp@data$NAME[dshp@data$NAME== "Wa Municipal"]<- "Wa"
dshp@data$NAME[dshp@data$NAME== "Wassa Amenfi Central"]<- "Wassa-Amenfi Central"
dshp@data$NAME[dshp@data$NAME== "Yendi Municipal"]<- "Yendi"
dshp@data$NAME[dshp@data$NAME== "Yilo Krobo"]<- "Yilo-Krobo"
dshp@data$NAME[dshp@data$NAME== "Daffiama Bussie"]<- "Daffiama-Bussie-Issa"
dshp@data$NAME[dshp@data$NAME== "Kma"]<- "Kumasi Metro"
dshp@data$NAME[dshp@data$NAME== "Sekyere Afram Plains North"]<- "Sekyere Kumawu"
dshp@data$NAME[dshp@data$NAME== "Afigya Sekyere"]<- "Sekyere South"
dshp@data$NAME[dshp@data$NAME== "Abura / Asebu / Kwamankese"]<- "Abura-Asebu-Kwamankese" 
dshp@data$NAME[dshp@data$NAME== "Asikuma / Odoben / Brakwa"]<- "Asikuma-Odoben-Brakwa" 
dshp@data$NAME[dshp@data$NAME== "Bosomtwe /Atwima / Kwanwoma"]<- "Bosomtwe" 
dshp@data$NAME[dshp@data$NAME== "Komenda Edna Eguafo / Abirem"]<- "Komenda-Edna-Eguafo-Abirem" 
dshp@data$NAME[dshp@data$NAME== "Ledzokuku / Krowor"]<- "Ledzokuku-Krowor" 
dshp@data$NAME[dshp@data$NAME== "Prestea / Huni Valley"]<- "Prestea-Huni Valley" 
dshp@data$NAME[is.na(dshp@data$NAME)] <- "Obuasi"

# Cross check the link before merging the tables
which (unique(sort(dshp@data$NAME))!=unique(sort(Med_cs$District)))
setdiff(sort(unique(dshp@data$NAME)), unique(Med_cs$District))
setdiff(unique(Med_cs$District), unique(dshp@data$NAME))
# Well everything is set and we will not have a missing value in the final dataset
dshp <- merge(x=dshp,y=Med_cs,by.x="NAME",by.y="District")

data2<-read.csv("C:/Users/ZEF/Downloads/koissimap.csv", header=TRUE)
View(data2)
dshp <- merge(x=dshp,y=data2,by.x="NAME",by.y="District")
#library(sf)
#library(raster)
#library(dplyr)
#library(spData)
#library(spDataLarge)
library(tmap)    # for static and interactive maps
#library(leaflet) # for interactive maps
library(ggplot2) # tidyverse data visualization package
library("wesanderson") # chose the palette of color
tm_shape(dshp) +
  tm_fill("clustnew",
          style="pretty", 
          palette=wes_palette("Chevalier1"),
          bbox = NULL,
          #title="Cluster"
          ) +
  tm_borders(col = "burlywood4", lwd = 0.125)+
  tm_layout(frame = FALSE, legend.position=c("right","bottom"))


require(cartography)
col <- colorRamps::matlab.like(12)

par(mfrow=c(1,3))
#library(Cairo)
#Cairo("min.png", width = 3, height = 7, dpi=800)
choroLayer(spdf = dshp, df = dshp@data, var = "Min_Inc",col=col,  legend.title.txt = "Minimun Incidences")
#layoutLayer(title = "Moran's I = 0.14", north = T, sources = "DHIMS, 2019")

choroLayer(spdf = dshp, df = dshp@data, var = "Average_Inc",col=col,
           legend.title.txt = "Mediane Incidences")
#layoutLayer(title = "Moran's I =0.31", north = T, sources = "DHIMS, 2019")

choroLayer(spdf = dshp, df = dshp@data, var = "Max_Inc",col=col,
           legend.title.txt = "Maximun Incidences")
#layoutLayer(title = "Moran's I = 0.46", north = T, sources = "DHIMS, 2019")


library(spdep)
library(sp)

dshp.nb<-poly2nb(dshp, queen=TRUE)
lw <- nb2listw(dshp.nb, style="W", zero.policy=TRUE)
est <- moran.mc(dshp$Med_Inc,lw,zero.policy=T,nsim=1000)
est_test<-moran.test(dshp$Med_Inc, lw, zero.policy=T,randomisation = FALSE)
#est_test2<-moran.test(dshp$Max_Inc, lw, zero.policy=T,randomisation = FALSE)
#est_test3<-moran.test(dshp$Min_Inc, lw, zero.policy=T,randomisation = FALSE)
nci<-moran.plot(dshp$Med_Inc, listw=lw, , zero.policy=T,
                xlab="Malaria incidence", ylab="Spatially lagged", labels=T, pch=16, col="grey")
text(c(60,60, 5,5),c(40,5,40,5), c("High-High", "High-Low", "Low-High", "Low-Low"), cex=0.8)

dshp$lag <- lag.listw(lw, dshp$Med_Inc,zero.policy=T)
locm_bm <- localmoran(dshp$Med_Inc, lw,zero.policy=T)
dshp <- st_as_sf(dshp) %>% 
  mutate(quad_sig = ifelse(dshp$Med_Inc > 0 & 
                             dshp$lag > 0 & 
                             locm_bm[,5] <= 0.05, 
                           "high-high",
                           ifelse(dshp$Med_Inc <= 0 & 
                                    dshp$lag <= 0 & 
                                    locm_bm[,5] <= 0.05, 
                                  "low-low", 
                                  ifelse(dshp$Med_Inc > 0 & 
                                           dshp$lag<= 0 & 
                                           locm_bm[,5] <= 0.05, 
                                         "high-low",
                                         ifelse(dshp$Med_Inc<= 0 & 
                                                  dshp$lag > 0 & 
                                                  locm_bm[,5] <= 0.05,
                                                "low-high", 
                                                "non-significant")))))

table(dshp$quad_sig)

#nrow(locm_bm[locm_bm[,5] <= 0.05,])
qtm(dshp, fill="quad_sig", fill="LISA cluster map", style ="bw")+
  tm_legend(legend.position = c("left", "top"),
            main.title = "",
            main.title.position = "right")

require(tmap)
tmap_mode("plot")
m<- tm_shape(dshp) +
  #tm_polygons('quad_sig', palette="-Blues")+
  tm_borders(col = "black", lwd = 0.3) +
  tm_fill(col = 'quad_sig', title = 'LISA Cluster', legend.hist = TRUE,
          palette = "-Blues", auto.palette.mapping = FALSE) +
  tm_legend(legend.outside = TRUE) +
  tm_layout(frame = FALSE)# +
  #tm_credits('Source: DHIMS 2019', position = 'right')
tmap_save(m,"LISA.png", dpi=800)
reg_hi<-dplyr::filter(dshp, quad_sig=="high-high")
tm_shape(dshp) + 
  tm_fill("Average_Inc", style = "quantile", palette = "Reds") +
  tm_borders(alpha = 1) +
  tm_layout(main.title = "Malaria Incidence", main.title.size = 0.7 ,
            legend.position = c("right", "top"), legend.title.size = 0.8)





#modl_lo<-loess(pshp@data$Average_Inc~log(pshp@data$BuiltInten), degree = 2, span= 0.5)
#summary(modl_lo)
#tz<-predict(modl_lo)






##Moran's I test for residual autocorrelation
pshp@data$Total= rowSums(totalincid)
col <- colorRamps::matlab.like(12)
morans <- rep(0,1)
morans.pval <- rep(0,1)
totalincid <- rep(0,1)

nb <- poly2nb(pshp, queen=TRUE)
lw <- nb2listw(nb, style="W", zero.policy=TRUE)
est <- moran.mc(shp$Total,lw,zero.policy=T,nsim=500)
choroLayer(spdf = pshp, df = pshp@data, var = "Average_Inc",breaks=seq(0.5,62.0072,2500),col=col,
           legend.title.txt = "Total Incidences")
layoutLayer(title = "Moran's I = 0.29")












ad_var<-read.csv("C:/Users/ZEF/Desktop/Related_ms2/Add_var_R.csv")
#ad_var$Edu<-ad_var$Edu_men + ad_var$Edu_wm
ad_var1<-ad_var[,-c(3:10)]
pop<-read.csv("C:/Users/ZEF/Desktop/Related_ms2/Rearranged_pop.csv")
pop15<- dplyr::filter(pop, Year == 2015)
pop_sum<- setNames(aggregate (pop15$Estimate, list(pop15$NAME), sum, na.rm=T), c("NAME",  "Tot_pop"))


ad_var1$NAME2[ad_var1$NAME2=="Accra Metropolis"]<- "Accra Metro"
ad_var1$NAME2[ad_var1$NAME2=="Adenta"]<-"Adentan"
ad_var1$NAME2[ad_var1$NAME2=="Afadzato South"]<-"Afadjato South"
ad_var1$NAME2[ad_var1$NAME2=="Afigya Kwabre"]<-"Afigya-Kwabre"
ad_var1$NAME2[ad_var1$NAME2=="Kwahu Afram Plains South"]<-"Afram Plains South"
ad_var1$NAME2[ad_var1$NAME2=="Agotime Ziope"]<-"Agortime-Ziope"
ad_var1$NAME2[ad_var1$NAME2=="Ahafo Ano North"]<-"Ahafo-Ano North"
ad_var1$NAME2[ad_var1$NAME2=="Ahafo Ano South"]<-"Ahafo-Ano South"
ad_var1$NAME2[ad_var1$NAME2=="Ajumako-Enyan-Esiam"]<-"Ajumako-Enyan-Essiam"
ad_var1$NAME2[ad_var1$NAME2=="Akwapem North"]<-"Akwapim North"
ad_var1$NAME2[ad_var1$NAME2=="Akwapem South"]<-"Akwapim South"
ad_var1$NAME2[ad_var1$NAME2=="Akyem Mansa"]<-"Akyemansa"
ad_var1$NAME2[ad_var1$NAME2=="Asante Akim Central Municipal"]<-"Asante Akim Central" 
ad_var1$NAME2[ad_var1$NAME2=="Asokore Mampong Municipal"]<-"Asokore-Mampong" 
ad_var1$NAME2[ad_var1$NAME2=="Atebubu Amantin"]<-"Atebubu-Amanten" 
ad_var1$NAME2[ad_var1$NAME2=="Atwima Kwanwoma"]<-"Atwima-Kwanwoma" 
ad_var1$NAME2[ad_var1$NAME2=="Awutu Senya East Municipal"]<-"Awutu Senya East"
ad_var1$NAME2[ad_var1$NAME2=="Bawku Municipal"]<-"Bawku"
ad_var1$NAME2[ad_var1$NAME2=="Bekwai Municipal"]<-"Bekwai"
ad_var1$NAME2[ad_var1$NAME2=="Birim Municipal" ]<-"Birim Central"
ad_var1$NAME2[ad_var1$NAME2=="Bolgatanga Municipal" ]<- "Bolgatanga"
ad_var1$NAME2[ad_var1$NAME2=="Bosomtwe /Atwima-Kwanwoma" ]<- "Bosomtwe"
ad_var1$NAME2[ad_var1$NAME2=="Cape Coast Metro" ]<- "Cape Coast"
ad_var1$NAME2[ad_var1$NAME2=="Bunkpurugu Yonyo" ]<-  "Bunkpurugu-Yunyoo"

ad_var1$NAME2[ad_var1$NAME2=="Asante-Mampong" ]<-  "Mampong Municipal"
ad_var1$NAME2[ad_var1$NAME2=="Effutu" ]<-  "Efutu"
ad_var1$NAME2[ad_var1$NAME2=="Ejisu Juaben"]<-  "Ejisu-Juaben"
ad_var1$NAME2[ad_var1$NAME2=="Ejura Sekye Dumase"]<- "Ejura-Sekyedumase"
ad_var1$NAME2[ad_var1$NAME2=="Ga Central Municipal" ]<- "Ga Central" 
ad_var1$NAME2[ad_var1$NAME2=="Garu Tempane" ]<- "Garu-Tempane" 
ad_var1$NAME2[ad_var1$NAME2=="Gonja Central"]<- "Central Gonja"
ad_var1$NAME2[ad_var1$NAME2=="Ho Municipal"]<- "Ho"
ad_var1$NAME2[ad_var1$NAME2=="Hohoe Municipal"]<- "Hohoe"
ad_var1$NAME2[ad_var1$NAME2=="Juabeso"]<- "Juaboso"
ad_var1$NAME2[ad_var1$NAME2=="Kasena Nankana East"]<- "Kasena-Nankana"
ad_var1$NAME2[ad_var1$NAME2=="Kasena Nankana West"]<- "Kasena-Nankana West"
ad_var1$NAME2[ad_var1$NAME2=="Keta Municipal"]<- "Keta"
ad_var1$NAME2[ad_var1$NAME2== "Komenda Edna Eguafo-Abirem"]<- "Komenda-Edna-Eguafo-Abirem"
ad_var1$NAME2[ad_var1$NAME2=="Kpando Municipal"]<- "Kpando"
ad_var1$NAME2[ad_var1$NAME2=="Kpone Katamanso"]<- "Kpone-Katamanso"
ad_var1$NAME2[ad_var1$NAME2=="Kumbumgu"]<- "Kumbungu"

ad_var1$NAME2[ad_var1$NAME2=="Kwabre"]<- "Kwabre East"
ad_var1$NAME2[ad_var1$NAME2=="Kwahu Afram Plains North"]<- "Kwahu North"
ad_var1$NAME2[ad_var1$NAME2=="La Dade Kotopon"]<- "La-Dade-Kotopon"
ad_var1$NAME2[ad_var1$NAME2=="La Nkwantanang Madina"]<- "La-Nkwantanang-Madina"
ad_var1$NAME2[ad_var1$NAME2=="Lambussie Karni"]<- "Lambussie-Karni"
ad_var1$NAME2[ad_var1$NAME2=="Lower Manya"]<- "Lower-Manya Krobo"
ad_var1$NAME2[ad_var1$NAME2=="Mampong Municipal"]<- "Asante-Mampong"
ad_var1$NAME2[ad_var1$NAME2=="Mamprugu Moagduri"]<- "Mamprugu-Moagduri"
ad_var1$NAME2[ad_var1$NAME2=="New Juaben Municipal"]<- "New Juaben"
ad_var1$NAME2[ad_var1$NAME2=="Nsawam Adoagyiri"]<- "Nsawam-Adoagyiri"
ad_var1$NAME2[ad_var1$NAME2=="Obuasi, Ashanti"]<- "Obuasi"

ad_var1$NAME2[ad_var1$NAME2=="Offinso Municipal"]<- "Offinso"
ad_var1$NAME2[ad_var1$NAME2=="Sagnerigu"]<- "Sagnarigu"
ad_var1$NAME2[ad_var1$NAME2=="Savelugu Nanton"]<- "Savelugu-Nanton"
ad_var1$NAME2[ad_var1$NAME2=="Sawla/Tuna/Kalba"]<- "Sawla-Tuna-Kalba" 
ad_var1$NAME2[ad_var1$NAME2=="Sefwi Akontombra"]<- "Sefwi-Akontombra"
ad_var1$NAME2[ad_var1$NAME2=="Sefwi Bibiani-Anhwiaso Bekwai"]<- "Bibiani-Anhwiaso-Bekwai"
ad_var1$NAME2[ad_var1$NAME2=="Sekondi Takoradi"]<- "Sekondi-Takoradi"
ad_var1$NAME2[ad_var1$NAME2== "Shai Osu Doku"]<- "Shai-Osudoku"
ad_var1$NAME2[ad_var1$NAME2=="Sissala  West"]<- "Sissala West"
ad_var1$NAME2[ad_var1$NAME2== "Suhum Municipal"]<- "Suhum"

ad_var1$NAME2[ad_var1$NAME2== "Tarkwa Nsuaem"]<- "Tarkwa-Nsuaem"
ad_var1$NAME2[ad_var1$NAME2== "Mamprusi East"]<- "East Mamprusi"
ad_var1$NAME2[ad_var1$NAME2== "Tamale North Sub Metro"]<- "Tamale"
ad_var1$NAME2[ad_var1$NAME2== "Tatale"]<- "Tatale-Sangule"
ad_var1$NAME2[ad_var1$NAME2== "Tema Metropolis"]<- "Tema"
ad_var1$NAME2[ad_var1$NAME2== "Twifo Ati-Morkwa"]<- "Twifo-Ati-Mokwa"
ad_var1$NAME2[ad_var1$NAME2== "Twifo Lower Denkyira"]<- "Twifo-Hemang Lower Denkyira"
ad_var1$NAME2[ad_var1$NAME2== "Suhum Municipal"]<- "Suhum"
ad_var1$NAME2[ad_var1$NAME2== "Upper Manya"]<- "Upper Manya-Krobo"
ad_var1$NAME2[ad_var1$NAME2== "Wa Municipal"]<- "Wa"

ad_var1$NAME2[ad_var1$NAME2== "Wassa Amenfi Central"]<- "Wassa-Amenfi Central"
ad_var1$NAME2[ad_var1$NAME2== "Yendi Municipal"]<- "Yendi"
ad_var1$NAME2[ad_var1$NAME2== "Yilo Krobo"]<- "Yilo-Krobo"
ad_var1$NAME2[ad_var1$NAME2== "Daffiama Bussie"]<- "Daffiama-Bussie-Issa"
ad_var1$NAME2[ad_var1$NAME2== "Kma"]<- "Kumasi Metro"
ad_var1$NAME2[ad_var1$NAME2== "Sekyere Afram Plains North"]<- "Sekyere Kumawu"
ad_var1$NAME2[ad_var1$NAME2== "Afigya Sekyere"]<- "Sekyere South"


ad_var1 <- merge(x=ad_var1,y=pop_sum,by.x="NAME",by.y="NAME")
View(ad_var1)

pshp <- merge(x=pshp,y=ad_var1,by.x="ID",by.y="ID")
View(pshp@data)




# Random forest
cl<-read.csv("clust.csv")
ad_var1<-read.csv("Add_var_R.csv")#
ad_var1$NAME2[ad_var1$NAME2=="Accra Metropolis"]<- "Accra Metro"
ad_var1$NAME2[ad_var1$NAME2=="Adenta"]<-"Adentan"
ad_var1$NAME2[ad_var1$NAME2=="Afadzato South"]<-"Afadjato South"
ad_var1$NAME2[ad_var1$NAME2=="Afigya Kwabre"]<-"Afigya-Kwabre"
ad_var1$NAME2[ad_var1$NAME2=="Kwahu Afram Plains South"]<-"Afram Plains South"
ad_var1$NAME2[ad_var1$NAME2=="Agotime Ziope"]<-"Agortime-Ziope"
ad_var1$NAME2[ad_var1$NAME2=="Ahafo Ano North"]<-"Ahafo-Ano North"
ad_var1$NAME2[ad_var1$NAME2=="Ahafo Ano South"]<-"Ahafo-Ano South"
ad_var1$NAME2[ad_var1$NAME2=="Ajumako-Enyan-Esiam"]<-"Ajumako-Enyan-Essiam"
ad_var1$NAME2[ad_var1$NAME2=="Akwapem North"]<-"Akwapim North"
ad_var1$NAME2[ad_var1$NAME2=="Akwapem South"]<-"Akwapim South"
ad_var1$NAME2[ad_var1$NAME2=="Akyem Mansa"]<-"Akyemansa"
ad_var1$NAME2[ad_var1$NAME2=="Asante Akim Central Municipal"]<-"Asante Akim Central" 
ad_var1$NAME2[ad_var1$NAME2=="Asokore Mampong Municipal"]<-"Asokore-Mampong" 
ad_var1$NAME2[ad_var1$NAME2=="Atebubu Amantin"]<-"Atebubu-Amanten" 
ad_var1$NAME2[ad_var1$NAME2=="Atwima Kwanwoma"]<-"Atwima-Kwanwoma" 
ad_var1$NAME2[ad_var1$NAME2=="Awutu Senya East Municipal"]<-"Awutu Senya East"
ad_var1$NAME2[ad_var1$NAME2=="Bawku Municipal"]<-"Bawku"
ad_var1$NAME2[ad_var1$NAME2=="Bekwai Municipal"]<-"Bekwai"
ad_var1$NAME2[ad_var1$NAME2=="Birim Municipal" ]<-"Birim Central"
ad_var1$NAME2[ad_var1$NAME2=="Bolgatanga Municipal" ]<- "Bolgatanga"
ad_var1$NAME2[ad_var1$NAME2=="Bosomtwe /Atwima-Kwanwoma" ]<- "Bosomtwe"
ad_var1$NAME2[ad_var1$NAME2=="Cape Coast Metro" ]<- "Cape Coast"
ad_var1$NAME2[ad_var1$NAME2=="Bunkpurugu Yonyo" ]<-  "Bunkpurugu-Yunyoo"
ad_var1$NAME2[ad_var1$NAME2=="Asante-Mampong" ]<-  "Mampong Municipal"
ad_var1$NAME2[ad_var1$NAME2=="Effutu" ]<-  "Efutu"
ad_var1$NAME2[ad_var1$NAME2=="Ejisu Juaben"]<-  "Ejisu-Juaben"
ad_var1$NAME2[ad_var1$NAME2=="Ejura Sekye Dumase"]<- "Ejura-Sekyedumase"
ad_var1$NAME2[ad_var1$NAME2=="Ga Central Municipal" ]<- "Ga Central" 
ad_var1$NAME2[ad_var1$NAME2=="Garu Tempane" ]<- "Garu-Tempane" 
ad_var1$NAME2[ad_var1$NAME2=="Gonja Central"]<- "Central Gonja"
ad_var1$NAME2[ad_var1$NAME2=="Ho Municipal"]<- "Ho"
ad_var1$NAME2[ad_var1$NAME2=="Hohoe Municipal"]<- "Hohoe"
ad_var1$NAME2[ad_var1$NAME2=="Juabeso"]<- "Juaboso"
ad_var1$NAME2[ad_var1$NAME2=="Kasena Nankana East"]<- "Kasena-Nankana"
ad_var1$NAME2[ad_var1$NAME2=="Kasena Nankana West"]<- "Kasena-Nankana West"
ad_var1$NAME2[ad_var1$NAME2=="Keta Municipal"]<- "Keta"
ad_var1$NAME2[ad_var1$NAME2== "Komenda Edna Eguafo-Abirem"]<- "Komenda-Edna-Eguafo-Abirem"
ad_var1$NAME2[ad_var1$NAME2=="Kpando Municipal"]<- "Kpando"
ad_var1$NAME2[ad_var1$NAME2=="Kpone Katamanso"]<- "Kpone-Katamanso"
ad_var1$NAME2[ad_var1$NAME2=="Kumbumgu"]<- "Kumbungu"
ad_var1$NAME2[ad_var1$NAME2=="Kwabre"]<- "Kwabre East"
ad_var1$NAME2[ad_var1$NAME2=="Kwahu Afram Plains North"]<- "Kwahu North"
ad_var1$NAME2[ad_var1$NAME2=="La Dade Kotopon"]<- "La-Dade-Kotopon"
ad_var1$NAME2[ad_var1$NAME2=="La Nkwantanang Madina"]<- "La-Nkwantanang-Madina"
ad_var1$NAME2[ad_var1$NAME2=="Lambussie Karni"]<- "Lambussie-Karni"
ad_var1$NAME2[ad_var1$NAME2=="Lower Manya"]<- "Lower-Manya Krobo"
ad_var1$NAME2[ad_var1$NAME2=="Mampong Municipal"]<- "Asante-Mampong"
ad_var1$NAME2[ad_var1$NAME2=="Mamprugu Moagduri"]<- "Mamprugu-Moagduri"
ad_var1$NAME2[ad_var1$NAME2=="New Juaben Municipal"]<- "New Juaben"
ad_var1$NAME2[ad_var1$NAME2=="Nsawam Adoagyiri"]<- "Nsawam-Adoagyiri"
ad_var1$NAME2[ad_var1$NAME2=="Obuasi, Ashanti"]<- "Obuasi"
ad_var1$NAME2[ad_var1$NAME2=="Offinso Municipal"]<- "Offinso"
ad_var1$NAME2[ad_var1$NAME2=="Sagnerigu"]<- "Sagnarigu"
ad_var1$NAME2[ad_var1$NAME2=="Savelugu Nanton"]<- "Savelugu-Nanton"
ad_var1$NAME2[ad_var1$NAME2=="Sawla/Tuna/Kalba"]<- "Sawla-Tuna-Kalba" 
ad_var1$NAME2[ad_var1$NAME2=="Sefwi Akontombra"]<- "Sefwi-Akontombra"
ad_var1$NAME2[ad_var1$NAME2=="Sefwi Bibiani-Anhwiaso Bekwai"]<- "Bibiani-Anhwiaso-Bekwai"
ad_var1$NAME2[ad_var1$NAME2=="Sekondi Takoradi"]<- "Sekondi-Takoradi"
ad_var1$NAME2[ad_var1$NAME2== "Shai Osu Doku"]<- "Shai-Osudoku"
ad_var1$NAME2[ad_var1$NAME2=="Sissala  West"]<- "Sissala West"
ad_var1$NAME2[ad_var1$NAME2== "Suhum Municipal"]<- "Suhum"
ad_var1$NAME2[ad_var1$NAME2== "Tarkwa Nsuaem"]<- "Tarkwa-Nsuaem"
ad_var1$NAME2[ad_var1$NAME2== "Mamprusi East"]<- "East Mamprusi"
ad_var1$NAME2[ad_var1$NAME2== "Tamale North Sub Metro"]<- "Tamale"
ad_var1$NAME2[ad_var1$NAME2== "Tatale"]<- "Tatale-Sangule"
ad_var1$NAME2[ad_var1$NAME2== "Tema Metropolis"]<- "Tema"
ad_var1$NAME2[ad_var1$NAME2== "Twifo Ati-Morkwa"]<- "Twifo-Ati-Mokwa"
ad_var1$NAME2[ad_var1$NAME2== "Twifo Lower Denkyira"]<- "Twifo-Hemang Lower Denkyira"
ad_var1$NAME2[ad_var1$NAME2== "Suhum Municipal"]<- "Suhum"
ad_var1$NAME2[ad_var1$NAME2== "Upper Manya"]<- "Upper Manya-Krobo"
ad_var1$NAME2[ad_var1$NAME2== "Wa Municipal"]<- "Wa"
ad_var1$NAME2[ad_var1$NAME2== "Wassa Amenfi Central"]<- "Wassa-Amenfi Central"
ad_var1$NAME2[ad_var1$NAME2== "Yendi Municipal"]<- "Yendi"
ad_var1$NAME2[ad_var1$NAME2== "Yilo Krobo"]<- "Yilo-Krobo"
ad_var1$NAME2[ad_var1$NAME2== "Daffiama Bussie"]<- "Daffiama-Bussie-Issa"
ad_var1$NAME2[ad_var1$NAME2== "Kma"]<- "Kumasi Metro"
ad_var1$NAME2[ad_var1$NAME2== "Sekyere Afram Plains North"]<- "Sekyere Kumawu"
ad_var1$NAME2[ad_var1$NAME2== "Afigya Sekyere"]<- "Sekyere South"
ad_var1$NAME2[ad_var1$NAME2== "Abura / Asebu / Kwamankese"]<- "Abura-Asebu-Kwamankese" 
ad_var1$NAME2[ad_var1$NAME2== "Asikuma / Odoben / Brakwa"]<- "Asikuma-Odoben-Brakwa" 
ad_var1$NAME2[ad_var1$NAME2== "Bosomtwe /Atwima / Kwanwoma"]<- "Bosomtwe" 
ad_var1$NAME2[ad_var1$NAME2== "Komenda Edna Eguafo / Abirem"]<- "Komenda-Edna-Eguafo-Abirem" 
ad_var1$NAME2[ad_var1$NAME2== "Ledzokuku / Krowor"]<- "Ledzokuku-Krowor" 
ad_var1$NAME2[ad_var1$NAME2== "Prestea / Huni Valley"]<- "Prestea-Huni Valley" 
ad_var1$NAME2[is.na(ad_var1$NAME2)] <- "Obuasi"

#which (unique(sort(cl$District))!=unique(sort(Med_cs$District)))
setdiff(sort(unique(cl$X)), unique(sort(ad_var1$NAME2)))
setdiff(unique(sort(ad_var1$NAME2)), sort(unique(cl$X)))
# Well everything is set and we will not have a missing value in the final dataset
Mg1 <- merge(x=cl,y=ad_var1,by.x="X",by.y="NAME2")

prVg<-read.csv("Precip_NDVI.csv")
Mg2<-merge(x=Mg1,y=prVg,by.x="ID",by.y="ID")
View(Mg2)
Mg2<-Mg2[,-c(8:16,24:33)]
write.csv(Mg2,"RandF.csv")
