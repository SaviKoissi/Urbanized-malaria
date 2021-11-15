library(tidyverse)
library(brooms)
library(sf)
library(rmapshaper)
library(tmap)
library(ggpubr)
library(cowplot)
library(heatmaply)
library(micromap)
library(gridExtra)
library(gridGraphics)
library(grid)
library(ComplexHeatmap)


dshp<-st_read("C:/Users/ZEF/Desktop/Related_ms2/Data/District Shapefile/Districts/Map_of_Districts_216.shp")
dshp <-ms_simplify(dshp)
object.size(dshp)

dshp<-dshp %>% 
  mutate(NAME = case_when(
    NAME=="Accra Metropolis" ~ "Accra Metro", 
    NAME=="Adenta" ~ "Adentan", 
    NAME=="Afadzato South" ~ "Afadjato South",
    NAME=="Afigya Kwabre" ~ "Afigya-Kwabre",
    NAME=="Kwahu Afram Plains South" ~ "Afram Plains South",
    NAME=="Agotime Ziope" ~ "Agortime-Ziope",
    NAME=="Ahafo Ano North" ~ "Ahafo-Ano North",
    NAME=="Ahafo Ano South" ~ "Ahafo-Ano South",
    NAME=="Ajumako-Enyan-Esiam" ~ "Ajumako-Enyan-Essiam",
    NAME=="Akwapem North" ~ "Akwapim North",
    NAME=="Akwapem South" ~ "Akwapim South",
    NAME=="Akyem Mansa" ~ "Akyemansa",
    NAME=="Asante Akim Central Municipal" ~ "Asante Akim Central", 
    NAME=="Asokore Mampong Municipal" ~ "Asokore-Mampong",
    NAME=="Atebubu Amantin" ~ "Atebubu-Amanten", 
    NAME=="Atwima Kwanwoma" ~ "Atwima-Kwanwoma", 
    NAME=="Awutu Senya East Municipal" ~ "Awutu Senya East",
    NAME=="Bawku Municipal" ~ "Bawku",
    NAME=="Bekwai Municipal" ~ "Bekwai",
    NAME=="Birim Municipal"  ~ "Birim Central",
    NAME=="Bolgatanga Municipal"  ~ "Bolgatanga",
    NAME=="Bosomtwe /Atwima-Kwanwoma"  ~  "Bosomtwe",
    NAME=="Cape Coast Metro"  ~  "Cape Coast",
    NAME=="Bunkpurugu Yonyo"  ~   "Bunkpurugu-Yunyoo",
    NAME=="Asante-Mampong"  ~   "Mampong Municipal",
    NAME=="Effutu"  ~   "Efutu",
    NAME=="Ejisu Juaben" ~   "Ejisu-Juaben",
    NAME=="Ejura Sekye Dumase" ~  "Ejura-Sekyedumase",
    NAME=="Ga Central Municipal"  ~  "Ga Central", 
    NAME=="Garu Tempane"  ~  "Garu-Tempane", 
    NAME=="Gonja Central" ~  "Central Gonja",
    NAME=="Ho Municipal" ~  "Ho",
    NAME=="Hohoe Municipal" ~  "Hohoe",
    NAME=="Juabeso" ~  "Juaboso",
    NAME=="Kasena Nankana East" ~  "Kasena-Nankana",
    NAME=="Kasena Nankana West" ~  "Kasena-Nankana West",
    NAME=="Keta Municipal" ~  "Keta",
    NAME== "Komenda Edna Eguafo-Abirem" ~  "Komenda-Edna-Eguafo-Abirem",
    NAME=="Kpando Municipal" ~  "Kpando",
    NAME=="Kpone Katamanso" ~  "Kpone-Katamanso",
    NAME=="Kumbumgu" ~  "Kumbungu",
    NAME=="Kwabre" ~  "Kwabre East",
    NAME=="Kwahu Afram Plains North" ~  "Kwahu North",
    NAME=="La Dade Kotopon" ~  "La-Dade-Kotopon",
    NAME=="La Nkwantanang Madina" ~  "La-Nkwantanang-Madina",
    NAME=="Lambussie Karni" ~  "Lambussie-Karni",
    NAME=="Lower Manya" ~  "Lower-Manya Krobo",
    NAME=="Mampong Municipal" ~  "Asante-Mampong",
    NAME=="Mamprugu Moagduri" ~  "Mamprugu-Moagduri",
    NAME=="New Juaben Municipal" ~  "New Juaben",
    NAME=="Nsawam Adoagyiri" ~  "Nsawam-Adoagyiri",
    NAME=="Obuasi, Ashanti" ~  "Obuasi",
    NAME=="Offinso Municipal" ~  "Offinso",
    NAME=="Sagnerigu" ~  "Sagnarigu",
    NAME=="Savelugu Nanton" ~  "Savelugu-Nanton",
    NAME=="Sawla/Tuna/Kalba" ~  "Sawla-Tuna-Kalba" ,
    NAME=="Sefwi Akontombra" ~  "Sefwi-Akontombra",
    NAME=="Sefwi Bibiani-Anhwiaso Bekwai" ~  "Bibiani-Anhwiaso-Bekwai",
    NAME=="Sekondi Takoradi" ~  "Sekondi-Takoradi",
    NAME== "Shai Osu Doku" ~  "Shai-Osudoku",
    NAME=="Sissala  West" ~  "Sissala West",
    NAME== "Suhum Municipal" ~  "Suhum",
    NAME== "Tarkwa Nsuaem" ~  "Tarkwa-Nsuaem",
    NAME== "Mamprusi East" ~  "East Mamprusi",
    NAME== "Tamale North Sub Metro" ~  "Tamale",
    NAME== "Tatale" ~  "Tatale-Sangule",
    NAME== "Tema Metropolis" ~  "Tema",
    NAME== "Twifo Ati-Morkwa" ~  "Twifo-Ati-Mokwa",
    NAME== "Twifo Lower Denkyira" ~  "Twifo-Hemang Lower Denkyira",
    NAME== "Suhum Municipal" ~  "Suhum",
    NAME== "Upper Manya" ~  "Upper Manya-Krobo",
    NAME== "Wa Municipal" ~  "Wa",
    NAME== "Wassa Amenfi Central" ~  "Wassa-Amenfi Central",
    NAME== "Yendi Municipal" ~  "Yendi",
    NAME== "Yilo Krobo" ~  "Yilo-Krobo",
    NAME== "Daffiama Bussie" ~  "Daffiama-Bussie-Issa",
    NAME== "Kma" ~  "Kumasi Metro",
    NAME== "Sekyere Afram Plains North" ~  "Sekyere Kumawu",
    NAME== "Afigya Sekyere" ~  "Sekyere South",
    NAME== "Abura / Asebu / Kwamankese" ~  "Abura-Asebu-Kwamankese" ,
    NAME== "Asikuma / Odoben / Brakwa" ~  "Asikuma-Odoben-Brakwa" ,
    NAME== "Bosomtwe /Atwima / Kwanwoma" ~  "Bosomtwe" ,
    NAME== "Komenda Edna Eguafo / Abirem" ~  "Komenda-Edna-Eguafo-Abirem" ,
    NAME== "Ledzokuku / Krowor" ~  "Ledzokuku-Krowor" ,
    NAME== "Prestea / Huni Valley" ~  "Prestea-Huni Valley", 
    is.na(NAME) ~  "Obuasi",
    TRUE ~ NAME
  )) 

#plot(dshp$geometry)

pop<-read_csv("C:/Users/ZEF/Desktop/MS3/WorlPop/Popu_15-18_gha.csv")

pop_density<- pop %>% 
  group_by(NAME) %>% 
  mutate(NAME = case_when(
    NAME=="Accra Metropolis" ~ "Accra Metro", 
    NAME=="Adenta" ~ "Adentan", 
    NAME=="Afadzato South" ~ "Afadjato South",
    NAME=="Afigya Kwabre" ~ "Afigya-Kwabre",
    NAME=="Kwahu Afram Plains South" ~ "Afram Plains South",
    NAME=="Agotime Ziope" ~ "Agortime-Ziope",
    NAME=="Ahafo Ano North" ~ "Ahafo-Ano North",
    NAME=="Ahafo Ano South" ~ "Ahafo-Ano South",
    NAME=="Ajumako-Enyan-Esiam" ~ "Ajumako-Enyan-Essiam",
    NAME=="Akwapem North" ~ "Akwapim North",
    NAME=="Akwapem South" ~ "Akwapim South",
    NAME=="Akyem Mansa" ~ "Akyemansa",
    NAME=="Asante Akim Central Municipal" ~ "Asante Akim Central", 
    NAME=="Asokore Mampong Municipal" ~ "Asokore-Mampong",
    NAME=="Atebubu Amantin" ~ "Atebubu-Amanten", 
    NAME=="Atwima Kwanwoma" ~ "Atwima-Kwanwoma", 
    NAME=="Awutu Senya East Municipal" ~ "Awutu Senya East",
    NAME=="Bawku Municipal" ~ "Bawku",
    NAME=="Bekwai Municipal" ~ "Bekwai",
    NAME=="Birim Municipal"  ~ "Birim Central",
    NAME=="Bolgatanga Municipal"  ~ "Bolgatanga",
    NAME=="Bosomtwe /Atwima-Kwanwoma"  ~  "Bosomtwe",
    NAME=="Cape Coast Metro"  ~  "Cape Coast",
    NAME=="Bunkpurugu Yonyo"  ~   "Bunkpurugu-Yunyoo",
    NAME=="Asante-Mampong"  ~   "Mampong Municipal",
    NAME=="Effutu"  ~   "Efutu",
    NAME=="Ejisu Juaben" ~   "Ejisu-Juaben",
    NAME=="Ejura Sekye Dumase" ~  "Ejura-Sekyedumase",
    NAME=="Ga Central Municipal"  ~  "Ga Central", 
    NAME=="Garu Tempane"  ~  "Garu-Tempane", 
    NAME=="Gonja Central" ~  "Central Gonja",
    NAME=="Ho Municipal" ~  "Ho",
    NAME=="Hohoe Municipal" ~  "Hohoe",
    NAME=="Juabeso" ~  "Juaboso",
    NAME=="Kasena Nankana East" ~  "Kasena-Nankana",
    NAME=="Kasena Nankana West" ~  "Kasena-Nankana West",
    NAME=="Keta Municipal" ~  "Keta",
    NAME== "Komenda Edna Eguafo-Abirem" ~  "Komenda-Edna-Eguafo-Abirem",
    NAME=="Kpando Municipal" ~  "Kpando",
    NAME=="Kpone Katamanso" ~  "Kpone-Katamanso",
    NAME=="Kumbumgu" ~  "Kumbungu",
    NAME=="Kwabre" ~  "Kwabre East",
    NAME=="Kwahu Afram Plains North" ~  "Kwahu North",
    NAME=="La Dade Kotopon" ~  "La-Dade-Kotopon",
    NAME=="La Nkwantanang Madina" ~  "La-Nkwantanang-Madina",
    NAME=="Lambussie Karni" ~  "Lambussie-Karni",
    NAME=="Lower Manya" ~  "Lower-Manya Krobo",
    NAME=="Mampong Municipal" ~  "Asante-Mampong",
    NAME=="Mamprugu Moagduri" ~  "Mamprugu-Moagduri",
    NAME=="New Juaben Municipal" ~  "New Juaben",
    NAME=="Nsawam Adoagyiri" ~  "Nsawam-Adoagyiri",
    NAME=="Obuasi, Ashanti" ~  "Obuasi",
    NAME=="Offinso Municipal" ~  "Offinso",
    NAME=="Sagnerigu" ~  "Sagnarigu",
    NAME=="Savelugu Nanton" ~  "Savelugu-Nanton",
    NAME=="Sawla/Tuna/Kalba" ~  "Sawla-Tuna-Kalba" ,
    NAME=="Sefwi Akontombra" ~  "Sefwi-Akontombra",
    NAME=="Sefwi Bibiani-Anhwiaso Bekwai" ~  "Bibiani-Anhwiaso-Bekwai",
    NAME=="Sekondi Takoradi" ~  "Sekondi-Takoradi",
    NAME== "Shai Osu Doku" ~  "Shai-Osudoku",
    NAME=="Sissala  West" ~  "Sissala West",
    NAME== "Suhum Municipal" ~  "Suhum",
    NAME== "Tarkwa Nsuaem" ~  "Tarkwa-Nsuaem",
    NAME== "Mamprusi East" ~  "East Mamprusi",
    NAME== "Tamale North Sub Metro" ~  "Tamale",
    NAME== "Tatale" ~  "Tatale-Sangule",
    NAME== "Tema Metropolis" ~  "Tema",
    NAME== "Twifo Ati-Morkwa" ~  "Twifo-Ati-Mokwa",
    NAME== "Twifo Lower Denkyira" ~  "Twifo-Hemang Lower Denkyira",
    NAME== "Suhum Municipal" ~  "Suhum",
    NAME== "Upper Manya" ~  "Upper Manya-Krobo",
    NAME== "Wa Municipal" ~  "Wa",
    NAME== "Wassa Amenfi Central" ~  "Wassa-Amenfi Central",
    NAME== "Yendi Municipal" ~  "Yendi",
    NAME== "Yilo Krobo" ~  "Yilo-Krobo",
    NAME== "Daffiama Bussie" ~  "Daffiama-Bussie-Issa",
    NAME== "Kma" ~  "Kumasi Metro",
    NAME== "Sekyere Afram Plains North" ~  "Sekyere Kumawu",
    NAME== "Afigya Sekyere" ~  "Sekyere South",
    NAME== "Abura / Asebu / Kwamankese" ~  "Abura-Asebu-Kwamankese" ,
    NAME== "Asikuma / Odoben / Brakwa" ~  "Asikuma-Odoben-Brakwa" ,
    NAME== "Bosomtwe /Atwima / Kwanwoma" ~  "Bosomtwe" ,
    NAME== "Komenda Edna Eguafo / Abirem" ~  "Komenda-Edna-Eguafo-Abirem" ,
    NAME== "Ledzokuku / Krowor" ~  "Ledzokuku-Krowor" ,
    NAME== "Prestea / Huni Valley" ~  "Prestea-Huni Valley", 
    is.na(NAME) ~  "Obuasi",
    TRUE ~ NAME
  )) %>% 
  mutate(density_glo = (Estimate*1e06)/AREA_M2) %>% 
  summarise(density = mean(density_glo))

covar_dt<-read_csv("C:/Users/ZEF/Downloads/koissimap.csv")

semi<-dshp %>% 
  left_join(pop_density) 

complete<- merge(semi, covar_dt, by.x = "NAME", by.y = "District")

dat_chlo<-complete %>% 
  filter(RGN_NM2012 == "Greater Accra" | RGN_NM2012 == "Ashanti")

dat_chlo %>% 
  ggplot(aes(x = Med_Inc, y = density)) + 
  geom_point(aes(size= BuiltAr)) +
  #geom_smooth()+
  facet_wrap(~RGN_NM2012)+
  theme_bw()+
  xlab("Median malaira incidence")+
  labs(size = "Built area\n")

+
  geom_cor(method = "Pearson", ypos = 1e5) 

  guides(color = guide_legend(override.aes = list(size = 3)))

dat_chlo %>% 
  group_by(RGN_NM2012) %>% 
  do(tidy(cor.test(x = .$Med_Inc,y = .$density)))

dat_chlo %>% 
  group_by(RGN_NM2012) %>% 
  do(tidy(cor.test(x = .$Med_Inc,y = .$BuiltAr)))

dat_chlo %>% 
  group_by(RGN_NM2012) %>% 
  do(tidy(cor.test(x = .$Med_Inc,y = .$BuiltInten)))

dat_chlo %>% 
  group_by(RGN_NM2012) %>% 
  do(tidy(cor.test(x = .$Med_Inc,y = .$NDVI)))

dat_chlo %>% 
  group_by(RGN_NM2012) %>% 
  do(tidy(cor.test(x = .$Med_Inc,y = .$ITN)))

dat_chlo %>% 
  group_by(RGN_NM2012) %>% 
  do(tidy(cor.test(.$Med_Inc, .$Precip)))

dat_chlo %>% 
  group_by(RGN_NM2012) %>% 
  do(tidy(cor.test(x = .$Med_Inc,y = .$Water)))

dat_chlo %>% 
  group_by(RGN_NM2012) %>% 
  do(tidy(cor.test(x = .$Med_Inc,y = .$Toilet)))

dat_chlo %>% 
  group_by(RGN_NM2012) %>% 
  do(tidy(cor.test(x = .$Med_Inc,y = .$vaccinations)))

dat_chlo %>% 
  group_by(RGN_NM2012) %>% 
  do(tidy(cor.test(x = .$Med_Inc,y = .$Healthcare)))

dat_chlo %>% 
  ggplot(aes(x =Med_Inc , y = density)) + 
  geom_point(aes(color= BuiltInten)) +
  #geom_smooth()+
  facet_wrap(~RGN_NM2012)+
  theme_bw()+
  xlab("Median malaira incidence")+
  #labs(color = "Built intensity\n")+
  guides(color = guide_legend(override.aes = list(size = 3)))
%>% 
  ggscatter( x = "Med_Inc", y = "density", data = dat_chlo, cor.coef = TRUE, cor.method = "pearson")


plot1<-dat_chlo %>% 
  ggplot(aes(x = Med_Inc, y = BuiltAr)) + 
  geom_point() +
  facet_wrap(~RGN_NM2012)+
  theme_bw()+
  xlab("Median malaira incidence")+
  ylab( "Built areas")

plot2<-dat_chlo %>% 
  ggplot(aes(x = Med_Inc, y = BuiltInten)) + 
  geom_point() +
  facet_wrap(~RGN_NM2012)+
  theme_bw()+
  xlab("Median malaira incidence")+
  ylab( "Built intensity")

plot3<-dat_chlo %>% 
  ggplot(aes(x = Med_Inc, y = ITN)) + 
  geom_point() +
  facet_wrap(~RGN_NM2012)+
  theme_bw()+
  xlab("Median malaira incidence")

plot4<-dat_chlo %>% 
  ggplot(aes(x = Med_Inc, y = NDVI)) + 
  geom_point() +
  facet_wrap(~RGN_NM2012)+
  theme_bw()+
  xlab("Median malaira incidence")

plot_grid(plot1, plot2,plot3, plot4, labels = "AUTO")


dat_chlo %>% 
  ggplot(aes(x = Med_Inc, y = NDVI)) + 
  geom_point(aes(size = ITN)) +
  facet_wrap(~RGN_NM2012)+
  theme_bw()+
  xlab("Median malaira incidence")+
  labs(size= "Coverage of ITN\n")+
  guides(color = guide_legend(override.aes = list(size = 3)))

dat_chlo %>% 
  ggplot(aes(x = Med_Inc, y = NDVI)) + 
  geom_point(aes(color = Toilet)) +
  facet_wrap(~RGN_NM2012)+
  theme_bw()+
  xlab("Median malaira incidence")+
  labs(color= "Lack of toilet \n")+
  guides(color = guide_legend(override.aes = list(size = 3)))

dat_chlo %>% 
  ggplot(aes(x = Med_Inc, y = ITN)) + 
  geom_point() +
  facet_wrap(~RGN_NM2012)+
  theme_bw()+
  xlab("Median malaira incidence")+
  labs(color= "Lack of toilet \n")

dat_chlo %>% 
  ggplot(aes(x = Med_Inc, y = BuiltAr)) + 
  geom_point() +
  facet_wrap(~RGN_NM2012)+
  theme_bw()+
  xlab("Median malaira incidence")+
  labs(color= "Lack of toilet \n")

dat_chlo %>% 
  ggplot(aes(x = Med_Inc, y = BuiltInten)) + 
  geom_point() +
  facet_wrap(~RGN_NM2012)+
  theme_bw()+
  xlab("Median malaira incidence")+
  labs(color= "Lack of toilet \n")+
  geom_text(aes(label=unique(NAME)),hjust=0, vjust=0)

complete %>% 
  tm_shape() + tm_polygons(col='RGN_NM2012', title = "Region"
              , palette = c("skyblue", "white","white","grey","skyblue","grey90","grey80","grey70","grey60","grey70")) + tm_scale_bar(position = c("right", "bottom")) +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.4,
            legend.position = c("right","top"),
            legend.bg.color = "white")

d1<-dat_chlo %>% 
  mutate(Med_IncR = normalize(Med_Inc)) %>% 
  tm_shape() + tm_polygons(col='Med_IncR', title = "Normalized Incidence", 
                            breaks =c(0,0.1,.2,.3,.4,.5,.6,
                                       .7,.8,.9,1), legend.show = FALSE
                           ) +   tm_scale_bar(,position = c("left", "bottom"))+tm_layout("Incidence") 
d2<-dat_chlo %>% 
  mutate(densityR = normalize(density)) %>% 
  tm_shape() + tm_polygons(col='densityR', title = "Normalized value", 
                           breaks =c(0,0.1,.2,.3,.4,.5,.6,                                                                       .7,.8,.9,1)) +   tm_scale_bar(position = c("left", "bottom")) +tm_layout("Density")
d3<-dat_chlo %>% 
  mutate(NDVIR = normalize(NDVI)) %>% 
  tm_shape() + tm_polygons(col='NDVIR', title = "Normalized NDVI",
                           breaks =c(0,0.1,.2,.3,.4,.5,.6,                                                                       .7,.8,.9,1), legend.show = FALSE )+ tm_scale_bar(position = c("left", "bottom"))+ 
  tm_layout("NDVI") 

d4<-dat_chlo %>% 
  mutate(ITNR = normalize(ITN)) %>% 
  tm_shape() + tm_polygons(col='ITNR', title = "Normalized ITN",
                        breaks =c(0,0.1,.2,.3,.4,.5,.6,.7,.8,.9,1), 
                        legend.show = FALSE) + tm_scale_bar(position = c("left", "bottom"))+
  tm_layout("ITN")
d5<-dat_chlo %>% 
  mutate(BuiltArR = normalize(BuiltAr)) %>% 
  tm_shape() + tm_polygons(col='BuiltArR', title = "Normalized Built Areas",
                           breaks =c(0,0.1,.2,.3,.4,.5,.6,.7,.8,.9,1), legend.show=FALSE
                           ) + tm_scale_bar(position = c("left", "bottom"))+tm_layout("Built Areas")

d6<-dat_chlo %>% 
  mutate(BuiltIntenR = normalize(BuiltInten)) %>% 
  tm_shape() + tm_polygons(col='BuiltIntenR', title = "Normalized Built Intensity",
                           breaks =c(0,0.1,.2,.3,.4,.5,.6,.7,.8,.9,1), legend.show=FALSE
                           ) + tm_scale_bar(position = c("left", "bottom"))+tm_layout("Built Intensity")

tmap_arrange(d1,d2)
tmap_arrange(d3, d4)
tmap_arrange(d5,d6)
             ,t1,t2)
tmap_arrange(d5,d6,t1,t2)
tmap_arrange(d5,d6)

grid.arrange (t1,t2)
arrangeGrob(t1,t2)
,d6,t1,t2)

d5<-dat_chlo%>% 
  mutate(clustnew = as.factor(clustnew))%>% 
  tm_shape() + tm_polygons(col='clustnew', title = "Clustering"
                           , palette = "magma") + tm_scale_bar(position = c("left", "bottom"))
tmap_arrange(d1,d5)

dataset<-dat_chlo %>% 
  as_tibble() %>% 
  select(NAME, density, BuiltAr, BuiltInten, ITN, 
          NDVI, Med_Inc) 

trans_matr<-t(as.matrix(dataset[,-1]))
norm_trans_matr<-normalize(trans_matr)
colnames(norm_trans_matr) = dataset$NAME
  m<-cor(norm_trans_matr) 
  t2<-heatmap(m)
  
matr<-as.matrix(dataset[,-1])
  norm_matr<-normalize(matr)
  rownames(norm_matr) = dataset$NAME
  t1<-heatmap(as.matrix(norm_matr))
  #heatmaply(
   # norm_matr, #dendrogram = "none",
    #xlab = "Covariates", 
    #ylab = "Districts"
     # )
  datasetR<-dataset %>% 
    mutate(across(where(is.double), normalize))
    
  heatmap(as.matrix(norm_matr))+heatmap(m)
  