# Multilevel model in epeidemics 

rm(list=ls())
path<-"C:/Users/ZEF/Desktop/Related_ms2/Data"
setwd(path)
data<-read.csv("Epi_data2.csv")
require(lubridate)
data$days <- paste(data$Month,"/", data$Year)
data$days<-parse_date_time(data$days,orders=c( "bY"))
startdate <- as.Date("2014/12/31","%Y/%m/%d")
data$date <- format(as.POSIXct(data$days,format='%Y/%m/%d %UTC'),format='%Y/%m/%d')
data$date2 <-  as.Date(data$date,"%Y/%m/%d")
data$NumDays  <- as.numeric(difftime(data$date2,startdate ,units="days"))


# Check the spatial heterogeneity with multilevel model
# Null-model or unconditional model
require(lme4)
require(nlme)
require(jtools)
md0<-lme(fixed = IncR~1, random = ~1|District, na.action="na.omit", data=data2)
summary(md0)
#Md0<-lmer(Incidence~1 + (1|District), na.action="na.omit", data=data)
#summ(Md0)
# The average incidence rate is 31.08081
vc<-VarCorr(md0);vc
Var_dtr<-as.numeric(vc[1,1]);Var_dtr
Var_rz<-as.numeric(vc[2,1]);Var_rz
ICC<-100*Var_dtr/(Var_dtr+Var_rz);ICC # The spatial correlation needed to be considered since the ICC is 17.52%
# Test of random intercepts 
require(lattice)
#xyplot(Incidence~Year|District, data=data, na.action="na.omit",type=c("p", "r"))#To visualize over time how the incidence behave
require(lmerTest)
ranova(Md0)
# We are going to check the better structure of the matrix of residual effect R
#Unconditional growth random slope
md1<-lme(fixed = IncR~Year, random = ~Year|District, na.action="na.omit", data=data2)
md1_00<-lme(fixed = IncR~Year, random = ~District|Year, na.action="na.omit", data=data2)
summary(md1)
Md1<-lmer(IncR~Year + (1+Year|District), na.action="na.omit", data=data)
summary(Md1)
summ(Md1)
##Unconditional growth random slope
md11<-lme(fixed = IncR~Year, random = ~1|District, na.action="na.omit", data=data)
summary(md11)
intervals(md11)
#intervals(md1)
# Let us check the deviance statistics
result1<-anova(md0,md1)# Compare the unconditional model to the unconditional growth with random slope
result2<-anova(md0,md11)# Compare the unconditional model to the unconditional growth with fixed  slope
result3<-anova(md1,md11) # we can chose either model for the rest of the analysis however a random slope looks better fit
vc2<-VarCorr(md1);vc2
Var_dtr2<-as.numeric(vc2[1,1]);Var_dtr2
Var_rz2<-as.numeric(vc2[2,1]);Var_rz2
ICC2<-100*Var_dtr2/(Var_dtr2+Var_rz2);ICC2

#vc2<-VarCorr(md1);vc2
#Var.Residual1<-as.numeric(vc2[3,1]); Var.Residual1
#Var.Time<-100*(Var_rz-Var.Residual1)/Var_rz;Var.Time # The spatial heterogeneity is not bounded to the time since the variability associated to time represents only 0.18%
# Conditional growth model
# Matrix R structure Independence
md2<-lme(fixed=IncR~Year*sex*Age, random=~Year|District, na.action="na.omit", data=data)
#First order auto regressive
ctrl <- lmeControl(opt='optim')
md3<-lme(fixed=IncR~Year*sex*Age, random=~Year|District, na.action="na.omit",control=ctrl, correlation=corAR1(),data=data2)
Md3<-lmer(IncR~Year+sex+Age + (Year|District), na.action="na.omit",REML=FALSE, data=data2)
# First order moving average
md4<-lme(fixed=IncR~Year*sex*Age, random=~Year|District, na.action="na.omit", control=ctrl,correlation=corARMA(p=0,q=1),data=data)
# First order auto regressive moving average
md5<-lme(fixed=IncR~Year*sex*Age, random=~Year|District, na.action="na.omit",control= lmeControl(maxIter = 1e8, msMaxIter = 1e8),correlation=corARMA(p=1,q=1),data=data) # Issue with the convergence of the matrix ==> Not the appropriate matrix
# Compound symmetry 
md6<-lme(fixed=IncR~Year*sex*Age, random=~Year|District, na.action="na.omit", control=ctrl,correlation=corCompSymm(),data=data)
# Best structure for the matrix of fixed effects
anova(md2,md3,md4,md6)

# Let us check the structure of the matrix G (matrix of random effect)
md7<-lme(fixed=IncR~Year*sex*Age, random=~Year|District, na.action="na.omit",  control=ctrl,correlation=corAR1(),weights = varIdent(form = ~1|District),data=data) # This structure of the matrix G is not possible since we have a problem of convergence. 
#anova(md3,md7)
anova(md3)
library(sjPlot)
tab_model(md0)
tab_model(md0,md1, md3, show.df = TRUE)
#require(afex)
library(texreg)
htmlreg(list(md0, md1, md3), 
        file='texreg.doc',
        single.row = T, 
        stars = numeric(0),
        caption = "",
        custom.note = "null model = intercept only,
                        unconditional growth=,
                         final model =  
                        
        ")

#install.packages("ggeffects", dependencies=TRUE)
require(ggeffects)
require(dplyr)
require(digest)
ggpredict(md3, c("sex","Age","Year"))%>% plot()
plot(p)
install.packages("digest", dependencies=TRUE)

md3_0<-lme(fixed=IncR~Year+sex+Age, random=~Year|District, na.action="na.omit",control=ctrl, correlation=corAR1(),data=data)
tab_model(md3_1)
tab_model(md0,md1, md3_1, show.df = TRUE)

md3_1<-lme(fixed=IncR~Year+sex+Age+sex*Age, random=~Year|District, na.action="na.omit",control=ctrl, correlation=corAR1(),data=data)
 
anova(md3, md3_0, md3_1)
