#install.packages("glmulti",dependencies = TRUE)
#install.packages("vortexR", dependencies = TRUE)
library(randomForest) 
library(dismo)
library(rms)
library(mgcv)
library(MASS)
library(glmulti)
library(lme4)
library(rgdal)

setwd("C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/Brainworm2/ProcessedData")
Dann<-readRDS("Dann_040618.R")
D2<-readRDS("D2_040618.R")
str(D2$death.age)
D2$death.age<-as.numeric(D2$death.age)
head(D2)
table(D2$PT)

D2$PTN<-NA
D2$PTN[D2$PT=="Yes"]<-1
D2$PTN[D2$PT=="No"]<-0
table(D2$PTN)
D2$PTNF<-factor(D2$PTN)



set.seed(30)
D2spr<-D2[D2$Season=="Spring",]
names(D2spr)
plot(rf_spr<-randomForest(PT~min.snow.date.jul+mean.nomigW+DeerHabLidWt.springTOT+AllDisturb+harvestAVEW+TOTALarea_km2+distLSW,data=D2spr, do.trace=10, ntree=500,mtry=4))
summary(rf_spr)
print(rf_spr)
round(importance(rf_spr),2)
print(importance(rf_spr,type = 2)) 
margin(rf_spr)
varImpPlot(rf_spr)

D2sum<-D2[D2$Season=="Summer",]
names(D2sum)
plot(rf_sum<-randomForest(PTNF~CentXW+CentYW+death.age+sex+min.snow.date.jul+mean.nomigW+DeerHabLidWt.summerTOT+AllDisturb+harvestAVEW+TOTALarea_km2+distLSW,data=D2sum, do.trace=10, ntree=1000,mtry=10))
summary(rf_sum)
print(rf_sum)
round(importance(rf_sum),2)
varImpPlot(rf_sum)


D2fall<-D2[D2$Season=="Fall",]
names(D2fall)
plot(rf_fall<-randomForest(PTNF~CentXW+CentYW+death.age+sex+min.snow.date.jul+mean.nomigW+DeerHabLidWt.fallTOT+AllDisturb+harvestAVEW+TOTALarea_km2+distLSW,data=D2fall, do.trace=10, ntree=1000,mtry=10))
summary(rf_fall)
print(rf_fall)
round(importance(rf_fall),2)
varImpPlot(rf_fall)

#all
D2$age.harvest<-D2$death.age*D2$harvestAVEW
D2$regen.distLS<-D2$RegenFrstW*D2$distLSW
D2$regen.CTISTANWetmeantW<-D2$RegenFrstW*D2$CTISTANWetmeantW
names(D2)
plot(rf_all<-randomForest(PTNF~min.snow.date.jul+CentXW+CentYW+age.harvest+harvestAVEW+regen.CTISTANWetmeantW+regen.distLS+RegenFrstW:death.age+RegenFrstW:distLSW+CTISTANWetmeantW:RegenFrstW,data=D2, do.trace=10, ntree=5000,mtry=5))
summary(rf_all)
print(rf_all)
round(importance(rf_all),2)
varImpPlot(rf_all)



fold<-kfold()

names(D2spr)

summer.glm <- glm(PTN~CentXW+CentYW+death.age+sex+min.snow.date.jul+mean.nomigW+DeerHabLidWt.summerTOT+AllDisturb+harvestAVEW+TOTALarea_km2+distLSW,data=D2sum, family = binomial)
summer.step <- stepAIC(summer.glm, trace = FALSE)
summer.step$anova
birthwt.step2 <- stepAIC(birthwt.glm, ~ .^2 + I(scale(age)^2)
                         + I(scale(lwt)^2), trace = FALSE)
birthwt.step2$anova

summer.glm.best <- glm(PTN~sex + DeerHabLidWt.springTOT, family = binomial,data=D2spr)
summary(summer.glm.best)

# #sort of working mixed effects model
# library(lme4)
# lmer.glmulti <- function (formula, data, random = "", ...) { 
#   glmer(paste(deparse(formula), random), family=binomial(link = "logit"),data = data, REML=F, ...) 
# } 
# 
# res <- glmulti(PTN~Season+CTISTANWetmeantW+min.snow.date.jul+RegenFrstW+distLSW, data=D2,
#                level=1,maxsize=3,method = "h", crit="aicc",confsetsize=50,fitfunc = lmer.glmulti, 
#                random = "+(1|MooseID)")

names(Dann)
Dann$distLSW
modspec<-"PTN~death.age+harvestAVEW+CentYW+CentXW+RegenWetCTI100+min.snow.date.jul+mean.migW+OpenWaterW"

names(D2)
res <- glmulti(modspec, data=Dann,
               level=2,maxsize=2,method = "h", crit="aicc",confsetsize=50, family=binomial(link = "logit"))
               
               

print(res)
plot(res)
tmp <- weightable(res)
tmp <- tmp[tmp$aicc <= min(tmp$aicc) + 2,]
tmp
plot(res, type="s")
weightable(res)
coef(res, select=2, varweighting="Buckland", icmethod="Burnham", alphaIC=0.05)
summary(res@objects[[1]])
summary(res@objects[[2]])
plot(res, type="s")
M1<-glm(PTN~RegenWetCTI100:death.age+RegenWetCTI100:CentYW, family = binomial("logit"),data=Dann)
summary(M1)
library(effects)
library("boot")
cost <- function(r, pi = 0) mean(abs(r - pi) > 0.5)  ## cost function necessary for binomial data
m1.cv <- cv.glm(data = Dann, M1, cost)
cv.glm(data = Dann, M1, cost, K = 6)$delta[1]# use leave-one-out cross validation (can use K-fold cross validation for larger data sets)
?cv.glm
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
m1.cv$delta
plot(allEffects(M1))
e.out <- allEffects(M1)
e.out$`RegenFrstW:death.age`$model.matrix

library(pscl)
pR2(M1)
library(survey)
regTermTest(M1, "harvestAVEW:death.age")


#best after a lot of runs: PTN~1+RegenFrstW:death.age+RegenFrstW:distLSW+CTISTANWetmeantW:RegenFrstW

#potentially look into re-doing the wet w/ forest layer but use the regen
#plots of brain worm by death.age*deerhavedensity

D2$CentYW.2<-D2$CentYW/1000000

mix1<-glmer(PTN~harvestAVEW*death.age + RegenFrstW +(1|MooseID),family = binomial("logit"),data=D2)
summary(mix1)

plot(allEffects(mix1))
e.out <- allEffects(mix1)
e.out$`RegenFrstW:death.age`$model.matrix


mix1<-glmer(PTN~death.age + deerharvSQMI2012W + deerharvSQMI2012W:death.age +(1|MooseID),family = binomial("logit"),data=D2)
summary(mix1)


library(boot)  
logit_test <- function(d,indices) {  
  d <- d[indices,]  
  fit <- glm(PTN~death.age + deerharvSQMI2012W + deerharvSQMI2012W:death.age, data = d, family = "binomial")  
  return(coef(fit))  
}

boot_fit <- boot(  
  data = D2spr, 
  statistic = logit_test, 
  R = 1e5
) 

boot_fit

#######test out with full set of indivs
setwd("C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/Brainworm2/ProcessedData")
Fdemo<-read.csv("Full_MooseInfo.csv")
str(Fdemo)

head(Fdemo)
#MB3.dat<-merge(MB3.dat,Fdemo,by.x="animalID",by.y="Moose_ID",all.x=TRUE)
head(Fdemo$deathtime)
Fdemo$deathtime<-paste(Fdemo$Date.of.death,Fdemo$time.of.death,sep=" ")
Fdemo$deathtime<-strptime(Fdemo$deathtime, "%m/%d/%Y %H:%M",tz="America/Chicago")
Fdemo$deathyear<-format(Fdemo$deathtime,"%Y")
names(Fdemo)
allFdemoSP<-SpatialPointsDataFrame(coords=Fdemo[,15:16],data=Fdemo,proj4string=CRS("+proj=utm +zone=15 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0 "))


deerharv14<-readOGR(dsn="C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/GIS_files/shp_env_mn_deer_harvest",layer="deer_harvest_2014")
plot(deerharv14)
plot(allcentSP,add=TRUE)

deerproj<-projection(deerharv14)
allcentSP2<-spTransform(allcentSP, crs(deerproj))

allFdemoSP_deer14<-extract(deerharv14,allFdemoSP)
names(allFdemoSP_deer14)
allFdemoSP_deer14<-allFdemoSP_deer14[,14]
allFdemoSP_deer14<-as.data.frame(allFdemoSP_deer14)
colnames(allFdemoSP_deer14)<-c("deerharvSQMI2014")


deerharv13<-readOGR(dsn="C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/GIS_files/shp_env_mn_deer_harvest",layer="deer_harvest_2013")
allFdemoSP_deer13<-extract(deerharv13,allFdemoSP)
names(allFdemoSP_deer13)
allFdemoSP_deer13<-allFdemoSP_deer13[,14]
allFdemoSP_deer13<-as.data.frame(allFdemoSP_deer13)
colnames(allFdemoSP_deer13)<-c("deerharvSQMI2013")


deerharv12<-readOGR(dsn="C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/GIS_files/shp_env_mn_deer_harvest",layer="deer_harvest_2012")
allFdemoSP_deer12<-extract(deerharv12,allFdemoSP)
names(allFdemoSP_deer12)
allFdemoSP_deer12<-allFdemoSP_deer12[,14]
allFdemoSP_deer12<-as.data.frame(allFdemoSP_deer12)
colnames(allFdemoSP_deer12)<-c("deerharvSQMI2012")

deerharv<-cbind(as.data.frame(allFdemoSP),allFdemoSP_deer12,allFdemoSP_deer13,allFdemoSP_deer14)
deerharv$harvestAVE<-(deerharv$deerharvSQMI2012+deerharv$deerharvSQMI2013+deerharv$deerharvSQMI2014)/3
hist(deerharv$harvestAVE)

Fdemo2<-deerharv

#Distance from centroid to Lake Superior
#read in a polygon of LS
LS<-readOGR("C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/GIS_files/shp_water_lake_superior_basin",layer="lake_superior_basin",)
head(str(LS))
plot(LS)
names(Fdemo2)
Fdemo2SP<-SpatialPointsDataFrame(coords=Fdemo2[,15:16],data=as.data.frame(Fdemo2),proj4string=CRS("+proj=utm +zone=15 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))
library(rgeos)
distLS<-as.vector(apply(gDistance(Fdemo2SP, LS,byid=TRUE),2,min))
str(distLS)
distLS2DF<-as.data.frame(distLS)
Fdemo3<-cbind(Fdemo2,distLS2DF)

Fdemo2$death.age<-as.numeric(Fdemo2$Age.at.Death)
head(Fdemo2)
table(Fdemo2$PT)

Fdemo2$PTN<-NA
Fdemo2$PTN[Fdemo2$PT=="Yes"]<-1
Fdemo2$PTN[Fdemo2$PT=="No"]<-0
table(Fdemo2$PTN)
Fdemo2$PTNF<-factor(Fdemo2$PTN)
Fdemo2$PTN

infect<-Fdemo2[Fdemo2$PTN==1,]
str(infect)
unique(infect$death.age)

names(Fdemo3)
table(Fdemo3$death.age,Fdemo3$PT)
Fdemo3$AgeGroup[Fdemo3$death.age<=5]<-"young"
Fdemo3$AgeGroup[Fdemo3$death.age>=6]<-"old"
Fdemo3$AgeGroup<-as.factor(Fdemo3$AgeGroup)

M2<-glm(PTN~distLS+death.age+death.age:harvestAVE + harvestAVE+distLS+deerharvSQMI2012, family = binomial("logit"),data=Fdemo3)
summary(M2)
Fdemo3$UTM.X.2<-Fdemo3$UTM.X/100000
Fdemo3$UTM.Y.2<-Fdemo3$UTM.Y/100000

Fdemo3$UTM.X.2<-scale(Fdemo3$UTM.X)
Fdemo3$UTM.Y.2<-scale(Fdemo3$UTM.Y)
Fdemo3$UTM.XY.2<-Fdemo3$UTM.X.2*Fdemo3$UTM.Y.2

Fdemo3$distLS.scale<-scale(Fdemo3$distLS)
Fdemo3$harvestAVE.scale<-scale(Fdemo3$harvestAVE)
hist(Fdemo3$deerharvSQMI2013.scale)

names(Fdemo3)
modspec<-"PTN~UTM.XY.2+death.age+harvestAVE"
#deerharvSQMI2014
res <- glmulti(modspec, data=Fdemo3,
               level=2,maxsize=3,method = "h", crit="aicc",confsetsize=50, family=binomial(link = "logit"))

# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)  
# (Intercept)       1.928e+00  1.188e+00   1.623   0.1047  
# harvestAVE:UTM.X -7.328e-06  3.552e-06  -2.063   0.0391 *

print(res)
plot(res)
tmp <- weightable(res)
tmp <- tmp[tmp$aicc <= min(tmp$aicc) + 2,]
tmp
summary(res@objects[[1]])
summary(res@objects[[2]])
summary(res@objects[[3]])
plot(res, type="s")
weightable(res)


plot(Fdemo3$UTM.X.2[1],Fdemo3$UTM.Y.2[1])

coef(res, select=2, varweighting="Buckland", icmethod="Burnham", alphaIC=0.05)
predict(res, select="all", newdata=NA, se.fit=TRUE,
        varweighting="Buckland", icmethod="Lukacs", alphaIC=0.05)

M2<-glm(PTN~harvestAVE:death.age, family = binomial(link = "logit"),data=Fdemo3)
summary(M2)

Mnull<-glm(death.age ~ 1, family = gaussian,data=infect)
summary(Mnull)
#parameter weight

plot(allEffects(M2))
#distLS + harvestAVE:distLS
#harvestAVE:death.age