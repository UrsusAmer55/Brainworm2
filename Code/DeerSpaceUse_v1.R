library(HATOPO)
library(chron)
library(spatstat)
library(raster)
library(ggplot2)
library(sp)  # classes for spatial data
library(raster)  # grids, rasters
library(maptools)
library(rgeos)
library(geosphere)
library(rgdal)
library(chron)
library(plyr)
library(ggmap)
library(mapproj)
library(ctmm)
library(move)

?HATOPO


deerW<-read.csv("C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/AmandaRSF/deer files-20170822T165945Z-001/deer files/s3.winter.all.b.csv") 
head(deerW)
deerW$Hsd<-"NA"

deerS<-read.csv("C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/AmandaRSF/deer files-20170822T165945Z-001/deer files/s3.summer.all.b.csv") 
head(deerS)
deerS$X.1<-NULL
colnames(deerS)[2]<-c("animalID")
names(deerW)
deerS$sndpt<-"NA"

deerA<-rbind(deerS,deerW)

D2SP<-SpatialPointsDataFrame(coords=deerA[c("easting","northing")],proj4string= CRS("+proj=utm +zone=15 ellps=WGS84"),data=deerA)
#plot(D2SP,col=D2SP$y)
table(D2SP$y)
#get proper lat long
D2SPlatlong <- spTransform(D2SP, CRS("+proj=longlat +datum=WGS84"))
Dcoords<-as.data.frame(D2SPlatlong@coords)
head(Dcoords)
colnames(Dcoords)<-c("long","lat")
deerA<-cbind(deerA,Dcoords)



head(deerA)
str(deerA)


deerSum<-deerA[deerA$season=="summer",]
head(deerSum)

library(dplyr)
hab<-deerSum %>% 
  group_by(animalID,cover) %>% 
  summarise(Frequency = sum(y))

tot<-deerSum %>% 
  group_by(animalID) %>% 
  summarise(Frequency = sum(y))

hab<-as.data.frame(hab)
tot<-as.data.frame(tot)

per<-merge(hab,tot,by="animalID",all.x=TRUE)

colnames(per)<-c("animalID","cover","use","total")
per$percent<-per$use/per$total
head(per)

boxplot(percent~cover,data=per)

ave<-aggregate(x=per,by=list())
aggregate( percent~cover,data=per, FUN = "mean")
aggregate( percent~cover,data=per, FUN = "sd")


?aggregate
###re-do but also by region
unique(deerSum$region)

deerSum$region[deerSum$region=="ely"]<-"Ely"
deerSum$region[deerSum$region=="isabella"]<-"Isa"
deerSum$region<-droplevels(deerSum$region)
unique(deerSum$region)

hab2<-deerSum %>% 
  group_by(region,animalID,cover) %>% 
  summarise(Frequency = sum(y))

tot2<-deerSum %>% 
  group_by(region,animalID) %>% 
  summarise(Frequency = sum(y))

hab2<-as.data.frame(hab2)
tot2<-as.data.frame(tot2)

head(hab2)
head(tot2)

per2<-merge(hab2,tot2,by="animalID",all.x=TRUE)
head(per2)
per2$region.y<-NULL
colnames(per2)<-c("animalID","region","cover","use","total")
per2$percent<-per2$use/per2$total
head(per2)

boxplot(percent~region+cover,data=per2)

###re-do but also by migrator
unique(deerSum$migrators)

hab3<-deerSum %>% 
  group_by(migrators,animalID,cover) %>% 
  summarise(Frequency = sum(y))

tot3<-deerSum %>% 
  group_by(migrators,animalID) %>% 
  summarise(Frequency = sum(y))

hab3<-as.data.frame(hab3)
tot3<-as.data.frame(tot3)

head(hab3)
head(tot3)

per3<-merge(hab3,tot3,by="animalID",all.x=TRUE)
head(per3)
per3$migrators.y<-NULL
colnames(per3)<-c("animalID","migrators","cover","use","total")
per3$percent<-per3$use/per3$total
head(per3)

boxplot(percent~migrators+cover,data=per3)


deer<-deerA[deerA$season=="summer",]
deer$animalID<-as.factor(deer$animalID)

# FITTING A GENERALISED FUNCTIONAL RESPONSE MODEL (see Matthiopoulos et al. 2011)
# View the columns of the 'habitatDat' data frame
head(habitatDat)
str(habitatDat)
table(habitatDat$id)
# NOTE! The rows are sorted by id. This is necessary for later stages.
# You can make sure this is the case as follows:

deer<-deer[order(deer$animalID),]

# The columns contain
# id: the sampling instance
# food, water: covariate values that vary by location within a sampling instance
# N: the population density associated with the entire sampling instance
# use: the count of animals in each grid cell in the data

# Specification of a Generalised Functional Response model
# The user is required to specify two components:

# The first is a formula involving the main effects
# (i.e. those variable that change at different spatial locations within a sampling instance)
# Here, food and temperature are of interest.
# Note that a quadratic main effect for temperature is also included to capture the optimal
# temperature if discernible within the data (see Matthiopoulos et al 2015 for a more extensive
# discussion on resourses, risks and conditions)

#####Amanda's top model for summer and winter: y ~ cover + h75 + st1 + st2 + (1 | AnimalID)* **
formula<-y~cover + h75 + st1 + st2

# The second component is a vector of text strings,
# the names of the instance-specific covariates
addexp="N"

# The gfrModel can now be fit to the habitatDat. An order 2 GFR will incorporate
# 1st and 2nd expectations of covariates within eack sampling instance
str(deer)
str(habitatDat)
deer$coverN<-as.numeric(deer$cover)
formula<-y~coverN + h75 + st1 + st2
deer$blk<-1
deer$blk<-as.integer(deer$blk)

gfrModel<-gfr(formula=formula,
              data=deer, order=2,family=gaussian,
              blockName=deer$blk,step=FALSE)

# Inspect quality of fit to observed usage (consider refitting the gfr with order 3 or higher)
smoothScatter(habitatDat$use,fitted(gfrModel$model), xlab="Observed usage", ylab="Fitted usage")
abline(0,1)


# FITTING A POPULATION MODEL TO DEMOGRAPHIC DATA USING SPATIAL INFORMATION
# Step 1: Generate a Gaussian mixture approximation of the habitat composition
# of different instances. This is the parametric habitat availability summary
# collected in the object fa. A total of G=30 mixture components are used to
# describe habitat availability in each instance (more is better - and slower).
fa<-favail(habitatDat[,2:3], blocking=habitatDat$id, G=5)
head(habitatDat)
unique(habitatDat$id)
unique(habitatDat$use)
# Step 2: Extract the combined coefficients of the model's main effects
# for each sampling instance from the GFR
ga<-ga.gfr(gfrModel$model, gfrModel$expectations)

# Step 3: Calculate the constructed covariate dataframe for the population model
pop.frame<-pop.covariates(fa, ga)

# Step 4: Combine the constructed covariates
# (describing the effects of the habitat composition)
# with the population data for each sampling instance
datPop<-cbind(populationDat, pop.frame)
# Perhaps, exclude very small population sizes, to avoid influencing the model by
# landscapes that were not used due to low overall density
datPop<-na.omit(datPop[datPop$N1>100,])

# Step 5: Fit the population model using constructed covariates for growth
# and density dependence (See Matthiopoulos et al. 2015)
spatial<-glm(N2~food+temp+temp_2+dens_dep:N1, family=poisson(), offset=log(N1), data=datPop)
# For comparison, a non-spatial version of the model is fitted
# (using just the averages of covariates)
meanfield<-glm(N2~AvFood+AvTemp+AvTemp2+N1, family=poisson(), offset=log(N1), data=datPop)

# Step 6: Some quality of fit plots
xax<-log(datPop$N2/datPop$N1)
yrange<-c(min(xax),max(xax))
par(mfrow=c(2,2))
# Plotting predictions for population growth rate
smoothScatter(xax, log(fitted(spatial)/datPop$N1), ylim=yrange, ylab="Predicted",
              xlab="Observed log(r)", main="Growth Rate (spatial)")
abline(0,1)
smoothScatter(xax, log(fitted(meanfield)/datPop$N1), ylim=yrange, ylab="Predicted",
              xlab="Observed log(r)", main="Growth Rate (mean field)")
abline(0,1)
# Plotting predictions for population carrying capacity
datPopK<-datPop[datPop$year>10,]
KKspatial<-(cbind(datPopK[,1]*0+1, datPopK$food, datPopK$temp, datPopK$temp_2)%*%
              spatial$coefficients[1:4])/(-spatial$coefficients[5]*datPopK$dens_dep)
KKmean<-(cbind(datPopK[,1]*0+1, datPopK$AvFood, datPopK$AvTemp, datPopK$AvTemp^2)%*%
           meanfield$coefficients[1:4])/(-meanfield$coefficients[5])
yrange<-c(0.9*min(datPopK$N2),1.1*max(datPopK$N2))
smoothScatter(datPopK$N2, KKspatial,  nrpoints=500, bandwidth=40, ylim=yrange, xlim=yrange,
              xlab="Observed carrying capacity (K)", ylab="K predicted", main="Carrying capacity (spatial)")
abline(0,1)
smoothScatter(datPopK$N2, KKmean,  nrpoints=500, bandwidth=40, ylim=yrange, xlim=yrange,
              xlab="Observed carrying capacity (K)", ylab="K predicted", main ="Carrying capacity (mean field)")
abline(0,1)
par(mfrow=c(1,1))
