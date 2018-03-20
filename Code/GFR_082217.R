library(HATOPO)

head(habitatDat)

table(habitatDat$use[habitatDat$id==1])
table(habitatDat$use[habitatDat$id==2])


hist(habitatDat$food[habitatDat$id==2])
populationDat
# FITTING A GENERALISED FUNCTIONAL RESPONSE MODEL (see Matthiopoulos et al. 2011)
# View the columns of the 'habitatDat' data frame
head(habitatDat)
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
form<-use~food+temp+I(temp^2)
form<-use~food+temp
# The second component is a vector of text strings,
# the names of the instance-specific covariates

addexp="N"
# The gfrModel can now be fit to the habitatDat. An order 2 GFR will incorporate
# 1st and 2nd expectations of covariates within eack sampling instance
gfrModel<-gfr(formula=formula,data=habitatDat,family=poisson, order=2, addexp=addexp,
              block=habitatDat$id, step=FALSE)

id<-as.numeric(habitatDat$id)
head(id)
gfrModel<-gfr(formula=formula,data=habitatDat,family=poisson, order=2, addexp=addexp,
              blockName=habitatDat$id, step=FALSE)

str(habitatDat)
str(id)
gfrModel<-gfr(formula=form,data=habitatDat,family=poisson,order=2,blockName = "id")
gfrModel$model
              
gfr.predict()
# Inspect quality of fit to observed usage (consider refitting the gfr with order 3 or higher)
smoothScatter(habitatDat$use,fitted(gfrModel$model), xlab="Observed usage", ylab="Fitted usage")
abline(0,1)
# FITTING A GENERALISED FUNCTIONAL RESPONSE MODEL (see Matthiopoulos et al. 2011)
# View the columns of the 'habitatDat' data frame
head(habitatDat)
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
formula<-use~food+temp+I(temp^2)
# The second component is a vector of text strings,
# the names of the instance-specific covariates
addexp="N"
# The gfrModel can now be fit to the habitatDat. An order 2 GFR will incorporate
# 1st and 2nd expectations of covariates within eack sampling instance
gfrModel<-gfr(formula=formula,
              data=habitatDat, family=poisson, order=2, addexp=addexp,
              block=habitatDat$id, step=FALSE)
# Inspect quality of fit to observed usage (consider refitting the gfr with order 3 or higher)
smoothScatter(habitatDat$use,fitted(gfrModel$model), xlab="Observed usage", ylab="Fitted usage")
abline(0,1)
# FITTING A POPULATION MODEL TO DEMOGRAPHIC DATA USING SPATIAL INFORMATION
# Step 1: Generate a Gaussian mixture approximation of the habitat composition
# of different instances. This is the parametric habitat availability summary
# collected in the object fa. A total of G=30 mixture components are used to
# describe habitat availability in each instance (more is better - and slower).
fa<-favail(habitatDat[,2:3], blocking=habitatDat$id, G=5)
pop.covariates 7
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
smoothScatter(datPopK$N2, KKspatial, nrpoints=500, bandwidth=40, ylim=yrange, xlim=yrange,
              xlab="Observed carrying capacity (K)", ylab="K predicted", main="Carrying capacity (spatial)")
abline(0,1)
smoothScatter(datPopK$N2, KKmean, nrpoints=500, bandwidth=40, ylim=yrange, xlim=yrange,
              xlab="Observed carrying capacity (K)", ylab="K predicted", main ="Carrying capacity (mean field)")
abline(0,1)
par(mfrow=c(1,1))