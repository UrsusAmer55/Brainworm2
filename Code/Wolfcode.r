#================================================#
#     A GENERALISED FUNCTIONAL RESPONSE          #
#FOR THE WOLF DATA OF HEBBLEWHITE & MERILL (2008)#
#          ® Jason Matthiopoulos,                #
# Geert Aarts, John Fieberg, Mark Hebblewhite    #
#================================================#


#  1. DATA FRAME PREPARATION====================================================

# Import saved data
dat<-read.table( file="C:/wolfsummerrs.txt", header=TRUE, sep="\t")

# Reduced sample size
aces<-length(dat$use[dat$use>0])
ids<-c(seq(1,aces, dil), (aces+1):length(dat[,1]))
subda<-dat[ids,]   # creates a sparser data set for fitting
# Dealing with factors
lkup<-list(c(3,4,5), c(1,2), 15, 14, c(12, 13), 10, 9)
labs<-c("Burnt", "Alpine", "Shrub", "Rock", "Oconif", "Herbacious", "Cutblock")

for(k in 1:length(lkup))
  {
  ids<-which(subda[,14] %in% lkup[[k]])
  colNew<-rep(0, length(subda[,14]))
  colNew[ids]<-1
  ifelse (k==1, cols<-data.frame(colNew), cols<-cbind(cols, colNew))
  }
names(cols)<-labs
subda<-cbind(subda,cols)

# Standardising covariates (helps with model convergence)
subda$slope_a<-(subda$slope_a-mean(subda$slope_a))/sd(subda$slope_a)
subda$distedg<-(subda$distedg-mean(subda$distedg))/sd(subda$distedg)
subda$disthgh<-(subda$disthgh-mean(subda$disthgh))/sd(subda$disthgh)

# Calculating dataframe with covariate expectations
sub<-subset(data, use==0)
slope_E<-tapply(sub$slope_a, sub$packid, mean)
slope_E2<-tapply(sub$slope_a^2, sub$packid, mean)
distedg_E<-tapply(sub$distedg, sub$packid, mean)
distedg_E2<-tapply(sub$distedg^2, sub$packid, mean)
disthgh_E<-tapply(sub$disthgh, sub$packid, mean)
disthgh_E2<-tapply(sub$disthgh^2, sub$packid, mean)
inds<-as.character(subda$packid)
subda<-cbind(subda,"slopeE"=slope_E[inds],"slopeE2"=slope_E2[inds],"distedgE"=distedg_E[inds],"distedgE2"=distedg_E2[inds],"disthghE"=disthgh_E[inds],"disthghE2"=disthgh_E2[inds],
          "BurntE"=Burnt_E[inds],"AlpineE"=Alpine_E[inds],"ShrubE"=Shrub_E[inds],"RockE"=Rock_E[inds],
          "OconifE"=Oconif_E[inds],"HerbaciousE"=Herbacious_E[inds],"CutblockE"=Cutblock_E[inds])


# 2.LEAVE-ONE-OUT VALIDATION ===================================================

# fitting the model with one missing wolf
require(lme4)
datless<-subset(subda, wolfuid!=86)
datonly<-subset(subda, wolfuid==86)

mod304<-lmer(use~slope_a+distedg+disthgh+Burnt+Alpine+Shrub+Rock+Herbacious+(slope_a+distedg+disthgh|wolfuid), datless, family=binomial, REML=F, verbose=T)

fs22<-lmer(use~
slope_a+distedg+disthgh+
slopeE+distedgE+disthghE+
Burnt+Alpine+Shrub+Rock+Herbacious+
slope_a:slopeE+slope_a:distedgE+slope_a:disthghE+
distedg:slopeE+distedg:distedgE+distedg:disthghE+
disthgh:slopeE+disthgh:distedgE+disthgh:disthghE+
(1|wolfuid), datless, family=binomial, REML=F, verbose=T)

anova(mod304, fs22) # Comparison between the two models


best<-glm(use~slope_a+distedg+disthgh+Burnt+Alpine+Shrub+Rock+Herbacious, datonly, family=binomial)
bestpreds<-predict.glm(best, type="link")

par(mfcol=c(1,2))
preds<- model.matrix(terms(mod304),datonly) %*% fixef(mod304)
gof(plogis(preds)/sum(plogis(preds)), plogis(bestpreds)/sum(plogis(bestpreds)), bino, "RE")

preds<- model.matrix(terms(fs22),datonly) %*% fixef(fs22)
gof(plogis(preds)/sum(plogis(preds)), plogis(bestpreds)/sum(plogis(bestpreds)), bino, "O1")

par(mfcol=c(1,1))

#======================================
#=           FUNCTIONS                =
#======================================
gof<-function(fits, obs, bino, title)        # plotting goodness-of-fit plot for a binomial model
  {
  smoothScatter(fits,obs, main=title,  xlab="Extrapolation", ylab="Best fit", nrpoints=500)
  abline(0,1)
  print(paste("Precision: ",sum((fits-obs)^2)))
  print(paste("Bias     : ",sum(fits-obs)))
  }
