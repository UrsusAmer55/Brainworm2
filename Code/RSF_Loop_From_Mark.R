
library(ResourceSelection) 
library(stats)

setwd("C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/Brainworm2")
w <- read.csv("S3.winter_03022018.csv")
w$AnimalID <- paste(w$animalID, w$year, sep = ".")
w <- droplevels(subset(w, !nlcd=="NULL"))
head(w)
unique(w$AnimalID)

w2 <- cbind(w[,c(18,2,3,15,17)], apply(X = w[,c(11:13)], MARGIN = 2,
                                                 FUN = function(x) {(x - mean(x)) / sd(x)})) #z-transformation for continuous variables on different scales

head(w2)
str(w2)
Out <- unique(w2$AnimalID)
Out

rsf.out <- NULL

for(j in 25:length(Out)){
  
  useDF<-subset(w2,subset=w2$AnimalID==unique(w2$AnimalID[j]))
  
  Animalname<-unique(useDF$AnimalID)
  Animalname
  
  #note that in the models below I created an offset (offset(off)) which is simply the log of the
  #home range area divided by the number of available points - I don't think* this is necessary and this was done
  #for my KDE areas - the following code is out of place (but noted out just so you could see how I set it up)
  
  #hrarea<-area.owin(kde.owin)
  #alldat$off<-log(hrarea/navail)
  
  useDF$nlcd <- relevel(useDF$nlcd, ref = 'woody.wetlands')
  
  fit1<-glm(y ~ as.factor(nlcd) + H75 + Stratum1 + Stratum2, data=useDF, family=binomial())
  summary(fit1)
  
  rsffit<-rsf(y ~ as.factor(nlcd) + H75 + Stratum1 + Stratum2, data=useDF, m=0)
  summary(rsffit)
  rsfout<-as.data.frame(summary(rsffit)$coefficients)
  
  
  coefs<-cbind(coef(fit1), c(NA,coef(rsffit)))
  colnames(coefs)<-c("LR no wts", "rsf") 
  rownames(coefs)<-names(coef(fit1))             
  outcoefs<-print(format(coefs,digits=3, scientific=T), quote=F)
  outcoefs<-as.data.frame(outcoefs)
  
  setwd("C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/Brainworm2")
  outcoefs$AnimalID<-Animalname
  rsf.out <- rbind(rsf.out, outcoefs)
  write.csv(outcoefs, file=paste(Animalname, ".csv", sep=""))
  
}





