#============================================
#=          Point-by-Point GFR              =
#=         Simulation & Analysis            =
#=   Jason Matthiopoulos & Robert Paton     =
#=     Last edited 11th October 2014        =
#============================================

setwd()

# Matrix stores for each simulation

store1<-matrix(nrow=30, ncol=36, byrow=T) 
store2<-matrix(nrow=30, ncol=36, byrow=T) 
store3<-matrix(nrow=30, ncol=36, byrow=T) 
store4<-matrix(nrow=30, ncol=36, byrow=T) 

#=========================
# Scenario Initialisation
#=========================

# Environment-related

d<-100  #  Arena dimensions

x1<-50  # Sets the number of distribution foci for food
x2<-50  # Sets the number of distribution foci for cover

mx1<-20 # Sets the abundance of food
mx2<-20 # Sets the abundance of cover

# Beginning of the loop which repeats the simulation 30 times for the above initital conditions.

for(r in 1:30)
{
  env1<-environ(d,x1,mx1)                   # Environ layer 1
  env2<-environ(d,x2, mx2)                   # Environ layer 2
  
  # SIMULATION
  rx<-rep(seq(1,d),d)
  ry<-rep(seq(1,d),each=d)
  r1<-env1[cbind(rx,ry)]/max(env1)
  r2<-env2[cbind(rx,ry)]/max(env2)
  
  
  # Animal related parameters
  enth1_2<-100        # Threshold for changing behav state 1 to 2
  cnth2_1<-100         # Threshold for changing behav state 2 to 1
  tmax<-1000000      # Simulation duration
  food<-env1        # Food distribution
  cover<-env2       # Cover distribution
  a<-0.2              # Functional response scale parameter
  b<-0.4         # Functional response half-saturation parameter
  den<-0.05       # Metabolic cost
  dencover<-0.05  # Metabolic cost sleeping
  ef<-10    # Search efficiency    
  
  
  # Initial conditions
  x<-round(d/2)             # x-coordinate
  y<-round(d/2)           # y-coordinate 
  st<-1                     # behavioural state
  en<-rep(70,tmax)          # energy state
  cn<-rep(100,tmax)        #cover state 
  ar<-array(0, dim=c(d,d))  # Utilisation distribution
  ar[x,y]<-1
  
  foodf<-food
  coverf<-cover
  
  mid<-round(d/2)
  betafood<-c()
  betacover<-c()
  avail<-c()
  sc<-c()
  err<-c()
  betafoodGFR<-c()
  betacoverGFR<-c()
  availGFR<-c()
  errGFR<-c()
  
  #===========
  # Simulation
  #===========
  
  # Main loop
  for (t in 2:tmax)
  {
    # Controlling for edge effects (torroidal boundaries)
    xu<-ifelse(x==d, 1, x+1)
    xl<-ifelse(x==1, d, x-1)
    yu<-ifelse(y==d, 1, y+1)
    yl<-ifelse(y==1, d, y-1)
    # Neumann neighbourhood
    nx<-c(x, x, xu, xl, x)
    ny<-c(y, yu, y, y, yl)
    
    # Mortality
    if(en[t]<=0) #starvation
    {
      print("Animal is dead") 
      x<-round(runif(1,1,d))             # x-coordinate
      y<-round(runif(1,1,d))            # y-coordinate
      en[t]<-70
      cn[t]<-100
    }
    
    if(cn[t]<=0) #exhaustion
    {
      print("Animal is dead") 
      x<-round(runif(1,1,d))             # x-coordinate
      y<-round(runif(1,1,d))            # y-coordinate
      en[t]<-70
      cn[t]<-100
    }
    
    if (st==1)
    {# Foraging
      en[t]<-en[t-1]-den # Metabolism
      cn[t]<-cn[t-1]-den
      en[t]<-en[t]+a*food[x,y]/(b+food[x,y])    # Energy gain
      if (en[t]>enth1_2) st<-2    # Possible change in behavioural state
      pot<-foodf[cbind(nx,ny)]+rnorm(5,0,10/ef*mean(food)) # Picking the next unit
    } else
    {# Sleeping
      en[t]<-en[t-1]-dencover # Lowered sleeping metabolism
      cn[t]<-cn[t]+a*cover[x,y]/(b+cover[x,y])    # sleep gain
      if (cn[t]>cnth2_1) st<-1   # Possible change in behavioural state
      pot<-coverf[cbind(nx,ny)]+rnorm(5,0,10/ef*mean(cover))
    }
    
    x<-nx[which.max(pot)]
    y<-ny[which.max(pot)]
    ar[x,y]<-ar[x,y]+1 # incrementing usage 
    
    if(t/5000==round(t/5000))
    {
      print(t/tmax)
      x<-round(runif(1,1,d))             # x-coordinate
      y<-round(runif(1,1,d))            # y-coordinate
      en[t]<-70
      cn[t]<-100
    }
  }
  
  par(mfrow=c(1,4))
  image(food,col = terrain.colors(50), axes=FALSE, main="Food")
  image(cover,col = terrain.colors(50), axes=FALSE, main="Cover")
  plot(rx,ry, pch=15, col=rgb(r2,r1,0), cex=2, axes=FALSE, xlab="", ylab="", main="Combined resources")
  image(ar,col = terrain.colors(50), axes=F, xlab="Lattitude", ylab="Longitude", main="Utilisation distribution")
  
  
  #=======================================
  # Sensitivity to sampling scale analysis
  #=======================================
  
  
  for(scale in 5:(mid-10))
  {
    x<-mid
    y<-mid
    sc<-c(sc,scale)
    rngx<-(x-scale):(x+scale)
    rngy<-(y-scale):(y+scale)
    ars<-ar[rngx,rngy]
    env1s<-env1[rngx,rngy]
    env2s<-env2[rngx,rngy]
    dat0<-data.frame("use"=c(ars),"food"=c(env1s),"cover"=c(env2s), "Efd"=c(mean(env1s)), "Ecv"=c(mean(env2s))) #Data frame specific to range x/y. Up to 50. Incluses averages necessary for GFR. 
    
    #Standard GLM
    mod0<-glm(use~food+cover, data=dat0, family=poisson) #Standard HSF
    
    #GLM Metrics 
    
    betafood<-c(betafood,mod0$coefficients[2]) #List of coefficients for later use (GLM Robustness)
    betacover<-c(betacover,mod0$coefficients[3])
    avail<-c(avail,sum(env1s)/(sum(env2s+env1s)))
    err<-c(err,lm(fitted(mod0)~c(ars))$coefficients[2])
    obvs<-c(ars)
    fit<-fitted(mod0) 
    
    #Point by Point GFR
    
    aicbest<-Inf
    
    for(it in 1:10)
    {
      bw<-it # radius of neighbourhood
      
      # Averaging template
      ite<-c()
      jte<-c()
      for(ii in -bw:bw)
      {
        for(jj in -bw:bw)
        {
          if(sqrt(ii^2+jj^2)<=bw)
          {
            ite<-c(ite,ii)
            jte<-c(jte,jj)
          }
        }
      } 
      
      for(i in rngx)
      {
        for(j in rngy)
        {
          row<-(i-min(rngx))*length(rngx)+(j-min(rngy))+1
          dat0[row,4]<-mean(env1[cbind(i+ite, j+jte)])
          dat0[row,5]<-mean(env2[cbind(i+ite, j+jte)])
        }
      }
      
      #GFR 
      
      mod<-glm(use~food+cover+Efd+Ecv+food:Efd+food:Ecv+cover:Efd+cover:Ecv, data=dat0, family=poisson) # Point-by-point GFR model structure
      pred<-fitted(mod)
      print(paste(it, ")  AIC:", round(AIC(mod))))
      testAIC<-AIC(mod)
      
      #Neighbourhood selection (compares AICs of models fit to neighbourhoods 1:10)
      
      if(testAIC<aicbest) 
      {
        aicbest<-testAIC
        predbest<-pred
        modbest<-mod
      }
    }
    
    #GFR Metrics 
    
    betafoodGFR<-c(betafoodGFR,modbest$coefficients[2]) #List of coefficients for later use (GLM Robustness)
    betacoverGFR<-c(betacoverGFR,modbest$coefficients[3])
    errGFR<-c(errGFR,lm(c(pred)~c(ars))$coefficients[2])
    fitGFR<-fitted(modbest) 
    
    #Neighbourhood selection reset
    
    aicbest<-Inf
    print(paste(scale,") AIC benchmark reset, next scale"))
    
  }
  
  
  #=================================================
  # Calculate non-transferability and homogenization
  #=================================================
  
  furthestfood<-max(abs((betafood[length(betafood)]-betafood)))
  metricfood<-(abs(betafood[length(betafood)]-betafood)/furthestfood)
  
  furthestcover<-max(abs((betacover[length(betacover)]-betacover)))
  metriccover<-(abs(betacover[length(betacover)]-betacover)/furthestcover)
  
  robGLM<-mean(metricfood+metriccover)
  
  furthestfoodGFR<-max(abs((betafoodGFR[length(betafoodGFR)]-betafoodGFR)))
  metricfoodGFR<-(abs(betafoodGFR[length(betafoodGFR)]-betafoodGFR)/furthestfoodGFR)
  
  furthestcoverGFR<-max(abs((betacoverGFR[length(betacoverGFR)]-betacoverGFR)))
  metriccoverGFR<-(abs(betacoverGFR[length(betacoverGFR)]-betacoverGFR)/furthestcoverGFR)
  
  robGFR<-mean(metricfoodGFR+metriccoverGFR)/2
  
  # Stores the data from the simulations. 
  
  store1[r,]<-err #Storing the data from the GLM loop in the empty matrix 
  store2[r,]<-robGLM
  store3[r,]<-errGFR #Storing the data from the GFR loop in the empty matrix
  store4[r,]<-robGFR
  
}

#======================================
#=           FUNCTIONS                =
#======================================

require(KernSmooth)
require(mclust)


# FUNCTION: Generates a random environmental layer in a square dxd arena using a total of x focal points
environ<-function(d,x,mx)
{
  ar<-array(0, dim=c(d,d))
  # Places seeds in arena
  slope<-1 # Setting the splope dictates the gradient - 1 means there is no gradient, below one creates a gradient. 
  cox<-cbind(1+99*runif(x, min=0, max=1)^slope, 1+99*runif(x, min=0, max=1)^slope)
  # Smooths seeds to create spatial autocorrelation
  bw<-2
  sarx<-bkde2D(cox, bandwidth = c(bw,bw), gridsize=c(d,d),range.x=list(c(1,d),c(1,d)))
  sarx$fhat<-mx*(sarx$fhat/sum(sarx$fhat))
  contour(sarx$x1,sarx$x2,sarx$fhat)
  return(sarx$fhat)
}


