library(raster)


#elev_P75_0p15plus_30METERS_UTM15.tif = 75th percentile for canopy height to help decrease influence from super-canopy trees
canheight<-raster("C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/GIS_files/CreateAM_RSF/elev_P75_0p15plus_30METERS_UTM15.tif")
plot(canheight)
#Canopy_Prop3m_UTM.tif = returns above 3-m for % canopy cover
cancover<-raster("C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/GIS_files/CreateAM_RSF/Canopy_Prop3m_UTM.tif")
#Prop_1_3m = non-ground returns between 1 and 3 m above ground (proxy for understory cover).
under<-raster("C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/GIS_files/CreateAM_RSF/Prop_1_3m")
plot(under)
#habitat classes from Amanda
hab<-raster("C:/Users/M.Ditmer/Documents/Research/Moose/BrainWorm/GIS_files/CreateAM_RSF/NLCD_RCL_AM")
habC<-crop(hab, under)
plot(habC)

under
canheight
cancover
habC


#create a separate raster for each cover type
habC0<-habC==0
habC1<-habC==1
habC2<-habC==2
habC3<-habC==3
habC4<-habC==4
plot(habC1)
plot(habC3)

#fall coeff other than hab categories
canheightCOEF<--.6
cancoverCOEF<--1.1
underCOEF<-.019

#calculate values for each habitat category (add in coeff for habitat below), should be a value of 0 if not in a given habitat types
#example with fall data (dummy values)
AMpredictFall_habC0<-(habC0*3.1+((under*underCOEF)*habC0)+((canheight*canheightCOEF)*habC0)+((cancover*cancoverCOEF)*habC0))
plot(AMpredictFall_hab0)
AMpredictFall_habC1<-(habC1*3.1+((under*underCOEF)*habC1)+((canheight*canheightCOEF)*habC1)+((cancover*cancoverCOEF)*habC1))
AMpredictFall_habC2<-(habC2*2.0+((under*underCOEF)*habC2)+((canheight*canheightCOEF)*habC2)+((cancover*cancoverCOEF)*habC2))
AMpredictFall_habC3<-(habC3*-1.2+((under*underCOEF)*habC3)+((canheight*canheightCOEF)*habC3)+((cancover*cancoverCOEF)*habC3))
AMpredictFall_habC4<-(habC4*-3+((under*underCOEF)*habC4)+((canheight*canheightCOEF)*habC4)+((cancover*cancoverCOEF)*habC4))
#add up all of the rasters
AMpredictFall_habC_ALL<-AMpredictFall_habC0+AMpredictFall_habC1+AMpredictFall_habC2+AMpredictFall_habC3+AMpredictFall_habC4
plot(AMpredictFall_habC_ALL)
hist(AMpredictFall_habC_ALL)
summary(AMpredictFall_habC_ALL)