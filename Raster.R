setwd("/home/ka4/Desktop/Dokumente Masterarbeit")
require(rgdal)
require(raster)
require(sp)

alt1 <- getData("worldclim", var="alt", res=.5, lon=-10, lat=55) 
alt2 <- getData("worldclim", var="alt", res=.5, lon=10, lat=55)
temp1 <- getData("worldclim", var="tmean", res=2.5, lon=-10, lat=55)[[1]]
temp2 <- getData("worldclim", var="tmean", res=2.5, lon=10, lat=55)[[1]]

alt<-merge(alt1,alt2) # altitude
temp<-merge(temp1,temp2) # temperature

require(rangeBuilder)
par(mfrow=c(1,1))
dev.off()


#min(Events_Relevant_QGIS$Latitude) # 30
#max(Events_Relevant_QGIS$Longitude) # 30
#min(Events_Relevant_QGIS$Longitude) # -10
grid.lat<-seq(30,70,by = 0.5)
grid.long<-seq(-10,30,by = 0.5)
coords<-expand.grid(grid.long,grid.lat)
# the steps to extract values for the variables you want from the coordinates:
points <- SpatialPoints(coords, proj4string = alt@crs)

# getting the 30s altitude for the points
altS <- extract(alt, points)
TempS <- extract(temp, points)

# bind it all into one dataframe
climate <- cbind.data.frame(coords, altS, TempS)

climate$x <- climate$Var1*2+21
climate$y <- climate$Var2*2-59

Altitude<-PopDens<-Temperature<-Data<-matrix(0,nrow = 81,ncol = 81)


################# Population Density 
# https://sedac.ciesin.columbia.edu/data/set/gpw-v3-population-density/data-download
popdens <- raster("QGIS/eu_gpwv3_pdens_00_wrk_25/eudens00/euds00g/dblbnd.adf")

pop <- extract(popdens, points)
climate$popdens <- pop

for(i in 1:length(climate$Var1)) PopDens[climate$x[i],climate$y[i]] <- climate$popdens[i]
for(i in 1:length(climate$Var1)) Altitude[climate$x[i],climate$y[i]] <- climate$altS[i]
for(i in 1:length(climate$Var1)) Temperature[climate$x[i],climate$y[i]] <- climate$TempS[i]

PopDens[  (is.na(PopDens)
         *(!is.na(cbind(PopDens[,81],PopDens[,1:80])))
         *(!is.na(rbind(PopDens[81,],PopDens[1:80,])))
         *(!is.na(rbind(PopDens[2:81,],PopDens[1,])))
         *(!is.na(cbind(PopDens[,2:81],PopDens[,1]))))
         ] <- 1 # Fix some less populated areas and rounding issues
PopDens[76,26]<-1 # Bulgaria
Altitude[,61:80] <- PopDens[,61:80]
###### Bring Data inside Grid

lat.grid<-round(Events_Relevant_QGIS$Latitude*2,0)-59
long.grid<-round(Events_Relevant_QGIS$Longitude*2,0)+21
Vic_Wounded_or_Killed <- Events_Relevant_QGIS$AttackTotalKilled+Events_Relevant_QGIS$AttackTotalWounded
source("BinaryClass.R")
rm(alt,alt1,alt2, climate, coords,temp,temp1,temp2,pdens)

##### Spatial Graphs
#image(alt,xlim=c(-10,30),ylim=c(30,70), xlab="Longitude",ylab="Latitude")
#image(temp,xlim=c(-10,30),ylim=c(30,70), xlab="Longitude",ylab="Latitude")
#image(log(popdens),xlim=c(-10,30),ylim=c(30,70), xlab="Longitude",ylab="Latitude")

#image(log(PopDens), xlab="Longitude",ylab="Latitude",axes=F)
#axis(2, at=c(0,0.25,0.5,0.75,1), labels=c(30,40,50,60,70))
#axis(1, at=c(0,0.25,0.5,0.75,1), labels=c(-10,0,10,20,30))
#axis(3,at=c(-1,2))
#axis(4,at=c(-1,2))

#image(Temperature, xlab="Longitude",ylab="Latitude",axes=F)
#axis(2, at=c(0,0.25,0.5,0.75,1), labels=c(30,40,50,60,70))
#axis(1, at=c(0,0.25,0.5,0.75,1), labels=c(-10,0,10,20,30))
#axis(3,at=c(-1,2))
#axis(4,at=c(-1,2))

#image(Altitude, xlab="Longitude",ylab="Latitude",axes=F)
#axis(2, at=c(0,0.25,0.5,0.75,1), labels=c(30,40,50,60,70))
#axis(1, at=c(0,0.25,0.5,0.75,1), labels=c(-10,0,10,20,30))
#axis(3,at=c(-1,2))
#axis(4,at=c(-1,2))

###################################################################
#IT<-subset(Events_Relevant_QGIS,TypeOfTerrorism=="Islamistic")
#RT<-subset(Events_Relevant_QGIS,TypeOfTerrorism=="Right Terror")
#LT<-subset(Events_Relevant_QGIS,TypeOfTerrorism=="Left Terror")

#QGIS<- data.frame(Latitude=Events_Relevant_QGIS$Latitude,Longitude=Events_Relevant_QGIS$Longitude,
                #WoundedKilled=Events_Relevant_QGIS$AttackTotalKilled+Events_Relevant_QGIS$AttackTotalWounded,
               #TypeofTerrorism = Events_Relevant_QGIS$TypeOfTerrorism)
#QGIS_I <- data.frame(Latitude=IT$Latitude,Longitude=IT$Longitude,
 #                    WoundedKilled=IT$AttackTotalKilled+IT$AttackTotalWounded,
  #                   TypeofTerrorism = IT$TypeOfTerrorism)
#QGIS_R <- data.frame(Latitude=RT$Latitude,Longitude=RT$Longitude,
 #                    WoundedKilled=RT$AttackTotalKilled+RT$AttackTotalWounded,
      #               TypeofTerrorism = RT$TypeOfTerrorism)
#QGIS_L <- data.frame(Latitude=LT$Latitude,Longitude=LT$Longitude,
   #                  WoundedKilled=LT$AttackTotalKilled+LT$AttackTotalWounded,
    #                 TypeofTerrorism = LT$TypeOfTerrorism)
#write_csv(QGIS, "/home/ka4/Desktop/Dokumente Masterarbeit/QGIS.csv")
#write_csv(QGIS_I, "/home/ka4/Desktop/Dokumente Masterarbeit/QGIS_I.csv")
#write_csv(QGIS_R, "/home/ka4/Desktop/Dokumente Masterarbeit/QGIS_R.csv")
#write_csv(QGIS_L, "/home/ka4/Desktop/Dokumente Masterarbeit/QGIS_L.csv")

