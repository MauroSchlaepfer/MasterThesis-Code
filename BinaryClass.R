### Implementation of Sam Morris (STEP 1)
# Cleaning
#testD.bin <- testD
#testD.bin[testD>1] <-1

pdens<-log(PopDens+1)
pdens[!is.na(pdens)]<-((pdens)[!is.na(pdens)])/max((pdens)[!is.na(pdens)])
pdens[is.na(pdens)]<-0
# image(pdens2)
### Clean up pdens by only the selected countries
pdensEurope <- pdens
pdensEurope[,1:12] <- 0
pdensEurope[20:81,13:14] <- 0
pdensEurope[20:46,15] <- 0
pdensEurope[73:81,15:22]<-0
pdensEurope[78:81,23]<-0
pdensEurope[75:81,]<-0
pdensEurope[74:75,27:31] <- 0
pdensEurope[73,31] <- 0
pdensEurope[70:81,36] <- 0
pdensEurope[72:81,32:36] <- 0
pdensEurope[69:81,37:45] <- 0
pdensEurope[70:81,46:47] <- 0
pdensEurope[71:81,48:50] <- 0
pdensEurope[72:81,51:53] <- 0
pdensEurope[74:81,54:60] <- 0


pdens2<-pdensEurope
pdens2[pdens2==0]<-NA

### Test / Training set 0.33, 0.66
todayCoded<-zoo::as.Date(today)
m=length(Events_Relevant_QGIS$Date)

set.seed(-35134)
Test <- sample(1:m,round(m/3,0) ); Train <- setdiff(c(1:m) , c(Test) )

Events_Relevant_QGIS.Train<-Events_Relevant_QGIS[Train,]
Events_Relevant_QGIS.Test<-Events_Relevant_QGIS[Test,]

EventsOrdered<-Events_Relevant_QGIS.Train[order(Events_Relevant_QGIS.Train$DateCoded),]
EventsOrdered$DateNumeric<-as.numeric(EventsOrdered$DateCoded)-min(as.numeric(todayCoded))+1

# Difference between time (todayCoded 1992-2020) and any given event. 
Difftime <- function(time, tEvent) (time - tEvent) # tEvent = time of Event, time = todayCoded = daily data between 1992 and 2020
Diff.Time <- sapply(todayCoded, Difftime, tEvent = as.numeric(EventsOrdered$DateCoded))

v=81
latlongAll<-(lat.grid*v+long.grid-v)
latlongAll.t<-table(latlongAll)
testD.bin<-numeric(v^2)
testD.bin[as.numeric(names(latlongAll.t))]<-latlongAll.t
testD.bin<-matrix(testD.bin,ncol=v)
GLM.P<-glm(na.omit(c(testD.bin+pdens2*0+Temperature*0))~na.omit(c(pdens2+Temperature*0))+na.omit(c(Temperature+pdens2*0)) ,family = "poisson")
GLM.PT<-glm(na.omit(c(testD.bin+pdens2*0+Temperature*0))~na.omit(c(pdens2+Temperature*0))+na.omit(c(Temperature+pdens2*0)) ,family = "poisson")
GLM.T<-glm(na.omit(c(testD.bin+pdens2*0+Temperature*0))~na.omit(c(Temperature+pdens2*0)) ,family = "poisson")
Y.head.P<-Y.head.PT<-Y.head.T<-rep(NA,v^2)

Y.head.P[!is.na(c(pdens2+Temperature))]<-exp(GLM.P$fit)/sum(exp(GLM.P$fit))
Y.head.PT[!is.na(c(pdens2+Temperature))]<-exp(GLM.PT$fit)/sum(exp(GLM.PT$fit))
Y.head.T[!is.na(c(pdens2+Temperature))]<-exp(GLM.T$fit)/sum(exp(GLM.T$fit))

#image(matrix(log(Y.head.P+10^-6),ncol=81))
#image(matrix(log(Y.head.T+10^-6),ncol=81))
#image(matrix(log(Y.head.PT+10^-6),ncol=81))

W <- function(s,v,rho) exp( ( sum(abs(s-v)/rho))^2 /(-2) )  # Scaled Gaussian Kernel
# rho = spatial correlation, Parameter to optimize, v = spatial coordinates of Event xy,
# s = spatial coordinates of Europe grid


s = expand.grid(seq(1,81,by=1),seq(1,81,by=1))

lat.grid.Train<-round(Events_Relevant_QGIS$Latitude[Train]*2,0)-59 # Coordinates of Attacks (in training set)
long.grid.Train<-round(Events_Relevant_QGIS$Longitude[Train]*2,0)+21

################################ Run untile here ###
s = expand.grid(seq(1,81,by=1),seq(1,81,by=1)) # dim=6561:2, coordinates of Grid
n=81^2
opt=c(0.1,2,0.6)
kk<-length(Events_Relevant_QGIS$Date[Train])


setwd("/home/ka4/Desktop/Dokumente Masterarbeit")
Optimise1<-function(opt){
        nn<-length(Train)
        CVn<-round(c(1,nn*0.25,nn*0.5,nn*0.75,nn),0)
        Fit<-numeric(length(CVn)-1)
        DateStart<-as.numeric(todayCoded)-min(as.numeric(todayCoded))+1 # start date by 1

                for(i in 1:(length(CVn)-1)){
            Train.CV<-Train[-(CVn[i]:CVn[i+1])]
            
            ### Left out
            LeftOut<-Train[(CVn[i]:CVn[i+1])]
            ####
            # Probability Matrix 13 seconds
            source("Step1FunctionToOptimise.R")
            ProbMat2<-Step1FunctionToOptimise2(opt,nEvents =  length(Train.CV),
                                               nTime =  length(todayCoded),Event = EventsOrdered$DateNumeric[-(CVn[i]:CVn[i+1])], smat = s ,
                                               long.grid = long.grid[Train.CV], lat.grid = lat.grid[Train.CV])  

            ProbMat2.Vec<-c(ProbMat2)
            
            # Build comparison matrix on left out values
            ControlMat.CV<-matrix(0,nrow=6561,ncol=length(todayCoded))
            latlong.Leftout<-(lat.grid[LeftOut]*81+long.grid[LeftOut]-81)
            Events.CV.Date <- as.numeric(EventsOrdered$DateNumeric[(CVn[i]:CVn[i+1])])-min(as.numeric(todayCoded))+1
            for(j in 1:length(LeftOut)){
                ControlMat.CV[latlong.Leftout[j],Events.CV.Date[j]] <- ControlMat.CV[latlong.Leftout[j],Events.CV.Date[j]]+1
            }
            ControlVec.CV<-c(ControlMat.CV)
            
            # Compare
            Fit[i]<-sum((ProbMat2.Vec-ControlVec.CV)^2) # squared loss function
                }
        sum(Fit)-47499132 # -47499133 only for visuality reasons
}
