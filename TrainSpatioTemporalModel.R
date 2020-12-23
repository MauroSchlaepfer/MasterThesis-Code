################################################################## Train
source("Step1FunctionToOptimise.R")
set.seed(123)
Train.ProbMat<-Step1FunctionToOptimise2(opt=c(0.1,0.1,0.99),nEvents =  length(Train),
                                        nTime =  length(todayCoded),Event = EventsOrdered$DateNumeric, smat = s ,
                                        long.grid = long.grid.Train, lat.grid = lat.grid.Train)



########### Plots
ProbabilityOverTime<-colSums(Train.ProbMat)
sum(na.omit(ProbabilityOverTime))
par(mar=c(5,6.5,2,1)+.1)
plot(todayCoded,ProbabilityOverTime,type="l",yaxt='n',ylim=c(0,max(ProbabilityOverTime)),
     ylab= expression(paste(sum("P[ Y=1, t=T ]",i=1,6561),sep="") ), xlab= "" )
plot(todayCoded[9983:10348],ProbabilityOverTime[9983:10348],type="l",yaxt='n',
     ylim=c(0,max(ProbabilityOverTime)),
     ylab= expression(paste(sum("P[ Y=1, t=T ]",i=1,6561),sep="") ),xlab="")

order(ProbabilityOverTime,decreasing = T)[1:20]
c(min(log(Train.ProbMat[,251]+10^-4)),
  max((Train.ProbMat[,251])))


ImagePlots  <- function(ProbMat, StartDay, SpatialSparsity=10^-4, Numbers=9){
    for(i in 0:(Numbers-1)){
        image(matrix(log(ProbMat[,StartDay+i]+SpatialSparsity),ncol=81)+pdens2*0,
              zlim=c(log(SpatialSparsity),log(max(ProbabilityOverTime)+SpatialSparsity)),
              xlab="",ylab="", main=bquote( rho ~"= 0.01"~ .(todayCoded[StartDay+i])),
              xaxt='n',yaxt='n')
    }
}
ImagePlots(Train.ProbMat,251)
ImagePlots(Train.ProbMat,9330)
Rho0.3<-Step1FunctionToOptimise2(opt=c(0.3,0.1,0.25),nEvents =  length(Train),
                                 nTime =  length(todayCoded),Event = EventsOrdered$DateNumeric, smat = s ,
                                 long.grid = long.grid.Train, lat.grid = lat.grid.Train)
Rho0.9<-Step1FunctionToOptimise2(opt=c(0.9,0.1,0.25),nEvents =  length(Train),
                                 nTime =  length(todayCoded),Event = EventsOrdered$DateNumeric, smat = s ,
                                 long.grid = long.grid.Train, lat.grid = lat.grid.Train)
Phi0.1_Rho0.5<-Step1FunctionToOptimise2(opt=c(0.5,0.01,0.25),nEvents =  length(Train),
                                        nTime =  length(todayCoded),Event = EventsOrdered$DateNumeric, smat = s ,
                                        long.grid = long.grid.Train, lat.grid = lat.grid.Train)
Phi0.5_Rho0.5<-Step1FunctionToOptimise2(opt=c(0.5,0.01,0.25),nEvents =  length(Train),
                                        nTime =  length(todayCoded),Event = EventsOrdered$DateNumeric, smat = s ,
                                        long.grid = long.grid.Train, lat.grid = lat.grid.Train)

ImagesGreece<-cbind(Train.ProbMat[,263],Rho0.1[,263],Rho0.5[,263])
par(mfrow=c(1,3),mar=c(2,2,3,2))
rho.values<-rep(c(0.01,0.1,0.5),3)
phi.values<-c(rep(0.01,3),rep(0.1,3),rep(0.5,3))
delta.values<-rep(0.25,9)
SpatialSparsity=10^-4

for(i in 1:3){
    image((matrix(log(ImagesGreece[,i]+SpatialSparsity),ncol=81)+pdens2*0)[53:74,10:31],
          zlim=c(log(SpatialSparsity),log(max(ProbabilityOverTime)+SpatialSparsity)),
          xlab="",ylab="",xaxt='n',yaxt='n',
          main=bquote( bold(rho) ~"=" ~.(rho.values[i])~","~ delta ~"=" ~.(phi.values[i])~","~ Phi ~"=" ~.(delta.values[i])))
}
ImagePlots(Train.ProbMat,263,SpatialSparsity=10^-4,Numbers = 1)
ImagePlots(Rho0.1,263,SpatialSparsity=10^-4,Numbers = 1)
ImagePlots(Rho0.5,263,SpatialSparsity=10^-4,Numbers = 1)
ImagePlots(Phi0.1_Rho0.5,263,SpatialSparsity=10^-4,Numbers = 1)
ImagePlots(Phi0.5_Rho0.5,263,SpatialSparsity=10^-4,Numbers = 1)

############################################################################################################## Testdataset
Events_Relevant_QGIS.Test<-Events_Relevant_QGIS[Test,]
EventsOrderedTest<-Events_Relevant_QGIS.Test[order(Events_Relevant_QGIS.Test$DateCoded),]
EventsOrderedTest$DateNumeric<-as.numeric(EventsOrderedTest$DateCoded)-min(as.numeric(todayCoded))+1


############################ MCMC Test set
SIM=100
SpatioTempMC<-function(SIM){
    # # Perfect hit
    latlong.Test<-(lat.grid[Test]*81+long.grid[Test]-81)
    SpatioTemporalEvent.Test.Matrix<-matrix(0,nrow=6561,ncol=length(todayCoded))
    Train.ProbMat.Vec<-c(Train.ProbMat)
    DateStart<-as.numeric(todayCoded)-min(as.numeric(todayCoded))+1 # start date by 1
    Events.Test.Date <- as.numeric(Events_Relevant_QGIS.Test$DateCoded)-min(as.numeric(todayCoded))+1
    for(i in 1:length(Test)){
        SpatioTemporalEvent.Test.Matrix[latlong.Test[i],Events.Test.Date[i]] <- SpatioTemporalEvent.Test.Matrix[latlong.Test[i],Events.Test.Date[i]]+1
    }
    SpatioTemporalEvent.Test.Vector<-c(SpatioTemporalEvent.Test.Matrix)
    Test.Event.SpatioTemp<-which(!SpatioTemporalEvent.Test.Vector==0)
    Obs_Attacks_PerDay <- colSums(SpatioTemporalEvent.Test.Matrix)# Only time dimension
    Test.Number <- which(Obs_Attacks_PerDay>0)
    n.days<-length(which(Obs_Attacks_PerDay>0))
    
    ## Manipulated probability matrix 
    MC<-numeric(length(Train.ProbMat.Vec))
    Train.ProbMat.Vec[which(Train.ProbMat.Vec==0)]<-NA
    Train.ProbMat.Vec.NAOMIT <- na.omit(Train.ProbMat.Vec) 
    n.SIM<-length(Train.ProbMat.Vec.NAOMIT)    
    
    ### Simulate
    R_Time_and_Space <- numeric(SIM)
    R_Space_fixed <- numeric(SIM)
    R_Space_fixed_time_relaxed_1 <- numeric(SIM)
    
    for(i in 1:SIM){
        MC.NAOMIT<-rep(0,length(Train.ProbMat.Vec.NAOMIT))
        MCMC <- sample(n.SIM,length(Test),p= c(Train.ProbMat.Vec.NAOMIT)  ) # SIM
        MC.NAOMIT[MCMC]<-1
        MC[!is.na(Train.ProbMat.Vec)]<-MC.NAOMIT
        Sim_Attacks_PerDay <- colSums(matrix(MC,ncol=10348))
        
        ### Compare
        DaysAttackSim<- which(Sim_Attacks_PerDay>0)
        R_Time_and_Space[i]<-sum(duplicated(c(which(MC==1),Test.Event.SpatioTemp)))/length(Test) # All
        R_Space_fixed[i] <- length(unique(which(abs(outer(Test.Number, DaysAttackSim, `-`)) <= 0, arr.ind = TRUE)[,1]))/min(n.days,length(DaysAttackSim)) # per day 
        R_Space_fixed_time_relaxed_1[i] <- length(unique(which(abs(outer(Test.Number, DaysAttackSim, `-`)) <= 1, arr.ind = TRUE)[,1]))/min(n.days,length(DaysAttackSim)) # per day with relaxation of one day
    }
    
    list(R_Time_and_Space=R_Time_and_Space,
         R_Space_fixed=R_Space_fixed,
         R_Space_fixed_time_relaxed_1=R_Space_fixed_time_relaxed_1)
}

set.seed(123)
SpatioTemporalResults<-SpatioTempMC(SIM)
mean(SpatioTemporalResults$R_Time_and_Space)
mean(SpatioTemporalResults$R_Space_fixed)
mean(SpatioTemporalResults$R_Space_fixed_time_relaxed_1)
sd(SpatioTemporalResults$R_Time_and_Space)/SIM
sd(SpatioTemporalResults$R_Space_fixed)/SIM
sd(SpatioTemporalResults$R_Space_fixed_time_relaxed_1)/SIM
