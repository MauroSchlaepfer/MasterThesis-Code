# Opt.Optimising<-optim(c(0.1,0.2,0.5),fn=Optimise1,control=list(fnscale=-1,ndeps=c(0.005,0.025,0.025),maxit=30,trace=TRUE) ) 
# Nelder and Mead (1965) - easiliy doable by hand, just maximise function output -> used since function is unknown -> derivative also unknown
#### Build Control Matrix ######################################################
latlong.Train<-(lat.grid[Train]*81+long.grid[Train]-81)
SpatioTemporalEvent.Train.Matrix<-matrix(0,nrow=6561,ncol=length(todayCoded))
DateStart<-as.numeric(todayCoded)-min(as.numeric(todayCoded))+1 # start date by 1
Events.Train.Date <- as.numeric(Events_Relevant_QGIS.Train$DateCoded)-min(as.numeric(todayCoded))+1
for(i in 1:length(Train)){
    SpatioTemporalEvent.Train.Matrix[latlong.Train[i],Events.Train.Date[i]] <- SpatioTemporalEvent.Train.Matrix[latlong.Train[i],Events.Train.Date[i]]+1
}
SpatioTemporalEvent.Train.Matrix<-c(SpatioTemporalEvent.Train.Matrix)
    Test.Event.SpatioTemp<-which(!SpatioTemporalEvent.Train.Matrix==0)
    
    
    #####################################################################
    set.seed(123)
    Optimising<-matrix(0,ncol=4,nrow=20)
    Optimising[1,1:3]<-c(0.1,1,0.5) #starting values
    colnames(Optimising)<-c("rho","delta","phi","Value")
    #system.time(Optimise1(Optimising[1,1:3])) # 40s
    #system.time(Optimise1(Optimising[1,1:3],SIM=1)) # 16s
    
    Optimising[1,4]<-Optimise1(Optimising[1,1:3])
    
    
    set.seed(123)
    Optimising[2,1:3]<-c(0.01,1,0.5)
    Optimising[2,4]<-Optimise1(Optimising[2,1:3])
    
    set.seed(123)
    Optimising[3,1:3]<-c(0.001,1,0.5)
    Optimising[3,4]<-Optimise1(Optimising[3,1:3])
    
    set.seed(123)
    Optimising[4,1:3]<-c(0.001,0.1,0.5)
    Optimising[4,4]<-Optimise1(Optimising[4,1:3])
    
    set.seed(123)
    Optimising[5,1:3]<-c(0.001,2,0.5)
    Optimising[5,4]<-Optimise1(Optimising[5,1:3])
    
    system.time(
    Optimise1(c(0.001,0.5))
    )
    set.seed(123)
    Optimising[7,1:3]<-c(0.001,2,0.1)
    Optimising[7,4]<-Optimise1(Optimising[7,1:3])
    
    set.seed(123)
    Optimising[8,1:3]<-c(0.001,3,0.2)
    Optimising[8,4]<-Optimise1(Optimising[8,1:3])
    
    set.seed(123)
    Optimising[9,1:3]<-c(0.001,2,0.2)
    Optimising[9,4]<-Optimise1(Optimising[9,1:3])
    
    set.seed(123)
    Optimising[10,1:3]<-c(0.001,2.5,0.2)
    Optimising[10,4]<-Optimise1(Optimising[10,1:3])
    
    set.seed(123)
    Optimising[10,1:3]<-c(0.01,2,0.25)
    Optimising[10,4]<-Optimise1(Optimising[10,1:3])
    
    set.seed(123)
    Optimising[11,1:3]<-c(0.01,1,0.8)
    Optimising[11,4]<-Optimise1(Optimising[11,1:3])
    
    set.seed(123)
    Optimising[12,1:3]<-c(0.01,1,0.25)
    Optimising[12,4]<-Optimise1(Optimising[12,1:3])
    
    set.seed(123)
    Optimising[13,1:3]<-c(0.01,3,0.3)
    Optimising[13,4]<-Optimise1(Optimising[13,1:3])
    
    set.seed(123)
    Optimising[14,1:3]<-c(0.01,3,0.5)
    Optimising[14,4]<-Optimise1(Optimising[14,1:3])
    
    set.seed(123)
    Optimising[14,1:3]<-c(0.01,3,0.5)
    Optimising[14,4]<-Optimise1(Optimising[14,1:3])
    
    set.seed(123)
    Optimising[15,1:3]<-c(0.01,0.1,0.25)
    Optimising[15,4]<-Optimise1(Optimising[15,1:3])
    
    set.seed(123)
    Optimising[16,1:3]<-c(0.1,0.1,0.25)
    Optimising[16,4]<-Optimise1(Optimising[16,1:3])
    
    set.seed(123)
    Optimising[17,1:3]<-c(0.0001,0.1,0.25)
    Optimising[17,4]<-Optimise1(Optimising[17,1:3])
    
    set.seed(123)
    Optimising[18,1:3]<-c(0.01,0.1,0.25)
    # delta seems to have no influence, rho between 0.0001 and 0.1 gives similar results.
    Optimising[18,4]<-Optimise1(Optimising[18,1:3])
    # done manually because it's faster, insane running time.
    
    set.seed(123)
    Optimising[19,1:3]<-c(0.2,0.1,0.25) 
    Optimising[19,4]<-Optimise1(Optimising[19,1:3])
    
    set.seed(123)
    Optimising[20,1:3]<-c(0.01,0.1,0.15)
    Optimising[20,4]<-Optimise1(Optimising[20,1:3])
    
    set.seed(123)
    Optimising[16,1:3]<-c(0.01,0.1,0.35)
    Optimising[16,4]<-Optimise1(Optimising[16,1:3])
    
    set.seed(123)
    Optimising[15,1:3]<-c(0.01,0.1,0.45)
    Optimising[15,4]<-Optimise1(Optimising[15,1:3])
    
    set.seed(123)
    Optimising[14,1:3]<-c(0.01,0.01,0.55)
    Optimising[14,4]<-Optimise1(Optimising[14,1:3])
    
    set.seed(123)
    Optimising[11,1:3]<-c(0.01,0.01,0.65)
    Optimising[11,4]<-Optimise1(Optimising[11,1:3])
    
    set.seed(123)
    Optimising[12,1:3]<-c(0.01,0.01,0.75)
    Optimising[12,4]<-Optimise1(Optimising[13,1:3])
    
    Optimise1(c(0.1,0.1,0.99)) # final parameter , opt[1]:rho, opt[2]:delta, , opt[3]:phi
    Optimise1(c(0.5,10,0.99))
    Optimise1(c(0.5,10,0.75))
    
    