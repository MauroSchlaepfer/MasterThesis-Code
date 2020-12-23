#### Spatial Only 
## AR(1)
dev.off()
m=81
n=m^2
rho=0.15
nn<-length(Train)
latlong<-(lat.grid[Train]*m+long.grid[Train]-m)
tab_latlong<-table(latlong)
Events<-numeric(n)
Events[as.numeric(names(tab_latlong))]<-tab_latlong
E<-Events/sum(Events)


Adiag <- diag(rep(1,n))
A <- cbind(Adiag[,2:n],rep(0,n))+cbind(rep(0,n),Adiag[,1:(n-1)]) 
A <- (A+cbind(Adiag[,(1+m):n],matrix(rep(0,m*n),ncol=m) ) +cbind(matrix(rep(0,m*n),ncol=m) ,Adiag[,1:(n-m)]) )*rho
A<-A+diag(n)*(1-rowSums(A) )

#eigs(A,1,retvet=F)$values # is 1 -> A/1=A
P.Train<-c(A%*%E)
sum(P.Train)

### Left out
latlong.Test<-(lat.grid[Test]*m+long.grid[Test]-m)
tab_latlong.Test<-table(latlong.Test)
Events.Test<-MC<-numeric(n)
Events.Test[as.numeric(names(tab_latlong.Test))]<-tab_latlong.Test

####
## MCMC
Proporion_Correctly_Predicted <-function(Prob.Mat){
    SIM=1000
    Prop_Correct_pred <- numeric(SIM)
    for(i in 1:SIM){
        MC<-sample(n,sum(Events.Test>0),prob=Prob.Mat) # only 112 entries to classify
        # Compare with left out
        Prop_Correct_pred[i]<-sum(duplicated(c(MC,which(Events.Test>0))))/length(MC)
    }
    
    p=(Prop_Correct_pred)
    
    list(mu=mean(Prop_Correct_pred),
         sd=sd(Prop_Correct_pred)/sqrt(SIM))
}
Prob.Mat=P.Train

####
### 10-fold-Crossvalidation
CrossVali<-function(SpatialMat){
    CVn<-round(c(1,nn*0.1,nn*0.2,nn*0.3,nn*0.4,nn*0.5,nn*0.6,nn*0.7,nn*0.8,nn*0.9,nn),0)
    Fit<-numeric(length(CVn)-1)
    i=1
    for(i in 1:(length(CVn)-1)){
        Train.CV<-Train[-(CVn[i]:CVn[i+1])]
        latlong.CV<-(lat.grid[Train.CV]*m+long.grid[Train.CV]-m)
        tab_latlong.CV<-table(latlong.CV)
        Events.CV<-numeric(n)
        Events.CV[as.numeric(names(tab_latlong.CV))]<-tab_latlong.CV
        E.CV<-Events.CV/sum(Events.CV)
        P.Train.CV<-c(SpatialMat%*%E.CV)
        
        ### Left out
        LeftOut<-Train[(CVn[i]:CVn[i+1])]
        latlong.CV<-(lat.grid[LeftOut]*m+long.grid[LeftOut]-m)
        tab_latlong.CV<-table(LeftOut)
        tab_latlong.CV<-table(latlong.CV)
        Events.CV<-MC<-numeric(n)
        Events.CV[as.numeric(names(tab_latlong.CV))]<-tab_latlong.CV
        ####
        
        ## MCMC
        MC<-sample(n,sum(Events.CV>0),prob=P.Train)
        
        # Compare with left out
        Fit[i]<-sum(duplicated(c(MC,which(Events.CV>0))))/length(MC)
    }
    mean(Fit)
}

# Optimize rho
set.seed(123)
CV.optim.AR1 <- function(rho){
    Adiag <- diag(rep(1,n))
    A <- cbind(Adiag[,2:n],rep(0,n))+cbind(rep(0,n),Adiag[,1:(n-1)]) 
    A <- (A+cbind(Adiag[,(1+m):n],matrix(rep(0,m*n),ncol=m) ) +cbind(matrix(rep(0,m*n),ncol=m) ,Adiag[,1:(n-m)]) )
    A<-A*rho^2+diag(n)*rho
    CrossValiLoop<-numeric(10)
    for(j in 1:10){
        CrossValiLoop[j]<-CrossVali(A)
    }
    mean(CrossValiLoop)
}
sum(Events.Test)
#rho.optim<-matrix(seq(0.025,0.6,by=0.005),ncol=1)
# CV.Values<-apply(rho.optim,1,CV.optim.AR1)
# rho.optim[which(CV.Values==max(CV.Values)),1] # 0.15 both max
#(prevelance.CV<-length((nn*0.1):(nn*0.2))/n) # set it to be 50%
#max(CV.Values)/prevelance.CV# 0.0452

######### Spatial covariates for Tree based models
altitude <- na.omit(c(Altitude+pdens2*0+Temperature*0))
altitude.norm <- (altitude-mean(altitude)) / sum(altitude)
temperature <- na.omit(c(Temperature+pdens2*0+Altitude*0))
temperature.norm <- (temperature-mean(temperature)) / sum(temperature)
pdensNAomit <- na.omit(c(pdens2+Altitude*0+Temperature*0))
pdensNAomit.norm <- (pdensNAomit-mean(pdensNAomit)) / sum(pdensNAomit)
omi<-c(Events+pdens2*0+Altitude*0+Temperature*0)
Events.Train.NAOmit<-na.omit(omi)
SpatialDataFrame.Train<-data.frame(E=c(Events.Train.NAOmit),pdens=c(pdensNAomit.norm),alt=c(altitude.norm),temp=c(temperature.norm))

########################################################################################################
###### Boosting ##########
# Implementation
# Additive model = interaction depth =1 
# spatial covariates - all 3
require(gbm)
set.seed(123)
Boost.Train.optim<-gbm(E~pdens+alt+temp,data=SpatialDataFrame.Train,distribution = "poisson",
                 cv.fold=10, interaction.depth = 1, shrinkage = 0.1,n.trees = 1000)
CV.optimize.boost<-gbm.perf(Boost.Train.optim, method="cv") # 443
Boost.Train<-gbm(E~pdens+alt+temp,data=SpatialDataFrame.Train,
                 distribution = "poisson",interaction.depth = 1, shrinkage = 0.1,n.trees = CV.optimize.boost)
P.Boost<-exp(Boost.Train$fit)/sum(exp(Boost.Train$fit))
summary.gbm(Boost.Train)
par(mfrow=c(1,1))
xx<-barplot(summary.gbm(Boost.Train)[,2],col="floralwhite",ylim=c(0,60),ylab="Relative influence in %")
text(x =  xx,c(3,4,5),y=c(2), font=2,
     label = c("Temperature","Population Density","Altitude"), pos = 3, cex = 1, col = 1)

####
sum(Events.Test>0)
P.Boost.Test<-Events*0
P.Boost.Test[which(!is.na(omi))]<-P.Boost


## MCMC
######################## Uniform
set.seed(123)
Unif<-matrix((rep(1,n)*!is.na(pdens2)),ncol=m)
P.Unif<-Unif/sum(Unif)

###################################################################
#### Values in Table
set.seed(123)
Proporion_Correctly_Predicted(P.Boost.Test)
Proporion_Correctly_Predicted(P.Train)
Proporion_Correctly_Predicted(P.Unif)

####################################################################
#### Graphic
image(matrix(log(Events+10^-2),ncol=81)+pdens2*0) ### AttacksTraining
image(matrix(log(Events.Test+10^-2),ncol=81)+pdens2*0) ### AttacksTraining
image(matrix(log(P.Train+10^-4),ncol=81)+pdens2*0) ### Neigbourhoodmatrix
image(matrix(log(P.Boost.Test+10^-4)+pdens2*0,ncol=m)) ### Boosting
