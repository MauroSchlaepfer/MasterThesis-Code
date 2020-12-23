#### IWLS
Y <- Events_Relevant_QGIS.Train$AtThreat[NonHurdle]
###
IWLS <- function(Omega1,Theta1,tollvl=1,X=X){
X <- as.matrix(X)
mu <- rep(mean(Y),length(X[,1]))
Omega <- Omega1
Theta <- Theta1
Y.head<-Y
tol=10
i=1

for(i in 1:150){ # sometimes tol gets very small but doesn't converge, for() is more stable
    #while(tol>tollvl){ # Iterative
    W<-diag(c(mu/(Omega[i] + Theta[i]*mu)) ) # Theta = 0 -> Quasipoisson, Omega = 1 -> negbin, both unfixed (reason wrote this code)
    Beta.head<-solve(t(X)%*%W%*%X,t(X), tol=9^-45)%*%W%*%Y.head
    nu <- X%*%Beta.head
    mu <- exp(nu)
    Y.head <- nu + (Y-mu)/mu # derivative of log(nu) -> 1/nu
    if(Theta1==0){
        Theta <- c(Theta,0)
        Omega <- c(Omega,mean( (Y-mu)^2/mu) )
    } else{ 
        if(Omega1==1){
            Theta<- c(Theta,mean( (( (Y-mu)^2) - mu )/mean(mu)^2))  
            Omega <- c(Omega,1)
        } else{ # NB2
            ThetaF  <- mean( (Y-mu)^2 - mu )/(mean(mu)^2) 
            Omega <- c(Omega,mean( ((Y-mu)^2)/mu - mu*ThetaF) )
            Theta  <- c(Theta,mean( ((Y-mu)^2 - mu*Omega[i+1] )/(mean(mu)^2 )) )
        }
    }
    tol <- abs(Omega[i+1]-Omega[i])+abs(Theta[i+1]-Theta[i])
    i <- i+1
}
list(Omega=Omega,
     Theta=Theta,
     Beta.head=Beta.head,
     SE = sqrt(diag(solve(t(X)%*%W%*%X, tol=9^-45))),
     FittedValues = mu)
}
set.seed(123)
######### MCMC Sample
Sample.Hurdle.Count <- function(SIM=1000, HurdleCoef, X, Beta, Theta=0, Omega=1){ # Total SIM is length(Data)*SIM
    X <- as.matrix(X)
    logit2prob<-function(logit){
        odds <- exp(logit)
        prob <- odds / (1 + odds)
        return(prob)
    }
    H<-rbinom(length(X[,1])*SIM,1,prob=logit2prob(X%*%HurdleCoef))
    mu<-X%*%Beta
    if(Omega>1&Theta==0){ # QuasiPoisson
        Theta <-mu/(Omega-1) 
        # rnbinom implemets theta1 as 1 + mu/theta instead of theta1 = 1 + mu*theta
    } else{
    if(Omega>1 & Theta>0){ # NB2
        Theta <- (-mu)*Theta/(Theta-Theta*Omega-mu)
        # rnbinom implemets theta1 as 1 + mu/theta instead of theta1 = 1 + mu*theta
    } else{
        Theta <- rep(Theta,length(mu))
        # rnbinom implemets theta1 as 1 + mu/theta instead of theta1 = 1 + mu*theta
    }
    }

    # Sample fron rnbinom
    mu <- rep(mu,SIM)
    Theta <- rep(Theta,SIM) 
    Y.pred <-rnbinom(n = SIM*length(X[,1]), mu = mu, size =Theta )

    while(sum(Y.pred==0)>0){ # rejection sampling
        Y.pred.new<-rnbinom(n = length(which(Y.pred==0)), mu = mu[which(Y.pred==0)], size =Theta[which(Y.pred==0)] )
        Y.pred[which(Y.pred==0)] <- Y.pred.new
    }

    Y.pred*H
} # Theta for Qpoisson mu/(omega-1) !theta here is 1/theta paper
X.E.Train<-model.matrix(Events_Relevant_QGIS.Train$AtThreat~EventType.Recoded.Train)
X.T.Train<-model.matrix(Events_Relevant_QGIS.Train$AtThreat~Terrorism.Train)
X.I.Train<-model.matrix(Events_Relevant_QGIS.Train$AtThreat~1)
##################### Models estimated and simulated based on training set #####################
#### Intercept only
### QuasiPoisson (own IWLS)
Quasi.Poisson.I<-IWLS(Omega1 = var(Y)/mean(Y),Theta1 = 0, tollvl =  10^-15,X=X.I.Train[NonHurdle] )
Y.I.QP <- Sample.Hurdle.Count(HurdleCoef = Hurdle.I$coefficients,Beta = Quasi.Poisson.I$Beta.head,
                            Omega = Quasi.Poisson.I$Omega[length(Quasi.Poisson.I$Omega)],X = X.I.Train)
MaxValue.I.QP<-apply(matrix(Y.I.QP,ncol=length(X.I.Train)),1,max)

### NegBin (own IWLS)
###
Neg.Bin.I<-IWLS(Omega1 = 1,Theta1 = (var(Y)/mean(Y)^2 - 1/mean(Y)), tollvl =  10^-15,X=X.I.Train[NonHurdle])
Y.I.NB <- Sample.Hurdle.Count(HurdleCoef = Hurdle.I$coefficients,Beta = Neg.Bin.I$Beta.head,
                              Theta = 1/Neg.Bin.I$Theta[length(Neg.Bin.I$Theta)],X = X.I.Train)
MaxValue.I.NB<-apply(matrix(Y.I.NB,ncol=length(X.I.Train)),1,max)

### NegBin2 (own IWLS)
###
Neg.Bin2.I<-IWLS(Omega1 = var(Y)/mean(Y),Theta1 = var(Y)/mean(Y)^2 - 1/mean(Y), tollvl =  10^-15,X=X.I.Train[NonHurdle])
Y.I.NB2 <- Sample.Hurdle.Count(HurdleCoef = Hurdle.I$coefficients,Beta = Neg.Bin2.I$Beta.head, Omega =Neg.Bin2.I$Omega[length(Neg.Bin2.I$Omega)],
                              Theta = 1/Neg.Bin2.I$Theta[length(Neg.Bin2.I$Theta)],X = X.I.Train)
MaxValue.I.NB2<-apply(matrix(Y.I.NB2,ncol=length(X.I.Train)),1,max)


##### Only Terrorism
### QuasiPoisson (own IWLS)
Quasi.Poisson.T<-IWLS(Omega1 = var(Y)/mean(Y),Theta1 = 0, tollvl =  10^-15,X=X.T.Train[NonHurdle,])
Y.T.QP <- Sample.Hurdle.Count(HurdleCoef = Hurdle$coefficients,Beta = Quasi.Poisson.T$Beta.head,
                            Omega = Quasi.Poisson.T$Omega[length(Quasi.Poisson.T$Omega)],X = X.T.Train)
MaxValue.T.QP<-apply(matrix(Y.T.QP,ncol=length(X.I.Train)),1,max)

### Negative Binomial (own dispersion)
Neg.Bin.T<-IWLS(Omega1 = 1,Theta1 = var(Y)/mean(Y)^2 - 1/mean(Y), tollvl =  10^-10,X=X.T.Train[NonHurdle,])
Y.T.NB <- Sample.Hurdle.Count(HurdleCoef = Hurdle$coefficients,Beta = Neg.Bin.T$Beta.head,
                            Theta = 1/Neg.Bin.T$Theta[length(Neg.Bin.T$Theta)],X = X.T.Train)
MaxValue.T.NB<-apply(matrix(Y.T.NB,ncol=length(X.I.Train)),1,max)

### Negative Binomial2 
Neg.Bin2.T<-IWLS(Omega1 =var(Y)/mean(Y),Theta1 = var(Y)/mean(Y)^2 - 1/mean(Y), tollvl =  10^-10,X=X.T.Train[NonHurdle,])
Y.T.NB2 <- Sample.Hurdle.Count(HurdleCoef = Hurdle$coefficients,Beta = Neg.Bin2.T$Beta.head, Omega=Neg.Bin2.T$Omega[length(Neg.Bin2.T$Omega)],
                              Theta = 1/Neg.Bin2.T$Theta[length(Neg.Bin2.T$Theta)],X = X.T.Train)
MaxValue.T.NB2<-apply(matrix(Y.T.NB2,ncol=length(X.I.Train)),1,max)
###

##### EventType
### QuasiPoisson (own IWLS)
Quasi.Poisson.E <-IWLS(Omega1 = var(Y)/mean(Y),Theta1 = 0, tollvl =  10^-10,X=X.E.Train[NonHurdle,])
Y.E.QP <- Sample.Hurdle.Count(HurdleCoef = Hurdle.E$coefficients,Beta = Quasi.Poisson.E$Beta.head,
                              Omega = Quasi.Poisson.E$Omega[length(Quasi.Poisson.E$Omega)],X = X.E.Train)
MaxValue.E.QP<-apply(matrix(Y.E.QP,ncol=length(X.I.Train)),1,max)
### Negative Binomial (own dispersion)
Neg.Bin.E<-IWLS(Omega1 = 1,Theta1 = var(Y)/mean(Y)^2 - 1/mean(Y), tollvl =  10^-10,X=X.E.Train[NonHurdle,])
Y.E.NB <- Sample.Hurdle.Count(HurdleCoef = Hurdle.E$coefficients,Beta = Neg.Bin.E$Beta.head,
                              Theta = 1/Neg.Bin.E$Theta[length(Neg.Bin.E$Theta)],X = X.E.Train)
MaxValue.E.NB<-apply(matrix(Y.E.NB,ncol=length(X.I.Train)),1,max)
### Negative Binomial2 
Neg.Bin2.E<-IWLS(Omega1 =(var(Y)/mean(Y)),Theta1 = var(Y)/mean(Y)^2 - 1/mean(Y), tollvl =  10^-10,X=X.E.Train[NonHurdle,])
Y.E.NB2 <- Sample.Hurdle.Count(HurdleCoef = Hurdle.E$coefficients,Beta = Neg.Bin2.E$Beta.head, Omega=1/Neg.Bin2.E$Omega[length(Neg.Bin2.E$Omega)],
                              Theta = 1/Neg.Bin2.E$Theta[length(Neg.Bin2.E$Theta)],X = X.E.Train)
MaxValue.E.NB2<-apply(matrix(Y.E.NB2,ncol=length(X.I.Train)),1,max)
Neg.Bin2.E$SE
### Weapons
X.W.Train <- model.matrix(Events_Relevant_QGIS.Train$AtThreat~Weapons.Recoded.Train)
table(Weapons.Recoded.Train)

## Quasi-Poisson
Quasi.Poisson.W<-IWLS(Omega1 =var(Y)/mean(Y),Theta1 = 0,
                 tollvl =  10^-10,X=X.W.Train[NonHurdle,])
Y.W.QP <- Sample.Hurdle.Count(HurdleCoef = Hurdle.W$coefficients,Beta = Quasi.Poisson.W$Beta.head,
                              Omega=Quasi.Poisson.W$Omega[length(Quasi.Poisson.W$Omega)],X=X.W.Train)

MaxValue.W.QP<-apply(matrix(Y.W.QP,ncol=length(X.I.Train)),1,max)

## Negative Binomial
Neg.Bin.W<-IWLS(Omega1 =1,Theta1 = var(Y)/mean(Y)^2 - 1/mean(Y),
                 tollvl =  10^-10,X=X.W.Train[NonHurdle,])
Y.W.NB <- Sample.Hurdle.Count(HurdleCoef = Hurdle.W$coefficients,Beta = Neg.Bin.W$Beta.head,
                               Theta = 1/Neg.Bin.W$Theta[length(Neg.Bin.W$Theta)],X=X.W.Train)
MaxValue.W.NB<-apply(matrix(Y.W.NB,ncol=length(X.I.Train)),1,max)

## Negative Binomial2 
Neg.Bin2.W<-IWLS(Omega1 =var(Y)/mean(Y),Theta1 = var(Y)/mean(Y)^2 - 1/mean(Y),
                 tollvl =  10^-10,X=X.W.Train[NonHurdle,])
Y.W.NB2 <- Sample.Hurdle.Count(HurdleCoef = Hurdle.W$coefficients,Beta = Neg.Bin2.W$Beta.head, Omega=Neg.Bin2.E$Omega[length(Neg.Bin2.E$Omega)],
                               Theta = 1/Neg.Bin2.W$Theta[length(Neg.Bin2.W$Theta)],X=X.W.Train)
MaxValue.W.NB2<-apply(matrix(Y.W.NB2,ncol=length(X.I.Train)),1,max)

