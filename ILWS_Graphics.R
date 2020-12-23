
################################### Graphics ############################################3
### Graphic MaxValue
Span=1100
par(mfrow=c(3,3))
par(mfrow=c(3,3),mar=c(5,5,5,5),las=1)

hist(MaxValue.I.QP[which(MaxValue.I.QP<Span)],mcex=3,main ="Intercept only", xlab = bquote("Maximum Value of Quasipoisson:" ~ omega ~"=" ~ 
                                                                                        .(round(Quasi.Poisson.I$Omega[length(Quasi.Poisson.I$Omega)],1)) ~ theta ~"=" ~ 
                                                                                        .(round(Quasi.Poisson.I$Theta[length(Quasi.Poisson.I$Theta)],1))),
     xlim=c(0,Span),col= "lightgreen",breaks = seq(0,Span,25) )
abline(v=max(Y),col=2)


hist(MaxValue.T.QP, main ="Y ~ Types of Terrorism", xlab = bquote("Maximum Value of Quasipoisson:" ~ omega ~"=" ~ 
                                                                      .(round(Quasi.Poisson.T$Omega[length(Quasi.Poisson.T$Omega)],1)) ~ theta ~"=" ~ 
                                                                      .(round(Quasi.Poisson.T$Theta[length(Quasi.Poisson.T$Theta)],1))),
     xlim=c(0,Span),col= "lightgreen",breaks = seq(0,Span,25) )
abline(v=max(Y),col=2)

hist(MaxValue.E.QP, main ="Y ~ Event type", xlab = bquote("Maximum Value of Quasipoisson:" ~ omega ~"=" ~ 
                                                              .(round(Quasi.Poisson.E$Omega[length(Quasi.Poisson.E$Omega)],1)) ~ theta ~"=" ~ 
                                                              .(round(Quasi.Poisson.E$Theta[length(Quasi.Poisson.E$Theta)],1))),
     xlim=c(0,Span),col= "lightgreen",breaks = seq(0,Span,25) )
abline(v=max(Y),col=2)


hist(MaxValue.I.NB, xlab = bquote("Maximum Value of Negative Binomial" ~ omega ~"=" ~ 
                                      .(round(Neg.Bin.I$Omega[length(Neg.Bin.I$Omega)],1)) ~ theta ~"=" ~ 
                                      .(round(Neg.Bin.I$Theta[length(Neg.Bin.I$Theta)],1))),
     main="",xlim=c(0,Span),col= "lightblue",breaks = seq(0,Span,25) )
abline(v=max(Y),col=2)

hist(MaxValue.T.NB, xlab = bquote("Maximum Value of Negative Binomial" ~ omega ~"=" ~ 
                                      .(round(Neg.Bin.T$Omega[length(Neg.Bin.T$Omega)],1)) ~ theta ~"=" ~ 
                                      .(round(Neg.Bin.T$Theta[length(Neg.Bin.T$Theta)],1))),
     main="",xlim=c(0,Span),col= "lightblue",breaks = seq(0,Span,25) )
abline(v=max(Y),col=2)

hist(MaxValue.E.NB, xlab = bquote("Maximum Value of Negative Binomial" ~ omega ~"=" ~ 
                                      .(round(Neg.Bin.E$Omega[length(Neg.Bin.E$Omega)],1)) ~ theta ~"=" ~ 
                                      .(round(Neg.Bin.E$Theta[length(Neg.Bin.E$Theta)],1))),
     main="",xlim=c(0,Span),col= "lightblue",breaks = seq(0,Span,25) )
abline(v=max(Y),col=2)

hist(MaxValue.I.NB2, xlab = bquote("Maximum Value of Negative Binomial2" ~ omega ~"=" ~ 
                                       .(round(Neg.Bin2.I$Omega[length(Neg.Bin2.I$Omega)],1)) ~ theta ~"=" ~ 
                                       .(round(Neg.Bin2.I$Theta[length(Neg.Bin2.I$Theta)],1))),
     main="",xlim=c(0,Span),col= "lightyellow",breaks = seq(0,Span,25) )
abline(v=max(Y),col=2)
hist(MaxValue.T.NB2[which(MaxValue.T.NB2<Span)], xlab = bquote("Maximum Value of Negative Binomial2" ~ omega ~"=" ~ 
                                                                   .(round(Neg.Bin2.T$Omega[length(Neg.Bin2.T$Omega)],1)) ~ theta ~"=" ~ 
                                                                   .(round(Neg.Bin2.T$Theta[length(Neg.Bin2.T$Theta)],1))),
     main="",xlim=c(0,Span),col= "lightyellow",breaks = seq(0,Span,25) )
abline(v=max(Y),col=2)


hist(MaxValue.E.NB2[which(MaxValue.T.NB2<Span)], xlab = bquote("Maximum Value of Negative Binomial2" ~ omega ~"=" ~ 
                                                                   .(round(Neg.Bin2.E$Omega[length(Neg.Bin2.E$Omega)],1)) ~ theta ~"=" ~ 
                                                                   .(round(Neg.Bin2.E$Theta[length(Neg.Bin2.E$Theta)],1))),
     main="",xlim=c(0,Span),col= "lightyellow",breaks = seq(0,Span,25) )
abline(v=max(Y),col=2)



### Histograms of % of zeros
# Intercept only
Y.Train<-Events_Relevant_QGIS.Train$AtThreat
hist(apply(matrix(Y.I.QP,ncol=length(Train)),1,function(x) mean(x==0)),breaks=seq(0.60,0.80,by=0.01),
     col= "lightgreen",main="Intercept Only", xlab=bquote("Percentage of zeros for QP with "~ omega ~"=" ~ 
                                                              .(round(Quasi.Poisson.I$Omega[length(Quasi.Poisson.I$Omega)],1)) ~ theta ~"=" ~ 
                                                              .(round(Quasi.Poisson.I$Theta[length(Quasi.Poisson.I$Theta)],1))))
abline(v=mean(Y.Train==0),col=2)

hist(apply(matrix(Y.T.QP,ncol=length(Train)),1,function(x) mean(x==0)),breaks=seq(0.60,0.80,by=0.01),
     col= "lightgreen",main="Y ~ Types of Terrorism", xlab=bquote("Percentage of zeros for QP with "~ omega ~"=" ~ 
                                        .(round(Quasi.Poisson.T$Omega[length(Quasi.Poisson.T$Omega)],1)) ~ theta ~"=" ~ 
                                        .(round(Quasi.Poisson.T$Theta[length(Quasi.Poisson.T$Theta)],1))))
abline(v=mean(Y.Train==0),col=2)

hist(apply(matrix(Y.E.QP,ncol=length(Train)),1,function(x) mean(x==0)),breaks=seq(0.60,0.80,by=0.01),
     col= "lightgreen",main="Y ~ Event type", xlab=bquote("Percentage of zeros for QP with "~ omega ~"=" ~ 
                                                 .(round(Quasi.Poisson.E$Omega[length(Quasi.Poisson.E$Omega)],1)) ~ theta ~"=" ~ 
                                                 .(round(Quasi.Poisson.E$Theta[length(Quasi.Poisson.E$Theta)],1))))
abline(v=mean(Y.Train==0),col=2)

hist(apply(matrix(Y.I.NB,ncol=length(Train)),1,function(x) mean(x==0)),breaks=seq(0.60,0.80,by=0.01),
     col= "lightblue",main="", xlab=bquote("Percentage of zeros for NB with "~ omega ~"=" ~ 
                                                                      .(round(Neg.Bin.I$Omega[length(Neg.Bin.I$Omega)],1)) ~ theta ~"=" ~ 
                                                                      .(round(Neg.Bin.I$Theta[length(Neg.Bin.I$Theta)],1))))
abline(v=mean(Y.Train==0),col=2)

hist(apply(matrix(Y.T.NB,ncol=length(Train)),1,function(x) mean(x==0)),breaks=seq(0.60,0.80,by=0.01),
     col= "lightblue",main="", xlab=bquote("Percentage of zeros for NB with "~ omega ~"=" ~ 
                                               .(round(Neg.Bin.T$Omega[length(Neg.Bin.T$Omega)],1)) ~ theta ~"=" ~ 
                                               .(round(Neg.Bin.T$Theta[length(Neg.Bin.T$Theta)],1))))
abline(v=mean(Y.Train==0),col=2)

hist(apply(matrix(Y.E.NB,ncol=length(Train)),1,function(x) mean(x==0)),breaks=seq(0.60,0.80,by=0.01),
     col= "lightblue",main="", xlab=bquote("Percentage of zeros for NB with "~ omega ~"=" ~ 
                                                 .(round(Neg.Bin.E$Omega[length(Neg.Bin.E$Omega)],1)) ~ theta ~"=" ~ 
                                                 .(round(Neg.Bin.E$Theta[length(Neg.Bin.E$Theta)],1))))
abline(v=mean(Y.Train==0),col=2)

hist(apply(matrix(Y.I.NB2,ncol=length(Train)),1,function(x) mean(x==0)),breaks=seq(0.60,0.80,by=0.01),
     col= "lightyellow",main="", xlab=bquote("Percentage of zeros for NB2 with "~ omega ~"=" ~ 
                                                              .(round(Neg.Bin2.I$Omega[length(Neg.Bin2.I$Omega)],1)) ~ theta ~"=" ~ 
                                                              .(round(Neg.Bin2.I$Theta[length(Neg.Bin2.I$Theta)],1))))
abline(v=mean(Y.Train==0),col=2)


hist(apply(matrix(Y.T.NB2,ncol=length(Train)),1,function(x) mean(x==0)),breaks=seq(0.60,0.80,by=0.01),
     col= "lightyellow",main="", xlab=bquote("Percentage of zeros for NB2 with "~ omega ~"=" ~ 
                                       .(round(Neg.Bin2.T$Omega[length(Neg.Bin2.T$Omega)],1)) ~ theta ~"=" ~ 
                                       .(round(Neg.Bin2.T$Theta[length(Neg.Bin2.T$Theta)],1))))
abline(v=mean(Y.Train==0),col=2)


hist(apply(matrix(Y.E.NB2,ncol=length(Train)),1,function(x) mean(x==0)),breaks=seq(0.60,0.80,by=0.01),
     col= "lightyellow",main="", xlab=bquote("Percentage of zeros for NB2 with "~ omega ~"=" ~ 
                                         .(round(Neg.Bin2.E$Omega[length(Neg.Bin2.E$Omega)],1)) ~ theta ~"=" ~ 
                                         .(round(Neg.Bin2.E$Theta[length(Neg.Bin2.E$Theta)],1))))
abline(v=mean(Y.Train==0),col=2)
###

mean(Y.Train==0)
### Histograms of % of one's
par(mfrow=c(3,3))
# Intercept only
Y.Train<-Events_Relevant_QGIS.Train$AtThreat
hist(apply(matrix(Y.I.QP,ncol=length(Train)),1,function(x) mean(x==1)),breaks=seq(0,0.25,by=0.01),
     col= "lightgreen",main="Intercept Only", xlab=bquote("Percentage of one's for QP with "~ omega ~"=" ~ 
                                                              .(round(Quasi.Poisson.I$Omega[length(Quasi.Poisson.I$Omega)],1)) ~ theta ~"=" ~ 
                                                              .(round(Quasi.Poisson.I$Theta[length(Quasi.Poisson.I$Theta)],1))))
abline(v=mean(Y.Train==1),col=2)

hist(apply(matrix(Y.T.QP,ncol=length(Train)),1,function(x) mean(x==1)),breaks=seq(0,0.25,by=0.01),
     col= "lightgreen",main="Y ~ Types of Terrorism", xlab=bquote("Percentage of one's for QP with "~ omega ~"=" ~ 
                                                                     .(round(Quasi.Poisson.T$Omega[length(Quasi.Poisson.T$Omega)],1)) ~ theta ~"=" ~ 
                                                                     .(round(Quasi.Poisson.T$Theta[length(Quasi.Poisson.T$Theta)],1))))
abline(v=mean(Y.Train==1),col=2)

hist(apply(matrix(Y.E.QP,ncol=length(Train)),1,function(x) mean(x==1)),breaks=seq(0,0.25,by=0.01),
     col= "lightgreen",main="Y ~ Event type", xlab=bquote("Percentage of one's for QP with "~ omega ~"=" ~ 
                                                               .(round(Quasi.Poisson.E$Omega[length(Quasi.Poisson.E$Omega)],1)) ~ theta ~"=" ~ 
                                                               .(round(Quasi.Poisson.E$Theta[length(Quasi.Poisson.E$Theta)],1))))
abline(v=mean(Y.Train==1),col=2)

hist(apply(matrix(Y.I.NB,ncol=length(Train)),1,function(x) mean(x==1)),breaks=seq(0,0.25,by=0.01),
     col= "lightblue",main="", xlab=bquote("Percentage of one's for NB with "~ omega ~"=" ~ 
                                                .(round(Neg.Bin.I$Omega[length(Neg.Bin.I$Omega)],1)) ~ theta ~"=" ~ 
                                                .(round(Neg.Bin.I$Theta[length(Neg.Bin.I$Theta)],1))))
abline(v=mean(Y.Train==1),col=2)

hist(apply(matrix(Y.T.NB,ncol=length(Train)),1,function(x) mean(x==1)),breaks=seq(0,0.25,by=0.01),
     col= "lightblue",main="", xlab=bquote("Percentage of one's for NB with "~ omega ~"=" ~ 
                                               .(round(Neg.Bin.T$Omega[length(Neg.Bin.T$Omega)],1)) ~ theta ~"=" ~ 
                                               .(round(Neg.Bin.T$Theta[length(Neg.Bin.T$Theta)],1))))
abline(v=mean(Y.Train==1),col=2)

hist(apply(matrix(Y.E.NB,ncol=length(Train)),1,function(x) mean(x==1)),breaks=seq(0,0.25,by=0.01),
     col= "lightblue",main="", xlab=bquote("Percentage of one's for NB with "~ omega ~"=" ~ 
                                                 .(round(Neg.Bin.E$Omega[length(Neg.Bin.E$Omega)],1)) ~ theta ~"=" ~ 
                                                 .(round(Neg.Bin.E$Theta[length(Neg.Bin.E$Theta)],1))))
abline(v=mean(Y.Train==1),col=2)

hist(apply(matrix(Y.I.NB2,ncol=length(Train)),1,function(x) mean(x==1)),breaks=seq(0,0.25,by=0.01),
     col= "lightyellow",main="", xlab=bquote("Percentage of one's for NB2 with "~ omega ~"=" ~ 
                                                .(round(Neg.Bin2.I$Omega[length(Neg.Bin2.I$Omega)],1)) ~ theta ~"=" ~ 
                                                .(round(Neg.Bin2.I$Theta[length(Neg.Bin2.I$Theta)],1))))
abline(v=mean(Y.Train==1),col=2)


hist(apply(matrix(Y.T.NB2,ncol=length(Train)),1,function(x) mean(x==1)),breaks=seq(0,0.25,by=0.01),
     col= "lightyellow",main="", xlab=bquote("Percentage of one's for NB2 with "~ omega ~"=" ~ 
                                               .(round(Neg.Bin2.T$Omega[length(Neg.Bin2.T$Omega)],1)) ~ theta ~"=" ~ 
                                               .(round(Neg.Bin2.T$Theta[length(Neg.Bin2.T$Theta)],1))))
abline(v=mean(Y.Train==1),col=2)


hist(apply(matrix(Y.E.NB2,ncol=length(Train)),1,function(x) mean(x==1)),breaks=seq(0,0.25,by=0.01),
     col= "lightyellow",main="", xlab=bquote("Percentage of one's for NB2 with "~ omega ~"=" ~ 
                                                 .(round(Neg.Bin2.E$Omega[length(Neg.Bin2.E$Omega)],1)) ~ theta ~"=" ~ 
                                                 .(round(Neg.Bin2.E$Theta[length(Neg.Bin2.E$Theta)],1))))
abline(v=mean(Y.Train==1),col=2)
###

### Histograms of % of two's
par(mfrow=c(3,3))
# Intercept only
Y.Train<-Events_Relevant_QGIS.Train$AtThreat
hist(apply(matrix(Y.I.QP,ncol=length(Train)),1,function(x) mean(x==2)),breaks=  seq(0,0.1 ,by=0.005),
     col= "lightgreen",main="Intercept Only", xlab=bquote("Percentage of two's for QP with "~ omega ~"=" ~ 
                                                              .(round(Quasi.Poisson.I$Omega[length(Quasi.Poisson.I$Omega)],1)) ~ theta ~"=" ~ 
                                                              .(round(Quasi.Poisson.I$Theta[length(Quasi.Poisson.I$Theta)],1))))
abline(v=mean(Y.Train==2),col=2)

hist(apply(matrix(Y.T.QP,ncol=length(Train)),1,function(x) mean(x==2)),breaks=  seq(0,0.1 ,by=0.005),
     col= "lightgreen",main="Y ~ Types of Terrorism", xlab=bquote("Percentage of two's for QP with "~ omega ~"=" ~ 
                                                                      .(round(Quasi.Poisson.T$Omega[length(Quasi.Poisson.T$Omega)],1)) ~ theta ~"=" ~ 
                                                                      .(round(Quasi.Poisson.T$Theta[length(Quasi.Poisson.T$Theta)],1))))
abline(v=mean(Y.Train==2),col=2)

hist(apply(matrix(Y.E.QP,ncol=length(Train)),1,function(x) mean(x==2)),breaks=  seq(0,0.1 ,by=0.005),
     col= "lightgreen",main="Y ~ Event type", xlab=bquote("Percentage of two's for QP with "~ omega ~"=" ~ 
                                                              .(round(Quasi.Poisson.E$Omega[length(Quasi.Poisson.E$Omega)],1)) ~ theta ~"=" ~ 
                                                              .(round(Quasi.Poisson.E$Theta[length(Quasi.Poisson.E$Theta)],1))))
abline(v=mean(Y.Train==2),col=2)

hist(apply(matrix(Y.I.NB,ncol=length(Train)),1,function(x) mean(x==2)),breaks=  seq(0,0.1 ,by=0.005),
     col= "lightblue",main="", xlab=bquote("Percentage of two's for NB with "~ omega ~"=" ~ 
                                               .(round(Neg.Bin.I$Omega[length(Neg.Bin.I$Omega)],1)) ~ theta ~"=" ~ 
                                               .(round(Neg.Bin.I$Theta[length(Neg.Bin.I$Theta)],1))))
abline(v=mean(Y.Train==2),col=2)

hist(apply(matrix(Y.T.NB,ncol=length(Train)),1,function(x) mean(x==2)),breaks=  seq(0,0.1 ,by=0.005),
     col= "lightblue",main="", xlab=bquote("Percentage of two's for NB with "~ omega ~"=" ~ 
                                               .(round(Neg.Bin.T$Omega[length(Neg.Bin.T$Omega)],1)) ~ theta ~"=" ~ 
                                               .(round(Neg.Bin.T$Theta[length(Neg.Bin.T$Theta)],1))))
abline(v=mean(Y.Train==2),col=2)

hist(apply(matrix(Y.E.NB,ncol=length(Train)),1,function(x) mean(x==2)),breaks=  seq(0,0.1 ,by=0.005),
     col= "lightblue",main="", xlab=bquote("Percentage of two's for NB with "~ omega ~"=" ~ 
                                               .(round(Neg.Bin.E$Omega[length(Neg.Bin.E$Omega)],1)) ~ theta ~"=" ~ 
                                               .(round(Neg.Bin.E$Theta[length(Neg.Bin.E$Theta)],1))))
abline(v=mean(Y.Train==2),col=2)

hist(apply(matrix(Y.I.NB2,ncol=length(Train)),1,function(x) mean(x==2)),breaks=  seq(0,0.1 ,by=0.005),
     col= "lightyellow",main="", xlab=bquote("Percentage of two's for NB2 with "~ omega ~"=" ~ 
                                                 .(round(Neg.Bin2.I$Omega[length(Neg.Bin2.I$Omega)],1)) ~ theta ~"=" ~ 
                                                 .(round(Neg.Bin2.I$Theta[length(Neg.Bin2.I$Theta)],1))))
abline(v=mean(Y.Train==2),col=2)


hist(apply(matrix(Y.T.NB2,ncol=length(Train)),1,function(x) mean(x==2)),breaks=  seq(0,0.1 ,by=0.005),
     col= "lightyellow",main="", xlab=bquote("Percentage of two's for NB2 with "~ omega ~"=" ~ 
                                                 .(round(Neg.Bin2.T$Omega[length(Neg.Bin2.T$Omega)],1)) ~ theta ~"=" ~ 
                                                 .(round(Neg.Bin2.T$Theta[length(Neg.Bin2.T$Theta)],1))))
abline(v=mean(Y.Train==2),col=2)


hist(apply(matrix(Y.E.NB2,ncol=length(Train)),1,function(x) mean(x==2)),breaks=  seq(0,0.1 ,by=0.005),
     col= "lightyellow",main="", xlab=bquote("Percentage of two's for NB2 with "~ omega ~"=" ~ 
                                                 .(round(Neg.Bin2.E$Omega[length(Neg.Bin2.E$Omega)],1)) ~ theta ~"=" ~ 
                                                 .(round(Neg.Bin2.E$Theta[length(Neg.Bin2.E$Theta)],1))))
abline(v=mean(Y.Train==2),col=2)
###

### Histograms of % of three's
par(mfrow=c(3,3))
# Intercept only
Y.Train<-Events_Relevant_QGIS.Train$AtThreat
hist(apply(matrix(Y.I.QP,ncol=length(Train)),1,function(x) mean(x==3)),breaks=  seq(0,0.1 ,by=0.005),
     col= "lightgreen",main="Intercept Only", xlab=bquote("Percentage of three's for QP with "~ omega ~"=" ~ 
                                                              .(round(Quasi.Poisson.I$Omega[length(Quasi.Poisson.I$Omega)],1)) ~ theta ~"=" ~ 
                                                              .(round(Quasi.Poisson.I$Theta[length(Quasi.Poisson.I$Theta)],1))))
abline(v=mean(Y.Train==3),col=2)

hist(apply(matrix(Y.T.QP,ncol=length(Train)),1,function(x) mean(x==3)),breaks=  seq(0,0.1 ,by=0.005),
     col= "lightgreen",main="Y ~ Types of Terrorism", xlab=bquote("Percentage of three's for QP with "~ omega ~"=" ~ 
                                                                      .(round(Quasi.Poisson.T$Omega[length(Quasi.Poisson.T$Omega)],1)) ~ theta ~"=" ~ 
                                                                      .(round(Quasi.Poisson.T$Theta[length(Quasi.Poisson.T$Theta)],1))))
abline(v=mean(Y.Train==3),col=2)

hist(apply(matrix(Y.E.QP,ncol=length(Train)),1,function(x) mean(x==3)),breaks=  seq(0,0.1 ,by=0.005),
     col= "lightgreen",main="Y ~ Event type", xlab=bquote("Percentage of three's for QP with "~ omega ~"=" ~ 
                                                              .(round(Quasi.Poisson.E$Omega[length(Quasi.Poisson.E$Omega)],1)) ~ theta ~"=" ~ 
                                                              .(round(Quasi.Poisson.E$Theta[length(Quasi.Poisson.E$Theta)],1))))
abline(v=mean(Y.Train==3),col=2)

hist(apply(matrix(Y.I.NB,ncol=length(Train)),1,function(x) mean(x==3)),breaks=  seq(0,0.1 ,by=0.005),
     col= "lightblue",main="", xlab=bquote("Percentage of three's for NB with "~ omega ~"=" ~ 
                                               .(round(Neg.Bin.I$Omega[length(Neg.Bin.I$Omega)],1)) ~ theta ~"=" ~ 
                                               .(round(Neg.Bin.I$Theta[length(Neg.Bin.I$Theta)],1))))
abline(v=mean(Y.Train==3),col=2)

hist(apply(matrix(Y.T.NB,ncol=length(Train)),1,function(x) mean(x==3)),breaks=  seq(0,0.1 ,by=0.005),
     col= "lightblue",main="", xlab=bquote("Percentage of three's for NB with "~ omega ~"=" ~ 
                                               .(round(Neg.Bin.T$Omega[length(Neg.Bin.T$Omega)],1)) ~ theta ~"=" ~ 
                                               .(round(Neg.Bin.T$Theta[length(Neg.Bin.T$Theta)],1))))
abline(v=mean(Y.Train==3),col=2)

hist(apply(matrix(Y.E.NB,ncol=length(Train)),1,function(x) mean(x==3)),breaks=  seq(0,0.1 ,by=0.005),
     col= "lightblue",main="", xlab=bquote("Percentage of three's for NB with "~ omega ~"=" ~ 
                                               .(round(Neg.Bin.E$Omega[length(Neg.Bin.E$Omega)],1)) ~ theta ~"=" ~ 
                                               .(round(Neg.Bin.E$Theta[length(Neg.Bin.E$Theta)],1))))
abline(v=mean(Y.Train==3),col=2)

hist(apply(matrix(Y.I.NB2,ncol=length(Train)),1,function(x) mean(x==3)),breaks=  seq(0,0.1 ,by=0.005),
     col= "lightyellow",main="", xlab=bquote("Percentage of three's for NB2 with "~ omega ~"=" ~ 
                                                 .(round(Neg.Bin2.I$Omega[length(Neg.Bin2.I$Omega)],1)) ~ theta ~"=" ~ 
                                                 .(round(Neg.Bin2.I$Theta[length(Neg.Bin2.I$Theta)],1))))
abline(v=mean(Y.Train==3),col=2)


hist(apply(matrix(Y.T.NB2,ncol=length(Train)),1,function(x) mean(x==3)),breaks=  seq(0,0.1 ,by=0.005),
     col= "lightyellow",main="", xlab=bquote("Percentage of three's for NB2 with "~ omega ~"=" ~ 
                                                 .(round(Neg.Bin2.T$Omega[length(Neg.Bin2.T$Omega)],1)) ~ theta ~"=" ~ 
                                                 .(round(Neg.Bin2.T$Theta[length(Neg.Bin2.T$Theta)],1))))
abline(v=mean(Y.Train==3),col=2)


hist(apply(matrix(Y.E.NB2,ncol=length(Train)),1,function(x) mean(x==3)),breaks=  seq(0,0.1 ,by=0.005),
     col= "lightyellow",main="", xlab=bquote("Percentage of three's for NB2 with "~ omega ~"=" ~ 
                                                 .(round(Neg.Bin2.E$Omega[length(Neg.Bin2.E$Omega)],1)) ~ theta ~"=" ~ 
                                                 .(round(Neg.Bin2.E$Theta[length(Neg.Bin2.E$Theta)],1))))
abline(v=mean(Y.Train==3),col=2)
###

### Histograms of % of four's
par(mfrow=c(3,3))
# Intercept only
Y.Train<-Events_Relevant_QGIS.Train$AtThreat
hist(apply(matrix(Y.I.QP,ncol=length(Train)),1,function(x) mean(x==4)),breaks=  seq(0,0.1 ,by=0.005),
     col= "lightgreen",main="Intercept Only", xlab=bquote("Percentage of four's for QP with "~ omega ~"=" ~ 
                                                              .(round(Quasi.Poisson.I$Omega[length(Quasi.Poisson.I$Omega)],1)) ~ theta ~"=" ~ 
                                                              .(round(Quasi.Poisson.I$Theta[length(Quasi.Poisson.I$Theta)],1))))
abline(v=mean(Y.Train==4),col=2)

hist(apply(matrix(Y.T.QP,ncol=length(Train)),1,function(x) mean(x==4)),breaks=  seq(0,0.1 ,by=0.005),
     col= "lightgreen",main="Y ~ Types of Terrorism", xlab=bquote("Percentage of four's for QP with "~ omega ~"=" ~ 
                                                                      .(round(Quasi.Poisson.T$Omega[length(Quasi.Poisson.T$Omega)],1)) ~ theta ~"=" ~ 
                                                                      .(round(Quasi.Poisson.T$Theta[length(Quasi.Poisson.T$Theta)],1))))
abline(v=mean(Y.Train==4),col=2)

hist(apply(matrix(Y.E.QP,ncol=length(Train)),1,function(x) mean(x==4)),breaks=  seq(0,0.1 ,by=0.005),
     col= "lightgreen",main="Y ~ Event type", xlab=bquote("Percentage of four's for QP with "~ omega ~"=" ~ 
                                                              .(round(Quasi.Poisson.E$Omega[length(Quasi.Poisson.E$Omega)],1)) ~ theta ~"=" ~ 
                                                              .(round(Quasi.Poisson.E$Theta[length(Quasi.Poisson.E$Theta)],1))))
abline(v=mean(Y.Train==4),col=2)

hist(apply(matrix(Y.I.NB,ncol=length(Train)),1,function(x) mean(x==4)),breaks=  seq(0,0.1 ,by=0.005),
     col= "lightblue",main="", xlab=bquote("Percentage of four's for NB with "~ omega ~"=" ~ 
                                               .(round(Neg.Bin.I$Omega[length(Neg.Bin.I$Omega)],1)) ~ theta ~"=" ~ 
                                               .(round(Neg.Bin.I$Theta[length(Neg.Bin.I$Theta)],1))))
abline(v=mean(Y.Train==4),col=2)

hist(apply(matrix(Y.T.NB,ncol=length(Train)),1,function(x) mean(x==4)),breaks=  seq(0,0.1 ,by=0.005),
     col= "lightblue",main="", xlab=bquote("Percentage of four's for NB with "~ omega ~"=" ~ 
                                               .(round(Neg.Bin.T$Omega[length(Neg.Bin.T$Omega)],1)) ~ theta ~"=" ~ 
                                               .(round(Neg.Bin.T$Theta[length(Neg.Bin.T$Theta)],1))))
abline(v=mean(Y.Train==4),col=2)

hist(apply(matrix(Y.E.NB,ncol=length(Train)),1,function(x) mean(x==4)),breaks=  seq(0,0.1 ,by=0.005),
     col= "lightblue",main="", xlab=bquote("Percentage of four's for NB with "~ omega ~"=" ~ 
                                               .(round(Neg.Bin.E$Omega[length(Neg.Bin.E$Omega)],1)) ~ theta ~"=" ~ 
                                               .(round(Neg.Bin.E$Theta[length(Neg.Bin.E$Theta)],1))))
abline(v=mean(Y.Train==4),col=2)

hist(apply(matrix(Y.I.NB2,ncol=length(Train)),1,function(x) mean(x==4)),breaks=  seq(0,0.1 ,by=0.005),
     col= "lightyellow",main="", xlab=bquote("Percentage of four's for NB2 with "~ omega ~"=" ~ 
                                                 .(round(Neg.Bin2.I$Omega[length(Neg.Bin2.I$Omega)],1)) ~ theta ~"=" ~ 
                                                 .(round(Neg.Bin2.I$Theta[length(Neg.Bin2.I$Theta)],1))))
abline(v=mean(Y.Train==4),col=2)


hist(apply(matrix(Y.T.NB2,ncol=length(Train)),1,function(x) mean(x==4)),breaks=  seq(0,0.1 ,by=0.005),
     col= "lightyellow",main="", xlab=bquote("Percentage of four's for NB2 with "~ omega ~"=" ~ 
                                                 .(round(Neg.Bin2.T$Omega[length(Neg.Bin2.T$Omega)],1)) ~ theta ~"=" ~ 
                                                 .(round(Neg.Bin2.T$Theta[length(Neg.Bin2.T$Theta)],1))))
abline(v=mean(Y.Train==4),col=2)


hist(apply(matrix(Y.E.NB2,ncol=length(Train)),1,function(x) mean(x==4)),breaks=  seq(0,0.1 ,by=0.005),
     col= "lightyellow",main="", xlab=bquote("Percentage of four's for NB2 with "~ omega ~"=" ~ 
                                                 .(round(Neg.Bin2.E$Omega[length(Neg.Bin2.E$Omega)],1)) ~ theta ~"=" ~ 
                                                 .(round(Neg.Bin2.E$Theta[length(Neg.Bin2.E$Theta)],1))))
abline(v=mean(Y.Train==4),col=2)
###


### Histograms of % of values  >10  &  <100
par(mfrow=c(3,3))
# Intercept only
hist(apply(matrix(Y.I.QP,ncol=length(Train)),1,function(x) mean(x>10 & x<100)),breaks=  seq(0,0.25 ,by=0.01),
     col= "lightgreen",main="Intercept Only", xlab=bquote("Percentage of  10 < Y > 100 for QP with "~ omega ~"=" ~ 
                                                              .(round(Quasi.Poisson.I$Omega[length(Quasi.Poisson.I$Omega)],1)) ~ theta ~"=" ~ 
                                                              .(round(Quasi.Poisson.I$Theta[length(Quasi.Poisson.I$Theta)],1))))
abline(v=mean(Y.Train>10 & Y.Train<100),col=2)

hist(apply(matrix(Y.T.QP,ncol=length(Train)),1,function(x) mean(x>10 & x<100)),breaks=  seq(0,0.25 ,by=0.01),
     col= "lightgreen",main="Y ~ Types of Terrorism", xlab=bquote("Percentage of  10 < Y > 100 for QP with "~ omega ~"=" ~ 
                                                                      .(round(Quasi.Poisson.T$Omega[length(Quasi.Poisson.T$Omega)],1)) ~ theta ~"=" ~ 
                                                                      .(round(Quasi.Poisson.T$Theta[length(Quasi.Poisson.T$Theta)],1))))
abline(v=mean(Y.Train>10 & Y.Train<100),col=2)

hist(apply(matrix(Y.E.QP,ncol=length(Train)),1,function(x) mean(x>10 & x<100)),breaks=  seq(0,0.25 ,by=0.01),
     col= "lightgreen",main="Y ~ Event type", xlab=bquote("Percentage of  10 < Y > 100 for QP with "~ omega ~"=" ~ 
                                                              .(round(Quasi.Poisson.E$Omega[length(Quasi.Poisson.E$Omega)],1)) ~ theta ~"=" ~ 
                                                              .(round(Quasi.Poisson.E$Theta[length(Quasi.Poisson.E$Theta)],1))))
abline(v=mean(Y.Train>10 & Y.Train<100),col=2)

hist(apply(matrix(Y.I.NB,ncol=length(Train)),1,function(x) mean(x>10 & x<100)),breaks=  seq(0,0.25 ,by=0.01),
     col= "lightblue",main="", xlab=bquote("Percentage of  10 < Y > 100 for NB with "~ omega ~"=" ~ 
                                               .(round(Neg.Bin.I$Omega[length(Neg.Bin.I$Omega)],1)) ~ theta ~"=" ~ 
                                               .(round(Neg.Bin.I$Theta[length(Neg.Bin.I$Theta)],1))))
abline(v=mean(Y.Train>10 & Y.Train<100),col=2)

hist(apply(matrix(Y.T.NB,ncol=length(Train)),1,function(x) mean(x>10 & x<100)),breaks=  seq(0,0.25 ,by=0.01),
     col= "lightblue",main="", xlab=bquote("Percentage of  10 < Y > 100 for NB with "~ omega ~"=" ~ 
                                               .(round(Neg.Bin.T$Omega[length(Neg.Bin.T$Omega)],1)) ~ theta ~"=" ~ 
                                               .(round(Neg.Bin.T$Theta[length(Neg.Bin.T$Theta)],1))))
abline(v=mean(Y.Train>10 & Y.Train<100),col=2)

hist(apply(matrix(Y.E.NB,ncol=length(Train)),1,function(x) mean(x>10 & x<100)),breaks=  seq(0,0.25 ,by=0.01),
     col= "lightblue",main="", xlab=bquote("Percentage of  10 < Y > 100 for NB with "~ omega ~"=" ~ 
                                               .(round(Neg.Bin.E$Omega[length(Neg.Bin.E$Omega)],1)) ~ theta ~"=" ~ 
                                               .(round(Neg.Bin.E$Theta[length(Neg.Bin.E$Theta)],1))))
abline(v=mean(Y.Train>10 & Y.Train<100),col=2)

hist(apply(matrix(Y.I.NB2,ncol=length(Train)),1,function(x) mean(x>10 & x<100)),breaks=  seq(0,0.25 ,by=0.01),
     col= "lightyellow",main="", xlab=bquote("Percentage of  10 < Y > 100 for NB2 with "~ omega ~"=" ~ 
                                                 .(round(Neg.Bin2.I$Omega[length(Neg.Bin2.I$Omega)],1)) ~ theta ~"=" ~ 
                                                 .(round(Neg.Bin2.I$Theta[length(Neg.Bin2.I$Theta)],1))))
abline(v=mean(Y.Train>10 & Y.Train<100),col=2)


hist(apply(matrix(Y.T.NB2,ncol=length(Train)),1,function(x) mean(x>10 & x<100)),breaks=  seq(0,0.25 ,by=0.01),
     col= "lightyellow",main="", xlab=bquote("Percentage of  10 < Y > 100 for NB2 with "~ omega ~"=" ~ 
                                                 .(round(Neg.Bin2.T$Omega[length(Neg.Bin2.T$Omega)],1)) ~ theta ~"=" ~ 
                                                 .(round(Neg.Bin2.T$Theta[length(Neg.Bin2.T$Theta)],1))))
abline(v=mean(Y.Train>10 & Y.Train<100),col=2)


hist(apply(matrix(Y.E.NB2,ncol=length(Train)),1,function(x) mean(x>10 & x<100)),breaks=  seq(0,0.25 ,by=0.01),
     col= "lightyellow",main="", xlab=bquote("Percentage of 10 < Y > 100 for NB2 with "~ omega ~"=" ~ 
                                                 .(round(Neg.Bin2.E$Omega[length(Neg.Bin2.E$Omega)],1)) ~ theta ~"=" ~ 
                                                 .(round(Neg.Bin2.E$Theta[length(Neg.Bin2.E$Theta)],1))))
abline(v=mean(Y.Train>10 & Y.Train<100),col=2)
###

## Weapons

par(mfrow=c(1,3),mar=c(5,5,5,5),las=1)
hist(MaxValue.W.QP, main ="", xlab = bquote("Maximum Value of Quasipoisson:" ~ omega ~"=" ~ 
                                                .(round(Quasi.Poisson.W$Omega[length(Quasi.Poisson.W$Omega)],1)) ~ theta ~"=" ~ 
                                                .(round(Quasi.Poisson.W$Theta[length(Quasi.Poisson.W$Theta)],1))),
     xlim=c(0,Span),col= "lightgreen",breaks = seq(0,Span,25) )
abline(v=max(Y),col=2)

hist(MaxValue.W.NB, main ="", xlab = bquote("Maximum Value of Negative Binomial:" ~ omega ~"=" ~ 
                                                .(round(Neg.Bin.W$Omega[length(Neg.Bin.W$Omega)],1)) ~ theta ~"=" ~ 
                                                .(round(Neg.Bin.W$Theta[length(Neg.Bin.W$Theta)],1))),
     xlim=c(0,Span),col= "lightblue",breaks = seq(0,Span,25) )
abline(v=max(Y),col=2)

hist(MaxValue.W.NB2, main ="", xlab = bquote("Maximum Value of Negative Binomial2:" ~ omega ~"=" ~ 
                                                 .(round(Neg.Bin2.W$Omega[length(Neg.Bin2.W$Omega)],1)) ~ theta ~"=" ~ 
                                                 .(round(Neg.Bin2.W$Theta[length(Neg.Bin2.W$Theta)],1))),
     xlim=c(0,Span),col= "lightyellow",breaks = seq(0,Span,25) )
abline(v=max(Y),col=2)
## >10 <100 (Weapons)
hist(apply(matrix(Y.W.QP,ncol=length(Train)),1,function(x) mean(x>10 & x<100)),breaks=  seq(0,0.25 ,by=0.01),
     col= "lightgreen",main="", xlab=bquote("Percentage of 10 < Y > 100 for QP with "~ omega ~"=" ~ 
                                                 .(round(Quasi.Poisson.W$Omega[length(Quasi.Poisson.W$Omega)],1)) ~ theta ~"=" ~ 
                                                 .(round(Quasi.Poisson.W$Theta[length(Quasi.Poisson.W$Theta)],1))))
abline(v=mean(Y.Train>10 & Y.Train<100),col=2)

hist(apply(matrix(Y.W.NB,ncol=length(Train)),1,function(x) mean(x>10 & x<100)),breaks=  seq(0,0.25 ,by=0.01),
     col= "lightblue",main="", xlab=bquote("Percentage of 10 < Y > 100 for NB with "~ omega ~"=" ~ 
                                                 .(round(Neg.Bin.W$Omega[length(Neg.Bin.W$Omega)],1)) ~ theta ~"=" ~ 
                                                 .(round(Neg.Bin.W$Theta[length(Neg.Bin.W$Theta)],1))))
abline(v=mean(Y.Train>10 & Y.Train<100),col=2)

hist(apply(matrix(Y.W.NB2,ncol=length(Train)),1,function(x) mean(x>10 & x<100)),breaks=  seq(0,0.25 ,by=0.01),
     col= "lightyellow",main="", xlab=bquote("Percentage of 10 < Y > 100 for NB2 with "~ omega ~"=" ~ 
                                                 .(round(Neg.Bin2.W$Omega[length(Neg.Bin2.W$Omega)],1)) ~ theta ~"=" ~ 
                                                 .(round(Neg.Bin2.W$Theta[length(Neg.Bin2.W$Theta)],1))))
abline(v=mean(Y.Train>10 & Y.Train<100),col=2)

# one's (Weapons)
hist(apply(matrix(Y.W.QP,ncol=length(Train)),1,function(x) mean(x==1)),breaks=  seq(0,0.25 ,by=0.01),
     col= "lightgreen",main="", xlab=bquote("Percentage of one's for QP with "~ omega ~"=" ~ 
                                                 .(round(Quasi.Poisson.W$Omega[length(Quasi.Poisson.W$Omega)],1)) ~ theta ~"=" ~ 
                                                 .(round(Quasi.Poisson.W$Theta[length(Quasi.Poisson.W$Theta)],1))))
abline(v=mean(Y.Train==1),col=2)

hist(apply(matrix(Y.W.NB,ncol=length(Train)),1,function(x) mean(x==1)),breaks=  seq(0,0.25 ,by=0.01),
     col= "lightblue",main="", xlab=bquote("Percentage of one's for NB with "~ omega ~"=" ~ 
                                                 .(round(Neg.Bin.W$Omega[length(Neg.Bin.W$Omega)],1)) ~ theta ~"=" ~ 
                                                 .(round(Neg.Bin.W$Theta[length(Neg.Bin.W$Theta)],1))))
abline(v=mean(Y.Train==1),col=2)

hist(apply(matrix(Y.W.NB2,ncol=length(Train)),1,function(x) mean(x==1)),breaks=  seq(0,0.25 ,by=0.01),
     col= "lightyellow",main="", xlab=bquote("Percentage of one's for NB2 with "~ omega ~"=" ~ 
                                                 .(round(Neg.Bin2.W$Omega[length(Neg.Bin2.W$Omega)],1)) ~ theta ~"=" ~ 
                                                 .(round(Neg.Bin2.W$Theta[length(Neg.Bin2.W$Theta)],1))))
abline(v=mean(Y.Train==1),col=2)

# two's (Weapons)
hist(apply(matrix(Y.W.QP,ncol=length(Train)),1,function(x) mean(x==2)),breaks=  seq(0,0.25 ,by=0.01),
     col= "lightgreen",main="", xlab=bquote("Percentage of two's for QP with "~ omega ~"=" ~ 
                                                .(round(Quasi.Poisson.W$Omega[length(Quasi.Poisson.W$Omega)],1)) ~ theta ~"=" ~ 
                                                .(round(Quasi.Poisson.W$Theta[length(Quasi.Poisson.W$Theta)],1))))
abline(v=mean(Y.Train==2),col=2)

hist(apply(matrix(Y.W.NB,ncol=length(Train)),1,function(x) mean(x==2)),breaks=  seq(0,0.25 ,by=0.01),
     col= "lightblue",main="", xlab=bquote("Percentage of two's for NB with "~ omega ~"=" ~ 
                                               .(round(Neg.Bin.W$Omega[length(Neg.Bin.W$Omega)],1)) ~ theta ~"=" ~ 
                                               .(round(Neg.Bin.W$Theta[length(Neg.Bin.W$Theta)],1))))
abline(v=mean(Y.Train==2),col=2)

hist(apply(matrix(Y.W.NB2,ncol=length(Train)),1,function(x) mean(x==2)),breaks=  seq(0,0.25 ,by=0.01),
     col= "lightyellow",main="", xlab=bquote("Percentage of two's for NB2 with "~ omega ~"=" ~ 
                                                 .(round(Neg.Bin2.W$Omega[length(Neg.Bin2.W$Omega)],1)) ~ theta ~"=" ~ 
                                                 .(round(Neg.Bin2.W$Theta[length(Neg.Bin2.W$Theta)],1))))
abline(v=mean(Y.Train==2),col=2)

# threes (Weapons)
hist(apply(matrix(Y.W.QP,ncol=length(Train)),1,function(x) mean(x==3)),breaks=  seq(0,0.25 ,by=0.01),
     col= "lightgreen",main="", xlab=bquote("Percentage of three's for QP with "~ omega ~"=" ~ 
                                                .(round(Quasi.Poisson.W$Omega[length(Quasi.Poisson.W$Omega)],1)) ~ theta ~"=" ~ 
                                                .(round(Quasi.Poisson.W$Theta[length(Quasi.Poisson.W$Theta)],1))))
abline(v=mean(Y.Train==3),col=2)

hist(apply(matrix(Y.W.NB,ncol=length(Train)),1,function(x) mean(x==3)),breaks=  seq(0,0.25 ,by=0.01),
     col= "lightblue",main="", xlab=bquote("Percentage of three's for NB with "~ omega ~"=" ~ 
                                               .(round(Neg.Bin.W$Omega[length(Neg.Bin.W$Omega)],1)) ~ theta ~"=" ~ 
                                               .(round(Neg.Bin.W$Theta[length(Neg.Bin.W$Theta)],1))))
abline(v=mean(Y.Train==3),col=2)

hist(apply(matrix(Y.W.NB2,ncol=length(Train)),1,function(x) mean(x==3)),breaks=  seq(0,0.25 ,by=0.01),
     col= "lightyellow",main="", xlab=bquote("Percentage of three's for NB2 with "~ omega ~"=" ~ 
                                                 .(round(Neg.Bin2.W$Omega[length(Neg.Bin2.W$Omega)],1)) ~ theta ~"=" ~ 
                                                 .(round(Neg.Bin2.W$Theta[length(Neg.Bin2.W$Theta)],1))))
abline(v=mean(Y.Train==3),col=2)
mean(Y.Train==0)
Y.Train[EventType.Recoded.Train=="Facility/Infrastructure Attack"]
######################################### Count values (IWLS step only)
## QP
Quasi.Poisson.Prob.I<-round(cbind(exp(c(Quasi.Poisson.I$Beta.head)),
                                  exp(Quasi.Poisson.I$Beta.head[,1]+qnorm(0.25)*Quasi.Poisson.I$SE),
                                  exp(Quasi.Poisson.I$Beta.head[,1]+qnorm(0.975)*Quasi.Poisson.I$SE)),3) # CI for E[Y|Y>0]

B2.QP.T<-Quasi.Poisson.T$Beta.head[2,1]+Quasi.Poisson.T$Beta.head[1,1]
B3.QP.T<-Quasi.Poisson.T$Beta.head[3,1]+Quasi.Poisson.T$Beta.head[1,1]
Quasi.Poisson.Prob.T<-round(cbind(exp(c(Quasi.Poisson.T$Beta.head[1,1],B2.QP.T,B3.QP.T)),
                          exp(c(Quasi.Poisson.T$Beta.head[1,1],B2.QP.T,B3.QP.T)+qnorm(0.25)*Quasi.Poisson.T$SE),
                          exp(c(Quasi.Poisson.T$Beta.head[1,1],B2.QP.T,B3.QP.T)+qnorm(0.975)*Quasi.Poisson.T$SE)),3)

B2.QP.E<-Quasi.Poisson.E$Beta.head[2,1]+Quasi.Poisson.E$Beta.head[1,1]
B3.QP.E<-Quasi.Poisson.E$Beta.head[3,1]+Quasi.Poisson.E$Beta.head[1,1]
B4.QP.E<-Quasi.Poisson.E$Beta.head[4,1]+Quasi.Poisson.E$Beta.head[1,1]
Quasi.Poisson.Prob.E<-round(cbind(exp(c(Quasi.Poisson.E$Beta.head[1,1],B2.QP.E,B3.QP.E,B4.QP.E)),
                                  exp(c(Quasi.Poisson.E$Beta.head[1,1],B2.QP.E,B3.QP.E,B4.QP.E)+qnorm(0.25)*Quasi.Poisson.E$SE),
                                  exp(c(Quasi.Poisson.E$Beta.head[1,1],B2.QP.E,B3.QP.E,B4.QP.E)+qnorm(0.975)*Quasi.Poisson.E$SE)),3)

B2.QP.W<-Quasi.Poisson.W$Beta.head[2,1]+Quasi.Poisson.W$Beta.head[1,1]
B3.QP.W<-Quasi.Poisson.W$Beta.head[3,1]+Quasi.Poisson.W$Beta.head[1,1]
B4.QP.W<-Quasi.Poisson.W$Beta.head[4,1]+Quasi.Poisson.W$Beta.head[1,1]
B5.QP.W<-Quasi.Poisson.W$Beta.head[5,1]+Quasi.Poisson.W$Beta.head[1,1]
B6.QP.W<-Quasi.Poisson.W$Beta.head[6,1]+Quasi.Poisson.W$Beta.head[1,1]
Quasi.Poisson.Prob.W<-round(cbind(exp(c(Quasi.Poisson.W$Beta.head[1,1],B2.QP.W,B3.QP.W,B4.QP.W,B5.QP.W,B6.QP.W)),
                                  exp(c(Quasi.Poisson.W$Beta.head[1,1],B2.QP.W,B3.QP.W,B4.QP.W,B5.QP.W,B6.QP.W)+qnorm(0.25)*Quasi.Poisson.W$SE),
                                  exp(c(Quasi.Poisson.W$Beta.head[1,1],B2.QP.W,B3.QP.W,B4.QP.W,B5.QP.W,B6.QP.W)+qnorm(0.975)*Quasi.Poisson.W$SE)),3)

## NB
Neg.Bin.Prob.I<-round(cbind(exp(c(Neg.Bin.I$Beta.head)),
                                  exp(Neg.Bin.I$Beta.head[,1]+qnorm(0.25)*Neg.Bin.I$SE),
                                  exp(Neg.Bin.I$Beta.head[,1]+qnorm(0.975)*Neg.Bin.I$SE)),3) # CI for E[Y|Y>0]

B2.NB.T<-Neg.Bin.T$Beta.head[2,1]+Neg.Bin.T$Beta.head[1,1]
B3.NB.T<-Neg.Bin.T$Beta.head[3,1]+Neg.Bin.T$Beta.head[1,1]
Neg.Bin.Prob.T<-round(cbind(exp(c(Neg.Bin.T$Beta.head[1,1],B2.NB.T,B3.NB.T)),
                                  exp(c(Neg.Bin.T$Beta.head[1,1],B2.NB.T,B3.NB.T)+qnorm(0.25)*Neg.Bin.T$SE),
                                  exp(c(Neg.Bin.T$Beta.head[1,1],B2.NB.T,B3.NB.T)+qnorm(0.975)*Neg.Bin.T$SE)),3)

B2.NB.E<-Neg.Bin.E$Beta.head[2,1]+Neg.Bin.E$Beta.head[1,1]
B3.NB.E<-Neg.Bin.E$Beta.head[3,1]+Neg.Bin.E$Beta.head[1,1]
B4.NB.E<-Neg.Bin.E$Beta.head[4,1]+Neg.Bin.E$Beta.head[1,1]
Neg.Bin.Prob.E<-round(cbind(exp(c(Neg.Bin.E$Beta.head[1,1],B2.NB.E,B3.NB.E,B4.NB.E)),
                                  exp(c(Neg.Bin.E$Beta.head[1,1],B2.NB.E,B3.NB.E,B4.NB.E)+qnorm(0.25)*Neg.Bin.E$SE),
                                  exp(c(Neg.Bin.E$Beta.head[1,1],B2.NB.E,B3.NB.E,B4.NB.E)+qnorm(0.975)*Neg.Bin.E$SE)),3)

B2.NB.W<-Neg.Bin.W$Beta.head[2,1]+Neg.Bin.W$Beta.head[1,1]
B3.NB.W<-Neg.Bin.W$Beta.head[3,1]+Neg.Bin.W$Beta.head[1,1]
B4.NB.W<-Neg.Bin.W$Beta.head[4,1]+Neg.Bin.W$Beta.head[1,1]
B5.NB.W<-Neg.Bin.W$Beta.head[5,1]+Neg.Bin.W$Beta.head[1,1]
B6.NB.W<-Neg.Bin.W$Beta.head[6,1]+Neg.Bin.W$Beta.head[1,1]
Neg.Bin.Prob.W<-round(cbind(exp(c(Neg.Bin.W$Beta.head[1,1],B2.NB.W,B3.NB.W,B4.NB.W,B5.NB.W,B6.NB.W)),
                                  exp(c(Neg.Bin.W$Beta.head[1,1],B2.NB.W,B3.NB.W,B4.NB.W,B5.NB.W,B6.NB.W)+qnorm(0.25)*Neg.Bin.W$SE),
                                  exp(c(Neg.Bin.W$Beta.head[1,1],B2.NB.W,B3.NB.W,B4.NB.W,B5.NB.W,B6.NB.W)+qnorm(0.975)*Neg.Bin.W$SE)),3)

## NB2
Neg.Bin2.Prob.I<-round(cbind(exp(c(Neg.Bin2.I$Beta.head)),
                            exp(Neg.Bin2.I$Beta.head[,1]+qnorm(0.25)*Neg.Bin2.I$SE),
                            exp(Neg.Bin2.I$Beta.head[,1]+qnorm(0.975)*Neg.Bin2.I$SE)),3) # CI for E[Y|Y>0]

B2.NB2.T<-Neg.Bin2.T$Beta.head[2,1]+Neg.Bin2.T$Beta.head[1,1]
B3.NB2.T<-Neg.Bin2.T$Beta.head[3,1]+Neg.Bin2.T$Beta.head[1,1]
Neg.Bin2.Prob.T<-round(cbind(exp(c(Neg.Bin2.T$Beta.head[1,1],B2.NB2.T,B3.NB2.T)),
                            exp(c(Neg.Bin2.T$Beta.head[1,1],B2.NB2.T,B3.NB2.T)+qnorm(0.25)*Neg.Bin2.T$SE),
                            exp(c(Neg.Bin2.T$Beta.head[1,1],B2.NB2.T,B3.NB2.T)+qnorm(0.975)*Neg.Bin2.T$SE)),3)

B2.NB2.E<-Neg.Bin2.E$Beta.head[2,1]+Neg.Bin2.E$Beta.head[1,1]
B3.NB2.E<-Neg.Bin2.E$Beta.head[3,1]+Neg.Bin2.E$Beta.head[1,1]
B4.NB2.E<-Neg.Bin2.E$Beta.head[4,1]+Neg.Bin2.E$Beta.head[1,1]
Neg.Bin2.Prob.E<-round(cbind(exp(c(Neg.Bin2.E$Beta.head[1,1],B2.NB2.E,B3.NB2.E,B4.NB2.E)),
                            exp(c(Neg.Bin2.E$Beta.head[1,1],B2.NB2.E,B3.NB2.E,B4.NB2.E)+qnorm(0.25)*Neg.Bin2.E$SE),
                            exp(c(Neg.Bin2.E$Beta.head[1,1],B2.NB2.E,B3.NB2.E,B4.NB2.E)+qnorm(0.975)*Neg.Bin2.E$SE)),3)

B2.NB2.W<-Neg.Bin2.W$Beta.head[2,1]+Neg.Bin2.W$Beta.head[1,1]
B3.NB2.W<-Neg.Bin2.W$Beta.head[3,1]+Neg.Bin2.W$Beta.head[1,1]
B4.NB2.W<-Neg.Bin2.W$Beta.head[4,1]+Neg.Bin2.W$Beta.head[1,1]
B5.NB2.W<-Neg.Bin2.W$Beta.head[5,1]+Neg.Bin2.W$Beta.head[1,1]
B6.NB2.W<-Neg.Bin2.W$Beta.head[6,1]+Neg.Bin2.W$Beta.head[1,1]
Neg.Bin2.Prob.W<-round(cbind(exp(c(Neg.Bin2.W$Beta.head[1,1],B2.NB2.W,B3.NB2.W,B4.NB2.W,B5.NB2.W,B6.NB2.W)),
                            exp(c(Neg.Bin2.W$Beta.head[1,1],B2.NB2.W,B3.NB2.W,B4.NB2.W,B5.NB2.W,B6.NB2.W)+qnorm(0.25)*Neg.Bin2.W$SE),
                            exp(c(Neg.Bin2.W$Beta.head[1,1],B2.NB2.W,B3.NB2.W,B4.NB2.W,B5.NB2.W,B6.NB2.W)+qnorm(0.975)*Neg.Bin2.W$SE)),3)




library(plotrix)
par(las=2)
par(mfrow=c(1,1),mar=c(15,3,1,1))

Pos.count<-c()
for(i in 1:14){
    Pos.count<-c(Pos.count,seq(-0.25,0.25,by=0.25)+i)
}


plotCI(Pos.count,y=log(c(Quasi.Poisson.Prob.I[,1],Neg.Bin.Prob.I[,1],Neg.Bin2.Prob.I[,1],
                     Quasi.Poisson.Prob.T[1,1],Neg.Bin.Prob.T[1,1],Neg.Bin2.Prob.T[1,1],
                     Quasi.Poisson.Prob.T[2,1],Neg.Bin.Prob.T[2,1],Neg.Bin2.Prob.T[2,1],
                     Quasi.Poisson.Prob.T[3,1],Neg.Bin.Prob.T[3,1],Neg.Bin2.Prob.T[3,1],
                     Quasi.Poisson.Prob.E[1,1],Neg.Bin.Prob.E[1,1],Neg.Bin2.Prob.E[1,1],
                     Quasi.Poisson.Prob.E[2,1],Neg.Bin.Prob.E[2,1],Neg.Bin2.Prob.E[2,1],
                     Quasi.Poisson.Prob.E[3,1],Neg.Bin.Prob.E[3,1],Neg.Bin2.Prob.E[3,1],
                     Quasi.Poisson.Prob.E[4,1],Neg.Bin.Prob.E[4,1],Neg.Bin2.Prob.E[4,1],
                     Quasi.Poisson.Prob.W[1,1],Neg.Bin.Prob.W[1,1],Neg.Bin2.Prob.W[1,1],
                     Quasi.Poisson.Prob.W[2,1],Neg.Bin.Prob.W[2,1],Neg.Bin2.Prob.W[2,1],
                     Quasi.Poisson.Prob.W[3,1],Neg.Bin.Prob.W[3,1],Neg.Bin2.Prob.W[3,1],
                     Quasi.Poisson.Prob.W[4,1],Neg.Bin.Prob.W[4,1],Neg.Bin2.Prob.W[4,1],
                     Quasi.Poisson.Prob.W[5,1],Neg.Bin.Prob.W[5,1],Neg.Bin2.Prob.W[5,1],
                     Quasi.Poisson.Prob.W[6,1],Neg.Bin.Prob.W[6,1],Neg.Bin2.Prob.W[6,1])),
       li = log(c(Quasi.Poisson.Prob.I[,2],Neg.Bin.Prob.I[,2],Neg.Bin2.Prob.I[,2],
              Quasi.Poisson.Prob.T[1,2],Neg.Bin.Prob.T[1,2],Neg.Bin2.Prob.T[1,2],
              Quasi.Poisson.Prob.T[2,2],Neg.Bin.Prob.T[2,2],Neg.Bin2.Prob.T[2,2],
              Quasi.Poisson.Prob.T[3,2],Neg.Bin.Prob.T[3,2],Neg.Bin2.Prob.T[3,2],
              Quasi.Poisson.Prob.E[1,2],Neg.Bin.Prob.E[1,2],Neg.Bin2.Prob.E[1,2],
              Quasi.Poisson.Prob.E[2,2],Neg.Bin.Prob.E[2,2],Neg.Bin2.Prob.E[2,2],
              Quasi.Poisson.Prob.E[3,2],Neg.Bin.Prob.E[3,2],Neg.Bin2.Prob.E[3,2],
              Quasi.Poisson.Prob.E[4,2],Neg.Bin.Prob.E[4,2],Neg.Bin2.Prob.E[4,2],
              Quasi.Poisson.Prob.W[1,2],Neg.Bin.Prob.W[1,2],Neg.Bin2.Prob.W[1,2],
              Quasi.Poisson.Prob.W[2,2],Neg.Bin.Prob.W[2,2],Neg.Bin2.Prob.W[2,2],
              Quasi.Poisson.Prob.W[3,2],Neg.Bin.Prob.W[3,2],Neg.Bin2.Prob.W[3,2],
              Quasi.Poisson.Prob.W[4,2],Neg.Bin.Prob.W[4,2],Neg.Bin2.Prob.W[4,2],
              Quasi.Poisson.Prob.W[5,2],Neg.Bin.Prob.W[5,2],Neg.Bin2.Prob.W[5,2],
              Quasi.Poisson.Prob.W[6,2],Neg.Bin.Prob.W[6,2],Neg.Bin2.Prob.W[6,2])),
       ui = log(c(Quasi.Poisson.Prob.I[,3],Neg.Bin.Prob.I[,3],Neg.Bin2.Prob.I[,3],
              Quasi.Poisson.Prob.T[1,3],Neg.Bin.Prob.T[1,3],Neg.Bin2.Prob.T[1,3],
              Quasi.Poisson.Prob.T[2,3],Neg.Bin.Prob.T[2,3],Neg.Bin2.Prob.T[2,3],
              Quasi.Poisson.Prob.T[3,3],Neg.Bin.Prob.T[3,3],Neg.Bin2.Prob.T[3,3],
              Quasi.Poisson.Prob.E[1,3],Neg.Bin.Prob.E[1,3],Neg.Bin2.Prob.E[1,3],
              Quasi.Poisson.Prob.E[2,3],Neg.Bin.Prob.E[2,3],Neg.Bin2.Prob.E[2,3],
              Quasi.Poisson.Prob.E[3,3],Neg.Bin.Prob.E[3,3],Neg.Bin2.Prob.E[3,3],
              Quasi.Poisson.Prob.E[4,3],Neg.Bin.Prob.E[4,3],Neg.Bin2.Prob.E[4,3],
              Quasi.Poisson.Prob.W[1,3],Neg.Bin.Prob.W[1,3],Neg.Bin2.Prob.W[1,3],
              Quasi.Poisson.Prob.W[2,3],Neg.Bin.Prob.W[2,3],Neg.Bin2.Prob.W[2,3],
              Quasi.Poisson.Prob.W[3,3],Neg.Bin.Prob.W[3,3],Neg.Bin2.Prob.W[3,3],
              Quasi.Poisson.Prob.W[4,3],Neg.Bin.Prob.W[4,3],Neg.Bin2.Prob.W[4,3],
              Quasi.Poisson.Prob.W[5,3],Neg.Bin.Prob.W[5,3],Neg.Bin2.Prob.W[5,3],
              Quasi.Poisson.Prob.W[6,3],Neg.Bin.Prob.W[6,3],Neg.Bin2.Prob.W[6,3]))
       ,scol=1,col=1,pt.bg=c(rep(c("lightgreen","lightblue","lightyellow"),4)),
           pch=21,xaxt="n",xlab="",ylab="",ylim=c(0,log(1800)))
legend(x=14.05,y=7.72,legend = c("QP","NB","NB2"),cex=0.7,pt.cex=1,pch=rep(21,3),
       col = c(1,1,1), pt.bg=c("lightgreen","lightblue","lightyellow") )
axis(1, at = 1:14,las=2, cex.axis=0.65,
     labels = c("E[ log(Y) | Y > 0 ]","E[ log(Y) | Type of Terrorism = Islamic Terror , Y > 0 ]","E[ log(Y) | Type of Terrorism = Left Terror , Y > 0 ]","E[ log(Y) | Type of Terrorism = Right Terror , Y > 0 ]",
                "E[ log(Y) | Event type = Bombing/Explosion , Y > 0 ]","E[ log(Y) | Event type = Infrastructure Attack , Y > 0 ]","E[ log(Y) | Event type = NSAG Attack , Y > 0 ]",
                "E[ log(Y) | Event type = Other , Y > 0 ]","E[ log(Y) | Weapons = Edged & Improvised , Y > 0 ]","E[ log(Y) | Weapons = Explosives , Y > 0 ]", "E[ log(Y) | Weapons = Firearm & Vehicle impact , Y > 0 ]",
                "E[ log(Y) | Weapons = Incendiary device , Y > 0 ]","E[ log(Y) | Weapons = Infrastructure Attack , Y > 0 ]", "E[ log(Y) | Weapons = Other , Y > 0 ]"))

abline(h=log(Quasi.Poisson.Prob.I[,1]),lty=2,col=2)
abline(v=c(1.5,4.5,8.5),lty=2,col=1)
text(x=c(1,3,6.5,11.5),y=rep(log(1600),4), labels=c("Y ~ 1","Y ~ Types of Terrorsim","Y ~ Event type","Y ~ Weapons"),cex=0.8)


#############################################
### 
Overdispersion.Variance <- function(Model){
    sigma2 = mean( Model$FittedValues *(Model$Omega[length(Model$Omega)]+ Model$FittedValues*Model$Theta[length(Model$Theta)]))
    phi2=mean(Model$Omega[length(Model$Omega)]+Model$FittedValues*Model$Theta[length(Model$Theta)])
    list(sigma2=sigma2,
    phi=c(phi2))
}
#############################################
library(ggplot2)

############ Type of Terrorism GGPLOT ###
Terrorism.Sample<-rep(Terrorism.Train,1000)
dat.T = data.frame(Y.T.QP=Y.T.QP,Y.T.NB=Y.T.NB,Y.T.NB2=Y.T.NB2,Types.of.Terrorism=Terrorism.Sample)

## Type of Terrorism QP
pT1<-ggplot(dat.T, aes(x=Y.T.QP, fill=Types.of.Terrorism)) +
    geom_histogram(alpha=0.65,color=1,binwidth = 10,position="identity")+
    scale_y_log10()+
    ylab("log(count)")+
    xlab("Amount of 'Wounded & Killed'")+
    xlim(c(-5,400))+
    labs(title="Simulations from Quasi-Poisson with",
         subtitle = bquote(omega ~"=" ~ 
                               .(round(Quasi.Poisson.T$Omega[length(Quasi.Poisson.T$Omega)],1)) ~ theta ~"=" ~ 
                               .(round(Quasi.Poisson.T$Theta[length(Quasi.Poisson.T$Theta)],1))))+
    coord_cartesian(ylim=c(2, 500000)) +
    theme(panel.background = element_rect(fill = 0, colour = 0),
          axis.title.x = element_text(family="Arial",size=13,
                                      face="bold",colour = "Black",vjust=-2,hjust=0.5),
          axis.title.y = element_text(family="Arial",size=13,
                                face="bold",colour = "Black",vjust=5,hjust=0.5),
          legend.title=element_text(family="Arial",size=13,
                                    face="bold",colour = "Black",vjust=-2,hjust=0.5),
          plot.title = element_text(hjust = 0.55,vjust=-11,size=16,family="Arial",face="bold"),
          plot.subtitle = element_text(hjust = 0.55,vjust=-11,size=16,family="Arial",face="bold"))+
    theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"),
          legend.spacing.y = unit(c(0.5,1,1,1,0.6,0.6,0.6), "cm"))+
    scale_fill_manual(name="Types of Terrorism",
                      values=c("green","red","white"))

## Type of Terrorism NB
pT2<-ggplot(dat.T, aes(x=Y.T.NB, fill=Types.of.Terrorism)) +
    geom_histogram(alpha=0.65,color=1,binwidth = 10,position="identity")+
    scale_y_log10()+
    ylab("log(count)")+
    xlab("Amount of 'Wounded & Killed'")+
    xlim(c(-5,400))+
    labs(title="Simulations from Negative Binomial with",
         subtitle = bquote(omega ~"=" ~ 
                               .(round(Neg.Bin.T$Omega[length(Neg.Bin.T$Omega)],1)) ~ theta ~"=" ~ 
                               .(round(Neg.Bin.T$Theta[length(Neg.Bin.T$Theta)],1))))+
    coord_cartesian(ylim=c(2, 500000)) +
    theme(panel.background = element_rect(fill = 0, colour = 0),
          axis.title.x = element_text(family="Arial",size=13,
                                      face="bold",colour = "Black",vjust=-2,hjust=0.5),
          axis.title.y = element_text(family="Arial",size=13,
                                      face="bold",colour = "Black",vjust=5,hjust=0.5),
          legend.title=element_text(family="Arial",size=13,
                                    face="bold",colour = "Black",vjust=-2,hjust=0.5),
          plot.title = element_text(hjust = 0.55,vjust=-11,size=16,family="Arial",face="bold"),
          plot.subtitle = element_text(hjust = 0.55,vjust=-11,size=16,family="Arial",face="bold"))+
    theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"),
          legend.spacing.y = unit(c(0.5,1,1,1,0.6,0.6,0.6), "cm"))+
    scale_fill_manual(name="Types of Terrorism",
                      values=c("green","red","white"))

## Type of Terrorism NB2
pT3<-ggplot(dat.T, aes(x=Y.T.NB2, fill=Types.of.Terrorism)) +
    geom_histogram(alpha=0.65,color=1,binwidth = 10,position="identity")+
    scale_y_log10()+
    ylab("log(count)")+
    xlab("Amount of 'Wounded & Killed'")+
    labs(title="Simulations from Negative Binomial with",
         subtitle = bquote(omega ~"=" ~ 
                               .(round(Neg.Bin2.T$Omega[length(Neg.Bin2.T$Omega)],1)) ~ theta ~"=" ~ 
                               .(round(Neg.Bin2.T$Theta[length(Neg.Bin2.T$Theta)],1))))+
    xlim(c(-5,400))+
    coord_cartesian(ylim=c(2, 500000)) +
    theme(panel.background = element_rect(fill = 0, colour = 0),
          axis.title.x = element_text(family="Arial",size=13,
                                      face="bold",colour = "Black",vjust=-2,hjust=0.5),
          axis.title.y = element_text(family="Arial",size=13,
                                      face="bold",colour = "Black",vjust=5,hjust=0.5),
          legend.title=element_text(family="Arial",size=13,
                                    face="bold",colour = "Black",vjust=-2,hjust=0.5),
          plot.title = element_text(hjust = 0.55,vjust=-11,size=16,family="Arial",face="bold"),
          plot.subtitle = element_text(hjust = 0.55,vjust=-11,size=16,family="Arial",face="bold"))+
    theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"),
          legend.spacing.y = unit(c(0.5,1,1,1,0.6,0.6,0.6), "cm"))+
    scale_fill_manual(name="Types of Terrorism",
                      values=c("green","red","white"))


############ Intercept only GGPLOT ###
dat.I = data.frame(Y.QP=Y.I.QP,Y.NB=Y.I.NB,Y.NB2=Y.I.NB2)

## Intercept only  QP
pI1<-ggplot(dat.I, aes(x=Y.QP)) +
    geom_histogram(alpha=0.65,color=1,binwidth = 20,position="identity")+
    scale_y_log10()+
    ylab("log(count)")+
    xlab("Amount of 'Wounded & Killed'")+
    xlim(c(-10,1000))+
    labs(title="Simulations from Quasi-Poisson with",
         subtitle = bquote(omega ~"=" ~ 
                               .(round(Quasi.Poisson.I$Omega[length(Quasi.Poisson.I$Omega)],1)) ~ theta ~"=" ~ 
                               .(round(Quasi.Poisson.I$Theta[length(Quasi.Poisson.I$Theta)],1))))+
    coord_cartesian(ylim=c(1.98, 1600000)) +
    theme(panel.background = element_rect(fill = 0, colour = 0),
          axis.title.x = element_text(family="Arial",size=13,
                                      face="bold",colour = "Black",vjust=-2,hjust=0.5),
          axis.title.y = element_text(family="Arial",size=13,
                                      face="bold",colour = "Black",vjust=5,hjust=0.5),
          legend.title=element_text(family="Arial",size=13,
                                    face="bold",colour = "Black",vjust=-2,hjust=0.5),
          plot.title = element_text(hjust = 0.55,vjust=-11,size=16,family="Arial",face="bold"),
          plot.subtitle = element_text(hjust = 0.55,vjust=-11,size=16,family="Arial",face="bold"))+
    theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"),
          legend.spacing.y = unit(c(0.5,1,1,1,0.6,0.6,0.6), "cm"))

## Intercept only  NB
pI2<-ggplot(dat.I, aes(x=Y.NB)) +
    geom_histogram(alpha=0.65,color=1,binwidth = 20,position="identity")+
    scale_y_log10()+
    ylab("log(count)")+
    xlab("Amount of 'Wounded & Killed'")+
    xlim(c(-10,1000))+
    labs(title="Simulations from Negative Binomial with",
         subtitle = bquote(omega ~"=" ~ 
                               .(round(Neg.Bin.I$Omega[length(Neg.Bin.I$Omega)],1)) ~ theta ~"=" ~ 
                               .(round(Neg.Bin.I$Theta[length(Neg.Bin.I$Theta)],1))))+
    coord_cartesian(ylim=c(1.98, 1600000)) +
    theme(panel.background = element_rect(fill = 0, colour = 0),
          axis.title.x = element_text(family="Arial",size=13,
                                      face="bold",colour = "Black",vjust=-2,hjust=0.5),
          axis.title.y = element_text(family="Arial",size=13,
                                      face="bold",colour = "Black",vjust=5,hjust=0.5),
          legend.title=element_text(family="Arial",size=13,
                                    face="bold",colour = "Black",vjust=-2,hjust=0.5),
          legend.position = "none",
          plot.title = element_text(hjust = 0.55,vjust=-11,size=16,family="Arial",face="bold"),
          plot.subtitle = element_text(hjust = 0.55,vjust=-11,size=16,family="Arial",face="bold"))+
    theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"),
          legend.spacing.y = unit(c(0.5,1,1,1,0.6,0.6,0.6), "cm"))
par(mfrow=c(3,1))
## Intercept only  NB2
pI3<-ggplot(dat.I, aes(x=Y.NB2)) +
    geom_histogram(alpha=0.65,color=1,binwidth = 20,position="identity")+
    scale_y_log10()+
    ylab("log(count)")+
    xlab("Amount of 'Wounded & Killed'")+
    labs(title="Simulations from Negative Binomial with",
         subtitle = bquote(omega ~"=" ~ 
                               .(round(Neg.Bin2.I$Omega[length(Neg.Bin2.I$Omega)],1)) ~ theta ~"=" ~ 
                               .(round(Neg.Bin2.I$Theta[length(Neg.Bin2.I$Theta)],1))))+
    xlim(c(0,1000))+
    coord_cartesian(ylim=c(1.82, 80000)) +
    theme(panel.background = element_rect(fill = 0, colour = 0),
          axis.title.x = element_text(family="Arial",size=13,
                                      face="bold",colour = "Black",vjust=-2,hjust=0.5),
          axis.title.y = element_text(family="Arial",size=13,
                                      face="bold",colour = "Black",vjust=5,hjust=0.5),
          legend.title=element_text(family="Arial",size=13,
                                    face="bold",colour = "Black",vjust=-2,hjust=0.5),
          legend.position = "none",
          plot.title = element_text(hjust = 0.55,vjust=-11,size=16,family="Arial",face="bold"),
          plot.subtitle = element_text(hjust = 0.55,vjust=-11,size=16,family="Arial",face="bold"))+
    theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"),
          legend.spacing.y = unit(c(0.5,1,1,1,0.6,0.6,0.6), "cm"))


############ Event type GGPLOT ###
EventType.Recoded.Sample<-rep(EventType.Recoded.Train,1000)
dat = data.frame(Y.QP=Y.E.QP,Y.NB=Y.E.NB,Y.NB2=Y.E.NB2,EventType.Recoded.Sample=EventType.Recoded.Sample)

## Event Type QP
pE1<-ggplot(dat, aes(x=Y.QP, fill=EventType.Recoded.Sample)) +
    geom_histogram(alpha=0.65,color=1,binwidth = 10,position="identity")+
    scale_y_log10()+
    ylab("log(count)")+
    xlab("Amount of 'Wounded & Killed'")+
    xlim(c(-5,500))+
    labs(title="Simulations from Quasi-Poisson with",
         subtitle = bquote(omega ~"=" ~ 
                               .(round(Quasi.Poisson.E$Omega[length(Quasi.Poisson.E$Omega)],1)) ~ theta ~"=" ~ 
                               .(round(Quasi.Poisson.E$Theta[length(Quasi.Poisson.E$Theta)],1))))+
    coord_cartesian(ylim=c(1.85, 40000)) +
    theme(panel.background = element_rect(fill = 0, colour = 0),
          axis.title.x = element_text(family="Arial",size=13,
                                      face="bold",colour = "Black",vjust=-2,hjust=0.5),
          axis.title.y = element_text(family="Arial",size=13,
                                      face="bold",colour = "Black",vjust=5,hjust=0.5),
          legend.title=element_text(family="Arial",size=13,
                                    face="bold",colour = "Black",vjust=-2,hjust=0.5),
          plot.title = element_text(hjust = 0.55,vjust=-11,size=16,family="Arial",face="bold"),
          plot.subtitle = element_text(hjust = 0.55,vjust=-11,size=16,family="Arial",face="bold"))+
    theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"),
          legend.spacing.y = unit(c(0.5,1,1,1,1,0.6,0.6), "cm"))+
    scale_fill_manual(name="Event Type",
                      labels=c("Bombing/Explosion","Facility/Infrastructure Attack", "NSAG Attack","Other"),
                      values=c("green","blue","white","red"))

## Event Type  NB
pE2<-ggplot(dat, aes(x=Y.NB, fill=EventType.Recoded.Sample)) +
    geom_histogram(alpha=0.65,color=1,binwidth = 10,position="identity")+
    scale_y_log10()+
    ylab("log(count)")+
    xlab("Amount of 'Wounded & Killed'")+
    xlim(c(-5,500))+
    labs(title="Simulations from Negative Binomial with",
         subtitle = bquote(omega ~"=" ~ 
                               .(round(Neg.Bin.E$Omega[length(Neg.Bin.E$Omega)],1)) ~ theta ~"=" ~ 
                               .(round(Neg.Bin.E$Theta[length(Neg.Bin.E$Theta)],1))))+
    coord_cartesian(ylim=c(1.85, 30000)) +
    theme(panel.background = element_rect(fill = 0, colour = 0),
          axis.title.x = element_text(family="Arial",size=13,
                                      face="bold",colour = "Black",vjust=-2,hjust=0.5),
          axis.title.y = element_text(family="Arial",size=13,
                                      face="bold",colour = "Black",vjust=5,hjust=0.5),
          legend.title=element_text(family="Arial",size=13,
                                    face="bold",colour = "Black",vjust=-2,hjust=0.5),
          legend.position = "none",
          plot.title = element_text(hjust = 0.55,vjust=-11,size=16,family="Arial",face="bold"),
          plot.subtitle = element_text(hjust = 0.55,vjust=-11,size=16,family="Arial",face="bold"))+
    theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"),
          legend.spacing.y = unit(c(0.5,1,1,1,1,0.6,0.6), "cm"))+
    scale_fill_manual(name="Event Type",
                      labels=c("Bombing/Explosion","Facility/Infrastructure Attack", "NSAG Attack","Other"),
                      values=c("green","blue","white","red"))


## Event Type NB2
pE3<-ggplot(dat, aes(x=Y.NB2, fill=EventType.Recoded.Sample)) +
    geom_histogram(alpha=0.65,color=1,binwidth = 10,position="identity")+
    scale_y_log10()+
    ylab("log(count)")+
    xlab("Amount of 'Wounded & Killed'")+
    xlim(c(-5,500))+
    labs(title="Simulations from Negative Binomial with",
         subtitle = bquote(omega ~"=" ~ 
                               .(round(Neg.Bin2.E$Omega[length(Neg.Bin2.E$Omega)],1)) ~ theta ~"=" ~ 
                               .(round(Neg.Bin2.E$Theta[length(Neg.Bin2.E$Theta)],1))))+
    coord_cartesian(ylim=c(1.85, 30000)) +
    theme(panel.background = element_rect(fill = 0, colour = 0),
          axis.title.x = element_text(family="Arial",size=13,
                                      face="bold",colour = "Black",vjust=-2,hjust=0.5),
          axis.title.y = element_text(family="Arial",size=13,
                                      face="bold",colour = "Black",vjust=5,hjust=0.5),
          legend.title=element_text(family="Arial",size=13,
                                    face="bold",colour = "Black",vjust=-2,hjust=0.5),
          legend.position = "none",
          plot.title = element_text(hjust = 0.55,vjust=-11,size=16,family="Arial",face="bold"),
          plot.subtitle = element_text(hjust = 0.55,vjust=-11,size=16,family="Arial",face="bold"))+
    theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"),
          legend.spacing.y = unit(c(0.5,1,1,1,1,0.6,0.6), "cm"))+
    scale_fill_manual(name="Event Type",
                      labels=c("Bombing/Explosion","Facility/Infrastructure Attack", "NSAG Attack","Other"),
                      values=c("green","blue","white","red"))


############ Weapons GGPLOT ###
Weapons.Recoded.Sample<-rep(Weapons.Recoded.Train,1000)
dat.W = data.frame(Y.QP=Y.W.QP,Y.NB=Y.W.NB,Y.NB2=Y.W.NB2,Weapons.Recoded.Sample=Weapons.Recoded.Sample)

## Weapons QP
pW1<-ggplot(dat.W, aes(x=Y.QP, fill=Weapons.Recoded.Sample)) +
    geom_histogram(alpha=0.65,color=1,binwidth = 15,position="identity")+
    scale_y_log10()+
    ylab("log(count)")+
    xlab("Amount of 'Wounded & Killed'")+
    xlim(c(-10,500))+
    labs(title="Simulations from Quasi-Poisson with",
         subtitle = bquote(omega ~"=" ~ 
                               .(round(Quasi.Poisson.W$Omega[length(Quasi.Poisson.W$Omega)],1)) ~ theta ~"=" ~ 
                               .(round(Quasi.Poisson.W$Theta[length(Quasi.Poisson.W$Theta)],1))))+
    coord_cartesian(ylim=c(1.85, 160000)) +
    theme(panel.background = element_rect(fill = 0, colour = 0),
          axis.title.x = element_text(family="Arial",size=13,
                                      face="bold",colour = "Black",vjust=-2,hjust=0.5),
          axis.title.y = element_text(family="Arial",size=13,
                                      face="bold",colour = "Black",vjust=5,hjust=0.5),
          legend.title=element_text(family="Arial",size=13,
                                    face="bold",colour = "Black",vjust=-2,hjust=0.5),
          plot.title = element_text(hjust = 0.55,vjust=-11,size=16,family="Arial",face="bold"),
          plot.subtitle = element_text(hjust = 0.55,vjust=-11,size=16,family="Arial",face="bold"))+
    theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"),legend.position="right",
          legend.spacing.y = unit(c(0.5,1,1,1,1,1,1), "cm"))+
    scale_fill_manual(name="Weapons",values=c("black","red","green","blue","orange","white"))

## Weapons  NB
pW2<-ggplot(dat.W, aes(x=Y.NB, fill=Weapons.Recoded.Sample)) +
    geom_histogram(alpha=0.65,color=1,binwidth = 15,position="identity")+
    scale_y_log10()+
    ylab("log(count)")+
    xlab("Amount of 'Wounded & Killed'")+
    xlim(c(-10,500))+
    labs(title="Simulations from Negative Binomial with",
         subtitle = bquote(omega ~"=" ~ 
                               .(round(Neg.Bin.W$Omega[length(Neg.Bin.W$Omega)],1)) ~ theta ~"=" ~ 
                               .(round(Neg.Bin.W$Theta[length(Neg.Bin.W$Theta)],1))))+
    coord_cartesian(ylim=c(1.85, 160000)) +
    theme(panel.background = element_rect(fill = 0, colour = 0),
          axis.title.x = element_text(family="Arial",size=13,
                                      face="bold",colour = "Black",vjust=-2,hjust=0.5),
          axis.title.y = element_text(family="Arial",size=13,
                                      face="bold",colour = "Black",vjust=5,hjust=0.5),
          legend.title=element_text(family="Arial",size=13,
                                    face="bold",colour = "Black",vjust=-2,hjust=0.5),
          plot.title = element_text(hjust = 0.55,vjust=-11,size=16,family="Arial",face="bold"),
          plot.subtitle = element_text(hjust = 0.55,vjust=-11,size=16,family="Arial",face="bold"))+
    theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"),legend.position="none",
          legend.spacing.y = unit(c(0.5,1,1,1,1,1,1), "cm"))+
    scale_fill_manual(name="Weapons",values=c("black","red","green","blue","orange","white"))

## Weapons NB2
pW3<-ggplot(dat.W, aes(x=Y.NB2, fill=Weapons.Recoded.Sample)) +
    geom_histogram(alpha=0.65,color=1,binwidth = 15,position="identity")+
    scale_y_log10()+
    ylab("log(count)")+
    xlab("Amount of 'Wounded & Killed'")+
    xlim(c(-10,500))+
    labs(title="Simulations from Negative Binomial with",
         subtitle = bquote(omega ~"=" ~ 
                               .(round(Neg.Bin2.W$Omega[length(Neg.Bin2.W$Omega)],1)) ~ theta ~"=" ~ 
                               .(round(Neg.Bin2.W$Theta[length(Neg.Bin2.W$Theta)],1))))+
    coord_cartesian(ylim=c(1.85, 160000)) +
    theme(panel.background = element_rect(fill = 0, colour = 0),
          axis.title.x = element_text(family="Arial",size=13,
                                      face="bold",colour = "Black",vjust=-2,hjust=0.5),
          axis.title.y = element_text(family="Arial",size=13,
                                      face="bold",colour = "Black",vjust=5,hjust=0.5),
          legend.title=element_text(family="Arial",size=13,
                                    face="bold",colour = "Black",vjust=-2,hjust=0.5),
          plot.title = element_text(hjust = 0.55,vjust=-11,size=16,family="Arial",face="bold"),
          plot.subtitle = element_text(hjust = 0.55,vjust=-11,size=16,family="Arial",face="bold"))+
    theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"),legend.position="none",
          legend.spacing.y = unit(c(0.5,1,1,1,1,1,1), "cm"))+
    scale_fill_manual(name="Weapons",values=c("black","red","green","blue","orange","white"))

##### Print GGplots ######
require(ggpubr)
par(mfrow=c(1,1))

ggarrange(pT1, pT2, pT3, ncol=3, nrow=1, common.legend = TRUE, legend="bottom")
ggarrange(pI1, pI2, pI3, ncol=3, nrow=1, common.legend = TRUE, legend="bottom")
ggarrange(pE1, pE2, pE3, ncol=3, nrow=1, common.legend = TRUE, legend="bottom")
ggarrange(pW1, pW2, pW3, ncol=3, nrow=1, common.legend = TRUE, legend="bottom")

