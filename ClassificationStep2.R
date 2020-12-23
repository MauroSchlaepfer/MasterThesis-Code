Y.Test<-Events_Relevant_QGIS$AtThreat[Test]
HistogramWeapons<-function(Y,Weapons,down=1.82,up=200000){
    dat.observed.W = data.frame(Y=rep(Y,2),Weapons=rep(Weapons,2))
    ggplot(dat.observed.W, aes(x=Y,fill=Weapons)) +
        geom_histogram(alpha=0.65,color=1,binwidth = 10,position="identity")+
        ylab("log(count)")+
        scale_y_log10()+
        xlab("Amount of 'Wounded & Killed'")+
        xlim(c(-5,500))+
        labs(title="Simulations from Negative Binomial with",
             subtitle = bquote(omega ~"=" ~ 
                                   .(round(Neg.Bin.W$Omega[length(Neg.Bin.W$Omega)],1)) ~ theta ~"=" ~ 
                                   .(round(Neg.Bin.W$Theta[length(Neg.Bin.W$Theta)],1))))+
        coord_cartesian(ylim=c(down, up))+
        theme(panel.background = element_rect(fill = 0, colour = 0),
              axis.title.x = element_text(family="Arial",size=13,
                                          face="bold",colour = "Black",vjust=-2,hjust=0.5),
              axis.title.y = element_text(family="Arial",size=13,
                                          face="bold",colour = "Black",vjust=5,hjust=0.5),
              legend.title=element_blank(),
              plot.title = element_text(hjust = 0.55,vjust=-11,size=16,family="Arial",face="bold"),
              plot.subtitle = element_text(hjust = 0.55,vjust=-11,size=16,family="Arial",face="bold"))+
        theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"),
              legend.spacing.y = unit(c(0.5,1,1,1,1,1,1), "cm"),legend.position="bottom")+
        scale_fill_manual(values=c("black","red","green","blue","orange","white"))
}

####
Weapons.Recoded.Test<-Weapons.Recoded[Test]
Terrorism.Test<-Terrorism[Test]
EventType.Recoded.Test<-EventType.Recoded[Test]
set.seed(123)
X.I.Test <- model.matrix(Y.Test~1)
X.T.Test <- model.matrix(Y.Test~Terrorism.Test)
X.E.Test <- model.matrix(Y.Test~EventType.Recoded.Test)
X.W.Test <- model.matrix(Y.Test~Weapons.Recoded.Test)

YTest.I.QP <- Sample.Hurdle.Count(HurdleCoef = Hurdle.I$coefficients,Beta = Quasi.Poisson.I$Beta.head, Omega=Quasi.Poisson.I$Omega[length(Quasi.Poisson.I$Omega)],X=X.I.Test)
YTest.I.NB <- Sample.Hurdle.Count(HurdleCoef = Hurdle.I$coefficients,Beta = Neg.Bin.I$Beta.head,Theta = 1/Neg.Bin.I$Theta[length(Neg.Bin.I$Theta)],X=X.I.Test)
YTest.I.NB2 <- Sample.Hurdle.Count(HurdleCoef = Hurdle.I$coefficients,Beta = Neg.Bin2.I$Beta.head, Omega=Neg.Bin2.E$Omega[length(Neg.Bin2.I$Omega)],Theta = 1/Neg.Bin2.I$Theta[length(Neg.Bin2.I$Theta)],X=X.I.Test)

YTest.T.QP <- Sample.Hurdle.Count(HurdleCoef = Hurdle$coefficients,Beta = Quasi.Poisson.T$Beta.head, Omega=Quasi.Poisson.T$Omega[length(Quasi.Poisson.T$Omega)],X=X.T.Test)
YTest.T.NB <- Sample.Hurdle.Count(HurdleCoef = Hurdle$coefficients,Beta = Neg.Bin.T$Beta.head,Theta = 1/Neg.Bin.T$Theta[length(Neg.Bin.T$Theta)],X=X.T.Test)
YTest.T.NB2 <- Sample.Hurdle.Count(HurdleCoef = Hurdle$coefficients,Beta = Neg.Bin2.T$Beta.head, Omega=Neg.Bin2.T$Omega[length(Neg.Bin2.T$Omega)],Theta = 1/Neg.Bin2.T$Theta[length(Neg.Bin2.T$Theta)],X=X.T.Test)

YTest.E.QP <- Sample.Hurdle.Count(HurdleCoef = Hurdle.E$coefficients,Beta = Quasi.Poisson.E$Beta.head, Omega=Quasi.Poisson.E$Omega[length(Quasi.Poisson.E$Omega)],X=X.E.Test)
YTest.E.NB <- Sample.Hurdle.Count(HurdleCoef = Hurdle.E$coefficients,Beta = Neg.Bin.E$Beta.head,Theta = 1/Neg.Bin.E$Theta[length(Neg.Bin.E$Theta)],X=X.E.Test)
YTest.E.NB2 <- Sample.Hurdle.Count(HurdleCoef = Hurdle.E$coefficients,Beta = Neg.Bin2.E$Beta.head, Omega=Neg.Bin2.E$Omega[length(Neg.Bin2.E$Omega)],Theta = 1/Neg.Bin2.E$Theta[length(Neg.Bin2.E$Theta)],X=X.E.Test)

YTest.W.QP <- Sample.Hurdle.Count(HurdleCoef = Hurdle.W$coefficients,Beta = Quasi.Poisson.W$Beta.head, Omega=Quasi.Poisson.W$Omega[length(Quasi.Poisson.W$Omega)],X=X.W.Test)
YTest.W.NB <- Sample.Hurdle.Count(HurdleCoef = Hurdle.W$coefficients,Beta = Neg.Bin.W$Beta.head,Theta = 1/Neg.Bin.W$Theta[length(Neg.Bin.W$Theta)],X=X.W.Test)
YTest.W.NB2 <- Sample.Hurdle.Count(HurdleCoef = Hurdle.W$coefficients,Beta = Neg.Bin2.W$Beta.head, Omega=Neg.Bin2.E$Omega[length(Neg.Bin2.E$Omega)],Theta = 1/Neg.Bin2.W$Theta[length(Neg.Bin2.W$Theta)],X=X.W.Test)

HistogramWeapons(YTest.W.NB2,Weapons.Recoded.Test)
HistogramWeapons(YTest.W.NB,Weapons.Recoded.Test)
HistogramWeapons(YTest.W.QP,Weapons.Recoded.Test)

require(coin)
require(afex)
require(spgs)
library(stats)

#(apply(matrix(YTest.W.NB2[1:(length(X.I.Test))],ncol=length(X.I.Test)), 1,compare.2.vectors,paired=T,y=Y.Test))

### "Wilcoxon signed rank test with continuity correction" aka 'paired Mann-Whitney' Test 
# https://en.wikipedia.org/wiki/Mann%E2%80%93Whitney_U_test
# Only ordinal test - influence of covariates
set.seed(123)
MannWhitneyTest.I.QP <- apply(matrix(YTest.I.QP,ncol=length(X.I.Test)), 1,function(x) wilcox.test(x,Y.Test,paired=T)$p.value)
MannWhitneyTest.I.NB <- apply(matrix(YTest.I.NB,ncol=length(X.I.Test)), 1,function(x) wilcox.test(x,Y.Test,paired=T)$p.value)
MannWhitneyTest.I.NB2 <- apply(matrix(YTest.I.NB2,ncol=length(X.I.Test)), 1,function(x) wilcox.test(x,Y.Test,paired=T)$p.value)

MannWhitneyTest.T.QP <- apply(matrix(YTest.T.QP,ncol=length(X.I.Test)), 1,function(x) wilcox.test(x,Y.Test,paired=T)$p.value)
MannWhitneyTest.T.NB <- apply(matrix(YTest.T.NB,ncol=length(X.I.Test)), 1,function(x) wilcox.test(x,Y.Test,paired=T)$p.value)
MannWhitneyTest.T.NB2 <- apply(matrix(YTest.T.NB2,ncol=length(X.I.Test)), 1,function(x) wilcox.test(x,Y.Test,paired=T)$p.value)

MannWhitneyTest.E.QP <- apply(matrix(YTest.E.QP,ncol=length(X.I.Test)), 1,function(x) wilcox.test(x,Y.Test,paired=T)$p.value)
MannWhitneyTest.E.NB <- apply(matrix(YTest.E.NB,ncol=length(X.I.Test)), 1,function(x) wilcox.test(x,Y.Test,paired=T)$p.value)
MannWhitneyTest.E.NB2 <- apply(matrix(YTest.E.NB2,ncol=length(X.I.Test)), 1,function(x) wilcox.test(x,Y.Test,paired=T)$p.value)

MannWhitneyTest.W.QP <- apply(matrix(YTest.W.QP,ncol=length(X.I.Test)), 1,function(x) wilcox.test(x,Y.Test,paired=T)$p.value)
MannWhitneyTest.W.NB <- apply(matrix(YTest.W.NB,ncol=length(X.I.Test)), 1,function(x) wilcox.test(x,Y.Test,paired=T)$p.value)
MannWhitneyTest.W.NB2 <- apply(matrix(YTest.W.NB2,ncol=length(X.I.Test)), 1,function(x) wilcox.test(x,Y.Test,paired=T)$p.value)
logit2prob <- function(logit){
    odds <- exp(logit)
    prob <- odds / (1 + odds)
    return(prob)
}
CI.p <- function(p) c(logit2prob(mean(log(p))+qnorm(0.025)*sd(log(p))),logit2prob(mean(log(p))+qnorm(0.975)*sd(log(p))))

## CI of pvalues
round(rbind(CI.p(MannWhitneyTest.I.QP),CI.p(MannWhitneyTest.I.NB),CI.p(MannWhitneyTest.I.NB2)),3)
round(rbind(CI.p(MannWhitneyTest.T.QP),CI.p(MannWhitneyTest.T.NB),CI.p(MannWhitneyTest.T.NB2)),3)
round(rbind(CI.p(MannWhitneyTest.E.QP),CI.p(MannWhitneyTest.E.NB),CI.p(MannWhitneyTest.E.NB2)),3)
round(rbind(CI.p(MannWhitneyTest.W.QP),CI.p(MannWhitneyTest.W.NB),CI.p(MannWhitneyTest.W.NB2)),3)

## Uniformity Test for pvalues
c(chisq.unif.test(MannWhitneyTest.I.QP)$p.value,chisq.unif.test(MannWhitneyTest.I.NB)$p.value,chisq.unif.test(MannWhitneyTest.I.NB2)$p.value)
c(chisq.unif.test(MannWhitneyTest.T.QP)$p.value,chisq.unif.test(MannWhitneyTest.T.NB)$p.value,chisq.unif.test(MannWhitneyTest.T.NB2)$p.value)
c(chisq.unif.test(MannWhitneyTest.E.QP)$p.value,chisq.unif.test(MannWhitneyTest.E.NB)$p.value,chisq.unif.test(MannWhitneyTest.E.NB2)$p.value)
c(chisq.unif.test(MannWhitneyTest.W.QP)$p.value,chisq.unif.test(MannWhitneyTest.W.NB)$p.value,chisq.unif.test(MannWhitneyTest.W.NB2)$p.value)

## Histograms
HistogramPvalues<-function(pvalue,model,dist,cov,max=0.6,color="lightgreen"){
    p.v = data.frame(p=pvalue)
    ggplot(p.v, aes(x=p,fill="green") )+
        geom_histogram(aes(y=..count../sum(..count..)),alpha=0.65,color=1,binwidth = 0.10,position="identity")+
        xlab(bquote("P[ "~Y^{SIM} ~ "&"  ~ Y^{Test}~"~ same underlying distribution ]"))+
        ylim(c(0,max))+
        xlim(c(-0.05,1.05))+
        ylab("")+
        labs(title=paste("P-values of ", dist," for '",cov,"' model",sep=""),
             subtitle = bquote(omega ~"=" ~ 
                                   .(round(model$Omega[length(model$Omega)],1)) ~ theta ~"=" ~ 
                                   .(round(model$Theta[length(model$Theta)],1))))+
        theme(panel.background = element_rect(fill = 0, colour = 0),
              axis.title.x = element_text(family="Arial",size=13,
                                          face="bold",colour = "Black",vjust=-2,hjust=0.5),
              axis.title.y = element_text(family="Arial",size=13,
                                          face="bold",colour = "Black",vjust=5,hjust=0.5),
              legend.title=element_blank(),
              plot.title = element_text(hjust = 0.55,vjust=-11,size=16,family="Arial",face="bold"),
              plot.subtitle = element_text(hjust = 0.55,vjust=-11,size=16,family="Arial",face="bold"))+
        theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"),
              legend.spacing.y = unit(c(0.5,1,1,1,1,1,1), "cm"),legend.position="none")+
        scale_fill_manual(values=c(color))
    } # paired Wilcoxon signed-rank test

HpI1<-HistogramPvalues(MannWhitneyTest.I.QP,Quasi.Poisson.I,"Quasi-Poisson","Y ~ 1")
HpI2<-HistogramPvalues(MannWhitneyTest.I.NB,Neg.Bin.I,"Negative Binomial","Y ~ 1",color = "lightblue")
HpI3<-HistogramPvalues(MannWhitneyTest.I.NB2,Neg.Bin2.I,"Negative Binomial","Y ~ 1",color = "lightyellow")
HpT1<-HistogramPvalues(MannWhitneyTest.T.QP,Quasi.Poisson.T,"Quasi-Poisson","Y ~ Type of Terrorism")
HpT2<-HistogramPvalues(MannWhitneyTest.T.NB,Neg.Bin.T,"Negative Binomial","Y ~ Type of Terrorism",color = "lightblue")
HpT3<-HistogramPvalues(MannWhitneyTest.T.NB2,Neg.Bin2.T,"Negative Binomial","Y ~ Type of Terrorism",color = "lightyellow")
HpE1<-HistogramPvalues(MannWhitneyTest.E.QP,Quasi.Poisson.E,"Quasi-Poisson","Y ~ Event type")
HpE2<-HistogramPvalues(MannWhitneyTest.E.NB,Neg.Bin.E,"Negative Binomial","Y ~ Event type",color = "lightblue")
HpE3<-HistogramPvalues(MannWhitneyTest.E.NB2,Neg.Bin2.E,"Negative Binomial","Y ~ Event type",color = "lightyellow")
HpW1<-HistogramPvalues(MannWhitneyTest.W.QP,Quasi.Poisson.W,"Quasi-Poisson","Y ~ Weapons")
HpW2<-HistogramPvalues(MannWhitneyTest.W.NB,Neg.Bin.W,"Negative Binomial","Y ~ Weapons",color = "lightblue")
HpW3<-HistogramPvalues(MannWhitneyTest.W.NB2,Neg.Bin2.W,"Negative Binomial","Y ~ Weapons",color = "lightyellow")
require(ggpubr)
ggarrange(HpI1, HpI2, HpI3, ncol=1, nrow=3)
ggarrange(HpT1, HpT2, HpT3, ncol=1, nrow=3)
ggarrange(HpE1, HpE2, HpE3, ncol=1, nrow=3)
ggarrange(HpW1, HpW2, HpW3, ncol=1, nrow=3)

#### Permutation Test (not used)
require(exactRankTests) # looks at integer values
PermTest.I.QP <- apply(matrix(YTest.I.QP,ncol=length(X.I.Test)), 1,function(x) perm.test(x,Y.Test,paired=T)$p.value)
PermTest.I.NB <- apply(matrix(YTest.I.NB,ncol=length(X.I.Test)), 1,function(x) perm.test(x,Y.Test,paired=T)$p.value)
PermTest.I.NB2 <- apply(matrix(YTest.I.NB2,ncol=length(X.I.Test)), 1,function(x) perm.test(x,Y.Test,paired=T)$p.value)

PermTest.T.QP <- apply(matrix(YTest.T.QP,ncol=length(X.I.Test)), 1,function(x) perm.test(x,Y.Test,paired=T)$p.value)
PermTest.T.NB <- apply(matrix(YTest.T.NB,ncol=length(X.I.Test)), 1,function(x) perm.test(x,Y.Test,paired=T)$p.value)
PermTest.T.NB2 <- apply(matrix(YTest.T.NB2,ncol=length(X.I.Test)), 1,function(x) perm.test(x,Y.Test,paired=T)$p.value)

PermTest.E.QP <- apply(matrix(YTest.E.QP,ncol=length(X.I.Test)), 1,function(x) perm.test(x,Y.Test,paired=T)$p.value)
PermTest.E.NB <- apply(matrix(YTest.E.NB,ncol=length(X.I.Test)), 1,function(x) perm.test(x,Y.Test,paired=T)$p.value)
PermTest.E.NB2 <- apply(matrix(YTest.E.NB2,ncol=length(X.I.Test)), 1,function(x) perm.test(x,Y.Test,paired=T)$p.value)

PermTest.W.QP <- apply(matrix(YTest.W.QP,ncol=length(X.I.Test)), 1,function(x) perm.test(x,Y.Test,paired=T)$p.value)
PermTest.W.NB <- apply(matrix(YTest.W.NB,ncol=length(X.I.Test)), 1,function(x) perm.test(x,Y.Test,paired=T)$p.value)
PermTest.W.NB2 <- apply(matrix(YTest.W.NB2,ncol=length(X.I.Test)), 1,function(x) perm.test(x,Y.Test,paired=T)$p.value)

##############################################################################
## Quantile Test
NonZero<-which(Y.Test>0)
Y.Test.NonZero <- Y.Test[NonZero]

MMM<-function(YTest){
    YTest<-apply(matrix(YTest,ncol=length(Y.Test)),2,function(x) x[x>0])
    Quantiles<-lapply(YTest,function(x) quantile(x))[NonZero]
    Quantiles<-matrix(unlist(Quantiles),byrow=T,ncol=5)

list(Quantiles=Quantiles,
     lossf=sum(  apply( Quantiles,2,function(x) abs(x-Y.Test.NonZero) )  )
     )
}

rbind(cbind(MMM(YTest.I.QP)$lossf,MMM(YTest.I.NB)$lossf,MMM(YTest.I.NB2)$lossf),
cbind(MMM(YTest.T.QP)$lossf,MMM(YTest.T.NB)$lossf,MMM(YTest.T.NB2)$lossf),
cbind(MMM(YTest.E.QP)$lossf,MMM(YTest.E.NB)$lossf,MMM(YTest.E.NB2)$lossf),
cbind(MMM(YTest.W.QP)$lossf,MMM(YTest.W.NB)$lossf,MMM(YTest.W.NB2)$lossf))



mean(YTest.E.QP)
mean(YTest.E.NB)
mean(YTest.E.NB2)
mean(Y.Test)



