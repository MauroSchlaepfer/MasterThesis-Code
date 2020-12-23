
### Hurdle Model
####### (re)Code
Events_Relevant_QGIS$AtThreat <- Events_Relevant_QGIS$AttackTotalWounded+Events_Relevant_QGIS$AttackTotalKilled
Events_Relevant_QGIS.Train<-Events_Relevant_QGIS[Train,]
Events_Relevant_QGIS.Train$AtThreat.Bin <- Events_Relevant_QGIS.Train$AtThreat
Events_Relevant_QGIS.Train$AtThreat.Bin[which(Events_Relevant_QGIS.Train$AtThreat.Bin>0)] <- 1


NonHurdle<-which(Events_Relevant_QGIS.Train$AtThreat>0)
Terrorism<-(as.factor(Events_Relevant_QGIS$TypeOfTerrorism))
Terrorism.Train<-(as.factor(Events_Relevant_QGIS.Train$TypeOfTerrorism))
Weapons.Train<-Weapons[Train]
####  Reduction of categoreis - Otherwise model is overspecifed - singular
Weapons.Recoded <- Weapons

Weapons.Recoded[which("Small caliber"== Weapons.Recoded)]<-"Firearm & Vehicle impact"
Weapons.Recoded[which("Firearm"== Weapons.Recoded)]<-"Firearm & Vehicle impact"
Weapons.Recoded[which("Virus/Malware"== Weapons.Recoded)]<-"Infrastructure Attack"
Weapons.Recoded[which("Arson"== Weapons.Recoded)]<-"Infrastructure Attack"
Weapons.Recoded[which("Rocks"== Weapons.Recoded)]<-"Infrastructure Attack"
Weapons.Recoded[which("Knife & Axe"== Weapons.Recoded)]<-"Other"
Weapons.Recoded[which("Unknown"== Weapons.Recoded)]<-"Other"
Weapons.Recoded[which("Vehicle impact"== Weapons.Recoded)]<-"Firearm & Vehicle impact"

EventType_merged # coded small categories to 'other'
EventType.Recoded <- EventType
EventType.Recoded[which("Armed Assault"== EventType.Recoded)]<-"Other"
EventType.Recoded[which("Unarmed Assault"== EventType.Recoded)]<-"Other"
EventType.Recoded[which("Unknown"== EventType.Recoded)]<-"Other"
EventType.Recoded[which("Assassination"== EventType.Recoded)]<-"Other"

EventType.Recoded.Train<-EventType.Recoded[Train]
Weapons.Recoded.Train<-Weapons.Recoded[Train]
##################################################################################################

# Start Hurdle Models
Hurdle.TE<-glm(Events_Relevant_QGIS.Train$AtThreat.Bin~Terrorism.Train+EventType.Recoded.Train,family = binomial(link="logit"))

Hurdle.W<-glm(Events_Relevant_QGIS.Train$AtThreat.Bin~Weapons.Recoded.Train,family = binomial(link="logit"))

Hurdle<-glm(Events_Relevant_QGIS.Train$AtThreat.Bin~Terrorism.Train,family = binomial(link="logit"))
Hurdle.E<-glm(Events_Relevant_QGIS.Train$AtThreat.Bin~EventType.Recoded.Train,family = binomial(link="logit"))
Hurdle.I<-glm(Events_Relevant_QGIS.Train$AtThreat.Bin~1,family = binomial(link="logit"))


sum.hurdle<-summary(Hurdle)
sum.hurdle.W<-summary(Hurdle.W)
sum.hurdle.I<-summary(Hurdle.I)
sum.hurdle.E<-summary(Hurdle.E)
c(sum.hurdle.I$aic,sum.hurdle$aic,sum.hurdle.E$aic,sum.hurdle.W$aic)

logit2prob2 <- function(logit){
    odds_i <- exp(logit[1])
    odds_p <- exp(logit[1]) * exp(logit[-1])
    c(odds_i / (1 + odds_i),odds_p / (1 + odds_p))
}


HurdleProb.W<-round(cbind(logit2prob2(sum.hurdle.W$coefficients[,1]),
                          logit2prob2(sum.hurdle.W$coefficients[,1]+qnorm(0.25)*sum.hurdle.W$coefficients[,2]),
                          logit2prob2(sum.hurdle.W$coefficients[,1]+qnorm(0.975)*sum.hurdle.W$coefficients[,2])),3)
colnames(HurdleProb.W) <- c("P[Y>0]","Lower","Upper")
rownames(HurdleProb.W) <- names(table(Weapons.Recoded.Train))
HurdleProb.W
HurdleProb<-round(cbind(logit2prob2(sum.hurdle$coefficients[,1]),
                        logit2prob2(sum.hurdle$coefficients[,1]+qnorm(0.25)*sum.hurdle$coefficients[,2]),
                        logit2prob2(sum.hurdle$coefficients[,1]+qnorm(0.975)*sum.hurdle$coefficients[,2])),3)
colnames(HurdleProb) <- c("P[Y>0]","Lower","Upper")
rownames(HurdleProb) <- c("Islamic Terror",names(table(Terrorism.Train))[-1])
HurdleProb.I<-round(cbind(logit2prob2(sum.hurdle.I$coefficients[,1]),
                        logit2prob2(sum.hurdle.I$coefficients[,1]+qnorm(0.25)*sum.hurdle.I$coefficients[,2]),
                        logit2prob2(sum.hurdle.I$coefficients[,1]+qnorm(0.975)*sum.hurdle.I$coefficients[,2])),3)
colnames(HurdleProb.I) <- c("P[Y>0]","Lower","Upper")
rownames(HurdleProb.I) <- "Total"

HurdleProb.E<-round(cbind(logit2prob2(sum.hurdle.E$coefficients[,1]),
                          logit2prob2(sum.hurdle.E$coefficients[,1]+qnorm(0.25)*sum.hurdle.E$coefficients[,2]),
                          logit2prob2(sum.hurdle.E$coefficients[,1]+qnorm(0.975)*sum.hurdle.E$coefficients[,2])),3)
colnames(HurdleProb.E) <- c("P[Y>0]","Lower","Upper")
rownames(HurdleProb.E) <- c(names(table(EventType.Recoded.Train)))

HurdleProb.W<-round(cbind(logit2prob2(sum.hurdle.W$coefficients[,1]),
                          logit2prob2(sum.hurdle.W$coefficients[,1]+qnorm(0.25)*sum.hurdle.W$coefficients[,2]),
                          logit2prob2(sum.hurdle.W$coefficients[,1]+qnorm(0.975)*sum.hurdle.W$coefficients[,2])),3)
colnames(HurdleProb.W) <- c("P[Y>0]","Lower","Upper")
rownames(HurdleProb.W) <- c(names(table(Weapons.Recoded.Train)))

par(mfrow=c(1,1))
require(plotrix)
par(las=2)
par(mar=c(13,3,1,1))
plotCI(1:14,y=c(HurdleProb.I[,1],HurdleProb[,1],HurdleProb.E[,1],HurdleProb.W[,1]), li = c(HurdleProb.I[,2],HurdleProb[,2],HurdleProb.E[,2],HurdleProb.W[,2]),
       ui = c(HurdleProb.I[,3],HurdleProb[,3],HurdleProb.E[,3],HurdleProb.W[,3]), ylim=c(0,1),scol=1,col=1,pt.bg=2,
       pch=21,xaxt="n",xlab="",ylab="")
axis(1, at = 1:14,las=2, cex.axis=0.65,
     labels = c("P[ Y > 0 ]","P[ Y > 0 | Type of Terrorism = Islamic Terror]","P[ Y > 0 | Type of Terrorism = Left Terror]","P[ Y > 0 | Type of Terrorism = Right Terror]",
                "P[ Y > 0 | Event type = Bombing/Explosion]","P[ Y > 0 | Event type = Infrastructure Attack]","P[ Y > 0 | Event type = NSAG Attack]",
                "P[ Y > 0 | Event type = Other]","P[ Y > 0 | Weapons = Edged & Improvised]","P[ Y > 0 | Weapons = Explosives]", "P[ Y > 0 | Weapons = Firearm & Vehicle impact]",
                "P[ Y > 0 | Weapons = Incendiary device]","P[ Y > 0 | Weapons = Infrastructure Attack]", "P[ Y > 0 | Weapons = Other]"))

abline(h=HurdleProb.I[,1],lty=2,col=2)
abline(v=c(1.5,4.5,8.5),lty=2,col=1)
text(c(1,3,6.5,11.5),y=rep(1,4), labels=c("Y ~ 1","Y ~ Types of Terrorsim","Y ~ Event type","Y ~ Weapons"),cex=0.8)
###
