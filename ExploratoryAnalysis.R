########### Exploratory Statistics
Events_Relevant_QGIS$AtThreat<-Events_Relevant_QGIS$AttackTotalKilled+Events_Relevant_QGIS$AttackTotalWounded


### Threat Variable - Important
IT<-subset(Events_Relevant_QGIS,TypeOfTerrorism=="Islamistic")$AtThreat
RT<-subset(Events_Relevant_QGIS,TypeOfTerrorism=="Right Terror")$AtThreat
LT<-subset(Events_Relevant_QGIS,TypeOfTerrorism=="Left Terror")$AtThreat
SummaryTable<-matrix(c(round(mean(IT),2),round(var(IT),2), paste(range(IT)[1],":",range(IT)[2],sep=""), round(var(IT)/mean(IT),1),length(IT),
                     round(mean(RT),2),round(var(RT),2), paste(range(RT)[1],":",range(RT)[2],sep=""), round(var(RT)/mean(RT),1),length(RT),
                     round(mean(LT),2),round(var(LT),2), paste(range(LT)[1],":",range(LT)[2],sep=""), round(var(LT)/mean(LT),1),length(LT)),
                     byrow = T,ncol=5,nrow=3)
colnames(SummaryTable)<-c("Mean","Var","Range","Mean/Var","n")
rownames(SummaryTable) <- c("IT","RT","LT")

### Suspects - Semi Important
Suspects<-Events_Relevant_QGIS
Suspects$SuspectsKilled[is.na(Suspects$SuspectsKilled)]<-0
IT_S<-subset(Suspects,TypeOfTerrorism=="Islamistic")$SuspectsKilled
RT_S<-subset(Suspects,TypeOfTerrorism=="Right Terror")$SuspectsKilled
LT_S<-subset(Suspects,TypeOfTerrorism=="Left Terror")$SuspectsKilled
SummaryTable_S<-matrix(c(round(mean(IT_S),2),round(var(IT_S),2), paste(range(IT_S)[1],":",range(IT_S)[2],sep=""), round(var(IT_S)/mean(IT_S),1),length(IT_S),
                       round(mean(RT_S),2),round(var(RT_S),2), paste(range(RT_S)[1],":",range(RT_S)[2],sep=""), round(var(RT_S)/mean(RT_S),1),length(RT_S),
                       round(mean(LT_S),2),round(var(LT_S),2), paste(range(LT_S)[1],":",range(LT_S)[2],sep=""), round(var(LT_S)/mean(LT_S),1),length(LT_S)),
                     byrow = T,ncol=5,nrow=3)
colnames(SummaryTable_S)<-c("Mean","Var","Range","Mean/Var","n")
rownames(SummaryTable_S) <- c("IT","RT","LT")

### Threat Variable per Country
table(Events_Relevant_QGIS$ISOName)

ISO_Dataframe<-unlist(tapply(Events_Relevant_QGIS$AtThreat, Events_Relevant_QGIS$ISOName, function(x) 
    c(round(mean(x),2),round(var(x),2),paste(range(x)[1],":",range(x)[2],sep=""),
      round(var(x)/mean(x),1),length(x)) ))
ISO_Mat<-matrix(ISO_Dataframe,ncol = 5,byrow = T)
colnames(ISO_Mat)<-c("Mean","Var","Range","Mean/Var","n")
rownames(ISO_Mat) <- names(table(Events_Relevant_QGIS$ISOName))

### Threat Variable per Weapon type
Weapons<-Events_Relevant_QGIS$Weapons
Weapons[grep("Vehicle", Weapons)]<-"Vehicle impact"
Weapons[grep("Explosive", Weapons)]<-"Explosives"
Weapons[grep("explosiv", Weapons)]<-"Explosives"
Weapons[grep("bomb", Weapons)]<-"Explosives"
Weapons[grep("timer", Weapons)]<-"Explosives"
Weapons[grep("Grenade", Weapons)]<-"Explosives"
Weapons[grep("Bomb", Weapons)]<-"Explosives"
Weapons[grep("Fire Infantry", Weapons)]<-"firearm"
Weapons[grep("Firearm", Weapons)]<-"firearm"
Weapons[grep("Fire", Weapons)]<-"Arson"
Weapons[grep("firearm", Weapons)]<-"Firearm"
Weapons[grep("Knife", Weapons)]<-"Knife & Axe"
Weapons[grep("Knives", Weapons)]<-"Knife & Axe"
Weapons[grep("stabbed", Weapons)]<-"Knife & Axe"
Weapons[grep("Axe", Weapons)]<-"Knife & Axe"
Weapons[grep("Incendiary", Weapons)]<-"Incendiary device"
Weapons[grep("Improvised", Weapons)]<-"Edged & Improvised"
Weapons[grep("Malware", Weapons)]<-"Virus/Malware"
Weapons[grep("flames", Weapons)]<-"Arson"
Weapons[grep("on fire", Weapons)]<-"Arson"
Weapons[grep("Arson", Weapons)]<-"Arson"
Weapons[grep("incendiary", Weapons)]<-"Incendiary device"
Weapons[grep("Molotov", Weapons)]<-"Arson"
Weapons[grep("flammable liquid", Weapons)]<-"Arson"
Weapons[grep("Pistol", Weapons)]<-"Small caliber"
Weapons[grep("Stones", Weapons)]<-"Rocks"
Weapons[grep("Unknown", Weapons)]<-"Other"
Weapons[grep("bottle", Weapons)]<-"Other"
Weapons[grep("Fists", Weapons)]<-"Other"
Weapons[grep("Iron bar", Weapons)]<-"Other"
Weapons[grep("bats", Weapons)]<-"Other"
Weapons[grep("Club", Weapons)]<-"Other"
Weapons[grep("Beaten to death", Weapons)]<-"Other"
Weapons[grep("Graffiti", Weapons)]<-"Other" # highly questionable if that should be considered a terror attack
Weapons[is.na(Weapons)] <- "Unknown"

Weapons[which(Weapons=="Edged & Improvised"&Events_Relevant_QGIS$Date== 20100311)]<-"Rocks" 
Weapons[which(Weapons=="Edged & Improvised"&Events_Relevant_QGIS$Date== 20130119)]<-"Knife & Axe" #
Weapons[which(Weapons=="Edged & Improvised"&Events_Relevant_QGIS$Date== 20140619)]<-"Knife & Axe" #
Weapons[which(Weapons=="Edged & Improvised"&Events_Relevant_QGIS$Date== 20150929)]<-"Knife & Axe" #
Weapons[which(Weapons=="Edged & Improvised"&Events_Relevant_QGIS$Date== 20151118)]<-"Knife & Axe" #
Weapons[which(Weapons=="Edged & Improvised"&Events_Relevant_QGIS$Date== 20160116)]<-"Other"       #
Weapons[which(Weapons=="Edged & Improvised"&Events_Relevant_QGIS$Date== 20160229)]<-"Knife & Axe" #
Weapons[which(Weapons=="Edged & Improvised"&Events_Relevant_QGIS$Date== 20160226)]<-"Knife & Axe" #
Weapons[which(Weapons=="Edged & Improvised"&Events_Relevant_QGIS$Date== 20160613)]<-"Knife & Axe" #
Weapons[which(Weapons=="Edged & Improvised"&Events_Relevant_QGIS$Date== 20160718)]<-"Knife & Axe" #
Weapons[which(Weapons=="Edged & Improvised"&Events_Relevant_QGIS$Date== 20160806)]<-"Knife & Axe" #
Weapons[which(Weapons=="Edged & Improvised"&Events_Relevant_QGIS$Date== 20161031)[1]]<-"Knife & Axe" #
Weapons[which(Weapons=="Edged & Improvised"&Events_Relevant_QGIS$Date== 20160111)]<-"Knife & Axe" #
Weapons[which(Weapons=="Edged & Improvised"&Events_Relevant_QGIS$Date== 20100514)]<-"Knife & Axe" #
Weapons[which(Weapons=="Edged & Improvised"&Events_Relevant_QGIS$Date== 20160726)]<-"Knife & Axe" #
Weapons[which(Weapons=="Edged & Improvised"&Events_Relevant_QGIS$Date== 20100311)]<-"Rocks"       #
Weapons[which(Weapons=="Edged & Improvised"&Events_Relevant_QGIS$Date== 20100514)]<-"Knife & Axe" #
Weapons[which(Weapons=="Edged & Improvised"&Events_Relevant_QGIS$Date== 20170826)]<-"Knife & Axe" #

(Table.Weapons<-table(Weapons))

Weapon_Dataframe<-unlist(tapply(Events_Relevant_QGIS$AtThreat, Weapons, function(x) 
    c(round(mean(x),2),round(var(x),2),paste(range(x)[1],":",range(x)[2],sep=""),
      round(var(x)/mean(x),1),length(x)) ))
Islamic<-which(Events_Relevant_QGIS$TypeOfTerrorism=="Islamistic")
Right_Terror<-which(Events_Relevant_QGIS$TypeOfTerrorism=="Right Terror")
Left_Terror<-which(Events_Relevant_QGIS$TypeOfTerrorism=="Left Terror")
Weapon_Islamic<-(tapply(Events_Relevant_QGIS$TypeOfTerrorism[Islamic], Weapons[Islamic], function(x) length(x)) )
Weapon_Right<-(tapply(Events_Relevant_QGIS$TypeOfTerrorism[Right_Terror], Weapons[Right_Terror], function(x) length(x)) )
Weapon_Left<-(tapply(Events_Relevant_QGIS$TypeOfTerrorism[Left_Terror], Weapons[Left_Terror], function(x) length(x)) )

Weapon_Mat<-as.data.frame(matrix(Weapon_Dataframe,ncol = 5,byrow = T))
rownames(Weapon_Mat) <- names(Table.Weapons)

Weapon_Mat_merged<-merge(merge(merge(Weapon_Mat, as.data.frame(Weapon_Islamic), by = "row.names", all = TRUE),
    as.data.frame(Weapon_Right), by.x = "Row.names", by.y = "row.names", all = TRUE),
    as.data.frame(Weapon_Left), by.x = "Row.names", by.y = "row.names", all = TRUE)
colnames(Weapon_Mat_merged)<-c("ID","Mean","Var","Range","Mean/Var","n Total", "n Islamic Attacks","n Right Attacks","n Left Attacks")
rownames(Weapon_Mat_merged) <- names(Table.Weapons)
(Weapon_Mat_merged<-Weapon_Mat_merged[,2:9])

###############################

#### Type of Attack 
EventType<-Events_Relevant_QGIS$EventType

EventType_Islamic<-(tapply(Events_Relevant_QGIS$TypeOfTerrorism[Islamic], EventType[Islamic], function(x) length(x)) )
EventType_Right<-(tapply(Events_Relevant_QGIS$TypeOfTerrorism[Right_Terror], EventType[Right_Terror], function(x) length(x)) )
EventType_Left<-(tapply(Events_Relevant_QGIS$TypeOfTerrorism[Left_Terror], EventType[Left_Terror], function(x) length(x)) )

EventType_Explosive_Mat_merged<-merge(merge(as.data.frame(EventType_Islamic),as.data.frame(EventType_Right), by = "row.names", all = TRUE),
                         as.data.frame(EventType_Left), by.x = "Row.names", by.y = "row.names", all = TRUE)
colnames(EventType_Explosive_Mat_merged)<-c("ID", "n Islamic Attacks","n Right Attacks","n Left Attacks")
rownames(EventType_Explosive_Mat_merged) <- names(table(EventType))
(EventType_Explosive_Mat_merged<-EventType_Explosive_Mat_merged[,2:4]) # NSAG - Non State Armed Group

#
EventType_Dataframe<-unlist(tapply(Events_Relevant_QGIS$AtThreat, EventType, function(x) 
  c(round(mean(x),2),round(var(x),2),paste(range(x)[1],":",range(x)[2],sep=""),
    round(var(x)/mean(x),1),length(x)) ))

EventType_Mat<-as.data.frame(matrix(EventType_Dataframe,ncol = 5,byrow = T))
rownames(EventType_Mat) <- names(table(EventType))

EventType_merged<-merge(merge(merge(EventType_Mat, as.data.frame(EventType_Islamic), by = "row.names", all = TRUE),
                               as.data.frame(EventType_Right), by.x = "Row.names", by.y = "row.names", all = TRUE),
                         as.data.frame(EventType_Left), by.x = "Row.names", by.y = "row.names", all = TRUE)
colnames(EventType_merged)<-c("ID","Mean","Var","Range","Mean/Var","n Total", "n Islamic Attacks","n Right Attacks","n Left Attacks")
rownames(EventType_merged) <- names(table(EventType))
(EventType_merged<-EventType_merged[,2:9])

### Type of Attack against Weapon used
Events_Relevant_QGIS$AtThreat.Category<-Events_Relevant_QGIS$AtThreat
Events_Relevant_QGIS$AtThreat.Category[which(Events_Relevant_QGIS$EventType=="Facility/Infrastructure Attack")]
EventType_Islamic<-(tapply(Weapons[Islamic], EventType[Islamic], function(x) table(x)) )
EventType_Right<-(tapply(Weapons[Right_Terror], EventType[Right_Terror], function(x) table(x)) )
EventType_Left<-(tapply(Weapons[Left_Terror], EventType[Left_Terror], function(x) table(x)) )


#rownames(EventType_Explosive_Mat) <- names(table(EventType))

#EventType_Explosive_Mat_merged<-merge(as.data.frame(EventType_Islamic), by = "row.names", all = TRUE)
#colnames(EventType_Explosive_Mat_merged)<-c("ID", "n Islamic Attacks","n Right Attacks","n Left Attacks")
#rownames(EventType_Explosive_Mat_merged) <- names(table(EventType))
#(EventType_Explosive_Mat_merged<-EventType_Explosive_Mat_merged[,2:4])

###
  require(qdapRegex)
TerrorGroup<-unlist(lapply((rm_between(Events_Relevant_QGIS$Actors, "Organisation name - ", " Organisation Description", extract=TRUE)),`[[`, 1))
TerrorGroup<-gsub(" -","",TerrorGroup)
TerrorGroup<-gsub("\\.","",TerrorGroup)

TerrorGroup[grep("Al-Qaeda", TerrorGroup)]<-"Al-Qaeda"
TerrorGroup[grep("Al-Qaida", TerrorGroup)]<-"Al-Qaeda"
TerrorGroup[grep("Anarchist Brigade", TerrorGroup)]<-"Anarchist Brigade"
#TerrorGroup[grep("Irish National Liberation Army (INLA)", TerrorGroup)]<-"Irish National Liberation Army (INLA)"
TerrorGroup[grep("Youths", TerrorGroup)]<-"Right-Wing Youths"
TerrorGroup[grep("Left-Wing", TerrorGroup)]<-"Left-Wing"

(table(TerrorGroup))
TG.Is<-table(TerrorGroup[which(Events_Relevant_QGIS$TypeOfTerrorism=="Islamistic")])
TG.Is.dataf<-cbind(TG.Is,rep("Islamic",length(TG.Is)))
TG.Le<-table(TerrorGroup[which(Events_Relevant_QGIS$TypeOfTerrorism=="Left Terror")])
TG.Le.dataf<-cbind(TG.Le,rep("Left Terror",length(TG.Le)))
TG.Ri<-table(TerrorGroup[which(Events_Relevant_QGIS$TypeOfTerrorism=="Right Terror")])
TG.Ri.dataf<-cbind(TG.Ri,rep("Right Terror",length(TG.Ri)))

TG.dataf<-data.frame(rbind(TG.Is.dataf,TG.Le.dataf,TG.Ri.dataf))
plot(TG.dataf$TG.Is, xaxt="n", yaxt="n",ylab="")
axis(1,at=1:length(TG.dataf$TG.Is) ,las=2, labels = names(TG.dataf$TG.Is))

TerrorCountry<-unlist(lapply((rm_between(Events_Relevant_QGIS$Actors, "Organisation Country - ", " NSAG Orientation", extract=TRUE)),`[[`, 1))
TerrorCountry2<-sub('.+Organisation Country - (.+)', '\\1', Events_Relevant_QGIS$Actors)
TerrorCountry[which(is.na(TerrorCountry))]<-TerrorCountry2[which(is.na(TerrorCountry))]
TerrorCountry<-gsub("\\.","",TerrorCountry)


################################ StackedBarplot
Events_Relevant_QGIS$AtThreat.Category <- Events_Relevant_QGIS$AtThreat
Events_Relevant_QGIS$AtThreat.Category[which(Events_Relevant_QGIS$AtThreat==0)] <- 0
Events_Relevant_QGIS$AtThreat.Category[which(Events_Relevant_QGIS$AtThreat>1&Events_Relevant_QGIS$AtThreat<11)] <- paste("1 < Y \u2264 10")
Events_Relevant_QGIS$AtThreat.Category[which(Events_Relevant_QGIS$AtThreat>10&Events_Relevant_QGIS$AtThreat<=40)] <- paste("10 < Y \u2264 40")
Events_Relevant_QGIS$AtThreat.Category[which(Events_Relevant_QGIS$AtThreat>40&Events_Relevant_QGIS$AtThreat<=100)] <- paste("40 < Y \u2264 100")
Events_Relevant_QGIS$AtThreat.Category[which(Events_Relevant_QGIS$AtThreat>100)] <- paste("Y > 100")
StackedBarplot<-c(paste(Events_Relevant_QGIS$AtThreat.Category,Events_Relevant_QGIS$TypeOfTerrorism))
tab.StackedBarplot <- table(StackedBarplot)
mat.StackedBarplot<-matrix(c(tab.StackedBarplot[1:3],tab.StackedBarplot[7:9],tab.StackedBarplot[4:6],tab.StackedBarplot[10:12],
         tab.StackedBarplot[15],0,tab.StackedBarplot[16],tab.StackedBarplot[13],0,tab.StackedBarplot[14]),ncol = 6)
colnames(mat.StackedBarplot) <-names(table(Events_Relevant_QGIS$AtThreat.Category))
par(mar=c(5,3,2,2),mfrow=c(1,1))
xx<-barplot(mat.StackedBarplot,col=c("greenyellow","firebrick2","deepskyblue1"),
            ylab = "Frequency", ylim=c(0,600))
text(x = xx, y = table(Events_Relevant_QGIS$AtThreat.Category), label = table(Events_Relevant_QGIS$AtThreat.Category), pos = 3, cex = 0.8, col = "black")
##################################################
### Wounded and Killed over time by Type of Terrorism
Year<-as.numeric(str_sub(Events_Relevant_QGIS$DateCoded,1,4))
Years<-as.numeric(names(table(Year)))
YearThreat<-aggregate(Events_Relevant_QGIS$AtThreat, by=list(Category=Year), FUN=sum)
YearThreatToT<-aggregate(Events_Relevant_QGIS$AtThreat, by=list(Category=Year,Terrorsim=Events_Relevant_QGIS$TypeOfTerrorism), FUN=sum)

YearTypeTerrorsim<-matrix(0,ncol=(2020-1992+1),nrow=3)
TerrorismYear<-as.numeric(as.factor(YearThreatToT[,2]))
for(i in 1:length(YearThreatToT[,1])){
YearTypeTerrorsim[TerrorismYear[i],(YearThreatToT[i,1]-min(YearThreatToT[,1])+1)] <- YearThreatToT[i,3]
}
rownames(YearTypeTerrorsim)<-names(table(Events_Relevant_QGIS$TypeOfTerrorism))
colnames(YearTypeTerrorsim) <- as.character(1992:2020)
par(mar=c(5,1,2,1))
xx<-barplot(YearTypeTerrorsim,col=c("greenyellow","firebrick2","deepskyblue1"),axes=F,ylim=c(0,1400), ylab = "Frequency",las=2)
text(x = xx, y = colSums(YearTypeTerrorsim), label =colSums(YearTypeTerrorsim),las=2, pos = 3, cex = 0.8, col = "black")

## Attacks over time
YearToT<-aggregate(rep(1,length(Events_Relevant_QGIS$AtThreat)), by=list(Category=Year,Terrorsim=Events_Relevant_QGIS$TypeOfTerrorism), FUN=sum)
YearTypeTerrorsim<-matrix(0,ncol=(2020-1992+1),nrow=3)
TerrorismYear<-as.numeric(as.factor(YearToT[,2]))
for(i in 1:length(YearThreatToT[,1])){
  YearTypeTerrorsim[TerrorismYear[i],(YearToT[i,1]-min(YearToT[,1])+1)] <- YearToT[i,3]
}
rownames(YearTypeTerrorsim)<-names(table(Events_Relevant_QGIS$TypeOfTerrorism))
colnames(YearTypeTerrorsim) <- as.character(1992:2020)
par(mar=c(5,1,2,1))
xx<-barplot(YearTypeTerrorsim,col=c("greenyellow","firebrick2","deepskyblue1"),axes=F,ylim=c(0,190), ylab = "Frequency",las=2)
text(x = xx, y = colSums(YearTypeTerrorsim), label =colSums(YearTypeTerrorsim),las=2, pos = 3, cex = 0.8, col = "black")


###############################################################
### GGPLOT Observed Data
require(ggplot2)
dat.observed = data.frame(Y.Train=rep(Events_Relevant_QGIS$AtThreat,2))
ggplot(dat.observed, aes(x=Y.Train)) +
  geom_histogram(alpha=0.9,color=1,binwidth = 10,position="identity")+
  ylab("Frequency")+
  scale_y_log10()+
  xlab("Amount of 'Wounded & Killed'")+
  xlim(c(-5,500))+
  coord_cartesian(ylim=c(1.6, 5000))+
  theme(panel.background = element_rect(fill = 0, colour = 0),
        axis.title.x = element_text(family="Arial",size=13,
                                    face="bold",colour = "Black",vjust=-2,hjust=0.5),
        axis.title.y = element_text(family="Arial",size=13,
                                    face="bold",colour = "Black",vjust=5,hjust=0.5),
        legend.title=element_text(family="Arial",size=13,
                                  face="bold",colour = "Black",vjust=-2,hjust=0.5),
        plot.title = element_text(hjust = 0.55,vjust=-11,size=16,family="Arial",face="bold"),
        plot.subtitle = element_text(hjust = 0.55,vjust=-11,size=16,family="Arial",face="bold"))+
  theme(plot.margin=unit(c(-1,1,3,1),"cm"),legend.position="none")

### Observed Data Terrorism
Terrorism.Obs<-Events_Relevant_QGIS$TypeOfTerrorism
dat.observed.T = data.frame(Y.Train=rep(Events_Relevant_QGIS$AtThreat,2),Terrorism.Obs=Terrorism.Obs )
ggplot(dat.observed.T, aes(x=Y.Train,fill=Terrorism.Obs)) +
  geom_histogram(alpha=0.65,color=1,binwidth = 10,position="identity")+
  ylab("Frequency")+
  scale_y_log10()+
  xlab("Amount of 'Wounded & Killed'")+
  xlim(c(-5,500))+
  coord_cartesian(ylim=c(1.6, 5000))+
  theme(panel.background = element_rect(fill = 0, colour = 0),
        axis.title.x = element_text(family="Arial",size=13,
                                    face="bold",colour = "Black",vjust=-2,hjust=0.5),
        axis.title.y = element_text(family="Arial",size=13,
                                    face="bold",colour = "Black",vjust=5,hjust=0.5),
        legend.title=element_blank(),
        plot.title = element_text(hjust = 0.55,vjust=-11,size=16,family="Arial",face="bold"),
        plot.subtitle = element_text(hjust = 0.55,vjust=-11,size=16,family="Arial",face="bold"))+
  theme(plot.margin=unit(c(-1,1,1.5,1),"cm"),
        legend.spacing.y = unit(c(0.5,1,1,1,1,1,1), "cm"),legend.position="bottom")+
  scale_fill_manual(name="Types of Terrorism",
                    values=c("green","red","white"))
### Observed Data Event type
dat.observed.E = data.frame(Y.Train=rep(Events_Relevant_QGIS$AtThreat,2),EventType=rep((EventType),2) )
ggplot(dat.observed.E, aes(x=Y.Train,fill=EventType)) +
  geom_histogram(alpha=0.65,color=1,binwidth = 10,position="identity")+
  ylab("Frequency")+
  scale_y_log10()+
  xlab("Amount of 'Wounded & Killed'")+
  xlim(c(-5,500))+
  coord_cartesian(ylim=c(1.6, 5000))+
  theme(panel.background = element_rect(fill = 0, colour = 0),
        axis.title.x = element_text(family="Arial",size=13,
                                    face="bold",colour = "Black",vjust=-2,hjust=0.5),
        axis.title.y = element_text(family="Arial",size=13,
                                    face="bold",colour = "Black",vjust=5,hjust=0.5),
        legend.title=element_blank(),
        plot.title = element_text(hjust = 0.55,vjust=-11,size=16,family="Arial",face="bold"),
        plot.subtitle = element_text(hjust = 0.55,vjust=-11,size=16,family="Arial",face="bold"))+
  theme(plot.margin=unit(c(-1,1,1.5,1),"cm"),
        legend.spacing.y = unit(c(0.5,1,1,1,1,1,1), "cm"),legend.position="bottom")
### Observed Data Weapons

dat.observed.W = data.frame(Y.Train=rep(Events_Relevant_QGIS$AtThreat,2),Weapons=rep(Weapons,2))
ggplot(dat.observed.W, aes(x=Y.Train,fill=Weapons)) +
  geom_histogram(alpha=0.65,color=1,binwidth = 10,position="identity")+
  ylab("Frequency")+
  scale_y_log10()+
  xlab("Amount of 'Wounded & Killed'")+
  xlim(c(-5,500))+
  coord_cartesian(ylim=c(1.6, 5000))+
  theme(panel.background = element_rect(fill = 0, colour = 0),
        axis.title.x = element_text(family="Arial",size=13,
                                    face="bold",colour = "Black",vjust=-2,hjust=0.5),
        axis.title.y = element_text(family="Arial",size=13,
                                    face="bold",colour = "Black",vjust=5,hjust=0.5),
        legend.title=element_blank(),
        plot.title = element_text(hjust = 0.55,vjust=-11,size=16,family="Arial",face="bold"),
        plot.subtitle = element_text(hjust = 0.55,vjust=-11,size=16,family="Arial",face="bold"))+
  theme(plot.margin=unit(c(-1,1,0.85,1),"cm"),
        legend.spacing.y = unit(c(0.5,1,1,1,1,1,1), "cm"),legend.position="bottom")
##################################################


       