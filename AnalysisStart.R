library(stringr)
library(readr)
library(readxl)
library(httr)
Dataset <- read_csv("/home/ka4/Desktop/Dokumente Masterarbeit/Dataset.xlsx")

# relevant variables
VariableNames <- c("EventID","Latitude","Longitude","Headline","Description","Date","ISOName","Actors","AttackTotalKilled","AttackTotalWounded",
                   "SuspectsKilled","ProvinceName","Weapons","EventType","TypeOfTerrorism")
Events_Relevant_QGIS<-Dataset[,VariableNames]

# Recode Date
Events_Relevant_QGIS$Date[which(is.na(as.Date(paste(str_sub(Events_Relevant_QGIS$Date,1,4),str_sub(Events_Relevant_QGIS$Date,5,6),
                                                    str_sub(Events_Relevant_QGIS$Date,7,8),sep="/"),
                                              "%Y/%m/%d"))==TRUE)]<-20021211
Events_Relevant_QGIS$DateCoded<-as.Date(paste(str_sub(Events_Relevant_QGIS$Date,1,4),str_sub(Events_Relevant_QGIS$Date,5,6),
              str_sub(Events_Relevant_QGIS$Date,7,8),sep="/"),
        "%Y/%m/%d")

today<-c(as.Date("1992/01/01"):as.Date("2020/04/30"))

### Export It into a Excel File
# write_csv(Dataset, "/home/ka4/Desktop/Dokumente Masterarbeit/Dataset.xlsx")
# write_csv(Events_Relevant_QGIS, "/home/ka4/Desktop/Dokumente Masterarbeit/QGIS/Events_Relevant_QGIS.xlsx")
# Events_Relevant_QGIS_IS <- Events_Relevant_QGIS[which(Events_Relevant_QGIS$TypeOfTerrorism=="Islamistic"),]
# Events_Relevant_QGIS_LT <- Events_Relevant_QGIS[which(Events_Relevant_QGIS$TypeOfTerrorism=="Left Terror"),]
# Events_Relevant_QGIS_RT <- Events_Relevant_QGIS[which(Events_Relevant_QGIS$TypeOfTerrorism=="Right Terror"),]
# write_csv(Events_Relevant_QGIS_IS, "/home/ka4/Desktop/Dokumente Masterarbeit/QGIS/Events_Relevant_QGIS_IS.xlsx")
# write_csv(Events_Relevant_QGIS_LT, "/home/ka4/Desktop/Dokumente Masterarbeit/QGIS/Events_Relevant_QGIS_LT.xlsx")
# write_csv(Events_Relevant_QGIS_RT, "/home/ka4/Desktop/Dokumente Masterarbeit/QGIS/Events_Relevant_QGIS_RT.xlsx")
