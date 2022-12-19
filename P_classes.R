########################################################## P Project Bawue ##################################################################
# Start 19.12.22
#

# Getting the the P  classes data and combining it with GIS soil data. 

# Can I read a dbf attribute table from GIS in R and combine the data?
#install.packages("foreign")
?Foreign
library(foreign)
library(dplyr)
library(readxl)
library(stringr)

#my_file <- choose.files()
#my_file

Bawue_gemeinde <- read.dbf("C:\\Users\\User\\OneDrive - bwedu\\Dokumente\\Landwirtschaftliche Betriebslehre\\Projekt_P_Bawü\\Spatial_data\\BaWue_Gemeindekreisen\\Ba_Wue\\Ba_wue_gemeinde.dbf")
glimpse(Bawue_gemeinde)

# Reading in P class file for Ackerbau
#my_file <-choose.files()
#my_file


P_classes_ackerbau<-read_xlsx("C:\\Users\\User\\OneDrive - bwedu\\Dokumente\\Landwirtschaftliche Betriebslehre\\Projekt_P_Bawü\\Excel_data\\P_Grundnaehrstoffversorgung LEL 2020\\Ackerbau\\Phosphat_klasse\\Phosphat_klasse_bawü_2013_18.xlsx")
Ph_ackerbau <- read_xlsx("C:\\Users\\User\\OneDrive - bwedu\\Dokumente\\Landwirtschaftliche Betriebslehre\\Projekt_P_Bawü\\Excel_data\\P_Grundnaehrstoffversorgung LEL 2020\\Ackerbau\\PH_wert\\PH_wert_2013_18_Gemeindeebene.xlsx")
Kali_ackerbau <- read_xlsx("C:\\Users\\User\\OneDrive - bwedu\\Dokumente\\Landwirtschaftliche Betriebslehre\\Projekt_P_Bawü\\Excel_data\\P_Grundnaehrstoffversorgung LEL 2020\\Ackerbau\\Kalium_klasse\\kalium_klassen_2013_18.xlsx")

P_classes_gruen <- read_xlsx("C:\\Users\\User\\OneDrive - bwedu\\Dokumente\\Landwirtschaftliche Betriebslehre\\Projekt_P_Bawü\\Excel_data\\P_Grundnaehrstoffversorgung LEL 2020\\Gruenland\\Phosphat_klasse_bawü_2013_18.xlsx")
Ph_gruen <- read_xlsx("C:\\Users\\User\\OneDrive - bwedu\\Dokumente\\Landwirtschaftliche Betriebslehre\\Projekt_P_Bawü\\Excel_data\\P_Grundnaehrstoffversorgung LEL 2020\\Gruenland\\PH_wert_2013_18_Gemeindeebene.xlsx")
Kali_gruen <- read_xlsx("C:\\Users\\User\\OneDrive - bwedu\\Dokumente\\Landwirtschaftliche Betriebslehre\\Projekt_P_Bawü\\Excel_data\\P_Grundnaehrstoffversorgung LEL 2020\\Gruenland\\Kalium_klasse.xlsx")

glimpse(P_classes_ackerbau)

####### joining the data 
ackerbau <- left_join(P_classes_ackerbau, Ph_ackerbau, by="Gemeindenummer")
ackerbau <- left_join(ackerbau, Kali_ackerbau, by="Gemeindenummer")
glimpse(ackerbau)

gruenland <- left_join(P_classes_gruen, Ph_gruen, by="Gemeindenummer")
gruenland <- left_join(gruenland, Kali_gruen, by="Gemeindenummer")
glimpse(gruenland)

# removing datasets
rm(P_classes_ackerbau, Ph_ackerbau,Kali_ackerbau, P_classes_gruen, Ph_gruen, Kali_gruen)

# somehow I need to use the Gemeindenummer or name 
Bawue_gemeinde %>% filter(GEN_2=="Stuttgart")
Bawue_gemeinde <-Bawue_gemeinde %>%  mutate(AGS_2=as.character(Bawue_gemeinde$AGS_2))


# AGS_2 is the variable to match the data, AGS_2 has 08 before each string. Need to add this to do leftjoin
str(Bawue_gemeinde)
str(ackerbau)
ackerbau <- ackerbau %>%  mutate(Gemeindenummer=as.character(ackerbau$Gemeindenummer))

ackerbau <- ackerbau %>%  mutate(Gemeindenummer=str_pad(ackerbau$Gemeindenummer,7, pad="8"))
ackerbau <- ackerbau %>%  mutate(AGS_2=str_pad(ackerbau$Gemeindenummer,8, pad="0"))
#P_classes_ackerbau <- P_classes_ackerbau %>%  mutate(AGS_2=as.factor(AGS_2))


#Alright now it should be the same ID for Gemeinde in both files

join_ackerbau<-left_join(Bawue_gemeinde, ackerbau, by="AGS_2")
glimpse(join_ackerbau)

# Now the P_level is added to the dataset. Can I export this now and add it to the shape?
getwd()
write.dbf(join_ackerbau, "ackerbau_P")

#Achievement:
# P_classes, K_classes and Ph are added now to the shapefile für Gemeinden in Bawü
# Now this work continues in QGis
# In case the same is needed for Gruenland, it can be easily reproduced.



