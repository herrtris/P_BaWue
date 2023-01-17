#######################################################################################
#   This script is based on the carbon farming script downloaded 21.12.22 from ModellAG                          #
#########################################################################################
rm(list=ls())

input <- choose.files()
input

test <- read.csv(input, sep=",", stringsAsFactors = T)
str(test)

#### Testing the same workstream as christian did in prepration for CropRota
library(dplyr)
library(tidyr)
library(readxl)
#install.packages("rJava")
#library(rJava)
#install.packages("writexl")
library(writexl)
library(ggplot2)
library(tidyverse)

Flaechennutzung_Nutzcode <-read.csv("C:\\Users\\User\\OneDrive - bwedu\\Dokumente\\Landwirtschaftliche Betriebslehre\\Projekt_P_Bawü\\GAMS\\Modell-AG\\Modell-AG\\Flaechennutzung_Nutzcode.csv", sep=";") 

### File has to come from GIS, GA DATen und DAten der Bodenkarte 50
AckerschlaegeBW <- read.csv("C:\\Users\\User\\OneDrive - bwedu\\Dokumente\\Landwirtschaftliche Betriebslehre\\Projekt_P_Bawü\\GAMS\\Modell-AG\\Modell-AG\\AckerschlaegeBW.csv", sep=";", stringsAsFactors = T)

glimpse(AckerschlaegeBW)

glimpse(test)

#create an ID for test
test$OID_<- 1:nrow(test)


## GA-Datensatz vorbereiten

Flaechennutzung_Nutzcode <- Flaechennutzung_Nutzcode %>% select(1:5) %>% filter(!is.na(NUTZCODE))

AckerschlaegeBW <-  AckerschlaegeBW %>% mutate(Schlag_ID = OID_) %>% select(Schlag_ID, FLAECHE_HA, NUTZCODE, NUTS_CODE, NUTS_NAME, NAME, SCHLUESSEL, NATBOD, P.2013...2) %>%
  left_join(Flaechennutzung_Nutzcode, by="NUTZCODE") %>%
  filter(Flaechenart =="Ackerland") 

glimpse(AckerschlaegeBW)

test2 <- test %>% mutate(Schlag_ID=OID_) %>% select(Schlag_ID, FLAECHE_HA, NUTZCODE, NUTS_2, AGS_0_2, NATBOD, NUTZUNG, P.2013...2, pH.2013.., K.2013...2)
test2 <- test2 %>% left_join(Flaechennutzung_Nutzcode, by="NUTZCODE") %>% filter(Flaechenart=="Ackerland")
test2 %>% head(10)

AckerschlaegeBW <- AckerschlaegeBW %>%

mutate(Bodenguete = ifelse(NATBOD %in% c("1,0"), "gering",
ifelse(NATBOD %in% c("1,5"), "gering",
ifelse(NATBOD %in% c( "2,0"), "mittel",
ifelse(NATBOD %in% c("2,5"), "mittel",
ifelse(NATBOD %in% c( "3,0"), "hoch",    
ifelse(NATBOD %in% c( "3,5"), "hoch",    
ifelse(NATBOD %in% c( "4,0"), "hoch",     
 "mittel")))))))) 

glimpse(AckerschlaegeBW)
AckerschlaegeBW$Bodenguete <- factor(AckerschlaegeBW$Bodenguete, levels=c("gering", "mittel", "hoch")) 
levels(AckerschlaegeBW$Bodenguete)

# There is only Bodenguete "miitel" in the daatset code above does not work
AckerschlaegeBW %>% group_by(Bodenguete) %>% count()

# AChtung: 10250 obs do not have a natbod value for ackerschlaege BW
AckerschlaegeBW %>% group_by(NATBOD) %>% count()

options(scipen = 999)
bodenguete_ABW <-ggplot(AckerschlaegeBW, aes(x=Bodenguete))+geom_bar()+labs(title="Ackerschlaege_BW")



test2 <- test2 %>%
  
  mutate(Bodenguete = ifelse(NATBOD %in% c("1,0"), "gering",
    ifelse(NATBOD %in% c("1,5"), "gering",
    ifelse(NATBOD %in% c( "2,0"), "mittel",
    ifelse(NATBOD %in% c("2,5"), "mittel",
    ifelse(NATBOD %in% c( "3,0"), "hoch",    
    ifelse(NATBOD %in% c( "3,5"), "hoch",    
    ifelse(NATBOD %in% c( "4,0"), "hoch",     
                "mittel")))))))) 

test2$Bodenguete <- factor(test2$Bodenguete, levels=c("gering", "mittel", "hoch")) 
test2 %>% group_by(Bodenguete) %>% count()

## Es gibt Unterschiede zwischen den Methoden, die Bodengüte ist im selben range aber doch Abweichungen von ca 5000 Einteilungen


# not necessary for test2
AckerschlaegeBW <- AckerschlaegeBW %>% mutate(FLAECHE_HA=as.numeric(gsub(",", ".", gsub("\\.", "", FLAECHE_HA)))) 

# Unterteilung der Ssumchlaggroesen 
AckerschlaegeBW$ha_Kat = cut(AckerschlaegeBW$FLAECHE_HA, c(0, 2, 5, 10, 1000), 
                    labels=c("1" ,"2", "5", "10"))

test2$ha_Kat = cut(test2$FLAECHE_HA, c(0,2,5,10,1000), levels=c("1", "2", "5","10", "1000"))

head(AckerschlaegeBW)

nuts_exclude <- c("DE715", "DE27D", "DE27C", "DE27A", "DE279", "DE278", "DE277", "DE26C", "DE26A", "DE269", "DE25A", "DE256")
AckerschlaegeBW %>% filter(!NUTS_CODE %in% nuts_exclude) %>%summarise(sum_ha=sum(FLAECHE_HA), n=n()) #abweichung kommt daher, dass anderer join in ARcGis zuvr geählt wrde

## excluding  schläge outside of bawü sum_ha =738223ha n=516,479 Schläge

head(test2)

test2 %>% summarise(sum_ha=sum(FLAECHE_HA))
abweichung=739122-739039.9
abweichung

test2 %>% select(Schlag_ID) %>% count()

# In test 2 gibt es 516,905 Schlaege und 739,039.9 ha GEsamtflaeche, circa eine diskrepanz von 80ha zu christians join in arcgis
# okay 

# in Ackerschlaege BW
# 739,122 ha Ackerland  
# 516,981 Schläge 

# plotting differences and Natbod
Bodenguete_test2<-ggplot(test2, aes(Bodenguete))+ geom_bar()+ labs(title="ZentroidMethod")

require(gridExtra)
grid.arrange(bodenguete_ABW, Bodenguete_test2, nrow=1)


NatBod_ABW <- ggplot(AckerschlaegeBW, aes(NATBOD))+ geom_bar()+ labs(title="ACkerschlaege_BW")
NatBod_Zenrtoid <- ggplot(test2, aes(NATBOD)) + geom_bar()+ labs(title="Znetroid_NatBod")

grid.arrange(NatBod_ABW, NatBod_Zenrtoid, nrow=1)


######################################################################################################################################################################
########################################################################################################################################################################
# Landuseplots
#########################################

### Aggregation der Schläge zu Landnutzungseinheiten
Landuse_plots <- AckerschlaegeBW %>% group_by(NUTS_CODE, SCHLUESSEL, Bodenguete, P.2013...2) %>% summarise(sum_ha = sum(FLAECHE_HA)) %>%
                 ungroup() %>% mutate(Plot_ID = row_number()) 

Landuse_plots <-  Landuse_plots[,c(5,1,3,2,4)]
head(Landuse_plots, 10)

Landuse_plots %>% filter(NUTS_CODE=="DE111")
#test3 %>% filter(NUTS_2=="DE111")

test3 <- test2 %>%filter(!Kennung=="B" | !Kennung=="Rog" | !Kennung=="KL") %>% group_by(NUTS_2, AGS_0_2, Bodenguete, P.2013...2) %>% 
                  summarise(sum_ha=sum(FLAECHE_HA))%>% ungroup() %>% mutate(PLOT_ID=row_number()) %>% filter(!AGS_0_2 %in% kommune_filter_out)


test3
landuse_plots_test3<-test3
test3 %>% filter(NUTS_2=="DE111")

# große abweichung zwischen land_use_plots_test3 und landuse_plots
landuse_plots_test3 %>% group_by(NUTS_2) %>% count() %>% print(n=44)
Landuse_plots %>% group_by(NUTS_CODE) %>% count() %>% print(n=57)

landuse_plots_test3 <- landuse_plots_test3[,c(6,1,3,2,4,5)]

head(landuse_plots_test3)
head(Landuse_plots)

#landuseplot_test3 2700 obs
# landuseplot_Acker 3572 obs --> somehow there is this large difference

landuse_1<-ggplot(landuse_plots_test3, aes(NUTS_2))+ geom_bar()+theme(axis.text.x = element_text(angle=90)) + labs(title="Zentroide")
landuse_plots_test3 %>% group_by(Bodenguete) %>%count()
landuse_plots_test3 %>% group_by(NUTS_2) %>%count()

landuse_2 <-ggplot(Landuse_plots, aes(NUTS_CODE))+geom_bar()+theme(axis.text.x = element_text(angle=90))+labs(title="AckerschlaegeBW")

require(gridExtra)
grid.arrange(landuse_2,landuse_1, ncol=1)

landuse_1_sumha<-ggplot(landuse_plots_test3%>% group_by(NUTS_2)%>% summarise(sum_ha=sum(sum_ha)), aes(NUTS_2, sum_ha))+ 
                  geom_col()+theme(axis.text.x = element_text(angle=90)) + labs(title="Zentroide")
landuse_2_sumha <-ggplot(Landuse_plots%>% group_by(NUTS_CODE)%>%summarise(sum_ha=sum(sum_ha)), aes(NUTS_CODE, sum_ha))+geom_col()+
                  theme(axis.text.x = element_text(angle=90))+labs(title="AckerschlaegeBW")

grid.arrange(landuse_2_sumha, landuse_1_sumha, ncol=1)

# landuse 2  contains Nuts codes from bayern, how does it look if I exclude these
nuts_exclude <- c("DE715", "DE27D", "DE27C", "DE27A", "DE279", "DE278", "DE277", "DE26C", "DE26A", "DE269", "DE25A", "DE256")
Landuse_plots_BAWueonly <- Landuse_plots %>% filter(!NUTS_CODE %in% nuts_exclude)

landuse_2_Bawue <-ggplot(Landuse_plots_BAWueonly, aes(NUTS_CODE))+geom_bar()+theme(axis.text.x = element_text(angle=90))+labs(title="Bawue_only_AckerschlaegeBW")
head(Landuse_plots_BAWueonly)

Landuse_plots_BAWueonly %>% summarize(sum_ha=sum(sum_ha))

# ohne Nuts außerhalb 738,222 ha für ackerschlaege BW
# eigene verschneidung  739,039.9                         # ziemlich identisch

##############################################################################################################################
### Landnutzung im Status Quo auf Kommunaler Ebene

Landuse_SQ <-  AckerschlaegeBW %>% group_by(SCHLUESSEL, NAME, Kennung) %>% summarise(sum_ha = sum(FLAECHE_HA))
Landuse_SQ_test <- test2 %>% group_by(AGS_0_2, Kennung) %>% summarise(sum_ha=sum(FLAECHE_HA))

### Kulturanteile auf Kreisebene für CropRota

Crop_Share_BW <-   AckerschlaegeBW %>%
filter(Kennung != "B") %>% group_by(NUTS_CODE) %>%
  mutate(sum_Kreis = sum(FLAECHE_HA)) %>% group_by(NUTS_CODE, Kennung, sum_Kreis) %>%
  summarise(sum=sum(FLAECHE_HA)) %>% mutate(Share = sum/sum_Kreis) %>% 
  select(-sum_Kreis, -sum) %>% spread(key=Kennung, value=Share)

Crop_share_BW_test <- test2 %>% filter(Kennung !="B") %>% group_by(NUTS_2) %>% mutate(sum_Kreis=sum(FLAECHE_HA)) %>%
                            group_by(NUTS_2, Kennung, sum_Kreis) %>% summarise(sum=sum(FLAECHE_HA)) %>% mutate(share=sum/sum_Kreis) %>%
                            select(-sum_Kreis, -sum) %>% spread(key=Kennung, value=share)

#####################################################################################
# Differences in the shares? ARe those large? Does not seem so in a visual check, but check Excel file

Crop_share_BW_test
share_test<-ggplot(test2 %>% filter(Kennung !="B") %>% group_by(NUTS_2) %>% mutate(sum_Kreis=sum(FLAECHE_HA)) %>%
         group_by(NUTS_2, Kennung, sum_Kreis) %>% summarise(sum=sum(FLAECHE_HA)) %>% mutate(share=sum/sum_Kreis) %>%
         select(-sum_Kreis, -sum), aes(NUTS_2, fill=Kennung))+ geom_bar(position="fill") + labs(title="zentroide") + theme(axis.text.x = element_text(angle=90))

share_ackerschlag <-ggplot(AckerschlaegeBW %>%
         filter(Kennung != "B") %>% group_by(NUTS_CODE) %>%
         mutate(sum_Kreis = sum(FLAECHE_HA)) %>% group_by(NUTS_CODE, Kennung, sum_Kreis) %>%
         summarise(sum=sum(FLAECHE_HA)) %>% mutate(Share = sum/sum_Kreis) %>% 
         select(-sum_Kreis, -sum), aes(NUTS_CODE, fill=Kennung))+geom_bar(position="fill")+ labs(title="Ackerschlaege") + theme(axis.text.x = element_text(angle=90))

grid.arrange(share_ackerschlag, share_test, ncol=1)

write_xlsx(x=Crop_Share_BW, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/crop_share_bw.xlsx", col_names = TRUE)
write_xlsx(x=Crop_share_BW_test, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/crop_share_bw_Zentroide.xlsx", col_names = TRUE)


write_xlsx(x=Landuse_SQ, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Landuse_SQ.xlsx", col_names = TRUE)
write_xlsx(x=Landuse_plots, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Landuse_plots.xlsx", col_names = TRUE)

write_xlsx(x=Landuse_SQ_test, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Landuse_SQ_Zentroide_.xlsx", col_names = TRUE)
write_xlsx(x=landuse_plots_test3, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Landuse_plots_Zentroide_P.xlsx", col_names = TRUE)


# Extracting unique Nuts_2 code and unique AGS_0_2 codes
# to include as sets into gams
landuse_plots_test3 %>% distinct(Bodenguete)
kreis<-landuse_plots_test3 %>% distinct(NUTS_2)
kommune<-landuse_plots_test3 %>% distinct(AGS_0_2)
kommune <- kommune %>% filter(!AGS_0_2 %in% kommune_filter_out)






write_xlsx(x=kreis, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/kreis_P.xlsx", col_names = TRUE)
write_xlsx(x=kommune, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/kommune_p.xlsx", col_names = TRUE)



#### checking this file, does it contain ROG and KL and B
### exclude these, is not in crop rotation file from felix
glimpse(Landuse_SQ_test)

Landuse_SQ_test_without_rogkl  <- Landuse_SQ_test %>% filter(!Kennung=="Rog") %>% filter(!Kennung=="KL") %>% filter(!Kennung=="B")
glimpse(Landuse_SQ_test_without_rogkl)
Landuse_SQ_test_without_rogkl %>% ungroup() %>%distinct(Kennung)

write_xlsx(x=Landuse_SQ_test_without_rogkl, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Landuse_SQ_Zentroide_wihtoutRogKM.xlsx", col_names = TRUE)

landuse_plots_test3 %>% distinct(AGS_0_2) %>% count()




#######################################################################################################################################################
#######################################################################################################################################################

## Preparing CropRota Output, at the moment coming from Felix's output file
#choose.files()

Rotations_felix <- read.csv("C:\\Users\\User\\OneDrive - bwedu\\Dokumente\\Landwirtschaftliche Betriebslehre\\Projekt_P_Bawü\\GAMS\\Crop_Rota_Felix\\Rotationen_Felix.csv", sep=";", stringsAsFactors = T)
str(Rotations_felix)

# How many unique Combos are there
Rotations_felix %>% distinct(Kombo) %>% count()

# There are 1193 distinct crop rotations in BaWue
Cr_bawue_distinct <- Rotations_felix %>% distinct(Kombo, Gewicht, Glied1, Glied2, Glied3, Glied4, Glied5) %>% select(Kombo:Gewicht)
glimpse(Cr_bawue_distinct)


levels(Cr_bawue_distinct$Glied1)
levels(Cr_bawue_distinct$Glied2)
levels(Cr_bawue_distinct$Glied3)
levels(Cr_bawue_distinct$Glied4)
levels(Cr_bawue_distinct$Glied5)
summary(Cr_bawue_distinct$Gewicht)

ggplot(Cr_bawue_distinct, aes(factor(Gewicht)))+ geom_bar()

# WE have 5gliedrig, 4 gliedirg, 3 gliedrig und 1 gliedrg in our dataset
# exclude eingliedrige Fruchtfolgen, eingliedrige Fruchtfolge has "-" for Glied2 to Glied5, or easier Gewicht equaling 1
Cr_bawue_distinct <- Cr_bawue_distinct %>% filter(!Gewicht=="1")

rotation_matrix <- Cr_bawue_distinct %>%  rowid_to_column()
write_xlsx(x=rotation_matrix, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/rotation_matrix.xlsx", col_names = TRUE)

crops <- levels(rotation_matrix$Glied1)
 glied1<-rotation_matrix %>% select(rowid, Glied1, Gewicht) %>% pivot_wider(names_from = Glied1, values_from = Gewicht)
glied2 <- rotation_matrix %>% select(rowid, Glied2, Gewicht) %>% pivot_wider(names_from = Glied2, values_from = Gewicht)
 glied3 <- rotation_matrix %>% select(rowid, Glied3, Gewicht) %>% pivot_wider(names_from = Glied3, values_from = Gewicht)
 glied4 <- rotation_matrix %>% select(rowid, Glied4, Gewicht) %>% pivot_wider(names_from = Glied4, values_from = Gewicht)
 glied5 <- rotation_matrix %>% select(rowid, Glied5, Gewicht) %>% pivot_wider(names_from = Glied5, values_from = Gewicht)

semi_join(glied1, glied2, by="rowid")

rotation_matrix %>% pivot_wider(names_from = c(Glied1), values_from = Gewicht, values_fill = 0)


rotation_matrix %>% select(rowid, Glied1, Gewicht) %>%pivot_longer(Glied1) %>% pivot_wider(names_from = value, values_from = Gewicht)

pivotlonger_test <- rotation_matrix %>% pivot_longer(c(Glied1, Glied2, Glied3, Glied4, Glied5))

Glied1<-pivotlonger_test %>%  filter(name=="Glied1") %>% 
           pivot_wider(names_from = value, values_from = Gewicht,  values_fill = 0) %>%
           mutate(Gr_Glied1=Gr, KG_Glied1=KG, KM_Glied1=KM, WG_Glied1=WG, 
                  SM_Glied1=SM, WW_Glied1=WW, WR_Glied1=WR, Win_Glied1=Win,
                  SG_Glied1=SG, Ha_Glied1=Ha, ZR_Glied1=ZR, Ka_Glied1=Ka) %>%
           select(-c("Gr":"Ka")) %>% select(-name)



Glied2<- pivotlonger_test %>%  filter(name=="Glied2") %>% 
                      pivot_wider(names_from = value, values_from = Gewicht, values_fill = 0) %>%
                      mutate(Gr_Glied2=Gr, KG_Glied2=KG, KM_Glied2=KM, WG_Glied2=WG, 
                             SM_Glied2=SM, WW_Glied2=WW, WR_Glied2=WR, Win_Glied2=Win,
                             SG_Glied2=SG, Ha_Glied2=Ha, ZR_Glied2=ZR, Ka_Glied2=Ka) %>%
                      select(-c("KM":"ZR")) %>% select(-name)

Glied3<- pivotlonger_test %>%  filter(name=="Glied3") %>% 
                      pivot_wider(names_from = value, values_from = Gewicht, values_fill = 0) %>%
                      mutate(Gr_Glied3=Gr, KG_Glied3=KG, KM_Glied3=KM, WG_Glied3=WG, 
                             SM_Glied3=SM, WW_Glied3=WW, WR_Glied3=WR, Win_Glied3=Win,
                             SG_Glied3=SG, Ha_Glied3=Ha, ZR_Glied3=ZR, Ka_Glied3=Ka) %>%
                      select(-c("Gr":"WR")) %>% select(-name)

Glied4<- pivotlonger_test %>%  filter(name=="Glied4") %>% 
                    pivot_wider(names_from = value, values_from = Gewicht, values_fill = 0) %>%
                    mutate(Gr_Glied4=Gr, KG_Glied4=KG, KM_Glied4=KM, WG_Glied4=WG, 
                           SM_Glied4=SM, WW_Glied4=WW, WR_Glied4=WR, Win_Glied4=Win,
                           SG_Glied4=SG, Ha_Glied4=Ha, ZR_Glied4=ZR, Ka_Glied4=Ka) %>%
                    select(-c("Gr":"-")) %>% select(-name)


Glied5<- pivotlonger_test %>%  filter(name=="Glied5") %>% 
                    pivot_wider(names_from = value, values_from = Gewicht, values_fill = 0) %>%
                    mutate(Gr_Glied5=Gr, KG_Glied5=KG, KM_Glied5=KM, WG_Glied5=WG, 
                           SM_Glied5=SM, WW_Glied5=WW, WR_Glied5=WR, Win_Glied5=Win,
                           SG_Glied5=SG, Ha_Glied5=Ha, ZR_Glied5=ZR, Ka_Glied5=Ka) %>%
                    select(-c("Gr":"-")) %>% select(-name)

rotation_matrix_full <- Glied1 %>% left_join(Glied2, by="rowid") %>% left_join(Glied3, by="rowid") %>%
                                   left_join(Glied4, by="rowid") %>% left_join(Glied5, by="rowid")
                                  

# creation of CR id out of row_id
rotation_matrix_full <- rotation_matrix_full %>% select(-c(Kombo.y,Kombo.y.y,Kombo.y,Kombo.x, Kombo, Kombo.x.x)) %>% 
                        mutate(CR_id=rowid) %>% select(-rowid) 
rotation_matrix_full <- rotation_matrix_full %>% relocate(CR_id)

dim(rotation_matrix_full)




#### Write CR_Glieder

CR_Glieder <- colnames(rotation_matrix_full) %>% as_tibble()
CR_Glieder <- CR_Glieder[-1,]
CR_Glieder <- CR_Glieder %>% rename(Cr_Glieder=value)


### Verknuepfe Info zwischen CR_id=row_id und Kommunen_information
head(rotation_matrix)
head(Rotations_felix)

kommune_CRid <- Rotations_felix %>% left_join(rotation_matrix, by="Kombo")
kommune_CRid <- kommune_CRid %>% select(rowid, LAU_ID)



## the parameter here will be linked to the set kommune in GAMS so I better check if they match
kommune_CRid
kommune

kommune_filter_out<-setdiff(kommune$AGS_0_2, kommune_CRid$LAU_ID)
# okay they do not match because of 8 kommunen... where are these eight
kommune_CRid %>% filter(LAU_ID=="8136040")
kommune %>% filter(AGS_0_2=="8136040")

## These 6 need to be filtered out in the files coming from my own GIS work
kommune_2 <- kommune %>% filter(!AGS_0_2 %in% kommune_filter_out)







## Verknüpfung mit crops
crops <- Cr_bawue_distinct %>% distinct(Glied1)
crop_CR_matrix <- rotation_matrix_full %>% slice(1:12)
crop_CR_matrix <- cbind(crops, crop_CR_matrix)
crop_CR_matrix <- crop_CR_matrix %>% select(-CR_id)
crop_CR_matrix


   
# crop_CR_matrix %>%mutate(Gr_Glied1 = if_else(Glied1=="Gr", 1,0))%>%
#         mutate(KG_Glied1 = if_else(Glied1 =="KG", 1,0))%>%
#         mutate(KM_GLied1 = if_else(Glied1=="KM", 1,0))%>%
#         mutate(WG_Glied1 = if_else(Glied1=="WG", 1,0))%>%
#         mutate(SM_Glied1 = if_else(Glied1=="SM", 1,0))%>%
#         mutate(WW_Glied1 = if_else(Glied1=="WW", 1,0))%>%
#         mutate(WR_Glied1 = if_else(Glied1=="WR", 1,0))%>%
#         mutate(Win_Glied1 = if_else(Glied1=="Win", 1,0))%>%
#         mutate(SG_Glied1 = if_else(Glied1=="SG", 1,0))%>%
#         mutate(HA_GLied1 = if_else(Glied1=="Ha", 1,0))%>%
#         mutate(ZR_Glied1 = if_else(Glied1=="ZR", 1,0))%>%
#         mutate(Ka_Glied1 = if_else(Glied1=="Ka", 1,0))%>%

# another way of doing this
# test %>%
#   mutate(Gr_Glied1 = case_when(Gr_Glied1 > 0 & Glied1 == "GR" ~ 1,
#                                TRUE ~ 0),
#          KG_Glied1 = case_when(KG_Glied1 > 0 & Glied1 == "KG" ~ 1,
#                                TRUE ~ 0),
#          KM_Glied1 = case_when(KM_GLied1 > 0 & Glied1 == "KM" ~ 1,
#                                TRUE ~ 0))

## the code here is standing on its own it is creating the whole dataframe
## valuable code wenn ich nur bezüge herstellen muss

values_list = c("Gr", "KG", "KM", "WG", "SM", "WW", "WR", "Win", "SG", "Ha", "ZR", "Ka")
for (value in values_list) {
  column_name1 = paste0(value, "_Glied1")
  column_name2 = paste0(value, "_Glied2")
  column_name3 = paste0(value, "_Glied3")
  column_name4 = paste0(value, "_Glied4")
  column_name5 = paste0(value, "_Glied5")
  crop_CR_matrix = crop_CR_matrix %>%
    mutate(!!column_name1 := if_else(Glied1 == value, 1, 0),
           !!column_name2 := if_else(Glied1 == value, 1, 0),
           !!column_name3 := if_else(Glied1 == value, 1, 0),
           !!column_name4 := if_else(Glied1 == value, 1, 0),
           !!column_name5 := if_else(Glied1 == value, 1, 0))
}

crop_CR_matrix <- crop_CR_matrix %>% -select(column_name1)

str(crop_CR_matrix)
write_xlsx(x=crop_CR_matrix, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/crop_CR_matrix.xlsx", col_names = TRUE)



# # Getting the matrix done
# values_list = c("Gr", "KG", "KM", "WG", "SM", "WW", "WR", "Win", "SG", "Ha", "ZR", "Ka")
#  for (value in values_list) {
#    column_name = paste0(value, "_Glied1")
#    crop_CR_matrix = crop_CR_matrix %>% mutate(!!column_name := if_else(Glied1 == value, 1, 0))
#  }  
#   
# values_list = c("Gr", "KG", "KM", "WG", "SM", "WW", "WR", "Win", "SG", "Ha", "ZR", "Ka")
#   for (value in values_list) {
#   column_name = paste0(value, "_Glied2")
#   crop_CR_matrix = crop_CR_matrix %>% mutate(!!column_name := if_else(Glied1 == value, 1, 0))
# }   
#   
# 
# values_list = c("Gr", "KG", "KM", "WG", "SM", "WW", "WR", "Win", "SG", "Ha", "ZR", "Ka")
# for (value in values_list) {
#   column_name = paste0(value, "_Glied3")
#   crop_CR_matrix = crop_CR_matrix %>% mutate(!!column_name := if_else(Glied1 == value, 1, 0))
# }  
# 
# 
# values_list = c("Gr", "KG", "KM", "WG", "SM", "WW", "WR", "Win", "SG", "Ha", "ZR", "Ka")
# for (value in values_list) {
#   column_name = paste0(value, "_Glied4")
#   crop_CR_matrix = crop_CR_matrix %>% mutate(!!column_name := if_else(Glied1 == value, 1, 0))
# }  
# 
# 
# values_list = c("Gr", "KG", "KM", "WG", "SM", "WW", "WR", "Win", "SG", "Ha", "ZR", "Ka")
# for (value in values_list) {
#   column_name = paste0(value, "_Glied5")
#   crop_CR_matrix = crop_CR_matrix %>% mutate(!!column_name := if_else(Glied1 == value, 1, 0))
# }  

# check it 
crop_CR_matrix










## here probably pushed into croprota


library(readxl)

CR_3 <-     read_excel("gamsdir/projdir/Brandenburg/Output/ResultsCF2.xlsx", 
                       sheet = "Crop_3", col_names = FALSE)


names(CR_3)[1] <- "NUTS_CODE"
names(CR_3)[2] <- "crop"
names(CR_3)[3] <- "crop2"
names(CR_3)[4] <- "crop3"
names(CR_3)[5] <- "share"


### clean crop rotations, i. e. remove monocultures



CR_3 <- CR_3 %>% select(-share) %>% mutate(CR_ID = row_number()) %>% 
  group_by(CR_ID) %>%
  gather(key="Crop", value = "NUTZG", 2:4) %>%
  mutate(Share=0.3333) %>% select(-Crop) 

CR_3 <- CR_3 %>% mutate(CR_ID = as.character(CR_ID))

# duplicate crop rotations with 10% flower strips

CR_3a <-   CR_3 %>% mutate(Komp="AF") %>%
  unite("CR_ID", Komp, CR_ID , sep="_", remove = T)  %>%
  mutate(Share = 0.3)

CR_3b <- CR_3 %>% mutate(Komp="AF") %>%
  unite("CR_ID", Komp, CR_ID , sep="_", remove = T)  %>%
  select(-NUTZG, -Share) %>% mutate(NUTZG="AF") %>%
  mutate(Share = 0.1)

CR_3 <- bind_rows(CR_3, CR_3a, CR_3b)


# vierfeldrige Fruchtfolgen auswerten

CR_4 <- read_excel("gamsdir/projdir/Brandenburg/Output/ResultsCF2.xlsx", 
                  sheet = "Crop_4", col_names = FALSE)

names(CR_4)[1] <- "NUTS_CODE"
names(CR_4)[2] <- "crop"
names(CR_4)[3] <- "crop2"
names(CR_4)[4] <- "crop3"
names(CR_4)[5] <- "crop4"
names(CR_4)[6] <- "share"


CR4 <- CR_4 %>% select(-share)  %>% mutate(CR_ID = row_number()) %>% 
  group_by(CR_ID) %>%
  gather(key="Crop", value = "NUTZG", 2:5) %>%
  mutate(Share=0.25) %>% select(-Crop)  


CR4_rem <- CR_4 %>% select(-share)  %>% mutate(CR_ID = row_number()) %>% 
  group_by(CR_ID) %>%
  gather(key="Crop", value = "NUTZG", 2:5) %>%
  mutate(Share=0.25) %>% select(-Crop)  %>% 
  group_by(CR_ID, NUTZG) %>% mutate(sum = sum(Share)) %>% 
  filter(! NUTZG %in% c("Gr", "KG")) %>% filter(sum>0.5) %>%
  ungroup() %>%select(CR_ID) %>% unique() %>% mutate(remove = "YES")


CR4 <-  CR4 %>% left_join(CR4_rem, by="CR_ID") %>% filter(is.na(remove)) %>% select(-remove)


# duplicate crop rotations with 10% flower strips

CR_4a <-   CR4 %>% mutate(Komp="AF") %>%
  unite("CR_ID", Komp, CR_ID , sep="_", remove = T)  %>%
  mutate(Share = 0.225)

CR_4b <- CR4 %>% mutate(Komp="AF") %>%
  unite("CR_ID", Komp, CR_ID , sep="_", remove = T)  %>%
  select(-NUTZG, -Share) %>% mutate(NUTZG="AF") %>%
  mutate(Share = 0.1) %>% unique()


CR4 <- CR4 %>% mutate(CR_ID = as.character(CR_ID))

CR4 <- bind_rows(CR4, CR_4a, CR_4b)

## Grünland-Fruchtfolge
CR_GR <-  CR4 %>% select(NUTS_CODE) %>% unique() %>% mutate(CR_ID="GR_1") %>%
  mutate(NUTZG="Gr") %>%
  mutate(Share = 1) 

Crop_rotations_BW <- bind_rows(CR_3, CR4, CR_GR) %>% mutate(n=1) %>% unique()

write_xlsx(x=Crop_rotations_BW, path = "D:/NBiomasseBW_2022/Crop_rotations_BW.xlsx", col_names = TRUE)







