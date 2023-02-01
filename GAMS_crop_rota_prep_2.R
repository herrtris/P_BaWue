### script online containing my workflow for GAMS, excluding AckerschlaegeBW
## Preparation file fuer CropRota - hier werden cropshares erzeugt, die als basis für crop rota dienen
## die anderen files dienen als input data fuer das P_modell


rm(list=ls())

# input <- choose.files()
# input


library(dplyr)
library(tidyr)
library(readxl)
library(writexl)
library(ggplot2)
library(tidyverse)

zentroid_p <- read.csv("C:\\Users\\User\\OneDrive - bwedu\\Dokumente\\Landwirtschaftliche Betriebslehre\\Projekt_P_Bawü\\Spatial_data\\Zentroide\\Ver_centroidga21_bawuegemeinde_bodke.csv", 
              sep=",", stringsAsFactors = T)

Flaechennutzung_Nutzcode <-read.csv("C:\\Users\\User\\OneDrive - bwedu\\Dokumente\\Landwirtschaftliche Betriebslehre\\Projekt_P_Bawü\\GAMS\\Modell-AG\\Modell-AG\\Flaechennutzung_Nutzcode.csv", sep=";") 
str(zentroid_p)

## GA-Datensatz vorbereiten
Flaechennutzung_Nutzcode <- Flaechennutzung_Nutzcode %>% select(1:5) %>% filter(!is.na(NUTZCODE))

#create an ID for test
zentroid_p$OID_<- 1:nrow(zentroid_p)

zentroid_p <- zentroid_p %>% mutate(Schlag_ID=OID_) %>% select(Schlag_ID, FLAECHE_HA, NUTZCODE, NUTS_2, AGS_0_2, NATBOD, NUTZUNG, P.2013...2, pH.2013.., K.2013...2)
zentroid_p<- zentroid_p %>% left_join(Flaechennutzung_Nutzcode, by="NUTZCODE") %>% filter(Flaechenart=="Ackerland")

zentroid_p %>% head(10)

zentroid_p <- zentroid_p %>%
  mutate(Bodenguete = ifelse(NATBOD %in% c("1,0"), "gering",
                      ifelse(NATBOD %in% c("1,5"), "gering",
                      ifelse(NATBOD %in% c( "2,0"), "mittel",
                      ifelse(NATBOD %in% c("2,5"), "mittel",
                      ifelse(NATBOD %in% c( "3,0"), "hoch",    
                      ifelse(NATBOD %in% c( "3,5"), "hoch",    
                      ifelse(NATBOD %in% c( "4,0"), "hoch","mittel")))))))) 

zentroid_p$Bodenguete <- factor(zentroid_p$Bodenguete, levels=c("gering", "mittel", "hoch")) 
zentroid_p %>% group_by(Bodenguete) %>% count()

# Unterteilung der Schlaggroesen 
zentroid_p$ha_Kat = cut(zentroid_p$FLAECHE_HA, c(0,2,5,10,1000), levels=c("1", "2", "5","10", "1000"))
zentroid_p %>% summarise(sum_ha=sum(FLAECHE_HA))
zentroid_p %>% select(Schlag_ID) %>% count()

# In total, 516905 Schlaege; GEsamtflaeche 739039.9 ha



######################################################################################################################################################
# Landuseplots

Landuse_plots_P <- zentroid_p  %>% group_by(NUTS_2, AGS_0_2, Bodenguete, P.2013...2) %>% 
                       summarise(sum_ha=sum(FLAECHE_HA))%>% ungroup() %>% mutate(PLOT_ID=row_number()) 

head(Landuse_plots_P)

Landuse_plots_P <- Landuse_plots_P[,c(6,1,3,2,4,5)]


#####################################################################################################################################################
### Landnutzung im Status Quo auf Kommunaler Ebene

Landuse_SQ_P <- zentroid_p %>% group_by(AGS_0_2, Kennung) %>% summarise(sum_ha=sum(FLAECHE_HA))
head(Landuse_SQ_P)

###################################################################################################################################################
### Kulturanteile auf Kreisebene für CropRota

Crop_share_BW_P <- zentroid_p %>% filter(Kennung !="B") %>% group_by(NUTS_2) %>% mutate(sum_Kreis=sum(FLAECHE_HA)) %>%
  group_by(NUTS_2, Kennung, sum_Kreis) %>% summarise(sum=sum(FLAECHE_HA)) %>% mutate(share=sum/sum_Kreis) %>%
  select(-sum_Kreis, -sum) %>% spread(key=Kennung, value=share)

head(zentroid_p)

#####################################################################################################################################################
### Writing out files 
### crop_share_BW_P - crop_share_bw_Zentroide.xlsx
### Landuse_SQ_P - Landuse_SQ_P
### Landuse_plots_P - Landuse_plots_P

write_xlsx(x=Crop_share_BW_P, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/crop_share_bw_Zentroide.xlsx", col_names = TRUE)
write_xlsx(x=Landuse_SQ_P, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/Landuse_SQ_P.xlsx", col_names = TRUE)
write_xlsx(x=Landuse_plots_P, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/Landuse_plots_P.xlsx", col_names = TRUE)


###################################################################################################################################################
###################################################################################################################################################
# Set preparation for GAMS
Landuse_plots_P %>% distinct(Bodenguete)
kreis<-zentroid_p %>% distinct(NUTS_2)
kommune<-zentroid_p %>% distinct(AGS_0_2)


#####################################################################################################################################################
### Writing out files 
### kreis - kreis_P
### kommune - kommune_p

write_xlsx(x=kreis, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/kreis_P.xlsx", col_names = FALSE)
write_xlsx(x=kommune, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/kommune_p.xlsx", col_names = FALSE)


############################################################################################################################################

#                                                     END - files creation from own QGIS work                                       ####

############################################################################################################################################
## Intermediate step harmonizing both files
## approach: take only the LAU_IDs that are within the kommune set
kommune %>% count()
Rotations_felix %>% distinct(LAU_ID) %>% count()

test1 <- kommune %>% mutate(LAU_ID=AGS_0_2) %>% right_join(Rotations_felix, by="LAU_ID")
test1 %>% distinct(LAU_ID) %>% count()



####################################################################################################


# READING IN CROP ROTATIONS FROM FELIX's FILE CROP ROTATION - LATER NEEDS TO BE OWN FILE
Rotations_felix <- read.csv("C:\\Users\\User\\OneDrive - bwedu\\Dokumente\\Landwirtschaftliche Betriebslehre\\Projekt_P_Bawü\\GAMS\\Crop_Rota_Felix\\Rotationen_Felix.csv", sep=";", stringsAsFactors = T)
str(Rotations_felix)

# How many unique Combos are there
Rotations_felix %>% distinct(Kombo) %>% count()
Cr_bawue_distinct <- Rotations_felix %>% distinct(Kombo, Gewicht, Glied1, Glied2, Glied3, Glied4, Glied5) %>% select(Kombo:Gewicht)
glimpse(Cr_bawue_distinct)

## There are 1193 distinct crop rotations in BAWUE


## looking at the variables
levels(Cr_bawue_distinct$Glied1)
levels(Cr_bawue_distinct$Glied2)
levels(Cr_bawue_distinct$Glied3)
levels(Cr_bawue_distinct$Glied4)
levels(Cr_bawue_distinct$Glied5)
summary(Cr_bawue_distinct$Gewicht)

## his file does not include "Rog" and "KL" - will be problem in GAMS sets

## Excluding crop rotations with only one crop
Cr_bawue_distinct <- Cr_bawue_distinct %>% filter(!Gewicht=="1")

### After exclusion: 1185 crop rotation options in BaWUE


## Creating a table containing the crop rotations
rotation_matrix <- Cr_bawue_distinct %>%  rowid_to_column() %>% mutate(CRid = rowid) %>% select(-rowid) %>% select(CRid, Kombo:Gewicht)
crops <- levels(rotation_matrix$Glied1)

## Preparation of crop rotation table, taking position of crop in crop rotation into account
glied1<-  rotation_matrix %>% select(CRid, Glied1, Gewicht) %>% pivot_wider(names_from = Glied1, values_from = Gewicht)
glied2 <- rotation_matrix %>% select(CRid, Glied2, Gewicht) %>% pivot_wider(names_from = Glied2, values_from = Gewicht)
glied3 <- rotation_matrix %>% select(CRid, Glied3, Gewicht) %>% pivot_wider(names_from = Glied3, values_from = Gewicht)
glied4 <- rotation_matrix %>% select(CRid, Glied4, Gewicht) %>% pivot_wider(names_from = Glied4, values_from = Gewicht)
glied5 <- rotation_matrix %>% select(CRid, Glied5, Gewicht) %>% pivot_wider(names_from = Glied5, values_from = Gewicht)

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


rotation_matrix_full <- Glied1 %>% left_join(Glied2, by="CRid") %>% left_join(Glied3, by="CRid") %>%
                        left_join(Glied4, by="CRid") %>% left_join(Glied5, by="CRid")

rotation_matrix_full <- rotation_matrix_full %>% select(-c(Kombo.y,Kombo.y.y,Kombo.y,Kombo.x, Kombo, Kombo.x.x))
                         
rm(glied1, Glied1, Glied2, Glied3, Glied4, Glied5)
rm(glied2, glied3, glied4 ,glied5)
rm(Crop_share_BW_P)
rm(Crop_share_BW_test)
rm(pivotlonger_test)

##################################################################################################################################################
##################################################################################################################################################

#### Create CR_Glieder - loaded as a set
CR_Glieder <- colnames(rotation_matrix_full) %>% as_tibble()
CR_Glieder <- CR_Glieder[-1,]
CR_Glieder <- CR_Glieder %>% rename(Cr_Glieder=value)



## Verknüpfung mit crops
crops <- Cr_bawue_distinct %>% distinct(Glied1)
crop_CR_matrix <- rotation_matrix_full %>% slice(1:12)
crop_CR_matrix <- cbind(crops, crop_CR_matrix)
crop_CR_matrix <- crop_CR_matrix %>% select(-CRid)


## Verknüpfung mit crops
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


### Verknuepfe Info zwischen CR_id=row_id und Kommunen_information
head(rotation_matrix)
head(Rotations_felix)


### Achtung ich hatte bei rotations_felix filter angelegt... und zwar für eingliedrige Fruchtfolgen die wurden raussortiert
### simply switch left_join around
#kommune_CRid <- Rotations_felix %>% left_join(rotation_matrix, by="Kombo")
kommune_CRid <- rotation_matrix %>% left_join(Rotations_felix, by="Kombo")
kommune_CRid <- kommune_CRid %>% select(CRid, LAU_ID)
kommune_CRid





##################################################################################################################################################
##################################################################################################################################################
# writing out files related to crop rotation

# Crop_glieder_set
write_xlsx(x=CR_Glieder, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/CR_Glieder.xlsx", col_names = FALSE)

#crop_CR_matrix, connect crop rotations with crop
write_xlsx(x=crop_CR_matrix_full, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/crop_CR_matrix.xlsx", col_names = FALSE)


#### kommune_CR_ID, welche croprot kommt in welcher kommune vor?
write_xlsx(x=kommune_CRid, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/kommune_CRid.xlsx", col_names = TRUE)



#######################################################################################################################################################
###################################################################################################################################################
##### Harmonizing both files

# crop rota files felix
CR_Glieder                                      #|should be ready |contains rotation glied
crop_CR_matrix                                  #|should be ready |contains crop/rotation_glied
rotation_matrix_full
kommune_CRid


# files from own production
kommune
kreis
Landuse_plots_P
Landuse_SQ_P












kommune <- kommune %>% filter(!AGS_0_2 %in% kommune_filter_out)
# code for later harmonization with Felix crop rotations, applied to landuse_plots_p and to landuse_SQ_P
filter(!Kennung=="B" | !Kennung=="Rog" | !Kennung=="KL") %>%
  filter(!AGS_0_2 %in% kommune_filter_out)
