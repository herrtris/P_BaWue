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

Landuse_plots_P <- zentroid_p  %>% group_by(NUTS_2, Bodenguete) %>% 
                       summarise(sum_ha=sum(FLAECHE_HA))%>% ungroup() %>% mutate(PLOT_ID=row_number()) 

# if I want to add P fer plot the group_by from above needs to include P
Landuse_plots_P_added <- zentroid_p  %>% group_by(NUTS_2, Bodenguete, P.2013...2) %>% 
  summarise(sum_ha=sum(FLAECHE_HA))%>% ungroup() %>% mutate(PLOT_ID=row_number()) 



head(Landuse_plots_P)
summary(Landuse_plots_P)

Landuse_plots_P <- Landuse_plots_P[,c(4,1,2,3)]
check<-Landuse_plots_P %>% group_by(NUTS_2)%>% count() #De125 hat nur 2 observations
Landuse_plots_P %>% filter(NUTS_2=="DE125")


write_xlsx(x=Landuse_plots_P, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/Landuse_plots_P.xlsx", col_names = TRUE)




#####################################################################################################################################################
### Landnutzung im Status Quo auf Kommunaler Ebene

Landuse_SQ_P <- zentroid_p %>% group_by(NUTS_2, Kennung) %>% summarise(sum_ha=sum(FLAECHE_HA))
head(Landuse_SQ_P)
View(Landuse_SQ_P)
Landuse_SQ_P %>% group_by(Kennung)%>%count(Kennung)

Landuse_SQ_P<- Landuse_SQ_P %>% filter(!Kennung=="B")


Landuse_SQ_P %>% ungroup%>% summarize(sum(sum_ha))
# in total for all bawue gibt es 706,121 ha - ohne Brache


###################################################################################################################################################
### Kulturanteile auf Kreisebene für CropRota

Crop_share_BW_P <- zentroid_p %>% filter(Kennung !="B") %>% group_by(NUTS_2) %>% mutate(sum_Kreis=sum(FLAECHE_HA)) %>%
  group_by(NUTS_2, Kennung, sum_Kreis) %>% summarise(sum=sum(FLAECHE_HA)) %>% mutate(share=sum/sum_Kreis) %>%
  select(-sum_Kreis, -sum) %>% spread(key=Kennung, value=share)

head(zentroid_p)


## wie hoch ist der MJNEL Bedarf im status quo?
head(zentroid_p)



futterbau_bedarf <- 
zentroid_p %>%
  mutate(Bodenguete = ifelse(NATBOD %in% c("1,0"), "niedrig",
                      ifelse(NATBOD %in% c("1,5"), "niedrig",
                      ifelse(NATBOD %in% c( "2,0"), "mittel",
                      ifelse(NATBOD %in% c("2,5"), "mittel",
                      ifelse(NATBOD %in% c( "3,0"), "hoch",    
                      ifelse(NATBOD %in% c( "3,5"), "hoch",    
                      ifelse(NATBOD %in% c( "4,0"), "hoch","mittel")))))))) %>% select(NUTS_2, Schlag_ID, FLAECHE_HA, Kennung,Bodenguete) 


#futterbau_bedarf<-  futterbau_bedarf  %>% group_by(NUTS_2, Bodenguete) %>% 
#                    summarise(sum_ha=sum(FLAECHE_HA))%>% ungroup() %>% mutate(PLOT_ID=row_number()) 

#futterbau_bedarf<- futterbau_bedarf[,c(4,1,2,3)]



#futterbau_bedarf <-   futterbau_bedarf %>% filter(Kennung %in% c("SM", "KG", "Gr"))
head(futterbau_bedarf)

### Wieviel MJNEL wird produziert je Flaeche? How are plot_id und sclag_id behaving towards each other

yields_soilqual_Fut %>% rename(Kennung=`crop abrre`, Bodenguete="Intensitaet")


crops<-crop_vector %>% as_tibble()%>% rename(crop="value")
kreis
yields_soilqual_Fut
yields_soilqual_Ack

one <- yields_soilqual_Ack %>% select(`crop abrre`,Intensitaet)
two <- yields_soilqual_Fut %>% select(`crop abrre`, Intensitaet)

Energie_pro_ha <- rbind(one, two)

new_df <- expand.grid(`crop abrre`=unique(Energie_pro_ha$`crop abrre`),
                      Intensitaet= unique(Energie_pro_ha$Intensitaet),
                      county=kreis$NUTS_2) %>% as_tibble()


Energie_proha<- left_join(new_df, yields_soilqual_Fut, by=c("crop abrre", "Intensitaet"))
Energie_proha <- Energie_proha %>% replace_na(list(`Ertrag (10 MJ NEL/ha)`=0))

### write out Energiebedarf je crop, intensity and county
write_xlsx(x=Energie_proha, path = "C:/Users/Tristan Herrmann/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/Energie_proha.xlsx", col_names = TRUE)

head(futterbau_bedarf)


futterbau_bedarf_joined<-futterbau_bedarf %>% left_join(yields_soilqual_Fut %>% rename(Kennung=`crop abrre`, Bodenguete="Intensitaet"), by=c("Kennung", "Bodenguete"))
dim(futterbau_bedarf_joined)
head(futterbau_bedarf_joined)

factor(futterbau_bedarf$Bodenguete)
factor(yields_soilqual_Fut$Intensitaet)


#futterbau_bedarf_joined<-futterbau_bedarf %>% left_join(yields_soilqual_Fut %>% rename(Kennung=`crop abrre`, Bodenguete="Intensitaet"), by=c("Kennung", "Bodenguete"))
dim(futterbau_bedarf_joined)
dim(futterbau_bedarf)
summary(futterbau_bedarf_joined)


futterbau_bedarf_joined <-futterbau_bedarf_joined %>% replace_na(list(`Ertrag (10 MJ NEL/ha)`=0)) 


head(futterbau_bedarf_joined)

## mutate futterbau Energiebedarf je schlag
futterbau_bedarf_joined<- futterbau_bedarf_joined %>% mutate(Energiebedarf_Schlag=FLAECHE_HA*`Ertrag (10 MJ NEL/ha)`)
head(futterbau_bedarf_joined)
futterbau_bedarf_joined %>% filter(is.na(Energiebedarf_Schlag))


### Fuege den Deckungsbeitrag je ha pro Kultur hinzu
### Deckungsbeiträge kommen aus extracted calculation data, muessen zur not bei anderen Runs angepasst werden

# futterbau_bedarf_joined %>% filter(Kennung=="KG")

# Deckungsbeiträge eingelsen, nicht mehr sinnvoll
# futterbau_bedarf_joined <-futterbau_bedarf_joined %>% mutate(DB_ha=case_when(
#                                     Kennung=='KG' & Bodenguete=='niedrig' ~ c(-123),
#                                     Kennung=='KG' & Bodenguete=='mittel'  ~ c(-233),
#                                     Kennung=='KG' & Bodenguete=='hoch'    ~ c(-343),
#                                     Kennung=='SM' & Bodenguete=='niedrig' ~ c(-223),
#                                     Kennung=='SM' & Bodenguete=='mittel'  ~ c(-114),
#                                     Kennung=='SM' & Bodenguete=='hoch'    ~ c(-20),
#                                     Kennung=='Gr' & Bodenguete=='niedrig' ~ c(-8),
#                                     Kennung=='Gr' & Bodenguete=='mittel'  ~ c(-32),
#                                     Kennung=='Gr' & Bodenguete=='hoch'    ~ c(-59),
# ))



futterbau_bedarf_joined   <-futterbau_bedarf_joined %>% filter(!Kennung=="B")
# head(futterbau_bedarf_joined)
# summary(futterbau_bedarf_joined)
# 
# 
# Energiebedarf <- futterbau_bedarf_joined %>% select(NUTS_2, Bodenguete, Kennung, `Ertrag (10 MJ NEL/ha)`)%>%
#                                              group_by(NUTS_2, Bodenguete, Kennung)
# 
# summary(Energiebedarf)
# head(Energiebedarf)
# Energiebedarf<-Energiebedarf %>% replace_na(list(`Ertrag (10 MJ NEL/ha)`=0)) 

# wie kann ich die plot_id adden? naja plot_id ist einfach die stufung, also jeder kreis 
# hat in summe 3 plot_id also
# hab hier 2701 plot_ids wie kommt die Zahl zu Stande?
# FEHLER in landuse_plots ist nochmal unterteilt auf die Kommune, bruach ich jedoch nicht
#           bei der momentanen Modellspezifaikation - now corrected!

# add plot_id
Energiebedarf






# hier gabs nen coolen befehl von linkedin ne varibale zu nem factor zu machen
# ## check it out
# levels(futterbau_bedarf_joined$Kennung)
# 
# # bauen von futterbau_ha_energie(crop, soil_qual, counties)
# futterbau_bedarf_joined %>% select(NUTS_2, Bodenguete, Kennung, `Ertrag (10 MJ NEL/ha)`) %>%
#                             group_by(NUTS_2, Bodenguete, Kennung)
# 
# 
# 
# # Deckungsbeitrag je schlag
# futterbau_bedarf_joined  <-  futterbau_bedarf_joined %>% mutate(DB_Schlag=DB_ha*FLAECHE_HA)
# 
# # Gesamt negativer DB nötig für die Tierhaltung
# futterbau_bedarf_DB <-  futterbau_bedarf_joined %>% group_by(NUTS_2) %>% summarize(DB_tierhaltung_Kreis_MJNEL= sum(DB_Schlag))




# wie ist die VErteilung von Futterbau auf Bodenguete?
futterbau_bedarf_kreis <- futterbau_bedarf_joined %>% select(NUTS_2, Energiebedarf_Schlag) %>% group_by(NUTS_2) %>% summarize(Energiebedarf_kreis_MJNEL=sum(Energiebedarf_Schlag))
head(futterbau_bedarf_kreis)  
  
write_xlsx(x=futterbau_bedarf_kreis, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/futterbau_bedarf_kreis.xlsx", col_names = TRUE)
write_xlsx(x=futterbau_bedarf_DB, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/futterbau_bedarf_DB.xlsx", col_names = TRUE)



#####################################################################################################################################################
### Writing out files 
### crop_share_BW_P - crop_share_bw_Zentroide.xlsx
### Landuse_SQ_P - Landuse_SQ_P
### Landuse_plots_P - Landuse_plots_P

write_xlsx(x=Crop_share_BW_P, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/crop_share_bw_Zentroide.xlsx", col_names = TRUE)
write_xlsx(x=Landuse_SQ_P, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/Landuse_SQ_P.xlsx", col_names = TRUE)
write_xlsx(x=Landuse_plots_P, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/Landuse_plots_P.xlsx", col_names = TRUE)

## write out county_Flaeche(counties) #calculated in GAMS
## Die Flaechenrestriktion auf Kriesebene
county_ha<-Landuse_plots_P %>%group_by(NUTS_2) %>%summarize(sum_ha)
county_ha<-county_ha %>% group_by(NUTS_2) %>% summarize(sum=sum(sum_ha))

# Restriktion der Kulturen. Summe der Kulturen muss die gleiche beliben
Landuse_SQ_P 





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
# READING IN CROP ROTATIONS, developed out of crop rota.gms stored under C: Gams und dem Rscript CROPROTGDX_2.R 

kommune %>% count()

## 01.02.23 reading in own crop rotation file
Rotations_tristan <-read.csv("C:\\Users\\User\\OneDrive - bwedu\\Dokumente\\Landwirtschaftliche Betriebslehre\\Projekt_P_Bawü\\P_BaWue\\Crop_Rota_Output\\Crop_rotations_Tristan.csv", sep=";", stringsAsFactors = T)
Rotations_tristan %>% distinct(NUTS_2) %>% count()
str(Rotations_tristan)

# How many unique Combos are there
Rotations_tristan %>% distinct(Kombo) %>% count()
Cr_bawue_distinct <- Rotations_tristan %>% distinct(Kombo, Gewicht, Glied1, Glied2, Glied3, Glied4, Glied5) %>% select(Kombo:Gewicht)
glimpse(Cr_bawue_distinct)

## There are 169 distinct crop rotations in BAWUE at the NUTS_2 level


## looking at the variables
levels(Cr_bawue_distinct$Glied1)
levels(Cr_bawue_distinct$Glied2)
levels(Cr_bawue_distinct$Glied3)
levels(Cr_bawue_distinct$Glied4)

# in der fünfgliederigen Fruchtfolge kommt kein ZR vor
levels(Cr_bawue_distinct$Glied5)
summary(Cr_bawue_distinct$Gewicht)


# ## Excluding crop rotations with only one crop
# Cr_bawue_distinct <- Cr_bawue_distinct %>% filter(!Gewicht=="1")

### After exclusion: 169 crop rotation options in BaWUE


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
                 SG_Glied1=SG, Ha_Glied1=Ha, ZR_Glied1=ZR, Ka_Glied1=Ka,
                 Rog_Glied1=Rog, KL_Glied1=KL) %>%
          select(-c("KM":"KG")) %>% select(-name)


  Glied2<- pivotlonger_test %>%  filter(name=="Glied2") %>% 
           pivot_wider(names_from = value, values_from = Gewicht, values_fill = 0) %>%
           mutate(Gr_Glied2=Gr, KG_Glied2=KG, KM_Glied2=KM, WG_Glied2=WG, 
                 SM_Glied2=SM, WW_Glied2=WW, WR_Glied2=WR, Win_Glied2=Win,
                 SG_Glied2=SG, Ha_Glied2=Ha, ZR_Glied2=ZR, Ka_Glied2=Ka,
                 Rog_Glied2=Rog, KL_Glied2=KL) %>%
           select(-c("-":"Gr")) %>% select(-name)

  Glied3<- pivotlonger_test %>%  filter(name=="Glied3") %>% 
           pivot_wider(names_from = value, values_from = Gewicht, values_fill = 0) %>%
           mutate(Gr_Glied3=Gr, KG_Glied3=KG, KM_Glied3=KM, WG_Glied3=WG, 
                 SM_Glied3=SM, WW_Glied3=WW, WR_Glied3=WR, Win_Glied3=Win,
                 SG_Glied3=SG, Ha_Glied3=Ha, ZR_Glied3=ZR, Ka_Glied3=Ka,
                 Rog_Glied3=Rog, KL_Glied3=KL) %>%
           select(-c("-":"KG")) %>% select(-name)

  Glied4<- pivotlonger_test %>%  filter(name=="Glied4") %>% 
           pivot_wider(names_from = value, values_from = Gewicht, values_fill = 0) %>%
           mutate(Gr_Glied4=Gr, KG_Glied4=KG, KM_Glied4=KM, WG_Glied4=WG, 
                 SM_Glied4=SM, WW_Glied4=WW, WR_Glied4=WR, Win_Glied4=Win,
                 SG_Glied4=SG, Ha_Glied4=Ha, ZR_Glied4=ZR, Ka_Glied4=Ka,
                 Rog_Glied4=Rog, KL_Glied4=KL) %>%
           select(-c("Gr":"-")) %>% select(-name)


  Glied5<- pivotlonger_test %>%  filter(name=="Glied5") %>% 
           pivot_wider(names_from = value, values_from = Gewicht, values_fill = 0) %>% mutate(ZR=0)%>%
           mutate(Gr_Glied5=Gr, KG_Glied5=KG, KM_Glied5=KM, WG_Glied5=WG, 
                 SM_Glied5=SM, WW_Glied5=WW, WR_Glied5=WR, Win_Glied5=Win,
                 SG_Glied5=SG, Ha_Glied5=Ha, ZR_Glied5=ZR, Ka_Glied5=Ka,
                 Rog_Glied5=Rog, KL_Glied5=KL) %>%
           select(-c("-":"ZR")) %>% select(-name)


rotation_matrix_full <- Glied1 %>% left_join(Glied2, by="CRid") %>% left_join(Glied3, by="CRid") %>%
                        left_join(Glied4, by="CRid") %>% left_join(Glied5, by="CRid")

rotation_matrix_full <- rotation_matrix_full %>% select(-c(Kombo.y,Kombo.y.y,Kombo.y,Kombo.x, Kombo, Kombo.x.x))
                         
rm(glied1, Glied1, Glied2, Glied3, Glied4, Glied5)
rm(glied2, glied3, glied4 ,glied5)
#rm(Crop_share_BW_P)

rm(pivotlonger_test)

##################################################################################################################################################
##################################################################################################################################################

#### Create CR_Glieder - loaded as a set
CR_Glieder <- colnames(rotation_matrix_full) %>% as_tibble()
CR_Glieder <- CR_Glieder[-1,]
CR_Glieder <- CR_Glieder %>% rename(Cr_Glieder=value)



## Verknüpfung mit crops
crops <- Cr_bawue_distinct %>% distinct(Glied1)
crop_CR_matrix <- rotation_matrix_full %>% slice(1:14)
crop_CR_matrix <- cbind(crops, crop_CR_matrix)
crop_CR_matrix <- crop_CR_matrix %>% select(-CRid)


## Verknüpfung mit crops
values_list = c("Gr", "KG", "KM", "WG", "SM", "WW", "WR", "Win", "SG", "Ha", "ZR", "Ka","KL", "Rog")
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


crop_CR_matrix<-crop_CR_matrix %>% rename(crop=Glied1)

#check
CR_Glieder
CR_Glieder_cropmatrix <- colnames(crop_CR_matrix) %>% as_tibble()
CR_Glieder_cropmatrix<-CR_Glieder_cropmatrix[-1,]

setdiff(CR_Glieder$Cr_Glieder, CR_Glieder_cropmatrix$value)
setdiff(CR_Glieder_cropmatrix$value, CR_Glieder$Cr_Glieder)

crop<- crop_CR_matrix[,1]
crop

crop_gams<-c("Gr", "Ha", "Ka", "KG", "KM", "SG", "SM", "WG", "Win", "WR", "WW", "ZR", "Rog", "KL")

setdiff(crop, crop_gams)
setdiff(crop_gams, crop)

# es liegt nicht an der Belegung der sets, sets sind identisch

### Verknuepfe Info zwischen CR_id=row_id und Kommunen_information
head(rotation_matrix)

kommune_CRid <- rotation_matrix %>% left_join(Rotations_tristan, by="Kombo")
kommune_CRid <- kommune_CRid %>% select(CRid, LAU_ID)
kommune_CRid

kommune_CRid<-kommune_CRid %>% mutate(AGS_0_2=LAU_ID)
kommune_CRid<- kommune_CRid %>% select(-LAU_ID) %>% mutate(value=1)
head(kommune_CRid)

## difference kommunen vektor

kommune_CRid <-kommune_CRid %>%pivot_wider(names_from = AGS_0_2, values_from = value, values_fill = 0)
kommune_CRid <- kommune_CRid %>% mutate(CR_id=CRid) %>% select(-CRid) 
kommune_CRid <-kommune_CRid[,c(45, 1:44)]

summary(test)


##################################################################################################################################################
##################################################################################################################################################
# writing out files related to crop rotation

# Crop_glieder_set
write_xlsx(x=CR_Glieder, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/CR_Glieder.xlsx", col_names = FALSE)

#crop_CR_matrix, connect crop rotations with crop
write_xlsx(x=crop_CR_matrix, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/crop_CR_matrix.xlsx", col_names = TRUE)


#### kommune_CR_ID, welche croprot kommt in welcher kommune vor?
write_xlsx(x=kommune_CRid, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/kommune_CRid.xlsx", col_names = TRUE)

## rotation_matrix_full
write_xlsx(x=rotation_matrix_full, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/rotation_matrix_full.xlsx", col_names = TRUE)


#######################################################################################################################################################
###################################################################################################################################################
#####

# crop rota files tristan
CR_Glieder                                      #|should be ready |contains rotation glied
crop_CR_matrix                                  #|should be ready |contains crop/rotation_glied
rotation_matrix_full
kommune_CRid


### CR_id must be linked with crops, where is the CR_id coming from?
rotation_matrix
summary(rotation_matrix)
head(rotation_matrix)

# # explanation of the for loop
# # For example, if crop is equal to "KM", then !!crop will be evaluated to "KM". Without the !! operator, 
# # rename(crop := temp) would treat crop as a string, and would create a new column named "crop" instead of a column named "KM".
# 
# The := operator is used to assign a value to a variable. In this case, it's used to assign the values of the temp column to the column named crop (or "KM" or "WW").
# 
# So, rename(!!crop := temp) dynamically evaluates the value of crop, and then creates a new column with that name and assigns the values of the temp column to it.
# This way, the loop can create columns with different names for each iteration.

rotation_matrix <- Cr_bawue_distinct %>%  rowid_to_column() %>% mutate(CRid = rowid) %>% select(-rowid) %>% select(CRid, Kombo:Gewicht)

crops <- c("Gr", "Ha", "Ka", "KG", "KL", "KM", "Rog", "SG", "SM", "WG", "Win", "WR", "WW", "ZR")
result <- rotation_matrix
for (crop in crops) {
  temp_result <- result %>%
    mutate(temp = ifelse(Glied1 %in% c(crop), 1, 0) |
             ifelse(Glied2 %in% c(crop), 1, 0) |
             ifelse(Glied3 %in% c(crop), 1, 0) |
             ifelse(Glied4 %in% c(crop), 1, 0) |
             ifelse(Glied5 %in% c(crop), 1, 0)) %>%
    mutate(temp = ifelse(temp %in% TRUE, 1, 0)) %>%
    rename(!!crop := temp)
  print(temp_result)
  result <- temp_result
}
rotation_matrix <- result
rotation_matrix<- rotation_matrix %>% as_tibble()

CR_id_crop <- rotation_matrix %>% select(CRid, Gr:ZR)

##############################################################################################################################################################################
#### write CR_id_crop
write_xlsx(x=CR_id_crop, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/CR_id_crop.xlsx", col_names = T)

###############################################################################################################################################################################







# files from own production
kommune
kreis
Landuse_plots_P
Landuse_SQ_P





####################################################################################################################################
#####################################################################################################################################

### Daten aus den Kalkulationsdaten des LEL
### Vorbereitung für GAMS input

# kulturspezifische Ertragsdaten und Preisdaten

# input <- choose.files()
#  input

getwd()
setwd("C:\\Users\\User\\OneDrive - bwedu\\Dokumente\\Landwirtschaftliche Betriebslehre\\Projekt_P_Bawü\\GAMS_P\\Input_data\\Kalkulationsdaten")

# reading data in
######### !!!! Excel file ist gelinkt zu Marktfruechte und Futterbau Klakulationstabellen, aenderungen in diesen Tabellen veraendern die input daten
######### !!!! Betrifft vor allem die Kostenseite, zum Beispiel Aenderung des Duengerpreises und des Dieselpreises auf 2022 Niveau
#########      Sollte Ertragsdaten nicht betreffen
Ackerkulturen <- read_excel("extracted_calculation_data.xlsx", sheet="Ackerkulturen")
Futterbau <- read_excel("extracted_calculation_data.xlsx", sheet="Futterbau")

excel_sheets("extracted_calculation_data.xlsx")


glimpse(Ackerkulturen)
glimpse(Futterbau)

yields_soilqual_Ack <- Ackerkulturen %>% select(`crop abrre`, Intensitaet, `Ertrag dt/ha`)
yields_soilqual_Ack
yields_soilqual_Fut <- Futterbau %>% select(`crop abrre`, Intensitaet, `Ertrag (10 MJ NEL/ha)`)
yields_soilqual_Fut


price_crop_Ack <- Ackerkulturen %>% select(`crop abrre`, `Price €/dt`) %>% distinct(`crop abrre`, `Price €/dt`)
price_crop_Ack


price_crop_Fut   <- Futterbau %>% select(`crop abrre`, `Preis (€/10MJ NEL)`) 
price_crop_Fut<- unique(price_crop_Fut[,c("crop abrre", "Preis (€/10MJ NEL)")])
price_crop_Fut


## writing out parameters yield and prices
write_xlsx(x=yields_soilqual_Ack, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/yields_soilqual_Ack.xlsx", col_names = T)
write_xlsx(x=yields_soilqual_Fut, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/yields_soilqual_Fut.xlsx", col_names = T)
write_xlsx(x=price_crop_Ack, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/price_crop_Ack.xlsx", col_names = T)
write_xlsx(x=price_crop_Fut, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/price_crop_Fut.xlsx", col_names = T)


##############################################################################################################################################################################
###############################################################################################################################################################################
# Variable kosten !Anpassung der Dieselpreise im file
# Dieselpreise auf 2021 Niveau

variable_cost_ackerbau <- Ackerkulturen %>% select(`crop abrre`,Intensitaet,`V. Kosten (€/ha)`)
variable_cost_futterbau <- Futterbau %>% select(`crop abrre`,Intensitaet,`V. Kosten (€/ha)`)


## writing out variable cost parameters
write_xlsx(x=variable_cost_ackerbau, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/variable_cost_ackerbau.xlsx", col_names = T)
write_xlsx(x=variable_cost_futterbau, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/variable_cost_futterbau.xlsx", col_names = T)

#####################################################################################################################################################################################

## Creating county_BG; verbindet intensity und soil_qual im GAMS script
kreis
Landuse_plots_P

county_BG <- Landuse_plots_P %>% select(NUTS_2, Bodenguete) %>%distinct(NUTS_2, Bodenguete) %>% mutate(value=1) %>% pivot_wider(names_from = Bodenguete, values_from = value, values_fill = 0)
county_BG  

write_xlsx(x=county_BG , path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/county_BG .xlsx", col_names = T)

setdiff(kreis$NUTS_2, county_BG$NUTS_2)
setdiff(county_BG$NUTS_2, kreis$NUTS_2)

########################################################################################################################################################################################

# Bearbeitung der Tierzahlen 
Tierzahlen <- read_excel("Tierzahl_neu.xlsx", sheet="Tierzahlen")
summary(Tierzahlen)


Tierzahlen <- Tierzahlen %>% select(`NUTS-Code`, Tierart, Tierzahl)
Tierzahlen <- Tierzahlen %>% mutate_at(vars(`NUTS-Code`,Tierart), factor)
levels(Tierzahlen$Tierart)
levels(Tierzahlen$`NUTS-Code`)


## Umrechnung Tierzahl in Großvieheinheiten
Tierzahlen <-
Tierzahlen %>% mutate(GV_Einheiten=case_when(
                                      Tierart=='Gef'   ~ Tierzahl*0.00377,  #Annahme Durchschnitt
                                      Tierart=='JR'    ~ Tierzahl*0.59,     #Annahme Durschnitt
                                      Tierart=='MV'    ~ Tierzahl*1.2,      #Annahme Milchkuh
                                      Tierart=='Ri'    ~ Tierzahl*0.678,    #Annahme Durchscnitt
                                      Tierart=='Schw'  ~ Tierzahl*0.14,     #Annahme Mastschweine 30-115Kg
                                      Tierart=='Sf'    ~ Tierzahl*0.1014,   #Annahme Durchschnitt
                                      Tierart=='Zi'    ~ Tierzahl*0.0678,   #Annahme Durchscnitt
                                       ))

## Umrechnung GV Einheit in Dungeinheiten

Tierzahlen <-
  Tierzahlen %>% mutate(Dungeinheiten=case_when(
    Tierart=='Gef'   ~ Tierzahl*0.0075,  #Annahme Dungeinheiten je stück Durchschnitt
    Tierart=='JR'    ~ GV_Einheiten*0.667,     #Annahme GV Durschnitt
    Tierart=='MV'    ~ GV_Einheiten*1,      #Annahme GV
    Tierart=='Ri'    ~ GV_Einheiten*0.667,    #Annahme GV Durchscnitt
    Tierart=='Schw'  ~ GV_Einheiten*0.2,     #Annahme GV Durchschnitt
    Tierart=='Sf'    ~ GV_Einheiten*0.1,   #Annahme GV
    Tierart=='Zi'    ~ GV_Einheiten*0.1,   #Annahme GV
  ))

boxplot(Tierzahlen$Dungeinheiten)
summary(Tierzahlen)

# laut https://www.landwirtschaft.sachsen.de/dungeinheitenschluessel-15620.html
## entspricht eine Dungeinheit dem Anfall an tierischen Exkrementen von 80kg N bzw. 70 kg P205

Tierzahlen_dueng<-Tierzahlen %>% mutate(N_org=Dungeinheiten*80, P205_org_kg=Dungeinheiten*70)%>% mutate(P_org_kg=0.4364*P205_org_kg)
summary(Tierzahlen_dueng)


## P_org je Kreis und P_verbrauch
Tierzahlen_dueng %>% group_by(`NUTS-Code`) %>% summarise(sum_P=sum(P_org_kg))
Tierzahlen_dueng %>% summarise(P_org_Bawue=sum(P_org_kg/1000))



## P Verbrauch je crop in abhängigkeit von soil_qual und intensitaet
P1 <- Ackerkulturen %>% select(`crop abrre`, Intensitaet, "P (kg/ha)")
P2 <- Futterbau %>% select(`crop abrre`, Intensitaet, "P (kg/ha)")

P_Verbrauch <- rbind(P1, P2)
P_Verbrauch

# expan.grid für jeden Kreis


new_df <- expand.grid(`crop abrre`=unique(P_Verbrauch$`crop abrre`),
                      Intensitaet= unique(P_Verbrauch$Intensitaet),
                      county=kreis$NUTS_2) %>% as_tibble()

P_Verbrauch<- left_join(new_df, P_Verbrauch, by=c("crop abrre", "Intensitaet"))

#P_Verbrauch<- P_Verbrauch[,c(4,1,2,3)]

summary(P_Verbrauch)


write_xlsx(x=P_Verbrauch , path = "C:/Users/Tristan Herrmann/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/P_Verbrauch.xlsx", col_names = T)


# where is the P_soil class at the moment
Landuse_SQ_P
Landuse_plots_P
summary(Landuse_plots_P$P.2013...2)

##########################################################################################################################################################################################
# Restriction for silage maize and 
sm_county<- zentroid_p %>% filter(Kennung=="SM")
head(sm_county)

sm_county<-sm_county %>% select(counties="NUTS_2", crop="Kennung", Bodenguete, FLAECHE_HA)
#sm_county <- sm_county %>% group_by(NUTS_2, Kennung)
sm_county <-sm_county %>% mutate(management= case_when(
                                   Bodenguete=='gering' ~c("niedrig"),
                                   Bodenguete=='mittel' ~c("mittel"),
                                   Bodenguete=='hoch' ~c("hoch")))

sm_county <- sm_county %>% select(counties, crop, FLAECHE_HA) %>% group_by(counties,crop) %>% summarize(sum_ha_sm=sum(FLAECHE_HA))


# Restriction for potatoes
Ka_county<- zentroid_p %>% filter(Kennung=="Ka")
head(Ka_county)

Ka_county<-Ka_county %>% select(counties="NUTS_2", crop="Kennung", Bodenguete, FLAECHE_HA)
#Ka_county <- Ka_county %>% group_by(NUTS_2, Kennung)
Ka_county <-Ka_county %>% mutate(management= case_when(
  Bodenguete=='gering' ~c("niedrig"),
  Bodenguete=='mittel' ~c("mittel"),
  Bodenguete=='hoch' ~c("hoch")))

Ka_county <- Ka_county %>% select(counties, crop, FLAECHE_HA) %>% group_by(counties,crop) %>% summarize(sum_ha_sm=sum(FLAECHE_HA))
Ka_county

# Beschraenkung KArtoffel Silomais auf die Ganze region Bawü
Ka_county %>%ungroup() %>%summarize(bawue_ka_ha=sum(sum_ha_sm))  # in total 5396ha Kartoffel in Bawü

sm_county %>%ungroup() %>%summarize(bawue_sm_ha=sum(sum_ha_sm))  # 133,258 ha Silomais in Bawü


## waehlen einer globalen beschraenkung 1.25 * Flaeche je crop, je regio in diesem Fall der Kreis
crop_kreis_res<-zentroid_p %>% select(counties="NUTS_2", crop="Kennung", Bodenguete, FLAECHE_HA)
crop_kreis_res <-crop_kreis_res %>% mutate(management= case_when(
                          Bodenguete=='gering' ~c("niedrig"),
                          Bodenguete=='mittel' ~c("mittel"),
                          Bodenguete=='hoch' ~c("hoch")))

crop_kreis_res <- crop_kreis_res %>% select(counties, crop, FLAECHE_HA) %>% group_by(counties,crop) %>% summarize(sum_ha_sm=sum(FLAECHE_HA))
crop_kreis_res <- crop_kreis_res %>% filter(!crop=="B")





## write out restriction datasets

write_xlsx(x=Ka_county, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/Ka_county.xlsx", col_names = T)
write_xlsx(x=sm_county, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/sm_county.xlsx", col_names = T)
write_xlsx(x=crop_kreis_res, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/crop_kreis_res.xlsx", col_names = T)



############################################################################################################################################################################################
############################################################################################################################################################################################
## Model diagnostics
######################################################################################################################################################################################################

## Einlesen und uerberpruefen der Ergebnisse gegenüber dem status quo
## should be a easily reproducible script, that compares

  #(1) Crop amounts for each county
  #(2) CRids available from crop rota to the ones grown in the respective county

############################################################################################################################################################################################
## status quo
########################################################################################################################################################################################

# (1) Amounts of crops for each county
# file containing status quo data
status_quo <- zentroid_p
status_quo <- status_quo %>% select(counties="NUTS_2",management="Bodenguete",crop="Kennung", FLAECHE_HA)
status_quo <- status_quo %>% filter(!crop=="B")
head(status_quo)
summary(status_quo)

## Or take the crop shares from status quo
crop_share_status_quo<-Crop_share_BW_P
crop_share_status_quo

###########################################################################################################################################################################################
## GAMS results
#########################################################################################################################################################################################

# loading results files from GAMS
setwd("C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/GAMS_P/Results/01.03.23")

Gams_results <- read_excel("results.xlsx", sheet="Tabelle1")
report_anbau <- read_excel("Report_Anbau.xlsx", sheet="Tabelle1")
head(report_anbau)
summary(report_anbau)

report_anbau  <-report_anbau %>% rename(counties="...1", crop="...2")

report_anbau <- pivot_longer(report_anbau, cols = c("mittel", "hoch", "niedrig"), names_to = "management", values_to = "FLAECHE_HA")
head(report_anbau)

#############################################################################################################################################################################################
## Comparison of results
##########################################################################################################################################################################################

# Comparing by county fuer die anbaumenge crop
status_quo %>% group_by(counties, crop) %>% summarize(crop_flaeche=sum(FLAECHE_HA)) #608x3 dataset
report_anbau %>%group_by(counties, crop) %>% summarize(crop_flaeche=sum(FLAECHE_HA)) #240x3 dataset

## Comparing crop shares
# create crop shares aus report_anbau
crop_share_GAMS<- report_anbau %>% group_by(counties) %>% mutate(sum_kreis=sum(FLAECHE_HA, na.rm=TRUE)) %>% group_by(counties, crop, sum_kreis) %>% 
                                        summarize(sum=sum(FLAECHE_HA, na.rm=TRUE)) %>% mutate(share=sum/sum_kreis)%>% select(-sum_kreis,-sum)%>%
                                        spread(key = crop, value = share)

crop_share_GAMS
crop_share_status_quo

crop_share_GAMS$Ha - crop_share_status_quo$Ha



# 01.03.23 Potatoe is wildly overrepresented, sugar beet not even in the solution

## how about the crop rotations
# crop roatation matrix
setwd("C:/Users/Tristan Herrmann/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/GAMS_P/Results")
CropRot <- read_excel("Crop_Rot.xlsx", sheet="Tabelle1")

setwd("C:/Users/Tristan Herrmann/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/GAMS_P/Results/28.02.23")
CropProdl <- read_excel("results.xlsx", sheet="Tabelle1")

CropRot<- CropRot %>% rename(CR_id="...1") 
CropRot

CropRot<-CropRot %>% replace_na(list(Gr=0)) %>%
             replace_na(list(Ha=0)) %>%
             replace_na(list(Ka=0)) %>%
             replace_na(list(KG=0)) %>%
             replace_na(list(KM=0)) %>%
             replace_na(list(SG=0)) %>%
             replace_na(list(SM=0)) %>%
             replace_na(list(WG=0)) %>%
             replace_na(list(Win=0)) %>%
             replace_na(list(WR=0)) %>%
             replace_na(list(WW=0)) %>%
              replace_na(list(ZR=0)) %>%
              replace_na(list(Rog=0)) %>%
              replace_na(list(KL=0)) 

CR_id_name<-rotation_matrix %>% select(CR_id=CRid,Kombo ) %>% mutate(CR_id=as.character(CR_id))


# leftjoin
#GAMS_crop_rotations <-left_join(CR_id_name, CropProdl, by="CR_id")
GAMS_crop_rotations<- left_join(CropProdl, CR_id_name, by="CR_id")
head(GAMS_crop_rotations)

GAMS_crop_rotations %>% group_by(Kombo) %>% count()
GAMS_crop_rotations %>% group_by(Counties,Kombo) %>% count()





