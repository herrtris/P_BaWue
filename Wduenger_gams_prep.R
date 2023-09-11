## 18.07.23
## Skript dient zur weiteren Bearbeitung des NPK estimates für BAWUE

## Der Duenger ist je NUTS_2 und je animal type verfugebar (der Duengertyp)

# Also:

# Kosten_duenger(NUTS, Crop_rotation)= organischer_Duenger(NUTS_2, Duengerart, Tierart)
# ich muss die unterschiedlichen contents berücksichtigen. Also wenn er ein kg N von einer Quelle nimmt muss das equivalente von P mitgenommen werden. 

# Task: nimm erstmal nur den duenger aus Milchvieh her. Aus DCOW. 


rm(list=ls())


library(dplyr)
#install.packages("tidyr")
library(tidyr)
library(readxl)
library(writexl)
library(ggplot2)
library(tidyverse)


laptob_work <- TRUE
options(scipen = 999)


if(laptob_work==TRUE) {
  setwd("C:\\Users\\User\\OneDrive - bwedu\\Dokumente\\Landwirtschaftliche Betriebslehre\\Projekt_P_Bawü\\P_Bawue\\Output_GAMS_P_Prep\\18.04.23")
} else {
  setwd("C:\\Users\\Tristan Herrmann\\OneDrive - bwedu\\Dokumente\\Landwirtschaftliche Betriebslehre\\Projekt_P_Bawü\\P_Bawue\\Output_GAMS_P_Prep\\18.04.23")
}

## Read in DuengerDAten
wduenger <- read_excel("NPK_estimate_bawue.xlsx")
wduenger

# Base checks
### There was an error where N and P je Tier was gleich, check for that error is it now corrected for all obs?
wduenger %>% mutate(check=if_else(N_kg_year==P205_kg_year, TRUE, FALSE)) %>% filter(check==TRUE) %>% filter(No_animals== !0)
wduenger %>% mutate(check=if_else(N_kg_year==K20_kg_year, TRUE, FALSE)) %>% filter(check==TRUE) %>% filter(No_animals== !0)
wduenger %>% mutate(check=if_else(P205_kg_year==K20_kg_year, TRUE, FALSE)) %>% filter(check==TRUE) %>% filter(No_animals== !0)
# Result: All differences between N and P are there, continue

# I have only 44 conuties?
wduenger %>% group_by(NUTS_2) %>% count(NUTS_2) #Correct, continue

# Wie viele Produkte habe ich, sollten 5 sein
wduenger %>% group_by(Produkt) %>% count(Produkt)
wduenger %>% select(NUTS_2, Type) %>% group_by(Type) %>% count()
wduenger %>% select(NUTS_2, Type, Produkt) %>% group_by(Type, Produkt) %>% count() %>% print(n=Inf)

wduenger %>% select(NUTS_2, Type, Produkt, Rasse) %>% filter(Type=="CALV") %>%group_by(Type, Produkt, Rasse) %>% count() %>% print(n=Inf)
##Okay erster Versuch. nehme das Basis-rgionsmodell aus dem Modul - es representiert die Region Stuttgart, es gibt dor nur Milchkuehe DCOW

stuttgart_DCOW <- wduenger %>% filter(NUTS_2=="DE111" & Type=="DCOW") %>% filter(!No_animals==0)
stuttgart_DCOW

# Gesamtzahl Tiere in DE111, ca. 390 Kuehe verschiedener Rasse in Stuttgart
stuttgart_DCOW %>% filter(verfahren=="strohbasiert") %>% summarize(sum(No_animals, na.rm=T)/3)
stuttgart_DCOW %>% filter(verfahren=="guellebasiert") %>% summarize(sum(No_animals, na.rm=T))

# Zur weiteren Vereinfachung mache ich einen group_by produkt. Kann ich später wieder komplizierter machen
duenger_vereinfacht <- stuttgart_DCOW %>%select(NUTS_2,Type, Produkt, N_kg_year:K20_kg_year,FM_regio_jahr)%>% group_by(NUTS_2,Type,Produkt) %>% summarize(across(c(N_kg_year,P205_kg_year,K20_kg_year,FM_regio_jahr), sum))
duenger_vereinfacht <- duenger_vereinfacht %>% mutate(P_kg_year=0.4364*P205_kg_year, K_kg_year=K20_kg_year*0.8302) %>% select(-c(P205_kg_year,K20_kg_year))
duenger_vereinfacht

# standardisierung in N_Einheit
duenger_vereinfacht %>% mutate(N_einheit=1, P_je_NEinheit=P_kg_year/N_kg_year, K_je_NEinheit=K_kg_year/N_kg_year)



#########################################################################
# For regional perspective to get stuttgart more or less right...
# How much area does stuttgart have? How much does Hohenlohe?

landuse_plots <- read_excel("Landuse_plots_P.xlsx")
head(landuse_plots)

stuttgart<-landuse_plots %>% filter(NUTS_2=="DE111")
stuttgart %>% group_by(NUTS_2, Bodenguete) %>% summarize(Flaeche=sum(sum_ha))
stuttgart %>% summarize(Flaeche=sum(sum_ha))

hohenlohe<-landuse_plots %>% filter(NUTS_2=="DE119")
hohenlohe %>% group_by(NUTS_2, Bodenguete) %>% summarize(Flaeche=sum(sum_ha))
hohenlohe %>% summarize(Flaeche=sum(sum_ha))





# create stuttgart_hohenlohe_tierart fuer stuttgart

stuttgart_hohenlohe_tierart <- wduenger %>% filter(NUTS_2=="DE111") %>% filter(!No_animals==0)
stuttgart_hohenlohe_tierart



stuttgart_hohenlohe_tierart <- stuttgart_hohenlohe_tierart %>%select(Type, Produkt, N_kg_year:K20_kg_year,FM_regio_jahr)%>% group_by(Type,Produkt) %>% summarize(across(c(N_kg_year,P205_kg_year,K20_kg_year,FM_regio_jahr), sum))
stuttgart_hohenlohe_tierart %>% print(n=Inf)

stuttgart_hohenlohe_tierart <- stuttgart_hohenlohe_tierart %>% mutate(P_kg_year=0.4364*P205_kg_year, K_kg_year=K20_kg_year*0.8302) %>% select(-c(P205_kg_year,K20_kg_year))
stuttgart_hohenlohe_tierart %>% print(n=Inf)



NPK_limit <-stuttgart_hohenlohe_tierart %>% select(-c(FM_regio_jahr))


set_tierart <- stuttgart_hohenlohe_tierart %>% select(Type) %>% distinct()
set_tierart

# FM per tonne, momentan noch in kg
stuttgart_tierart <- stuttgart_tierart %>% mutate(FM_regio_jahr_tonnes=FM_regio_jahr/1000) %>% select(-c(FM_regio_jahr))

content <- stuttgart_tierart %>% mutate(N_je_tFM=N_kg_year/FM_regio_jahr_tonnes, P_je_tFM=P_kg_year/FM_regio_jahr_tonnes, K_je_tFM=K_kg_year/FM_regio_jahr_tonnes) %>% select(Type, Produkt, N_je_tFM:K_je_tFM)

# hier vorherige version je Einheit N
#content <- stuttgart_tierart %>% mutate(N_einheit=1, P_je_NEinheit=P_kg_year/N_kg_year, K_je_NEinheit=K_kg_year/N_kg_year) %>% select(Type, Produkt, N_einheit:K_je_NEinheit)

stuttgart_tierart%>% ungroup() %>% select(Produkt) %>% distinct()  
  
  
# ausspucken der Daten
if(laptob_work==TRUE) {
  write_xlsx(x=content, path = "C:/Users/User/OneDrive - bwedu/Dokumente/GAMS/Investition_Finazierung_dynamic decision models/Teil 2_ Dynamische Entscheidungsmodelle/content_type.xlsx", col_names = TRUE)
} else {
  write_xlsx(x=content, path = "C:/Users/User/OneDrive - bwedu/Dokumente/GAMS/Investition_Finazierung_dynamic decision models/Teil 2_ Dynamische Entscheidungsmodelle/content_type.xlsx", col_names = TRUE)
}

if(laptob_work==TRUE) {
  write_xlsx(x=set_tierart, path = "C:/Users/User/OneDrive - bwedu/Dokumente/GAMS/Investition_Finazierung_dynamic decision models/Teil 2_ Dynamische Entscheidungsmodelle/set_tierart.xlsx", col_names = F)
} else {
  write_xlsx(x=set_tierart, path = "C:/Users/User/OneDrive - bwedu/Dokumente/GAMS/Investition_Finazierung_dynamic decision models/Teil 2_ Dynamische Entscheidungsmodelle/set_tierart.xlsx", col_names = F)
}

if(laptob_work==TRUE) {
  write_xlsx(x=NPK_limit, path = "C:/Users/User/OneDrive - bwedu/Dokumente/GAMS/Investition_Finazierung_dynamic decision models/Teil 2_ Dynamische Entscheidungsmodelle/NPK_limit.xlsx", col_names = TRUE)
} else {
  write_xlsx(x=NPK_limit, path = "C:/Users/User/OneDrive - bwedu/Dokumente/GAMS/Investition_Finazierung_dynamic decision models/Teil 2_ Dynamische Entscheidungsmodelle/NPK_limit.xlsx", col_names = TRUE)
}

##############################################################################################################################################################################################
# adding one more region hohenlohe

# create stuttgart_Tierart fuer stuttgart & Hohenlohe add a set for the region nuts3

stuttgart_hohenlohe_tierart <- wduenger %>% filter(NUTS_2=="DE111" | NUTS_2=="DE119") %>% filter(!No_animals==0)
stuttgart_hohenlohe_tierart



stuttgart_hohenlohe_tierart <- stuttgart_hohenlohe_tierart %>%select(NUTS_2,Type, Produkt, N_kg_year:K20_kg_year,FM_regio_jahr)%>% group_by(NUTS_2,Type,Produkt) %>% summarize(across(c(N_kg_year,P205_kg_year,K20_kg_year,FM_regio_jahr), sum))
stuttgart_hohenlohe_tierart %>% print(n=Inf)

stuttgart_hohenlohe_tierart <- stuttgart_hohenlohe_tierart %>% mutate(P_kg_year=0.4364*P205_kg_year, K_kg_year=K20_kg_year*0.8302) %>% select(-c(P205_kg_year,K20_kg_year))
stuttgart_hohenlohe_tierart %>% print(n=Inf)



NPK_limit <-stuttgart_hohenlohe_tierart %>% select(-c(FM_regio_jahr))


set_tierart <- stuttgart_hohenlohe_tierart %>%ungroup() %>% select(Type) %>% distinct()
set_tierart

# FM per tonne, momentan noch in kg
stuttgart_hohenlohe_tierart <- stuttgart_hohenlohe_tierart %>% mutate(FM_regio_jahr_tonnes=FM_regio_jahr/1000) %>% select(-c(FM_regio_jahr))
stuttgart_hohenlohe_tierart

content <- stuttgart_hohenlohe_tierart %>% mutate(N_je_tFM=N_kg_year/FM_regio_jahr_tonnes, P_je_tFM=P_kg_year/FM_regio_jahr_tonnes, K_je_tFM=K_kg_year/FM_regio_jahr_tonnes) %>% select(NUTS_2, Type, Produkt, N_je_tFM:K_je_tFM)
content %>% print(n=Inf)
# hier vorherige version je Einheit N
#content <- stuttgart_hohenlohe_tierart %>% mutate(N_einheit=1, P_je_NEinheit=P_kg_year/N_kg_year, K_je_NEinheit=K_kg_year/N_kg_year) %>% select(Type, Produkt, N_einheit:K_je_NEinheit)

stuttgart_hohenlohe_tierart%>% ungroup() %>% select(Produkt) %>% distinct()  


# ausspucken der Daten
if(laptob_work==TRUE) {
  write_xlsx(x=content, path = "C:/Users/User/OneDrive - bwedu/Dokumente/GAMS/Investition_Finazierung_dynamic decision models/Teil 2_ Dynamische Entscheidungsmodelle/content_type.xlsx", col_names = TRUE)
} else {
  write_xlsx(x=content, path = "C:/Users/User/OneDrive - bwedu/Dokumente/GAMS/Investition_Finazierung_dynamic decision models/Teil 2_ Dynamische Entscheidungsmodelle/content_type.xlsx", col_names = TRUE)
}

if(laptob_work==TRUE) {
  write_xlsx(x=set_tierart, path = "C:/Users/User/OneDrive - bwedu/Dokumente/GAMS/Investition_Finazierung_dynamic decision models/Teil 2_ Dynamische Entscheidungsmodelle/set_tierart.xlsx", col_names = F)
} else {
  write_xlsx(x=set_tierart, path = "C:/Users/User/OneDrive - bwedu/Dokumente/GAMS/Investition_Finazierung_dynamic decision models/Teil 2_ Dynamische Entscheidungsmodelle/set_tierart.xlsx", col_names = F)
}

if(laptob_work==TRUE) {
  write_xlsx(x=NPK_limit, path = "C:/Users/User/OneDrive - bwedu/Dokumente/GAMS/Investition_Finazierung_dynamic decision models/Teil 2_ Dynamische Entscheidungsmodelle/NPK_limit.xlsx", col_names = TRUE)
} else {
  write_xlsx(x=NPK_limit, path = "C:/Users/User/OneDrive - bwedu/Dokumente/GAMS/Investition_Finazierung_dynamic decision models/Teil 2_ Dynamische Entscheidungsmodelle/NPK_limit.xlsx", col_names = TRUE)
}



### mieral uebersetzung gilt fuer beide
### Preparing fertilizer_mineral content
## content for mineral fertilizer
# NUTS_2 <- c("DE111", "DE112", "DE113", "DE114", "DE115", "DE116", "DE117", "DE118", "DE119", "DE11A","DE11B","DE11C","DE11D", 
#             "DE121","DE122","DE123","DE124","DE125", "DE126", "DE127", "DE128", "DE129", "DE12A","DE12B", "DE12C",
#             "DE131","DE132","DE133","DE134", "DE135","DE136","DE137","DE138","DE139","DE13A",
#             "DE141","DE142","DE143","DE144","DE145","DE146","DE147","DE148","DE149")
# 
# fertilizer_mineral <- c("N_mineral", "P_mineral", "K_mineral")
# 
# # # Number of fields
# # num_fields <- 50
# 
# # # List of fertilizer types
# # fertilizer_types <- c("N_mineral", "P_mineral", "K_mineral", "fluessigmist", "frischmist", "jauche", "rottemist" )
# 
# 
# # Create a data frame with all possible combinations
# dataset1 <- expand_grid(NUTS_2 = paste(NUTS_2), Fertilizer_Type = fertilizer_mineral)
# 
# content_mineral<-dataset1 %>% mutate(N=if_else(Fertilizer_Type=="N_mineral", 1,0))%>%
#                                mutate(P=if_else(Fertilizer_Type=="P_mineral", 1,0))%>%
#                                mutate(K=if_else(Fertilizer_Type=="K_mineral", 1,0))
# 
# content_mineral
# 
# 
# content_mineral %>% filter(NUTS_2=="DE111") %>% select(-NUTS_2)


### Preparing the Output for the whole Baden-Wuerttmberg

# starting file is wduenger
# here you can find the excreted amount of N, P, and K 
# Potential reductions from N_excreted to N_ausbringung need to be checked! What did Thuenen do in between, what do I need to do to get to the plant-available N, that is needed for the crops
# Potential reductions need to be doone for N_excreted anyway, for the amount going grassland - wiesen und weiden

# But! GOAL 1: Start modelling and ignore these things first! - then see how far off you are from the data on sold P, N, K 
# Note: For the whole of BaWue more or less 10.000t to 12.000t are sold each season - in this area I do have to land

# Raw preparation for the whole region of Bawue

# create stuttgart_Tierart fuer stuttgart & Hohenlohe add a set for the region nuts3

bawue_tierart <- wduenger %>% filter(!No_animals==0)
bawue_tierart


# Group by Tierart, Region und Produkt und bilde die Summe - somit wird gesamt_menge an Mist/Guelle je Duengerart gebildet
bawue_tierart <- bawue_tierart %>%select(NUTS_2,Type, Produkt, N_kg_year:K20_kg_year,FM_regio_jahr)%>% group_by(NUTS_2,Type,Produkt) %>% 
                                  summarize(across(c(N_kg_year,P205_kg_year,K20_kg_year,FM_regio_jahr), sum))

bawue_tierart

bawue_tierart <- bawue_tierart %>% mutate(P_kg_year=0.4364*P205_kg_year, K_kg_year=K20_kg_year*0.8302) %>% select(-c(P205_kg_year,K20_kg_year))
bawue_tierart

# Einbau erster Abschlag aus Thuenen auf bawue tierart
# 1. NUtze N for spreading Anteil an total N excreted. Wahrscheinlich falle hier nach Thüenen Emmissionsversluste / Lagerungsverluste an
#    Berechne somit die amount for spreading je kreis 

# Vermutlich falsche Annahme: Verluste werden auf alle Wirtschaftsduengerarten gleich angewandt, vermutlich ist slurry stärker betroffen
# Gibt es hier noch Daten zu den Lagerungsverlusten?
# Weitere Annahme: P ist erstmal nicht von gorßen LAgerungsverlusten betroffen. Normalerweise gast P ja nicht aus, kann nicht sein. Somit sind die einzigen P-verluste z.B. Versickerungen
# und damit im niderschwelligen Prozentbereich

bawue_tierart %>%ungroup() %>% distinct(Type)

# Get shares der Weidezeit etc. 
setwd("C:\\Users\\User\\OneDrive - bwedu\\Dokumente\\Landwirtschaftliche Betriebslehre\\Projekt_P_Bawü\\GAMS_P\\Input_data\\Kalkulationsdaten")
abschlagshares_thuenen<- landuse_plots <- read_excel("Datenvergleich_schaetzung_9.9.xlsx", sheet = "N_for_spreading_2020")
abschlagshares_thuenen


bawue_tierart  <-    bawue_tierart %>% left_join(abschlagshares_thuenen %>% rename(Type="Animal_type"), by="Type") 

bawue_tierart<- bawue_tierart %>% select(-c(Total_N_exctreted, N_for_spreading_kg)) %>% rename(share_N_Ausbringung="share_on_total_excreted") %>%
                  mutate(N_kg_year_thAbschlag= N_kg_year*share_N_Ausbringung/100,
                         N_pasture= Grazing_time_per_year/100*N_kg_year,
                         P_pasture=Grazing_time_per_year/100*P_kg_year,
                         K_pasture=Grazing_time_per_year/100*K_kg_year,
                         FM_regio_jahr_pasture=Grazing_time_per_year/100*FM_regio_jahr)

### Ich habe jedoch nur N for spreading als information. P und K for pasture muessen auch noch weg. 
bawue_tierart
bawue_tierart <- bawue_tierart %>% mutate(P_kg_year_Pabschlag=P_kg_year-P_pasture,
                         K_kg_year_Pabschlag=K_kg_year-K_pasture,
                         FM_regio_jahr_Pabschlag=FM_regio_jahr- FM_regio_jahr_pasture)  
  

## Zur Bedarfsdeckung von Wiesen und Weiden
bawue_pasture <- bawue_tierart %>% select(c(NUTS_2:Produkt, N_pasture:FM_regio_jahr_pasture))

# Daten nach Abschlaege fuer N nach Thuenen und fuer P und K, begruendet nach grazing time je Tierart
# Hier nachfragen ob es grazing time auch je kreis zur verfügung gibt
bawue_tierart<- bawue_tierart %>%  select(NUTS_2:Produkt, N_kg_year_thAbschlag, P_kg_year_Pabschlag, K_kg_year_Pabschlag,FM_regio_jahr_Pabschlag)

bawue_tierart


#####
###################################################################################################################################################################################

## renaming variables back, so that I dont have to do anything :)
bawue_tierart<- bawue_tierart %>% rename(N_kg_year="N_kg_year_thAbschlag",P_kg_year="P_kg_year_Pabschlag",K_kg_year="K_kg_year_Pabschlag",  FM_regio_jahr="FM_regio_jahr_Pabschlag")

#####################################################################################################################################################################################


NPK_limit <-bawue_tierart %>% select(-c(FM_regio_jahr))
NPK_limit <-NPK_limit %>% rename(N="N_kg_year", P="P_kg_year", K="K_kg_year")
coloumn_names <-names(NPK_limit)
coloumn_names[1:3] <- ""
names(NPK_limit)<- coloumn_names
NPK_limit
############################################################################


set_tierart <- bawue_tierart %>%ungroup() %>% select(Type) %>% distinct()
set_tierart

# FM per tonne, momentan noch in kg
bawue_tierart <- bawue_tierart %>% mutate(FM_regio_jahr_tonnes=FM_regio_jahr/1000) %>% select(-c(FM_regio_jahr))
bawue_tierart

content <- bawue_tierart %>% mutate(N_je_tFM=N_kg_year/FM_regio_jahr_tonnes, P_je_tFM=P_kg_year/FM_regio_jahr_tonnes, K_je_tFM=K_kg_year/FM_regio_jahr_tonnes) %>% select(NUTS_2, Type, Produkt, N_je_tFM:K_je_tFM)
content %>% print(n=Inf)
# hier vorherige version je Einheit N
#content <- stuttgart_hohenlohe_tierart %>% mutate(N_einheit=1, P_je_NEinheit=P_kg_year/N_kg_year, K_je_NEinheit=K_kg_year/N_kg_year) %>% select(Type, Produkt, N_einheit:K_je_NEinheit)

# make content GAMS ready - dont need to modify the excel in any way, does just read into gams
content <-content %>% rename(N="N_je_tFM", P="P_je_tFM", K="K_je_tFM")
coloumn_names <-names(content)
coloumn_names[1:3] <- ""
names(content)<- coloumn_names
content



if(laptob_work==TRUE) {
  setwd("C:\\Users\\User\\OneDrive - bwedu\\Dokumente\\Landwirtschaftliche Betriebslehre\\Projekt_P_Bawü\\GAMS_P")
} else {
  setwd("C:\\Users\\Tristan Herrmann\\OneDrive - bwedu\\Dokumente\\Landwirtschaftliche Betriebslehre\\Projekt_P_Bawü\\GAMS_P")
}



# ausspucken der Daten direkt in GAMS_P
if(laptob_work==TRUE) {
  write_xlsx(x=content, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/GAMS_P/content_type.xlsx", col_names = TRUE)
} else {
  write_xlsx(x=content, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/GAMS_P/content_type.xlsx", col_names = TRUE)
}

if(laptob_work==TRUE) {
  write_xlsx(x=set_tierart, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/GAMS_P/set_tierart.xlsx", col_names = F)
} else {
  write_xlsx(x=set_tierart, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/GAMS_P/set_tierart.xlsx", col_names = F)
}

if(laptob_work==TRUE) {
  write_xlsx(x=NPK_limit, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/GAMS_P/NPK_limit_2.xlsx", col_names = TRUE)
} else {
  write_xlsx(x=NPK_limit, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/GAMS_P/NPK_limit_2.xlsx", col_names = TRUE)
}




















  
