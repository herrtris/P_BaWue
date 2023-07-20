#P_organic_R_2
# here I'm using the GV units per NUTS_3 to estimate the amount of animals per NUTS_3 

rm(list=ls())


library(dplyr)
#install.packages("tidyr")
library(tidyr)
library(readxl)
library(writexl)
library(ggplot2)
library(tidyverse)


# Starting with cows that are producing milk
# several elements are important to estimate the fertilizer amount per NUTS_2

laptob_work <- TRUE
options(scipen = 999)
# 1. Task one date set with the amount of animals per RP region -  this data is complete and will later on be the basis for estimation, based on the GV units of the Thuenen institute

#3. The amount of cows in the region is relevant of course, how many cows per NUTS2 are there producing N, P, K
if(laptob_work==TRUE) {
  setwd("C:\\Users\\User\\OneDrive - bwedu\\Dokumente\\Landwirtschaftliche Betriebslehre\\Projekt_P_Bawü\\GAMS_P\\Input_data\\Kalkulationsdaten\\Agrarstrukturerhebung\\Tierzahlen_bawue_März_2020")
} else {
  setwd("C:\\Users\\Tristan Herrmann\\OneDrive - bwedu\\Dokumente\\Landwirtschaftliche Betriebslehre\\Projekt_P_Bawü\\GAMS_P\\Input_data\\Kalkulationsdaten\\Agrarstrukturerhebung\\Tierzahlen_bawue_März_2020")
}

#strich gibt nichtsd . wissen es nicht

### Rinder bawue
rinder_1 <- read_excel("Tierzahlen_complete_2020.xlsx",sheet = "Tab2_Seite02_03", skip = 7, col_names = FALSE )
rinder_1<-rinder_1 %>% select(region=...1, total_rinder=...5, cows=...6, suckler_cows=...7, Kae_JuR_U1_male=...8, Kae_JuR_U1_male_female=...9, JuR_Ue1_U2_male=...10, JuR_Ue1_U2__female=...11, JuR_U2_male=...12, JuR_U2_female=...13)
rinder_1

rinder_2 <- read_excel("Tierzahlen_complete_2020.xlsx",sheet = "nochTab2_Seite04_05", skip=7, col_names = FALSE )
rinder_2<-rinder_2 %>% select(region=...1, total_rinder=...5, cows=...6, suckler_cows=...7, Kae_JuR_U1_male=...8, Kae_JuR_U1_male_female=...9, JuR_Ue1_U2_male=...10, JuR_Ue1_U2__female=...11, JuR_U2_male=...12, JuR_U2_female=...13)

rinder_bawue <- rbind(rinder_1, rinder_2)
rinder_bawue<- mutate(rinder_bawue, ID=row_number())
rinder_bawue %>% print(n=Inf)


### Schweine bawue
schweine_1 <- read_excel("Tierzahlen_complete_2020.xlsx",sheet = "Tab3_Seite06", skip = 7, col_names = FALSE )
schweine_1<-schweine_1 %>% select(region=...1, total_schweine=...5, ferkel=...6, zuchtsauen=...7, andere_schweine=...8)
schweine_1

schweine_2 <- read_excel("Tierzahlen_complete_2020.xlsx",sheet = "nochTab3_Seite07", skip=7, col_names = FALSE )
schweine_2<-schweine_2 %>% select(region=...1, total_schweine=...5, ferkel=...6, zuchtsauen=...7, andere_schweine=...8)
schweine_2

schweine_bawue <- rbind(schweine_1, schweine_2)
schweine_bawue<- mutate(schweine_bawue, ID=row_number())
schweine_bawue %>% print(n=Inf)

### Gefluegel BaWue

gefluegel_1 <- read_excel("Tierzahlen_complete_2020.xlsx",sheet = "Tab4_Seite8_9", skip = 7, col_names = FALSE )
gefluegel_1<-gefluegel_1 %>% select(region=...1, total_gefluegel=...5, junghennen=...6, legehennen=...7, mastgefluegel=...8, gaense=...10, enten=...12, truthuehner=...14)
gefluegel_1

gefluegel_2 <- read_excel("Tierzahlen_complete_2020.xlsx",sheet = "nochTab4_Seite10_11", skip=7, col_names = FALSE )
gefluegel_2<-  gefluegel_2 %>% select(region=...1, total_gefluegel=...5, junghennen=...6, legehennen=...7, mastgefluegel=...8, gaense=...10, enten=...12, truthuehner=...14)
gefluegel_2

gefluegel_bawue <- rbind(gefluegel_1, gefluegel_2)
gefluegel_bawue<-gefluegel_bawue[c(1:49),]
gefluegel_bawue<- mutate(gefluegel_bawue, ID=row_number())
gefluegel_bawue %>% print(n=Inf)


#### Schafe BaWue
schafe_1 <- read_excel("Tierzahlen_complete_2020.xlsx",sheet = "Tab5_Seite12", skip = 7, col_names = FALSE )
schafe_1<- schafe_1 %>% select(region=...1, total_schafe=...3,schafe_U1=...4,total_milch_mutterschafe=...5, milchschafe=...6, andere_mutterschafe=...7, boecke_hammel_other=...8)
schafe_1

schafe_2 <- read_excel("Tierzahlen_complete_2020.xlsx",sheet = "nochTab5_Seite13", skip=7, col_names = FALSE )
schafe_2<- schafe_2 %>% select(region=...1, total_schafe=...3,schafe_U1=...4,total_milch_mutterschafe=...5, milchschafe=...6, andere_mutterschafe=...7, boecke_hammel_other=...8)
schafe_2

schafe_bawue <- rbind(schafe_1, schafe_2)
schafe_bawue<-schafe_bawue[c(1:49),]
schafe_bawue<- mutate(schafe_bawue, ID=row_number())
schafe_bawue %>% print(n=Inf)


#### Ziegen BaWue
ziegen_1 <- read_excel("Tierzahlen_complete_2020.xlsx",sheet = "Tab6_Seite14", skip = 7, col_names = FALSE )
ziegen_1<-ziegen_1 %>% select(region=...1, total_ziegen=...3, ziegen_female=...4, andere_ziegen=...5)
ziegen_1

ziegen_2 <- read_excel("Tierzahlen_complete_2020.xlsx",sheet = "nochTab6_Seite15", skip=7, col_names = FALSE )
ziegen_2<- ziegen_2 %>% select(region=...1, total_ziegen=...3, ziegen_female=...4, andere_ziegen=...5)
ziegen_2

ziegen_bawue <- rbind(ziegen_1, ziegen_2)
ziegen_bawue<-ziegen_bawue[c(1:49),]
ziegen_bawue<- mutate(ziegen_bawue, ID=row_number())
ziegen_bawue %>% print(n=Inf)


#### Pferde BaWue

pferde_2 <- read_excel("Tierzahlen_complete_2020.xlsx",sheet = "Tab7_Seite16", skip=5, col_names = FALSE )
pferde_2

pferde_3<- pferde_2 %>% select(region=...1, total_pferde=...3)
pferde_4<- pferde_2 %>% select(region=...4, total_pferde=...6)
pferde_bawue <- rbind(pferde_3, pferde_4)


pferde_bawue<-pferde_bawue %>% filter(!is.na(total_pferde))
pferde_bawue<- mutate(pferde_bawue, ID=row_number())
pferde_bawue %>% print(n=Inf)



# create complete dataset
nutztiere_bawue <-rinder_bawue %>% left_join(schweine_bawue %>% select(total_schweine:ID), by="ID") %>% 
                                   left_join(gefluegel_bawue %>% select(total_gefluegel:ID),by="ID") %>%
                                   left_join(ziegen_bawue%>%select(total_ziegen:ID), by="ID") %>% 
                                   left_join(schafe_bawue %>% select(total_schafe:ID),by="ID") %>%
                                   left_join(pferde_bawue %>% select(total_pferde, ID), by="ID")

if(laptob_work==TRUE) {
  write_xlsx(x=nutztiere_bawue, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/18.04.23/nutztiere_bawue_raw.xlsx", col_names = TRUE)
  
} else {
  write_xlsx(x=nutztiere_bawue, path = "C:/Users/Tristan Herrmann/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/18.04.23/nutztiere_bawue_raw.xlsx", col_names = TRUE)
  
}




rm(ziegen_1, ziegen_2, ziegen_bawue, pferde_2, pferde_3, pferde_4, pferde_bawue, schafe_1, 
   schafe_2, schafe_bawue, schweine_1, schweine_2, schweine_bawue, rinder_1, rinder_2, rinder_bawue, 
   gefluegel_1, gefluegel_2,gefluegel_bawue)

#####################################################################################################################################################################################

# original data
nutztiere_bawue


############################################################################################################################################################################################################

# select Daten auf Ebene des RPs und auf Ebene Gesamt_bawue

nutztiere_RP_Bawue<-nutztiere_bawue[c(14,27,38,48,49),]
str(nutztiere_RP_Bawue)

nutztiere_kreise_bawue<-nutztiere_bawue[-c(14,27,38,48,49),]
print(nutztiere_kreise_bawue %>% select(region), n=Inf)
# Fuege NUTS_2 codes hinzu
NUTS_2 <- c("DE111", "DE112", "DE113", "DE114", "DE115", "DE116", "DE117", "DE118", "DE119", "DE11A","DE11B","DE11C","DE11D", 
            "DE121","DE122","DE123","DE124","DE125", "DE126", "DE127", "DE128", "DE129", "DE12A","DE12B", "DE12C",
            "DE131","DE132","DE133","DE134", "DE135","DE136","DE137","DE138","DE139","DE13A",
            "DE141","DE142","DE143","DE144","DE145","DE146","DE147","DE148","DE149")

NUTS_2 %>% as_tibble()

NUTS_2_order <- c("DE111", "DE112", "DE113", "DE114", "DE115", "DE116", "DE117", "DE118", "DE119", "DE11A","DE11B","DE11C","DE11D", 
            "DE121","DE122","DE123","DE124","DE125", "DE126", "DE127", "DE128", "DE129", "DE12A","DE12B", "DE12C",
            "DE131","DE132","DE133","DE134", "DE135","DE136","DE137","DE138","DE139","DE13A",
            "DE141","DE142","DE143","DE144","DE145","DE146","DE147","DE148","DE149")


nutztiere_kreise_bawue<-cbind(NUTS_2, nutztiere_kreise_bawue)


# In theory the animal per region estimation to obtain N,P,K works as follows
# 1.Use Grossvieheinheiten per kreis, was ich von Thuenen bekomme um basierden auf Nutztiere_RP_bawue die einzelnen Tierzahlen zu schaetzen
# 2. hierfuer nutze einen scaling factor um die das gewichts der großvieheinheiten beizubehalten, alle scaling factors ergeben jedoch gmeinsam 1, einzelne Stadkreise muessen hierfuer noch zusammengelegt werden
# 3. stelle die schaetzung den tatsaechlichen Zahlen gegenuber (die mit den Luecken) --> nutztiere_kreise_bawue auch hier fasse stadtkreise zu den landkreisen zusammen
# 

# Lade Daten, notwendig um adresszusaetze Thuenen mit NUTS_2 zu verknuepfen

if(laptob_work==TRUE) {
  setwd("C:\\Users\\User\\OneDrive - bwedu\\Dokumente\\Landwirtschaftliche Betriebslehre\\Projekt_P_Bawü\\GAMS_P\\Input_data\\Kalkulationsdaten\\Agrarstrukturerhebung\\Tierzahlen_bawue_März_2020")
} else {
  setwd("C:\\Users\\Tristan Herrmann\\OneDrive - bwedu\\Dokumente\\Landwirtschaftliche Betriebslehre\\Projekt_P_Bawü\\GAMS_P\\Input_data\\Kalkulationsdaten\\Agrarstrukturerhebung\\Tierzahlen_bawue_März_2020")
}

translation<-read_excel("trans_adress_NUTS.xlsx",sheet = "Tabelle1",col_names = T )

# Lade Thuenen Data aus dem Agraratlas, Gorsvieheinheiten je kreis

if(laptob_work==TRUE) {
  setwd("C:\\Users\\User\\OneDrive - bwedu\\Dokumente\\Landwirtschaftliche Betriebslehre\\Projekt_P_Bawü\\Agraratlas_Daten_Thuenen")
} else {
  setwd("C:\\Users\\Tristan Herrmann\\OneDrive - bwedu\\Dokumente\\Landwirtschaftliche Betriebslehre\\Projekt_P_Bawü\\Agraratlas_Daten_Thuenen")
}

GV_thuenen<-read_excel("Kopie von Kreis_Tiere_LU_2020_Ext_TH_01_2023-06-21_12-49-16.xlsx",sheet = "Sheet1",col_names = T )
GV_thuenen


# 1.Use Grossvieheinheiten per kreis, was ich von Thuenen bekomme um basierden auf Nutztiere_RP_bawue die einzelnen Tierzahlen zu schaetzen

# filter nach Baden_wuertemberg kreisen
GV_thuenen_bawue<-translation %>%select(reg=Adressverzeichniss,Kreis_name:NUTS_2) %>%  left_join(GV_thuenen, by="reg")
GV_thuenen_bawue %>% print(n=Inf)

# Durchnittsdaten je Rasse für Bawue, kreisdurchschnitte sind nicht verfügbar, DAten für 2021, Seite 54
GV_thuenen_bawue<-GV_thuenen_bawue  %>% mutate(RP=case_when(
              
  NUTS_2 %in% c("DE111", "DE112", "DE113", "DE114", "DE115", "DE116", "DE117", "DE118", "DE119", "DE11A","DE11B","DE11C","DE11D") == TRUE  ~ "RP_Stuttgart", 
  NUTS_2 %in% c("DE121","DE122","DE123","DE124","DE125", "DE126", "DE127", "DE128", "DE129", "DE12A","DE12B", "DE12C") == TRUE  ~ "RP_Karlsruhe",
  NUTS_2 %in% c("DE131","DE132","DE133","DE134", "DE135","DE136","DE137","DE138","DE139","DE13A") == TRUE  ~ "RP_Freiburg",
  NUTS_2 %in% c("DE141","DE142","DE143","DE144","DE145","DE146","DE147","DE148","DE149") == TRUE  ~ "RP_Tuebingen"
  
))

###############################################################################################################################################################################################
# adjust POUF with POUF_T # not needed anymore

if(laptob_work==TRUE) {
  setwd("C:\\Users\\User\\OneDrive - bwedu\\Dokumente\\Landwirtschaftliche Betriebslehre\\Projekt_P_Bawü\\Agraratlas_Daten_Thuenen")
} else {
  setwd("C:\\Users\\Tristan Herrmann\\OneDrive - bwedu\\Dokumente\\Landwirtschaftliche Betriebslehre\\Projekt_P_Bawü\\Agraratlas_Daten_Thuenen")
}

GV_thuenen_gef<-read_excel("Kopie von Kreis_Tiere_LU_2020_Ext_TH_01_2023-06-21_12-49-16.xlsx",sheet = "Sheet1",col_names = T )
GV_thuenen_bawue_gef<-translation %>%select(reg=Adressverzeichniss,Kreis_name:NUTS_2) %>%  left_join(GV_thuenen_gef, by="reg")
GV_thuenen_bawue_gef %>% print(n=Inf)

# Durchnittsdaten je Rasse für Bawue, kreisdurchschnitte sind nicht verfügbar, DAten für 2021, Seite 54
GV_thuenen_bawue_gef<-GV_thuenen_bawue_gef  %>% mutate(RP=case_when(
  
  NUTS_2 %in% c("DE111", "DE112", "DE113", "DE114", "DE115", "DE116", "DE117", "DE118", "DE119", "DE11A","DE11B","DE11C","DE11D") == TRUE  ~ "RP_Stuttgart", 
  NUTS_2 %in% c("DE121","DE122","DE123","DE124","DE125", "DE126", "DE127", "DE128", "DE129", "DE12A","DE12B", "DE12C") == TRUE  ~ "RP_Karlsruhe",
  NUTS_2 %in% c("DE131","DE132","DE133","DE134", "DE135","DE136","DE137","DE138","DE139","DE13A") == TRUE  ~ "RP_Freiburg",
  NUTS_2 %in% c("DE141","DE142","DE143","DE144","DE145","DE146","DE147","DE148","DE149") == TRUE  ~ "RP_Tuebingen"
  
))

# comand can be pushed to the start of the ocument, where this dataset is finalized                  
nutztiere_kreise_bawue[nutztiere_kreise_bawue == "."] <- 0
nutztiere_kreise_bawue[nutztiere_kreise_bawue == "–"] <- NA

nutztiere_kreise_bawue %>% select(NUTS_2,region, mastgefluegel, truthuehner) %>% left_join(GV_thuenen_bawue_gef %>% select(NUTS_2, POUF), by="NUTS_2")

nutztiere_kreise_bawue %>% select(NUTS_2,region, mastgefluegel, truthuehner) %>% left_join(GV_thuenen_bawue_gef %>% select(NUTS_2, POUF), by="NUTS_2") %>%
  mutate(GV_mastgefluegel=as.numeric(mastgefluegel)*0.002,
         GV_truthuehner=as.numeric(truthuehner)*0.024) %>% 
  filter(!is.na(GV_mastgefluegel)) %>% filter(!is.na(GV_truthuehner)) %>%
  filter(GV_mastgefluegel>0 & GV_truthuehner>0)

nutztiere_kreise_bawue %>% select(NUTS_2,region, mastgefluegel, truthuehner) %>% left_join(GV_thuenen_bawue_gef %>% select(NUTS_2, POUF), by="NUTS_2") %>%
  mutate(GV_mastgefluegel=as.numeric(mastgefluegel)*0.002/1000,
         GV_truthuehner=as.numeric(truthuehner)*0.024/1000)%>%
  filter(!is.na(mastgefluegel)) %>%
  filter(mastgefluegel>0) %>%
  
  mutate(POUF_T = POUF- GV_mastgefluegel)




GV_factor<-read_excel("GV_factor_estimate.xlsx",sheet = "Tabelle1",col_names = T )

GV_factor<-GV_factor%>% mutate(reg=paste0(region1, paste0(region2)))

# GV_m

is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}


GV_factor %>%
  mutate(outlier = ifelse(is_outlier(GV_m), GV_m, as.numeric(NA))) %>% print(n=Inf)


GV_factor<-GV_factor %>%
  mutate(outlier_GV_t = ifelse(is_outlier(GV_t), GV_t, as.numeric(NA)))

GV_factor  %>% print(n=Inf)


GV_factor %>%
  mutate(outlier = ifelse(is_outlier(GV_m), GV_m, as.numeric(NA))) %>%
  ggplot(., aes(y = GV_m)) +
  geom_boxplot() +
  geom_text(aes(x=GV_m, label = outlier), na.rm = TRUE, hjust = -0.3)


GV_factor %>%
  mutate(outlier = ifelse(is_outlier(GV_t), GV_t, as.numeric(NA))) %>%
  ggplot(., aes(y = GV_t)) +
  geom_boxplot() +
  geom_text(aes(x=GV_t, label = outlier), na.rm = TRUE, hjust = -0.3)


ggplot(GV_factor, aes(y=GV_m))+
  geom_boxplot()


ggplot(GV_factor, aes(x=GV_m))+
  geom_histogram()

#GV_T
ggplot(GV_factor, aes(y=GV_t))+
  geom_boxplot()

ggplot(GV_factor, aes(x=GV_t))+
  geom_histogram()


## estimating GV_factor, and som visual checks
GV_factor %>% filter(is.na(outlier_GV_t)) %>% summarize(mean(GV_t),
                                                        median(GV_t),
                                                        sd(GV_t))


GV_factor %>%  summarize(mean(GV_t),
                        median(GV_t),
                        sd(GV_t))

GV_factor %>% filter(is.na(outlier_GV_t) | outlier_GV_t>0) %>% summarize(mean(GV_t),
                                                                        median(GV_t),
                                                                        sd(GV_t))


# extract GV_t aus POUF
GV_thuenen_bawue_gef
nutztiere_kreise_bawue %>% select(NUTS_2,region, mastgefluegel, truthuehner)

nutztiere_kreise_bawue$truthuehner <- as.numeric(nutztiere_kreise_bawue$truthuehner)

#RP stuttgart hat insgesamt 899,603 truthuehner, coloumn sum is 863,424
# it does not work das herauszurechnen 
899603-863424
nutztiere_kreise_bawue %>% select(NUTS_2,region, mastgefluegel, truthuehner) %>% left_join(GV_thuenen_bawue_gef %>% select(NUTS_2,RP, POUF), by="NUTS_2") %>% as_tibble() %>%
                           filter(RP=="RP_Stuttgart") %>% mutate(truthuehner_estimate=if_else(truthuehner==0, (899603-863424)/5, truthuehner))%>%
                           mutate(POUF_T=0.00564*as.numeric(truthuehner)/1000)


nutztiere_kreise_bawue
GV_thuenen_bawue %>% filter(NUTS_2=="DE111")


# assumption: basieren auf dem ktbl rechner ist ein Masthuhn 0.002 bis 0.0024 GVs, 
# so basierend auf den GV daten verteile ich jetzt einfach Masthuehner auf die Kreise, die somit auch Puten, Gaense etc beinhalten
# die finale Tierzahl sollte also wesentlich hoeher sein als vorher

# MAsthuhn 0.002-0.0024 GV schlachtgewicht 2-2.2 kg
# Pute 0.017-0.022 GV schlachtgewicht 20 kg
GV_thuenen_bawue %>% mutate(no._masthuehner_estimated= POUF*1000/0.0022) %>% left_join(nutztiere_kreise_bawue %>% select(NUTS_2, mastgefluegel:truthuehner), by="NUTS_2") %>% print(n=Inf)

no_masthuehner_estimate<-GV_thuenen_bawue %>% mutate(no._masthuehner_estimated= POUF*1000/0.0022) %>% left_join(nutztiere_kreise_bawue %>% select(NUTS_2, mastgefluegel:truthuehner), by="NUTS_2") %>% select(NUTS_2, no._masthuehner_estimated)
no_masthuehner_estimate

no_masthuehner_estimate$no._masthuehner_estimated<-round(no_masthuehner_estimate$no._masthuehner_estimated,digits = 0)




# #ktbl 0.02
# POUF_T  <-nutztiere_kreise_bawue %>% select(NUTS_2,region, mastgefluegel, truthuehner) %>% left_join(GV_thuenen_bawue_gef %>% select(NUTS_2, POUF), by="NUTS_2") %>%
#                                           mutate(GV_mastgefluegel=as.numeric(mastgefluegel)*0.008/1000) %>%
#                                           filter(!is.na(mastgefluegel)) %>%
#                                           filter(mastgefluegel>0) %>%
#                                           
#                                           mutate(POUF_T = POUF- GV_mastgefluegel)

### Join POUF_T
#GV_thuenen_bawue<-GV_thuenen_bawue %>% left_join(POUF_T %>% mutate(POUF_T_TRUE=TRUE) %>% select(NUTS_2, POUF_T, POUF_T_TRUE), by="NUTS_2")
#GV_thuenen_bawue<-GV_thuenen_bawue %>% rowwise()%>%mutate(POUF_2=POUF-POUF_T) %>% mutate(POUF_3=if_else(is.na(POUF_2)==T, POUF, POUF_2)) %>% select(-c(POUF,POUF_T,POUF_2)) %>% rename(POUF="POUF_3")



###########################################################################################################################################################################################


# Create the scaling factor for each column by RP# scaling factor ansatz

RP_stuttgart <- filter(GV_thuenen_bawue, RP=="RP_Stuttgart")
RP_karlsruhe <- filter(GV_thuenen_bawue, RP=="RP_Karlsruhe")
RP_freiburg <- filter(GV_thuenen_bawue, RP=="RP_Freiburg")
RP_tuebingen <- filter(GV_thuenen_bawue, RP=="RP_Tuebingen")


# Define the column names you want to scale
columns_to_scale <- c("DCOW", "SCOW", "BULL", "CALV", "HEIT", "SOWS", "PIGF", "HENS", "POUF", "SHGM", "OANI")

# Iterate over the columns and calculate the scaling factor
for (col in columns_to_scale) {
  RP_stuttgart[[paste0(col, "_scale")]] <- (1 / sum(RP_stuttgart[[col]],na.rm = T)) * RP_stuttgart[[col]]
}

for (col in columns_to_scale) {
  RP_karlsruhe[[paste0(col, "_scale")]] <- (1 / sum(RP_karlsruhe[[col]],na.rm = T)) * RP_karlsruhe[[col]]
}


for (col in columns_to_scale) {
  RP_freiburg[[paste0(col, "_scale")]] <- (1 / sum(RP_freiburg[[col]],na.rm = T)) * RP_freiburg[[col]]
}


for (col in columns_to_scale) {
  RP_tuebingen[[paste0(col, "_scale")]] <- (1 / sum(RP_tuebingen[[col]],na.rm = T)) * RP_tuebingen[[col]]
}


GV_thuenen_bawue<-rbind(RP_stuttgart, RP_karlsruhe, RP_freiburg, RP_tuebingen)


#check if scaling worked, all values must add up to one
test<-GV_thuenen_bawue %>% group_by(RP) %>% summarize(sum(DCOW_scale), sum(SCOW_scale), sum(BULL_scale), sum(CALV_scale), sum(HEIT_scale), sum(SOWS_scale, na.rm=T), sum(PIGF_scale), sum(HENS_scale), sum(POUF_scale), sum(SHGM_scale), sum(OANI_scale))
str(GV_thuenen_bawue)

GV_thuenen_bawue<-GV_thuenen_bawue %>% select(RP, reg:allYEAR,DCOW_scale:OANI_scale) 
GV_thuenen_bawue <- GV_thuenen_bawue[order(match(GV_thuenen_bawue$NUTS_2, NUTS_2_order)), ]
GV_thuenen_bawue


# finde die vermeintlich korrekte Equivalente in den Agrarstrukturdaten und codiere sie passend
nutztiere_RP_Bawue<-nutztiere_RP_Bawue %>% slice_head(n=4)
# muterkuehe und milchkuehe
nutztiere_RP_Bawue %>% select(region,cows, suckler_cows)
# kaelber
nutztiere_RP_Bawue %>% select(region,Kae_JuR_U1_male,Kae_JuR_U1_male_female)
#Faersen, JuR_Ue1_U2__female bisschen fragwuerdig, normalerweise ist eine Faerse Ü2 nicht 1-2 Jahre alt
nutztiere_RP_Bawue %>% select(region,JuR_U2_female ,JuR_Ue1_U2__female)
# Bullenmast
nutztiere_RP_Bawue %>% select(region,JuR_U2_male ,JuR_Ue1_U2_male)
# mastschweine
nutztiere_RP_Bawue %>% select(region,ferkel ,andere_schweine)
#zuchtsauen
nutztiere_RP_Bawue %>% select(region,zuchtsauen)
# legehennen
nutztiere_RP_Bawue %>% select(region,legehennen, junghennen)
# masthaehne_und_huehner
nutztiere_RP_Bawue %>% select(region,mastgefluegel)
str(nutztiere_RP_Bawue)

GV_thuenen_bawue

first_estimate_num<-GV_thuenen_bawue  %>% mutate(DCOW_num=case_when(
  
  RP %in% "RP_Stuttgart" == TRUE   ~ 93567*DCOW_scale, 
  RP %in% "RP_Karlsruhe" == TRUE   ~ 21782*DCOW_scale,
  RP %in% "RP_Freiburg" == TRUE    ~ 61966*DCOW_scale,
  RP %in% "RP_Tuebingen" == TRUE   ~ 147917*DCOW_scale
)) %>%
  
  mutate(SCOW_num=case_when(
    
    RP %in% "RP_Stuttgart" == TRUE   ~ 13910*SCOW_scale, 
    RP %in% "RP_Karlsruhe" == TRUE   ~ 6569*SCOW_scale,
    RP %in% "RP_Freiburg" == TRUE    ~ 20414*SCOW_scale,
    RP %in% "RP_Tuebingen" == TRUE   ~ 12554*SCOW_scale
  )) %>%

  mutate(CALV_num=case_when(
    
    RP %in% "RP_Stuttgart" == TRUE   ~ (34557+51542)*CALV_scale, 
    RP %in% "RP_Karlsruhe" == TRUE   ~ (9383+13368)*CALV_scale,
    RP %in% "RP_Freiburg" == TRUE    ~ (22805+33998)*CALV_scale,
    RP %in% "RP_Tuebingen" == TRUE   ~ (34012+69193)*CALV_scale
  )) %>% 

mutate(CALV_num_male=case_when(
    
    RP %in% "RP_Stuttgart" == TRUE   ~ (34557)*CALV_scale, 
    RP %in% "RP_Karlsruhe" == TRUE   ~ (9383)*CALV_scale,
    RP %in% "RP_Freiburg" == TRUE    ~ (22805)*CALV_scale,
    RP %in% "RP_Tuebingen" == TRUE   ~ (34012)*CALV_scale
  )) %>% 
  
mutate(CALV_num_female=case_when(
    
    RP %in% "RP_Stuttgart" == TRUE   ~ (51542)*CALV_scale, 
    RP %in% "RP_Karlsruhe" == TRUE   ~ (13368)*CALV_scale,
    RP %in% "RP_Freiburg" == TRUE    ~ (33998)*CALV_scale,
    RP %in% "RP_Tuebingen" == TRUE   ~ (69193)*CALV_scale
  )) %>%   
  
# HEIT including 1 bis 2 JAhre und über 2 jahre (nicht abgekalbt)
  mutate(HEIT_num=case_when(

    RP %in% "RP_Stuttgart" == TRUE   ~ (15480+47790)*HEIT_scale,
    RP %in% "RP_Karlsruhe" == TRUE   ~ (3983+13047)*HEIT_scale,
    RP %in% "RP_Freiburg" == TRUE    ~ (12018+30366)*HEIT_scale,
    RP %in% "RP_Tuebingen" == TRUE   ~ (22944+65489)*HEIT_scale
  )) %>%

mutate(HEIT_num_2jahre_aelter=case_when(
    
    RP %in% "RP_Stuttgart" == TRUE   ~ (15480)*HEIT_scale,
    RP %in% "RP_Karlsruhe" == TRUE   ~ (3983)*HEIT_scale,
    RP %in% "RP_Freiburg" == TRUE    ~ (12018)*HEIT_scale,
    RP %in% "RP_Tuebingen" == TRUE   ~ (22944)*HEIT_scale
  )) %>%
  
mutate(HEIT_num_ein_bis_jahre=case_when(
    
    RP %in% "RP_Stuttgart" == TRUE   ~ (47790)*HEIT_scale,
    RP %in% "RP_Karlsruhe" == TRUE   ~ (13047)*HEIT_scale,
    RP %in% "RP_Freiburg" == TRUE    ~ (30366)*HEIT_scale,
    RP %in% "RP_Tuebingen" == TRUE   ~ (65489)*HEIT_scale
  )) %>%

# Heit nur ueber 2 Jahre
  # mutate(HEIT_num=case_when(
  #   
  #   RP %in% "RP_Stuttgart" == TRUE   ~ (15480)*HEIT_scale, 
  #   RP %in% "RP_Karlsruhe" == TRUE   ~ (3983)*HEIT_scale,
  #   RP %in% "RP_Freiburg" == TRUE    ~ (12018)*HEIT_scale,
  #   RP %in% "RP_Tuebingen" == TRUE   ~ (22944)*HEIT_scale
  # )) %>%  
  

mutate(BULL_num=case_when(
    
    RP %in% "RP_Stuttgart" == TRUE   ~ (2107+22619)*BULL_scale, 
    RP %in% "RP_Karlsruhe" == TRUE   ~ (895+6377)*BULL_scale,
    RP %in% "RP_Freiburg" == TRUE    ~ (2104+12847)*BULL_scale,
    RP %in% "RP_Tuebingen" == TRUE   ~ (1847+21278)*BULL_scale
  )) %>%
  
mutate(BULL_num_2jahre_aelter=case_when(
    
    RP %in% "RP_Stuttgart" == TRUE   ~ (2107)*BULL_scale, 
    RP %in% "RP_Karlsruhe" == TRUE   ~ (895)*BULL_scale,
    RP %in% "RP_Freiburg" == TRUE    ~ (2104)*BULL_scale,
    RP %in% "RP_Tuebingen" == TRUE   ~ (1847)*BULL_scale
  )) %>%
  
mutate(BULL_num_ein_bis_jahre=case_when(
    
    RP %in% "RP_Stuttgart" == TRUE   ~ (22619)*BULL_scale, 
    RP %in% "RP_Karlsruhe" == TRUE   ~ (6377)*BULL_scale,
    RP %in% "RP_Freiburg" == TRUE    ~ (12847)*BULL_scale,
    RP %in% "RP_Tuebingen" == TRUE   ~ (21278)*BULL_scale
  )) %>%

# # ferkel und andere schweine
# mutate(PIGF_num=case_when(
#     
#     RP %in% "RP_Stuttgart" == TRUE   ~ (392170+441896)*PIGF_scale, 
#     RP %in% "RP_Karlsruhe" == TRUE   ~ (17369+47791)*PIGF_scale,
#     RP %in% "RP_Freiburg" == TRUE    ~ (32877+62365)*PIGF_scale,
#     RP %in% "RP_Tuebingen" == TRUE   ~ (222692+316784)*PIGF_scale
#   )) %>%

# nur andere schweine, mastschweine
mutate(PIGF_num=case_when(
    
    RP %in% "RP_Stuttgart" == TRUE   ~ (441896)*PIGF_scale, 
    RP %in% "RP_Karlsruhe" == TRUE   ~ (47791)*PIGF_scale,
    RP %in% "RP_Freiburg" == TRUE    ~ (62365)*PIGF_scale,
    RP %in% "RP_Tuebingen" == TRUE   ~ (316784)*PIGF_scale
  )) %>%
  
# zuchtsauen
mutate(SOWS_num=case_when(

    RP %in% "RP_Stuttgart" == TRUE   ~ (80943)*SOWS_scale,
    RP %in% "RP_Karlsruhe" == TRUE   ~ (3583)*SOWS_scale,
    RP %in% "RP_Freiburg" == TRUE    ~ (8315)*SOWS_scale,
    RP %in% "RP_Tuebingen" == TRUE   ~ (43624)*SOWS_scale
  )) %>%

#ferkel sind in GV_pigf enthalten, wereden aber weiterhin so beahndelt
# mutate(FERKEL_num=case_when(
# 
#   RP %in% "RP_Stuttgart" == TRUE   ~ (392170)*PIGF_scale,
#   RP %in% "RP_Karlsruhe" == TRUE   ~ (17369)*PIGF_scale,
#   RP %in% "RP_Freiburg" == TRUE    ~ (32877)*PIGF_scale,
#   RP %in% "RP_Tuebingen" == TRUE   ~ (222692)*PIGF_scale
# )) %>%
  
  
# hennen
mutate(JUNGHENS_num=case_when(
    
    RP %in% "RP_Stuttgart" == TRUE   ~ (51271)*HENS_scale, 
    RP %in% "RP_Karlsruhe" == TRUE   ~ (3488)*HENS_scale,
    RP %in% "RP_Freiburg" == TRUE    ~ (10197)*HENS_scale,
    RP %in% "RP_Tuebingen" == TRUE   ~ (367640)*HENS_scale
  )) %>%
  
mutate(HENS_num=case_when(
    
    RP %in% "RP_Stuttgart" == TRUE   ~ (1339966)*HENS_scale, 
    RP %in% "RP_Karlsruhe" == TRUE   ~ (287792)*HENS_scale,
    RP %in% "RP_Freiburg" == TRUE    ~ (759335)*HENS_scale,
    RP %in% "RP_Tuebingen" == TRUE   ~ (853703)*HENS_scale
  )) %>%
#mastgefluegel, for now only masthuehner und haehne, aber koenten auch puten, gaense, enten mitreinschieben
mutate(POUF_num=case_when(
    
    RP %in% "RP_Stuttgart" == TRUE   ~ (688736)*POUF_scale, 
    RP %in% "RP_Karlsruhe" == TRUE   ~ (4430)*POUF_scale,
    RP %in% "RP_Freiburg" == TRUE    ~ (26548)*POUF_scale,
    RP %in% "RP_Tuebingen" == TRUE   ~ (648454)*POUF_scale
  )) %>% 

select(RP:allYEAR, DCOW_num:POUF_num)

first_estimate_num

# Fuege schafe, Pferde und Ziegen zum estimate dazu, danach ist aber sense
nutztiere_kreise_bawue %>% str()

first_estimate_num <- first_estimate_num %>%
left_join(nutztiere_kreise_bawue %>% select(NUTS_2 , total_pferde, total_ziegen, total_milch_mutterschafe, lambs=schafe_U1, boecke_hammel_other), by="NUTS_2") 


#############################################################################################################################################
# Do the estimations in accordance with thuenen, emission inventroy

#Calves factor 4/12

first_estimate_num<-first_estimate_num %>% mutate(calves_EI=4/12*CALV_num)

# total amount of heifers
first_estimate_num<-first_estimate_num %>% mutate(total_heifers_EI= CALV_num*((1-4/12)*0.6)+ HEIT_num_ein_bis_jahre+HEIT_num_2jahre_aelter) 
first_estimate_num %>% summarize(sum(total_heifers_EI, na.rm=T))


# dairy heifers & female beef cattle
first_estimate_num<-first_estimate_num %>% mutate(female_beef_cattle=total_heifers_EI*11.94/100) %>% mutate(dairy_heifers=total_heifers_EI-female_beef_cattle)
first_estimate_num %>% select(total_heifers_EI, female_beef_cattle, dairy_heifers) %>% summarize(across(total_heifers_EI:dairy_heifers, sum))


#male beef cattle
first_estimate_num %>% mutate(MaleBeefCattle= CALV_num*((1-4/12)*0.4)+ BULL_num_2jahre_aelter+ BULL_num_ein_bis_jahre)%>%
                       select(MaleBeefCattle) %>% summarize(sum(MaleBeefCattle))
                  
first_estimate_num <-first_estimate_num %>% mutate(MaleBeefCattle= CALV_num*((1-4/12)*0.4)+ BULL_num_2jahre_aelter+ BULL_num_ein_bis_jahre)

#### DEaling with ferkel, zuchtsauen etc

nutztiere_kreise_bawue$ferkel <- as.numeric(nutztiere_kreise_bawue$ferkel)

#nutztiere_kreise_bawue %>% filter(RP=="RP_Stuttgart") %>% select(ferkel)

nutztiere_kreise_bawue<-nutztiere_kreise_bawue  %>% mutate(RP=case_when(
  
  NUTS_2 %in% c("DE111", "DE112", "DE113", "DE114", "DE115", "DE116", "DE117", "DE118", "DE119", "DE11A","DE11B","DE11C","DE11D") == TRUE  ~ "RP_Stuttgart", 
  NUTS_2 %in% c("DE121","DE122","DE123","DE124","DE125", "DE126", "DE127", "DE128", "DE129", "DE12A","DE12B", "DE12C") == TRUE  ~ "RP_Karlsruhe",
  NUTS_2 %in% c("DE131","DE132","DE133","DE134", "DE135","DE136","DE137","DE138","DE139","DE13A") == TRUE  ~ "RP_Freiburg",
  NUTS_2 %in% c("DE141","DE142","DE143","DE144","DE145","DE146","DE147","DE148","DE149") == TRUE  ~ "RP_Tuebingen"
  
))

# see excel kreis complete um zu sehen woher die zahlen kommen, letztendlich werden die restmengen vertielt auf die kreise die ungleich 0 sind
ferkel_estimate<-nutztiere_kreise_bawue %>% select(NUTS_2, region, RP,ferkel) %>% as_tibble() %>% filter(RP=="RP_Stuttgart") %>%
  mutate(ferkel_estimate= if_else(ferkel==0, (392170-388221)/2, ferkel)) %>%
  
  
  rbind(nutztiere_kreise_bawue %>% select(NUTS_2, region, RP,ferkel) %>% as_tibble() %>% filter(RP=="RP_Karlsruhe") %>%
         mutate(ferkel_estimate= if_else(ferkel==0, (17369-16084)/2, ferkel)  )) %>%
  
  
  rbind(nutztiere_kreise_bawue %>% select(NUTS_2, region, RP,ferkel) %>% as_tibble() %>% filter(RP=="RP_Freiburg") %>%
        mutate(ferkel_estimate= if_else(ferkel==0, (32877-32380)/2, ferkel) )) %>%
  
  
  rbind(nutztiere_kreise_bawue %>% select(NUTS_2, region, RP,ferkel) %>% as_tibble() %>% filter(RP=="RP_Tuebingen") %>%
         mutate(ferkel_estimate= ferkel) )


first_estimate_num <-first_estimate_num %>% left_join(ferkel_estimate%>% select(NUTS_2, ferkel_estimate), by="NUTS_2")

### schetzung fuer suckling pigs von ferkel abziehen
# piglets abgezogen von mastschweinen
first_estimate_num <- first_estimate_num  %>% mutate(n_piglets=0.072199*PIGF_num) 

# suckling pigs abezogen von ferkel und aufschlag aus npiglets
first_estimate_num <- first_estimate_num %>% mutate(suckling_pigs=0.5771*as.numeric(ferkel_estimate)) %>% mutate(ferkel=ferkel_estimate-suckling_pigs+n_piglets)
first_estimate_num %>% summarize(sum(ferkel, na.rm=T))

first_estimate_num %>% select(PIGF_num) %>% mutate(n_piglets=0.072199*PIGF_num) %>% summarize(sum(n_piglets, na.rm=T))

first_estimate_num %>% mutate(PIGF_T =PIGF_num-n_piglets) %>% summarize(sum(PIGF_T))

options(scipen = 999)

# get the masthuehner estimate inside, oder anders gesagt mastgefluegel ausgedrückt in masthuehner

first_estimate_num<-first_estimate_num %>% left_join(no_masthuehner_estimate, by="NUTS_2")


#first_estimate_num$DCOW_num<-round(first_estimate_num$DCOW_num,digits = 0)
#first_estimate_num$SCOW_num<-round(first_estimate_num$SCOW_num,digits = 0)
#first_estimate_num$CALV_num<-round(first_estimate_num$CALV_num,digits = 0)
#first_estimate_num$HEIT_num<-round(first_estimate_num$HEIT_num,digits = 0)
#first_estimate_num$BULL_num<-round(first_estimate_num$BULL_num,digits = 0)
#first_estimate_num$PIGF_num<-round(first_estimate_num$PIGF_num,digits = 0)
#first_estimate_num$SOWS_num<-round(first_estimate_num$SOWS_num,digits = 0)
#first_estimate_num$HENS_num<-round(first_estimate_num$HENS_num,digits = 0)
#first_estimate_num$POUF_num<-round(first_estimate_num$POUF_num,digits = 0)

first_estimate_num %>% group_by(RP) %>% summarize(sum(DCOW_num))
first_estimate_num %>% print(n=Inf)


# Time to compare and validate the data one by one
nutztiere_kreise_bawue

# ckeck DCOW --- The cow estimate is nearly perfect
first_estimate_num %>% select(RP,Kreis_name, NUTS_2, DCOW_num) %>% left_join(nutztiere_kreise_bawue %>% select(NUTS_2, cows), by="NUTS_2", suffix=c("_estimate","_real")) %>% 
                       mutate(diff=DCOW_num -as.numeric(cows)) %>%print(n=Inf)


# ckeck SCOW --- The suckler cow estimate fits perfectly
first_estimate_num %>% select(RP,Kreis_name, NUTS_2, SCOW_num) %>% left_join(nutztiere_kreise_bawue %>% select(NUTS_2, suckler_cows), by="NUTS_2", suffix=c("_estimate","_real")) %>% 
                        mutate(diff=SCOW_num -as.numeric(suckler_cows)) %>%print(n=Inf)

# ckeck CALV --- The CALV estimate fits perfectly
first_estimate_num %>% select(RP,Kreis_name, NUTS_2, CALV_num) %>% left_join(nutztiere_kreise_bawue %>% select(NUTS_2, Kae_JuR_U1_male,Kae_JuR_U1_male_female), by="NUTS_2", suffix=c("_estimate","_real")) %>% 
                       mutate(diff=CALV_num -(as.numeric(Kae_JuR_U1_male)+ as.numeric(Kae_JuR_U1_male_female))) %>%print(n=Inf)

#check HEIT (FAerse) --- not as perfect as before, but still good :)
first_estimate_num %>% select(RP,Kreis_name, NUTS_2, HEIT_num) %>% left_join(nutztiere_kreise_bawue %>% select(NUTS_2, JuR_U2_female,JuR_Ue1_U2__female), by="NUTS_2", suffix=c("_estimate","_real")) %>% 
                        mutate(diff=HEIT_num -(as.numeric(JuR_U2_female) + as.numeric(JuR_Ue1_U2__female))) %>%print(n=Inf)


#check BULL  --- not as perfect as cowsff before, but still good :)
first_estimate_num %>% select(RP,Kreis_name, NUTS_2, BULL_num) %>% left_join(nutztiere_kreise_bawue %>% select(NUTS_2, JuR_U2_male,JuR_Ue1_U2_male), by="NUTS_2", suffix=c("_estimate","_real")) %>% 
                        mutate(diff=BULL_num -(as.numeric(JuR_U2_male) + as.numeric(JuR_Ue1_U2_male))) %>%print(n=Inf)


#check PIGF  --- matches perfectly, note: ferkel are not included here, nur "andere_schweine"
first_estimate_num %>% select(RP,Kreis_name, NUTS_2, PIGF_num) %>% left_join(nutztiere_kreise_bawue %>% select(NUTS_2,  andere_schweine), by="NUTS_2", suffix=c("_estimate","_real")) %>% 
                       mutate(diff=PIGF_num -( as.numeric(andere_schweine))) %>%print(n=Inf)


#check SOWS  --- matches good enough
# zuxhtsauen
first_estimate_num %>% select(RP,Kreis_name, NUTS_2, SOWS_num) %>% left_join(nutztiere_kreise_bawue %>% select(NUTS_2,  zuchtsauen), by="NUTS_2", suffix=c("_estimate","_real")) %>% 
                       mutate(diff=SOWS_num -( as.numeric(zuchtsauen))) %>%print(n=Inf)


# zuchtsauen und ferkel --- does not work... 
first_estimate_num %>% select(RP,Kreis_name, NUTS_2, SOWS_num) %>% left_join(nutztiere_kreise_bawue %>% select(NUTS_2, ferkel,zuchtsauen), by="NUTS_2", suffix=c("_estimate","_real")) %>% 
                      mutate(diff=SOWS_num -(as.numeric(ferkel) + as.numeric(zuchtsauen))) %>%print(n=Inf)


# legehennen und junghennen --- is good enough...
first_estimate_num %>% select(RP,Kreis_name, NUTS_2, HENS_num) %>% left_join(nutztiere_kreise_bawue %>% select(NUTS_2, junghennen,legehennen), by="NUTS_2", suffix=c("_estimate","_real")) %>% 
                        mutate(diff=HENS_num -(as.numeric(junghennen) + as.numeric(legehennen))) %>%print(n=Inf)


first_estimate_num %>% select(RP,Kreis_name, NUTS_2, HENS_num) %>% left_join(nutztiere_kreise_bawue %>% select(NUTS_2, legehennen), by="NUTS_2", suffix=c("_estimate","_real")) %>% 
                        mutate(diff=HENS_num -(as.numeric(legehennen))) %>%print(n=Inf)

# die ferkelaufzucht ist in PIGF enthalten

# first_estimate_num %>% select(RP,Kreis_name, NUTS_2, FERKEL_num) %>% left_join(nutztiere_kreise_bawue %>% 
#                       select(NUTS_2, ferkel), by="NUTS_2", suffix=c("_estimate","_real")) %>% 
#   mutate(diff=FERKEL_num -(as.numeric(ferkel))) %>%print(n=Inf)

# mastgeflügel, in diesm fall nur masthuehner und haehne
# first_estimate_num %>% select(RP,Kreis_name, NUTS_2,POUF_T_TRUE, POUF_num) %>% left_join(nutztiere_kreise_bawue %>% select(NUTS_2, mastgefluegel), by="NUTS_2", suffix=c("_estimate","_real")) %>% 
#   mutate(diff=POUF_num -(as.numeric(mastgefluegel))) %>%filter(POUF)     %>%print(n=Inf)
# 
# 
# first_estimate_num %>% select(RP,Kreis_name, NUTS_2, POUF_num) %>% left_join(nutztiere_kreise_bawue %>% select(NUTS_2, mastgefluegel), by="NUTS_2", suffix=c("_estimate","_real")) %>% 
#   mutate(diff=POUF_num -(as.numeric(mastgefluegel))) %>%filter(POUF)   %>% mutate(percent_diff=diff/as.numeric(mastgefluegel))  %>%print(n=Inf)
# 
# 
# 
# first_estimate_num %>% select(RP,Kreis_name, NUTS_2, POUF_num) %>% left_join(nutztiere_kreise_bawue %>% select(NUTS_2, mastgefluegel, truthuehner), by="NUTS_2", suffix=c("_estimate","_real")) %>% 
#   mutate(diff=POUF_num -(as.numeric(mastgefluegel))) %>%  filter(!is.na(diff))  %>% mutate(check=(as.numeric(mastgefluegel))+as.numeric(truthuehner)) %>%  print(n=Inf)

# this is now a very good result, an estimate of the number of animals per region, only thin that is missing is the amount of ferkel
# hier sollte ich nachfragen, warum ferkel nicht erfasst wurde... bei sebastian oder ob das schon innherhalb einer der suaen oder mastschweine miteinfließt

first_estimate_num %>% select(-POUF_num)
nutztiere_kreise_bawue
nutztiere_RP_Bawue %>% select(region,ferkel)

# nutztiere_kreise_bawue<-nutztiere_kreise_bawue  %>% mutate(RP=case_when(
#   
#   NUTS_2 %in% c("DE111", "DE112", "DE113", "DE114", "DE115", "DE116", "DE117", "DE118", "DE119", "DE11A","DE11B","DE11C","DE11D") == TRUE  ~ "RP_Stuttgart", 
#   NUTS_2 %in% c("DE121","DE122","DE123","DE124","DE125", "DE126", "DE127", "DE128", "DE129", "DE12A","DE12B", "DE12C") == TRUE  ~ "RP_Karlsruhe",
#   NUTS_2 %in% c("DE131","DE132","DE133","DE134", "DE135","DE136","DE137","DE138","DE139","DE13A") == TRUE  ~ "RP_Freiburg",
#   NUTS_2 %in% c("DE141","DE142","DE143","DE144","DE145","DE146","DE147","DE148","DE149") == TRUE  ~ "RP_Tuebingen"
#   
# ))

# nutztiere_kreise_bawue$ferkel <- as.numeric(nutztiere_kreise_bawue$ferkel)
# 
# nutztiere_kreise_bawue %>% filter(RP=="RP_Stuttgart") %>% select(ferkel)
# 
# 
# # see excel kreis complete um zu sehen woher die zahlen kommen, letztendlich werden die restmengen vertielt auf die kreise die ungleich 0 sind
# ferkel_estimate<-nutztiere_kreise_bawue %>% select(NUTS_2, region, RP,ferkel) %>% as_tibble() %>% filter(RP=="RP_Stuttgart") %>%
#                            mutate(ferkel_estimate= if_else(ferkel==0, (392170-388221)/2, ferkel)) %>%
# 
# 
# rbind(nutztiere_kreise_bawue %>% select(NUTS_2, region, RP,ferkel) %>% as_tibble() %>% filter(RP=="RP_Karlsruhe") %>%
#   mutate(ferkel_estimate= if_else(ferkel==0, (17369-16084)/2, ferkel)  )) %>%
#   
#   
# rbind(nutztiere_kreise_bawue %>% select(NUTS_2, region, RP,ferkel) %>% as_tibble() %>% filter(RP=="RP_Freiburg") %>%
#          mutate(ferkel_estimate= if_else(ferkel==0, (32877-32380)/2, ferkel) )) %>%
#   
# 
# rbind(nutztiere_kreise_bawue %>% select(NUTS_2, region, RP,ferkel) %>% as_tibble() %>% filter(RP=="RP_Tuebingen") %>%
#         mutate(ferkel_estimate= ferkel) )
# 
# 
# first_estimate_num <-first_estimate_num %>% left_join(ferkel_estimate%>% select(NUTS_2, ferkel_estimate), by="NUTS_2")
first_estimate_num <- first_estimate_num %>% mutate(unit="No_of_animals")
first_estimate_num%>% print(n=Inf)

if(laptob_work==TRUE) {
  write_xlsx(x=first_estimate_num, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/18.04.23/Tierzahlen_kreis_full.xlsx", col_names = TRUE)
  
} else {
  write_xlsx(x=first_estimate_num, path = "C:/Users/Tristan Herrmann/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/18.04.23/Tierzahlen_kreis_full.xlsx", col_names = TRUE)
}  
  
# FIRST big result: anzahl der Tiere je region, "sauber" geschaetzt ueber die GV_einheiten des thuenen instituts
#############################################################################################################################################################################################################

rm(ferkel_estimate, GV_factor, GV_thuenen, GV_thuenen_bawue, GV_thuenen_bawue_gef, GV_thuenen_gef,no_masthuehner_estimate,nutztiere_bawue, nutztiere_kreise_bawue, nutztiere_RP_Bawue, RP_freiburg, RP_karlsruhe, RP_stuttgart,test, RP_tuebingen, translation)

# NEXT Block: aus dden Tierzahlen NPK ableiten, sam procedure as before


# milchkuehe, DCOW
# 1. The amount of organic fertilizer depends on the intensity of milk production (how many kg per year)

if(laptob_work==TRUE) {
  setwd("C:\\Users\\User\\OneDrive - bwedu\\Dokumente\\Landwirtschaftliche Betriebslehre\\Projekt_P_Bawü\\GAMS_P\\Input_data\\Kalkulationsdaten\\LKV_Milchleistungspruefung")
} else {
  setwd("C:\\Users\\Tristan Herrmann\\OneDrive - bwedu\\Dokumente\\Landwirtschaftliche Betriebslehre\\Projekt_P_Bawü\\GAMS_P\\Input_data\\Kalkulationsdaten\\LKV_Milchleistungspruefung")
}


avg_milk_production <- read_excel("Milchleistung_je_kreis.xlsx",sheet = "Durchschnittsleistung" )

glimpse(avg_milk_production)


avg_milk_production$`Milch kg`<-   gsub(" ","",avg_milk_production$`Milch kg`)
avg_milk_production$`Milch kg` <- as.numeric(avg_milk_production$`Milch kg`)

avg_milk_production$`MLP Kuehe 09.2019`<-   gsub(" ","",avg_milk_production$`MLP Kuehe 09.2019`)
avg_milk_production$`MLP Kuehe 09.2019` <- as.numeric(avg_milk_production$`MLP Kuehe 09.2019`)

glimpse(avg_milk_production)


avg_milk_production_2021<- avg_milk_production %>% filter(Jahr=="2021")
rm(avg_milk_production)
glimpse(avg_milk_production_2021)

#2. The cow races that are present in the regions, this influences the performance and average milk production
# potenziell denkbar Holstein und Fleckvieh zu unterscheiden, da KTBL Daten verfügbar für beide für Wirtschaftsdüngeranfall
# und anteil je kreis auch verfügbar LKV Jahresbericht page 69

# adding the race proportions by region for holstein, fleckvieh, and braunvieh
race_proportion <- read_excel("Milchleistung_je_kreis.xlsx",sheet = "Verteilung_Rasse_per_kreis")
glimpse(race_proportion)

#looping
# Create a vector of the variable names we want to modify
var_names <- c("Fleckvieh %", "Braunvieh %", "Holstein-sbt %", "Holsteins-Rbt", "Vorderwälder %", "Hinterwälder %")

# Loop through each variable name and modify the column in the data frame
# The double square brackets make the save to the dataframe
# Ersetzt komma durch punkt, dass ich das agnze zur Zahl machen kann
for (var in var_names) {
  # Replace commas with periods
  race_proportion[[var]] <- gsub(",", ".", race_proportion[[var]])
  
  # Convert to numeric
  race_proportion[[var]] <- as.numeric(race_proportion[[var]])
}

race_proportion


milchkuehe<- first_estimate_num %>% select(NUTS_2, region=Kreis_name, cows=DCOW_num)


#### Now I have three datasets
avg_milk_production_2021 %>% print(n=Inf)
race_proportion %>% print(n=Inf)
milchkuehe %>% print(n=Inf)
#########################

# filling the gaps
# stadtkreise kriegen wenn in der tierzaehlung vorhanden die selben shares wie der landkreis, oder der direkt umschließende Kreis

# race proportions die für die landkreise gelten, gelten auch fuer die stadtkreise (von landkreisen umschlossen)
race_proportion<-add_row(race_proportion, race_proportion %>% filter(Region=="Heilbronn") %>% select(`Fleckvieh %`:`Hinterwälder %`) %>% 
                           mutate(Region="Heilbronn, stadtkreis",
                                  NUTS_2="DE117"))

race_proportion<-add_row(race_proportion, race_proportion %>% filter(Region=="Rastatt") %>% select(`Fleckvieh %`:`Hinterwälder %`) %>% 
                           mutate(Region="Stadtkreis, Baden Baden",
                                  NUTS_2="DE121"))

race_proportion<-add_row(race_proportion, race_proportion %>% filter(Region=="Karlsruhe") %>% select(`Fleckvieh %`:`Hinterwälder %`) %>% 
                           mutate(Region="Stadtkreis, Karlsruhe",
                                  NUTS_2="DE122"))

race_proportion<-add_row(race_proportion, race_proportion %>% filter(Region=="Alb-Donau-Kreis") %>% select(`Fleckvieh %`:`Hinterwälder %`) %>% 
                           mutate(Region="Ulm. Stadtkreis",
                                  NUTS_2="DE144"))

race_proportion<-add_row(race_proportion, race_proportion %>% filter(Region=="Rhein-Neckar") %>% select(`Fleckvieh %`:`Hinterwälder %`) %>% 
                           mutate(Region="Heidelberg, Stadtkreis",
                                  NUTS_2="DE125"))

race_proportion<-add_row(race_proportion, race_proportion %>% filter(Region=="Rhein-Neckar") %>% select(`Fleckvieh %`:`Hinterwälder %`) %>% 
                           mutate(Region="Mannheim, Stadtkreis",
                                  NUTS_2="DE126"))

race_proportion<-add_row(race_proportion, race_proportion %>% filter(Region=="Breisgau-Hochschw.") %>% select(`Fleckvieh %`:`Hinterwälder %`) %>% 
                           mutate(Region="Freiburg, Stadtkreis",
                                  NUTS_2="DE131"))

race_proportion<-add_row(race_proportion, race_proportion %>% filter(Region=="Enzkreis") %>% select(`Fleckvieh %`:`Hinterwälder %`) %>% 
                           mutate(Region="Pforzheim",
                                  NUTS_2="DE129"))


milchkuehe %>% print(n=Inf)
race_proportion %>% print(n=Inf)

# replacing NAs in race proportion with 0
race_proportion<-  replace_na(race_proportion, list(`Braunvieh %` = 0))
race_proportion<-  replace_na(race_proportion, list(`Holstein-sbt %` = 0))
race_proportion<-  replace_na(race_proportion, list(`Holsteins-Rbt` = 0))
race_proportion<-  replace_na(race_proportion, list(`Vorderwälder %` = 0))
race_proportion<-  replace_na(race_proportion, list(`Hinterwälder %` = 0))


# Tierrassenanteile und kreise werden gejoined
milchkuehe<-milchkuehe %>% left_join(race_proportion %>% select(-Region), by="NUTS_2")
milchkuehe %>% print(n=Inf)

# Problem: die prozentzahlen summieren sich nicht zu 100%, und im RP Freiburg spielt die Vorderwaelder-RAsse eine besondere Rolle, die sollte dort beruecksichtigt werden
Rest <- milchkuehe %>% filter(!NUTS_2 %in% c("DE131", "DE132", "DE133", "DE134", "DE135", "DE136", "DE137", "DE13A", "DE139", "DE138"))
freiburg <- milchkuehe %>% filter(NUTS_2 %in% c("DE131", "DE132", "DE133", "DE134", "DE135", "DE136", "DE137", "DE13A", "DE139", "DE138"))

Rest<-Rest %>% rowwise()%>%mutate(check=sum(`Fleckvieh %`,`Holsteins-Rbt`,`Holstein-sbt %`, `Braunvieh %`)) %>% mutate(diff=100-check)

freiburg<-freiburg %>% rowwise()%>%mutate(check=sum(`Fleckvieh %`,`Holsteins-Rbt`,`Holstein-sbt %`, `Braunvieh %`, `Vorderwälder %`)) %>% mutate(diff=100-check)


# easy approach: diff wird einfach proportional auf alle Rassen aufgeschlagen
# Proportionales aufschlage der Prozentzahlen, sodass 100% entstehen
Rest<-Rest  %>% mutate(`Fleckvieh %`=`Fleckvieh %`+diff/4,
                       `Braunvieh %`=`Braunvieh %`+diff/4,
                       `Holsteins-Rbt`=`Holsteins-Rbt`+diff/4,
                       `Holstein-sbt %`=`Holstein-sbt %`+diff/4)



freiburg<-freiburg  %>% mutate(`Fleckvieh %`=`Fleckvieh %`+diff/5,
                               `Braunvieh %`=`Braunvieh %`+diff/5,
                               `Holsteins-Rbt`=`Holsteins-Rbt`+diff/5,
                               `Holstein-sbt %`=`Holstein-sbt %`+diff/5,
                               `Vorderwälder %`=`Vorderwälder %`+diff/5)





Rest %>% rowwise()%>%mutate(check=sum(`Fleckvieh %`,`Holsteins-Rbt`,`Holstein-sbt %`, `Braunvieh %`)) %>% mutate(diff=100-check) %>% print(n=Inf)
freiburg %>% rowwise()%>%mutate(check=sum(`Fleckvieh %`,`Holsteins-Rbt`,`Holstein-sbt %`, `Braunvieh %`, `Vorderwälder %`)) %>% mutate(diff=100-check) %>%  print(n=Inf)

rbind(Rest,freiburg) %>% print(n=Inf)
milchkuehe <- rbind(Rest,freiburg)

rm(Rest,freiburg)

# check, does it now add up to 100?
adjusted_race_proportions <- milchkuehe %>% select(NUTS_2:`Vorderwälder %`)
adjusted_race_proportions %>% mutate(sum(`Braunvieh %`,`Fleckvieh %`, `Holstein-sbt %`,`Holsteins-Rbt`, `Vorderwälder %`)) %>% print(n=Inf)


milchkuehe<-milchkuehe %>% select(-c(diff, check, `Hinterwälder %`))
milchkuehe %>% print(n=Inf)

# splitting the cows by races
# Anzahl der Kühe je Kreis, je Rasse
milchkuehe<-milchkuehe %>% mutate(Fleckvieh=cows*`Fleckvieh %`/100,
                                  Braunvieh=cows*`Braunvieh %`/100,
                                  Holstein_rbt=cows*`Holsteins-Rbt`/100,
                                  Holstein_sbt=cows*`Holstein-sbt %`/100,
                                  Vorderwaelder=cows*`Vorderwälder %`/100)

#milchkuehe
#milchkuehe$Fleckvieh<-round(milchkuehe$Fleckvieh,digits = 0)
#milchkuehe$Braunvieh<-round(milchkuehe$Braunvieh,digits = 0)
#milchkuehe$Holstein_rbt<-round(milchkuehe$Holstein_rbt,digits = 0)
#milchkuehe$Holstein_sbt<-round(milchkuehe$Holstein_sbt,digits = 0)
#milchkuehe$Vorderwaelder<-round(milchkuehe$Vorderwaelder,digits = 0)

milchkuehe %>% print(n=Inf)
# DONE! Milchkühe je Region splitted by 5 different races

# Bezugnahme zur Milchleistung je Rasse

# nach der Hochrechnung der Milchleistung über die Rasse, wie weit bin ich weg von von dem LKV average je kreis?
#step2 avg_milchleistung nach Rasse gewichten, sodass der gesamt average per region jedoch gewahrt wird
if(laptob_work==TRUE) {
  setwd("C:\\Users\\User\\OneDrive - bwedu\\Dokumente\\Landwirtschaftliche Betriebslehre\\Projekt_P_Bawü\\GAMS_P\\Input_data\\Kalkulationsdaten\\LKV_Milchleistungspruefung")
  
} else {
  setwd("C:\\Users\\Tristan Herrmann\\OneDrive - bwedu\\Dokumente\\Landwirtschaftliche Betriebslehre\\Projekt_P_Bawü\\GAMS_P\\Input_data\\Kalkulationsdaten\\LKV_Milchleistungspruefung")
}


avg_milk_race <- read_excel("Milchleistung_je_kreis.xlsx",sheet = "avg_leistung_rasse_2021" )
avg_milk_race 

# pivot tbl for subsequent join 
milchkuehe<-milchkuehe %>% select(NUTS_2:region, Fleckvieh:Vorderwaelder) %>% pivot_longer(cols = c(Fleckvieh, Braunvieh, Holstein_rbt, Holstein_sbt, Vorderwaelder), names_to = "Race")
milchkuehe <- milchkuehe %>% rename(No_animals=value)
milchkuehe
milchkuehe %>% print(n=Inf)

# join with kreis average for milk performance, rassenspezifischer durchschnitt milchleistung in BaWue
milchkuehe_2 <-milchkuehe %>% mutate(avg_milk_production_race=case_when(
              Race=='Fleckvieh'       ~ 7966,  #Annahme Durchschnitt seite 54
              Race=='Braunvieh'       ~ 7751,     #Annahme Durschnitt
              Race=='Holstein_rbt'    ~ 8518,      #Annahme Milchkuh
              Race=='Holstein_sbt'    ~ 9522,    #Annahme Durchscnitt
              Race=='Vorderwaelder'    ~ 5647 #Annahme Durchschnitt
            ))


milchkuehe_2

# Erste hochrechnung der Durchschnittsleistung per kuh je kreis über die Rassen 
milchkuehe_2<-milchkuehe_2 %>% mutate(milk_production=No_animals*avg_milk_production_race)

avg_milk_prod_race<-milchkuehe_2 %>%  group_by(NUTS_2) %>% summarize(animals_per_region=sum(No_animals), sum_milk_prod=sum(milk_production))%>%
                                       mutate(avg_milk_prod=sum_milk_prod/animals_per_region)

avg_milk_prod_race


# Vergleich zu den LKV durchschnittsdaten je kreis "tatsaechliche Daten"
## get in average per kreis
avg_milk_production_2021

# does not contain values for stadtkreise, for which kreise there is no data?
avg_milk_prod_race %>% anti_join(avg_milk_production_2021, by="NUTS_2")


# same procedure as before, stadtkreise ohne daten bekommen denselben average wie landkreise, die sie umschließen
avg_milk_production_2021<-add_row(avg_milk_production_2021, avg_milk_production_2021 %>% filter(Region=="Heilbronn") %>% select(NUTS_2:`Milch kg`) %>% 
                                    mutate(Region="Heilbronn, stadtkreis",
                                           NUTS_2="DE117"))

avg_milk_production_2021<-add_row(avg_milk_production_2021, avg_milk_production_2021 %>% filter(Region=="Rastatt") %>% select(NUTS_2:`Milch kg`) %>% 
                                    mutate(Region="Stadtkreis, Baden Baden",
                                           NUTS_2="DE121"))


avg_milk_production_2021<-add_row(avg_milk_production_2021, avg_milk_production_2021 %>% filter(Region=="Karlsruhe") %>% select(NUTS_2:`Milch kg`) %>% 
                                    mutate(Region="Stadtkreis, Karlsruhe",
                                           NUTS_2="DE122"))

avg_milk_production_2021<-add_row(avg_milk_production_2021, avg_milk_production_2021 %>% filter(Region=="Rhein-Neckar") %>% select(NUTS_2:`Milch kg`) %>% 
                                    mutate(Region="Heidelberg, Stadtkreis",
                                           NUTS_2="DE125"))

avg_milk_production_2021<-add_row(avg_milk_production_2021, avg_milk_production_2021 %>% filter(Region=="Rhein-Neckar") %>% select(NUTS_2:`Milch kg`) %>% 
                                    mutate(Region="Mannheim, Stadtkreis",
                                           NUTS_2="DE126"))

avg_milk_production_2021<-add_row(avg_milk_production_2021, avg_milk_production_2021 %>% filter(Region=="Enzkreis") %>% select(NUTS_2:`Milch kg`) %>% 
                                    mutate(Region="Pforzheim",
                                           NUTS_2="DE129"))

avg_milk_production_2021<-add_row(avg_milk_production_2021, avg_milk_production_2021 %>% filter(Region=="Breisgau-Hochschw.") %>% select(NUTS_2:`Milch kg`) %>% 
                                    mutate(Region="Freiburg, Stadtkreis",
                                           NUTS_2="DE131"))


avg_milk_production_2021<-add_row(avg_milk_production_2021, avg_milk_production_2021 %>% filter(Region=="Alb-Donau-Kreis") %>%  select(NUTS_2:`Milch kg`) %>% 
                                    mutate(Region="Ulm. Stadtkreis",
                                           NUTS_2="DE144"))

# Baden Baden und Mannheim haben zero cows, das ist korrekt und passt zu first_estimate
# Milch kg = rassenunspezifischer durchscnittswert je kreis
# avg_milk_prod == rassenspezifischer durchschnittswert in der milchleistung
avg_milk_prod_race <- avg_milk_prod_race %>% left_join(avg_milk_production_2021 %>% select(NUTS_2, `Milch kg`))
avg_milk_prod_race %>% print(n=Inf)

# wie groß ist abweichung des rassendurchschnitts zum kreisdurchschnitts
diskrepanz<-avg_milk_prod_race %>% mutate(diskrepanz_kreisdurchschnitt=`Milch kg` - avg_milk_prod) %>% select(NUTS_2, diskrepanz_kreisdurchschnitt)
diskrepanz %>% print(n=Inf)

# Berechne die prozentuale abweichung
diskrepanz_revisited<-avg_milk_prod_race %>% rename(LKV_GEsamtdurchschnitt=`Milch kg`, Durchschnitt_rassenbasis=avg_milk_prod, total_milk_prod_tonnes=sum_milk_prod)
diskrepanz_revisited<-diskrepanz_revisited %>% mutate(diff=LKV_GEsamtdurchschnitt-Durchschnitt_rassenbasis)
diskrepanz_revisited<- diskrepanz_revisited %>% mutate("%Abweichung"=(LKV_GEsamtdurchschnitt-Durchschnitt_rassenbasis)/LKV_GEsamtdurchschnitt*100)
diskrepanz_revisited %>% print(n=Inf)

# Result: Abweichung uebersteigt in der REgel nicht mehr als 15%

######## linear adjustment to match the county average production level
# der berechnete durchschnitt wird angepasst um den Kreisdurchschnitt zu treffen
milchkuehe_2<-milchkuehe_2 %>% left_join(diskrepanz, by="NUTS_2")
milchkuehe_2

## diskrepanz uebersicht
milchkuehe_2 %>% print(n=Inf)

# Diskrepanz wird auf jede Rasse auf oder abgeschlagen
milchkuehe_2<-milchkuehe_2 %>% mutate(adjusted_avg_milk_production=avg_milk_production_race+diskrepanz_kreisdurchschnitt)
milchkuehe_2

milchkuehe_2<-milchkuehe_2 %>% mutate(adj_milk_production=No_animals*adjusted_avg_milk_production)
milchkuehe_2 %>% print(n=Inf)

# check if Milchleistung matches now den Kreisdurchschnitt der LKV per region
avg_milk_production_2021
adj_avg_milk_production<- milchkuehe_2 %>%  group_by(NUTS_2) %>% summarize(animals_per_region=sum(No_animals, na.rm=T), adjusted_sum_milk_prod=sum(adj_milk_production, na.rm=T))%>%
                                            mutate(adjusted_avg_milk_prod=adjusted_sum_milk_prod/animals_per_region)


avg_milk_production_2021<-avg_milk_production_2021 %>% left_join(adj_avg_milk_production%>% select(NUTS_2, adjusted_avg_milk_prod), by="NUTS_2")%>%
                                                        mutate(check=`Milch kg`- adjusted_avg_milk_prod)

print(avg_milk_production_2021, n=Inf)
# Result, the adjustement was succesful - jetzt habe ich variation der DAten aufgrund der rassenunterschiede und der unterschieldichen produktivitäts levels der kreise

#adjusted average milk production contains the values needed, die Milchleistung je Tier wurde hochskaliert/runterskaliert nicht die Tierzahl!
#Annahme: es gibt kreisunterschiede bei der milchleistung. 
milchkuehe_2
milchkuehe_2$adjusted_avg_milk_production<-round(milchkuehe_2$adjusted_avg_milk_production,digits = 0)

rm(race_proportion,milchkuehe,diskrepanz, diskrepanz_revisited, avg_milk_race, avg_milk_prod_race)


if(laptob_work==TRUE) {
  write_xlsx(x=milchkuehe_2, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/18.04.23/Tierzahlen_leistung_jekreis_milch.xlsx", col_names = TRUE)
  
} else {
  write_xlsx(x=milchkuehe_2, path = "C:/Users/Tristan Herrmann/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/18.04.23/Tierzahlen_leistung_jekreis_milch.xlsx", col_names = TRUE)
}  


###################################################################################################################################################################
# step2 Estimating the amount of N, P, K per region based on the amount of cows per race
P_org_milch <- milchkuehe_2 %>% select(NUTS_2:Race, No_animals, adjusted_avg_milk_production)
P_org_milch %>% print(n=Inf)



# Hochrechnung von N, P, K based on KTBL
# Laden der Mistproduktion auf Basis des performance levels

if(laptob_work==TRUE) {
  setwd("C:\\Users\\User\\OneDrive - bwedu\\Dokumente\\Landwirtschaftliche Betriebslehre\\Projekt_P_Bawü\\GAMS_P\\Input_data\\Kalkulationsdaten\\KTBL\\Wirtschaftsduengeranfall")
} else {
  setwd("C:\\Users\\Tristan Herrmann\\OneDrive - bwedu\\Dokumente\\Landwirtschaftliche Betriebslehre\\Projekt_P_Bawü\\GAMS_P\\Input_data\\Kalkulationsdaten\\KTBL\\Wirtschaftsduengeranfall")
}

performance_level <- read_excel("overview_wirtschaftduengeranfall.xlsx",sheet = "Milch_leistungsniveau" )
performance_level


Fleckvieh<-P_org_milch %>% filter(Race=="Fleckvieh")
sbt_rbt <- P_org_milch %>% filter(Race=="Holstein_sbt" | Race=="Holstein_rbt")
Braunvieh <- P_org_milch %>% filter(Race=="Braunvieh")
Vorderwaelder <-P_org_milch %>% filter(Race=="Vorderwaelder")

# Estimate performance levels je nutztierrasse, das level wird dann benutzt um NPK je Tier zu schaetzen

Fleckvieh<- Fleckvieh %>%
  mutate(performance_level = case_when(
    round(Fleckvieh$adjusted_avg_milk_production,-3) == 6000 ~ 1,
    round(Fleckvieh$adjusted_avg_milk_production,-3) == 7000 ~ 2,
    round(Fleckvieh$adjusted_avg_milk_production, -3) == 8000 ~ 3
  ))



sbt_rbt<- sbt_rbt %>%
  mutate(performance_level = case_when(
    round(sbt_rbt$adjusted_avg_milk_production,-3) == 7000 ~ 1,
    round(sbt_rbt$adjusted_avg_milk_production,-3) == 8000 ~ 2,
    round(sbt_rbt$adjusted_avg_milk_production,-3) == 9000 ~ 2,
    round(sbt_rbt$adjusted_avg_milk_production, -3) == 10000 ~ 3
  ))



Braunvieh<- Braunvieh %>%
  mutate(performance_level = case_when(
    round(Braunvieh$adjusted_avg_milk_production,-3) == 6000 ~ 1,
    round(Braunvieh$adjusted_avg_milk_production,-3) == 7000 ~ 2,
    round(Braunvieh$adjusted_avg_milk_production, -3) == 8000 ~ 3
  ))


Vorderwaelder<- Vorderwaelder %>%
  mutate(performance_level = case_when(
    round(Vorderwaelder$adjusted_avg_milk_production,-3) == 4000 ~ 1,
    round(Vorderwaelder$adjusted_avg_milk_production,-3) == 5000 ~ 1,
    round(Vorderwaelder$adjusted_avg_milk_production,-3) == 6000 ~ 1,
    round(Vorderwaelder$adjusted_avg_milk_production,-3) == 7000 ~ 2,
    round(Vorderwaelder$adjusted_avg_milk_production, -3) == 8000 ~ 3
  ))


NUTS_2_milk_performance_level <- rbind(Fleckvieh, Braunvieh,sbt_rbt, Vorderwaelder)

# 6.07.23 adjusting nach Verfahren, strohbasiert oder guellebasiert
# nach thuenen emission report: in Bawue im schnitt 87.4 guellebasiert und 12.6% strohbasiert
# adding a split
Fleckvieh<-Fleckvieh %>% mutate(No_animals_guelle=0.874*No_animals,
                        No_animals_stroh=0.125*No_animals)

Braunvieh<-Braunvieh %>% mutate(No_animals_guelle=0.874*No_animals,
                                No_animals_stroh=0.125*No_animals)

sbt_rbt<-sbt_rbt %>% mutate(No_animals_guelle=0.874*No_animals,
                                No_animals_stroh=0.125*No_animals)

Vorderwaelder<-Vorderwaelder %>% mutate(No_animals_guelle=0.874*No_animals,
                                No_animals_stroh=0.125*No_animals)


Fleckvieh %>% summarize(sum(No_animals))
Braunvieh %>% summarize(sum(No_animals))
sbt_rbt %>% summarize(sum(No_animals))
Vorderwaelder %>% summarize(sum(No_animals))

### joining data with the performance level
Fleckvieh_g <-Fleckvieh  %>%  left_join(performance_level%>% filter(Rasse=="Fleckvieh" & Produkt=="Fluessigmist"), by="performance_level")
Braunvieh_g <-Braunvieh %>%  left_join(performance_level%>% filter(Rasse=="Fleckvieh" & Produkt=="Fluessigmist"), by="performance_level")
sbt_rbt_g   <- sbt_rbt %>% left_join(performance_level%>% filter(Rasse=="Sbt_HF" & Produkt=="Fluessigmist"), by="performance_level")
Vorderwaelder_g <- Vorderwaelder %>% left_join(performance_level %>% filter(Rasse=="Fleckvieh" & Produkt=="Fluessigmist"), by="performance_level")

Fleckvieh_s <-Fleckvieh  %>%  left_join(performance_level%>% filter(Rasse=="Fleckvieh") %>%filter(!Produkt=="Fluessigmist"), by="performance_level")
Braunvieh_s <-Braunvieh %>%  left_join(performance_level%>% filter(Rasse=="Fleckvieh") %>%filter(!Produkt=="Fluessigmist"), by="performance_level")
sbt_rbt_s   <- sbt_rbt %>% left_join(performance_level%>% filter(Rasse=="Sbt_HF")%>% filter(!Produkt=="Fluessigmist"), by="performance_level")
Vorderwaelder_s <- Vorderwaelder %>% left_join(performance_level %>% filter(Rasse=="Fleckvieh") %>% filter(!Produkt=="Fluessigmist"), by="performance_level")


### Getting the scaling factors. Ich möchte die Düngermenge anpassen basierend auf die durchschnittliche Milchleistung je Kreis
Fleckvieh
#FM_kg_Tier_jahr,TM_kg_Tier_jahr,N_kg_Tier_jahr,P205_kg_Tier_jahr, K20_kg_Tier_jahr Kalkulation_milchleistung

cows_PKN_s <- rbind(Fleckvieh_s,Braunvieh_s,sbt_rbt_s, Vorderwaelder_s)
cows_PKN_g <- rbind(Fleckvieh_g,Braunvieh_g,sbt_rbt_g, Vorderwaelder_g)


# what do I get for total N, if I dont do the scaling
total_N_g<-cows_PKN_g %>% mutate(total_N=No_animals_guelle*N_kg_Tier_jahr) %>%select(total_N) %>%
               summarize(total_N_sum=sum(total_N, na.rm=T)) 


total_N_s<-cows_PKN_s %>% mutate(total_N=No_animals_stroh*N_kg_Tier_jahr) %>%select(total_N) %>%
               summarize(total_N_sum=sum(total_N, na.rm=T))

cows_PKN_s %>% summarize(sum(No_animals)/3)
cows_PKN_g %>% summarize(sum(No_animals))

# ohne scaling bekomme ich eine 3% abweichung, ist in Ordnung, denk ich
# erklaerung koennte sein, dass ich vorderwaelder und rbt ueberschaetze

#cows_PKN %>% print(n=Inf) 

#Berechnung des scaling factors
# NPK soll bezogen werden auf die Milchleistung, daher wird das hier linear skaliert
# cows_PKN_g<-cows_PKN_g %>% mutate(FM_scaling_factor=FM_kg_Tier_jahr/Kalkulation_milchleistung,
#                               TM_scaling_factor=TM_kg_Tier_jahr/Kalkulation_milchleistung,
#                               N_scaling_factor=N_kg_Tier_jahr/Kalkulation_milchleistung,
#                               P205_scaling_factor=P205_kg_Tier_jahr/Kalkulation_milchleistung,
#                               K20_scaling_factor=K20_kg_Tier_jahr/Kalkulation_milchleistung)

# 20.07.2023 Don't do the scaling: It is easier and you get closer to the Thuenen estimate; does not change much anyway
cows_PKN_g <- cows_PKN_g %>% select(NUTS_2:Rasse, Produkt,Leistungsniveau, Einstreu, N_kg_Tier_jahr:K20_kg_Tier_jahr)
cows_PKN_g<-cows_PKN_g %>%select(-No_animals_stroh)
cows_PKN_g<-cows_PKN_g %>%select(-No_animals) %>% rename(No_animals=No_animals_guelle) %>% mutate(verfahren="guellebasiert")

# Berechnung scaling factor fuer stroh
# cows_PKN_s<-cows_PKN_s %>% mutate(FM_scaling_factor=FM_kg_Tier_jahr/Kalkulation_milchleistung,
#                                   TM_scaling_factor=TM_kg_Tier_jahr/Kalkulation_milchleistung,
#                                   N_scaling_factor=N_kg_Tier_jahr/Kalkulation_milchleistung,
#                                   P205_scaling_factor=P205_kg_Tier_jahr/Kalkulation_milchleistung,
#                                   K20_scaling_factor=K20_kg_Tier_jahr/Kalkulation_milchleistung)

cows_PKN_s <- cows_PKN_s %>% select(NUTS_2:Rasse, Produkt,Leistungsniveau,Einstreu, N_kg_Tier_jahr:K20_kg_Tier_jahr )
cows_PKN_s<-cows_PKN_s %>%select(-No_animals_guelle)
cows_PKN_s<-cows_PKN_s %>%select(-No_animals) %>% rename(No_animals=No_animals_stroh) %>% mutate(verfahren="strohbasiert")

# final calculation of N, P, K per region 
# question, this needs to be reduced by the amount that goes to Grünland and the amount that is not available to plants 

cows_PKN_g<-cows_PKN_g %>% mutate(N_region_kgjahr=No_animals*N_kg_Tier_jahr,
                              P205_region_kgjahr=No_animals*P205_kg_Tier_jahr,
                              K20_region_kgjahr=No_animals*K20_kg_Tier_jahr)

cows_PKN_s<-cows_PKN_s %>% mutate(N_region_kgjahr=No_animals*N_kg_Tier_jahr,
                              P205_region_kgjahr=No_animals*P205_kg_Tier_jahr,
                              K20_region_kgjahr=No_animals*K20_kg_Tier_jahr)



cows_PKN_s<-cows_PKN_s %>% select(NUTS_2:performance_level,No_animals,verfahren,Produkt,Einstreu,Leistungsniveau, N_region_kgjahr:K20_region_kgjahr)
cows_PKN_g<-cows_PKN_g %>% select(NUTS_2:performance_level,No_animals,verfahren,Produkt,Einstreu,Leistungsniveau, N_region_kgjahr:K20_region_kgjahr)


total_N_g<-cows_PKN_g %>% summarize(total_N_sum=sum(N_region_kgjahr, na.rm=T)) 
total_N_g

total_N_s<-cows_PKN_s %>% summarize(total_N_sum=sum(N_region_kgjahr, na.rm=T)) 
total_N_s

total_N_s+total_N_g

cows_PKN<-rbind(cows_PKN_g, cows_PKN_s)
cows_PKN
cows_PKN %>% summarize(N_total_bawue=sum(N_region_kgjahr, na.rm=T))


if(laptob_work==TRUE) {
  write_xlsx(x=cows_PKN, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/18.04.23/cows_PKN.xlsx", col_names = TRUE)
  
} else {
  write_xlsx(x=cows_PKN, path = "C:/Users/Tristan Herrmann/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/18.04.23/cows_PKN.xlsx", col_names = TRUE)
  
}  

rm(Fleckvieh, sbt_rbt, Braunvieh,Vorderwaelder, P_org_milch)

#####################################################################################################################################################################
### Duengerabschaetzung fuer mutterkuhaltung

first_estimate_num
mutterkuhhaltung <- first_estimate_num %>% select(NUTS_2, region=Kreis_name, other_cows=SCOW_num)
mutterkuhhaltung<-mutterkuhhaltung %>% mutate(id=1)

# laden der ktbl daten, und waehlen der wirtschaftsweise
NPK_mutterkuh <- read_excel("overview_wirtschaftduengeranfall.xlsx",sheet = "7.6_Mutterkuhhaltung" )


NPK_mutterkuh<-NPK_mutterkuh %>% select(`performance and feed_level`, `Einstreu kg FM/(Tier · d)`, Produkt, N_kg_Tier_jahr:K20_kg_Tier_jahr) %>% 
                                filter(`performance and feed_level`=="Winterstall und Sommerweide 165 stalltage, 600kg, 270kg") %>%
                                filter(`Einstreu kg FM/(Tier · d)`==6 | `Einstreu kg FM/(Tier · d)`==0 ) %>% mutate(id=1)

NPK_mutterkuh

mutterkuhhaltung_g<-mutterkuhhaltung %>% select(NUTS_2, region, other_cows, id) %>% 
  mutate(No_animals_g=other_cows*0.246)%>%
  left_join(NPK_mutterkuh %>% filter(`Einstreu kg FM/(Tier · d)`==0), by="id") %>% select(-id)

mutterkuhhaltung_s<-mutterkuhhaltung %>% select(NUTS_2, region, other_cows, id) %>% 
  mutate(No_animals_s=other_cows*0.754)%>%
  left_join(NPK_mutterkuh %>% filter(`Einstreu kg FM/(Tier · d)`==6), by="id") %>% select(-id)

mutterkuhhaltung_g<-mutterkuhhaltung_g %>% mutate(N_region_jahr=No_animals_g*N_kg_Tier_jahr,
                                              P205_region_Jahr=No_animals_g*P205_kg_Tier_jahr,
                                              K20_regio_jahr=No_animals_g*K20_kg_Tier_jahr)%>% select(-c(P205_kg_Tier_jahr,K20_kg_Tier_jahr, N_kg_Tier_jahr))

mutterkuhhaltung_s<-mutterkuhhaltung_s %>% mutate(N_region_jahr=No_animals_s*N_kg_Tier_jahr,
                                                  P205_region_Jahr=No_animals_s*P205_kg_Tier_jahr,
                                                  K20_regio_jahr=No_animals_s*K20_kg_Tier_jahr)%>% select(-c(P205_kg_Tier_jahr,K20_kg_Tier_jahr, N_kg_Tier_jahr))

mutterkuhhaltung_s<-mutterkuhhaltung_s %>% select(-other_cows) %>% rename(No_animals=No_animals_s) %>% mutate(verfahren="strobasiert")
mutterkuhhaltung_g<-mutterkuhhaltung_g %>% select(-other_cows) %>% rename(No_animals=No_animals_g) %>% mutate(verfahren="guellebasiert")

mutterkuhhaltung_g %>% summarize(sum(N_region_jahr))
mutterkuhhaltung_s %>% summarize(sum(N_region_jahr))


mutterkuhhaltung %>% summarize(total_animals=sum(other_cows, na.rm=T))
mutterkuhhaltung<-rbind(mutterkuhhaltung_g, mutterkuhhaltung_s)
mutterkuhhaltung<-mutterkuhhaltung %>% rename(Leistungsniveau=`performance and feed_level`)


mutterkuhhaltung %>% summarize(total_N=sum(N_region_jahr, na.rm=T))




if(laptob_work==TRUE) {
  write_xlsx(x=mutterkuhhaltung, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/18.04.23/mutterkuh_PKN.xlsx", col_names = TRUE)
  
} else {
  write_xlsx(x=mutterkuhhaltung, path = "C:/Users/Tristan Herrmann/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/18.04.23/mutterkuh.xlsx", col_names = TRUE)
  
}


##########################################################################
## Schaetzung Kaelberauzucht

# diese über alle RAsse gerundete performance wird genutzt um die Kaelber zu schaetzen
# moment mal.. habe ja auch bei den Kaelberndaten verschiedene Rassen, dass ist auch bei der Mast! sehr relevant!
# Die selben shares wie vorher bei den Milchkuehen wird benutzt um die Kaelber auf verschiedene Rassen splitten 

#adjusted_race_proportions %>% print(n=Inf)
first_estimate_num %>% summarize(sum(calves_EI, na.rm=T))
kaelber<-first_estimate_num %>% select(NUTS_2, region=Kreis_name, total_kaelber=calves_EI)

# kaelber<-kaelber %>% left_join(adjusted_race_proportions %>% select(- c(region, cows)), by="NUTS_2")
# kaelber %>% print(n=Inf)
# 
# kaelber<-kaelber %>% mutate(Fleckvieh_kU1=`Fleckvieh %`/100*total_kaelber,
#                                   Braunvieh_kU1=`Braunvieh %`/100*total_kaelber,
#                                   sbt_ku1=`Holstein-sbt %`/100*total_kaelber,
#                                   rbt_ku1=`Holsteins-Rbt`/100*total_kaelber,
#                                   voerder_ku1=`Vorderwälder %`/100*total_kaelber) %>% 
#   select(NUTS_2, region, total_kaelber,Fleckvieh_kU1:voerder_ku1)
# 
# kaelber %>% summarize(sum(total_kaelber, na.rm=T))

# runden der Werte
#kaelber$Fleckvieh_kU1<- round(kaelber$Fleckvieh_kU1)
#kaelber$Braunvieh_kU1 <- round(kaelber$Braunvieh_kU1)
#kaelber$sbt_ku1<- round(kaelber$sbt_ku1)
#kaelber$rbt_ku1<- round(kaelber$rbt_ku1)
#kaelber$voerder_ku1<- round(kaelber$voerder_ku1)



# kaelber<-kaelber %>% pivot_longer(c(Fleckvieh_kU1,Braunvieh_kU1,sbt_ku1,rbt_ku1, voerder_ku1)) %>% rename(Rasse="name", No_animals="value")
# kaelber
# 
# # getting the performance levels from milk production per area
# kaelber_F<-kaelber%>% filter(Rasse=="Fleckvieh_kU1") %>% left_join(NUTS_2_milk_performance_level%>%filter(Race=="Fleckvieh") %>%
#                                                                           select(NUTS_2, performance_level), by="NUTS_2")
# 
# kaelber_B<-kaelber%>% filter(Rasse=="Braunvieh_kU1") %>% left_join(NUTS_2_milk_performance_level%>%filter(Race=="Braunvieh") %>%
#                                                                           select(NUTS_2, performance_level), by="NUTS_2")
# 
# kaelber_sbt<-kaelber%>% filter(Rasse=="sbt_ku1") %>% left_join(NUTS_2_milk_performance_level%>%filter(Race=="Holstein_sbt") %>%
#                                                                       select(NUTS_2, performance_level), by="NUTS_2")
# 
# kaelber_rbt<-kaelber%>% filter(Rasse=="rbt_ku1") %>% left_join(NUTS_2_milk_performance_level%>%filter(Race=="Holstein_rbt") %>%
#                                                                       select(NUTS_2, performance_level), by="NUTS_2")
# 
# 
# kaelber_voer<-kaelber%>% filter(Rasse=="voerder_ku1") %>% left_join(NUTS_2_milk_performance_level%>%filter(Race=="Vorderwaelder") %>%
#                                                                            select(NUTS_2, performance_level), by="NUTS_2")
# 
# kaelber_B %>% summarize(sum(No_animals))
# kaelber_F %>% summarize(sum(No_animals))
# kaelber_sbt %>% summarize(sum(No_animals))
# kaelber_rbt %>% summarize(sum(No_animals))
# kaelber_voer %>% summarize(sum(No_animals))

# performance level beziehen sich auf den Zuwachs Fleckvieh 90, 95, 100kg Zuwachs; SBT_HF 80, 85, 90 kg Zuwachs



#ktbl daten fuer wirtschaftsuenger pferde, take the average from thuenen and fit the data accordingly
kaelber<-kaelber %>% mutate(N_Tier=16.6, P_Tier=6.4, K_Tier=15.3) %>% mutate(Leistungsniveau="Kaelberaufzucht16wochen")

kaelber<-kaelber %>% mutate(N_kg_year=total_kaelber*N_Tier,
                          P205_kg_year=total_kaelber*P_Tier,
                          K20_kg_year=total_kaelber*K_Tier)


kaelber <- kaelber %>% select(-c(N_Tier:K_Tier))
kaelber

kaelber %>% summarize(sum(N_kg_year))
kaelber %>% summarise(sum(total_kaelber))









# performance_level_kaelber <- read_excel("overview_wirtschaftduengeranfall.xlsx",sheet = "4.6_Kaelberaufzucht" )
# performance_level_kaelber
# tail(performance_level_kaelber)
# #performance_level_kaelber %>% filter(Rasse=="voerder_ku1") %>% print(n=Inf)

# kaelber_F<-kaelber_F %>% left_join(performance_level_kaelber%>% filter(Rasse=="Fleckvieh"), by="performance_level")
# 
# 
# kaelber_B<-kaelber_B %>% left_join(performance_level_kaelber%>% filter(Rasse=="Fleckvieh") , by="performance_level")
# kaelber_sbt<-kaelber_sbt %>% left_join(performance_level_kaelber%>% filter(Rasse=="Sbt_HF") , by="performance_level")
# kaelber_rbt<-kaelber_rbt %>% left_join(performance_level_kaelber%>% filter(Rasse=="Sbt_HF") , by="performance_level")
# kaelber_voer<-kaelber_voer %>% left_join(performance_level_kaelber%>% filter(Rasse=="Fleckvieh")  , by="performance_level")
# 
# kaelber_voer %>% print(n=Inf)
# 
# kaelber <-rbind(kaelber_F,kaelber_B, kaelber_rbt,kaelber_sbt,kaelber_voer)
# kaelber %>% summarize(sum(No_animals)/3)



# kaelber<-kaelber %>% rowwise()%>%mutate(N_kg_year=No_animals*N_kg_Tier_jahr,
#                                   P205_kg_year=No_animals*P205_kg_Tier_jahr,
#                                   K20_kg_year=No_animals*K20_kg_Tier_jahr)
# 
# kaelber<-kaelber %>% select(NUTS_2:performance_level,Rasse=Rasse.x,Produkt, Leistungsniveau,Einstreu, N_kg_year:K20_kg_year)

#checks
# kaelber %>% summarize(total_N=sum(N_kg_year, na.rm=T)) %>% summarize(sum(total_N))
# kaelber %>% summarize(total_animal_number=sum(No_animals, na.rm=T)) %>% summarize(sum(total_animal_number)/3)


# DONE schaetzung NPK je Rasse je kreis fuer Kaelber
if(laptob_work==TRUE) {
  write_xlsx(x=kaelber, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/18.04.23/kaelber_U1_PKN.xlsx", col_names = TRUE)
  
} else {
  write_xlsx(x=kaelber, path = "C:/Users/Tristan Herrmann/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/18.04.23/kaelber_U1_PKN.xlsx", col_names = TRUE)
  
}  


#########################################################################################################################################################################################

# Rinder 1 bis unter 2 Jahre alte Tiere
kaelber
# KTBL DAten: weibliche Kaelber zwischen 1 und 2 Jahren werden als Jungrinder unter KTBL betrachtet, bzw werden als Färsen nach thuenen betrachtet
# Hier gibt es 3 Zuwachsraten für Fleckvieh und Schwarzbunt. D.h. fuer diese Altersklasse kann dasselbe Vorgehen angewendet werden wie auf Kaelber
# Daten nach RAssen splitten, Kreisperformance levels holen und dann das hochrechnen um den organischen NPK zu bekommen - allerdings nur fuer die Faersen nach thuenen


# Faersen nach thuenen sind hier weiblich jungrinder
jungrinder_female<-first_estimate_num %>% select(NUTS_2, region=Kreis_name, jungrinder_female_total=dairy_heifers)
jungrinder_female %>% summarize(sum(jungrinder_female_total))


# same procedure as with Kaelber, ausplitten nach RAssen und performance_level
jungrinder_female<-jungrinder_female %>% left_join(adjusted_race_proportions%>% select(-c(cows, region)), by="NUTS_2")


jungrinder_female<-jungrinder_female %>% mutate(Fleckvieh_kU1=`Fleckvieh %`/100*jungrinder_female_total,
                                                Braunvieh_kU1=`Braunvieh %`/100*jungrinder_female_total,
                                                sbt_ku1=`Holstein-sbt %`/100*jungrinder_female_total,
                                                rbt_ku1=`Holsteins-Rbt`/100*jungrinder_female_total,
                                                voerder_ku1=`Vorderwälder %`/100*jungrinder_female_total) 

jungrinder_female<-jungrinder_female %>% select(NUTS_2, region, jungrinder_female_total, Fleckvieh_kU1:voerder_ku1)


# rounding of No. animals
#jungrinder_female$Fleckvieh_kU1<- round(jungrinder_female$Fleckvieh_kU1)
#jungrinder_female$Braunvieh_kU1 <- round(jungrinder_female$Braunvieh_kU1)
#jungrinder_female$sbt_ku1<- round(jungrinder_female$sbt_ku1)
#jungrinder_female$rbt_ku1<- round(jungrinder_female$rbt_ku1)
#jungrinder_female$voerder_ku1<- round(jungrinder_female$voerder_ku1)

jungrinder_female<-jungrinder_female %>%select(-jungrinder_female_total)%>% pivot_longer(c(Fleckvieh_kU1,Braunvieh_kU1,sbt_ku1,rbt_ku1, voerder_ku1)) %>% rename(Rasse="name", No_animals="value")
jungrinder_female

#split in stroh und guellebasiert
jungrinder_female<-jungrinder_female %>% mutate(No_animals_g=0.552*No_animals,
                             No_animals_s=0.448*No_animals)


# getting the performance levels from milk production per area
jungrinder_F<-jungrinder_female%>% filter(Rasse=="Fleckvieh_kU1") %>% left_join(NUTS_2_milk_performance_level%>%filter(Race=="Fleckvieh") %>%
                                                                                  select(NUTS_2, performance_level), by="NUTS_2")

jungrinder_B<-jungrinder_female%>% filter(Rasse=="Braunvieh_kU1") %>% left_join(NUTS_2_milk_performance_level%>%filter(Race=="Braunvieh") %>%
                                                                                  select(NUTS_2, performance_level), by="NUTS_2")

jungrinder_sbt<-jungrinder_female%>% filter(Rasse=="sbt_ku1") %>% left_join(NUTS_2_milk_performance_level%>%filter(Race=="Holstein_sbt") %>%
                                                                              select(NUTS_2, performance_level), by="NUTS_2")

jungrinder_rbt<-jungrinder_female%>% filter(Rasse=="rbt_ku1") %>% left_join(NUTS_2_milk_performance_level%>%filter(Race=="Holstein_rbt") %>%
                                                                              select(NUTS_2, performance_level), by="NUTS_2")


jungrinder_voer<-jungrinder_female%>% filter(Rasse=="voerder_ku1") %>% left_join(NUTS_2_milk_performance_level%>%filter(Race=="Vorderwaelder") %>%
                                                                                   select(NUTS_2, performance_level), by="NUTS_2")



# performance levels from KTBL data
performance_level_jungrinder <- read_excel("overview_wirtschaftduengeranfall.xlsx",sheet = "5.6_Jungrinder" )
performance_level_jungrinder

# I can choose the amount of straw used on average, here for consistency I use 3 kg FM per day and animal
#performance_level_jungrinder<-performance_level_jungrinder %>% filter(Einstreu==3)

jungrinder_F_s<-jungrinder_F %>% left_join(performance_level_jungrinder%>% filter(Rasse=="Fleckvieh") %>% select(-c(Rasse))  %>% filter(Einstreu==1.5)       , by="performance_level")
jungrinder_B_s<-jungrinder_B %>% left_join(performance_level_jungrinder%>% filter(Rasse=="Fleckvieh") %>% select(-c(Rasse))  %>% filter(Einstreu==1.5)       , by="performance_level")
jungrinder_sbt_s<-jungrinder_sbt %>% left_join(performance_level_jungrinder%>% filter(Rasse=="Sbt") %>% select(-c( Rasse))   %>% filter(Einstreu==1.5)       , by="performance_level")
jungrinder_rbt_s<-jungrinder_rbt %>% left_join(performance_level_jungrinder%>% filter(Rasse=="Sbt") %>% select(-c(Rasse))    %>% filter(Einstreu==1.5)       , by="performance_level")
jungrinder_voer_s<-jungrinder_voer %>% left_join(performance_level_jungrinder%>% filter(Rasse=="Fleckvieh") %>% select(-c(Rasse))    %>% filter(Einstreu==1.5)     , by="performance_level")


jungrinder_F_g<-jungrinder_F %>% left_join(performance_level_jungrinder%>% filter(Rasse=="Fleckvieh") %>% select(-c(Rasse))  %>% filter(Einstreu==0)       , by="performance_level")
jungrinder_B_g<-jungrinder_B %>% left_join(performance_level_jungrinder%>% filter(Rasse=="Fleckvieh") %>% select(-c(Rasse))  %>% filter(Einstreu==0)       , by="performance_level")
jungrinder_sbt_g<-jungrinder_sbt %>% left_join(performance_level_jungrinder%>% filter(Rasse=="Sbt") %>% select(-c( Rasse))   %>% filter(Einstreu==0)       , by="performance_level")
jungrinder_rbt_g<-jungrinder_rbt %>% left_join(performance_level_jungrinder%>% filter(Rasse=="Sbt") %>% select(-c(Rasse))    %>% filter(Einstreu==0)       , by="performance_level")
jungrinder_voer_g<-jungrinder_voer %>% left_join(performance_level_jungrinder%>% filter(Rasse=="Fleckvieh") %>% select(-c(Rasse))    %>% filter(Einstreu==0)     , by="performance_level")


jungrinder_female_s <-rbind(jungrinder_F_s,jungrinder_B_s, jungrinder_sbt_s,jungrinder_rbt_s,jungrinder_voer_s)
jungrinder_female_s<-jungrinder_female_s %>% select(-c(No_animals, No_animals_g))

jungrinder_female_g <-rbind(jungrinder_F_g,jungrinder_B_g, jungrinder_sbt_g,jungrinder_rbt_g,jungrinder_voer_g)
jungrinder_female_g<-jungrinder_female_g %>% select(-c(No_animals, No_animals_s))


# Doing the final checks
jungrinder_female_s %>% summarize(sum(No_animals_s)/3)
jungrinder_female_g %>% summarize(sum(No_animals_g))

jungrinder_female_s<-jungrinder_female_s %>% mutate(N_kg_year=No_animals_s*N_kg_Tier_jahr,
                                                P205_kg_year=No_animals_s*P205_kg_Tier_jahr,
                                                K20_kg_year=No_animals_s*K20_kg_Tier_jahr)

jungrinder_female_g<-jungrinder_female_g %>% mutate(N_kg_year=No_animals_g*N_kg_Tier_jahr,
                                                    P205_kg_year=No_animals_g*P205_kg_Tier_jahr,
                                                    K20_kg_year=No_animals_g*K20_kg_Tier_jahr)


# amount of N je system
jungrinder_female_g %>% summarize(sum(N_kg_year))
jungrinder_female_s %>% summarize(sum(N_kg_year))


jungrinder_female_s<-jungrinder_female_s %>% select(NUTS_2:performance_level,Produkt,Leistungsniveau, Einstreu, N_kg_year:K20_kg_year)
jungrinder_female_g<-jungrinder_female_g %>% select(NUTS_2:performance_level,Produkt,Leistungsniveau,Einstreu, N_kg_year:K20_kg_year)

jungrinder_female_s<-jungrinder_female_s %>% mutate(verfahren="strohbasiert") %>% rename(No_animals="No_animals_s")
jungrinder_female_g<-jungrinder_female_g %>% mutate(verfahren="guellebasiert") %>% rename(No_animals="No_animals_g")

jungrinder_female <-rbind(jungrinder_female_g, jungrinder_female_s)


jungrinder_female %>% summarize(sum(N_kg_year, na.rm=T))
str(jungrinder_female)

#jungrinder_female %>% print(n=Inf)

# DONE schaetzung NPK je Rasse je kreis fuer jungrinder female
if(laptob_work==TRUE) {
  write_xlsx(x=jungrinder_female, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/18.04.23/jungrinder_female_PKN.xlsx", col_names = TRUE)
  
} else {
  write_xlsx(x=jungrinder_female, path = "C:/Users/Tristan Herrmann/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/18.04.23/jungrinder_female_PKN.xlsx", col_names = TRUE)
  
} 

rm(jungrinder_B, jungrinder_F, jungrinder_rbt, jungrinder_sbt, jungrinder_voer)
rm(jungrinder_B_s, jungrinder_F_s, jungrinder_rbt_s, jungrinder_sbt_s, jungrinder_voer_s)
rm(jungrinder_B_g, jungrinder_F_g, jungrinder_rbt_g, jungrinder_sbt_g, jungrinder_voer_g)
rm(jungrinder_female_g, jungrinder_female_s)
rm(performance_level_jungrinder, performance_level_kaelber)
rm(performance_level)

######################################################################################################################################################################################################
# Estimate for Jungrinder male - Rindermast - BULL in thuenen data
first_estimate_num %>% summarize(sum(MaleBeefCattle, na.rm=T))
jungrinder_male <- first_estimate_num%>% select(NUTS_2, region=Kreis_name, jungrinder_male_total=MaleBeefCattle)
jungrinder_male


# same procedure as with Kaelber, ausplitten nach RAssen und performance_level
#jungrinder_male<-jungrinder_male %>% left_join(adjusted_race_proportions%>% select(-c(cows,region)), by="NUTS_2")


#jungrinder_male<-jungrinder_male %>% mutate(Fleckvieh_kU1=`Fleckvieh %`/100*jungrinder_male_total,
#                                            Braunvieh_kU1=`Braunvieh %`/100*jungrinder_male_total,
#                                            sbt_ku1=`Holstein-sbt %`/100*jungrinder_male_total,
#                                            rbt_ku1=`Holsteins-Rbt`/100*jungrinder_male_total,
#                                            voerder_ku1=`Vorderwälder %`/100*jungrinder_male_total) 

#jungrinder_male<-jungrinder_male %>% select(NUTS_2, region, jungrinder_male_total, Fleckvieh_kU1:voerder_ku1)


# rounding of No. animals

#jungrinder_male$Fleckvieh_kU1<- round(jungrinder_male$Fleckvieh_kU1)
#jungrinder_male$Braunvieh_kU1 <- round(jungrinder_male$Braunvieh_kU1)
#jungrinder_male$sbt_ku1<- round(jungrinder_male$sbt_ku1)
#jungrinder_male$rbt_ku1<- round(jungrinder_male$rbt_ku1)
#jungrinder_male$voerder_ku1<- round(jungrinder_male$voerder_ku1)

#jungrinder_male<-jungrinder_male %>% pivot_longer(c(Fleckvieh_kU1,Braunvieh_kU1,sbt_ku1,rbt_ku1, voerder_ku1)) %>% rename(Rasse="name", No_animals="value")
#jungrinder_male <- jungrinder_male %>% select(-jungrinder_male_total)

jungrinder_male <- jungrinder_male %>% mutate(No_animals_s=jungrinder_male_total*0.527,
                                              No_animals_g=jungrinder_male_total*0.473)

jungrinder_male#
jungrinder_male %>% summarize(sum(jungrinder_male_total))

# getting the performance levels from milk production per area
# same performance je kreis wie bei milch, as ist ziemlich questionable - aber okay for now
#jungrinder_F<-jungrinder_male%>% filter(Rasse=="Fleckvieh_kU1") #%>% left_join(NUTS_2_milk_performance_level%>%filter(Race=="Fleckvieh") %>%
                                                                 #               select(NUTS_2, performance_level), by="NUTS_2")

#jungrinder_B<-jungrinder_male%>% filter(Rasse=="Braunvieh_kU1")# %>% left_join(NUTS_2_milk_performance_level%>%filter(Race=="Braunvieh") %>%
                                                                #                select(NUTS_2, performance_level), by="NUTS_2")

#jungrinder_sbt<-jungrinder_male%>% filter(Rasse=="sbt_ku1") #%>% left_join(NUTS_2_milk_performance_level%>%filter(Race=="Holstein_sbt") %>%
                                                             #               select(NUTS_2, performance_level), by="NUTS_2")

#jungrinder_rbt<-jungrinder_male%>% filter(Rasse=="rbt_ku1") #%>% left_join(NUTS_2_milk_performance_level%>%filter(Race=="Holstein_rbt") %>%
                                                             #               select(NUTS_2, performance_level), by="NUTS_2")


#jungrinder_voer<-jungrinder_male%>% filter(Rasse=="voerder_ku1")# %>% left_join(NUTS_2_milk_performance_level%>%filter(Race=="Vorderwaelder") %>%
                                                                 #                select(NUTS_2, performance_level), by="NUTS_2")



# performance levels from KTBL data
performance_level_rindermast <- read_excel("overview_wirtschaftduengeranfall.xlsx",sheet = "6.6_Rindermast" )
performance_level_rindermast

performance_level_rindermast<-performance_level_rindermast %>% filter(Leistungsniveau== "Fleckvieh 135bis650kg LM, 15.5 Monate")
performance_level_rindermast<-performance_level_rindermast %>% rename(verfahren=performance_level)

# filter for Einstreu == 1; two is also a option
# I can choose the amount of straw used on average, here for consistency I use 1 kg FM per day and animal
#performance_level_rindermast<-performance_level_rindermast %>% filter(Einstreu==1)

#jungrinder_F_s<-jungrinder_F %>% left_join(performance_level_rindermast%>% filter(Rasse=="Fleckvieh") %>% select(-c(Rasse)), by="performance_level")
#jungrinder_B_s<-jungrinder_B %>% left_join(performance_level_rindermast%>% filter(Rasse=="Fleckvieh") %>% select(-c(Rasse)), by="performance_level")
#jungrinder_sbt_s<-jungrinder_sbt %>% left_join(performance_level_rindermast%>% filter(Rasse=="Sbt") %>% select(-c(Rasse)), by="performance_level")
#jungrinder_rbt_s<-jungrinder_rbt %>% left_join(performance_level_rindermast%>% filter(Rasse=="Sbt") %>% select(-c(Rasse)), by="performance_level")
#jungrinder_voer_s<-jungrinder_voer %>% left_join(performance_level_rindermast%>% filter(Rasse=="Fleckvieh") %>% select(-c(Rasse)), by="performance_level")


#jungrinder_mast <-rbind(jungrinder_F,jungrinder_B, jungrinder_sbt,jungrinder_rbt,jungrinder_voer)

jungrinder_male<-jungrinder_male %>% mutate(verfahren=1)


jungrinder_mast_s <- jungrinder_male %>% select(NUTS_2:region, No_animals_s, verfahren) %>% left_join(performance_level_rindermast %>% filter(Einstreu==2), by="verfahren")
jungrinder_mast_g <- jungrinder_male %>% select(NUTS_2:region, No_animals_g, verfahren) %>% left_join(performance_level_rindermast %>% filter(Einstreu==0), by="verfahren")

jungrinder_mast_g <- jungrinder_mast_g %>% mutate(verfahren="guellebasiert")
jungrinder_mast_s <- jungrinder_mast_s %>% mutate(verfahren="strohbasiert")


jungrinder_mast_s<-jungrinder_mast_s %>% mutate(N_kg_year=No_animals_s*N_kg_Tier_jahr,
                                            P205_kg_year=No_animals_s*P205_kg_Tier_jahr,
                                            K20_kg_year=No_animals_s*K20_kg_Tier_jahr)


jungrinder_mast_g<-jungrinder_mast_g %>% mutate(N_kg_year=No_animals_g*N_kg_Tier_jahr,
                                                P205_kg_year=No_animals_g*P205_kg_Tier_jahr,
                                                K20_kg_year=No_animals_g*K20_kg_Tier_jahr)


# N je verfahren
jungrinder_mast_s %>% summarize(sum(N_kg_year))
jungrinder_mast_g %>% summarize(sum(N_kg_year))


jungrinder_mast_s<-jungrinder_mast_s %>% select(NUTS_2:verfahren,Einstreu,Leistungsniveau,Produkt, N_kg_year:K20_kg_year) %>% rename(No_animals="No_animals_s")
jungrinder_mast_g<-jungrinder_mast_g %>% select(NUTS_2:verfahren,Einstreu,Produkt,Leistungsniveau, N_kg_year:K20_kg_year) %>% rename(No_animals="No_animals_g")


jungrinder_mast<-rbind(jungrinder_mast_s, jungrinder_mast_g)

jungrinder_mast

str(jungrinder_mast)

jungrinder_mast %>% summarize(sum(N_kg_year))


# reality check




# DONE schaetzung NPK je Rasse je kreis fuer jungrinder female
if(laptob_work==TRUE) {
  write_xlsx(x=jungrinder_mast, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/18.04.23/jungrinder_mast_PKN.xlsx", col_names = TRUE)
  
} else {
  write_xlsx(x=jungrinder_mast, path = "C:/Users/Tristan Herrmann/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/18.04.23/jungrinder_mast_PKN.xlsx", col_names = TRUE)
  
}  


########################################################################################################################################################################################
# remove all datasets except the following ones
jungrinder_mast
jungrinder_female
kaelber
mutterkuhhaltung
cows_PKN
#No_animals

####################################################################################################################################################################################


rm(list = ls()[!ls() %in% c("jungrinder_mast", "jungrinder_female", "kaelber","mutterkuhhaltung", "cows_PKN", "milchkuehe_2","first_estimate_num")])

##################################################################################################################################################################################################

# Is it possible to put these datatest togehter?
milchvieh_NPK_complete<-jungrinder_mast %>% mutate(Type="BULL", performance_level=NA, Rasse="Fleckvieh") %>%
                          rbind(jungrinder_female %>% mutate(Type="HEIT"))%>%
                          rbind(kaelber %>% rename(No_animals="total_kaelber") %>% select(NUTS_2:K20_kg_year) %>% 
                                  mutate(Type="CALV", verfahren="strohbasiert", Einstreu=NA, Produkt="Frischmist&Rottemist",Rasse="Kalb", performance_level="NA"))%>%
  
  rbind(mutterkuhhaltung  %>%rename(Einstreu= `Einstreu kg FM/(Tier · d)`) %>% 
                                mutate(performance_level=NA, Type="SCOW", Rasse=NA) %>% 
                                rename(N_kg_year="N_region_jahr", P205_kg_year="P205_region_Jahr",K20_kg_year="K20_regio_jahr")) %>%
                          rbind(cows_PKN %>% select(-adjusted_avg_milk_production)%>% 
                                rename(Rasse="Race") %>% mutate(Type="DCOW") %>% 
                                rename(N_kg_year="N_region_kgjahr", P205_kg_year="P205_region_kgjahr",K20_kg_year="K20_region_kgjahr"))

# achievement: one dataset with NPK estiamtes per region for milchvieh!

##########################################################################################################################################
# Next up: Schweinehaltung

laptob_work<-TRUE
str(first_estimate_num)
first_estimate_num %>% summarize(sum(PIGF_num))
first_estimate_num <- first_estimate_num %>% mutate(thunen_mastschwein=PIGF_num-n_piglets)
first_estimate_num %>% summarize(sum(thunen_mastschwein))
first_estimate_num %>% summarize(sum(ferkel, na.rm=T))
first_estimate_num %>% summarize(sum(suckling_pigs, na.rm=T))

pigs_bawue<- first_estimate_num %>% select(NUTS_2, region=Kreis_name, RP,"andere Schweine"=thunen_mastschwein, Zuchtsauen=SOWS_num, Ferkel=ferkel)
pigs_bawue
pigs_bawue<-pigs_bawue %>% pivot_longer(cols = c(Ferkel, Zuchtsauen, `andere Schweine`), names_to = "pigs") %>% rename(No.="value")

############################################################################################################################################
# MAstschweine
# Getting KTBL NPK per region 
if(laptob_work==TRUE) {
  setwd("C:\\Users\\User\\OneDrive - bwedu\\Dokumente\\Landwirtschaftliche Betriebslehre\\Projekt_P_Bawü\\GAMS_P\\Input_data\\Kalkulationsdaten\\KTBL\\Wirtschaftsduengeranfall")
} else {
  setwd("C:\\Users\\Tristan Herrmann\\OneDrive - bwedu\\Dokumente\\Landwirtschaftliche Betriebslehre\\Projekt_P_Bawü\\GAMS_P\\Input_data\\Kalkulationsdaten\\KTBL\\Wirtschaftsduengeranfall")
}


# performance levels from KTBL data
performance_level_schwein <- read_excel("schweinemast_ktbl.xlsx",sheet = "10.6_Schweinemast", skip=2 )
performance_level_schwein


# In this case I'm calculating with the Standardfutter, hier einmal fuer Stroh und einmal fuer Fluessigmist
performance_level_schwein_s<-performance_level_schwein %>% filter(`Einstreu kg FM/(TP · d)`=="1.1" & `Leistung und Futergrundlage`=="790g_mittlere_ tägliche_Zunahme,257kg_Zuwachs_je_TP_und Jahr,Standardfutter")
performance_level_schwein_g<-performance_level_schwein %>% filter(`Einstreu kg FM/(TP · d)`=="0" & Wirtshaftsdüngerart=="Fluessigmist" &`Leistung und Futergrundlage`=="790g_mittlere_ tägliche_Zunahme,257kg_Zuwachs_je_TP_und Jahr,Standardfutter")


performance_level_schwein %>% mutate(pigs="andere Schweine")

## Estimating Mastschweine
pigs_bawue


mast_bawue_s <- pigs_bawue %>% filter(pigs=="andere Schweine") %>% select(region, NUTS_2, pigs, No.) %>% mutate(No_animals_s=No.*0.089)%>%
              left_join(performance_level_schwein_s %>% mutate(pigs="andere Schweine", Futter="Standardfutter") %>%
              select(pigs, Futter,Leistungsniveau="Leistung und Futergrundlage", Einstreu=`Einstreu kg FM/(TP · d)`, Produkt=Wirtshaftsdüngerart, N,P2O5, K2O), by="pigs")

mast_bawue_g <- pigs_bawue %>% filter(pigs=="andere Schweine") %>% select(region, NUTS_2, pigs, No.) %>% mutate(No_animals_g=No.*0.911)%>%
                left_join(performance_level_schwein_g %>% mutate(pigs="andere Schweine", Futter="Standardfutter") %>% 
                select(pigs, Futter,Leistungsniveau="Leistung und Futergrundlage" ,Einstreu=`Einstreu kg FM/(TP · d)`, Produkt=Wirtshaftsdüngerart, N,P2O5, K2O), by="pigs")


mast_bawue_s<- mast_bawue_s %>% select(-c(No., Futter))
mast_bawue_g<- mast_bawue_g %>% select(-c(No., Futter))

mast_bawue_s<-mast_bawue_s %>% mutate(N_kg_year=No_animals_s*N,
                                  P2O5_kg_year=No_animals_s*P2O5,
                                  K2O_kg_year=No_animals_s*K2O)


mast_bawue_g<-mast_bawue_g %>% mutate(N_kg_year=No_animals_g*N,
                                    P2O5_kg_year=No_animals_g*P2O5,
                                    K2O_kg_year=No_animals_g*K2O)


mast_bawue_g <- mast_bawue_g %>% select(-c(N,P2O5,K2O )) 
mast_bawue_s <- mast_bawue_s %>% select(-c(N,P2O5,K2O ))

mast_bawue_g <- mast_bawue_g %>% mutate(Verfahren="guellebasiert") %>% rename(No_animals="No_animals_g")
mast_bawue_s <- mast_bawue_s %>% mutate(Verfahren="strohabsiert") %>% rename(No_animals="No_animals_s")

# check nach verfahren
mast_bawue_g %>% summarize(sum(N_kg_year))
mast_bawue_s %>% summarize(sum(N_kg_year))

mast_bawue <- rbind(mast_bawue_s, mast_bawue_g)

# check1: GEsamtzahl Tiere:
mast_bawue_g %>% summarize(sum(No_animals))
mast_bawue_s %>% summarize(sum(No_animals)/2)



mast_bawue %>% summarize(N_total=sum(N_kg_year, na.rm=T))
#9,284,000

mast_bawue_g  %>% summarize(N_total=sum(N_kg_year, na.rm=T))
mast_bawue_s  %>% summarize(N_total=sum(N_kg_year, na.rm=T))


mast_bawue %>% print(n=Inf)


# DONE schaetzung NPK for schweinemast
if(laptob_work==TRUE) {
  write_xlsx(x=mast_bawue, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/18.04.23/mast_schweine_bawue_PKN.xlsx", col_names = TRUE)
  
} else {
  write_xlsx(x=mast_bawue, path = "C:/Users/Tristan Herrmann/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/18.04.23/mast_schweine_bawue_PKN.xlsx", col_names = TRUE)
  
}  

###################################################################################################################################################################
# Getting started with Ferkelaufzucht
performance_level_ferkelaufzucht <- read_excel("fertillizer_sang.xlsx",sheet = "9.6_Ferkelaufzucht", skip=2 )
performance_level_ferkelaufzucht
str(performance_level_ferkelaufzucht)

# KTBL data is for 8kg Lebendmasse, daher wird das gewaehlt, Standardfutter
# 8kg is die annahme bei Thuenen emssion report, und sie geben eine taeglich zunahme 2020 von 480 an, 2021 sogar 500

# waehle passendes prodsystem
performance_level_ferkelaufzucht <- performance_level_ferkelaufzucht %>% filter(Prod_system=="Ferkel nach 28 Tage Säugezeit, Anfangsgewicht 8kg, 450g tägliche Zunahme, 6.76 Umtriebe je Jahr")

# berechnung fuer strohbasiertes system, Berechnung hier mit Standardfutter
performance_level_s <- performance_level_ferkelaufzucht %>% mutate(pigs="Ferkel") %>% filter(`Leistung und Futergrundlage`=="143kg Zuwachs je Tierplatz und Jahr,Standardfutter")
performance_level_s<-performance_level_s %>% filter(`Einstreu kg FM/(TP · d)`== "0.1" | `Einstreu kg FM/(TP · d)`== "0.10")
performance_level_s

#guellebasiertes system
performance_level_g <- performance_level_ferkelaufzucht %>% mutate(pigs="Ferkel") %>% filter(`Leistung und Futergrundlage`=="143kg Zuwachs je Tierplatz und Jahr,Standardfutter")
performance_level_g<-performance_level_g %>% filter(`Einstreu kg FM/(TP · d)`== "0")
performance_level_g



ferkel<-pigs_bawue %>% filter(pigs=="Ferkel")
ferkel %>% filter(pigs=="Ferkel") %>% summarize(sum(No., na.rm=T))
ferkel <- ferkel %>% mutate(No_animals_s=0.089*No., No_animals_g=No.*0.911)
ferkel

first_estimate_num %>% select(ferkel_estimate) %>% summarize(sum(ferkel_estimate, na.rm=T))

## Estimate NPK for Ferkel in BaWue 2020, stroh und guellebasiertes system
ferkel_s<- ferkel %>% select(-c(No., No_animals_g)) %>% left_join(performance_level_s %>% select(Leistungsniveau=`Leistung und Futergrundlage`,`Einstreu kg FM/(TP · d)`:pigs), by="pigs") 

ferkel_g<- ferkel %>% select(-c(No., No_animals_s)) %>% left_join(performance_level_g %>% select(Leistungsniveau=`Leistung und Futergrundlage`,`Einstreu kg FM/(TP · d)`:pigs), by="pigs") 


ferkel_s$N <- as.numeric(ferkel_s$N)
ferkel_s$P2O5 <- as.numeric(ferkel_s$P2O5)
ferkel_s$K2O <- as.numeric(ferkel_s$K2O)

ferkel_g$N <- as.numeric(ferkel_g$N)
ferkel_g$P2O5 <- as.numeric(ferkel_g$P2O5)
ferkel_g$K2O <- as.numeric(ferkel_g$K2O)


ferkel<-rbind(ferkel_s %>% rename(No_animals=No_animals_s) %>% mutate(verfahren="strohbasiert"), ferkel_g %>% rename(No_animals=No_animals_g) %>% mutate(verfahren="guellebasiert"))

ferkel_NPK<-ferkel %>% select(NUTS_2:pigs, No_animals,verfahren, Leistungsniveau, Einstreu=`Einstreu kg FM/(TP · d)`, Produkt, N:K2O) %>% mutate(N_kg_year=No_animals*N,
                                                                           P2O5_kg_year=No_animals*P2O5,
                                                                           K2O_kg_year=No_animals*K2O) %>% select(-c(N,P2O5,K2O ))

ferkel_NPK %>% print(n=Inf)    

# Doing the checks
ferkel_NPK %>% filter(verfahren=="guellebasiert") %>% summarize(sum(No_animals, na.rm=T), sum(N_kg_year,na.rm=T))
ferkel_NPK %>% filter(verfahren=="strohbasiert") %>% summarize(sum(No_animals, na.rm=T)/3, sum(N_kg_year,na.rm=T))


# DONE schaetzung NPK for ferkeleraufzucht
if(laptob_work==TRUE) {
  write_xlsx(x=ferkel_NPK, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/18.04.23/ferkel_NPK.xlsx", col_names = TRUE)
  
} else {
  write_xlsx(x=ferkel_NPK, path = "C:/Users/Tristan Herrmann/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/18.04.23/ferkel_NPK.xlsx", col_names = TRUE)
  
}  


#####################################################################################################################################################################################################
## continue with Ferkelerzeugung

performance_level_ferkelerzeugung <- read_excel("fertillizer_sang.xlsx",sheet = "8.6_Ferkelerzeugung", skip=2 )

## For now I choose "standardfutter"
## Fix assumption is 26 ferkel 279kg zuwachs je TP und Jahr

performance_level_ferkelerzeugung <- performance_level_ferkelerzeugung %>% filter(`Leistung und Futergrundlage`=="26_Ferkel,279kg_Zuwachs_je_TierplatzJahr,Standardfutter")
performance_level_ferkelerzeugung

## split into strobasiet und guellebasiert
performance_level_s <- performance_level_ferkelerzeugung %>% filter(`Einstreu kg FM/(Tier · d)`==1.2) %>% mutate(pigs="Zuchtsauen")
performance_level_g <- performance_level_ferkelerzeugung %>% filter(`Einstreu kg FM/(Tier · d)`==0) %>% mutate(pigs="Zuchtsauen")

# Tierzahlen fuer den join vorbereiten
zuchtsauen<-pigs_bawue %>% filter(pigs=="Zuchtsauen")
zuchtsauen

# split in guelee und strobasiert
zuchtsauen_s<- zuchtsauen %>% mutate(No_animals_s=No.*0.146)
zuchtsauen_g<- zuchtsauen %>% mutate(No_animals_g=No.*0.854)

## Estimate NPK for zuchtsauen in BaWue 2020
zuchtsauen_s<- zuchtsauen_s %>% left_join(performance_level_s %>% select(Leistungsniveau=`Leistung und Futergrundlage`, Einstreu=`Einstreu kg FM/(Tier · d)`,Wirtshaftsdüngerart:pigs), by="pigs") 
zuchtsauen_g<- zuchtsauen_g %>% left_join(performance_level_g %>% select(Leistungsniveau=`Leistung und Futergrundlage`, Einstreu=`Einstreu kg FM/(Tier · d)`,Wirtshaftsdüngerart:pigs), by="pigs") 

# Schaetzung NPK
zuchtsauen_s<-zuchtsauen_s %>% select(NUTS_2:pigs,Leistungsniveau,Einstreu,No_animals=No_animals_s, Produkt="Wirtshaftsdüngerart", N:K2O) %>% 
                                                                                        mutate(N_kg_year=No_animals*N,
                                                                                                P2O5_kg_year=No_animals*P2O5,
                                                                                                K2O_kg_year=No_animals*K2O) %>% select(-c(N,P2O5,K2O ))

zuchtsauen_g<-zuchtsauen_g %>% select(NUTS_2:pigs,Leistungsniveau,Einstreu,No_animals=No_animals_g, Produkt="Wirtshaftsdüngerart", N:K2O) %>% 
                              mutate(N_kg_year=No_animals*N,
                                     P2O5_kg_year=No_animals*P2O5,
                                     K2O_kg_year=No_animals*K2O) %>% select(-c(N,P2O5,K2O ))


zuchtsauen_NPK<-rbind(zuchtsauen_s %>% mutate(verfahren="strohbasiert"), zuchtsauen_g %>% mutate(verfahren="guellebasiert"))

zuchtsauen_NPK

# Doing the checks
zuchtsauen_NPK %>% filter(verfahren=="guellebasiert") %>% summarize(sum(No_animals, na.rm=T), sum(N_kg_year,na.rm=T))
zuchtsauen_NPK %>% filter(verfahren=="strohbasiert") %>% summarize(sum(No_animals, na.rm=T)/3, sum(N_kg_year,na.rm=T))

#animal_number
116539+19924

# estimate total N
2820254+743150





# DONE schaetzung NPK for ferkeleraufzucht
if(laptob_work==TRUE) {
  write_xlsx(x=zuchtsauen_NPK, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/18.04.23/zuchtsauen_NPK.xlsx", col_names = TRUE)
  
} else {
  write_xlsx(x=zuchtsauen_NPK, path = "C:/Users/Tristan Herrmann/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/18.04.23/zuchtsauen_NPK.xlsx", col_names = TRUE)
  
}  

##### final pigs dataset
mast_bawue
ferkel_NPK
zuchtsauen_NPK

head(milchvieh_NPK_complete)

bawue_milchvieh_pigs_complete<-milchvieh_NPK_complete %>% 
  rbind(mast_bawue %>% mutate(Rasse="Mastschwein", performance_level=NA,Type="PIGF") %>% select(-c(pigs))  %>%
                       rename(N_kg_year="N_kg_year", P205_kg_year="P2O5_kg_year",K20_kg_year="K2O_kg_year", verfahren="Verfahren"))%>%

  rbind(ferkel_NPK %>% mutate(Rasse="Ferkel", performance_level=NA,Type="FERK") %>% select(-c(RP,pigs)) %>% 
                       rename(N_kg_year="N_kg_year", P205_kg_year="P2O5_kg_year",K20_kg_year="K2O_kg_year"))%>%

  rbind(zuchtsauen_NPK %>% mutate(Rasse="Zuchtsau", performance_level=NA,Type="SOWS") %>% select(-c(RP,pigs))  %>%
                       rename(N_kg_year="N_kg_year", P205_kg_year="P2O5_kg_year",K20_kg_year="K2O_kg_year"))

#################################################################################################################################################################
# legehennen, komplett strohbasiertes system, hier sind junghennen und legehennen aggrgiert

str(first_estimate_num)

hens <- first_estimate_num %>% select(NUTS_2, region=Kreis_name, HENS_num) %>% mutate(Type="HENS")
hens

performance_level_hens <- read_excel("fertillizer_sang.xlsx",sheet = "11.6_Legehennen", skip=2 )
performance_level_hens

# choose prod verfahren 1, Legehennen 19.5kg Eimasse
performance_level_hens_choice <- performance_level_hens %>% mutate(Type="HENS") %>% filter(`Einstreu kg FM/(TP · d)`=="0.13")
performance_level_hens_choice<- performance_level_hens_choice[c(1:2),]
performance_level_hens_choice

# nach thuenen emission inventory ist das way to high, ich mache hier eine anpassung beim ersten minus 23.5% beim zweiten - 40%
performance_level_hens_choice<-performance_level_hens_choice %>%
  mutate(N= case_when(
    Wirtshaftsdüngerart == "Frischmist" ~ 0.81-(0.81*0.2353457),
    Wirtshaftsdüngerart == "Rottemist" ~ 0.48-(0.48*0.3971458)
  ))

performance_level_hens_choice<-performance_level_hens_choice %>%
  mutate(P2O5= case_when(
    Wirtshaftsdüngerart == "Frischmist" ~ 0.81-(0.81*0.2353457)-0.4,
    Wirtshaftsdüngerart == "Rottemist" ~ 0.48-(0.48*0.3971458)-0.07
  ))

performance_level_hens_choice<-performance_level_hens_choice %>%
  mutate(K2O= case_when(
    Wirtshaftsdüngerart == "Frischmist" ~ 0.81-(0.81*0.2353457)-0.45,
    Wirtshaftsdüngerart == "Rottemist" ~ 0.48-(0.48*0.3971458)-0.12
  ))



## Estimate NPK for hens in BaWue 2020
hens<- hens %>% left_join(performance_level_hens_choice %>% select(Einstreu=`Einstreu kg FM/(TP · d)`,Leistungsniveau=`Produktions-verfahren` ,Wirtshaftsdüngerart:Type), by="Type") 

hens$N <- as.numeric(hens$N)
hens$P2O5 <- as.numeric(hens$P2O5)
hens$K2O <- as.numeric(hens$K2O)

hens_NPK<-hens %>% select(NUTS_2:Type,Einstreu,Leistungsniveau,Produkt="Wirtshaftsdüngerart", N:K2O) %>% mutate(N_kg_year=HENS_num*N,
                                                                                                      P2O5_kg_year=HENS_num*P2O5,
                                                                                                              K2O_kg_year=HENS_num*K2O) %>% 
                                                                                                      select(-c(N,P2O5,K2O ))
hens_NPK<-hens_NPK %>% rename(No_animals=HENS_num)

# Do the basic checks
# animal number
first_estimate_num %>% summarize(sum(HENS_num))
hens_NPK %>% summarize(sum(No_animals, na.rm=T)/2)


## Total numbers are the same

#Total N
hens_NPK %>%  summarize(sum(N_kg_year, na.rm=T))
hens_NPK %>% group_by(Produkt) %>%  summarize(sum(N_kg_year, na.rm=T))



# DONE schaetzung NPK for ferkeleraufzucht
if(laptob_work==TRUE) {
  write_xlsx(x=hens_NPK, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/18.04.23/hens_NPK.xlsx", col_names = TRUE)
  
} else {
  write_xlsx(x=hens_NPK, path = "C:/Users/Tristan Herrmann/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/18.04.23/hens_NPK.xlsx", col_names = TRUE)
  
}  

# Berechnung fuer Junghennen

str(first_estimate_num)

JUNGHENS <- first_estimate_num %>% select(NUTS_2, region=Kreis_name, JUNGHENS_num) %>% mutate(Type="JUNGHENS")
JUNGHENS

performance_level_JUNGHENS <- read_excel("fertillizer_sang.xlsx",sheet = "11.6_Legehennen", skip=2 )
performance_level_JUNGHENS

# choose prod verfahren 1, Legehennen 19.5kg Eimasse
performance_level_JUNGHENS_choice <- performance_level_JUNGHENS %>% mutate(Type="JUNGHENS") %>% filter(`Einstreu kg FM/(TP · d)`=="0.13")
performance_level_JUNGHENS_choice<- performance_level_JUNGHENS_choice[c(1:2),]
performance_level_JUNGHENS_choice

# nach thuenen emission inventory ist das way to high, ich mache hier eine anpassung beim ersten minus 23.5% beim zweiten - 40%
performance_level_JUNGHENS_choice<-performance_level_JUNGHENS_choice %>% filter(Wirtshaftsdüngerart == "Frischmist")%>%
  mutate(N= case_when(
    Wirtshaftsdüngerart == "Frischmist" ~ 0.3
  ))

performance_level_JUNGHENS_choice<-performance_level_JUNGHENS_choice %>% filter(Wirtshaftsdüngerart == "Frischmist")%>%
  mutate(P2O5= case_when(
    Wirtshaftsdüngerart == "Frischmist" ~ 0.18
  ))

performance_level_JUNGHENS_choice<-performance_level_JUNGHENS_choice %>% filter(Wirtshaftsdüngerart == "Frischmist")%>%
  mutate(K2O= case_when(
    Wirtshaftsdüngerart == "Frischmist" ~ 0.15
  ))



## Estimate NPK for hens in BaWue 2020
JUNGHENS<- JUNGHENS %>% left_join(performance_level_JUNGHENS_choice %>% select(Einstreu=`Einstreu kg FM/(TP · d)`,Leistungsniveau=`Produktions-verfahren` ,Wirtshaftsdüngerart:Type), by="Type") 

JUNGHENS$N <- as.numeric(JUNGHENS$N)
JUNGHENS$P2O5 <- as.numeric(JUNGHENS$P2O5)
JUNGHENS$K2O <- as.numeric(JUNGHENS$K2O)

JUNGHENS_NPK<-JUNGHENS %>% select(NUTS_2:Type,Einstreu,Leistungsniveau,Produkt="Wirtshaftsdüngerart", N:K2O) %>% mutate(N_kg_year=JUNGHENS_num*N,
                                                                                                                P2O5_kg_year=JUNGHENS_num*P2O5,
                                                                                                                K2O_kg_year=JUNGHENS_num*K2O) %>% 
  select(-c(N,P2O5,K2O ))
JUNGHENS_NPK<-JUNGHENS_NPK %>% rename(No_animals=JUNGHENS_num)

# Do the basic checks
# animal number
first_estimate_num %>% summarize(sum(JUNGHENS_num))
JUNGHENS_NPK %>% summarize(sum(No_animals, na.rm=T))


## Total numbers are the same

#Total N
JUNGHENS_NPK %>%  summarize(sum(N_kg_year, na.rm=T))
JUNGHENS_NPK %>% group_by(Produkt) %>%  summarize(sum(N_kg_year, na.rm=T))

#################################################################################
# estimation for masthuehner, note: auch puten, enten, gaense stekct hier drin - uebersetzt in huehner, war hier nicht besser moeglich
str(first_estimate_num)

masth <- first_estimate_num %>% select(NUTS_2, region=Kreis_name, no._masthuehner_estimated) %>% mutate(Type="MASTH")
masth

performance_level_masth <- read_excel("fertillizer_sang.xlsx",sheet = "12.6_Huehnermast", skip=2 )
performance_level_masth

#verfahren1
performance_level_masth_choice <- performance_level_masth %>% mutate(Type="MASTH") %>% filter(`Einstreu kg FM/(TP · d)`=="0.57") %>% slice_head(n=2)
performance_level_masth_choice

#verfahren2
#performance_level_masth_choice <- performance_level_masth %>% mutate(Type="MASTH") %>% filter(`Einstreu kg FM/(TP · d)`=="1.14") %>% slice_head(n=2)
#performance_level_masth_choice

## Estimate NPK for masth in BaWue 2020
masth<- masth %>% left_join(performance_level_masth_choice %>% select(Leistungsniveau=`Produktions-verfahren`, Einstreu=`Einstreu kg FM/(TP · d)`, Wirtshaftsdüngerart:Type), by="Type") 

masth$N <- as.numeric(masth$N)
masth$P2O5 <- as.numeric(masth$P2O5)
masth$K2O <- as.numeric(masth$K2O)

masth_NPK<-masth %>% select(NUTS_2:Type,Leistungsniveau,Einstreu,Produkt="Wirtshaftsdüngerart", N:K2O) %>% mutate(N_kg_year=no._masthuehner_estimated*N,
                                                                                       P2O5_kg_year=no._masthuehner_estimated*P2O5,
                                                                                       K2O_kg_year=no._masthuehner_estimated*K2O) %>% select(-c(N,P2O5,K2O ))
masth_NPK<-masth_NPK %>% rename(No_animals="no._masthuehner_estimated")

# Doing the checks
# hier hab ich aber einfach die GVs in Hühnermast umgerechnet, aslo sollte ich ca rankommen wenn ich Legehennen und junghennen aus gefluegel herausrechne

masth_NPK %>% summarize(total_N=sum(N_kg_year, na.rm=T))
masth_NPK %>% summarize(total_animals=sum(No_animals)/2)


# DONE schaetzung NPK for ferkeleraufzucht
if(laptob_work==TRUE) {
  write_xlsx(x=masth_NPK, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/18.04.23/masth_NPK.xlsx", col_names = TRUE)
  
} else {
  write_xlsx(x=masth_NPK, path = "C:/Users/Tristan Herrmann/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/18.04.23/masth_NPK.xlsx", col_names = TRUE)
  
}  

############################################################################################################################################################
## add the new categories e.g. female beef cattle, basically repeat bull 
str(first_estimate_num)

######################################################################################################################################################################################################
# Estimate for Jungrinder male - Rindermast - BULL in thuenen data
first_estimate_num %>% summarize(sum(female_beef_cattle, na.rm=T))
fbeef <- first_estimate_num%>% select(NUTS_2, region=Kreis_name, female_beef_cattle)
fbeef

# strobasiert und guellebasiert, do the split
fbeef <- fbeef %>% mutate(No_animals_s=female_beef_cattle*0.552,
                          No_animals_g=female_beef_cattle*0.448)

fbeef#


# performance levels from KTBL data
performance_level_rindermast <- read_excel("overview_wirtschaftduengeranfall.xlsx",sheet = "6.6_Rindermast" )
performance_level_rindermast

performance_level_rindermast<-performance_level_rindermast %>% filter(Leistungsniveau== "Fleckvieh 135bis650kg LM, 15.5 Monate")
performance_level_rindermast<-performance_level_rindermast %>% rename(verfahren=performance_level)

# create joining link
fbeef<-fbeef %>% mutate(verfahren=1)

# choose amount of einstreu nach verfahren
jungrinder_mastfemale_s <- fbeef %>% select(NUTS_2:region, No_animals_s, verfahren) %>% left_join(performance_level_rindermast %>% filter(Einstreu==2), by="verfahren")
jungrinder_mastfemale_g <- fbeef %>% select(NUTS_2:region, No_animals_g, verfahren) %>% left_join(performance_level_rindermast %>% filter(Einstreu==0), by="verfahren")



jungrinder_mastfemale_s<-jungrinder_mastfemale_s %>% mutate(N_kg_year=No_animals_s*N_kg_Tier_jahr,
                                                P205_kg_year=No_animals_s*P205_kg_Tier_jahr,
                                                K20_kg_year=No_animals_s*K20_kg_Tier_jahr)


jungrinder_mastfemale_g<-jungrinder_mastfemale_g %>% mutate(N_kg_year=No_animals_g*N_kg_Tier_jahr,
                                                P205_kg_year=No_animals_g*P205_kg_Tier_jahr,
                                                K20_kg_year=No_animals_g*K20_kg_Tier_jahr)


# N je verfahren
jungrinder_mastfemale_s %>% summarize(sum(N_kg_year))
jungrinder_mastfemale_g %>% summarize(sum(N_kg_year))


jungrinder_mastfemale_s<-jungrinder_mastfemale_s %>% select(NUTS_2:verfahren,Einstreu,Leistungsniveau,Produkt, N_kg_year:K20_kg_year) %>% rename(No_animals="No_animals_s")
jungrinder_mastfemale_g<-jungrinder_mastfemale_g %>% select(NUTS_2:verfahren,Einstreu,Produkt,Leistungsniveau, N_kg_year:K20_kg_year) %>% rename(No_animals="No_animals_g")


jungrinder_mastfemale<-rbind(jungrinder_mastfemale_s, jungrinder_mastfemale_g)

jungrinder_mastfemale

str(jungrinder_mastfemale)

jungrinder_mastfemale %>% summarize(sum(N_kg_year))


# reality check

# DONE schaetzung NPK je Rasse je kreis fuer jungrinder female
if(laptob_work==TRUE) {
  write_xlsx(x=jungrinder_mastfemale, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/18.04.23/jungrinder_mastfemale_PKN.xlsx", col_names = TRUE)
  
} else {
  write_xlsx(x=jungrinder_mastfemale, path = "C:/Users/Tristan Herrmann/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/18.04.23/jungrinder_mastfemale_PKN.xlsx", col_names = TRUE)
  
}  

########################################################################################################################
##### pferde
str(first_estimate_num)

pferde<- first_estimate_num%>% select(NUTS_2, region=Kreis_name, total_pferde)
pferde

# strobasiert und guellebasiert, do the split
pferde<-pferde %>% mutate(verfahren="strohbasiert")

#ktbl daten fuer wirtschaftsuenger pferde, take the average from thuenen and fit the data accordingly
pferde<-pferde %>% mutate(N_Tier=49.2143, P_Tier=23.4-1.8857, K_Tier=57.5-1.8857) %>% mutate(Leistungsniveau="500-600kgLG,leichtearbeit")


pferde<-pferde %>% mutate(N_kg_year=total_pferde*N_Tier,
                          P205_kg_year=total_pferde*P_Tier,
                          K20_kg_year=total_pferde*K_Tier)


pferde <- pferde %>% select(-c(N_Tier:K_Tier))
pferde

pferde %>% summarize(sum(N_kg_year))
pferde %>% summarise(sum(total_pferde))


#### schafe, getrennt nach laemmer und schafe/boecke
str(first_estimate_num)

lamb<- first_estimate_num%>% select(NUTS_2, region=Kreis_name, lambs,boecke_hammel_other, total_milch_mutterschafe)
lamb <-lamb %>% mutate(schafe=as.numeric(boecke_hammel_other)+as.numeric(total_milch_mutterschafe))
lamb

lamb %>% summarize(sum(schafe, na.rm=T))

# strobasiert und guellebasiert, do the split
lamb<-lamb %>% mutate(verfahren="strohbasiert")

lamb <- lamb %>% mutate(lambs=as.numeric(lambs)+((20024/167218))*schafe) 
lamb %>% summarize(sum(lambs, na.rm = T))


#Daten von LTZ Augustenberg 
lamb<-lamb %>% mutate(N_Tier=5.9, P_Tier=1.9, K_Tier=6.5) %>% mutate(Leistungsniveau="Laemmer_bis1Jahr,konv")

lamb$lambs <- as.numeric(lamb$lambs)

lamb<-lamb %>% mutate(N_kg_year=lambs*N_Tier,
                          P205_kg_year=lambs*P_Tier,
                          K20_kg_year=lambs*K_Tier)


lamb <- lamb %>% select(-c(N_Tier:K_Tier))
lamb

lamb %>% summarize(sum(N_kg_year, na.rm=T))
lamb %>% summarise(sum(lambs, na.rm=T))

######################################################################################################################
### mutterscahfe und andere
str(first_estimate_num)

mutterschafe<- first_estimate_num%>% select(NUTS_2, region=Kreis_name, boecke_hammel_other, total_milch_mutterschafe)
mutterschafe <-mutterschafe %>% mutate(schafe=as.numeric(boecke_hammel_other)+as.numeric(total_milch_mutterschafe))

# strobasiert und guellebasiert, do the split
mutterschafe<-mutterschafe %>% mutate(verfahren="strohbasiert")

#Daten von LTZ Augustenberg
mutterschafe<-mutterschafe %>% mutate(N_Tier=14.2, P_Tier=4.3, K_Tier=15.5) %>% mutate(Leistungsniveau="mutterschafe_ohneLamm_andereschafe_konv")


## anpassung thuenen von lambs zu schafe, zuschlag 20,024 tiere
1-(155209/167218)
mutterschafe <- mutterschafe %>% mutate(schafe=schafe-(1-(155209/167218))*schafe) 

mutterschafe<-mutterschafe %>% mutate(N_kg_year=schafe*N_Tier,
                      P205_kg_year=schafe*P_Tier,
                      K20_kg_year=schafe*K_Tier)


mutterschafe <- mutterschafe %>% select(-c(N_Tier:K_Tier))
mutterschafe

mutterschafe %>% summarize(sum(N_kg_year, na.rm=T))
mutterschafe %>% summarise(sum(schafe, na.rm=T))


#### ziegen

str(first_estimate_num)

ziegen<- first_estimate_num%>% select(NUTS_2, region=Kreis_name, total_ziegen)
ziegen

# strobasiert und guellebasiert, do the split
ziegen<-ziegen %>% mutate(verfahren="strohbasiert")

#ktbl daten fuer wirtschaftsuenger pferde, take the average from thuenen and fit the data accordingly
ziegen<-ziegen %>% mutate(N_Tier=15.2, P_Tier=5.7, K_Tier=18) %>% mutate(Leistungsniveau="Mutterziege(1.5laemmer)800kgmilch,andereziegen")

ziegen$total_ziegen<- as.numeric(ziegen$total_ziegen)

ziegen<-ziegen %>% mutate(N_kg_year=total_ziegen*N_Tier,
                          P205_kg_year=total_ziegen*P_Tier,
                          K20_kg_year=total_ziegen*K_Tier)


ziegen <- ziegen %>% select(-c(N_Tier:K_Tier))
ziegen

ziegen %>% summarize(sum(N_kg_year, na.rm = T))
ziegen %>% summarise(sum(total_ziegen, na.rm=T))











############################################################################################################################################################
## create one final dataset with all animals (zumdinest die fuer die ich ktbl data hab)

bawue_milchvieh_pigs_complete

hens_NPK
masth_NPK

# add ziegen, mutterschafe, lamb, pferde

NPK_estimate_bawue<-bawue_milchvieh_pigs_complete %>%
    rbind(hens_NPK  %>% mutate(Rasse="Hennen", performance_level=NA, verfahren="strohbasiert") %>%
          rename(N_kg_year="N_kg_year", P205_kg_year="P2O5_kg_year",K20_kg_year="K2O_kg_year")) %>%

    rbind(masth_NPK %>%  mutate(Rasse="mastgefluegel_estimated_in_masth", performance_level=NA, verfahren="strohbasiert") %>%
          rename(N_kg_year="N_kg_year", P205_kg_year="P2O5_kg_year",K20_kg_year="K2O_kg_year"))%>%
  
    rbind(jungrinder_mastfemale %>% mutate(Type="Female_BEEF_cattle", performance_level=NA, Rasse="Fleckvieh"))%>%

    rbind(pferde %>% rename(No_animals="total_pferde") %>% select(NUTS_2:K20_kg_year) %>% 
        mutate(Type="HORSE", verfahren="strohbasiert", Einstreu=NA, Produkt="Frischmist&Rottemist",Rasse="Pferd", performance_level=NA))%>%
  
  rbind(ziegen %>% rename(No_animals="total_ziegen") %>% select(NUTS_2:K20_kg_year) %>% 
          mutate(Type="ZIEG", verfahren="strohbasiert", Einstreu=NA, Produkt="Frischmist&Rottemist",Rasse=NA, performance_level=NA)) %>%
  
  rbind(lamb %>% rename(No_animals="lambs") %>% select(NUTS_2:K20_kg_year) %>% 
          mutate(Type="LAMB", verfahren="strohbasiert", Einstreu=NA, Produkt="Frischmist&Rottemist",Rasse="LAMB", performance_level=NA) %>% select(-c(boecke_hammel_other:schafe))) %>%

  rbind(mutterschafe %>% rename(No_animals="schafe") %>% select(NUTS_2 ,region, No_animals:K20_kg_year) %>% 
          mutate(Type="CALV", verfahren="strohbasiert", Einstreu=NA, Produkt="Frischmist&Rottemist",Rasse="Kalb", performance_level="NA"))


if(laptob_work==TRUE) {
  write_xlsx(x=NPK_estimate_bawue, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/18.04.23/NPK_estimate_bawue.xlsx", col_names = TRUE)
  
} else {
  write_xlsx(x=NPK_estimate_bawue, path = "C:/Users/Tristan Herrmann/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/18.04.23/NPK_estimate_bawue.xlsx", col_names = TRUE)
  
} 

first_estimate_num
milchkuehe_2

file_roxanne <- milchkuehe_2 %>% select(NUTS_2, region, Race,No_animals ,durchschnitt_milchleistung_rasse="avg_milk_production_race", milchleistung_estimate="adjusted_avg_milk_production") %>%
                                 mutate(type="DCOW")


if(laptob_work==TRUE) {
  write_xlsx(x=file_roxanne, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/18.04.23/DCOW_file_roxanne.xlsx", col_names = TRUE)
  
} else {
  write_xlsx(x=file_roxanne, path = "C:/Users/Tristan Herrmann/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/18.04.23/DCOW_file_roxanne.xlsx", col_names = TRUE)
  
} 

############################################################################################################################################################################
##

# Gesamt NPK fuer Bawue
NPK_estimate_bawue %>% summarize(sum(N_kg_year, na.rm=T), P_total=sum(P205_kg_year, na.rm=T)*0.4364)










#Einschub wie haben sich denn die Tierzahlen über die Zeit in BaWue entwickelt?

if(laptob_work==TRUE) {
  setwd("C:\\Users\\User\\OneDrive - bwedu\\Dokumente\\Landwirtschaftliche Betriebslehre\\Projekt_P_Bawü\\GAMS_P\\Input_data\\Kalkulationsdaten\\Agrarstrukturerhebung\\Tierzahlen_bawue_März_2020")
} else {
  setwd("C:\\Users\\Tristan Herrmann\\OneDrive - bwedu\\Dokumente\\Landwirtschaftliche Betriebslehre\\Projekt_P_Bawü\\GAMS_P\\Input_data\\Kalkulationsdaten\\Agrarstrukturerhebung\\Tierzahlen_bawue_März_2020")
}

DEV_no_animals <- read_excel("Tierzahlen_complete_2020.xlsx",sheet = "Grafik_1", skip = 0, col_names = T )
DEV_no_animals %>% print(n=Inf)


DEV_no_animals$`1991` <- as.numeric(DEV_no_animals$`1991`)



# Reshape the data frame to long format
DEV_no_animals <- pivot_longer(DEV_no_animals, cols = -Tierart, names_to = "Year", values_to = "Number")
DEV_no_animals<-  replace_na(DEV_no_animals, list(Number = 0))

DEV_no_animals<-pivot_wider(DEV_no_animals,names_from = "Tierart", values_from = "Number")
DEV_no_animals<- rename(DEV_no_animals, Pferde="Pferde/Einhufer2)")
DEV_no_animals

# Plot the graph
ggplot(DEV_no_animals) +
  geom_line(aes(x = Year, y = Rinder, group=1),colour="red")+
  geom_point(aes(x = Year, y = Rinder),colour="red")+
  
  geom_line(aes(x = Year, y = Schweine, group=1),colour="blue")+
  geom_point(aes(x = Year, y = Schweine),colour="blue")+
  
  geom_line(aes(x = Year, y = Schafe, group=1),colour="green")+
  geom_point(aes(x = Year, y = Schafe),colour="green")+
 
  geom_line(aes(x = Year, y = Ziegen, group=1),colour="orange")+
  geom_point(aes(x = Year, y = Ziegen),colour="orange")+
 
  geom_line(aes(x = Year, y = Pferde, group=1),colour="pink")+
    geom_point(aes(x = Year, y = Pferde),colour="pink")+

  geom_line(aes(x = Year, y = Hühner, group=1),colour="gray")+
    geom_point(aes(x = Year, y = Hühner),colour="gray")+

  geom_line(aes(x = Year, y = Truthühner, group=1),colour="black")+
    geom_point(aes(x = Year, y = Truthühner),colour="black")
  
 

