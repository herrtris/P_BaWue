## File to anaylse sptial data regarding Gruenland
laptob_work <- TRUE

library(dplyr)
library(tidyr)
library(readxl)
library(writexl)
library(ggplot2)
library(tidyverse)


setwd("C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/Spatial_data")


# Load Gruenland Data
gruenland_1 <-read.csv("C:\\Users\\User\\OneDrive - bwedu\\Dokumente\\Landwirtschaftliche Betriebslehre\\Projekt_P_Bawü\\Spatial_data\\P_Bawue_mowing_1.csv", sep=";", stringsAsFactors = T)
gruenland_2 <-read.csv("C:\\Users\\User\\OneDrive - bwedu\\Dokumente\\Landwirtschaftliche Betriebslehre\\Projekt_P_Bawü\\Spatial_data\\P_Bawue_mowing_2.csv", sep=";", stringsAsFactors = T)
gruenland_3 <-read.csv("C:\\Users\\User\\OneDrive - bwedu\\Dokumente\\Landwirtschaftliche Betriebslehre\\Projekt_P_Bawü\\Spatial_data\\P_Bawue_mowing_3.csv", sep=";", stringsAsFactors = T)
gruenland_4 <-read.csv("C:\\Users\\User\\OneDrive - bwedu\\Dokumente\\Landwirtschaftliche Betriebslehre\\Projekt_P_Bawü\\Spatial_data\\P_Bawue_mowing_4.csv", sep=";", stringsAsFactors = T)

#############################################################################################################################################################################################


gruenland_1 <- gruenland_1 %>% select(OID_, NUTZCODE,FAKT_CODE,OEVF_CODE,FLAECHE_HA,JAHR, NUTZUNG, NATBOD, NUTS_2, DN,NUTZCODE_1,FAKT_COD_1,OEVF_COD_1,FLAECHE__1, JAHR_1,Region_1,Kennung, Mowing_fie, Mowing_mow)
gruenland_2 <- gruenland_2 %>% select(OID_, NUTZCODE,FAKT_CODE,OEVF_CODE,FLAECHE_HA,JAHR, NUTZUNG, NATBOD, NUTS_2, DN,NUTZCODE_1,FAKT_COD_1,OEVF_COD_1,FLAECHE__1, JAHR_1,Region_1,Kennung, Mowing_fie, Mowing_mow)
gruenland_3 <- gruenland_3 %>% select(OID_, NUTZCODE,FAKT_CODE,OEVF_CODE,FLAECHE_HA,JAHR, NUTZUNG, NATBOD, NUTS_2, DN,NUTZCODE_1,FAKT_COD_1,OEVF_COD_1,FLAECHE__1, JAHR_1,Region_1,Kennung, Mowing_fie, Mowing_mow)
gruenland_4 <- gruenland_4 %>% select(OID_, NUTZCODE,FAKT_CODE,OEVF_CODE,FLAECHE_HA,JAHR, NUTZUNG, NATBOD, NUTS_2, DN,NUTZCODE_1,FAKT_COD_1,OEVF_COD_1,FLAECHE__1, JAHR_1,Region_1,Kennung, Mowing_fie, Mowing_mow)

#binding zentroid file 1 to 4 to one zentroid file
gruenland_bawue <- rbind(gruenland_1, gruenland_2, gruenland_3, gruenland_4)

# Making field area numeric
gruenland_bawue$FLAECHE_HA<-   gsub(",",".",gruenland_bawue$FLAECHE_HA)
gruenland_bawue$FLAECHE_HA <- as.numeric(gruenland_bawue$FLAECHE_HA)

# Loading area inforamtion on field usage
Flaechennutzung_Nutzcode <-read.csv("C:\\Users\\User\\OneDrive - bwedu\\Dokumente\\Landwirtschaftliche Betriebslehre\\Projekt_P_Bawü\\GAMS\\Modell-AG\\Modell-AG\\Flaechennutzung_Nutzcode.csv", sep=";") 
str(Flaechennutzung_Nutzcode)

Flaechennutzung_Nutzcode <- Flaechennutzung_Nutzcode %>% select(1:5) %>% filter(!is.na(NUTZCODE))
Flaechennutzung_Nutzcode$NUTZCODE<- as.factor(Flaechennutzung_Nutzcode$NUTZCODE)

## filter mowing intensity größer 0
glimpse(gruenland_bawue)

# Es gibt 360.000 gruenland-schlaege in Bawue
gruenland_bawue_filtered <- gruenland_bawue %>% filter(Mowing_mow>0)
glimpse(gruenland_bawue_filtered)

gruenland_bawue_filtered %>% group_by(NUTS_2) %>% summarize(mowing_mean=mean(Mowing_mow)) %>% arrange(desc(mowing_mean)) %>%print(n=Inf)
gruenland_bawue_filtered$OID_<- 1:nrow(gruenland_bawue_filtered)

## Left_join mit nutzcodes
Flaechennutzung_Nutzcode$NUTZCODE <- as.factor(Flaechennutzung_Nutzcode$NUTZCODE)
gruenland_bawue_filtered$NUTZCODE_1 <- as.factor(gruenland_bawue_filtered$NUTZCODE_1)


gruenland_bawue_filtered %>% select(OID_, NUTS_2, NATBOD, FLAECHE_HA, FLAECHE__1, JAHR, NUTZUNG, NUTZCODE, NUTZCODE_1, Mowing_mow) 
gruenland_bawue_filtered %>% select(OID_, NUTS_2, NATBOD, FLAECHE_HA, FLAECHE__1, JAHR, NUTZUNG, NUTZCODE, NUTZCODE_1, Mowing_mow) %>% group_by(NUTZUNG) %>% count(NUTZUNG)
gruenland_bawue_filtered %>% select(OID_, NUTS_2, NATBOD, FLAECHE_HA, FLAECHE__1, JAHR, NUTZUNG, NUTZCODE, NUTZCODE_1, Mowing_mow)  %>% count(NUTZCODE_1)
gruenland_bawue_filtered %>% select(OID_, NUTS_2, NATBOD, FLAECHE_HA, FLAECHE__1, JAHR, NUTZUNG, NUTZCODE, NUTZCODE_1, Mowing_mow)  %>% count(NUTZCODE)

# Welche NUTZ_codes sollte ich nehmen?
gruenland_bawue_filtered %>% filter(NUTZCODE_1=="57") %>% print(nrow(25)) #ueberwiegend gruenland, untergeordnet Wlad
gruenland_bawue_filtered %>% filter(NUTZCODE_1=="451") %>% print(nrow(25)) #vorherrschend Wald
gruenland_bawue_filtered %>% filter(NUTZCODE_1=="452") %>% print(nrow(25)) #ueberwiegend grünland
gruenland_bawue_filtered %>% filter(NUTZCODE_1=="453") %>% print(nrow(25)) #ueberwiegend grünland
gruenland_bawue_filtered %>% filter(NUTZCODE_1=="454") %>% print(nrow(25)) #ueberwiegend grünland
gruenland_bawue_filtered %>% filter(NUTZCODE_1=="455") %>% print(nrow(25)) #vorherrschend Wald
gruenland_bawue_filtered %>% filter(NUTZCODE_1=="460") %>% print(nrow(25)) #vorherrschend Wald
gruenland_bawue_filtered %>% filter(NUTZCODE_1=="462") %>% print(nrow(25)) #vorherrschend Wald
gruenland_bawue_filtered %>% filter(NUTZCODE_1=="481") %>% print(nrow(25)) #vorherrschend Wald
gruenland_bawue_filtered %>% filter(NUTZCODE_1=="458") %>% print(nrow(25)) #vorherrschend Wald

gruenland_bawue_filtered %>% filter(NUTZCODE_1=="567") %>% print(nrow(25)) #vorherrschend Wald
gruenland_bawue_filtered %>% filter(NUTZCODE_1=="592") %>% print(nrow(25)) #vorherrschend Wald
gruenland_bawue_filtered %>% filter(NUTZCODE_1=="925") %>% print(nrow(25)) #vorherrschend Wald
gruenland_bawue_filtered %>% filter(NUTZCODE_1=="994") %>% print(nrow(25)) #vorherrschend Wald


# wieviel Gesamtflaeche ist das und wie doe it compare to official statistics

gruenland_bawue_filtered$FLAECHE_HA<-   gsub(",",".",gruenland_bawue_filtered$FLAECHE_HA)
gruenland_bawue_filtered$FLAECHE_HA <- as.numeric(gruenland_bawue_filtered$FLAECHE_HA)
gruenland_bawue_filtered$FLAECHE__1<-   gsub(",",".",gruenland_bawue_filtered$FLAECHE__1)
gruenland_bawue_filtered$FLAECHE__1 <- as.numeric(gruenland_bawue_filtered$FLAECHE__1)

gruenland_bawue_filtered %>% summarise(total_gruen=sum(FLAECHE_HA), total_gruen_2=sum(FLAECHE__1))


###################################################################################################################################
gruenland_bawue_abschlagrechnung <- gruenland_bawue_filtered %>% select(NUTS_2, JAHR, FLAECHE__1,Mowing_mow)
gruenland_bawue_abschlagrechnung <- gruenland_bawue_abschlagrechnung %>% as_tibble()
gruenland_bawue_abschlagrechnung

gruenland_bawue_abschlagrechnung %>% count(Mowing_mow)
gruenland_bawue_abschlagrechnung %>% group_by(Mowing_mow)%>% summarise(total_gruen_2=sum(FLAECHE__1))


## Read in Gruenland Duengebedarf
## Vorerst nehme ich keine weiteren Ausbringungsabschläge für Grünland an

duengebedarf_gruenland<-read_excel("duengebedarf_gruenland.xlsx")
duengebedarf_gruenland <- duengebedarf_gruenland %>% select(Anzahl_Nutzungen_Grassilage:LeL_Ertragsniveau_netto_dt_TM_je_ha, N,P,K)
duengebedarf_gruenland

gruenland_bawue_abschlagrechnung <-gruenland_bawue_abschlagrechnung %>% rename(schnitthauefigkeit="Mowing_mow")%>% 
                                   left_join(duengebedarf_gruenland %>% rename(schnitthauefigkeit="Anzahl_Nutzungen_Grassilage"), by="schnitthauefigkeit")

gruenland_bawue_abschlagrechnung <-gruenland_bawue_abschlagrechnung %>% mutate(total_N=FLAECHE__1*N, total_P=FLAECHE__1*P, total_K=FLAECHE__1*K)


########################################################################################################################################################################################
county_gruenland_NPK_demand<- gruenland_bawue_abschlagrechnung %>% group_by(NUTS_2) %>% summarize(total_county_N=sum(total_N), total_county_P=sum(total_P), total_county_K=sum(total_K))

county_gruenland_NPK_demand<- county_gruenland_NPK_demand %>% slice_tail(n=44)
county_gruenland_NPK_demand<- county_gruenland_NPK_demand %>% rename(N="total_county_N", P="total_county_P", K="total_county_K")

# make content GAMS ready - dont need to modify the excel in any way, does just read into gams
coloumn_names <-names(county_gruenland_NPK_demand)
coloumn_names[1] <- ""
names(county_gruenland_NPK_demand)<- coloumn_names
county_gruenland_NPK_demand

if(laptob_work==TRUE) {
  write_xlsx(x=county_gruenland_NPK_demand, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/GAMS_P/county_gruenland_NPK_demand.xlsx", col_names = TRUE)
} else {
  write_xlsx(x=county_gruenland_NPK_demand, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/GAMS_P/county_gruenland_NPK_demand.xlsx", col_names = TRUE)
}








