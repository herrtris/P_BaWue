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


##Okay erster Versuch. nehme das Basis-rgionsmodell aus dem Modul - es representiert die Region Stuttgart, es gibt dor nur Milchkuehe DCOW

stuttgart_DCOW <- wduenger %>% filter(NUTS_2=="DE111" & Type=="DCOW") %>% filter(!No_animals==0)
stuttgart_DCOW

# Gesamtzahl Tiere in DE111, ca. 390 Kuehe verschiedener Rasse in Stuttgart
stuttgart_DCOW %>% filter(verfahren=="strohbasiert") %>% summarize(sum(No_animals, na.rm=T)/3)
stuttgart_DCOW %>% filter(verfahren=="guellebasiert") %>% summarize(sum(No_animals, na.rm=T))

# Zur weiteren Vereinfachung mache ich einen group_by produkt. Kann ich später wieder komplizierter machen
duenger_vereinfacht <- stuttgart_DCOW %>%select(NUTS_2, Produkt, N_kg_year:K20_kg_year)%>% group_by(Produkt) %>% summarize(across(c(N_kg_year,P205_kg_year,K20_kg_year), sum))
duenger_vereinfacht

stuttgart_DCOW %>%select(NUTS_2, Produkt, N_kg_year)%>% group_by(Produkt, NUTS_2) %>% summarize(sum(N_kg_year))
stuttgart_DCOW %>%select(NUTS_2, Produkt, P205_kg_year)%>% group_by(Produkt, NUTS_2) %>% summarize(sum(P205_kg_year))
stuttgart_DCOW %>%select(NUTS_2, Produkt, K20_kg_year)%>% group_by(Produkt, NUTS_2) %>% summarize(sum(K20_kg_year))



