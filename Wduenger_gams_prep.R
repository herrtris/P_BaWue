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




