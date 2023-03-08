## #####################################################################################################################################################################################################################################
##########################################################################################################################################################################################################################################

# MODEL DIAGNOSTICS
###################################################################################################################################################################################################################################


# setting up the working directory
setwd("C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/GAMS_P/Results")


library(dplyr)
library(tidyr)
library(readxl)
library(writexl)
library(ggplot2)
library(tidyverse)

##################################################################################################################################################################################################################################
# Problem: DAs Modell hält die verschiedenen Stufen nicht ein. aus irgendwelchen Gründen

# Theorie1: Die negativen DBs in crop rotation sind das Problem. Denn anstatt den neagtiven zu nehmen, nimmt es einfach den DB=0 an

# Test: der komplette Parameter DB_cr verfuegbar wird fuer alle CRs um das selbe erhoeht, sodass alle positiv sind.
#       dann wird ueberprueft ob das modell trotzdem alle stufungen einhaelt



DB_cr_verfuegbar <- read_excel("DB_CR_verfuegbar.xlsx", sheet="Tabelle1")
DB_cr_verfuegbar<- DB_cr_verfuegbar %>% rename(plot_id="...1", counties="...2",CR_id="...3" )
DB_cr_verfuegbar  <-DB_cr_verfuegbar[,c(2,1,3,6,4,5)]
DB_cr_verfuegbar


DB_cr_verfuegbar %>% filter(niedrig<0)
DB_cr_verfuegbar %>% filter(niedrig<(-200))
DB_cr_verfuegbar %>% filter(mittel<0)
DB_cr_verfuegbar %>% filter(hoch<0)

DB_cr_verfuegbar%>%select(CR_id:hoch) %>% group_by(CR_id) %>%filter(niedrig<0 | mittel<0 | hoch<0) %>% count(CR_id)
negative_DBs<-DB_cr_verfuegbar%>%select(CR_id:hoch) %>% group_by(CR_id) %>%filter(niedrig<0 | mittel<0 | hoch<0)
negative_DBs

# what is the minimal value for niedrig, mittel, hoch
summary(negative_DBs)

# For niedrig -220.7, mittel -87, hoch -121.6

# add 221 to all values in DB_CR_verfuegbar and it should be fine




























