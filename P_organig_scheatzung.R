### script zur Schätzung der Tierzahlen und dem Duengeranfall je Kreis ###
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

# 10 city counties are missing in the data, they will be estimated
avg_milk_production_2021<- avg_milk_production %>% filter(Jahr=="2021")

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

#3. The amount of cows in the region is relevant of course, how many cows per NUTS2 are there producing N, P, K
if(laptob_work==TRUE) {
  setwd("C:\\Users\\User\\OneDrive - bwedu\\Dokumente\\Landwirtschaftliche Betriebslehre\\Projekt_P_Bawü\\GAMS_P\\Input_data\\Kalkulationsdaten\\Agrarstrukturerhebung\\Tierzahlen_bawue_März_2020")
} else {
  setwd("C:\\Users\\Tristan Herrmann\\OneDrive - bwedu\\Dokumente\\Landwirtschaftliche Betriebslehre\\Projekt_P_Bawü\\GAMS_P\\Input_data\\Kalkulationsdaten\\Agrarstrukturerhebung\\Tierzahlen_bawue_März_2020")
}

No_animals <- read_excel("Tierzahlen_maerz_2020.xlsx",sheet = "Rinder_milchkuehe" )

glimpse(No_animals)

milchkuehe <- No_animals %>% select(NUTS_2=...2,region=...1,cows=...9, other_cows=...10)
milchkuehe <- milchkuehe[-c(1:2),]
milchkuehe


milchkuehe$cows <- as.numeric(milchkuehe$cows)
milchkuehe$other_cows <- as.numeric(milchkuehe$other_cows)

milchkuehe$cows<-round(milchkuehe$cows,digits = 0)
milchkuehe$other_cows<- round(milchkuehe$other_cows, digits=0)

milchkuehe <- replace_na(milchkuehe, list(other_cows = 0))

#### Now I have three datasets
avg_milk_production_2021 %>% print(n=Inf)
race_proportion %>% print(n=Inf)
milchkuehe %>% print(n=Inf)
###############################

# step1: combine the cow races with the milchkuehe dataset
# the LKV data on races does not have all the NUTS_2

# NUTS_2 that occur in milchkuehe but not in the race_proportions
milchkuehe %>% anti_join(race_proportion, by="NUTS_2")

# filling the gaps
# stadtkreise kriegen wenn in der tierzaehlung vorhanden die selben shares wie der landkreis, oder der direkt umschließende Kreis


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

race_proportion<-  replace_na(race_proportion, list(`Braunvieh %` = 0))
race_proportion<-  replace_na(race_proportion, list(`Holstein-sbt %` = 0))
race_proportion<-  replace_na(race_proportion, list(`Holsteins-Rbt` = 0))
race_proportion<-  replace_na(race_proportion, list(`Vorderwälder %` = 0))
race_proportion<-  replace_na(race_proportion, list(`Hinterwälder %` = 0))




# Nun sind alle kreise vorhanden, ich kann die daten nun joinen


# combining tierzahl for cows and race proportions
milchkuehe<-milchkuehe %>% left_join(race_proportion, by="NUTS_2")

milchkuehe %>% print(n=Inf)





# milchkuehe$other_cows<- as.numeric(milchkuehe$other_cows)
# milchkuehe$cows<- as.numeric(milchkuehe$cows)

#25.05. other cows sind mutterkuehe - diese werden gesondert betrachtet, aber mit den selben shares

milchkuehe<-milchkuehe %>% mutate(all_cows=cows+other_cows) 
milchkuehe



# wie weit bin ich von den 100% je kreis weg?
# fuer Freiburg kreise werden vorderwaelder mitberuecksichtigt


  
Rest <- milchkuehe %>% filter(!NUTS_2 %in% c("DE131", "DE132", "DE133", "DE134", "DE135", "DE136", "DE137", "DE13A", "DE139", "DE138"))
freiburg <- milchkuehe %>% filter(NUTS_2 %in% c("DE131", "DE132", "DE133", "DE134", "DE135", "DE136", "DE137", "DE13A", "DE139", "DE138"))
  
Rest<-Rest %>% rowwise()%>%mutate(check=sum(`Fleckvieh %`,`Holsteins-Rbt`,`Holstein-sbt %`, `Braunvieh %`)) %>% mutate(diff=100-check)

freiburg<-freiburg %>% rowwise()%>%mutate(check=sum(`Fleckvieh %`,`Holsteins-Rbt`,`Holstein-sbt %`, `Braunvieh %`, `Vorderwälder %`)) %>% mutate(diff=100-check)

  




# Im RP Freiburg gibt es verdammt viele Vorderwälder ca. 10.4% - sollte ich wahrscheinlich noch integrieren
# Vorderwaelder haben eine Durchschnittsleistung von 5647kg pro jahr
milchkuehe %>% print(n=Inf)


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



adjusted_race_proportions <- milchkuehe %>% select(NUTS_2:`Vorderwälder %`)
adjusted_race_proportions %>% mutate(sum(`Braunvieh %`,`Fleckvieh %`, `Holstein-sbt %`,`Holsteins-Rbt`, `Vorderwälder %`)) %>% print(n=Inf)



milchkuehe<-milchkuehe %>% select(-c(diff, check, `Hinterwälder %`))
milchkuehe %>% print(n=Inf)

mutterkuhhaltung <- milchkuehe %>% select(NUTS_2, region, cows, other_cows)

# splitting the cows by races
# Anzahl der Kühe je Kreis, je Rasse
milchkuehe<-milchkuehe %>% mutate(Fleckvieh=cows*`Fleckvieh %`/100,
                                  Braunvieh=cows*`Braunvieh %`/100,
                                  Holstein_rbt=cows*`Holsteins-Rbt`/100,
                                  Holstein_sbt=cows*`Holstein-sbt %`/100,
                                  Vorderwaelder=cows*`Vorderwälder %`/100)

milchkuehe
milchkuehe$Fleckvieh<-round(milchkuehe$Fleckvieh,digits = 0)
milchkuehe$Braunvieh<-round(milchkuehe$Braunvieh,digits = 0)
milchkuehe$Holstein_rbt<-round(milchkuehe$Holstein_rbt,digits = 0)
milchkuehe$Holstein_sbt<-round(milchkuehe$Holstein_sbt,digits = 0)
milchkuehe$Vorderwaelder<-round(milchkuehe$Vorderwaelder,digits = 0)

milchkuehe %>% print(n=Inf)

# DONE! Milchkühe je Region splitted by 4 different races



# nach der Hochrechnung der Milchleistung über die Rasse, wie weit bin ich weg von von dem LKV average je kreis?
#step2 avg_milchleistung nach Rasse gewichten, sodass der gesamt average per region jedoch gewahrt wird
if(laptob_work==TRUE) {
  setwd("C:\\Users\\User\\OneDrive - bwedu\\Dokumente\\Landwirtschaftliche Betriebslehre\\Projekt_P_Bawü\\GAMS_P\\Input_data\\Kalkulationsdaten\\LKV_Milchleistungspruefung")
  
} else {
  setwd("C:\\Users\\Tristan Herrmann\\OneDrive - bwedu\\Dokumente\\Landwirtschaftliche Betriebslehre\\Projekt_P_Bawü\\GAMS_P\\Input_data\\Kalkulationsdaten\\LKV_Milchleistungspruefung")
}


avg_milk_race <- read_excel("Milchleistung_je_kreis.xlsx",sheet = "avg_leistung_rasse_2021" )

avg_milk_race 



milchkuehe<-milchkuehe %>% select(NUTS_2:region, Fleckvieh:Vorderwaelder) %>% pivot_longer(cols = c(Fleckvieh, Braunvieh, Holstein_rbt, Holstein_sbt, Vorderwaelder), names_to = "Race")
milchkuehe <- milchkuehe %>% rename(No_animals=value)
milchkuehe
milchkuehe %>% print(n=Inf)

# Durchnittsdaten je Rasse für Bawue, kreisdurchschnitte sind nicht verfügbar, DAten für 2021, Seite 54
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

avg_milk_prod_race <- avg_milk_prod_race %>% left_join(avg_milk_production_2021 %>% select(NUTS_2, `Milch kg`))
avg_milk_prod_race

diskrepanz<-avg_milk_prod_race %>% mutate(diskrepanz_kreisdurchschnitt=`Milch kg` - avg_milk_prod) %>% select(NUTS_2, diskrepanz_kreisdurchschnitt)
diskrepanz

diskrepanz_revisited<-avg_milk_prod_race %>% rename(LKV_GEsamtdurchschnitt=`Milch kg`, Durchschnitt_rassenbasis=avg_milk_prod, total_milk_prod_tonnes=sum_milk_prod)
diskrepanz_revisited<-diskrepanz_revisited %>% mutate(diff=LKV_GEsamtdurchschnitt-Durchschnitt_rassenbasis)
diskrepanz_revisited<- diskrepanz_revisited %>% mutate("%Abweichung"=(LKV_GEsamtdurchschnitt-Durchschnitt_rassenbasis)/LKV_GEsamtdurchschnitt*100)
diskrepanz_revisited %>% print(n=Inf)


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


# check if Milchleistung matches now den Kreisdurchschnitt der LKV per region
avg_milk_production_2021
adj_avg_milk_production<- milchkuehe_2 %>%  group_by(NUTS_2) %>% summarize(animals_per_region=sum(No_animals, na.rm=T), adjusted_sum_milk_prod=sum(adj_milk_production, na.rm=T))%>%
                  mutate(adjusted_avg_milk_prod=adjusted_sum_milk_prod/animals_per_region)


avg_milk_production_2021<-avg_milk_production_2021 %>% left_join(adj_avg_milk_production%>% select(NUTS_2, adjusted_avg_milk_prod), by="NUTS_2")%>%
                                                        mutate(check=`Milch kg`-adjusted_avg_milk_prod)
print(avg_milk_production_2021, n=Inf)

# Result, the adjustement was succesful


#adjusted average milk production contains the values needed, die Milchleistung je Tier wurde hochskaliert nicht die Tierzahl!
#Annahme: es gibt kreisunterschiede bei der milchleistung. 
milchkuehe_2
milchkuehe_2$adjusted_avg_milk_production<-round(milchkuehe_2$adjusted_avg_milk_production,digits = 0)


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



### joining data with the performance level
Fleckvieh <-Fleckvieh %>%  left_join(performance_level%>% filter(Rasse=="Fleckvieh"), by="performance_level")
Braunvieh <-Braunvieh %>%  left_join(performance_level%>% filter(Rasse=="Fleckvieh"), by="performance_level")
sbt_rbt   <- sbt_rbt %>% left_join(performance_level%>% filter(Rasse=="Sbt_HF"), by="performance_level")
Vorderwaelder <- Vorderwaelder %>% left_join(performance_level %>% filter(Rasse=="Fleckvieh"), by="performance_level")

### Getting the scaling factors. Ich möchte die Düngermenge anpassen basierend auf die durchschnittliche Milchleistung je Kreis
Fleckvieh
#FM_kg_Tier_jahr,TM_kg_Tier_jahr,N_kg_Tier_jahr,P205_kg_Tier_jahr, K20_kg_Tier_jahr Kalkulation_milchleistung

cows_PKN <- rbind(Fleckvieh,Braunvieh,sbt_rbt, Vorderwaelder)
cows_PKN %>% print(n=Inf) 

cows_PKN<-cows_PKN %>% mutate(FM_scaling_factor=FM_kg_Tier_jahr/Kalkulation_milchleistung,
                              TM_scaling_factor=TM_kg_Tier_jahr/Kalkulation_milchleistung,
                              N_scaling_factor=N_kg_Tier_jahr/Kalkulation_milchleistung,
                              P205_scaling_factor=N_kg_Tier_jahr/Kalkulation_milchleistung,
                              K20_scaling_factor=K20_kg_Tier_jahr/Kalkulation_milchleistung)
                      
cows_PKN <- cows_PKN %>% select(NUTS_2:Rasse, Produkt, N_scaling_factor:K20_scaling_factor )
cows_PKN


# final calculation of N, P, K per region 
# question, this needs to be reduced by the amount that goes to Grünland and the amount that is not available to plants 

cows_PKN<-cows_PKN %>% mutate(N_region_kgjahr=No_animals*N_scaling_factor*adjusted_avg_milk_production,
                    P205_region_kgjahr=No_animals*P205_scaling_factor*adjusted_avg_milk_production,
                    K20_region_kgjahr=No_animals*K20_scaling_factor*adjusted_avg_milk_production)


cows_PKN<-cows_PKN %>% select(NUTS_2:performance_level,Produkt, N_region_kgjahr:K20_region_kgjahr) 

# NUTS_2 NPK regional estimate

if(laptob_work==TRUE) {
  write_xlsx(x=cows_PKN, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/18.04.23/cows_PKN.xlsx", col_names = TRUE)
  
} else {
  write_xlsx(x=cows_PKN, path = "C:/Users/Tristan Herrmann/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/18.04.23/cows_PKN.xlsx", col_names = TRUE)
  
}  

#####################################################################################################################################################################
### Duengerabschaetzung fuer mutterkuhaltung
mutterkuhhaltung<-mutterkuhhaltung %>% mutate(id=1)

NPK_mutterkuh <- read_excel("overview_wirtschaftduengeranfall.xlsx",sheet = "7.6_Mutterkuhhaltung" )
NPK_mutterkuh<-NPK_mutterkuh %>% select(`performance and feed_level`, `Einstreu kg FM/(Tier · d)`, Produkt, N_kg_Tier_jahr:K20_kg_Tier_jahr) %>% 
                  filter(`performance and feed_level`=="Winterstall und Sommerweide 165 stalltage, 600kg, 270kg") %>%
                  filter(`Einstreu kg FM/(Tier · d)`==4) %>% mutate(id=1)
NPK_mutterkuh

mutterkuhhaltung<-mutterkuhhaltung %>% select(NUTS_2, region, other_cows, id) %>% left_join(NPK_mutterkuh, by="id") %>% select(-id)

mutterkuhhaltung<-mutterkuhhaltung %>% mutate(N_region_jahr=other_cows*N_kg_Tier_jahr,
                            P205_region_Jahr=other_cows*P205_kg_Tier_jahr,
                            K20_regio_jahr=other_cows*K20_kg_Tier_jahr)%>% select(-c(P205_kg_Tier_jahr,K20_kg_Tier_jahr, N_kg_Tier_jahr))


if(laptob_work==TRUE) {
  write_xlsx(x=mutterkuhhaltung, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/18.04.23/mutterkuh_PKN.xlsx", col_names = TRUE)
  
} else {
  write_xlsx(x=mutterkuhhaltung, path = "C:/Users/Tristan Herrmann/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/18.04.23/mutterkuh.xlsx", col_names = TRUE)
  
}  




######################################################################################################################################################################

################### Duengerabschaetzung fuer Kaelber
###### Einlesen Daten aus den Tierzahlen fuer Kaelber und junrinder unter 1 Jahr, 1 bis 2 Jahre alte Tiere, 2 Jahre und aeltere Tiere in all cases male and female animals

##Kaelber und Jungrinder bis unter 1 Jahre alt
No_animals
kaelber_U1 <- No_animals %>% select(NUTS_2=...2,region=...1,Kealber_U1_male=`Rinder Noch: davon`, Kaelber_U1_female=...12)
kaelber_U1
kaelber_U1<-kaelber_U1[-c(1:2),]
kaelber_U1
kaelber_U1$Kealber_U1_male <- as.numeric(kaelber_U1$Kealber_U1_male)
kaelber_U1$Kaelber_U1_female<- as.numeric(kaelber_U1$Kaelber_U1_female)
kaelber_U1$Kealber_U1_male<- round(kaelber_U1$Kealber_U1_male, digits = 0)
kaelber_U1$Kaelber_U1_female<- round(kaelber_U1$Kaelber_U1_female, digits = 0)


# Rinder 1 bis unter 2 Jahre alte Tiere
kaelber_U12 <- No_animals %>% select(NUTS_2=...2,region=...1,Kealber_U12_male=...13, Kaelber_U12_female=...14)
kaelber_U12<-kaelber_U12[-c(1:2),]
kaelber_U12
kaelber_U12$Kealber_U12_male<- as.numeric(kaelber_U12$Kealber_U12_male)
kaelber_U12$Kaelber_U12_female<- as.numeric(kaelber_U12$Kaelber_U12_female)
kaelber_U12$Kealber_U12_male<- round(kaelber_U12$Kealber_U12_male, digits = 0)
kaelber_U12$Kaelber_U12_female<- round(kaelber_U12$Kaelber_U12_female, digits = 0)

# Jungrinder 2 Jahre und aeltere Tiere
Kaelber_Ue2 <- No_animals %>% select(NUTS_2=...2,region=...1,Kealber_Ue2_male=...15, Kaelber_Ue2_female=...16)
Kaelber_Ue2<-Kaelber_Ue2[-c(1:2),]
Kaelber_Ue2
Kaelber_Ue2$Kealber_Ue2_male<- as.numeric(Kaelber_Ue2$Kealber_Ue2_male)
Kaelber_Ue2$Kaelber_Ue2_female<- as.numeric(Kaelber_Ue2$Kaelber_Ue2_female)
Kaelber_Ue2$Kealber_Ue2_male<- round(Kaelber_Ue2$Kealber_Ue2_male, digits = 0)
Kaelber_Ue2$Kaelber_Ue2_female<- round(Kaelber_Ue2$Kaelber_Ue2_female, digits = 0)
Kaelber_Ue2

##########################################################################
## Schaetzung Kaelberauzucht

# diese über alle RAsse gerundete performance wird genutzt um die Kaelber zu schaetzen
# moment mal.. habe ja auch bei den Kaelberndaten verschiedene Rassen, dass ist auch bei der Mast! sehr relevant!
# Die selben shares wie vorher bei den Milchkuehen wird benutzt um die Kaelber auf verschiedene Rassen splitten 

adjusted_race_proportions %>% print(n=Inf)
kaelber_U1 <- kaelber_U1 %>% rowwise()%>%mutate(total_kaelber=sum(Kaelber_U1_female, Kealber_U1_male))
kaelber_U1<-kaelber_U1 %>% left_join(adjusted_race_proportions, by="NUTS_2")


kaelber_U1<-kaelber_U1 %>% mutate(Fleckvieh_kU1=`Fleckvieh %`/100*total_kaelber,
                                  Braunvieh_kU1=`Braunvieh %`/100*total_kaelber,
                                  sbt_ku1=`Holstein-sbt %`/100*total_kaelber,
                                  rbt_ku1=`Holsteins-Rbt`/100*total_kaelber,
                                  voerder_ku1=`Vorderwälder %`/100*total_kaelber) %>% 
                           select(NUTS_2, region=region.x, total_kaelber,Fleckvieh_kU1:voerder_ku1)
            

kaelber_U1$Fleckvieh_kU1<- round(kaelber_U1$Fleckvieh_kU1)
kaelber_U1$Braunvieh_kU1 <- round(kaelber_U1$Braunvieh_kU1)
kaelber_U1$sbt_ku1<- round(kaelber_U1$sbt_ku1)
kaelber_U1$rbt_ku1<- round(kaelber_U1$rbt_ku1)
kaelber_U1$voerder_ku1<- round(kaelber_U1$voerder_ku1)



kaelber_U1<-kaelber_U1 %>% pivot_longer(c(Fleckvieh_kU1,Braunvieh_kU1,sbt_ku1,rbt_ku1, voerder_ku1)) %>% rename(Rasse="name", No_animals="value")
kaelber_U1


# getting the performance levels from milk production per area
kaelber_U1F<-kaelber_U1%>% filter(Rasse=="Fleckvieh_kU1") %>% left_join(NUTS_2_milk_performance_level%>%filter(Race=="Fleckvieh") %>%
                                                             select(NUTS_2, performance_level), by="NUTS_2")

kaelber_U1B<-kaelber_U1%>% filter(Rasse=="Braunvieh_kU1") %>% left_join(NUTS_2_milk_performance_level%>%filter(Race=="Braunvieh") %>%
                                                                          select(NUTS_2, performance_level), by="NUTS_2")

kaelber_U1sbt<-kaelber_U1%>% filter(Rasse=="sbt_ku1") %>% left_join(NUTS_2_milk_performance_level%>%filter(Race=="Holstein_sbt") %>%
                                                                          select(NUTS_2, performance_level), by="NUTS_2")

kaelber_U1rbt<-kaelber_U1%>% filter(Rasse=="rbt_ku1") %>% left_join(NUTS_2_milk_performance_level%>%filter(Race=="Holstein_rbt") %>%
                                                                          select(NUTS_2, performance_level), by="NUTS_2")


kaelber_U1voer<-kaelber_U1%>% filter(Rasse=="voerder_ku1") %>% left_join(NUTS_2_milk_performance_level%>%filter(Race=="Vorderwaeldert") %>%
                                                                      select(NUTS_2, performance_level), by="NUTS_2")


#kaelber_U1<-rbind(kaelber_U1F, kaelber_U1B, kaelber_U1rbt, kaelber_U1sbt)

# performance level beziehen sich auf den Zuwachs Fleckvieh 90, 95, 100kg Zuwachs; SBT_HF 80, 85, 90 kg Zuwachs
performance_level_kaelber <- read_excel("overview_wirtschaftduengeranfall.xlsx",sheet = "4.6_Kaelberaufzucht" )
performance_level_kaelber


kaelber_U1F<-kaelber_U1F %>% left_join(performance_level_kaelber%>% filter(Rasse=="Fleckvieh") %>% select(-c(Einstreu,Rasse)), by="performance_level")
kaelber_U1B<-kaelber_U1B %>% left_join(performance_level_kaelber%>% filter(Rasse=="Fleckvieh") %>% select(-c(Einstreu,Rasse)), by="performance_level")
kaelber_U1sbt<-kaelber_U1sbt %>% left_join(performance_level_kaelber%>% filter(Rasse=="Sbt_HF") %>% select(-c(Einstreu, Rasse)), by="performance_level")
kaelber_U1rbt<-kaelber_U1rbt %>% left_join(performance_level_kaelber%>% filter(Rasse=="Sbt_HF") %>% select(-c(Einstreu,Rasse)), by="performance_level")
kaelber_U1voer<-kaelber_U1voer %>% left_join(performance_level_kaelber%>% filter(Rasse=="Fleckvieh") %>% select(-c(Einstreu,Rasse)), by="performance_level")



kaelber_U1 <-rbind(kaelber_U1F,kaelber_U1B, kaelber_U1rbt,kaelber_U1sbt,kaelber_U1voer)
kaelber_U1


kaelber_U1<-kaelber_U1 %>% mutate(N_kg_year=No_animals*N_kg_Tier_jahr,
                      P205_kg_year=No_animals*P205_kg_Tier_jahr,
                      K20_kg_year=No_animals*K20_kg_Tier_jahr)

kaelber_U1<-kaelber_U1 %>% select(NUTS_2:performance_level, N_kg_year:K20_kg_year)
kaelber_U1

# DONE schaetzung NPK je Rasse je kreis fuer Kaelber
if(laptob_work==TRUE) {
  write_xlsx(x=kaelber_U1, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/18.04.23/kaelber_U1_PKN.xlsx", col_names = TRUE)
  
} else {
  write_xlsx(x=kaelber_U1, path = "C:/Users/Tristan Herrmann/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/18.04.23/kaelber_U1_PKN.xlsx", col_names = TRUE)
  
}  

#########################################################################################################################################################################################

# Rinder 1 bis unter 2 Jahre alte Tiere
kaelber_U12 
Kaelber_Ue2
# KTBL DAten: weibliche Kaelber zwischen 1 und 2 Jahren werden als Jungrinder unter KTBL betrachtet
# Hier gibt es 3 Zuwachsraten für Fleckvieh und Schwarzbunt. D.h. fuer diese Altersklasse kann dasselbe Vorgehen angewendet werden wie auf Kaelber
# Daten nach RAssen splitten, Kreisperformance levels holen und dann das hochrechnen um den organischen NPK zu bekommen - allerdings nur fuer die weiblichen Jungrinder, die maenlichen gehen in die Mast

# Maenliche Rinder in der Mast: 

# Jungrinder male female getrennt betrachten
jungrinder<-kaelber_U12 %>% left_join(Kaelber_Ue2%>% select(-c(region)), by="NUTS_2")

jungrinder_female <- jungrinder %>% select(NUTS_2, region, Kaelber_U12_female, Kaelber_Ue2_female)



## Rough Schaetzung female rinder
jungrinder_female<-jungrinder_female %>% mutate(jungrinder_female_total = Kaelber_U12_female+Kaelber_Ue2_female)
jungrinder_female

# same procedure as with Kaelber, ausplitten nach RAssen und performance_level
jungrinder_female<-jungrinder_female %>% left_join(adjusted_race_proportions%>% select(-c(cows, other_cows, Region, region)), by="NUTS_2")


jungrinder_female<-jungrinder_female %>% mutate(Fleckvieh_kU1=`Fleckvieh %`/100*jungrinder_female_total,
                                  Braunvieh_kU1=`Braunvieh %`/100*jungrinder_female_total,
                                  sbt_ku1=`Holstein-sbt %`/100*jungrinder_female_total,
                                  rbt_ku1=`Holsteins-Rbt`/100*jungrinder_female_total,
                                  voerder_ku1=`Vorderwälder %`/100*jungrinder_female_total) 

jungrinder_female<-jungrinder_female %>% select(NUTS_2, region, jungrinder_female_total, Fleckvieh_kU1:voerder_ku1)


# rounding of No. animals

jungrinder_female$Fleckvieh_kU1<- round(jungrinder_female$Fleckvieh_kU1)
jungrinder_female$Braunvieh_kU1 <- round(jungrinder_female$Braunvieh_kU1)
jungrinder_female$sbt_ku1<- round(jungrinder_female$sbt_ku1)
jungrinder_female$rbt_ku1<- round(jungrinder_female$rbt_ku1)
jungrinder_female$voerder_ku1<- round(jungrinder_female$voerder_ku1)

jungrinder_female<-jungrinder_female %>% pivot_longer(c(Fleckvieh_kU1,Braunvieh_kU1,sbt_ku1,rbt_ku1, voerder_ku1)) %>% rename(Rasse="name", No_animals="value")
jungrinder_female

# getting the performance levels from milk production per area
jungrinder_F<-jungrinder_female%>% filter(Rasse=="Fleckvieh_kU1") %>% left_join(NUTS_2_milk_performance_level%>%filter(Race=="Fleckvieh") %>%
                                                                          select(NUTS_2, performance_level), by="NUTS_2")

jungrinder_B<-jungrinder_female%>% filter(Rasse=="Braunvieh_kU1") %>% left_join(NUTS_2_milk_performance_level%>%filter(Race=="Braunvieh") %>%
                                                                          select(NUTS_2, performance_level), by="NUTS_2")

jungrinder_sbt<-jungrinder_female%>% filter(Rasse=="sbt_ku1") %>% left_join(NUTS_2_milk_performance_level%>%filter(Race=="Holstein_sbt") %>%
                                                                      select(NUTS_2, performance_level), by="NUTS_2")

jungrinder_rbt<-jungrinder_female%>% filter(Rasse=="rbt_ku1") %>% left_join(NUTS_2_milk_performance_level%>%filter(Race=="Holstein_rbt") %>%
                                                                      select(NUTS_2, performance_level), by="NUTS_2")


jungrinder_voer<-jungrinder_female%>% filter(Rasse=="voerder_ku1") %>% left_join(NUTS_2_milk_performance_level%>%filter(Race=="Vorderwaeldert") %>%
                                                                           select(NUTS_2, performance_level), by="NUTS_2")

# performance levels from KTBL data
performance_level_jungrinder <- read_excel("overview_wirtschaftduengeranfall.xlsx",sheet = "5.6_Jungrinder" )
performance_level_jungrinder

# I can choose the amount of straw used on average, here for consistency I use 3 kg FM per day and animal
performance_level_jungrinder<-performance_level_jungrinder %>% filter(Einstreu==3)

jungrinder_F<-jungrinder_F %>% left_join(performance_level_jungrinder%>% filter(Rasse=="Fleckvieh") %>% select(-c(Einstreu,Rasse)), by="performance_level")
jungrinder_B<-jungrinder_B %>% left_join(performance_level_jungrinder%>% filter(Rasse=="Fleckvieh") %>% select(-c(Einstreu,Rasse)), by="performance_level")
jungrinder_sbt<-jungrinder_sbt %>% left_join(performance_level_jungrinder%>% filter(Rasse=="Sbt_HF") %>% select(-c(Einstreu, Rasse)), by="performance_level")
jungrinder_rbt<-jungrinder_rbt %>% left_join(performance_level_jungrinder%>% filter(Rasse=="Sbt_HF") %>% select(-c(Einstreu,Rasse)), by="performance_level")
jungrinder_voer<-jungrinder_voer %>% left_join(performance_level_jungrinder%>% filter(Rasse=="Fleckvieh") %>% select(-c(Einstreu,Rasse)), by="performance_level")



jungrinder_female <-rbind(jungrinder_F,jungrinder_B, jungrinder_sbt,jungrinder_rbt,jungrinder_voer)
jungrinder_female


jungrinder_female<-jungrinder_female %>% mutate(N_kg_year=No_animals*N_kg_Tier_jahr,
                                  P205_kg_year=No_animals*P205_kg_Tier_jahr,
                                  K20_kg_year=No_animals*K20_kg_Tier_jahr)

jungrinder_female<-jungrinder_female %>% select(NUTS_2:performance_level,Produkt, N_kg_year:K20_kg_year)
jungrinder_female

# DONE schaetzung NPK je Rasse je kreis fuer jungrinder female
if(laptob_work==TRUE) {
  write_xlsx(x=jungrinder_female, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/18.04.23/jungrinder_female_PKN.xlsx", col_names = TRUE)
  
} else {
  write_xlsx(x=jungrinder_female, path = "C:/Users/Tristan Herrmann/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/18.04.23/jungrinder_female_PKN.xlsx", col_names = TRUE)
  
}  


######################################################################################################################################################################################################
# Estimate for Jungrinder male - Rindermast

jungrinder_male <- jungrinder%>% select(NUTS_2, region, Kealber_U12_male,Kealber_Ue2_male)
jungrinder_male

jungrinder_male<-jungrinder_male %>% mutate(jungrinder_male_total = Kealber_U12_male+Kealber_Ue2_male)
jungrinder_male

# same procedure as with Kaelber, ausplitten nach RAssen und performance_level
jungrinder_male<-jungrinder_male %>% left_join(adjusted_race_proportions%>% select(-c(cows, other_cows, Region, region)), by="NUTS_2")


jungrinder_male<-jungrinder_male %>% mutate(Fleckvieh_kU1=`Fleckvieh %`/100*jungrinder_male_total,
                                                Braunvieh_kU1=`Braunvieh %`/100*jungrinder_male_total,
                                                sbt_ku1=`Holstein-sbt %`/100*jungrinder_male_total,
                                                rbt_ku1=`Holsteins-Rbt`/100*jungrinder_male_total,
                                                voerder_ku1=`Vorderwälder %`/100*jungrinder_male_total) 

jungrinder_male<-jungrinder_male %>% select(NUTS_2, region, jungrinder_male_total, Fleckvieh_kU1:voerder_ku1)


# rounding of No. animals

jungrinder_male$Fleckvieh_kU1<- round(jungrinder_male$Fleckvieh_kU1)
jungrinder_male$Braunvieh_kU1 <- round(jungrinder_male$Braunvieh_kU1)
jungrinder_male$sbt_ku1<- round(jungrinder_male$sbt_ku1)
jungrinder_male$rbt_ku1<- round(jungrinder_male$rbt_ku1)
jungrinder_male$voerder_ku1<- round(jungrinder_male$voerder_ku1)

jungrinder_male<-jungrinder_male %>% pivot_longer(c(Fleckvieh_kU1,Braunvieh_kU1,sbt_ku1,rbt_ku1, voerder_ku1)) %>% rename(Rasse="name", No_animals="value")
jungrinder_male


# getting the performance levels from milk production per area
jungrinder_F<-jungrinder_male%>% filter(Rasse=="Fleckvieh_kU1") %>% left_join(NUTS_2_milk_performance_level%>%filter(Race=="Fleckvieh") %>%
                                                                                  select(NUTS_2, performance_level), by="NUTS_2")

jungrinder_B<-jungrinder_male%>% filter(Rasse=="Braunvieh_kU1") %>% left_join(NUTS_2_milk_performance_level%>%filter(Race=="Braunvieh") %>%
                                                                                  select(NUTS_2, performance_level), by="NUTS_2")

jungrinder_sbt<-jungrinder_male%>% filter(Rasse=="sbt_ku1") %>% left_join(NUTS_2_milk_performance_level%>%filter(Race=="Holstein_sbt") %>%
                                                                              select(NUTS_2, performance_level), by="NUTS_2")

jungrinder_rbt<-jungrinder_male%>% filter(Rasse=="rbt_ku1") %>% left_join(NUTS_2_milk_performance_level%>%filter(Race=="Holstein_rbt") %>%
                                                                              select(NUTS_2, performance_level), by="NUTS_2")


jungrinder_voer<-jungrinder_male%>% filter(Rasse=="voerder_ku1") %>% left_join(NUTS_2_milk_performance_level%>%filter(Race=="Vorderwaeldert") %>%
                                                                                   select(NUTS_2, performance_level), by="NUTS_2")



# performance levels from KTBL data
performance_level_rindermast <- read_excel("overview_wirtschaftduengeranfall.xlsx",sheet = "6.6_Rindermast" )
performance_level_rindermast

# filter for Einstreu == 1; two is also a option
# I can choose the amount of straw used on average, here for consistency I use 1 kg FM per day and animal
performance_level_rindermast<-performance_level_rindermast %>% filter(Einstreu==1)

jungrinder_F<-jungrinder_F %>% left_join(performance_level_rindermast%>% filter(Rasse=="Fleckvieh") %>% select(-c(Einstreu,Rasse)), by="performance_level")
jungrinder_B<-jungrinder_B %>% left_join(performance_level_rindermast%>% filter(Rasse=="Fleckvieh") %>% select(-c(Einstreu,Rasse)), by="performance_level")
jungrinder_sbt<-jungrinder_sbt %>% left_join(performance_level_rindermast%>% filter(Rasse=="Sbt_HF") %>% select(-c(Einstreu, Rasse)), by="performance_level")
jungrinder_rbt<-jungrinder_rbt %>% left_join(performance_level_rindermast%>% filter(Rasse=="Sbt_HF") %>% select(-c(Einstreu,Rasse)), by="performance_level")
jungrinder_voer<-jungrinder_voer %>% left_join(performance_level_rindermast%>% filter(Rasse=="Fleckvieh") %>% select(-c(Einstreu,Rasse)), by="performance_level")



jungrinder_mast <-rbind(jungrinder_F,jungrinder_B, jungrinder_sbt,jungrinder_rbt,jungrinder_voer)
jungrinder_mast


jungrinder_mast<-jungrinder_mast %>% mutate(N_kg_year=No_animals*N_kg_Tier_jahr,
                                                P205_kg_year=No_animals*P205_kg_Tier_jahr,
                                                K20_kg_year=No_animals*K20_kg_Tier_jahr)

jungrinder_mast<-jungrinder_mast %>% select(NUTS_2:performance_level,Produkt, N_kg_year:K20_kg_year)
jungrinder_mast

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
kaelber_U1
mutterkuhhaltung
cows_PKN
No_animals

####################################################################################################################################################################################

rm(list=ls())

rm(list = ls()[!ls() %in% c("jungrinder_mast", "jungrinder_female", "kaelber_U1","mutterkuhhaltung", "cows_PKN")])

##################################################################################################################################################################################################

# Next up: Schweinehaltung

laptob_work<-TRUE

#3. The amount of cows in the region is relevant of course, how many cows per NUTS2 are there producing N, P, K
if(laptob_work==TRUE) {
  setwd("C:\\Users\\User\\OneDrive - bwedu\\Dokumente\\Landwirtschaftliche Betriebslehre\\Projekt_P_Bawü\\GAMS_P\\Input_data\\Kalkulationsdaten\\Agrarstrukturerhebung\\Tierzahlen_bawue_März_2020")
} else {
  setwd("C:\\Users\\Tristan Herrmann\\OneDrive - bwedu\\Dokumente\\Landwirtschaftliche Betriebslehre\\Projekt_P_Bawü\\GAMS_P\\Input_data\\Kalkulationsdaten\\Agrarstrukturerhebung\\Tierzahlen_bawue_März_2020")
}

No_animals_schweine <- read_excel("Tierzahlen_maerz_2020.xlsx",sheet = "Schweinehaltung_schaetzung", skip = 2 )
No_animals_schweine

No_animals_schweine<-No_animals_schweine %>% rename(region="...1", total_pig_owners="insgesamt...4", total_pigs="insgesamt...7")
No_animals_schweine <- No_animals_schweine %>% select(region, NUTS_2, RP, total_pigs, Ferkel:`andere Schweine`)
No_animals_schweine

#making all NA's into zeroes
No_animals_schweine<-replace_na(No_animals_schweine, list(total_pigs = 0,
                                     Ferkel=0,
                                     Zuchtsauen=0,
                                    `andere Schweine`=0))

# 1.step filling the blanks by using the RP averages weighted for the region
# 1.1 if this results in negative values for estimated animals set to zero and substract from other estimated category
# step 2. use scaling on all that have pigs so that the amount of animals per RP is matched, and the dat is matching overall

#scaling so that RP values are matched
pigs_stuttgart <- No_animals_schweine %>% filter(RP=="Stuttgart")
pigs_karlsruhe <- No_animals_schweine %>% filter(RP=="Karlsruhe")
pigs_tuebingen <- No_animals_schweine %>% filter(RP=="Tuebingen")
pigs_freiburg <- No_animals_schweine %>% filter(RP=="Freiburg")

# starting with Stuttgart
pigs_stuttgart

# filter for regions having pigs
pigs_stuttgart<-pigs_stuttgart %>% filter(total_pigs>0) %>% pivot_longer(cols = c(total_pigs, Ferkel, Zuchtsauen, `andere Schweine`), names_to = "pigs") %>% rename(No.="value")
print(pigs_stuttgart, n=Inf)


pigs_stuttgart_check<- pigs_stuttgart %>% filter(NUTS_2=="NA")
pigs_stuttgart_check

pigs_stuttgart<- pigs_stuttgart %>% filter(!NUTS_2=="NA")
pigs_stuttgart %>% filter(!pigs=="total_pigs")
pigs_stuttgart %>% group_by(NUTS_2) %>% count() ## in total there are 10 counties with pigs in RP Stuttgart


pigs_stuttgart_check %>% filter(!region=="RP Stuttgart") %>%filter(!pigs=="total_pigs")%>% mutate(lin_adjust=No./10)


pigs_stuttgart<-pigs_stuttgart %>% filter(!pigs=="total_pigs") %>% 
      left_join(
      pigs_stuttgart_check %>% filter(!region=="RP Stuttgart") %>%filter(!pigs=="total_pigs")%>% mutate(lin_adjust=No./10) %>% select(-c(region, NUTS_2, RP, No.)), by="pigs")

pigs_stuttgart<-pigs_stuttgart %>% rowwise() %>% mutate(adj_No.=No.+lin_adjust)



pigs_stuttgart$adj_No.<- round(pigs_stuttgart$adj_No.,digits = 0)
pigs_stuttgart

# does it now match the total amount of pigs in BaWue Stuttgart after rounding? In total there are 915,009 pigs in Bawue stuttgart
pigs_stuttgart_check

pigs_stuttgart %>%ungroup() %>% summarize(No.Pigs_stuttgart=sum(adj_No.)) # after linearly upscaling across all NUTS with pigs there 2 pigs more due to rounding 


# How much % is the increase now?
# percentag increase haelt sich in Grenzen, skalierung bedeutet jedoch, dass Regionen mit wenig Schweinen einen höheren prozentualen Saklierungseffekt erfahren
# andere Argumentationen waeren diese Anzahl vermehrt Regionen mit eh schon vielen Schweinen zuzuschlagen, aber vorerst bleibt das so
pigs_stuttgart<-pigs_stuttgart %>% rowwise() %>% mutate(percent_increase=(adj_No.- No.)/adj_No.*100)
pigs_stuttgart %>% print(n=Inf)


######################################################################################################################################################################
# second Karlsruhe
pigs_karlsruhe

pigs_karlsruhe<-pigs_karlsruhe %>% filter(total_pigs>0) %>% pivot_longer(cols = c(total_pigs, Ferkel, Zuchtsauen, `andere Schweine`), names_to = "pigs") %>% rename(No.="value")
print(pigs_karlsruhe, n=Inf)


pigs_karlsruhe_check<- pigs_karlsruhe %>% filter(NUTS_2=="NA")
pigs_karlsruhe_check

pigs_karlsruhe<- pigs_karlsruhe %>% filter(!NUTS_2=="NA")
pigs_karlsruhe %>% filter(!pigs=="total_pigs")
pigs_karlsruhe %>% group_by(NUTS_2) %>% count() ## in total there are 7 counties with pigs in RP Karlsruhe


pigs_karlsruhe_check %>% filter(!region=="RP Karlsruhe") %>%filter(!pigs=="total_pigs")%>% mutate(lin_adjust=No./7)


pigs_karlsruhe<-pigs_karlsruhe %>% filter(!pigs=="total_pigs") %>% 
  left_join(
    pigs_karlsruhe_check %>% filter(!region=="RP Karlsruhe") %>%filter(!pigs=="total_pigs")%>% mutate(lin_adjust=No./7) %>% select(-c(region, NUTS_2, RP, No.)), by="pigs")

pigs_karlsruhe<-pigs_karlsruhe %>% rowwise() %>% mutate(adj_No.=No.+lin_adjust)



pigs_karlsruhe$adj_No.<- round(pigs_karlsruhe$adj_No.,digits = 0)
pigs_karlsruhe

# does it now match the total amount of pigs in BaWue Karlsruhe after rounding? In total there are 68,743 pigs in Bawue Karlsruhe
pigs_karlsruhe_check

pigs_karlsruhe %>%ungroup() %>% summarize(No.pigs_karlsruhe=sum(adj_No.)) # after linearly upscaling across all NUTS with pigs there 2 pigs more due to rounding 


# How much % is the increase now?
# percentag increase haelt sich in Grenzen, skalierung bedeutet jedoch, dass Regionen mit wenig Schweinen einen höheren prozentualen Saklierungseffekt erfahren
# andere Argumentationen waeren diese Anzahl vermehrt Regionen mit eh schon vielen Schweinen zuzuschlagen, aber vorerst bleibt das so
pigs_karlsruhe<-pigs_karlsruhe %>% rowwise() %>% mutate(percent_increase=(adj_No.- No.)/adj_No.*100)
pigs_karlsruhe %>% print(n=Inf)

###############################################################################################################################################################################################
### Third freiburg
pigs_freiburg

pigs_freiburg<-pigs_freiburg %>% filter(total_pigs>0) %>% pivot_longer(cols = c(total_pigs, Ferkel, Zuchtsauen, `andere Schweine`), names_to = "pigs") %>% rename(No.="value")
print(pigs_freiburg, n=Inf)


pigs_freiburg_check<- pigs_freiburg %>% filter(NUTS_2=="NA")
pigs_freiburg_check

pigs_freiburg<- pigs_freiburg %>% filter(!NUTS_2=="NA")
pigs_freiburg %>% filter(!pigs=="total_pigs")
pigs_freiburg %>% group_by(NUTS_2) %>% count() ## in total there are 8 counties with pigs in RP Freiburg


pigs_freiburg_check %>% filter(!region=="RP Freiburg") %>%filter(!pigs=="total_pigs")%>% mutate(lin_adjust=No./8)


pigs_freiburg<-pigs_freiburg %>% filter(!pigs=="total_pigs") %>% 
  left_join(
    pigs_freiburg_check %>% filter(!region=="RP Freiburg") %>%filter(!pigs=="total_pigs")%>% mutate(lin_adjust=No./8) %>% select(-c(region, NUTS_2, RP, No.)), by="pigs")

pigs_freiburg<-pigs_freiburg %>% rowwise() %>% mutate(adj_No.=No.+lin_adjust)



pigs_freiburg$adj_No.<- round(pigs_freiburg$adj_No.,digits = 0)
pigs_freiburg

# does it now match the total amount of pigs in BaWue Freiburg after rounding? In total there are 103,557 pigs in Bawue Freiburg
pigs_freiburg_check

pigs_freiburg %>%ungroup() %>% summarize(No.pigs_freiburg=sum(adj_No.)) # after linearly upscaling across all NUTS with pigs there are 103,565 pigs  due to rounding 


# How much % is the increase now?
# percentag increase haelt sich in Grenzen, skalierung bedeutet jedoch, dass Regionen mit wenig Schweinen einen höheren prozentualen Saklierungseffekt erfahren
# andere Argumentationen waeren diese Anzahl vermehrt Regionen mit eh schon vielen Schweinen zuzuschlagen, aber vorerst bleibt das so
pigs_freiburg<-pigs_freiburg %>% rowwise() %>% mutate(percent_increase=(adj_No.- No.)/adj_No.*100)
pigs_freiburg %>% print(n=Inf)


#################################################################################################################################################
# Fourth pigs tuebingen

pigs_tuebingen <- No_animals_schweine %>% filter(RP=="Tuebingen")
pigs_tuebingen

pigs_tuebingen<-pigs_tuebingen %>% filter(total_pigs>0) %>% pivot_longer(cols = c(total_pigs, Ferkel, Zuchtsauen, `andere Schweine`), names_to = "pigs") %>% rename(No.="value")
print(pigs_tuebingen, n=Inf)


pigs_tuebingen_check<- pigs_tuebingen %>% filter(NUTS_2=="NA")
pigs_tuebingen_check

pigs_tuebingen<- pigs_tuebingen %>% filter(!NUTS_2=="NA")
pigs_tuebingen<-pigs_tuebingen %>% filter(!pigs=="total_pigs")
pigs_tuebingen %>% group_by(NUTS_2) %>% count() ## in total there are 9 counties with pigs in RP Tuebingen


pigs_tuebingen_check %>% filter(!region=="RP Tuebingen") %>%filter(!pigs=="total_pigs")%>% mutate(lin_adjust=No./9)

# no adjustement needed for tuebingen, the data is matching already
# pigs_tuebingen<-pigs_tuebingen %>% filter(!pigs=="total_pigs") %>% 
#   left_join(
#     pigs_tuebingen_check %>% filter(!region=="RP Tuebingen") %>%filter(!pigs=="total_pigs")%>% mutate(lin_adjust=No./7) %>% select(-c(region, NUTS_2, RP, No.)), by="pigs")
# 
# pigs_tuebingen<-pigs_tuebingen %>% rowwise() %>% mutate(adj_No.=No.+lin_adjust)



# pigs_tuebingen$adj_No.<- round(pigs_tuebingen$adj_No.,digits = 0)
# pigs_tuebingen

# does it now match the total amount of pigs in BaWue Karlsruhe after rounding? In total there are 68,743 pigs in Bawue Karlsruhe
# pigs_tuebingen_check
# 
# pigs_tuebingen %>%ungroup() %>% summarize(No.pigs_tuebingen=sum(No.)) # after linearly upscaling across all NUTS with pigs there 2 pigs more due to rounding 


# How much % is the increase now?
# percentag increase haelt sich in Grenzen, skalierung bedeutet jedoch, dass Regionen mit wenig Schweinen einen höheren prozentualen Saklierungseffekt erfahren
# andere Argumentationen waeren diese Anzahl vermehrt Regionen mit eh schon vielen Schweinen zuzuschlagen, aber vorerst bleibt das so
# pigs_tuebingen<-pigs_tuebingen %>% rowwise() %>% mutate(percent_increase=(adj_No.- No.)/adj_No.*100)
# pigs_tuebingen %>% print(n=Inf)

pigs_tuebingen %>% summarize(sum(No.))

###########################################################################################################################################
pigs_tuebingen
pigs_freiburg
pigs_stuttgart
pigs_karlsruhe

# bringing the data togehter
pigs_bawue<-rbind(pigs_stuttgart, pigs_karlsruhe, pigs_freiburg)
pigs_bawue<-pigs_bawue %>% mutate(adjusted_No.=TRUE)

head(pigs_tuebingen)
pigs_tuebingen<-pigs_tuebingen %>% mutate(lin_adjust=0, adj_No.=No., percent_increase=0, adjusted_No.=FALSE)

pigs_bawue<-rbind(pigs_bawue, pigs_tuebingen)
pigs_bawue<-pigs_bawue %>%as_tibble()



############################################################################################################################################
# Getting KTBL NPK per region 


if(laptob_work==TRUE) {
  setwd("C:\\Users\\User\\OneDrive - bwedu\\Dokumente\\Landwirtschaftliche Betriebslehre\\Projekt_P_Bawü\\GAMS_P\\Input_data\\Kalkulationsdaten\\KTBL\\Wirtschaftsduengeranfall")
} else {
  setwd("C:\\Users\\Tristan Herrmann\\OneDrive - bwedu\\Dokumente\\Landwirtschaftliche Betriebslehre\\Projekt_P_Bawü\\GAMS_P\\Input_data\\Kalkulationsdaten\\KTBL\\Wirtschaftsduengeranfall")
}


# performance levels from KTBL data
performance_level_schwein <- read_excel("schweinemast_ktbl.xlsx",sheet = "10.6_Schweinemast", skip=2 )
performance_level_schwein

performance_level_schwein<-performance_level_schwein %>% filter(`Einstreu kg FM/(TP · d)`=="0.3") %>% slice_head(n=3)
performance_level_schwein %>% mutate(pigs="andere Schweine")

## Estimating Mastschweine
mast_bawue <- pigs_bawue %>% filter(pigs=="andere Schweine") %>% select(region, NUTS_2, pigs, adj_No., adjusted_No.) %>% 
                   left_join(performance_level_schwein %>% mutate(pigs="andere Schweine", Futter="Standardfutter") %>%select(pigs, Futter, Einstreu=`Einstreu kg FM/(TP · d)`, 
                                                                                                                             Produkt=Wirtshaftsdüngerart, N,P2O5, K2O), 
                             by="pigs")

mast_bawue<-mast_bawue %>% mutate(N_kg_year=adj_No.*N,
                          P2O5_kg_year=adj_No.*P2O5,
                          K2O_kg_year=adj_No.*K2O)


mast_bawue


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

# KTBL data is for 8kg Lebendmasse, daher wird das gewaehlt, Standardfutter

performance_level_ferkelaufzucht_choice <- performance_level_ferkelaufzucht %>% mutate(pigs="Ferkel") %>% filter(`Leistung und Futergrundlage`=="143kg Zuwachs je Tierplatz und Jahr,Standardfutter")
performance_level_ferkelaufzucht_choice<-performance_level_ferkelaufzucht_choice %>% filter(`Einstreu kg FM/(TP · d)`== "0.1" | `Einstreu kg FM/(TP · d)`== "0.10")

ferkel<-pigs_bawue %>% filter(pigs=="Ferkel")
ferkel

## Estimate NPK for Ferkel in BaWue 2020
ferkel<- ferkel %>% left_join(performance_level_ferkelaufzucht_choice %>% select(`Einstreu kg FM/(TP · d)`:pigs), by="pigs") 

ferkel$N <- as.numeric(ferkel$N)
ferkel$P2O5 <- as.numeric(ferkel$P2O5)
ferkel$K2O <- as.numeric(ferkel$K2O)

ferkel_NPK<-ferkel %>% select(region:pigs, adj_No.:adjusted_No., Produkt, N:K2O) %>% mutate(N_kg_year=adj_No.*N,
                                                                                P2O5_kg_year=adj_No.*P2O5,
                                                                                K2O_kg_year=adj_No.*K2O)

# DONE schaetzung NPK for ferkeleraufzucht
if(laptob_work==TRUE) {
  write_xlsx(x=ferkel_NPK, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/18.04.23/ferkel_NPK.xlsx", col_names = TRUE)
  
} else {
  write_xlsx(x=ferkel_NPK, path = "C:/Users/Tristan Herrmann/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/18.04.23/ferkel_NPK.xlsx", col_names = TRUE)
  
}  

#####################################################################################################################################################################################################
## continue with Ferkelerzeugung

performance_level_ferkelerzeugung <- read_excel("fertillizer_sang.xlsx",sheet = "8.6_Ferkelerzeugung", skip=2 )
performance_level_ferkelerzeugung

# KTBL data is for 8kg Lebendmasse, daher wird das gewaehlt, Standardfutter

performance_level_ferkelerzeugung_choice <- performance_level_ferkelerzeugung %>% mutate(pigs="Zuchtsauen") %>% filter(Prod_sys=="Sauen und Ferkel bis 8 kg Lebendmasse, 28 Tag Säugezeit", 
                                                                                                                   `Einstreu kg FM/(Tier · d)`=="0.8",
                                                                                                                   `Leistung und Futergrundlage`=="26 Ferkel,279kg Zuwachs je Tierplatz und Jahr,Standardfutter")


performance_level_ferkelerzeugung_choice



#performance_level_ferkelerzeugung_choice<-performance_level_ferkelerzeugung_choice %>% filter(`Einstreu kg FM/(TP · d)`== "0.1" | `Einstreu kg FM/(TP · d)`== "0.10")

zuchtsauen<-pigs_bawue %>% filter(pigs=="Zuchtsauen")
zuchtsauen

## Estimate NPK for zuchtsauen in BaWue 2020
zuchtsauen<- zuchtsauen %>% left_join(performance_level_ferkelerzeugung_choice %>% select(`Einstreu kg FM/(Tier · d)`:pigs), by="pigs") 

zuchtsauen$N <- as.numeric(zuchtsauen$N)
zuchtsauen$P2O5 <- as.numeric(zuchtsauen$P2O5)
zuchtsauen$K2O <- as.numeric(zuchtsauen$K2O)

zuchtsauen_NPK<-zuchtsauen %>% select(region:pigs, adj_No.:adjusted_No., Produkt="Wirtshaftsdüngerart", N:K2O) %>% mutate(N_kg_year=adj_No.*N,
                                                                                            P2O5_kg_year=adj_No.*P2O5,
                                                                                            K2O_kg_year=adj_No.*K2O)
zuchtsauen_NPK

# DONE schaetzung NPK for ferkeleraufzucht
if(laptob_work==TRUE) {
  write_xlsx(x=zuchtsauen_NPK, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/18.04.23/zuchtsauen_NPK.xlsx", col_names = TRUE)
  
} else {
  write_xlsx(x=zuchtsauen_NPK, path = "C:/Users/Tristan Herrmann/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/18.04.23/zuchtsauen_NPK.xlsx", col_names = TRUE)
  
}  

###########################################################################################################################################################################################################
# Continue with Hähnchen und Geflügel, and then stop... evtl noch Abschlag für Pferde schätzen


# loading and data cleaning
No_gefluegel <- read_excel("Tierzahlen_maerz_2020.xlsx",sheet = "Gefluegel" )
No_gefluegel

No_gefluegel<-No_gefluegel %>% mutate(insgesamt = paste0(insgesamt...7, ...8)) %>% select(-c(insgesamt...7, ...8)) %>% mutate(Legehennen=paste0(Legehennen...10, ...11)) %>%
                 select(-c(Legehennen...10, ...11)) 

No_gefluegel

No_gefluegel<-No_gefluegel %>% select(Region, NUTS_2, RP, HalterInsgesamt=insgesamt...4, LegehennenhalterIn=Legehennen...5, MasthuehnerHalterIn=Masthuehner, insgesamt, Junghennen, Legehennen, `Masthuehner und Haehne`, ...13)
No_gefluegel



No_gefluegel <- No_gefluegel %>%
  mutate(Masthuehner_und_Haehne = ifelse(!is.na(...13), paste0(`Masthuehner und Haehne`, ...13), `Masthuehner und Haehne`)) %>% select(-c(`Masthuehner und Haehne`,...13))




No_gefluegel<-No_gefluegel %>%
  mutate_all(~na_if(., ".")) %>% mutate_all(~na_if(., "—")) %>% mutate_all(~na_if(., "_")) %>% mutate_all(~na_if(., "z—")) %>% mutate_all(~na_if(., ".NA"))  %>% mutate_all(~na_if(., "..")) %>%
  mutate_all(~na_if(., "—NA")) %>% mutate_all(~na_if(., ".—"))


No_gefluegel %>% print(n=Inf)

if(laptob_work==TRUE) {
  write_xlsx(x=No_gefluegel, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/GAMS_P/Input_data/Kalkulationsdaten/Agrarstrukturerhebung/Tierzahlen_bawue_März_2020/No_gefluegel.xlsx", col_names = TRUE)
  
} else {
  write_xlsx(x=No_gefluegel, path = "C:/Users/Tristan Herrmann/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/GAMS_P/Input_data/Kalkulationsdaten/Agrarstrukturerhebung/Tierzahlen_bawue_März_2020/No_gefluegel.xlsx", col_names = TRUE)
  
} 


## first step of cleaning done

## filling the blanks if necessary, same procedure as pigs
# 1.step filling the blanks by using the RP averages weighted for the region
# 1.1 if this results in negative values for estimated animals set to zero and substract from other estimated category
# step 2. use scaling on all that have pigs so that the amount of animals per RP is matched, and the dat is matching overall

if(laptob_work==TRUE) {
  setwd("C:\\Users\\User\\OneDrive - bwedu\\Dokumente\\Landwirtschaftliche Betriebslehre\\Projekt_P_Bawü\\GAMS_P\\Input_data\\Kalkulationsdaten\\Agrarstrukturerhebung\\Tierzahlen_bawue_März_2020")
} else {
  setwd("C:\\Users\\Tristan Herrmann\\OneDrive - bwedu\\Dokumente\\Landwirtschaftliche Betriebslehre\\Projekt_P_Bawü\\GAMS_P\\Input_data\\Kalkulationsdaten\\Agrarstrukturerhebung\\Tierzahlen_bawue_März_2020")
}


No_gefluegel <- read_excel("No_gefluegel_estimated.xlsx",sheet = "Gefluegel_estimated" )
No_gefluegel

No_gefluegel <- No_gefluegel %>% select(-c(HalterInsgesamt:MasthuehnerHalterIn))
No_gefluegel <- No_gefluegel %>% select(-c(`%Jungehennen`:`scaling factor`))
No_gefluegel <- No_gefluegel %>% select(-c(Check))

########################################################################################################################################

#scaling so that RP values are matched
gefluegel_stuttgart <- No_gefluegel %>% filter(RP=="Stuttgart")
gefluegel_karlsruhe <- No_gefluegel %>% filter(RP=="Karlsruhe")
gefluegel_tuebingen <- No_gefluegel %>% filter(RP=="Tuebingen")
gefluegel_freiburg <- No_gefluegel %>% filter(RP=="Freiburg")

# starting with Stuttgart
gefluegel_stuttgart

# filter for regions having gefluegel

gefluegel_stuttgart<-gefluegel_stuttgart %>% rename(total_gefluegel="insgesamt")

gefluegel_stuttgart<-replace_na(gefluegel_stuttgart, list(total_gefluegel = 0,
                                                          Junghennen=0,
                                                          Legehennen=0,
                                                          Masthuehner_und_Haehne=0))


gefluegel_stuttgart<-gefluegel_stuttgart  %>% pivot_longer(cols = c(total_gefluegel, Junghennen, Legehennen, Masthuehner_und_Haehne), names_to = "gefluegel")  %>%
rename(No.="value")

print(gefluegel_stuttgart, n=Inf)


gefluegel_stuttgart_check<- gefluegel_stuttgart %>% filter(NUTS_2=="NA")
gefluegel_stuttgart_check

gefluegel_stuttgart<- gefluegel_stuttgart %>% filter(!NUTS_2=="NA")
gefluegel_stuttgart %>% filter(!gefluegel=="total_gefluegel")
gefluegel_stuttgart %>% filter(Estimations_done==TRUE) %>% group_by(NUTS_2) %>% count() ## in total there are 7 counties with estimations for gefluegel in RP Stuttgart


gefluegel_stuttgart_check %>% filter(!Region=="RP Stuttgart") %>%filter(!gefluegel=="total_gefluegel")%>% mutate(lin_adjust=No./13)

# liner adjustment is done here to balance the dataset
# Here for Stuttgart die Abweichung wird geteilt durch die Anzahl der kreise mit  Geflügel und diskrepanz wird dann schlicht auf alle auf oder abgeschlagen:
# Allerdings möchte ich das nicht machen wenn die Daten auf Kreisebene eigentlich passe, also Check =0 gilt, ich möchte dieses lineare adjustent nur fuer Kreise machen wo ich "herumgeschaetzt" habe
# Wobei ich hier auch auf die STadtkreise aufpasen sollte 

# gefluegel_stuttgart <-gefluegel_stuttgart %>% filter(!gefluegel=="total_gefluegel") %>% 
#   left_join(
#     gefluegel_stuttgart_check %>% filter(!Region=="RP Stuttgart") %>%filter(!gefluegel=="total_gefluegel")%>% mutate(lin_adjust=No./13) %>% select(-c(Region, NUTS_2, RP, No.)), by="gefluegel")
# 
# gefluegel_stuttgart<-gefluegel_stuttgart %>% rowwise() %>% mutate(adj_No.=No.+lin_adjust)


# change of procedure
# first part of the join
# Esslingen ist ausgenommen von adjustment

gefluegel_stuttgart<-gefluegel_stuttgart %>% filter(!gefluegel=="total_gefluegel") %>% filter(Estimations_done==TRUE) %>%filter(!NUTS_2=="DE113") %>%
                     left_join(
                     gefluegel_stuttgart_check %>% filter(!Region=="RP Stuttgart") %>%filter(!gefluegel=="total_gefluegel")%>% select(-Estimations_done) %>%
                     mutate(lin_adjust=No./7) %>% select(-c(Region, NUTS_2, RP, No.)), by="gefluegel") %>% 
                     rbind(gefluegel_stuttgart%>% filter(Estimations_done==FALSE) %>%filter(!gefluegel=="total_gefluegel") %>% mutate(lin_adjust=0)) 

gefluegel_stuttgart %>% print(n=Inf)

gefluegel_stuttgart<-gefluegel_stuttgart %>% rowwise() %>% mutate(adj_No.=No.+lin_adjust)

gefluegel_stuttgart$adj_No.<- round(gefluegel_stuttgart$adj_No.,digits = 0)
gefluegel_stuttgart



# does it now match the total amount of gefluegel in BaWue Stuttgart after rounding? In total there are 2,079,973 gefluegel in Bawue stuttgart
gefluegel_stuttgart_check

gefluegel_stuttgart %>%ungroup() %>% summarize(No.gefluegel_stuttgart=sum(adj_No.)) # after linearly upscaling across all NUTS with gefluegel it matches 


# How much % is the increase now?
# percentag increase haelt sich in Grenzen, skalierung bedeutet jedoch, dass Regionen mit wenig Schweinen einen höheren prozentualen Saklierungseffekt erfahren
# andere Argumentationen waeren diese Anzahl vermehrt Regionen mit eh schon vielen Schweinen zuzuschlagen, aber vorerst bleibt das so

gefluegel_stuttgart %>% rowwise() %>% mutate(percent_increase=(adj_No.- No.)/adj_No.*100)
gefluegel_stuttgart %>% print(n=Inf)

#########################################################################################################################################################
# karlsruhe
gefluegel_karlsruhe <- No_gefluegel %>% filter(RP=="Karlsruhe")
gefluegel_karlsruhe

# filter for regions having gefluegel

gefluegel_karlsruhe<-gefluegel_karlsruhe %>% rename(total_gefluegel="insgesamt")

gefluegel_karlsruhe<-replace_na(gefluegel_karlsruhe, list(total_gefluegel = 0,
                                                          Junghennen=0,
                                                          Legehennen=0,
                                                          Masthuehner_und_Haehne=0))


gefluegel_karlsruhe<-gefluegel_karlsruhe  %>% pivot_longer(cols = c(total_gefluegel, Junghennen, Legehennen, Masthuehner_und_Haehne), names_to = "gefluegel")  %>%
  rename(No.="value")

print(gefluegel_karlsruhe, n=Inf)


gefluegel_karlsruhe_check<- gefluegel_karlsruhe %>% filter(NUTS_2=="NA")
gefluegel_karlsruhe_check

gefluegel_karlsruhe<- gefluegel_karlsruhe %>% filter(!NUTS_2=="NA")
gefluegel_karlsruhe %>% filter(!gefluegel=="total_gefluegel")
gefluegel_karlsruhe %>% filter(Estimations_done==TRUE) %>% group_by(NUTS_2) %>% count() ## in total there are 8 counties with estimations for gefluegel in RP Stuttgart


gefluegel_karlsruhe_check %>% filter(!Region=="RP Stuttgart") %>%filter(!gefluegel=="total_gefluegel")%>% mutate(lin_adjust=No./5)

# liner adjustment is done here to balance the dataset
# Here for Stuttgart die Abweichung wird geteilt durch die Anzahl der kreise mit  Geflügel und diskrepanz wird dann schlicht auf alle auf oder abgeschlagen:
# Allerdings möchte ich das nicht machen wenn die Daten auf Kreisebene eigentlich passe, also Check =0 gilt, ich möchte dieses lineare adjustent nur fuer Kreise machen wo ich "herumgeschaetzt" habe
# Wobei ich hier auch auf die STadtkreise aufpasen sollte 

# gefluegel_karlsruhe <-gefluegel_karlsruhe %>% filter(!gefluegel=="total_gefluegel") %>% 
#   left_join(
#     gefluegel_karlsruhe_check %>% filter(!Region=="RP Stuttgart") %>%filter(!gefluegel=="total_gefluegel")%>% mutate(lin_adjust=No./13) %>% select(-c(Region, NUTS_2, RP, No.)), by="gefluegel")
# 
# gefluegel_karlsruhe<-gefluegel_karlsruhe %>% rowwise() %>% mutate(adj_No.=No.+lin_adjust)


# change of procedure
# first part of the join
# Esslingen ist ausgenommen von adjustment

gefluegel_karlsruhe<-gefluegel_karlsruhe %>% filter(!gefluegel=="total_gefluegel") %>% filter(Estimations_done==TRUE)  %>%
  left_join(
    gefluegel_karlsruhe_check %>% filter(!Region=="RP Karlsruhe") %>%filter(!gefluegel=="total_gefluegel")%>% select(-Estimations_done) %>%
      mutate(lin_adjust=No./5) %>% select(-c(Region, NUTS_2, RP, No.)), by="gefluegel") %>% 
  rbind(gefluegel_karlsruhe%>% filter(Estimations_done==FALSE) %>%filter(!gefluegel=="total_gefluegel") %>% mutate(lin_adjust=0)) 

gefluegel_karlsruhe %>% print(n=Inf)

gefluegel_karlsruhe<-gefluegel_karlsruhe %>% rowwise() %>% mutate(adj_No.=No.+lin_adjust)

gefluegel_karlsruhe$adj_No.<- round(gefluegel_karlsruhe$adj_No.,digits = 0)
gefluegel_karlsruhe



# does it now match the total amount of gefluegel in BaWue Stuttgart after rounding? In total there are 295,710 gefluegel in Bawue stuttgart
gefluegel_karlsruhe_check

gefluegel_karlsruhe %>%ungroup() %>% summarize(No.gefluegel_karlsruhe=sum(adj_No.)) # after linearly upscaling across all NUTS with gefluegel it matches +1


# How much % is the increase now?
# percentag increase haelt sich in Grenzen, skalierung bedeutet jedoch, dass Regionen mit wenig Schweinen einen höheren prozentualen Saklierungseffekt erfahren
# andere Argumentationen waeren diese Anzahl vermehrt Regionen mit eh schon vielen Schweinen zuzuschlagen, aber vorerst bleibt das so

gefluegel_karlsruhe %>% rowwise() %>% mutate(percent_increase=(adj_No.- No.)/adj_No.*100)
gefluegel_karlsruhe %>% print(n=Inf)

##############################################################################################################################################################
#RP Freiburg 

gefluegel_freiburg <- No_gefluegel %>% filter(RP=="Freiburg")
gefluegel_freiburg

# filter for regions having gefluegel

gefluegel_freiburg<-gefluegel_freiburg %>% rename(total_gefluegel="insgesamt")

gefluegel_freiburg<-replace_na(gefluegel_freiburg, list(total_gefluegel = 0,
                                                          Junghennen=0,
                                                          Legehennen=0,
                                                          Masthuehner_und_Haehne=0))


gefluegel_freiburg<-gefluegel_freiburg  %>% pivot_longer(cols = c(total_gefluegel, Junghennen, Legehennen, Masthuehner_und_Haehne), names_to = "gefluegel")  %>%
  rename(No.="value")

print(gefluegel_freiburg, n=Inf)


gefluegel_freiburg_check<- gefluegel_freiburg %>% filter(NUTS_2=="NA")
gefluegel_freiburg_check

gefluegel_freiburg<- gefluegel_freiburg %>% filter(!NUTS_2=="NA")
gefluegel_freiburg %>% filter(!gefluegel=="total_gefluegel")
gefluegel_freiburg %>% filter(Estimations_done==TRUE) %>% group_by(NUTS_2) %>% count() ## in total there are 7 counties with estimations for gefluegel in RP Freiburg


gefluegel_freiburg_check %>% filter(!Region=="RP Freiburg") %>%filter(!gefluegel=="total_gefluegel")%>% mutate(lin_adjust=No./5)

# liner adjustment is done here to balance the dataset
# Here for Stuttgart die Abweichung wird geteilt durch die Anzahl der kreise mit  Geflügel und diskrepanz wird dann schlicht auf alle auf oder abgeschlagen:
# Allerdings möchte ich das nicht machen wenn die Daten auf Kreisebene eigentlich passe, also Check =0 gilt, ich möchte dieses lineare adjustent nur fuer Kreise machen wo ich "herumgeschaetzt" habe
# Wobei ich hier auch auf die STadtkreise aufpasen sollte 

# gefluegel_freiburg <-gefluegel_freiburg %>% filter(!gefluegel=="total_gefluegel") %>% 
#   left_join(
#     gefluegel_freiburg_check %>% filter(!Region=="RP Stuttgart") %>%filter(!gefluegel=="total_gefluegel")%>% mutate(lin_adjust=No./13) %>% select(-c(Region, NUTS_2, RP, No.)), by="gefluegel")
# 
# gefluegel_freiburg<-gefluegel_freiburg %>% rowwise() %>% mutate(adj_No.=No.+lin_adjust)


# change of procedure
# first part of the join
# Esslingen ist ausgenommen von adjustment

gefluegel_freiburg<-gefluegel_freiburg %>% filter(!gefluegel=="total_gefluegel") %>% filter(Estimations_done==TRUE)  %>%
  left_join(
    gefluegel_freiburg_check %>% filter(!Region=="RP Freiburg") %>%filter(!gefluegel=="total_gefluegel")%>% select(-Estimations_done) %>%
      mutate(lin_adjust=No./5) %>% select(-c(Region, NUTS_2, RP, No.)), by="gefluegel") %>% 
  rbind(gefluegel_freiburg%>% filter(Estimations_done==FALSE) %>%filter(!gefluegel=="total_gefluegel") %>% mutate(lin_adjust=0)) 

gefluegel_freiburg %>% print(n=Inf)

gefluegel_freiburg<-gefluegel_freiburg %>% rowwise() %>% mutate(adj_No.=No.+lin_adjust)

gefluegel_freiburg$adj_No.<- round(gefluegel_freiburg$adj_No.,digits = 0)
gefluegel_freiburg



# does it now match the total amount of gefluegel in BaWue Stuttgart after rounding? In total there are 295,710 gefluegel in Bawue stuttgart
gefluegel_freiburg_check

gefluegel_freiburg %>%ungroup() %>% summarize(No.gefluegel_freiburg=sum(adj_No.)) # after linearly upscaling across all NUTS with gefluegel it matches +1


# How much % is the increase now?
# percentag increase haelt sich in Grenzen, skalierung bedeutet jedoch, dass Regionen mit wenig Schweinen einen höheren prozentualen Saklierungseffekt erfahren
# andere Argumentationen waeren diese Anzahl vermehrt Regionen mit eh schon vielen Schweinen zuzuschlagen, aber vorerst bleibt das so

gefluegel_freiburg %>% rowwise() %>% mutate(percent_increase=(adj_No.- No.)/adj_No.*100)
gefluegel_freiburg %>% print(n=Inf)


#https://www.airmeet.com/e/4f6e7f80-df9b-11ed-8a4c-e342f8af2fa3

##################################################################################################################################################################
#RP Tuebingen

gefluegel_tuebingen <- No_gefluegel %>% filter(RP=="Tuebingen")
gefluegel_tuebingen

# filter for regions having gefluegel

gefluegel_tuebingen<-gefluegel_tuebingen %>% rename(total_gefluegel="insgesamt")

gefluegel_tuebingen<-replace_na(gefluegel_tuebingen, list(total_gefluegel = 0,
                                                        Junghennen=0,
                                                        Legehennen=0,
                                                        Masthuehner_und_Haehne=0))


gefluegel_tuebingen<-gefluegel_tuebingen  %>% pivot_longer(cols = c(total_gefluegel, Junghennen, Legehennen, Masthuehner_und_Haehne), names_to = "gefluegel")  %>%
  rename(No.="value")

print(gefluegel_tuebingen, n=Inf)


gefluegel_tuebingen_check<- gefluegel_tuebingen %>% filter(NUTS_2=="NA")
gefluegel_tuebingen_check

gefluegel_tuebingen<- gefluegel_tuebingen %>% filter(!NUTS_2=="NA")
gefluegel_tuebingen %>% filter(!gefluegel=="total_gefluegel")
gefluegel_tuebingen %>% filter(Estimations_done==TRUE) %>% group_by(NUTS_2) %>% count() ## in total there are 4 counties with estimations for gefluegel in RP Freiburg


gefluegel_tuebingen_check %>% filter(!Region=="RP Tuebingen") %>%filter(!gefluegel=="total_gefluegel")%>% mutate(lin_adjust=No./4)

# liner adjustment is done here to balance the dataset
# Here for Stuttgart die Abweichung wird geteilt durch die Anzahl der kreise mit  Geflügel und diskrepanz wird dann schlicht auf alle auf oder abgeschlagen:
# Allerdings möchte ich das nicht machen wenn die Daten auf Kreisebene eigentlich passe, also Check =0 gilt, ich möchte dieses lineare adjustent nur fuer Kreise machen wo ich "herumgeschaetzt" habe
# Wobei ich hier auch auf die STadtkreise aufpasen sollte 

# gefluegel_tuebingen <-gefluegel_tuebingen %>% filter(!gefluegel=="total_gefluegel") %>% 
#   left_join(
#     gefluegel_tuebingen_check %>% filter(!Region=="RP Stuttgart") %>%filter(!gefluegel=="total_gefluegel")%>% mutate(lin_adjust=No./13) %>% select(-c(Region, NUTS_2, RP, No.)), by="gefluegel")
# 
# gefluegel_tuebingen<-gefluegel_tuebingen %>% rowwise() %>% mutate(adj_No.=No.+lin_adjust)


# change of procedure
# first part of the join
# Esslingen ist ausgenommen von adjustment

gefluegel_tuebingen<-gefluegel_tuebingen %>% filter(!gefluegel=="total_gefluegel") %>% filter(Estimations_done==TRUE)  %>%
  left_join(
    gefluegel_tuebingen_check %>% filter(!Region=="RP Tuebingen") %>%filter(!gefluegel=="total_gefluegel")%>% select(-Estimations_done) %>%
      mutate(lin_adjust=No./5) %>% select(-c(Region, NUTS_2, RP, No.)), by="gefluegel") %>% 
  rbind(gefluegel_tuebingen%>% filter(Estimations_done==FALSE) %>%filter(!gefluegel=="total_gefluegel") %>% mutate(lin_adjust=0)) 

gefluegel_tuebingen %>% print(n=Inf)

gefluegel_tuebingen<-gefluegel_tuebingen %>% rowwise() %>% mutate(adj_No.=No.+lin_adjust)

gefluegel_tuebingen$adj_No.<- round(gefluegel_tuebingen$adj_No.,digits = 0)
gefluegel_tuebingen



# does it now match the total amount of gefluegel in BaWue Stuttgart after rounding? In total there are 295,710 gefluegel in Bawue stuttgart
gefluegel_tuebingen_check

gefluegel_tuebingen %>%ungroup() %>% summarize(No.gefluegel_tuebingen=sum(adj_No.)) # after linearly upscaling across all NUTS with gefluegel it matches +1


# How much % is the increase now?
# percentag increase haelt sich in Grenzen, skalierung bedeutet jedoch, dass Regionen mit wenig Schweinen einen höheren prozentualen Saklierungseffekt erfahren
# andere Argumentationen waeren diese Anzahl vermehrt Regionen mit eh schon vielen Schweinen zuzuschlagen, aber vorerst bleibt das so

gefluegel_tuebingen %>% rowwise() %>% mutate(percent_increase=(adj_No.- No.)/adj_No.*100)
gefluegel_tuebingen %>% print(n=Inf)

# Note beim linear adjust will ich eigentlich dass nur die angepasst werden wo ich gefüllt habe, hier ist es jetzt so dass pauschal alle angepasst werden... also in vielen Fällen muss ich die Legehennen ausklammern



