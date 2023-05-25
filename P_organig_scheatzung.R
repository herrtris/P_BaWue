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

laptob_work <- FALSE

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


milchkuehe<-milchkuehe %>% mutate(all_cows=cows+other_cows) %>% select(NUTS_2, region, `Fleckvieh %`:all_cows)
milchkuehe

# wie weit bin ich von den 100% je kreis weg?
milchkuehe<-milchkuehe %>% rowwise()%>%mutate(check=sum(`Fleckvieh %`,`Holsteins-Rbt`,`Holstein-sbt %`, `Braunvieh %`)) %>% mutate(diff=100-check)
milchkuehe

# easy approach: diff wird einfach proportional auf alle Rassen aufgeschlagen
# Proportionales aufschlage der Prozentzahlen, sodass 100% entstehen
milchkuehe<-milchkuehe %>% mutate(`Fleckvieh %`=`Fleckvieh %`+diff/4,
                      `Braunvieh %`=`Braunvieh %`+diff/4,
                      `Holsteins-Rbt`=`Holsteins-Rbt`+diff/4,
                      `Holstein-sbt %`=`Holstein-sbt %`+diff/4)

milchkuehe %>% rowwise()%>%mutate(check=sum(`Fleckvieh %`,`Holsteins-Rbt`,`Holstein-sbt %`, `Braunvieh %`)) %>% mutate(diff=100-check)


adjusted_race_proportions <- milchkuehe %>% select(NUTS_2:`Holsteins-Rbt`)
adjusted_race_proportions %>% mutate(sum(`Braunvieh %`,`Fleckvieh %`, `Holstein-sbt %`,`Holsteins-Rbt`)) %>% print(n=Inf)



milchkuehe<-milchkuehe %>% select(-c(diff, check, `Vorderwälder %`, `Hinterwälder %`))
milchkuehe %>% print(n=Inf)

# splitting the cows by races
# Anzahl der Kühe je Kreis, je Rasse
milchkuehe<-milchkuehe %>% mutate(Fleckvieh=all_cows*`Fleckvieh %`/100,
                                  Braunvieh=all_cows*`Braunvieh %`/100,
                                  Holstein_rbt=all_cows*`Holsteins-Rbt`/100,
                                  Holstein_sbt=all_cows*`Holstein-sbt %`/100)

milchkuehe
milchkuehe$Fleckvieh<-round(milchkuehe$Fleckvieh,digits = 0)
milchkuehe$Braunvieh<-round(milchkuehe$Braunvieh,digits = 0)
milchkuehe$Holstein_rbt<-round(milchkuehe$Holstein_rbt,digits = 0)
milchkuehe$Holstein_sbt<-round(milchkuehe$Holstein_sbt,digits = 0)

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
milchkuehe<-milchkuehe %>% select(NUTS_2:region, Fleckvieh:Holstein_sbt) %>% pivot_longer(cols = c(Fleckvieh, Braunvieh, Holstein_rbt, Holstein_sbt), names_to = "Race")
milchkuehe <- milchkuehe %>% rename(No_animals=value)
milchkuehe
milchkuehe %>% print(n=Inf)

# Durchnittsdaten je Rasse für Bawue, kreisdurchschnitte sind nicht verfügbar
milchkuehe_2 <-milchkuehe %>% mutate(avg_milk_production_race=case_when(
    Race=='Fleckvieh'       ~ 7966,  #Annahme Durchschnitt
    Race=='Braunvieh'       ~ 7751,     #Annahme Durschnitt
    Race=='Holstein_rbt'    ~ 8518,      #Annahme Milchkuh
    Race=='Holstein_sbt'    ~ 9522,    #Annahme Durchscnitt
    
  ))


milchkuehe_2

# Erste hochrechnung der Durchschnittsleistung per kuh je kreis über die Rassen 
milchkuehe_2<-milchkuehe_2 %>% mutate(milk_production=No_animals*avg_milk_production_race)

avg_milk_prod_race<-milchkuehe_2 %>%  group_by(NUTS_2) %>% summarize(animals_per_region=sum(No_animals), sum_milk_prod=sum(milk_production))%>%
                                       mutate(avg_milk_prod=sum_milk_prod/animals_per_region)

avg_milk_prod_race

# Vergleich zu den LKV durchschnittsdaten je kreis
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


######## linear adjustment to match the county average production level
# der berechnete durchschnitt wird angepasst um den Kreisdurchschnitt zu treffen
milchkuehe_2<-milchkuehe_2 %>% left_join(diskrepanz, by="NUTS_2")
milchkuehe_2

# Diskrepanz wird auf jede Rasse auf oder abgeschlagen
milchkuehe_2<-milchkuehe_2 %>% mutate(adjusted_avg_milk_production=avg_milk_production_race+diskrepanz_kreisdurchschnitt)
milchkuehe_2

milchkuehe_2<-milchkuehe_2 %>% mutate(adj_milk_production=No_animals*adjusted_avg_milk_production)


# check if Milchleistung matches now den Kreisdurchschnitt der LKV per region
avg_milk_production_2021
adj_avg_milk_production<- milchkuehe_2 %>%  group_by(NUTS_2) %>% summarize(animals_per_region=sum(No_animals, na.rm=T), sum_milk_prod=sum(adj_milk_production, na.rm=T))%>%
                  mutate(avg_milk_prod=sum_milk_prod/animals_per_region)


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



NUTS_2_milk_performance_level <- rbind(Fleckvieh, Braunvieh,sbt_rbt)



### joining data with the performance level
Fleckvieh <-Fleckvieh %>%  left_join(performance_level%>% filter(Rasse=="Fleckvieh"), by="performance_level")
Braunvieh <-Braunvieh %>%  left_join(performance_level%>% filter(Rasse=="Fleckvieh"), by="performance_level")
sbt_rbt   <- sbt_rbt %>% left_join(performance_level%>% filter(Rasse=="Sbt_HF"), by="performance_level")


### Getting the scaling factors. Ich möchte die Düngermenge anpassen basierend auf die durchschnittliche Milchleistung je Kreis
Fleckvieh
#FM_kg_Tier_jahr,TM_kg_Tier_jahr,N_kg_Tier_jahr,P205_kg_Tier_jahr, K20_kg_Tier_jahr Kalkulation_milchleistung

cows_PKN <- rbind(Fleckvieh,Braunvieh,sbt_rbt)
 

cows_PKN<-cows_PKN %>% mutate(FM_scaling_factor=FM_kg_Tier_jahr/Kalkulation_milchleistung,
                              TM_scaling_factor=TM_kg_Tier_jahr/Kalkulation_milchleistung,
                              N_scaling_factor=N_kg_Tier_jahr/Kalkulation_milchleistung,
                              P205_scaling_factor=N_kg_Tier_jahr/Kalkulation_milchleistung,
                              K20_scaling_factor=K20_kg_Tier_jahr/Kalkulation_milchleistung)
                      
cows_PKN <- cows_PKN %>% select(NUTS_2:Rasse, Produkt, FM_scaling_factor:K20_scaling_factor )
cows_PKN


# final calculation of N, P, K per region 
# question, this needs to be reduced by the amount that goes to Grünland and the amount that is not available to plants 

cows_PKN<-cows_PKN %>% mutate(N_region_kgjahr=No_animals*N_scaling_factor*adjusted_avg_milk_production,
                    P205_region_kgjahr=No_animals*P205_scaling_factor*adjusted_avg_milk_production,
                    K20_region_kgjahr=No_animals*K20_scaling_factor*adjusted_avg_milk_production)


cows_PKN %>% select(NUTS_2:performance_level,Produkt, N_region_kgjahr:K20_region_kgjahr)

# NUTS_2 NPK regional estimate

if(laptob_work==TRUE) {
  write_xlsx(x=cows_PKN, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/18.04.23/cows_PKN.xlsx", col_names = TRUE)
  
} else {
  write_xlsx(x=cows_PKN, path = "C:/Users/Tristan Herrmann/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/18.04.23/cows_PKN.xlsx", col_names = TRUE)
  
}  


######################################################################################################################################################################

################### Duengerabschaetzung fuer Kaelber
###### Daten aus den Tierzahlen
No_animals
kaelber_U1 <- No_animals %>% select(NUTS_2=...2,region=...1,Kealber_U1_male=`Rinder Noch: davon`, Kaelber_U1_female=...12)
kaelber_U1
kaelber_U1<-kaelber_U1[-c(1:2),]
kaelber_U1$Kealber_U1_male <- as.numeric(kaelber_U1$Kealber_U1_male)
kaelber_U1$Kaelber_U1_female<- as.numeric(kaelber_U1$Kaelber_U1_female)
kaelber_U1$Kealber_U1_male<- round(kaelber_U1$Kealber_U1_male, digits = 0)
kaelber_U1$Kaelber_U1_female<- round(kaelber_U1$Kaelber_U1_female, digits = 0)



kaelber_U12 <- No_animals %>% select(NUTS_2=...2,region=...1,Kealber_U12_male=...13, Kaelber_U12_female=...14)
kaelber_U12<-kaelber_U12[-c(1:2),]
kaelber_U12
kaelber_U12$Kealber_U12_male<- as.numeric(kaelber_U12$Kealber_U12_male)
kaelber_U12$Kaelber_U12_female<- as.numeric(kaelber_U12$Kaelber_U12_female)
kaelber_U12$Kealber_U12_male<- round(kaelber_U12$Kealber_U12_male, digits = 0)
kaelber_U12$Kaelber_U12_female<- round(kaelber_U12$Kaelber_U12_female, digits = 0)


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

adjusted_race_proportions
kaelber_U1 <- kaelber_U1 %>% rowwise()%>%mutate(total_kaelber=sum(Kaelber_U1_female, Kealber_U1_male))
kaelber_U1<-kaelber_U1 %>% left_join(adjusted_race_proportions, by="NUTS_2")
kaelber_U1<-kaelber_U1 %>% mutate(Fleckvieh_kU1=`Fleckvieh %`/100*total_kaelber,
                                  Braunvieh_kU1=`Braunvieh %`/100*total_kaelber,
                                  sbt_ku1=`Holstein-sbt %`/100*total_kaelber,
                                  rbt_ku1=`Holsteins-Rbt`/100*total_kaelber) %>% 
                           select(NUTS_2, region=region.x, total_kaelber,Fleckvieh_kU1:rbt_ku1)
            

kaelber_U1$Fleckvieh_kU1<- round(kaelber_U1$Fleckvieh_kU1)
kaelber_U1$Braunvieh_kU1 <- round(kaelber_U1$Braunvieh_kU1)
kaelber_U1$sbt_ku1<- round(kaelber_U1$sbt_ku1)
kaelber_U1$rbt_ku1<- round(kaelber_U1$rbt_ku1)

kaelber_U1<-kaelber_U1 %>% pivot_longer(c(Fleckvieh_kU1,Braunvieh_kU1,sbt_ku1,rbt_ku1)) %>% rename(Rasse="name", No_animals="value")
kaelber_U1


kaelber_U1F<-kaelber_U1%>% filter(Rasse=="Fleckvieh_kU1") %>% left_join(NUTS_2_milk_performance_level%>%filter(Race=="Fleckvieh") %>%
                                                             select(NUTS_2, performance_level), by="NUTS_2")

kaelber_U1B<-kaelber_U1%>% filter(Rasse=="Braunvieh_kU1") %>% left_join(NUTS_2_milk_performance_level%>%filter(Race=="Braunvieh") %>%
                                                                          select(NUTS_2, performance_level), by="NUTS_2")

kaelber_U1sbt<-kaelber_U1%>% filter(Rasse=="sbt_ku1") %>% left_join(NUTS_2_milk_performance_level%>%filter(Race=="Holstein_sbt") %>%
                                                                          select(NUTS_2, performance_level), by="NUTS_2")

kaelber_U1rbt<-kaelber_U1%>% filter(Rasse=="rbt_ku1") %>% left_join(NUTS_2_milk_performance_level%>%filter(Race=="Holstein_rbt") %>%
                                                                          select(NUTS_2, performance_level), by="NUTS_2")


#kaelber_U1<-rbind(kaelber_U1F, kaelber_U1B, kaelber_U1rbt, kaelber_U1sbt)


performance_level_kaelber <- read_excel("overview_wirtschaftduengeranfall.xlsx",sheet = "Kaelberaufzucht" )
performance_level_kaelber


kaelber_U1F<-kaelber_U1F %>% left_join(performance_level_kaelber%>% filter(Rasse=="Fleckvieh") %>% select(-c(Einstreu,Rasse)), by="performance_level")
kaelber_U1B<-kaelber_U1B %>% left_join(performance_level_kaelber%>% filter(Rasse=="Fleckvieh") %>% select(-c(Einstreu,Rasse)), by="performance_level")
kaelber_U1sbt<-kaelber_U1sbt %>% left_join(performance_level_kaelber%>% filter(Rasse=="Sbt_HF") %>% select(-c(Einstreu, Rasse)), by="performance_level")
kaelber_U1rbt<-kaelber_U1rbt %>% left_join(performance_level_kaelber%>% filter(Rasse=="Sbt_HF") %>% select(-c(Einstreu,Rasse)), by="performance_level")

kaelber_U1 <-rbind(kaelber_U1F,kaelber_U1B, kaelber_U1rbt,kaelber_U1sbt)
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






#https://www.airmeet.com/e/4f6e7f80-df9b-11ed-8a4c-e342f8af2fa3





