## #####################################################################################################################################################################################################################################
##########################################################################################################################################################################################################################################

# MODEL DIAGNOSTICS
###################################################################################################################################################################################################################################

# setting up the working directory to output-folder of GAMS model
# !! Careful a new rund changes the results, always store previous results in a seprate folder !!!
setwd("C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/GAMS_P")


library(dplyr)
#install.packages("tidyr")
library(tidyr)
library(readxl)
library(writexl)
library(ggplot2)
library(tidyverse)
library(stringr)

################################################################################# Anbau Diagnostics ################################################################################################################

### Anbau Fragestellungen die überprüft werden müssen

# 1.) Wird die Struktur der Bodenguete eingehalten, also auf High nur high, low, medium; auf medium nur medium or low, auf low nur low
# 2.) Wird die vollständige Fläche ausgenutzt oder gibt es Brachen? Wenn ja in welchen REgionen?
# 3.) Werden in den Kreisen auch nur die Fruchtfolgen angebaut die erlaubt sind in DB_Cr_verfuegbar?
# 4.) Wie weit ist das Modell weg vom status quo? Hier ist ein test der crop shares notwendig je kreis. 


# 1.) Struktur der Bodenguete und Intensity, wird die struktur eingehalten 

CropProd_levels <- read_excel("CropProd.xlsx", sheet="Tabelle1")
CropProd_levels <- CropProd_levels %>% rename(plot_id="...1", counties="...2", CR_id="...3")
CropProd_levels %>% head(n=20)

#Einfuegen der bodenguete information aus status quo
Landuse_plots <-read_excel("Landuse_plots_P.xlsx")
Landuse_plots <- Landuse_plots %>% rename(plot_id="PLOT_ID")

# Fruchtfolgen sind mir fuer diesen check egal
check_intensity_bodenguete <- CropProd_levels %>% group_by(plot_id, counties) %>% summarize(low_grouped=sum(low, na.rm = T)) %>% left_join(CropProd_levels %>% group_by(plot_id) %>% summarize(medium_grouped=sum(medium, na.rm = T)), by="plot_id") %>%
                     left_join(CropProd_levels %>% group_by(plot_id) %>% summarize(high_grouped=sum(high, na.rm = T)), by="plot_id")

# WErden Stufungen eingehalten? # Doing a left_join to get bodenguete
check_intensity_bodenguete$plot_id <- as.double(check_intensity_bodenguete$plot_id)
check_intensity_bodenguete <- check_intensity_bodenguete %>% left_join(Landuse_plots %>% select( plot_id,Bodenguete, P_level_category), by="plot_id")

# Doing the check # low
check_intensity_bodenguete %>% filter(Bodenguete=="low") %>% mutate(check=if_else(medium_grouped>0 | high_grouped>0, "WRONG", "CORRECT")) %>% filter(check=="WRONG")

# Doing the check # medium
check_intensity_bodenguete %>% filter(Bodenguete=="medium") %>% mutate(check=if_else(high_grouped>0, "WRONG", "CORRECT")) %>% filter(check=="WRONG")
  # wird auf meidum, low angebaut?
  check_intensity_bodenguete %>% filter(Bodenguete=="medium") %>% filter(low_grouped>0)

  # wird auf high, low oder medium angebaut?
  check_intensity_bodenguete %>% filter(Bodenguete=="high") %>% filter(low_grouped>0 | medium_grouped>0)

  
# 1.) Result: Die struktur der Intensitaet wird eingehalten
###############################################################################################################################################################################################################

# 2.) Wird die vollständige Fläche ausgenutzt oder gibt es Brachen? Wenn ja in welchen Regionen?
# Input data #738,981 ha arabel land in bawue
Landuse_plots
Landuse_plots %>% summarize(total_ha_bawue = sum(sum_ha))
check_intensity_bodenguete %>% pivot_longer(low_grouped:high_grouped) %>% ungroup() %>% summarize(model_total_ha_bawue= sum(value))

Landuse_plots %>% group_by(Bodenguete) %>% summarize(total_ha_bawue = sum(sum_ha))
check_intensity_bodenguete %>% pivot_longer(low_grouped:high_grouped) %>% rename(intensity="name")%>% group_by(intensity, Bodenguete) %>% summarize(model_total_ha_bawue= sum(value))


# result data #738,796 ha arable land used in 
CropProd_levels %>% count(plot_id) # 277 anstatt 294 plot_ids
check_intensity_bodenguete %>% pivot_longer(low_grouped:high_grouped) %>% ungroup() %>% summarize(model_total_ha_bawue= sum(value))
check_intensity_bodenguete %>% pivot_longer(low_grouped:high_grouped) %>% rename(intensity="name")%>% group_by(intensity, Bodenguete) %>% summarize(model_total_ha_bawue= sum(value))

# The biggest difference is in the group low. 42 zu 30.000 in the status quo to the model

# Are there plot_ids that are in landuse plot and not in the model output? How much are they in ha, if yes?
Landuse_plots %>% anti_join(check_intensity_bodenguete, by="plot_id") #17 plot_ids are not used for cropping
Landuse_plots %>% anti_join(check_intensity_bodenguete, by="plot_id") %>% summarize(not_in_model_ha=sum(sum_ha))
Landuse_plots %>% anti_join(check_intensity_bodenguete, by="plot_id") %>% group_by(Bodenguete)%>% summarize(not_in_model_ha=sum(sum_ha))

# auf high feldern wird auf jeden Fall etwas angebaut.Dennoch muss es Plot-id gruppen geben, die nicht vollkommen befuellt sind. 

#################################################################################################################################################################################################################
# 3.) Werden in den Kreisen auch nur die Fruchtfolgen angebaut die erlaubt sind in DB_Cr_verfuegbar?

# Info welche Fruchtfolgen je Kreis moeglich sind und deren DBS
DB_cr_verfuegbar <- read_excel("DB_CR_verfuegbar.xlsx", sheet="Tabelle1")
DB_cr_verfuegbar<- DB_cr_verfuegbar %>% rename(plot_id="...1", counties="...2",CR_id="...3" )
DB_cr_verfuegbar

# Select only the crop rotations 
#DB_cr_verfuegbar<- DB_cr_verfuegbar %>% select(plot_id:CR_id)

# test fuer ein countie
DB_cr_verfuegbar %>% filter(counties=="DE111")  %>% distinct(CR_id)


# Model anbau der crop rotations
model<- CropProd_levels %>% select(plot_id:CR_id) %>% filter(counties=="DE111") %>% distinct(CR_id)
CropProd_levels  %>% filter(counties=="DE111")
CropProd_levels  %>% filter(counties=="DE111") %>% distinct(CR_id)

# checke andere kreise
CropProd_levels  %>% filter(counties=="DE121") %>% distinct(CR_id)
DB_cr_verfuegbar %>% filter(counties=="DE121") %>% distinct(CR_id)

CropProd_levels  %>% filter(counties=="DE113") %>% distinct(CR_id)
DB_cr_verfuegbar %>% filter(counties=="DE113") %>% distinct(CR_id)

CropProd_levels  %>% filter(counties=="DE116") %>% distinct(CR_id)
DB_cr_verfuegbar %>% filter(counties=="DE116") %>% distinct(CR_id)


# Define a vector of regions
regions <- c("DE111", "DE112", "DE113", "DE114", "DE115", "DE116", "DE117", "DE118", "DE119", "DE11A","DE11B","DE11C","DE11D", 
             "DE121","DE122","DE123","DE124","DE125", "DE126", "DE127", "DE128", "DE129", "DE12A","DE12B", "DE12C",
             "DE131","DE132","DE133","DE134", "DE135","DE136","DE137","DE138","DE139","DE13A",
             "DE141","DE142","DE143","DE144","DE145","DE146","DE147","DE148","DE149")

# Create an empty list to store the results for each region
results_list <- list()

# Loop through each region
for (region in regions) {
  # Filter and get distinct CR_ids from CropProd_levels for the current region
  crops_from_cropprod <- CropProd_levels %>%
    filter(counties == region) %>%
    distinct(CR_id)
  
  # Filter and get distinct CR_ids from DB_cr_verfuegbar for the current region
  crops_from_db_cr_verfuegbar <- DB_cr_verfuegbar %>%
    filter(counties == region) %>%
    distinct(CR_id)
  
  # Test if CR_ids from crops_from_cropprod are in crops_from_db_cr_verfuegbar
  crops_in_both <- crops_from_cropprod$CR_id %in% crops_from_db_cr_verfuegbar$CR_id
  
  # Store the results in the list
  results_list[[region]] <- crops_in_both
}

results_list

# if all true, all cropprod.levels crop rotations are allowed to be grwon in the region. THere is none, that is "illegal"

##########################################################################################################################################################################################################################
# 4.) Wie ist die Anbaustruktur im Kreis und warum werden machen Kreise Flächenmäßig nicht ausgeschöpft
# Betrachte die Fruchtfolgen, WElche crops sind im Verhältnis über oder unterrepräsentiert zu meinen Status quo daten 

#### ERgebnisse aus Modell
report_anbau <- read_excel("report_anbau.xlsx", sheet="Tabelle1")
report_anbau<- report_anbau %>% rename(counties="...1", crop="...2")

report_anbau<-report_anbau %>%
pivot_longer(cols = c("low", "medium", "high")) %>% rename(intensity="name", hectare ="value")

# crop in ha je kreis
crop_sum_ha<-report_anbau %>% group_by(counties, crop) %>% summarise(crop_sum_ha=sum(hectare, na.rm = T))
crop_sum_ha %>% print(n=Inf)

report_anbau %>% distinct(crop)
report_anbau %>% distinct(counties)
44*14 # quick check 616 is maximal row number

# How much is grown in Bawue overall compared to the status quo?
# First the modell
crop_sum_ha %>% group_by(crop) %>% summarise(bawue_model_CROPSUM=sum(crop_sum_ha, na.rm=T))

crop_sum_ha_2<- crop_sum_ha %>% pivot_wider(values_from = crop_sum_ha, names_from = crop)

# No the real data
setwd("C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/01.09.23")
crop_sum_quo <- read_excel("crop_sum_quo.xlsx", sheet="Sheet1")

crop_sum_quo %>% group_by(Kennung) %>% summarise(bawue_QUO_CROPSUM=sum(crop_sum_quo, na.rm=T))
crop_sum_quo %>% group_by(Kennung) %>% summarise(bawue_QUO_CROPSUM=sum(crop_sum_quo, na.rm=T)) %>% summarise(sum(bawue_QUO_CROPSUM))


# ! Comparison of crops ha count between model and status quo
# Compare the data
crop_comparison_bawue<- crop_sum_ha %>% group_by(crop) %>% summarise(bawue_model_CROPSUM=sum(crop_sum_ha, na.rm=T)) %>%
  left_join(
  crop_sum_quo %>%rename(crop="Kennung") %>% group_by(crop) %>% summarise(bawue_QUO_CROPSUM=sum(crop_sum_quo, na.rm=T)), by="crop") %>%
  mutate(model_minus_quo_absolut= bawue_model_CROPSUM- bawue_QUO_CROPSUM) %>% arrange(desc(model_minus_quo_absolut))

crop_comparison_bawue
# compare the shares for the rgion of BaWue, this can be a slide

crop_comparison_bawue %>% summarize(total_modelsum=sum(bawue_model_CROPSUM, na.rm=T), total_QUOsum=sum(bawue_QUO_CROPSUM, na.rm=T))
total_arable_land<-crop_comparison_bawue %>% summarize(total_modelsum=sum(bawue_model_CROPSUM, na.rm=T))


crop_comparison_bawue<- crop_comparison_bawue %>% mutate(crop_share_model=bawue_model_CROPSUM/total_arable_land$total_modelsum*100,
                                 crop_share_QUO=bawue_QUO_CROPSUM/total_arable_land$total_modelsum*100 )
       
       
crop_comparison_bawue<- crop_comparison_bawue %>% mutate(model_minus_quo_share=crop_share_model-crop_share_QUO)      

crop_comparison_bawue %>% arrange(desc(model_minus_quo_share))


###########################################################################################################################################################
# write out bawue descriptive results





# Now for crop and kreis
crop_sum_ha %>% group_by(counties,crop) %>% summarise(bawue_model_CROPSUM=sum(crop_sum_ha, na.rm=T)) %>%
  left_join(
  crop_sum_quo %>%rename(crop="Kennung", counties="NUTS_2") %>% group_by(counties,crop) %>% summarise(bawue_QUO_CROPSUM=sum(crop_sum_quo, na.rm=T)), by=c("counties","crop"))%>%
  mutate(model_minus_quo= bawue_model_CROPSUM- bawue_QUO_CROPSUM) %>% arrange(desc(model_minus_quo)) %>% print(n=Inf)

# KG SG, Gr, KL sind unterrepräsentier, Weizen und silomais, körnermais, winterraps, triticale sind ueberrepresentiert
# in welchen kreisen kommen die unter representierten am meisten im status quovor? sind da welche von notfilled dabei, besonder KG, gr und KL von interesse


KG_QUO<- crop_sum_quo %>%rename(crop="Kennung", counties="NUTS_2") %>% group_by(counties,crop) %>% summarise(bawue_QUO_CROPSUM=sum(crop_sum_quo, na.rm=T))%>% 
         filter(crop=="KG") %>% arrange(desc(bawue_QUO_CROPSUM)) %>% print(n=Inf)
    
# Info aus county_rey welche Kreise werden nicht befuellt
setwd("C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/GAMS_P")

county_res <- read_excel("county_res.xlsx", sheet="Tabelle1")
county_res

model_counties_NOTFILLED<- county_res %>% mutate(filled=if_else(level==upper, "Filled", "NOTFILLED")) %>% filter(filled=="NOTFILLED") %>% mutate(diff=upper-level)
model_counties_NOTFILLED

# Welche von den nicht befuellten haben den viel KG
KG_QUO %>% right_join(model_counties_NOTFILLED, by="counties")

# Was ist die häufigste Fruchtfolge die KG enthaelt im Modell
# Load all crop rotations that are optios in the modell
setwd("C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/01.09.23")
Cr_bawue_distinct <- read_excel("Cr_bawue_distinct.xlsx", sheet="Sheet1")

Cr_bawue_distinct$CRid <- as.character(Cr_bawue_distinct$CRid)
Cr_bawue_distinct

# Crop rotations je kreis in the model, which are available for each county
DB_cr_verfuegbar

# Combine
possible_CROPROTS<- DB_cr_verfuegbar %>% left_join(Cr_bawue_distinct%>% select(CR_id=CRid, Kombo), by="CR_id") %>% select(counties, CR_id,Kombo)

possible_CROPROTS<- possible_CROPROTS %>% mutate(combination=paste0(counties, (CR_id))) %>% distinct(combination, .keep_all=T)
possible_CROPROTS %>% mutate(combination=paste0(counties, (CR_id))) %>% distinct(combination, .keep_all=T) %>% filter(counties=="DE111")

possible_CROPROTS

# Does every county have a KG rotation?
possible_CROPROTS %>% mutate(KG_check=if_else(str_detect(Kombo, "KG")==T, "YES", "NO")) %>% filter(KG_check=="YES")

possible_CROPROTS %>% mutate(KG_check=if_else(str_detect(Kombo, "KG")==T, "YES", "NO")) %>% filter(KG_check=="YES") %>% distinct(combination, .keep_all = T) %>% print(n=Inf)
possible_CROPROTS %>% mutate(KG_check=if_else(str_detect(Kombo, "KG")==T, "YES", "NO")) %>% filter(KG_check=="YES") %>% distinct(combination, .keep_all = T) %>% print(n=Inf) %>% 
                      count(counties)  %>% arrange(desc(n))%>% print(n=Inf)

possible_CROPROTS %>% mutate(KG_check=if_else(str_detect(Kombo, "KG")==T, "YES", "NO")) %>% filter(KG_check=="YES") %>% distinct(combination, .keep_all = T) %>% print(n=Inf) %>% 
                      count(CR_id)  %>% arrange(desc(n))%>% print(n=Inf)

# every county has at least one KG rotation has an option, and most common are CRs 119, 108, 157

# Does every county have a GR rotation?
possible_CROPROTS %>% mutate(GR_check=if_else(str_detect(Kombo, "Gr")==T, "YES", "NO")) %>% filter(GR_check=="YES")

possible_CROPROTS %>% mutate(GR_check=if_else(str_detect(Kombo, "Gr")==T, "YES", "NO")) %>% filter(GR_check=="YES") %>% distinct(combination, .keep_all = T) %>% print(n=Inf)
possible_CROPROTS %>% mutate(GR_check=if_else(str_detect(Kombo, "Gr")==T, "YES", "NO")) %>% filter(GR_check=="YES") %>% distinct(combination, .keep_all = T) %>% print(n=Inf) %>% 
  count(counties)  %>% arrange(desc(n))%>% print(n=Inf)

possible_CROPROTS %>% mutate(GR_check=if_else(str_detect(Kombo, "Gr")==T, "YES", "NO")) %>% filter(GR_check=="YES") %>% distinct(combination, .keep_all = T) %>% print(n=Inf) %>% 
  count(CR_id)  %>% arrange(desc(n))%>% print(n=Inf)

# CR_id 117 in jedem kreis vorhanden?
possible_CROPROTS %>% mutate(GR_check=if_else(str_detect(Kombo, "Gr")==T, "YES", "NO")) %>% filter(GR_check=="YES") %>% distinct(combination, .keep_all = T) %>% filter(CR_id=="117")%>%
    print(n=Inf)




county_ha<- crop_sum_ha %>% group_by(counties) %>% summarise(county_ha=sum(crop_sum_ha, na.rm = T)) 

crop_shares_modell<- crop_sum_ha %>% left_join(county_ha, by="counties") %>% mutate(crop_share=crop_sum_ha/county_ha*100) 
crop_shares_modell %>% print(n=Inf)


# Load original crop shares status quo
setwd("C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/18.04.23")
crop_shares_quo <- read_excel("crop_share_bw_Zentroide_2.xlsx", sheet="Sheet1")
crop_shares_quo<- crop_shares_quo  %>% rename(crop="name")

crop_shares_quo <-crop_shares_quo %>% pivot_longer(cols = c("DE111", "DE112", "DE113", "DE114", "DE115", "DE116", "DE117", "DE118", "DE119", "DE11A","DE11B","DE11C","DE11D", 
                                          "DE121","DE122","DE123","DE124","DE125", "DE126", "DE127", "DE128", "DE129", "DE12A","DE12B", "DE12C",
                                          "DE131","DE132","DE133","DE134", "DE135","DE136","DE137","DE138","DE139","DE13A",
                                          "DE141","DE142","DE143","DE144","DE145","DE146","DE147","DE148","DE149")) %>% rename(counties="name") %>% select(counties, crop, crop_share_quo="value")



share_comparison<- full_join(crop_shares_modell, crop_shares_quo, by=c("counties" , "crop")) %>% rename(crop_sum_ha_modell="crop_sum_ha", county_ha_modell= "county_ha", crop_share_modell="crop_share")
share_comparison

# Info aus county_rey welche Kreise werden nicht befuellt
#setwd("C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/GAMS_P")

#county_res <- read_excel("county_res.xlsx", sheet="Tabelle1")
#county_res

#model_counties_NOTFILLED<- county_res %>% mutate(filled=if_else(level==upper, "Filled", "NOTFILLED")) %>% filter(filled=="NOTFILLED") %>% mutate(diff=upper-level)
#model_counties_NOTFILLED

## Select the counties that are not filled
share_comparison_NOTFILLED<-model_counties_NOTFILLED%>% select(counties)  %>%  left_join(share_comparison, by="counties") 
share_comparison_NOTFILLED <- share_comparison_NOTFILLED %>% mutate(crop_share_quo=crop_share_quo*100)
share_comparison_NOTFILLED %>% mutate(diff_share=crop_share_modell-crop_share_quo) %>% print(n=Inf)

share_comparison_NOTFILLED %>% print(n=Inf)

#share_comparison_NOTFILLED<- share_comparison_NOTFILLED %>% left_join(crop_sum_ha, by=c("counties", "crop")) %>% rename(crop_sum_ha_quo="crop_sum_ha") %>% mutate(diff_share=crop_share_modell-crop_share_quo)

# Load crop hectare sum of status quo
#setwd("C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/01.09.23")
#crop_sum_quo <- read_excel("crop_sum_quo.xlsx", sheet="Sheet1")


# combine with notfilled
share_comparison_NOTFILLED <- share_comparison_NOTFILLED %>% left_join(crop_sum_quo %>% rename(counties="NUTS_2", crop="Kennung"), by=c("counties", "crop")) %>% mutate(diff_crop_sum=crop_sum_ha_modell-crop_sum_quo)
share_comparison_NOTFILLED <- share_comparison_NOTFILLED %>% mutate(diff_crop_share=crop_share_modell-crop_share_quo)

share_comparison_NOTFILLED %>% print(n=Inf)


# Erkenntnis 05.09.23: Die crops Gr, KG und KL kommen in den Kreisen nicht vor obwohl sie im status quo sind
#                      Eine globale ERhöhung im Modell fuer Gr fuehrt aber zu infeasible


# 5. WElche crop rotations werden in den Kreisen angebaut oder sthen zur auswahl, die nicht beufuellt werden. 
setwd("C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/GAMS_P")

counties_NOTFILLED <- share_comparison_NOTFILLED %>% distinct(counties)


CropRot_NOTFILLED <- CropProd_levels %>% filter(CropProd_levels$counties %in% counties_NOTFILLED$counties) %>% select(counties:high) %>% group_by(counties, CR_id) 
CropRot_NOTFILLED <- CropRot_NOTFILLED %>% group_by(counties, CR_id) %>%summarise(across(c(low, medium, high), ~sum(.x,na.rm=T)))
CropRot_NOTFILLED %>% print(n=Inf)
#CropRot_NOTFILLED %>% group_by(counties, CR_id) %>%summarise(sum(low, na.rm = T))


# GEt CCR description                               
#setwd("C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/01.09.23")
#Cr_bawue_distinct <- read_excel("Cr_bawue_distinct.xlsx", sheet="Sheet1")

#Cr_bawue_distinct$CRid <- as.character(Cr_bawue_distinct$CRid)
CropRot_NOTFILLED<- CropRot_NOTFILLED %>% left_join(Cr_bawue_distinct %>% select(CR_id=CRid, Kombo), by="CR_id")
CropRot_NOTFILLED %>% print(n=Inf)


# I want to "force" Gr into the solution
CropRot_NOTFILLED %>%
        mutate(has_Ha = str_detect(Kombo, "Gr")) %>% print(n=Inf)

CropRot_NOTFILLED %>%
  mutate(has_Ha = str_detect(Kombo, "WG")) %>% print(n=Inf)

CropRot_NOTFILLED %>%
  mutate(has_Ha = str_detect(Kombo, "KG")) %>% print(n=Inf)

CropRot_NOTFILLED %>%
  mutate(has_KG = str_detect(Kombo, "KG")) %>% filter(counties=="DE11B")


CropRot_NOTFILLED %>%
  mutate(has_Ha = str_detect(Kombo, "KL")) %>% print(n=Inf)

