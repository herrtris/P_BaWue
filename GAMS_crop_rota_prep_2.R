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

# 1. Laden der Teildatasets
# Wegen der 2GB output beschränkung von ArcGis siond die DAten auf 4 csv files aufgeteilt, folgende Bearbeitungsschritte sind bisher passiert mit jedem der data packages
# Zentroid bildung (Mittelpunkt der Feldflächen) der GA21 Daten. 
# Verschneiden der zentroide mit der bodenkarte 50 um NATBOD zu bekommen
# Verschneiden mit der Karte der administrativen Granzen BaWüs, um die Feldzentroide Kreisen und Kommunen zuzuordenen
# Verschneiden mit der JRC Karte chemical soil properties phosphate um jedem Feld einen P_cal Wert zuzuordnen

# Alle Datasetes werden hier eingelesen:



# zenroid_p_1
zentroid_p1 <- read.csv("C:\\Users\\User\\OneDrive - bwedu\\Dokumente\\Landwirtschaftliche Betriebslehre\\Projekt_P_Bawü\\Spatial_data\\12.04\\P_zentroid_1.csv", 
                       sep=";", stringsAsFactors = T)

zentroid_p2 <- read.csv("C:\\Users\\User\\OneDrive - bwedu\\Dokumente\\Landwirtschaftliche Betriebslehre\\Projekt_P_Bawü\\Spatial_data\\12.04\\P_zentroid_2.csv", 
                        sep=";", stringsAsFactors = T)

zentroid_p3 <- read.csv("C:\\Users\\User\\OneDrive - bwedu\\Dokumente\\Landwirtschaftliche Betriebslehre\\Projekt_P_Bawü\\Spatial_data\\12.04\\P_zentroid_3.csv", 
                        sep=";", stringsAsFactors = T)

zentroid_p4 <- read.csv("C:\\Users\\User\\OneDrive - bwedu\\Dokumente\\Landwirtschaftliche Betriebslehre\\Projekt_P_Bawü\\Spatial_data\\12.04\\P_zentroid_4.csv", 
                        sep=";", stringsAsFactors = T)


## short check of read datasets zentroidp1 to 4
glimpse(zentroid_p1)
glimpse(zentroid_p2)
glimpse(zentroid_p3)
glimpse(zentroid_p4)


#binding zentroid file 1 to 4 to one zentroid file
zentroid_p <- rbind(zentroid_p1, zentroid_p2, zentroid_p3, zentroid_p4)

glimpse(zentroid_p)


## Making field area numeric
zentroid_p$FLAECHE_HA<-   gsub(",",".",zentroid_p$FLAECHE_HA)
zentroid_p$FLAECHE_HA <- as.numeric(zentroid_p$FLAECHE_HA)
glimpse(zentroid_p)




# old shapefile data drom qgis verschniedung
# # shape file without P information; 1,406,122 obs of 196 variables
# zentroid_p <- read.csv("C:\\Users\\User\\OneDrive - bwedu\\Dokumente\\Landwirtschaftliche Betriebslehre\\Projekt_P_Bawü\\Spatial_data\\Zentroide\\Ver_centroidga21_bawuegemeinde_bodke.csv", 
#               sep=",", stringsAsFactors = T)
# 
# zentroid_p %>% distinct(NUTS_2)
# 
# 
# check<-zentroid_p %>% select(NUTS_2) %>% group_by(NUTS_2) %>% count()

# Loading area inforamtion on field usage
Flaechennutzung_Nutzcode <-read.csv("C:\\Users\\User\\OneDrive - bwedu\\Dokumente\\Landwirtschaftliche Betriebslehre\\Projekt_P_Bawü\\GAMS\\Modell-AG\\Modell-AG\\Flaechennutzung_Nutzcode.csv", sep=";") 
str(Flaechennutzung_Nutzcode)


# lodaing P_level at the county level from MLR to replace zeroes of JRC map
county_P_levels <- read.csv("C:\\Users\\User\\OneDrive - bwedu\\Dokumente\\Landwirtschaftliche Betriebslehre\\Projekt_P_Bawü\\Spatial_data\\12.04\\county_P_levels.csv", 
                        sep=";", stringsAsFactors = T)

head(county_P_levels)

county_P_levels <-county_P_levels %>% select(NUTS_2,AGS_0_2,P_level= P_2013___2,pH=pH_2013__, K=K_2013___2 )
county_P_levels


###################################### GA-Datensatz vorbereiten ###################################################################################################
##################################################################################################################################################################

Flaechennutzung_Nutzcode <- Flaechennutzung_Nutzcode %>% select(1:5) %>% filter(!is.na(NUTZCODE))
Flaechennutzung_Nutzcode$NUTZCODE<- as.factor(Flaechennutzung_Nutzcode$NUTZCODE)



#Creating a row ID, field ID
zentroid_p$OID_<- 1:nrow(zentroid_p)
max(zentroid_p$OID_)


zentroid_p <- zentroid_p %>% mutate(Schlag_ID=OID_) %>% select(Schlag_ID, FLAECHE_HA, NUTZCODE, NUTS_2, AGS_0_2, NATBOD, NUTZUNG, P_level=DN)


# joining landuse data with usage code of the area
glimpse(zentroid_p)
glimpse(Flaechennutzung_Nutzcode)


Flaechennutzung_Nutzcode$NUTZCODE <- as.factor(Flaechennutzung_Nutzcode$NUTZCODE)
zentroid_p$NUTZCODE <- as.factor(zentroid_p$NUTZCODE)

zentroid_p_joined<- zentroid_p %>% left_join(Flaechennutzung_Nutzcode, by="NUTZCODE") %>% filter(Flaechenart=="Ackerland")


### Joining based on "arable land" criteria yields 516,991 observations

## Creating the factor of soil quality
zentroid_p_joined %>% head(10)

zentroid_p_joined <- zentroid_p_joined %>%
  mutate(Bodenguete = ifelse(NATBOD %in% c("1,0"), "low",
                      ifelse(NATBOD %in% c("1,5"), "low",
                      ifelse(NATBOD %in% c( "2,0"), "medium",
                      ifelse(NATBOD %in% c("2,5"), "medium",
                      ifelse(NATBOD %in% c( "3,0"), "high",    
                      ifelse(NATBOD %in% c( "3,5"), "high",    
                      ifelse(NATBOD %in% c( "4,0"), "high","medium")))))))) 




# There is one NA for the NUTS_2 level
# Checking for NAs
summary(zentroid_p_joined)
levels(zentroid_p_joined$NUTS_2)
levels(zentroid_p_joined$NUTZUNG)


zentroid_p_joined$NUTS_2 <- gsub(" ","NA",zentroid_p_joined$NUTS_2)

zentroid_p_joined_filtered <- zentroid_p_joined%>%filter(!NUTS_2=="NA")

zentroid_p_joined_filtered$Kulturgruppe <- as.factor(zentroid_p_joined_filtered$Kulturgruppe)


levels(zentroid_p_joined_filtered$Kulturgruppe)

# zentroid_p_joined_filtered does still contain Brache


##########################################################################################################################################################################################
# Intermediate result: soil_qual in ha per county
## Checking the occurence in my dataset for soil qualities
## General
# ggplot(zentroid_p_joined_filtered, aes(x = Bodenguete, fill=NUTS_2)) +
#   geom_bar() +
#   labs(title = "Frequency of categories in soil_quality", 
#        x = "soil_qual", y = "Frequency")
# 
# 
# # Soil quality for each county, frequency table
# ggplot(zentroid_p_joined_filtered, aes(x = NUTS_2, fill=Bodenguete)) +
#   geom_bar() +
#   labs(title = "Frequency of categories in soil_quality", 
#        x = "NUTS_2", y = "Frequency")
# 
# 
# # Soil quality for each county, area in ha
# ggplot(zentroid_p_joined_filtered %>% group_by(Bodenguete, NUTS_2) %>%summarise(sum_FLAECHE_ha=sum(FLAECHE_HA)), aes(x = NUTS_2,y=sum_FLAECHE_ha, fill=Bodenguete)) +
#   geom_col() +
#   labs(title = "Frequency of categories in soil_quality", 
#        x = "NUTS_2", y = "area in ha")


zentroid_p_joined_filtered %>% summarize(Total_area=sum(FLAECHE_HA))
zentroid_p_joined_filtered %>% filter(!Kulturgruppe=="Brache") %>% summarize(n=n(), Total_area=sum(FLAECHE_HA))


### crop distribution across different NUTs_2 in FLAECHE_ha
# ggplot(zentroid_p_joined_filtered %>% group_by(Kulturgruppe, NUTS_2) %>%summarise(sum_FLAECHE_ha=sum(FLAECHE_HA)), aes(x = NUTS_2,y=sum_FLAECHE_ha, fill=Kulturgruppe)) +
#   geom_col() +
#   labs(title = "crop disrtbution across NUTS_2", 
#        x = "NUTS_2", y = "area in ha")
# 
# 
# ggplot(zentroid_p_joined_filtered %>% group_by(Kulturgruppe, NUTS_2) %>%summarise(sum_FLAECHE_ha=sum(FLAECHE_HA)), aes(x = Kulturgruppe,y=sum_FLAECHE_ha)) +
#   geom_col() + facet_wrap(~NUTS_2)+ theme(axis.text.x=element_text(angle=90,hjust=1))
#   labs(title = "crop disrtbution across NUTS_2", 
#        x = "NUTS_2", y = "area in ha")

#17.04.23
## After excluding NAs for NUTS_2 there are 516,915 fields with an area of 739,047 ha including "Brache"
## After excluding Brache and NAs there are 706,128ha and 439,740 fields 

# Previous result
# In total, 516905 Schlaege; GEsamtflaeche 739039.9 ha

zentroid_p_joined_filtered%>% group_by(Bodenguete) %>% count()



# creating a size scale for field area
zentroid_p_joined_filtered$ha_Kat = cut(zentroid_p_joined_filtered$FLAECHE_HA, c(0,2,5,10,1000), levels=c("1", "2", "5","10", "1000"))
zentroid_p_joined_filtered %>% summarise(sum_ha=sum(FLAECHE_HA))
zentroid_p_joined_filtered %>% select(Schlag_ID) %>% count()


#cheking the P-level per field
glimpse(zentroid_p_joined_filtered)
zentroid_p_joined_filtered$P_level <- as.numeric(zentroid_p_joined_filtered$P_level)
zentroid_p_joined_filtered<- zentroid_p_joined_filtered %>% mutate(P_level=P_level/10)

# # P_level for each county, boxplot
# ggplot(zentroid_p_joined_filtered, aes(y = P_level)) +
#   geom_boxplot() + facet_wrap(~NUTS_2)
#   labs(title = "Frequency of categories in soil_quality", 
#        x = "NUTS_2", y = "Frequency")
# 
# # histogram of P-levels all counties, we have more P_levels below 4 skewed to the left side
# 
# ggplot(zentroid_p_joined_filtered, aes(x =P_level)) +
#   geom_histogram(color = "black", fill = "white", binwidth = 0.5) +
#     labs(x = "P_level", y = "Frequency", title = "Histogram of P_levels all counties")
# 
# 
# # boxplot for all the region of bawue
# ggplot(zentroid_p_joined_filtered, aes(y = P_level)) +
#   geom_boxplot() +
# labs(title = "", 
#      x = "", y = "P_level")



# I have 5430 P_levels of 0 in the dataset, which cannot be... 
#####################################################################################################################################################################################################
## End intermediate results for status quo data


# Adjusting the P_level 
# Each P_level of zero will get the value at the county level from the map by the MLR in county_P_level dataset
head(county_P_levels)
head(zentroid_p_joined_filtered)

P_level_0 <-zentroid_p_joined_filtered %>% filter(P_level==0)
head(P_level_0)

#left_join based on AGS_0_2
P_level_0 %>% left_join(county_P_levels, by="AGS_0_2") 


zentroid_p_joined_filtered <-  zentroid_p_joined_filtered %>% left_join(county_P_levels, by="AGS_0_2")
head(zentroid_p_joined_filtered)

zentroid_p_joined_filtered <- zentroid_p_joined_filtered %>% rename(P_level_JRC="P_level.x", P_level_county="P_level.y")

## replacing P_level_JRC by the county P_level if JRC equals zero
str(zentroid_p_joined_filtered)
zentroid_p_joined_filtered$P_level_county<- as.numeric(zentroid_p_joined_filtered$P_level_county)


zentroid_p_joined_filtered<- zentroid_p_joined_filtered %>% mutate(P_level=if_else(P_level_JRC==0, P_level_county, P_level_JRC))

# checking the operation, now each value for that is zero in JRC for P should get the value at the county level assigned to it
# source LEL https://lel.landwirtschaft-bw.de/pb/,Lde/Startseite/Service_+Downloads/Glossar
P_level_0 <-zentroid_p_joined_filtered %>% filter(P_level_JRC==0)
head(P_level_0)

# now there are 103 ibs without a P-level value, they are treated as NA and are excluded from the dataset
P_level_0 <- zentroid_p_joined_filtered %>% filter(P_level==0)
head(P_level_0)

# filtering out the remaining 103 zero values
zentroid_p_joined_filtered <-  zentroid_p_joined_filtered %>% filter(!P_level==0)


# building P categories based on Buczko et al 

zentroid_p_joined_filtered %>% filter(P_level<=3.3)


zentroid_p_joined_filtered <- zentroid_p_joined_filtered %>%
  mutate(P_level_category = ifelse(P_level<=3.3, "low",
                            ifelse(P_level>3.3 & P_level <=5.8,"medium",
                            ifelse(P_level>5.8, "high", "check"      ))))       
    
zentroid_p_joined_filtered %>% filter(P_level >5.8)    
    
    
  




##########################################################################################################################################################################################
# Intermediate result: P_levels in the region in the status quo

# ggplot(zentroid_p_joined_filtered, aes(x =P_level)) +
#   geom_histogram(color = "black", fill = "white", binwidth = 0.5) +
#   labs(x = "P_level", y = "Frequency", title = "Histogram of P_levels all counties")
# 
# 
# # histograms per counties at NUTS_2
# # How do P_levels of fields differ regionally?
# levels(zentroid_p_joined_filtered$NUTS_2.y)
# 
# ggplot(zentroid_p_joined_filtered, aes(x =P_level)) +
#   geom_histogram(color = "black", fill = "white", binwidth = 0.5) + facet_wrap(~NUTS_2.y)+
#   labs(x = "P_level", y = "Frequency", title = "Histogram of P_levels all counties")
# 
# 
# # based on buczko categories P<=3.34 low; >3.34 and <=5.82 medium, and >5.82 high
# ggplot(zentroid_p_joined_filtered, aes(x = NUTS_2.y, fill=P_level_category)) +
#   geom_bar() +
#   labs(title = "Frequency of categories in soil_quality", 
#        x = "NUTS_2", y = "Frequency")
# 
# 
# # bocplot for all the region of bawue
# ggplot(zentroid_p_joined_filtered, aes(y = P_level)) +
#   geom_boxplot() + facet_wrap(~NUTS_2.y)+
#   labs(title = "", 
#        x = "", y = "P_level")
# 
# 
#   ggplot(zentroid_p_joined_filtered, aes(y = P_level)) +
#     geom_boxplot() + 
#   labs(title = "", 
#        x = "", y = "P_level")



#####################################################################################################################################################################################################
## End intermediate results for status quo data







# Starting to produce output for Crop Rota and GAMS MODEL
######################################################################################################################################################################################################
# Landuseplots
# Contains: plot_id, NUTS_2, P_level category, sum_ha 

zentroid_p_joined_filtered    <-zentroid_p_joined_filtered %>% select(-NUTS_2.y) %>% rename(NUTS_2 ="NUTS_2.x")
head(zentroid_p_joined_filtered)  
  
  
Landuse_plots_P <- zentroid_p_joined_filtered  %>% group_by(NUTS_2, Bodenguete, P_level_category) %>% 
                       summarise(sum_ha=sum(FLAECHE_HA))%>% ungroup() %>% mutate(PLOT_ID=row_number()) 


head(Landuse_plots_P)
Landuse_plots_P <- Landuse_plots_P[,c(5,1,2,3,4)]
Landuse_plots_P %>% group_by(NUTS_2)%>% count() #De125 hat nur 2 observations
Landuse_plots_P %>% filter(NUTS_2=="DE125")


write_xlsx(x=Landuse_plots_P, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/18.04.23/Landuse_plots_P.xlsx", col_names = TRUE)



### Translating soil quality into management intensity per region
### Contains: plotid, counties, intensity in ones and zeroes

plot_bodenguete <- Landuse_plots_P

plot_bodenguete <-  plot_bodenguete %>% mutate(low= ifelse(Bodenguete %in% "low",1,0))
plot_bodenguete<-   plot_bodenguete %>% mutate(medium= ifelse(Bodenguete %in% "medium",1,0))
plot_bodenguete<-   plot_bodenguete %>% mutate(high= ifelse(Bodenguete %in% "high",1,0))
plot_bodenguete <-plot_bodenguete %>% select(PLOT_ID, counties=NUTS_2, low, medium, high)
plot_bodenguete

write_xlsx(x=plot_bodenguete, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/18.04.23/plot_bodenguete.xlsx", col_names = TRUE)




#####################################################################################################################################################
### Area usage in status quo on county level
###

Landuse_SQ_P <- zentroid_p_joined_filtered %>% group_by(NUTS_2, Kennung) %>% summarise(sum_ha=sum(FLAECHE_HA))
head(Landuse_SQ_P)

Landuse_SQ_P %>% group_by(Kennung)%>%count(Kennung)



# Filtering out Brache (no agricultural usage of the area)
Landuse_SQ_P<- Landuse_SQ_P %>% filter(!Kennung=="B")


Landuse_SQ_P %>% ungroup%>% summarize(sum(sum_ha))

# Intermediate result. Without Brache there is area of 706,066 ha under agricultural production, considering 14 main crops
###################################################################################################################################################
### crop shares on the county level as output for Crop Rota
### filtering out Brache

Crop_share_BW_P <-zentroid_p_joined_filtered %>% filter(Kennung !="B") %>% group_by(NUTS_2) %>% mutate(sum_Kreis=sum(FLAECHE_HA)) %>%
  group_by(NUTS_2, Kennung, sum_Kreis) %>% summarise(sum=sum(FLAECHE_HA)) %>% mutate(share=sum/sum_Kreis) %>%
  select(-sum_Kreis, -sum) %>% spread(key=Kennung, value=share)


####################################################################################################################################################################################
## Regional energy consumption of animals per county in status quo
## Considering the energy consumption in the region that is needed for fodder production


head(zentroid_p_joined_filtered)
futterbau_bedarf <- zentroid_p_joined_filtered %>%select(NUTS_2, Schlag_ID, FLAECHE_HA, Kennung,Bodenguete)
head(futterbau_bedarf)

# %>%
#   mutate(Bodenguete = ifelse(NATBOD %in% c("1,0"), "low",
#                       ifelse(NATBOD %in% c("1,5"), "low",
#                       ifelse(NATBOD %in% c( "2,0"), "medium",
#                       ifelse(NATBOD %in% c("2,5"), "medium",
#                       ifelse(NATBOD %in% c( "3,0"), "high",    
#                       ifelse(NATBOD %in% c( "3,5"), "high",    
#                       ifelse(NATBOD %in% c( "4,0"), "high","medium")))))))) %>% select(NUTS_2, Schlag_ID, FLAECHE_HA, Kennung,Bodenguete) 



getwd()
setwd("C:\\Users\\User\\OneDrive - bwedu\\Dokumente\\Landwirtschaftliche Betriebslehre\\Projekt_P_Bawü\\GAMS_P\\Input_data\\Kalkulationsdaten")

# reading data in
######### !!!! Excel file ist gelinkt zu Marktfruechte und Futterbau Klakulationstabellen, aenderungen in diesen Tabellen veraendern die input daten
######### !!!! Betrifft vor allem die Kostenseite, zum Beispiel Aenderung des Duengerpreises und des Dieselpreises auf 2022 Niveau
#########      Sollte Ertragsdaten nicht betreffen
Ackerkulturen <- read_excel("extracted_calculation_data.xlsx", sheet="Ackerkulturen")
Futterbau <- read_excel("extracted_calculation_data.xlsx", sheet="Futterbau")
head(Futterbau)
head(Ackerkulturen)


## Contain: crop, management Intensity and yields

yields_soilqual_Ack <- Ackerkulturen %>% select(`crop abrre`, Intensitaet, `Ertrag dt/ha`)
yields_soilqual_Ack
yields_soilqual_Fut <- Futterbau %>% select(`crop abrre`, Intensitaet, `Ertrag (10 MJ NEL/ha)`)
yields_soilqual_Fut


### How many MJNEL are produced per area? How are plot ID and schalgId behaving towards each other?
yields_soilqual_Fut %>% rename(Kennung=`crop abrre`, Bodenguete="Intensitaet")


crop_vector <- c("Gr", "Ha", "Ka", "KG", "KM", "SG", "SM", "WG", "Win", "WR", "WW", "ZR", "Rog", "KL") %>% as_tibble()
crops<-crop_vector%>% rename(crop="value")
crops
kreis <- Landuse_plots_P %>% select(NUTS_2) %>% distinct(NUTS_2)

kreis
yields_soilqual_Fut
yields_soilqual_Ack


## Combining the crops and the intensity levels 
one <- yields_soilqual_Ack %>% select(`crop abrre`,Intensitaet)
two <- yields_soilqual_Fut %>% select(`crop abrre`, Intensitaet)

Energie_pro_ha <- rbind(one, two)
Energie_pro_ha


# added on the 13.03.23
new_df <- expand.grid(`crop abrre`=unique(Energie_pro_ha$`crop abrre`),
                      Intensitaet= unique(Energie_pro_ha$Intensitaet),
                      county=kreis$NUTS_2) %>% as_tibble()


# Putting the energy per ha for fodder crops into the dataset
Energie_proha<- left_join(new_df, yields_soilqual_Fut, by=c("crop abrre", "Intensitaet"))
Energie_proha <- Energie_proha %>% replace_na(list(`Ertrag (10 MJ NEL/ha)`=0))
Energie_proha %>% print(n=Inf)


### write out Energy supply per crop per ha and soil quality or intenstiy 
write_xlsx(x=Energie_proha, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/18.04.23/Energie_proha.xlsx", col_names = TRUE)



#################################################################################################################################################################

#Calculating MJNEL demand per county, this demand needs to be covered by threee crop types Gr, KL, SM
# Just doing some renaming


yields_soilqual_Fut <- yields_soilqual_Fut %>% select(`crop abrre`, Intensity="Intensitaet", `Ertrag (10 MJ NEL/ha)`)

                        
# Adding the per ha production of MJNEL information to futterbau_bedarf
futterbau_bedarf_joined<-futterbau_bedarf %>% left_join(yields_soilqual_Fut %>% rename(Kennung=`crop abrre`, Bodenguete="Intensity"), by=c("Kennung", "Bodenguete"))
head(futterbau_bedarf_joined)

futterbau_bedarf_joined %>% filter(!is.na(`Ertrag (10 MJ NEL/ha)`))


dim(futterbau_bedarf_joined)
head(futterbau_bedarf_joined)

factor(futterbau_bedarf$Bodenguete)
factor(yields_soilqual_Fut$Intensitaet)

# replacing NAs with zeroes
futterbau_bedarf_joined <-futterbau_bedarf_joined %>% replace_na(list(`Ertrag (10 MJ NEL/ha)`=0)) 
head(futterbau_bedarf_joined)

## Calculating the MJNEL production per field depending on the intensity/soil quality level
futterbau_bedarf_joined<- futterbau_bedarf_joined %>% mutate(Energiebedarf_Schlag=FLAECHE_HA*`Ertrag (10 MJ NEL/ha)`)
head(futterbau_bedarf_joined)
futterbau_bedarf_joined %>% filter(is.na(Energiebedarf_Schlag))

# filtering out areas without crop production Brache
futterbau_bedarf_joined   <-futterbau_bedarf_joined %>% filter(!Kennung=="B")


# summarizing the energy demand per county and soil quality
dim(futterbau_bedarf_joined)

futterbau_bedarf_kreis <-   futterbau_bedarf_joined %>% select(NUTS_2,Bodenguete, Energiebedarf_Schlag) %>% 
                            group_by(NUTS_2) %>% summarize(Energiebedarf_kreis_MJNEL=sum(Energiebedarf_Schlag))%>%
                            select(couties="NUTS_2",  Energiebedarf_kreis_MJNEL)

head(futterbau_bedarf_kreis)  

#### Writing out fodder demand per county  
write_xlsx(x=futterbau_bedarf_kreis, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/18.04.23/futterbau_bedarf_kreis.xlsx", col_names = TRUE)



#####################################################################################################################################################
### Writing out files 
### crop_share_BW_P - crop_share_bw_Zentroide.xlsx
### Landuse_SQ_P - Landuse_SQ_P
### Landuse_plots_P - Landuse_plots_P

write_xlsx(x=Crop_share_BW_P, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep//18.04.23/crop_share_bw_Zentroide.xlsx", col_names = TRUE)
write_xlsx(x=Landuse_SQ_P, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/18.04.23/Landuse_SQ_P.xlsx", col_names = TRUE)
write_xlsx(x=Landuse_plots_P, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/18.04.23/Landuse_plots_P.xlsx", col_names = TRUE)

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
kreis<-zentroid_p %>%filter(!NUTS_2==" ") %>% distinct(NUTS_2)
kommune<-zentroid_p %>% distinct(AGS_0_2)


#####################################################################################################################################################
### Writing out files 
### kreis - kreis_P
### kommune - kommune_p

write_xlsx(x=kreis, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/18.04.23/kreis_P.xlsx", col_names = FALSE)
write_xlsx(x=kommune, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/18.04.23/kommune_p.xlsx", col_names = FALSE)


#################################################################################################################################################################
#CropRota input generation
Crop_share_BW_P


Crop_share_BW_P_2<-Crop_share_BW_P %>% pivot_longer(cols = c("Gr", "Ha", "Ka", "KG", "KL", "Rog", "SG", "SM", "WG", "Win", "WR", "WW", "ZR", "KM")) 
Crop_share_BW_P_2 %>% filter(is.na(value))
Crop_share_BW_P_2$value[is.na(Crop_share_BW_P_2$value)] <-0

Crop_share_BW_P_2 <- Crop_share_BW_P_2%>% pivot_wider(names_from = NUTS_2, values_from ="value")
Crop_share_BW_P_2


write_xlsx(x=Crop_share_BW_P_2, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep//18.04.23/crop_share_bw_Zentroide_2.xlsx", col_names = TRUE)









##############################################################################################################################################################

#                                                     END - files creation from own QGIS work                                       ####

###########################################################################################################################################################
# READING IN CROP ROTATIONS, developed out of crop rota.gms stored under C: Gams und dem Rscript CROPROTGDX_2.R 

kommune %>% count()

## 01.02.23 reading in own crop rotation file
## 19.04.23 working with new data need to update croprota output
Rotations_tristan <-read.csv("C:\\Users\\User\\OneDrive - bwedu\\Dokumente\\Landwirtschaftliche Betriebslehre\\Projekt_P_Bawü\\P_BaWue\\Crop_Rota_Output\\Crop_rotations_Tristan.csv", sep=";", stringsAsFactors = T)
Rotations_tristan %>% distinct(NUTS_2) %>% count()
str(Rotations_tristan)

# How many unique Combos are there
Rotations_tristan %>% distinct(Kombo) %>% count()
Cr_bawue_distinct <- Rotations_tristan %>% distinct(Kombo, Gewicht, Glied1, Glied2, Glied3, Glied4, Glied5) %>% select(Kombo:Gewicht)
glimpse(Cr_bawue_distinct)

## There are 169 distinct crop rotations in BAWUE at the NUTS_2 level
count_values <- function(x) {
  table(x)[as.character(x)]
}

test <-Cr_bawue_distinct %>%select(Glied1:Glied5)

# converting all variables to as.character
value_counts <-  apply(test[, c("Glied1", "Glied2", "Glied3", "Glied4", "Glied5")], 1, count_values)
summary(value_counts)

value_counts<-value_counts %>% as_tibble()
value_counts

more_than_four<-value_counts %>% select_if(~any(. >=4))
more_than_four

Cr_to_filter<-colnames(more_than_four)
Cr_to_filter <-as.integer(sub("^V", "", Cr_to_filter)) %>% as_tibble()
Cr_to_filter <- Cr_to_filter %>% rename(CRid=value)
Cr_to_filter


# Result: 
# 37 crop rotations have three or more times the same crop
# 23 crop rotations have four or more times the same crop


################################################################################################################################
# ## hier wird nur angeschaut, nix gebaut!!!
# # kommune CR_id wird weiter unten generiert
# 
# 
# 
# # in welchen kreisen kommt die Cr_to_filter am häufigsten vor? code only works if kommune_CRid is unfiltered
# Rotations_tristan
# kommune_CRid
# Cr_to_filter
# 
# # wieviele der gefiltereten sind in den Kreisen verfügbar
# kommune_CRid %>% filter(CRid %in% Cr_to_filter$CRid) %>% group_by(LAU_ID) %>% summarize(n=n()) %>% filter(n>2)
# 
# # wieviele sind insgesamt vefügabr je kreis
# kommune_CRid %>% group_by(LAU_ID) %>% summarize(n=n()) %>% filter(n>2)





############################################################################################################################
# Excluding crop rotations with four or more times the same crop in the crop rotation

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

### Excluding crop rotations with more than 4 times the same crop in the rotation

Cr_bawue_distinct <-Cr_bawue_distinct %>%  rowid_to_column() %>% mutate(CRid = rowid) %>% select(-rowid) %>% select(CRid, Kombo:Gewicht)

# Here the filter is happening 
Cr_bawue_distinct<- Cr_bawue_distinct %>% filter(!CRid %in% Cr_to_filter$CRid)
glimpse(Cr_bawue_distinct)

Cr_bawue_distinct <- Cr_bawue_distinct %>% select(-CRid)
str(Cr_bawue_distinct)


#Result 24.04.23
# 39 crop rotations got filtered out if 4 are filterd out, leaving 132
 





## Creating a table containing the crop rotations
rotation_matrix <- Cr_bawue_distinct %>%  rowid_to_column() %>% mutate(CRid = rowid) %>% select(-rowid) %>% select(CRid, Kombo:Gewicht)
str(rotation_matrix)
crops <- levels(rotation_matrix$Glied1)

## Preparation of crop rotation table, taking position of crop in crop rotation into account
glied1<-  rotation_matrix %>% select(CRid, Glied1, Gewicht) %>% pivot_wider(names_from = Glied1, values_from = Gewicht)
glied2 <- rotation_matrix %>% select(CRid, Glied2, Gewicht) %>% pivot_wider(names_from = Glied2, values_from = Gewicht)
glied3 <- rotation_matrix %>% select(CRid, Glied3, Gewicht) %>% pivot_wider(names_from = Glied3, values_from = Gewicht)
glied4 <- rotation_matrix %>% select(CRid, Glied4, Gewicht) %>% pivot_wider(names_from = Glied4, values_from = Gewicht)
glied5 <- rotation_matrix %>% select(CRid, Glied5, Gewicht) %>% pivot_wider(names_from = Glied5, values_from = Gewicht)

pivotlonger_test <- rotation_matrix %>% pivot_longer(c(Glied1, Glied2, Glied3, Glied4, Glied5))


## Defines which one needs a mutate 0 in the operation, here ZR in glied5
## looking at the variables
levels(Cr_bawue_distinct$Glied1)
levels(Cr_bawue_distinct$Glied2)
levels(Cr_bawue_distinct$Glied3)
levels(Cr_bawue_distinct$Glied4)

# in der fünfgliederigen Fruchtfolge kommt kein ZR vor
levels(Cr_bawue_distinct$Glied5)
summary(Cr_bawue_distinct$Gewicht)



  Glied1<-pivotlonger_test %>%  filter(name=="Glied1") %>% 
          pivot_wider(names_from = value, values_from = Gewicht,  values_fill = 0) %>%
          mutate(Gr_Glied1=Gr, KG_Glied1=KG, KM_Glied1=KM, WG_Glied1=WG, 
                 SM_Glied1=SM, WW_Glied1=WW, WR_Glied1=WR, Win_Glied1=Win,
                 SG_Glied1=SG, Ha_Glied1=Ha, ZR_Glied1=ZR, Ka_Glied1=Ka,
                 Rog_Glied1=Rog, KL_Glied1=KL)
  
  
  Glied1 <-Glied1[,-c(3:17)]



  Glied2<- pivotlonger_test %>%  filter(name=="Glied2") %>% 
           pivot_wider(names_from = value, values_from = Gewicht, values_fill = 0) %>%
           mutate(Gr_Glied2=Gr, KG_Glied2=KG, KM_Glied2=KM, WG_Glied2=WG, 
                 SM_Glied2=SM, WW_Glied2=WW, WR_Glied2=WR, Win_Glied2=Win,
                 SG_Glied2=SG, Ha_Glied2=Ha, ZR_Glied2=ZR, Ka_Glied2=Ka,
                 Rog_Glied2=Rog, KL_Glied2=KL) 
  
  Glied2 <-Glied2[,-c(3:17)]
  
 

  Glied3<- pivotlonger_test %>%  filter(name=="Glied3") %>% 
           pivot_wider(names_from = value, values_from = Gewicht, values_fill = 0) %>% 
           mutate(Gr_Glied3=Gr, KG_Glied3=KG, KM_Glied3=KM, WG_Glied3=WG, 
                 SM_Glied3=SM, WW_Glied3=WW, WR_Glied3=WR, Win_Glied3=Win,
                 SG_Glied3=SG, Ha_Glied3=Ha, ZR_Glied3=ZR, Ka_Glied3=Ka,
                 Rog_Glied3=Rog, KL_Glied3=KL) 
  
  Glied3 <-Glied3[,-c(3:17)]
 

  Glied4<- pivotlonger_test %>%  filter(name=="Glied4") %>% 
           pivot_wider(names_from = value, values_from = Gewicht, values_fill = 0) %>% 
           mutate(Gr_Glied4=Gr, KG_Glied4=KG, KM_Glied4=KM, WG_Glied4=WG, 
                 SM_Glied4=SM, WW_Glied4=WW, WR_Glied4=WR, Win_Glied4=Win,
                 SG_Glied4=SG, Ha_Glied4=Ha, ZR_Glied4=ZR, Ka_Glied4=Ka,
                 Rog_Glied4=Rog, KL_Glied4=KL) 

  Glied4 <-Glied4[,-c(3:18)]
  
  Glied5<- pivotlonger_test %>%  filter(name=="Glied5") %>% 
           pivot_wider(names_from = value, values_from = Gewicht, values_fill = 0) %>% mutate(ZR=0)%>%
           mutate(Gr_Glied5=Gr, KG_Glied5=KG, KM_Glied5=KM, WG_Glied5=WG, 
                 SM_Glied5=SM, WW_Glied5=WW, WR_Glied5=WR, Win_Glied5=Win,
                 SG_Glied5=SG, Ha_Glied5=Ha, ZR_Glied5=ZR, Ka_Glied5=Ka,
                 Rog_Glied5=Rog, KL_Glied5=KL)
           


  Glied5 <-Glied5[,-c(3:18)]  
  
  
rotation_matrix_full <- Glied1 %>% left_join(Glied2, by="CRid") %>% left_join(Glied3, by="CRid") %>%
                        left_join(Glied4, by="CRid") %>% left_join(Glied5, by="CRid")

rotation_matrix_full <- rotation_matrix_full %>% select(-c(Kombo.y,Kombo.y.y,Kombo.y,Kombo.x, Kombo, Kombo.x.x))
                         

glimpse(rotation_matrix_full)
summary(rotation_matrix_full)
rm(glied1, Glied1, Glied2, Glied3, Glied4, Glied5)
rm(glied2, glied3, glied4 ,glied5)
#rm(Crop_share_BW_P)

rm(pivotlonger_test)




##################################################################################################################################################
##################################################################################################################################################

#### Creating set of crop position within a crop rotation (ex: GR_Glied1, Gr_Glied2,…, Gr_Glied5)
CR_Glieder <- colnames(rotation_matrix_full) %>% as_tibble()
CR_Glieder <- CR_Glieder[-1,]
CR_Glieder <- CR_Glieder %>% rename(Cr_Glieder=value)



## creating Connection between CR_Glieder and the crop set 
crops <- Cr_bawue_distinct %>% distinct(Glied1)
crop_CR_matrix <- rotation_matrix_full %>% slice(1:14)
crop_CR_matrix <- cbind(crops, crop_CR_matrix)
crop_CR_matrix <- crop_CR_matrix %>% select(-CRid)


## Connection with crops
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
str(crop_CR_matrix)

#checking the results and sets beween GAMS and here, no differences are allowed
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

# check: sets for CR_Glieder and crops are identical to GAMS

## Creating Connection between CR_id and NUTS_2 region. 
## Which crop rotation occurs in which region. More than four times the same crop in rotations excluded.
## In GAMS the model can only choose crop rotations that crop Rota generated per region

head(rotation_matrix)

kommune_CRid <- rotation_matrix %>% left_join(Rotations_tristan, by="Kombo")
kommune_CRid <- kommune_CRid %>% select(CRid, LAU_ID)
head(kommune_CRid)

kommune_CRid<-kommune_CRid %>% mutate(AGS_0_2=LAU_ID)
kommune_CRid<- kommune_CRid %>% select(-LAU_ID) %>% mutate(value=1)
head(kommune_CRid)

## Pivoting to get a nicer table

kommune_CRid <-kommune_CRid %>%pivot_wider(names_from = AGS_0_2, values_from = value, values_fill = 0)
kommune_CRid <- kommune_CRid %>% mutate(CR_id=CRid) %>% select(-CRid) 
kommune_CRid <-kommune_CRid[,c(45, 1:44)]
head(kommune_CRid)




##################################################################################################################################################
##################################################################################################################################################
# writing out files related to crop rotations and Crop Rota

# Crop_glieder_set
write_xlsx(x=CR_Glieder, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/18.04.23/CR_Glieder.xlsx", col_names = FALSE)

#crop_CR_matrix, connect crop rotations with crop
write_xlsx(x=crop_CR_matrix, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/18.04.23/crop_CR_matrix.xlsx", col_names = TRUE)


#### kommune_CR_ID, welche croprot kommt in welcher kommune vor?
write_xlsx(x=kommune_CRid, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/18.04.23/kommune_CRid.xlsx", col_names = TRUE)

## rotation_matrix_full
write_xlsx(x=rotation_matrix_full, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/18.04.23/rotation_matrix_full.xlsx", col_names = TRUE)


#######################################################################################################################################################
###################################################################################################################################################
#####




# crop rota files tristan
CR_Glieder                                      #|should be ready |contains rotation glied
crop_CR_matrix                                  #|should be ready |contains crop/rotation_glied
rotation_matrix_full
kommune_CRid

### Creating connection between CRid and the crop. Information is necessary for GAMS
### CR_id must be linked with crops, where is the CR_id coming from?
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
write_xlsx(x=CR_id_crop, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/18.04.23/CR_id_crop.xlsx", col_names = T)

###############################################################################################################################################################################




# Calculation data preparation
################################################################################################################################################################################


# Preparation of the calculation data for GAMS 
# Data can be found under GAMS_P/Input_data/Kalkulationsdaten
# Data source is the LEL
###########################################################################################################################################################################
############################################################################################################################################################################
# input <- choose.files()
#  input

getwd()
#setwd("C:\\Users\\Tristan Herrmann\\OneDrive - bwedu\\Dokumente\\Landwirtschaftliche Betriebslehre\\Projekt_P_Bawü\\GAMS_P\\Input_data\\Kalkulationsdaten")

# reading data in
######### !!!! Excel file ist gelinkt zu Marktfruechte und Futterbau Klakulationstabellen, aenderungen in diesen Tabellen veraendern die input daten
######### !!!! Betrifft vor allem die Kostenseite, zum Beispiel Aenderung des Duengerpreises und des Dieselpreises auf 2022 Niveau
#########      Sollte Ertragsdaten nicht betreffen

# reading in data
Ackerkulturen <- read_excel("extracted_calculation_data.xlsx", sheet="Ackerkulturen")
Futterbau <- read_excel("extracted_calculation_data.xlsx", sheet="Futterbau")
excel_sheets("extracted_calculation_data.xlsx")


glimpse(Ackerkulturen)
glimpse(Futterbau)

# yields_soilqual_Ack <- Ackerkulturen %>% select(`crop abrre`, Intensitaet, `Ertrag dt/ha`)
# yields_soilqual_Ack
# yields_soilqual_Fut <- Futterbau %>% select(`crop abrre`, Intensitaet, `Ertrag (10 MJ NEL/ha)`)
# yields_soilqual_Fut


price_crop_Ack <- Ackerkulturen %>% select(`crop abrre`, `Price €/dt`) %>% distinct(`crop abrre`, `Price €/dt`)
price_crop_Ack


price_crop_Fut   <- Futterbau %>% select(`crop abrre`, `Preis (€/10MJ NEL)`) 
price_crop_Fut<- unique(price_crop_Fut[,c("crop abrre", "Preis (€/10MJ NEL)")])
price_crop_Fut


## writing out parameters yield and prices
write_xlsx(x=yields_soilqual_Ack, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/18.04.23/yields_soilqual_Ack.xlsx", col_names = T)
write_xlsx(x=yields_soilqual_Fut, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/18.04.23/yields_soilqual_Fut.xlsx", col_names = T)
write_xlsx(x=price_crop_Ack, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/18.04.23/price_crop_Ack.xlsx", col_names = T)
write_xlsx(x=price_crop_Fut, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/18.04.23/price_crop_Fut.xlsx", col_names = T)


##############################################################################################################################################################################
###############################################################################################################################################################################
# Variable kosten !Anpassung der Dieselpreise im file
# Dieselpreise auf 2021 Niveau

variable_cost_ackerbau <- Ackerkulturen %>% select(`crop abrre`,Intensitaet,`V. Kosten (€/ha)`)
variable_cost_futterbau <- Futterbau %>% select(`crop abrre`,Intensitaet,`V. Kosten (€/ha)`)


## writing out variable cost parameters
write_xlsx(x=variable_cost_ackerbau, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/18.04.23/variable_cost_ackerbau.xlsx", col_names = T)
write_xlsx(x=variable_cost_futterbau, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/18.04.23/variable_cost_futterbau.xlsx", col_names = T)

######################################################################################################################################################
## P Verbrauch je crop in abhängigkeit von soil_qual und intensitaet generated out extracted calculation data. 
## Average demands per crop


P1 <- Ackerkulturen %>% select(`crop abrre`, Intensitaet,"N (kg/ha)","P (kg/ha)" ,"K (kg/ha)")
P2 <- Futterbau %>% select(`crop abrre`, Intensitaet, "N (kg/ha)","P (kg/ha)" ,"K (kg/ha)")

Duengebedarf_crop_Int <- rbind(P1, P2)
Duengebedarf_crop_Int

# new_df is needed to get the county information
new_df <- expand.grid(`crop abrre`=unique(Duengebedarf_crop_Int$`crop abrre`),
                      Intensitaet= unique(Duengebedarf_crop_Int$Intensitaet),
                      county=kreis$NUTS_2) %>% as_tibble()

Duengebedarf_crop_Int<- left_join(new_df, Duengebedarf_crop_Int, by=c("crop abrre", "Intensitaet"))


summary(Duengebedarf_crop_Int)


write_xlsx(x=Duengebedarf_crop_Int , path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/18.04.23/Duengebedarf_crop_Int.xlsx", col_names = T)

##################################################################################################################################################################

# Creating restrictions 
# Restriction for silage maize and potatoe
sm_county<- zentroid_p_joined_filtered %>% filter(Kennung=="SM")
head(sm_county)

sm_county<-sm_county %>% select(counties="NUTS_2", crop="Kennung", management="Bodenguete", FLAECHE_HA)

sm_county <- sm_county %>% select(counties, crop, FLAECHE_HA) %>% group_by(counties,crop) %>% summarize(sum_ha_sm=sum(FLAECHE_HA))
head(sm_county)

# Restriction for potatoes
Ka_county<- zentroid_p_joined_filtered %>% filter(Kennung=="Ka")
head(Ka_county)

Ka_county<-Ka_county %>% select(counties="NUTS_2", crop="Kennung", management="Bodenguete", FLAECHE_HA)


Ka_county <- Ka_county %>% select(counties, crop, FLAECHE_HA) %>% group_by(counties,crop) %>% summarize(sum_ha_sm=sum(FLAECHE_HA))
Ka_county

# Beschraenkung KArtoffel Silomais auf die Ganze region Bawü
Ka_county %>%ungroup() %>%summarize(bawue_ka_ha=sum(sum_ha_sm))  # in total 5395ha Kartoffel in Bawü

sm_county %>%ungroup() %>%summarize(bawue_sm_ha=sum(sum_ha_sm))  # 133,251 ha Silomais in Bawü

## Creating a global restriction on all crops, they can expand max 25% per NUTS_2
## waehlen einer globalen beschraenkung 1.25 * Flaeche je crop, je regio in diesem Fall der Kreis
crop_kreis_res<-zentroid_p_joined_filtered %>% select(counties="NUTS_2", crop="Kennung", management="Bodenguete", FLAECHE_HA)

crop_kreis_res <- crop_kreis_res %>% select(counties, crop, FLAECHE_HA) %>% group_by(counties,crop) %>% summarize(sum_ha_sm=sum(FLAECHE_HA))
crop_kreis_res <- crop_kreis_res %>% filter(!crop=="B")
head(crop_kreis_res)

## write out restriction datasets
write_xlsx(x=Ka_county, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/18.04.23/Ka_county.xlsx", col_names = T)
write_xlsx(x=sm_county, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/18.04.23/sm_county.xlsx", col_names = T)
write_xlsx(x=crop_kreis_res, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Output_GAMS_P_Prep/18.04.23/crop_kreis_res.xlsx", col_names = T)







#27.04.23 checked until here - integration of P,N, K is following


### Trennung von Phosphat angebot tierisch und chemisch per NUTS_2 Ebene


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





# where is the P_soil class at the moment
Landuse_SQ_P
Landuse_plots_P
summary(Landuse_plots_P$P.2013...2)

##########################################################################################################################################################################################



















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





