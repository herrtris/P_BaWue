#######################################################################################
#   This script is based on the carbon farming script downloaded 21.12.22 from ModellAG                          #
#########################################################################################

input <- choose.files()
input

test <- read.csv(input, sep=",")
str(test)

#### Testing the same workstream as christian did in prepration for CropRota
library(dplyr)
library(tidyr)
library(readxl)
#install.packages("rJava")
library(rJava)
#install.packages("writexl")
library(writexl)

Flaechennutzung_Nutzcode <-read.csv("C:\\Users\\User\\OneDrive - bwedu\\Dokumente\\Landwirtschaftliche Betriebslehre\\Projekt_P_Bawü\\GAMS\\Modell-AG\\Modell-AG\\Flaechennutzung_Nutzcode.csv", sep=";") 

### File has to come from GIS, GA DATen und DAten der Bodenkarte 50
AckerschlaegeBW <- read.csv("C:\\Users\\User\\OneDrive - bwedu\\Dokumente\\Landwirtschaftliche Betriebslehre\\Projekt_P_Bawü\\GAMS\\Modell-AG\\Modell-AG\\AckerschlaegeBW.csv", sep=";")

glimpse(AckerschlaegeBW)

glimpse(test)

#create an ID for test
test$OID_<- 1:nrow(test)


## GA-Datensatz vorbereiten

Flaechennutzung_Nutzcode <- Flaechennutzung_Nutzcode %>% select(1:5) %>% filter(!is.na(NUTZCODE))

AckerschlaegeBW <-  AckerschlaegeBW %>% mutate(Schlag_ID = OID_) %>% select(Schlag_ID, FLAECHE_HA, NUTZCODE, NUTS_CODE, NUTS_NAME, NAME, SCHLUESSEL, NATBOD) %>%
  left_join(Flaechennutzung_Nutzcode, by="NUTZCODE") %>%
  filter(Flaechenart =="Ackerland") 

glimpse(AckerschlaegeBW)

test2 <- test %>% mutate(Schlag_ID=OID_) %>% select(Schlag_ID, FLAECHE_HA, NUTZCODE, NUTS_2, Gemeindenu, NATBOD, NUTZUNG, P.2013...2, pH.2013.., K.2013...2)
test2 <- test2 %>% left_join(Flaechennutzung_Nutzcode, by="NUTZCODE") %>% filter(Flaechenart=="Ackerland")
test2 %>% head(10)

AckerschlaegeBW <- AckerschlaegeBW %>%

mutate(Bodenguete = ifelse(NATBOD %in% c("1.0"), "gering",
ifelse(NATBOD %in% c("1.5"), "gering",
ifelse(NATBOD %in% c( "2.0"), "mittel",
ifelse(NATBOD %in% c("2.5"), "mittel",
ifelse(NATBOD %in% c( "3.0"), "hoch",    
ifelse(NATBOD %in% c( "3.5"), "hoch",    
ifelse(NATBOD %in% c( "4.0"), "hoch",     
 "mittel")))))))) 


test2 <- test2 %>%
  
  mutate(Bodenguete = ifelse(NATBOD %in% c("1.0"), "gering",
    ifelse(NATBOD %in% c("1.5"), "gering",
    ifelse(NATBOD %in% c( "2.0"), "mittel",
    ifelse(NATBOD %in% c("2.5"), "mittel",
    ifelse(NATBOD %in% c( "3.0"), "hoch",    
    ifelse(NATBOD %in% c( "3.5"), "hoch",    
    ifelse(NATBOD %in% c( "4.0"), "hoch",     
                "mittel")))))))) 

# not necessary for test2
AckerschlaegeBW <- AckerschlaegeBW %>% mutate(FLAECHE_HA=as.numeric(gsub(",", ".", gsub("\\.", "", FLAECHE_HA)))) 

# Unterteilung der Ssumchlaggroesen 
AckerschlaegeBW$ha_Kat = cut(AckerschlaegeBW$FLAECHE_HA, c(0, 2, 5, 10, 1000), 
                    labels=c("1" ,"2", "5", "10"))
head(AckerschlaegeBW)
AckerschlaegeBW %>% summarise(sum_ha=sum(FLAECHE_HA)) #abweichung kommt daher, dass anderer join in ARcGis zuvr geählt wrde

head(test2)

test2 %>% summarise(sum_ha=sum(FLAECHE_HA))
abweichung=739122-739039.9
abweichung

test2 %>% select(Schlag_ID) %>% count()

# In test 2 gibt es 516,905 Schlaege und 739,039.9 ha GEsamtflaeche, circa eine diskrepanz von 80ha zu christians join in arcgis
# okay 

# 739,122 ha Ackerland  
# 516,981 Schläge  

### Aggregation der Schläge zu Landnutzungseinheiten
Landuse_plots <- AckerschlaegeBW %>% group_by(NUTS_CODE, SCHLUESSEL, Bodenguete) %>% summarise(sum_ha = sum(FLAECHE_HA)) %>%
                 ungroup() %>% mutate(Plot_ID = row_number()) 

Landuse_plots <-  Landuse_plots[,c(5,1,3,2,4)]
Landuse_plots %>% filter(NUTS_CODE=="DE111")
test3 %>% filter(NUTS_2=="DE111")

test3 <- test2 %>% group_by(NUTS_2, Gemeindenu, Bodenguete) %>% summarise(sum_ha=sum(FLAECHE_HA))%>% ungroup() %>% mutate(PLOT_ID=row_number()) 
test3
landuse_plots_test3<-test3

# große abweichung zwischen land_use_plots_test3 und landuse_plots
landuse_plots_test3 %>% group_by(NUTS_2) %>% count() %>% print(n=44)

Landuse_plots %>% group_by(NUTS_CODE) %>% count() %>% print(n=57)

glimpse(test)

### Landnutzung im Status Quo auf Kommunaler Ebene

Landuse_SQ <-  AckerschlaegeBW %>% group_by(SCHLUESSEL, NAME, Kennung) %>% summarise(sum_ha = sum(FLAECHE_HA))
Landuse_SQ_test <- test2 %>% group_by(Gemeindenu, Kennung) %>% summarise(sum_ha=sum(FLAECHE_HA))

### Kulturanteile auf Kreisebene für CropRota

Crop_Share_BW <-   AckerschlaegeBW %>%
filter(Kennung != "B") %>% group_by(NUTS_CODE) %>%
  mutate(sum_Kreis = sum(FLAECHE_HA)) %>% group_by(NUTS_CODE, Kennung, sum_Kreis) %>%
  summarise(sum=sum(FLAECHE_HA)) %>% mutate(Share = sum/sum_Kreis) %>% 
  select(-sum_Kreis, -sum) %>% spread(key=Kennung, value=Share)

Crop_share_BW_test <- test2 %>% filter(Kennung !="B") %>% group_by(NUTS_2) %>% mutate(sum_Kreis=sum(FLAECHE_HA)) %>%
                            group_by(NUTS_2, Kennung, sum_Kreis) %>% summarise(sum=sum(FLAECHE_HA)) %>% mutate(share=sum/sum_Kreis) %>%
                            select(-sum_Kreis, -sum) %>% spread(key=Kennung, value=share)



write_xlsx(x=Crop_Share_BW, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/crop_share_bw.xlsx", col_names = TRUE)
write_xlsx(x=Crop_share_BW_test, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/crop_share_bw_test2.xlsx", col_names = TRUE)


write_xlsx(x=Landuse_SQ, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Landuse_SQ.xlsx", col_names = TRUE)
write_xlsx(x=Landuse_plots, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Landuse_plots.xlsx", col_names = TRUE)

write_xlsx(x=landuse_sq_test3, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Landuse_SQ_test3.xlsx", col_names = TRUE)
write_xlsx(x=landuse_plots_test3, path = "C:/Users/User/OneDrive - bwedu/Dokumente/Landwirtschaftliche Betriebslehre/Projekt_P_Bawü/P_BaWue/Landuse_plots_test3.xlsx", col_names = TRUE)



## here probably pushed into croprota


library(readxl)

CR_3 <-     read_excel("gamsdir/projdir/Brandenburg/Output/ResultsCF2.xlsx", 
                       sheet = "Crop_3", col_names = FALSE)


names(CR_3)[1] <- "NUTS_CODE"
names(CR_3)[2] <- "crop"
names(CR_3)[3] <- "crop2"
names(CR_3)[4] <- "crop3"
names(CR_3)[5] <- "share"


### clean crop rotations, i. e. remove monocultures



CR_3 <- CR_3 %>% select(-share) %>% mutate(CR_ID = row_number()) %>% 
  group_by(CR_ID) %>%
  gather(key="Crop", value = "NUTZG", 2:4) %>%
  mutate(Share=0.3333) %>% select(-Crop) 

CR_3 <- CR_3 %>% mutate(CR_ID = as.character(CR_ID))

# duplicate crop rotations with 10% flower strips

CR_3a <-   CR_3 %>% mutate(Komp="AF") %>%
  unite("CR_ID", Komp, CR_ID , sep="_", remove = T)  %>%
  mutate(Share = 0.3)

CR_3b <- CR_3 %>% mutate(Komp="AF") %>%
  unite("CR_ID", Komp, CR_ID , sep="_", remove = T)  %>%
  select(-NUTZG, -Share) %>% mutate(NUTZG="AF") %>%
  mutate(Share = 0.1)

CR_3 <- bind_rows(CR_3, CR_3a, CR_3b)


# vierfeldrige Fruchtfolgen auswerten

CR_4 <- read_excel("gamsdir/projdir/Brandenburg/Output/ResultsCF2.xlsx", 
                  sheet = "Crop_4", col_names = FALSE)

names(CR_4)[1] <- "NUTS_CODE"
names(CR_4)[2] <- "crop"
names(CR_4)[3] <- "crop2"
names(CR_4)[4] <- "crop3"
names(CR_4)[5] <- "crop4"
names(CR_4)[6] <- "share"


CR4 <- CR_4 %>% select(-share)  %>% mutate(CR_ID = row_number()) %>% 
  group_by(CR_ID) %>%
  gather(key="Crop", value = "NUTZG", 2:5) %>%
  mutate(Share=0.25) %>% select(-Crop)  


CR4_rem <- CR_4 %>% select(-share)  %>% mutate(CR_ID = row_number()) %>% 
  group_by(CR_ID) %>%
  gather(key="Crop", value = "NUTZG", 2:5) %>%
  mutate(Share=0.25) %>% select(-Crop)  %>% 
  group_by(CR_ID, NUTZG) %>% mutate(sum = sum(Share)) %>% 
  filter(! NUTZG %in% c("Gr", "KG")) %>% filter(sum>0.5) %>%
  ungroup() %>%select(CR_ID) %>% unique() %>% mutate(remove = "YES")


CR4 <-  CR4 %>% left_join(CR4_rem, by="CR_ID") %>% filter(is.na(remove)) %>% select(-remove)


# duplicate crop rotations with 10% flower strips

CR_4a <-   CR4 %>% mutate(Komp="AF") %>%
  unite("CR_ID", Komp, CR_ID , sep="_", remove = T)  %>%
  mutate(Share = 0.225)

CR_4b <- CR4 %>% mutate(Komp="AF") %>%
  unite("CR_ID", Komp, CR_ID , sep="_", remove = T)  %>%
  select(-NUTZG, -Share) %>% mutate(NUTZG="AF") %>%
  mutate(Share = 0.1) %>% unique()


CR4 <- CR4 %>% mutate(CR_ID = as.character(CR_ID))

CR4 <- bind_rows(CR4, CR_4a, CR_4b)

## Grünland-Fruchtfolge
CR_GR <-  CR4 %>% select(NUTS_CODE) %>% unique() %>% mutate(CR_ID="GR_1") %>%
  mutate(NUTZG="Gr") %>%
  mutate(Share = 1) 

Crop_rotations_BW <- bind_rows(CR_3, CR4, CR_GR) %>% mutate(n=1) %>% unique()

write_xlsx(x=Crop_rotations_BW, path = "D:/NBiomasseBW_2022/Crop_rotations_BW.xlsx", col_names = TRUE)







