#################################################ä######################################
#   This script is based on the carbon farming script downloaded 21.12.22 from ModellAG                          #
#########################################################################################




## GA-Datensatz vorbereiten

Flaechennutzung_Nutzcode <- Flaechennutzung_Nutzcode %>% select(1:5) %>% filter(!is.na(NUTZCODE))

AckerschlaegeBW <-  AckerschlaegeBW %>% mutate(Schlag_ID = OID_) %>% select(Schlag_ID, FLAECHE_HA, NUTZCODE, NUTS_CODE, NUTS_NAME, NAME, SCHLUESSEL, NATBOD) %>%
  left_join(Flaechennutzung_Nutzcode, by="NUTZCODE") %>%
  filter(Flaechenart =="Ackerland") 



AckerschlaegeBW <- AckerschlaegeBW %>%

mutate(Bodenguete = ifelse(NATBOD %in% c("1.0"), "gering",
ifelse(NATBOD %in% c("1.5"), "gering",
ifelse(NATBOD %in% c( "2.0"), "mittel",
ifelse(NATBOD %in% c("2.5"), "mittel",
ifelse(NATBOD %in% c( "3.0"), "hoch",    
ifelse(NATBOD %in% c( "3.5"), "hoch",    
ifelse(NATBOD %in% c( "4.0"), "hoch",     
 "mittel")))))))) 

AckerschlaegeBW$ha_Kat = cut(AckerschlaegeBW$FLAECHE_HA, c(0, 2, 5, 10, 1000), 
                    labels=c("1" ,"2", "5", "10"))

# 739,122 ha Ackerland  
# 516,981 Schläge  

### Aggregation der Schläge zu Landnutzungseinheiten
Landuse_plots <- AckerschlaegeBW %>% group_by(NUTS_CODE, SCHLUESSEL, Bodenguete) %>% summarise(sum_ha = sum(FLAECHE_HA)) %>%
                 ungroup() %>% mutate(Plot_ID = row_number()) 

Landuse_plots <-  Landuse_plots[,c(5,1,3,2,4)]


### Landnutzung im Status Quo auf Kommunaler Ebene

Landuse_SQ <-  AckerschlaegeBW %>% group_by(SCHLUESSEL, NAME, Kennung) %>% summarise(sum_ha = sum(FLAECHE_HA))


### Kulturanteile auf Kreisebene für CropRota

Crop_Share_BW <-   AckerschlaegeBW %>%
filter(Kennung != "B") %>% group_by(NUTS_CODE) %>%
  mutate(sum_Kreis = sum(FLAECHE_HA)) %>% group_by(NUTS_CODE, Kennung, sum_Kreis) %>%
  summarise(sum=sum(FLAECHE_HA)) %>% mutate(Share = sum/sum_Kreis) %>% 
  select(-sum_Kreis, -sum) %>% spread(key=Kennung, value=Share)

write_xlsx(x=Crop_Share_BW, path = "C:/Users/410B-PC29/Documents/gamsdir/projdir/Brandenburg/Crop_Share_BW.xlsx", col_names = TRUE)


write_xlsx(x=Landuse_SQ, path = "D:/NBiomasseBW_2022/Landuse_SQ.xlsx", col_names = TRUE)
write_xlsx(x=Landuse_plots, path = "D:/NBiomasseBW_2022/Landuse_plots.xlsx", col_names = TRUE)



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







