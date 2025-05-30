# Pakotnes ----

library(readxl)
library(tidyverse)
library(sfarrow)
library(sf)
library(ggplot2)


# Faili ----

# Novērojumu dati
noverojumi <- read_excel("../Ievades_dati/Noverojumi/SuguNoverojumi_BITES.xlsx")

# Vides pārmaiņas
vides_parmainas <- st_read_parquet("../Ievades_dati/Ainava/VidesParmainas_visas.parquet")
# sugām raksturīgās dzīves telpas rādiusi vides pārmaiņu izvērtēšanai
radiusi <- read_excel("../Ievades_dati/Noverojumi/Bites_radiusi.xlsx")
# Režģis novērojumu telpiskajai filtrēšanai
rezgis100 <- st_read_parquet("../Ievades_dati/References/tikls100_sauzeme.parquet")





# Novērojumu sagatavošana ----
# Sākotnēji jāveic novērojumu pārskatīšana, lai konstatētu, kurām sugām ir pietiekams novērojumu
# skaits, lai tās iekļautu

# grafikam - apkopojums
noverojumu_skaits <- noverojumi %>%
  group_by(Kods) %>%
  summarise(pirms = n())

# pēc pirmā atlases soļa
atlase1 <- noverojumi %>%
  filter(Izmantojams == 1)  %>%
  group_by(Kods) %>%
  summarise(atlase1 = n())

noverojumu_skaits <- noverojumu_skaits %>%
left_join(atlase1,by="Kods")


# Noskaidroju, kuras sugas sasniedz 20 novērojumu slieksni
nov_sk_pietiek <- noverojumi %>%
  filter(Izmantojams == 1) %>%
  count(Kods) %>%
  filter(n >= 20) %>%
  pull(Kods)   

# Attiecīgi sugas ar nepietiekamu novērojumu skaitu jāatmet - sugas izfiltrēju no novērojumu faila
noverojumi <- noverojumi %>%
  filter(Izmantojams == 1) %>%
  filter(Kods %in% nov_sk_pietiek)

# Uztaisu katrai sugai rezultātu direktoriju un saglabāju katras sugas novērojumus atsevišķā csv failā
for (suga in nov_sk_pietiek) {
  dati_sugai <- noverojumi %>%
    filter(Kods == suga)
  suga_dir <- paste0("../Rezultati/", suga)
  dir.create(suga_dir, recursive = TRUE)
  write.csv(dati_sugai,
            file = paste0("../Rezultati/", suga, "/Nov_", suga, "_visi.csv"),
            row.names = FALSE)
} 

rm(nov_sk_pietiek)





# Savienošana ar režģi ----
# Lai varētu veikt telpisko filtrēšanu, katrs novērojums vispirms jāsavieno ar references režģi

noverojumi <- st_as_sf(noverojumi, coords = c("X", "Y"), crs = 3059)
noverojumi <- st_transform(noverojumi, crs = st_crs(rezgis100)) 

noverojumi_rezgis <- st_join(noverojumi,rezgis100) 

coords <- st_coordinates(noverojumi_rezgis)
coords_df <- as.data.frame(coords)
noverojumi <- cbind(noverojumi_rezgis, coords_df)

rm(noverojumi_rezgis, coords, coords_df, rezgis100)





# Vides pārmaiņas ----
# Katru novērojumu nepieciešams izvērtēt pēc vides pārmainu apjoma, atmetot tos, kuros vides
# pārmaiņas kopš novērojuma ir lielākas par 10% no dzīves telpas platības

dati2=noverojumi %>% 
  left_join(radiusi,by=c("Kods"="Kods"))

vides_parmainas=data.frame(vides_parmainas) %>% 
  dplyr::select(-geom)
datiem=dati2 %>% 
  dplyr::select(id,ID,Gads,Radiuss)
datiem=datiem %>% 
  left_join(vides_parmainas,by="id")


# radiuss 500m


datiem_r500=datiem %>% 
  rowwise() %>% 
  mutate(cell_TCL=ifelse(Gads==2017,
                         sum(c(cell_TCL17,cell_TCL18,cell_TCL19,cell_TCL20,cell_TCL21,cell_TCL22,cell_TCL23),
                             na.rm=TRUE),
                         NA)) %>% 
  mutate(cell_TCL=ifelse(Gads==2018,
                         sum(c(cell_TCL18,cell_TCL19,cell_TCL20,cell_TCL21,cell_TCL22,cell_TCL23),
                             na.rm=TRUE),
                         cell_TCL)) %>% 
  mutate(cell_TCL=ifelse(Gads==2019,
                         sum(c(cell_TCL19,cell_TCL20,cell_TCL21,cell_TCL22,cell_TCL23),
                             na.rm=TRUE),
                         cell_TCL)) %>% 
  mutate(cell_TCL=ifelse(Gads==2020,
                         sum(c(cell_TCL20,cell_TCL21,cell_TCL22,cell_TCL23),
                             na.rm=TRUE),
                         cell_TCL)) %>% 
  mutate(cell_TCL=ifelse(Gads==2021,
                         sum(c(cell_TCL21,cell_TCL22,cell_TCL23),
                             na.rm=TRUE),
                         cell_TCL)) %>% 
  mutate(cell_TCL=ifelse(Gads==2022,
                         sum(c(cell_TCL22,cell_TCL23),
                             na.rm=TRUE),
                         cell_TCL)) %>% 
  mutate(cell_TCL=ifelse(Gads==2023,
                         sum(c(cell_TCL23),
                             na.rm=TRUE),
                         cell_TCL)) %>% 
  mutate(cell_TCL=ifelse(Gads==2024,0,cell_TCL)) %>% 
  mutate(hr_TCL=ifelse(Gads==2017,
                       sum(c(r500_TCL17,r500_TCL18,r500_TCL19,r500_TCL20,r500_TCL21,r500_TCL22,r500_TCL23),
                           na.rm=TRUE),
                       NA)) %>% 
  mutate(hr_TCL=ifelse(Gads==2018,
                       sum(c(r500_TCL18,r500_TCL19,r500_TCL20,r500_TCL21,r500_TCL22,r500_TCL23),
                           na.rm=TRUE),
                       hr_TCL)) %>% 
  mutate(hr_TCL=ifelse(Gads==2019,
                       sum(c(r500_TCL19,r500_TCL20,r500_TCL21,r500_TCL22,r500_TCL23),
                           na.rm=TRUE),
                       hr_TCL)) %>% 
  mutate(hr_TCL=ifelse(Gads==2020,
                       sum(c(r500_TCL20,r500_TCL21,r500_TCL22,r500_TCL23),
                           na.rm=TRUE),
                       hr_TCL)) %>% 
  mutate(hr_TCL=ifelse(Gads==2021,
                       sum(c(r500_TCL21,r500_TCL22,r500_TCL23),
                           na.rm=TRUE),
                       hr_TCL)) %>% 
  mutate(hr_TCL=ifelse(Gads==2022,
                       sum(c(r500_TCL22,r500_TCL23),
                           na.rm=TRUE),
                       hr_TCL)) %>% 
  mutate(hr_TCL=ifelse(Gads==2023,
                       sum(c(r500_TCL23),
                           na.rm=TRUE),
                       hr_TCL)) %>% 
  mutate(hr_TCL=ifelse(Gads==2024,0,hr_TCL)) %>%
  mutate(cell_DWchange=ifelse(Gads==2017,
                              sum(c(cell_DWchange1718,cell_DWchange1819,cell_DWchange1920,cell_DWchange2021,
                                    cell_DWchange2122,cell_DWchange2223),
                                  na.rm=TRUE),
                              NA)) %>% 
  mutate(cell_DWchange=ifelse(Gads==2018,
                              sum(c(cell_DWchange1819,cell_DWchange1920,cell_DWchange2021,
                                    cell_DWchange2122,cell_DWchange2223),
                                  na.rm=TRUE),
                              cell_DWchange)) %>% 
  mutate(cell_DWchange=ifelse(Gads==2019,
                              sum(c(cell_DWchange1920,cell_DWchange2021,
                                    cell_DWchange2122,cell_DWchange2223),
                                  na.rm=TRUE),
                              cell_DWchange)) %>% 
  mutate(cell_DWchange=ifelse(Gads==2020,
                              sum(c(cell_DWchange2021,
                                    cell_DWchange2122,cell_DWchange2223),
                                  na.rm=TRUE),
                              cell_DWchange)) %>% 
  mutate(cell_DWchange=ifelse(Gads==2021,
                              sum(c(cell_DWchange2122,cell_DWchange2223),
                                  na.rm=TRUE),
                              cell_DWchange)) %>% 
  mutate(cell_DWchange=ifelse(Gads==2022,
                              sum(c(cell_DWchange2223),
                                  na.rm=TRUE),
                              ifelse(is.na(cell_DWchange),1,cell_DWchange))) %>%
  mutate(cell_DWchange=ifelse(Gads %in% c(2023,2024),0,cell_DWchange)) %>% 
  mutate(hr_DWchange=ifelse(Gads==2017,
                            sum(c(r500_DWchange1718,r500_DWchange1819,r500_DWchange1920,r500_DWchange2021,
                                  r500_DWchange2122,r500_DWchange2223),
                                na.rm=TRUE),
                            NA)) %>% 
  mutate(hr_DWchange=ifelse(Gads==2018,
                            sum(c(r500_DWchange1819,r500_DWchange1920,r500_DWchange2021,
                                  r500_DWchange2122,r500_DWchange2223),
                                na.rm=TRUE),
                            hr_DWchange)) %>% 
  mutate(hr_DWchange=ifelse(Gads==2019,
                            sum(c(r500_DWchange1920,r500_DWchange2021,
                                  r500_DWchange2122,r500_DWchange2223),
                                na.rm=TRUE),
                            hr_DWchange)) %>% 
  mutate(hr_DWchange=ifelse(Gads==2020,
                            sum(c(r500_DWchange2021,
                                  r500_DWchange2122,r500_DWchange2223),
                                na.rm=TRUE),
                            hr_DWchange)) %>% 
  mutate(hr_DWchange=ifelse(Gads==2021,
                            sum(c(r500_DWchange2122,r500_DWchange2223),
                                na.rm=TRUE),
                            hr_DWchange)) %>% 
  mutate(hr_DWchange=ifelse(Gads==2022,
                            sum(c(r500_DWchange2223),
                                na.rm=TRUE),
                            ifelse(is.na(hr_DWchange),1,hr_DWchange))) %>%
  mutate(hr_DWchange=ifelse(Gads %in% c(2023,2024),0,hr_DWchange)) %>% 
  mutate(atlase=ifelse(cell_TCL<0.1&cell_DWchange<0.1&hr_TCL<0.1&hr_DWchange<0.1,1,0))
table(datiem_r500$atlase,useNA="always")
table(datiem_r500$atlase,datiem_r500$Gads,useNA="always")

# datiem_r500=datiem_r500 %>% 
#   dplyr::select(ID,cell_TCL,hr_TCL,cell_DWchange,hr_DWchange,atlase)

atlase <- datiem_r500 %>%
  dplyr::select(ID,atlase)

noverojumi <- as.data.frame(noverojumi)
dati_atlase <- noverojumi %>%
  left_join(atlase, by = "ID")

# grafikam
atlase2 <- dati_atlase %>%
  filter(atlase == 1) %>%
  group_by(Kods) %>% 
  summarise(atlase2 = n())

noverojumu_skaits <- noverojumu_skaits %>%
  left_join(atlase2,by="Kods")


# Telpiskā retināšana ----
# saglabājot tikai pirmo novērojumu katrā 1km šūnā

dati_atlase2_1km <- dati_atlase %>%
  filter(atlase == 1) %>% 
  group_by(Kods,Suga) %>% 
  filter(!duplicated(ID1km)) %>% 
  ungroup()

# grafikam 
atlase3 <- dati_atlase2_1km %>%
  group_by(Kods) %>% 
  summarise(atlase3 = n())

noverojumu_skaits <- noverojumu_skaits %>%
  left_join(atlase3,by="Kods")


# Cik noverojumi atlikusi katrai sugai?
nov_sk_1km <- dati_atlase2_1km %>% 
  group_by(Kods) %>% 
  summarise(skaits=n()) %>% 
  ungroup()

# Jāatmet sugas, kuram nepietiek noverojumu 
nov_sk_pietiek <- dati_atlase2_1km %>%
  count(Kods) %>%
  filter(n >= 20) %>%
  pull(Kods)   

# Attiecīgi saglabaju sugu novērojumus ar pietiekamu skaitu atsevišķos failos
noverojumi_1km=dati_atlase2_1km %>% 
  dplyr::select(Kods,X,Y)

for (suga in nov_sk_pietiek) {
  dati_sugai <- noverojumi_1km %>%
    filter(Kods == suga)
  write.csv(dati_sugai,
            file = paste0("../Rezultati/", suga, "/Nov_", suga, "_tiriti1km.csv"),
            row.names = FALSE)
} 



# Vizualizācijas ----

############### Novērojumu atlases gaitas grafiks

write.xlsx(noverojumu_skaits,"../Rezultati/Noverojumu_atlase.xlsx")
noverojumu_skaits <- read_xlsx("../Rezultati/Noverojumu_atlase.xlsx")

colnames(noverojumu_skaits) <- c("Suga","Visi novērojumi / All observations","Uzticamības izvērtējums / Reliability evaluation",
                                 "Vides mainības izvērtējums / Environmental change evaluation", "Telpiskā retināšana / Spatial filtering")

noverojumu_long <- noverojumu_skaits %>%
  pivot_longer(
    cols = -Suga,
    names_to = "Kategorija",
    values_to = "Skaits"
  )

# manuāla kodu pārsaukšana
noverojumu_long$Suga <- recode(noverojumu_long$Suga,
                               "ANDHAT" = "Andrena hattorfiana",
                               "DASHIR" = "Dasypoda hirtipes")

noverojumu_long$Kategorija <- factor(noverojumu_long$Kategorija, levels = c(
  "Visi novērojumi / All observations",
  "Uzticamības izvērtējums / Reliability evaluation",
  "Vides mainības izvērtējums / Environmental change evaluation",
  "Telpiskā retināšana / Spatial filtering"
))

# Grafiks tikai divām pētījumā apskatītajām modeļsugām

modelsugas <- noverojumu_long %>%
  filter(Suga %in% c("Andrena hattorfiana","Dasypoda hirtipes"))

nov <- ggplot(modelsugas, aes(x = Kategorija, y = Skaits, fill = Kategorija)) +
  geom_bar(stat = "identity", width = 0.7) +
  facet_wrap(~Suga, scales = "fixed") +
  labs(
    y = "Novērojumu skaits / Number of observations",
    fill = "Atlases posms / Selection stage",
    title = ""
  ) +
  scale_fill_manual(values = c(
    "Visi novērojumi / All observations" = "#9fcae0",
    "Uzticamības izvērtējums / Reliability evaluation" = "#52a0c7",
    "Vides mainības izvērtējums / Environmental change evaluation" = "#2c6887",
    "Telpiskā retināšana / Spatial filtering" = "#132d3a"
  )) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    strip.text = element_text(size = 13, face = "italic"),
    legend.position = "right"
  )
plot(nov)

# mapīte attēliem
dir.create("../Rezultati/1_Atteli")

ggsave(nov,
       filename=paste0("../Rezultati/1_Atteli/VarImp_abas_sugas.png"),
       width=160,height=220,units="mm",dpi=120)



############## Novērojumu izvietojums telpā

teritorija <- raster::raster("../Ievades_dati/References/LV100m_10km.tif")
teritorija[!is.na(teritorija[])] <- 1
teritorija_df <- as.data.frame(teritorija, xy = TRUE)
colnames(teritorija_df) <- c("x", "y", "layer")
teritorija_df <- na.omit(teritorija_df)
teritorija_df$layer <- factor(teritorija_df$layer)

# Novērojumi
AH_pirms <- read.csv("../Rezultati/ANDHAT/Nov_ANDHAT_visi.csv", sep = ",")
AH_pec <- read.csv("../Rezultati/ANDHAT/Nov_ANDHAT_tiriti1km.csv", sep = ",")

DH_pirms <- read.csv("../Rezultati/DASHIR/Nov_DASHIR_visi.csv", sep = ",")
DH_pec <- read.csv("../Rezultati/DASHIR/Nov_DASHIR_tiriti1km.csv", sep = ",")

# Kartes
AH_karte_pirms <- ggplot(teritorija_df, aes(x = x, y = y,fill = layer)) +
  geom_raster() +
  coord_fixed(ratio = 1) +
  scale_fill_manual(values = c("1" = "#132d3a"), na.value = NA) +
  ggthemes::theme_map()  +
  theme(plot.title = element_text(hjust = 0.5),
    legend.position = "none",
    plot.background = element_rect(fill = "white", color = "white")
  ) +
  geom_point(
    data = AH_pirms,
    aes(x = X, y = Y),
    col = "white",  
    alpha = 0.3,
    pch = 20,
    size = 2,
    inherit.aes = FALSE
  ) + labs (title = "")

plot(AH_karte_pirms)

AH_karte_pec <- ggplot(teritorija_df, aes(x = x, y = y,fill = layer)) +
  geom_raster() +
  coord_fixed(ratio = 1) +
  scale_fill_manual(values = c("1" = "#132d3a"), na.value = NA) +
  ggthemes::theme_map()  +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none",
        plot.background = element_rect(fill = "white", color = "white"),
        title =  element_text(face = "italic")
  ) +
  geom_point(
    data = AH_pec,
    aes(x = X, y = Y),
    col = "white",  
    alpha = 0.3,
    pch = 20,
    size = 2,
    inherit.aes = FALSE
  ) + labs (title = "")

plot(AH_karte_pec)

DH_karte_pirms <- ggplot(teritorija_df, aes(x = x, y = y,fill = layer)) +
  geom_raster() +
  coord_fixed(ratio = 1) +
  scale_fill_manual(values = c("1" = "#132d3a"), na.value = NA) +
  ggthemes::theme_map()  +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none",
        plot.background = element_rect(fill = "white", color = "white")
  ) +
  geom_point(
    data = DH_pirms,
    aes(x = X, y = Y),
    col = "white",  
    alpha = 0.3,
    pch = 20,
    size = 2,
    inherit.aes = FALSE
  ) + labs (title = "")

plot(DH_karte_pirms)

DH_karte_pec <- ggplot(teritorija_df, aes(x = x, y = y,fill = layer)) +
  geom_raster() +
  coord_fixed(ratio = 1) +
  scale_fill_manual(values = c("1" = "#132d3a"), na.value = NA) +
  ggthemes::theme_map()  +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none",
        plot.background = element_rect(fill = "white", color = "white")
  ) +
  geom_point(
    data = DH_pec,
    aes(x = X, y = Y),
    col = "white",  
    alpha = 0.3,
    pch = 20,
    size = 2,
    inherit.aes = FALSE
  ) + labs (title = "")

plot(DH_karte_pec)

kartes_comb <- (AH_karte_pirms + AH_karte_pec + DH_karte_pirms + DH_karte_pec) +
  plot_layout(ncol = 2)
kartes_comb

ggsave(kartes_comb,
       filename=paste0("../Rezultati/1_Atteli/Noverojumi_izvietojums.png"),
       width=150,height=140,units="mm",dpi=120)
