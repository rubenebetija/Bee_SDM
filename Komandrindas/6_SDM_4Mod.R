# pakotnes ----

library(tidyverse)
library(sf)
library(arrow)
library(sfarrow)
library(stringr)
library(terra)
library(readxl)
library(maps)
library(rasterVis)
library(plotROC)
library(ecospat)
library(maxnet)
library(SDMtune)
library(ENMeval)
library(zeallot)
library(openxlsx)
library(patchwork)


# apraksts ----

# Šis skripts nodrošina metožu apakšnodaļā 2.4.1. aprakstītās pieejas komandrindas
# četru modeļu izveidei ar dažādiem paraugošanas piepūles kontroles līmeņiem:
#  •	Modelis bez paraugošanas piepūles kontroles (Mod1);
#  •	Modelis ar novērojumu telpisko filtrēšanu (Mod2);
#  •	Modelis ar novērojumu telpisko filtrēšanu un bitēm kopīgo paraugošanas piepūles noviržu slāni (Mod3);
#  •	Modelis ar novērojumu telpisko filtrēšanu un sugām specifisko paraugošanas piepūles noviržu slāni (Mod4);
# To rezultāti ANDHAT un DASHIR aprakstīti apakšnodaļā 3.3.

# Skripts veidots vienai sugai - modelējamās sugas kods jānorāda zemāk:
suga <- ""



# Faili ----

# bias slāņi
bisu_bias <- rast("../Ievades_dati/Bias_layers/Bias_kopa_max.tif")
suga_bias <- rast("../Ievades_dati/Bias_layers/Bias_",suga,"_max.tif")

# analīzes telpa
telpa <- rast("../Ievades_dati/References/nulles_LV100m_10km.tif")
telpa[telpa == 0] <- 1 # :)





# Apmācību un testa datu dalīšana ----

# direktorija treniņa un testa datu saglabāšanai
dir.create("../Rezultati/",suga,"/Dati")


########## visi novērojumi (bez fitrēšanas)

noverojumi1 <- read.csv(
  paste0("../Rezultati/",suga,"/Nov_",suga,"_visi.csv"),
  sep = ",")

# Piešķiru identifikatoru
noverojumi1 <- noverojumi1 %>%
  mutate(ID = row_number())

# testa klātbūtnes dati, 25% no visiem novērojumiem
set.seed(1);testam1 <- noverojumi1 %>% 
  sample_frac(0.25)

# apmācību dati - visi pārējie, kas nav jau iekļauti testa kopā
apmacibai1 <- noverojumi1 %>% 
  filter(!(ID %in% testam1$ID)) %>% 
  dplyr::select(Kods, X, Y)

testam1 <- testam1 %>% 
  dplyr::select(Kods, X, Y)

# Saglabāju 
write_parquet(testam1,paste0("../Rezultati/",suga,"/Dati/PresenceTest_",suga,".parquet"))
write_parquet(apmacibai1, paste0 ("../Rezultati/",suga,"/Dati/PresenceTrain_",suga,".parquet"))

rm(noverojumi1)

#testam1 <- read_parquet("../Rezultati/",suga,"/Dati/PresenceTest_",suga,".parquet")
#apmacibai1 <- read_parquet("../Rezultati/",suga,"/Dati/PresenceTrain_",suga,".parquet")


########## filtrētie novērojumi (1 km tīklā)

noverojumi2 <- read.csv(
  paste0("../Rezultati/",suga,"/Nov_",suga,"_tiriti1km.csv"),
  sep = ",")

# Piešķiru identifikatoru
noverojumi2 <- noverojumi2 %>%
  mutate(ID = row_number())

# testa klātbūtnes dati, 25% no visiem novērojumiem
set.seed(1);testam2 <- noverojumi2 %>% 
  sample_frac(0.25)

# apmācību dati - visi pārējie, kas nav jau iekļauti testa kopā
apmacibai2 <- noverojumi2 %>% 
  filter(!(ID %in% testam2$ID)) %>% 
  dplyr::select(Kods, X, Y)

testam2 <- testam2 %>% 
  dplyr::select(Kods, X, Y)

# Saglabāju 
write_parquet(testam2,paste0("../Rezultati/",suga,"/Dati/PresenceTest_filtered_",suga,".parquet"))
write_parquet(apmacibai2,paste0("../Rezultati/",suga,"/Dati/PresenceTrain_filtered_",suga,".parquet"))

rm(noverojumi2)

#testam2 <- read_parquet("../Rezultati/",suga,"/Dati/PresenceTest_filtered_",suga,".parquet")
#apmacibai2 <- read_parquet("../Rezultati/",suga,"/Dati/PresenceTrain_filtered_",suga,".parquet")



# Pseidoiztrūkuma (fona) datu sagatavošana ----

############### Bez bias slāņa (Mod1 un Mod2)

vides_fons1 <- terra::spatSample(telpa, size = 27500, na.rm = TRUE, 
                                 values = TRUE, xy = TRUE,method="weights") |> as.data.frame()

# Pievienoju identifikatoru
vides_fons1$rinda <- rownames(vides_fons1)

# 7500 novērojumi testam, pārējie apmācībai
set.seed(1);fons_testam1 <- vides_fons1 %>% 
  sample_n(7500)

fons_apmacibai1 <- vides_fons1 %>% 
  filter(!(rinda %in% fons_testam1$rinda)) %>% 
  dplyr::select(-rinda)

fons_testam1 <- fons_testam1 %>% 
  dplyr::select(-rinda)

# Saglabāju
write_parquet(fons_testam1, paste0("../Rezultati/",suga,"/Dati/BackgroundTest_",suga,".parquet"))
write_parquet(fons_apmacibai1,paste0("../Rezultati/",suga,"/Dati/BackgroundTrain_",suga,".parquet"))

#fons_testam1 <- read_parquet("../Rezultati/",suga,"/Dati/BackgroundTest_",suga,".parquet")
#fons_apmacibai1 <- read_parquet("../Rezultati/",suga,"/Dati/BackgroundTrain_",suga,".parquet")
rm(vides_fons1)


############### Ar bias slāni visu bišu novērojumiem (Mod3)

vides_fons2 <- terra::spatSample(bisu_bias, size = 27500, na.rm = TRUE, 
                                 values = TRUE, xy = TRUE,method="weights") |> as.data.frame()

# Pievienoju identifikatoru
vides_fons2$rinda <- rownames(vides_fons2)

# 7500 novērojumi testam, pārējie apmācībai
set.seed(1);fons_testam2 <- vides_fons2 %>% 
  sample_n(7500)

fons_apmacibai2 <- vides_fons2 %>% 
  filter(!(rinda %in% fons_testam2$rinda)) %>% 
  dplyr::select(-rinda)

fons_testam2 <- fons_testam2 %>% 
  dplyr::select(-rinda)

# Saglabāju
write_parquet(fons_testam2, paste0("../Rezultati/",suga,"/Dati/BackgroundTest_beebias_",suga,".parquet"))
write_parquet(fons_apmacibai2, paste0("../Rezultati/",suga,"/Dati/BackgroundTrain_beebias_",suga,".parquet"))

#fons_testam2 <- read_parquet("../Rezultati/",suga,"/Dati/BackgroundTest_beebias_",suga,".parquet")
#fons_apmacibai2 <- read_parquet("../Rezultati/",suga,"/Dati/BackgroundTrain_beebias_",suga,".parquet")
rm(vides_fons2)


############### Ar sugai specifisko bias slāni (Mod4)

vides_fons3 <- terra::spatSample(suga_bias, size = 27500, na.rm = TRUE, 
                                 values = TRUE, xy = TRUE,method="weights") |> as.data.frame()

# Pievienoju identifikatoru
vides_fons3$rinda <- rownames(vides_fons3)

# 7500 novērojumi testam, pārējie apmācībai
set.seed(1);fons_testam3 <- vides_fons3 %>% 
  sample_n(7500)

fons_apmacibai3 <- vides_fons3 %>% 
  filter(!(rinda %in% fons_testam3$rinda)) %>% 
  dplyr::select(-rinda)

fons_testam3 <- fons_testam3 %>% 
  dplyr::select(-rinda)

# Saglabāju
write_parquet(fons_testam3, paste0("../Rezultati/",suga,"/Dati/BackgroundTest_spbias_",suga,".parquet"))
write_parquet(fons_apmacibai3, paste0 ("../Rezultati/",suga,"/Dati/BackgroundTrain_spbias_",suga,".parquet"))

#fons_testam3 <- read_parquet("../Rezultati/",suga,"/Dati/BackgroundTest_spbias_",suga,".parquet")
#fons_apmacibai3 <- read_parquet("../Rezultati/",suga,"/Dati/BackgroundTrain_spbias_",suga,".parquet")
rm(vides_fons3)





# EGV ----
# ielasa un noformē nepieciešamos 

egv_faili=data.frame(egv_fails=list.files(path="../Ievades_dati/EGV/EGV_faili/",
                                          pattern=".tif$"),
                     egv_cels=list.files(path="../Ievades_dati/EGV/EGV_faili/",
                                         pattern=".tif$",full.names = TRUE))

egv_izvelei=read_excel(paste0("../Rezultati/",suga,"/EGV_izvele/EGV_",suga,".xlsx"))

egv_izvelei2=egv_izvelei %>% 
  filter(!is.na(sakumVIF))

egv_faili=egv_faili %>% 
  right_join(egv_izvelei2,by=c("egv_fails"="scale_NAME"))

egvs=terra::rast(egv_faili$egv_cels)
egv_izvelei2$id <- paste0("EGV_", 1:nrow(egv_izvelei2))
names(egvs)=egv_izvelei2$id
names(egvs) 

rm(egv_faili,egv_izvelei,egv_izvelei2)





# Modelēšana ----

# Rezultātu saglabāšanai
dir.create(paste0("../Rezultati/",suga,"/Modeli"))



################ MODELIS 1 (Mod1) - VISI DATI (1) BEZ BIAS SLĀŅA (1)

# apmācību dati
trenin_dati1 <- prepareSWD(species = suga,
                           p = apmacibai1[,2:3],
                           a = fons_apmacibai1[,1:2],
                           env = egvs)
trenin_dati1=addSamplesToBg(trenin_dati1)

block_folds1 <- get.block(occ = trenin_dati1@coords[trenin_dati1@pa == 1, ], 
                          bg = trenin_dati1@coords[trenin_dati1@pa == 0, ])

# testa dati
testa_dati1 <- prepareSWD(species=suga,
                       p = testam1[,2:3],
                       a = fons_testam1[1:2],
                       env = egvs)

# modelis ar MaxEnt noklusējuma iestatījumiem
mod1 <- train(method = "Maxnet", 
              data = trenin_dati1,
              test=testa_dati1,
              fc="lqph",
              reg = 1,
              folds = block_folds1)

# saglabāju
write_rds(mod1,paste0("../Rezultati/",suga,"/Modeli/Modelis1_visidati_",suga,".RDS"))
#mod1 <- readRDS(paste0("../Rezultati/",suga,"/Modeli/Modelis1_visidati_",suga,".RDS"))

# kombinetais
mod1_comb <- combineCV(mod1)
write_rds(mod1_comb,paste0("../Rezultati/",suga,"/Modeli/Modelis1_visidati_Comb_",suga,".RDS"))
#mod1_comb <- readRDS(paste0("../Rezultati/",suga,"/Modeli/Modelis1_visidati_Comb_",suga,".RDS"))


# Prognoze
# dalot telpu daļās
tks_rezgis <- sfarrow::st_read_parquet("../Ievades_dati/References/tks93_50km.parquet")
dir.create(paste0("../Rezultati/",suga,"/Modelis1_proj"))
lapas <- levels(factor(tks_rezgis$NUMURS))
for(i in seq_along(lapas)){
  print(i)
  sakums=Sys.time()
  lapai=lapas[i]
  lapa=tks_rezgis %>%
    filter(NUMURS == lapai)
  egv_mazs=terra::crop(egvs,lapa)
  #names(egv_mazs)
  projekcijai=predict(mod1_comb,
                      data = egv_mazs,
                      type = "cloglog",
                      file = paste0("../Rezultati/",suga,"/Modelis1_proj/proj_",lapai,".tif"),
                      overwrite=TRUE)
  #plot(projekcijai)
  beigas=Sys.time()
  ilgums=beigas-sakums
  print(ilgums)
}
slani=list.files("../Rezultati/",suga,"/Modelis1_proj/",full.names=TRUE)
virt_slani=terra::vrt(slani)
writeRaster(virt_slani,paste0("../Rezultati/",suga,"/Modeli/Modelis1_HSmap_",suga,".tif", overwrite = TRUE))
projekcija=rast(paste0("../Rezultati/",suga,"/Modeli/Modelis1_HSmap_",suga,".tif"))
plot(projekcija)


# thresholds
ths <- SDMtune::thresholds(mod1_comb, 
                           type = "cloglog",
                           test=testa_dati1)
ths$suga=suga
write.xlsx(ths,paste0("../Rezultati/",suga,"/Modeli/Modelis1_visidati_",suga,".xlsx"))


# TSS, AUC apmācībai
indep_tss1 <- tss(mod1_comb, test = testa_dati1)
indep_auc1 <- auc(mod1_comb, test = testa_dati1)


# var importance
vi_tss <- varImp(mod1_CV,permut = 99)
write.xlsx(vi_tss,paste0("../Rezultati/",suga,"/Modeli/Modelis1_visidati_VarImp_",suga,".xlsx"))





################ MODELIS 2 - FILTRĒTIE DATI (2) BEZ BIAS SLĀŅA (1)

# apmācību dati
trenin_dati2 <- prepareSWD(species = suga,
                           p = apmacibai2[,2:3],
                           a = fons_apmacibai1[,1:2],
                           env = egvs)
trenin_dati2 <- addSamplesToBg(trenin_dati2)

block_folds2 <- get.block(occ = trenin_dati2@coords[trenin_dati2@pa == 1, ], 
                          bg = trenin_dati2@coords[trenin_dati2@pa == 0, ])

# testa dati
testa_dati2 <- prepareSWD(species = suga,
                       p = testam2[,2:3],
                       a = fons_testam1[1:2],
                       env = egvs)

# modelis ar MaxEnt noklusējuma iestatījumiem
mod2 <- train(method = "Maxnet", 
              data = trenin_dati2,
              test=testa_dati2,
              fc="lqph",
              reg = 1,
              folds = block_folds2)

# saglabāju
write_rds(mod2,paste0("../Rezultati/",suga,"/Modeli/Modelis2_filter_",suga,".RDS"))
#mod2 <- readRDS(paste0("../Rezultati/",suga,"/Modeli/Modelis2_filter_",suga,".RDS"))

# kombinetais
mod2_comb <- combineCV(mod2)
write_rds(mod2_comb,paste0("../Rezultati/",suga,"/Modeli/Modelis2_filter_Comb_",suga,".RDS"))
#mod2_comb <- readRDS(paste0("../Rezultati/",suga,"/Modeli/Modelis2_filter_Comb_",suga,".RDS"))

# Prognoze
dir.create(paste0("../Rezultati/",suga,"/Modelis2_proj"))
lapas=levels(factor(tks_rezgis$NUMURS))
for(i in seq_along(lapas)){
  print(i)
  sakums=Sys.time()
  lapai=lapas[i]
  lapa=tks_rezgis %>%
    filter(NUMURS == lapai)
  egv_mazs=terra::crop(egvs,lapa)
  #names(egv_mazs)
  projekcijai=predict(mod2_comb,
                      data = egv_mazs,
                      type = "cloglog",
                      file = paste0("../Rezultati/",suga,"/Modelis2_proj/proj_",lapai,".tif"),
                      overwrite=TRUE)
  #plot(projekcijai)
  beigas=Sys.time()
  ilgums=beigas-sakums
  print(ilgums)
}
slani=list.files("../Rezultati/",suga,"/Modelis2_proj/",full.names=TRUE)
virt_slani=terra::vrt(slani)
writeRaster(virt_slani,paste0("../Rezultati/",suga,"/Modeli/Modelis2_HSmap_",suga,".tif"))
projekcija=rast("../Rezultati/",suga,"/Modeli/Modelis2_HSmap_",suga,".tif")
plot(projekcija)

# thresholds
ths <- SDMtune::thresholds(mod2_comb, 
                           type = "cloglog",
                           test=testa_dati2)
ths$suga <- suga
write.xlsx(ths,paste0("../Rezultati/",suga,"/Modeli/Modelis2_filter_",suga,".xlsx"))

# TSS, AUC
indep_tss2 <- tss(mod2_comb, test = testa_dati2)
indep_auc2 <- auc(mod2_comb, test = testa_dati2)

# var importance
vi_tss <- varImp(mod2_CV,permut = 99)
write.xlsx(vi_tss,paste0("../Rezultati/",suga,"/Modeli/Modelis2_filter_VarImp_",suga,".xlsx"))





################ MODELIS 3 - FILTRĒTIE DATI (2) AR BIŠU BIAS SLĀNI (2)

# apmācību dati
trenin_dati3 <- prepareSWD(species = suga,
                           p = apmacibai2[,2:3],
                           a = fons_apmacibai2[,1:2],
                           env = egvs)
trenin_dati3=addSamplesToBg(trenin_dati3)

block_folds3 <- get.block(occ = trenin_dati3@coords[trenin_dati3@pa == 1, ], 
                          bg = trenin_dati3@coords[trenin_dati3@pa == 0, ])

# testa dati
testa_dati3=prepareSWD(species = suga,
                       p = testam2[,2:3],
                       a = fons_testam2[1:2],
                       env = egvs)

# modelis ar MaxEnt noklusējuma iestatījumiem
mod3 <- train(method = "Maxnet", 
              data = trenin_dati3,
              test=testa_dati3,
              fc="lqph",
              reg = 1,
              folds = block_folds3)

# saglabāju
write_rds(mod3,paste0("../Rezultati/",suga,"/Modeli/Modelis3_filterbeebias_",suga,".RDS"))
#mod3 <- readRDS("../Rezultati/",suga,"/Modeli/Modelis3_filterbeebias_",suga,".RDS")

# kombinetais
mod3_comb=combineCV(mod3)
write_rds(mod3_comb,paste0("../Rezultati/",suga,"/Modeli/Modelis3_filterbeebias_Comb_",suga,".RDS"))
#mod3_comb <- readRDS("../Rezultati/",suga,"/Modeli/Modelis3_filterbeebias_Comb_",suga,".RDS")

# Prognoze
dir.create("../Rezultati/",suga,"/Modelis3_proj")
lapas=levels(factor(tks_rezgis$NUMURS))
for(i in seq_along(lapas)){
  print(i)
  sakums=Sys.time()
  lapai=lapas[i]
  lapa=tks_rezgis %>%
    filter(NUMURS == lapai)
  egv_mazs=terra::crop(egvs,lapa)
  #names(egv_mazs)
  projekcijai=predict(mod3_comb,
                      data = egv_mazs,
                      type = "cloglog",
                      file = paste0("../Rezultati/",suga,"/Modelis3_proj/proj_",lapai,".tif"),
                      overwrite=TRUE)
  #plot(projekcijai)
  beigas=Sys.time()
  ilgums=beigas-sakums
  print(ilgums)
}
slani=list.files("../Rezultati/",suga,"/Modelis3_proj/",full.names=TRUE)
virt_slani=terra::vrt(slani)
writeRaster(virt_slani,paste0("../Rezultati/",suga,"/Modeli/Modelis3_HSmap_",suga,".tif", overwrite = TRUE))
projekcija=rast("../Rezultati/",suga,"/Modeli/Modelis3_HSmap_",suga,".tif")
plot(projekcija)


# thresholds
ths <- SDMtune::thresholds(mod3_comb, 
                           type = "cloglog",
                           test=testa_dati3)
ths$suga = suga
write.xlsx(ths,paste0("../Rezultati/",suga,"/Modeli/Modelis3_filterbeebias_",suga,".xlsx"))

# TSS, AUC
indep_tss3 <- tss(mod3_comb, test = testa_dati3)
indep_auc3 <- auc(mod3_comb, test = testa_dati3)

# var importance
vi_tss <- varImp(mod3_CV,permut = 99)
write.xlsx(vi_tss,paste0("../Rezultati/",suga,"/Modeli/Modelis3_filterbeebias_VarImp_",suga,".xlsx"))





################ MODELIS 4 - FILTRĒTIE DATI (2) AR SUGAS BIAS SLĀNI (3)

# apmācību dati
trenin_dati4 <- prepareSWD(species = suga,
                           p = apmacibai2[,2:3],
                           a = fons_apmacibai3[,1:2],
                           env = egvs)
trenin_dati4=addSamplesToBg(trenin_dati4)

block_folds4 <- get.block(occ = trenin_dati4@coords[trenin_dati4@pa == 1, ], 
                          bg = trenin_dati4@coords[trenin_dati4@pa == 0, ])

# testa dati
testa_dati4=prepareSWD(species = suga,
                       p = testam2[,2:3],
                       a = fons_testam3[1:2],
                       env = egvs)

# modelis ar MaxEnt noklusējuma iestatījumiem
mod4 <- train(method = "Maxnet", 
              data = trenin_dati4,
              test=testa_dati4,
              fc="lqph",
              reg = 1,
              folds = block_folds4)

# saglabāju
write_rds(mod4,paste0("../Rezultati/",suga,"/Modeli/Modelis4_filterspbias_",suga,".RDS"))
#mod4 <- readRDS("../Rezultati/",suga,"/Modeli/Modelis4_filterspbias_",suga,".RDS")

# kombinetais
mod4_comb=combineCV(mod4)
write_rds(mod4_comb,paste0("../Rezultati/",suga,"/Modeli/Modelis4_filterspbias_Comb_",suga,".RDS"))
#mod4_comb <- readRDS("../Rezultati/",suga,"/Modeli/Modelis4_filterspbias_Comb_",suga,".RDS")

# Prognoze
dir.create("../Rezultati/",suga,"/Modelis4_proj")
lapas=levels(factor(tks_rezgis$NUMURS))
for(i in seq_along(lapas)){
  print(i)
  sakums=Sys.time()
  lapai=lapas[i]
  lapa=tks_rezgis %>%
    filter(NUMURS == lapai)
  egv_mazs=terra::crop(egvs,lapa)
  #names(egv_mazs)
  projekcijai=predict(mod4_comb,
                      data = egv_mazs,
                      type = "cloglog",
                      file = paste0("../Rezultati/",suga,"/Modelis4_proj/proj_",lapai,".tif"),
                      overwrite=TRUE)
  #plot(projekcijai)
  beigas=Sys.time()
  ilgums=beigas-sakums
  print(ilgums)
}
slani=list.files("../Rezultati/",suga,"/Modelis4_proj/",full.names=TRUE)
virt_slani=terra::vrt(slani)
writeRaster(virt_slani,paste0("../Rezultati/",suga,"/Modeli/Modelis4_HSmap_",suga,".tif", overwrite = TRUE))
projekcija=rast("../Rezultati/",suga,"/Modeli/Modelis4_HSmap_",suga,".tif")
plot(projekcija)

# thresholds
ths <- SDMtune::thresholds(mod4_comb, 
                           type = "cloglog",
                           test=testa_dati4)
ths$suga = suga
write.xlsx(ths,paste0("../Rezultati/",suga,"/Modeli/Modelis4_filterspbias_",suga,".xlsx"))

# TSS, AUC
indep_tss4 <- tss(mod4_comb, test = testa_dati4)
indep_auc4 <- auc(mod4_comb, test = testa_dati4)

# var importance
vi_tss <- varImp(mod4_CV,permut = 99)
#write.xlsx(vi_tss,paste0("../Rezultati/",suga,"/Modeli/Modelis4_filterspbias_VarImp_",suga,".xlsx"))


