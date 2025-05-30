# Pakotnes ----

library(terra)
library(MASS)
library(readxl)
library(tidyverse)
library(sf)
library(raster)

# Faili ----
# References

ref100m <- raster::raster("../Ievades_dati/References/LV100m_10km.tif")
ref100m_t <- rast("../Ievades_dati/References/LV100m_10km.tif")

# Bišu dati
# novērojumi lejupielādēti no portāla "dabasdati.lv"

api <- read.csv("../Ievades_dati/Noverojumi/Apidae_16-24.csv", sep = ";")
and <- read.csv("../Ievades_dati/Noverojumi/Andrenidae_16-24.csv", sep = ";")
meg <- read.csv("../Ievades_dati/Noverojumi/Megachilidae_16-24.csv", sep = ";")
col <- read.csv("../Ievades_dati/Noverojumi/Colletidae_16-24.csv", sep = ";")
hal <- read.csv("../Ievades_dati/Noverojumi/Halictidae_16-24.csv", sep = ";")
mel <- read.csv("../Ievades_dati/Noverojumi/Melittidae_16-24.csv", sep = ";")
ant <- read.csv("../Ievades_dati/Noverojumi/Anthophoridae_16-24.csv", sep = ";") # novecojis nosaukums

bites_list <- list(api,and,meg,col,hal,mel,ant)
bites <- Reduce(function(x, y) merge(x, y, all=TRUE), bites_list)

rm(and,ant,api,bites_list,col,hal,meg,mel)

bites_coords <- bites %>%
  dplyr::select(X,Y)
bites_coords <- as.data.frame(bites_coords)
bites_coords <- bites_coords %>%
  mutate(Y = as.numeric(Y) - 6000000)

# Bounding box
xlims <- c(xmin(ref100m), xmax(ref100m))
ylims <- c(ymin(ref100m), ymax(ref100m))
# 302800, 772800, 162900, 448900


# Direktorija slāņu saglabāšanai
dir.create("../Ievades_dati/Bias_layers")
dir.create("../Ievades_dati/Bias_layers/Bias_starprezultati")


# Novērojumu sagatavošana ----

noverojumi <- read_excel("../Ievades_dati/Noverojumi/SuguNoverojumi_BITES.xlsx")
noverojumi <- noverojumi %>%
  filter(Izmantojams == 1) 

noverojumi$avots <- sub( "_.*", "", noverojumi$ID) 

noverojumi_apk <- noverojumi %>%
  group_by(avots) %>%
  summarise(n = n()) %>%
  ungroup()

noverojumi_rubene <- noverojumi %>%
  filter(avots == "B.Rubene") %>%
  group_by(Gads) %>%
  summarise(n = n()) %>%
  ungroup()

# Piešķirt grupas novērojumiem
noverojumi$grupa <- ifelse(noverojumi$avots == "B.Rubene" & noverojumi$Gads != 2022, "Citi",
                           ifelse(noverojumi$avots %in% c("Agrihorts", "B.Rubene", "M.Merle"), "Petijumi", "Citi"))

# Cik novērojumu katrā grupā
noverojumi_kopa <- noverojumi %>%
  group_by(grupa) %>%
  summarise(n = n())

nov_sugas <- noverojumi %>%
  count(Kods, grupa) %>%
  pivot_wider(names_from = grupa, values_from = n, values_fill = 0)


# Gadījuma rakstura novērojumi ----

citi_nov <- noverojumi %>%
  filter(grupa == "Citi") %>%
  filter(avots != "Dabasdati") %>%
  dplyr::select(X,Y)

gadijuma_nov <- rbind(bites_coords,citi_nov)

dens <- kde2d(gadijuma_nov$X, gadijuma_nov$Y, h=10000, n = c(nrow(ref100m), ncol(ref100m)), 
              lims = c(302800, 772800, 162900, 448900)) 

dens.ras <- raster::raster(dens)
plot(dens.ras)

bias <- rast(dens.ras)
bias <- terra::resample(bias, ref100m_t, method = "bilinear")
bias_crop <- crop(bias,ref100m_t,mask = TRUE)
plot(bias_crop)

min(bias_crop, na.rm = TRUE)
writeRaster(bias_crop,"../Ievades_dati/Bias_layers/Bias_starprezultati/Bias_gadijuma_10000.tif", overwrite = TRUE)

rm(bias,bias_crop,dens,dens.ras)





# Strukturēti ievākti dati ----


# Šajos failos apkopotas koordinātas (x,y) un lamatu eksponēšanas dienas (Lamatdienas)
rubene <- read.csv("../Ievades_dati/Noverojumi/Koordinatas_Rubene.csv", sep = ";")
merle <- read.csv("../Ievades_dati/Noverojumi/Koordinatas_Merle.csv", sep = ";")

rubene_coords <- rubene %>% 
  mutate(rinda=rownames(.)) 


# CRS maiņa uz LKS-92
funkcijaX <- function(x){
  a=x %>% 
    st_as_sf(coords=c("Y","X"),crs=4326) %>% 
    st_transform(crs=3059) %>% 
    st_coordinates() %>% 
    data.frame()
  a[,1]}

funkcijaY <- function(x){
  a=x %>% 
    st_as_sf(coords=c("Y","X"),crs=4326) %>% 
    st_transform(crs=3059) %>% 
    st_coordinates() %>% 
    data.frame()
  a[,2]}

rubene_coords$lksX=NA_real_
rubene_coords$lksY=NA_real_
for(i in seq_along(rubene_coords$rinda)){
  rubene_coords$lksX[i]=funkcijaX(rubene_coords[i,])
  rubene_coords$lksY[i]=funkcijaY(rubene_coords[i,])
}

rubene_coords <- rubene_coords[, c("lksX", "lksY", "Lamatdienas")]
colnames(rubene_coords) <- c("X", "Y", "Lamatdienas")
mr_coords <- rbind(merle, rubene_coords)

mr_coords <- mr_coords %>% 
  uncount(Lamatdienas)

# "Agrihorts" pētījumi
# Failā apkopotas koordinātas (x,y) un lamatu eksponēšanas dienas (Lamatdienas)
agrih <- read.csv("../Ievades_dati/Noverojumi/Koordinatas_agrihorts.csv", sep = ";")

agrih_coords <- agrih %>%
  dplyr::select(X,Y,Lamatdienas)
agrih_coords <- as.data.frame(agrih_coords)


# CRS maiņa uz LKS-92
agrih_coords <- agrih_coords %>% 
  mutate(rinda=rownames(.)) 

agrih_coords$lksX=NA_real_
agrih_coords$lksY=NA_real_
for(i in seq_along(agrih_coords$rinda)){
  agrih_coords$lksX[i]=funkcijaX(agrih_coords[i,])
  agrih_coords$lksY[i]=funkcijaY(agrih_coords[i,])
}

agrih_coords <- agrih_coords %>% 
  uncount(Lamatdienas)
agrih_coords <- agrih_coords %>%
  dplyr::select(lksX,lksY)
colnames(agrih_coords) <- c("X", "Y")

# visu pētījumu koordinātu apvienojums
petijumu_coords <- rbind(agrih_coords,mr_coords)


# paraugošanas piepūles noviržu slāņa izveide

dens <- kde2d(petijumu_coords$X, petijumu_coords$Y, h = 10000, n = c(nrow(ref100m), ncol(ref100m)), 
              lims = c(302800, 772800, 162900, 448900)) 

dens.ras <- raster::raster(dens)
plot(dens.ras)

bias <- rast(dens.ras)
bias <- terra::resample(bias, ref100m_t, method = "bilinear")
bias_crop <- crop(bias,ref100m_t,mask = TRUE)
plot(bias_crop)

writeRaster(bias_crop,"../Ievades_dati/Bias_layers/Bias_starprezultati/Bias_petijumi_10000.tif", overwrite = TRUE)

rm(bias,bias_crop,dens,dens.ras)


# Bias slāņu izveide ----

# 1. katru izveidoto slāni mērogot ar max=1
# 2. aprēķināt šūnu svērtos vidējos:
# 2.a. kopējam novērojumu sadalījumam
# 2.b ik sugai specifiskajam novērojumu sadalījumam
# 3. mērogot rezultējošos slāņus ar max=1


# 1. 

bias_petijumi <- rast("../Ievades_dati/Bias_layers/Bias_starprezultati/Bias_petijumi_10000.tif")
bias_gadijuma <- rast("../Ievades_dati/Bias_layers/Bias_starprezultati/Bias_gadijuma_10000.tif")
plot(bias_gadijuma)
plot(bias_petijumi)

# Pētījumu novērojumi

max  <- global(bias_petijumi, fun = "max", na.rm = TRUE)[1, 1]
merogots_petijumi <- bias_petijumi / max
plot(merogots_petijumi)
rm(max)

# Gadījuma novērojumi

max  <- global(bias_gadijuma, fun = "max", na.rm = TRUE)[1, 1]
merogots_gadijuma <- bias_gadijuma / max
plot(merogots_gadijuma)
rm(max)


# 2. Svērtie vidējie
# 2.a - Kopējie

gadijuma <- 847  
petijumi <- 1665  
kopa=gadijuma+petijumi

weighted <- (merogots_gadijuma * (gadijuma/kopa) + merogots_petijumi * (petijumi/kopa))
max_w=global(weighted, fun = "max", na.rm = TRUE)[1, 1]
weighted=weighted/max_w
plot(weighted, main = "Kopā weighted")


# 2.b - Sugu specifiskie
# ANDHAT

gad_andhat <- 71
pet_andhat <- 5

weighted_andhat <- (merogots_gadijuma * gad_andhat + merogots_petijumi * pet_andhat) / (gad_andhat + pet_andhat)
max_ah=global(weighted_andhat, fun = "max", na.rm = TRUE)[1, 1]
weighted_andhat=weighted_andhat/max_ah
plot(weighted_andhat, main = "ANDHAT weighted")

# DASHIR

gad_dashir <- 62
pet_dashir <- 116

weighted_dashir <- (merogots_gadijuma * gad_dashir + merogots_petijumi * pet_dashir) / (gad_dashir + pet_dashir)
max_dh=global(weighted_dashir, fun = "max", na.rm = TRUE)[1, 1]
weighted_dashir=weighted_dashir/max_dh
plot(weighted_dashir, main = "DASHIR weighted")



# 3. mērogošana

# Kopējais
max  <- global(weighted, fun = "max", na.rm = TRUE)[1, 1]
Kopejais_bias_max <- weighted / max
Kopejais_bias_max=ifel(Kopejais_bias_max==0,9e-16,Kopejais_bias_max)
writeRaster(Kopejais_bias_max, "../Ievades_dati/Bias_layers/Bias_kopa_max.tif",
            overwrite=TRUE)

# ANDHAT
max  <- global(weighted_andhat, fun = "max", na.rm = TRUE)[1, 1]
ANDHAT_bias_max <- weighted_andhat / max
ANDHAT_bias_max=ifel(ANDHAT_bias_max==0,9e-16,ANDHAT_bias_max)
writeRaster(ANDHAT_bias_max, "../Ievades_dati/Bias_layers/Bias_ANDHAT_max.tif",
            overwrite=TRUE)


# DASHIR
max  <- global(weighted_dashir, fun = "max", na.rm = TRUE)[1, 1]
DASHIR_bias_max <- weighted_dashir / max
DASHIR_bias_max=ifel(DASHIR_bias_max==0,9e-16,DASHIR_bias_max)
writeRaster(DASHIR_bias_max, "../Ievades_dati/Bias_layers/Bias_DASHIR_max.tif",
            overwrite=TRUE)
