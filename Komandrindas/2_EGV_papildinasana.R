# Pakotnes ----

library(readxl)
library(terra)
library(tidyverse)
library(openxlsx)


# Papildus jau pieejamajiem EGV, pieņemts lēmums par papildus mainīgo izveidi
# (ekoloģiskais pamatojums aprakstīts darba 2.3.1. apakšnodaļā):

# 1) apvienot visas aramzemes, kultivētos zālājus un papuves (šūnas līmenī, r500 un r1250);
# 2) apvienot izcirtumus, krūmājus un jaunaudzes (r500 un r1250)
# 3) Izveidot papildus augsnes parametrus raksturojošus EGV - kalcija karbonāta saturu,
# augsnes fragmentu izmēru un augsnes blīvumu 0 - 10 cm slānī (šūnas līmenī)

# mapes starprezultātu saglabāšanai
dir.create("../Ievades_dati/EGV/EGV_starprezultati")
dir.create("../Ievades_dati/EGV/EGV_starprezultati/for_scaling")


# 1) Aramzemes + kultivētie zālāji + papuves ----

## Faili ----

# šūnas līmenis (100m)
aramz <- rast("../Ievades_dati/EGV/EGV_faili/Lauku_AramVisas_cell.tif")
kultivz <- rast("../Ievades_dati/EGV/EGV_faili/Lauku_ZalajiKultivetie_cell.tif")
pap <- rast ("../Ievades_dati/EGV/EGV_faili/Lauku_Papuves_cell.tif")

# 500m rādiuss ap šūnu
aramz500 <- rast("../Ievades_dati/EGV/EGV_faili/Lauku_AramVisas_r500.tif")
kultivz500 <- rast("../Ievades_dati/EGV/EGV_faili/Lauku_ZalajiKultivetie_r500.tif")
pap500 <- rast ("../Ievades_dati/EGV/EGV_faili/Lauku_Papuves_r500.tif")

# 1250m rādiuss ap šūnu
aramz1250 <- rast("../Ievades_dati/EGV/EGV_faili/Lauku_AramVisas_r1250.tif")
kultivz1250 <- rast("../Ievades_dati/EGV/EGV_faili/Lauku_ZalajiKultivetie_r1250.tif")
pap1250 <- rast ("../Ievades_dati/EGV/EGV_faili/Lauku_Papuves_r1250.tif")


## Slāņu izveide un saglabāšana ----

# šūnas līmenis (100m)
lauki1 <- mosaic(aramz, kultivz, pap, fun = "sum",
                 filename = "../Ievades_dati/EGV/EGV_starprezultati/for_scaling/Lauku_AramzKultivzPapuves_cell.tif")

# 500m rādiuss ap šūnu
lauki2 <- mosaic(aramz500, kultivz500, pap500, fun = "sum",
                 filename = "../Ievades_dati/EGV/EGV_starprezultati/for_scaling/Lauku_AramzKultivzPapuves_r500.tif")

# 1250m rādiuss ap šūnu
lauki3 <- mosaic(aramz1250, kultivz1250, pap1250, fun = "sum",
                 filename = "../Ievades_dati/EGV/EGV_starprezultati/for_scaling/Lauku_AramzKultivzPapuves_r1250.tif")


rm(aramz,kultivz,pap,aramz500,kultivz500,pap500,aramz1250,kultivz1250,pap1250,
   lauki1,lauki2,lauki3,lauki4)


# 2) Izcirtumi, krūmāji un jaunaudzes ----

## Faili ----

# 500m rādiuss ap šūnu
jkr500 <- rast("../EGV_2024/rastri_100m/Proj/Mezi_Jaunaudzes5mKrumaji_cell.tif")
izc500 <- rast("../EGV_2024/rastri_100m/Proj/Mezi_IzcUNzem5m_cell.tif")

# 1250m rādiuss ap šūnu
jkr1250 <- rast("../EGV_2024/rastri_100m/Proj/Mezi_Jaunaudzes5mKrumaji_r1250.tif")
izc1250 <- rast("../EGV_2024/rastri_100m/Proj/Mezi_IzcUNzem5m_r1250.tif")


## Slāņu izveide un saglabāšana ----

# 500m rādiuss ap šūnu
mezi1 <- mosaic(izc500, jkr500, fun = "sum", 
                filename = "../Ievades_dati/EGV/EGV_starprezultati/for_scaling/Mezi_IzcKrumiJaunaudzes_r500.tif")

# 1250m rādiuss ap šūnu
mezi2 <- mosaic(izc1250, jkr1250, fun = "sum",
                filename = "../Ievades_dati/EGV/EGV_starprezultati/for_scaling/Mezi_IzcKrumiJaunaudzes_r1250.tif")


rm(izc500,jkr500,izc1250,jkr1250,mezi1,mezi2,mezi3)



# 3) Augšņu dati ----

## Faili ----
# references rastri

ref100m <- rast("../Ievades_dati/References/nulles_LV100m_10km.tif")
ref10m <- rast("../Ievades_dati/References/LV10m_10km.tif")

# augšņu dati
bd010 <- rast("../JRC_EJP_LV/LV_BD010.tif")
cf <- rast("../JRC_EJP_LV/LV_CF.tif")
caco3 <- rast("../JRC_EJP_LV/LV_CaCO3.tif")

# 10m rastri
udeni <- rast("../Ievades_dati/Rastri_10m/Ainava_UdeniYN.tif")
apbuve <- rast("../Ievades_dati/Rastri_10m/Ainava_ApbuveYN.tif")



## Masku izveide ----

# Ūdeņu maska 
# Visās 100m šūnās, kurās kaut viens pikselis 10m izšķirtspējā ir ūdeņi, likšu vērtību 0 (pārējais NA)
udeni100 <- resample(udeni,ref100m,method = "max")
udeni100[udeni100 == 0] <- NA # 0 par NA
udeni100[udeni100 == 1] <- 0 # 1 par 0
plot(udeni100, main = "Ūdeņu maska")
writeRaster(udeni100, "../Ievades_dati/EGV_starprezultati/Udeni100m_maska.tif", overwrite = TRUE)

# Apbūves maska 
# Visās 100m šūnās, kurās kaut viens pikselis 10m izšķirtspējā ir apbūve, likšu vērtību 0 (pārējais NA)
apbuve100 <- resample(apbuve,ref100m,method = "max")
apbuve100[apbuve100 == 0] <- NA # 0 par NA
apbuve100[apbuve100 == 1] <- 0 # 1 par 0
plot(apbuve100, main = "Apbūves maska")
writeRaster(apbuve100, "../Ievades_dati/EGV_starprezultati/Apbuve100m_maska.tif", overwrite = TRUE)

rm(udeni,apbuve)





## Bulk density in the 0 - 10 cm layer ----

plot(bd010, main = "Bulk density in the 0 - 10 cm layer")
# pārprojicēšana atbilstoši references rastram
bd010_2 = project(bd010,ref100m,use_gdal=TRUE,method="bilinear", filename = "../Ievades_dati/EGV_starprezultati/BD010_100m_proj.tif", overwrite=TRUE)

# tukšo pikseļu inventarizācija
emp_bd <- is.na(bd010_2) & !is.na(ref100m)
emp_bd_count <- global(emp_bd, fun="sum", na.rm=TRUE)
emp_bd_count # 16936
plot(emp_bd, main = "Bulk density tukšie pikseļi") # gandrīz visi gar malām


# Ūdeņu maskas pielietošana - tukšumu aizpildīšana vērtību 0
bd_udeni <- cover(bd010_2,udeni100)
plot(bd_udeni, main = "Bulk density in the 0 - 10 cm layer + ūdeņu maska")

# Apbūves maskas pielietošana - tukšumu aizpildīšana ar vērtību 0
bd_masked <- cover(bd_udeni,apbuve100, filename = "starprezultati/bd_masked.tif", overwrite=TRUE)
plot(bd_masked, main = "Bulk density in the 0 - 10 cm layer + ūdeņu maska + apbūves maska")


# atkārtota tukšo pikseļu inventarizācija pēc ūdeņu un apbūves masku pielietošanas
emp_bd <- is.na(bd_masked) & !is.na(ref100m)
emp_bd_count <- global(emp_bd, fun="sum", na.rm=TRUE)
emp_bd_count # 6512
plot(emp_bd, main = "Bulk density tukšie pikseļi pēc masku pielietošanas") # gandrīz visi gar malām


# cik platas ir malas bez vērtībām?
aizpilditie <- ifel(!emp_bd, 1, NA)
platumi <- distance(aizpilditie)
plot(platumi, main = "Attālums līdz tuvākajai ne-NA vērtībai")
max_attalums <- global(platumi, fun = "max", na.rm = TRUE)
print(max_attalums) # lielākais platums ir 223
# Bet šis ir attālums (metri) - filtrs prasa pikseļu (100 x 100m) skaitu, 
# tātad (apaļojot uz augšu) 3 * 2 = 6


# tukšumu aizpildīšana
wbt_fill_missing_data(
  i = "../Ievades_dati/EGV_starprezultati/bd_masked.tif",
  output = "../Ievades_dati/EGV_starprezultati/bd_filled.tif",
  filter = 6,     
  weight = 2,
  no_edges = FALSE
)

result <- rast("../Ievades_dati/EGV_starprezultati/bd_filled.tif")
plot(result, main = "weight = 2")


# lieko malu apgriešana (kas neietilpst 100m references rastrā)
bd_filled <- mask(result, ref100m, filename = "../Ievades_dati/EGV/EGV_starprezultati/for_scaling/Soils_BulkDensity010.tif", overwrite=TRUE)

# Pārbaude - vai tiešām viss ir aizpildījies ar šādu filtra vērtību
emp_bd <- is.na(bd_filled) & !is.na(ref100m)
emp_bd_count <- global(emp_bd, fun="sum", na.rm=TRUE)
emp_bd_count # 0

rm(bd_filled,bd010,bd010_2,bd_udeni,bd_masked,emp_bd,emp_bd_count,max_attalums,platumi,result,aizpilditie)





## Coarse fragments ----

plot(cf, main = "Coarse fragments")
# pārprojicēšana atbilstoši references rastram
cf_2 = project(cf,ref100m, use_gdal=TRUE, method="bilinear", filename = "../Ievades_dati/EGV_starprezultati/cf_100m_proj.tif", overwrite=TRUE)


# tukšo pikseļu inventarizācija
emp_cf <- is.na(cf_2) & !is.na(ref100m)
emp_cf_count <- global(emp_cf, fun="sum", na.rm=TRUE)
emp_cf_count # 17158
plot(emp_cf, main = "Coarse fragments tukšie pikseļi") # gandrīz visi gar malām


# Ūdeņu maskas pielietošana - tukšumu aizpildīšana vērtību 0
cf_udeni <- cover(cf_2,udeni100)
plot(cf_udeni, main = "Coarse fragments + ūdeņu maska")

# Apbūves maskas pielietošana - tukšumu aizpildīšana ar vērtību 0
cf_masked <- cover(cf_udeni,apbuve100, filename = "../Ievades_dati/EGV_starprezultati/cf_masked.tif", overwrite=TRUE)
plot(cf_masked, main = "CaCO3 + ūdeņu maska + apbūves maska")

# atkārtota tukšo pikseļu inventarizācija pēc ūdeņu un apbūves masku pielietošanas
emp_cf <- is.na(cf_masked) & !is.na(ref100m)
emp_cf_count <- global(emp_cf, fun="sum", na.rm=TRUE)
emp_cf_count # 6592
plot(emp_cf, main = "Bulk density tukšie pikseļi pēc masku pielietošanas") # gandrīz visi gar malām


# cik platas ir malas bez vērtībām?
aizpilditie <- ifel(!emp_cf, 1, NA)
platumi <- distance(aizpilditie)
plot(platumi, main = "Attālums līdz tuvākajai ne-NA vērtībai")
max_attalums <- global(platumi, fun = "max", na.rm = TRUE)
print(max_attalums) # lielākais platums ir 223
# Bet šis ir attālums (metri) - filtrs prasa pikseļu (100x 100m) skaitu, tātad 3 * 2 = 6


# tukšumu aizpildīšana

wbt_fill_missing_data(
  i = "../Ievades_dati/EGV_starprezultati/cf_masked.tif",
  output = "../Ievades_dati/EGV_starprezultati.tif",
  filter = 6,     
  weight = 1,
  no_edges = FALSE
)

result <- rast("../Ievades_dati/EGV_starprezultati/cf_filled.tif")
plot(result, main = "weight = 2")

# lieko malu apgriešana (kas neietilpst 100m references rastrā)
cf_filled <- mask(result, ref100m, filename = "../Ievades_dati/EGV/EGV_starprezultati/for_scaling/Soils_CoarseFragments.tif", overwrite=TRUE)

# Pārbaude - vai tiešām viss ir aizpildījies ar šādu filtra vērtību
emp_cf <- is.na(cf_filled) & !is.na(ref100m)
emp_cf_count <- global(emp_cf, fun="sum", na.rm=TRUE)
emp_cf_count # 0

rm(cf_filled,cf,cf_2,cf_udeni,cf_masked,emp_cf,emp_cf_count,max_attalums,platumi,result,aizpilditie)




## CaCO3 ----

plot(caco3, main = "CaCO3")

# pārprojicēšana atbilstoši references rastram
caco3_2 = project(caco3,ref100m, use_gdal=TRUE, method="bilinear", filename = "../Ievades_dati/EGV_starprezultati/caco3_100m_proj.tif", overwrite=TRUE)

# tukšo pikseļu inventarizācija
emp_caco3 <- is.na(caco3_2) & !is.na(ref100m)
emp_caco3_count <- global(emp_caco3, fun="sum", na.rm=TRUE)
emp_caco3_count # 195100
plot(emp_caco3, main = "CaCO3 tukšie pikseļi") 
# jau iepriekš bija redzamas ūdeņu problēmas, bet arī gar malām ir gana daudz iztrūkumu


# Ūdeņu maskas pielietošana - tukšumu aizpildīšana vērtību 0
caco3_udeni <- cover(caco3_2,udeni100)
plot(caco3_udeni, main = "CaCO3 + ūdeņu maska")

# Apbūves maskas pielietošana - tukšumu aizpildīšana ar vērtību 0
caco3_masked <- cover(caco3_udeni,apbuve100, filename = "../Ievades_dati/EGV_starprezultati/caco3_masked.tif", overwrite=TRUE)
plot(caco3_masked, main = "CaCO3 + ūdeņu maska + apbūves maska")


# atkārtota tukšo pikseļu inventarizācija pēc ūdeņu un apbūves masku pielietošanas
emp_caco3 <- is.na(caco3_masked) & !is.na(ref100m)
emp_caco3_count <- global(emp_caco3, fun="sum", na.rm=TRUE)
emp_caco3_count # 37489
plot(emp_caco3, main = "CaCO3 tukšie pikseļi") 

# cik platas ir malas bez vērtībām?
aizpilditie <- ifel(!emp_caco3, 1, NA)
platumi <- distance(aizpilditie)
plot(platumi, main = "Attālums līdz tuvākajai ne-NA vērtībai")
max_attalums <- global(platumi, fun = "max", na.rm = TRUE)
print(max_attalums) # lielākais platums ir 721
# Bet šis ir attālums (metri) - filtrs prasa pikseļu (100 x 100m) skaitu, 
# tātad (apaļojot uz augšu) 8 * 2 = 16

# tukšumu aizpildīšana

wbt_fill_missing_data(
  i = "../Ievades_dati/EGV_starprezultati/caco3_masked.tif",
  output = "../Ievades_dati/EGV_starprezultati/caco3_filled.tif",
  filter = 16,     
  weight = 1,
  no_edges = FALSE
)

result <- rast("../Ievades_dati/EGV_starprezultati/caco3_filled.tif")
plot(result, main = "weight = 2")


# lieko malu apgriešana (kas neietilpst 100m references rastrā)
caco3_filled <- mask(result, ref100m, filename = "../Ievades_dati/EGV/EGV_starprezultati/for_scaling/Soils_CalciumCarbonate.tif", overwrite=TRUE)

# Pārbaude - vai tiešām viss ir aizpildījies ar šādu filtra vērtību
emp_caco3 <- is.na(caco3_filled) & !is.na(ref100m)
emp_caco3_count <- global(emp_caco3, fun="sum", na.rm=TRUE)
emp_caco3_count # 0




# Centrēšana un mērogošana ----

definicija=rast("../Ievades_dati/References/nulles_LV100m_10km.tif")
failu_celi=list.files("../Ievades_dati/EGV/EGV_starprezultati/for_scaling/",full.names = TRUE,pattern=".tif$")
nosaukumiem=list.files("../Ievades_dati/EGV/EGV_starprezultati/for_scaling/",full.names = FALSE,pattern=".tif$")

faili=data.frame(faili=failu_celi,
                 nosaukumi=nosaukumiem,
                 pietrukst=NA)


faili$pietrukst2=NA
for(i in seq_along(faili$faili)){
  print(i)
  sakums=Sys.time()
  fails=rast(faili$faili[i])
  fails=project(fails,definicija)
  maskets=mask(fails,definicija)
  iztrukumi=ifel(is.na(maskets)&!is.na(definicija),1,NA)
  frekvences=freq(iztrukumi)
  vertiba=frekvences$count[frekvences$value==1]
  faili$pietrukst2[i]=vertiba
  beigas=Sys.time()
  laiks=beigas-sakums
  print(laiks)
}

write.xlsx(faili,"./gatavie/RAW_names.xlsx")

# nekas netrūkst

nosaukumiem=read.xlsx("./gatavie/RAW_names.xlsx")

dir.create("../Ievades_dati/EGV/EGV_starprezultati/proj")

for(i in seq_along(nosaukumiem$nosaukumi)){
  print(i)
  sakums=Sys.time()
  fails=rast(nosaukumiem$faili[i])
  fails=project(fails,definicija)
  maskets=mask(fails,definicija)
  names(maskets)=substr(nosaukumiem$nosaukumi[i],1,nchar(nosaukumiem$nosaukumi[i])-4)
  writeRaster(maskets,
              filename=paste0("../Ievades_dati/EGV/EGV_starprezultati/proj/",nosaukumiem$nosaukumi[i]),
              overwrite=TRUE)
  videjais=global(maskets,fun="mean",na.rm=TRUE)
  centrets=maskets-videjais[,1]
  standartnovirze=terra::global(centrets,fun="rms",na.rm=TRUE)
  merogots=centrets/standartnovirze[,1]
  nosaukumiem$var_videjais[i]=videjais
  nosaukumiem$var_standartnovirze[i]=standartnovirze
  writeRaster(merogots,
              filename=paste0("../Ievades_dati/EGV/EGV_faili/",nosaukumiem$nosaukumi[i]),
              overwrite=TRUE)
  beigas=Sys.time()
  ilgums=beigas-sakums
  print(ilgums)
}

# Šaja failā būs pieejamas centrēto un mērogoto slāņu vidējās vērtība 
write.xlsx(nosaukumiem,"../Ievades_dati/EGV/EGV_starprezultati/EGV_names_scaling.xlsx")

