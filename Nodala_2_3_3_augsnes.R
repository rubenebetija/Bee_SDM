# Pakotnes ----

if(!require(terra)) {install.packages(terra); require(terra)} 
if(!require(whitebox)) {install.packages(whitebox); require(whitebox)} 



# Faili ----
# references rastri

ref100m <- rast("../ref_rastri/LV100m_10km.tif")
ref10m <- rast("../ref_rastri/LV10m_10km.tif")

# augšņu dati
bd010 <- rast("../JRC_EJP_LV/LV_BD010.tif")
cf <- rast("../JRC_EJP_LV/LV_CF.tif")
caco3 <- rast("../JRC_EJP_LV/LV_CaCO3.tif")

# 10m rastri
udeni <- rast("../EGV_2024/rastri_10m/Ainava_UdeniYN.tif")
apbuve <- rast("../EGV_2024/rastri_10m/Ainava_ApbuveYN.tif")



####################### Bulk density in the 0 - 10 cm layer ####################################

plot(bd010, main = "Bulk density in the 0 - 10 cm layer")

# pārprojicēšana atbilstoši references rastram
bd010_2 = project(bd010,ref100m,use_gdal=TRUE,method="bilinear", filename = "starprezultati/BD010_100m_proj.tif", overwrite=TRUE)

# tukšo pikseļu inventarizācija
emp_bd <- is.na(bd010_2) & !is.na(ref100m)
emp_bd_count <- global(emp_bd, fun="sum", na.rm=TRUE)
emp_bd_count # 16936
plot(emp_bd, main = "Bulk density tukšie pikseļi") # gandrīz visi gar malām

# cik platas ir malas bez vērtībām?
aizpilditie <- ifel(!emp_bd, 1, NA)
platumi <- distance(aizpilditie)
plot(platumi, main = "Attālums līdz tuvākajai ne-NA vērtībai")
max_attalums <- global(platumi, fun = "max", na.rm = TRUE)
print(max_attalums) # lielākais platums ir 1300
# Bet šis ir attālums (metri) - filtrs prasa pikseļu (100 x 100m) skaitu, tātad 13 * 2 = 26

# tukšumu aizpildīšana

wbt_fill_missing_data(
  i = "starprezultati/BD010_100m_proj.tif",
  output = "starprezultati/bd_filled_w1.tif",
  filter = 26,     
  weight = 1,
  no_edges = FALSE
)

wbt_fill_missing_data(
  i = "starprezultati/BD010_100m_proj.tif",
  output = "starprezultati/bd_filled_w2.tif",
  filter = 26,     
  weight = 2,
  no_edges = FALSE
)

result_w1 <- rast("starprezultati/bd_filled_w1.tif")
plot(result_w1, main = "weight = 1")
result_w2 <- rast("starprezultati/bd_filled_w2.tif")
plot(result_w2, main = "weight = 2")
# vizuāli lielu atšķirību neredzu, GIS gan to var redzēt


bd_w1_filled <- mask(result_w1, ref100m, filename = "gatavie/BD010_cell_w1.tif", overwrite=TRUE)
bd_w2_filled <- mask(result_w2, ref100m, filename = "gatavie/BD010_cell_w2.tif", overwrite=TRUE)

# Pārbaude - vai tiešām viss ir aizpildījies ar šādu filtra vērtību
emp_bd <- is.na(bd_w1_filled) & !is.na(ref100m)
emp_bd_count <- global(emp_bd, fun="sum", na.rm=TRUE)
emp_bd_count # 0

rm(bd_w1_filled,bd_w2_filled,bd010,bd010_2,emp_bd,emp_bd_count,max_attalums,platumi, result_w1,result_w2)




######################## Coarse fragments ##################################################

plot(cf, main = "Coarse fragments")

# pārprojicēšana atbilstoši references rastram
cf_2 = project(cf,ref100m, use_gdal=TRUE, method="bilinear", filename = "starprezultati/cf_100m_proj.tif", overwrite=TRUE)

# tukšo pikseļu inventarizācija
emp_cf <- is.na(cf_2) & !is.na(ref100m)
emp_cf_count <- global(emp_cf, fun="sum", na.rm=TRUE)
emp_cf_count # 17158
plot(emp_cf, main = "Coarse fragments tukšie pikseļi") # gandrīz visi gar malām

# cik platas ir malas bez vērtībām?
aizpilditie <- ifel(!emp_cf, 1, NA)
platumi <- distance(aizpilditie)
plot(platumi, main = "Attālums līdz tuvākajai ne-NA vērtībai")
max_attalums <- global(platumi, fun = "max", na.rm = TRUE)
print(max_attalums) # lielākais platums ir 1300
# Bet šis ir attālums (metri) - filtrs prasa pikseļu (100x 100m) skaitu, tātad 13 * 2 = 26

# tukšumu aizpildīšana

wbt_fill_missing_data(
  i = "starprezultati/cf_100m_proj.tif",
  output = "starprezultati/cf_filled_w1.tif",
  filter = 26,     
  weight = 1,
  no_edges = FALSE
)

wbt_fill_missing_data(
  i = "starprezultati/cf_100m_proj.tif",
  output = "starprezultati/cf_filled_w2.tif",
  filter = 26,     
  weight = 2,
  no_edges = FALSE
)

result_w1 <- rast("starprezultati/cf_filled_w1.tif")
plot(result_w1, main = "weight = 1")
result_w2 <- rast("starprezultati/cf_filled_w2.tif")
plot(result_w2, main = "weight = 2")
# vizuāli lielu atšķirību neredzu, GIS gan to var redzēt


cf_w1_filled <- mask(result_w1, ref100m, filename = "gatavie/CF010_cell_w1.tif", overwrite=TRUE)
cf_w2_filled <- mask(result_w2, ref100m, filename = "gatavie/CF010_cell_w2.tif", overwrite=TRUE)

# Pārbaude - vai tiešām viss ir aizpildījies ar šādu filtra vērtību
emp_cf <- is.na(cf_w1_filled) & !is.na(ref100m)
emp_cf_count <- global(emp_cf, fun="sum", na.rm=TRUE)
emp_cf_count # 0

rm(cf_w1_filled,cf_w2_filled,cf,cf_2,emp_cf,emp_cf_count,max_attalums,platumi, result_w1,result_w2)




################### CaCO3 saturs augsnē ##################################################

plot(caco3, main = "CaCO3")

# pārprojicēšana atbilstoši references rastram
caco3_2 = project(caco3,ref100m, use_gdal=TRUE, method="bilinear", filename = "starprezultati/caco3_100m_proj.tif", overwrite=TRUE)

# tukšo pikseļu inventarizācija
emp_caco3 <- is.na(caco3_2) & !is.na(ref100m)
emp_caco3_count <- global(emp_caco3, fun="sum", na.rm=TRUE)
emp_caco3_count # 195100
plot(emp_caco3, main = "CaCO3 tukšie pikseļi") 
# jau iepriekš bija redzamas ūdeņu problēmas, bet arī gar malām ir gana daudz iztrūkumu


# Ūdeņu maska - visās 100m šūnās, kurās kaut viens pikselis 10m izšķirtspējā ir ūdeņi, likšu vērtību 0
udeni100 <- resample(udeni,ref100m,method = "max")
udeni100[udeni100 == 0] <- NA # 0 par NA
udeni100[udeni100 == 1] <- 0 # 1 par 0
plot(udeni100, main = "Ūdeņu maska")
writeRaster(udeni100, "starprezultati/Udeni100m_maska.tif", overwrite = TRUE)

# maskas pielietošana - tukšumu aizpildīšana ar ūdeņu masku (vērtību 0)
caco3_udeni <- cover(caco3_2,udeni100)
plot(caco3_udeni, main = "CaCO3 + ūdeņu maska")


# Apbūves maska - visās 100m šūnās, kurās kaut viens pikselis 10m izšķirtspējā ir apbūve, likšu vērtību 0
apbuve100 <- resample(apbuve,ref100m,method = "max")
apbuve100[apbuve100 == 0] <- NA # 0 par NA
apbuve100[apbuve100 == 1] <- 0 # 1 par 0
plot(apbuve100, main = "Apbūves maska")
writeRaster(apbuve100, "starprezultati/Apbuve100m_maska.tif", overwrite = TRUE)

# maskas pielietošana - tukšumu aizpildīšana ar apbūves masku (vērtību 0)
caco3_masked <- cover(caco3_udeni,apbuve100, filename = "starprezultati/caco3_masked.tif", overwrite=TRUE)
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
  i = "starprezultati/caco3_masked.tif",
  output = "starprezultati/caco3_filled_w1.tif",
  filter = 16,     
  weight = 1,
  no_edges = FALSE
)

wbt_fill_missing_data(
  i = "starprezultati/caco3_masked.tif",
  output = "starprezultati/caco3_filled_w2.tif",
  filter = 16,     
  weight = 2,
  no_edges = FALSE
)

result_w1 <- rast("starprezultati/caco3_filled_w1.tif")
plot(result_w1, main = "weight = 1")
result_w2 <- rast("starprezultati/caco3_filled_w2.tif")
plot(result_w2, main = "weight = 2")
# vizuāli lielu atšķirību neredzu, GIS gan to var redzēt


caco3_w1_filled <- mask(result_w1, ref100m, filename = "gatavie/caco3_cell_w1.tif", overwrite=TRUE)
caco3_w2_filled <- mask(result_w2, ref100m, filename = "gatavie/caco3_cell_w2.tif", overwrite=TRUE)

# Pārbaude - vai tiešām viss ir aizpildījies ar šādu filtra vērtību
emp_caco3 <- is.na(caco3_w1_filled) & !is.na(ref100m)
emp_caco3_count <- global(emp_caco3, fun="sum", na.rm=TRUE)
emp_caco3_count # 0

