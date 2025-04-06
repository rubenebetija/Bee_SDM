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



# Bulk density in the 0 - 10 cm layer

plot(bd010)

# pārprojicēšana atbilstoši references rastram
bd010_2 = project(bd010,ref100m,use_gdal=TRUE,method="bilinear")

# tukšo pikseļu inventarizācija
emp_bd <- is.na(bd010_2) & !is.na(ref100m)
emp_bd_count <- global(emp_bd, fun="sum", na.rm=TRUE)
emp_bd_count # 16936
plot(emp_bd, main = "bulk density") # gandrīz visi gar malām

# cik platas ir malas bez vērtībām?
 aizpilditie <- ifel(!emp_bd, 1, NA)
platumi <- distance(aizpilditie)
plot(platumi, main = "Attālums līdz tuvākajai ne-NA vērtībai")
max_attalums <- global(platumi, fun = "max", na.rm = TRUE)
print(max_attalums) # lielākais platums ir 1300, tātad *2?
# Bet šie ir metri - filtrs prasa pikseļu skaitu, tātad 13 * 2

# tukšumu aizpildīšana
# tomēr vispirms jāuztaisa maska visam, kas nav Latvijas sauszemes teritorijā
# jo funkcija aizpildīs visas NA vērtības
maska_ref100m <- is.na(ref100m)
maska_ref100m <- ifel(maska_bd, 1000, NA) 
# maskai piešķirta augsta vērtība, kas noteikti nepārklāsies ar rastra vērtībām
masked_bd <- mosaic(bd010_2,maska_ref100m, fun = "sum")
plot(masked_bd)

bd_masked <- mask(bd010_2, ref100m, maskvalues=NA)
plot(bd_masked)

global(is.na(rast("BD010_100m_proj.tif")), fun = "sum", na.rm = FALSE)
writeRaster(bd_masked, "bd_masked.tif", overwrite=TRUE)
wbt_fill_missing_data(
  i = "BD010_100m_proj.tif",
  output = "bd_filled_w1.tif",
  filter = 26,     
  weight = 1,
  no_edges = FALSE
)

wbt_fill_missing_data(
  i = "BD010_100m_proj.tif",
  output = "bd_filled_w2.tif",
  filter = 26,     
  weight = 2,
  no_edges = FALSE
)

result_w1 <- rast("bd_filled_w1.tif")
plot(result_w1, main = "weight = 1")

result_w2 <- rast("bd_filled_w2.tif")
plot(result_w2, main = "weight = 2")
# vizuāli lielu atšķirību neredzu


bd_w1_filled <- mask(result_w1, ref100m)
plot(bd_w1_filled)

# Pārbaude
emp_bd <- is.na(bd_w1_filled) & !is.na(ref100m)
emp_bd_count <- global(emp_bd, fun="sum", na.rm=TRUE)
emp_bd_count # 0

