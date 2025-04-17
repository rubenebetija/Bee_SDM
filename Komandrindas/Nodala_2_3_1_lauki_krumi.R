# Pakotnes ----

if(!require(terra)) {install.packages(terra); require(terra)} 


# Faili ----
# aprakstu par failu iegūšanu un strukturēšanu?

## Aramzemes + kultivētie zālāji + papuves ----

# šūnas līmenis (100m)
aramz <- rast("../EGV_2024/rastri_100m/Proj/Lauku_AramVisas_cell.tif")
kultivz <- rast("../EGV_2024/rastri_100m/Proj/Lauku_ZalajiKultivetie_cell.tif")
pap <- rast ("../EGV_2024/rastri_100m/Proj/Lauku_Papuves_cell.tif")

# 500m rādiuss ap šūnu
aramz500 <- rast("../EGV_2024/rastri_100m/Proj/Lauku_AramVisas_r500.tif")
kultivz500 <- rast("../EGV_2024/rastri_100m/Proj/Lauku_ZalajiKultivetie_r500.tif")
pap500 <- rast ("../EGV_2024/rastri_100m/Proj/Lauku_Papuves_r500.tif")

# 1250m rādiuss ap šūnu
aramz1250 <- rast("../EGV_2024/rastri_100m/Proj/Lauku_AramVisas_r1250.tif")
kultivz1250 <- rast("../EGV_2024/rastri_100m/Proj/Lauku_ZalajiKultivetie_r1250.tif")
pap1250 <- rast ("../EGV_2024/rastri_100m/Proj/Lauku_Papuves_r1250.tif")

# 3000m rādiuss ap šūnu
aramz3000 <- rast("../EGV_2024/rastri_100m/Proj/Lauku_AramVisas_r3000.tif")
kultivz3000 <- rast("../EGV_2024/rastri_100m/Proj/Lauku_ZalajiKultivetie_r3000.tif")
pap3000 <- rast ("../EGV_2024/rastri_100m/Proj/Lauku_Papuves_r3000.tif")


## Izcirtumi, krūmāji un jaunaudzes ----

# 500m rādiuss ap šūnu
jkr500 <- rast("../EGV_2024/rastri_100m/Proj/Mezi_Jaunaudzes5mKrumaji_cell.tif")
izc500 <- rast("../EGV_2024/rastri_100m/Proj/Mezi_IzcUNzem5m_cell.tif")

# 1250m rādiuss ap šūnu
jkr1250 <- rast("../EGV_2024/rastri_100m/Proj/Mezi_Jaunaudzes5mKrumaji_r1250.tif")
izc1250 <- rast("../EGV_2024/rastri_100m/Proj/Mezi_IzcUNzem5m_r1250.tif")

# 3000m rādiuss ap šūnu
jkr3000 <- rast("../EGV_2024/rastri_100m/Proj/Mezi_Jaunaudzes5mKrumaji_r3000.tif")
izc3000 <- rast("../EGV_2024/rastri_100m/Proj/Mezi_IzcUNzem5m_r3000.tif")



# Slāņu izveide un saglabāšana ----

## Aramzemes + kultivētie zālāji + papuves ----

# šūnas līmenis (100m)
lauki1 <- mosaic(aramz, kultivz, pap, fun = "sum", filename = "gatavie/Lauku_AramzKultivzPapuves_cell.tif")

# 500m rādiuss ap šūnu
lauki2 <- mosaic(aramz500, kultivz500, pap500, fun = "sum", filename = "gatavie/Lauku_AramzKultivzPapuves_r500.tif")

# 1250m rādiuss ap šūnu
lauki3 <- mosaic(aramz1250, kultivz1250, pap1250, fun = "sum", filename = "gatavie/Lauku_AramzKultivzPapuves_r1250.tif")

# 3000m rādiuss ap šūnu
lauki4 <- mosaic(aramz3000, kultivz3000, pap3000, fun = "sum", filename = "gatavie/Lauku_AramzKultivzPapuves_r3000.tif")

rm(aramz,kultivz,pap,aramz500,kultivz500,pap500,aramz1250,kultivz1250,pap1250,
   aramz3000,kultivz3000,pap3000,lauki1,lauki2,lauki3,lauki4)


## Izcirtumi, krūmāji un jaunaudzes ----

# 500m rādiuss ap šūnu
mezi1 <- mosaic(izc500, jkr500, fun = "sum", filename = "gatavie/Mezi_IzcKrumiJaunaudzes_r500.tif")

# 1250m rādiuss ap šūnu
mezi2 <- mosaic(izc1250, jkr1250, fun = "sum", filename = "gatavie/Mezi_IzcKrumiJaunaudzes_r1250.tif")

# 3000m rādiuss ap šūnu
mezi3 <- mosaic(izc3000, jkr3000, fun = "sum", filename = "gatavie/Mezi_IzcKrumiJaunaudzes_r3000.tif")

rm(izc500,jkr500,izc1250,jkr1250,izc3000,jkr3000,mezi1,mezi2,mezi3)


