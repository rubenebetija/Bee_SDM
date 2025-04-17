# pakotnes ----
if(!require(terra)) {install.packages(terra); require(terra)} 
if(!require(tidyverse)) {install.packages(tidyverse); require(tidyverse)} 
if(!require(readxl)) {install.packages(readxl); require(readxl)} 
if(!require(openxlsx)) {install.packages(openxlsx); require(openxlsx)} 


# 

definicija=rast("../ref_rastri/nulles_LV100m_10km.tif")
failu_celi=list.files("./gatavie/",full.names = TRUE,pattern=".tif$")
nosaukumiem=list.files("./gatavie/",full.names = FALSE,pattern=".tif$")

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

for(i in seq_along(nosaukumiem$nosaukumi)){
  print(i)
  sakums=Sys.time()
  fails=rast(nosaukumiem$faili[i])
  fails=project(fails,definicija)
  maskets=mask(fails,definicija)
  names(maskets)=substr(nosaukumiem$nosaukumi[i],1,nchar(nosaukumiem$nosaukumi[i])-4)
  writeRaster(maskets,
              filename=paste0("./gatavie_proj/",nosaukumiem$nosaukumi[i]),
              overwrite=TRUE)
  videjais=global(maskets,fun="mean",na.rm=TRUE)
  centrets=maskets-videjais[,1]
  standartnovirze=terra::global(centrets,fun="rms",na.rm=TRUE)
  merogots=centrets/standartnovirze[,1]
  nosaukumiem$var_videjais[i]=videjais
  nosaukumiem$var_standartnovirze[i]=standartnovirze
  writeRaster(merogots,
              filename=paste0("./gatavie_scaled/",nosaukumiem$nosaukumi[i]),
              overwrite=TRUE)
  beigas=Sys.time()
  ilgums=beigas-sakums
  print(ilgums)
}
write.xlsx(nosaukumiem,"./gatavie_scaled/EGV_names_scaling.xlsx")
