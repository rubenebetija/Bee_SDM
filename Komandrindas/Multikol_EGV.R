# pakotnes ----

if(!require(usdm)) {install.packages("usdm"); require(usdm)}
if(!require(tidyverse)) {install.packages("tidyverse"); require(tidyverse)}
if(!require(terra)) {install.packages("terra"); require(terra)}
if(!require(readxl)) {install.packages("readxl"); require(readxl)}
if(!require(doParallel)) {install.packages("doParallel"); require(doParallel)}
if(!require(foreach)) {install.packages("foreach"); require(foreach)}



# Ceļi uz EGV ----
faili=data.frame(fails=list.files(path="../EGV_2024/Rastri_100m/Scaled/",
                                  pattern=".tif$"),
                 cels=list.files(path="../EGV_2024/Rastri_100m/Scaled/",
                                 pattern=".tif$",full.names = TRUE))



# Sākotnēji izvēlētie EGV sugām ----
sakumam=read_excel("../../EGVsaraksts_Rubene_bites_tirits.xlsx")
sakumam_long=sakumam %>% 
  pivot_longer(cols = ANDCIN:OSMBIC,names_to="suga_kods",values_to="sakuma_izvele")

ne1=faili %>% 
  filter(!(fails %in% sakumam$scale_NAME))

# EGV Izvēle ----
sugas=levels(factor(sakumam_long$suga_kods))

cl <- makeCluster(2)
registerDoParallel(cl)


i <- 15

foreach(i = 1:length(sugas)) %dopar% {
  suppressPackageStartupMessages(library(tidyverse))
  suppressPackageStartupMessages(library(terra))
  suppressPackageStartupMessages(library(arrow))
  suppressPackageStartupMessages(library(usdm))
  suppressPackageStartupMessages(library(maps))
  suppressPackageStartupMessages(library(rasterVis))
  suppressPackageStartupMessages(library(readxl))
  suppressPackageStartupMessages(library(SDMtune))
  suppressPackageStartupMessages(library(ENMeval))
  suppressPackageStartupMessages(library(zeallot))
  suppressPackageStartupMessages(library(openxlsx))
  print(i)
  sakums=Sys.time()
  suga=sugas[i]
  print(suga)
  sakumsaraksts_sugai=sakumam_long %>% 
    filter(suga_kods==suga)
  isssakums_sugai=sakumsaraksts_sugai %>% 
    filter(sakuma_izvele > 0)
  faili_sugai=faili %>% 
    filter(fails %in% isssakums_sugai$scale_NAME)
  rastri_sugai=terra::rast(faili_sugai$cels)
  names(rastri_sugai)
  
  VifStep_sugai=usdm::vifstep(rastri_sugai,th=10,size=20000)
  izslegt=VifStep_sugai@excluded
  saglabat=VifStep_sugai@results
  
  saglabat2=saglabat %>% 
    mutate(faila_nosaukums=paste0(Variables,".tif"),
           sakumVIF=VIF) %>% 
    dplyr::select(faila_nosaukums,sakumVIF)
  sakums2=sakumsaraksts_sugai %>% 
    left_join(saglabat2,by=c("scale_NAME"="faila_nosaukums"))
  
  EGVtabulai=paste0("../../SuguModeli/EGVselection_visiEGV/EGV_",suga,".xlsx")
  write.xlsx(sakums2,EGVtabulai)
  beigas=Sys.time()
  ilgums=beigas-sakums
  print(ilgums)
}
stopCluster(cl)

