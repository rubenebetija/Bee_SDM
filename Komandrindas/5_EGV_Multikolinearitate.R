# Pakotnes ----

library(usdm)
library(tidyverse)
library(terra)
library(readxl)
library(doParralel)
library(foreach)



# Ceļi uz EGV ----

faili=data.frame(fails=list.files(path="../Ievades_dati/EGV/EGV_faili/",
                                  pattern=".tif$"),
                 cels=list.files(path="../Ievades_dati/EGV/EGV_faili/",
                                 pattern=".tif$",full.names = TRUE))


# Sākotnēji izvēlētie EGV sugām ----
sakumam=read_excel("../Ievades_dati/EGV/EGVsaraksts_bites.xlsx")

sakumam_long=sakumam %>% 
  pivot_longer(cols = 7:ncol(sakumam),names_to="suga_kods",values_to="sakuma_izvele")


########## Šis skripts veidots ar pieeju vienai sugai - tās kods jānorāda zemāk:
suga <- "ANDHAT"


# EGV Izvēle ----

sakumsaraksts_sugai=sakumam_long %>% 
  filter(suga_kods==suga)
isssakums_sugai=sakumsaraksts_sugai %>% 
  filter(sakuma_izvele > 0)
faili_sugai=faili %>% 
  filter(fails %in% isssakums_sugai$scale_NAME)
rastri_sugai=terra::rast(faili_sugai$cels)
names(rastri_sugai)

set.seed(1)
VifStep_sugai=usdm::vifstep(rastri_sugai,th=10,size=20000)
izslegt=VifStep_sugai@excluded
saglabat=VifStep_sugai@results
print(saglabat)
saglabat2=saglabat %>% 
  mutate(faila_nosaukums=paste0(Variables,".tif"),
         sakumVIF=VIF) %>% 
  dplyr::select(faila_nosaukums,sakumVIF)
sakums2=sakumsaraksts_sugai %>% 
  left_join(saglabat2,by=c("scale_NAME"="faila_nosaukums"))

# EGV izvēles saglabāšana
EGVtabulai=paste0("../Rezultati/",suga,"/EGV_izvele/EGV_",suga,".xlsx")
write.xlsx(sakums2,EGVtabulai)


