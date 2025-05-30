# Pakotnes ----

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

# Šis skripts nodrošina metožu apakšnodaļā 2.4.2. aprakstītās pieejas komandrindas
# gala modeļa (Mod4) parametrizācijai
# Parametrizēto modeļu sugām ANDHAT un DASHIR rezultāti aprakstīti apakšnodaļā 3.4.

# Skripts veidots vienai sugai - modelējamās sugas kods jānorāda zemāk:
suga <- ""




# Faili ----

testam2 <- read_parquet("../Rezultati/",suga,"/Dati/PresenceTest_filtered_",suga,".parquet")
apmacibai2 <- read_parquet("../Rezultati/",suga,"/Dati/PresenceTrain_filtered_",suga,".parquet")

fons_testam3 <- read_parquet("../Rezultati/",suga,"/Dati/BackgroundTest_spbias_",suga,".parquet")
fons_apmacibai3 <- read_parquet("../Rezultati/",suga,"/Dati/BackgroundTrain_spbias_",suga,".parquet")




# EGV ----

egv_faili=data.frame(egv_fails=list.files(path="../Ievades_dati/EGV/EGV_faili/",
                                          pattern=".tif$"),
                     egv_cels=list.files(path="../Ievades_dati/EGV/EGV_faili/",
                                         pattern=".tif$",full.names = TRUE))

egv_izvelei=read_excel("../Rezultati/",suga,"/EGV_izvele/EGV_",suga,".xlsx")

egv_izvelei2=egv_izvelei %>% 
  filter(!is.na(sakumVIF))

egv_faili=egv_faili %>% 
  right_join(egv_izvelei2,by=c("egv_fails"="scale_NAME"))

egvs=terra::rast(egv_faili$egv_cels)
egv_izvelei2$id <- paste0("EGV_", 1:nrow(egv_izvelei2))
names(egvs)=egv_izvelei2$id


# Izvēle par mazsvarīgo EGV izslēgšanu no parametrizācijas
# Saglabāti tie EGV, kuru nozīmība 3. un 4. modelī (vismaz vienā no tiem) ir >1

a1 <- read_excel("../Rezultati/",suga,"/Modeli/Modelis1_visidati_VarImp_",suga,".xlsx")
a1 <- a1 %>%
  select(Variable,Permutation_importance)
colnames(a1) <- c("Variable", "Mod1")

a2 <- read_excel("../Rezultati/",suga,"/Modeli/Modelis2_filter_VarImp_",suga,".xlsx")
a2 <- a2 %>%
  select(Variable,Permutation_importance)
colnames(a2) <- c("Variable", "Mod2")

a3 <- read_excel("../Rezultati/",suga,"/Modeli/Modelis3_filterbeebias_VarImp_",suga,".xlsx")
a3 <- a3 %>%
  select(Variable,Permutation_importance)
colnames(a3) <- c("Variable", "Mod3")

a4 <- read_excel("../Rezultati/",suga,"/Modeli/Modelis4_filterspbias_VarImp_",suga,".xlsx")
a4 <- a4 %>%
  select(Variable,Permutation_importance)
colnames(a4) <- c("Variable", "Mod4")

by <- join_by(Variable)

nozime <- left_join(a1,a2,by)
nozime <- left_join(nozime,a3,by)
nozime <- left_join(nozime,a4,by)

nozime_filter2 <- nozime %>%
  filter(Mod3 >= 1 & Mod4 >= 1)

# atbrīvojos no mazsvarīgajiem EGV
imp_egvs <- ANDHAT_filter2[[1]] 
egvs_par <- egvs[[imp_egvs]]
names(egvs_par)


# Modelēšana ----

# apmācību dati
trenin_dati5 <- prepareSWD(species = suga,
                           p = apmacibai2[,2:3],
                           a = fons_apmacibai3[,1:2],
                           env = egvs_par)
trenin_dati5 <- addSamplesToBg(trenin_dati5)

block_folds5 <- get.block(occ = trenin_dati5@coords[trenin_dati5@pa == 1, ], 
                          bg = trenin_dati5@coords[trenin_dati5@pa == 0, ])

# testa dati
testa_dati5 <- prepareSWD(species = suga,
                       p = testam2[,2:3],
                       a = fons_testam3[1:2],
                       env = egvs_par)


sakummodelis <- train(method = "Maxnet", 
                      data = trenin_dati5,
                      test=testa_dati5,
                      fc="lqph",
                      folds = block_folds5)

summary(sakummodelis)

# norādu regularizācijas multiplikatoru un
fc_lqph <- list(reg = c(0.2, 1/3, 0.5, 0.75, 1, 1.25, 2, 3, 5, 7.5, 10), 
                fc = c("l","lq", "lp", "lqp", "qp","lh","qh","lqh","lhp","qhp","lqhp"))
# kļūdas paziņojums pēc gridSearch - izmantošu fc_lqph2

fc_lqph2 <- list(reg = c(0.2, 1/3, 0.5, 0.75, 1, 1.25, 2, 3, 5, 7.5, 10), 
                 fc = c("l","lq", "lp", "lqp", "qp","q"))

# modeļu apmācība un izvēle ar gridSearch
izveles_rezgis=gridSearch(sakummodelis, 
                          hypers = fc_lqph2, 
                          metric = "tss")

# neatkarīgie testi - AUC un TSS aprēķināšana
izveles_tabula=izveles_rezgis@results
izveles_tabula$IndepTest_auc=NA_real_
izveles_tabula$IndepTest_tss=NA_real_

darbiba_auc=function(numurs){
  rez=try(auc(combineCV(izveles_rezgis@models[[numurs]]),test=testa_dati5))
  if (inherits(rez, 'try-error')) {
    return(NA)
  }
  return(rez)
}
darbiba_tss=function(numurs){
  rez=try(tss(combineCV(izveles_rezgis@models[[numurs]]),test=testa_dati5))
  if (inherits(rez, 'try-error')) {
    return(NA)
  }
  return(rez)
}

for(i in 1:nrow(izveles_tabula)){
  print(i)
  fin_auc=darbiba_auc(i)
  izveles_tabula$IndepTest_auc[i]=fin_auc
  fin_tss=darbiba_tss(i)
  izveles_tabula$IndepTest_tss[i]=fin_tss
  #print(paste0("independent AUC: ",fin_auc))
  #print(paste0("independent TSS: ",fin_tss))
}
names(izveles_tabula)=c("fc","reg","train_TSS","validation_TSS","diff_TSS","IndepTest_auc","IndepTest_tss")

write_rds(izveles_rezgis,paste0("../Rezultati/",suga,"/Modeli/Modelis5_GGSModeli_",suga,".RDS"))
write.xlsx(izveles_tabula,paste0("../Rezultati/",suga,"/Modeli/Modelis5_GSTabula_",suga,".xlsx"))

#izveles_rezgis <- readRDS("../Rezultati/",suga,"/Modeli/Modelis5_GGSModeli_",suga,".RDS")
#izveles_tabula <- read_excel("../Rezultati/",suga,"/Modeli/Modelis5_GSTabula_",suga,".xlsx")

labakajam_modelim=izveles_tabula %>% 
  mutate(rinda=as.numeric(rownames(.))) %>% 
  filter(IndepTest_tss==max(IndepTest_tss,na.rm=TRUE)) %>% 
  filter(diff_TSS==min(diff_TSS,na.rm=TRUE)) %>% 
  filter(validation_TSS==max(validation_TSS,na.rm=TRUE)) %>% 
  filter(nchar(fc)==min(nchar(fc)))
labaka_numurs=labakajam_modelim$rinda

# krosvalidētais
labakais_CV=izveles_rezgis@models[[labaka_numurs]]
write_rds(labakais_CV,paste0("../Rezultati/",suga,"/Modeli/Modelis5_CV_",suga,".RDS"))
#labakais_CV <- readRDS("../Rezultati/",suga,"/Modeli/Modelis5_CV_",suga,".RDS")

# kombinētais
labakais_comb=combineCV(izveles_rezgis@models[[labaka_numurs]])
write_rds(labakais_comb,paste0("../Rezultati/",suga,"/Modeli/Modelis5_Comb_",suga,".RDS"))
#labakais_comb <- readRDS("../Rezultati/",suga,"/Modeli/Modelis5_Comb_",suga,".RDS")

# projekcija
tks_rezgis=sfarrow::st_read_parquet("../Ievades_dati/References/tks93_50km.parquet")
dir.create("../Rezultati/",suga,"/Modelis5_projs/")
lapas=levels(factor(tks_rezgis$NUMURS))
for(i in seq_along(lapas)){
  print(i)
  sakums=Sys.time()
  lapai=lapas[i]
  lapa=tks_rezgis %>%
    filter(NUMURS == lapai)
  egv_mazs=terra::crop(egvs_par,lapa)
  #names(egv_mazs)
  projekcijai=predict(labakais_comb,
                      data = egv_mazs,
                      type = "cloglog",
                      file = paste0("../Rezultati/",suga,"/Modelis5_projs/proj_",lapai,".tif"),
                      overwrite=TRUE)
  #plot(projekcijai)
  beigas=Sys.time()
  ilgums=beigas-sakums
  print(ilgums)
}
slani=list.files("../Rezultati/",suga,"/Modelis5_projs/",full.names=TRUE)
virt_slani=terra::vrt(slani)
writeRaster(virt_slani,"../Rezultati/",suga,"/Modeli/Modelis5_HSmap_",suga,".tif", overwrite = TRUE)
projekcija=rast("../Rezultati/",suga,"/Modeli/Modelis5_HSmap_",suga,".tif")
plot(projekcija)

# thresholds
ths <- SDMtune::thresholds(labakais_comb, 
                           type = "cloglog",
                           test=testa_dati5)
ths$suga = suga
write.xlsx(ths,paste0("../Rezultati/",suga,"/Modeli/Modelis5_Ths_",suga,".xlsx"))


# pROC
labakais_proc=SDMtune::plotROC(labakais_comb,test = testa_dati5)
labakais_proc
write_rds(labakais_proc,paste0("../Rezultati/",suga,"/Modeli/Modelis5_ROC_",suga,".RDS"))
labakais_proc <- readRDS("../Rezultati/",suga,"/Modeli/Modelis5_ROC_",suga,".RDS")

# variable importance
vi_tss <- varImp(labakais_CV,permut = 99)
write.xlsx(vi_tss,paste0("../Rezultati/",suga,"/Modeli/Modelis5_VarImp_",suga,".xlsx"))
#vi_tss <- read_excel("../Rezultati/",suga,"/Modeli/Modelis5_VarImp_",suga,".xlsx")

# marginal responses
modelis_CV=labakais_CV
mainigo_tabula=vi_tss
mainigie=mainigo_tabula
#augstums=ceiling(length(mainigie$Variable)/6)*30

a=plotResponse(modelis_CV, 
               var = mainigie$Variable[1], 
               type = "cloglog",
               only_presence = TRUE,
               marginal = TRUE, 
               fun=median,
               rug = TRUE,
               col="black")
b=ggplot2::ggplot_build(a)

saistibas_funkcija=b$plot$data
saistibas_funkcija$nosaukums=mainigie$Variable[1]
saistibas_funkcija$egv=mainigie$Variable[1]
vietas_presence=b$data[[3]]
vietas_presence$y=1
vietas_presence$nosaukums=mainigie$Variable[1]
vietas_presence$egv=mainigie$Variable[1]
vietas_absence=b$data[[4]]
vietas_absence$y=-0.03
vietas_absence$nosaukums=mainigie$Variable[1]
vietas_absence$egv=mainigie$Variable[1]

for(i in 2:length(mainigie$Variable)){
  a=plotResponse(modelis_CV, 
                 var = mainigie$Variable[i], 
                 type = "cloglog",
                 only_presence = TRUE,
                 marginal = TRUE, 
                 rug = TRUE,
                 fun=median,
                 col="black")
  b=ggplot2::ggplot_build(a)
  
  saistibas_funkcija_i=b$plot$data
  saistibas_funkcija_i$nosaukums=mainigie$Variable[i]
  saistibas_funkcija_i$egv=mainigie$Variable[i]
  vietas_presence_i=b$data[[3]]
  vietas_presence_i$y=1
  vietas_presence_i$nosaukums=mainigie$Variable[i]
  vietas_presence_i$egv=mainigie$Variable[i]
  vietas_absence_i=b$data[[4]]
  vietas_absence_i$y=-0.03
  vietas_absence_i$nosaukums=mainigie$Variable[i]
  vietas_absence_i$egv=mainigie$Variable[i]
  
  saistibas_funkcija=bind_rows(saistibas_funkcija,saistibas_funkcija_i)
  vietas_presence=bind_rows(vietas_presence,vietas_presence_i)
  vietas_absence=bind_rows(vietas_absence,vietas_absence_i)
}

# pārsaukšana - grafikiem
egv_nosaukumi <- egv_izvelei2 %>%
  select(Nosaukums,sakums,id)
saistibas_funkcija <- left_join(saistibas_funkcija, egv_nosaukumi, by = c("egv"="id"))
vietas_presence <- left_join(vietas_presence, egv_nosaukumi, by = c("egv"="id"))
vietas_absence <- left_join(vietas_absence, egv_nosaukumi, by = c("egv"="id"))
#rm(saistibas_funkcija,vietas_presence,vietas_absence)

################# plot
attels=ggplot(saistibas_funkcija)+
  geom_ribbon(data=saistibas_funkcija,aes(x=x,y=y,ymin=y_min,ymax=y_max),alpha=0.5)+
  geom_line(data=saistibas_funkcija,aes(x=x,y=y))+
  facet_wrap(~Nosaukums,scales = "free_x",ncol=3,
             labeller = label_wrap_gen(width=25,multi_line = TRUE))+
  geom_point(data=vietas_presence,aes(x=x,y=y),size=0.5,alpha=0.5)+
  geom_point(data=vietas_absence,aes(x=x,y=y),size=0.5,alpha=0.5)+
  coord_cartesian(ylim=c(0,1))+
  scale_y_continuous(breaks=seq(0,1,0.25))+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        strip.text = element_text(size = 8))+
  labs(y="Marginālo atbilžu funkcijas (cloglog)")
ggsave(attels,
       filename=paste0("../Rezultati/",suga,"/Modeli/Modelis5_MargResp_",suga,".png"),
       width=160,height=500,units="mm",dpi=120)


# nulles modelis ----

ref_fc=toupper(labakais_comb@model@fc)
ref_rm=labakais_comb@model@reg

occs=trenin_dati5@coords[trenin_dati5@pa==1,]
bg=trenin_dati5@coords[trenin_dati5@pa==0,]



tune.args <- list(fc = ref_fc, rm = ref_rm)
nulles_reference <- ENMevaluate(occs = occs, envs = egvs_par, bg = bg,
                                algorithm = 'maxnet', partitions = 'block', 
                                tune.args = tune.args)
write_rds(nulles_reference,paste0("../Rezultati/",suga,"/Modeli/Modelis5_NullRef_",suga,".RDS"))


# Nulles salidzinajumi

nulles_modeli <- ENMnulls(nulles_reference, mod.settings = list(fc = ref_fc, rm = ref_rm), no.iter = 100)
write_rds(nulles_modeli,paste0("../Rezultati/",suga,"/Modeli/Modelis5_NullModels_",suga,".RDS"))


# modeļa raksturojumi ----

# novērtējums

izveles_tabula_long=izveles_tabula %>% 
  pivot_longer(cols=train_TSS:IndepTest_tss,
               names_to = "veids",
               values_to = "vertibas") %>% 
  mutate(veids_long=case_when(veids=="train_TSS"~"Apmācību\nTSS",
                              veids=="validation_TSS"~"Validācijas\nTSS",
                              veids=="diff_TSS"~"Apmācību un\nvalidācijas\nTSS starpība",
                              veids=="IndepTest_auc"~"Neatkarīgā\ntesta\nAUC",
                              veids=="IndepTest_tss"~"Neatkarīgā\ntesta\nTSS")) %>% 
  mutate(sec_veids=case_when(veids=="train_TSS"~4,
                             veids=="validation_TSS"~3,
                             veids=="diff_TSS"~2,
                             veids=="IndepTest_auc"~5,
                             veids=="IndepTest_tss"~1))



izveles_labakais=izveles_tabula %>% 
  mutate(rinda=as.numeric(rownames(.))) %>% 
  filter(IndepTest_tss==max(IndepTest_tss,na.rm=TRUE)) %>% 
  filter(diff_TSS==min(diff_TSS,na.rm=TRUE)) %>% 
  filter(validation_TSS==max(validation_TSS,na.rm=TRUE)) %>% 
  filter(nchar(fc)==min(nchar(fc)))

izveles_labakais_long=izveles_labakais %>% 
  pivot_longer(cols=train_TSS:IndepTest_tss,
               names_to = "veids",
               values_to = "vertibas") %>% 
  mutate(veids_long=case_when(veids=="train_TSS"~"Apmācību\nTSS",
                              veids=="validation_TSS"~"Validācijas\nTSS",
                              veids=="diff_TSS"~"Apmācību un\nvalidācijas\nTSS starpība",
                              veids=="IndepTest_auc"~"Neatkarīgā\ntesta\nAUC",
                              veids=="IndepTest_tss"~"Neatkarīgā\ntesta\nTSS")) %>% 
  mutate(sec_veids=case_when(veids=="train_TSS"~4,
                             veids=="validation_TSS"~3,
                             veids=="diff_TSS"~2,
                             veids=="IndepTest_auc"~5,
                             veids=="IndepTest_tss"~1))


# roci
rociem=labakais_proc


nullem=nulles_modeli
nullu_tabula_apkopots=nullem@null.emp.results
nullu_tabula_nulles=nullem@null.results

nullu_tabulai=evalplot.nulls(nullem, 
                             stats = c("auc.val","cbi.val","or.mtp","or.10p"), 
                             plot.type = "violin",
                             return.tbl = TRUE)
nulles=nullu_tabulai$null.avgs %>% 
  mutate(nosaukumi=case_when(metric=="auc.val"~"AUC\n(validācijas)",
                             metric=="cbi.val"~"Continuous\nBoyce index\n(validācijas)",
                             metric=="or.10p"~"10% training\nomission rate",
                             metric=="or.mtp"~"Minimum\ntraining presence\nomission rate")) %>% 
  mutate(secibai=case_when(metric=="auc.val"~1,
                           metric=="cbi.val"~2,
                           metric=="or.10p"~3,
                           metric=="or.mtp"~4))
empiriskie=nullu_tabulai$empirical.results %>% 
  mutate(nosaukumi=case_when(metric=="auc.val"~"AUC\n(validācijas)",
                             metric=="cbi.val"~"Continuous\nBoyce index\n(validācijas)",
                             metric=="or.10p"~"10% training\nomission rate",
                             metric=="or.mtp"~"Minimum\ntraining presence\nomission rate")) %>% 
  mutate(secibai=case_when(metric=="auc.val"~1,
                           metric=="cbi.val"~2,
                           metric=="or.10p"~3,
                           metric=="or.mtp"~4))

kreisais=ggplot()+
  geom_violin(data=izveles_tabula_long,
              aes(reorder(veids_long,sec_veids),vertibas),
              col="grey",
              fill="grey",
              alpha=0.2)+
  geom_jitter(data=izveles_tabula_long,
              aes(reorder(veids_long,sec_veids),vertibas),
              col="grey",
              width=0.25)+
  geom_point(data=izveles_labakais_long,
             aes(reorder(veids_long,sec_veids),vertibas),
             col="black",
             size=3)+
  theme_classic()+
  coord_cartesian(ylim=c(0,1))+
  scale_y_continuous(breaks=seq(0,1,by=0.1))+
  labs(x="Metrika",
       y="Vērtība",
       subtitle="Modeļa izvēle")+
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=11),
        plot.subtitle = element_text(size=14))

vidus=rociem+
  theme_bw()+
  labs(subtitle="ROC līkne")+
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=11),
        plot.subtitle = element_text(size=14),
        legend.title = element_text(size=14),
        legend.text = element_text(size=11),
        legend.position="inside",
        legend.position.inside = c(0.8,0.15))

labais=ggplot()+
  geom_violin(data=nulles,
              aes(reorder(nosaukumi,secibai),avg),
              col="grey",
              fill="grey",
              alpha=0.2)+
  geom_jitter(data=nulles,
              aes(reorder(nosaukumi,secibai),avg),
              col="grey",
              width=0.2,
              shape=3)+
  geom_point(data=empiriskie,
             aes(reorder(nosaukumi,secibai),avg),
             col="black",
             size=3)+
  coord_cartesian(ylim=c(0,1))+
  theme_classic()+
  scale_y_continuous(breaks=seq(0,1,0.1))+
  labs(x="Metrika",y="Vērtība",subtitle="Izvēlētā modeļa salīdzinājums ar nejaušību")+
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=11),
        plot.subtitle = element_text(size=14))

izveles_attelam=kreisais+vidus+labais+plot_annotation(tag_levels="A")+
  ggview::canvas(width=1750,height=500,units="px",dpi=100)
izveles_attels=kreisais+vidus+labais+plot_annotation(tag_levels="A")
ggsave(izveles_attels,filename=paste0("../Rezultati/ANDHAT/Modeli/Modelis5_IzvelesAttels_ANDHAT.png"),
       width=1750,height=500,units="px",dpi=100)

# karte - pārcentrēta pēc thresholds

slieksni=read_excel(paste0("../Rezultati/",suga,"/Modeli/Modelis5_Ths_",suga,".xlsx"))
slieksnis=slieksni[2,2]
slieksnis_vert=as.numeric(slieksnis)
apaksdala=mean(c(0,slieksnis_vert))
augsdala=mean(c(1,slieksnis_vert))


slanis=terra::rast(paste0("../Rezultati/",suga,"/Modeli/Modelis5_HSmap_",suga,"_vrt.tif"))

slanis_df=terra::as.data.frame(slanis,xy=TRUE)
colnames(slanis_df) <- c("x","y","lyr1")

slanis_df_augstie=slanis_df[slanis_df$lyr1>=slieksnis_vert,]
slanis_df_zemie=slanis_df[slanis_df$lyr1<slieksnis_vert,]

krasas <- c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c")
parejas <- c(0, apaksdala, slieksnis_vert, augsdala, 1)


karte=ggplot(slanis_df,aes(x=x,y=y,fill=lyr1))+
  geom_raster() +
  coord_fixed(ratio=1)+
  scale_fill_gradientn("",
                       colors = krasas,
                       values = parejas,
                       breaks=c(0,round(slieksnis_vert,3),1),
                       limits=c(0,1))+
  ggthemes::theme_map()+
  theme(legend.position = "inside",
        legend.position.inside=c(0,0.6),
        plot.background = element_rect(fill="white",color="white"))
plot(karte)
ggsave(karte,
       filename=paste0("../Rezultati/",suga,"/Modeli/Modelis5_projekcijakarte_",suga,".png"),
       width=200,height=120,units="mm",dpi=120)