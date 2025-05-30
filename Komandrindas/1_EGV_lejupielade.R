# Pakotnes 

library(curl)
library(readxl)

# Pirms komandrindu izpildes manuāli jāaizpilda fails "EGVsaraksts.xlsx", izvēloties informatīvus
# ekoģeogrāfiskos faktorus. Faila aizpildīšanas piemērs atbilstoši šajā pētījumā izvēlētajām
# bišu modeļsugām atrodams failā "EGVsaraksts_bites.xlsx"

EGV <- read_excel("../Ievades_dati/EGV/EGVsaraksts_bites.xlsx")


# Apkopo, kuri EGV nepieciešami vismaz vienai sugai

EGV$lejupieladei <- apply(EGV[, 7:ncol(EGV)], 1, max, na.rm = TRUE) 
vajadzigieEGV <- EGV %>%
  filter(lejupieladei == 1)

# izveidoju direktoriju failu lejupielādei
dir.create("..Ievades_dati/EGV/EGV_faili")

# Nepieciešamo EGV failu lejupielāde

for (i in seq_len(nrow(vajadzigieEGV))) {
  url <- vajadzigieEGV$Lejupieladei[i]
  filename <- vajadzigieEGV$scale_NAME[i]
  dest <- file.path("../Ievades_dati/EGV/EGV_faili",filename)
  curl_download(url, destfile = dest)
}


