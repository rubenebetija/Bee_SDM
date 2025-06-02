# Ievades dati

Šī darba plūsma sagatavota tā, lai tā būtu izpildāma visām izvēlētajām sugām, neatkarīgi no to skaita,
tomēr tā pieņem konkrēt datu struktūru, kas atbilst pētījumā izmantoto modeļsugu datu struktūrai.

Katrai sugai nepieciešams piešķirt unikālu kodu. Pētījumā izmantotajām modeļsugām tas veidots no trīs
pirmajiem ģints un trīs pirmajiem sugas epiteta burtiem:

- ANDHAT - pētereņu smilšbite *Andrena hattorfiana*
- DASHIR - bikšainā grumbuļbite *Dasypoda hirtipes*


Vispirms nepieciešams izveidot šādas apakšdirektorijas direktorijā "Ievades_dati":

- Ainava, kurā jāievieto zemāk nosauktie faili;

- References, kurā jāievieto zemāk nosauktie faili;

- EGV, kurā tiks ievietoti ekoģeogrāfiskie mainīgie (EGV) un to starprezultāti, kā arī
jāaizpilda EGV izvēles fails atbilstoši katras izvēlētās sugas ekoloģijai;

- Noverojumi, kurā jāievieto sugu novērojumu fails un sugām raksturīgās dzīves telpas rādiusu fails.


# Pirms darba uzsākšanas nepieciešams iegūt šādus failus:

Direktorijā "Ainava"

1) vides pārmaiņas - fails VidesParmainas_visas.parquet  
Diemžēl nav iespējams piedāvāt šo failu tiešai lietošanai, tomēr tā izveides komandrindas pieejamas [šī materiāla 3.2.3. apakšnodaļā](https://aavotins.github.io/PutnuSDMs_gramata/Chapter3.html#Chapter3.3)

Direktorijā "References":

1) nulles_LV100m_10km.tif
failu iespējams lejupielādēt [šeit](https://github.com/aavotins/ACCGEN_LVM/tree/main/Rastri_100m)

2) LV10m_10km.tif
failu iespējams lejupielādēt šeit [šeit](https://zenodo.org/records/14497070)

3) tikls100_sauzeme.parquet  
failu iespējams lejupielādēt [šeit](https://zenodo.org/records/14277114)


# Pirms darba uzsākšanas nepieciešams atbilstoši aizpildīt šādus failus:

1) EGV / EGV_saraksts.xlsx, kurā jāveic katras sugas modelēšanai nepieciešamo EGV izvēle. 
Šī izvēle ir jāveic, balstoties uz pieejamo literatūru un/vai sugu ekspertu viedokļiem.  
Failā EGV_saraksts_bites.xlsx dots piemērs par saraksta aizpildīšanu šī pētījuma modeļsugām.  
Ar pelēku atzīmēti tie trīs mainīgie, kuru iegūšanu šī darbplūsma nepiedāvā, jo tās izveidei izmantotie
slāņi nav publiski pieejami.

2) Noverojumi / Noverojumi.xlsx, kurā jāsagatavo sugu novērojumi. Šajā pašā failā pieejami arī aizpildāmo aiļu skaidrojumi. 
Sagatavotajā failā, ja nepieciešams, drīkst būt papildu kolonnas.

3) Noverojumi / Radiusi.xlsx, kurā jāsagatavo informācija par izvēlētajām sugām raksturīgo dzīves telpas rādiusu, balstoties 
uz pieejamo literatūru par sugas tipisko apdzīvoto telpu. Izvēle jāveic, ņemot vērā pieejamo EGV rādiusus - 
500m, 1250m, 3000m un 10000m. No šiem variantiem jāizvēlas sugai atbilstošākais. 
Failā Radiusi_bites.xlsx dots piemērs par novērojumu faila aizpildīšanu šī pētījuma modeļsugām