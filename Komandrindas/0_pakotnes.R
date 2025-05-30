# Sekojošās komandrindas pārbauda, vai ir instalētas darbam nepieciešamās
# pakotnes un instalē trūkstošās


if(!require(arrow)) {install.packages("arrow"); require(arrow)}
if(!require(curl)) {install.packages("curl"); require(curl)}
if(!require(doParallel)) {install.packages("doParallel"); require(doParallel)}
if(!require(ENMeval)) {install.packages("ENMeval"); require(ENMeval)}
if(!require(ecospat)) {install.packages("ecospat"); require(ecospat)}
if(!require(foreach)) {install.packages("foreach"); require(foreach)}
if(!require(ggplot2)) {install.packages("ggplot2"); require(ggplot2)}
if(!require(maps)) {install.packages("maps"); require(maps)}
if(!require(MASS)) {install.packages("MASS"); require(MASS)}
if(!require(maxnet)) {install.packages("maxnet"); require(maxnet)}
if(!require(openxlsx)) {install.packages(openxlsx); require(openxlsx)}
if(!require(patchwork)) {install.packages(patchwork); require(patchwork)} 
if(!require(plotROC)) {install.packages("plotROC"); require(plotROC)}
if(!require(raster)) {install.packages("raster"); require(raster)}
if(!require(rasterVis)) {install.packages("rasterVis"); require(rasterVis)}
if(!require(readxl)) {install.packages("readxl"); require(readxl)}
if(!require(SDMtune)) {install.packages("SDMtune"); require(SDMtune)}
if(!require(sf)) {install.packages("sf"); require(sf)}
if(!require(sfarrow)) {install.packages("sfarrow"); require(sfarrow)}
if(!require(st)) {install.packages("st"); require(st)}
if(!require(stringr)) {install.packages("stringr"); require(stringr)}
if(!require(terra)) {install.packages("terra"); require(terra)}
if(!require(tidyverse)) {install.packages("tidyverse"); require(tidyverse)}
if(!require(usdm)) {install.packages("usdm"); require(usdm)}
if(!require(whitebox)) {install.packages(whitebox); require(whitebox)} 
if(!require(zeallot)) {install.packages(zeallot); require(zeallot)} 
