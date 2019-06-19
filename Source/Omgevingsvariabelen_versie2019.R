########################
#### Bestandsnamen invoergegevens
########################

#---------------------
# Gegevens grasland moeras
#---------------------

dbINBOVeg_2018 <- "../Data/MeetnetgegevensMoerasGrasland/INBOVeg_Export_2018-11-12/"
voortgangDir <- "../Data/MeetnetgegevensMoerasGrasland/Voortgang/."
voortgangFile <- "Stavaza2018"
dataMicrorelief <-"../Data/MeetnetgegevensMoerasGrasland/1330_mircorelief_versie2018-08-20.csv"

#---------------------
# Gegevens heide en 6510
#---------------------

voortgangHeideEn6510 <- "../Data/MeetgegevensHeide6510/VoortgangMeetnetHeide6510_Versie2016-12-21.csv"
dbHeideEn6510_2018 <- "../Data/MeetgegevensHeide6510/FieldMapData_Heide6510_versie2018-06-11.accdb"

#---------------------
# Gegevens MONEOS
#---------------------

dbINBOVeg_MONEOS <- "../Data/MeetgegevensMONEOS/INBOVeg_Export_2018-11-06/"
structuur91E0_sf <- "../Data/MeetgegevensMONEOS/Structuurgegevens_91E0_sf.csv"
structuur1330_da <- "../Data/MeetgegevensMONEOS/Structuurgegevens_1330_da.csv"
coordinatenPQ91E0_sf <- "../Data/MeetgegevensMONEOS/PQsMoneos_91E0_sf_Coord.csv"

#---------------------
# Gegevens bos MHK
#---------------------

dbBosExtra <- "../Data/MeetgegevensBoshabitats/FieldMapData_Boshab_versi2018-06-14.accdb"
shapePlotsDir <- "../Data/MeetgegevensBoshabitats/Meetproces/."
shapeA3 <- "A3_buffer9mGrid_points_ADM"
shapeA2 <- "Regenerationplot_polyg_ADM"
shapeVegPlot <- "Vegetation_polyg_ADM"
treeSegmentsFile <- "../Data/MeetgegevensBoshabitats/Meetproces/TreeSegments.csv"

bosGISdir <-"../Data/MeetgegevensBoshabitats/GISdata/."
MSA <- "Bos_clusters"
bosleeftijd <- "Blftd"

#---------------------
# Gegevens bos VBI
#---------------------

dbVBI2 <- "../Data/MeetgegevensVBI2/Versie3/FieldMapDataVal.accdb"
dbVBI1 <- "../Data/MeetgegevensVBI1/bosinv1_2011Val.accdb"
dbVBI1_veg <- "../Data/MeetgegevensVBI1/Bosveg4Val.mdb"
dbVBIMeetproces<-"../Data/MeetgegevensVBI2/Versie3/VBI_Meetproces_v2016-08-31.accdb"
dbVBIAnalyse <- "../Data/MeetgegevensVBI2/Versie3/VBI_AnalyseDatabank_v2016-08-31.accdb"
dbVBIExterneData<-"../Data/ExterneData/VBIExterneData.accdb"
dbVBIStrata<-"../Data/MeetgegevensVBI2/Versie3/VBI_Strata_v2016-08-31.mdb"

#---------------
# gegevens PINK
#------------------

data2110 <- "../Data/MeetgegevensPINK/Polygoon_2110_v2.csv"
dbINBOVeg_PINK <- "../Data/MeetgegevensPINK/INBOVeg_Export_2018-11-26/"
PINKdir <- "../Data/MeetgegevensPINK/."
PINK_PQ_shape <- "PQ_Duinen_20180420"
verbossing_2160 <- "../data/MeetgegevensPINK/Verbossing_2160.csv"

#--------------
# gegevens meren
#-----------------

dataMeren <- "../Data/MeetgegevensWater/Data_Meren.xlsx"
dataMerenEGV <- "../data/meetgegevenswater/Data_Meren_EGV.xlsx"
overzichtMeetpuntenMeren <- "../data/meetgegevenswater/overzicht_meetpunten_31xx.csv"
oppervlakteStrataMeren <- "../data/MeetgegevensWater/OppStrata31xx.csv"

#--------------
# gegevens rivieren
#-----------------

dataRivieren <- "../data/meetgegevenswater/Data_MFDB3260.xlsx"
variabelenRivieren <- "../data/meetgegevenswater/LijstVariabelen3260.xlsx"
overzichtMeetpuntenRivieren <- "../data/meetgegevenswater/overzicht_meetpunten_3260.csv"
lengteStrataRivieren <- "../data/MeetgegevensWater/LengteStrata3260.csv"
dataRivierentrend <- "../data/meetgegevenswater/Data_Waterlopen_trendLSVI_VMM.xlsx"


#---------------------
#Externe data
#---------------------

schaalInfo  <- "../LSVIData/Schalen.csv"
indicatorenLSVI_fn <- "../LSVIData/Indicatoren_LSVI.csv"
# soortengroepenLSVI_fn <- "../LSVIData/Soortengroepen_LSVI.csv"
# soortengroepenLSVIRekenmodule_fn <-  "../LSVIData/Soortenlijst_LSVIRekenmodule_v3.csv"

lifeforms6230 <- "../LSVIData/selectieSoorten6230_lifeforms.csv"
oppervlakteBWK <- "../Data/Steekproef/BWK2018_Oppervlaktes.csv"

#----------------------
# Sample
#----------------------

dirSample <- "../Data/Steekproef/."
sampleHeideFile <- "meetnet_heide_versie201400611"
sample6510File <- "steekproef_6510_versie20140506"
sampleSizeCalcFile <- "../Data/Steekproef/steekproefgrootte_versie20140324.txt"
sampleBosFile <- "meetnet_bos_versie20150401_orthocheck_y123"

#-------------------
# Strata
#-----------------

dirStrata <- "../Data/Shapefiles/."
bioGeoregions <- "BioGeoregions_Lambert1972"
SBZH <- "SBZH"


cat("Omgevingsvariabelen ingelezen\n--------------------------------\n\n")


