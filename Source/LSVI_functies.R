########################################################################################################
### FUNCTIES VOOR OPHALEN  GEGEVENS VEGETATIEOPNAME VOOR BEREKENING INDICATOREN VEGETATIE EN VERSTORING
#########################################################################################################

### geobserveerd habitattype uit MHK databank halen

#db <- dbHeideEn6510_2016

getObservedHabMHK <- function (db = dbHeideEn6510_2014_2015){

  query_HabTypeObserved <- "
  SELECT
  Standdescription.IDPlots,
  Standdescription.ID,
  Standdescription.Area_m2,
  Standdescription.HABITAT,
  qHABITAT.Value1
  FROM Standdescription LEFT JOIN qHABITAT ON Standdescription.HABITAT = qHABITAT.ID;
  "

  if(substr(db,nchar(db)-3,nchar(db)) == ".mdb"){
    connectieDB <-   odbcConnectAccess(db)
  } else if (substr(db,nchar(db)-5,nchar(db)) == ".accdb") {
    connectieDB <-   odbcConnectAccess2007(db)
  }

  habObservedOrig <- sqlQuery(connectieDB, query_HabTypeObserved, stringsAsFactors = TRUE)

  odbcClose(connectieDB)

  habObserved <- habObservedOrig %>%
    rename(IDSegments = ID,
           HabObservedCode = HABITAT,
           HabObserved = Value1)

  habObserved$HabObserved <- revalue(habObserved$HabObserved,c('6510 hu'="6510_hu","2330 bu" = "2330_bu", "6510 hua" = "6510_hua", "6510 hus" = "6510_hus", "6510 huk" = "6510_huk"))

  habObserved$Weight <- ifelse (is.na(habObserved$Area_m2),1,habObserved$Area_m2/(pi*18^2))

  result <- habObserved

  return (result)
}

####################################################

### geobserveerd habitattype uit MHK databank boshabitats halen

#db <- dbHeideEn6510_2016

getObservedHabBosMHK <- function (db = dbBosExtra){

  query_HabTypeObserved <- "
 SELECT Standdescription_segments.IDPlots,
  Standdescription_segments.ID,
  Standdescription_segments.Area_m2,
  Standdescription_segments.HAB,
  qHABITAT.Value1,
  Standdescription_segments.Landuse,
  qLanduse.Value1
  FROM (Standdescription_segments LEFT JOIN qHABITAT ON Standdescription_segments.HAB = qHABITAT.ID) LEFT   JOIN qLanduse ON Standdescription_segments.Landuse = qLanduse.ID;
  "

  if(substr(db,nchar(db)-3,nchar(db)) == ".mdb"){
    connectieDB <-   odbcConnectAccess(db)
  } else if (substr(db,nchar(db)-5,nchar(db)) == ".accdb") {
    connectieDB <-   odbcConnectAccess2007(db)
  }

  habObservedOrig <- sqlQuery(connectieDB, query_HabTypeObserved, stringsAsFactors = TRUE)

  odbcClose(connectieDB)

  habObserved <- habObservedOrig %>%
    rename(IDSegments = ID,
           HabObservedCode = HAB,
           HabObserved = Value1,
           LanduseCode = Landuse,
           Landuse = Value1.1)

  habObserved$HabObserved <- revalue(habObserved$HabObserved,c('91E0 vo'="91E0_vo",
                                                               '91E0 vm'="91E0_vm"))

  habObserved$Weight <- ifelse (is.na(habObserved$Area_m2), 1, habObserved$Area_m2/(pi*18^2))

  result <- habObserved %>%
    select(IDPlots, IDSegments, Landuse, HabObserved, Weight)

  return (result)
}

####################################################

# plotID's en habitattypes van opgemeten plots uit MHK-databank halen

getMeasuredPlotsMHK <- function(db = dbHeideEn6510_2014_2015){

  speciesMeasured <- getCoverSpeciesMHK(db)
  coverPlots <- getCoverVeglayersMHK(db) %>%
    unique()

  coverPlots$IDSegments <- 1

  habMeasured <- getObservedHabMHK(db)

  measuredPlots <- habMeasured %>%
                  rename(HabCode = HabObserved) %>%
                  left_join(select(coverPlots, IDPlots, IDSegments, HabObservedPQ), by = c("IDPlots", "IDSegments")) %>%
                  group_by(IDPlots) %>%
                  mutate(MixedPlot = n() > 1) %>%
                  ungroup()

  measuredPlots$HabCode <- as.character(measuredPlots$HabCode)
  measuredPlots$HabObservedPQ <- as.character(measuredPlots$HabObservedPQ)

  measuredPlots$matchHab <- measuredPlots$HabCode == measuredPlots$HabObservedPQ

  #enkel segmenten met habitat
  measuredPlotsHab <- measuredPlots %>%
                      filter(!MixedPlot | HabCode == HabObservedPQ) %>%
                      group_by(IDPlots) %>%
                      summarise(HabCode = unique(HabObservedPQ),
                                Weight = round(sum(Weight),3))

  return (measuredPlotsHab)
}

####################################################
getStatusFieldWorkMHK <- function(db = dbHeideEn6510_2018){

query_gridpoints <- "
SELECT Grid_points.IDPlots
  , Grid_points.X_m
  , Grid_points.Y_m
  , Grid_points.ID
  , Grid_points.RANKING
  , Grid_points.YEAR
  , Grid_points.SingleID
  , Grid_points.Status_Fieldwork
  , YesNo.Value1
  , Grid_points.Info_Status_Fieldwork
  , qinfo_Status_Fieldwork.Value1
  , Grid_points.Remark
  , Grid_points.X
  , Grid_points.Y
  , Grid_points.RANK_VL
  , Grid_points.HABT1
  , Grid_points.HABSUBT1
  , Grid_points.HABT2
  , Grid_points.HABSUBT2
  , Grid_points.HABT3
  , Grid_points.HABSUBT3
  , Grid_points.SBZH
  , Grid_points.NB
  , Grid_points.EXTRA_SUBT
  , Grid_points.Fieldteam
  , qFieldteam.Value1
  , Grid_points.Pol_beschr

FROM ((Grid_points LEFT JOIN qFieldteam ON Grid_points.Fieldteam = qFieldteam.ID)
  LEFT JOIN qinfo_Status_Fieldwork ON Grid_points.Info_Status_Fieldwork = qinfo_Status_Fieldwork.ID)
  LEFT JOIN [YesNo] ON Grid_points.Status_Fieldwork = YesNo.ID
;
"

  if(substr(db,nchar(db)-3,nchar(db)) == ".mdb"){
    connectieDB <-   odbcConnectAccess(db)
  } else if (substr(db,nchar(db)-5,nchar(db)) == ".accdb") {
    connectieDB <-   odbcConnectAccess2007(db)
  }


 gridpointsOrig <- sqlQuery(connectieDB, query_gridpoints)
  gridPoint <- gridpointsOrig %>%
    select(IDPlots = ID, Status_Fieldwork = Value1, Info_Status_Fieldwork = Value1.1, Remark) %>%
    filter(!is.na(Status_Fieldwork))

  odbcClose(connectieDB)

  return(gridPoint)
}



######################################################

# overzicht bezochte plots uit MHK-databank halen

getVisitedPlotsMHK <- function(db = dbHeideEn6510_2014_2015){

# inlezen datat
  statusFieldwork <- getStatusFieldWorkMHK(db)

  sampleHeide <- getSample(fileSample = sampleHeideFile)

  sample6510 <- getSample(fileSample = sample6510File)

  habObserved <- getObservedHabMHK(db )

  coverPlots <- getCoverVeglayersMHK(db)

  coverSpecies <- getCoverSpeciesMHK(db)

  structureHeide <- getStructurePlotHeide(db)

  structure6510 <- getStructurePlot6510(db)

  metadata <- getMetaDataMHK_2016(db)


  heide6510 <- c("4010", "4030", "2310", "6510_hu", "2330_bu",  "6510_hua", "6510_hus", "6510_huk", "6510", "2330")

  # voortgang <- read.csv2(voortgangHeideEn6510, header = TRUE) %>%
  # select(IDPlots = RANKING, Visited = punt.bezocht_adj, Measured = opname, Reason = reden.geen.opname_adj, HabObserved = geobserveerd.habitat ) %>%
  # mutate(Status = ifelse(Visited == "ja" & Measured == "ja", "Opname uitgevoerd",
  #                   ifelse(Visited == "ja" & Measured == "nee", ifelse(Reason %in% c("Geen opname : geen doelhabitat", "Geen opname : permanent nt toegankelijk", "wellicht geen doelveg"), "Geen opname", "Opname to do"), "Opname to do")))


  visitedPlots <- habObserved %>%
                  rename(HabObservedPlot = HabObserved) %>%
                  left_join(select(coverPlots, IDPlots,  HabObservedPQ), by = c("IDPlots")) %>%
                  group_by(IDPlots) %>%
                  mutate(MixedPlot = n() > 1) %>%
                  ungroup()

  visitedPlots$HabObservedPlot <- as.character(visitedPlots$HabObservedPlot)
  visitedPlots$HabObservedPQ <- as.character(visitedPlots$HabObservedPQ)

  visitedPlots <-  visitedPlots%>%
    group_by(IDPlots) %>%
    mutate(MatchHab = ifelse(MixedPlot,
                             sum(HabObservedPlot == HabObservedPQ, na.rm = TRUE) > 0,
                             HabObservedPlot == HabObservedPQ),
           Visited = ifelse(MixedPlot,
                            TRUE,
                             !(is.na(HabObservedPlot) & is.na(HabObservedPQ))),
           Select = (!MixedPlot) | (MatchHab & HabObservedPlot == HabObservedPQ) | (!MatchHab & IDSegments == 1))

  visitedPlots_selected <- visitedPlots %>%
    filter(!is.na(Select) & Select) %>%
    group_by(IDPlots, HabObservedPlot, HabObservedPQ, MixedPlot, Visited, MatchHab) %>%
    summarise(Weight = min(1, sum(Weight, na.rm = TRUE)))


  #welke gegevens ingezameld per plot?
  coverPlots$VeglayerMeasured <- !is.na(coverPlots$CoverHerblayer)
  structureHeide$StructureMeasured <- !is.na(structureHeide$Shrub_and_Treelayer_18m) | !is.na(structureHeide$Herbs)
  structure6510$StructureMeasured <-  !is.na(structure6510$Shrub_and_Treelayer_18m)
  structureHeide6510 <- bind_rows(select(structureHeide, IDPlots, StructureMeasured),
                                  select(structure6510, IDPlots, StructureMeasured))

  coverSpeciesPlot <- coverSpecies %>%
    group_by(IDPlots) %>%
    summarise(VegspeciesMeasured = n() > 0)


  visitedPlots_metadata <-  visitedPlots_selected %>%
    mutate(HabObservedCenter = ifelse(is.na(MatchHab),
                                      ifelse(!is.na(HabObservedPlot), HabObservedPlot, HabObservedPQ),
                                      ifelse(MatchHab, HabObservedPlot,
                                             ifelse(!(HabObservedPlot %in% heide6510) & (HabObservedPQ %in% heide6510),
                                                    HabObservedPQ, HabObservedPlot)))) %>%
    left_join(select(coverPlots, IDPlots, VeglayerMeasured), by = "IDPlots") %>%
    left_join(structureHeide6510, by = "IDPlots") %>%
    left_join(coverSpeciesPlot, by = "IDPlots") %>%
    ungroup() %>%
    mutate(StructureMeasured = ifelse(is.na(StructureMeasured), FALSE,StructureMeasured),
           VeglayerMeasured = ifelse(is.na(VeglayerMeasured), FALSE, VeglayerMeasured),
           VegspeciesMeasured = ifelse(is.na(VegspeciesMeasured), FALSE, VegspeciesMeasured)) %>%
    mutate(Measured = StructureMeasured & VegspeciesMeasured)

    #doelhabitat

  sample <- bind_rows(sampleHeide, sample6510)
  sample$IDPlots <- as.numeric(as.character(sample$IDPlots))

  allPlots <- sample %>%
    filter(IDPlots > 0) %>%
    full_join(visitedPlots_metadata, by ="IDPlots") %>%
    left_join(statusFieldwork, by = "IDPlots") %>%
    #full_join(select(voortgang, IDPlots, Visited_v = Visited, Measured_v = Measured, Reason), by = "IDPlots") %>%
    mutate(Status = ifelse(Measured & !is.na(Measured), "Opname uitgevoerd",
                           ifelse(Orthocontr == "0" & !is.na(Orthocontr),"Geen doelhabitat - orthocontrole",
                              ifelse(Orthocontr == "ontoegankelijk" & !is.na(Orthocontr), "Ontoegankelijk - orthocontrole",
                                  ifelse(Visited & !is.na(Visited) , as.character(Info_Status_Fieldwork),"To do"))))) %>%
    mutate(Visited = ifelse(is.na(Visited), FALSE, Visited),
           Measured = ifelse(is.na(Measured), FALSE, Measured),
           StructureMeasured = ifelse(is.na(StructureMeasured), FALSE, StructureMeasured),
           VegspeciesMeasured = ifelse(is.na(VegspeciesMeasured), FALSE, VegspeciesMeasured),
           VeglayerMeasured = ifelse(is.na(VeglayerMeasured), FALSE, VeglayerMeasured))


   allPlot_unique <- allPlots %>%
     group_by(IDPlots, SBZH, x,y, Weight, HabObservedCenter, Visited, Measured, StructureMeasured, VegspeciesMeasured, VeglayerMeasured, Status, VisitedQC = Status_Fieldwork) %>%
     summarise(HabTarget1 = HabTarget1[1],
               HabTarget2 = HabTarget2[1]) %>%
     ungroup() %>%
     left_join(select(metadata, IDPlots, Date, X_m, Y_m), by = "IDPlots")

   result <- allPlot_unique %>%
     filter(IDPlots != 0) %>%
     select(IDPlots, SBZH, HabTarget1, HabTarget2, HabObserved = HabObservedCenter, Weight, Visited, VisitedQC, Measured, StructureMeasured, VegspeciesMeasured, VeglayerMeasured, Status, Date, X = x, Y = y, X_m, Y_m)




  return (result)
}


####################################################

# overzicht bezochte plots uit MHK-databank halen

getVisitedPlotsBosMHK <- function(db = dbBosExtra){

# inlezen datat

  habObserved <- getObservedHabBosMHK(db )

  coverPlots <- getCoverVeglayersVBI2(db)

  coverSpecies <- getCoverSpeciesVBI2(db)

  treesA3A4 <- getTreesA3A4RawData(db)

  treesA2 <- getTreesA2VBI2(db)

  sample <- getSample(fileSample = sampleBosFile) %>%
    filter(HabTarget1 != "91E0_sf")

  metadata <- getMetaDataBosMHK(db)

 # heide6510 <- c("4010", "4030", "2310", "6510_hu", "2330_bu",  "6510_hua", "6510_hus", "6510_huk", "6510", "2330")

  #welke gegevens ingezameld per plot?
  #bedekking vegetatielagen
  coverPlots$VeglayerMeasured <- !is.na(coverPlots$CoverHerblayer)

  #bedekking soorten
  coverSpeciesPlot <- coverSpecies %>%
    group_by(IDPlots) %>%
    summarise(VegspeciesMeasured = n() > 0)

  #A3A4 bomen
  treesA3A4Plot <- treesA3A4 %>%
     group_by(IDPlots) %>%
    summarise(TreesA3A4Measured = n() > 0)

  #A2 bomen
  treesA2Plot <- treesA2 %>%
     group_by(IDPlots) %>%
    summarise(TreesA2Measured = n() > 0)

  visitedPlots <- habObserved %>%
    left_join(select(coverPlots, IDPlots, VeglayerMeasured), by = "IDPlots") %>%
    left_join(coverSpeciesPlot, by = "IDPlots") %>%
    mutate(VegspeciesMeasured = ifelse(is.na(VegspeciesMeasured), FALSE,VegspeciesMeasured)) %>%
    left_join(treesA3A4Plot, by = "IDPlots") %>%
    mutate(TreesA3A4Measured = ifelse(is.na(TreesA3A4Measured), FALSE,TreesA3A4Measured)) %>%
    left_join(treesA2Plot, by = "IDPlots") %>%
    mutate(TreesA2Measured = ifelse(is.na(TreesA2Measured), FALSE,TreesA2Measured)) %>%
    mutate(StructureMeasured = (Landuse == "bos - kapvlakte" & !is.na(Landuse)) | TreesA3A4Measured | TreesA2Measured) %>%
    mutate(Visited = ifelse(!is.na(HabObserved), "ja", "nee"),
           Measured = ifelse(VegspeciesMeasured & StructureMeasured, "ja", "nee"))

    #doelhabitat

  sample$IDPlots <- as.numeric(as.character(sample$IDPlots))

  allPlots <- sample %>%
    filter(IDPlots > 0) %>%
    left_join(visitedPlots, by ="IDPlots") %>%
    #full_join(select(voortgang, IDPlots, Visited_v = Visited, Measured_v = Measured, Reason), by = "IDPlots") %>%
    mutate(Status = ifelse(Measured == "ja" & !is.na(Measured), "Opname uitgevoerd",
                           ifelse(Orthocontr == "2" & !is.na(Orthocontr),"Geen doelhabitat - orthocontrole",
                              ifelse(Orthocontr == "ontoegankelijk" & !is.na(Orthocontr), "Ontoegankelijk - orthocontrole",
                                  ifelse(Visited == "ja" & !is.na(Visited) , "Geen doelhabitat - terreincheck","To do"))))) %>%
    mutate(Visited = ifelse(is.na(Visited), "nee", Visited),
           Measured = ifelse(is.na(Measured), "nee", Measured),
           StructureMeasured = ifelse(is.na(StructureMeasured), FALSE, StructureMeasured),
           VegspeciesMeasured = ifelse(is.na(VegspeciesMeasured), FALSE, VegspeciesMeasured))


   allPlot_unique <- allPlots %>%
     group_by(IDPlots, IDSegments, SBZH, x,y, Weight, Year, HabObserved, Visited, Measured, StructureMeasured, VegspeciesMeasured,  Status, Orthocontr) %>%
     summarise(HabTarget1 = HabTarget1[1],
               HabTarget2 = HabTarget2[1]) %>%
     ungroup() %>%
     left_join(select(metadata, IDPlots, Date, X_m, Y_m), by = "IDPlots")

   result <- allPlot_unique %>%
     filter(IDPlots != 0) %>%
     select(IDPlots, IDSegments, SBZH, HabTarget1, HabTarget2,Year_planning = Year, HabObserved, Weight, Visited, Measured, StructureMeasured, VegspeciesMeasured, Status, Orthocontr, Date, X = x, Y = y, X_m, Y_m)

  return (result)
}
#####################################################

getMeasuredPlotsBosMHK <- function(db = dbBosExtra){

# inlezen datat

  habObserved <- getObservedHabBosMHK(db )

  coverPlots <- getCoverVeglayersVBI2(db)

  coverSpecies <- getCoverSpeciesVBI2(db)

  treesA3A4 <- getTreesA3A4RawData(db)

  treesA2 <- getTreesA2VBI2(db)

  #welke gegevens ingezameld per plot?
  #bedekking vegetatielagen
  coverPlots$VeglayerMeasured <- !is.na(coverPlots$CoverHerblayer)

  #bedekking soorten
  coverSpeciesPlot <- coverSpecies %>%
    group_by(IDPlots) %>%
    summarise(VegspeciesMeasured = n() > 0)

  #A3A4 bomen
  treesA3A4Plot <- treesA3A4 %>%
     group_by(IDPlots) %>%
    summarise(TreesA3A4Measured = n() > 0)

  #A2 bomen
  treesA2Plot <- treesA2 %>%
     group_by(IDPlots) %>%
    summarise(TreesA2Measured = n() > 0)

  measuredPlots <- habObserved %>%
    left_join(select(coverPlots, IDPlots, VeglayerMeasured), by = "IDPlots") %>%
    left_join(coverSpeciesPlot, by = "IDPlots") %>%
    mutate(VegspeciesMeasured = ifelse(is.na(VegspeciesMeasured), FALSE,VegspeciesMeasured)) %>%
    left_join(treesA3A4Plot, by = "IDPlots") %>%
    mutate(TreesA3A4Measured = ifelse(is.na(TreesA3A4Measured), FALSE,TreesA3A4Measured)) %>%
    left_join(treesA2Plot, by = "IDPlots") %>%
    mutate(TreesA2Measured = ifelse(is.na(TreesA2Measured), FALSE,TreesA2Measured)) %>%
    mutate(StructureMeasured = (Landuse == "bos - kapvlakte" & !is.na(Landuse)) | TreesA3A4Measured | TreesA2Measured) %>%
    filter(VegspeciesMeasured | StructureMeasured)


  return (measuredPlots)
}



###################################################
getMetaDataMHK <- function(db= dbHeideEn6510_2014_2015){

  query_metadataMHK <- "
   SELECT
  Standdescription.IDPlots,
  Observer_date.Add_date,
  Grid_points.X_m,
  Grid_points.Y_m,
  GPS_REF.X_m,
  GPS_REF.Y_m,
  Grid_points.SBZH
  FROM GPS_REF
  RIGHT JOIN ((Standdescription INNER JOIN Observer_date ON Standdescription.IDPlots = Observer_date.IDPlots)   LEFT JOIN Grid_points ON Standdescription.IDPlots = Grid_points.ID) ON GPS_REF.SingleID = Standdescription.IDPlots;
    "

  connectieDB <-   odbcConnectAccess(db)

  metadataMHK <- sqlQuery(connectieDB, query_metadataMHK, stringsAsFactors = TRUE)

  odbcClose(connectieDB)

  metadataMHK <- rename(metadataMHK, X_sampleframe=  X_m, Y_sampleframe = Y_m, X_measured = X_m.1, Y_measured = Y_m.1, Date = Add_date  )

  metadataMHK$Date <- as.Date(metadataMHK$Date)
  metadataMHK$Year <- as.numeric(format(metadataMHK$Date,'%Y'))

  metadata <- unique(metadataMHK)

  return(metadata)

}



####################################################
getStatusFieldWork <- function(db = dbHeideEn6510_2014_2015){

  query_HabTypeTarget <- "
SELECT
Grid_points.ID,
Grid_points.SingleID,
Grid_points.Status_Fieldwork,
Grid_points.Info_Status_Fieldwork,
qinfo_Status_Fieldwork.Value,
Grid_points.HABSUBT1,
Grid_points.HABT2,
Grid_points.SBZH
FROM Grid_points LEFT JOIN qinfo_Status_Fieldwork ON Grid_points.Info_Status_Fieldwork = qinfo_Status_Fieldwork.ID;
"
  connectieDB <-   odbcConnectAccess(db)
  statusFieldworkOrig <- sqlQuery(connectieDB, query_HabTypeTarget, stringsAsFactors = TRUE)
  odbcClose(connectieDB)

  statusFieldwork <- plyr::rename(statusFieldworkOrig, c(ID = "IDPlots", SingleID = "ID", Info_Status_Fieldwork = "Info_Status_Fieldwork_Code",Value = "Info_Status_Fieldwork",HABSUBT1 = "HabTarget1", HABT2 = "HabTarget2"))

  return (statusFieldwork)

}




###################################################
getMetaDataMHK_2016 <- function(db= dbHeideEn6510_2016){

  query_metadataMHK <- "
  SELECT
  Observer_date.IDPlots,
  Observer_date.Add_date
  FROM Observer_date;
    "

  query_coordMHK <- "
  SELECT
  GPS_REF.SingleID,
  GPS_REF.X_m,
  GPS_REF.Y_m
  FROM GPS_REF;
    "

  if(substr(db,nchar(db)-3,nchar(db)) == ".mdb"){
    connectieDB <-   odbcConnectAccess(db)
  } else if (substr(db,nchar(db)-5,nchar(db)) == ".accdb") {
    connectieDB <-   odbcConnectAccess2007(db)
  }

  metadataMHK <- sqlQuery(connectieDB, query_metadataMHK, stringsAsFactors = TRUE)
  coordMHKOrig <- sqlQuery(connectieDB, query_coordMHK)

  odbcClose(connectieDB)

  metadataMHK <- rename(metadataMHK,  Date = Add_date  )

  metadataMHK$Date <- as.Date(metadataMHK$Date)
  metadataMHK$Year <- as.numeric(format(metadataMHK$Date,'%Y'))

  coordMHK <- coordMHKOrig %>%
    rename(IDPlots = SingleID) %>%
    filter(!is.na(IDPlots))


  # SBZH
#
#   steekproefHeide <- getSample(fileSample =  sampleHeideFile)
#   steekproef6510 <- getSample(fileSample = sample6510File)
#   steekproefHeide6510 <- rbind(select(steekproefHeide, -Orthocontr), steekproef6510)
#
#   steekproefHeide6510$IDPlots<- as.numeric(as.character(steekproefHeide6510$IDPlots))
  metadataMHK <- left_join(metadataMHK, coordMHK, by = "IDPlots")

  metadataMHK <- unique(metadataMHK)

  return(metadataMHK)

}

getMetaDataBosMHK <- function(db= dbBosExtra){

  query_metadataMHK <- "
 SELECT Observer_date.IDPlots,
        Observer_date.Add_date,
        Plots_Forest_Inventory.X_m,
        Plots_Forest_Inventory.Y_m
        FROM Observer_date INNER JOIN Plots_Forest_Inventory ON Observer_date.IDPlots = Plots_Forest_Inventory.Plotnr;

    "

  if(substr(db,nchar(db)-3,nchar(db)) == ".mdb"){
    connectieDB <-   odbcConnectAccess(db)
  } else if (substr(db,nchar(db)-5,nchar(db)) == ".accdb") {
    connectieDB <-   odbcConnectAccess2007(db)
  }

  metadataMHK <- sqlQuery(connectieDB, query_metadataMHK, stringsAsFactors = TRUE)

  odbcClose(connectieDB)

  metadataMHK <- rename(metadataMHK,  Date = Add_date  )

  metadataMHK$Date <- as.Date(metadataMHK$Date)
  metadataMHK$Year <- as.numeric(format(metadataMHK$Date,'%Y'))


  # SBZH
#
#   steekproefHeide <- getSample(fileSample =  sampleHeideFile)
#   steekproef6510 <- getSample(fileSample = sample6510File)
#   steekproefHeide6510 <- rbind(select(steekproefHeide, -Orthocontr), steekproef6510)
#
#   steekproefHeide6510$IDPlots<- as.numeric(as.character(steekproefHeide6510$IDPlots))

  metadataMHK <- unique(metadataMHK)

  return(metadataMHK)

}




getHabtarget <- function(){

  steekproefHeide <- getSample(fileSample =  sampleHeideFile)
  steekproef6510 <- getSample(fileSample = sample6510File)
  steekproefHeide6510 <- rbind(select(steekproefHeide, -Orthocontr), steekproef6510)

  steekproefHeide6510$IDPlots<- as.numeric(as.character(steekproefHeide6510$IDPlots))
  steekproefHeide6510

  steekproefHabTarget <- summarise(group_by(steekproefHeide6510, IDPlots),
                                  HabTarget1 = HabTarget1[1],
                                  HabTarget2 = HabTarget2[1])

  steekproefHabTarget <- ungroup(steekproefHabTarget)
  return(steekproefHabTarget)

}



####################################################



### Selectie van VBI-plots met bijhorend N2000-habitattype (volgens habitatkaart versie 20140324), waarvoor we LSVI willen berekenen


getMeasuredPlotsVBI2 <- function(db = dbVBI2){

  connectionStrata <- odbcConnectAccess2007(dbVBIStrata)

  VBI_plotsOrig <- sqlFetch(connectionStrata,"tblN2000Habitat_versie20140324" )

  odbcClose(connectionStrata)

  ### Selectie van VBI-plots die al opgemeten werden in 2de cyclus

  connectionMeetproces <- odbcConnectAccess2007(dbVBIMeetproces)

  records <- sqlFetch(connectionMeetproces, "tblRecordsVBI2+")

  odbcClose(connectionMeetproces)

  ### Selectie van plots met Natura 2000 habitat EN waarvoor gegevens werden ingezameld in 2de cyclys

  VBI_Hab <- VBI_plotsOrig[VBI_plotsOrig$IDPlots %in% records$IDPlots, c("IDPlots","HabCode")]

  return(VBI_Hab)

}


####################################################

getCoverSpeciesVBI1 <- function (db =  dbVBI1_veg, plotIDs = NULL){

query_veglayerVBI1 <-"
SELECT Opnamen.Opnamenummer
, Opnamen.[Belgisch volgnummer]
, Opnamen.Vegetatielaag
, Opnamen.Abund_Transf
FROM Opnamen
;
"
connectieVBI1 <- odbcConnectAccess2007(db) #dit is een accdb file

veglayerVBI1Orig <- sqlQuery(connectieVBI1, query_veglayerVBI1, stringsAsFactors = TRUE)

odbcClose(connectieVBI1)

veglayerVBI1Orig <- plyr::rename(veglayerVBI1Orig,c(Opnamenummer="IDPlots","Belgisch volgnummer"="IDSpVBI1",Abund_Transf="Coverage")) #

veglayerVBI1 <- veglayerVBI1Orig



 # externe data

  connectieExterneData <- odbcConnectAccess2007(dbExterneData) #dit is een accdb file
  treeList <- sqlFetch(connectieExterneData,"tblTreeSpeciesCharacteristics",stringsAsFactors = TRUE)
  treeListExtra <- sqlFetch(connectieExterneData,"tblSpeciesTreelayerCharacteristics",stringsAsFactors = TRUE)
  speciesListComb<-sqlFetch(connectieExterneData, "tblspeciesListComb", stringsAsFactors = TRUE)
  odbcClose(connectieExterneData)

  veglayers <- merge(veglayerVBI1,speciesListComb,by="IDSpVBI1",all.x=TRUE)

  scaleBB <- data.frame(BBID = c(1:9), Cover = c(0.25,1,2.25,4,8.75,18.75,37.5,62.5,87.5))

  veglayers$Scale <- "Braun-Blanquet"

  veglayers<-merge(veglayers,scaleBB,by.x="Coverage",by.y="BBID",all.x=TRUE)

  veglayers <- veglayers[order(veglayers$IDPlots),]

  veglayers$Tree <- (veglayers$NameNl %in% treeList$NameNl) | (veglayers$NameNl %in% treeListExtra$NameNl[treeListExtra$Tree==1])

  #veglayers[veglayers$NameNl == "_ANDERE SOORT",]$Tree <- FALSE

  veglayers <- veglayers[,!names(veglayers) %in% c("Coverage","IDSpVBI2", "IDSpVBI1")]

  veglayers$Vegetatielaag <- ifelse(veglayers$Vegetatielaag == "k", "kruidlaag",
                            ifelse(veglayers$Vegetatielaag == "s", "struiklaag",
                                   ifelse(veglayers$Vegetatielaag == "m", "moslaag",
                                          ifelse(veglayers$Vegetatielaag == "b", "boomlaag",NA))))



  if (is.null(plotIDs)){

    result <- veglayers

  } else {

    result <- veglayers[veglayers$IDPlots %in% plotIDs,]

  }

  return(result)

}


##############################################################


### Haal bedekking soorten per vegetatielaag in 16x16m proefvlak uit VBI2 databank; duid aan welke soorten bomen zijn op basis van lijst in externe databank
# Is het wel nodig om aan te duiden welke soorten bomen zijn voor berekening LSVI --> ja, voor bepaling natuurlijke vrejonging --> groeiklasse 2 #?#

getCoverSpeciesVBI2 <- function (db =  dbVBI2, dbExterneData = dbVBIExterneData, plotIDs = NULL){

  query_herblayer<-"
  SELECT Herblayer.IDPlots,
  Herblayer.Species,
  [qVEG_HerbSpecies].Value1,
  [qVEG_HerbSpeciesScientific].Value1,
  Herblayer.Coverage_date1,
  Herblayer.Coverage_date2
  FROM (Herblayer
  LEFT JOIN [qVEG_HerbSpecies] ON Herblayer.Species = [qVEG_HerbSpecies].ID)
  LEFT JOIN [qVEG_HerbSpeciesScientific] ON Herblayer.Species = [qVEG_HerbSpeciesScientific].ID;
  "
  query_shrublayer<-"
  SELECT Shrublayer.IDPlots,
  Shrublayer.Species,
  [qVEG_TreeSpecies].Value1,
  [qVEG_TreeSpeciesScientific].Value1,
  Shrublayer.Coverage
  FROM (Shrublayer
  LEFT JOIN [qVEG_TreeSpecies] ON Shrublayer.Species = [qVEG_TreeSpecies].ID)
  LEFT JOIN [qVEG_TreeSpeciesScientific] ON Shrublayer.Species = [qVEG_TreeSpeciesScientific].ID;
  "

  query_treelayer<-"
  SELECT Treelayer.IDPlots,
  Treelayer.Species,
  [qVEG_TreeSpecies].Value1,
  [qVEG_TreeSpeciesScientific].Value1,
  Treelayer.Coverage
  FROM (Treelayer
  LEFT JOIN [qVEG_TreeSpecies] ON Treelayer.Species = [qVEG_TreeSpecies].ID)
  LEFT JOIN [qVEG_TreeSpeciesScientific] ON Treelayer.Species = [qVEG_TreeSpeciesScientific].ID;
  "

  query_scaleBB <-"
  SELECT qCoverHerbs.ID,
  qCoverHerbs.Value1
  FROM qCoverHerbs"

  connectieVBI2 <- odbcConnectAccess2007(db)

  herblayerOrig <- sqlQuery(connectieVBI2, query_herblayer, stringsAsFactors = FALSE)
  shrublayerOrig <- sqlQuery(connectieVBI2, query_shrublayer, stringsAsFactors = FALSE)
  treelayerOrig <- sqlQuery(connectieVBI2, query_treelayer, stringsAsFactors = FALSE)
  #scaleBBOrig <- sqlQuery(connectieVBI2,query_scaleBB, stringsAsFactors = FALSE)

  odbcClose(connectieVBI2)

  herblayer <- herblayerOrig %>%
    filter(!is.na(Species)) %>%
    rename(IDSpVBI2 = Species,
           NameNl = Value1,
           NameSc = Value1.1) %>%
    mutate(Coverage = pmax(Coverage_date1, Coverage_date2, na.rm=TRUE),
           Vegetatielaag = "kruidlaag") %>%
    select(-Coverage_date1, -Coverage_date2)

   shrublayer <- shrublayerOrig %>%
    filter(!is.na(Species)) %>%
    rename(IDSpVBI2 = Species,
           NameNl = Value1,
           NameSc = Value1.1) %>%
    mutate(Vegetatielaag = "struiklaag")

   treelayer <- treelayerOrig %>%
    filter(!is.na(Species)) %>%
    rename(IDSpVBI2 = Species,
           NameNl = Value1,
           NameSc = Value1.1) %>%
   mutate(Vegetatielaag = "boomlaag")

  # scaleBB <- scaleBBOrig %>%
  #  rename(BBID = ID,
  #        ClassName = Value1)

  veglayers <- bind_rows(herblayer,shrublayer,treelayer) %>%
    rename(BBID = Coverage)

  # externe data

  connectieExterneData <- odbcConnectAccess2007(dbExterneData) #dit is een accdb file

  treeList <- sqlFetch(connectieExterneData,"tblTreeSpeciesCharacteristics",stringsAsFactors = TRUE)

  treeListExtra <- sqlFetch(connectieExterneData,"tblSpeciesTreelayerCharacteristics",stringsAsFactors = TRUE)

  speciesListComb <- sqlFetch(connectieExterneData, "tblspeciesListComb", stringsAsFactors = TRUE)

  odbcClose(connectieExterneData)

  treeListExtra <- treeListExtra %>%
    filter(Tree == 1)

  treeList <- treeList %>%
    filter(NameNl != "_ANDERE SOORT")

 # odbcClose(connectieExterneData)

  # scaleBB$Cover <- c(0.25, 1, 2.25, 4, 8.75, 18.75, 37.5, 62.5, 87.5)
  #
  # veglayers$Scale <- "Braun-Blanquet"

  scaleBB <- selectScale("Braun-Blanquet") %>%
    select(BBID  = KlasseID, Cover = BedekkingGem)

  veglayers_Cover <- veglayers %>%
    left_join(scaleBB, by ="BBID") %>%
    arrange(IDPlots) %>%
    select(-IDSpVBI2, BBID) %>%
    mutate(Tree = (NameNl %in% treeList$NameNl) | (NameNl %in% treeListExtra$Species))

  if (is.null(plotIDs)){

    result <- veglayers_Cover

  } else {

    result <- veglayers_Cover %>%
      filter(IDPlots %in% plotIDs)

  }

  return(result)

}

#################################################################################################
### Haal bedekking soorten per vegetatielaag in 16x16m proefvlak uit databank meetnet habitatkwaliteit voor heide en 6510; duid aan welke soorten bomen zijn op basis van lijst in externe databank
#?# Is het wel nodig om aan te duiden welke soorten bomen zijn voor berekening LSVI? #?#

#db <- dbHeideEn6510_2014_2015

getCoverSpeciesMHK <- function(db = dbHeideEn6510_2014_2015, plotIDs =NULL){

  query_herblayer<-"
  SELECT Herblayer.IDPlots,
  Herblayer.Species,
  qVEG_HerbSpecies.Value1,
  qVEG_HerbSpeciesScientific.Value1,
  Herblayer.Coverage_date1,
  qLondo.Value1
  FROM ((Herblayer
  LEFT JOIN qVEG_HerbSpecies ON Herblayer.Species = qVEG_HerbSpecies.ID)
  LEFT JOIN qVEG_HerbSpeciesScientific ON Herblayer.Species_scientific = qVEG_HerbSpeciesScientific.ID)
  LEFT JOIN qLondo ON Herblayer.Coverage_date1 = qLondo.ID;
  "

  query_shrublayer <- "
  SELECT Shrublayer.IDPlots,
  Shrublayer.Species,
  qVEG_TreeSpecies.Value1,
  qVEG_TreeSpeciesScientific.Value1,
  Shrublayer.Coverage,
  qLondo.Value1
  FROM ((Shrublayer
  LEFT JOIN qVEG_TreeSpecies ON Shrublayer.Species = qVEG_TreeSpecies.ID)
  LEFT JOIN qVEG_TreeSpeciesScientific ON Shrublayer.Species_Scientific = qVEG_TreeSpeciesScientific.ID)
  LEFT JOIN qLondo ON Shrublayer.Coverage = qLondo.ID;

  "

  query_treelayer <- "
  SELECT Treelayer.IDPlots,
  Treelayer.Species,
  qVEG_TreeSpecies.Value1,
  qVEG_TreeSpeciesScientific.Value1,
  Treelayer.Coverage,
  qLondo.Value1
  FROM ((Treelayer
  LEFT JOIN qVEG_TreeSpecies ON Treelayer.Species = qVEG_TreeSpecies.ID)
  LEFT JOIN qVEG_TreeSpeciesScientific ON Treelayer.Species_Scientific = qVEG_TreeSpeciesScientific.ID)
  LEFT JOIN qLondo ON Treelayer.Coverage = qLondo.ID;
  "

  query_mosslayer <- "
  SELECT Mosslayer.IDPlots,
  Mosslayer.Species,
  qVEG_MossSpecies.Value1,
  qVEG_MossSpeciesScientific.Value1,
  Mosslayer.Coverage,
  qLondo.Value1
  FROM ((Mosslayer
  LEFT JOIN qVEG_MossSpecies ON Mosslayer.Species = qVEG_MossSpecies.ID)
  LEFT JOIN qVEG_MossSpeciesScientific ON Mosslayer.Species_Scientific = qVEG_MossSpeciesScientific.ID)
  LEFT JOIN qLondo ON Mosslayer.Coverage = qLondo.ID;
  "

  if(substr(db,nchar(db)-3,nchar(db)) == ".mdb"){
    connectieDB <-   odbcConnectAccess(db)
  } else if (substr(db,nchar(db)-5,nchar(db)) == ".accdb") {
    connectieDB <-   odbcConnectAccess2007(db)
  }

  herblayerOrig <- sqlQuery(connectieDB, query_herblayer, stringsAsFactors = TRUE)
  shrublayerOrig <- sqlQuery(connectieDB, query_shrublayer, stringsAsFactors = TRUE)
  treelayerOrig <- sqlQuery(connectieDB, query_treelayer, stringsAsFactors = TRUE)
  mosslayerOrig <- sqlQuery(connectieDB, query_mosslayer, stringsAsFactors = TRUE)

  odbcClose(connectieDB)

  herblayer <- plyr::rename(herblayerOrig[!is.na(herblayerOrig$Species),],c(Species = "IDHerbSpMHK",Value1="NameNl",Value1.1="NameSc", Coverage_date1 = "ClassCode", Value1.2 = "ClassName"))
  shrublayer <- plyr::rename(shrublayerOrig[!is.na(shrublayerOrig$Species),],c(Species = "IDTreeSpMHK",Value1="NameNl",Value1.1="NameSc", Coverage = "ClassCode", Value1.2 = "ClassName"))
  treelayer <- plyr::rename(treelayerOrig[!is.na(treelayerOrig$Species),],c(Species = "IDTreeSpMHK",Value1="NameNl",Value1.1="NameSc", Coverage = "ClassCode", Value1.2 = "ClassName"))
  mosslayer <- plyr::rename(mosslayerOrig[!is.na(mosslayerOrig$Species),],c(Species = "IDMossSpMHK",Value1="NameNl",Value1.1="NameSc", Coverage = "ClassCode", Value1.2 = "ClassName"))

  herblayer$Vegetatielaag <- "kruidlaag"
  shrublayer$Vegetatielaag <- "struiklaag"
  treelayer$Vegetatielaag <- "boomlaag"
  mosslayer$Vegetatielaag <- "moslaag"
  veglayers <- rbind.fill(herblayer,shrublayer,treelayer,mosslayer)
  veglayers$Scale <- "Londo"


  # externe data
#
#   connectieExterneData <- odbcConnectAccess2007(dbExterneData) #dit is een accdb file
#
#   treeList<-sqlFetch(connectieExterneData,"tblTreeSpeciesCharacteristics",stringsAsFactors = TRUE)
#
#   treeListExtra <- sqlFetch(connectieExterneData,"tblSpeciesTreelayerCharacteristics",stringsAsFactors = TRUE)

  #speciesListComb<-sqlFetch(connectieExterneData, "tblspeciesListComb", stringsAsFactors = TRUE)


  # odbcClose(connectieExterneData)

  londoScale <- data.frame(ClassCode=c(8, 10, 11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,33,34,35),Cover=c(7.5,20,30,40,50,60,70,80,90,97.5,10,0.5,2,4,0.5,2,4,0.5,2,4,0.5,2,4,12.5,47.5,52.5))



  veglayers<-merge(veglayers,londoScale,by = "ClassCode",all.x=TRUE)

  veglayers<-veglayers[order(veglayers$NameNl),]
  veglayers<-veglayers[order(veglayers$Vegetatielaag),]
  veglayers<-veglayers[order(veglayers$IDPlots),]

  # veglayers$Tree <- (veglayers$NameNl %in% treeList$NameNl) | (veglayers$NameNl %in% treeListExtra$NameNl[treeListExtra$Tree==1])
  #
  # veglayers$Tree <- ifelse(veglayers$NameNl == "_ANDERE SOORT",FALSE, veglayers$Tree)

  veglayers <- veglayers[,c("IDPlots","NameNl","NameSc","Vegetatielaag","Scale","ClassName","Cover")]

  if (is.null(plotIDs)){

    result <- veglayers

  } else {

    result <- veglayers[veglayers$IDPlots %in% plotIDs,]

  }

  return(result)


}


################################################################################################

selectScale <- function(nameScale = "CoverVeglayers"){

  scaleInfo <- read.csv2(schaalInfo)

  result <- scaleInfo %>%
    filter(Schaal == nameScale)

  return(result)

}


################################################################################################

### Haal bedekking van de afzonderlijke vegetatielagen in 16x16m proefvlak uit VBI2 databank

getCoverVeglayersVBI2 <- function (db =  dbVBI2, plotIDs = NULL) {

  query_veglayers <- "SELECT
  Vegetation.IDPlots,
  Vegetation.ID,
  Vegetation.Total_herb_cover,
  Vegetation.Total_moss_cover,
  Vegetation.Total_shrub_cover,
  Vegetation.Total_tree_cover,
  Vegetation.Total_cover
  FROM Vegetation;"


  connectieVBI2 <- odbcConnectAccess2007(db) #dit is een mdb file

  veglayerOrig <- sqlQuery(connectieVBI2, query_veglayers, stringsAsFactors = TRUE)

  odbcClose(connectieVBI2)

  veglayer <- veglayerOrig %>%
    rename(IDSegments = ID) %>%
    filter(IDSegments == 1)

  scaleInfo <- selectScale("CoverVeglayers") %>%
    select(CoverID = KlasseID, Cover = BedekkingGem)

  veglayer_Cover <- veglayer %>%
    select(-Total_cover, -IDSegments) %>%
    gather(-IDPlots, key = "Layer", value = "CoverID") %>%
    left_join(scaleInfo, by = "CoverID") %>%
    select(-CoverID) %>%
    spread(key = "Layer", value = "Cover") %>%
    rename(CoverHerblayer = Total_herb_cover,
           CoverMosslayer = Total_moss_cover,
           CoverShrublayer = Total_shrub_cover,
           CoverTreelayer = Total_tree_cover) %>%
    mutate(CoverTreeAndShrublayer = (1 - (1 - CoverTreelayer/100) * (1 - CoverShrublayer/100)) * 100,
           CoverHerbAndMosslayer =  CoverHerblayer + CoverMosslayer)


  if (is.null(plotIDs)){

    result <- veglayer_Cover

  } else {

    result <- veglayer_Cover %>%
      filter(IDPlots %in% plotIDs)
  }

  return(result)

}

###############################################################################################################################

################################################################################################

### Haal bedekking van de afzonderlijke vegetatielagen in 16x16m proefvlak uit VBI1 databank

getCoverVeglayersVBI1 <- function (db =  dbVBI1_veg, plotIDs = NULL) {

  query_veglayers <- "SELECT
    KOP.Opnamenummer,
    KOP.[Bedekking kruidlaag],
    KOP.[Bedekking moslaag],
    KOP.[Bedekking struiklaag],
    KOP.[Bedekking boomlaag]
    FROM KOP;"

  connectieVBI1 <- odbcConnectAccess2007(db) #dit is een mdb file

  veglayerOrig <- sqlQuery(connectieVBI1, query_veglayers, stringsAsFactors = TRUE)

  odbcClose(connectieVBI1)

  veglayer <- veglayerOrig %>%
    rename(IDPlots = Opnamenummer,
          CoverHerblayer = "Bedekking kruidlaag",
          CoverMosslayer = "Bedekking moslaag",
          CoverShrublayer = "Bedekking struiklaag",
          CoverTreelayer = "Bedekking boomlaag")

  veglayer$CoverTreeAndShrublayer <- (1 - (1 - veglayer$CoverTreelayer/100) * (1 - veglayer$CoverShrublayer/100))*100

  if (is.null(plotIDs)){

    result <- veglayer

  } else {

    result <- veglayer[veglayer$IDPlots %in% plotIDs,]

  }

  return(result)

}

###############################################################################################################################




# db <- dbHeideEn6510_2016

getCoverVeglayersMHK <- function(db = dbHeideEn6510_2014_2015, plotIDs = NULL){

  query_CoverPlot <- "
  SELECT
  VegPQ.IDPlots,
  VegPQ.ID,
  VegPQ.HAB1,
  qHABITAT.Value1,
  VegPQ.Licheneslayer,
  VegPQ.Sphagnumlayer,
  VegPQ.OtherMosslayer,
  VegPQ.Herblayer,
  VegPQ.Shrublayer,
  VegPQ.Treelayer,
  VegPQ.Shrub_and_Treelayer
  FROM VegPQ LEFT JOIN qHABITAT ON VegPQ.HAB1 = qHABITAT.ID;"


   if(substr(db,nchar(db)-3,nchar(db)) == ".mdb"){
    connectieDB <-   odbcConnectAccess(db)
  } else if (substr(db,nchar(db)-5,nchar(db)) == ".accdb") {
    connectieDB <-   odbcConnectAccess2007(db)
  }

  coverPlots <- sqlQuery(connectieDB, query_CoverPlot, stringsAsFactors = TRUE)

  odbcClose(connectieDB)

  coverPlots$CoverMosslayer <- coverPlots$Sphagnumlayer + coverPlots$OtherMosslayer

  coverPlots <- plyr::rename(coverPlots, c(Value1 = "HabObservedPQ",Herblayer = "CoverHerblayer", Shrublayer = "CoverShrublayer", Treelayer = "CoverTreelayer", Shrub_and_Treelayer = "CoverTreeAndShrublayer" ))

  # een vegetatieopname per plot

  coverPlots <- coverPlots %>%
   filter(ID == 1)

  coverPlots <- coverPlots[,c("IDPlots","HabObservedPQ","CoverHerblayer","CoverMosslayer", "CoverShrublayer", "CoverTreelayer", "CoverTreeAndShrublayer")]

   coverPlots$HabObservedPQ <- revalue(coverPlots$HabObservedPQ,c('6510 hu'="6510_hu","2330 bu" = "2330_bu", "6510 hua" = "6510_hua", "6510 hus" = "6510_hus", "6510 huk" = "6510_huk"))

  if (is.null(plotIDs)){

    result <- coverPlots

  } else {

    result <-coverPlots[coverPlots$IDPlots %in% plotIDs,]

  }

  return(result)

}

##########################################################################♣
### FUNCTIES VOOR RAADPLEGEN STEEKPROEF
##########################################################################♣
#
# dirSampleHab <- dirSample
#  fileSample <- sampleHeide

getSample <- function(dirSampleHab = dirSample, fileSample){

sample <-  readOGR(dsn = dirSampleHab, layer = fileSample, verbose = FALSE)
sample <- sample@data

if("orthocheck" %in% colnames(sample)){
  sample <- sample %>%
    rename(Orthocontr = orthocheck)
}

if("Orthocontr" %in% colnames(sample)){

sample <- select(sample, ID,  IDPlots = Ranking, nb  ,HabTarget1 = habsubt, HabTarget2 = Doelhab2, Year = year, SBZH, Orthocontr, x, y )

} else {

   sample <- select(sample, ID,  IDPlots = Ranking, nb, HabTarget1 = habsubt, HabTarget2 = Doelhab2, Year = year, SBZH, x, y)

}

return(sample)

}

##########################################################################♣

getSampleSize <- function(sampleSizeCalc = sampleSizeCalcFile, habtypes = "All") {

  sampleSize <- read.table(sampleSizeCalc, header = TRUE, dec = ",")

if(habtypes == "All"){

  sampleSizeSelect <- sampleSize

} else {

  sampleSizeSelect <- sampleSize[sampleSize$habsubt %in% habtypes,]
}


sampleSizeSelect$nBezoekenSBZH <- sampleSizeSelect$n_habt_SBZH_bruto + sampleSizeSelect$extra_habsubt_SBZH_bruto
sampleSizeSelect$nGewenstSBZH <- sampleSizeSelect$n_habt_SBZH_netto + sampleSizeSelect$extra_habsubt_SBZH_netto

sampleSizeSelect$nBezoekenBuiten <- sampleSizeSelect$n_habt_buiten_bruto + sampleSizeSelect$extra_habsubt_buiten_bruto
sampleSizeSelect$nGewenstBuiten <- sampleSizeSelect$n_habt_buiten_netto + sampleSizeSelect$extra_habsubt_buiten_netto

#long formaat
sampleSizeSelect_long1 <- sampleSizeSelect %>%
  select(habsubt, Binnen = nBezoekenSBZH, Buiten = nBezoekenBuiten ) %>%
  gather(Binnen, Buiten, key = "SBZH", value = "nBezoeken")

sampleSizeSelect_long2 <- sampleSizeSelect %>%
  select(habsubt, Binnen = nGewenstSBZH, Buiten = nGewenstBuiten ) %>%
  gather(Binnen, Buiten, key = "SBZH", value = "nGewenst")

sampleSizeSelect_long3 <- sampleSizeSelect %>%
  select(habsubt, Binnen = prop_hab_SBZH, Buiten = prop_hab_buiten ) %>%
  gather(Binnen, Buiten, key = "SBZH", value = "Trefkans_verwacht")

sampleSizeSelect_long <- sampleSizeSelect_long1 %>%
  left_join(sampleSizeSelect_long2, by = c("habsubt", "SBZH")) %>%
  left_join(sampleSizeSelect_long3, by = c("habsubt", "SBZH"))

return (sampleSizeSelect_long)

}



######################################################################################
### FUNCTIES VOOR OPHALEN DENDROMETRISCHE GEGEVENS VOOR BEREKENING INDICATOREN HABITATSTRUCTUUR BOSSEN
#######################################################################################
getTreesA3A4RawData <- function (db = dbVBI2, plotIDs = NULL){

  query_trees<-"
  SELECT Trees_2eBosinv.IDPlots,
  Trees_2eBosinv.ID,
  Trees_2eBosinv.Perimeter_cm,
  Trees_2eBosinv.DBH_mm,
  Trees_2eBosinv.Height_m,
  Trees_2eBosinv.Species,
  qTreeSpecies.Value1,
  Trees_2eBosinv.Status_tree,
  qStatusTree.Value1,
  Trees_2eBosinv.CodeCoppice_Individual,
  qCoppice_Individual.Value1,
  Trees_2eBosinv.IntactTree,
  qIntactTree.Value1
  FROM (((Trees_2eBosinv LEFT JOIN qTreeSpecies ON Trees_2eBosinv.Species = qTreeSpecies.ID)
  LEFT JOIN qStatusTree ON Trees_2eBosinv.Status_tree = qStatusTree.ID)
  LEFT JOIN qCoppice_Individual ON Trees_2eBosinv.CodeCoppice_Individual = qCoppice_Individual.ID)
  LEFT JOIN qIntactTree ON Trees_2eBosinv.IntactTree = qIntactTree.ID;
  "

  connectieVBI2 <- odbcConnectAccess2007(db)
  treesA3A4Orig <- sqlQuery(connectieVBI2, query_trees, stringsAsFactors = TRUE)
  odbcClose(connectieVBI2)

  treesA3A4 <- rename(treesA3A4Orig, IDTreeSp = Species,
                                    Species = Value1,
                                    StatusTreeCode = Status_tree,
                                    StatusTree = Value1.1,
                                    Coppice_IndividualCode = CodeCoppice_Individual,
                                    Coppice_Individual = Value1.2,
                                    IntactTreeCode = IntactTree,
                                    IntactTree = Value1.3)
  if (is.null(plotIDs)){

    result <- treesA3A4

  } else {

    result <- treesA3A4 %>%
      filter(IDPlots %in% plotIDs)

  }

  return(result)
}







#'Haal gegevens over de bomen uit de A3A4 - plots van de tweede Vlaamse bosinventarisatie
#'
#'Deze functie haalt meetresultaten en soortgegevens over bomen uit de A3- en de A4-plots van #'de tweede Vlaamse bosinventarisatie.Ontbrekende hoogtes worden vervangen door de mediaan van #'de boomhoogtes binnen een plot.Bomen met ontbrekende waarden voor 'status' beschouwen we als #'levende bomen; Bomen met ontbrekende waarden voor 'individueel/hakhout' beschouwen we als een #'individuele stam.
#'Op basis van gegevens uit de databank 'VBImeetproces' wordt elke boom aan een segment binnen #'een plot toegekend en wordt aan elk segment de oppervlakte toegevoegd van het deel van de A4 #'en de A3 plot dat binnen dit segment valt. Op basis van deze oppervlaktes worden de
#'expansiecoefficienten bepaald.
#'
#'@param db Databank met meetgegevens (defaultwaarde = dbVBI2; te definiëren in 'Omgeveingsvariabelen.R')
#'@param dbMeetproc Databank met gegevens over meetproces (defaultwaarde = dbMeetproces; te definiëren in 'Omgeveingsvariabelen.R')
#'@param plotIDs ID's van plots waarvoor gegevens worden opgehaald (default: alle gegevens uit gespecifieerde databank)
#'
#'
#'@return dataframe met velden DataSet, IDPlots, IDSegments, ID (ID voor boom),
#' AreaA4_m2 en AreaA3_m2 (per segment),DBH_mm, Perimeter_cm, Height_m,
#' IDTree (ID voor boomsoort), Alive(1=levend, 0=dood), NameNl,
#' IntactTreeCode (10 = intacte boom, 20 = niet-intacte boom),
#' Coppice_IndividualCode (10=individuele stam, 20= hakhout), NameNl,
#' IsAutochtoon (1=ja, 0=nee), Genus en Species.
#'
#'@export
#'

getTreesA3A4VBI2 <- function (db = dbVBI2, dbMeetproc = dbVBIMeetproces, plotIDs = NULL){

  connectieMetadata <- odbcConnectAccess2007(dbMeetproc)

  #tabel met plotgewichten en segmentgewichten en oppervlaktes van A2, A3 en A4 plots
  plotWeights <- sqlFetch(connectieMetadata,"tblPlotWeights")

  #tabel met er boom de ID van het segment waarbinnen de boom valt
  treesSegmentID <- sqlFetch(connectieMetadata,"tblTreesSegmentID")

  odbcClose(connectieMetadata)

  query_trees<-"
  SELECT Trees_2eBosinv.IDPlots,
  Trees_2eBosinv.ID,
  Trees_2eBosinv.Perimeter_cm,
  Trees_2eBosinv.DBH_mm,
  Trees_2eBosinv.Height_m,
  Trees_2eBosinv.Species,
  qTreeSpecies.Value1,
  Trees_2eBosinv.Status_tree,
  qStatusTree.Value1,
  Trees_2eBosinv.CodeCoppice_Individual,
  qCoppice_Individual.Value1,
  Trees_2eBosinv.IntactTree,
  qIntactTree.Value1
  FROM (((Trees_2eBosinv LEFT JOIN qTreeSpecies ON Trees_2eBosinv.Species = qTreeSpecies.ID)
  LEFT JOIN qStatusTree ON Trees_2eBosinv.Status_tree = qStatusTree.ID)
  LEFT JOIN qCoppice_Individual ON Trees_2eBosinv.CodeCoppice_Individual = qCoppice_Individual.ID)
  LEFT JOIN qIntactTree ON Trees_2eBosinv.IntactTree = qIntactTree.ID;
  "

  connectieVBI2 <- odbcConnectAccess2007(db)
  treesA3A4Orig <- sqlQuery(connectieVBI2, query_trees, stringsAsFactors = TRUE)
  odbcClose(connectieVBI2)

  treesA3A4 <- rename(treesA3A4Orig, IDTreeSp = Species,
                                    Species = Value1,
                                    StatusTreeCode = Status_tree,
                                    StatusTree = Value1.1,
                                    Coppice_IndividualCode = CodeCoppice_Individual,
                                    Coppice_Individual = Value1.2,
                                    IntactTreeCode = IntactTree,
                                    IntactTree = Value1.3)

  #Bomen met ID=0 verwijderen
  treesA3A4 <- treesA3A4 %>%
    filter(ID>0) %>%
    left_join(treesSegmentID, by=c("IDPlots","ID")) %>%
    mutate(IDSegments = ifelse(is.na(IDSegments), 1, IDSegments))
  # ontbrekende waarde voor status --> we veronderstemmen levende boom; ontbrekende waarde voor hakhout-individueel --> we veronderstellen individuele boom; ontbrekende waarde voor intact/niet-intacte boom --> we veronderstellen een intacte boom

  treesA3A4[is.na(treesA3A4$StatusTreeCode),]$StatusTreeCode <- 1
  treesA3A4[is.na(treesA3A4$StatusTree),]$StatusTree<- "levend"

  treesA3A4[is.na(treesA3A4$Coppice_IndividualCode),]$Coppice_IndividualCode<-10
  treesA3A4[is.na(treesA3A4$Coppice_Individual),]$Coppice_Individual<- "Individuele boom"

  treesA3A4[is.na(treesA3A4$IntactTreeCode),]$IntactTreeCode<-10
  treesA3A4[is.na(treesA3A4$IntactTree),]$IntactTree<- "Intacte boom"

  #### Bijschatten ontbrekende hoogtes: mediaan van boomhoogte per plot

  plots_medianHeight<-ddply(treesA3A4,.(IDPlots,Periode),summarise,
                            medianHeight=median(Height_m,na.rm=TRUE))


  #Zet de bomen waarvoor geen Height_m gekend gelijka aan de mediaanhoogte van de bomen in het plot
  treesA3A4<-merge(treesA3A4,plots_medianHeight,by=c("IDPlots","Periode"), all.x=TRUE) #gewijzigd naar all.x
  treesA3A4$Height_m<-ifelse(is.na(treesA3A4$Height_m),treesA3A4$medianHeight,treesA3A4$Height_m)

  treesA3A4$Height_m <- ifelse(is.na(treesA3A4$Height_m), mean(treesA3A4$Height_m,na.rm=TRUE),treesA3A4$Height_m)

  #enkel bomen selecteren die in segmenten aangeduid als bos vallen
  #'plotWeightsVBI2' bevat, per segment met bos, de oppervlaktes van het deel van de A3 en A4 plot dat in het segment valt. 'treesA3A4' bevat ook enkele bomen die foutief gelocaliseerd zijn in segmenten zonder bos. Via een inner join, verwijderen we deze foutief gelocaliseerde bomen.

  treesA3A4 <- merge(treesA3A4,plotWeights,by=c("IDPlots","IDSegments"))

  treesA3A4$DataSet <- "VBI2"

  treesA3A4$Alive <- treesA3A4$StatusTreeCode==1

  treesA3A4 <- treesA3A4[!is.na(treesA3A4$IDPlots),c("DataSet","IDPlots","IDSegments","ID","AreaA4_m2","AreaA3_m2","Perimeter_cm","DBH_mm","Height_m","IDTreeSp","Alive","IntactTreeCode","Coppice_IndividualCode")]


  #extra informatie over boomsoorten toevoegen

  connectieExterneData <- odbcConnectAccess2007(dbExterneData)

    treeList <- sqlFetch(connectieExterneData,"tblTreeSpeciesCharacteristics")

  odbcClose(connectieExterneData)

  treesA3A4 <- merge(treesA3A4,treeList,by="IDTreeSp",all.x=TRUE)


  if (is.null(plotIDs)){

    result <- treesA3A4

  } else {

    result <- treesA3A4[treesA3A4$IDPlots %in% plotIDs,]

  }

  return(result)

}

#############################################################################################################

#'Haal gegevens over de bomen uit de A3A4 - plots van de eerste Vlaamse bosinventarisatie
#'
#'Deze functie haalt meetresultaten en soortgegevens over bomen uit de A3- en de A4-plots van #'de eerst Vlaamse bosinventarisatie.Ontbrekende hoogtes worden vervangen door de mediaan van #'de boomhoogtes binnen een plot.Bomen met ontbrekende waarden voor 'status' beschouwen we als #'levende bomen;

#'
#'@param db Databank met meetgegevens (defaultwaarde = dbVBI2; te definiëren in 'Omgeveingsvariabelen.R')
#'@param dbMeetproc Databank met gegevens over meetproces (defaultwaarde = dbMeetproces; te definiëren in 'Omgeveingsvariabelen.R')
#'@param plotIDs ID's van plots waarvoor gegevens worden opgehaald (default: alle gegevens uit gespecifieerde databank)
#'
#'
#'@return dataframe met velden DataSet, IDPlots, IDSegments, ID (ID voor boom),
#' AreaA4_m2 en AreaA3_m2 (per segment),DBH_mm, Perimeter_cm, Height_m,
#' IDTree (ID voor boomsoort), Alive(1=levend, 0=dood), NameNl,
#' IntactTreeCode (10 = intacte boom, 20 = niet-intacte boom),
#' Coppice_IndividualCode (10=individuele stam, 20= hakhout), NameNl,
#' IsAutochtoon (1=ja, 0=nee), Genus en Species.
#'
#'@export
#'

getTreesA3A4VBI1 <- function (db = dbVBI1, dbMeetproc = dbMeetproces, plotIDs = NULL){

  query_trees<-"
SELECT
tblA34.PLOTNR
, tblA34.BOOMNR
, tblA34.BOOMSOORT
, tblBoomsoorten.NAAM,tblA34.AFSTAND
, tblA34.OMTREK
, tblA34.HOOGTE
, tblA34.DOOD
FROM tblBoomsoorten
RIGHT JOIN tblA34
ON tblBoomsoorten.BOOMSOORTID = tblA34.BOOMSOORT
;"

  connectieVBI1<- odbcConnectAccess2007(db)
  treesA3A4Orig <- sqlQuery(connectieVBI1, query_trees)
  convC130 <- sqlFetch(connectieVBI1,"tblCoefOmzetOmtrek")
  odbcClose(connectieVBI1)

  treesA3A4 <- plyr::rename(treesA3A4Orig,c(PLOTNR="IDPlots",
                                  BOOMNR="ID",
                                  BOOMSOORT="IDTreeSp",
                                  NAAM="Species",
                                  OMTREK="Perimeter_cm",
                                  HOOGTE="Height_m",
                                  DOOD="StatusTreeCode"
  ))

  #codering dood hout gelijk stellen aan VBI2
  treesA3A4$StatusTreeCode <- treesA3A4$StatusTreeCode+1

  #conversie omtrek op 1,5m naar 1,3m
  convC130 <- plyr::rename(convC130,c(BOOMSOORTID="IDTreeSp"))
  treesA3A4 <- merge(treesA3A4,convC130,by="IDTreeSp",all.x=TRUE)
  treesA3A4$C130 <- treesA3A4$A+treesA3A4$B * treesA3A4$Perimeter_cm
  treesA3A4$Perimeter_cm <- treesA3A4$C130

  ### Bijschatten van hoogtes = 0 (slechts 4 bomen; normaal alle hoogtes opgemeten): mediaan van boomhoogte per plot
  # enkel levende bomen

  plots_medianHeight <- treesA3A4 %>%
    filter(StatusTreeCode == 1) %>%
    dplyr::group_by(IDPlots) %>%
    dplyr::summarise(medianHeight = median(Height_m,na.rm=TRUE) )

  treesA3A4 <- left_join(treesA3A4, plots_medianHeight, by="IDPlots")

  # !! enkel voor de levende bomen met hoogte = 0 (alle dode bomen standaard hoogte = 0)
  treesA3A4$Height_m <- ifelse((treesA3A4$Height_m == 0 & treesA3A4$StatusTreeCode == 1),treesA3A4$medianHeight,treesA3A4$Height_m)
  # (c)A: blijkbaar ook 13 NA's voor hoogte (+ als na controle blijkt dat een boom levend is, dan is de hoogte in "bosinv1_2011Val.accdb" plots niet meer "0", maar "NA")
  treesA3A4$Height_m <- ifelse((is.na (treesA3A4$Height_m) & treesA3A4$StatusTreeCode == 1),treesA3A4$medianHeight,treesA3A4$Height_m)

  ###ontberekende velden

  treesA3A4$IntactTreeCode <- 10

  #inA3A4 cirkel wordt geen hakhout opgemeten; enkel individuele bomen (code = 10)
  treesA3A4$Coppice_IndividualCode <- 10

  #plots worden niet opgesplitst in VBI1
  treesA3A4$IDSegments <- 1
  treesA3A4$AreaA4_m2 <- pi * 18 * 18
  treesA3A4$AreaA3_m2 <- pi * 9 * 9
  treesA3A4$DBH_mm <- treesA3A4$Perimeter_cm*10/pi

  treesA3A4$DataSet <- "VBI1"

  treesA3A4$Alive <- treesA3A4$StatusTreeCode==1

  treesA3A4 <- treesA3A4[!is.na(treesA3A4$IDPlots),c("DataSet","IDPlots","IDSegments","ID","AreaA4_m2","AreaA3_m2","Perimeter_cm","DBH_mm","Height_m","IDTreeSp","Alive","IntactTreeCode","Coppice_IndividualCode")]


  #extra informatie over boomsoorten toevoegen

  connectieExterneData <- odbcConnectAccess2007(dbExterneData)

    treeList <- sqlFetch(connectieExterneData,"tblTreeSpeciesCharacteristics")

  odbcClose(connectieExterneData)

  treesA3A4 <- merge(treesA3A4,treeList,by="IDTreeSp",all.x=TRUE)


  if (is.null(plotIDs)){

    result <- treesA3A4

  } else {

    result <- treesA3A4[treesA3A4$IDPlots %in% plotIDs,]

  }

  return(result)

}

#############################################################################################################
#############################################################################################################
getTreesA3A4MHK <- function (db = dbBosExtra,  plotIDs = NULL){

  query_trees <- "
  SELECT Trees_2eBosinv.IDPlots,
  Trees_2eBosinv.ID,
  Trees_2eBosinv.Perimeter_cm,
  Trees_2eBosinv.DBH_mm,
  Trees_2eBosinv.Height_m,
  Trees_2eBosinv.Species,
  qTreeSpecies.Value1,
  Trees_2eBosinv.Status_tree,
  qStatusTree.Value1,
  Trees_2eBosinv.CodeCoppice_Individual,
  qCoppice_Individual.Value1,
  Trees_2eBosinv.IntactTree,
  qIntactTree.Value1
  FROM (((Trees_2eBosinv LEFT JOIN qTreeSpecies ON Trees_2eBosinv.Species = qTreeSpecies.ID)
  LEFT JOIN qStatusTree ON Trees_2eBosinv.Status_tree = qStatusTree.ID)
  LEFT JOIN qCoppice_Individual ON Trees_2eBosinv.CodeCoppice_Individual = qCoppice_Individual.ID)
  LEFT JOIN qIntactTree ON Trees_2eBosinv.IntactTree = qIntactTree.ID;
  "

  connectieMHK <- odbcConnectAccess2007(db)
  treesA3A4Orig <- sqlQuery(connectieMHK, query_trees, stringsAsFactors = TRUE)
  odbcClose(connectieMHK)

  plotWeights <- getPlotWeights(db)

  treeSegmentsOrig <- read.csv2(treeSegmentsFile)
  treeSegments <- treeSegmentsOrig %>%
    rename(IDPlots = IDPLOTS, ID = ID_TREE, IDSegments = IDSEGMENT)

  treesA3A4 <- treesA3A4Orig %>%
    rename(IDTreeSp = Species,
          Species = Value1,
          StatusTreeCode = Status_tree,
          StatusTree = Value1.1,
          Coppice_IndividualCode = CodeCoppice_Individual,
          Coppice_Individual = Value1.2,
          IntactTreeCode = IntactTree,
          IntactTree = Value1.3) %>%
    filter(ID > 0) %>% #Bomen met ID=0 verwijderen
    left_join(treeSegments, by = c("IDPlots", "ID")) %>%  #voor elke boom aangeven in welk segment het valt
    left_join(plotWeights, by = c("IDPlots","IDSegments")) %>% # grootte A3 en A4 plot/segment toevoegen --> bepaalt expansiefactor
    mutate(StatusTreeCode = ifelse(is.na(StatusTreeCode), 1 , StatusTreeCode),
           StatusTree = ifelse(is.na(StatusTree), "levend" , as.character(StatusTree)),# ontbrekende waarde voor status --> we veronderstellen levende boom;
           Coppice_IndividualCode = ifelse(is.na(Coppice_IndividualCode), 10 , Coppice_IndividualCode),
           Coppice_Individual = ifelse(is.na(Coppice_Individual), "Individuele boom" , as.character(Coppice_Individual)), #ontbrekende waarde voor hakhout-individueel --> we veronderstellen individuele boom
           IntactTreeCode = ifelse(is.na(IntactTreeCode), 10 , IntactTreeCode),
           IntactTree = ifelse(is.na(IntactTree), "Intacte boom" , as.character(IntactTree))) %>% #ontbrekende waarde voor intact/niet-intacte boom --> we veronderstellen een intacte boom
    group_by(IDPlots) %>%
    mutate(medianHeight = median(Height_m,na.rm=TRUE)) %>% # Bijschatten ontbrekende hoogtes: mediaan van boomhoogte per plot
    ungroup() %>%
    mutate(Height_m = ifelse(is.na(Height_m), medianHeight, Height_m),
           Alive = StatusTreeCode == 1) %>%
    select(IDPlots, IDSegments, ID, AreaA4_m2, AreaA3_m2, Perimeter_cm, DBH_mm, Height_m, IDTreeSp, Alive, StatusTree, IntactTreeCode, IntactTree, Coppice_IndividualCode, Coppice_Individual)


  connectieExterneData <- odbcConnectAccess2007(dbVBIExterneData)

  treeList <- sqlFetch(connectieExterneData,"tblTreeSpeciesCharacteristics")

  odbcClose(connectieExterneData)

  treesA3A4 <- treesA3A4 %>%
    left_join(treeList, by = "IDTreeSp")


  if (is.null(plotIDs)){

    result <- treesA3A4

  } else {

    result <- treesA3A4[treesA3A4$IDPlots %in% plotIDs,]

  }

  return(result)

}

#############################################################################################################


### Haal gegevens op van A2-bomen uit VBI2-databank

getTreesA2VBI2 <- function (db =  dbVBI2, plotIDs = NULL){

  query_treesA2<-"
SELECT
  Doorgroeiende_verjonging.IDPlots
  , Doorgroeiende_verjonging.Species
  , qTreeSpecies.Value1
  , Doorgroeiende_verjonging.Number
  FROM Doorgroeiende_verjonging
  LEFT JOIN qTreeSpecies
  ON Doorgroeiende_verjonging.Species = qTreeSpecies.ID
  ;"

  connectieVBI2 <- odbcConnectAccess2007(db) #dit is een mdb file

    treesA2Orig<-sqlQuery(connectieVBI2, query_treesA2, stringsAsFactors = TRUE)

  odbcClose(connectieVBI2)

  treesA2 <- rename(treesA2Orig,
                    IDTreeSp = Species,
                    Species = Value1)

  #extra informatie over boomsoorten toevoegen

  connectieExterneData <- odbcConnectAccess2007(dbVBIExterneData)

  treeList <- sqlFetch(connectieExterneData,"tblTreeSpeciesCharacteristics")

  odbcClose(connectieExterneData)

  treesA2_detail <- treesA2 %>%
    filter(!is.na(IDTreeSp)) %>%
    left_join(treeList, by = "IDTreeSp")

  if (is.null(plotIDs)){

    result <- treesA2_detail

  } else {

    result <- treesA2_detail %>%
      filter(IDPlots %in% plotIDs)

  }

  return(result)

  }

##############################################################################################

### Haal gegevens op van hakhoutspillen uit VBI2 databank


getShootsVBI2 <- function(db =  dbVBI2, plotIDs = NULL){

query_shoots<-"
SELECT
  Shoots_2eBosinv.IDPlots
  , Shoots_2eBosinv.IDTrees_2eBosinv
  , Shoots_2eBosinv.ID
  , Shoots_2eBosinv.Perimeter_cm
  , Shoots_2eBosinv.Height_m
  , Shoots_2eBosinv.DBH_MM
  FROM Shoots_2eBosinv
  ;"

  connectieVBI2 <- odbcConnectAccess2007(db)

  shootsOrig <- sqlQuery(connectieVBI2, query_shoots, stringsAsFactors = TRUE);

  odbcClose(connectieVBI2)

  shoots <- plyr::rename(shootsOrig,c(ID="ShootID",IDTrees_2eBosinv="ID",Height_m="Height_shoot_m"))

  if (is.null(plotIDs)){

    result <- shoots

  } else {

    result <- shoots[shoots$IDPlots %in% plotIDs,]

  }

  return(result)


}

##############################################################################################

### Haal gegevens op van hakhoutspillen uit VBI1 databank


getShootsVBI1 <- function(db =  dbVBI1, plotIDs = NULL){

query_treesA2VBI1 <-"
  SELECT
  tblA2.PLOTNR
  , tblA2.BOOMNR
  , tblA2.IDTree
  , tblA2.AZIMUT
  , tblA2.AFSTAND
  , tblA2.BOOMSOORT
  , tblBoomsoorten.NAAM
  , tblA2.HOOGHOUT
  , tblA2.OMTREK
  , tblA2.DOOD
  FROM tblBoomsoorten
  RIGHT JOIN tblA2
  ON tblBoomsoorten.BOOMSOORTID = tblA2.BOOMSOORT
  ;"

  connectieVBI1 <- odbcConnectAccess2007(dbVBI1) #dit is een accdb file

  treesA2VBI1Orig <- sqlQuery(connectieVBI1, query_treesA2VBI1, stringsAsFactors = TRUE)
  convC130 <- sqlFetch(connectieVBI1,"tblCoefOmzetOmtrek")

  odbcClose(connectieVBI1)

  treesA2VBI1<-plyr::rename(treesA2VBI1Orig,c(PLOTNR="IDPlots",
                                      IDTree="ID",    # ipv BOOMNR
                                      BOOMSOORT="IDTreeSp",
                                      NAAM="Species",
                                      OMTREK="Perimeter_cm",
                                      DOOD="StatusTreeCode"))

  shootsVBI1 <- treesA2VBI1[treesA2VBI1$HOOGHOUT == 0 & treesA2VBI1$Perimeter_cm > 0,]
  shootsVBI1$Alive <- shootsVBI1$StatusTreeCode == 0

  #conversie omtrek op 1,5m naar 1,3m
  convC130 <- plyr::rename(convC130,c(BOOMSOORTID="IDTreeSp"))
  shootsVBI1 <- merge(shootsVBI1,convC130,by="IDTreeSp",all.x=TRUE)
  shootsVBI1$C130 <- shootsVBI1$A + shootsVBI1$B * shootsVBI1$Perimeter_cm
  shootsVBI1$Perimeter_cm <- shootsVBI1$C130



    #extra informatie over boomsoorten toevoegen

  connectieExterneData <- odbcConnectAccess2007(dbExterneData)

    treeList <- sqlFetch(connectieExterneData,"tblTreeSpeciesCharacteristics")

  odbcClose(connectieExterneData)

  shootsVBI1 <- merge(shootsVBI1,treeList,by="IDTreeSp",all.x=TRUE)



  if (is.null(plotIDs)){

    result <- shootsVBI1

  } else {

    result <- shootsVBI1[shootsVBI1$IDPlots %in% plotIDs,]

  }

  return(result)


}


### Haal gegevens A2-bomen (zonder hakhout) uit VBI1 databank


getTreesA2VBI1 <- function(db =  dbVBI1, plotIDs = NULL){

query_treesA2VBI1 <-"
  SELECT
  tblA2.PLOTNR
  , tblA2.BOOMNR
  , tblA2.IDTree
  , tblA2.AZIMUT
  , tblA2.AFSTAND
  , tblA2.BOOMSOORT
  , tblBoomsoorten.NAAM
  , tblA2.HOOGHOUT
  , tblA2.OMTREK
  , tblA2.DOOD
  FROM tblBoomsoorten
  RIGHT JOIN tblA2
  ON tblBoomsoorten.BOOMSOORTID = tblA2.BOOMSOORT
  ;"

  connectieVBI1 <- odbcConnectAccess2007(dbVBI1) #dit is een accdb file

  treesA2VBI1Orig <- sqlQuery(connectieVBI1, query_treesA2VBI1, stringsAsFactors = TRUE)

  odbcClose(connectieVBI1)

  treesA2VBI1 <- plyr::rename(treesA2VBI1Orig,c(PLOTNR="IDPlots",
                                      IDTree="ID",    # ipv BOOMNR
                                      BOOMSOORT="IDTreeSp",
                                      NAAM="Species",
                                      OMTREK="Perimeter_cm",
                                      DOOD="StatusTreeCode"))

  treesA2VBI1 <- treesA2VBI1 %>%
    dplyr::filter(Perimeter_cm == 0)

  treesA2VBI1$Alive <- treesA2VBI1$StatusTreeCode == 0



  if (is.null(plotIDs)){

    result <- treesA2VBI1

  } else {

    result <- treesA2VBI1[treesA2VBI1$IDPlots %in% plotIDs,]

  }

  return(result)


}



### Haal gegevens over liggend dood hout uit de VBI2-databank


getLogsVBI2 <- function (db = dbVBI2, plotIDs = NULL ) {

  query_LIM<-"SELECT LIM_data.IDPlots, LIM_data.IDLine_intersect_method, LIM_data.Diameter_cm, LIM_data.Angle_degrees
FROM LIM_data;
  "
  connectieVBI2 <- odbcConnectAccess2007(db)


  LIMOrig<- sqlQuery(connectieVBI2,query_LIM, stringsAsFactors = TRUE)

  odbcClose(connectieVBI2)


  logs <- LIMOrig

  Li<-45

  logs$Volume_ha <- pi^2/8/Li*(logs$Diameter_cm^2)/cos(logs$Angle_degrees*pi/180)

  if (is.null(plotIDs)){

    result <- logs

  } else {

    result <- logs[logs$IDPlots %in% plotIDs,]

  }

  return(result)

}

### Haal gegevens over liggend dood hout uit de VBI1-databank

getLogsVBI1 <- function (db = dbVBI1_veg, plotIDs = NULL ) {

  query_LogsVBI1<-" SELECT KOP.Opnamenummer,
    KOP.[7-22cm liggend dood hout],
    KOP.[>22cm liggend dood hout],
    KOP.[>40cm liggend dood hout]
    FROM KOP;"

  connectieVBI1 <- odbcConnectAccess2007(db) #dit is een accdb file
  logsVBI1Orig <- sqlQuery(connectieVBI1, query_LogsVBI1, stringsAsFactors = TRUE)
  odbcClose(connectieVBI1)

  logsVBI1 <- plyr::rename(logsVBI1Orig,c(Opnamenummer="IDPlots"
                                ,"7-22cm liggend dood hout"="LogLength_Diam_7_22_cm"
                                ,">22cm liggend dood hout"="LogLength_Diam_22_40_cm"
                                ,">40cm liggend dood hout"="LogLength_Diam_plus40_cm"))

  logsVBI1[is.na(logsVBI1)] <- 0

  logsVBI1$Volume_ha <- (logsVBI1$LogLength_Diam_7_22_cm*(14.5/100/2)^2*pi
                         + logsVBI1$LogLength_Diam_22_40_cm*(31/100/2)^2*pi
                         + logsVBI1$LogLength_Diam_plus40_cm*(60/100/2)^2*pi)/(16*16)*10000


  if (is.null(plotIDs)){

    result <- logsVBI1

  } else {

    result <- logsVBI1[logsVBI1$IDPlots %in% plotIDs,]

  }

  return(result)

}

######################################################################################
### FUNCTIES VOOR BEREKENING GRONDVLAK EN VOLUME --> NODIG VOOR INDICATOREN HABITATSTRUCTUUR BOSSEN
#######################################################################################

#### Bereking grondvlak en volume waarbij tarieven en aantal ingangen worden gespecifieerd. De berekeningen gebeuren op basis van een data.frame met de omtrekgegevens (default variabele naam ='Perimeter_cm') en hoogte-gegevens (in geval van 2 ingangen; default variabele naam = 'Height_m'). Aan deze data.frame worden de berekende volumes toegevoegd (default variabele naam = 'Volume') en de berekende grondvlakken (default variabele naam = 'BasalArea_m2')


calcVolumeAndBasalAreaTree <- function(treeMeasurements, tarieven, nIngang) {

  trees <- treeMeasurements %>%
    left_join(select(tarieven, -Species), by = "IDTreeSp") %>% #code soortnaam identiek voor verschillende periodes, maar soortnaam kan verschillen
    mutate( Radius_m = 1/100 * Perimeter_cm / (2 * pi),
            BasalArea_m2 = pi * Radius_m^2,
            Diameter_cm = Perimeter_cm/ pi)

  ### 2 ingangen
  if (nIngang == 2) {

    trees <- trees %>%
      mutate(Volume = ifelse(Formule_type == 1,
                             yes = a + b * Perimeter_cm + c * (Perimeter_cm^2) + d * (Perimeter_cm^3) + e*Height_m + f * Height_m * Perimeter_cm + g * Height_m * (Perimeter_cm^2),
                             no =  1/1000 *
                            #spil
                            (exp(1.10597 * log(Height_m) + 1.78865 * log(Diameter_cm) - 3.07192) -
                            #Verlies
                            exp(-4.608923 * log(Diameter_cm) + 3.005989 * log(Height_m) - 1.3209 * log(Height_m) * log(Height_m) + 1.605266 * log(Diameter_cm) * log(Height_m) + 5.410272))))

  } else if (nIngang == 1){

    trees <- trees %>%
      mutate(Volume = a + b * Perimeter_cm + c * (Perimeter_cm^2) + d *(Perimeter_cm^3))

  }

  trees <- trees %>%
    select(-a, -b, -c, -d, -e, -f, -g, -Formule_type, -Tarief, -groepNaam) %>%
    mutate(Volume = pmax(0,Volume))

  return(trees)
}


###################################################################################################

### Berkening volume en grondvlak op basis van hoogte- en omtrekgegevens VBI2

calculateVolumeAndBasalArea <- function(treesA3A4, shoots, dbExterneData = dbVBIExterneData){

  # tarieven

  query_tarieven2ing <-"
SELECT
  tblTariefgroepBoomsoort.ID
  , tblTariefgroepBoomsoort.Value
  , tblTarieven_2ing.Tarief
  , tblTarieven_2ing.groepNaam
  , tblTarieven_2ing.a
  , tblTarieven_2ing.b
  , tblTarieven_2ing.c
  , tblTarieven_2ing.d
  , tblTarieven_2ing.e
  , tblTarieven_2ing.f
  , tblTarieven_2ing.g
  , tblTarieven_2ing.Formule_type
  FROM tblTariefgroepBoomsoort
  INNER JOIN tblTarieven_2ing ON tblTariefgroepBoomsoort.TariefID = tblTarieven_2ing.groepID
  ;"

  connectieExterneData <- odbcConnectAccess2007(dbExterneData)

  tarieven2ingOrig <- sqlQuery(connectieExterneData, query_tarieven2ing, stringsAsFactors = TRUE)

  odbcClose(connectieExterneData)

  tarieven2ing <- tarieven2ingOrig %>%
    rename(IDTreeSp = ID, Species = Value)

  # volume van boom berekenen op basis van tarieven met 2 ingangen

  treesA3A4 <- treesA3A4 %>%
    calcVolumeAndBasalAreaTree(tarieven2ing, 2) %>%
    mutate(Volume = ifelse(IntactTree == "Niet intacte boom", BasalArea_m2 * Height_m, Volume), #aanpassen volume voor niet intacte bomen: volume cilinder
           Volume_ha =  ifelse(Perimeter_cm < 122, 10000 * Volume / AreaA3_m2, 10000 * Volume / AreaA4_m2), #expansiefactoren --> volume per ha
           BasalArea_ha = ifelse(Perimeter_cm < 122, BasalArea_m2 * 10000 / AreaA3_m2, BasalArea_m2 * 10000 / AreaA4_m2)) %>% #expansiefactoren --> grondvlak per ha
    arrange(IDPlots)


  #volume & grondvlak hakhout

  #om volume te berekenen per shoot hebben we gegevens nodig uit 'treesA3A4': boomsoort, hoogte hakhoutstoof, gewichten etc

  hakhout <- shoots %>%
    left_join(select(treesA3A4, -Perimeter_cm), by = c("IDPlots", "ID")) %>%
    calcVolumeAndBasalAreaTree(tarieven2ing,2) %>%
     mutate(Volume_ha =  ifelse(Perimeter_cm < 122, 10000 * Volume / AreaA3_m2, 10000 * Volume / AreaA4_m2), #expansiefactoren --> volume per ha
           BasalArea_ha = ifelse(Perimeter_cm < 122, BasalArea_m2 * 10000 / AreaA3_m2, BasalArea_m2 * 10000 / AreaA4_m2)) %>%
    group_by(IDPlots, ID) %>%
    summarise(Volume_ha_hakhout = sum(Volume_ha,na.rm = TRUE),
              BasalArea_ha_hakhout = sum(BasalArea_ha, na.rm = TRUE),
              MaxPerimeter_cm = max(Perimeter_cm))

  # volumes en grondvlak per boom voor alle bomen

  allTreesA3A4 <- treesA3A4 %>%
    left_join(hakhout, by = c("IDPlots", "ID")) %>%
    mutate(Volume_ha = ifelse(!is.na(Volume_ha_hakhout), Volume_ha_hakhout, Volume_ha),
           BasalArea_ha = ifelse(!is.na(BasalArea_ha_hakhout), BasalArea_ha_hakhout, BasalArea_ha),
           Perimeter_cm = ifelse(!is.na(MaxPerimeter_cm), MaxPerimeter_cm, Perimeter_cm)) %>%
    mutate(Volume_ha = pmax(0, Volume_ha)) %>%  #negatieve volumes = 0
    select(-BasalArea_m2, -Volume, -Volume_ha_hakhout, -BasalArea_ha_hakhout, -MaxPerimeter_cm, -Radius_m)

  return (allTreesA3A4)

}

##############################################################################

### Berkening volume en grondvlak op basis van hoogte- en omtrekgegevens VBI1

calculateVolumeAndBasalAreaVBI1 <- function(treesA3A4, shoots){

  # tarieven

  query_tarieven2ing <-"
SELECT
  tblTariefgroepBoomsoort.ID
  , tblTariefgroepBoomsoort.Value
  , tblTarieven_2ing.Tarief
  , tblTarieven_2ing.groepNaam
  , tblTarieven_2ing.a
  , tblTarieven_2ing.b
  , tblTarieven_2ing.c
  , tblTarieven_2ing.d
  , tblTarieven_2ing.e
  , tblTarieven_2ing.f
  , tblTarieven_2ing.g
  , tblTarieven_2ing.Formule_type
  FROM tblTariefgroepBoomsoort
  INNER JOIN tblTarieven_2ing ON tblTariefgroepBoomsoort.TariefID = tblTarieven_2ing.groepID
  ;"

  query_tarieven1ing<-"
SELECT tblTariefgroepBoomsoort.ID
, tblTariefgroepBoomsoort.Value
, tblTariefgroepBoomsoort.TariefID
, tblTarieven_1ing.groepNaam
, tblTarieven_1ing.a
, tblTarieven_1ing.b
, tblTarieven_1ing.c
, tblTarieven_1ing.d
, tblTarieven_1ing.Tarief
FROM tblTariefgroepBoomsoort
LEFT JOIN tblTarieven_1ing ON tblTariefgroepBoomsoort.TariefID = tblTarieven_1ing.groepID
;"

  connectieExterneData <- odbcConnectAccess2007(dbExterneData)
  tarieven2ingOrig <- sqlQuery(connectieExterneData, query_tarieven2ing, stringsAsFactors = TRUE)
  tarieven1ingOrig <- sqlQuery(connectieExterneData, query_tarieven1ing, stringsAsFactors = TRUE)
  odbcClose(connectieExterneData)

  tarieven2ing <- plyr::rename(tarieven2ingOrig,c(ID="IDTreeSp",Value="Species"))
  tarieven1ing <- plyr::rename(tarieven1ingOrig,c(ID="IDTreeSp",Value="Species"))

  ### volume & grondvlak individuele bomen
  treesA3A4_levend <- treesA3A4 %>%
    filter(Alive | is.na(Alive)) %>%
    calcVolumeAndBasalAreaTree(tarieven2ing,2)

  #dood hout heeft steeds als hoogte 0 in databank --> tarieven met 1 ingang
  treesA3A4_dood <- treesA3A4 %>%
    filter(!Alive & !is.na(Alive)) %>%
    calcVolumeAndBasalAreaTree(tarieven1ing,1)

  treesA3A4 <- bind_rows(treesA3A4_levend, treesA3A4_dood)

  #expansiefactoren --> volume per ha
  treesA3A4$Volume_ha <- ifelse(treesA3A4$Perimeter_cm < 122,
                                  10000 * treesA3A4$Volume / treesA3A4$AreaA3_m2,
                                  10000 * treesA3A4$Volume / treesA3A4$AreaA4_m2)

  #expansiefactoren -->  grondvlak per hectare
  treesA3A4$BasalArea_ha <- ifelse(treesA3A4$Perimeter_cm < 122,
                                 10000 * treesA3A4$BasalArea_m2/treesA3A4$AreaA3_m2,
                                 10000 * treesA3A4$BasalArea_m2/treesA3A4$AreaA4_m2)

  treesA3A4 <- treesA3A4[order(treesA3A4$IDPlots),]

  ### volume & grondvlak per shoot
  shootsA2 <- calcVolumeAndBasalAreaTree(shoots,tarieven1ing,1)
  shootsA2$AreaA2_m2 <- pi * 4.5 * 4.5

  #expansiefactoren voor A2 plot
  shootsA2$Volume_ha <- 10000 * shootsA2$Volume / shootsA2$AreaA2_m2
  shootsA2$BasalArea_ha <- 10000 * shootsA2$BasalArea_m2 / shootsA2$AreaA2_m2

  ### volume & grondvlak per hakhoutstoof
  hakhout <- shootsA2 %>%
    dplyr::group_by(IDPlots, ID, Alive) %>%
    dplyr::summarise(Volume_ha = sum(Volume_ha, na.rm = TRUE),
              BasalArea_ha = sum(BasalArea_ha, na.rm = TRUE),
              Perimeter_cm = max(Perimeter_cm, na.rm =TRUE),
              IDTreeSp = unique(IDTreeSp)[1],
              NameNl = unique(NameNl)[1],
              NameSc = unique(NameSc)[1],
              ExoticSpecies = unique(ExoticSpecies)[1],
              InvasiveSpecies = unique(InvasiveSpecies)[1],
              Genus = unique(Genus)[1],
              Spec = unique(Spec)[1]) %>%
    ungroup()

  hakhout$DataSet <- "VBI1"
  hakhout$IDSegments <- 1
  hakhout$Coppice_IndividualCode <- 20
  hakhout$IntactTreeCode <- 10
  hakhout$DBH_mm <- hakhout$Perimeter_cm/pi * 10

  trees_all <- treesA3A4 %>%
    dplyr::select(-AreaA4_m2, -AreaA3_m2, -BasalArea_m2, -D, -Volume, -TariefID) %>%
    bind_rows(hakhout)

  return (trees_all)

}


############################################################################################

############################################################################################


getStructurePlotHeide <- function(db = dbHeideEn6510_2014_2015){

  query_StructurePlotHeide <- "SELECT
  SiteDescription_HEIDE.IDPlots,
SiteDescription_HEIDE.Edit_date,
  SiteDescription_HEIDE.Shrub_and_Treelayer_18m,
  SiteDescription_HEIDE.Sphagnumlayer,
  SiteDescription_HEIDE.Campylopus_introflexus,
  SiteDescription_HEIDE.LowShrublayer,
  SiteDescription_HEIDE.Brushwood,
  SiteDescription_HEIDE.Herbs,
  SiteDescription_HEIDE.Calluna_phase_pioneer,
  SiteDescription_HEIDE.Calluna_phase_devel,
  SiteDescription_HEIDE.Calluna_phase_climax,
  SiteDescription_HEIDE.Calluna_phase_degen,
  SiteDescription_HEIDE.Pioneer_phase_open_soil,
  SiteDescription_HEIDE.Pioneer_Coryn_Aira,
  SiteDescription_HEIDE.Pioneer_Mos,
  SiteDescription_HEIDE.Pioneer_Lichenen
  FROM SiteDescription_HEIDE;"

  if(substr(db,nchar(db)-3,nchar(db)) == ".mdb"){
    connectieDB <-   odbcConnectAccess(db)
  } else if (substr(db,nchar(db)-5,nchar(db)) == ".accdb") {
    connectieDB <-   odbcConnectAccess2007(db)
  }

  structurePlots <- sqlQuery(connectieDB, query_StructurePlotHeide, stringsAsFactors = TRUE)

  #tansleyScale <- sqlFetch(connectieDB,"qCoverageTansley")

  odbcClose(connectieDB)

  structurePlots$Jaar <- as.numeric(format(as.Date(structurePlots$Edit_date), "%Y"))

  tansleyScale <- data.frame(TansleyCode = c(1,5,11,16,22,23),Scale ="Tansley", Class = c("zeldzaam","occasioneel","frequent","abundant","codominant","dominant" ), Cover = c(0.5, 2.5, 15, 27.5,45,75 ))

# een deel van de structuurvariabelen steeds via Tansley-schaal bepaald
  structurePlots_Tansley_1 <- structurePlots %>%
    select(IDPlots, Jaar, LowShrublayer, starts_with("Calluna")) %>%
    gather(-IDPlots, -Jaar, key = "StructureVar", value = "TansleyCode") %>%
    left_join(tansleyScale, by = "TansleyCode")

# een deel van de structuurvariabelen via via Tansley-schaal bepaald voor 2016 en via bedekking na 2016 --> moet nog aangepast worden in de databank
  structurePlots_Tansley_2 <- structurePlots %>%
    select(-Edit_date, -LowShrublayer, -starts_with("Calluna"), -Shrub_and_Treelayer_18m) %>%
    filter(Jaar < 2016) %>%
    gather(-IDPlots, -Jaar, key = "StructureVar", value = "TansleyCode") %>%
    left_join(tansleyScale, by = "TansleyCode")

  structurePlots_Cover_1 <- structurePlots %>%
    select(-Edit_date, -LowShrublayer, -starts_with("Calluna"), -Shrub_and_Treelayer_18m) %>%
    filter(Jaar >= 2016) %>%
    gather(-IDPlots, -Jaar, key = "StructureVar", value = "Cover")

# verbossing steeds als bedekking ingeschat
  structurePlots_Cover_2 <- structurePlots %>%
    select(IDPlots, Jaar,  Shrub_and_Treelayer_18m) %>%
    gather(Shrub_and_Treelayer_18m, key = "StructureVar", value = "Cover")

# alle records als bedekking uitgedrukt
  structurePlots_cover_long <- bind_rows(structurePlots_Tansley_1,
                                   structurePlots_Tansley_2,
                                   structurePlots_Cover_1,
                                   structurePlots_Cover_2) %>%
    select(IDPlots, Jaar, StructureVar, Cover)

  structurePlots_cover_wide <- structurePlots_cover_long %>%
    spread(key = StructureVar, value = Cover)

  return(structurePlots_cover_wide)

}

############################################################################################

getStructurePlot6510 <- function(db = dbHeideEn6510_2014_2015){

  query_StructurePlot6510 <- "SELECT
  SiteDescription_6510.IDPlots,
  SiteDescription_6510.Shrub_and_Treelayer_18m
  FROM SiteDescription_6510"

     if(substr(db,nchar(db)-3,nchar(db)) == ".mdb"){
    connectieDB <-   odbcConnectAccess(db)
  } else if (substr(db,nchar(db)-5,nchar(db)) == ".accdb") {
    connectieDB <-   odbcConnectAccess2007(db)
  }

  structurePlots <- sqlQuery(connectieDB, query_StructurePlot6510, stringsAsFactors = TRUE)

  #tansleyScale <- sqlFetch(connectieDB,"qCoverageTansley")

  odbcClose(connectieDB)

  return(structurePlots)
}



######################################################################################
### FUNCTIES VOOR BEREKENING LSVI-INDICATOREN (OP NIVEAU VAN ANALYSEVARIABELE)
#######################################################################################

berekenGroeiklassenVBI2 <- function(db = dbVBI2, plotIDs = NULL, niveau = "plot"){

  # groeiklasse 4 tot groeiklasse 6 leiden we af uit A3A4 bomen
  treesA3A4 <- getTreesA3A4RawData(db = db, plotIDs = plotIDs)

  # als we analyse op plotniveau wensen dan zetten we IDSegments overal op 1
  if (niveau == "plot"){
    treesA3A4$IDSegments <- 1
  }

  #voor hakhout bepalen we de groeiklasse op basis van de maixmale diameter van de shoots
  shoots <- getShootsVBI2(db,plotIDs = plotIDs) %>%
    group_by(IDPlots, ID) %>%
    summarise(DBH_mm_max = max(DBH_MM)) %>%
    ungroup()

  # groeiklassen voor levende bomen (dode bomen rekenen we niet mee)
  groeiklassen_4_5_6_7 <- treesA3A4 %>%
    left_join(shoots, by = c("IDPlots", "ID")) %>%
    mutate(DBH_mm = ifelse(!is.na(Coppice_Individual) & Coppice_Individual == "Hakhoutstoof", DBH_mm_max, DBH_mm)) %>%
    filter(is.na(StatusTree) | StatusTree == "levend") %>%
    group_by(IDPlots, IDSegments) %>%
    summarise(Groeiklasse4 = sum(DBH_mm >= 70 & DBH_mm < 140, na.rm = TRUE) > 0,
              Groeiklasse5 = sum(DBH_mm >= 140 & DBH_mm < 500, na.rm = TRUE) > 0,
              Groeiklasse6 = sum(DBH_mm >= 500 & DBH_mm < 800, na.rm = TRUE) > 0,
              Groeiklasse7 = sum(DBH_mm >= 800, na.rm = TRUE) > 0)

  #groeiklasse3 komt overeen met A2-boom
  treesA2 <- getTreesA2VBI2(db, plotIDs)

  groeiklasse3 <- treesA2 %>%
    mutate(Groeiklasse3 = TRUE) %>%
    select(IDPlots, Groeiklasse3) %>%
    unique()

  #groeiklasse2 = natuurlijke verjonging (boomsoort in kruidlaag)
  coverSpecies <- getCoverSpeciesVBI2(db, plotIDs = plotIDs)

  groeiklasse2 <- coverSpecies %>%
    group_by(IDPlots) %>%
    summarise(Groeiklasse2 = sum(Tree & Vegetatielaag == "kruidlaag", na.rm = TRUE) > 0)

  groeiklassen <- groeiklassen_4_5_6_7 %>%
    left_join(groeiklasse3, by = "IDPlots") %>%
    mutate(Groeiklasse3 = ifelse(is.na(Groeiklasse3), FALSE, Groeiklasse3)) %>%
    left_join(groeiklasse2, by = "IDPlots") %>%
    mutate(Groeiklasse1 = NA) %>% #groeiklasse1 = open ruimte in bos --> kan niet afgeleid worden uit gegegevens
    gather(-IDPlots,-IDSegments, key = "Groeiklasse", value = "Waarde") %>%
    arrange(IDPlots, Groeiklasse) %>%
    ungroup()

return(groeiklassen)

}


#######################################################################################

berekenAVGroeiklassen <- function(db = dbVBI2, plotIDs = NULL, niveau = "plot") {

  groeiklassen <- berekenGroeiklassenVBI2(db, plotIDs, niveau)

  groeiklassenAV <- groeiklassen %>%
    group_by(IDPlots, IDSegments) %>%
    summarise(AantalGroeiklassen = sum(Waarde, na.rm = TRUE),
              Groeiklasse7 = sum((Groeiklasse == "Groeiklasse7") * Waarde, na.rm = TRUE),
              Groeiklasse4_5_6 = ifelse(sum(Groeiklasse %in% c("Groeiklasse4", "Groeiklasse5", "Groeiklasse6") * Waarde, na.rm = TRUE) > 0, 1, 0))

  result <- groeiklassenAV %>%
    gather(AantalGroeiklassen, Groeiklasse7, Groeiklasse4_5_6, key = AnalyseVariabele, value = Waarde)

  return(result)
}
#######################################################################################

berekenAVVegetatielagen <- function(db = dbVBI2, plotIDs = NULL) {

    veglagen_bedekking <- getCoverVeglayersVBI2(db, plotIDs)

    klasseTalrijk <- selectScale("Beheermonitoringsschaal") %>%
  filter(KlasseCode == "T")

    veglagenAV <- veglagen_bedekking %>%
      mutate(AantalTarlijkeVegetatielagen = (CoverShrublayer >= klasseTalrijk$BedekkingGem) + (CoverTreelayer >= klasseTalrijk$BedekkingGem) + (CoverHerbAndMosslayer >=  klasseTalrijk$BedekkingGem)) %>%
      select(IDPlots, AantalTarlijkeVegetatielagen)


}


#######################################################################################

berekenDoodHout <- function(db = dbVBI2, plotIDs = NULL, niveau = "plot"){

  treesA3A4MHK <- getTreesA3A4MHK(db = db, plotIDs = plotIDs)
  shootsMHK <- getShootsVBI2(db = db, plotIDs = plotIDs)
  treesA3A4_Vol <-  calculateVolumeAndBasalArea(treesA3A4MHK, shootsMHK, dbExterneData = dbVBIExterneData)
  logs <- getLogsVBI2(db = db, plotIDs = plotIDs)

  doodHoutStaand <- treesA3A4_Vol %>%
  group_by(IDPlots, IDSegments) %>%
  summarise(StaandDoodHout = sum(Volume_ha * (StatusTree == "dood"), na.rm = TRUE),
            StaandLevendHout =  sum(Volume_ha * (StatusTree == "levend"), na.rm = TRUE),
            DikStaandDoodHout = sum((Diameter_cm > 40) * (StatusTree == "dood") * 10000 / AreaA4_m2, na.rm =TRUE))

  doodHoutLiggend <- logs %>%
    group_by(IDPlots) %>%
    summarise(LiggendDoodHout = sum(Volume_ha, na.rm = TRUE))

  doodHout <- doodHoutStaand %>%
    left_join(doodHoutLiggend, by = "IDPlots") %>%
    mutate(LiggendDoodHout = ifelse(is.na(LiggendDoodHout), 0, LiggendDoodHout)) %>%
    gather(StaandDoodHout, StaandLevendHout, DikStaandDoodHout, LiggendDoodHout, key = "Kenmerk", value = "Waarde" ) %>%
    mutate(Eenheid = ifelse(Kenmerk == "DikStaandDoodHout", "Aantal_ha", "Volume_ha"),
           TypeKenmerk = NA,
           Type = NA)

  return(doodHout)

}

#######################################################################################

berekenAVDoodHout <- function(db = dbVBI2, plotIDs = NULL, niveau = "plot"){

  doodHout <- berekenDoodHout(db, plotIDs, niveau)

  doodHoutAV <- doodHout %>%
  select(-Eenheid) %>%
  spread(key = "Kenmerk", value = "Waarde" ) %>%
  mutate(volumeAandeelDoodHout = (StaandDoodHout + LiggendDoodHout)/(StaandDoodHout + StaandLevendHout + LiggendDoodHout),
         dikDoodHoutStaand_ha = DikStaandDoodHout) %>%
  select(IDPlots, IDSegments, volumeAandeelDoodHout, dikDoodHoutStaand_ha) %>%
  gather(volumeAandeelDoodHout, dikDoodHoutStaand_ha,  key = "AnalyseVariabele", value = "Waarde")

  return(doodHoutAV)

}

#######################################################################################

berekenLevendHoutSoort <- function(db = dbVBI2, plotIDs = NULL, niveau = "plot"){

  treesA3A4MHK <- getTreesA3A4MHK(db = db, plotIDs = plotIDs)
  shootsMHK <- getShootsVBI2(db = db, plotIDs = plotIDs)
  treesA3A4_Vol <-  calculateVolumeAndBasalArea(treesA3A4MHK, shootsMHK, dbExterneData = dbVBIExterneData)
  logs <- getLogsVBI2(db = db, plotIDs = plotIDs)

  levendHoutSoort <- treesA3A4_Vol %>%
    group_by(IDPlots, IDSegments, NameSc) %>%
    summarise(Volume_ha = sum(Volume_ha * (StatusTree == "levend"), na.rm = TRUE),
              Grondvlak_ha = sum(BasalArea_ha  * (StatusTree == "levend"), na.rm =TRUE)) %>%
    ungroup() %>%
    gather(Volume_ha, Grondvlak_ha, key = Eenheid, value = Waarde) %>%
    rename(Kenmerk = NameSc) %>%
    mutate(TypeKenmerk = "Soort_Lat",
           Type = NA)

}

#######################################################################################

berekenAVLevendHoutSleutelsoorten <- function(db = dbVBI2, plotIDs = NULL, plotHabtypes, niveau = "plot"){

  sleutelsoortenBossen <- geefSoortenlijst(Habitatgroep = "Bossen en struwelen", Indicator = "sleutelsoorten van de boom- en struiklaag") %>%
    select(Versie, HabCode = Habitatsubtype, Soort_lat = WetNaam)

  levendHout <- berekenLevendHoutSoort(db, plotIDs, niveau)

  levendHoutSs <- levendHout %>%
    filter(Eenheid == "Grondvlak_ha") %>%
    left_join(plotHabtypes, by = "IDPlots") %>%
    rename(Soort_lat = Kenmerk) %>%
    mutate(Soort_lat = ifelse(Soort_lat == "Quercus robur/petraea", "Quercus robur", as.character(Soort_lat)), # inlandse eik
           Soort_lat = ifelse(Soort_lat == "Betula tremula/alba", # berk
                              ifelse(HabCode %in% c("9110", "9120", "9130", "9160", "91E0_vo", "91E0_vm"), "Betula pendula Roth", "Betula pubescens Ehrh."),
                              as.character(Soort_lat))) %>%
    group_by(IDPlots, IDSegments) %>%
    mutate(GrondvlakLevendTot = sum(Waarde, na.rm = TRUE)) %>%
    ungroup() %>%
    left_join(sleutelsoortenBossen, by = c("HabCode", "Soort_lat")) %>%
    filter(!is.na(Versie)) %>%
    group_by(IDPlots, IDSegments, Versie) %>%
    summarise(Waarde = sum(Waarde, na.rm = TRUE)/ unique(GrondvlakLevendTot),
              AnalyseVariabele = "sleutelsoorten_boomlaag_grondvlakAandeel") %>%
    ungroup()

  return(levendHoutSs)

}


#######################################################################################

### Berekening indicatoren habitatstructuur van boshabitats + berekening grondvlakaandeel sleutelsoorten (vegetatie-indicator)

calculateLSVI_dendroVBI2 <- function(db = dbVBI2, plotHabtypes, niveau = "segment", versieLSVI = "versie3"){

  # connDB <-   odbcConnectAccess2007(dbLSVI)
  # soortenlijstLSVI <- sqlQuery(connDB, 'select * from tblSoortenlijst_LSVI_HeideEnBoshabitats')
  # indicatorenLSVI <- sqlQuery(connDB, 'select * from tblIndicatoren_LSVI_HeideEnBoshabitats')
  # odbcClose(connDB)

  indicatorenLSVI <- read.csv2(indicatorenLSVI)

  ### Soortenlijst opvragen voor gewenste versie van LSVI
  # if (versieLSVI == "versie3"){
  #
  #   soortenlijstLSVI <- soortenlijstLSVI[soortenlijstLSVI$Versie3==1,]
  #
  # } else if (versieLSVI == "versie2") {
  #
  #   soortenlijstLSVI <- soortenlijstLSVI[soortenlijstLSVI$Versie2==1,]
  #
  # }


  # data voor berekening indicatoren

  treesA3A4 <- getTreesA3A4VBI2(db = db,plotIDs = plotHabtypes$IDPlots)

  # als we analyse op plotniveau wensen dan zetten we IDSegments overal op 1
  if (niveau == "plot"){
    treesA3A4$IDSegments <- 1
  }

  shoots <- getShootsVBI2(db,plotIDs = plotHabtypes$IDPlots)

  treesA3A4_VolBA <- calculateVolumeAndBasalArea(treesA3A4, shoots)

  ssBoomlaag <- soortenlijstLSVI[!is.na(soortenlijstLSVI$Omschrijving) & soortenlijstLSVI$Omschrijving == "sleutelsoorten_boomlaag",drop=F]

  #groeiklasse

  treesA3A4_VolBA$Groeiklasse <- ifelse (treesA3A4_VolBA$DBH_mm >=70 & treesA3A4_VolBA$DBH_mm < 140 & treesA3A4_VolBA$Alive,"Groeiklasse4",
                                         ifelse(treesA3A4_VolBA$DBH_mm < 500 & treesA3A4_VolBA$Alive, "Groeiklasse5",
                                                ifelse(treesA3A4_VolBA$DBH_mm < 800 & treesA3A4_VolBA$Alive, "Groeiklasse6",
                                                       ifelse(treesA3A4_VolBA$DBH_mm > 800 & treesA3A4_VolBA$Alive, "Groeiklasse7",NA))))

  treesA2 <- getTreesA2VBI2(db,plotHabtypes$IDPlots)

  speciesVeglayers <- getCoverSpeciesVBI2(db,plotHabtypes$IDPlots)

  coverVeglayers <- getCoverVeglayersVBI2(db,plotHabtypes$IDPlots)

  logs <- getLogsVBI2(db,plotHabtypes$IDPlots)

  treesA3A4_VolBA <- merge(treesA3A4_VolBA,plotHabtypes,by="IDPlots",all.x=TRUE)

  # treesA3A4_VolBA_check <- treesA3A4_VolBA %>%
  #   dplyr::rename(WetNaam = NameSc) %>%
  #   left_join(ssBoomlaag, by = c("HabCode","WetNaam"))
  #
  # plots_check <- treesA3A4_VolBA_check %>%
  #   group_by(IDPlots,Alive) %>%
  #   summarise(GrondvlakSs = sum(BasalArea_ha * !is.na(Omschrijving) ))



  calcIndic <- function(treedata,treelist){
    data.frame(
      VolumeStaandDood = sum(treedata$Volume_ha*(!treedata$Alive),na.rm = TRUE),
      VolumeStaandLevend = sum(treedata$Volume_ha*(treedata$Alive),na.rm = TRUE),
      GrondvlakDood = sum(treedata$BasalArea_ha*(!treedata$Alive),na.rm = TRUE),
      GrondvlakLevend = sum(treedata$BasalArea_ha*(treedata$Alive),na.rm = TRUE),
      GrondvlakLevendSs = sum(treedata$BasalArea_ha*(treedata$Alive)*(treedata$NameSc %in% treelist[treelist$HabCode == as.character(unique(treedata$HabCode)),]$WetNaam),na.rm = TRUE),
      DikDoodHoutStaand_ha = sum((!treedata$Alive)*(treedata$DBH_mm > 400) * (treedata$Coppice_IndividualCode == 10) *ifelse(treedata$Perimeter_cm < 122, 10000.0/treedata$AreaA3_m2, 10000.0/treedata$AreaA4_m2),na.rm = TRUE),
      AantalGroeiklassenA3A4 = length (unique(na.omit(treedata$Groeiklasse))),
      Groeiklasse7 = "Groeiklasse7" %in% treedata$Groeiklasse,
      Groeiklasse5_6_7 = "Groeiklasse5" %in% treedata$Groeiklasse | "Groeiklasse6" %in% treedata$Groeiklasse | "Groeiklasse7" %in% treedata$Groeiklasse)
  }

  plots <- ddply(treesA3A4_VolBA, .(IDPlots,IDSegments,HabCode), calcIndic, treelist= ssBoomlaag)

  # een A2 boom komt oveeen met groeiklasse 3
  treesA2_plots <- data.frame(IDPlots = unique(treesA2$IDPlots), Groeiklasse3 = TRUE)
  treesA2_plots <- merge(treesA2_plots,plotHabtypes,by="IDPlots",all.x=TRUE)

  plots <- merge(plots,treesA2_plots,by=c("IDPlots","HabCode"), all= TRUE)

  # natuurlijke verjonging komt overeen met groeiklasse 2
  speciesVeglayers$Groeiklasse2_species <- speciesVeglayers$Tree & (speciesVeglayers$Vegetatielaag == "kruidlaag")

  speciesVeglayers_plot <- ddply(speciesVeglayers,.(IDPlots),summarise,
                                 Groeiklasse2 = sum(Groeiklasse2_species) > 0)

  speciesVeglayers_plot <- merge(speciesVeglayers_plot,plotHabtypes,by="IDPlots",all.x=TRUE)

  plots <- merge(plots, speciesVeglayers_plot[,c("IDPlots", "HabCode","Groeiklasse2")], by =c("IDPlots","HabCode"),all=TRUE)

  plots$IDSegments <- ifelse(is.na(plots$IDSegments),1,plots$IDSegments)

  plots$Groeiklasse2 <- ifelse(is.na(plots$Groeiklasse2),FALSE,plots$Groeiklasse2)
  plots$Groeiklasse3 <- ifelse(is.na(plots$Groeiklasse3),FALSE,plots$Groeiklasse3)
  plots$Groeiklasse7 <- ifelse(is.na(plots$Groeiklasse7),FALSE,plots$Groeiklasse7)
  plots$AantalGroeiklassenA3A4 <- ifelse(is.na(plots$AantalGroeiklassenA3A4),FALSE,plots$AantalGroeiklassenA3A4)

  plots$AantalGroeiklassen <- plots$AantalGroeiklassenA3A4 + plots$Groeiklasse3 + plots$Groeiklasse2

  # groeiklasse 1 komt overeen met 'open ruimte in bos', maar kunnen we niet afleiden uit VBI2-data

  # volume liggend dood hout per plot
  logs_plot <- ddply(logs,.(IDPlots), summarise,
                     VolumeLiggendDood = sum(Volume_ha,na.rm=TRUE))

  plots <- merge(plots,logs_plot, by = "IDPlots", all.x=TRUE)

  plots[is.na(plots$VolumeLiggendDood),]$VolumeLiggendDood <- 0

  # aandeel dood hout
  plots$VolumeAandeelDoodhoutStaand <- plots$VolumeStaandDood/(plots$VolumeStaandDood + plots$VolumeStaandLevend) * 100

  plots$GrondvlakAandeelDoodhoutStaand <- plots$GrondvlakDood/(plots$GrondvlakDood + plots$GrondvlakLevend) * 100

  plots$VolumeAandeelDoodhoutTotaal <- (plots$VolumeStaandDood + plots$VolumeLiggendDood) / (plots$VolumeStaandDood + plots$VolumeLiggendDood + plots$VolumeStaandLevend) * 100

  # grondvlakaandeel sleutelsoorten
  plots$GrondvlakAandeelSs <- ifelse(plots$GrondvlakLevend > 0,plots$GrondvlakLevendSs/plots$GrondvlakLevend * 100,0)

  calcIndic2 <- function(treedata,treelist){
    data.frame(
      GrondvlakLevendSs_soort = sum(treedata$BasalArea_ha*(treedata$Alive)*(treedata$NameSc %in% treelist[treelist$HabCode == as.character(unique(treedata$HabCode)),]$WetNaam),na.rm=TRUE)
    )
  }

  treeSpecies <- ddply(treesA3A4_VolBA,.(IDPlots,IDSegments,NameSc,HabCode), calcIndic2,treelist = ssBoomlaag)

  treeSpecies <- merge(treeSpecies,plots[,c("IDPlots","IDSegments","GrondvlakLevend")], by = c("IDPlots","IDSegments"), all.x =TRUE)

  treeSpecies$GrondvlakAandeelSs_soort <- treeSpecies$GrondvlakLevendSs_soort/treeSpecies$GrondvlakLevend * 100

  plots2 <- ddply(treeSpecies,.(IDPlots, IDSegments),summarise,
                  AantalSsBedekkingMinstens10 = sum(GrondvlakAandeelSs_soort >= 10))

  plots <- merge(plots, plots2, by=c("IDPlots","IDSegments"), all.x= TRUE)

  # aantal abundante vegetatielagen

  coverVeglayers$HerbLayerAbundant <- (coverVeglayers$CoverHerblayer >=25) | ((coverVeglayers$CoverHerblayer + coverVeglayers$CoverMosslayer) >= 25)

  coverVeglayers$ShrubLayerAbundant <- coverVeglayers$CoverShrublayer >= 25

  coverVeglayers$TreeLayerAbundant <- coverVeglayers$CoverTreelayer >= 25

  coverVeglayers$AantalAbundanteVegetatielagen <- coverVeglayers$HerbLayerAbundant + coverVeglayers$ShrubLayerAbundant + coverVeglayers$TreeLayerAbundant

  # aantal aanwezige vegetatielagen

  coverVeglayers$HerbLayerPresent <- (coverVeglayers$CoverHerblayer >0) | ((coverVeglayers$CoverHerblayer + coverVeglayers$CoverMosslayer) > 0)

  coverVeglayers$ShrubLayerPresent <- coverVeglayers$CoverShrublayer > 0

  coverVeglayers$TreeLayerPresent <- coverVeglayers$CoverTreelayer > 0

  coverVeglayers$AantalAanwezigeVegetatielagen <- coverVeglayers$HerbLayerPresent + coverVeglayers$ShrubLayerPresent + coverVeglayers$TreeLayerPresent

  # aantal frequente vegetatielagen

  coverVeglayers$HerbLayerFrequent <- (coverVeglayers$CoverHerblayer > 5) | ((coverVeglayers$CoverHerblayer + coverVeglayers$CoverMosslayer) > 5)

  coverVeglayers$ShrubLayerFrequent <- coverVeglayers$CoverShrublayer > 5

  coverVeglayers$TreeLayerFrequent <- coverVeglayers$CoverTreelayer > 5

  coverVeglayers$AantalFrequenteVegetatielagen <- coverVeglayers$HerbLayerFrequent + coverVeglayers$ShrubLayerFrequent + coverVeglayers$TreeLayerFrequent


  coverVeglayers <- merge(coverVeglayers,plotHabtypes,by="IDPlots",all.x=TRUE)

  plots <- merge(plots, coverVeglayers[,c("IDPlots","HabCode","AantalAbundanteVegetatielagen", "AantalAanwezigeVegetatielagen", "AantalFrequenteVegetatielagen")],by=c("IDPlots","HabCode"),all=TRUE)

  plots$VolumeAandeelDoodhoutStaand <- ifelse(is.na(plots$VolumeAandeelDoodhoutStaand),0,plots$VolumeAandeelDoodhoutStaand)

  plots$GrondvlakAandeelDoodhoutStaand <- ifelse(is.na(plots$GrondvlakAandeelDoodhoutStaand),0,plots$GrondvlakAandeelDoodhoutStaand)

  plots$VolumeAandeelDoodhoutTotaal <- ifelse(is.na(plots$VolumeAandeelDoodhoutTotaal),0,plots$VolumeAandeelDoodhoutTotaal)

  plots$AantalSsBedekkingMinstens10 <- ifelse(is.na(plots$AantalSsBedekkingMinstens10),0,plots$AantalSsBedekkingMinstens10)

  plots$GrondvlakAandeelSs <- ifelse(is.na(plots$GrondvlakAandeelSs),0,plots$GrondvlakAandeelSs)

  ### Selectie indicatoren voor LSVI_v3

  structuurIndicatoren_LSVI3 <- plots[,c("IDPlots","IDSegments","HabCode","AantalFrequenteVegetatielagen","AantalGroeiklassen", "Groeiklasse7", "Groeiklasse5_6_7", "VolumeAandeelDoodhoutStaand","GrondvlakAandeelSs","DikDoodHoutStaand_ha")]

  #naamgeving conform databank indicatoren

  structuurIndicatoren_LSVI3 <- plyr::rename(structuurIndicatoren_LSVI3, c(AantalFrequenteVegetatielagen ="frequenteVegetatielagen", AantalGroeiklassen = "groeiklassen", Groeiklasse7 = "groeiklasse7",Groeiklasse5_6_7 ="groeiklasse5_6_7" , VolumeAandeelDoodhoutStaand = "volumeAandeelDoodHout", GrondvlakAandeelSs = "sleutelsoorten_boomlaag_grondvlakAandeel", DikDoodHoutStaand_ha = "dikDoodHoutStaand_ha"))

structuurIndicatoren_LSVI3_long <- gather(structuurIndicatoren_LSVI3, sleutelsoorten_boomlaag_grondvlakAandeel, frequenteVegetatielagen, groeiklassen, groeiklasse7 , groeiklasse5_6_7, volumeAandeelDoodHout, dikDoodHoutStaand_ha, key = "AnalyseVariabele", value = "Waarde")

structuurIndicatoren_LSVI3_selectie <- merge(structuurIndicatoren_LSVI3_long, indicatorenLSVI[indicatorenLSVI$Meting == "structuurplot",], by = c("HabCode", "AnalyseVariabele"))

indicatoren <- structuurIndicatoren_LSVI3_selectie[,c("HabCode","IDPlots", "IDSegments", "Criterium", "Indicator", "AnalyseVariabele", "Soortengroep", "Vegetatielaag","Eenheid", "Drempelwaarde","Indicatortype","Meting", "Combinatie", "Waarde")]

indicatoren$Beoordeling <- ifelse (indicatoren$Indicatortype == "negatief", ifelse(indicatoren$Waarde <= indicatoren$Drempelwaarde, 1,0),
                                   ifelse (indicatoren$Indicatortype == "positief", ifelse(indicatoren$Waarde >= indicatoren$Drempelwaarde, 1,0),NA))

if (niveau == "plot"){
  indicatoren <- indicatoren[,!colnames(indicatoren) %in% "IDSegments"]
}

  return(indicatoren)

}

###########################################################################################################################
  ############################################################################
####TABEL MET PLOTGEWICHTEN, SEGMENTGEWICHTEN EN OPPERVLAKTE A3 en A4 PLOT
############################################################################
getPlotWeights <- function(db = dbBosExtra,
                           shapePlotsDirectory = shapePlotsDir,
                           shapefileA3Plots = shapeA3,
                           shapefileA2Plots = shapeA2,
                           shapefileVegPlots = shapeVegPlot){

  if(substr(db,nchar(db)-3,nchar(db)) == ".mdb"){
    connectieDB <-   odbcConnectAccess(db)
  } else if (substr(db,nchar(db)-5,nchar(db)) == ".accdb") {
    connectieDB <-   odbcConnectAccess2007(db)
  }
  query_segments <- "SELECT Standdescription_segments.IDPlots,
                    Standdescription_segments.ID,
                    Standdescription_segments.Area_share_perc,
                    Standdescription_segments.Area_m2,
                    Standdescription_segments.Landuse,
                    qLanduse.Value1
                    FROM Standdescription_segments
                    LEFT JOIN qLanduse ON Standdescription_segments.Landuse = qLanduse.ID;"

  segmentsOrig <- sqlQuery(connectieDB, query_segments)

  odbcClose(connectieDB)

  #oppervlaktes A3, A2 en vegetatieplot voor verschillende segmenten
  areaA3_plot_shape <- readOGR(shapePlotsDirectory, shapefileA3Plots, verbose = FALSE)
  areaA3_plot <- areaA3_plot_shape@data %>%
    select(IDPlots = IDPLOTS, IDSegments = ID, AreaA3_m2 = Area_m2) %>%
    mutate(IDPlots = as.numeric(as.character(IDPlots)),
           IDSegments = as.numeric(as.character(IDSegments)))

  areaA2_plot_shape <- readOGR(shapePlotsDirectory, shapefileA2Plots,  verbose = FALSE)
  areaA2_plot <- areaA2_plot_shape@data %>%
    select(IDPlots = IDPLOTS, IDSegments = ID, AreaA2_m2 = Area_m2) %>%
    mutate(IDPlots = as.numeric(as.character(IDPlots)),
           IDSegments = as.numeric(as.character(IDSegments)))

  areaVegPlot_shape <- readOGR(shapePlotsDirectory, shapefileVegPlots, verbose = FALSE)
  areaVegPlot <- areaVegPlot_shape@data %>%
    select(IDPlots = IDPLOTS, IDSegments = ID, AreaVeg_m2 = Area_m2) %>%
    mutate(IDPlots = as.numeric(as.character(IDPlots)),
           IDSegments = as.numeric(as.character(IDSegments)))

  #overzicht segmenten
  segments <- segmentsOrig %>%
    select(IDPlots, IDSegments = ID, Area_share_perc, Area_m2, LanduseCode = Landuse, Landuse = Value1) %>%
    filter(!is.na(LanduseCode)) %>% #plots die nog niet opgemeten zijn verwijderen
    mutate( Area_share_perc = ifelse(is.na(Area_share_perc), 100, Area_share_perc)) %>% #indien ontbrekende waarde --> 100%
    group_by(IDPlots) %>%
    mutate(BosPlot = sum(LanduseCode %in% c(1,2)) > 0,
           PlotWeight = sum(Area_share_perc * LanduseCode %in% c(1,2))/100) %>% #Plotgewicht --> som bosoppervlakte in bos
    ungroup() %>%
    mutate(SegmentWeight = ifelse(LanduseCode %in% c(1,2), Area_share_perc/100/PlotWeight, 0),
           AreaA4_m2 = (pi*18^2) * Area_share_perc/100) %>%
    left_join(areaA3_plot, by = c("IDPlots", "IDSegments")) %>%
    left_join(areaA2_plot, by = c("IDPlots", "IDSegments")) %>%
    left_join(areaVegPlot, by = c("IDPlots", "IDSegments")) %>%
    mutate(AreaA3_m2 = ifelse(is.na(AreaA3_m2), 0 , AreaA3_m2),
           AreaA2_m2 = ifelse(is.na(AreaA2_m2), 0 , AreaA2_m2),
           AreaVeg_m2 = ifelse(is.na(AreaVeg_m2), 0 , AreaVeg_m2)) %>%
    select(-Area_share_perc, -Area_m2, -LanduseCode, -BosPlot)

  return(segments)
}







### Berekening indicatoren habitatstructuur van boshabitats + berekening grondvlakaandeel sleutelsoorten (vegetatie-indicator)

calculateLSVI_dendroVBI1 <- function(db_dendro = dbVBI1, db_veg = dbVBI1_veg, plotHabtypes, niveau = "segment", versieLSVI = "versie3"){

  ### Soortenlijst opvragen voor gewenste versie van LSVI
  connDB <-   odbcConnectAccess2007(dbLSVI)
  soortenlijstLSVI <- sqlQuery(connDB, 'select * from tblSoortenlijst_LSVI_HeideEnBoshabitats')
  indicatorenLSVI <- sqlQuery(connDB, 'select * from tblIndicatoren_LSVI_HeideEnBoshabitats')
  odbcClose(connDB)

  if (versieLSVI == "versie3"){

    soortenlijstLSVI <- soortenlijstLSVI[soortenlijstLSVI$Versie3==1,]

  } else if (versieLSVI == "versie2") {

    soortenlijstLSVI <- soortenlijstLSVI[soortenlijstLSVI$Versie2==1,]

  }



  # data voor berekening indicatoren

  treesA3A4 <- getTreesA3A4VBI1(db = db_dendro,plotIDs = plotHabtypes$IDPlots)

  shoots <- getShootsVBI1(db_dendro,plotIDs = plotHabtypes$IDPlots)

  treesA3A4_VolBA <- calculateVolumeAndBasalAreaVBI1(treesA3A4, shoots)

  ssBoomlaag <- soortenlijstLSVI[!is.na(soortenlijstLSVI$Omschrijving) & soortenlijstLSVI$Omschrijving == "sleutelsoorten_boomlaag",,drop=F]

  #groeiklasse

  treesA3A4_VolBA$Groeiklasse <- ifelse (treesA3A4_VolBA$DBH_mm >=70 & treesA3A4_VolBA$DBH_mm < 140 & treesA3A4_VolBA$Alive,"Groeiklasse4",
                                         ifelse(treesA3A4_VolBA$DBH_mm < 500 & treesA3A4_VolBA$Alive, "Groeiklasse5",
                                                ifelse(treesA3A4_VolBA$DBH_mm < 800 & treesA3A4_VolBA$Alive, "Groeiklasse6",
                                                       ifelse(treesA3A4_VolBA$DBH_mm > 800 & treesA3A4_VolBA$Alive, "Groeiklasse7",NA))))

  treesA2 <- getTreesA2VBI1(db_dendro,plotHabtypes$IDPlots)

  speciesVeglayers <- getCoverSpeciesVBI1(db_veg,plotHabtypes$IDPlots)

  coverVeglayers <- getCoverVeglayersVBI1(db_veg,plotHabtypes$IDPlots)

  logs <- getLogsVBI1(db_veg, plotHabtypes$IDPlots)

  treesA3A4_VolBA <- merge(treesA3A4_VolBA,plotHabtypes,by="IDPlots",all.x=TRUE)

  calcIndic <- function(treedata,treelist){
    data.frame(
      VolumeStaandDood = sum(treedata$Volume_ha*(!treedata$Alive),na.rm = TRUE),
      VolumeStaandLevend = sum(treedata$Volume_ha*(treedata$Alive),na.rm = TRUE),
      GrondvlakDood = sum(treedata$BasalArea_ha*(!treedata$Alive),na.rm = TRUE),
      GrondvlakLevend = sum(treedata$BasalArea_ha*(treedata$Alive),na.rm = TRUE),
      GrondvlakLevendSs = sum(treedata$BasalArea_ha*(treedata$Alive)*(treedata$NameSc %in% treelist[treelist$HabCode == as.character(unique(treedata$HabCode)),]$WetNaam),na.rm = TRUE),
      DikDoodHoutStaand_ha = sum((!treedata$Alive) * (treedata$DBH_mm > 400) * (treedata$Coppice_IndividualCode == 10) *  ifelse(treedata$Perimeter_cm < 122, 10000.0/(9*9*pi), 10000.0/(18*18*pi)),na.rm = TRUE),
      AantalGroeiklassenA3A4 = length (unique(na.omit(treedata$Groeiklasse))),
      Groeiklasse7 = "Groeiklasse7" %in% treedata$Groeiklasse,
      Groeiklasse5_6_7 = "Groeiklasse5" %in% treedata$Groeiklasse | "Groeiklasse6" %in% treedata$Groeiklasse | "Groeiklasse7" %in% treedata$Groeiklasse
      )
  }

  plots <- ddply(treesA3A4_VolBA, .(IDPlots,IDSegments,HabCode), calcIndic, treelist= ssBoomlaag)

  # een A2 boom komt oveeen met groeiklasse 3
  treesA2_plots <- data.frame(IDPlots = unique(treesA2$IDPlots), Groeiklasse3 = TRUE)
  treesA2_plots <- merge(treesA2_plots,plotHabtypes,by="IDPlots",all.x=TRUE)

  plots <- merge(plots,treesA2_plots,by=c("IDPlots","HabCode"), all= TRUE)

  # natuurlijke verjonging komt overeen met groeiklasse 2
  speciesVeglayers$Groeiklasse2_species <- speciesVeglayers$Tree & (speciesVeglayers$Vegetatielaag== "kruidlaag")

  speciesVeglayers_plot <- ddply(speciesVeglayers,.(IDPlots),summarise,
                                 Groeiklasse2 = sum(Groeiklasse2_species) > 0)

  speciesVeglayers_plot <- merge(speciesVeglayers_plot,plotHabtypes,by="IDPlots",all.x=TRUE)

  plots <- merge(plots, speciesVeglayers_plot[,c("IDPlots", "HabCode","Groeiklasse2")], by =c("IDPlots","HabCode"),all=TRUE)

  plots$IDSegments <- ifelse(is.na(plots$IDSegments),1,plots$IDSegments)

  #plots$Groeiklasse2 <- ifelse(is.na(plots$Groeiklasse2),FALSE,plots$Groeiklasse2)
  plots$Groeiklasse3 <- ifelse(is.na(plots$Groeiklasse3),FALSE,plots$Groeiklasse3)
  plots$Groeiklasse7 <- ifelse(is.na(plots$Groeiklasse7),FALSE,plots$Groeiklasse7)
  plots$AantalGroeiklassenA3A4 <- ifelse(is.na(plots$AantalGroeiklassenA3A4),FALSE,plots$AantalGroeiklassenA3A4)

  plots$AantalGroeiklassen <- plots$AantalGroeiklassenA3A4 + plots$Groeiklasse3 + plots$Groeiklasse2

  # groeiklasse 1 komt overeen met 'open ruimte in bos', maar kunnen we niet afleiden uit VBI2-data

  # volume liggend dood hout per plot
  logs_plot <- ddply(logs,.(IDPlots), summarise,
                     VolumeLiggendDood = sum(Volume_ha,na.rm=TRUE))

  plots <- merge(plots,logs_plot, by = "IDPlots", all.x=TRUE)

  # aandeel dood hout
  plots$VolumeAandeelDoodhoutStaand <- plots$VolumeStaandDood/(plots$VolumeStaandDood + plots$VolumeStaandLevend) * 100

  plots$GrondvlakAandeelDoodhoutStaand <- plots$GrondvlakDood/(plots$GrondvlakDood + plots$GrondvlakLevend) * 100

  plots$VolumeAandeelDoodhoutTotaal <- (plots$VolumeStaandDood + plots$VolumeLiggendDood) / (plots$VolumeStaandDood + plots$VolumeLiggendDood + plots$VolumeStaandLevend) * 100

  # grondvlakaandeel sleutelsoorten
  plots$GrondvlakAandeelSs <- ifelse(plots$GrondvlakLevend > 0,plots$GrondvlakLevendSs/plots$GrondvlakLevend * 100,0)

  calcIndic2 <- function(treedata,treelist){
    data.frame(
      GrondvlakLevendSs_soort = sum(treedata$BasalArea_ha*(treedata$Alive)*(treedata$NameSc %in% treelist[treelist$HabCode == as.character(unique(treedata$HabCode)),]$WetNaam),na.rm=TRUE)
    )
  }

  treeSpecies <- ddply(treesA3A4_VolBA,.(IDPlots,IDSegments,NameSc,HabCode), calcIndic2,treelist = ssBoomlaag)

  treeSpecies <- merge(treeSpecies,plots[,c("IDPlots","IDSegments","GrondvlakLevend")], by = c("IDPlots","IDSegments"), all.x =TRUE)

  treeSpecies$GrondvlakAandeelSs_soort <- treeSpecies$GrondvlakLevendSs_soort/treeSpecies$GrondvlakLevend * 100

  plots2 <- ddply(treeSpecies,.(IDPlots, IDSegments),summarise,
                  AantalSsBedekkingMinstens10 = sum(GrondvlakAandeelSs_soort >= 10))

  plots <- merge(plots, plots2, by=c("IDPlots","IDSegments"), all.x= TRUE)

  # aantal abundante vegetatielagen

  coverVeglayers$HerbLayerAbundant <- (coverVeglayers$CoverHerblayer >=25) | ((coverVeglayers$CoverHerblayer + coverVeglayers$CoverMosslayer) >= 25)

  coverVeglayers$ShrubLayerAbundant <- coverVeglayers$CoverShrublayer >= 25

  coverVeglayers$TreeLayerAbundant <- coverVeglayers$CoverTreelayer >= 25

  coverVeglayers$AantalAbundanteVegetatielagen <- coverVeglayers$HerbLayerAbundant + coverVeglayers$ShrubLayerAbundant + coverVeglayers$TreeLayerAbundant

  # aantal aanwezige vegetatielagen

  coverVeglayers$HerbLayerPresent <- (coverVeglayers$CoverHerblayer >0) | ((coverVeglayers$CoverHerblayer + coverVeglayers$CoverMosslayer) > 0)

  coverVeglayers$ShrubLayerPresent <- coverVeglayers$CoverShrublayer > 0

  coverVeglayers$TreeLayerPresent <- coverVeglayers$CoverTreelayer > 0

  coverVeglayers$AantalAanwezigeVegetatielagen <- coverVeglayers$HerbLayerPresent + coverVeglayers$ShrubLayerPresent + coverVeglayers$TreeLayerPresent

  # aantal frequente vegetatielagen

  coverVeglayers$HerbLayerFrequent <- (coverVeglayers$CoverHerblayer > 5) | ((coverVeglayers$CoverHerblayer + coverVeglayers$CoverMosslayer) > 5)

  coverVeglayers$ShrubLayerFrequent <- coverVeglayers$CoverShrublayer > 5

  coverVeglayers$TreeLayerFrequent <- coverVeglayers$CoverTreelayer > 5

  coverVeglayers$AantalFrequenteVegetatielagen <- coverVeglayers$HerbLayerFrequent + coverVeglayers$ShrubLayerFrequent + coverVeglayers$TreeLayerFrequent


  coverVeglayers <- merge(coverVeglayers,plotHabtypes,by="IDPlots",all.x=TRUE)

  plots <- merge(plots, coverVeglayers[,c("IDPlots","HabCode","AantalAbundanteVegetatielagen", "AantalAanwezigeVegetatielagen", "AantalFrequenteVegetatielagen")],by=c("IDPlots","HabCode"),all=TRUE)

  plots$VolumeAandeelDoodhoutStaand <- ifelse(is.na(plots$VolumeAandeelDoodhoutStaand),0,plots$VolumeAandeelDoodhoutStaand)
  #
  plots$GrondvlakAandeelDoodhoutStaand <- ifelse(is.na(plots$GrondvlakAandeelDoodhoutStaand),0,plots$GrondvlakAandeelDoodhoutStaand)
  #
  # plots$VolumeAandeelDoodhoutTotaal <- ifelse(is.na(plots$VolumeAandeelDoodhoutTotaal),0,plots$VolumeAandeelDoodhoutTotaal)
  #
  plots$AantalSsBedekkingMinstens10 <- ifelse(is.na(plots$AantalSsBedekkingMinstens10),0,plots$AantalSsBedekkingMinstens10)
  #
  plots$GrondvlakAandeelSs <- ifelse(is.na(plots$GrondvlakAandeelSs),0,plots$GrondvlakAandeelSs)

  ### Selectie indicatoren voor LSVI_v3

  structuurIndicatoren_LSVI3 <- plots[,c("IDPlots","IDSegments","HabCode","AantalFrequenteVegetatielagen","AantalGroeiklassen", "Groeiklasse7", "Groeiklasse5_6_7", "VolumeAandeelDoodhoutStaand","GrondvlakAandeelSs","DikDoodHoutStaand_ha")]

  #naamgeving conform databank indicatoren

  structuurIndicatoren_LSVI3 <- plyr::rename(structuurIndicatoren_LSVI3, c(AantalFrequenteVegetatielagen ="frequenteVegetatielagen", AantalGroeiklassen = "groeiklassen", Groeiklasse7 = "groeiklasse7",Groeiklasse5_6_7 ="groeiklasse5_6_7" , VolumeAandeelDoodhoutStaand = "volumeAandeelDoodHout", GrondvlakAandeelSs = "sleutelsoorten_boomlaag_grondvlakAandeel", DikDoodHoutStaand_ha = "dikDoodHoutStaand_ha"))

structuurIndicatoren_LSVI3_long <- gather(structuurIndicatoren_LSVI3, sleutelsoorten_boomlaag_grondvlakAandeel, frequenteVegetatielagen, groeiklassen, groeiklasse7 , groeiklasse5_6_7, volumeAandeelDoodHout, dikDoodHoutStaand_ha, key = "AnalyseVariabele", value = "Waarde")

structuurIndicatoren_LSVI3_selectie <- merge(structuurIndicatoren_LSVI3_long, indicatorenLSVI[indicatorenLSVI$Meting == "structuurplot",], by = c("HabCode", "AnalyseVariabele"))

indicatoren <- structuurIndicatoren_LSVI3_selectie[,c("HabCode","IDPlots", "IDSegments", "Criterium", "Indicator", "AnalyseVariabele", "Soortengroep", "Vegetatielaag","Eenheid", "Drempelwaarde","Indicatortype","Meting", "Combinatie", "Waarde")]

indicatoren$Beoordeling <- ifelse (indicatoren$Indicatortype == "negatief", ifelse(indicatoren$Waarde <= indicatoren$Drempelwaarde, 1,0),
                                   ifelse (indicatoren$Indicatortype == "positief", ifelse(indicatoren$Waarde >= indicatoren$Drempelwaarde, 1,0),NA))


  indicatoren <- indicatoren[,!colnames(indicatoren) %in% "IDSegments"]
  indicatoren <- arrange(indicatoren, IDPlots)

  return(indicatoren)

}



###########################################################################################################


calculateLSVI_structuurplotHeide <- function(structurePlot, plotHabtypes, versieLSVI = "beide"){

  structurePlotHeide <- structurePlot

  ### Indicatoren opvragen voor beide versies van LSVI
  connDB <-   odbcConnectAccess2007(dbLSVI)

  indicatorenLSVI <- sqlQuery(connDB, 'select * from tblIndicatoren_LSVI_HeideEnBoshabitats')

  odbcClose(connDB)

  # dwergstruiken
  structurePlotHeide$dwergstruiken <-ifelse(is.na(structurePlotHeide$LowShrublayer),0, structurePlotHeide$LowShrublayer)

  # ouderdomstructuur struikhei
  structurePlotHeide$BedekkingPionierstadium <- ifelse(is.na(structurePlotHeide$Calluna_phase_pioneer),0,structurePlotHeide$Calluna_phase_pioneer)
  structurePlotHeide$BedekkingOntwikkelingsstadium <- ifelse(is.na(structurePlotHeide$Calluna_phase_devel),0,structurePlotHeide$Calluna_phase_devel)
  structurePlotHeide$BedekkingClimaxstadium <- ifelse(is.na(structurePlotHeide$Calluna_phase_climax),0,structurePlotHeide$Calluna_phase_climax)
  structurePlotHeide$BedekkingDegeneratiestadium <- ifelse(is.na(structurePlotHeide$Calluna_phase_degen),0,structurePlotHeide$Calluna_phase_degen)

  structurePlotHeide$ouderdomstadia <- (structurePlotHeide$BedekkingPionierstadium > 0) + (structurePlotHeide$BedekkingOntwikkelingsstadium > 0) + (structurePlotHeide$BedekkingClimaxstadium > 0) + (structurePlotHeide$BedekkingDegeneratiestadium > 0)

  structurePlotHeide$struikhei <- structurePlotHeide$ouderdomstadia > 0

  structurePlotHeide$climaxOfDegradatieStadium <- (structurePlotHeide$BedekkingClimaxstadium > 0) | (structurePlotHeide$BedekkingDegeneratiestadium > 0)

  # bedekking naakte bodem
  structurePlotHeide$naakte_bodem <- ifelse(is.na(structurePlotHeide$Pioneer_phase_open_soil),0,structurePlotHeide$Pioneer_phase_open_soil)

  # pionierstadia/ bedekking open vegetatie
  structurePlotHeide$moslaag <- ifelse(is.na(structurePlotHeide$Pioneer_Mos),0,structurePlotHeide$Pioneer_Mos)
  structurePlotHeide$BedekkingBuntgras <- ifelse(is.na(structurePlotHeide$Pioneer_Coryn_Aira),0,structurePlotHeide$Pioneer_Coryn_Aira)

  structurePlotHeide$korstmosvegetaties <- ifelse(is.na(structurePlotHeide$Pioneer_Lichenen),0,structurePlotHeide$Pioneer_Lichenen)
  #
  structurePlotHeide$pionierStadia <- (structurePlotHeide$naakte_bodem > 0) + (structurePlotHeide$BedekkingBuntgras > 0) + (structurePlotHeide$moslaag > 0) + (structurePlotHeide$korstmosvegetaties > 0)

  structurePlotHeide$openVegetatieOfKaalZand <- pmin(structurePlotHeide$moslaag + structurePlotHeide$korstmosvegetaties + structurePlotHeide$naakte_bodem + structurePlotHeide$BedekkingBuntgras,100)

  structurePlotHeide$openVegetatie <- pmin(structurePlotHeide$moslaag + structurePlotHeide$korstmosvegetaties +  structurePlotHeide$BedekkingBuntgras, 100)

  # bedekking moslaag (mos + korstmos)
  structurePlotHeide$BedekkingMosEnKorstmos <- structurePlotHeide$moslaag + structurePlotHeide$korstmosvegetaties

  # bedekking veenmos
  structurePlotHeide$veenmoslaag <- ifelse(is.na(structurePlotHeide$Sphagnumlayer),0,structurePlotHeide$Sphagnumlayer)

  # verbossing
  structurePlotHeide$verbossing <- structurePlotHeide$Shrub_and_Treelayer_18m

  # vergrassing
  structurePlotHeide$vergrassing <- ifelse(is.na(structurePlotHeide$Herbs),0,structurePlotHeide$Herbs)

  # verruiging
  structurePlotHeide$verruiging <- ifelse(is.na(structurePlotHeide$Brushwood),0,structurePlotHeide$Brushwood)


  # vergrassing+verruiging
  structurePlotHeide$vergrassingEnVerruiging <- structurePlotHeide$vergrassing + structurePlotHeide$verruiging

  #invasieve exoten
  structurePlotHeide$invasieve_exoten <- ifelse(is.na(structurePlotHeide$Campylopus_introflexus),0,structurePlotHeide$Campylopus_introflexus)

  #result <- structurePlotHeide[,c("IDPlots","BedekkingDwergstruiken","AantalOuderdomstadiaAanwezig", "AantalOuderdomstadiaFrequent","VoorkomenClimaxOfDegeneratieStadium","BedekkingNaakteBodem", "BedekkingMosEnKorstmos","AantalPionierstadiaAanwezig","BedekkingOpenVegetatieZand", "BedekkingOpenVegetatie","BedekkingVeenmos","Vergrassing","Verruiging","VergrassingEnVerruiging","Verbossing","BedekkingInvasieveExoten")]

  # habitattype per plot

  structurePlotHeide <- merge (structurePlotHeide, plotHabtypes, by = "IDPlots")

  structurePlotHeide_long <- melt(structurePlotHeide, id.vars = c("IDPlots","HabCode"),variable.name = "AnalyseVariabele", value.name = "Waarde")

  structurePlotHeide_indicatoren <- merge(structurePlotHeide_long, indicatorenLSVI[indicatorenLSVI$Meting == "structuurplot",],  by = c("HabCode","AnalyseVariabele") )

  indicatoren <- structurePlotHeide_indicatoren[,c("HabCode","IDPlots", "Criterium", "Indicator", "AnalyseVariabele", "Soortengroep", "Vegetatielaag","Eenheid", "Drempelwaarde","Indicatortype","Meting", "Combinatie", "Waarde","VersieLSVI")]

  indicatoren$Beoordeling <- ifelse (indicatoren$Indicatortype == "negatief",
                                     ifelse(indicatoren$Waarde <= indicatoren$Drempelwaarde, 1,0),
                                     ifelse (indicatoren$Indicatortype == "positief",
                                             ifelse(indicatoren$Waarde >= indicatoren$Drempelwaarde,
                                                    1,
                                                    0),
                                             NA))

  return (indicatoren)

}


#############################################################################################################

### Berekening van LSVI-indicatoren voor vegetatie en verstoring op basis van vegetatie-opname

#?# Is er nog een correctie nodig van totale bedekking van een soortengroep in functie van totale bedekking van vegetatielaag.
#?# Misschien best soortenlijst met bedekkingen als input, dan wordt het generieker #?#

calculateLSVI_vegetatieopname <- function (plotHabtypes, bedekkingSoorten, bedekkingVeglagen  ,versieLSVI = "beide"){

  ### Soortenlijst opvragen voor gewenste versie van LSVI

  # connDB <-   odbcConnectAccess2007(dbLSVI)

  soortenlijstLSVI <- read.csv2(soortengroepenLSVI_fn)
  #soortenlijstLSVI <- soortenlijstLSVI[soortenlijstLSVI$LSVI_v3==1,]
  indicatorenLSVI <- read.csv2(indicatorenLSVI_fn)

  # odbcClose(connDB)

  # Long formaat
  soortenlijstLSVI <- soortenlijstLSVI %>%
    gather( Versie2, Versie3, key = VersieLSVI, value = Selectie) %>%
    filter(Selectie == 1 & !is.na(Selectie))

  soortenlijstLSVI$VersieLSVI <- tolower(soortenlijstLSVI$VersieLSVI)

  if (versieLSVI != "beide"){
    soortenlijstLSVI <- soortenlijstLSVI %>%
      filter(VersieLSVI == versieLSVI)
  }

  ### Selectie van indicatoren die op basis van vegetatieopname worden berekend
  indicatorenLSVI_selectie <- indicatorenLSVI[indicatorenLSVI$Meting == "vegetatieplot",]


  ### Totale bedekking van soorten voor verschillende (combinaties van) vegetatielagen
  somBedekkingSoorten <- ddply(bedekkingSoorten,.(IDPlots),summarise,
                               SomBedekkingKruidlaag = (1 - prod((100 - Cover * (Vegetatielaag == "kruidlaag"))/100, na.rm=TRUE)) * 100,
                               SomBedekkingStruiklaag = (1 - prod((100 - Cover * (Vegetatielaag == "struiklaag"))/100, na.rm=TRUE)) * 100,
                               SomBedekkingBoomlaag = (1 - prod((100 - Cover * (Vegetatielaag == "boomlaag"))/100, na.rm=TRUE)) * 100,
                               SomBedekkingBoomEnStruiklaag = (1 - prod((100 - Cover * (Vegetatielaag %in% c("boomlaag", "struiklaag")))/100, na.rm=TRUE)) * 100,
                               SomBedekkingTotaal = (1 - prod((100 - Cover) /100, na.rm=TRUE)) * 100)


  ### Berekeing van totale bedekking van combinaties van vegetatielagen op basis van ingeschatte bedekkingen per laag
  bedekkingVeglagen$CoverHerbShrubTreeLayer <- (1 - (100- bedekkingVeglagen$CoverHerblayer)/100 * (100- bedekkingVeglagen$CoverShrublayer)/100 * (100- bedekkingVeglagen$CoverTreelayer)/100) *100

  ### Aanmaak van data.frame met berekende waarden voor analysevariabelen per segment/plot

  # Eerst kennen we aan elke plot de juiste indicatoren toe op basis van het habitattype
  indicatoren <- merge(plotHabtypes,indicatorenLSVI_selectie, by = "HabCode")

  # Vervolgens lopen we de indicatoren een voor een af en berkenen we de waarde op basis van de vegetatie-opnamegegevens

  indicatoren$Waarde <- NA

  for (i in 1: nrow(indicatoren)){

    # lijst met soorten binnen gespecifieerde soortgroep
    soortenlijst <- soortenlijstLSVI[(as.character(soortenlijstLSVI$HabCode) == as.character(indicatoren$HabCode[i])) & (as.character(soortenlijstLSVI$Omschrijving) == as.character(indicatoren$Soortengroep[i])) & (as.character(soortenlijstLSVI$VersieLSVI) == as.character(indicatoren$VersieLSVI[i])) ,]$WetNaam

    # selectie van (opgemeten) soorten voor gespecifieerde plot en binnen gespecifieerde vegetatielaag
    if (is.na(indicatoren$Vegetatielaag[i])| indicatoren$Vegetatielaag[i] == "" ){

      soortenOpname <-  bedekkingSoorten[bedekkingSoorten$IDPlots == indicatoren$IDPlots[i],]

    } else if (indicatoren$Vegetatielaag[i] == "kruidlaag"){

      soortenOpname <-  bedekkingSoorten[bedekkingSoorten$IDPlots == indicatoren$IDPlots[i] & bedekkingSoorten$Vegetatielaag == "kruidlaag",]

    } else if (indicatoren$Vegetatielaag[i] == "boomEnStruiklaag"){

      soortenOpname <-  bedekkingSoorten[bedekkingSoorten$IDPlots == indicatoren$IDPlots[i] & bedekkingSoorten$Vegetatielaag %in% c("boomlaag","struiklaag"),]
    }

    # selectie van soorten binnen soortengroep en berekening gezamelijke bedekking of aantal soorten
    if (nrow(soortenOpname) > 0 ){

      selectieSoortenOpname <- soortenOpname[soortenOpname$NameSc %in% soortenlijst,]

      if (nrow(selectieSoortenOpname) == 0){

        indicatoren$Waarde[i] <- 0

      } else {

        if (indicatoren$Eenheid[i] == "bedekking"){

          bedekking <- selectieSoortenOpname$Cover
          bedekkingInv <- (100 - bedekking)/100
          bedekkingSom <- (1 - prod(bedekkingInv, na.rm=T))*100

          # indien sleutelsoorten kruidlaag boshabitat: relatieve bedekking t.o.v. bedekking kruidlaag
          if (indicatoren$Indicator[i] == "sleutelsoorten_kruidlaag"  ){

            bedekkingKruidlaag <- bedekkingVeglagen[bedekkingVeglagen$IDPlots == indicatoren$IDPlots[i], ]$CoverHerblayer

            # indien de bedekking van de kruidlaag ontbreekt, nemen we de som van de bedekkingen van alle soorten in de kruidlaag

            if (is.na(bedekkingKruidlaag)){

              bedekkingKruidlaag <- somBedekkingSoorten[somBedekkingSoorten$IDplots == indicatoren$IDPlots[i],]$SomBedekkingKruidlaag

            }

            bedekkingSom <- min(100,bedekkingSom/bedekkingKruidlaag * 100)

          }

          indicatoren$Waarde[i] <- bedekkingSom

        } else if (indicatoren$Eenheid[i] == "aantal"){

          indicatoren$Waarde[i] <- length(unique(selectieSoortenOpname$NameSc))

        } else if (indicatoren$Eenheid[i] == "aantalFrequent"){

          indicatoren$Waarde[i] <-  sum(selectieSoortenOpname$Cover >= 5)

        }

      }

    } else if (nrow(soortenOpname) == 0 ){

      indicatoren$Waarde[i] <- NA

    }

  }

  # beoordeling van analysevariabele op basis van drempelwaarde

  indicatoren$Beoordeling <- ifelse (indicatoren$Indicatortype == "negatief", ifelse(indicatoren$Waarde <= indicatoren$Drempelwaarde, 1,0),
                             ifelse (indicatoren$Indicatortype == "positief", ifelse(indicatoren$Waarde >= indicatoren$Drempelwaarde, 1,0),NA))

  indicatoren <- indicatoren [,!colnames(indicatoren) %in% "Id"]

  return(indicatoren)

}


#############################################################################################################

# calculateLSVI_Indicatoren_Bos <- function(db = dbVBI2, plotHabtypes,versieLSVI = "versie3" , niveau ="segment"){
#
#
#   habitatStructuur <- calculateLSVI_HabitatstructuurBosHabitats(db = db, plotHabtypes = plotHabtypes, niveau = niveau, versieLSVI = versieLSVI)
#
#   verstoringVegetatie <- calculateLSVI_VegetatieVerstoring (db = db, plotHabtypes = plotHabtypes, versieLSVI = versieLSVI)
#
#   if (niveau == "segment"){
#
#     plotSegments <- unique(habitatStructuur[,c("IDPlots","IDSegments")])
#
#     verstoringVegetatie <- merge (plotSegments, verstoringVegetatie, by = "IDPlots", all =TRUE)
#
#     verstoringVegetatie$IDSegments <- ifelse(is.na(verstoringVegetatie$IDSegments),1,verstoringVegetatie$IDSegments)
#
#
#   }
#
#   indicatoren <- rbind(habitatStructuur,verstoringVegetatie)
#   indicatoren <- indicatoren[order(indicatoren$Criterium),]
#   indicatoren <- indicatoren[order(indicatoren$HabCode),]
#   indicatoren <- indicatoren[order(indicatoren$IDPlots),]
#
#   return(indicatoren)
#
# }
#




####################################################################################
### NOG WAT FUNCTIES VOOR HABITATSLEUTEL TE KUNNEN TOEPASSEN
####################################################################################


getStandAge <- function(db = dbAnalyse, plotIDs = NULL, Periode = 2){

query_StandAge <- "SELECT tbl2BestandskaraktKwal.IDPlots,
tbl2BestandskaraktKwal.IDSegments,
tbl2BestandskaraktKwal.Periode,
tbl2BestandskaraktKwal.v5_StandAge
FROM tbl2BestandskaraktKwal;
"
connectieAnalyse <- odbcConnectAccess2007(dbAnalyse)

standAgeOrig <- sqlQuery(connectieAnalyse, query_StandAge, stringsAsFactors = TRUE)

odbcClose(connectieAnalyse)

standAge <- standAgeOrig[standAgeOrig$Periode == 2,]
standAge <- rename(standAge,c(v5_StandAge = "Bestandsleeftijd"))

standAge <- standAge[,c("IDPlots","IDSegments","Bestandsleeftijd")]


if (is.null(plotIDs)){

  result <- standAge

} else {

  result <- standAge[standAge$IDPlots %in% plotIDs,]

}

return(result)


}


######################################################

getLargeTreesVBI2 <- function (db = dbVBI2, soort = c("Inlandse eik", "Beuk"), enkelLevend = FALSE, plotIDs = NULL){

  query_Trees <- "SELECT
Trees_2eBosinv.IDPlots,
Trees_2eBosinv.DBH_mm,
Trees_2eBosinv.Species,
qTreeSpecies.Value,
Trees_2eBosinv.Status_tree
FROM Trees_2eBosinv INNER JOIN qTreeSpecies ON Trees_2eBosinv.Species = qTreeSpecies.ID;
"

  connectieVBI2 <- odbcConnectAccess(dbVBI2)

  treesOrig <- sqlQuery(connectieVBI2, query_Trees, stringsAsFactors = TRUE)

  odbcClose(connectieVBI2)

  trees <- rename (treesOrig, c(Species = "IDSpVBI2", Value = "NameNl"))

  if (enkelLevend){

    trees <- trees[trees$Status_tree == 1,]

  }

  trees$SelectedSpecies <- trees$NameNl %in% soort

  maxTreeSize_Plot <- ddply(trees,.(IDPlots), summarise,
                            MaxDiameterAllspecies_cm = max(DBH_mm, na.rm = TRUE)/10,
                            MaxDiameterSelectedSpecies_cm =max(DBH_mm * SelectedSpecies, na.rm=TRUE)/10)

  if (soort == ""){

    result <- maxTreeSize_Plot[,c("IDPlots", "MaxDiameterAllspecies_cm")]
    result <- rename (result, c(MaxDiameterAllspecies_cm = "MaxDiameter_cm"))

  } else {

    result <- maxTreeSize_Plot[,c("IDPlots", "MaxDiameterSelectedSpecies_cm")]
    result <- rename (result, c(MaxDiameterSelectedSpecies_cm = "MaxDiameter_cm"))
  }

  if (is.null(plotIDs)){

    result <- result

  } else {

    result <- result[result$IDPlots %in% plotIDs,]

  }

  return(result)




}



###############################################
### NOG EEN OUDE FUNCTIE VAN PIETER DIE IK NIET MEER GEBRUIK
###########################################################

#Functie die de mogelijkheid geeft om een som van bedekking of aantal soorten binnen een bepaalde laag of over alle lagen heen te berekenen.
critVoorkomenSoorten <- function(NR, habitat, soorten="<ALL>", fun="Aantal", laag="", vegdata, soortenlijst)
  #NR: een (character)vector die de plotnummers weergeeft
  #habitat: een (character)vector met dezelfde lengte als NR die het habitat per plotnummer weergeeft
  #soorten: een (character)vector met 1 of meerdere elementen die de variabelengroepen weergeeft die bij de gewenste indicator horen. Indien deze leeg is worden alle soorten genomen.
  #fun: kan "Aantal", "Bedekking", "Maximum" zijn en controleert welke functie we wensen te gebruiken
  #laag: de soorten uit welke laag nemen we?, Indien "" wordt niet gekeken naar de lagen en alle corresponderende soorten gegeven
  #vegdata: dataset met de vegetatieopnames
  #soortenlijst: dataset met soortenlijst voor de  variabelegroepen per habitat
  #structuurdata: dataset die de structuurkenmerken weergeeft, nodig als er delingen moeten gebeuren door totale bedekking van soorten.
{
  #als laag leeg, betekent dit alle lagen
  if("<ALL>" %in% soorten) soorten <- unique(soortenlijst$Omschrijving)
  tmpdata <- data.frame(NR=NR, HRL_code=habitat, stringsAsFactors=F)

  #Volgende lange functie berekent het aantal soorten, de bedekking of de maximale bedekking
  #De functie gaat rij per rij de plotnummers af, kiest de bijhorende bedekkingen en berekent het resultaat
  rv <- apply(tmpdata, 1, vegdata=vegdata, omschrijving=soorten, lijst=soortenlijst,
              function(x, vegdata, omschrijving, lijst){
                #Welke soorten horen bij de indicator die we onderzoeken
                hablijst <- lijst[lijst$HRL_code==x["HRL_code"] &  (lijst$Omschrijving %in% omschrijving), , drop=F]

                #indien er geen corresponderende soorten zijn voor deze indicator, zet het resultaat op NA
                if(dim(hablijst)[1]==0){
                  rv <- NA
                } else
                  #anders haal de opname van dit plot op uit de vegetatieopnames
                {
                  opname <- vegdata[vegdata$IDPlots==x["NR"], , drop=F] #as.character nodig omdat x een character vector wordt
                  #als er geen opnames zijn, zet het resultaat op 0
                  if(dim(opname)[1]==0){
                    rv <- 0; warning(paste("Plot",x["NR"], "heeft geen vegetatieopnames, criterium op 0 gezet"))
                  } else
                    #anders kies de gewenste functie en bereken het resultaat
                  {
                    #het aantal is de hoeveelheid soorten uit de gekozen indicator waargenoemen zijn
                    if(fun == "Aantal"){
                      rv <- length(unique(opname$NameSc[opname$NameSc %in% hablijst$VolNaam ]))
                    } else
                      #de bedekking sommeert de bedekkingen van de waargenomen soorten van de indicator
                      if (fun=="Bedekking")  #Keuze om de absolute bedekkingsoppervlakte te nemen
                      {
                        #indien geen laag gedefinieerd is, neem de som van alle bedekkingen
                        if(laag == "") {
                          bedekking <- opname$Cover[opname$NameSc %in% hablijst$VolNaam]
                          bedekkingInv <- (100 - bedekking)/100
                          rv <- (1 - prod(bedekkingInv, na.rm=T))*100

                        } else
                          #anders neem enkel de soorten die uit de gedefinieerde laag komen
                        {
                          bedekking <- opname$Cover[(opname$NameSc %in% hablijst$VolNaam) & opname$Vegetatielaag %in% laag]
                          bedekkingInv <- (100 - bedekking)/100
                          rv <- (1 - prod(bedekkingInv, na.rm=T))*100
                        }
                      } else
                        #Berekent gewoon de maximale bedekking
                        if (fun == "Maximum")
                        {
                          tvar <- opname$Cover[opname$NameSc %in% hablijst$VolNaam ]
                          if(length(tvar)==0) tvar <- 0
                          rv <- max(as.numeric(tvar), na.rm=T)
                        }
                    #als geen van voorgaande functies doorgegeven was, zet het resultaat op NA
                    else rv <- NA
                  }
                }
                rv
              })
  rv
}



