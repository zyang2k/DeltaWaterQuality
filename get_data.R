# Package ID: edi.458.12 Cataloging System:https://pasta.edirepository.org.
# Data set title: Interagency Ecological Program: Discrete water quality monitoring in the Sacramento-San Joaquin Bay-Delta, collected by the Environmental Monitoring Program, 1975-2023.
# Data set creator:  Morgan Battey - California Department of Water Resources 
# Data set creator:  Sarah Perry - California Department of Water Resources 
# Contact:  Morgan Battey - Environmental Scientist California Department of Water Resources  - morgan.battey@water.ca.gov
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/458/12/a8ca73ec21f14b58caf2152720403cf3" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "Station",     
                 "Date",     
                 "Time",     
                 "SampleDescription",     
                 "Flag",     
                 "FlagDescription",     
                 "FieldNotes",     
                 "Weather",     
                 "AirTemp",     
                 "WindVelocity",     
                 "WindDirection",     
                 "Latitude",     
                 "Longitude",     
                 "Chla_Sign",     
                 "Chla",     
                 "Pheoa_Sign",     
                 "Pheoa",     
                 "TotAlkalinity_Sign",     
                 "TotAlkalinity",     
                 "TotAmmonia_Sign",     
                 "TotAmmonia",     
                 "DissAmmonia_Sign",     
                 "DissAmmonia",     
                 "DissBromide_Sign",     
                 "DissBromide",     
                 "DissCalcium_Sign",     
                 "DissCalcium",     
                 "TotChloride_Sign",     
                 "TotChloride",     
                 "DissChloride_Sign",     
                 "DissChloride",     
                 "DissNitrateNitrite_Sign",     
                 "DissNitrateNitrite",     
                 "DOC_Sign",     
                 "DOC",     
                 "TOC_Sign",     
                 "TOC",     
                 "DON_Sign",     
                 "DON",     
                 "TON_Sign",     
                 "TON",     
                 "DissOrthophos_Sign",     
                 "DissOrthophos",     
                 "TotPhos_Sign",     
                 "TotPhos",     
                 "DissSilica_Sign",     
                 "DissSilica",     
                 "TDS_Sign",     
                 "TDS",     
                 "TSS_Sign",     
                 "TSS",     
                 "VSS_Sign",     
                 "VSS",     
                 "TKN_Sign",     
                 "TKN",     
                 "WaterDepth",     
                 "Secchi",     
                 "Microcystis",     
                 "LightExtinction",     
                 "SpCndSurface",     
                 "DOSurface",     
                 "DOpercentSurface",     
                 "WaterTempSurface",     
                 "TurbiditySurface",     
                 "pHSurface",     
                 "SpCndBottom",     
                 "DOBottom",     
                 "DOpercentBottom",     
                 "WaterTempBottom",     
                 "TurbidityBottom",     
                 "pHBottom"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$Station)!="factor") dt1$Station<- as.factor(dt1$Station)                                   
# attempting to convert dt1$Date dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1Date<-as.Date(dt1$Date,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(nrow(dt1[dt1$Date != "",]) == length(tmp1Date[!is.na(tmp1Date)])){dt1$Date <- tmp1Date } else {print("Date conversion failed for dt1$Date. Please inspect the data and do the date conversion yourself.")}                                                                    

if (class(dt1$Time)!="factor") dt1$Time<- as.factor(dt1$Time)
if (class(dt1$SampleDescription)!="factor") dt1$SampleDescription<- as.factor(dt1$SampleDescription)
if (class(dt1$Flag)!="factor") dt1$Flag<- as.factor(dt1$Flag)
if (class(dt1$FlagDescription)!="factor") dt1$FlagDescription<- as.factor(dt1$FlagDescription)
if (class(dt1$FieldNotes)!="factor") dt1$FieldNotes<- as.factor(dt1$FieldNotes)
if (class(dt1$Weather)!="factor") dt1$Weather<- as.factor(dt1$Weather)
if (class(dt1$AirTemp)=="factor") dt1$AirTemp <-as.numeric(levels(dt1$AirTemp))[as.integer(dt1$AirTemp) ]               
if (class(dt1$AirTemp)=="character") dt1$AirTemp <-as.numeric(dt1$AirTemp)
if (class(dt1$WindVelocity)=="factor") dt1$WindVelocity <-as.numeric(levels(dt1$WindVelocity))[as.integer(dt1$WindVelocity) ]               
if (class(dt1$WindVelocity)=="character") dt1$WindVelocity <-as.numeric(dt1$WindVelocity)
if (class(dt1$WindDirection)=="factor") dt1$WindDirection <-as.numeric(levels(dt1$WindDirection))[as.integer(dt1$WindDirection) ]               
if (class(dt1$WindDirection)=="character") dt1$WindDirection <-as.numeric(dt1$WindDirection)
if (class(dt1$Latitude)=="factor") dt1$Latitude <-as.numeric(levels(dt1$Latitude))[as.integer(dt1$Latitude) ]               
if (class(dt1$Latitude)=="character") dt1$Latitude <-as.numeric(dt1$Latitude)
if (class(dt1$Longitude)=="factor") dt1$Longitude <-as.numeric(levels(dt1$Longitude))[as.integer(dt1$Longitude) ]               
if (class(dt1$Longitude)=="character") dt1$Longitude <-as.numeric(dt1$Longitude)
if (class(dt1$Chla_Sign)!="factor") dt1$Chla_Sign<- as.factor(dt1$Chla_Sign)
if (class(dt1$Chla)=="factor") dt1$Chla <-as.numeric(levels(dt1$Chla))[as.integer(dt1$Chla) ]               
if (class(dt1$Chla)=="character") dt1$Chla <-as.numeric(dt1$Chla)
if (class(dt1$Pheoa_Sign)!="factor") dt1$Pheoa_Sign<- as.factor(dt1$Pheoa_Sign)
if (class(dt1$Pheoa)=="factor") dt1$Pheoa <-as.numeric(levels(dt1$Pheoa))[as.integer(dt1$Pheoa) ]               
if (class(dt1$Pheoa)=="character") dt1$Pheoa <-as.numeric(dt1$Pheoa)
if (class(dt1$TotAlkalinity_Sign)!="factor") dt1$TotAlkalinity_Sign<- as.factor(dt1$TotAlkalinity_Sign)
if (class(dt1$TotAlkalinity)=="factor") dt1$TotAlkalinity <-as.numeric(levels(dt1$TotAlkalinity))[as.integer(dt1$TotAlkalinity) ]               
if (class(dt1$TotAlkalinity)=="character") dt1$TotAlkalinity <-as.numeric(dt1$TotAlkalinity)
if (class(dt1$TotAmmonia_Sign)!="factor") dt1$TotAmmonia_Sign<- as.factor(dt1$TotAmmonia_Sign)
if (class(dt1$TotAmmonia)=="factor") dt1$TotAmmonia <-as.numeric(levels(dt1$TotAmmonia))[as.integer(dt1$TotAmmonia) ]               
if (class(dt1$TotAmmonia)=="character") dt1$TotAmmonia <-as.numeric(dt1$TotAmmonia)
if (class(dt1$DissAmmonia_Sign)!="factor") dt1$DissAmmonia_Sign<- as.factor(dt1$DissAmmonia_Sign)
if (class(dt1$DissAmmonia)=="factor") dt1$DissAmmonia <-as.numeric(levels(dt1$DissAmmonia))[as.integer(dt1$DissAmmonia) ]               
if (class(dt1$DissAmmonia)=="character") dt1$DissAmmonia <-as.numeric(dt1$DissAmmonia)
if (class(dt1$DissBromide_Sign)!="factor") dt1$DissBromide_Sign<- as.factor(dt1$DissBromide_Sign)
if (class(dt1$DissBromide)=="factor") dt1$DissBromide <-as.numeric(levels(dt1$DissBromide))[as.integer(dt1$DissBromide) ]               
if (class(dt1$DissBromide)=="character") dt1$DissBromide <-as.numeric(dt1$DissBromide)
if (class(dt1$DissCalcium_Sign)!="factor") dt1$DissCalcium_Sign<- as.factor(dt1$DissCalcium_Sign)
if (class(dt1$DissCalcium)=="factor") dt1$DissCalcium <-as.numeric(levels(dt1$DissCalcium))[as.integer(dt1$DissCalcium) ]               
if (class(dt1$DissCalcium)=="character") dt1$DissCalcium <-as.numeric(dt1$DissCalcium)
if (class(dt1$TotChloride_Sign)!="factor") dt1$TotChloride_Sign<- as.factor(dt1$TotChloride_Sign)
if (class(dt1$TotChloride)=="factor") dt1$TotChloride <-as.numeric(levels(dt1$TotChloride))[as.integer(dt1$TotChloride) ]               
if (class(dt1$TotChloride)=="character") dt1$TotChloride <-as.numeric(dt1$TotChloride)
if (class(dt1$DissChloride_Sign)!="factor") dt1$DissChloride_Sign<- as.factor(dt1$DissChloride_Sign)
if (class(dt1$DissChloride)=="factor") dt1$DissChloride <-as.numeric(levels(dt1$DissChloride))[as.integer(dt1$DissChloride) ]               
if (class(dt1$DissChloride)=="character") dt1$DissChloride <-as.numeric(dt1$DissChloride)
if (class(dt1$DissNitrateNitrite_Sign)!="factor") dt1$DissNitrateNitrite_Sign<- as.factor(dt1$DissNitrateNitrite_Sign)
if (class(dt1$DissNitrateNitrite)=="factor") dt1$DissNitrateNitrite <-as.numeric(levels(dt1$DissNitrateNitrite))[as.integer(dt1$DissNitrateNitrite) ]               
if (class(dt1$DissNitrateNitrite)=="character") dt1$DissNitrateNitrite <-as.numeric(dt1$DissNitrateNitrite)
if (class(dt1$DOC_Sign)!="factor") dt1$DOC_Sign<- as.factor(dt1$DOC_Sign)
if (class(dt1$DOC)=="factor") dt1$DOC <-as.numeric(levels(dt1$DOC))[as.integer(dt1$DOC) ]               
if (class(dt1$DOC)=="character") dt1$DOC <-as.numeric(dt1$DOC)
if (class(dt1$TOC_Sign)!="factor") dt1$TOC_Sign<- as.factor(dt1$TOC_Sign)
if (class(dt1$TOC)=="factor") dt1$TOC <-as.numeric(levels(dt1$TOC))[as.integer(dt1$TOC) ]               
if (class(dt1$TOC)=="character") dt1$TOC <-as.numeric(dt1$TOC)
if (class(dt1$DON_Sign)!="factor") dt1$DON_Sign<- as.factor(dt1$DON_Sign)
if (class(dt1$DON)=="factor") dt1$DON <-as.numeric(levels(dt1$DON))[as.integer(dt1$DON) ]               
if (class(dt1$DON)=="character") dt1$DON <-as.numeric(dt1$DON)
if (class(dt1$TON_Sign)!="factor") dt1$TON_Sign<- as.factor(dt1$TON_Sign)
if (class(dt1$TON)=="factor") dt1$TON <-as.numeric(levels(dt1$TON))[as.integer(dt1$TON) ]               
if (class(dt1$TON)=="character") dt1$TON <-as.numeric(dt1$TON)
if (class(dt1$DissOrthophos_Sign)!="factor") dt1$DissOrthophos_Sign<- as.factor(dt1$DissOrthophos_Sign)
if (class(dt1$DissOrthophos)=="factor") dt1$DissOrthophos <-as.numeric(levels(dt1$DissOrthophos))[as.integer(dt1$DissOrthophos) ]               
if (class(dt1$DissOrthophos)=="character") dt1$DissOrthophos <-as.numeric(dt1$DissOrthophos)
if (class(dt1$TotPhos_Sign)!="factor") dt1$TotPhos_Sign<- as.factor(dt1$TotPhos_Sign)
if (class(dt1$TotPhos)=="factor") dt1$TotPhos <-as.numeric(levels(dt1$TotPhos))[as.integer(dt1$TotPhos) ]               
if (class(dt1$TotPhos)=="character") dt1$TotPhos <-as.numeric(dt1$TotPhos)
if (class(dt1$DissSilica_Sign)!="factor") dt1$DissSilica_Sign<- as.factor(dt1$DissSilica_Sign)
if (class(dt1$DissSilica)=="factor") dt1$DissSilica <-as.numeric(levels(dt1$DissSilica))[as.integer(dt1$DissSilica) ]               
if (class(dt1$DissSilica)=="character") dt1$DissSilica <-as.numeric(dt1$DissSilica)
if (class(dt1$TDS_Sign)!="factor") dt1$TDS_Sign<- as.factor(dt1$TDS_Sign)
if (class(dt1$TDS)=="factor") dt1$TDS <-as.numeric(levels(dt1$TDS))[as.integer(dt1$TDS) ]               
if (class(dt1$TDS)=="character") dt1$TDS <-as.numeric(dt1$TDS)
if (class(dt1$TSS_Sign)!="factor") dt1$TSS_Sign<- as.factor(dt1$TSS_Sign)
if (class(dt1$TSS)=="factor") dt1$TSS <-as.numeric(levels(dt1$TSS))[as.integer(dt1$TSS) ]               
if (class(dt1$TSS)=="character") dt1$TSS <-as.numeric(dt1$TSS)
if (class(dt1$VSS_Sign)!="factor") dt1$VSS_Sign<- as.factor(dt1$VSS_Sign)
if (class(dt1$VSS)=="factor") dt1$VSS <-as.numeric(levels(dt1$VSS))[as.integer(dt1$VSS) ]               
if (class(dt1$VSS)=="character") dt1$VSS <-as.numeric(dt1$VSS)
if (class(dt1$TKN_Sign)!="factor") dt1$TKN_Sign<- as.factor(dt1$TKN_Sign)
if (class(dt1$TKN)=="factor") dt1$TKN <-as.numeric(levels(dt1$TKN))[as.integer(dt1$TKN) ]               
if (class(dt1$TKN)=="character") dt1$TKN <-as.numeric(dt1$TKN)
if (class(dt1$WaterDepth)=="factor") dt1$WaterDepth <-as.numeric(levels(dt1$WaterDepth))[as.integer(dt1$WaterDepth) ]               
if (class(dt1$WaterDepth)=="character") dt1$WaterDepth <-as.numeric(dt1$WaterDepth)
if (class(dt1$Secchi)=="factor") dt1$Secchi <-as.numeric(levels(dt1$Secchi))[as.integer(dt1$Secchi) ]               
if (class(dt1$Secchi)=="character") dt1$Secchi <-as.numeric(dt1$Secchi)
if (class(dt1$Microcystis)!="factor") dt1$Microcystis<- as.factor(dt1$Microcystis)
if (class(dt1$LightExtinction)=="factor") dt1$LightExtinction <-as.numeric(levels(dt1$LightExtinction))[as.integer(dt1$LightExtinction) ]               
if (class(dt1$LightExtinction)=="character") dt1$LightExtinction <-as.numeric(dt1$LightExtinction)
if (class(dt1$SpCndSurface)=="factor") dt1$SpCndSurface <-as.numeric(levels(dt1$SpCndSurface))[as.integer(dt1$SpCndSurface) ]               
if (class(dt1$SpCndSurface)=="character") dt1$SpCndSurface <-as.numeric(dt1$SpCndSurface)
if (class(dt1$DOSurface)=="factor") dt1$DOSurface <-as.numeric(levels(dt1$DOSurface))[as.integer(dt1$DOSurface) ]               
if (class(dt1$DOSurface)=="character") dt1$DOSurface <-as.numeric(dt1$DOSurface)
if (class(dt1$DOpercentSurface)=="factor") dt1$DOpercentSurface <-as.numeric(levels(dt1$DOpercentSurface))[as.integer(dt1$DOpercentSurface) ]               
if (class(dt1$DOpercentSurface)=="character") dt1$DOpercentSurface <-as.numeric(dt1$DOpercentSurface)
if (class(dt1$WaterTempSurface)=="factor") dt1$WaterTempSurface <-as.numeric(levels(dt1$WaterTempSurface))[as.integer(dt1$WaterTempSurface) ]               
if (class(dt1$WaterTempSurface)=="character") dt1$WaterTempSurface <-as.numeric(dt1$WaterTempSurface)
if (class(dt1$TurbiditySurface)=="factor") dt1$TurbiditySurface <-as.numeric(levels(dt1$TurbiditySurface))[as.integer(dt1$TurbiditySurface) ]               
if (class(dt1$TurbiditySurface)=="character") dt1$TurbiditySurface <-as.numeric(dt1$TurbiditySurface)
if (class(dt1$pHSurface)=="factor") dt1$pHSurface <-as.numeric(levels(dt1$pHSurface))[as.integer(dt1$pHSurface) ]               
if (class(dt1$pHSurface)=="character") dt1$pHSurface <-as.numeric(dt1$pHSurface)
if (class(dt1$SpCndBottom)=="factor") dt1$SpCndBottom <-as.numeric(levels(dt1$SpCndBottom))[as.integer(dt1$SpCndBottom) ]               
if (class(dt1$SpCndBottom)=="character") dt1$SpCndBottom <-as.numeric(dt1$SpCndBottom)
if (class(dt1$DOBottom)=="factor") dt1$DOBottom <-as.numeric(levels(dt1$DOBottom))[as.integer(dt1$DOBottom) ]               
if (class(dt1$DOBottom)=="character") dt1$DOBottom <-as.numeric(dt1$DOBottom)
if (class(dt1$DOpercentBottom)=="factor") dt1$DOpercentBottom <-as.numeric(levels(dt1$DOpercentBottom))[as.integer(dt1$DOpercentBottom) ]               
if (class(dt1$DOpercentBottom)=="character") dt1$DOpercentBottom <-as.numeric(dt1$DOpercentBottom)
if (class(dt1$WaterTempBottom)=="factor") dt1$WaterTempBottom <-as.numeric(levels(dt1$WaterTempBottom))[as.integer(dt1$WaterTempBottom) ]               
if (class(dt1$WaterTempBottom)=="character") dt1$WaterTempBottom <-as.numeric(dt1$WaterTempBottom)
if (class(dt1$TurbidityBottom)=="factor") dt1$TurbidityBottom <-as.numeric(levels(dt1$TurbidityBottom))[as.integer(dt1$TurbidityBottom) ]               
if (class(dt1$TurbidityBottom)=="character") dt1$TurbidityBottom <-as.numeric(dt1$TurbidityBottom)
if (class(dt1$pHBottom)=="factor") dt1$pHBottom <-as.numeric(levels(dt1$pHBottom))[as.integer(dt1$pHBottom) ]               
if (class(dt1$pHBottom)=="character") dt1$pHBottom <-as.numeric(dt1$pHBottom)

# Convert Missing Values to NA for non-dates

dt1$Time <- as.factor(ifelse((trimws(as.character(dt1$Time))==trimws("NA")),NA,as.character(dt1$Time)))
dt1$SampleDescription <- as.factor(ifelse((trimws(as.character(dt1$SampleDescription))==trimws("NA")),NA,as.character(dt1$SampleDescription)))
dt1$Flag <- as.factor(ifelse((trimws(as.character(dt1$Flag))==trimws("NA")),NA,as.character(dt1$Flag)))
dt1$FlagDescription <- as.factor(ifelse((trimws(as.character(dt1$FlagDescription))==trimws("NA")),NA,as.character(dt1$FlagDescription)))
dt1$FieldNotes <- as.factor(ifelse((trimws(as.character(dt1$FieldNotes))==trimws("NA")),NA,as.character(dt1$FieldNotes)))
dt1$Weather <- as.factor(ifelse((trimws(as.character(dt1$Weather))==trimws("NA")),NA,as.character(dt1$Weather)))
dt1$AirTemp <- ifelse((trimws(as.character(dt1$AirTemp))==trimws("NA")),NA,dt1$AirTemp)               
suppressWarnings(dt1$AirTemp <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$AirTemp))==as.character(as.numeric("NA"))),NA,dt1$AirTemp))
dt1$WindVelocity <- ifelse((trimws(as.character(dt1$WindVelocity))==trimws("NA")),NA,dt1$WindVelocity)               
suppressWarnings(dt1$WindVelocity <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$WindVelocity))==as.character(as.numeric("NA"))),NA,dt1$WindVelocity))
dt1$WindDirection <- ifelse((trimws(as.character(dt1$WindDirection))==trimws("NA")),NA,dt1$WindDirection)               
suppressWarnings(dt1$WindDirection <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$WindDirection))==as.character(as.numeric("NA"))),NA,dt1$WindDirection))
dt1$Latitude <- ifelse((trimws(as.character(dt1$Latitude))==trimws("NA")),NA,dt1$Latitude)               
suppressWarnings(dt1$Latitude <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Latitude))==as.character(as.numeric("NA"))),NA,dt1$Latitude))
dt1$Longitude <- ifelse((trimws(as.character(dt1$Longitude))==trimws("NA")),NA,dt1$Longitude)               
suppressWarnings(dt1$Longitude <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Longitude))==as.character(as.numeric("NA"))),NA,dt1$Longitude))
dt1$Chla_Sign <- as.factor(ifelse((trimws(as.character(dt1$Chla_Sign))==trimws("NA")),NA,as.character(dt1$Chla_Sign)))
dt1$Chla <- ifelse((trimws(as.character(dt1$Chla))==trimws("NA")),NA,dt1$Chla)               
suppressWarnings(dt1$Chla <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Chla))==as.character(as.numeric("NA"))),NA,dt1$Chla))
dt1$Pheoa <- ifelse((trimws(as.character(dt1$Pheoa))==trimws("NA")),NA,dt1$Pheoa)               
suppressWarnings(dt1$Pheoa <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Pheoa))==as.character(as.numeric("NA"))),NA,dt1$Pheoa))
dt1$TotAlkalinity <- ifelse((trimws(as.character(dt1$TotAlkalinity))==trimws("NA")),NA,dt1$TotAlkalinity)               
suppressWarnings(dt1$TotAlkalinity <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$TotAlkalinity))==as.character(as.numeric("NA"))),NA,dt1$TotAlkalinity))
dt1$TotAmmonia <- ifelse((trimws(as.character(dt1$TotAmmonia))==trimws("NA")),NA,dt1$TotAmmonia)               
suppressWarnings(dt1$TotAmmonia <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$TotAmmonia))==as.character(as.numeric("NA"))),NA,dt1$TotAmmonia))
dt1$DissAmmonia <- ifelse((trimws(as.character(dt1$DissAmmonia))==trimws("NA")),NA,dt1$DissAmmonia)               
suppressWarnings(dt1$DissAmmonia <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DissAmmonia))==as.character(as.numeric("NA"))),NA,dt1$DissAmmonia))
dt1$DissBromide <- ifelse((trimws(as.character(dt1$DissBromide))==trimws("NA")),NA,dt1$DissBromide)               
suppressWarnings(dt1$DissBromide <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DissBromide))==as.character(as.numeric("NA"))),NA,dt1$DissBromide))
dt1$DissCalcium <- ifelse((trimws(as.character(dt1$DissCalcium))==trimws("NA")),NA,dt1$DissCalcium)               
suppressWarnings(dt1$DissCalcium <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DissCalcium))==as.character(as.numeric("NA"))),NA,dt1$DissCalcium))
dt1$TotChloride <- ifelse((trimws(as.character(dt1$TotChloride))==trimws("NA")),NA,dt1$TotChloride)               
suppressWarnings(dt1$TotChloride <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$TotChloride))==as.character(as.numeric("NA"))),NA,dt1$TotChloride))
dt1$DissChloride <- ifelse((trimws(as.character(dt1$DissChloride))==trimws("NA")),NA,dt1$DissChloride)               
suppressWarnings(dt1$DissChloride <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DissChloride))==as.character(as.numeric("NA"))),NA,dt1$DissChloride))
dt1$DissNitrateNitrite <- ifelse((trimws(as.character(dt1$DissNitrateNitrite))==trimws("NA")),NA,dt1$DissNitrateNitrite)               
suppressWarnings(dt1$DissNitrateNitrite <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DissNitrateNitrite))==as.character(as.numeric("NA"))),NA,dt1$DissNitrateNitrite))
dt1$DOC <- ifelse((trimws(as.character(dt1$DOC))==trimws("NA")),NA,dt1$DOC)               
suppressWarnings(dt1$DOC <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DOC))==as.character(as.numeric("NA"))),NA,dt1$DOC))
dt1$TOC <- ifelse((trimws(as.character(dt1$TOC))==trimws("NA")),NA,dt1$TOC)               
suppressWarnings(dt1$TOC <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$TOC))==as.character(as.numeric("NA"))),NA,dt1$TOC))
dt1$DON <- ifelse((trimws(as.character(dt1$DON))==trimws("NA")),NA,dt1$DON)               
suppressWarnings(dt1$DON <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DON))==as.character(as.numeric("NA"))),NA,dt1$DON))
dt1$TON <- ifelse((trimws(as.character(dt1$TON))==trimws("NA")),NA,dt1$TON)               
suppressWarnings(dt1$TON <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$TON))==as.character(as.numeric("NA"))),NA,dt1$TON))
dt1$DissOrthophos <- ifelse((trimws(as.character(dt1$DissOrthophos))==trimws("NA")),NA,dt1$DissOrthophos)               
suppressWarnings(dt1$DissOrthophos <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DissOrthophos))==as.character(as.numeric("NA"))),NA,dt1$DissOrthophos))
dt1$TotPhos <- ifelse((trimws(as.character(dt1$TotPhos))==trimws("NA")),NA,dt1$TotPhos)               
suppressWarnings(dt1$TotPhos <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$TotPhos))==as.character(as.numeric("NA"))),NA,dt1$TotPhos))
dt1$DissSilica <- ifelse((trimws(as.character(dt1$DissSilica))==trimws("NA")),NA,dt1$DissSilica)               
suppressWarnings(dt1$DissSilica <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DissSilica))==as.character(as.numeric("NA"))),NA,dt1$DissSilica))
dt1$TDS <- ifelse((trimws(as.character(dt1$TDS))==trimws("NA")),NA,dt1$TDS)               
suppressWarnings(dt1$TDS <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$TDS))==as.character(as.numeric("NA"))),NA,dt1$TDS))
dt1$TSS <- ifelse((trimws(as.character(dt1$TSS))==trimws("NA")),NA,dt1$TSS)               
suppressWarnings(dt1$TSS <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$TSS))==as.character(as.numeric("NA"))),NA,dt1$TSS))
dt1$VSS <- ifelse((trimws(as.character(dt1$VSS))==trimws("NA")),NA,dt1$VSS)               
suppressWarnings(dt1$VSS <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$VSS))==as.character(as.numeric("NA"))),NA,dt1$VSS))
dt1$TKN <- ifelse((trimws(as.character(dt1$TKN))==trimws("NA")),NA,dt1$TKN)               
suppressWarnings(dt1$TKN <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$TKN))==as.character(as.numeric("NA"))),NA,dt1$TKN))
dt1$WaterDepth <- ifelse((trimws(as.character(dt1$WaterDepth))==trimws("NA")),NA,dt1$WaterDepth)               
suppressWarnings(dt1$WaterDepth <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$WaterDepth))==as.character(as.numeric("NA"))),NA,dt1$WaterDepth))
dt1$Secchi <- ifelse((trimws(as.character(dt1$Secchi))==trimws("NA")),NA,dt1$Secchi)               
suppressWarnings(dt1$Secchi <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Secchi))==as.character(as.numeric("NA"))),NA,dt1$Secchi))
dt1$Microcystis <- as.factor(ifelse((trimws(as.character(dt1$Microcystis))==trimws("NA")),NA,as.character(dt1$Microcystis)))
dt1$LightExtinction <- ifelse((trimws(as.character(dt1$LightExtinction))==trimws("NA")),NA,dt1$LightExtinction)               
suppressWarnings(dt1$LightExtinction <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$LightExtinction))==as.character(as.numeric("NA"))),NA,dt1$LightExtinction))
dt1$SpCndSurface <- ifelse((trimws(as.character(dt1$SpCndSurface))==trimws("NA")),NA,dt1$SpCndSurface)               
suppressWarnings(dt1$SpCndSurface <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$SpCndSurface))==as.character(as.numeric("NA"))),NA,dt1$SpCndSurface))
dt1$DOSurface <- ifelse((trimws(as.character(dt1$DOSurface))==trimws("NA")),NA,dt1$DOSurface)               
suppressWarnings(dt1$DOSurface <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DOSurface))==as.character(as.numeric("NA"))),NA,dt1$DOSurface))
dt1$DOpercentSurface <- ifelse((trimws(as.character(dt1$DOpercentSurface))==trimws("NA")),NA,dt1$DOpercentSurface)               
suppressWarnings(dt1$DOpercentSurface <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DOpercentSurface))==as.character(as.numeric("NA"))),NA,dt1$DOpercentSurface))
dt1$WaterTempSurface <- ifelse((trimws(as.character(dt1$WaterTempSurface))==trimws("NA")),NA,dt1$WaterTempSurface)               
suppressWarnings(dt1$WaterTempSurface <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$WaterTempSurface))==as.character(as.numeric("NA"))),NA,dt1$WaterTempSurface))
dt1$TurbiditySurface <- ifelse((trimws(as.character(dt1$TurbiditySurface))==trimws("NA")),NA,dt1$TurbiditySurface)               
suppressWarnings(dt1$TurbiditySurface <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$TurbiditySurface))==as.character(as.numeric("NA"))),NA,dt1$TurbiditySurface))
dt1$pHSurface <- ifelse((trimws(as.character(dt1$pHSurface))==trimws("NA")),NA,dt1$pHSurface)               
suppressWarnings(dt1$pHSurface <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$pHSurface))==as.character(as.numeric("NA"))),NA,dt1$pHSurface))
dt1$SpCndBottom <- ifelse((trimws(as.character(dt1$SpCndBottom))==trimws("NA")),NA,dt1$SpCndBottom)               
suppressWarnings(dt1$SpCndBottom <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$SpCndBottom))==as.character(as.numeric("NA"))),NA,dt1$SpCndBottom))
dt1$DOBottom <- ifelse((trimws(as.character(dt1$DOBottom))==trimws("NA")),NA,dt1$DOBottom)               
suppressWarnings(dt1$DOBottom <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DOBottom))==as.character(as.numeric("NA"))),NA,dt1$DOBottom))
dt1$DOpercentBottom <- ifelse((trimws(as.character(dt1$DOpercentBottom))==trimws("NA")),NA,dt1$DOpercentBottom)               
suppressWarnings(dt1$DOpercentBottom <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DOpercentBottom))==as.character(as.numeric("NA"))),NA,dt1$DOpercentBottom))
dt1$WaterTempBottom <- ifelse((trimws(as.character(dt1$WaterTempBottom))==trimws("NA")),NA,dt1$WaterTempBottom)               
suppressWarnings(dt1$WaterTempBottom <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$WaterTempBottom))==as.character(as.numeric("NA"))),NA,dt1$WaterTempBottom))
dt1$TurbidityBottom <- ifelse((trimws(as.character(dt1$TurbidityBottom))==trimws("NA")),NA,dt1$TurbidityBottom)               
suppressWarnings(dt1$TurbidityBottom <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$TurbidityBottom))==as.character(as.numeric("NA"))),NA,dt1$TurbidityBottom))
dt1$pHBottom <- ifelse((trimws(as.character(dt1$pHBottom))==trimws("NA")),NA,dt1$pHBottom)               
suppressWarnings(dt1$pHBottom <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$pHBottom))==as.character(as.numeric("NA"))),NA,dt1$pHBottom))


# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(Station)
summary(Date)
summary(Time)
summary(SampleDescription)
summary(Flag)
summary(FlagDescription)
summary(FieldNotes)
summary(Weather)
summary(AirTemp)
summary(WindVelocity)
summary(WindDirection)
summary(Latitude)
summary(Longitude)
summary(Chla_Sign)
summary(Chla)
summary(Pheoa_Sign)
summary(Pheoa)
summary(TotAlkalinity_Sign)
summary(TotAlkalinity)
summary(TotAmmonia_Sign)
summary(TotAmmonia)
summary(DissAmmonia_Sign)
summary(DissAmmonia)
summary(DissBromide_Sign)
summary(DissBromide)
summary(DissCalcium_Sign)
summary(DissCalcium)
summary(TotChloride_Sign)
summary(TotChloride)
summary(DissChloride_Sign)
summary(DissChloride)
summary(DissNitrateNitrite_Sign)
summary(DissNitrateNitrite)
summary(DOC_Sign)
summary(DOC)
summary(TOC_Sign)
summary(TOC)
summary(DON_Sign)
summary(DON)
summary(TON_Sign)
summary(TON)
summary(DissOrthophos_Sign)
summary(DissOrthophos)
summary(TotPhos_Sign)
summary(TotPhos)
summary(DissSilica_Sign)
summary(DissSilica)
summary(TDS_Sign)
summary(TDS)
summary(TSS_Sign)
summary(TSS)
summary(VSS_Sign)
summary(VSS)
summary(TKN_Sign)
summary(TKN)
summary(WaterDepth)
summary(Secchi)
summary(Microcystis)
summary(LightExtinction)
summary(SpCndSurface)
summary(DOSurface)
summary(DOpercentSurface)
summary(WaterTempSurface)
summary(TurbiditySurface)
summary(pHSurface)
summary(SpCndBottom)
summary(DOBottom)
summary(DOpercentBottom)
summary(WaterTempBottom)
summary(TurbidityBottom)
summary(pHBottom) 
# Get more details on character variables

summary(as.factor(dt1$Station)) 
summary(as.factor(dt1$Time)) 
summary(as.factor(dt1$SampleDescription)) 
summary(as.factor(dt1$Flag)) 
summary(as.factor(dt1$FlagDescription)) 
summary(as.factor(dt1$FieldNotes)) 
summary(as.factor(dt1$Weather)) 
summary(as.factor(dt1$Chla_Sign)) 
summary(as.factor(dt1$Pheoa_Sign)) 
summary(as.factor(dt1$TotAlkalinity_Sign)) 
summary(as.factor(dt1$TotAmmonia_Sign)) 
summary(as.factor(dt1$DissAmmonia_Sign)) 
summary(as.factor(dt1$DissBromide_Sign)) 
summary(as.factor(dt1$DissCalcium_Sign)) 
summary(as.factor(dt1$TotChloride_Sign)) 
summary(as.factor(dt1$DissChloride_Sign)) 
summary(as.factor(dt1$DissNitrateNitrite_Sign)) 
summary(as.factor(dt1$DOC_Sign)) 
summary(as.factor(dt1$TOC_Sign)) 
summary(as.factor(dt1$DON_Sign)) 
summary(as.factor(dt1$TON_Sign)) 
summary(as.factor(dt1$DissOrthophos_Sign)) 
summary(as.factor(dt1$TotPhos_Sign)) 
summary(as.factor(dt1$DissSilica_Sign)) 
summary(as.factor(dt1$TDS_Sign)) 
summary(as.factor(dt1$TSS_Sign)) 
summary(as.factor(dt1$VSS_Sign)) 
summary(as.factor(dt1$TKN_Sign)) 
summary(as.factor(dt1$Microcystis))
detach(dt1)               


inUrl2  <- "https://pasta.lternet.edu/package/data/eml/edi/458/12/ada2f452284c8d88cde03fdf98280dc9" 
infile2 <- tempfile()
try(download.file(inUrl2,infile2,method="curl"))
if (is.na(file.size(infile2))) download.file(inUrl2,infile2,method="auto")


dt2 <-read.csv(infile2,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "Station",     
                 "StationID",     
                 "Location",     
                 "StationType",     
                 "Latitude",     
                 "Longitude",     
                 "Status",     
                 "StartDate",     
                 "EndDate",     
                 "StationPaused"    ), check.names=TRUE)

unlink(infile2)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt2$Station)!="factor") dt2$Station<- as.factor(dt2$Station)
if (class(dt2$StationID)!="factor") dt2$StationID<- as.factor(dt2$StationID)
if (class(dt2$Location)!="factor") dt2$Location<- as.factor(dt2$Location)
if (class(dt2$StationType)!="factor") dt2$StationType<- as.factor(dt2$StationType)
if (class(dt2$Latitude)=="factor") dt2$Latitude <-as.numeric(levels(dt2$Latitude))[as.integer(dt2$Latitude) ]               
if (class(dt2$Latitude)=="character") dt2$Latitude <-as.numeric(dt2$Latitude)
if (class(dt2$Longitude)=="factor") dt2$Longitude <-as.numeric(levels(dt2$Longitude))[as.integer(dt2$Longitude) ]               
if (class(dt2$Longitude)=="character") dt2$Longitude <-as.numeric(dt2$Longitude)
if (class(dt2$Status)!="factor") dt2$Status<- as.factor(dt2$Status)
if (class(dt2$StartDate)!="factor") dt2$StartDate<- as.factor(dt2$StartDate)
if (class(dt2$EndDate)!="factor") dt2$EndDate<- as.factor(dt2$EndDate)
if (class(dt2$StationPaused)!="factor") dt2$StationPaused<- as.factor(dt2$StationPaused)

# Convert Missing Values to NA for non-dates

dt2$StationID <- as.factor(ifelse((trimws(as.character(dt2$StationID))==trimws("NA")),NA,as.character(dt2$StationID)))
dt2$Latitude <- ifelse((trimws(as.character(dt2$Latitude))==trimws("NA")),NA,dt2$Latitude)               
suppressWarnings(dt2$Latitude <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Latitude))==as.character(as.numeric("NA"))),NA,dt2$Latitude))
dt2$Longitude <- ifelse((trimws(as.character(dt2$Longitude))==trimws("NA")),NA,dt2$Longitude)               
suppressWarnings(dt2$Longitude <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Longitude))==as.character(as.numeric("NA"))),NA,dt2$Longitude))
dt2$EndDate <- as.factor(ifelse((trimws(as.character(dt2$EndDate))==trimws("NA")),NA,as.character(dt2$EndDate)))


# Here is the structure of the input data frame:
str(dt2)                            
attach(dt2)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(Station)
summary(StationID)
summary(Location)
summary(StationType)
summary(Latitude)
summary(Longitude)
summary(Status)
summary(StartDate)
summary(EndDate)
summary(StationPaused) 
# Get more details on character variables

summary(as.factor(dt2$Station)) 
summary(as.factor(dt2$StationID)) 
summary(as.factor(dt2$Location)) 
summary(as.factor(dt2$StationType)) 
summary(as.factor(dt2$Status)) 
summary(as.factor(dt2$StartDate)) 
summary(as.factor(dt2$EndDate)) 
summary(as.factor(dt2$StationPaused))
detach(dt2)               


inUrl3  <- "https://pasta.lternet.edu/package/data/eml/edi/458/12/afc5b55a61e9a16d29fcaef4d802f5be" 
infile3 <- tempfile()
try(download.file(inUrl3,infile3,method="curl"))
if (is.na(file.size(infile3))) download.file(inUrl3,infile3,method="auto")


dt3 <-read.csv(infile3,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "Flag.Code",     
                 "Description"    ), check.names=TRUE)

unlink(infile3)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt3$Flag.Code)!="factor") dt3$Flag.Code<- as.factor(dt3$Flag.Code)
if (class(dt3$Description)!="factor") dt3$Description<- as.factor(dt3$Description)

# Convert Missing Values to NA for non-dates



# Here is the structure of the input data frame:
str(dt3)                            
attach(dt3)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(Flag.Code)
summary(Description) 
# Get more details on character variables

summary(as.factor(dt3$Flag.Code)) 
summary(as.factor(dt3$Description))
detach(dt3)               


stations <- read.csv("dwq_stations.csv")


