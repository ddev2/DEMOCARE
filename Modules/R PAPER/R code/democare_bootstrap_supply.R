Anna <- TRUE
ProyectoDoc <- TRUE
if (Anna) {
  setwd("G:\\Mi unidad\\Daniel_Devolder\\R")
  #files are compacted with ZIP
  zipped <- FALSE
  path <- "G:\\Mi unidad\\Daniel_Devolder\\Netlogo\\resultats\\"
  pathData <- path
} else {
  if (ProyectoDoc) {
    setwd("~/Google Drive/Daniel_Devolder/R")
    #files are compacted with ZIP
    zipped <- FALSE
    path <- "~/Google Drive/Daniel_Devolder/NetLogo/Resultats/"
    pathData <- path
  } else {
    setwd("~/Google Drive/DemoCare_BS")
    #files are compacted with ZIP
    zipped <- TRUE
    path <- paste (getwd(), '/BootStraps/', sep='')
    pathData <- '/private/tmp/'
  }
}


#number of kinship runs from the microsimulation program (up to 20)
nSimulFiles <- 20
#number of DemoCare runs from the same microsimulation run (up to 20)
nDemoCareFiles <- 20

nDemRegFileNames <- c(
  'KIN1908_EDU1_VAR',
  'KIN1918_EDU1_VAR',
  'KIN1928_EDU1_VAR',
  'KIN1938_EDU1_VAR',
  'KIN1948_EDU1_VAR',
  'KIN1958_EDU1_VAR',
  'KIN1968_EDU1_VAR'
)

#load (file="demReg.RData")



createCareSupplyDF(nSimulFiles, nDemoCareFiles, nDemRegFileNames)

#load(file="FertNupt.RData")
#load(file="careSupply.RData")
#with(cTabFertNuptDF,table(iSimul, iDemoCare))
#with(careSupplyDF,table(iSimul, iDemoCare))
#createCareSupplyDF(nSimulFiles=10, nDemoCareFiles=20, nDemRegFileNames, careSupplyDF, cTabFertNuptDF, debSimul=1, debDemoCare=1)

#load(file="FertNupt.RData")
#cTabFertNuptDF <- subset(cTabFertNuptDF, cohort!=1958)
#cTabFertNuptDF <- subset(cTabFertNuptDF, cohort!=1969)
#cTabFertNuptDF <- subset(cTabFertNuptDF, cohort!=1970)
#load(file="careSupply.RData")
#careSupplyDF <- subset(careSupplyDF, cohort!=1958)
#careSupplyDF <- subset(careSupplyDF, cohort!=1969)
#careSupplyDF <- subset(careSupplyDF, cohort!=1970)

#createCareSupplyDF(nSimulFiles=10, nDemoCareFiles=10, c("KIN1969_EDU1_VAR", "KIN1970_EDU1_VAR"), careSupplyDF, cTabFertNuptDF, debSimul=1, debDemoCare=1)
#createCareSupplyDF(nSimulFiles=10, nDemoCareFiles=10, c("KIN1971_EDU1_VAR"), careSupplyDF, cTabFertNuptDF, debSimul=1, debDemoCare=1)
createCareSupplyDF(nSimulFiles=5, nDemoCareFiles=5, c("SANT_JUST_"))

#createCareSupplyDF(nSimulFiles, nDemoCareFiles, "KIN1968_EDU1_VAR", debSimul=10, debDemoCare=1)
