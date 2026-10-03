if (FALSE) {
} else {
	ProyectoDoc <- TRUE
  if (ProyectoDoc) {
    setwd("~/Google Drive/Daniel_Devolder/R")
    #files are compacted with ZIP
    zipped <- FALSE
    path <- "~/Google Drive/Daniel_Devolder/NetLogo/Resultats/"
    pathOut <- path
  } else {
    setwd("~/Google Drive/DemoCare_BS")
    #files are compacted with ZIP
    zipped <- TRUE
    path <- paste (getwd(), '/BootStraps/', sep='')
    pathOut <- '/private/tmp/'
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
  }
}
library(reshape2)


#computeInformalCare(2, 10, nDemRegFileNames)
#computeInformalCare(nSimulFiles=20, nDemoCareFiles=20, nDemRegFileNames, informalCareDF, debSimul=3, debDemoCare=1)
#
#load(file="formal_informal.RData")
#with(informalCareDF,table(iSimul, iDemoCare))
#computeInformalCare(nSimulFiles=10, nDemoCareFiles=20, nDemRegFileNames, debSimul=1, debDemoCare=1)
#computeInformalCare(nSimulFiles=20, nDemoCareFiles=20, nDemRegFileNames, informalCareDF, debSimul=11, debDemoCare=1)

#load(file="formal_informal.RData")
#informalCareDF <- subset(informalCareDF, cohort!=1958)
#informalCareDF <- subset(informalCareDF, cohort!=1969)
#informalCareDF <- subset(informalCareDF, cohort!=1970)

#computeInformalCare(nSimulFiles=20, nDemoCareFiles=20, c("KIN1958_EDU1_VAR", "KIN1968_EDU1_VAR"), informalCareDF)

#computeInformalCare(1, 1, c("KIN1969_EDU1_VAR", "KIN1970_EDU1_VAR"), informalCareDF)
#computeInformalCare(nSimulFiles=10, nDemoCareFiles=10, c("KIN1969_EDU1_VAR", "KIN1970_EDU1_VAR"), informalCareDF)
#computeInformalCare(nSimulFiles=10, nDemoCareFiles=10, c("KIN1971_EDU1_VAR"), informalCareDF)
#computeInformalCare(nSimulFiles=5, nDemoCareFiles=5, c("SANT_JUST_"))
#with(informalCareDF,table(iSimul, cohort))

#computeInformalCare(nSimulFiles, nDemoCareFiles, nDemRegFileNames, debSimul=8)
