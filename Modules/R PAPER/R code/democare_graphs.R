currPath <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(currPath)
source ("democare_compute.R")
source ("democare_graphs_lib.R")

careType <- 2

singleFile <- rstudioapi::showQuestion("Which kind of DEMOCARE files",
                                   "Bootstrapped or single ones?", ok = "Single", cancel = "Bootstrap")

if (singleFile==TRUE) {
  sensitivityGraphs <- FALSE
  birthCohort <- rstudioapi::showPrompt("Choose cohort", "Year of birth for the cohort", default = 2020)
  birthCohort <- as.numeric(birthCohort)
} else {
  sensitivityGraphs <- TRUE
}

answer <- rstudioapi::showDialog("Choose input directory", "...for DEMOCARE input files")

pathIn <- currPath
path <- rstudioapi::selectDirectory(
  caption = "Select Input Directory",
  label = "Select",
  path = currPath
)

if (!is.null(path)) {
  pathIn <- paste (path, getDirSep(), sep="")
}
setwd(pathIn)

pathCurr <- getwd()
pathOut <- paste (getwd(), getDirSep(), 'care', careType, 'plots', sep='')

load ("formal_informal.RData")

if (careType == 1) {
  informalCareDF$informalCare <- informalCareDF$CareByPartner + informalCareDF$CareByChildren
} else {
  informalCareDF$informalCare <- informalCareDF$Care2ByPartner + informalCareDF$Care2ByChildren
}

sFields <- names(informalCareDF)[-6]

#keep info by familyKind, excluding Totals
informalCareAll <- informalCareDF[informalCareDF$familyKind!="Total", ]
#this one only has totals
informalCareDF <- informalCareDF[informalCareDF$familyKind=="Total", sFields]

labsSex <- data.frame(T=c("Total"), M=c("Males"), F=c("Females"), stringsAsFactors =FALSE)

##### 1 dependency #####
# UPDATED (2026-08-14): plot PropDep -- the proportion of egos ALIVE at
# this tick who are dependent -- instead of dividing the raw Dep headcount
# by a hardcoded initial-cohort-size constant (11000, split 48.8%/51.2% by
# sex). PropDep now comes straight out of computeBalance()/doIt() in
# democare_compute.R, computed from the actual dependent + non-dependent
# individual counts at each tick, so it's correct regardless of the run's
# actual population size and properly reflects mortality shrinking the
# survivor pool at older ages (the old fixed-N approach didn't). See
# feedback_computecare_testing.md for the full reasoning.
depCurves <- BS_curves (informalCareDF, "PropDep")

pDepPlotAllC <- dependencyPlot ("T", greyColor=FALSE)
pDepPlotAll <- dependencyPlot ("T")
pDepPlotMales <- dependencyPlot ("M")
pDepPlotFemales <- dependencyPlot ("F")

pDepPlot <- plotSameSizeRow(pDepPlotAll, noLabelY (pDepPlotFemales), noLabelY (pDepPlotMales))

if (!dir.exists(pathOut)) {dir.create(pathOut)}
setwd(pathOut)
ggsave (file="1dependency.pdf", plot=pDepPlot, device="pdf", width=14.85, height=6.83)
setwd(pathCurr)

##### 2 Informal care #####
informalCareCurves <- BS_curves(informalCareDF, "informalCare")

yLim <- c(0.4, 1)
breaksY <- seq(yLim[1], yLim[2], 0.1)

setwd(pathOut)
if (singleFile) {
  ggsave (file="2ProportionInformalCareAllGrey.pdf", plot=pICPlots(TRUE, breaksY=breaksY, labelIC=c(birthCohort),
                                                                   cohortIC=c(birthCohort),
                                                                   xLabelsIC=c(60)), device="pdf", width=14.85, height=6.83)
  ggsave (file="2ProportionInformalCareAllColor.pdf", plot=pICPlots(FALSE, breaksY=breaksY, labelIC=c(birthCohort),
                                                                    cohortIC=c(birthCohort),
                                                                    xLabelsIC=c(60)), device="pdf", width=14.85, height=6.83)
} else {
  ggsave (file="2ProportionInformalCareAllGrey.pdf", plot=pICPlots(TRUE, breaksY=breaksY), device="pdf", width=14.85, height=6.83)
  ggsave (file="2ProportionInformalCareAllColor.pdf", plot=pICPlots(FALSE, breaksY=breaksY), device="pdf", width=14.85, height=6.83)
}
setwd(pathCurr)

yLim <- c(0, 1)
breaksY <- seq(yLim[1], yLim[2], 0.1)
setwd(pathOut)
if (singleFile) {
  ggsave (file="2ProportionInformalCareAllGrey_yLim0-1.pdf", plot=pICPlots(TRUE, yLim=yLim, breaksY=breaksY, labelIC=c(birthCohort),
                                                                           cohortIC=c(birthCohort),
                                                                           xLabelsIC=c(60)), device="pdf", width=14.85, height=6.83)
  ggsave (file="2ProportionInformalCareAllColor_yLim0-1.pdf", plot=pICPlots(FALSE, yLim=yLim, breaksY=breaksY, labelIC=c(birthCohort),
                                                                            cohortIC=c(birthCohort),
                                                                            xLabelsIC=c(60)), device="pdf", width=14.85, height=6.83)
} else {
  ggsave (file="2ProportionInformalCareAllGrey_yLim0-1.pdf", plot=pICPlots(TRUE, yLim=yLim, breaksY=breaksY), device="pdf", width=14.85, height=6.83)
  ggsave (file="2ProportionInformalCareAllColor_yLim0-1.pdf", plot=pICPlots(FALSE, yLim=yLim, breaksY=breaksY), device="pdf", width=14.85, height=6.83)
}
setwd(pathCurr)

if (sensitivityGraphs) {
  pICAll_s <- informalCarePlot ("T", greyColor=FALSE, labelIC=c("1968", "1968s1", "1968s2", "1968s3"), cohortIC=c(1968, 1969, 1970, 1971), xLabelsIC=c(65, 55, 75, 85))
  pICMales_s <- informalCarePlot ("M", greyColor=FALSE, labelIC=c("1968", "1968s1", "1968s2", "1968s3"), cohortIC=c(1968, 1969, 1970, 1971), xLabelsIC=c(65, 55, 75, 85))
  pICFemales_s <- informalCarePlot ("F", greyColor=FALSE, labelIC=c("1968", "1968s1", "1968s2", "1968s3"), cohortIC=c(1968, 1969, 1970, 1971), xLabelsIC=c(65, 55, 75, 85))
  
  pIC_s <- plotSameSizeRow(pICAll_s, noLabelY (pICFemales_s), noLabelY (pICMales_s))
  
  setwd(pathOut)
  ggsave (file="2sProportionInformalCareAll_sensitivity.pdf", plot=pIC_s, device="pdf", width=14.85, height=6.83)
  setwd(pathCurr)
}

##### 3 careByPartner & careByChildren #####
if (careType == 1) {
  careByPartnerCurves <- BS_curves(informalCareDF, "CareByPartner")
  careByChildrenCurves <- BS_curves(informalCareDF, "CareByChildren")
} else {
  careByPartnerCurves <- BS_curves(informalCareDF, "Care2ByPartner")
  careByChildrenCurves <- BS_curves(informalCareDF, "Care2ByChildren")
}

myMelt_BS_curves <- function (dfIn, category=NULL) {
  #dfIn <- informalCare_P_noC
  dfOut <- melt(dfIn, id=c("ticks", "cohort"))
  dfOut <- subset(dfOut, variable %in% c("F", "M", "T"))
  dfOut$variable <- factor (dfOut$variable)
  names(dfOut)[3] <- "sex"
  dfOut$category <- category
  
  return (dfOut)
}

df1 <- myMelt_BS_curves(careByPartnerCurves, "3careByPartner")
df2 <- myMelt_BS_curves(careByChildrenCurves, "2careByChildren")
dfFormalCare <- df2
dfFormalCare$category <- "1formalCare"
dfFormalCare$value <- 1 - df2$value - df1$value
dfAll <- rbind(df1, df2, dfFormalCare)
dfAll <- addCohortSex (dfAll)

if (singleFile) {
  pCareRelativesYEAR_MF <- plotCareRelatives(df=dfAll, cohortsSubs=c(birthCohort))

  setwd(pathOut)
  fileName <- paste("4careRelatives", birthCohort, "_MF_color.pdf", sep="")
  ggsave(fileName, pCareRelativesYEAR_MF, width=14.85, height=6.83)
  setwd(pathCurr)
  
} else {
  pCareRelatives1908_1968_MF <- plotCareRelatives(df=dfAll)
  pCareRelatives1908_1968_MF_grey <- plotCareRelatives(df=dfAll, greyColor = TRUE)
  #pCareRelatives1908_1968_MF2 <- plotCareRelatives(myColors=c("#999999", "#E69F00", "#56B4E9"))
  
  pCareRelatives1908_28_48_68_T <- plotCareRelatives(
    cohortsSubs=c(1908, 1928, 1948, 1968), sexSubs=c("T")
  )
  pCareRelatives1908_28_48_68_T_grey <- plotCareRelatives(
    cohortsSubs=c(1908, 1928, 1948, 1968), sexSubs=c("T"), greyColor=TRUE
  )
  
  setwd(pathOut)
  ggsave("3careRelativesTot1908_28_48_68_color.pdf", pCareRelatives1908_28_48_68_T, width=14.85, height=6.83)
  ggsave("3careRelativesTot1908_28_48_68_grey.pdf", pCareRelatives1908_28_48_68_T_grey, width=14.85, height=6.83)
  ggsave("4careRelatives1908_1968_MF_color.pdf", pCareRelatives1908_1968_MF, width=14.85, height=6.83)
  ggsave("4careRelatives1908_1968_MF_grey.pdf", pCareRelatives1908_1968_MF_grey, width=14.85, height=6.83)
  setwd(pathCurr)
}

if (sensitivityGraphs) {
  dfAll2 <- dfAll
  dfAll2$cohort_sex <- ifelse(dfAll2$cohort_sex=="1969","1968s1",dfAll2$cohort_sex)
  dfAll2$cohort_sex <- ifelse(dfAll2$cohort_sex=="1970","1968s2",dfAll2$cohort_sex)
  dfAll2$cohort_sex <- ifelse(dfAll2$cohort_sex=="1971","1968s3",dfAll2$cohort_sex)
  pCareRelatives1968_Sensitivity_T <- plotCareRelatives(df=dfAll2,
                                                        cohortsSubs=c(1968, 1969, 1970, 1971), sexSubs=c("T")
  )
  
  setwd(pathOut)
  ggsave("4scareRelatives1968_Sensitivity_T.pdf", pCareRelatives1968_Sensitivity_T, width=14.85, height=6.83)
  setwd(pathCurr)
}

#pCareRelativesPZ1 <- plotCareRelatives(myColors=c("#d57c40", "#1bb0b7", "#98356e"))
#pCareRelativesPZ2 <- plotCareRelatives(myColors=c("#d57c40", "#226679", "#98356e"))

##### 4 decomposition of formal care plots #####
informalCare_NoP_noC <- informalCareAll[informalCareAll$familyKind=="noPartner",]
informalCare_P_noC <- informalCareAll[informalCareAll$familyKind=="Partner",]
informalCare_P_C <- informalCareAll[informalCareAll$familyKind=="Children",]

demandCareTotal_curves <- BS_curves(informalCareDF, "Care")
supplyCareTotal_curves <- BS_curves(informalCareDF, "informalCare")
demandCare_NoP_noC_curves <- BS_curves(informalCare_NoP_noC, "Care")
demandCare_P_noC_curves <- BS_curves(informalCare_P_noC, "Care")
demandCare_P_C_curves <- BS_curves(informalCare_P_C, "Care")
if (careType == 1) {
  supplyCare_P_noC_curves <- BS_curves(informalCare_P_noC, "CareByPartner")
} else {
  supplyCare_P_noC_curves <- BS_curves(informalCare_P_noC, "Care2ByPartner")
}
supplyCare_P_C_curves <- BS_curves(informalCare_P_C, "informalCare")

dfdemandCareTotal <- myMelt_BS_curves(demandCareTotal_curves, "demandCareTotal")
dfdemandCare_P_C <- myMelt_BS_curves(demandCare_P_C_curves, "netDemandCare_P_C")
dfsupplyCare_P_noC <- myMelt_BS_curves(supplyCare_P_noC_curves, "supplyCare_P_noC")
dfsupplyCare_P_C <- myMelt_BS_curves(supplyCare_P_C_curves, "supplyCare_P_C")

# complete details of informal care
dfCareByPartner <- myMelt_BS_curves(careByPartnerCurves, "5careByPartner")
dfCareByChildren <- myMelt_BS_curves(careByChildrenCurves, "4careByChildren")
dfFormalCare_NoP_NoC <- myMelt_BS_curves(demandCare_NoP_noC_curves, "1formalCare_NoP_NoC")
dfFormalCare_NoP_NoC$value <- dfFormalCare_NoP_NoC$value / dfdemandCareTotal$value
dfFormalCare_P_NoC <- myMelt_BS_curves(demandCare_P_noC_curves, "2formalCare_P_NoC")
dfFormalCare_P_NoC$value <- dfFormalCare_P_NoC$value *
  ( 1 - dfsupplyCare_P_noC$value )
dfFormalCare_P_NoC$value <- dfFormalCare_P_NoC$value / dfdemandCareTotal$value

dfFormalCare_P_C <- dfCareByPartner
dfFormalCare_P_C$category <- "3formalCare_P_C"
dfFormalCare_P_C$value <- 1 -
  dfFormalCare_P_NoC$value -
  dfFormalCare_NoP_NoC$value -
  dfCareByChildren$value -
  dfCareByPartner$value

dfFormalCare_P_C_check <- dfdemandCare_P_C
dfFormalCare_P_C_check$value <- dfFormalCare_P_C_check$value *
  ( 1 - dfsupplyCare_P_C$value )
dfFormalCare_P_C_check$value <- dfFormalCare_P_C_check$value / dfdemandCareTotal$value

dfAll_decFC <- rbind(dfCareByPartner,
                     dfCareByChildren,
                     dfFormalCare_NoP_NoC,
                     dfFormalCare_P_NoC,
                     dfFormalCare_P_C)
dfAll_decFC$category <- factor (dfAll_decFC$category)
dfAll_decFC$sexString <- ""
dfAll_decFC$sexString <- ifelse (dfAll_decFC$sex=="T", "", dfAll_decFC$sexString)
dfAll_decFC$sexString <- ifelse (dfAll_decFC$sex=="M", " - Males", dfAll_decFC$sexString)
dfAll_decFC$sexString <- ifelse (dfAll_decFC$sex=="F", " - Females", dfAll_decFC$sexString)
dfAll_decFC$cohort_sex <- paste(dfAll_decFC$cohort, dfAll_decFC$sexString, sep="")

if (singleFile) {
  pCareRelatives_decFC_YEAR_T <- plotCareRelatives(
    df=dfAll_decFC,
    cohortsSubs=c(birthCohort), sexSubs=c("T"),
    myColors=c("#005566", "#007766", "#009966", "#FF0033", "#0000CC"),
    myLabels=c(" FC: no fam. ", " FC: childless ", " FC: with fam. ", " Children ", " Partner ")
  )
  centerLegend(pCareRelatives_decFC_YEAR_T)
  setwd(pathOut)
  fileName <- paste("5careRelativesTot", birthCohort, "_decFC_color.pdf", sep="")
  ggsave(fileName, pCareRelatives_decFC_YEAR_T, width=14.85, height=6.83)
  setwd(pathCurr)
  
} else {
  pCareRelatives_decFC_1908_28_48_68_T <- plotCareRelatives(
    df=dfAll_decFC,
    cohortsSubs=c(1908, 1928, 1948, 1968), sexSubs=c("T"),
    myColors=c("#005566", "#007766", "#009966", "#FF0033", "#0000CC"),
    myLabels=c(" FC: no fam. ", " FC: childless ", " FC: with fam. ", " Children ", " Partner ")
  )
  centerLegend(pCareRelatives_decFC_1908_28_48_68_T)
  
  pCareRelatives_decFC_1908_28_48_68_T_grey <- plotCareRelatives(
    df=dfAll_decFC,
    cohortsSubs=c(1908, 1928, 1948, 1968), sexSubs=c("T"),
    myColors=c("#005566", "#007766", "#009966", "#FF0033", "#0000CC"),
    myLabels=c(" FC: no fam. ", " FC: childless ", " FC: with fam. ", " Children ", " Partner "),
    greyColor = TRUE
  )
  centerLegend(pCareRelatives_decFC_1908_28_48_68_T_grey)
  setwd(pathOut)
  ggsave("5careRelativesTot1908_28_48_68_decFC_color.pdf", pCareRelatives_decFC_1908_28_48_68_T, width=14.85, height=6.83)
  ggsave("5careRelativesTot1908_28_48_68_decFC_grey.pdf", pCareRelatives_decFC_1908_28_48_68_T_grey, width=14.85, height=6.83)
  setwd(pathCurr)
}


# informal care grouped
dfInformalCare <- myMelt_BS_curves(supplyCareTotal_curves, "4informalCare")

dfAll_decFC_IC <- rbind(dfInformalCare,
                     dfFormalCare_NoP_NoC,
                     dfFormalCare_P_NoC,
                     dfFormalCare_P_C)
dfAll_decFC_IC$category <- factor (dfAll_decFC_IC$category)
dfAll_decFC_IC$sexString <- ""
dfAll_decFC_IC$sexString <- ifelse (dfAll_decFC_IC$sex=="T", "", dfAll_decFC_IC$sexString)
dfAll_decFC_IC$sexString <- ifelse (dfAll_decFC_IC$sex=="M", " - Males", dfAll_decFC_IC$sexString)
dfAll_decFC_IC$sexString <- ifelse (dfAll_decFC_IC$sex=="F", " - Females", dfAll_decFC_IC$sexString)
dfAll_decFC_IC$cohort_sex <- paste(dfAll_decFC_IC$cohort, dfAll_decFC_IC$sexString, sep="")

if (singleFile) {
  pCareRelatives_decFC_IC_YEAR_T <- plotCareRelatives(
    df=dfAll_decFC_IC,
    cohortsSubs=c(birthCohort), sexSubs=c("T"),
    myColors=c("#005566", "#007766", "#009966", "#CCCCCC"),
    myLabels=c(" FC: no relatives ", " FC: childless ", " FC: partner+children ", " Informal Care ")
  )
  centerLegend(pCareRelatives_decFC_IC_YEAR_T)
  setwd(pathOut)
  fileName <- paste("6careRelativesTot", birthCohort, "_decFC_IC_color.pdf", sep="")
  ggsave(fileName, pCareRelatives_decFC_IC_YEAR_T, width=14.85, height=6.83)
  setwd(pathCurr)
} else {
  pCareRelatives_decFC_IC_1908_28_48_68_T <- plotCareRelatives(
    df=dfAll_decFC_IC,
    cohortsSubs=c(1908, 1928, 1948, 1968), sexSubs=c("T"),
    myColors=c("#005566", "#007766", "#009966", "#CCCCCC"),
    myLabels=c(" FC: no relatives ", " FC: childless ", " FC: partner+children ", " Informal Care ")
  )
  centerLegend(pCareRelatives_decFC_IC_1908_28_48_68_T)
  
  pCareRelatives_decFC_IC_1908_28_48_68_T_grey <- plotCareRelatives(
    df=dfAll_decFC_IC,
    cohortsSubs=c(1908, 1928, 1948, 1968), sexSubs=c("T"),
    myColors=c("#005566", "#007766", "#009966", "#CCCCCC"),
    myLabels=c(" FC: no relatives ", " FC: childless ", " FC: partner+children ", " Informal Care "),
    greyColor = TRUE
  )
  centerLegend(pCareRelatives_decFC_IC_1908_28_48_68_T)
  
  setwd(pathOut)
  ggsave("6careRelativesTot1908_28_48_68_decFC_IC_color.pdf", pCareRelatives_decFC_IC_1908_28_48_68_T, width=14.85, height=6.83)
  ggsave("6careRelativesTot1908_28_48_68_decFC_IC_grey.pdf", pCareRelatives_decFC_IC_1908_28_48_68_T_grey, width=14.85, height=6.83)
  setwd(pathCurr)
}

if (sensitivityGraphs) {
  dfAll_decFC_IC2 <- dfAll_decFC_IC
  dfAll_decFC_IC2$cohort_sex <- ifelse(dfAll_decFC_IC2$cohort_sex=="1969","1968s1",dfAll_decFC_IC2$cohort_sex)
  dfAll_decFC_IC2$cohort_sex <- ifelse(dfAll_decFC_IC2$cohort_sex=="1970","1968s2",dfAll_decFC_IC2$cohort_sex)
  dfAll_decFC_IC2$cohort_sex <- ifelse(dfAll_decFC_IC2$cohort_sex=="1971","1968s3",dfAll_decFC_IC2$cohort_sex)
  pCareRelatives_decFC_IC_1968_sensitivity_T <- plotCareRelatives(
    df=dfAll_decFC_IC2,
    cohortsSubs=c(1968, 1969, 1970, 1971), sexSubs=c("T"),
    myColors=c("#005566", "#007766", "#009966", "#CCCCCC"),
    myLabels=c(" FC: no relatives ", " FC: childless ", " FC: partner+children ", " Informal Care ")
  )
  
  setwd(pathOut)
  ggsave("6scareRelatives_decFC_IC_1968_sensitivity_T.pdf", pCareRelatives_decFC_IC_1968_sensitivity_T, width=14.85, height=6.83)
  setwd(pathCurr)
}

if (singleFile) {
} else {
  pCareRelatives_decFC_IC_1948_68_MF <- plotCareRelatives(
    df=dfAll_decFC_IC,
    cohortsSubs=c(1948, 1968), sexSubs=c("F", "M"),
    myColors=c("#005566", "#007766", "#009966", "#CCCCCC"),
    myLabels=c(" FC: no relatives ", " FC: childless ", " FC: partner+children ", " Informal Care ")
  )
  
  pCareRelatives_decFC_IC_1908_28_48_68_MF <- plotCareRelatives(
    df=dfAll_decFC_IC,
    cohortsSubs=c(1908, 1928, 1948, 1968), sexSubs=c("F", "M"),
    myColors=c("#005566", "#007766", "#009966", "#CCCCCC"),
    myLabels=c(" FC: no relatives ", " FC: childless ", " FC: partner+children ", " Informal Care ")
  )
}



##### 5 demographic regimes plots #####
demRegDF <- data.frame (cohorts=         c(1898, 1908, 1918, 1928, 1938, 1948, 1958, 1968, 1969, 1970, 1971, 2020),
                        TFR=             c(3.01, 2.74, 2.54, 2.52, 2.60, 2.27, 1.82, 1.53, 1.53, 1.53, 1.53, 1.53),
                        childlessness=   c(0.26, 0.25, 0.22, 0.18, 0.14, 0.13, 0.15, 0.21, 0.21, 0.21, 0.21, 0.21),
                        everMarried=     c(0.86, 0.85, 0.87, 0.89, 0.92, 0.95, 0.94, 0.92, 0.92, 0.92, 0.92, 0.92))


demRegDF$TFR1 <- demRegDF$TFR / (1 - demRegDF$childlessness)

demRegDF_sub <- data.frame(childlessnessEverMarried=rep(0, dim(demRegDF)[1]))
demRegDF_sub$childlessnessEverMarried <- (1 - demRegDF$everMarried)
demRegDF_sub$cohorts <- demRegDF$cohorts
demRegDF_sub$childlessnessMarried <- demRegDF$childlessness - demRegDF_sub$childlessnessEverMarried
mDemReg <- melt(demRegDF_sub, id=c("cohorts"))

##### 5-a direct and indirect childlessness levels #####

demRegVariables <- c("childlessnessMarried", "childlessnessEverMarried")
mDemReg$variable <- factor (mDemReg$variable, levels=demRegVariables)

if (singleFile) {
} else {
  labels_demReg <- data.frame(
    x=c(1920, 1920),
    y=c(0.16, 0.05),
    labels=c("No children", "No partner and no children"),
    variable=demRegVariables
  )
  
  pDemRegFert <- ggplot(mDemReg, aes(x=cohorts, y=value, fill=variable)) +
    geom_area() + theme_classic() +
    labs(x="birth cohort", y="proportion of Egos") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    theme(legend.position="none") +
    geom_label(data=labels_demReg, aes(x=x, y=y, label=labels), fill="white")
  pDemRegFert + theme_text() + theme (plot.margin = margin(0, 1, 0, 0, "cm"))
}

##### 5-b number of relatives #####

#cTabFertNupt
load (file="FertNupt.RData")
#careSupplyDF
load (file="careSupply.RData")

netTFR <- BS_curves (cTabFertNuptDF, "TFR", "cohort ~ sex")
netChildlessness <- BS_curves (cTabFertNuptDF, "childlessness", "cohort ~ sex")
netSinglehood <- BS_curves (cTabFertNuptDF, "singlehood", "cohort ~ sex")
netWedlock <- BS_curves (cTabFertNuptDF, "wedlock", "cohort ~ sex")
netWidowhood <- BS_curves (cTabFertNuptDF, "widowhood", "cohort ~ sex")

if (singleFile) {
  ppp1s <- plotSupply(cohortsSubs=c(birthCohort), nKinAvailableForCareLine=TRUE, noChildless = TRUE, nCols=4, sizeLabels=5, sizeTextLegend=14, sexAsTitle = FALSE, Brewer=FALSE)
  ppp5s <- plotSupply(cohortsSubs=c(birthCohort), noChildless = TRUE, sex=c("F", "M"),
                      compPartner=TRUE, splitChildDep=TRUE,
                      nCols=4, sizeLabels=5, sizeTextLegend=14, Brewer=FALSE)

  setwd(pathOut)
  fileName <- paste("7supply", birthCohort, "_T_scolor.pdf", sep="")
  ggsave(fileName, ppp1s, width=14.85, height=6.83)
  fileName <- paste("8b_supply", birthCohort, "_MF_scolor.pdf", sep="")
  ggsave(fileName, ppp5s, width=14.85, height=6.83)
  setwd(pathCurr)
} else {
  # plotSupply(sexAsTitle = TRUE)
  # plotSupply(noChildless = TRUE, sexAsTitle = FALSE, compPartner=TRUE, cohortsSubs=c(1968), sex=c("M", "F"), sizeLabels=3)
  # plotSupply(noChildless = TRUE, sizeLabels=2, sexAsTitle = TRUE)
  # plotSupply(dependency="notDependent")
  ppp1 <- plotSupply(cohortsSubs=c(1908, 1928, 1948, 1968), noChildless = TRUE, nCols=4, sizeLabels=5, sizeTextLegend=14, sexAsTitle = FALSE)
  ppp1_l <- plotSupply(cohortsSubs=c(1908, 1928, 1948, 1968), nKinAvailableForCareLine=TRUE, noChildless = TRUE, nCols=4, sizeLabels=5, sizeTextLegend=14, sexAsTitle = FALSE)
  ppp1s <- plotSupply(cohortsSubs=c(1908, 1928, 1948, 1968), noChildless = TRUE, nCols=4, sizeLabels=5, sizeTextLegend=14, sexAsTitle = FALSE, Brewer=FALSE)
  ppp1s_l <- plotSupply(cohortsSubs=c(1908, 1928, 1948, 1968), nKinAvailableForCareLine=TRUE, noChildless = TRUE, nCols=4, sizeLabels=5, sizeTextLegend=14, sexAsTitle = FALSE, Brewer=FALSE)
  ppp1_grey <- plotSupply(cohortsSubs=c(1908, 1928, 1948, 1968), noChildless = TRUE,
                          nCols=4, sizeLabels=5, sizeTextLegend=14, sexAsTitle = FALSE, greyColor = TRUE)
  ppp1_grey_l <- plotSupply(cohortsSubs=c(1908, 1928, 1948, 1968), nKinAvailableForCareLine=TRUE, noChildless = TRUE,
                            nCols=4, sizeLabels=5, sizeTextLegend=14, sexAsTitle = FALSE, greyColor = TRUE)
  ppp1_sensitivity <- plotSupply(cohortsSubs=c(1968, 1969, 1970, 1971), noChildless = TRUE, nCols=3, sizeLabels=5, sizeTextLegend=14, sexAsTitle = FALSE)
  # plotSupply(cohortsSubs=c(1908, 1928, 1948, 1968), noChildless = TRUE, sex=c("M"), nCols=4, sizeLabels=3, sexAsTitle = TRUE)
  # plotSupply(cohortsSubs=c(1908, 1928, 1948, 1968), noChildless = TRUE, sex=c("F"), nCols=4, sizeLabels=3, sexAsTitle = TRUE)
  ppp2 <- plotSupply(cohortsSubs=c(1908, 1968), noChildless = TRUE, sex=c("F", "M"), compPartner=TRUE, nCols=4, sizeLabels=5, sizeTextLegend=14)
  ppp3 <- plotSupply(cohortsSubs=c(1908, 1968), noChildless = TRUE, sex=c("F", "M"),
                     compPartner=TRUE, compSiblings=TRUE, splitChildDep=TRUE, nCols=4, sizeLabels=5, sizeTextLegend=14)
  ppp3s <- plotSupply(cohortsSubs=c(1908, 1968), noChildless = TRUE, sex=c("F", "M"),
                      compPartner=TRUE, compSiblings=TRUE, splitChildDep=TRUE,
                      nCols=4, sizeLabels=5, sizeTextLegend=14, Brewer=FALSE)
  ppp3_grey <- plotSupply(cohortsSubs=c(1908, 1968), noChildless = TRUE, sex=c("F", "M"),
                          compPartner=TRUE, compSiblings=TRUE, splitChildDep=TRUE, nCols=4, sizeLabels=5, sizeTextLegend=14, greyColor = TRUE)
  ppp3_l <- plotSupply(cohortsSubs=c(1908, 1968), nKinAvailableForCareLine=TRUE, noChildless = TRUE, sex=c("F", "M"),
                       compPartner=TRUE, compSiblings=TRUE, splitChildDep=TRUE, nCols=4, sizeLabels=5, sizeTextLegend=14)
  ppp3s_l <- plotSupply(cohortsSubs=c(1908, 1968), nKinAvailableForCareLine=TRUE, noChildless = TRUE, sex=c("F", "M"),
                        compPartner=TRUE, compSiblings=TRUE, splitChildDep=TRUE,
                        nCols=4, sizeLabels=5, sizeTextLegend=14, Brewer=FALSE)
  ppp3_grey_l <- plotSupply(cohortsSubs=c(1908, 1968), nKinAvailableForCareLine=TRUE, noChildless = TRUE, sex=c("F", "M"),
                            compPartner=TRUE, compSiblings=TRUE, splitChildDep=TRUE, nCols=4, sizeLabels=5, sizeTextLegend=14, greyColor = TRUE)
  ppp4 <- plotSupply(cohortsSubs=c(1948, 1968), noChildless = TRUE, sex=c("F", "M"),
                     compPartner=TRUE, compSiblings=TRUE, splitChildDep=TRUE,
                     nCols=4, sizeLabels=5, sizeTextLegend=14)
  ppp4_grey <- plotSupply(cohortsSubs=c(1948, 1968), noChildless = TRUE, sex=c("F", "M"),
                          compPartner=TRUE, compSiblings=TRUE, splitChildDep=TRUE,
                          nCols=4, sizeLabels=5, sizeTextLegend=14, greyColor = TRUE)
  ppp4s <- plotSupply(cohortsSubs=c(1948, 1968), noChildless = TRUE, sex=c("F", "M"),
                      compPartner=TRUE, compSiblings=TRUE, splitChildDep=TRUE,
                      nCols=4, sizeLabels=5, sizeTextLegend=14, Brewer=FALSE)
  ppp4s_l <- plotSupply(cohortsSubs=c(1948, 1968), nKinAvailableForCareLine=TRUE, noChildless = TRUE, sex=c("F", "M"),
                        compPartner=TRUE, compSiblings=TRUE, splitChildDep=TRUE,
                        nCols=4, sizeLabels=5, sizeTextLegend=14, Brewer=FALSE)
  ppp5 <- plotSupply(cohortsSubs=c(1908, 1968), noChildless = TRUE, sex=c("F", "M"),
                     compPartner=TRUE, splitChildDep=TRUE,
                     nCols=4, sizeLabels=5, sizeTextLegend=14)
  ppp5_grey <- plotSupply(cohortsSubs=c(1908, 1968), noChildless = TRUE, sex=c("F", "M"),
                          compPartner=TRUE, splitChildDep=TRUE,
                          nCols=4, sizeLabels=5, sizeTextLegend=14, greyColor = TRUE)
  ppp5s <- plotSupply(cohortsSubs=c(1908, 1968), noChildless = TRUE, sex=c("F", "M"),
                      compPartner=TRUE, splitChildDep=TRUE,
                      nCols=4, sizeLabels=5, sizeTextLegend=14, Brewer=FALSE)
  ppp5s_l <- plotSupply(cohortsSubs=c(1908, 1968), nKinAvailableForCareLine=TRUE, noChildless = TRUE, sex=c("F", "M"),
                        compPartner=TRUE, splitChildDep=TRUE,
                        nCols=4, sizeLabels=5, sizeTextLegend=14, Brewer=FALSE)
  # plotSupply(cohortsSubs=c(1928, 1948), noChildless = FALSE, sex=c("F", "M"), compPartner=TRUE, nCols=4, splitChildDep=TRUE, sizeLabels=3)
  
  setwd(pathOut)
  ggsave("7supply1908_28_48_68_T_scolor.pdf", ppp1s_l, width=14.85, height=6.83)
  ggsave("7supply1908_28_48_68_T_color.pdf", ppp1_l, width=14.85, height=6.83)
  ggsave("7supply1908_28_48_68_T_grey.pdf", ppp1_grey_l, width=14.85, height=6.83)
  ggsave("8supply1908_68_MF_l_color.pdf", ppp3_l, width=14.85, height=6.83)
  ggsave("8supply1908_68_MF_l_scolor.pdf", ppp3s_l, width=14.85, height=6.83)
  ggsave("8supply1908_68_MF_l_grey.pdf", ppp3_grey_l, width=14.85, height=6.83)
  ggsave("8b_supply1908_68_MF_color.pdf", ppp5, width=14.85, height=6.83)
  ggsave("8b_supply1908_68_MF_scolor.pdf", ppp5s, width=14.85, height=6.83)
  ggsave("8b_supply1908_68_MF_grey.pdf", ppp5_grey, width=14.85, height=6.83)
  setwd(pathCurr)
}

#df_out <- plotSupply(cohortsSubs=c(1908, 1928, 1948, 1968), noChildless = TRUE, sex=c("F", "M"), retData=TRUE)
