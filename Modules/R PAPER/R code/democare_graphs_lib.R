library2("reshape2")
library2("ggplot2")
library2 ("ggrepel")

plotSameSizeRow <- function(...) {
  library(gtable)
  library(grid)
  library(gridExtra)
  aList = list(...)
  gl <- lapply(aList, ggplotGrob)
  aList <- c(gl, list(size = "first"))
  g <- do.call (cbind, aList)
  
  grid.draw(g)
  
  return (g)
}

centerLegend <- function(p) {
  library(gtable)
  
  g <- ggplotGrob(p)
  id <- which(g$layout$name == "guide-box")
  g$layout[id, c("l","r")] <- c(1, ncol(g))
  grid.newpage()
  grid.draw(g)
}

countIt <- function(x) {
  return (length(x))
}

sel <- function (x, pos) {
  x <- sort(x)
  pos <- max(pos, 1)
  return (x[pos])
}

sel2 <- function (x, relpos) {
  print(relpos)
  pos <- integer(length(x)*relpos)
  
  x <- sort(x)
  return (x[pos])
}

BS_curves <- function (tab, var, formulaStr="cohort + ticks ~ sex", sets=list(c(1908, 1918, 1928, 1938, 1948, 1958, 1968), c(1969, 1970, 1971))) {
  #average curve
  #tab <- informalCareDF
  #var <- "Dep"
  #formulaStr <- "cohort ~ sex"
  createThem <- function(tabTemp) {
    #tabTemp <- tabNotInSets
    if (dim(tabTemp)[1] == 0) return (data.frame())
    aCurve <- dcast(tabTemp, formulaStr, value.var=var, mean)
    aCurve_count <- dcast(tabTemp, formulaStr, value.var=var, fun.aggregate = countIt)
    num <- aCurve_count$F[1]
    # NEW (2026-08-14): with only ONE replicate per (cohort, ticks) cell
    # (num <= 1 -- no bootstrap ensemble, e.g. a single NetLogo/baseR/
    # tidyverse comparison run per source), sel() at the 2.5th/97.5th
    # percentile degenerates to the single observed value == the mean
    # (as.integer(0.025*1) and as.integer(0.975*1) both truncate to 0,
    # clamped up to position 1 by sel()'s own `pos <- max(pos, 1)`). That
    # used to silently draw a real-looking, zero-width "confidence
    # interval" ribbon. Report NA instead so plotIt() can detect there is
    # no real interval to show and skip the ribbon entirely, rather than
    # drawing one that implies false precision.
    if (num > 1) {
      aCurve_min <- dcast(tabTemp, formulaStr, value.var=var, fun.aggregate = sel, as.integer(0.025*num))
      aCurve_max <- dcast(tabTemp, formulaStr, value.var=var, fun.aggregate = sel, as.integer(0.975*num))
      aCurve$F_MIN <- aCurve_min$F
      aCurve$F_MAX <- aCurve_max$F
      aCurve$M_MIN <- aCurve_min$M
      aCurve$M_MAX <- aCurve_max$M
      aCurve$T_MIN <- aCurve_min$T
      aCurve$T_MAX <- aCurve_max$T
    } else {
      aCurve$F_MIN <- NA
      aCurve$F_MAX <- NA
      aCurve$M_MIN <- NA
      aCurve$M_MAX <- NA
      aCurve$T_MIN <- NA
      aCurve$T_MAX <- NA
    }

    return (aCurve)
  }
  
  tabNotInSets <- tab
  for ( i in (1:length(sets))) {
    #i <- 1
    tabNotInSets <- subset(tabNotInSets, !(cohort %in% sets[[i]]))
    tabInSets <- subset(tab, cohort %in% sets[[i]])
    if ( i == 1 ) {
      #i <- 1
      aCurve <- createThem(tabInSets)
    } else {
      aCurve <- rbind(aCurve, createThem(tabInSets))
    }
  }
  if (dim(tabNotInSets)[1] > 0) {
    if (dim(aCurve)[1] > 0) {
      aCurve <- rbind(aCurve, createThem(tabNotInSets))
    } else {
      aCurve <- createThem(tabNotInSets)
    }
  }
  aCurve$cohort <- factor (aCurve$cohort)
  # tranform tick into age
  if (!is.null(aCurve$ticks)) {
    aCurve$ticks <- aCurve$ticks + 50
  }
  
  return (aCurve)
}

theme_text <- function(mainRelSize=2) {
  biggerRelSize <- mainRelSize * 4 / 3
  return (
    theme(
      axis.text = element_text(size = rel(mainRelSize)),
      axis.title = element_text(size = rel(mainRelSize)),
      plot.title = element_text(size = rel(biggerRelSize)),
      strip.text = element_text(size = rel(mainRelSize)),
      legend.title = element_text(size = rel(mainRelSize)),
      legend.text = element_text(colour="black", size = rel(mainRelSize)),
      axis.ticks.length=unit(0.18, "cm")
    )
  )
}

noLabelY <- function(p) {
  return (p +
            theme(
              axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank()
            ) + labs(y=NULL)
  )
}
# theme(axis.title=element_blank(),
#       axis.ticks=element_blank(),
#       axis.text.x=element_text(margin = margin(t = 20)),
#       axis.text.y=element_text(margin = margin(r = 20)))

addCohortSex <- function (df) {
  df$sexString <- ""
  df$sexString <- ifelse (df$sex=="T", "", df$sexString)
  df$sexString <- ifelse (df$sex=="M", " - Males", df$sexString)
  df$sexString <- ifelse (df$sex=="F", " - Females", df$sexString)
  df$cohort_sex <- paste(df$cohort, df$sexString, sep="")
  df$sexString <- NULL
  return (df)
}

plotIt <- function (df, colSex,
                    cohorts=NULL, labels=NULL, labelY="Y", labelSex=NULL,
                    yLim=NULL, breaksY=waiver(),
                    diffLineType=FALSE, greyColor=FALSE, alphaLev=0.1,
                    useRepel=FALSE, forceRepel=1,
                    scale_y_percent=TRUE) {
  min <- paste (colSex, "_MIN", sep="")
  max <- paste (colSex, "_MAX", sep="")
  
  isBlack <- "black"
  
  if (!is.null(cohorts)) {
    df <- df[df$cohort %in% cohorts,]
  }
  df$cohort <- factor (df$cohort)
  df$noLinetype <- "1"
  if (diffLineType) { lineTypeVar <- "cohort" } else { lineTypeVar <- "noLinetype" }
  if (greyColor) { colorSet <- "noLinetype" } else { colorSet <- "cohort" }
  p <- ggplot(data=df)
  p <- p + geom_line(aes_string(x="ticks", y=colSex, group="cohort", linetype=lineTypeVar, color=colorSet))
  # NEW (2026-08-14): only draw the ribbon when there's an actual interval
  # to show. BS_curves() now reports NA for the *_MIN/*_MAX columns
  # whenever a (cohort, ticks) cell had just one replicate run -- no
  # bootstrap ensemble to compute a percentile interval from. Drawing
  # geom_ribbon() in that case used to produce a real-looking but
  # zero-width ribbon (MIN==MAX==the single value); skip it instead so a
  # single-run plot doesn't visually imply uncertainty that was never
  # computed.
  hasInterval <- (min %in% names(df)) && (max %in% names(df)) &&
    !all(is.na(df[[min]])) && !all(is.na(df[[max]]))
  if (hasInterval) {
    p <- p + geom_ribbon(aes_string(x="ticks", ymin=min, ymax=max, group="cohort", fill=colorSet), alpha = alphaLev)
  }
  if (!is.null(labels)) {
    labels$noLinetype <- factor ("1")
    labels$cohort <- factor(labels$label)
    if (useRepel) {
      p <- p + geom_label_repel(data=labels, aes_string(x="x", y="y", label="label", color=colorSet), force=forceRepel)
    } else {
      p <- p + geom_label(data=labels, aes_string(x="x", y="y", label="label", color=colorSet))
    }
  }
  p <- p + labs(x="ego's age", y=labelY)
  if (!is.null(labelSex)) {
    p <- p + labs(title=labelSex)
  }
  p <- p + theme_classic() + theme_text()
  if (!is.null(yLim)) {
    p <- p + coord_cartesian(ylim = yLim)
  }
  p <- p + theme(legend.position="none")
  if (scale_y_percent) {
    p <- p + scale_y_continuous(breaks=breaksY, labels = scales::percent_format(accuracy = 1))
  } else {
    p <- p + scale_y_continuous(breaks=breaksY)
  }
  
  if (greyColor) {
    p <- p + scale_color_grey(start=0, end=0.9)
    p <- p + scale_fill_grey()
  }
  return (p)
}

dependencyPlot <- function (colSex, greyColor=TRUE, exclist = c(1969, 1970, 1971), ...) {
  #colSex<-"T"
  #labelDep <- c(1908, 1918, 1928, 1938, 1948, 1958, 1968, 1969, 2020)
  
  labelDep <- names(table(depCurves$cohort))
  labelDep <- labelDep[!(labelDep %in% exclist)]
  n <- length(labelDep)
  xLabelsDep <-  c(60, 65, 70, 75, 80, 85, 90)
  yLabelsDep <- c()
  for (ind in (1:n)) {
    #ind <- 1
    yLabelsDep <- c(
      yLabelsDep, 
      depCurves[(depCurves$cohort==labelDep[ind])&(depCurves$ticks==xLabelsDep[ind]), colSex]
    )
  }
  
  labelsDep <- data.frame (
    label=labelDep,
    x=xLabelsDep[1:n],
    y=yLabelsDep[1:n]
  )
  
  depCurvesToPlot <- subset(depCurves, !(cohort %in% exclist))
  depCurvesToPlot$cohort <- factor(depCurvesToPlot$cohort, levels=labelDep)
  
  return (
    plotIt (depCurvesToPlot, colSex,
            labels=labelsDep, labelY="Prop. surviving and dependent", labelSex=labsSex[1,colSex], yLim=c(0, 0.3),
            alphaLev=0.2, greyColor=greyColor, ...)
  )
}

informalCarePlot <- function (colSex, greyColor=FALSE,
                              labelIC=c(1908, 1928, 1948, 1968),
                              cohortIC=c(1908, 1928, 1948, 1968),
                              xLabelsIC=c(60, 55, 75, 80),
                              yLim=c(0.4, 1), ...) {
  
  yLabelsIC <- c()
  lc <- length(cohortIC)
  for (ind in (1:lc)) {
    #ind <- 1
    yLabelsIC <- c(
      yLabelsIC, 
      informalCareCurves[(informalCareCurves$cohort==cohortIC[ind])&(informalCareCurves$ticks==xLabelsIC[ind]), colSex]
    )
  }
  
  labelsIC <- data.frame (
    label=labelIC,
    x=xLabelsIC,
    y=yLabelsIC
  )
  
  return (
    plotIt (informalCareCurves, colSex, labels=labelsIC, cohorts=cohortIC,
            labelY="Proportion Total Care", labelSex=labsSex[1,colSex],
            yLim=yLim, diffLineType=TRUE, greyColor=greyColor, alphaLev=0.1, ...)
  )
  
}

pICPlots <- function(greyColor=FALSE, ...) {
  posTitle <- theme (plot.title = element_text(hjust = 0.5))
  pICAll <- informalCarePlot ("T", greyColor=greyColor, ...) + posTitle
  pICAll <- informalCarePlot ("T", greyColor=greyColor, ...)
  pICMales <- informalCarePlot ("M", greyColor=greyColor, ...)
  pICFemales <- informalCarePlot ("F", greyColor=greyColor, ...)
  
  return ( plotSameSizeRow(pICAll, noLabelY (pICFemales), noLabelY (pICMales)) )
  
}

plotCareRelatives <- function (
    df=dfAll,
    cohortsSubs=c(1908, 1968),
    sexSubs=c("M", "F"),
    nCols=4,
    myColors=c("#009966", "#FF0033", "#0000CC"),
    myLabels=c(" Formal Care ", " Children ", " Partner "),
    greyColor=FALSE
) {
  #df = dfAll_decFC
  
  subs <- (df$cohort%in%cohortsSubs)&(df$sex%in%sexSubs)
  
  breaksY <- seq(0, 1, 0.1)
  
  p <- ggplot(data=df[subs,])
  p <- p + geom_area(aes(x=ticks, y=value, fill=category))
  p <- p + scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks=breaksY)
  p <- p + facet_wrap(vars(cohort_sex), ncol=nCols)
  p <- p + labs(x="ego's age", y="Proportion Total Care")
  p <- p + theme_minimal() +
    theme(panel.background=element_rect(fill = "white")) + theme_text()
  p <- p + theme(legend.title=element_blank(), legend.position="bottom")
  
  
  if (greyColor) {
    p <- p + scale_color_grey(start=0, end=0.9)
    p <- p + scale_fill_grey(labels=myLabels)
  } else {
    p <- p +
      scale_fill_manual(
        values=myColors, 
        labels=myLabels
      )
  }
  
  return (p)
}

plotSupplyOptions <- function(choice="ppp3_grey") {
  #default set
  dependency <<- "dependent"
  cohortsSubs <<- c(1908, 1968)
  nKinAvailableForCareLine <<- FALSE
  sex <<- "T"
  nCols <<- 2
  sizeLabels <<- 4
  sexAsTitle <<- FALSE
  compPartner <<- FALSE
  compSiblings <<- FALSE
  noChildless <<- FALSE
  widowhood <<- FALSE
  splitChildDep <<- FALSE
  otherColorScheme <<- NULL
  sizeTextLegend <<- 0
  greyColor <<- FALSE
  Brewer <<- TRUE
  labelFactors <<- FALSE
  retData <<- FALSE
  
  #other options
  #ppp1s <- plotSupply(cohortsSubs=c(1908, 1928, 1948, 1968), noChildless = TRUE, nCols=4, sizeLabels=5, sizeTextLegend=14, sexAsTitle = FALSE, Brewer=FALSE)
  
  if (choice == "ppp1s") {
    cohortsSubs <<- c(1908, 1928, 1948, 1968)
    noChildless <<- TRUE
    nCols <<- 4
    sizeLabels <<- 5
    sizeTextLegend <<- 14
    sexAsTitle <<- FALSE
    Brewer <<- FALSE
  }
  
  #ppp1s_l <- plotSupply(cohortsSubs=c(1908, 1928, 1948, 1968), nKinAvailableForCareLine=TRUE, noChildless = TRUE, nCols=4, sizeLabels=5, sizeTextLegend=14, sexAsTitle = FALSE, Brewer=FALSE)
  
  if (choice == "ppp1s_l") {
    cohortsSubs <<- c(1908, 1928, 1948, 1968)
    nKinAvailableForCareLine <<- TRUE
    noChildless <<- TRUE
    nCols <<- 4
    sizeLabels <<- 5
    sizeTextLegend <<- 14
    sexAsTitle <<- FALSE
    Brewer <<- FALSE
  }
  #ppp3_grey <- plotSupply(cohortsSubs=c(1908, 1968), noChildless = TRUE, sex=c("F", "M"),
  #  compPartner=TRUE, compSiblings=TRUE, splitChildDep=TRUE, nCols=4, sizeLabels=5, sizeTextLegend=14, greyColor = TRUE)
  
  if (choice == "ppp3_grey") {
    noChildless <<- TRUE
    sex <<- c("F", "M")
    compPartner <<- TRUE
    compSiblings <<- TRUE
    splitChildDep <<- TRUE
    nCols <<- 4
    sizeLabels <<- 5
    sizeTextLegend <<- 14
    greyColor <<- TRUE
  }
  
  #ppp1_grey <- plotSupply(cohortsSubs=c(1908, 1928, 1948, 1968), noChildless = TRUE,
  #                        nCols=4, sizeLabels=5, sizeTextLegend=14, sexAsTitle = FALSE, greyColor = TRUE)
  
  if (choice == "ppp1_grey") {
    cohortsSubs <<- c(1908, 1928, 1948, 1968)
    noChildless  <<-  TRUE
    nCols <<- 4
    sizeLabels <<- 5
    sizeTextLegend <<- 14
    sexAsTitle <<- FALSE
    greyColor <<- TRUE
  }
}
#plotSupplyOptions("ppp1s_l")

plotSupply <- function(dependency="dependent",
                       cohortsSubs=c(1908, 1968),
                       nKinAvailableForCareLine=FALSE, #a plot of a line for the number of kin available for caring ego (not debugged: creates a mess with colors)
                       sex="T", #only one sex or total at a time
                       nCols=2,
                       sizeLabels=4,
                       sexAsTitle=FALSE,
                       compPartner=FALSE, #show competition by ego's partner for child care
                       compSiblings=FALSE, #show siblings and other relatives comp. for child care
                       noChildless=FALSE, #noChildless is whether we have all persons (FALSE) or those with at least one child (TRUE)
                       #noChildless was also thought to incorporate childlessness effect if TRUE, but this is desactivated now
                       widowhood=FALSE, #if TRUE we add widowhoodeffect (women die before the end of their reproductive life)
                       splitChildDep=FALSE, #age and health partition for children
                       otherColorScheme=NULL, #substitute myColors by another scheme (see the components of myColors in the code)
                       sizeTextLegend=0,
                       greyColor=FALSE,
                       Brewer=TRUE,
                       labelFactors=FALSE, #put a number and a label on each factor
                       retData=FALSE #whether the function returns a dataframe
) {
  compOtherRel <- (compPartner == TRUE) | (compSiblings==TRUE)
  
  dataToPlot <- function (aList, sex) {
    #aList <- CS_dep
    #sex="M"
    df <- aList$partner_mort[, c("cohort", "ticks")]
    
    #DemoCare results
    for (ind in (1:length(varList))) {
      #ind=1
      df[, varList[ind]] <- aList[[varList[ind]]][, sex]
    }
    
    #Demographic Regime values
    df$TFR <- 0
    df$childlessness <- 0
    if ( noChildless ) {
      df$TFR1 <- 0
    }
    df$everMarried <- 0
    df$netTFR <- 0
    
    for (cohort in demRegDF$cohorts) {
      #cohort <- 1908
      sel1 <- df$cohort == cohort
      sel2 <- demRegDF$cohorts == cohort
      if ( cohort %in% df$cohort ) {
        df[sel1,]$TFR <- demRegDF[sel2,]$TFR
        df[sel1,]$childlessness <- demRegDF[sel2,]$childlessness
        if ( noChildless ) {
          df[sel1,]$TFR1 <- demRegDF[sel2,]$TFR / (1 - demRegDF[sel2,]$childlessness)
        }
        df[sel1,]$everMarried <- demRegDF[sel2,]$everMarried
        df[sel1,]$netTFR <- netTFR[netTFR$cohort==cohort, sex]
      }
    }
    
    #standardize partner values
    sPartner <- df$partner_mort + df$partner_dep + df$partner_work +
      df$partner_comp + df$partner_ego
    if ( noChildless ) {
      sPartner <- sPartner
    } else {
      sPartner <- sPartner / df$everMarried
    }
    
    df$partner_mort <- df$partner_mort / sPartner
    df$partner_dep <- df$partner_dep / sPartner
    df$partner_work <- df$partner_work / sPartner
    df$partner_comp <- df$partner_comp / sPartner
    df$partner_ego <- df$partner_ego / sPartner
    
    if (widowhood) { df$widowhoodEffect <- df$TFR - df$netTFR }
    
    #standardize children values
    sChildren <- df$children_comp_children + df$children_comp_partner + df$children_comp_siblings +
      df$children_dep + df$children_ego + df$children_mort + df$children_work
    
    if (widowhood) { sChildren <- sChildren + df$widowhoodEffect }
    
    if ( noChildless ) {
      sChildren <- sChildren / df$TFR1
    } else {
      sChildren <- sChildren / df$TFR
    }
    
    df$children_comp_children <- df$children_comp_children / sChildren
    df$children_comp_partner <- df$children_comp_partner / sChildren
    df$children_comp_siblings <- df$children_comp_siblings / sChildren
    df$children_dep <- df$children_dep / sChildren
    df$children_dep_health <- df$children_dep_health / sChildren
    df$children_dep_age <- df$children_dep_age / sChildren
    df$children_ego <- df$children_ego / sChildren
    df$children_mort <- df$children_mort / sChildren
    df$children_work <- df$children_work / sChildren
    
    if (nKinAvailableForCareLine) {
      df$availableCareForEgo <- df$partner_ego + df$children_ego
    }
    
    if (widowhood) { df$widowhoodEffect <- df$widowhoodEffect / sChildren }
    
    if ( noChildless ) {
      df$TFR <- NULL
      df$childlessness <- NULL
      df$everMarried <- NULL
    } else {
      df$childlessness <- NULL
      df$everMarried <- NULL
    }
    df$netTFR <- NULL
    
    if (splitChildDep) {
      df$children_dep <- NULL
    } else {
      df$children_dep_health <- NULL
      df$children_dep_age <- NULL
    }
    return (df)
  }
  
  careSupplyDF_dep <- careSupplyDF[careSupplyDF$dependency==dependency,]
  careSupplyDF_dep <- na.omit(careSupplyDF_dep)
  varList <- names(careSupplyDF)
  #remove first seven variables
  varList <- varList[-c(1:7)]
  CS_dep <- list()
  for (ind in (1:length(varList))) {
    CS_dep[[varList[ind]]] <- BS_curves (careSupplyDF_dep, varList[ind])
  }
  
  data_dep <- dataToPlot (CS_dep, sex[1])
  
  subs <- data_dep$cohort %in% cohortsSubs
  mData_dep <- melt(data_dep[subs, ], id=c("ticks", "cohort"))
  
  twoSexes <- FALSE
  if (length(sex) > 1) {
    twoSexes <- TRUE
    data_dep <- dataToPlot (CS_dep, sex[2])
    mData_dep$sex <- sex[1]
    mData_dep2 <- melt(data_dep[subs, ], id=c("ticks", "cohort"))
    mData_dep2$sex <- sex[2]
    mData_dep <- rbind(mData_dep, mData_dep2)
    mData_dep <- addCohortSex(mData_dep)
  }
  
  if ( noChildless ) {
    TFR_value <- "TFR1"
    TFR_value_lab <- "TFR1+"
  } else {
    TFR_value <- "TFR"
    TFR_value_lab <- "TFR"
  }
  
  variables <- c(
    "partner_mort", "partner_dep", "partner_work", "partner_comp", "partner_ego",
    "children_ego", "children_comp_partner", "children_comp_siblings", "children_comp_children",
    "children_work", "children_dep_health", "children_dep", "children_dep_age", "children_mort",
    "widowhoodEffect", TFR_value, "availableCareForEgo"
  )
  if (!widowhood) { variables <- variables [-which(variables=="widowhoodEffect")] }
  if (!splitChildDep) { variables <- variables [-which(variables=="children_dep_age")] }
  if (!splitChildDep) { variables <- variables [-which(variables=="children_dep_health")] }
  if (splitChildDep) { variables <- variables [-which(variables=="children_dep")] }
  if (!nKinAvailableForCareLine) { variables <- variables [-which(variables=="availableCareForEgo")] }
  mData_dep$variable <- factor (mData_dep$variable, levels=variables)
  
  if(retData) {return (mData_dep)}
  
  mData_dep_noTFR <- subset(mData_dep, variable!=TFR_value)
  mData_dep_TFR <- subset(mData_dep, variable==TFR_value)
  if (nKinAvailableForCareLine) {
    mData_dep_noTFR <- subset(mData_dep_noTFR, variable!="availableCareForEgo")
    mData_dep_availCare <- subset(mData_dep, variable=="availableCareForEgo")
  }
  
  #to selectively put a box in the legend for "time for ego", use on of the tricks:
  #https://stackoverflow.com/questions/41704598/how-to-make-ggplot-legend-exactly-reflect-appearance-of-bars
  #https://stackoverflow.com/questions/29994535/how-to-selectively-add-box-around-legend-key/29998347
  #### color supply plot ####
  
  if (greyColor) {
    color_mortality <- "black"
    color_dependency <- "grey30"
    color_dependency_age <- "grey25"
    color_work <- "grey40"
    color_ego <- "grey90"
    color_competition1 <- "grey80"
    if (compPartner) {
      color_competition2 <- "grey50"
    } else {
      color_competition2 <- color_competition1
    }
    if (compSiblings) {
      color_competition3 <- "grey65"
    } else {
      color_competition3 <- color_competition1
    }
    color_childlessnessEffect <- "grey80"
    color_everMarriedEffect <- "grey80"
    color_TFR <- "black"
    color_availableCareForEgo <- "black"
    color_widowhoodEffect <- "grey80"
  } else {
    color_mortality <- "blue"
    color_dependency <- "yellow3"
    color_dependency_age <- "yellow"
    color_work <- "red"
    color_ego <- "grey90"
    color_competition1 <- "orange"
    if (compPartner) {
      color_competition2 <- "orange4"
    } else {
      color_competition2 <- color_competition1
    }
    if (compSiblings) {
      color_competition3 <- "orange3"
    } else {
      color_competition3 <- color_competition1
    }
    color_childlessnessEffect <- "blue"
    color_everMarriedEffect <- "lightskyblue"
    color_TFR <- "black"
    color_availableCareForEgo <- "black"
    color_widowhoodEffect <- "lightskyblue"
  }
  
  if (Brewer) {
    colorBrewerPalette <- c('#e5f5e0','#c7e9c0','#a1d99b','#74c476','#41ab5d','#238b45','#006d2c','#00441b')
    greyBrewerPalette <- c('#f0f0f0','#d9d9d9','#bdbdbd','#969696','#737373','#525252','#252525','#000000')
    
    if (greyColor) {myPalette <- greyBrewerPalette} else {myPalette <- colorBrewerPalette}
    
    color_mortality <- myPalette[8]
    color_dependency <- myPalette[6]
    color_dependency_age <- myPalette[7]
    color_work <- myPalette[4]
    color_ego <- myPalette[1]
    color_competition1 <- myPalette[6]
    if (compPartner) {
      color_competition2 <- myPalette[3]
    } else {
      color_competition2 <- color_competition1
    }
    if (compSiblings) {
      color_competition3 <- myPalette[2]
    } else {
      color_competition3 <- color_competition1
    }
    color_childlessnessEffect <- myPalette[1]
    color_everMarriedEffect <- myPalette[1]
    color_TFR <- "black"
    color_availableCareForEgo <- "black"
    color_widowhoodEffect <- myPalette[1]
  } # if (Brewer)
  
  if (TRUE) {
    color_children_comp_children <- color_competition1
    color_children_comp_partner <- color_competition2
    color_children_comp_siblings <- color_competition3
    color_children_dep <- color_dependency
    color_children_dep_health <- color_dependency
    color_children_dep_age <- color_dependency_age
    color_children_ego <- color_ego
    color_children_mort <- color_mortality
    color_children_work <- color_work
    color_children_ego <- color_ego
    color_partner_comp <- color_competition1
    color_partner_dep <- color_dependency
    color_partner_ego <- color_ego
    color_partner_mort <- color_mortality
    color_partner_work <- color_work
  }
  
  if (!is.null(otherColorScheme)) {
    myColors <- otherColorScheme
  } else {
    myColors <- c(
      color_partner_mort,
      color_partner_dep,
      color_partner_work,
      color_partner_comp,
      color_partner_ego,
      color_children_ego,
      color_children_comp_partner,
      color_children_comp_siblings,
      color_children_comp_children,
      color_children_work,
      color_children_dep,
      color_children_dep_age,
      color_children_mort,
      color_TFR,
      color_availableCareForEgo,
      color_widowhoodEffect
    )
  }
  if (!splitChildDep) { myColors <- myColors [-(length(myColors)-4)] }
  if (!nKinAvailableForCareLine) { myColors <- myColors [-(length(myColors)-1)] }
  if (!widowhood) { myColors <- myColors [-length(myColors)] }
  
  myBreaks <- c(
    "widowhoodEffect",
    "partner_ego",
    "partner_mort",
    "children_dep_age",
    "partner_dep",
    "partner_work",
    "partner_comp",
    "children_comp_partner",
    "children_comp_siblings"
  )
  if (!widowhood) { myBreaks <- myBreaks[-which(myBreaks=="widowhoodEffect")] }
  if (!splitChildDep) { myBreaks <- myBreaks [-which(myBreaks=="children_dep_age")] }
  if (!compPartner) { myBreaks <- myBreaks[-which(myBreaks=="children_comp_partner")] }
  if (!compSiblings) { myBreaks <- myBreaks[-which(myBreaks=="children_comp_siblings")] }
  
  myLabels <- c(
    "Widowhood ",
    "Care supply available ",
    "Mortality ",
    "Age Dep. ",
    "Dependency ",
    "Work ",
    "Comp. other relatives ",
    "Comp. ego's partner ",
    "Comp. children's siblings "
  )
  if (!widowhood) { myLabels <- myLabels[-which(myLabels=="Widowhood ")] }
  if (!splitChildDep) { myLabels <- myLabels [-which(myLabels=="Age Dep. ")] }
  if (!compOtherRel) { myLabels[which(myLabels=="Comp. other relatives ")] <- "Competition " }
  if (!compPartner) { myLabels <- myLabels[-which(myLabels=="Comp. ego's partner ")] }
  if (!compSiblings) { myLabels <- myLabels[-which(myLabels=="Comp. children's siblings ")] }
  
  if (labelFactors) {
    for (i in (1:length(myLabels))) {
      myLabels[i] <- paste(i, ": ", myLabels[i], sep="")
    }
  }
  
  removeValuesLabs <- function(aVector=NULL) {
    if (!splitChildDep) { aVector <- aVector[-(length(aVector)-4)] }
    if (!widowhood) { aVector <- aVector[-(length(aVector)-2)] }
    if (!nKinAvailableForCareLine) { aVector <- aVector[-(length(aVector))] }
    return (aVector)
  }
  
  cohortsValue <- c()
  yDep1 <- c()
  yDep2 <- c()
  labDep <- c("mort", "dep", "work", "comp", "Partner to Ego",
              "Children to Ego", "cPartner", "cSiblings", "cGrandChildren", "work",
              "dep", "ageDep", "mort", "widowhood", TFR_value_lab, "availableCareForEgo")
  labDep <- removeValuesLabs (labDep)
  labDepNum <- c("2", "3", "4", "5", "1", "1", "5", "4", "3", "2")
  labDepNumDetailled <- c("2", "4", "5", "6", "1", "6", "7", "8", "5", "4", "3", "2")
  if (compSiblings) { labDepNum <- labDepNumDetailled }
  if (nKinAvailableForCareLine) {
    S1 <- mData_dep_availCare$value[77-50+1]
    labDep[length(labDep)] <- "S1"
  } else {
    S1 <-NA
  }
  
  for (ind in (1:length(cohortsSubs))) {
    #ind <- 1
    TFR_val <- demRegDF[demRegDF$cohorts==cohortsSubs[ind], TFR_value]
    pCompPartner <- 0
    pCompSiblings <- 0
    pCompGdCh <- 0
    cohortsValue <- c(cohortsValue, rep(cohortsSubs[ind], length(myColors)))
    yDep1 <- c(yDep1, c(NA, NA, NA, NA, TFR_val + .2, TFR_val - .2, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA))
    yDep1 <- removeValuesLabs (yDep1)
    yDep2 <- c(yDep2, c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, TFR_val, S1))
    yDep2 <- removeValuesLabs (yDep2)
  }
  xDep <- c(80, 65, 55, 65, 60, 61, 70, 70, 70, 70, 55, 90, 90, 70, 77, 77)
  xDep <- removeValuesLabs (xDep)
  
  labels_dep1 <- data.frame(
    cohort=cohortsValue,
    x=rep(xDep, length(cohortsSubs)),
    y=yDep1,
    labels=rep(labDep, length(cohortsSubs)),
    variable=rep(variables, length(cohortsSubs)),
    stringsAsFactors = FALSE
  )
  labels_dep2 <- data.frame(
    cohort=cohortsValue,
    x=rep(xDep, length(cohortsSubs)),
    y=yDep2,
    labels=rep(labDep, length(cohortsSubs)),
    variable=rep(variables, length(cohortsSubs)),
    stringsAsFactors = FALSE
  )
  
  if (labelFactors) {
    labels_dep2$labels[1:length(labDepNum)] <- labDepNum
    labels_dep2$y[2] <- .2
  }
  
  if (twoSexes) {
    dupSexLabels <- function (df) {
      #df <- labels_dep1
      df$sex <- sex[1]
      temp <- df
      temp$sex <- sex[2]
      df <- rbind(df, temp)
      return (df)
    }
    labels_dep1 <- dupSexLabels(labels_dep1)
    labels_dep2 <- dupSexLabels(labels_dep2)
    labels_dep1 <- addCohortSex(labels_dep1)
    labels_dep2 <- addCohortSex(labels_dep2)
  }
  
  if (nKinAvailableForCareLine) {
    nSet <- length(labels_dep2$y) / length(myColors)
    nAge <- length(mData_dep_availCare$value) / nSet
    for (ind in (1:nSet)) {
      labels_dep2$y[length(myColors)*ind] <- mData_dep_availCare$value[(77-50+1)+(ind-1)*nAge]
    }
  }
  
  sexStr <- list("T"="Total", "M"="Males", "F"="Females")
  lgdTitle <- "Time 'use' of relatives:"
  
  #myColors <- rep("white", 14)
  #myColors[2] <- "black"
  
  #### plot ####
  if (TRUE) {
    pDemReg_dep <- ggplot(mData_dep, aes(x=ticks, y=value))
    pDemReg_dep <- pDemReg_dep + geom_area(data=mData_dep_noTFR, aes(fill=variable, colour=variable))
    if (nKinAvailableForCareLine) {
      pDemReg_dep <- pDemReg_dep + geom_line(data=mData_dep_availCare, color=color_TFR, size=2)
    }
    pDemReg_dep <- pDemReg_dep + geom_line(data=mData_dep_TFR, color=color_TFR, size=2)
    if (twoSexes) {
      pDemReg_dep <- pDemReg_dep + facet_wrap(vars(cohort_sex), ncol=nCols)
    } else {
      pDemReg_dep <- pDemReg_dep + facet_wrap(vars(cohort), ncol=nCols)
    }
    pDemReg_dep <- pDemReg_dep +
      scale_fill_manual(breaks=myBreaks, values=myColors, labels=myLabels)
    pDemReg_dep <- pDemReg_dep +
      scale_colour_manual(breaks=myBreaks, values=myColors, labels=myLabels)
    pDemReg_dep <- pDemReg_dep + theme_minimal() +
      theme(panel.background=element_rect(fill = "white")) + theme_text()
    pDemReg_dep <- pDemReg_dep + labs(x="ego's age", y="mean number of relatives",
                                      fill=lgdTitle, colour=lgdTitle)
    pDemReg_dep <- pDemReg_dep +
      geom_text(data=labels_dep1, aes(x=x, y=y, label=labels), color="black", size=sizeLabels, show.legend = FALSE)
    pDemReg_dep <- pDemReg_dep +
      geom_label(data=labels_dep2, aes(x=x, y=y, label=labels), fill="white", size=sizeLabels, show.legend = FALSE)
    pDemReg_dep <- pDemReg_dep + theme(legend.position="bottom", legend.text = element_text(colour="black", size = sizeLabels))
    if (sizeTextLegend != 0) {
      pDemReg_dep <- pDemReg_dep + theme(legend.text = element_text(colour="black", size = sizeTextLegend))
    }
    if (sexAsTitle) {
      pDemReg_dep <- pDemReg_dep + labs(caption=sexStr[sex])
    }
  }
  
  return (pDemReg_dep)
  
}

