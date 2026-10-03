# 2026-08-14 UPDATE (2) -- computeBalance()'s doIt() now also reports
#   NoDep (non-dependent headcount), Alive (Dep+NoDep) and PropDep
#   (Dep/Alive, the age-specific dependency RATE among egos alive at that
#   tick) alongside the existing Dep raw headcount. Previously "Dep" was
#   the only thing available downstream, and the plotting scripts (see
#   democare_graphs.R's "1 dependency" section) normalized it by dividing
#   by a hardcoded initial-cohort-size constant (11000, split 48.8%/51.2%
#   by sex) -- wrong for two reasons: it doesn't hold for arbitrary
#   comparison runs with a different population size, and it ignores
#   mortality shrinking the denominator as the cohort ages, which
#   understates dependency prevalence at older ages. This is what produced
#   the "150,000%" y-axis Daniel saw in compare_three_sources.R's
#   dependency plot -- that script fed the raw Dep headcount straight into
#   a percent-formatted axis with no normalization at all. PropDep is now
#   computed correctly from the data itself: cTabF0/cTabM0/cTabTot0 (the
#   dependent==0 mirror of the existing cTabF/cTabM/cTabTot) are built the
#   same way as their dependent==1 counterparts and passed into doIt()
#   alongside them, entirely within computeBalance() -- no external
#   population-size assumption needed anywhere. See doIt()'s own header
#   comment for the column-by-column detail, and
#   feedback_computecare_testing.md for the full writeup.
#
# 2026-08-14 UPDATE -- computeBalance() made resilient to a genuinely-
#   empty familyKind category (see below, inside computeBalance()).
#   Previously it assumed every (tick, sex, dependent, familyKind)
#   combination appears at least once, and crashed ("replacement has 0
#   rows..."/"replacement has 1 row, data has 0") whenever a whole
#   category -- most often familyKind=="noPartner" (no lifetime partner
#   AND no lifetime children, a real ~10% of egos) -- happened to have
#   zero individuals anywhere in a given run/sample. Fixed in three
#   places, all marked "ROBUSTNESS FIX (2026-08-14)" inline: the
#   familyKind factor() call now always declares all 3 levels explicitly
#   (so a fully-absent category still gets processed as an empty one
#   instead of vanishing as a level); adjust() now falls back to a zero-
#   individual placeholder instead of crashing when no earlier tick has a
#   seed row to duplicate forward; complete()'s row-count-based guard
#   (which could skip padding a genuinely incomplete tick if the table
#   happened to be "big enough" overall) was removed in favour of an
#   unconditional per-tick check. Tested against: the full sample
#   population (identical output to before, byte-for-byte -- this is a
#   true no-op whenever nothing was actually missing); a run where
#   "noPartner" never occurs at all (previously crashed, now completes
#   and correctly reports NaN, not 0, for the by-partner/by-children
#   proportions of that empty cell -- 0 would misleadingly imply "no
#   coverage", NaN honestly means "nobody in this cell").
library2 <- function(pack) {
  #pack<-"tcltk2"
  if( !(pack %in% installed.packages()))
  {install.packages(pack)}
  library(pack,character.only = TRUE)
}

library2("reshape2")

#max age (will limit the max tick)
kMaxAge <- 95
kMaxTick <- kMaxAge - 50


getDirSep <- function() {
  if (.Platform$OS.type == "unix") {
    return ("/")
  } else {
    return (intToUtf8(92))
  }
}

#### care supply ----
careSupply <- function (tab, cohort, iSimulFiles, iDemoCareFiles) {
  #cohort <- "1908"
  tab$dependent <- ifelse(tab$care < 0, 1, 0)
  tab$individual <- 1
  
  listFields <- c(
    "individual",
    "kPartnerHours",
    "kTotalHours",
    "kNoPartner_mort",
    "kNoChildren_mort",
    "kPartnerOthers",
    "kWork_partner",
    "kWork_children",
    "kDependency_partner",
    "kDependency_children",
    "kDependency_health_children",
    "kDependency_age_children",
    "kCompetition_partner",
    "kCompetition_children",
    "kCompetition_children_family",
    "egoPtoChild",
    "childToEgoP",
    "childToPartner",
    "childToSiblings",
    "childTograndCh"
  )
  mTab <- melt(tab, id=c("ticks", "who", "sex", "dependent"), measure=listFields)
  cTab <- dcast(mTab, ticks + sex + dependent ~ variable, sum)
  
  cTab$cohort <- as.integer(cohort)
  cTab$iSimul <- iSimulFiles
  cTab$iDemoCare <- iDemoCareFiles
  
  return (cTab)
}

calcFertNupt <- function (tab, cohort, iSimulFiles, iDemoCareFiles) {
  tab$individual <- 1
  tab$childless <- ifelse(tab$nChildren==0,1,0)
  tab$single <- ifelse(tab$maritalStatus==0,1,0)
  tab$married <- ifelse(tab$maritalStatus==1,1,0)
  tab$widow <- ifelse(tab$maritalStatus==2,1,0)
  
  mTab <- melt(tab, id=c("ticks", "who", "sex"), measure=c("individual", "childless", "nChildren", "single", "married", "widow"))
  cTab <- dcast(mTab, ticks + sex ~ variable, sum)
  
  cTabFertNuptF <- data.frame (cohort=cohort, iSimul=iSimulFiles, iDemoCare=iDemoCareFiles, sex="F",
                               TFR=cTab[1,]$nChildren / cTab[1,]$individual,
                               childlessness=cTab[1,]$childless / cTab[1,]$individual,
                               singlehood=cTab[1,]$single / cTab[1,]$individual,
                               wedlock=cTab[1,]$married / cTab[1,]$individual,
                               widowhood=cTab[1,]$widow / cTab[1,]$individual
  )
  cTabFertNuptM <- data.frame (cohort=cohort, iSimul=iSimulFiles, iDemoCare=iDemoCareFiles, sex="M",
                               TFR=cTab[2,]$nChildren / cTab[2,]$individual,
                               childlessness=cTab[2,]$childless / cTab[2,]$individual,
                               singlehood=cTab[2,]$single / cTab[2,]$individual,
                               wedlock=cTab[2,]$married / cTab[2,]$individual,
                               widowhood=cTab[2,]$widow / cTab[2,]$individual
  )
  cTabFertNuptT <- data.frame (cohort=cohort, iSimul=iSimulFiles, iDemoCare=iDemoCareFiles, sex="T",
                               TFR=(cTab[1,]$nChildren+cTab[2,]$nChildren) / (cTab[1,]$individual + cTab[2,]$individual),
                               childlessness=(cTab[1,]$childless+cTab[2,]$childless) / (cTab[1,]$individual + cTab[2,]$individual),
                               singlehood=(cTab[1,]$single+cTab[2,]$single) / (cTab[1,]$individual + cTab[2,]$individual),
                               wedlock=(cTab[1,]$married+cTab[2,]$married) / (cTab[1,]$individual + cTab[2,]$individual),
                               widowhood=(cTab[1,]$widow+cTab[2,]$widow) / (cTab[1,]$individual + cTab[2,]$individual)
  )
  
  return (rbind (cTabFertNuptT, cTabFertNuptF, cTabFertNuptM))
}

reg <- function (cTabInt) {
  if (dim(cTabInt)[1] < kMaxTick) {
    #uncomplete table. We add missing ticks at the end, duplicating last row
    nRows <- dim(cTabInt)[1]
    lastRow <- cTabInt[nRows,]
    for ( indTick in ((nRows+1):kMaxTick) ) {
      #indTick <- nRows+1
      cTabInt <- rbind (cTabInt, lastRow)
      cTabInt[indTick,]$ticks <- indTick - 1
    }
  }
  return ( cTabInt )
  
}

computeCareSupply <- function (cTab) {
  doIt <- function (cTabInt, aSex, dependency) {
    #aSex <- "F"
    #dependency <- "notDependent"
    #cTabInt <- cTabNoDep[(cTabNoDep$sex=="F"), ]
    
    cTabRel <- data.frame(cohort=rep(cohort, kMaxTick), iSimul=rep(iSimul, kMaxTick), iDemoCare=rep(iDemoCare, kMaxTick),
                          ticks=c(0:(kMaxTick-1)), sex=rep(aSex, kMaxTick),
                          dependency=rep(dependency, kMaxTick)
    )
    
    cTabRel$individual <- cTabInt$individual
    
    cTabRel$partner_mort <- cTabInt$kNoPartner_mort / cTabInt$kPartnerHours
    cTabRel$partner_dep <- cTabInt$kDependency_partner / cTabInt$kPartnerHours
    cTabRel$partner_work <- cTabInt$kWork_partner / cTabInt$kPartnerHours
    cTabRel$partner_comp <- cTabInt$egoPtoChild / cTabInt$kPartnerHours
    cTabRel$partner_ego <- 1 - (cTabRel$partner_mort + cTabRel$partner_dep +
                                  cTabRel$partner_work + cTabRel$partner_comp)
    
    cTabRel$children_mort <- cTabInt$kNoChildren_mort / cTabInt$kChildrenHours
    cTabRel$children_dep <- cTabInt$kDependency_children / cTabInt$kChildrenHours
    cTabRel$children_dep_health <- cTabInt$kDependency_health_children / cTabInt$kChildrenHours
    cTabRel$children_dep_age <- cTabInt$kDependency_age_children / cTabInt$kChildrenHours
    cTabRel$children_work <- cTabInt$kWork_children / cTabInt$kChildrenHours
    cTabRel$children_comp_partner <- cTabInt$childToEgoP / cTabInt$kChildrenHours
    cTabRel$children_comp_siblings <- cTabInt$childToSiblings / cTabInt$kChildrenHours
    cTabRel$children_comp_children <- (cTabInt$childTograndCh + cTabInt$childToPartner) / cTabInt$kChildrenHours
    cTabRel$children_ego <- 1 - cTabRel$children_mort - cTabRel$children_dep -
      cTabRel$children_work - cTabRel$children_comp_partner -
      cTabRel$children_comp_siblings -
      cTabRel$children_comp_children
    
    return (cTabRel)
  }
  
  cTab <- cTab[cTab$ticks < kMaxTick,]
  iSimul <- cTab$iSimul[1]
  iDemoCare <- cTab$iDemoCare[1]
  cohort <- cTab$cohort[1]
  
  cTab$kChildrenHours <- cTab$kTotalHours - cTab$kPartnerHours
  
  #remove sex field (which is a factor variable than cannot be added)
  sFields <- names(cTab)[-2]
  cTabDep <- cTab[(cTab$dependent==1), ]
  cTabRelDepF <- doIt ( reg ( cTabDep[(cTabDep$sex=="F"), sFields] ), "F", "dependent" )
  cTabRelDepM <- doIt ( reg ( cTabDep[(cTabDep$sex=="M"), sFields] ), "M", "dependent" )
  cTabRelDepT <- doIt ( reg ( cTabDep[(cTabDep$sex=="M"), sFields] ) +
                          reg ( cTabDep[(cTabDep$sex=="F"), sFields] ), "T", "dependent" )
  
  cTabNoDep <- cTab[(cTab$dependent==0), ]
  cTabRelNoDepF <- doIt ( reg ( cTabNoDep[(cTabNoDep$sex=="F"), sFields] ), "F", "notDependent" )
  cTabRelNoDepM <- doIt ( reg ( cTabNoDep[(cTabNoDep$sex=="M"), sFields] ), "M", "notDependent" )
  cTabRelNoDepT <- doIt ( reg ( cTabNoDep[(cTabNoDep$sex=="M"), sFields] ) +
                            reg ( cTabNoDep[(cTabNoDep$sex=="F"), sFields] ), "T", "notDependent" )
  
  cTabTotF <- reg ( cTab[(cTab$dependent==1)&(cTab$sex=="F"), sFields] ) +
    reg ( cTab[(cTab$dependent==0)&(cTab$sex=="F"), sFields] )
  cTabTotM <- reg ( cTab[(cTab$dependent==1)&(cTab$sex=="M"), sFields] ) +
    reg ( cTab[(cTab$dependent==0)&(cTab$sex=="M"), sFields] )
  cTabRelTotF <- doIt ( cTabTotF, "F", "total" )
  cTabRelTotM <- doIt ( cTabTotM, "M", "total" )
  cTabRelTotT <- doIt ( cTabTotF + cTabTotM, "T", "total" )
  
  return ( rbind (cTabRelDepF, cTabRelDepM, cTabRelDepT,
                  cTabRelNoDepF, cTabRelNoDepM, cTabRelNoDepT,
                  cTabRelTotF, cTabRelTotM, cTabRelTotT) )
  
}

checkBSFiles <- function(verbose=FALSE, unzipIt=FALSE) {
  for (nDemReg in (1:length(nDemRegFileNames))) {
    # nDemReg <- 1
    nf <- nDemRegFileNames[nDemReg]
    cohort <- substring(nf, 4, 7)
    for (iSimulFiles in (1:nSimulFiles)) {
      # iSimulFiles <- 1
      for (iDemoCareFiles in (1:nDemoCareFiles)) {
        # iDemoCareFiles <- 1
        nameFile <- paste(nf, "_", iSimulFiles, "_", iDemoCareFiles, ".txt" , sep="")
        nameFileZip <- paste(path, nameFile, ".zip", sep="")
        if (verbose) message(nameFileZip)
        res <- unzip (nameFileZip, list=!unzipIt)
        if (unzipIt) {
          tab <- read.table(nameFile, header=TRUE)
          good <- file.remove(nameFile)
        }
      }
    }
  }
}

createCareSupplyDF <- function(nDemRegFileNames=NULL, onlyOneFile=FALSE,
                               nSimulFiles=1, nDemoCareFiles=1, 
                               careSupplyDF=data.frame(),
                               cTabFertNuptDF=data.frame(),
                               debSimul=1, debDemoCare=1, pathOut=NULL
) {
  if (onlyOneFile) {
     
  } else {
    for (nDemReg in (1:length(nDemRegFileNames))) {
      # nDemReg <- 1
      nf <- nDemRegFileNames[nDemReg]
      cohort <- as.integer ( substring(nf, 4, 7) )
      if (is.na(cohort)) {cohort <- 2020}
      cohort <- as.character(cohort)
      for (iSimulFiles in (debSimul:nSimulFiles)) {
        # iSimulFiles <- 1
        for (iDemoCareFiles in (debDemoCare:nDemoCareFiles)) {
          # iDemoCareFiles <- 3
          nameFile <- paste(nf, "_", iSimulFiles, "_", iDemoCareFiles, ".txt" , sep="")
          if (zipped) {
            nameFileZip <- paste(path, nameFile, ".zip", sep="")
            unzip (nameFileZip, exdir=pathData)
          }
          message (nameFile)
          nameFileOut <- paste(pathData, nameFile, sep="")
          #nameFile <- paste(path, "KIN1938_EDU1_VAR_1_3.txt", sep="")
          tab <- read.table(nameFileOut, header=TRUE, row.names=NULL, sep="\t")
          if (zipped) {
            good <- file.remove(nameFileOut)
          }
          #cohort<-1938
          cTabFertNupt <- calcFertNupt (tab, cohort, iSimulFiles, iDemoCareFiles)
          cTabFertNuptDF <- rbind ( cTabFertNuptDF, cTabFertNupt )
          cTab <- careSupply (tab, cohort, iSimulFiles, iDemoCareFiles)
          cTabRel <- computeCareSupply (cTab)
          careSupplyDF <- rbind (careSupplyDF, cTabRel)
        }
        save (cTabFertNuptDF, file="FertNupt.RData")
        save (careSupplyDF, file=paste ("careSupply", ".RData", sep=""))
        message (paste ("careSupplyDF saved for cohort", cohort))
      }
    }
  }
}

#### informal care ----
balanceFormalInformal <- function (tab, cohort, iSimulFiles, iDemoCareFiles) {
  #cohort <- "1908"
  #BALANCE BETWEEN FORMAL AND INFORMAL CARE
  
  tab$dependent <- ifelse(tab$care < 0, 1, 0)
  tab$individual <- 1
  #'noPartner' means NO partner and NO children
  #'Partner' means having a partner and NO children
  #'Children' means having a partner AND children
  tab$familyKind <- ifelse(tab$nPartner > 0,"Partner","noPartner")
  tab$familyKind <- ifelse(tab$nChildren > 0,"Children",tab$familyKind)
  # ROBUSTNESS FIX (2026-08-14): declare all 3 possible levels explicitly,
  # even if one of them (typically "noPartner") happens to have zero rows
  # in THIS run -- without an explicit levels=, factor() only declares
  # whichever values actually occur, so a fully-absent category would
  # silently disappear as a level too, and computeBalance() below (which
  # loops over levels(cTab$familyKind) and also references "noPartner" by
  # name for its Total-row calculation) would then either skip it
  # entirely or break.
  tab$familyKind <- factor(tab$familyKind, levels = c("noPartner", "Partner", "Children"))
  
  mTab <- melt(tab, id=c("ticks", "who", "sex", "dependent", "familyKind"),
               measure=c("individual", "care", "netCareDemand", "carebyPartner", "carebyChildren", "care2byPartner", "care2byChildren"))
  cTab <- dcast(mTab, ticks + sex + dependent + familyKind ~ variable, sum)
  cTab$cohort <- as.integer(cohort)
  cTab$iSimul <- iSimulFiles
  cTab$iDemoCare <- iDemoCareFiles
  
  return (cTab)
}

computeBalance <- function (cTab) {
  
  adjust <- function (cTab) {
    #add missing rows at the end, duplicating preceding ones
    #
    # ROBUSTNESS FIX (2026-08-14): the original version always looked back
    # exactly ONE tick (tick-1) for a row to duplicate forward, and
    # crashed ("replacement has 1 row, data has 0") whenever a (sex,
    # dependent, familyKind) combination had never appeared in ANY
    # earlier tick either -- e.g. familyKind=="noPartner" is a real but
    # small share of egos, so a smaller run/sample can end up with zero
    # such individuals at every tick up to 29. This version searches
    # backward for the MOST RECENT tick that does have a row for that
    # combination; if none exists at all, it inserts a zero-individual
    # placeholder instead of crashing -- an NA-row template (cTab[1,][NA,])
    # is used so this keeps working regardless of exactly which columns
    # cTab has, the same convention complete() already uses below.
    zero_template <- cTab[1, ][NA, ]  # one row, all columns, all NA
    zero_template[, c("individual", "care", "netCareDemand",
                      "carebyPartner", "carebyChildren",
                      "care2byPartner", "care2byChildren")] <- 0

    for (tick in (30:(kMaxTick-1))) {
      #tick=42
      for (sex in c("F", "M")) {
        #sex="M"
        for (dependent in c(0, 1)) {
          #dependent=1
          for (familyKind in levels(cTab$familyKind)) {
            #familyKind="Partner"
            if (dim(cTab[
              cTab$ticks==tick &
              cTab$sex==sex &
              cTab$dependent==dependent &
              cTab$familyKind==familyKind,])[1]==0) {
              earlierRows <- cTab[
                cTab$ticks < tick &
                  cTab$sex==sex &
                  cTab$dependent==dependent &
                  cTab$familyKind==familyKind,]
              if (dim(earlierRows)[1] > 0) {
                lastRow <- earlierRows[which.max(earlierRows$ticks), ]
              } else {
                lastRow <- zero_template
                lastRow$sex <- sex
                lastRow$dependent <- dependent
                lastRow$familyKind <- familyKind
                lastRow$cohort    <- cTab$cohort[1]
                lastRow$iSimul    <- cTab$iSimul[1]
                lastRow$iDemoCare <- cTab$iDemoCare[1]
              }
              lastRow$ticks <- tick
              cTab <- rbind (cTab, lastRow)
            }
          }
        }
      }
    }
    return (cTab)
  }
  
  doIt <- function (cTabInt, cTabIntNoDep, aSex) {
    #aSex <- "M"
    #cTabInt <- cTab[(cTab$sex==aSex)&(cTab$dependent==1), sFields]
    #cTabIntNoDep <- cTab[(cTab$sex==aSex)&(cTab$dependent==0), sFields]

    temp <- data.frame(iSimul=rep(iSimul, kMaxTick),
                       iDemoCare=rep(iDemoCare, kMaxTick),
                       cohort=rep(cohort, kMaxTick),
                       ticks=c(0:(kMaxTick-1)),
                       sex=rep(aSex, kMaxTick),
                       familyKind=rep("noPartner", kMaxTick))
    cTabRel <- temp
    temp$familyKind <- "Partner"
    cTabRel <- rbind(cTabRel, temp)
    temp$familyKind <- "Children"
    cTabRel <- rbind(cTabRel, temp)
    temp$familyKind <- "Total"
    cTabRel <- rbind(cTabRel, temp)
    cTabRel$familyKind <- factor (cTabRel$familyKind)
    cTabRel$Dep <- 0
    # NEW (2026-08-14): NoDep is the non-dependent headcount for the same
    # (tick, sex, familyKind) cell as Dep -- both are needed to get "living
    # egos at this tick" (Alive = Dep + NoDep) and a real age-specific
    # dependency rate (PropDep = Dep / Alive), instead of dividing the raw
    # Dep headcount by a hardcoded initial-cohort-size constant downstream
    # in the plotting scripts (which ignored mortality thinning the cohort
    # as it ages, and assumed a population size that doesn't hold for
    # arbitrary comparison runs). See feedback_computecare_testing.md.
    cTabRel$NoDep <- 0
    cTabRel$Care <- 0
    cTabRel$CareByPartner <- 0
    cTabRel$CareByChildren <- 0
    cTabRel$Care2ByPartner <- 0
    cTabRel$Care2ByChildren <- 0

    for (fk in levels(cTabInt$familyKind)) {
      #fk <- levels(cTabInt$familyKind)[1]
      s1 <- cTabRel$familyKind==fk
      s2 <- cTabInt$familyKind==fk
      s2n <- cTabIntNoDep$familyKind==fk
      cTabRel[s1,]$Care <- - cTabInt[s2,]$care
      cTabRel[s1,]$Dep <- cTabInt[s2,]$individual
      cTabRel[s1,]$NoDep <- cTabIntNoDep[s2n,]$individual
      cTabRel[s1,]$CareByPartner <- cTabInt[s2,]$carebyPartner / (- cTabInt[s2,]$care)
      cTabRel[s1,]$CareByChildren <- cTabInt[s2,]$carebyChildren / (- cTabInt[s2,]$care)
      cTabRel[s1,]$Care2ByPartner <- cTabInt[s2,]$care2byPartner / (- cTabInt[s2,]$care)
      cTabRel[s1,]$Care2ByChildren <- cTabInt[s2,]$care2byChildren / (- cTabInt[s2,]$care)
    }
    s1 <- cTabRel$familyKind=="Total"
    s2 <- cTabInt$familyKind=="Partner"
    s3 <- cTabInt$familyKind=="noPartner"
    s4 <- cTabInt$familyKind=="Children"
    s2n <- cTabIntNoDep$familyKind=="Partner"
    s3n <- cTabIntNoDep$familyKind=="noPartner"
    s4n <- cTabIntNoDep$familyKind=="Children"
    cTabRel[s1,]$Care <- - cTabInt[s2,]$care - cTabInt[s3,]$care - cTabInt[s4,]$care
    cTabRel[s1,]$Dep <- cTabInt[s2,]$individual + cTabInt[s3,]$individual + cTabInt[s4,]$individual
    cTabRel[s1,]$NoDep <- cTabIntNoDep[s2n,]$individual + cTabIntNoDep[s3n,]$individual + cTabIntNoDep[s4n,]$individual
    cTabRel[s1,]$CareByPartner <- (cTabInt[s2,]$carebyPartner + cTabInt[s3,]$carebyPartner + cTabInt[s4,]$carebyPartner) /
      cTabRel[s1,]$Care
    cTabRel[s1,]$CareByChildren <- (cTabInt[s2,]$carebyChildren + cTabInt[s3,]$carebyChildren + cTabInt[s4,]$carebyChildren) /
      cTabRel[s1,]$Care
    cTabRel[s1,]$Care2ByPartner <- (cTabInt[s2,]$care2byPartner + cTabInt[s3,]$care2byPartner + cTabInt[s4,]$care2byPartner) /
      cTabRel[s1,]$Care
    cTabRel[s1,]$Care2ByChildren <- (cTabInt[s2,]$care2byChildren + cTabInt[s3,]$care2byChildren + cTabInt[s4,]$care2byChildren) /
      cTabRel[s1,]$Care

    # NEW (2026-08-14): Alive/PropDep -- the actual age-specific dependency
    # rate. NaN (not 0) when Alive==0, i.e. genuinely nobody tracked in
    # that cell -- consistent with the ROBUSTNESS FIX (2026-08-14) NaN
    # convention already used elsewhere in this file (0 would misleadingly
    # claim "no dependency" when the honest answer is "no data here").
    cTabRel$Alive <- cTabRel$Dep + cTabRel$NoDep
    cTabRel$PropDep <- ifelse (cTabRel$Alive > 0, cTabRel$Dep / cTabRel$Alive, NaN)

    return (cTabRel)
  }
  
  complete <- function (cTabS) {
    #cTabS <- cTabM
    # ROBUSTNESS FIX (2026-08-14): the original `if (dim(cTabS)[1] <
    # (kMaxTick * 3))` guard skipped this whole padding step once the
    # table had "enough" rows OVERALL -- but that is a total-row-count
    # heuristic, not a real per-tick check, so it could wrongly skip
    # padding a tick that is missing one familyKind while some OTHER tick
    # has extra rows to compensate. Just always run the per-tick check
    # instead -- cheap (at most kMaxTick*3 rows to scan) and correct
    # regardless of the overall count.
    nt <- table(cTabS$ticks)
    nt <- nt[nt<3]
    # seq_along(), not 1:length(nt): when nt is empty (nothing needs
    # padding -- the common case now that this runs unconditionally),
    # 1:length(nt) is the classic R gotcha 1:0 == c(1, 0), which would
    # wrongly run this loop body with a nonsensical index.
    for (i in seq_along(nt)) {
      t <- as.integer(names(nt[i]))
      s <- cTabS[cTabS$ticks==t,]$familyKind
      ss <- levels(s)
      for (fk in ss) {
        if (!(fk %in% s)) {
          dfS <- data.frame(ticks=c(t), individual=c(0), familyKind=fk,
                            care=0, netCareDemand=0, carebyPartner=0, carebyChildren=0,
                            care2byPartner=0, care2byChildren=0)
          cTabS <- rbind(cTabS, dfS)
        }
      }
    }
    cTabS <- cTabS[order(cTabS$ticks),]
    return (cTabS)
  }
  cTab <- cTab[cTab$ticks < kMaxTick,]
  cTab <- adjust (cTab)
  iSimul <- cTab$iSimul[1]
  iDemoCare <- cTab$iDemoCare[1]
  cohort <- cTab$cohort[1]
  
  sFields <- c("ticks", "individual", "familyKind", "care", "netCareDemand", "carebyPartner", "carebyChildren", "care2byPartner", "care2byChildren")
  cTabF <- cTab[(cTab$sex=="F")&(cTab$dependent==1), sFields]
  cTabM <- cTab[(cTab$sex=="M")&(cTab$dependent==1), sFields]
  cTabF <- complete(cTabF)
  cTabM <- complete(cTabM)

  # NEW (2026-08-14): the non-dependent counterpart of cTabF/cTabM above,
  # same shape, same complete()-padding -- this is what lets doIt() report
  # NoDep/Alive/PropDep (see doIt()'s header comment). adjust() (called on
  # the full cTab just above) already fills in both dependent==0 and
  # dependent==1 rows, so this is just the mirror-image subset/complete()
  # of what the dependent==1 branch already does.
  cTabF0 <- cTab[(cTab$sex=="F")&(cTab$dependent==0), sFields]
  cTabM0 <- cTab[(cTab$sex=="M")&(cTab$dependent==0), sFields]
  cTabF0 <- complete(cTabF0)
  cTabM0 <- complete(cTabM0)

  cTabRelF <- doIt ( cTabF, cTabF0, "F" )
  cTabRelM <- doIt ( cTabM, cTabM0, "M" )
  cTabTot <- cTabF
  cTabTot$individual <- cTabTot$individual + cTabM$individual
  cTabTot$care <- cTabTot$care + cTabM$care
  cTabTot$netCareDemand <- cTabTot$netCareDemand + cTabM$netCareDemand
  cTabTot$carebyPartner <- cTabTot$carebyPartner + cTabM$carebyPartner
  cTabTot$carebyChildren <- cTabTot$carebyChildren + cTabM$carebyChildren
  cTabTot$care2byPartner <- cTabTot$care2byPartner + cTabM$care2byPartner
  cTabTot$care2byChildren <- cTabTot$care2byChildren + cTabM$care2byChildren

  cTabTot0 <- cTabF0
  cTabTot0$individual <- cTabTot0$individual + cTabM0$individual

  cTabRelT <- doIt ( cTabTot, cTabTot0, "T" )

  return ( rbind (cTabRelF, cTabRelM, cTabRelT) )
}

computeInformalCare <- function(nDemRegFileNames=NULL,
                                nSimulFiles=1, nDemoCareFiles=1, 
                                informalCareDF=data.frame(),
                                debSimul=1, debDemoCare=1,
                                zipped=TRUE, path=NULL, pathOut=NULL) {
  for (nDemReg in (1:length(nDemRegFileNames))) {
    # nDemReg <- 1
    nf <- nDemRegFileNames[nDemReg]
    cohort <- as.integer( substring(nf, 4, 7) )
    if (is.na(cohort)) {cohort <- 2020}
    cohort <- as.character(cohort)
    for (iSimulFiles in (debSimul:nSimulFiles)) {
      # iSimulFiles <- 1
      for (iDemoCareFiles in (debDemoCare:nDemoCareFiles)) {
        # iDemoCareFiles <- 1
        nameFile <- paste(nf, "_", iSimulFiles, "_", iDemoCareFiles, ".txt" , sep="")
        if (zipped) {
          nameFileZip <- paste(path, nameFile, ".zip", sep="")
          unzip (nameFileZip, exdir=pathOut)
        }
        message (nameFile)
        nameFileOut <- paste(pathOut, nameFile, sep="")
        #nameFile <- paste(path, "KIN1938_EDU1_VAR_1_3.txt", sep="")
        tab <- read.table(nameFileOut, header=TRUE, row.names = NULL, sep="\t")
        if (zipped) {
          good <- file.remove(nameFileOut)
        }
        
        #cohort<-1938
        cTab <- balanceFormalInformal (tab, cohort, iSimulFiles, iDemoCareFiles)
        cTabRel <- computeBalance (cTab)
        informalCareDF <- rbind (informalCareDF, cTabRel)
      }
    }
    save (informalCareDF, file="formal_informal.RData")
    message (paste ("informalCareDF saved for cohort", cohort))
  }
}

#### Create DemoCare data files from only one file ----
createDemoCareFiles <- function (filePath=NULL, pathOut=NULL, cohort=2020) {
  if (exists("DEBUG1")) browser()
  
  if (is.null(pathOut)) pathOut <- paste (dirname(filePath), getDirSep(), sep="")
  tab <- read.table(filePath, header=TRUE, row.names = NULL, sep="\t")
  
  cohort <- as.character(cohort)
  iSimulFiles <- 1
  iDemoCareFiles <- 1
  
  cTab <- balanceFormalInformal (tab, cohort, iSimulFiles, iDemoCareFiles)
  informalCareDF <- computeBalance (cTab)
  aFileNameWithPath <- paste(pathOut, "formal_informal.RData", sep="")
  save (informalCareDF, file=aFileNameWithPath)
  message (paste(aFileNameWithPath, " saved", sep=""))
  

  cTabFertNuptDF <- calcFertNupt (tab, cohort, iSimulFiles, iDemoCareFiles)
  aFileNameWithPath <- paste(pathOut, "FertNupt.RData", sep="")
  save (cTabFertNuptDF, file=aFileNameWithPath)
  message (paste(aFileNameWithPath, " saved", sep=""))

  cTab <- careSupply (tab, cohort, iSimulFiles, iDemoCareFiles)
  careSupplyDF <- computeCareSupply (cTab)
  aFileNameWithPath <- paste(pathOut, "careSupply.RData", sep="")
  save (careSupplyDF, file=aFileNameWithPath)
  message (paste(aFileNameWithPath, " saved", sep=""))
  
}
