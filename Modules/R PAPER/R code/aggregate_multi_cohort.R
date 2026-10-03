currPath <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(currPath)
source("democare_compute.R")

# ==== 1. Build the cohort -> file map ====

# This is the piece createCareSupplyDF()/computeInformalCare() (further down
# in democare_compute.R) already do, but those two assume the rigid
# bootstrap naming convention nf_iSimul_iDemoCare.txt (e.g.
# "KIN1938_EDU1_VAR_1_3.txt") plus optional .zip files. This script does the
# same aggregation across birth-cohort files, but for the simpler case of
# one plain .dat file per cohort (the same "chooseFile"-style output
# createDemoCareFiles.R already reads for a single cohort) -- just looped
# over as many cohort files as you pick.

# OPTION A (recommended): pick files interactively, one cohort at a time.
build_file_map_interactive <- function(startPath = currPath) {
  file_map <- character(0)
  repeat {
    filePath <- rstudioapi::selectFile(
      caption  = sprintf("Select a DEMOCARE file for cohort #%d (*.dat) -- Cancel when done",
                         length(file_map) + 1),
      filter   = "DAT Files (*.dat)",
      path     = startPath,
      existing = TRUE
    )
    if (is.null(filePath)) break

    cohort <- rstudioapi::showPrompt(
      title   = "Cohort",
      message = sprintf("Birth cohort for:\n%s", basename(filePath)),
      default = ""
    )
    if (is.null(cohort) || cohort == "") {
      message("No cohort label given -- file skipped.")
    } else {
      file_map[cohort] <- filePath
    }

    another <- rstudioapi::showQuestion("Add another cohort?",
                                        sprintf("%d cohort(s) selected so far.", length(file_map)),
                                        ok = "Yes", cancel = "No, I'm done")
    if (!another) break
  }
  file_map
}

file_map <- build_file_map_interactive()

# OPTION B: skip the picker and just edit this named vector directly, then
# comment out the OPTION A call above. Names are the cohort labels used in
# the output (they don't have to come from the file name):
#
# file_map <- c(
#   "1908" = "/path/to/cohort1908.dat",
#   "1928" = "/path/to/cohort1928.dat",
#   "1948" = "/path/to/cohort1948.dat",
#   "1968" = "/path/to/cohort1968.dat"
# )

if (length(file_map) == 0) stop("no cohort files selected")
message(sprintf("Cohorts to aggregate: %s", paste(names(file_map), collapse = ", ")))


# ==== 2. Output directory ====

answer <- rstudioapi::showQuestion("Save aggregated files",
                                   "Output directory same as the first cohort file's?",
                                   ok = "Same", cancel = "Different")
pathOut <- paste(dirname(file_map[[1]]), getDirSep(), sep = "")
if (answer == FALSE) {
  path <- rstudioapi::selectDirectory(caption = "Select Output Directory",
                                      label = "Select", path = pathOut)
  if (!is.null(path)) pathOut <- paste(path, getDirSep(), sep = "")
}


# ==== 3. Loop over cohorts, accumulate the three DFs ====

# Same three data frames/functions createDemoCareFiles() builds for one
# file (see democare_compute.R), just rbind-ed across cohorts here. Column
# names/order are untouched so democare_graphs.R can load these exactly as
# it already does (it loads "FertNupt.RData"/"careSupply.RData"/
# "formal_informal.RData" and expects cTabFertNuptDF/careSupplyDF/
# informalCareDF respectively).

iSimulFiles    <- 1
iDemoCareFiles <- 1

cTabFertNuptDF <- data.frame()
careSupplyDF   <- data.frame()
informalCareDF <- data.frame()

for (cohort in names(file_map)) {
  message(sprintf("Processing cohort %s: %s", cohort, file_map[[cohort]]))
  tab <- read.table(file_map[[cohort]], header = TRUE, row.names = NULL, sep = "\t")

  cTabFertNupt   <- calcFertNupt(tab, cohort, iSimulFiles, iDemoCareFiles)
  cTabFertNuptDF <- rbind(cTabFertNuptDF, cTabFertNupt)

  cTab    <- careSupply(tab, cohort, iSimulFiles, iDemoCareFiles)
  cTabRel <- computeCareSupply(cTab)
  careSupplyDF <- rbind(careSupplyDF, cTabRel)

  cTabBal    <- balanceFormalInformal(tab, cohort, iSimulFiles, iDemoCareFiles)
  cTabRelBal <- computeBalance(cTabBal)
  informalCareDF <- rbind(informalCareDF, cTabRelBal)
}


# ==== 4. Save, using the exact names democare_graphs.R expects ====

save(cTabFertNuptDF, file = paste0(pathOut, "FertNupt.RData"))
message(paste0(pathOut, "FertNupt.RData saved (", nrow(cTabFertNuptDF), " rows, ",
              length(unique(cTabFertNuptDF$cohort)), " cohorts)"))

save(careSupplyDF, file = paste0(pathOut, "careSupply.RData"))
message(paste0(pathOut, "careSupply.RData saved (", nrow(careSupplyDF), " rows, ",
              length(unique(careSupplyDF$cohort)), " cohorts)"))

save(informalCareDF, file = paste0(pathOut, "formal_informal.RData"))
message(paste0(pathOut, "formal_informal.RData saved (", nrow(informalCareDF), " rows, ",
              length(unique(informalCareDF$cohort)), " cohorts)"))

# democare_graphs.R can now load these three files as usual (see its own
# "load (...)" calls near the top) and the birthCohort-faceted plots further
# down will show every cohort aggregated here.
