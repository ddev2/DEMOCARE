########################################################################
# computeCare_baseR_bootstrap.R
#
# Runs computeCare_baseR.R's run_computeCare() over many bootstrap
# replicates -- resampling whole FAMILY NETWORKS (see
# bootstrap_resample.R) before each run, not resampling rows out of an
# already-computed results file. See the "Bootstrapping" section of
# Democare 2026.docx for why that distinction matters here.
#
# WHAT THIS SCRIPT DOES NOT DO: touch a single line of
# computeCare_baseR.R's simulation logic. It only (a) resamples the
# >>> Claude 2026-09-29: tidyverse engine removed (base R only)
# INPUT family-network files (bootstrap_resample.R, also used by
# app.R) and (b)
# <<< Claude 2026-09-29
# loops run_computeCare() -- already a
# clean, file-in/data-frame-out function, unchanged from Section 9 of
# computeCare_baseR.R -- over the resulting replicate input files.
#
# OUTPUT NAMING: each replicate is written as
#   <OUTPUT_DIR>/<ROOT_NAME>_<iSimul>_<iBootstrap>.txt
# -- the exact "<root>_<iSimul>_<iDemoCare>.txt" convention Modules/
# R PAPER/R code/democare_compute.R's createCareSupplyDF()/
# computeInformalCare() already read (nSimulFiles/nDemoCareFiles there =
# N_SIMUL/N_BOOTSTRAP here). Point pathData at OUTPUT_DIR/ and
# nDemRegFileNames at ROOT_NAME and those functions work unchanged.
########################################################################

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load computeCare_baseR.R's functions (read_population(), read_links(),
# run_computeCare(), and the PATH_* constants) WITHOUT triggering its own
# Section 10 demo run -- see that file's Section 10 comment.
COMPUTECARE_NO_AUTORUN <- TRUE
source("computeCare_baseR.R")
source("bootstrap_resample.R")


# ==== Configuration -- edit these for your own run ====

# One (population, links) pair per iSimul -- i.e. per distinct KINFERT
# family-network draw you have (NetLogo's bootstrapList calls this
# dimension "nSimul": e.g. KIN1908_EDU1_VAR_1, _2, _3...). For a single
# base file (the sample data this repo ships with, or a single real
# KINFERT run), just the one entry below is fine -- add more named
# (population_path, links_path) pairs to POPULATION_PATHS/LINKS_PATHS to
# loop over several.
POPULATION_PATHS <- list(PATH_POPULATION)
LINKS_PATHS      <- list(PATH_LINKS)

ROOT_NAME    <- "computeCare_baseR"  # file-name root; becomes nDemRegFileNames in democare_compute.R
N_BOOTSTRAP  <- 20                    # family-resampling replicates PER input file (nDemoCareFiles there)
OUTPUT_DIR   <- "bootstrap_output"
BASE_SEED    <- 1000                  # every replicate's seed is derived from this, see below
MAX_TICKS    <- 90                    # passed straight to run_computeCare(); lower this for a quick smoke test

N_SIMUL <- length(POPULATION_PATHS)
dir.create(OUTPUT_DIR, showWarnings = FALSE)


# ==== Run the grid ====

for (iSimul in seq_len(N_SIMUL)) {

  message(sprintf("==== iSimul %d/%d: reading base family-network files ====", iSimul, N_SIMUL))
  base_population <- read_population(POPULATION_PATHS[[iSimul]])
  base_links      <- read_links(LINKS_PATHS[[iSimul]])

  for (iBootstrap in seq_len(N_BOOTSTRAP)) {

    # Deterministic-but-distinct seed per (iSimul, iBootstrap) cell, so
    # the whole grid is exactly reproducible from BASE_SEED alone, and no
    # two cells (even across different iSimul) ever draw the same
    # sequence. Room for up to 9999 iBootstrap replicates per iSimul
    # before this scheme would need widening.
    replicate_seed <- BASE_SEED + iSimul * 10000L + iBootstrap

    out_path <- file.path(OUTPUT_DIR, sprintf("%s_%d_%d.txt", ROOT_NAME, iSimul, iBootstrap))

    message(sprintf("---- iSimul %d / iBootstrap %d (seed %d) ----", iSimul, iBootstrap, replicate_seed))

    resampled <- bootstrap_resample_families(base_population, base_links, seed = replicate_seed)

    pop_path   <- file.path(OUTPUT_DIR, sprintf("%s_%d_%d_population.csv", ROOT_NAME, iSimul, iBootstrap))
    links_path <- file.path(OUTPUT_DIR, sprintf("%s_%d_%d_links.csv",      ROOT_NAME, iSimul, iBootstrap))
    write_resampled_files(resampled, pop_path, links_path)

    out <- run_computeCare(
      population_path  = pop_path,
      links_path       = links_path,
      dependency_path  = PATH_DEPENDENCY,
      dep_trans_path   = PATH_DEP_TRANS,
      activity_path    = PATH_ACTIVITY,
      act_trans_path   = PATH_ACT_TRANS,
      care_hours_path  = PATH_CARE_HOURS,
      seed             = replicate_seed,
      max_ticks        = MAX_TICKS
    )

    write.table(out, out_path, sep = "\t", row.names = FALSE, quote = FALSE)
    message(sprintf("  -> %s (%d ego-year rows, %d families drawn)",
                     out_path, nrow(out), length(unique(resampled$population$idFamily))))
  }
}

message(sprintf(
  "\nBootstrap run finished: %d x %d = %d replicate files written to %s/.\n",
  N_SIMUL, N_BOOTSTRAP, N_SIMUL * N_BOOTSTRAP, OUTPUT_DIR))
message(sprintf(
  "Feed them to createCareSupplyDF()/computeInformalCare() in Modules/R PAPER/\nR code/democare_compute.R with: nSimulFiles = %d, nDemoCareFiles = %d,\npathData = \"%s/\", nDemRegFileNames = \"%s\", zipped = FALSE.",
  N_SIMUL, N_BOOTSTRAP, OUTPUT_DIR, ROOT_NAME))
