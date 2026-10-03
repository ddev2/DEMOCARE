########################################################################
# app.R -- computeCare menu (Shiny)
#
# A point-and-click front end for the computeCare functions in this
# >>> Claude 2026-09-29: tidyverse engine removed (base R only)
# folder (computeCare_baseR.R / bootstrap_resample.R). The tidyverse
# engine was dropped in September 2026; only base R is kept.
# <<< Claude 2026-09-29 Meant to be opened and run from RStudio -- no server
# needed: click "Run App" (or source(); shiny::runApp()) with this file
# as the active tab, and RStudio opens it in the Viewer pane / a
# separate window.
#
# WHAT THIS APP DOES NOT DO: touch a single line of the three sourced
# scripts' own logic. It only sources them (see Section 2 for why the
# usual `setwd(dirname(rstudioapi::getActiveDocumentContext()$path))`
# line at the top of each is neutralised here) and wires their existing
# functions to a UI.
#
# TWO PANELS
# >>> Claude 2026-09-29
#   "Run a simulation"   -- one call to run_computeCare() (base R
#                            engine), with a results preview,
# <<< Claude 2026-09-29
#                            a quick plot, and a save-to-file button.
# >>> Claude 2026-09-29
#   "Bootstrap replicates" -- the same loop computeCare_baseR_bootstrap.R
#                            already does
# <<< Claude 2026-09-29 (resample families -> run_computeCare()
#                            -> write one file per replicate), just
#                            driven from the UI instead of editing the
#                            CONFIGURATION block of those scripts by
#                            hand. Single (population, links) pair only
#                            (N_SIMUL = 1 in those scripts' terms) --
#                            for a multi-file grid, use the wrapper
#                            scripts directly.
########################################################################

library(shiny)

# DT gives a nicer results table (sortable/searchable); fall back to a
# plain table if it isn't installed so the app still runs either way.
HAS_DT <- requireNamespace("DT", quietly = TRUE)
if (HAS_DT) library(DT)


# ==== 1. Locate this app's own folder ====

# Shiny sets the working directory to the app's own folder for the
# duration of the app (both runApp() and RStudio's "Run App" button do
# this), so capturing getwd() here -- before anything below can change
# it -- reliably gives Modules/computeCare_R/, regardless of which
# document happens to be focused in the editor. This sidesteps the
# `rstudioapi::getActiveDocumentContext()`-based setwd() at the top of
# >>> Claude 2026-09-29
# computeCare_baseR.R, which assumes ITS OWN
# <<< Claude 2026-09-29
# script is the active editor tab -- not true once it's being source()d
# from inside this app. Section 2 below re-derives every input path as
# an ABSOLUTE path from APP_DIR, so wherever that internal setwd() ends
# up leaving the working directory afterwards no longer matters.
APP_DIR <- normalizePath(getwd())

KINFERT_DIR <- normalizePath(file.path(APP_DIR, "..", "KINFERT", "output"))
NETLOGO_DIR <- normalizePath(file.path(APP_DIR, "..", "NETLOGO", "inputs"))

# "Population_indKin.txt" is the NetLogo-standard population file name
# (matches what PopulationImporter.nls/PopulationCreator.nls read/write
# -- see Modules/NETLOGO/src/) -- Daniel confirmed this, not the
# >>> Claude 2026-09-29
# "Population 0.1.txt" name computeCare_baseR.R happens to default to
# <<< Claude 2026-09-29
# internally, which this app never actually reads
# (see Section 3's derive_links_path()).
DEFAULT_PATHS <- list(
  population = file.path(KINFERT_DIR, "Population_indKin.txt")
)

# The Dependency/Activity/Care-hours family of input files always lives
# together in one folder and never changes between "Run a simulation"
# and "Bootstrap replicates" -- Daniel's request -- so it's ONE shared
# setting (a folder, not five separate file paths) with fixed file names
# inside it, rather than a per-tab, per-file set of text inputs.
INPUT_FILE_NAMES <- c(
  dependency = "dependency 0.1.csv",
  dep_trans  = "dependency_transitions 0.1.csv",
  activity   = "activity 0.3.csv",
  act_trans  = "act_transitions.csv",
  care_hours = "care 0.1.csv"
)

OUTPUT_DIR <- file.path(APP_DIR, "app_output")
dir.create(OUTPUT_DIR, showWarnings = FALSE)

# ---- Persisted "standard" input files folder ----
#
# Changing the shared input-files folder via the "Change folder..."
# button becomes the new standard for next time too, per Daniel's
# request -- so it's saved to a plain-text settings file next to app.R
# (one "key=value" line; readable/editable by hand if ever needed) and
# reloaded on the next launch, instead of resetting to NETLOGO_DIR every
# time.
SETTINGS_PATH <- file.path(APP_DIR, "app_settings.txt")

load_app_settings <- function() {
  settings <- list(input_dir = NETLOGO_DIR)
  if (file.exists(SETTINGS_PATH)) {
    for (line in readLines(SETTINGS_PATH, warn = FALSE)) {
      parts <- strsplit(line, "=", fixed = TRUE)[[1]]
      if (length(parts) >= 2) {
        key <- trimws(parts[1])
        val <- trimws(paste(parts[-1], collapse = "="))
        if (identical(key, "input_dir") && nzchar(val) && dir.exists(val)) {
          settings$input_dir <- val
        }
      }
    }
  }
  settings
}

save_app_settings <- function(settings) {
  writeLines(sprintf("input_dir=%s", settings$input_dir), SETTINGS_PATH)
}


# ==== 2. Source the two engines + the bootstrap helpers ====

# Each engine defines the SAME function names (run_computeCare(),
# read_population(), ...), so each is sourced into its own environment
# to keep them from clobbering one another. COMPUTECARE_NO_AUTORUN
# suppresses each script's own "Section 10" demo run (see that section
# >>> Claude 2026-09-29
# in that script) -- same flag computeCare_baseR_bootstrap.R already
# sets for the same reason. Only the base R engine is kept; the list
# structure below is left in place so run_engine() needs no change.
# <<< Claude 2026-09-29
#
# THIS ONLY WORKS IF ENGINE_ENVS BELOW CAN SEE IT: Shiny does not source
# app.R into .GlobalEnv -- it builds its own dedicated environment for
# the app (so that re-running or running several apps never collide)
# and evaluates this whole script there instead. So COMPUTECARE_NO_AUTORUN
# actually lives in THAT environment, not .GlobalEnv, and each
# ENGINE_ENVS entry has to chain up to it via `environment()` (this
# script's own current environment, whatever it turns out to be), not
# via a hard-coded `globalenv()` -- see the note below.
COMPUTECARE_NO_AUTORUN <- TRUE

# Both engine scripts start with
#   setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# -- meant for when Daniel has THAT script itself open and clicks
# Source, not for being source()d from inside this app, where whatever
# document happens to be focused in RStudio (this app.R, nothing, the
# console...) is irrelevant -- and, if no document is focused, that
# path comes back empty and the setwd() call errors outright ("cannot
# change working directory"). Rather than editing either script, this
# app temporarily points rstudioapi::getActiveDocumentContext() at
# APP_DIR for the duration of sourcing -- a harmless no-op setwd(),
# since APP_DIR is already the correct folder -- then restores the real
# function immediately after, so nothing else in the R session is
# affected.
with_stubbed_rstudioapi <- function(expr) {
  ns <- asNamespace("rstudioapi")
  real_fun <- get("getActiveDocumentContext", envir = ns)
  was_locked <- bindingIsLocked("getActiveDocumentContext", ns)
  stub_fun <- function(...) list(id = "", path = file.path(APP_DIR, "app.R"))

  if (was_locked) unlockBinding("getActiveDocumentContext", ns)
  assign("getActiveDocumentContext", stub_fun, envir = ns)
  on.exit({
    if (was_locked) unlockBinding("getActiveDocumentContext", ns)
    assign("getActiveDocumentContext", real_fun, envir = ns)
    if (was_locked) lockBinding("getActiveDocumentContext", ns)
  }, add = TRUE)

  force(expr)
}

# `parent = environment()`, NOT `parent = globalenv()`: environment()
# with no arguments, called here at this script's own top level, means
# "wherever THIS code is actually running" -- .GlobalEnv when app.R is
# merely source()d (e.g. in a test), but Shiny's private app environment
# when launched for real via "Run App"/runApp(). Either way, that is
# where COMPUTECARE_NO_AUTORUN was just assigned, so each engine's own
# `exists("COMPUTECARE_NO_AUTORUN")` (which searches upward from wherever
# it's sourced) reliably finds it and Section 10's demo run stays
# suppressed. `parent = globalenv()` (the earlier version of this file)
# looked right in a plain source()-in-a-fresh-session test, but is a
# SIBLING of Shiny's app environment, not its ancestor -- so under a real
# "Run App" launch the lookup missed COMPUTECARE_NO_AUTORUN entirely and
# both engines silently ran their full Section 10 demo (default
# max_ticks = 90) the moment the app started, before any button was
# clicked. Confirmed both ways: reproduced via shiny::runApp() on this
# exact app.R, then re-confirmed fixed with the same launch method.
# >>> Claude 2026-09-29: tidyverse engine removed (base R only)
ENGINE_ENVS <- list(
  baseR = new.env(parent = environment())
)
with_stubbed_rstudioapi({
  source(file.path(APP_DIR, "computeCare_baseR.R"), local = ENGINE_ENVS$baseR)
})
# <<< Claude 2026-09-29

# bootstrap_resample.R is engine-agnostic (no setwd(), no library()
# calls, and no COMPUTECARE_NO_AUTORUN-gated autorun -- see its own
# header) so the parent-environment choice doesn't matter for it, but
# `environment()` is used here too for consistency.
BOOT_ENV <- new.env(parent = environment())
source(file.path(APP_DIR, "bootstrap_resample.R"), local = BOOT_ENV)

# democare_compute.R (Modules/R PAPER/R code/) turns DEMOCARE output
# files (from either tab above) into the three .RData files
# democare_graphs.R expects -- see "Analysing results and doing plots"
# in Democare 2026.docx. It has no autorun step and only one top-level
# side effect (attaching the "reshape2" package via its own library2()
# helper, installing it first if missing), so it's sourced the same way
# as bootstrap_resample.R above.
DEMO_ENV <- new.env(parent = environment())
source(file.path(APP_DIR, "..", "R PAPER", "R code", "democare_compute.R"), local = DEMO_ENV)

# >>> Claude 2026-09-29: tidyverse engine removed (base R only)
ENGINE <- "baseR"
# <<< Claude 2026-09-29


# ==== 3. Shared helpers ====

# Runs run_computeCare() for the chosen engine, forwarding message()
# output ("Tick %d: ...") into a Shiny progress bar instead of the
# console, so a long run still gives visible feedback in the UI.
run_engine <- function(engine, paths, seed, max_ticks, progress = NULL) {
  eng <- ENGINE_ENVS[[engine]]

  run_it <- function() {
    eng$run_computeCare(
      population_path = paths$population,
      links_path      = paths$links,
      dependency_path = paths$dependency,
      dep_trans_path  = paths$dep_trans,
      activity_path   = paths$activity,
      act_trans_path  = paths$act_trans,
      care_hours_path = paths$care_hours,
      seed            = seed,
      max_ticks       = max_ticks
    )
  }

  if (is.null(progress)) return(run_it())

  withCallingHandlers(
    run_it(),
    message = function(m) {
      tick <- regmatches(conditionMessage(m), regexpr("(?<=Tick )[0-9]+", conditionMessage(m), perl = TRUE))
      if (length(tick) == 1) {
        progress$set(value = as.numeric(tick) / max_ticks, detail = trimws(conditionMessage(m)))
      }
      invokeRestart("muffleMessage")
    }
  )
}

# One bootstrap replicate: resample families, write the replicate's
# population/links files, run computeCare on them. Mirrors the loop
# >>> Claude 2026-09-29
# body in computeCare_baseR_bootstrap.R for a single iSimul.
# <<< Claude 2026-09-29
run_bootstrap_replicate <- function(engine, base_population, base_links, paths,
                                     root_name, i_bootstrap, seed, max_ticks, output_dir) {
  eng <- ENGINE_ENVS[[engine]]

  resampled <- BOOT_ENV$bootstrap_resample_families(base_population, base_links, seed = seed)

  # "<root>_<i>.csv" / "<root>_<i>_link.csv" -- same "_link"-before-
  # extension convention as derive_links_path() above, so this app's own
  # bootstrap output follows the identical rule as real KINFERT files.
  # Purely internal/transient (written here, read right back below by
  # run_computeCare()) -- nothing else reads these by name.
  pop_path   <- file.path(output_dir, sprintf("%s_%d.csv",      root_name, i_bootstrap))
  links_path <- file.path(output_dir, sprintf("%s_%d_link.csv", root_name, i_bootstrap))
  BOOT_ENV$write_resampled_files(resampled, pop_path, links_path)

  out <- eng$run_computeCare(
    population_path = pop_path,
    links_path      = links_path,
    dependency_path = paths$dependency,
    dep_trans_path  = paths$dep_trans,
    activity_path   = paths$activity,
    act_trans_path  = paths$act_trans,
    care_hours_path = paths$care_hours,
    seed            = seed,
    max_ticks       = max_ticks
  )

  # "<root>_1_<i>.txt" -- the "_1_" is a fixed iSimul index (this app only
  # ever resamples ONE population/links pair, i.e. N_SIMUL = 1 in
  # computeCare_baseR_bootstrap.R's terms), kept in the name so the output
  # matches the "<nf>_<iSimulFiles>_<iDemoCareFiles>.txt" pattern
  # createCareSupplyDF()/computeInformalCare() (Modules/R PAPER/R code/
  # democare_compute.R) already expect -- see the Word documentation's
  # "Analysing results and doing plots" section for a worked example.
  out_path <- file.path(output_dir, sprintf("%s_1_%d.txt", root_name, i_bootstrap))
  write.table(out, out_path, sep = "\t", row.names = FALSE, quote = FALSE)

  list(out_path = out_path, n_rows = nrow(out),
       n_families = length(unique(resampled$population$idFamily)))
}

# The links file's name is always the population file's own name with
# "_link" inserted right before the extension -- the actual NetLogo
# convention (Daniel confirmed): "Population_indKin.txt" pairs with
# "Population_indKin_link.txt" (same folder, same extension, read/
# written by PopulationImporter.nls / PopulationCreator.nls in
# Modules/NETLOGO/src/). run_bootstrap_replicate() below writes its own
# replicate files the same way ("<root>_<i>.csv" / "<root>_<i>_link.csv")
# so ONE rule covers both real KINFERT output and this app's own
# bootstrap output. So there is no separate "Links" input at all: given
# the population path, this derives the links path Daniel's UI no
# longer asks for.
#
# Also tries the OLDER "Population 0.1.txt" / "links 0.1.txt"-style
# naming (swap the word "population" for "links") as a fallback, in
# case a population file following that convention is ever pointed at
# instead -- but only when a file with THAT name actually exists on
# disk; the NetLogo "_link" convention is always tried first and always
# preferred. Returns NA only when the population file name doesn't
# support either pattern (no extension AND no "population" substring).
derive_links_path <- function(population_path) {
  if (is.null(population_path) || !nzchar(population_path)) return(NA_character_)

  candidates <- character(0)

  ext <- tools::file_ext(population_path)
  if (nzchar(ext)) {
    candidates <- c(candidates, paste0(tools::file_path_sans_ext(population_path), "_link.", ext))
  }

  base <- basename(population_path)
  if (grepl("population", base, ignore.case = TRUE)) {
    candidates <- c(candidates,
                     file.path(dirname(population_path), sub("population", "links", base, ignore.case = TRUE)))
  }

  if (length(candidates) == 0) return(NA_character_)
  found <- candidates[file.exists(candidates)]
  if (length(found) > 0) found[1] else candidates[1]
}

# >>> Claude 2026-10-02: zipped KINFERT files
# ---- Zipped KINFERT population / links files ----
#
# KINFERT files now usually arrive zipped. Any of these layouts is
# accepted, for the population file the user picks:
#   (a) X_indKin.zip       containing X_indKin.txt AND X_indKin_link.txt
#   (b) X_indKin.zip       + a separate X_indKin_link.zip next to it
#       (also X_indKin.txt.zip + X_indKin_link.txt.zip)
#   (c) X_indKin.zip       + a plain X_indKin_link.txt next to it
#   (d) X_indKin.txt       + a zipped X_indKin_link.zip / .txt.zip
#   (e) X_indKin.txt       + X_indKin_link.txt (plain, as before)
#
# "Unzip, then zip again afterwards": the files are never unzipped in
# place. Each needed entry is extracted into a fresh temporary folder
# (tempfile(), outside the project), the simulation reads it from there,
# and the temporary folder is deleted as soon as the run (or the whole
# bootstrap) is over, even if it fails. The .zip files themselves are
# only read, never rewritten, so the project folder ends up exactly as
# it was: zipped. Only base R's utils::unzip() is used (no package, no
# external "zip" program needed).

is_zip_path <- function(p) length(p) == 1 && !is.na(p) && grepl("\\.zip$", p, ignore.case = TRUE)

# "X_indKin.txt" -> "X_indKin_link.txt"; "X_indKin" -> "X_indKin_link".
insert_link_suffix <- function(p) {
  ext <- tools::file_ext(p)
  if (nzchar(ext)) paste0(tools::file_path_sans_ext(p), "_link.", ext) else paste0(p, "_link")
}

# Real data entries of a zip, i.e. without the "__MACOSX/" and
# ".DS_Store" entries that the macOS Finder "Compress" command adds.
zip_data_entries <- function(zip_path) {
  e <- utils::unzip(zip_path, list = TRUE)$Name
  e[!grepl("(^|/)__MACOSX/|(^|/)\\.DS_Store$|/$", e)]
}

# Which entry of a zip is the population file / the links file. A links
# entry is recognised by "_link" right before its extension (or at the
# end of its name). Returns NA when there is no unique candidate.
pick_zip_entry <- function(zip_path, want = c("population", "links")) {
  want <- match.arg(want)
  e <- zip_data_entries(zip_path)
  is_link <- grepl("_link(\\.[^./]+)?$", basename(e), ignore.case = TRUE)
  hits <- if (want == "links") e[is_link] else e[!is_link]
  # A zip holding a single file is taken at face value, whatever its name
  # (e.g. a links zip whose only entry is not "_link"-named).
  if (length(hits) == 0 && length(e) == 1 && want == "links" &&
      grepl("_link", basename(zip_path), ignore.case = TRUE)) hits <- e
  if (length(hits) > 1) {
    pref <- hits[grepl("_indKin", basename(hits), ignore.case = TRUE)]
    if (length(pref) == 1) hits <- pref
  }
  if (length(hits) == 1) hits else NA_character_
}

# Where the links file is, for a given population file (plain or zipped).
# Returns list(path, entry, found):
#   path   the .txt file or the .zip that holds the links file
#   entry  the name of the links file inside that zip (NULL if not a zip)
#   found  FALSE when nothing exists on disk; `path` is then the best guess
# Returns NULL only when the population path is empty.
locate_links_source <- function(population_path) {
  if (is.null(population_path) || !nzchar(population_path)) return(NULL)

  safe_pick <- function(zip, want) tryCatch(pick_zip_entry(zip, want), error = function(e) NA_character_)

  if (is_zip_path(population_path)) {
    # (a) the links file sits in the same zip as the population file
    if (file.exists(population_path)) {
      entry <- safe_pick(population_path, "links")
      if (!is.na(entry)) return(list(path = population_path, entry = entry, found = TRUE))
    }
    stem <- sub("\\.zip$", "", population_path, ignore.case = TRUE)  # "X_indKin" or "X_indKin.txt"
    link_stem <- insert_link_suffix(stem)                           # "X_indKin_link" or "X_indKin_link.txt"
    candidates <- c(paste0(link_stem, ".zip"),                      # (b)
                    if (nzchar(tools::file_ext(stem))) link_stem else paste0(link_stem, ".txt"))  # (c)
  } else {
    txt <- derive_links_path(population_path)                       # (e), also the old naming
    if (is.na(txt)) return(list(path = NA_character_, entry = NULL, found = FALSE))
    candidates <- c(txt,
                    paste0(txt, ".zip"),                            # (d) X_indKin_link.txt.zip
                    paste0(tools::file_path_sans_ext(txt), ".zip")) # (d) X_indKin_link.zip
  }

  for (cand in candidates[file.exists(candidates)]) {
    if (!is_zip_path(cand)) return(list(path = cand, entry = NULL, found = TRUE))
    entry <- safe_pick(cand, "links")
    if (!is.na(entry)) return(list(path = cand, entry = entry, found = TRUE))
  }
  list(path = candidates[1], entry = NULL, found = FALSE)
}

# Make a (population, links) pair readable by run_computeCare(): plain
# files are passed through unchanged; zipped ones are extracted into a
# temporary folder. ALWAYS pair with release_kinfert_pair() (via
# on.exit()) so the temporary copies are deleted afterwards.
prepare_kinfert_pair <- function(population_path) {
  if (is.null(population_path) || !nzchar(population_path) || !file.exists(population_path)) {
    stop("Population file not found: ", population_path, call. = FALSE)
  }
  links <- locate_links_source(population_path)
  if (is.null(links) || !isTRUE(links$found)) {
    stop("Links file not found (looked for: ", links$path, ").", call. = FALSE)
  }
  if (!is_zip_path(population_path) && !is_zip_path(links$path)) {
    return(list(population = population_path, links = links$path, tmp_dir = NULL))
  }

  tmp_dir <- tempfile("kinfert_unzipped_")
  dir.create(tmp_dir)
  extract_one <- function(zip_path, entry) {
    utils::unzip(zip_path, files = entry, exdir = tmp_dir, junkpaths = TRUE)
    out <- file.path(tmp_dir, basename(entry))
    if (!file.exists(out)) stop("Could not extract ", entry, " from ", zip_path, call. = FALSE)
    out
  }

  pop <- population_path
  if (is_zip_path(population_path)) {
    entry <- pick_zip_entry(population_path, "population")
    if (is.na(entry)) {
      unlink(tmp_dir, recursive = TRUE)
      stop("Cannot tell which file in ", basename(population_path), " is the population file. ",
           "It contains: ", paste(zip_data_entries(population_path), collapse = ", "), call. = FALSE)
    }
    pop <- extract_one(population_path, entry)
  }
  lnk <- if (is_zip_path(links$path)) extract_one(links$path, links$entry) else links$path

  list(population = pop, links = lnk, tmp_dir = tmp_dir)
}

release_kinfert_pair <- function(pair) {
  if (!is.null(pair) && !is.null(pair$tmp_dir)) unlink(pair$tmp_dir, recursive = TRUE)
  invisible(NULL)
}
# <<< Claude 2026-10-02

# Given the shared input-files folder, the fixed file names inside it.
resolve_input_files <- function(dir) {
  as.list(setNames(file.path(dir, INPUT_FILE_NAMES), names(INPUT_FILE_NAMES)))
}

missing_input_files <- function(dir) {
  paths <- resolve_input_files(dir)
  names(paths)[!vapply(paths, file.exists, logical(1))]
}

# ---- Analyse results: reading DEMOCARE output back into .RData files ----
#
# democare_compute.R's own functions (see Section 2 above) are used
# exactly as they are, gotchas and all -- this app works around them
# rather than changing their logic. Two gotchas matter here (also
# documented in Democare 2026.docx's "Analysing results and doing plots"
# section):
#   1. createCareSupplyDF() reads `path`, `pathData` and `zipped` as free
#      variables, not as arguments -- and because they're free variables,
#      R looks them up starting from the environment createCareSupplyDF()
#      was DEFINED in (DEMO_ENV), not from wherever it's called. So they
#      have to be assign()ed directly into DEMO_ENV, not just set as
#      ordinary local variables in the server code below.
#   2. Both createCareSupplyDF() and computeInformalCare() always call
#      save(..., file="careSupply.RData") etc. with no path prefix, so
#      the .RData files land in R's current working directory no matter
#      what pathOut is set to. run_aggregate_batch()/run_aggregate_single()
#      below setwd() to the chosen output folder for the duration of the
#      call (restored on exit, even if the call errors) rather than
#      relying on pathOut for that.

# Recognises "<root>_<iSimul>_<iDemoCare>.txt" files -- the naming both
# this app's "Bootstrap replicates" tab and the standalone bootstrap
# wrapper scripts use (see "Bootstrapping" in Democare 2026.docx) -- and
# reports, per root name, the iSimul/iDemoCare range found on disk and
# whether every file in that range actually exists. A large batch (a
# couple of hundred replicates isn't unusual) is impractical to check by
# eye in Finder/Explorer, and a missing file in the middle of the range
# is exactly what makes createCareSupplyDF()/computeInformalCare() stop
# partway through with a bare "cannot open file" error and no indication
# of which one -- this surfaces that before the batch is even started.
summarise_bootstrap_roots <- function(dir) {
  files <- list.files(dir, pattern = "^.+_[0-9]+_[0-9]+\\.txt$")
  if (length(files) == 0) return(data.frame())

  m <- regmatches(files, regexec("^(.+)_([0-9]+)_([0-9]+)\\.txt$", files))
  parsed <- do.call(rbind, lapply(m, function(x) {
    if (length(x) != 4) return(NULL)
    data.frame(root = x[2], iSimul = as.integer(x[3]), iDemoCare = as.integer(x[4]))
  }))
  if (is.null(parsed) || nrow(parsed) == 0) return(data.frame())

  do.call(rbind, lapply(split(parsed, parsed$root), function(g) {
    n_expected <- (max(g$iSimul) - min(g$iSimul) + 1) * (max(g$iDemoCare) - min(g$iDemoCare) + 1)
    data.frame(
      root = g$root[1],
      debSimul = min(g$iSimul), nSimulFiles = max(g$iSimul),
      debDemoCare = min(g$iDemoCare), nDemoCareFiles = max(g$iDemoCare),
      n_files = nrow(g), n_expected = n_expected,
      complete = nrow(g) == n_expected
    )
  }))
}

# Batch path: createCareSupplyDF() (-> careSupply.RData + FertNupt.RData)
# then computeInformalCare() (-> formal_informal.RData) over one or more
# root names. `progress`/`n_expected` hook the same message()-driven
# progress-bar pattern run_engine() uses above -- both functions call
# message() once per file read, plus once per (root, iSimul) when they
# save, so n_expected is intentionally an upper bound, not exact; the
# progress bar is capped at 1 to stay sane about that.
run_aggregate_batch <- function(input_dir, output_dir, roots,
                                 debSimul, nSimulFiles, debDemoCare, nDemoCareFiles,
                                 progress = NULL, n_expected = 1) {
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(output_dir)

  input_dir_slash <- paste0(input_dir, DEMO_ENV$getDirSep())
  assign("pathData", input_dir_slash, envir = DEMO_ENV)
  assign("path",     input_dir_slash, envir = DEMO_ENV)
  assign("zipped",   FALSE,           envir = DEMO_ENV)

  n_done <- 0
  track_progress <- function(m) {
    n_done <<- n_done + 1
    if (!is.null(progress)) {
      progress$set(value = min(n_done / n_expected, 1), detail = trimws(conditionMessage(m)))
    }
    invokeRestart("muffleMessage")
  }

  withCallingHandlers(
    DEMO_ENV$createCareSupplyDF(
      nDemRegFileNames = roots,
      nSimulFiles = nSimulFiles, debSimul = debSimul,
      nDemoCareFiles = nDemoCareFiles, debDemoCare = debDemoCare
    ),
    message = track_progress
  )
  withCallingHandlers(
    # zipped = FALSE, so computeInformalCare() never touches `path` --
    # pathOut is the one that matters here, and (per gotcha 2 above) it
    # plays the role of the INPUT folder for this function specifically,
    # not an output destination.
    DEMO_ENV$computeInformalCare(
      nDemRegFileNames = roots,
      nSimulFiles = nSimulFiles, debSimul = debSimul,
      nDemoCareFiles = nDemoCareFiles, debDemoCare = debDemoCare,
      zipped = FALSE, path = NULL, pathOut = input_dir_slash
    ),
    message = track_progress
  )

  invisible(NULL)
}

# Single-run path: createDemoCareFiles() genuinely honours pathOut (see
# the docx), so the setwd() trick isn't strictly needed for it, but it's
# used anyway for symmetry with run_aggregate_batch() above.
run_aggregate_single <- function(file_path, output_dir, cohort) {
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(output_dir)

  DEMO_ENV$createDemoCareFiles(
    filePath = file_path,
    pathOut  = paste0(output_dir, DEMO_ENV$getDirSep()),
    cohort   = cohort
  )
}

# Reads the three .RData files back in (into a throwaway environment, so
# this never collides with anything else) for the results preview -- NULL
# for any that don't exist yet, e.g. a fresh output folder.
load_aggregate_preview <- function(output_dir) {
  read_one <- function(file, objname) {
    path <- file.path(output_dir, file)
    if (!file.exists(path)) return(NULL)
    e <- new.env()
    load(path, envir = e)
    if (!exists(objname, envir = e)) return(NULL)
    get(objname, envir = e)
  }
  list(
    careSupply     = read_one("careSupply.RData",       "careSupplyDF"),
    fertNupt       = read_one("FertNupt.RData",          "cTabFertNuptDF"),
    formalInformal = read_one("formal_informal.RData",   "informalCareDF")
  )
}

# Native OS folder/file dialogs -- only available from inside a real
# RStudio session (exactly the environment Daniel runs this app in).
# isAvailable() is the one rstudioapi function that is always safe to
# call and never errors outside RStudio (unlike getActiveDocumentContext()
# -- see the Section 2 note above), so it is the right guard here.
browse_for_folder <- function(current, caption = "Select the input files folder") {
  if (!requireNamespace("rstudioapi", quietly = TRUE) || !rstudioapi::isAvailable()) {
    showNotification("Folder browsing needs RStudio -- edit the path directly instead.", type = "warning")
    return(NULL)
  }
  tryCatch(
    rstudioapi::selectDirectory(caption = caption, path = current),
    error = function(e) NULL  # user cancelled, or the dialog itself failed
  )
}

browse_for_population_file <- function(current, caption = "Select the population file") {
  if (!requireNamespace("rstudioapi", quietly = TRUE) || !rstudioapi::isAvailable()) {
    showNotification("File browsing needs RStudio -- edit the path directly instead.", type = "warning")
    return(NULL)
  }
  tryCatch(
    rstudioapi::selectFile(caption = caption, path = dirname(current), existing = TRUE),
    error = function(e) NULL
  )
}


# ==== 4. UI ====

ui <- navbarPage(
  title = "computeCare",

  # A common header (shown above every tab, not just one of them -- see
  # navbarPage()'s `header` argument) for the ONE setting "Run a
  # simulation" and "Bootstrap replicates" both need and never disagree
  # on: the folder holding the Dependency/Activity/Care-hours family of
  # input files. Changing it here changes it for both tabs at once.
  header = tags$div(
    style = "padding: 8px 15px 4px; background-color: #f7f7f7; border-bottom: 1px solid #ddd; margin-bottom: 8px;",
    fluidRow(
      column(7,
        tags$strong("Input files folder"),
        tags$span(" (Dependency / Activity / Care hours -- shared by both tabs): "),
        tags$code(textOutput("input_dir_display", inline = TRUE))
      ),
      column(2, actionButton("browse_input_dir", "Change folder...", width = "100%")),
      column(3, uiOutput("input_dir_status"))
    )
  ),

  # ---- Panel 1: single simulation run ----
  tabPanel(
    "Run a simulation",
    sidebarLayout(
      sidebarPanel(
        width = 4,
        # >>> Claude 2026-09-29: engine picker removed (base R only)
        # <<< Claude 2026-09-29
        numericInput("seed", "Seed", value = 1, min = 1, step = 1),
        numericInput("max_ticks", "Max ticks", value = 90, min = 1, step = 1),
        tags$hr(),
        tags$strong("Population file"),
        fluidRow(
          column(9, textInput("population_path", NULL, value = DEFAULT_PATHS$population, width = "100%")),
          column(3, actionButton("population_path_browse", "Browse..."))
        ),
        tags$p(class = "text-muted", "Links file (auto-detected from the population file's name):"),
        uiOutput("links_path_display"),
        tags$hr(),
        actionButton("run_sim", "Run simulation", class = "btn-primary", width = "100%")
      ),
      mainPanel(
        width = 8,
        uiOutput("sim_status"),
        tags$h4("Preview (first 200 rows)"),
        if (HAS_DT) DTOutput("sim_table") else tableOutput("sim_table"),
        tags$h4("Egos alive per tick"),
        plotOutput("sim_plot", height = "300px"),
        uiOutput("sim_download")
      )
    )
  ),

  # ---- Panel 2: bootstrap replicates ----
  tabPanel(
    "Bootstrap replicates",
    sidebarLayout(
      sidebarPanel(
        width = 4,
        # >>> Claude 2026-09-29: engine picker removed (base R only)
        # <<< Claude 2026-09-29
        tags$strong("Population file"),
        fluidRow(
          column(9, textInput("boot_population_path", NULL, value = DEFAULT_PATHS$population, width = "100%")),
          column(3, actionButton("boot_population_path_browse", "Browse..."))
        ),
        tags$p(class = "text-muted", "Links file (auto-detected from the population file's name):"),
        uiOutput("boot_links_path_display"),
        numericInput("n_bootstrap", "Number of replicates", value = 20, min = 1, step = 1),
        numericInput("base_seed", "Base seed", value = 1000, min = 1, step = 1),
        numericInput("boot_max_ticks", "Max ticks", value = 90, min = 1, step = 1),
        textInput("root_name", "Output file name root", value = "computeCare_baseR"),
        textInput("boot_output_dir", "Output folder", value = file.path(APP_DIR, "bootstrap_output"), width = "100%"),
        tags$hr(),
        actionButton("run_boot", "Run bootstrap", class = "btn-primary", width = "100%")
      ),
      mainPanel(
        width = 8,
        uiOutput("boot_status"),
        tags$h4("Replicates written"),
        tableOutput("boot_table")
      )
    )
  ),

  # ---- Panel 3: analyse results ----
  tabPanel(
    "Analyse results",
    sidebarLayout(
      sidebarPanel(
        width = 4,
        radioButtons("agg_mode", "What to analyse",
                     choices = c("One simulation run" = "single",
                                 "Many bootstrap replicates" = "batch"),
                     selected = "batch"),

        conditionalPanel(
          condition = "input.agg_mode == 'single'",
          tags$strong("DEMOCARE output file"),
          fluidRow(
            column(9, textInput("agg_file_path", NULL, value = OUTPUT_DIR, width = "100%")),
            column(3, actionButton("agg_file_browse", "Browse..."))
          ),
          numericInput("agg_cohort", "Birth cohort label", value = 2020, step = 1)
        ),

        conditionalPanel(
          condition = "input.agg_mode == 'batch'",
          tags$strong("Bootstrap replicates folder"),
          fluidRow(
            column(9, textInput("agg_input_dir", NULL, value = file.path(APP_DIR, "bootstrap_output"), width = "100%")),
            column(3, actionButton("agg_input_dir_browse", "Browse..."))
          ),
          uiOutput("agg_roots_summary"),
          selectInput("agg_roots", "Root name(s) to include", choices = character(0), multiple = TRUE),
          fluidRow(
            column(6, numericInput("agg_debSimul", "First iSimul", value = 1, min = 1, step = 1)),
            column(6, numericInput("agg_nSimulFiles", "Last iSimul", value = 1, min = 1, step = 1))
          ),
          fluidRow(
            column(6, numericInput("agg_debDemoCare", "First replicate", value = 1, min = 1, step = 1)),
            column(6, numericInput("agg_nDemoCareFiles", "Last replicate", value = 1, min = 1, step = 1))
          )
        ),

        tags$hr(),
        tags$strong("Output folder (.RData files)"),
        fluidRow(
          column(9, textInput("agg_output_dir", NULL, value = file.path(APP_DIR, "analysis_output"), width = "100%")),
          column(3, actionButton("agg_output_dir_browse", "Browse..."))
        ),
        tags$hr(),
        actionButton("run_aggregate", "Build .RData files", class = "btn-primary", width = "100%")
      ),
      mainPanel(
        width = 8,
        uiOutput("agg_status"),
        tags$h4("careSupplyDF (careSupply.RData)"),
        tableOutput("agg_care_preview"),
        tags$h4("cTabFertNuptDF (FertNupt.RData)"),
        tableOutput("agg_fert_preview"),
        tags$h4("informalCareDF (formal_informal.RData)"),
        tableOutput("agg_informal_preview")
      )
    )
  ),

  # ---- Panel 4: about ----
  tabPanel(
    "About",
    fluidRow(column(
      8, offset = 2,
      tags$h4("What this is"),
      # >>> Claude 2026-09-29: "(both engines)" -> "(base R engine)" below
      tags$p("A menu front end for the computeCare functions in Modules/computeCare_R/ -- ",
             tags$code("run_computeCare()"), " (base R engine) and the family-network bootstrap ",
             "in ", tags$code("bootstrap_resample.R"), ". See Democare 2026.docx for what the ",
             "simulation itself does."),
      # <<< Claude 2026-09-29
      tags$p("The links file is never asked for directly -- it's derived from the population ",
             "file's own name by adding \"_link\" before the extension (e.g. ",
             tags$code("Population_indKin.txt"), " -> ", tags$code("Population_indKin_link.txt"),
             "), the standard NetLogo naming convention. ",
             # >>> Claude 2026-10-02: zipped KINFERT files
             "Zipped KINFERT files can be picked directly (e.g. ", tags$code("X_indKin.zip"),
             "): the links file is found inside the same zip, in a separate ", tags$code("X_indKin_link.zip"),
             ", or as a plain ", tags$code("X_indKin_link.txt"), ". Zipped files are unzipped to a ",
             "temporary folder for the run and deleted afterwards; the .zip files are never changed. ",
             # <<< Claude 2026-10-02
             "The Dependency/Activity/",
             "Care-hours folder at the top applies to both tabs; use \"Change folder...\" to point ",
             "it somewhere else -- that becomes the new default next time the app is opened too."),
      tags$p("Results from \"Run a simulation\" and \"Bootstrap replicates\" are written to ",
             tags$code(OUTPUT_DIR), " and ", tags$code(file.path(APP_DIR, "bootstrap_output")),
             " respectively, in the same tab-separated *.dat / *.txt format ",
             tags$code("democare_compute.R"), " already reads."),
      tags$p("\"Analyse results\" is the last step before plotting: it turns those *.dat/*.txt ",
             "files into the three ", tags$code(".RData"), " files ", tags$code("democare_graphs.R"),
             " expects (", tags$code("careSupply.RData"), ", ", tags$code("FertNupt.RData"), ", ",
             tags$code("formal_informal.RData"), "), calling ", tags$code("createDemoCareFiles()"),
             " for one run, or ", tags$code("createCareSupplyDF()"), " / ", tags$code("computeInformalCare()"),
             " for a whole folder of bootstrap replicates at once -- it scans the folder for you and ",
             "fills in the replicate range, rather than counting files by hand."),
      tags$p("This app only calls the existing functions -- it does not change any of their logic.")
    ))
  )
)


# ==== 5. Server ====

server <- function(input, output, session) {

  # ---- Shared: the Dependency/Activity/Care-hours input files folder ----
  # (one reactiveVal, read by both tabs; changing it via "Change
  # folder..." persists it to app_settings.txt as the new default too)

  input_dir <- reactiveVal(load_app_settings()$input_dir)

  output$input_dir_display <- renderText({ input_dir() })

  output$input_dir_status <- renderUI({
    missing <- missing_input_files(input_dir())
    if (length(missing) == 0) {
      # \u2713 (a plain ASCII escape in the source file itself) rather
      # than a literal checkmark character, so this renders correctly
      # regardless of the encoding R happens to assume for this file.
      tags$span(class = "text-success", "\u2713 all files found")
    } else {
      tags$span(class = "text-danger", sprintf("Missing: %s", paste(missing, collapse = ", ")))
    }
  })

  observeEvent(input$browse_input_dir, {
    picked <- browse_for_folder(input_dir())
    if (length(picked) == 1 && nzchar(picked)) {
      input_dir(picked)
      save_app_settings(list(input_dir = picked))
      showNotification(paste("Input files folder set to", picked), type = "message")
    }
  })

  # ---- Shared: population-file browse buttons + auto-detected links ----

  # >>> Claude 2026-10-02: zipped KINFERT files
  render_links_display <- function(population_path) {
    links <- locate_links_source(population_path)
    zip_note <- if (is_zip_path(population_path) || (!is.null(links) && is_zip_path(links$path))) {
      tags$p(class = "text-muted",
             "Zipped: unzipped to a temporary folder for the run, deleted afterwards; the .zip files are left as they are.")
    }
    if (is.null(links) || is.na(links$path)) {
      tags$span(class = "text-danger",
                "Could not detect -- the population file's name should contain \"population\".")
    } else if (isTRUE(links$found)) {
      shown <- if (is.null(links$entry)) links$path else sprintf("%s (inside %s)", links$entry, links$path)
      tagList(tags$span(class = "text-success", shown), zip_note)
    } else {
      tags$span(class = "text-danger", paste("Not found:", links$path))
    }
  }
  # <<< Claude 2026-10-02
  output$links_path_display      <- renderUI(render_links_display(input$population_path))
  output$boot_links_path_display <- renderUI(render_links_display(input$boot_population_path))

  observeEvent(input$population_path_browse, {
    picked <- browse_for_population_file(input$population_path)
    if (length(picked) == 1 && nzchar(picked)) updateTextInput(session, "population_path", value = picked)
  })
  observeEvent(input$boot_population_path_browse, {
    picked <- browse_for_population_file(input$boot_population_path)
    if (length(picked) == 1 && nzchar(picked)) updateTextInput(session, "boot_population_path", value = picked)
  })

  # ---- Single simulation run ----

  sim_result <- eventReactive(input$run_sim, {
    # >>> Claude 2026-10-02: zipped KINFERT files
    pair <- tryCatch(prepare_kinfert_pair(input$population_path),
                     error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL })
    if (is.null(pair)) return(NULL)
    on.exit(release_kinfert_pair(pair), add = TRUE)

    paths <- c(
      list(population = pair$population, links = pair$links),
      resolve_input_files(input_dir())
    )
    # <<< Claude 2026-10-02
    missing <- names(paths)[!vapply(paths, file.exists, logical(1))]
    if (length(missing) > 0) {
      showNotification(paste("File not found for:", paste(missing, collapse = ", ")), type = "error")
      return(NULL)
    }

    # >>> Claude 2026-09-29
    engine <- ENGINE
    # <<< Claude 2026-09-29

    progress <- shiny::Progress$new(session)
    # >>> Claude 2026-10-02: add = TRUE, so the temporary unzipped files are still deleted
    on.exit(progress$close(), add = TRUE)
    # <<< Claude 2026-10-02
    progress$set(message = "Running computeCare...", value = 0)
    run_engine(engine, paths, seed = input$seed, max_ticks = input$max_ticks, progress = progress)
  })

  output$sim_status <- renderUI({
    res <- sim_result()
    if (is.null(res)) return(NULL)
    tags$p(class = "text-success",
           sprintf("Finished: %d ego-year rows, %d ticks reached.",
                    nrow(res), max(res$ticks)))
  })

  if (HAS_DT) {
    output$sim_table <- renderDT({
      req(sim_result())
      datatable(head(sim_result(), 200), options = list(scrollX = TRUE, pageLength = 10))
    })
  } else {
    output$sim_table <- renderTable({
      req(sim_result())
      head(sim_result(), 200)
    })
  }

  output$sim_plot <- renderPlot({
    res <- req(sim_result())
    alive <- aggregate(who ~ ticks, data = res, FUN = length)
    plot(alive$ticks, alive$who, type = "l", lwd = 2,
         xlab = "Tick", ylab = "Egos alive", main = NULL)
  })

  output$sim_download <- renderUI({
    req(sim_result())
    downloadButton("download_sim", "Save full results to a file")
  })

  output$download_sim <- downloadHandler(
    filename = function() {
      # >>> Claude 2026-09-29
      engine <- ENGINE
      # <<< Claude 2026-09-29
      sprintf("computeCare_%s_seed%d.dat", engine, input$seed)
    },
    content = function(file) {
      write.table(sim_result(), file, sep = "\t", row.names = FALSE, quote = FALSE)
    }
  )

  # ---- Bootstrap replicates ----

  boot_result <- eventReactive(input$run_boot, {
    # >>> Claude 2026-10-02: zipped KINFERT files
    # The base files are read only once (below), so the temporary
    # unzipped copies can be deleted right after reading.
    pair <- tryCatch(prepare_kinfert_pair(input$boot_population_path),
                     error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL })
    if (is.null(pair)) return(NULL)
    on.exit(release_kinfert_pair(pair), add = TRUE)
    # <<< Claude 2026-10-02

    paths <- resolve_input_files(input_dir())
    missing <- names(paths)[!vapply(paths, file.exists, logical(1))]
    if (length(missing) > 0) {
      showNotification(paste("File not found for:", paste(missing, collapse = ", ")), type = "error")
      return(NULL)
    }

    # >>> Claude 2026-09-29
    engine <- ENGINE
    # <<< Claude 2026-09-29
    eng <- ENGINE_ENVS[[engine]]
    dir.create(input$boot_output_dir, showWarnings = FALSE, recursive = TRUE)

    # >>> Claude 2026-10-02: read from the (possibly unzipped) pair, then free the temporary copies
    base_population <- eng$read_population(pair$population)
    base_links      <- eng$read_links(pair$links)
    release_kinfert_pair(pair)
    # <<< Claude 2026-10-02

    n <- input$n_bootstrap
    rows <- vector("list", n)

    progress <- shiny::Progress$new(session)
    # >>> Claude 2026-10-02
    on.exit(progress$close(), add = TRUE)
    # <<< Claude 2026-10-02
    progress$set(message = "Running bootstrap replicates...", value = 0)

    for (i in seq_len(n)) {
      progress$set(value = i / n, detail = sprintf("Replicate %d of %d", i, n))
      replicate_seed <- input$base_seed + i
      res <- run_bootstrap_replicate(
        engine, base_population, base_links, paths,
        root_name = input$root_name, i_bootstrap = i, seed = replicate_seed,
        max_ticks = input$boot_max_ticks, output_dir = input$boot_output_dir
      )
      rows[[i]] <- data.frame(
        replicate = i, seed = replicate_seed, families = res$n_families,
        rows = res$n_rows, file = res$out_path
      )
    }

    do.call(rbind, rows)
  })

  output$boot_status <- renderUI({
    res <- boot_result()
    if (is.null(res)) return(NULL)
    tags$p(class = "text-success",
           sprintf("Finished: %d replicate files written to %s.", nrow(res), input$boot_output_dir))
  })

  output$boot_table <- renderTable({
    req(boot_result())
  })

  # ---- Analyse results ----

  observeEvent(input$agg_file_browse, {
    picked <- browse_for_population_file(input$agg_file_path, caption = "Select a DEMOCARE output file")
    if (length(picked) == 1 && nzchar(picked)) updateTextInput(session, "agg_file_path", value = picked)
  })
  observeEvent(input$agg_input_dir_browse, {
    picked <- browse_for_folder(input$agg_input_dir, caption = "Select the bootstrap replicates folder")
    if (length(picked) == 1 && nzchar(picked)) updateTextInput(session, "agg_input_dir", value = picked)
  })
  observeEvent(input$agg_output_dir_browse, {
    picked <- browse_for_folder(input$agg_output_dir, caption = "Select the output folder for the .RData files")
    if (length(picked) == 1 && nzchar(picked)) updateTextInput(session, "agg_output_dir", value = picked)
  })

  # Re-scanned whenever the folder path changes -- this is what lets the
  # "Root name(s)" list and the iSimul/replicate range below fill
  # themselves in instead of Daniel counting files by hand.
  roots_summary <- reactive({
    req(input$agg_input_dir)
    if (!dir.exists(input$agg_input_dir)) return(data.frame())
    summarise_bootstrap_roots(input$agg_input_dir)
  })

  observeEvent(roots_summary(), {
    summ <- roots_summary()
    if (nrow(summ) == 0) {
      updateSelectInput(session, "agg_roots", choices = character(0))
      return()
    }
    labels <- sprintf("%s  (iSimul %d-%d, replicate %d-%d, %d files%s)",
                       summ$root, summ$debSimul, summ$nSimulFiles,
                       summ$debDemoCare, summ$nDemoCareFiles, summ$n_files,
                       ifelse(summ$complete, "", ", SOME MISSING"))
    choices <- setNames(summ$root, labels)
    updateSelectInput(session, "agg_roots", choices = choices, selected = summ$root[1])
  })

  output$agg_roots_summary <- renderUI({
    if (nrow(roots_summary()) == 0) {
      tags$p(class = "text-muted",
             "No \"<root>_<iSimul>_<iDemoCare>.txt\" files found in this folder yet.")
    } else {
      NULL
    }
  })

  # Selecting root(s) fills in the iSimul/replicate range from what was
  # actually found on disk for them; still editable by hand afterwards,
  # e.g. to analyse only the first 50 of 200 replicates.
  observeEvent(input$agg_roots, {
    summ <- roots_summary()
    sel <- summ[summ$root %in% input$agg_roots, ]
    if (nrow(sel) == 0) return()
    updateNumericInput(session, "agg_debSimul",      value = min(sel$debSimul))
    updateNumericInput(session, "agg_nSimulFiles",    value = max(sel$nSimulFiles))
    updateNumericInput(session, "agg_debDemoCare",    value = min(sel$debDemoCare))
    updateNumericInput(session, "agg_nDemoCareFiles", value = max(sel$nDemoCareFiles))
  }, ignoreNULL = FALSE)

  agg_result <- eventReactive(input$run_aggregate, {
    dir.create(input$agg_output_dir, showWarnings = FALSE, recursive = TRUE)

    progress <- shiny::Progress$new(session)
    on.exit(progress$close())
    progress$set(message = "Building .RData files...", value = 0)

    if (input$agg_mode == "single") {
      if (!file.exists(input$agg_file_path)) {
        showNotification("DEMOCARE output file not found.", type = "error")
        return(NULL)
      }
      progress$set(value = 0.3)
      run_aggregate_single(input$agg_file_path, input$agg_output_dir, input$agg_cohort)
    } else {
      if (length(input$agg_roots) == 0) {
        showNotification("Pick at least one root name to aggregate.", type = "error")
        return(NULL)
      }
      if (!dir.exists(input$agg_input_dir)) {
        showNotification("Bootstrap replicates folder not found.", type = "error")
        return(NULL)
      }
      n_files <- (input$agg_nSimulFiles - input$agg_debSimul + 1) *
        (input$agg_nDemoCareFiles - input$agg_debDemoCare + 1) *
        length(input$agg_roots)
      run_aggregate_batch(
        input_dir = input$agg_input_dir, output_dir = input$agg_output_dir,
        roots = input$agg_roots,
        debSimul = input$agg_debSimul, nSimulFiles = input$agg_nSimulFiles,
        debDemoCare = input$agg_debDemoCare, nDemoCareFiles = input$agg_nDemoCareFiles,
        progress = progress, n_expected = max(n_files, 1) * 2
      )
    }

    load_aggregate_preview(input$agg_output_dir)
  })

  output$agg_status <- renderUI({
    res <- agg_result()
    if (is.null(res)) return(NULL)
    tags$p(class = "text-success",
           sprintf("Finished. .RData files written to %s.", input$agg_output_dir))
  })

  output$agg_care_preview <- renderTable({
    req(agg_result())
    head(agg_result()$careSupply, 10)
  })
  output$agg_fert_preview <- renderTable({
    req(agg_result())
    head(agg_result()$fertNupt, 10)
  })
  output$agg_informal_preview <- renderTable({
    req(agg_result())
    head(agg_result()$formalInformal, 10)
  })
}


# ==== 6. Launch ====

shinyApp(ui, server)
