########################################################################
# democare_compare_helpers.R
#
# Shared helpers for comparing DEMOCARE output between sources (NetLogo
# vs computeCare_R) and/or aggregating it across several birth-cohort
# input files. Both `compare_netlogo_vs_r.R` and `aggregate_multi_cohort.R`
# source this file rather than duplicating logic.
#
# WHY THIS WORKS: NetLogo's own output file (one .dat per run, from the
# "chooseFile" workflow -- gMainFileHeader in democare.nlogo) and
# computeCare_baseR.R/computeCare_tidyverse.R's streamlined .dat output
# are NOT identical column sets (NetLogo's is the full ~55-column
# accounting table; computeCare_R only writes the ~30 columns
# democare_compute.R actually reads), but every column
# democare_compute.R's own functions (calcFertNupt/careSupply/
# balanceFormalInformal) need IS present in both, with the same names.
# So the exact same aggregation functions apply to either source
# unchanged -- the only thing that differs between "comparing NetLogo
# vs R" and "just aggregating R's own output" is which file you point
# at and what label you attach.
#
# This file assumes it lives next to democare_compute.R (both in
# Modules/R PAPER/R code/) and sources it directly below.
########################################################################

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("democare_compute.R")   # calcFertNupt, careSupply, computeCareSupply,
                                # balanceFormalInformal, computeBalance,
                                # kMaxTick, and library2("reshape2")

library2("ggplot2")


# ==== 1. Reading a single output file ====

# Both NetLogo's .dat and computeCare_R's .dat are written the same way
# (tab-separated, header row, see computeCare_baseR.R Section 9's
# write_tsv/write.table note) -- this is exactly the read.table() call
# createDemoCareFiles() already uses, pulled out here so both scripts
# below can share it.
read_democare_output <- function(path) {
  read.table(path, header = TRUE, row.names = NULL, sep = "\t")
}


# ==== 2. Aggregating one file (one cohort, one source) ====

# Runs the same three aggregation steps createDemoCareFiles() does
# (calcFertNupt, careSupply -> computeCareSupply, balanceFormalInformal
# -> computeBalance) on a single input file, and tags every result row
# with `source_label` so two calls to this function -- one per source --
# can be safely rbind()ed together afterwards for comparison.
#
#   path            path to a NetLogo or computeCare_R .dat file
#   cohort          birth-cohort label (e.g. "1938") -- becomes the
#                   `cohort` column, same as createDemoCareFiles()
#   source_label    e.g. "NetLogo" or "R" -- your choice of label,
#                   whatever you want the `source` column to read
#   iSimulFiles/iDemoCareFiles   passed straight through to the
#                   democare_compute.R functions; irrelevant for a
#                   single non-bootstrapped run, default 1/1 is fine
#                   almost always -- see that script's own comments if
#                   you ARE comparing multiple simulation variants.
aggregate_one_file <- function(path, cohort, source_label = NA_character_,
                                iSimulFiles = 1, iDemoCareFiles = 1) {

  tab <- read_democare_output(path)
  cohort <- as.character(cohort)

  fertNupt <- calcFertNupt(tab, cohort, iSimulFiles, iDemoCareFiles)

  cTabSupply   <- careSupply(tab, cohort, iSimulFiles, iDemoCareFiles)
  careSup      <- computeCareSupply(cTabSupply)

  cTabBalance  <- balanceFormalInformal(tab, cohort, iSimulFiles, iDemoCareFiles)
  informal     <- computeBalance(cTabBalance)

  fertNupt$source <- source_label
  careSup$source  <- source_label
  informal$source <- source_label

  list(fertNupt = fertNupt, careSupply = careSup, informalCare = informal)
}


# ==== 3. Aggregating across several cohort files, ONE source ====

# The multi-cohort equivalent of Section 2 -- this is what
# createCareSupplyDF()/computeInformalCare() in democare_compute.R do,
# but WITHOUT their zip-handling / nSimulFiles x nDemoCareFiles bootstrap
# looping, which only matters if you have several simulation replicates
# per cohort. For "one set of input per cohort" (a single NetLogo run or
# a single computeCare_R run per birth cohort), that machinery is more
# than you need -- this is the simpler version.
#
#   file_map   a NAMED vector or list: names = cohort labels, values =
#              file paths. Example:
#                file_map <- c(
#                  "1908" = "/path/to/KIN1908_output.dat",
#                  "1938" = "/path/to/KIN1938_output.dat",
#                  "1968" = "/path/to/KIN1968_output.dat"
#                )
#              The names become each row's `cohort` value -- they do NOT
#              need to follow any particular file-naming convention,
#              unlike createCareSupplyDF()'s nDemRegFileNames.
#
# Returns the same shape as aggregate_one_file(), just with every
# cohort's rows stacked via rbind() -- ready to `save()` under the same
# object names democare_graphs.R expects (see aggregate_multi_cohort.R).
aggregate_cohorts <- function(file_map, source_label = NA_character_,
                               iSimulFiles = 1, iDemoCareFiles = 1) {

  if (is.null(names(file_map)) || any(names(file_map) == "")) {
    stop("file_map must be a NAMED vector/list: names(file_map) are the ",
         "cohort labels, e.g. c(`1908` = \"path/to/file.dat\", ...)")
  }

  fertNuptDF <- data.frame()
  careSupplyDF <- data.frame()
  informalCareDF <- data.frame()

  for (cohort in names(file_map)) {
    message(sprintf("Aggregating cohort %s (%s)...", cohort, file_map[[cohort]]))
    agg <- aggregate_one_file(file_map[[cohort]], cohort, source_label,
                               iSimulFiles, iDemoCareFiles)
    fertNuptDF     <- rbind(fertNuptDF,     agg$fertNupt)
    careSupplyDF   <- rbind(careSupplyDF,   agg$careSupply)
    informalCareDF <- rbind(informalCareDF, agg$informalCare)
  }

  list(fertNupt = fertNuptDF, careSupply = careSupplyDF, informalCare = informalCareDF)
}


# ==== 4. Combining two sources for comparison ====

# Simple rbind() of two aggregate_one_file()/aggregate_cohorts() results
# -- pulled out as its own step mainly so the intent (stack two SOURCES
# together, as opposed to two cohorts) is explicit at the call site.
combine_sources <- function(agg_a, agg_b) {
  list(
    fertNupt     = rbind(agg_a$fertNupt,     agg_b$fertNupt),
    careSupply   = rbind(agg_a$careSupply,   agg_b$careSupply),
    informalCare = rbind(agg_a$informalCare, agg_b$informalCare)
  )
}


# ==== 5. Numeric diff/ratio tables ====

# Turns a combined (two-source) table into a wide one row per group_cols
# combination, with each metric split into <source>_<metric> pairs plus
# a _diff (source_b - source_a) and _ratio (source_b / source_a) column.
# This is the "pin down exact discrepancies" half of the comparison --
# use the plots (Section 6) for the visual sweep, this for the numbers.
#
#   combined_df   a table with a `source` column holding exactly the two
#                 values in `sources` (e.g. the output of combine_sources())
#   group_cols    columns that identify one comparable row, e.g.
#                 c("ticks", "sex") or c("ticks", "sex", "dependency")
#   metric_cols   the numeric columns you want compared
#   sources       c(source_a, source_b) -- diff/ratio are always
#                 source_b MINUS/OVER source_a, so put NetLogo first if
#                 you want "how far off is R from NetLogo" (diff/ratio
#                 close to 0/1 = good agreement).
diff_table <- function(combined_df, group_cols, metric_cols,
                        source_col = "source", sources = c("NetLogo", "R")) {

  if (length(sources) != 2) stop("diff_table() compares exactly two sources.")

  a <- combined_df[combined_df[[source_col]] == sources[1], c(group_cols, metric_cols)]
  b <- combined_df[combined_df[[source_col]] == sources[2], c(group_cols, metric_cols)]

  names(a)[names(a) %in% metric_cols] <- paste0(sources[1], "_", metric_cols)
  names(b)[names(b) %in% metric_cols] <- paste0(sources[2], "_", metric_cols)

  merged <- merge(a, b, by = group_cols)

  for (m in metric_cols) {
    colA <- paste0(sources[1], "_", m)
    colB <- paste0(sources[2], "_", m)
    merged[[paste0(m, "_diff")]]  <- merged[[colB]] - merged[[colA]]
    merged[[paste0(m, "_ratio")]] <- merged[[colB]] / merged[[colA]]
  }

  merged
}

# Convenience filter on top of diff_table(): keep only the rows where at
# least one metric's ratio strays outside [1-tol, 1+tol] -- a quick way
# to scan a big diff table for the handful of rows actually worth a
# second look, instead of reading every tick by eye. NA/Inf ratios
# (denominator 0 -- e.g. nobody dependent at that tick/group) are kept
# by default (flag_na_inf = TRUE) since a 0-vs-nonzero mismatch is
# usually exactly the kind of thing worth looking at, not noise.
flag_large_discrepancies <- function(diff_df, metric_cols, tol = 0.10, flag_na_inf = TRUE) {
  ratio_cols <- paste0(metric_cols, "_ratio")
  ratio_cols <- ratio_cols[ratio_cols %in% names(diff_df)]

  is_off <- function(r) {
    off <- abs(r - 1) > tol
    if (flag_na_inf) off | is.na(r) | is.infinite(r) else off & !is.na(r) & !is.infinite(r)
  }

  keep <- Reduce(`|`, lapply(diff_df[ratio_cols], is_off))
  diff_df[keep, ]
}


# ==== 6. Comparison plots (ggplot2, base style matching democare_graphs.R) ====

# Fixed NetLogo/R colors, assigned once and reused everywhere below --
# never re-derived per plot, so "blue = NetLogo" stays true across every
# figure you make. (Slots 1/2 of the project's validated categorical
# palette -- chosen for solid colorblind-safe separation, not arbitrary.)
COMPARE_COLORS <- c(NetLogo = "#2a78d6", R = "#eb6834")

compare_theme <- function() {
  theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(),
          legend.position = "bottom",
          legend.title = element_blank())
}

# Generic: plot any single numeric column from a combined (two-source)
# table as a line-per-source time series, optionally faceted (e.g. by
# sex, or by sex + dependency/familyKind). Use this directly for any
# column not already covered by the named helpers below.
plot_compare_metric <- function(combined_df, metric, facet_cols = "sex",
                                 source_col = "source", title = metric) {
  facet_formula <- as.formula(paste("~", paste(facet_cols, collapse = " + ")))
  ggplot(combined_df, aes(x = ticks, y = .data[[metric]],
                           color = .data[[source_col]], linetype = .data[[source_col]])) +
    geom_line(linewidth = 0.7) +
    facet_wrap(facet_formula) +
    scale_color_manual(values = COMPARE_COLORS) +
    labs(title = title, x = "Tick", y = metric) +
    compare_theme()
}

# careSupplyDF-style comparison: the "supply decomposition" proportions
# (partner_mort/dep/work/comp/ego, children_*) computeCareSupply()
# produces, one component at a time. `dependency_filter` matches the
# `dependency` column ("dependent" / "notDependent" / "total").
plot_compare_supply <- function(careSupply_combined, component,
                                 dependency_filter = "dependent", sex_filter = "T") {
  df <- careSupply_combined[
    careSupply_combined$dependency == dependency_filter &
    careSupply_combined$sex        == sex_filter, ]
  plot_compare_metric(df, component, facet_cols = "dependency",
                       title = sprintf("%s (sex=%s, dependency=%s)",
                                        component, sex_filter, dependency_filter))
}

# informalCareDF-style comparison: care hours and the by-partner/
# by-children shares computeBalance() produces. `familyKind_filter`
# matches the `familyKind` column ("noPartner" / "Partner" / "Children" /
# "Total").
plot_compare_informal <- function(informalCare_combined, metric = "Care",
                                   familyKind_filter = "Total") {
  df <- informalCare_combined[informalCare_combined$familyKind == familyKind_filter, ]
  plot_compare_metric(df, metric, facet_cols = "sex",
                       title = sprintf("%s (familyKind=%s)", metric, familyKind_filter))
}

# calcFertNupt() output is a SINGLE snapshot per cohort/sex, not a tick
# series (TFR/childlessness/singlehood/wedlock/widowhood are lifetime
# summary rates) -- so this is a dot comparison, not a line plot.
plot_compare_fertnupt <- function(fertNupt_combined, metric = "TFR", sex_filter = "T") {
  df <- fertNupt_combined[fertNupt_combined$sex == sex_filter, ]
  ggplot(df, aes(x = cohort, y = .data[[metric]], color = source, shape = source)) +
    geom_point(size = 3) +
    scale_color_manual(values = COMPARE_COLORS) +
    labs(title = sprintf("%s (sex=%s)", metric, sex_filter), x = "Cohort", y = metric) +
    compare_theme()
}
