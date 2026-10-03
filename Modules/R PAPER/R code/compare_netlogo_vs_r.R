currPath <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(currPath)
source("democare_compare_helpers.R")

# 2026-08-14 UPDATE -- NetLogo's carebyPartner/carebyChildren columns are
# ALWAYS 0 whenever care2Only?=TRUE (Daniel's actual config). Verified by
# reading the NetLogo source directly:
#   - CareUpdate.nls: t_carebyPartner/t_carebyChildren are only ever set
#     inside `computeCare`'s `if not care2Only? [ ... ]` block (the OLD
#     care1 algorithm -- explore_netCare_Partners/Children). With
#     care2Only?=TRUE that whole block never runs, so both stay at the
#     reset value of 0 for every ego, every tick.
#   - democare.nlogo's printInfoEgo (the row writer) confirms the header
#     order lines up 1:1 with the file-print items: header column
#     "carebyPartner"/"carebyChildren" <- t_carebyPartner/t_carebyChildren
#     (the always-0 care1 values), while header column
#     "care2byPartner"/"care2byChildren" <- item kCare2EgoPToEgo/
#     kCare2ChildToEgo t_care2CompCount -- cumulative "care received by
#     ego from partner"/"from children" totals from the care2 cascade
#     ledger (t_care2To_list via addCare2Comp/cumulCompCare2), which DOES
#     run under care2Only?=TRUE.
#   - This matches Daniel's own hypothesis exactly: NetLogo's real
#     informal-care-received numbers live in care2byPartner/
#     care2byChildren, not carebyPartner/carebyChildren.
#   - computeCare_R's carebyPartner/carebyChildren already implement the
#     care2-cascade logic (2026-07-28 work) and care2byPartner/
#     care2byChildren are pure ALIASES of them (see democare_compute.R) --
#     so for a NetLogo-vs-R comparison, the columns that are actually
#     comparable on BOTH sides are care2byPartner/care2byChildren
#     (equivalently: R's carebyPartner/carebyChildren, since those are
#     identical to R's care2byPartner/care2byChildren by construction).
#     NetLogo's plain carebyPartner/carebyChildren will read 0 no matter
#     what R produces -- that is expected, not a bug, and is NOT a useful
#     comparison metric under care2Only?=TRUE.
# The diff CSV below already included Care2ByPartner/Care2ByChildren
# (Section 3), so the numeric comparison was always available -- what's
# fixed here is (a) the overlay PLOTS (Section 4), which were plotting
# the always-0-for-NetLogo carebyPartner/carebyChildren instead, and
# (b) the >10% discrepancy flagging, which was flagging that expected
# 0-vs-nonzero mismatch as if it were a real disagreement every single run.

# ==== 1. Pick the two output files (same input, two engines) ====

# NOTE: pick the NetLogo .dat file produced by the "chooseFile" workflow in
# democare.nlogo (File > choose input, writes gPathOut/fileRoot + ".dat").
# Daniel's setup writes this as "Population.dat" inside
# Modules/KINFERT/output/ -- the picker below starts there directly. Also
# pick the .dat file produced by running computeCare_baseR.R or
# computeCare_tidyverse.R on the SAME input population/links files. Both
# files must come from the same input for the comparison to be meaningful.

NETLOGO_OUTPUT_DIR <- file.path("..", "..", "KINFERT", "output")

filePathNetLogo <- rstudioapi::selectFile(
  caption = "Select the NetLogo output file (typically Population.dat)",
  filter  = "DAT Files (*.dat)",
  path    = if (dir.exists(NETLOGO_OUTPUT_DIR)) NETLOGO_OUTPUT_DIR else currPath,
  existing = TRUE
)
if (is.null(filePathNetLogo)) stop("no NetLogo file selected")

filePathR <- rstudioapi::selectFile(
  caption = "Select the computeCare_R output file (*.dat)",
  filter  = "DAT Files (*.dat)",
  path    = dirname(filePathNetLogo),
  existing = TRUE
)
if (is.null(filePathR)) stop("no R file selected")

cohortLabel <- rstudioapi::showPrompt(
  title   = "Cohort",
  message = "Birth cohort of this run (used only to label the output)",
  default = "2020"
)
if (is.null(cohortLabel)) stop("no cohort label given")

answer <- rstudioapi::showQuestion("Save comparison output",
                                   "Output directory same as NetLogo file's?",
                                   ok = "Same", cancel = "Different")
pathOut <- paste(dirname(filePathNetLogo), getDirSep(), sep = "")
if (answer == FALSE) {
  path <- rstudioapi::selectDirectory(caption = "Select Output Directory",
                                      label = "Select", path = pathOut)
  if (!is.null(path)) pathOut <- paste(path, getDirSep(), sep = "")
}


# ==== 2. Aggregate both files the same way, then combine ====

aggNetLogo <- aggregate_one_file(filePathNetLogo, cohortLabel, source_label = "NetLogo")
aggR       <- aggregate_one_file(filePathR,       cohortLabel, source_label = "R")

combined <- combine_sources(aggNetLogo, aggR)


# ==== 3. Numeric diff/ratio tables ====

# NOTE on group_cols/metric_cols below: these must match the exact column
# names calcFertNupt()/computeCareSupply()/computeBalance() produce in
# democare_compute.R -- see that file if these functions' outputs change.

fertNuptMetrics <- c("TFR", "childlessness", "singlehood", "wedlock", "widowhood")
diffFertNupt <- diff_table(combined$fertNupt,
                           group_cols  = c("cohort", "sex"),
                           metric_cols = fertNuptMetrics)

careSupplyMetrics <- c("individual", "partner_mort", "partner_dep", "partner_work",
                       "partner_comp", "partner_ego", "children_mort", "children_dep",
                       "children_dep_health", "children_dep_age", "children_work",
                       "children_comp_partner", "children_comp_siblings",
                       "children_comp_children", "children_ego")
diffCareSupply <- diff_table(combined$careSupply,
                             group_cols  = c("cohort", "ticks", "sex", "dependency"),
                             metric_cols = careSupplyMetrics)

# Full set kept in the CSV for transparency (see 2026-08-14 note above) --
# CareByPartner/CareByChildren will read 0 for NetLogo whenever
# care2Only?=TRUE, by design, so treat those two columns as informational
# only. Care2ByPartner/Care2ByChildren are the columns that are actually
# comparable across sources under that config.
# PropDep/NoDep added (2026-08-14): Dep alone (a raw headcount) is only
# comparable across sources if NetLogo and R happen to be tracking the
# exact same population size, which isn't guaranteed. PropDep
# (Dep / (Dep+NoDep), computed in democare_compute.R's doIt()) is the
# metric that's actually meaningful to diff/ratio between sources
# regardless of population size; NoDep is kept alongside Dep for the same
# transparency reason CareByPartner/CareByChildren are kept.
informalCareMetrics <- c("Dep", "NoDep", "PropDep", "Care", "CareByPartner", "CareByChildren",
                         "Care2ByPartner", "Care2ByChildren")
diffInformalCare <- diff_table(combined$informalCare,
                               group_cols  = c("cohort", "ticks", "sex", "familyKind"),
                               metric_cols = informalCareMetrics)

# Rows where NetLogo and R disagree by more than 10% (tol is adjustable).
# NOTE: CareByPartner/CareByChildren are excluded from the flagged set --
# under care2Only?=TRUE NetLogo's values there are always 0 by design (see
# 2026-08-14 note above), so every row would flag as a "100% off"
# discrepancy regardless of how well the two engines actually agree. Use
# Care2ByPartner/Care2ByChildren for the real comparison; CareByPartner/
# CareByChildren stay in diffInformalCare/the CSV for reference only. If
# you ever run NetLogo with care2Only?=FALSE, add them back here.
# PropDep replaces Dep in the flagged set (2026-08-14) for the same
# population-size reason noted above -- Dep stays in diffInformalCare/the
# CSV for reference.
informalCareFlagMetrics <- c("PropDep", "Care", "Care2ByPartner", "Care2ByChildren")

flagFertNupt     <- flag_large_discrepancies(diffFertNupt,     fertNuptMetrics,     tol = 0.10)
flagCareSupply   <- flag_large_discrepancies(diffCareSupply,   careSupplyMetrics,   tol = 0.10)
flagInformalCare <- flag_large_discrepancies(diffInformalCare, informalCareFlagMetrics, tol = 0.10)

message(sprintf("fertNupt:     %d / %d rows flagged (>10%% off)",
               nrow(flagFertNupt), nrow(diffFertNupt)))
message(sprintf("careSupply:   %d / %d rows flagged (>10%% off)",
               nrow(flagCareSupply), nrow(diffCareSupply)))
message(sprintf("informalCare: %d / %d rows flagged (>10%% off)",
               nrow(flagInformalCare), nrow(diffInformalCare)))

# NOTE: dependency/activity are drawn randomly each tick in both engines, so
# even a correct R port will NOT match NetLogo tick-for-tick or ego-for-ego.
# These tables compare AGGREGATED series (summed/rated across the whole
# population per tick), where the two engines should track each other
# closely if the underlying logic matches -- persistent large ratios (not
# just single noisy ticks) are the signal worth investigating.

write.csv(diffFertNupt,     paste0(pathOut, "compare_fertNupt_",     cohortLabel, ".csv"), row.names = FALSE)
write.csv(diffCareSupply,   paste0(pathOut, "compare_careSupply_",   cohortLabel, ".csv"), row.names = FALSE)
write.csv(diffInformalCare, paste0(pathOut, "compare_informalCare_", cohortLabel, ".csv"), row.names = FALSE)
message(paste0("Diff tables saved to ", pathOut))


# ==== 4. Overlay plots (NetLogo vs R) ====

plotWidth  <- 14.85
plotHeight <- 6.83

pFertNupt <- lapply(fertNuptMetrics, function(m) plot_compare_fertnupt(combined$fertNupt, metric = m))
names(pFertNupt) <- fertNuptMetrics
for (m in fertNuptMetrics) {
  ggsave(paste0(pathOut, "compare_fertNupt_", m, ".pdf"), plot = pFertNupt[[m]],
        width = plotWidth, height = plotHeight)
}

careSupplyPlotMetrics <- c("individual", "children_dep", "children_work",
                          "partner_dep", "partner_work")
pCareSupply <- lapply(careSupplyPlotMetrics, function(m)
  plot_compare_supply(combined$careSupply, component = m,
                      dependency_filter = "dependent", sex_filter = "T"))
names(pCareSupply) <- careSupplyPlotMetrics
for (m in careSupplyPlotMetrics) {
  ggsave(paste0(pathOut, "compare_careSupply_", m, ".pdf"), plot = pCareSupply[[m]],
        width = plotWidth, height = plotHeight)
}

# Care2ByPartner/Care2ByChildren, not CareByPartner/CareByChildren -- see
# the 2026-08-14 note above: under care2Only?=TRUE (Daniel's config),
# NetLogo's plain CareByPartner/CareByChildren are always 0, so plotting
# those would show a flat 0 line for NetLogo next to R's real values --
# not a meaningful comparison. Care2ByPartner/Care2ByChildren are the
# columns that are genuinely comparable on both sides.
informalCarePlotMetrics <- c("Care", "Care2ByPartner", "Care2ByChildren")
pInformalCare <- lapply(informalCarePlotMetrics, function(m)
  plot_compare_informal(combined$informalCare, metric = m, familyKind_filter = "Total"))
names(pInformalCare) <- informalCarePlotMetrics
for (m in informalCarePlotMetrics) {
  ggsave(paste0(pathOut, "compare_informalCare_", m, ".pdf"), plot = pInformalCare[[m]],
        width = plotWidth, height = plotHeight)
}

message(paste0("Comparison plots saved to ", pathOut))

# Everything stays in the environment for interactive inspection too:
#   combined$fertNupt / combined$careSupply / combined$informalCare
#   diffFertNupt / diffCareSupply / diffInformalCare
#   flagFertNupt / flagCareSupply / flagInformalCare
#   pFertNupt / pCareSupply / pInformalCare  (named lists of ggplot objects)
