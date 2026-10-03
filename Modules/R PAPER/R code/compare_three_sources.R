########################################################################
# compare_three_sources.R
#
# 2026-08-14 -- Compares THREE outputs of the SAME input population
# (NetLogo, computeCare_baseR.R, computeCare_tidyverse.R) side by side,
# reusing democare_graphs.R's existing cohort-faceted plotting machinery
# instead of building a separate plotting path.
#
# HOW THIS WORKS: aggregate_cohorts() (democare_compare_helpers.R) loops
# calcFertNupt()/careSupply()+computeCareSupply()/balanceFormalInformal()
# +computeBalance() over a NAMED file_map and stacks the results, using
# each name as that file's `cohort` value. Nothing in that machinery is
# actually birth-cohort-specific -- it works unchanged for "one label per
# SOURCE" instead of "one label per birth cohort".
#
# ONE REAL CONSTRAINT (found by testing this against real R, not assumed):
# democare_compute.R's careSupply() and balanceFormalInformal() both do
# `cTab$cohort <- as.integer(cohort)`, but calcFertNupt() does NOT -- it
# keeps whatever you pass it as-is. A text label like "NetLogo" becomes NA
# in careSupplyDF/informalCareDF (silently, via R's usual
# as.integer("text") -> NA + warning) while staying "NetLogo" in
# cTabFertNuptDF -- an inconsistent, confusing failure. Tested and
# confirmed with real execution. So this script uses small SENTINEL
# INTEGER codes (9001/9002/9003, passed as their character form so
# aggregate_cohorts()'s file_map naming rules are satisfied) as the
# actual cohort key -- these behave exactly like any other integer
# birth-cohort code everywhere in democare_compute.R and
# democare_graphs.R -- and keeps a separate SOURCE_COHORTS/SOURCE_LABELS
# lookup to show real names ("NetLogo", "baseR", "tidyverse") in plots.
#
# Chosen 9001-9003 specifically so they can never collide with a real
# birth-cohort year, INCLUDING demRegDF's registry-overlay years in
# democare_graphs.R's plotSupply() (only lists 1898-2020): for 9001-9003
# none of demRegDF's years match, so that overlay silently no-ops
# (TFR/childlessness/everMarried/netTFR just stay 0 for those rows)
# instead of doing anything meaningful.
#
# WHAT THIS BUYS YOU: democare_graphs.R's GENERIC helpers -- BS_curves(),
# plotIt(), plotCareRelatives() -- all treat `cohort` purely as a
# categorical grouping/color variable (`factor(cohort)`, never used
# arithmetically). Tested directly: BS_curves()'s DEFAULT `sets=`
# argument (the two historical-year bins) safely no-ops for 9001-9003 --
# neither bin matches, so all three pseudo-cohorts correctly fall through
# to the "not in any set" branch and get processed together, no code
# changes needed there. Call the generic functions with
# cohortsSubs = SOURCE_COHORTS (defined below) to overlay the three
# sources exactly like you'd overlay three real birth cohorts.
#
# The many PRE-BUILT convenience plots further down in democare_graphs.R
# (pCareRelatives1908_28_48_68_T, ppp1/ppp2/..., and friends) are
# hardcoded to real cohort years (1908/1928/1948/1968/...) and will NOT
# automatically pick up 9001-9003 -- call the generic functions directly
# instead (Sections 3-4 below have ready-to-adapt examples).
#
# NOT USED HERE, DELIBERATELY: democare_graphs.R's plotSupply(). It's not
# just cosmetically birth-cohort-specific -- its dataToPlot() DIVIDES the
# partner/children components by demRegDF-derived `everMarried`/`TFR`
# values (2026-08-14 finding, confirmed by reading the code, not assumed)
# to rescale them onto a "per person who ever married/had children" basis.
# Those overlay values stay 0 for non-matching cohorts (previous
# paragraph), so that division would produce Inf/NaN for 9001-9003, not
# just a silently-missing overlay line. Section 4 below builds its own,
# simpler overlay plots straight from careSupplyDF instead -- its
# partner_*/children_* proportions already sum to 1 by construction
# (computeCareSupply()'s doIt() defines *_ego as 1 minus the rest), no
# demRegDF normalization needed.
#
# CareByPartner/CareByChildren vs Care2ByPartner/Care2ByChildren: use the
# Care2By* columns for NetLogo comparisons -- see the 2026-08-14 note in
# compare_netlogo_vs_r.R (NetLogo's plain CareByPartner/CareByChildren
# read 0 whenever care2Only?=TRUE).
########################################################################

currPath <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(currPath)
source("democare_compare_helpers.R")   # sources democare_compute.R too


# ==== 1. Pick the three same-input output files ====

NETLOGO_OUTPUT_DIR <- file.path("..", "..", "KINFERT", "output")

filePathNetLogo <- rstudioapi::selectFile(
  caption  = "Select the NetLogo output file (typically Population.dat)",
  filter   = "DAT Files (*.dat)",
  path     = if (dir.exists(NETLOGO_OUTPUT_DIR)) NETLOGO_OUTPUT_DIR else currPath,
  existing = TRUE
)
if (is.null(filePathNetLogo)) stop("no NetLogo file selected")

filePathBaseR <- rstudioapi::selectFile(
  caption  = "Select the computeCare_baseR.R output file (*.dat)",
  filter   = "DAT Files (*.dat)",
  path     = dirname(filePathNetLogo),
  existing = TRUE
)
if (is.null(filePathBaseR)) stop("no baseR file selected")

filePathTidyverse <- rstudioapi::selectFile(
  caption  = "Select the computeCare_tidyverse.R output file (*.dat)",
  filter   = "DAT Files (*.dat)",
  path     = dirname(filePathBaseR),
  existing = TRUE
)
if (is.null(filePathTidyverse)) stop("no tidyverse file selected")

answer <- rstudioapi::showQuestion("Save comparison output",
                                   "Output directory same as NetLogo file's?",
                                   ok = "Same", cancel = "Different")
pathOut <- paste(dirname(filePathNetLogo), getDirSep(), sep = "")
if (answer == FALSE) {
  path <- rstudioapi::selectDirectory(caption = "Select Output Directory",
                                      label = "Select", path = pathOut)
  if (!is.null(path)) pathOut <- paste(path, getDirSep(), sep = "")
}


# ==== 2. Aggregate all three, tagged with sentinel pseudo-cohort codes ====

# See header note above for why these are integers-as-strings, not the
# source names directly.
SOURCE_COHORTS <- c(NetLogo = 9001, baseR = 9002, tidyverse = 9003)

file_map <- setNames(
  c(filePathNetLogo, filePathBaseR, filePathTidyverse),
  as.character(SOURCE_COHORTS[c("NetLogo", "baseR", "tidyverse")])
)

agg <- aggregate_cohorts(file_map, source_label = NA_character_)

cTabFertNuptDF <- agg$fertNupt
careSupplyDF   <- agg$careSupply
informalCareDF <- agg$informalCare

# UPDATED (2026-08-14): informalCare = proportion of a dependent ego's
# care NEED that's covered by informal care from partner+children
# (Care2By*, not CareBy* -- see header note). Mirrors democare_graphs.R's
# own careType==2 branch (`informalCareDF$informalCare <- ...Care2ByPartner
# + ...Care2ByChildren`) so this script stays consistent with it. Computed
# here, before any filtering, since Care2ByPartner/Care2ByChildren are
# already well-defined for every familyKind row.
informalCareDF$informalCare <- informalCareDF$Care2ByPartner + informalCareDF$Care2ByChildren

# Save using the exact names/files democare_graphs.R expects -- open that
# script afterwards, answer "Single" when asked (the birthCohort value it
# prompts for is unused by anything below), point its file picker at
# pathOut, then run the snippets below in the console. Saved BEFORE the
# familyKind filter in Section 2b, so this RData still holds all
# familyKind levels, same shape democare_graphs.R expects to load and
# filter itself.
save(cTabFertNuptDF, file = paste0(pathOut, "FertNupt.RData"))
save(careSupplyDF,   file = paste0(pathOut, "careSupply.RData"))
save(informalCareDF, file = paste0(pathOut, "formal_informal.RData"))
message(paste0("Saved FertNupt.RData / careSupply.RData / formal_informal.RData to ", pathOut))
message("cohort codes: ", paste(names(SOURCE_COHORTS), "=", SOURCE_COHORTS, collapse = ", "))


# ==== 2b. Filter informalCareDF to Total rows before any BS_curves() use ====

# UPDATED (2026-08-14): this filter is now done HERE, unconditionally, by
# this script itself -- it used to be skipped, on the assumption that
# whoever ran the Section 3 snippets below would have ALREADY run
# democare_graphs.R first in the same session (that script's own top
# section reduces its global informalCareDF to familyKind=="Total" before
# ever calling BS_curves()). That's a fragile cross-script dependency and
# it's exactly what produced the mysterious, much-too-wide "confidence
# interval" ribbons Daniel saw on both the Dependency plot and, more
# visibly, the Care2ByPartner/Care2ByChildren plots: without this filter,
# informalCareDF still has 4 familyKind rows (noPartner/Partner/Children/
# Total) per (cohort, ticks, sex) cell, and BS_curves()'s dcast (keyed
# only on cohort+ticks, not familyKind) silently averaged across all 4 as
# if they were 4 bootstrap replicate runs. That's not a confidence
# interval at all -- it's the spread ACROSS FAMILY-COMPOSITION SUBGROUPS
# (e.g. Care2ByPartner is ~0 for familyKind=="noPartner" by definition,
# since there's no partner to receive care from, versus a real value for
# "Partner"/"Children" -- a large, meaningless spread), mistaken for
# bootstrap uncertainty. Confirmed directly with real R: filtering to
# Total first makes num==1 per cell (as intended for a single run per
# source) and the ribbon disappears; without the filter num==4 and a wide,
# spurious ribbon appears. This script is now self-contained -- Section 3
# below works whether or not democare_graphs.R was run first.
informalCareTotal <- informalCareDF[informalCareDF$familyKind == "Total", ]


# ==== 3. Dependency and informal-care-coverage plots, 3 sources overlaid ====

source ("democare_graphs_lib.R")

sourceLabel <- function(cohortCode) names(SOURCE_COHORTS)[match(as.numeric(as.character(cohortCode)), SOURCE_COHORTS)]

relabelSources <- function(df) {
#   # drop = FALSE matters here: R's `[.data.frame` silently drops to a
#   # plain (non-data.frame) vector whenever the row-subset ends up with
#   # only one column left -- e.g. if BS_curves()'s createThem() fell back
#   # to its empty-input branch for some (cohort, tick) combos and the
#   # surviving frame only has the `cohort` column. Without drop=FALSE that
#   # turns `df` into an atomic vector, and the next line's `df$cohort`
#   # then fails with "$ operator is invalid for atomic vectors" -- this
#   # was hit and confirmed with real R, not theoretical.
  df <- df[df$cohort %in% SOURCE_COHORTS, , drop = FALSE]
  df$cohort <- factor(sourceLabel(df$cohort), levels = names(SOURCE_COHORTS))
  df
}

# # -- Dependency levels, all three sources overlaid --
# PropDep = proportion of egos alive at this tick who are dependent, not
# the raw Dep headcount -- feeding the raw headcount into a
# percent-formatted axis with no population denominator is what produced
# the original "150,000%" y-axis. See democare_compute.R's doIt() and the
# matching note in democare_graphs.R's "1 dependency" section. With
# exactly one run per source (NetLogo/baseR/tidyverse) and the Total-only
# filter above, BS_curves() now correctly reports NA for the MIN/MAX
# columns (no ensemble to bootstrap a percentile interval from) and
# plotIt() skips drawing the ribbon accordingly.
depCurves3 <- relabelSources(BS_curves(informalCareTotal, "PropDep"))
plotIt(depCurves3, "T", labelY = "Proportion dependent", diffLineType = TRUE)

# # -- Informal care received, by partner / by children (Care2By*, not CareBy* -- see header note) --
careP3 <- relabelSources(BS_curves(informalCareTotal, "Care2ByPartner"))
careC3 <- relabelSources(BS_curves(informalCareTotal, "Care2ByChildren"))
plotIt(careP3, "T", labelY = "Care received from partner", diffLineType = TRUE)
plotIt(careC3, "T", labelY = "Care received from children", diffLineType = TRUE)

# # -- NEW (2026-08-14): total informal-care coverage -- of a dependent
# #    ego's care NEED, what fraction is covered by partner+children
# #    combined (Care2ByPartner + Care2ByChildren, capped conceptually at
# #    1 -- the remainder, 1 - informalCare, is what's NOT covered by
# #    informal care, i.e. either goes unmet or is covered by formal
# #    (paid/institutional) care, which this model doesn't track directly).
informalCov3 <- relabelSources(BS_curves(informalCareTotal, "informalCare"))
plotIt(informalCov3, "T", labelY = "Care need covered by informal care", diffLineType = TRUE)

# # -- calcFertNupt() metrics (TFR, childlessness, ...) are a single snapshot
# #    per cohort/sex, not a tick series -- a dot/bar comparison, not BS_curves/plotIt:
fnDF <- cTabFertNuptDF[as.character(cTabFertNuptDF$cohort) %in% as.character(SOURCE_COHORTS) & cTabFertNuptDF$sex == "T", , drop = FALSE]
fnDF$source <- factor(sourceLabel(fnDF$cohort), levels = names(SOURCE_COHORTS))
ggplot(fnDF, aes(x = source, y = TFR)) + geom_col() + compare_theme()

# NOTE on cTabFertNuptDF$cohort: calcFertNupt() does NOT coerce cohort to
# integer (unlike careSupply()/balanceFormalInformal()), so it stays
# character there -- the as.character() comparisons above are needed for
# that one data frame specifically; careSupplyDF/informalCareDF$cohort are
# integer and compare against SOURCE_COHORTS directly.
#
# If depCurves3/careP3/careC3/informalCov3 end up with fewer rows than
# expected (e.g. fewer than 3 x 45 ticks), check
# dim(informalCareTotal[informalCareTotal$cohort %in% SOURCE_COHORTS, ])
# directly -- the drop=FALSE fix above prevents a crash either way, but a
# genuinely short result then likely means informalCareDF doesn't have
# all three sources/all ticks, worth tracing back to how the three .dat
# files were built.


# ==== 4. Care-supply decomposition: why doesn't ego get enough care? ====

# careSupplyDF (computeCareSupply(), saved above as careSupply.RData)
# already splits partner/children hours into WHY they're unavailable to
# ego, one proportion per reason -- these numbers sum to 1 by
# construction (computeCareSupply()'s doIt() defines *_ego as 1 minus the
# rest), so no demRegDF/TFR/marriage-rate normalization is needed (see the
# "NOT USED HERE" header note on plotSupply() for why that function isn't
# reused for this).
#
#   partner_mort   no partner at all right now (deceased / never partnered)
#   partner_dep    partner exists but is themself dependent
#   partner_work   partner is working
#   partner_comp   partner's caring hours already claimed by someone else
#                  first (egoPtoChild -- e.g. the partner is busy caring
#                  for HIS/HER OWN dependent children ahead of ego)
#   partner_ego    what's actually left over for ego
#
#   children_mort           no children at all right now
#   children_dep             children exist but are themselves dependent
#                            (children_dep_age/children_dep_health split
#                            this further if you want age- vs
#                            health-driven dependency separately -- both
#                            are already in careSupplyDF)
#   children_work            children are working
#   children_comp_partner    children's hours claimed by ego's PARTNER first
#   children_comp_siblings   claimed by ego's OTHER children (ego's
#                             grandchildren's aunts/uncles) first
#   children_comp_children   claimed by ego's children's OWN children/
#                             partner (ego's grandchildren-in-law side)
#                             first
#   children_ego             what's actually left over for ego
#
# `dependency` filter answers Daniel's two framings directly, since
# computeCareSupply() already computes both without any extra work here:
#   "dependent"    of the egos who currently need care, where does the
#                  shortfall come from (the original question)
#   "total"        the same decomposition across ALL egos, not just those
#                  currently dependent (the follow-up: "for all egos, not
#                  only those that need care")
# ("notDependent" also exists in careSupplyDF if useful, not plotted below.)

supplyComponents <- c(
  partner_mort           = "Partner: no partner right now",
  partner_dep            = "Partner: partner is dependent",
  partner_work           = "Partner: partner is working",
  partner_comp           = "Partner: partner's hours claimed by others first",
  partner_ego            = "Partner: hours actually available to ego",
  children_mort          = "Children: no children right now",
  children_dep           = "Children: children are dependent",
  children_work          = "Children: children are working",
  children_comp_partner  = "Children: claimed by ego's partner first",
  children_comp_siblings = "Children: claimed by ego's other children first",
  children_comp_children = "Children: claimed by grandchildren/children's partner first",
  children_ego           = "Children: hours actually available to ego"
)

supplyPlot <- function(component, dependencyFilter) {
  df <- careSupplyDF[careSupplyDF$dependency == dependencyFilter, ]
  curves <- relabelSources(BS_curves(df, component))
  plotIt(curves, "T", labelY = supplyComponents[[component]], diffLineType = TRUE)
}

# Named lists of ggplot objects, one per component -- e.g.
# pSupplyDependent[["partner_comp"]] or pSupplyTotal[["children_work"]].
# ggsave(..., plot = pSupplyDependent[["partner_mort"]]) to export any one.
pSupplyDependent <- lapply(names(supplyComponents), supplyPlot, dependencyFilter = "dependent")
names(pSupplyDependent) <- names(supplyComponents)

pSupplyTotal <- lapply(names(supplyComponents), supplyPlot, dependencyFilter = "total")
names(pSupplyTotal) <- names(supplyComponents)

message("See Sections 3-4 comments in this file for ready-to-adapt example plots.")
message("pSupplyDependent / pSupplyTotal: named lists of care-supply-decomposition plots, one per component (see supplyComponents for the full list).")
