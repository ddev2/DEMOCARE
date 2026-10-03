# computeCare -- Base R version #########################################
#
# This script is a pedagogical, working R translation of the "computeCare"
# NetLogo module described in "Democare 2026.docx" (sections "Simulating
# the need for care" and "Rules for giving or receiving care in the
# family network").
#
# GOAL OF THIS SCRIPT
#   For a population of simulated family networks (produced by KINFERT),
#   simulate, year by year ("tick" by "tick"), each individual's:
#     - dependency level (0 = none .. 3 = high)
#     - labour force activity status (A = not active, P = part-time,
#       C = full-time)
#   and, for the persons flagged as "ego" (the reference 50-year-olds),
#   whether their need for care is covered by their family (informal
#   care) or not (formal care).
#
# HOW TO READ THIS SCRIPT
#   It is organised in numbered sections (see the RStudio outline,
#   Ctrl+Shift+O / Cmd+Shift+O, for quick navigation). Each section is a
#   self-contained piece: you can run it top to bottom the first time,
#   then jump around once you are familiar with it.
#
# WHAT IS DELIBERATELY SIMPLIFIED (see also the NOTE blocks below)
# >>> Claude 2026-10-02: separation and second unions
#   - Separation and second unions ARE supported (2026-10-02). A KINFERT
#     file made with the separation option has the same format; the only
#     difference is that a blood relative (ego, or one of ego's children)
#     can have several "M" links, one per union, never two at the same
#     time. Each partner is present only from tickIn to tickOut (tickOut
#     is the separation or the death), so at every tick the "current"
#     partner is simply the one who is active. See build_family_tree()
#     (Section 3), ego_lifetime_stats() (Section 3b) and run_one_tick()
#     (Section 8).
#   - Step-children are NOT included: KINFERT only writes the blood
#     relatives of ego, so every child of any of ego's partners is one of
#     ego's own children (Daniel, 2026-10-02). Rule 3 (step-children and
#     brothers/sisters-in-law kept apart) therefore never applies.
# <<< Claude 2026-10-02
#   - The "share care proportionally when both need it" rule (rule 4d)
#     is applied only between ego and ego's partner, exactly as the
#     documentation states it. One generation down (child vs child's
#     partner) we do not apply proportional sharing; see the
#     `cascade_care()` NOTE below.
#   - Only a streamlined set of output columns is written (not the full
#     ~40-column legacy NetLogo accounting table), as agreed with Daniel.
#   - The 3-generation family shape (ego+partner / children+their
#     partners / grandchildren) is hard-coded, matching what KINFERT
#     actually generates for DEMOCARE. A deeper or shallower tree would
#     need a more general (recursive) version of `cascade_care()`.
#
# 2026-07-28 UPDATE -- "care2" columns added
#   The original NetLogo model ("Modules/NETLOGO/src/CareUpdate.nls")
#   turns out to compute care sharing TWICE every tick, with two
#   different algorithms that both start from the same raw need/supply
#   number per person:
#     - "care1" (procedures explore_netCare_*): the older, scalar-only
#       algorithm; guarded by `if not care2Only? [...]`, i.e. it can be
#       switched off entirely.
#     - "care2" (procedures care2Couple / care2CoupleChildren /
#       care2Parents / care2Siblings, called from care2Update): a
#       generation-by-generation cascade with proportional splitting
#       when both a parent and partner are dependent -- this is the
#       algorithm behind the worked example in the ODD protocol document
#       (80h/30h need, 90h supply -> 65.45h/24.55h split), and it ALSO
#       keeps a detailed ledger of exactly which relative gave care to
#       which other relative (t_care2From_list / t_care2To_list).
#   `cascade_care()` below was already written to match care2's logic
#   (generation-by-generation, proportional split when both dependent),
#   not care1's -- confirmed by tracing NetLogo's own output-writing code
#   (`printInfoEgo` in democare.nlogo): the columns it labels
#   "care2byPartner" / "care2byChildren" are literally the totals of
#   partner->ego and children->ego transfers from the care2 ledger, which
#   is exactly what this script's `carebyPartner` / `carebyChildren`
#   already compute. So care2byPartner/care2byChildren are written here
#   as aliases of carebyPartner/carebyChildren -- see the Section 7 and
#   Section 8 notes below for the full reasoning, and the project memory
#   for how this was traced through the NetLogo source.
#
#   This update also adds the five relationship-transfer columns that
#   feed the "careSupply" analysis in Modules/R PAPER/R code/
#   democare_compute.R (egoPtoChild, childToEgoP, childToPartner,
#   childToSiblings, childTograndCh), plus maritalStatus/nPartner/
#   nChildren (lifetime counts) and the ticks/who column names that
#   script expects. Output is now written tab-separated (*.dat) instead
#   of CSV, so it can be fed to that script directly.
#
#   NOT covered by this update: the ~20-column "k-hours" theoretical-
#   capacity accounting block (kTotalHours, kPartnerHours, kCompetition_*,
#   etc.) that democare_compute.R's careSupply()/plotSupply() needs for
#   the "supply decomposition" plots (roughly half of democare_graphs.R's
#   figures). That block comes from a different part of the NetLogo model
#   (egoAccountsActualize/egoAccountsActualize2) and is a separate,
#   larger piece of work -- see the project memory note.
#
# 2026-08-13 UPDATE -- the 14-column "k-hours" block is now implemented
#   (kPartnerHours, kTotalHours, kNoPartner_mort, kNoChildren_mort,
#   kPartnerOthers, kWork_partner, kWork_children, kDependency_partner,
#   kDependency_children, kDependency_health_children,
#   kDependency_age_children, kCompetition_partner, kCompetition_children,
#   kCompetition_children_family), so createDemoCareFiles()'s careSupply()
#   step and democare_graphs.R's plotSupply() ("supply decomposition")
#   figures now work directly off this script's *.dat output -- see
#   Section 6b (deficit_hours(), the per-person "hours lost to youth/
#   dependency/work" breakdown, NetLogo's deficitCare()/care2Kind()) and
#   Section 7c (the 14 columns themselves, NetLogo's
#   egoAccountsActualize2() -- the care2Only? variant, matching the rest
#   of this script) for the full derivation and the two verified
#   simplifications (an age-specific table lookup that is provably flat
#   and equal to MAX_CARE_HOURS for this dataset; CareFromChildrenInLaw?
#   confirmed OFF in Daniel's NetLogo interface file).
#
#   One NetLogo quirk ported AS-IS rather than "fixed": kNoChildren_mort's
#   formula appears to double-count not-yet-born children (see Section
#   7c's comment there) -- flag if the resulting plots look off.

library(utils)  # read.csv, read.table -- part of base R, loaded for clarity

# Make the relative paths below work regardless of where R's working
# directory happens to be when you hit Source/Run -- jump to this
# script's own folder first. (Needs the script to be the active tab in
# RStudio; running via Rscript/a terminal instead, cd to
# Modules/computeCare_R/ yourself before running it.)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# ==== 1. File locations ====

# All paths are relative to this script's own folder
# (Modules/computeCare_R/) inside the DemoCare 2026 project.
# Point DATA_DIR elsewhere (e.g. the full 10,000-ego run) to reuse this
# script unchanged.

KINFERT_DIR <- file.path("..", "KINFERT", "output")
NETLOGO_DIR <- file.path("..", "NETLOGO", "inputs")

PATH_POPULATION   <- file.path(KINFERT_DIR, "Population 0.1.txt")
PATH_LINKS        <- file.path(KINFERT_DIR, "links 0.1.txt")
PATH_DEPENDENCY   <- file.path(NETLOGO_DIR, "dependency 0.1.csv")
PATH_DEP_TRANS    <- file.path(NETLOGO_DIR, "dependency_transitions 0.1.csv")
PATH_ACTIVITY     <- file.path(NETLOGO_DIR, "activity 0.3.csv")
PATH_ACT_TRANS    <- file.path(NETLOGO_DIR, "act_transitions.csv")
PATH_CARE_HOURS   <- file.path(NETLOGO_DIR, "care 0.1.csv")

MAX_TABLE_AGE <- 105  # the dependency/activity/care tables stop at 105;
                       # anyone older is looked up as if they were 105.

# >>> Claude 2026-10-02: separation and second unions
# How an ego whose last partner LEFT (rather than died) is reported.
#   FALSE (default): maritalStatus = 3 ("separated") and the partner's
#                    60 hours are counted in kNoPartner_div.
#   TRUE           : reproduce NetLogo, which has no "separated" status:
#                    maritalStatus = 2 ("widowed") and the 60 hours are
#                    counted in kNoPartner_mort; kNoPartner_div stays 0.
# NOTE: democare_compute.R only builds single/married/widow shares from
# codes 0/1/2, so with FALSE a separated ego is in none of the three.
SEPARATION_AS_WIDOWHOOD <- FALSE
MARITAL_SEPARATED <- 3L
# <<< Claude 2026-10-02


# ==== 2. Reading the input files ====

# --- 2.1 Population and links -------------------------------------

read_population <- function(path) {
  # >>> Claude 2026-09-29
  # Fail early, with a readable message, if a numeric column was read
  # as text (for example, rows with a different number of fields, as
  # happens when an idFamily column is present only on some rows).
  # Without this check the run stops much later with "non-numeric
  # argument to binary operator".
  pop <- read.csv(path, stringsAsFactors = FALSE)
  num_cols <- intersect(c("idFamily", "id", "age", "ageDef", "tickIn", "tickOut"), names(pop))
  bad <- num_cols[!vapply(pop[num_cols], is.numeric, logical(1))]
  n_fields <- count.fields(path, sep = ",", quote = "\"", blank.lines.skip = TRUE)
  if (length(bad) > 0 || length(unique(n_fields)) > 1) {
    stop(sprintf(paste0("read_population(): '%s' is malformed. Column(s) read as text: %s. ",
                        "Rows have %s fields (header has %d). Check that every row has ",
                        "the same columns as the header."),
                 path, if (length(bad)) paste(bad, collapse = ", ") else "none",
                 paste(sort(unique(n_fields)), collapse = " or "), n_fields[1]),
         call. = FALSE)
  }
  pop
  # <<< Claude 2026-09-29
}

# IMPORTANT, NON-OBVIOUS POINT ABOUT THE "age" COLUMN:
# we first assumed "age" was everyone's age at tick 0, so that current
# age at tick t would simply be age + t. That holds for individuals who
# marry into the network (their "age" is a fictitious, back-extrapolated
# age at tick 0), but NOT for individuals who are born during the
# simulation (a child's "age" is recorded as 0, not as a negative
# tick-0 age). Using age + t for those rows silently gives the wrong
# current age.
#
# The one formula that works for EVERY row, regardless of how the
# individual entered the network, is to count back from their age at
# death:
#     current age at tick t  =  ageDef - (tickOut - t)
# This relies only on ageDef and tickOut, which are always reliable, so
# we use it everywhere instead of the "age" column. (Daniel: worth
# double-checking this against the NetLogo code if you still have it --
# this was reverse-engineered from the sample data, not read off a spec.)
# >>> Claude 2026-10-02: separation and second unions
# With separation, tickOut is no longer always the tick of death: a
# partner who separates leaves at the separation, while ageDef is still
# the age at death. "ageDef - (tickOut - t)" would then make a separated
# partner too old. So the formula is now split in two:
#   - born during the simulation (age == 0 and tickIn > 0): unchanged,
#     ageDef - (tickOut - t). Nobody born during the simulation is a
#     partner of a blood relative, so separation never affects them.
#   - everybody else: age + t, since "age" is the age at tick 0 (also
#     the back-extrapolated one for people who join later by union).
# Checked on Population_indKin.txt (65,672 rows, no separation): for
# every person not born during the simulation, age + tickOut == ageDef,
# so the two formulas give exactly the same ages on files without
# separation.
age_at_tick <- function(pop_rows, t) {
  born_during <- pop_rows$age == 0 & pop_rows$tickIn > 0
  ifelse(born_during, pop_rows$ageDef - (pop_rows$tickOut - t), pop_rows$age + t)
}
# <<< Claude 2026-10-02

read_links <- function(path) {
  # The links file has no header row: id1, id2, type ("M"=union,
  # "D"=descendant, always mother -> child), family number.
  read.csv(path, header = FALSE,
           col.names = c("id1", "id2", "type", "family"),
           stringsAsFactors = FALSE)
}

## Nicolas fix start ---------------------------------------------------
## Reason: Nicolas' review (rows Dep 01, Dep 02, Act 01, Act 02, Net
## careneed). Every lookup in this script is a match() on a composite key
## and silently takes the FIRST hit if that key is duplicated. Nothing in
## the current input files is duplicated, but a future file with an extra
## dimension (a CCAA region breakdown, say) would break the lookups
## without any visible error. This helper makes the assumption explicit
## and is called from each read_* function below.

check_unique_keys <- function(tab, key_cols, what) {
  key <- do.call(paste, c(tab[key_cols], sep = "\r"))
  dup <- duplicated(key)
  if (any(dup)) {
    stop(sprintf(
      "%s: %d duplicated lookup key(s) on (%s), e.g. \"%s\". Every lookup in this script takes the first match, so duplicated keys would silently bias the draws.",
      what, sum(dup), paste(key_cols, collapse = ", "),
      gsub("\r", " | ", key[dup][1], fixed = TRUE)))
  }
  invisible(TRUE)
}
## Nicolas fix end -----------------------------------------------------


# --- 2.2 Dependency tables ------------------------------------------

read_dependency_init <- function(path) {
  # Dep, Edu, Sex, CCAA, exactAge, N  (N = proportion; sums to 1 over Dep
  # for a given Edu/Sex/exactAge combination).
  tab <- read.csv(path, stringsAsFactors = FALSE)
  ## Nicolas fix start -------------------------------------------------
  ## Reason: Nicolas' review, row Dep 01. CCAA is not part of the lookup
  ## key (it is always "Total" today), so a regional file would produce
  ## several rows per individual.
  check_unique_keys(tab, c("Dep", "Edu", "Sex", "exactAge"),
                    "read_dependency_init()")
  ## Nicolas fix end ---------------------------------------------------
  tab
}

read_dependency_trans <- function(path) {
  # Edu, Sex, CCAA, Age, variable (p0_1 / p1_2 / p2_3), value
  # = probability of moving UP one dependency level before the next tick.
  tab <- read.csv(path, stringsAsFactors = FALSE)
  ## Nicolas fix start -------------------------------------------------
  ## Reason: Nicolas' review, row Dep 02. Same point as Dep 01: CCAA is
  ## not part of the key used by sample_dependency_transition().
  check_unique_keys(tab, c("Edu", "Sex", "Age", "variable"),
                    "read_dependency_trans()")
  ## Nicolas fix end ---------------------------------------------------
  tab
}

# --- 2.3 Activity tables ---------------------------------------------

read_activity_init <- function(path) {
  # Dep, Edu, Sex, Act (A/P/C), exactAge, N  (N sums to 1 over Act for a
  # given Dep/Edu/Sex/exactAge).
  tab <- read.csv(path, stringsAsFactors = FALSE)
  tab$exactAge <- as.numeric(tab$exactAge)  # strips the stray leading spaces
  ## Nicolas fix start -------------------------------------------------
  ## Reason: Nicolas' review, row Act 01.
  check_unique_keys(tab, c("Dep", "Edu", "Sex", "Act", "exactAge"),
                    "read_activity_init()")
  ## Nicolas fix end ---------------------------------------------------
  tab
}

read_activity_trans <- function(path) {
  # sex, edu, dep, origin, age, dest, value = P(origin -> dest) within one
  # tick (only the two "moving" probabilities are given per origin state;
  # the probability of staying is the remainder, see sample_activity()).
  tab <- read.csv(path, stringsAsFactors = FALSE)

  ## Nicolas fix start -------------------------------------------------
  ## Reason: Nicolas' review, rows Act 02 and Act Trans. The file stores
  ## the second destination as a CUMULATIVE threshold, so a file written
  ## in the raw (non-cumulative) convention would be read wrongly here
  ## with no visible error. This check makes that mistake noisy.
  ##
  ## It is deliberately a warning() and NOT a stopifnot(): four cells of
  ## the current act_transitions.csv legitimately exceed 1, all at age 64
  ## (F/A/dep0 from P = 1.058, M/B/dep0 from P = 1.040, F/A/dep2 from
  ## C = 1.506, F/B/dep2 from C = 1.222). The only consequence is that
  ## those individuals always change activity at that age, which is a
  ## harmless overshoot, not a reason to stop the simulation.
  check_unique_keys(tab, c("sex", "edu", "dep", "origin", "age", "dest"),
                    "read_activity_trans()")
  bad <- is.na(tab$value) | tab$value < 0
  if (any(bad)) {
    stop(sprintf("read_activity_trans(): %d transition value(s) are missing or negative.",
                 sum(bad)))
  }
  over <- tab$value > 1
  if (any(over)) {
    warning(sprintf(
      "read_activity_trans(): %d of %d cumulative threshold(s) exceed 1 (max %.3f). Those individuals always change activity. Expected: 4 cells at age 64. A much larger count usually means the file was written with RAW rather than cumulative probabilities.",
      sum(over), nrow(tab), max(tab$value)))
  }
  ## Nicolas fix end ---------------------------------------------------

  tab
}

# --- 2.4 Care hours table (Table 1 of the documentation) --------------

read_care_hours <- function(path) {
  # Wide format on disk: Act, Age, Dep0, Dep1, Dep2, Dep3.
  wide <- read.csv(path, stringsAsFactors = FALSE)

  # NOTE -- DATA INCONSISTENCY WORTH FLAGGING TO DANIEL:
  # care 0.1.csv codes part-time activity as "M", while activity 0.3.csv
  # and act_transitions.csv both code it as "P" (and use "C" for
  # full-time, "A" for not-active, consistently across all three). We
  # remap "M" -> "P" here so a single Act code is used everywhere in
  # this script. Please double-check "M" really does mean "part-time"
  # here and not something else (e.g. "married"/a leftover label) --
  # the hour values line up with Table 1's "Employed part-time" row,
  # which is what this remapping assumes.
  wide$Act[wide$Act == "M"] <- "P"

  # Reshape to long format: one row per (Act, Age, Dep), with the level
  # number extracted from the column name ("Dep0" -> 0, etc.). This is a
  # tiny, fixed-size reshape (4 dependency columns) so we just do it by
  # hand with rbind() rather than reaching for reshape()/melt().
  long <- do.call(rbind, lapply(0:3, function(d) {
    data.frame(
      Act    = wide$Act,
      Age    = wide$Age,
      Dep    = d,
      hours  = wide[[paste0("Dep", d)]],
      stringsAsFactors = FALSE
    )
  }))
  ## Nicolas fix start -------------------------------------------------
  ## Reason: Nicolas' review, row Net careneed. lookup_care_hours() takes
  ## the first match on (Act, Age, Dep).
  check_unique_keys(long, c("Act", "Age", "Dep"), "read_care_hours()")
  ## Nicolas fix end ---------------------------------------------------
  long
}


# ==== 3. Building the family tree ====

# We only need three generations, exactly as KINFERT produces them for
# DEMOCARE: ego's generation (ego + partner), the children's generation
# (ego's children + the children's partners, i.e. sons/daughters-in-law),
# and the grandchildren's generation. `build_family_tree()` walks the
# links table once (topology never changes over time; only who is
# currently "active" changes tick by tick) and returns one row per
# individual with:
#   generation      0 = ego/partner, 1 = child/in-law, 2 = grandchild
#   role            "ego" / "partner" / "child" / "inlaw" / "grandchild"
#   isBioLine       TRUE for individuals who are blood relatives of ego
#                   (ego, ego's children, grandchildren); FALSE for
#                   in-marrying partners (ego's partner, children's
#                   partners) -- this is what Rule 5 cares about.
#   parentBioId     id of this person's parent one generation up who is
#                   on the biological line (NA for generation 0)
#   partnerId       id of the current partner, if any (NA otherwise)

build_family_tree <- function(population, links) {

  marriages   <- links[links$type == "M", ]
  descendants <- links[links$type == "D", ]  # mother -> child

  # >>> Claude 2026-10-02: separation and second unions
  # A lookup of "who is (or was) in a union with whom". Each union is
  # stored as two symmetric rows (a->b and b->a); both directions are
  # added here anyway, so a future file that keeps only one link per
  # union (see the documentation) is read the same way. ALL unions are
  # kept, in file order: with the separation option, a blood relative
  # can have several partners over their life, one after the other.
  # Which of them is the CURRENT partner is decided tick by tick in
  # run_one_tick() (Section 8), from each partner's tickIn/tickOut.
  union_pairs <- unique(rbind(
    data.frame(a = marriages$id1, b = marriages$id2),
    data.frame(a = marriages$id2, b = marriages$id1)
  ))
  partners_by_id <- split(union_pairs$b, union_pairs$a)

  get_partners <- function(id) {
    key <- as.character(id)
    if (key %in% names(partners_by_id)) partners_by_id[[key]] else integer(0)
  }
  first_or_na <- function(x) if (length(x) == 0) NA_integer_ else x[1]

  # children_of_mother[id] = vector of the mother's children ids.
  children_by_mother <- split(descendants$id2, descendants$id1)
  get_children_of_mother <- function(mother_id) {
    if (is.na(mother_id)) return(integer(0))
    key <- as.character(mother_id)
    if (key %in% names(children_by_mother)) children_by_mother[[key]] else integer(0)
  }

  sex_of <- setNames(population$sex, population$id)

  # D-links are always recorded from the mother. A blood relative's
  # children are therefore the D-links of that person if female, or of
  # ALL of his partners if male. Since KINFERT only writes blood
  # relatives of ego (no step-children), every child of any of his
  # partners is his own child (Daniel, 2026-10-02).
  get_bio_children <- function(person_id, partner_ids) {
    mothers <- if (isTRUE(sex_of[as.character(person_id)] == "F")) person_id
               else partner_ids[sex_of[as.character(partner_ids)] %in% "F"]
    unique(unlist(lapply(mothers, get_children_of_mother)))
  }

  rows <- list()

  # Columns of every row below:
  #   partnerId      first partner in the file (NA if none). Kept for
  #                  reference only: the partner used at tick t is
  #                  recomputed in run_one_tick().
  #   everPartnered  TRUE if the person has at least one union in the
  #                  file (lifetime flag, same meaning as the old
  #                  "!is.na(partnerId)" test).
  egos <- population$id[population$ego]
  for (ego_id in egos) {

    ego_partners <- get_partners(ego_id)

    rows[[length(rows) + 1]] <- data.frame(
      id = ego_id, generation = 0L, role = "ego",
      isBioLine = TRUE, parentBioId = NA_integer_, partnerId = first_or_na(ego_partners),
      everPartnered = length(ego_partners) > 0
    )
    for (p_id in ego_partners) {
      rows[[length(rows) + 1]] <- data.frame(
        id = p_id, generation = 0L, role = "partner",
        isBioLine = FALSE, parentBioId = NA_integer_, partnerId = ego_id,
        everPartnered = TRUE
      )
    }

    children <- get_bio_children(ego_id, ego_partners)

    for (child_id in children) {
      child_partners <- get_partners(child_id)
      rows[[length(rows) + 1]] <- data.frame(
        id = child_id, generation = 1L, role = "child",
        isBioLine = TRUE, parentBioId = ego_id, partnerId = first_or_na(child_partners),
        everPartnered = length(child_partners) > 0
      )
      for (cp_id in child_partners) {
        rows[[length(rows) + 1]] <- data.frame(
          id = cp_id, generation = 1L, role = "inlaw",
          isBioLine = FALSE, parentBioId = ego_id, partnerId = child_id,
          everPartnered = TRUE
        )
      }

      grandchildren <- get_bio_children(child_id, child_partners)

      for (gc_id in grandchildren) {
        rows[[length(rows) + 1]] <- data.frame(
          id = gc_id, generation = 2L, role = "grandchild",
          isBioLine = TRUE, parentBioId = child_id, partnerId = NA_integer_,
          everPartnered = FALSE
        )
      }
    }
  }

  fam <- do.call(rbind, rows)

  # Overlapping unions: Daniel's rule is "more than one partner, never at
  # the same time". Check it once here, so a file that breaks the rule is
  # reported instead of silently handled. (If it ever happens,
  # run_one_tick() keeps the partner who entered last.)
  nb <- fam[!fam$isBioLine, c("id", "partnerId")]
  nb$tickIn  <- population$tickIn[match(nb$id, population$id)]
  nb$tickOut <- population$tickOut[match(nb$id, population$id)]
  nb <- nb[order(nb$partnerId, nb$tickIn), ]
  overlap <- FALSE
  if (nrow(nb) > 1) {
    same_person <- c(FALSE, nb$partnerId[-1] == nb$partnerId[-nrow(nb)])
    overlap <- same_person & c(FALSE, nb$tickIn[-1] < nb$tickOut[-nrow(nb)])
  }
  if (any(overlap)) {
    warning(sprintf(
      "build_family_tree(): %d union(s) start before the previous partner of the same person has left (e.g. person %s). The partner who entered last is used while both are present.",
      sum(overlap), nb$partnerId[which(overlap)[1]]), call. = FALSE)
  }
  n_multi <- sum(table(nb$partnerId) > 1)
  if (n_multi > 0) message(sprintf("Family tree: %d blood relative(s) with more than one partner.", n_multi))
  # <<< Claude 2026-10-02

  # Family number.
  #
  # As of 2026-08-11, population files may carry a leading "idFamily"
  # column (added so whole families can be pulled out/duplicated directly
  # from the population file for bootstrap resampling). read_population()
  # picks it up automatically by name if it's there -- read.csv() doesn't
  # care about column order. When it's there we trust it directly, which
  # also fixes the fallback below for isolated egos (see next paragraph).
  # Older population files (no idFamily column) fall back to the original
  # behaviour: family number recovered from the links table.
  if ("idFamily" %in% names(population)) {
    idfam_of_id <- setNames(population$idFamily, population$id)
    fam$familyId <- unname(idfam_of_id[as.character(fam$id)])
  } else {
    # Recovered from the links table (all rows belonging to one family
    # share the same 4th column). Every partnered or D-linked individual
    # appears somewhere in `links$id1`, so this covers everyone except a
    # completely isolated, partner-less, childless ego (who never appears
    # in `links` at all) -- for that rare case we fall back to using the
    # ego's own id as its family number.
    fam_of_id <- rbind(
      data.frame(id = marriages$id1, family = marriages$family),
      data.frame(id = descendants$id2, family = descendants$family)
    )
    fam_of_id <- fam_of_id[!duplicated(fam_of_id$id), ]
    fam$familyId <- fam_of_id$family[match(fam$id, fam_of_id$id)]
    fam$familyId[fam$role == "ego"] <-
      links$family[match(fam$id[fam$role == "ego"], links$id1)]
    isolated_ego <- fam$role == "ego" & is.na(fam$familyId)
    fam$familyId[isolated_ego] <- fam$id[isolated_ego]
  }

  rownames(fam) <- NULL
  fam
}

# small helper: x %||% y  returns x unless it is NA/empty, then y
`%||%` <- function(x, y) if (length(x) == 0 || is.na(x)) y else x


# ==== 3b. Per-ego lifetime statistics (constant over the whole run) ====

# A few of the columns the R PAPER analysis scripts expect (maritalStatus,
# nPartner, nChildren -- Table 3 of the documentation) describe the ego's
# WHOLE LIFETIME, unlike e.g. nLivingChildren (Section 8), which only
# counts who is alive right now. Lifetime facts never change once the
# family tree is built, so we compute them once here instead of inside
# the tick loop.
#
# >>> Claude 2026-10-02: separation and second unions
#   nPartner          lifetime number of partners (0, 1, or more when the
#                      KINFERT file was made with the separation option).
#   nChildren          lifetime number of children (the tree's topology
#                      never changes, so this is just a count of "child"
#                      rows per ego).
#   attr(, "unions")   one row per (ego, partner) union, with the
#                      partner's tickIn/tickOut and whether the partner
#                      left by SEPARATION (exit before death) rather
#                      than by death. maritalStatus (Section 8) is
#                      computed from it at every tick: single before the
#                      first union, married while a partner is active,
#                      then widowed or separated according to how the
#                      LAST partner left. With a single union and no
#                      separation this is exactly the old rule (single /
#                      married / widowed from the partner's tickIn and
#                      tickOut), which mirrors NetLogo's UpdateSurvival.
#
#   How a separation is recognised: tickOut is the year the partner left
#   the simulation (death OR separation, see the documentation), while
#   ageDef is the age at death. A partner who died at exit has
#   age + tickOut == ageDef; a partner with age + tickOut < ageDef was
#   still alive when leaving, so the union ended by separation.
# <<< Claude 2026-10-02

ego_lifetime_stats <- function(fam, pop) {
  egos <- fam[fam$role == "ego", ]

  nChildrenById <- table(fam$parentBioId[fam$role == "child"])
  nChildrenVec  <- as.integer(nChildrenById[as.character(egos$id)])
  nChildrenVec[is.na(nChildrenVec)] <- 0L

  # 2026-08-18: nInLaws, added for full NetLogo/R output parity at
  # Daniel's request -- lifetime count of ego's children's partners
  # (in-laws), mirroring nChildren exactly (same "table() over
  # parentBioId" pattern -- inlaw rows in `fam` [Section 3] already carry
  # parentBioId = the EGO's own id directly, same as child rows, per
  # build_family_tree()'s own construction). Matches NetLogo's t_nInLaws
  # [NetworkCreator.nls], which is likewise set once at family-tree build
  # time using kInactive (i.e. counting the full lifetime roster,
  # regardless of current active/dead status).
  nInLawsById <- table(fam$parentBioId[fam$role == "inlaw"])
  nInLawsVec  <- as.integer(nInLawsById[as.character(egos$id)])
  nInLawsVec[is.na(nInLawsVec)] <- 0L

  # >>> Claude 2026-10-02: separation and second unions
  unions <- fam[fam$role == "partner", c("partnerId", "id")]
  names(unions) <- c("egoId", "partnerId")
  p_row <- match(unions$partnerId, pop$id)
  unions$tickIn    <- pop$tickIn[p_row]
  unions$tickOut   <- pop$tickOut[p_row]
  unions$separated <- (pop$age[p_row] + pop$tickOut[p_row]) < pop$ageDef[p_row]
  rownames(unions) <- NULL

  nPartnerById <- table(factor(unions$egoId, levels = egos$id))

  stats <- data.frame(
    id              = egos$id,
    nPartner        = as.integer(nPartnerById[as.character(egos$id)]),
    nChildren       = nChildrenVec,
    nInLaws         = nInLawsVec,
    stringsAsFactors = FALSE
  )
  attr(stats, "unions") <- unions
  if (any(unions$separated)) {
    message(sprintf("Ego unions: %d of %d ended by separation.", sum(unions$separated), nrow(unions)))
  }
  stats
  # <<< Claude 2026-10-02
}


# ==== 4. Initial status at tick 0 (dependency and activity) ====

# Both draws follow the same recipe: look up the handful of category
# probabilities that apply to this individual's (age, sex, education
# [, dependency]) group, turn them into a cumulative distribution, draw
# one uniform random number per individual, and see which category it
# falls into. Because there are only ever 3-4 categories, we vectorise
# over *individuals* and simply loop over the (few) categories.

sample_categorical <- function(group_ids, prob_table, group_cols, category_col, prob_col, categories) {
  # group_ids     : data.frame with one row per individual, columns
  #                 matching `group_cols` (e.g. exactAge, Sex, Edu)
  # prob_table    : long-format probabilities (one row per group x category)
  # categories    : the possible category values, in a fixed order

  n <- nrow(group_ids)
  cum <- matrix(0, nrow = n, ncol = length(categories))
  key_ids  <- do.call(paste, c(group_ids[group_cols], sep = "\r"))
  key_tab  <- do.call(paste, c(prob_table[group_cols], sep = "\r"))

  running <- rep(0, n)
  for (k in seq_along(categories)) {
    idx <- match(paste(key_ids, categories[k]), paste(key_tab, prob_table[[category_col]]))
    p_k <- ifelse(is.na(idx), 0, prob_table[[prob_col]][idx])
    running <- running + p_k
    cum[, k] <- running
  }
  ## Nicolas fix start ---------------------------------------------------
  ## Reason: Nicolas' review, row Init Dep (comment G11). The line below
  ## forces the last category's cumulative probability to 1, which is
  ## right for rounding but also means that an individual whose group key
  ## is ABSENT from the table (all probabilities 0) is silently assigned
  ## the HIGHEST category, i.e. Dep = 3 or Act = "C". The input tables are
  ## complete today, so this never fires, but a table trimmed at a
  ## different age boundary would corrupt the draw without any error.
  uncovered <- running < 0.999
  if (any(uncovered)) {
    warning(sprintf(
      "sample_categorical(): %d individual(s) have no (or incomplete) coverage in the probability table on (%s); their probabilities sum to %.4f at worst. They are being assigned the last category, \"%s\". Check the table's age range.",
      sum(uncovered), paste(group_cols, collapse = ", "),
      min(running), as.character(categories[length(categories)])))
  }
  ## Nicolas fix end -----------------------------------------------------

  cum[, length(categories)] <- 1  # guard against rounding leaving a gap < 1

  draw <- runif(n)
  # `reached` is TRUE in column k once that category's cumulative
  # probability has caught up with (covers) this individual's random
  # draw. The first TRUE in each row is the chosen category -- exactly
  # what a cumulative-distribution draw is supposed to pick.
  reached <- cum >= draw
  chosen <- categories[max.col(reached, ties.method = "first")]
  chosen
}

init_dependency <- function(pop_rows, current_age, dep_init) {
  key <- data.frame(exactAge = pmin(pmax(current_age, 20), MAX_TABLE_AGE),
                     Sex = pop_rows$sex, Edu = pop_rows$status)
  sample_categorical(key, dep_init,
                      group_cols = c("exactAge", "Sex", "Edu"),
                      category_col = "Dep", prob_col = "N",
                      categories = 0:3)
}

init_activity <- function(pop_rows, current_age, act_init, dependency) {
  key <- data.frame(exactAge = pmin(pmax(current_age, 0), MAX_TABLE_AGE),
                     Sex = pop_rows$sex, Edu = pop_rows$status, Dep = dependency)
  sample_categorical(key, act_init,
                      group_cols = c("exactAge", "Sex", "Edu", "Dep"),
                      category_col = "Act", prob_col = "N",
                      categories = c("A", "P", "C"))
}


# ==== 5. Yearly transitions ====

# --- 5.1 Dependency: one-directional (can only move UP one level) -----

sample_dependency_transition <- function(current_dep, age, sex, edu, dep_trans) {
  # dependency level 3 is absorbing: nobody recovers or worsens further.
  can_move <- current_dep < 3
  new_dep <- current_dep

  var_name <- paste0("p", current_dep, "_", current_dep + 1)
  key_ids <- paste(pmin(pmax(age, 20), MAX_TABLE_AGE), sex, edu, var_name, sep = "\r")
  key_tab <- paste(dep_trans$Age, dep_trans$Sex, dep_trans$Edu, dep_trans$variable, sep = "\r")
  idx <- match(key_ids, key_tab)
  p_up <- ifelse(is.na(idx), 0, dep_trans$value[idx])

  moves_up <- can_move & (runif(length(current_dep)) < p_up)
  new_dep[moves_up] <- current_dep[moves_up] + 1
  new_dep
}

# --- 5.2 Activity: reversible, 3 states, 6 possible transitions -------

sample_activity_transition <- function(current_act, age, sex, edu, dep, act_trans) {
  other_states <- list(A = c("P", "C"), P = c("A", "C"), C = c("A", "P"))
  new_act <- current_act

  for (origin in names(other_states)) {
    is_here <- current_act == origin
    if (!any(is_here)) next

    dest1 <- other_states[[origin]][1]
    dest2 <- other_states[[origin]][2]

    lookup_p <- function(dest) {
      key_ids <- paste(sex[is_here], edu[is_here], dep[is_here], origin,
                        pmin(pmax(age[is_here], 0), MAX_TABLE_AGE), dest, sep = "\r")
      key_tab <- paste(act_trans$sex, act_trans$edu, act_trans$dep, act_trans$origin,
                        act_trans$age, act_trans$dest, sep = "\r")
      idx <- match(key_ids, key_tab)
      ifelse(is.na(idx), 0, act_trans$value[idx])
    }

    p1 <- lookup_p(dest1)
    p2 <- lookup_p(dest2)
    # act_trans stores dest2's value already as a cumulative threshold
    # (P(origin->dest1) + P(origin->dest2) -- see smooth_actividad.R,
    # "NetLogo program needed this"), so p2 itself is the upper cutoff.
    # cumulative thresholds: [0, p1) -> dest1, [p1, p2) -> dest2,
    # [p2, 1) -> stay in `origin`
    draw <- runif(sum(is_here))
    outcome <- ifelse(draw < p1, dest1, ifelse(draw < p2, dest2, origin))
    new_act[is_here] <- outcome
  }
  new_act
}


# ==== 6. Hours of care needed / available (Table 1 lookup) ====

lookup_care_hours <- function(activity, age, dependency, care_hours) {
  key_ids <- paste(activity, pmin(pmax(age, 0), MAX_TABLE_AGE), dependency, sep = "\r")
  key_tab <- paste(care_hours$Act, care_hours$Age, care_hours$Dep, sep = "\r")
  idx <- match(key_ids, key_tab)
  ## Nicolas fix start ---------------------------------------------------
  ## Reason: Nicolas' review, row Careneed hours (comment F15). Without
  ## this guard a missing (Act, Age, Dep) combination returns NA, which
  ## travels silently into rawCare and from there into every care total.
  ## The table is complete today, so this only fires on a bad input file.
  if (anyNA(idx)) {
    stop(sprintf(
      "lookup_care_hours(): %d (Act, Age, Dep) combination(s) are missing from the care hours table, e.g. \"%s\".",
      sum(is.na(idx)), gsub("\r", " | ", key_ids[is.na(idx)][1], fixed = TRUE)))
  }
  ## Nicolas fix end -----------------------------------------------------
  care_hours$hours[idx]
}

# --- 6b. "k-hours" theoretical-capacity accounting -------------------
# (NetLogo: kMaxCare / kAgeNoDep constants, deficitCare()/care2Kind() in
# CareUpdate.nls). These feed the 14 "kXxx" columns added in Section 7c,
# which Modules/R PAPER/R code/democare_compute.R's careSupply() needs
# for the "supply decomposition" plots in democare_graphs.R.

# kMaxCare: confirmed from care 0.1.csv directly -- the "not working
# (Act=A), not dependent (Dep0)" row for age 25 is 60 (NetLogo:
# `set kMaxCare item 0 item 0 item 25 tcareArray` in CareImporter.nls).
MAX_CARE_HOURS <- 60

# kAgeNoDep: "age from which the person is not dependent if healthy" --
# read directly off the NetLogo global's declared default (democare.nlogo:
# `set kAgeNoDep 16`).
AGE_NO_DEP <- 16

# deficit_hours(): NetLogo's deficitCare() + care2Kind() decompose each
# individual's own raw care hours into how much of their theoretical
# MAX_CARE_HOURS ceiling was "lost" to three competing reasons: being too
# young to help (cr_young), being dependent themselves (cr_dep), or being
# occupied with paid work (cr_work). Ported line-for-line from
# deficitCare() [CareUpdate.nls]:
#   - if raw_care == MAX_CARE_HOURS (i.e. Act="A"/not-working AND Dep=0):
#     nothing lost, all three are 0.
#   - if age <= AGE_NO_DEP: the ENTIRE ceiling is lost to being too young
#     (cr_young = MAX_CARE_HOURS); cr_dep/cr_work stay 0. (Ages this
#     young never reach the branches below.)
#   - if dependency > 1 (levels 2-3, "high" dependency): the ENTIRE
#     ceiling is lost to dependency (cr_dep = MAX_CARE_HOURS).
#   - otherwise (age > AGE_NO_DEP, dependency 0 or 1): split the loss
#     between dependency and work by comparing three numbers at the same
#     age: this person's actual hours, their hours if they had the same
#     activity but were NOT dependent, and the ceiling if they were ALSO
#     not working.
#
# VERIFIED SIMPLIFICATION: NetLogo's own formula for that third number
# looks up an age-specific "not working (Act=A), not dependent (Dep0)"
# value from the care-hours table (`gCareArray[age][Act=A][Dep=0]`)
# rather than reusing the flat MAX_CARE_HOURS constant -- in principle
# these need not be equal at every age. We checked care 0.1.csv directly:
# for every age above AGE_NO_DEP (17 to 105), the Act=A/Dep0 column is a
# flat 60, i.e. identical to MAX_CARE_HOURS (ages 0-16 are lower/negative,
# but never reach this branch anyway, per the AGE_NO_DEP short-circuit
# above). So we use the constant directly instead of a second table
# lookup -- this is a verified equivalence for this dataset, not a guess;
# re-check care 0.1.csv's Act=A/Dep0 column if it is ever replaced.
deficit_hours <- function(age, activity, dependency, raw_care, care_hours) {
  n <- length(age)
  cr_young <- rep(0, n)
  cr_dep   <- rep(0, n)
  cr_work  <- rep(0, n)

  at_max    <- raw_care == MAX_CARE_HOURS
  too_young <- !at_max & age <= AGE_NO_DEP
  high_dep  <- !at_max & !too_young & dependency > 1
  general   <- !at_max & !too_young & !high_dep

  cr_young[too_young] <- MAX_CARE_HOURS
  cr_dep[high_dep]    <- MAX_CARE_HOURS

  if (any(general)) {
    # this person's hours if they kept their actual activity but were
    # NOT dependent (dependency forced to 0)
    hours_if_not_dependent <- lookup_care_hours(activity[general], age[general], 0, care_hours)
    cr_dep[general]  <- hours_if_not_dependent - raw_care[general]
    cr_work[general] <- MAX_CARE_HOURS - hours_if_not_dependent
  }

  data.frame(cr_young = cr_young, cr_dep = cr_dep, cr_work = cr_work)
}


# ==== 7. The care-sharing rules ("cascade_care") ====

# This is the heart of the "Rules for giving or receiving care" section
# of the documentation. Because our family tree has a fixed depth, we
# apply the *same* two-step cascade twice: once for grandchildren versus
# their parents (generation 2 vs 1), and once for children versus ego
# (generation 1 vs 0). Both calls use this one function.
#
# For a set of "parent" individuals (each with an optional partner) and
# their own biological "kids" (grouped by parent id):
#   Rule 1 / 4a / 4b  -- kids' needs are met FIRST, out of the bio-parent's
#                        own available hours (not the in-law partner's),
#                        and -- for ego's own generation only, see
#                        `partner_gives_to_kids` below -- out of the
#                        partner's own hours too.
#   Rule 4c            -- if the parent her/himself needs care, it is met
#                        first by the partner's spare hours (whatever is
#                        left of them after `partner_gives_to_kids`).
#   Rule 4d            -- (only when `share_if_both_dependent = TRUE`,
#                        which we use for ego's own generation only) if
#                        parent AND partner both need care at once, they
#                        share the pool of kids' leftover hours in
#                        proportion to how many hours each of them needs.
#   Rule 2             -- unpartnered kids who need care are helped first
#                        by their unpartnered siblings' spare hours,
#                        before any leftover kids' pool is computed.
#   Rule 5             -- only the bio-parent's (never the in-law's) spare
#                        hours ever count as "available for kids" one
#                        generation down (grandkids), and only the
#                        parent's own children (never grandchildren or
#                        in-laws) ever count as "available for the
#                        parent".
#
# NOTE ON A SIMPLIFICATION: for the generation-1-vs-0 call, "parent" is
# ego and "kids" are ego's children. For the generation-2-vs-1 call,
# "parent" is each child (the bio-line member) and "kids" are that
# child's own children (the grandchildren); the in-law partner's hours
# are only used to cover the *parent's own* need (rule 4c), never passed
# further down to grandkids -- consistent with Rule 5.
#
# `partner_gives_to_kids` (added 2026-07-28, see the file header's "care2"
# note): NetLogo's care2 algorithm has BOTH ego and ego's partner look
# after ego's children before either one looks after the other
# (`care2CoupleChildren`, called for BOTH `agent` and `agentPartner`) --
# i.e. Rule 1 ("an individual with a partner looks first at whether their
# OWN children need care") applies to the partner too, not just to ego.
# We reproduce that for the ego-vs-children cascade (pass 2 in
# run_one_tick()) but deliberately NOT for the children-vs-grandchildren
# cascade (pass 1), where Rule 5 keeps the in-law partner's hours out of
# the grandkids' pool. The amount the partner ends up giving directly to
# the kids is returned as `given_to_kids_partner` -- this is exactly
# NetLogo's "egoPtoChild" output column.

cascade_care <- function(parent_id, partner_id, parent_raw, partner_raw,
                          kid_parent_id, kid_raw, kid_partnered,
                          share_if_both_dependent = FALSE,
                          partner_gives_to_kids = FALSE) {

  # ---- step a: split each person's raw hours into supply (+) / need (-)
  parent_supply  <- pmax(parent_raw, 0)
  parent_need    <- pmax(-parent_raw, 0)
  partner_supply <- ifelse(is.na(partner_raw), 0, pmax(partner_raw, 0))
  partner_need   <- ifelse(is.na(partner_raw), 0, pmax(-partner_raw, 0))

  kid_need   <- pmax(-kid_raw, 0)
  kid_supply <- pmax(kid_raw, 0)

  # ---- step b (Rule 2): unpartnered kids help each other first --------
  # We only let *unpartnered* kids pool hours together; a partnered kid's
  # need/supply passes through untouched (they lean on their own partner
  # via the next cascade_care() call one level up, not on siblings).
  kid_need_after_siblings   <- kid_need
  kid_supply_after_siblings <- kid_supply
  unpart <- !kid_partnered
  if (any(unpart)) {
    grp <- kid_parent_id[unpart]
    supply_pool <- ave(kid_supply[unpart], grp, FUN = sum)
    need_pool   <- ave(kid_need[unpart],   grp, FUN = sum)
    fill_ratio  <- ifelse(need_pool > 0,   pmin(1, supply_pool / need_pool), 0)
    use_ratio   <- ifelse(supply_pool > 0, pmin(1, need_pool / supply_pool), 0)
    kid_need_after_siblings[unpart]   <- kid_need[unpart]   * (1 - fill_ratio)
    kid_supply_after_siblings[unpart] <- kid_supply[unpart] * (1 - use_ratio)
  }

  # how much of each kid's own supply was actually used up helping an
  # unpartnered sibling -- this is NetLogo's "childToSiblings" (Table 3:
  # care given to their siblings by children). Kept per-kid here and
  # aggregated up to the parent below, the same way kid_need/kid_supply
  # already are.
  kid_given_to_siblings <- kid_supply - kid_supply_after_siblings

  # ---- aggregate kids' remaining need/supply up to their parent -------
  # (one number per parent; a parent with no kids at all -- e.g. a
  # childless ego, or a child with no children of their own -- simply
  # gets 0, which is why we special-case the "no kids" situation instead
  # of handing tapply() an empty vector.)
  if (length(kid_raw) == 0) {
    need_by_parent           <- rep(0, length(parent_id))
    supply_by_parent         <- rep(0, length(parent_id))
    siblings_given_by_parent <- rep(0, length(parent_id))
  } else {
    agg_need     <- tapply(kid_need_after_siblings,   kid_parent_id, sum)
    agg_supply   <- tapply(kid_supply_after_siblings, kid_parent_id, sum)
    agg_sibgiven <- tapply(kid_given_to_siblings,     kid_parent_id, sum)
    # unname() here is important: tapply() returns a *named* vector, and
    # parent_id is NA for anyone with no bio-parent (root generation) or
    # simply absent from kid_parent_id's groups (no kids). Indexing a named
    # vector by such a lookup produces result elements whose *name* is NA
    # (not just whose value is NA) -- the is.na() cleanup two lines below
    # only fixes the values, not the leftover NA names, and those NA names
    # later crash data.frame() with "row names contain missing values"
    # when siblings_given_by_parent is passed straight through as a column.
    need_by_parent           <- unname(agg_need[as.character(parent_id)])
    supply_by_parent         <- unname(agg_supply[as.character(parent_id)])
    siblings_given_by_parent <- unname(agg_sibgiven[as.character(parent_id)])
    need_by_parent[is.na(need_by_parent)]                     <- 0
    supply_by_parent[is.na(supply_by_parent)]                 <- 0
    siblings_given_by_parent[is.na(siblings_given_by_parent)] <- 0
  }

  # ---- step c (Rule 1 / 4a / 4b): bio-parent's OWN supply serves kids -
  given_to_kids       <- pmin(parent_supply, need_by_parent)
  parent_supply_left  <- parent_supply - given_to_kids
  need_still_for_kids <- need_by_parent - given_to_kids

  # ---- step c2 (NEW -- `partner_gives_to_kids` only): the partner's own
  # supply also serves whatever of the kids' need the parent couldn't
  # cover, BEFORE the partner's remaining hours go to help the parent
  # (step e) -- see the function header note above.
  if (partner_gives_to_kids) {
    given_to_kids_partner <- pmin(partner_supply, need_still_for_kids)
  } else {
    given_to_kids_partner <- rep(0, length(parent_id))
  }
  partner_supply_left <- partner_supply - given_to_kids_partner

  # ---- step d (Rule 4b, continued): leftover parent supply serves partner
  given_to_partner      <- pmin(parent_supply_left, partner_need)
  parent_supply_left2   <- parent_supply_left - given_to_partner
  partner_need_after_parent <- partner_need - given_to_partner

  # ---- step e (Rule 4c): partner's OWN (remaining) supply serves the parent
  # (the in-law/partner never gives to the grandkids directly -- Rule 5)
  given_to_parent_from_partner <- pmin(partner_supply_left, parent_need)
  parent_need_after_partner <- parent_need - given_to_parent_from_partner

  # ---- step f (Rules 4c/4d/5): kids' pool serves remaining need -------
  if (share_if_both_dependent) {
    # Rule 4d: if BOTH the parent and the partner still need care, they
    # share the (bio-)kids' pool in proportion to their remaining needs
    # -- this mirrors the worked example in the ODD protocol document
    # (80h vs 30h needed, 90h available -> split 65h27m / 24h32m).
    total_need  <- parent_need_after_partner + partner_need_after_parent
    total_given <- pmin(supply_by_parent, total_need)
    parent_receives_from_kids  <- ifelse(total_need > 0, total_given * parent_need_after_partner  / total_need, 0)
    partner_receives_from_kids <- ifelse(total_need > 0, total_given * partner_need_after_parent / total_need, 0)
  } else {
    # one level down (children vs grandchildren): only the bio-parent
    # draws on the kids' pool, never the in-law partner (Rule 5).
    parent_receives_from_kids  <- pmin(parent_need_after_partner, supply_by_parent)
    partner_receives_from_kids <- 0
  }

  parent_net_need  <- parent_need_after_partner  - parent_receives_from_kids
  partner_net_need <- partner_need_after_parent  - partner_receives_from_kids

  data.frame(
    parent_id  = parent_id,
    partner_id = partner_id,
    given_to_kids          = given_to_kids,           # Rules 1/4a/4b: parent -> kids = NetLogo's "childTograndCh" (pass 1) / internal (pass 2)
    given_to_kids_partner  = given_to_kids_partner,    # NEW: partner -> kids (pass 2 only) = NetLogo's "egoPtoChild"
    given_to_partner       = given_to_partner,         # Rule 4b: parent -> partner = NetLogo's "childToPartner" (pass 1)
    siblings_given         = siblings_given_by_parent, # Rule 2, aggregated: kids -> their siblings = NetLogo's "childToSiblings" (pass 2)
    supply_up              = parent_supply_left2,      # bio-parent's leftover hours, available one level further up
    carebyPartner           = given_to_parent_from_partner,  # Rule 4c -- also NetLogo's "care2byPartner", see file header note
    carebyChildren           = parent_receives_from_kids,     # Rules 4c/5 -- also NetLogo's "care2byChildren", see file header note
    netCareDemand           = parent_net_need,                # still unmet -> formal care
    partner_carebyPartner   = given_to_partner,
    partner_carebyChildren   = partner_receives_from_kids,     # NetLogo's "childToEgoP" (pass 2)
    partner_netCareDemand    = partner_net_need
  )
}


# ==== 8. One tick of the simulation ====

# Runs the two cascade_care() passes (grandchildren -> children, then
# children -> ego) for every family that is still active at tick `t`,
# and returns one row per EGO with the streamlined set of output
# columns agreed with Daniel, extended (2026-07-28) with the care2 /
# relationship-transfer / lifetime columns the R PAPER analysis scripts
# expect -- see the file header note.

run_one_tick <- function(t, pop, fam, dep_trans, act_trans, care_hours, ego_stats) {

  # BUG FIX (2026-08-16): this was `t <= pop$tickOut` (inclusive of the tick
  # of death) -- but NetLogo's UpdateSurvival [democare.nlogo] kills an agent
  # (`die`) the moment `ticks >= tickOut`, and does so BEFORE that same
  # tick's computeCare/care2Update cascade runs (both are in the same
  # per-tick `ask actives [...]` sequence, survival first). So in NetLogo an
  # individual can give/receive care only for ticks strictly LESS than their
  # own tickOut -- this file's own marital-status logic (~line 899,
  # `t < es$partner_tickOut`) already used that exclusive convention; this
  # "active" filter didn't, which was self-inconsistent within this file
  # AND out of step with NetLogo. Found while diagnosing why NetLogo's
  # Care2ByChildren ran higher than this port's -- verified this specific
  # fix pushes the SAME direction (this port's numbers go lower still, per
  # a real before/after comparison run against the sample population), so
  # it does not explain that gap, but it's a genuine correctness fix on its
  # own terms, independent of that investigation.
  active <- pop$tickIn <= t & t < pop$tickOut
  if (!any(active & pop$ego)) return(NULL)  # no ego left alive -> caller stops

  cur <- pop[active, c("id", "sex", "age", "status")]
  cur$currentAge  <- age_at_tick(pop[active, ], t)
  cur$dependency  <- pop$dependency[active]
  cur$activity    <- pop$activity[active]
  cur$rawCare     <- lookup_care_hours(cur$activity, cur$currentAge, cur$dependency, care_hours)

  # Section 7c's k-hours accounting needs each active individual's own
  # "hours lost to youth/dependency/work" breakdown (Section 6b) -- computed
  # once here for everyone currently active, then carried through fam_cur
  # like rawCare is, so it is available for whichever role (partner, child)
  # ends up needing it below.
  cr <- deficit_hours(cur$currentAge, cur$activity, cur$dependency, cur$rawCare, care_hours)
  cur$cr_young <- cr$cr_young
  cur$cr_dep   <- cr$cr_dep
  cur$cr_work  <- cr$cr_work

  # 2026-08-18: also carry dependency/activity through to fam_cur (they
  # were already computed on `cur` above but not previously passed along)
  # -- needed below to summarise each ego's children's OWN dependency and
  # activity status (nDependentChildren/nChildrenActivityC/
  # nChildrenActivityP), added at Daniel's request to help localize
  # NetLogo-vs-R Care2ByChildren discrepancies by ego age.
  fam_cur <- merge(fam, cur[, c("id", "rawCare", "cr_young", "cr_dep", "cr_work", "dependency", "activity")],
                    by = "id", all.x = FALSE)  # inactive members drop out

  # >>> Claude 2026-10-02: separation and second unions
  # The CURRENT partner of each blood relative (ego or child) at tick t:
  # among that person's partners, the one who is active now. fam_cur only
  # holds active people, so this is just "the partner row still present
  # in fam_cur". A separated or dead partner has already dropped out, and
  # a future one has not entered yet, so both give NA (no partner now),
  # which every lookup below already treats as "no partner". If two
  # partners were ever active at once (see build_family_tree()'s
  # warning), the one who entered last is kept.
  # Without separation this gives the same partner as before whenever
  # that partner is active, and the lookups below already ignored an
  # inactive partner, so results on old files do not change.
  nb_cur <- fam_cur[!fam_cur$isBioLine, c("id", "partnerId")]
  nb_cur <- nb_cur[order(nb_cur$partnerId, -pop$tickIn[match(nb_cur$id, pop$id)]), ]
  nb_cur <- nb_cur[!duplicated(nb_cur$partnerId), ]
  bio_now <- fam_cur$isBioLine & fam_cur$role %in% c("ego", "child")
  fam_cur$partnerId[bio_now] <- nb_cur$id[match(fam_cur$id[bio_now], nb_cur$partnerId)]
  # <<< Claude 2026-10-02

  # ---- pass 1: grandchildren vs. children --------------------------
  children  <- fam_cur[fam_cur$role == "child", ]
  grandkids <- fam_cur[fam_cur$role == "grandchild", ]
  inlaw_raw <- setNames(fam_cur$rawCare[fam_cur$role == "inlaw"], fam_cur$id[fam_cur$role == "inlaw"])

  pass1 <- cascade_care(
    parent_id      = children$id,
    partner_id     = children$partnerId,
    parent_raw     = children$rawCare,
    partner_raw    = unname(inlaw_raw[as.character(children$partnerId)]),
    kid_parent_id  = grandkids$parentBioId,
    kid_raw        = grandkids$rawCare,
    kid_partnered  = rep(FALSE, nrow(grandkids)),  # grandchildren's own unions are out of scope here
    share_if_both_dependent = FALSE,
    partner_gives_to_kids   = FALSE  # Rule 5: in-laws never give to grandkids directly
  )

  # each child's "effective" contribution one level up: their own
  # leftover supply if they are not dependent, or their still-unmet need
  # (as a negative number) if they are -- see Section 7's header note.
  children$effectiveRaw   <- ifelse(pass1$netCareDemand > 0, -pass1$netCareDemand, pass1$supply_up)
  # >>> Claude 2026-10-02: lifetime flag, as before (partnerId is now the
  # CURRENT partner, see above). Rule 2's "unpartnered sibling" keeps its
  # old meaning, "never had a partner", so results on files without
  # separation do not change. A separated child who has not repartnered
  # therefore does NOT join the unpartnered siblings' pool: to be
  # confirmed by Daniel.
  children$hasPartner      <- children$everPartnered
  # <<< Claude 2026-10-02
  # NetLogo's "childTograndCh" and "childToPartner" (Table 3): each
  # child's own contribution down to grandkids, and to their own partner.
  children$givenToGrandCh <- pass1$given_to_kids
  children$givenToPartner <- pass1$given_to_partner

  # ---- pass 2: children vs. ego/partner -----------------------------
  egos     <- fam_cur[fam_cur$role == "ego", ]
  partners <- setNames(fam_cur$rawCare[fam_cur$role == "partner"], fam_cur$id[fam_cur$role == "partner"])

  pass2 <- cascade_care(
    parent_id      = egos$id,
    partner_id     = egos$partnerId,
    parent_raw     = egos$rawCare,
    partner_raw    = unname(partners[as.character(egos$partnerId)]),
    kid_parent_id  = children$parentBioId,
    kid_raw        = children$effectiveRaw,
    kid_partnered  = children$hasPartner,
    share_if_both_dependent = TRUE,
    partner_gives_to_kids   = TRUE  # Rule 1 applies to ego's partner too (NetLogo's care2CoupleChildren)
  )

  # ---- assemble the streamlined output row per ego -------------------
  out <- merge(egos[, c("id", "familyId")], pop[active, c("id", "sex", "status")], by = "id")
  out$ticks       <- t
  out$age         <- age_at_tick(pop[match(out$id, pop$id), ], t)
  # 2026-08-18: renamed from "dependency" to "egoDependencyLevel" at
  # Daniel's request, so it reads unambiguously as the EGO's own
  # dependency level (0-3) when pivot-tabling against the various
  # kDependency_* columns below, which are about the partner's/children's
  # dependency, not the ego's.
  out$egoDependencyLevel <- pop$dependency[match(out$id, pop$id)]
  out$activity    <- pop$activity[match(out$id, pop$id)]
  # >>> Claude 2026-10-02: lifetime flag, like NetLogo's t_hasPartnerEgo
  # (partnerId is now the CURRENT partner, see above)
  out$hasPartner  <- egos$everPartnered[match(out$id, egos$id)]
  # <<< Claude 2026-10-02
  # 2026-08-18: renamed from "nLivingChildren" to "nActiveChildren" for
  # full parity with NetLogo, which has always used "nActiveChildren"
  # for this exact concept (currently active/alive children) as part of
  # its nActiveX/nFutureActiveX/nDeadX naming family for partner,
  # children AND in-laws -- adopting NetLogo's naming here since this
  # script now also needs to add the matching nActivePartner/nDeadPartner/
  # nActiveInLaws/etc. columns below, and mixing "nLivingChildren" with
  # "nActivePartner" in the same file would be inconsistent.
  out$nActiveChildren <- as.integer(table(factor(children$parentBioId, levels = out$id))[as.character(out$id)])

  # -- lifetime / marital-status columns (Table 3: maritalStatus,
  # nPartner, nChildren) -- looked up from the pre-computed ego_stats
  # (Section 3b), since they describe the ego's whole lifetime, not just
  # who is alive right now.
  es <- ego_stats[match(out$id, ego_stats$id), ]
  # >>> Claude 2026-10-02: separation and second unions
  # Counts over ALL of the ego's unions (Section 3b's "unions" table):
  #   started  partners who have entered by tick t (tickIn <= t)
  #   active   partners present now (tickIn <= t < tickOut)
  #   future   partners not entered yet (t < tickIn)
  #   departed partners who have left (tickOut <= t), by death or by
  #            separation
  # maritalStatus: 0 single (no union started yet), 1 married (a partner
  # is active), otherwise 2 widowed or MARITAL_SEPARATED (3), according
  # to how the most recently departed partner left. With one union and
  # no separation, this is exactly the previous rule.
  un <- attr(ego_stats, "unions")
  un <- un[un$egoId %in% out$id, ]
  count_by_ego <- function(flag) {
    as.integer(table(factor(un$egoId[flag], levels = out$id))[as.character(out$id)])
  }
  n_started  <- count_by_ego(un$tickIn <= t)
  n_active   <- count_by_ego(un$tickIn <= t & t < un$tickOut)
  n_future   <- count_by_ego(t < un$tickIn)
  n_departed <- count_by_ego(un$tickOut <= t)

  gone <- un[un$tickOut <= t, ]
  gone <- gone[order(gone$egoId, -gone$tickOut), ]
  gone <- gone[!duplicated(gone$egoId), ]
  last_separated <- gone$separated[match(out$id, gone$egoId)] %in% TRUE
  code_after_separation <- if (SEPARATION_AS_WIDOWHOOD) 2L else MARITAL_SEPARATED

  out$maritalStatus <- ifelse(n_started == 0L, 0L,
                        ifelse(n_active > 0L, 1L,
                        ifelse(last_separated, code_after_separation, 2L)))
  # <<< Claude 2026-10-02
  out$nPartner  <- es$nPartner
  out$nChildren <- es$nChildren
  out$nInLaws   <- es$nInLaws

  # 2026-08-18: nActivePartner/nFutureActivePartner/nDeadPartner, added
  # at Daniel's request for full NetLogo/R output parity -- since this
  # script's family model has at most one lifetime partner (see the file
  # header note), these are just the 0/1 breakdown of that single
  # partner slot's current status, mirroring NetLogo's own
  # nActivePartner/nFutureActivePartner/nDeadPartner [calcCount,
  # CareUpdate.nls] exactly: not yet joined (nFutureActivePartner),
  # currently active (nActivePartner -- identical condition to
  # maritalStatus==1/married, computed independently here rather than
  # reused from out$maritalStatus purely for readability), or already
  # departed (nDeadPartner -- widowed/deceased, no remarriage modeled).
  # >>> Claude 2026-10-02: now counts over all unions (this replaces the
  # "at most one lifetime partner" description above). nDeadPartner counts
  # every partner who has LEFT (died or separated), which is also what
  # NetLogo would count, since a separated agent "dies" at its tickOut.
  out$nActivePartner       <- n_active
  out$nFutureActivePartner <- n_future
  out$nDeadPartner          <- n_departed
  # <<< Claude 2026-10-02

  out$care             <- egos$rawCare[match(out$id, egos$id)]
  out$carebyPartner     <- pass2$carebyPartner[match(out$id, pass2$parent_id)]
  out$carebyChildren    <- pass2$carebyChildren[match(out$id, pass2$parent_id)]
  out$netCareDemand    <- pass2$netCareDemand[match(out$id, pass2$parent_id)]
  out$netCareSupply    <- pass2$supply_up[match(out$id, pass2$parent_id)]
  out$formalCareNeeded <- pmax(out$netCareDemand, 0)  # hours per week not covered by informal care

  # 2026-08-18: care2Demand/care2Supply, added for full NetLogo/R output
  # parity -- IMPORTANT semantic note discovered while mapping this out:
  # NetLogo prints FOUR demand/supply-shaped columns (netCareDemand,
  # netCareSupply, care2Demand, care2Supply), but under Daniel's actual
  # run config (care2Only?=TRUE) NetLogo's plain netCareDemand/
  # netCareSupply are DEAD -- they belong to the older, disabled "care1"
  # cascade (t_netCareDemand/t_netCareSupply are only ever touched by
  # checkCare/explore_netCare_Parents, which never run under
  # care2Only?). NetLogo's care2Demand/care2Supply (t_care2Demand/
  # t_care2Supply) are the REAL, active, post-cascade leftover values --
  # confirmed by tracing care2firstCareSecond [CareUpdate.nls], which
  # decrements exactly these two variables as care is actually
  # transferred. This script's own netCareDemand/netCareSupply (just
  # above) are ALREADY the real, active values (this script only ever
  # implements the care2 algorithm), so they are what NetLogo's
  # care2Demand/care2Supply should be compared against -- care2Demand/
  # care2Supply here are simply duplicates under NetLogo's naming, same
  # treatment as care2byPartner/care2byChildren already get. Do NOT
  # compare NetLogo's plain netCareDemand/netCareSupply against this
  # script's netCareDemand/netCareSupply -- that pairs a dead column
  # against a real one.
  out$care2Demand <- out$netCareDemand
  out$care2Supply <- out$netCareSupply

  # -- "care2" columns (see the file header note): computeCare_R only
  # ever implements the one cascade algorithm, which structurally
  # matches NetLogo's "care2" (not the older, scalar-only "care1"). So
  # care2byPartner/care2byChildren are, here, simply carebyPartner/
  # carebyChildren under the other name -- we still write both because
  # democare_graphs.R's default (careType <- 2) reads the "care2" names
  # specifically.
  out$care2byPartner  <- out$carebyPartner
  out$care2byChildren <- out$carebyChildren

  # -- the remaining relationship-transfer columns from Table 3 --------
  out$egoPtoChild     <- pass2$given_to_kids_partner[match(out$id, pass2$parent_id)]
  out$childToEgoP     <- pass2$partner_carebyChildren[match(out$id, pass2$parent_id)]
  out$childToSiblings <- pass2$siblings_given[match(out$id, pass2$parent_id)]

  childTograndCh_by_ego <- tapply(children$givenToGrandCh, children$parentBioId, sum)
  childToPartner_by_ego <- tapply(children$givenToPartner, children$parentBioId, sum)
  out$childTograndCh <- unname(childTograndCh_by_ego[as.character(out$id)])
  out$childTograndCh[is.na(out$childTograndCh)] <- 0
  out$childToPartner <- unname(childToPartner_by_ego[as.character(out$id)])
  out$childToPartner[is.na(out$childToPartner)] <- 0

  # 2026-08-18: nGrandChildren / nDependentChildren / nChildrenActivityC /
  # nChildrenActivityP, added at Daniel's request to help localize
  # NetLogo-vs-R Care2ByChildren discrepancies by ego age.
  #
  # nGrandChildren is a two-hop rollup, same shape as childTograndCh_by_ego
  # just above: grandkids$parentBioId is the CHILD's id (a grandkid's
  # parent is one of ego's children), so first count grandkids per child,
  # then sum those counts up to the ego via children$parentBioId (the
  # child's own parent = the ego). Nobody previously rolled this all the
  # way up to ego in either engine -- NetLogo's care2Update [CareUpdate.nls]
  # computes myGrandChildren per child too, but never sums it to ego either.
  grandkids_per_child <- as.integer(table(factor(grandkids$parentBioId, levels = children$id)))
  nGrandChildren_by_ego <- tapply(grandkids_per_child, children$parentBioId, sum)
  out$nGrandChildren <- unname(nGrandChildren_by_ego[as.character(out$id)])
  out$nGrandChildren[is.na(out$nGrandChildren)] <- 0L

  # nDependentChildren / nChildrenActivityC / nChildrenActivityP: how many
  # of an ego's currently-active children are themselves dependent (i.e.
  # competing for care rather than able to give it), and how many are in
  # each non-default labour-activity category. "A" (the default/remainder
  # category) is not broken out separately since it's always
  # nActiveChildren - nChildrenActivityC - nChildrenActivityP.
  nDependentChildren_by_ego <- tapply(children$dependency > 0, children$parentBioId, sum)
  out$nDependentChildren <- unname(nDependentChildren_by_ego[as.character(out$id)])
  out$nDependentChildren[is.na(out$nDependentChildren)] <- 0L

  nChildrenActivityC_by_ego <- tapply(children$activity == "C", children$parentBioId, sum)
  nChildrenActivityP_by_ego <- tapply(children$activity == "P", children$parentBioId, sum)
  out$nChildrenActivityC <- unname(nChildrenActivityC_by_ego[as.character(out$id)])
  out$nChildrenActivityC[is.na(out$nChildrenActivityC)] <- 0L
  out$nChildrenActivityP <- unname(nChildrenActivityP_by_ego[as.character(out$id)])
  out$nChildrenActivityP[is.na(out$nChildrenActivityP)] <- 0L

  # 2026-08-18: nActiveInLaws / nFutureActiveInLaws / nDeadInLaws, added
  # for full NetLogo/R output parity -- same "currently active / not yet
  # tickIn'd / already gone" breakdown already used for children just
  # below (Section 7c's nFutureActiveChildren block), applied to the
  # inlaw rows instead. fam_cur (this function's top) already only
  # contains rows for people currently active at tick t regardless of
  # role, so fam_cur's own inlaw rows ARE exactly the currently-active
  # in-laws -- no extra filtering needed for the "active" count, mirroring
  # how nActiveChildren above is computed straight from `children`
  # (itself just fam_cur filtered to role=="child").
  active_inlaws <- fam_cur[fam_cur$role == "inlaw", ]
  nActiveInLaws_by_ego <- table(factor(active_inlaws$parentBioId, levels = out$id))
  out$nActiveInLaws <- as.integer(nActiveInLaws_by_ego[as.character(out$id)])

  all_inlaws <- fam[fam$role == "inlaw", ]
  future_inlaws <- all_inlaws[pop$tickIn[match(all_inlaws$id, pop$id)] > t, ]
  n_future_inlaws_by_ego <- table(factor(future_inlaws$parentBioId, levels = out$id))
  out$nFutureActiveInLaws <- as.integer(n_future_inlaws_by_ego[as.character(out$id)])

  out$nDeadInLaws <- out$nInLaws - out$nActiveInLaws - out$nFutureActiveInLaws

  # nDeadChildren: NetLogo computes this directly (t_nDeadChildren =
  # t_nChildren - t_nActiveChildren - t_nFutureActiveChildren
  # [CareUpdate.nls's calcCount]) -- same arithmetic here, once
  # nFutureActiveChildren exists a few lines below (Section 7c), so this
  # assignment is completed there rather than here (see the
  # kNoChildren_mort block).

  # ---- Section 7c: the "k-hours" theoretical-capacity accounting -----
  # Ported from NetLogo's egoAccountsActualize2() [CareUpdate.nls] -- the
  # care2Only?-mode variant, since computeCare_R only ever implements the
  # care2 algorithm (see the file header note). These 14 columns feed
  # Modules/R PAPER/R code/democare_compute.R's careSupply(), which
  # democare_graphs.R's plotSupply() (the "supply decomposition" figures)
  # needs. Unlike the care-sharing cascade above, this is a separate
  # accounting exercise layered on top: it does not change who gets how
  # much care, it only explains, for each ego, how much of the partner's/
  # children's THEORETICAL 60h/week ceiling was "used up" by each of
  # several competing reasons (mortality, work, their own dependency,
  # care given elsewhere in the family) before whatever was left could
  # reach the ego.
  #
  # kTotalHours / kPartnerHours: the theoretical ceiling itself -- every
  # lifetime partner and lifetime (bio) child theoretically has
  # MAX_CARE_HOURS to give, regardless of whether they are alive/active
  # right now. NetLogo optionally adds nInLaws*MAX_CARE_HOURS here too if
  # the CareFromChildrenInLaw? switch is on; we confirmed it is OFF in
  # Daniel's interface file (democare.nlogo widget default), matching
  # this script's own CareFromChildrenInLaw?-excluded family tree
  # elsewhere, so that term is omitted here. Flag this if that switch is
  # ever turned on in NetLogo.
  out$kPartnerHours <- MAX_CARE_HOURS * as.integer(out$hasPartner)
  out$kTotalHours   <- out$kPartnerHours + out$nChildren * MAX_CARE_HOURS

  # kNoPartner_mort / kNoChildren_mort: ceiling hours lost because the
  # partner has died (widowed), or because some of the lifetime children
  # are not currently alive-and-born (dead, or not born yet). Ported
  # literally from NetLogo's own formula, which also adds back
  # nFutureActiveChildren -- i.e. "not yet born" children count toward
  # this loss TWICE (once via nChildren - nLivingChildren, once via the
  # explicit "+ nFutureActiveChildren" term). This looks like it may be a
  # quirk/double-count in the original NetLogo code, but we port it
  # as-is rather than silently "fixing" it -- flag to Daniel if the
  # supply-decomposition plots look off for cohorts with many not-yet-born
  # children.
  out$kNoPartner_mort <- MAX_CARE_HOURS * as.integer(out$maritalStatus == 2L)  # kWidowed
  # >>> Claude 2026-10-02: kNoPartner_div, NetLogo's "hours of care not
  # available due to divorce" slot (always 0 in NetLogo, which has no
  # separation). Non-zero only when SEPARATION_AS_WIDOWHOOD is FALSE.
  out$kNoPartner_div <- MAX_CARE_HOURS * as.integer(out$maritalStatus == MARITAL_SEPARATED)
  # <<< Claude 2026-10-02

  all_children <- fam[fam$role == "child", ]
  future_children <- all_children[pop$tickIn[match(all_children$id, pop$id)] > t, ]
  n_future_by_ego <- table(factor(future_children$parentBioId, levels = out$id))
  out$nFutureActiveChildren <- as.integer(n_future_by_ego[as.character(out$id)])
  out$kNoChildren_mort <- (out$nChildren - out$nActiveChildren + out$nFutureActiveChildren) * MAX_CARE_HOURS

  # nDeadChildren, completed here (see the comment left above, near
  # nActiveInLaws/nDeadInLaws) now that nFutureActiveChildren exists --
  # same "lifetime - active - future" arithmetic as nDeadInLaws, matching
  # NetLogo's calcCount [CareUpdate.nls].
  out$nDeadChildren <- out$nChildren - out$nActiveChildren - out$nFutureActiveChildren

  # kPartnerOthers: in NetLogo this is only ever populated by the older
  # "care1" algorithm (egoCareKind, guarded by `if not care2Only?`), which
  # never runs under care2Only?=TRUE -- confirmed as Daniel's actual
  # NetLogo run configuration (democare.nlogo interface default) and the
  # mode this whole script matches (see file header). So kPartnerOthers is
  # always exactly 0 in the NetLogo runs this script is meant to match --
  # not a simplification on our part, just how the upstream model itself
  # behaves in that mode.
  out$kPartnerOthers <- 0

  # kWork_partner / kWork_children / kDependency_partner /
  # kDependency_children (split into kDependency_age_children, the
  # "too young to help" share, and kDependency_health_children, the
  # "dependent" share): each active partner's / bio child's own
  # cr_work / cr_dep / cr_young (Section 6b), summed up to the ego. A
  # dead/non-existent partner or childless ego simply contributes 0 --
  # same unname()-before-lookup care as cascade_care() (Section 7): a
  # tapply()/vector lookup by id can otherwise leave stray NA *names* on
  # the result, not just NA values, which breaks downstream use.
  partner_cr_dep  <- unname(setNames(fam_cur$cr_dep[fam_cur$role == "partner"],
                                      fam_cur$id[fam_cur$role == "partner"])[as.character(egos$partnerId[match(out$id, egos$id)])])
  partner_cr_work <- unname(setNames(fam_cur$cr_work[fam_cur$role == "partner"],
                                      fam_cur$id[fam_cur$role == "partner"])[as.character(egos$partnerId[match(out$id, egos$id)])])
  out$kDependency_partner <- ifelse(is.na(partner_cr_dep),  0, partner_cr_dep)
  out$kWork_partner       <- ifelse(is.na(partner_cr_work), 0, partner_cr_work)

  children_cr_young_by_ego <- unname(tapply(children$cr_young, children$parentBioId, sum)[as.character(out$id)])
  children_cr_dep_by_ego   <- unname(tapply(children$cr_dep,   children$parentBioId, sum)[as.character(out$id)])
  children_cr_work_by_ego  <- unname(tapply(children$cr_work,  children$parentBioId, sum)[as.character(out$id)])
  out$kDependency_age_children    <- ifelse(is.na(children_cr_young_by_ego), 0, children_cr_young_by_ego)
  out$kDependency_health_children <- ifelse(is.na(children_cr_dep_by_ego),   0, children_cr_dep_by_ego)
  out$kWork_children               <- ifelse(is.na(children_cr_work_by_ego), 0, children_cr_work_by_ego)
  out$kDependency_children <- out$kDependency_age_children + out$kDependency_health_children

  # kCompetition_partner / kCompetition_children / kCompetition_children_family:
  # traced through NetLogo's t_care2CompCount ledger (egoAccountsActualize2,
  # cumulCompCare2/addCare2Comp in CareUpdate.nls) down to which named
  # relationship-transfer each slot holds -- and every one of them turns
  # out to be a column this script already computes above (each cross-
  # checked against this script's own NetLogo-name comments), so no new
  # cascade logic is needed, just these three sums:
  #   kCompetition_partner         = childToEgoP   (children's care given to ego's partner)
  #   kCompetition_children        = egoPtoChild + childToSiblings
  #   kCompetition_children_family = childToPartner + childTograndCh
  out$kCompetition_partner         <- out$childToEgoP
  out$kCompetition_children        <- out$egoPtoChild + out$childToSiblings
  out$kCompetition_children_family <- out$childToPartner + out$childTograndCh

  # 2026-08-18: remaining k-hours columns, added for full NetLogo/R output
  # parity. Traced through CareUpdate.nls's egoAccountsActualize2()
  # [lines ~656-713] and explore_totalCare() [lines ~138-187] -- the
  # care2Only?-mode-active versions, since this script only ever
  # implements the care2 algorithm (same rationale as the rest of
  # Section 7c above).
  #
  # kPartnerDependency / kPartnerWork: under care2Only?=TRUE, NetLogo sets
  # these to the SAME values it sets kDependency_partner/kWork_partner to
  # (egoAccountsActualize2 lines 677-678 vs 685; both read from
  # kDependency2_partner/kWork2_partner) -- i.e. under Daniel's actual run
  # config these are exact duplicates of the kDependency_partner/
  # kWork_partner columns already computed above, just kept under their
  # own names for parity (same treatment as care2byPartner/
  # care2byChildren elsewhere in this function).
  out$kPartnerDependency <- out$kDependency_partner
  out$kPartnerWork       <- out$kWork_partner

  # kTotalDependency / kTotalWork: NetLogo sums these directly from the
  # care2-sourced partner/children shares (egoAccountsActualize2 lines
  # 679-680), which is algebraically identical to summing this script's
  # own already-computed sibling columns.
  out$kTotalDependency <- out$kPartnerDependency + out$kDependency_children
  out$kTotalWork        <- out$kPartnerWork + out$kWork_children

  # kChildren_fert: every lifetime child's theoretical ceiling, regardless
  # of current status (egoAccountsActualize2 line 665) -- same lifetime
  # convention as kTotalHours/kPartnerHours above.
  out$kChildren_fert <- out$nChildren * MAX_CARE_HOURS

  # kTotalHoursLivings / kTotalAvailable / kPartnerAvailable: the
  # CURRENTLY-ACTIVE counterparts to the lifetime kTotalHours/
  # kPartnerHours ceiling -- explore_totalCare() sums kMaxCare (the
  # theoretical slot) and max(0, t_care) (the raw hours actually
  # generated, pre-cascade) across just the active partner + active bio
  # children. NetLogo would also include in-laws here if
  # CareFromChildrenInLaw? were TRUE (kCheckInLaws), but that switch is
  # confirmed OFF in Daniel's interface file (see the kTotalHours comment
  # above), so in-laws are excluded here too, consistent with the rest of
  # this script.
  partner_rawCare <- unname(setNames(fam_cur$rawCare[fam_cur$role == "partner"],
                                      fam_cur$id[fam_cur$role == "partner"])[as.character(egos$partnerId[match(out$id, egos$id)])])
  partner_avail <- ifelse(is.na(partner_rawCare), 0, pmax(0, partner_rawCare))
  children_avail_by_ego <- unname(tapply(pmax(0, children$rawCare), children$parentBioId, sum)[as.character(out$id)])
  children_avail_by_ego <- ifelse(is.na(children_avail_by_ego), 0, children_avail_by_ego)

  out$kTotalHoursLivings <- MAX_CARE_HOURS * (out$nActivePartner + out$nActiveChildren)
  out$kTotalAvailable    <- partner_avail + children_avail_by_ego
  out$kPartnerAvailable  <- partner_avail

  # kPartnerHoursLiving: unlike kPartnerHours (lifetime, uses hasPartner),
  # this is the CURRENT-status version -- NetLogo only sets it to kMaxCare
  # when t_maritalStatus = kMarried (egoAccountsActualize2 line 659-661;
  # explore_totalCare's own attempt to set it, line 165, is commented out
  # in the source and never runs) -- i.e. 0 for never-partnered AND for
  # widowed egos alike, not just never-partnered.
  out$kPartnerHoursLiving <- MAX_CARE_HOURS * as.integer(out$maritalStatus == 1L)

  # kDeficit: unmet demand after currently-active family hours are
  # accounted for (egoAccountsActualize2 lines 710-711, using the real
  # care2Demand under care2Only?=TRUE -- see the care2Demand/care2Supply
  # comment above for why care2Demand, not plain netCareDemand, is the
  # right pairing here).
  out$kDeficit <- pmax(0, out$care2Demand - out$kTotalHoursLivings)

  out
}


# ==== 9. Running the full simulation ====

run_computeCare <- function(population_path, links_path, dependency_path,
                             dep_trans_path, activity_path, act_trans_path,
                             care_hours_path, seed = 1, max_ticks = NULL) {

  set.seed(seed)

  message("Reading input files...")
  pop        <- read_population(population_path)
  links      <- read_links(links_path)
  dep_init   <- read_dependency_init(dependency_path)
  dep_trans  <- read_dependency_trans(dep_trans_path)
  act_init   <- read_activity_init(activity_path)
  act_trans  <- read_activity_trans(act_trans_path)
  care_hours <- read_care_hours(care_hours_path)

  ## Nicolas fix start ---------------------------------------------------
  ## Reason: Nicolas' review, row Sched 01 (comment F23). max_ticks used
  ## to be hardcoded at 90, so an ego living beyond tick 90 was silently
  ## cut off. The number of ticks the data actually needs is known once
  ## the population file has been read: it is the largest tickOut among
  ## the egos. max_ticks now defaults to that value. Passing max_ticks
  ## explicitly still works exactly as before (app.R and the bootstrap
  ## wrappers do), but a value that is too small now warns instead of
  ## truncating in silence.
  ## tickOut is EXCLUSIVE (an individual is no longer active on the tick
  ## they exit, see run_one_tick()), so the last tick at which an ego is
  ## still alive is max(tickOut) - 1.
  ticks_needed <- suppressWarnings(max(pop$tickOut[pop$ego], na.rm = TRUE)) - 1L
  if (!is.finite(ticks_needed)) ticks_needed <- 90
  if (is.null(max_ticks)) {
    max_ticks <- ticks_needed
    message(sprintf("max_ticks not supplied: using %d, the last tick at which an ego is still alive.",
                    max_ticks))
  } else if (max_ticks < ticks_needed) {
    warning(sprintf(
      "max_ticks = %d, but the last ego exits at tick %d. The simulation will be cut off and %d tick(s) of ego-years will be missing from the output.",
      max_ticks, ticks_needed, ticks_needed - max_ticks))
  }
  ## Nicolas fix end -----------------------------------------------------

  message("Building the family tree...")
  fam <- build_family_tree(pop, links)
  ego_stats <- ego_lifetime_stats(fam, pop)

  # dependency/activity start out unknown (NA) for everyone. We do NOT
  # draw them all at once "as of tick 0": someone who is born or who
  # marries into the network at tick 12 should get their initial status
  # drawn at THEIR age at tick 12, not inherit a meaningless tick-0
  # draw made at the wrong age (see the age_at_tick() note in Section 2).
  # So initial draws happen tick by tick, for whoever is newly entering
  # that tick -- see the "newcomers" step in the loop below.
  pop$dependency <- NA_integer_
  pop$activity   <- NA_character_

  results <- list()
  for (t in 0:max_ticks) {

    # ---- draw initial status for individuals entering this tick -----
    newcomers <- pop$tickIn == t
    if (any(newcomers)) {
      age_new <- age_at_tick(pop[newcomers, ], t)
      pop$dependency[newcomers] <- init_dependency(pop[newcomers, ], age_new, dep_init)
      pop$activity[newcomers]   <- init_activity(pop[newcomers, ], age_new, act_init,
                                                  pop$dependency[newcomers])
    }

    tick_out <- run_one_tick(t, pop, fam, dep_trans, act_trans, care_hours, ego_stats)
    if (is.null(tick_out)) {
      message(sprintf("Tick %d: no ego still alive -- simulation ends.", t))
      break
    }
    results[[length(results) + 1]] <- tick_out
    if (t %% 10 == 0) message(sprintf("Tick %d: %d egos still alive.", t, nrow(tick_out)))

    # ---- draw next year's dependency and activity status ------------
    # BUG FIX (2026-08-16): same tickOut boundary fix as in run_one_tick()
    # above (was `t <= pop$tickOut`) -- an individual is no longer "active"
    # on the tick they die (NetLogo kills them via `die` before that tick's
    # transitions/care run), so this must be exclusive too, for consistency
    # with run_one_tick()'s `active` filter and with this file's own
    # `t < es$partner_tickOut` marital-status convention (~line 914).
    active <- pop$tickIn <= t & t < pop$tickOut
    age_now <- age_at_tick(pop[active, ], t)

    pop$dependency[active] <- sample_dependency_transition(
      pop$dependency[active], age_now, pop$sex[active], pop$status[active], dep_trans)

    pop$activity[active] <- sample_activity_transition(
      pop$activity[active], age_now, pop$sex[active], pop$status[active],
      pop$dependency[active], act_trans)
  }

  out <- do.call(rbind, results)

  # Rename to the column names Modules/R PAPER/R code/democare_compute.R
  # and democare_graphs.R expect (Table 3 of the documentation calls the
  # ego's own id "who" -- confirmed from NetLogo's own output-writing
  # code, printInfoEgo in democare.nlogo, which prints the built-in
  # NetLogo `who` of the ego turtle being asked).
  names(out)[names(out) == "id"] <- "who"

  # 2026-08-18: fixed, explicit column order -- this used to be a
  # "firstCols, then whatever's left over" arrangement, which meant this
  # script's own column order could silently drift from
  # computeCare_tidyverse.R's (they had already diverged: same 41-column
  # set, different order -- confirmed while homogenizing both scripts'
  # output at Daniel's request, to make direct Excel/pivot-table
  # comparison against each other and against NetLogo's own per-ego
  # output file [printInfoEgo/gMainFileHeader in democare.nlogo] practical).
  # Both R scripts now use this SAME explicit list, in this SAME order --
  # if you add a column to one, add it to the other's matching list too
  # (computeCare_tidyverse.R's run_one_tick() final select()).
  # >>> Claude 2026-09-29
  # 2026-09-29: the tidyverse engine has been dropped; computeCare_
  # tidyverse.R is no longer maintained or distributed, so this list
  # now only needs to match NetLogo's printInfoEgo/gMainFileHeader.
  # <<< Claude 2026-09-29
  #
  # 2026-08-18 UPDATE: re-expanded to a full 65-column master order at
  # Daniel's explicit "exact parity" request -- the UNION of every column
  # either engine (NetLogo or R) ever produces, identical set/order in
  # NetLogo's printInfoEgo/gMainFileHeader [democare.nlogo] and in BOTH R
  # scripts, so Excel pivot tables can compare any of the three files
  # directly. Several columns kept here are exact duplicates of another
  # column under Daniel's actual care2Only?=TRUE NetLogo run config
  # (kPartnerDependency==kDependency_partner, kPartnerWork==kWork_partner,
  # care2Demand==netCareDemand, care2Supply==netCareSupply,
  # care2byPartner==carebyPartner, care2byChildren==carebyChildren) --
  # kept anyway because they exist as separate columns on the NetLogo
  # side and Daniel wants every column mirrored, not deduplicated.
  #
  # kPartnerOthers is kept even though it is always 0 under Daniel's
  # actual care2Only?=TRUE NetLogo configuration (see this file's own
  # comment where it's set, ~line 992) -- Modules/R PAPER/R code/
  # democare_compute.R's careSupply() reads it by exact name via
  # melt(tab, measure=listFields), so dropping it would break that
  # existing analysis script even though the column carries no
  # information in practice.
  outCols <- c(
    "ticks", "who", "familyId", "sex", "status", "age", "egoDependencyLevel", "activity", "hasPartner",
    "nGrandChildren", "maritalStatus", "nPartner", "nActivePartner", "nFutureActivePartner", "nDeadPartner",
    "nChildren", "nActiveChildren", "nFutureActiveChildren", "nDeadChildren",
    "nDependentChildren", "nChildrenActivityC", "nChildrenActivityP",
    "nInLaws", "nActiveInLaws", "nFutureActiveInLaws", "nDeadInLaws",
    "care", "netCareDemand", "netCareSupply", "formalCareNeeded", "care2Demand", "care2Supply",
    "carebyPartner", "carebyChildren", "care2byPartner", "care2byChildren",
    "egoPtoChild", "childToEgoP", "childToPartner", "childToSiblings", "childTograndCh",
    "kPartnerHours", "kTotalHours", "kTotalHoursLivings", "kTotalAvailable", "kTotalDependency", "kTotalWork",
    "kPartnerHoursLiving", "kPartnerAvailable", "kPartnerDependency", "kPartnerWork", "kPartnerOthers",
    # >>> Claude 2026-10-02: kNoPartner_div added after kNoPartner_mort
    "kNoPartner_mort", "kNoPartner_div", "kChildren_fert", "kNoChildren_mort", "kWork_partner", "kWork_children",
    # <<< Claude 2026-10-02
    "kDependency_partner", "kDependency_children", "kDependency_health_children", "kDependency_age_children",
    "kCompetition_partner", "kCompetition_children", "kCompetition_children_family", "kDeficit"
  )
  out <- out[, outCols]

  out
}


# ==== 10. Example run on the sample files ====

# This runs automatically whenever the script is sourced or run with
# Rscript. If you only want the functions defined above (e.g. to call
# run_computeCare() yourself with different paths), just comment this
# block out.
#
# 2026-08-19: guarded behind COMPUTECARE_NO_AUTORUN so that
# computeCare_baseR_bootstrap.R (Modules/computeCare_R/) can source()
# this file to reuse run_computeCare() and the read_*() helpers WITHOUT
# triggering this demo run every time -- set that variable to TRUE
# before sourcing to skip it. Default (variable unset) behaviour is
# unchanged: Source/Rscript-ing this file directly still runs the demo
# exactly as before.

if (!exists("COMPUTECARE_NO_AUTORUN") || !isTRUE(COMPUTECARE_NO_AUTORUN)) {

output <- run_computeCare(
  population_path  = PATH_POPULATION,
  links_path       = PATH_LINKS,
  dependency_path  = PATH_DEPENDENCY,
  dep_trans_path   = PATH_DEP_TRANS,
  activity_path    = PATH_ACTIVITY,
  act_trans_path   = PATH_ACT_TRANS,
  care_hours_path  = PATH_CARE_HOURS,
  seed = 1
)

message(sprintf("Simulation finished: %d ego-year rows produced.", nrow(output)))
print(head(output, 20))

# Tab-separated, not CSV: Modules/R PAPER/R code/democare_compute.R reads
# its input with read.table(path, header=TRUE, row.names=NULL, sep="\t").
# The .dat extension matches the file picker in createDemoCareFiles.R.
write.table(output, "computeCare_output_baseR.dat",
            sep = "\t", row.names = FALSE, quote = FALSE)
message("Output written to computeCare_output_baseR.dat (tab-separated, ready for Modules/R PAPER/R code/democare_compute.R)")

}
