library(stats)

# ==== 1. Constants ====

# Default activity-status coding used throughout DemoCare (module 2 input).
# Named integer vector: label used in the raw survey -> internal numeric code.
# The numeric codes 1/2/3 fix the ORDER in which statuses are reported in the
# output columns (p1_1, p1_2, p1_3, p2_1, ...), matching transiciones.xlsx.
DEFAULT_STATUS_LEVELS <- c(Inactivo = 1L, Parcial = 2L, Completo = 3L)


# ==== 2. Build wave pairs from long individual-level data ====

# NOTE ON THE SOURCE LOGIC:
#   The raw survey has TWO rows per individual, one per wave, one year apart
#   (e.g. CICLO 139 -> 143 in the original EPA extract)
#   we pivot this into one row per individual with:
#     Age    = age at the EARLIER wave (the "current age" the transition
#              starts from)
#     ORIGEN = activity status at the earlier wave  (1/2/3)
#     DESTINO = activity status at the later wave    (1/2/3)
#   and keeps only individuals with a valid (non-missing) status in BOTH
#   waves (Excel's "DOS" flag). build_wave_pairs() below reproduces exactly
#   this step, generically: it works off age order within each id rather
#   than assuming a specific wave/cycle code, so it plugs into any two-wave
#   survey as long as age increases by (about) one year between waves.

build_wave_pairs <- function(data,
                              id_var,
                              age_var,
                              status_var,
                              sex_var,
                              status_levels = DEFAULT_STATUS_LEVELS,
                              extra_vars = character(0)) {

  d <- data[, unique(c(id_var, age_var, status_var, sex_var, extra_vars))]
  names(d)[names(d) == id_var]     <- "id"
  names(d)[names(d) == age_var]    <- "age"
  names(d)[names(d) == status_var] <- "status_raw"
  names(d)[names(d) == sex_var]    <- "sex"

  # Recode the raw status labels ("Inactivo"/"Parcial"/"Completo", or
  # whatever labels the caller's survey uses) to the numeric 1/2/3 codes.
  d$status <- unname(status_levels[as.character(d$status_raw)])

  # Order so that, within each id, row 1 is the earlier wave (younger age)
  # and row 2 is the later wave. Ties on age (rare data-entry noise) are
  # broken by original row order, which is good enough for a one-year gap.
  d <- d[order(d$id, d$age), ]

  # Keep only ids observed exactly twice with a non-missing status both
  # times -- this is the direct equivalent of Excel's "DOS == 1" filter.
  n_obs   <- tabulate(match(d$id, unique(d$id)))
  ok_id   <- unique(d$id)[n_obs == 2]
  d       <- d[d$id %in% ok_id & !is.na(d$status), ]
  n_valid <- tabulate(match(d$id, ok_id))
  ok_id2  <- ok_id[n_valid == 2 & (ok_id %in% ok_id)]
  # (belt-and-braces re-check, in case one wave's status was NA)
  keep_ids <- names(which(table(d$id) == 2))
  d <- d[as.character(d$id) %in% keep_ids, ]

  first  <- d[!duplicated(d$id), ]
  second <- d[duplicated(d$id), ]
  second <- second[match(first$id, second$id), ]

  pairs <- data.frame(
    id      = first$id,
    sex     = first$sex,
    age     = first$age,
    origen  = first$status,
    destino = second$status,
    stringsAsFactors = FALSE
  )

  # Carry through any extra stratifying variables (e.g. education level),
  # taken from the EARLIER wave -- matching how Excel's pas1 pivot uses the
  # baseline-wave niveduc/PROV as the individual's row key.
  for (v in extra_vars) {
    pairs[[v]] <- first[[v]]
  }

  pairs
}


# ==== 3. Raw (unsmoothed) transition probabilities by single year of age ====

# Cross-tabulates ORIGEN x DESTINO counts for every (group, age) cell, then
# row-normalises within ORIGEN

raw_transition_probs <- function(pairs, group_vars, status_levels, age_range) {

  K <- length(status_levels)
  codes <- unname(status_levels)

  # Every (group, age, origen, destino) combination, including the zeros --
  # needed so the smoother sees a complete, evenly-spaced age series.
  group_keys <- unique(pairs[, group_vars, drop = FALSE])
  grid <- expand.grid(
    row_id  = seq_len(nrow(group_keys)),
    age     = age_range,
    origen  = codes,
    destino = codes,
    KEEP.OUT.ATTRS = FALSE
  )
  grid <- cbind(grid, group_keys[grid$row_id, , drop = FALSE])
  grid$row_id <- NULL

  key_cols <- c(group_vars, "age", "origen", "destino")
  pairs$n <- 1L
  counts <- aggregate(n ~ ., data = pairs[, c(key_cols, "n")], FUN = sum)

  merge_key <- do.call(paste, c(grid[key_cols], sep = "\r"))
  count_key <- do.call(paste, c(counts[key_cols], sep = "\r"))
  grid$n <- counts$n[match(merge_key, count_key)]
  grid$n[is.na(grid$n)] <- 0L

  # Row-normalise within (group, age, origen): p(o -> d) = n(o, d) / sum_d n(o, d)
  denom_key <- do.call(paste, c(grid[c(group_vars, "age", "origen")], sep = "\r"))
  denom <- tapply(grid$n, denom_key, sum)
  grid$p_raw <- grid$n / denom[denom_key]

  grid
}


# ==== 4. Smooth the raw probabilities with LOESS ====

smooth_transition_probs <- function(raw, group_vars, span = 0.5, degree = 2,
                                     weighted = TRUE) {

  raw$p_smooth <- NA_real_
  split_key <- do.call(paste, c(raw[c(group_vars, "origen", "destino")], sep = "\r"))

  for (k in unique(split_key)) {
    idx <- which(split_key == k)
    sub <- raw[idx, ]
    if (length(unique(sub$age)) < 4 || all(sub$n == 0)) {
      # Not enough distinct ages (or no exposure at all) to fit a local
      # regression sensibly -- fall back to the raw (unsmoothed) values.
      raw$p_smooth[idx] <- sub$p_raw
      next
    }

    w <- if (weighted) {
      # Weight each age's contribution to the fit by how many individuals
      # were actually observed with that origin/age (more exposure = more
      # reliable proportion). Excel's SMOOTH() treated every age equally;
      # weighting is the recommended improvement, not a faithful replica --
      # set weighted = FALSE to mirror the original unweighted behaviour.
      pmax(tapply(raw$n[idx], sub$age, sum)[as.character(sub$age)], 1)
    } else {
      rep(1, nrow(sub))
    }

    fit <- loess(p_raw ~ age, data = sub, span = span, degree = degree,
                 weights = w, control = loess.control(surface = "direct"))
    raw$p_smooth[idx] <- predict(fit, newdata = data.frame(age = sub$age))
  }

  raw
}


# ==== 5. Floor at zero and renormalise ====

# LOESS can push a probability slightly below 0 or
# push the three destinations for one origin away from summing to 1. Excel
# fixes this with MAX(0, ...) (columns AF:AN) followed by a renormalisation
# so each origin's three destination probabilities sum back to 1 exactly
# (columns AO:AW). Same two steps here.

finalise_transition_probs <- function(smoothed, group_vars) {

  smoothed$p_floor <- pmax(0, smoothed$p_smooth)

  denom_key <- do.call(paste, c(smoothed[c(group_vars, "age", "origen")], sep = "\r"))
  denom <- tapply(smoothed$p_floor, denom_key, sum)
  smoothed$p_final <- ifelse(denom[denom_key] > 0,
                              smoothed$p_floor / denom[denom_key],
                              smoothed$p_floor)
  smoothed
}


# ==== 6. Top-level function ====

#' Estimate age-specific activity-status transition probabilities
#'
#' Takes individual-level, two-wave labour force survey data (one row per
#' person per wave) and returns, for each single year of age and each group
#' (sex, and optionally other strata such as education level), the smoothed
#' probability of moving from each activity status to each activity status
#' one year later.
#'
#' @param data           Long data frame, one row per person per wave.
#' @param id_var         Column identifying the individual (same value in
#'                        both wave-rows).
#' @param age_var        Column with the individual's age at that wave
#'                        (integer years).
#' @param sex_var        Column with sex/gender (used to stratify the
#'                        output; pass the same coding your data already
#'                        uses, e.g. 1/6 or "M"/"F" -- it is only ever used
#'                        as a grouping key, never interpreted).
#' @param status_var     Column with the activity status at that wave, using
#'                        the labels named in `status_levels`.
#' @param status_levels  Named integer vector mapping status labels to the
#'                        1/2/3 codes used in the output (defaults to
#'                        c(Inactivo = 1, Parcial = 2, Completo = 3), i.e.
#'                        1 = no activity, 2 = partial activity, 3 = full
#'                        activity, matching the DemoCare convention).
#' @param strata_vars    Optional character vector of extra columns to
#'                        stratify by in addition to sex (e.g. "niveduc" for
#'                        education level, as in the workbook's "pas2" tab).
#'                        Left NULL, the output pools across everything but
#'                        sex, matching the "edad simple" tab exactly (EDU
#'                        and PROV both set to "(All)" there).
#' @param age_range      Integer vector of the single-year ages to produce
#'                        smoothed estimates for. Defaults to the full range
#'                        of ages present in the data. The workbook restricts
#'                        this to 16:64, dropping the top age because too few
#'                        respondents remain there to smooth sensibly --
#'                        consider doing the same if your survey shows the
#'                        same thinning-out at the oldest ages.
#' @param span, degree   Passed straight to stats::loess() -- the smoothing
#'                        window (span) and local polynomial degree.
#' @param weighted       If TRUE (default), the loess fit at each age is
#'                        weighted by the number of individuals observed
#'                        with that origin/age, which stabilises the
#'                        smoothing for status pairs with few observations
#'                        (e.g. "Parcial", generally the smallest group).
#'                        Set FALSE to mirror Excel's SMOOTH(), which
#'                        weighted every age equally regardless of sample
#'                        size.
#' @param require_stable_strata  If TRUE, drop individuals whose value of
#'                        any `strata_vars` (or of sex) changes between the
#'                        two waves. This reproduces a side effect of how
#'                        the workbook's "pas1" pivot table is built (its
#'                        row fields include niveduc/PROV alongside id, so
#'                        anyone whose recorded education level or province
#'                        differs between waves silently falls out of the
#'                        DOS == 1 count). It is NOT a deliberate modelling
#'                        choice, just a quirk of the pivot -- left FALSE
#'                        (the default) here to keep every valid pair, which
#'                        we'd recommend. Set TRUE only if you're trying to
#'                        reconcile numbers against the workbook.
#' @param wide           If TRUE (default), return one row per (group, age)
#'                        with columns p1_1, p1_2, p1_3, p2_1, ... p3_3,
#'                        exactly matching the "edad simple" tab's BA:BI
#'                        layout. If FALSE, return the tidy long form
#'                        (one row per group/age/origen/destino).
#'
#' @return A data frame of transition probabilities (wide or long, see
#'         `wide` above).
compute_activity_transitions <- function(data,
                                          id_var        = "id",
                                          age_var       = "EDAD",
                                          sex_var       = "SEXO",
                                          status_var    = "actDEMOCARE",
                                          status_levels = DEFAULT_STATUS_LEVELS,
                                          strata_vars   = NULL,
                                          age_range     = NULL,
                                          span          = 0.5,
                                          degree        = 2,
                                          weighted      = TRUE,
                                          require_stable_strata = FALSE,
                                          wide          = TRUE) {

  group_vars <- c("sex", strata_vars)

  pairs <- build_wave_pairs(data, id_var, age_var, status_var, sex_var,
                             status_levels, extra_vars = strata_vars)

  if (require_stable_strata && length(strata_vars) > 0) {
    # Reproduce the pas1-pivot quirk described above: an individual whose
    # education (or other stratum) differs between the two raw rows never
    # forms a valid pair, because the pivot splits their two waves into two
    # different rows. We approximate this by simply re-deriving the strata
    # value from BOTH waves and dropping mismatches.
    raw <- data[, unique(c(id_var, strata_vars))]
    names(raw)[names(raw) == id_var] <- "id"
    stable <- Reduce(`&`, lapply(strata_vars, function(v) {
      tapply(raw[[v]], raw$id, function(x) length(unique(x)) == 1)
    }))
    stable_ids <- names(stable)[stable]
    pairs <- pairs[as.character(pairs$id) %in% stable_ids, ]
  }

  if (is.null(age_range)) {
    age_range <- min(pairs$age, na.rm = TRUE):max(pairs$age, na.rm = TRUE)
  }
  pairs <- pairs[pairs$age %in% age_range, ]

  raw       <- raw_transition_probs(pairs, group_vars, status_levels, age_range)
  smoothed  <- smooth_transition_probs(raw, group_vars, span = span,
                                        degree = degree, weighted = weighted)
  final     <- finalise_transition_probs(smoothed, group_vars)

  out <- final[, c(group_vars, "age", "origen", "destino", "p_final")]
  names(out)[names(out) == "p_final"] <- "prob"

  if (!wide) {
    return(out[order(out[[group_vars[1]]], out$age, out$origen, out$destino), ])
  }

  # Reshape to one row per (group, age), columns p1_1 .. p3_3 (or however
  # many statuses status_levels defines), matching "edad simple" BA:BI.
  out$col <- paste0("p", out$origen, "_", out$destino)
  wide_df <- reshape(out[, c(group_vars, "age", "col", "prob")],
                      idvar = c(group_vars, "age"), timevar = "col",
                      direction = "wide")
  names(wide_df) <- sub("^prob\\.", "", names(wide_df))
  codes <- unname(status_levels)
  col_order <- paste0("p", rep(codes, each = length(codes)), "_", rep(codes, length(codes)))
  wide_df <- wide_df[, c(group_vars, "age", col_order)]
  wide_df[order(wide_df[[group_vars[1]]], wide_df$age), ]
}


# ==== 7. Example usage ====
#
# survey <- readxl::read_excel("transiciones.xlsx", sheet = "actDEMOCARE.csv")
#
# transitions <- compute_activity_transitions(
#   survey,
#   id_var     = "id",
#   age_var    = "EDAD",
#   sex_var    = "SEXO",
#   status_var = "actDEMOCARE",
#   age_range  = 16:64
# )
#
# head(transitions)
#
# # Stratify by education level as well as sex, and reproduce the workbook's
# # pas1-pivot quirk exactly (for cross-checking against transiciones.xlsx):
# transitions_by_edu <- compute_activity_transitions(
#   survey, strata_vars = "niveduc", require_stable_strata = TRUE
# )
