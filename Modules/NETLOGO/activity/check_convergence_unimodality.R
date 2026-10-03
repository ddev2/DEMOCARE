########################################################################
# check_convergence_unimodality.R
#
# Diagnostic companion to smooth_actividad.R's calc_proba() / iter().
#
# WHAT THIS CHECKS
#   iter() fits, for one (sex, edu, dep) cell at a time, a set of
#   activity-transition probabilities by repeatedly rescaling an initial
#   guess (built from the EPA labour-force transition rates) so that,
#   applied to that dependency level's own age-specific activity-status
#   stock (from the health survey), it reproduces that stock one year
#   later. min_test/"err" is the size of the mismatch between what the
#   fitted probabilities imply for next year's stock and the actual
#   next-year stock, summed across ages, at each iteration.
#
#   The production code in smooth_actividad.R assumes this error curve
#   is unimodal (decreases, then increases) and stops at the first
#   uptick, keeping the best iteration seen. This script checks that
#   assumption directly instead of just acting on it: it reruns the same
#   iteration WITHOUT stopping early, for many more iterations than the
#   production n = 7, and records the FULL error trajectory for every
#   (sex, edu, dep) cell so the actual shape can be inspected.
#
# HOW TO RUN
#   1. Run smooth_actividad.R first, at least through the line that
#      builds `actMat` and `EPA_transMat` (the "activity table all
#      transitions" section). This script reuses those two objects
#      rather than recomputing them.
#   2. source("check_convergence_unimodality.R")
#   3. Read the printed summary, and open convergence_traces.png.
#
# WHAT TO LOOK FOR
#   - "Unimodal" cells: exactly one valley -- confirms the "stop at
#     first increase" rule in the production code is doing the right
#     thing for that cell.
#   - "Not unimodal" cells: the error wiggles up and down more than
#     once. For these, stopping at the *first* uptick may not land on
#     the best iteration -- worth a closer look at that cell's plotted
#     line.
#   - "Still decreasing at N_MAX": the minimum hasn't been reached even
#     after N_MAX iterations -- raise N_MAX further for those specific
#     cells and re-check.
########################################################################

if (!exists("actMat") || !exists("EPA_transMat")) {
  stop(
    "actMat / EPA_transMat not found in this R session.\n",
    "Run smooth_actividad.R first (through the line that builds actMat ",
    "and EPA_transMat), then source this script in the SAME session."
  )
}

# N_MAX <- 30  # production code uses n = 7; we go well beyond that here
               # purely to see the shape of the error curve, not to choose
               # a new default -- raise this further if cells are still
               # improving at iteration 30 (see "still_decreasing" below)


# ==== 1. A version of iter() that never stops early ====================

# Mirrors calc_proba()/iter() in smooth_actividad.R exactly -- same
# maths, same "hack for negative values" loops -- but always runs
# n_max iterations and returns the WHOLE error trajectory instead of
# just the single best p.
trace_iter <- function(a, EPA_proba, n_max = N_MAX) {

  proba <- array(0, dim = c(9, 105))
  proba[1, ] <- 1
  proba[4, ] <- 0
  proba[7, ] <- 0
  proba[, c(17:65)] <- EPA_proba
  proba[, c(66:74)] <- c(1, 0, 0, 0.95, 0.05, 0, 0.95, 0, 0.05)
  proba[, 75] <- c(1, 0, 0, 1, 0, 0, 1, 0, 0)

  p  <- proba
  ch <- array(0, dim = c(9, 105))
  err_trace <- numeric(n_max)

  for (ind in seq_len(n_max)) {
    ch[1, ] <- a[1, c(1:105)] * p[1, ]
    ch[2, ] <- a[1, c(1:105)] * p[2, ]
    ch[3, ] <- a[1, c(1:105)] * p[3, ]
    ch[4, ] <- a[2, c(1:105)] * p[4, ]
    ch[5, ] <- a[2, c(1:105)] * p[5, ]
    ch[6, ] <- a[2, c(1:105)] * p[6, ]
    ch[7, ] <- a[3, c(1:105)] * p[7, ]
    ch[8, ] <- a[3, c(1:105)] * p[8, ]
    ch[9, ] <- a[3, c(1:105)] * p[9, ]

    b <- array(0, dim = c(3, 105))
    b[1, ] <- ch[1, ] + ch[4, ] + ch[7, ]
    b[2, ] <- ch[2, ] + ch[5, ] + ch[8, ]
    b[3, ] <- ch[3, ] + ch[6, ] + ch[9, ]

    ch2 <- ch

    ch2[2, ] <- ifelse(b[2, ] > 0, ch[2, ] * a[2, c(2:106)] / b[2, ], a[2, c(2:106)])
    ch2[3, ] <- ifelse(b[3, ] > 0, ch[3, ] * a[3, c(2:106)] / b[3, ], a[3, c(2:106)])
    for (age in 2:105) {
      # hack for negative values (same as production code)
      if (a[1, age] - (ch2[2, age] + ch2[3, age]) < 0) {
        ch2[2, age] <- ch2[2, age - 1]
        ch2[3, age] <- ch2[3, age - 1]
      }
    }
    ch2[1, ] <- a[1, c(1:105)] - ch2[2, ] - ch2[3, ]

    ch2[4, ] <- ifelse(b[1, ] > 0, ch[4, ] * a[1, c(2:106)] / b[1, ], a[1, c(2:106)])
    ch2[6, ] <- ifelse(b[3, ] > 0, ch[6, ] * a[3, c(2:106)] / b[3, ], a[3, c(2:106)])
    for (age in 2:105) {
      if (a[2, age] - (ch2[4, age] + ch2[6, age]) < 0) {
        ch2[4, age] <- ch2[4, age - 1]
        ch2[6, age] <- ch2[6, age - 1]
      }
    }
    ch2[5, ] <- a[2, c(1:105)] - ch2[4, ] - ch2[6, ]

    ch2[7, ] <- ifelse(b[1, ] > 0, ch[7, ] * a[1, c(2:106)] / b[1, ], a[1, c(2:106)])
    ch2[8, ] <- ifelse(b[2, ] > 0, ch[8, ] * a[2, c(2:106)] / b[2, ], a[2, c(2:106)])
    for (age in 2:105) {
      if (a[3, age] - (ch2[7, age] + ch2[8, age]) < 0) {
        ch2[7, age] <- ch2[7, age - 1]
        ch2[8, age] <- ch2[8, age - 1]
      }
    }
    ch2[9, ] <- a[3, c(1:105)] - ch2[7, ] - ch2[8, ]

    b2 <- b
    b2[1, ] <- ch2[1, ] + ch2[4, ] + ch2[7, ]
    b2[2, ] <- ch2[2, ] + ch2[5, ] + ch2[8, ]
    b2[3, ] <- ch2[3, ] + ch2[6, ] + ch2[9, ]

    min_test <- sqrt((b2[1, ] - a[1, c(2:106)])^2 +
                      (b2[2, ] - a[2, c(2:106)])^2 +
                      (b2[3, ] - a[3, c(2:106)])^2)
    err_trace[ind] <- sum(min_test, na.rm = TRUE)

    p[1, ] <- ifelse(a[1, c(1:105)] > 0, ch2[1, ] / a[1, c(1:105)], 0)
    p[2, ] <- ifelse(a[1, c(1:105)] > 0, ch2[2, ] / a[1, c(1:105)], 0)
    p[3, ] <- ifelse(a[1, c(1:105)] > 0, ch2[3, ] / a[1, c(1:105)], 0)
    p[4, ] <- ifelse(a[2, c(1:105)] > 0, ch2[4, ] / a[2, c(1:105)], 0)
    p[5, ] <- ifelse(a[2, c(1:105)] > 0, ch2[5, ] / a[2, c(1:105)], 0)
    p[6, ] <- ifelse(a[2, c(1:105)] > 0, ch2[6, ] / a[2, c(1:105)], 0)
    p[7, ] <- ifelse(a[3, c(1:105)] > 0, ch2[7, ] / a[3, c(1:105)], 0)
    p[8, ] <- ifelse(a[3, c(1:105)] > 0, ch2[8, ] / a[3, c(1:105)], 0)
    p[9, ] <- ifelse(a[3, c(1:105)] > 0, ch2[9, ] / a[3, c(1:105)], 0)
    # (deliberately no early stopping here -- we want the full trajectory)
  }

  err_trace
}


# ==== 2. Unimodality test ===============================================

# TRUE if x decreases (or stays flat) to a single minimum and then
# increases (or stays flat) -- i.e. exactly one valley, no wiggling back
# and forth more than once. A strictly monotonic trace (all decreasing,
# or all increasing) also counts as "unimodal" in the strict sense, but
# is flagged separately below (via still_decreasing) since it means the
# turning point hasn't actually been observed within n_max iterations.
is_unimodal <- function(x, tol = 1e-10) {
  d <- diff(x)
  sgn <- ifelse(d < -tol, -1L, ifelse(d > tol, 1L, 0L))
  sgn <- sgn[sgn != 0L]                 # ignore essentially-flat steps
  if (length(sgn) == 0L) return(TRUE)   # completely flat
  turns <- which(diff(sgn) != 0L)
  if (length(turns) == 0L) return(TRUE)       # monotonic throughout
  if (length(turns) > 1L) return(FALSE)       # more than one turning point
  sgn[turns[1L]] == -1L && sgn[turns[1L] + 1L] == 1L  # must be a valley, not a peak
}


# ==== 3. Run it for all 24 (sex, edu, dep) cells ========================

cells <- expand.grid(sex = 1:2, edu = 1:3, dep = 1:4)

trace_list <- vector("list", nrow(cells))

trace_summary <- do.call(rbind, lapply(seq_len(nrow(cells)), function(i) {
  sx <- cells$sex[i]; ed <- cells$edu[i]; dp <- cells$dep[i]
  tr <- trace_iter(actMat[sx, ed, dp, , ], EPA_transMat[, sx, ])
  trace_list[[i]] <<- tr

  argmin <- which.min(tr)
  data.frame(
    sex              = sx,
    edu              = ed,
    dep              = dp,
    argmin_iter      = argmin,
    min_err          = tr[argmin],
    err_at_n7        = tr[7],
    err_at_nmax      = tr[N_MAX],
    unimodal         = is_unimodal(tr),
    still_decreasing = argmin == N_MAX
  )
}))


# ==== 4. Report ==========================================================

cat("\n#### Convergence-shape check across", nrow(trace_summary),
    "(sex, edu, dep) cells, n_max =", N_MAX, "####\n\n")

cat("Unimodal (single valley):        ", sum(trace_summary$unimodal),
    "/", nrow(trace_summary), "\n")
cat("Still decreasing at iteration", N_MAX, "(minimum not yet reached):",
    sum(trace_summary$still_decreasing), "/", nrow(trace_summary), "\n")
cat("Range of argmin iteration across cells:",
    paste(range(trace_summary$argmin_iter), collapse = " - "), "\n")
cat("Cells whose minimum falls within the production n = 7:",
    sum(trace_summary$argmin_iter <= 7), "/", nrow(trace_summary), "\n\n")

not_unimodal <- subset(trace_summary, !unimodal)
if (nrow(not_unimodal) > 0) {
  cat("Cells whose error curve is NOT a clean single valley (inspect the plot for these):\n")
  print(not_unimodal[, c("sex", "edu", "dep", "argmin_iter", "min_err")])
} else {
  cat("Every cell's error curve has exactly one valley -- the 'stop at first",
      "increase' rule used in smooth_actividad.R matches what actually happens here.\n")
}

still_dec <- subset(trace_summary, still_decreasing)
if (nrow(still_dec) > 0) {
  cat("\nCells still improving at iteration", N_MAX,
      "-- the true minimum lies beyond what was tested; raise N_MAX for these and re-run:\n")
  print(still_dec[, c("sex", "edu", "dep", "min_err")])
}


# ==== 5. Plot every trajectory ==========================================

# Log scale on the y-axis since the error can span a couple of orders of
# magnitude across cells (working-age, high-exposure cells vs. sparse,
# edge-of-range ones).
mat <- do.call(cbind, trace_list)

png("convergence_traces.png", width = 900, height = 600)
matplot(seq_len(N_MAX), mat, type = "l", lty = 1, col = rainbow(ncol(mat)),
        log = "y", xlab = "iteration", ylab = "error (min_test, summed over ages, log scale)",
        main = "calc_proba() convergence: one line per (sex, edu, dep) cell")
abline(v = 7, lty = 2, col = "grey40")
legend("topright", legend = "production n = 7", lty = 2, col = "grey40", bty = "n")
dev.off()

cat("\nSaved plot to convergence_traces.png (in the working directory).\n")
