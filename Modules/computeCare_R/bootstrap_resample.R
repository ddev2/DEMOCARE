########################################################################
# bootstrap_resample.R
#
# Resamples whole FAMILY NETWORKS, with replacement, from a KINFERT
# population/links file pair -- the input side of computeCare, not its
# output. See the "Bootstrapping" section of Democare 2026.docx for the
# full reasoning; short version: egos inside one family network are not
# independent observations (their care variables are computed FROM each
# other), so the correct bootstrap resampling unit is the family, not
# the individual ego-row in a results file. This mirrors the mechanism
# already built into the NetLogo model -- the "idFamily" column added to
# population files (see PopulationImporter.nls / PopulationCreator.nls /
# computeCare_baseR.R's read_population() comments) exists specifically
# "so whole families can be pulled out/duplicated directly from the
# population file for bootstrap resampling".
#
# >>> Claude 2026-09-29: tidyverse engine removed (base R only)
# This file is deliberately self-contained (no dependency on
# computeCare_baseR.R and no tidyverse functions), so
# computeCare_baseR_bootstrap.R and app.R can both source() it
# unchanged.
# <<< Claude 2026-09-29
#
# WHAT IT DOES NOT DO: run computeCare itself, or touch a single line of
# either engine's simulation logic. It only produces resampled
# population/links FILES in the same format read_population()/
# read_links() already expect -- the two *_bootstrap.R wrapper scripts
# are what feed those into run_computeCare().
########################################################################


# ==== 1. Which family does each population row/link row belong to? ====

# Reusable version of the same idFamily-or-derive-from-links logic
# already in build_family_tree() (Section 3 of both engines) -- kept
# here as a small standalone step because bootstrap_resample_families()
# needs it applied to POPULATION rows (to know which rows travel
# together when a family is drawn), not to build a role/generation tree.
#
# Returns a CHARACTER vector, one entry per population row, in
# population's row order. Character, not numeric, on purpose: isolated
# egos (no partner, no children -- they never appear in the links file
# at all) fall back to using their own "id" as their family label, and
# that raw id could numerically collide with a real links$family number
# for someone else's family. Prefixing isolated egos' fallback label
# ("isoNNN") rules that collision out; every other family id (whether
# from idFamily or from links$family) is converted to character too so
# the whole vector compares consistently.
assign_family_id <- function(population, links) {
  if ("idFamily" %in% names(population)) {
    return(as.character(population$idFamily))
  }

  # No idFamily column (this is the format Modules/KINFERT/output's
  # sample files are actually in): recover family membership from the
  # links table instead, exactly like build_family_tree()'s fallback
  # branch does, but keyed off BOTH link columns (id1 and id2) rather
  # than just marriages$id1/descendants$id2 -- a small superset that
  # costs nothing and is safer if a links file ever has a layout
  # build_family_tree()'s narrower version wouldn't fully cover.
  fam_of_id <- rbind(
    data.frame(id = links$id1, family = links$family),
    data.frame(id = links$id2, family = links$family)
  )
  fam_of_id <- fam_of_id[!duplicated(fam_of_id$id), ]

  familyId <- as.character(fam_of_id$family[match(population$id, fam_of_id$id)])
  isolated <- is.na(familyId)
  familyId[isolated] <- paste0("iso", population$id[isolated])
  familyId
}


# ==== 2. Resample families with replacement ====

# population, links   the ORIGINAL (un-resampled) population/links data
#                      frames, as returned by read_population()/
#                      read_links() -- pass the raw data frames in, not
#                      file paths, so this can be called once per
#                      replicate without re-reading the base files from
#                      disk every time.
# n_draw               how many families to draw (with replacement) for
#                      this replicate. Defaults to the original number
#                      of families, matching standard bootstrap practice
#                      (resample n units from n original units).
# seed                 set.seed() value for this replicate's draw.
#
# Returns list(population = <resampled population df>,
#              links      = <resampled links df>), ready to write with
# write_resampled_files() below and feed straight into run_computeCare().
#
# HOW COLLISIONS ARE AVOIDED: with replacement means the same family can
# be drawn more than once in one replicate, and the same underlying ids
# would otherwise collide across those copies (and across replicates, if
# you kept re-using the base ids). Every occurrence i (1..n_draw) gets
# its ids shifted by i * id_mult, where id_mult is one power of ten above
# the largest id in the base population -- e.g. base ids up to 65672 (5
# digits) get shifted by multiples of 100000, so occurrence i's ids read
# as "<i><original 5-digit id>" and can never overlap another occurrence's.
# Each occurrence also gets a fresh idFamily = i, written into the
# resampled population file's leading "idFamily" column (the same
# leading-column format PopulationCreator.nls already knows how to read
# positionally) -- so the resampled files stay usable by NetLogo too, not
# just by the two R engines, if you ever want to cross-check a bootstrap
# replicate that way.
bootstrap_resample_families <- function(population, links, n_draw = NULL, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  population$.familyId <- assign_family_id(population, links)
  fam_of_pop_id <- setNames(population$.familyId, as.character(population$id))
  links$.familyId <- unname(fam_of_pop_id[as.character(links$id1)])

  fam_ids <- unique(population$.familyId)
  if (is.null(n_draw)) n_draw <- length(fam_ids)

  # split() once, then each replicate just indexes into the list --
  # much faster than re-filtering the whole data frame per family per
  # replicate (this population has ~9,600 families; a naive filter-per-
  # family loop would re-scan the full ~65,000/~77,000-row tables that
  # many times over).
  pop_split   <- split(population, population$.familyId)
  links_split <- split(links, links$.familyId)

  drawn <- sample(fam_ids, size = n_draw, replace = TRUE)

  id_digits <- nchar(as.character(max(population$id, na.rm = TRUE)))
  id_mult   <- 10 ^ id_digits

  new_pop   <- vector("list", n_draw)
  new_links <- vector("list", n_draw)

  for (i in seq_len(n_draw)) {
    fid <- drawn[i]
    p <- pop_split[[fid]]
    l <- links_split[[fid]]
    if (is.null(l)) l <- links[0, , drop = FALSE]  # isolated ego: no links rows

    id_shift <- i * id_mult

    p$.familyId <- NULL
    p$id        <- as.integer(p$id + id_shift)
    p$idFamily  <- i

    if (nrow(l) > 0) {
      l$.familyId <- NULL
      l$id1   <- as.integer(l$id1 + id_shift)
      l$id2   <- as.integer(l$id2 + id_shift)
      l$family <- i
    }

    new_pop[[i]]   <- p
    new_links[[i]] <- l
  }

  list(
    population = do.call(rbind, new_pop),
    links      = do.call(rbind, new_links)
  )
}


# ==== 3. Write a resampled replicate back out in read_population()/ ====
# ====    read_links()'s expected format                             ====

# population   as returned by bootstrap_resample_families()$population
#              (already carries the new leading "idFamily" column)
# links        as returned by bootstrap_resample_families()$links
#
# Column order for population is deliberately idFamily-first, id,
# sex, age, ageDef, status, tickIn, tickOut, ..., ego -- matching the
# "newer format" PopulationCreator.nls's own comments describe (every
# field after idFamily "shifts one position to the right"), so these
# files stay directly readable by NetLogo's InitializePopulation as well
# as by both R engines' read_population(), not just the latter.
write_resampled_files <- function(resampled, population_path_out, links_path_out) {
  pop <- resampled$population
  front <- intersect(c("idFamily", "id", "sex", "age", "ageDef", "status",
                        "tickIn", "tickOut"), names(pop))
  rest  <- setdiff(names(pop), front)
  pop   <- pop[, c(front, rest)]

  write.csv(pop, population_path_out, row.names = FALSE)

  # No header, comma-separated, same 4 columns read_links() expects.
  write.table(resampled$links[, c("id1", "id2", "type", "family")],
              links_path_out, sep = ",", row.names = FALSE,
              col.names = FALSE, quote = FALSE)
}
