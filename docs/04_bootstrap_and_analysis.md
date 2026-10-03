# 4. Bootstrap and analysis of results

<!-- >>> Claude 2026-10-02 -->
Summary of sections 3g and 3h of [`Democare_2026.md`](Democare_2026.md).
<!-- <<< Claude 2026-10-02 -->

## Why families, not result rows, are resampled

Bootstrap replicates are used to compute confidence intervals. Resampling is done on the KINFERT families, before the simulation, and not on the rows of a results file. The care an ego receives depends on the other members of the same family, so egos of one family are not independent observations. Resampling result rows would understate the uncertainty and could combine an ego's care figures with relatives it was never simulated with.

With `n` families drawn `n` times with replacement, a given family is missed with probability `(1 - 1/n)^n`, close to `1/e` (about 36.8%). About 36.8% of families appear once and about 26.4% twice or more.

## How it is done in R

- `bootstrap_resample.R`: `bootstrap_resample_families()` draws whole families with replacement (partner, children, children's partners and grandchildren stay together) and renumbers ids so that copies never collide; `write_resampled_files()` writes a new population and links pair.
- `computeCare_baseR_bootstrap.R`: loops resampling and `run_computeCare()` over the requested number of replicates, and supports several base populations (`iSimul`).

Each replicate is a full rerun of the simulation, so run time grows with the number of replicates.

File names, for root `computeCare_baseR`:

| File | Content |
| --- | --- |
| `computeCare_baseR_1.csv`, `computeCare_baseR_1_link.csv` | Resampled population and links for replicate 1 |
| `computeCare_baseR_1_1.txt` | Simulation output, pattern `<root>_<iSimul>_<iBootstrap>.txt` |

## From output files to `.RData` files

`Modules/R PAPER/R code/democare_compute.R` builds three objects, each saved in a fixed file that `democare_graphs.R` reads:

| Object | File | Content |
| --- | --- | --- |
| `cTabFertNuptDF` | `FertNupt.RData` | Fertility and nuptiality indicators by year and sex |
| `careSupplyDF` | `careSupply.RData` | Who supplies care to a dependent ego: partner, children or nobody |
| `informalCareDF` | `formal_informal.RData` | Balance between care needed and informal care received |

Entry points:

- **One run:** `createDemoCareFiles(filePath, pathOut, cohort)`.
- **Many replicates:** `createCareSupplyDF()` (writes `careSupply.RData` and `FertNupt.RData`) and `computeInformalCare()` (writes `formal_informal.RData`), with `nDemRegFileNames` (the roots), `nSimulFiles`, `debSimul`, `nDemoCareFiles` and `debDemoCare`.

Points to know before running these functions by hand (the Shiny app handles all three):

- `createCareSupplyDF()` reads `path`, `pathData` and `zipped` as free variables; they must be defined before the call.
- Both functions ignore `pathOut` for their output and save in the current working directory, so `setwd()` to the output folder first.
- The birth cohort is read from characters 4 to 7 of the root name (`KIN1938_...` gives 1938). A root without this pattern gets cohort 2020, with a coercion warning.
- The processing assumes output files that cover the whole projection (at least 45 years). Output from short test runs cannot be processed.

## Comparing birth cohorts

Each birth cohort is one computeCare run and one output file, with the cohort in the file name (`KIN<cohort>_...`). `aggregate_multi_cohort.R` lets a cohort label be attached by hand to any set of files through its `file_map` vector.
