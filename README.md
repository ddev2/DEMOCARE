# DEMOCARE

DEMOCARE is a microsimulation model of care for older people with disabilities. It projects simulated family networks forward in time, year by year, and computes whether the members of each family can provide the hours of care that a dependent person needs (informal care), or whether care from outside the family (formal care) is required.

The model was used in:

> Spijker, J., Devolder, D., and Zueras, P. (2022). The impact of demographic change in the balance between formal and informal old-age care in Spain. Results from a mixed microsimulation-agent-based model. *Ageing and Society* 42(3), 588-613. <https://doi.org/10.1017/S0144686X20001026>

The model is being rebuilt in 2026. The first version of the care module was written in NetLogo; it has now been rewritten in R, and the NetLogo code is kept in this repository for reference and for comparison.

## The three modules

1. **KINFERT** simulates kinship networks (genealogies). For DEMOCARE it produces, for each ego aged exactly 50, the relatives who can give or need care: partner, children, children's partners and grandchildren. KINFERT has its own repository: <https://github.com/ddev2/KINFERT>.
2. **computeCare** reads these families and projects every member forward, one year per step, until the last ego dies. Each year it updates dependency level and labour force activity, and allocates care within the family. This is the R code in `Modules/computeCare_R/`.
3. **Analysis** code in R reads the computeCare output and produces the indicators and plots used in the paper (`Modules/R PAPER/R code/`).

## Repository layout

```
<!-- >>> Claude 2026-10-02 -->
docs/                          Full documentation and short summaries (Markdown)
<!-- <<< Claude 2026-10-02 -->
Modules/computeCare_R/         R version of the care module and its Shiny app
Modules/R PAPER/R code/        Output processing, comparison and plotting scripts
Modules/NETLOGO/src/           Original NetLogo model (democare.nlogo and .nls files)
Modules/NETLOGO/activity/      R scripts that build the activity transition table
Modules/NETLOGO/inputs/        Place for the input tables (not included)
Modules/KINFERT/output/        Place for the KINFERT population and links files (not included)
```

Data files are not distributed. The README file in each data folder lists the files the code expects and their format.

## Requirements

- R (4.x) and RStudio
- R packages: `shiny`, `rstudioapi`, `DT` (optional, for the results table), `reshape2`; the analysis and plotting scripts also use `dplyr`, `tidyr` and `ggplot2`
- NetLogo 6, only to run the original NetLogo model

## Quick start

1. Copy the input tables into `Modules/NETLOGO/inputs/` and a KINFERT population and links pair into `Modules/KINFERT/output/`.
2. Open `Modules/computeCare_R/app.R` in RStudio and click **Run App**.
3. Use the tabs in order: **Run a simulation** (one run), **Bootstrap replicates** (several runs on resampled families), **Analyse results** (builds the three `.RData` files that the plotting script reads).

A single run can also be started from a script with `run_computeCare()` in `computeCare_baseR.R`, and a set of bootstrap replicates with `computeCare_baseR_bootstrap.R`.

A full run on the reference population (about 65,000 individuals and 10,000 egos) takes several minutes, and each bootstrap replicate is a full run.

## Documentation

<!-- >>> Claude 2026-10-02 -->
- [`docs/Democare_2026.md`](docs/Democare_2026.md): the complete documentation, converted from the Word file `Democare 2026.docx` (regenerate it with `docs/tools/docx_to_md.py`)
<!-- <<< Claude 2026-10-02 -->
- [`docs/01_model_and_kinship_files.md`](docs/01_model_and_kinship_files.md): the three modules and the KINFERT file format
- [`docs/02_input_tables.md`](docs/02_input_tables.md): disability, activity and care-hours inputs
- [`docs/03_care_simulation.md`](docs/03_care_simulation.md): yearly steps, care rules and output file
- [`docs/04_bootstrap_and_analysis.md`](docs/04_bootstrap_and_analysis.md): bootstrap resampling and processing of results
- [`docs/05_shiny_app.md`](docs/05_shiny_app.md): the computeCare Shiny app
- [`docs/06_netlogo_vs_r.md`](docs/06_netlogo_vs_r.md): known differences between the NetLogo and R versions

## Status

<!-- >>> Claude 2026-10-02 -->
- The R care module and the Shiny app run end to end (single run, bootstrap, analysis).
- KINFERT files produced with the separation option (several successive partners) are read by the R version, and the Shiny app opens zipped KINFERT files directly.
<!-- <<< Claude 2026-10-02 -->
- The comparison of R and NetLogo results is not finished. The number of hours of care from children over the ego's life differs between the two versions; see `docs/06_netlogo_vs_r.md`.
- The scripts that produce the figures of the 2022 paper are being cleaned up; some of them still contain paths from the original computers.

## Authors

Daniel Devolder. The model design is described in Spijker, Devolder and Zueras (2022) and in Calduch Verdiell et al. (2017), Papers de Demografia 456, Centre d'Estudis Demogràfics.

## License

To be decided.
