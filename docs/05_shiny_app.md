# 5. The computeCare Shiny app

<!-- >>> Claude 2026-10-02 -->
Summary of section 3i of [`Democare_2026.md`](Democare_2026.md).
<!-- <<< Claude 2026-10-02 -->

`Modules/computeCare_R/app.R` gives a point-and-click front end to the model, the bootstrap and the analysis step. Open it in RStudio and click **Run App** (or run `shiny::runApp()` from that folder). No server is needed. The app only calls the functions described in the other documents and does not change their logic.

A header shared by all tabs shows the folder of input tables (dependency, activity, care hours) and whether all files were found. **Change folder...** updates it for every tab and becomes the default the next time the app opens.

## Run a simulation

<!-- >>> Claude 2026-10-02 -->
One call to `run_computeCare()`. Set the seed and the number of years (ticks), and choose a population file; the links file is found from its name (`_link` before the extension). The population file can be a zip file: the links file is then looked for in the same zip, in `X_indKin_link.zip`, or as a plain `X_indKin_link.txt`, and the app shows where it found it. Zipped files are unzipped to a temporary folder for the run and deleted afterwards; the zip files are not changed. The main panel shows the first 200 rows, a plot of egos alive per year, and a button to save the full result.
<!-- <<< Claude 2026-10-02 -->

## Bootstrap replicates

<!-- >>> Claude 2026-10-02 -->
The resample-then-simulate loop of `computeCare_baseR_bootstrap.R`, for one population and links pair (plain or zipped, as above).
<!-- <<< Claude 2026-10-02 --> Set the number of replicates, a base seed, the number of years, a file name root and an output folder. Each replicate is written as `<root>_1_<i>.txt`.

## Analyse results

Builds the three `.RData` files read by `democare_graphs.R`. Choose **One simulation run** for a single file or **Many bootstrap replicates** for a folder. In batch mode the app scans the folder, detects the root names and the range of replicates, and marks roots with missing files as "SOME MISSING". After **Build .RData files**, the first rows of each data frame are shown.

## About

What each shared setting does and where results are written.

## Notes

- A population file whose rows do not all have the same columns as the header now stops the run at once with a message naming the file and the columns concerned.
- Only the base R engine is provided. An earlier tidyverse version of the engine was dropped in September 2026.
