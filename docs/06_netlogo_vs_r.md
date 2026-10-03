# 6. NetLogo and R versions: known differences

This page records the state of the comparison between the original NetLogo model (`Modules/NETLOGO/src/democare.nlogo`) and the R rewrite (`Modules/computeCare_R/computeCare_baseR.R`). The comparison is not finished.

## Open question

The number of hours of care given by children over the ego's life differs between the two versions. The differences below are the most likely explanation, but this still has to be confirmed with a full comparison run.

## Order of steps within one year

- **NetLogo** (`letsGo`): new entrants, then dependency transition (`UpdateDependency`), then re-draw of activity for individuals whose dependency changed (`setActivity`), then activity transition (`UpdateActivity`, ages 15 to 74 only), then survival, then care, and the output row is written last.
- **R**: new entrants, then care and the output row, then dependency transition, then activity transition.

## Differences that follow

1. **Timing of the output row.** In NetLogo the row for year t reflects the transitions of year t; in R it reflects the state carried over from year t-1. This is a one-year shift in the state variables for any given year label.
2. **Activity re-draw when dependency increases.** In NetLogo, when an individual moves up one dependency level, their activity is reset and drawn again from the stock activity table at the new level. The R version does not do this. Since the stock table moves sharply towards "not working" as dependency rises, NetLogo takes newly dependent people out of the labour force faster than R does, which leaves more care available in R. This is a real gap in the rewrite, not a detail, and is the first candidate to explain the difference in care from children.
3. **Age window for activity transitions.** NetLogo applies them only between ages 15 and 74; R applies them at every age and relies on the values at the edges of the table.
4. **Ages above 104.** NetLogo uses the age-104 dependency row for older ages and keeps applying transitions; R finds no row and leaves these individuals unchanged.

## Tools

- `Modules/R PAPER/R code/compare_netlogo_vs_r.R`: compares NetLogo and R output for the same input population.
- `Modules/R PAPER/R code/compare_three_sources.R` and `democare_compare_helpers.R`: comparison of three outputs of the same population.

## Next steps

- Decide whether the R version should reproduce points 2 to 4, or whether they are recorded as deliberate differences.
- Run both versions on the same population with the same settings and compare care from children and from the partner over the ego's life.
