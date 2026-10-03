# 2. Input tables

<!-- >>> Claude 2026-10-02 -->
Summary of section 3 (a to c) of [`Democare_2026.md`](Democare_2026.md). The files themselves are not included in the repository; see `Modules/NETLOGO/inputs/README.md`.
<!-- <<< Claude 2026-10-02 -->

There are three groups of inputs: disability risks (from a health or disability survey), labour force activity (from a Labour Force Survey), and hours of care needed or available.

## Disability

Source in the reference simulation: the Spanish 2008 Survey on Disability, Personal Autonomy and Dependency Situations (EDAD2008). Four dependency levels, 0 (none) to 3.

- **`dependency 0.1.csv`** (stock): proportion of persons at each dependency level by age, sex and educational level, used to assign the level at the start. Persons under 20 get the values of age 20 (variable `ageRecDep`), because the survey has too few cases at younger ages. The `CCAA` column is always `Total` (all of Spain).
- **`dependency_transitions 0.1.csv`** (flow): yearly probability of moving up one level, by age, sex, education and current level.

The transitions are derived from the stock table as if it described a synthetic cohort. For each sex, age and education level a 4 by 4 linear system is solved:

```
dp(0) = -m(0->1)
dp(1) =  m(0->1) - m(1->2)
dp(2) =  m(1->2) - m(2->3)
dp(3) =  m(2->3)
```

where `dp(i)` is the change in the proportion at level `i` between ages x and x+1. The transition probability is `m(i->i+1) / p(i)`. Two assumptions: dependency can only increase, one level at a time; and mortality does not depend on dependency level.

## Labour force activity

Three states: `A` not working, `P` part time, `C` full time.

- **`activity 0.3.csv`** (stock): proportion in each state by age, sex, dependency level and education, from EDAD2008. Activity starts at 16 and ends at 75.
- **`act_transitions.csv`** (flow): yearly transition probabilities between the three states, in both directions.

Because transitions are reversible there are six unknowns for three equations, so a second source is needed: two waves of the Spanish Labour Force Survey (EPA), 2nd quarter 2007 and 2nd quarter 2008, which give transition rates by age and sex (but not by dependency). An iterative procedure adjusts the EPA rates, separately for each sex, age, education and dependency level, until the projected distribution at age x+1 matches the EDAD2008 distribution (equations 1 to 3 in the PDF). The results are smoothed with loess.

Scripts (in `Modules/NETLOGO/activity/`):

- `compute_activity_transitions.R`: yearly transition rates by age and sex from the two EPA waves.
- `smooth_actividad.R`: combines them with the EDAD2008 table and writes `activity.csv` and `act_transitions.csv`.
- `check_convergence_unimodality.R`: diagnostics for the iterative step.

### Cumulative encoding in `act_transitions.csv`

From each state there are two possible destinations. To choose among "stay", "destination 1" and "destination 2" with one random number per person and year, the file stores for each origin the first transition probability (L) and the sum of both (H):

- random number < L: move to the first destination
- L <= random number < H: move to the second destination
- random number >= H: no change

This is built by `calc_proba()` in `smooth_actividad.R`. The R model checks the file and warns if many thresholds exceed 1, which would mean the file contains raw rather than cumulative probabilities.

## Hours of care (`care 0.1.csv`)

Weekly hours of care needed (negative) or available (positive), loosely based on EDAD2008 tabulations.

| Activity status | Age | Dep0 | Dep1 | Dep2 | Dep3 |
| --- | --- | --- | --- | --- | --- |
| Not employed | 0-4 | -20 | -20 | -30 | -80 |
| | 5-10 | -10 | -20 | -30 | -80 |
| | 11-16 | -5 | -16 | -30 | -80 |
| | 17-105 | 60 | 30 | -30 | -80 |
| Part time | 17-105 | 45 | 22.5 | -30 | -80 |
| Full time | 17-105 | 30 | 15 | -30 | -80 |
