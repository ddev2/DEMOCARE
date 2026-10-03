# 3. Simulation of family care

<!-- >>> Claude 2026-10-02 -->
Summary of section 3 (d to f) of [`Democare_2026.md`](Democare_2026.md).
<!-- <<< Claude 2026-10-02 -->

## Start of the simulation

The population and links files are read, and each family member enters or leaves the simulation according to `tickIn` and `tickOut`. Each individual then receives, at random:

1. a dependency level, from `dependency 0.1.csv` (by age, sex and education);
2. an activity status, from `activity 0.3.csv` (by age, sex, education and dependency level).

## Yearly steps

The simulation moves forward one year per step until the last ego dies. Each year:

1. **Dependency.** For each individual, a random number is compared with the probability in `dependency_transitions 0.1.csv` for their age, sex, education and current level. If it is lower, the individual moves up one level.
2. **Activity.** The activity transition table is used in the same way, with the cumulative thresholds described in `02_input_tables.md`.
3. **Care.** Hours of care needed and available are computed for every family member and allocated within the family.

The exact order of these steps, and one step that differs between NetLogo and R, are discussed in `06_netlogo_vs_r.md`.

## Rules for giving and receiving care

Care is computed for families in which ego is at dependency level 2 or 3.

1. Biological children and grandchildren come first: a person with a partner first covers the needs of their own children, before those of their partner or parents.
2. Siblings without a partner care for each other before their parents are considered.
<!-- >>> Claude 2026-10-02 -->
3. With separation and stepchildren, brothers- and sisters-in-law neither give care to nor receive care from the group of biological siblings, but they can give care to, or receive it from, their parents-in-law. KINFERT files contain no stepchildren, so this rule never applies and is not implemented in R.
<!-- <<< Claude 2026-10-02 -->
4. The network is traversed from the top down: first the children's families, then ego. Ego receives care first from the partner and then, if needed, from the children. If ego and partner are both dependent, they share the hours available from the children.
5. Only the partner and the biological children give care to ego; children-in-law and grandchildren do not.

<!-- >>> Claude 2026-10-02 -->
## Separation and second unions (R version)

The R version reads files in which ego or a child of ego has several successive partners. At each year, the current partner is the one present (`tickIn` <= t < `tickOut`); former and future partners take no part in care. A new partner of ego acts as ego's partner for all the rules, including care to and from ego's children. A child who has separated is still counted as "partnered" in rule 2. Marital status becomes 0 single, 1 married, 2 widowed or 3 separated (from how the last partner left); the switch `SEPARATION_AS_WIDOWHOOD <- TRUE` reports separation as widowhood, as NetLogo does. `nPartner` and the partner and in-law counts cover all unions, and a new output column, `kNoPartner_div`, counts the 60 hours lost after a separation. On files without separation the results are identical to the previous version. Details in [`Democare_2026.md`](Democare_2026.md).

## Allocation algorithm ("care2")
<!-- <<< Claude 2026-10-02 -->

The allocation works as a cascade: it starts with the relatives furthest from ego (grandchildren, then children, then partner) and at each step checks whether that relative has hours left for the dependent ego. The NetLogo model has two versions of this cascade, "care1" and "care2", written so that each could be checked against the other. The R version implements only "care2"; "care1" results are obsolete.

## Output file

One row per ego and year. <!-- >>> Claude 2026-10-02 -->
The NetLogo model and `computeCare_baseR.R` write the same columns in the same order (Table 3 of the documentation), so their results can be compared directly, for example in an Excel pivot table; the only exception is `kNoPartner_div`, written by the R version only, after `kNoPartner_mort`.
<!-- <<< Claude 2026-10-02 --> Table 2 of the PDF is a dictionary of the internal variables, which the R code computes under the same names.
