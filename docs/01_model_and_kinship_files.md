# 1. The model and the kinship files

<!-- >>> Claude 2026-10-02 -->
Summary of sections 1 and 2 of [`Democare_2026.md`](Democare_2026.md).
<!-- <<< Claude 2026-10-02 -->

## Modules

DEMOCARE has three modules:

1. **KINFERT** (<https://github.com/ddev2/KINFERT>) generates genealogies up to the fourth degree from cohort demographic parameters. For DEMOCARE it writes, for each ego aged exactly 50, the relatives who can give or receive care.
2. **computeCare** projects each family forward in yearly steps and computes the care each dependent ego needs and receives. It was first written in NetLogo and has been rewritten in R (`Modules/computeCare_R/computeCare_baseR.R`). The R code follows the NetLogo logic and writes the same output format.
3. **Analysis** code in R turns the computeCare output into indicators and plots.

## KINFERT parameters

KINFERT uses cohort parameters, entered in a dialog (one cohort, stable population) or read from a text file prepared in Excel (`cohort_values.xlsx` / `cohort_values.txt`, not included). The reference file contains Spanish values for cohorts born every ten years from 1688 to 2098; values before the twentieth century are rough approximations and values for the twenty-first century are projections.

The main parameters, for each cohort, are: the number of egos simulated; life expectancy at birth by sex; the proportion ever in union and age at first union; fertility of women in union as parity progression ratios; probability of separation; and probability of re-partnering after separation or widowhood. A second group controls birth spacing (contraceptive use and its duration after union formation and after each birth) and can in most cases be left at the standard values.

For DEMOCARE only egos, partners, children, children's partners and grandchildren are needed. Parents of ego and of ego's partner can be added if care to them is to be modelled.

## Population file

One row per individual, comma separated, with a header (reference file: about 65,000 rows for 10,000 egos).

| Column | Meaning |
| --- | --- |
| `idFamily` | Family number (optional; older files do not have it) |
| `id` | Unique individual number |
| `sex` | `F` or `M` |
| `age` | Age at the start of the simulation (egos are 50) |
| `ageDef` | Age at death |
| `status` | Educational level: `A` high, `M` medium, `B` low |
| `tickIn` | Year the individual enters the simulation (birth or union if > 0) |
| `tickOut` | Year the individual leaves (death or separation) |
| `linked` | `TRUE` if the individual has a family, `FALSE` if alone |
| `ego` | `TRUE` for the ego of each family |
| `useful` | Individual can be used in the care simulation (normally `TRUE`) |

A family always starts with its ego. Both the NetLogo and the R versions accept files with and without `idFamily`. The bootstrap needs `idFamily`, or rebuilds the family number from the links file when it is missing.

<!-- >>> Claude 2026-10-02 -->
Current age at year `t` is computed as `age + t`, except for individuals born during the simulation (`age` = 0 and `tickIn` > 0), for whom it is `ageDef - (tickOut - t)`. The first formula is needed for partners who leave by separation (their `tickOut` is the year of separation, while `ageDef` is still the age at death); on files without separation both formulas give the same ages.
<!-- <<< Claude 2026-10-02 -->

## Links file

No header. One row per link: `id1`, `id2`, type, family number.

- `M` (union) links come in pairs, one in each direction.
- `D` (descendant) links always go from the mother to each of her children.

The links file has the same name as the population file with `_link` before the extension, for example `Population_indKin.txt` and `Population_indKin_link.txt`. Birth cohort is carried in the file name (`KIN1938_indKin.txt`), not in a column.

<!-- >>> Claude 2026-10-02 -->
## Files with separation and second unions

When KINFERT is run with the separation option, the format does not change. The only difference is that a blood relative of ego (ego or a child of ego) can have several `M` links, one per union, never two at the same time. Each partner is present from `tickIn` to `tickOut`, where `tickOut` is the death or the separation; a separated partner is not followed afterwards. A separation is recognised by `age + tickOut < ageDef` (the partner leaves before dying). Only biological relatives of ego are included: there are no stepchildren, so every child of one of ego's partners is ego's child. See "Separation and second unions (R version)" in [`Democare_2026.md`](Democare_2026.md).

## Zipped files

KINFERT files are now usually zipped. The Shiny app opens them directly: the population and links files can be in one zip, in two zips (`X_indKin.zip` and `X_indKin_link.zip`), or one zipped and one plain. The files are unzipped to a temporary folder for the run and deleted afterwards; the zip files are not changed. Run outside the app, `computeCare_baseR.R` needs plain text files.
<!-- <<< Claude 2026-10-02 -->
