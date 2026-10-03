# KINFERT files (not included)

Put here a population file and its links file produced by KINFERT, for example:

- `Population_indKin.txt`: one row per individual, with header (`idFamily`, `id`, `sex`, `age`, `ageDef`, `status`, `tickIn`, `tickOut`, `linked`, `ego`, `useful`);
- `Population_indKin_link.txt`: one row per link, no header (`id1`, `id2`, type `M` or `D`, family number).

The links file has the name of the population file with `_link` before the extension. Both files may be zipped when they are opened from the Shiny app. The format is described in [`docs/01_model_and_kinship_files.md`](../../../docs/01_model_and_kinship_files.md).
