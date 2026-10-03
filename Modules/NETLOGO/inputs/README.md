# Input tables (not included)

Put here the five input tables read by the care module (`Modules/computeCare_R/`), with these exact names:

- `dependency 0.1.csv`: initial proportions by dependency level, education, sex and age;
- `dependency_transitions 0.1.csv`: probabilities of moving up one dependency level;
- `activity 0.3.csv`: initial proportions by activity status (A, P, C);
- `act_transitions.csv`: activity transition thresholds (cumulative, see `docs/02_input_tables.md`);
- `care 0.1.csv`: weekly hours of care needed or available by activity, age and dependency level.

The Shiny app can also read them from another folder ("Change folder..."). The tables are described in [`docs/02_input_tables.md`](../../../docs/02_input_tables.md).
