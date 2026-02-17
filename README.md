# DegreeDayCalc

**DegreeDayCalc** is an R package that provides an interactive **Shiny application** for calculating insect phenology using **degree-day (thermal time) models**.  
It allows users to explore developmental progression across life stages (egg, larval/nymphal instars, pupa, adult, and preoviposition) using multiple degree-day calculation methods.

The package is designed for **entomologists, ecologists, agronomists, and pest management researchers** interested in modeling temperature-driven development.

---

## Features

- Interactive **Shiny application**
- Multiple degree-day calculation methods:
  - Average
  - Average (cut)
  - Triangular
  - Triangular with upper threshold
  - Sine
  - Sine with upper threshold
- Flexible life-stage definitions:
  - Egg
  - Larva 1–6 *or* Nymph 1–6 (user selectable)
  - Pupa
  - Adult
  - Preoviposition
- Upload daily temperature data (CSV)
- Automatic assignment of developmental stages
- Visualization of cumulative degree-days and stage transitions
- Downloadable results table (CSV) and figure (PNG)

---

## Installation

### From GitHub (development version)

```r
# install.packages("devtools")
devtools::install_github("almarazkrae-4081/DegreeDayCalc")
