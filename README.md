# Study Habits Robust Regression

> Original survey and robust regression revealing which habits really drive study time.

## Overview

- Group project with 3 teammates identifying which everyday habits move daily study hours, and which only look important.

## Data / Scope

- Original Google Forms survey, 45 usable responses after cleaning; outcome is daily study hours with 8 predictors (age, credits, sleep, social media, work, activities, commute, exercise).

## Workflow

- Fit OLS; the Breusch-Pagan test (p = 0.037) revealed heteroskedasticity.
- Compared HC3 robust errors, WLS, Robust WLS and GLS, plus normality, autocorrelation and multicollinearity checks.
- Used stepwise selection and outlier analysis; chose OLS with HC3 errors.

## Results

- Organizational activity time was the strongest positive predictor, significant at 1% in every model version.
- More sleep was linked to less study time, and this held after correction.
- Several predictors lost significance once robust correction was applied.

## Tech Stack

OLS regression, HC3 robust errors, WLS / GLS, Breusch-Pagan test, Google Forms

## Repository Structure

```
r/                  R scripts: cleaning and visualization, analysis, modelling (run in order)
data/               Survey data (respondent names replaced by anonymous IDs)
report/             Final report (PDF)
```

## How to Run

1. Install R packages used in the scripts (`readxl`, `dplyr`, `ggplot2`, `lmtest`, `sandwich`, `car` and similar; see `library()` calls at the top of each script).
2. Run `r/01_...R`, `r/02_...R`, then `r/03_...R` from the repository root so the `data/survei_anonymized.xlsx` path resolves.

## Team

Regression Analysis course project by Britney, Ivana, Nadya and Vania.

## Notes

- Original absolute file paths in the scripts were replaced with `data/survei_anonymized.xlsx`; the analysis logic is unchanged.
- Respondent names were removed from the survey data to protect privacy.
- The 32 MB presentation PDF is not included (slides are linked from the course submission).

## Author

Nadya Angelie Lislie, BINUS University (Computer Science and Statistics). [LinkedIn](https://www.linkedin.com/in/nadya-angelie-lislie/) | [GitHub](https://github.com/Nadya-AngelieLislie)

## Continuous Integration

GitHub Actions (`.github/workflows/ci.yml`) runs `ruff` and `pytest` repository-health checks on every push; run them locally with `make lint test`.
