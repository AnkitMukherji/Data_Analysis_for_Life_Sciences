# Data Analysis for the Life Sciences

R and R Markdown work on the statistical foundations of biological data analysis, combining data manipulation, exploratory visualization, probability, sampling, and inference.

## Contents

| Section | Topics |
| --- | --- |
| [Week 1](Statistics%20and%20R/Week1/) | Getting started, dplyr, exploratory analysis, and exercises |
| [Week 2](Statistics%20and%20R/Week2/) | R refresher, populations/samples, random variables, central limit theorem, t-distribution, and exercises |
| [Week 3](Statistics%20and%20R/Week3/) | Confidence intervals and exercises |

The source `.Rmd` and `.R` files are the primary material. Rendered HTML and associated web assets provide reading copies where present.

## Use

Open the R Markdown sources in RStudio or compatible R tooling. Inspect each document's YAML header and package imports before rendering; use its intended R Markdown/Quarto engine. Set the working directory to match the relative paths used in that document.

The repository includes `renv/` infrastructure and an `.Rprofile`, but the current file tree does not contain a root `renv.lock`. Exact package restoration is therefore not available from a committed lockfile. Install the packages required by the selected lesson in an isolated R project.

## Why these foundations matter

Biological interpretation depends on the sampling process and the quantity being estimated. Distinguish variation among observations from uncertainty in an estimate, examine distributions before selecting tests, and interpret confidence intervals with effect sizes and study design.

This collection documents continuing statistical practice and course-based learning. Applied projects include [FRDA transcriptomics](https://github.com/AnkitMukherji/FRDA_Analysis) and [statistical genetics](https://github.com/AnkitMukherji/statistical_genetics).
