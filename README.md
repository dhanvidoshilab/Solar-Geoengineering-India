# Solar Geoengineering Research: India's Scientific Contribution

## Overview

This project looks at how India's scientific contribution to solar geoengineering research compares with that of leading high-income countries. Solar geoengineering (sometimes called solar radiation management) refers to proposed methods of deliberately reflecting sunlight to cool the planet. As this field gets more scientific and policy attention, this analysis asks a simple question: is India, despite being one of the most climate-vulnerable countries in the world, contributing to this research at a level comparable to leading high-income countries?

## Research Question

As solar geoengineering gains increasing scientific and policy attention, this study examines whether India, despite its high vulnerability to climate change, is contributing to the research shaping this emerging technology at a level comparable to leading high-income countries.

## Data Sources

1. **Scopus** (academic publication database). Searched using the terms "solar geoengineering," "solar radiation management," and "solar radiation modification." The initial search returned 931 unique publications. These were manually screened for relevance (removing false positives like unrelated solar-panel or solar-energy research), leaving a final screened dataset of 900 publications used for analysis.
2. **ND-GAIN Country Index** (Notre Dame Global Adaptation Initiative). Used to pull each country's climate vulnerability score, which provides context for why this comparison matters in the first place.

## Methodology Summary

Each publication is credited to every country represented by at least one author's institutional affiliation. So a paper with one author in India and one in the UK counts fully toward both countries. This is a standard "whole counting" approach in bibliometrics.

Countries were ranked by total publication count, and whichever countries came out on top became the natural comparison set for India, rather than picking comparison countries ahead of time.

A per-capita metric (publications per million people) was also calculated to account for population differences between countries. India's climate vulnerability was contextualized using the ND-GAIN index, comparing India's score against the same set of comparison countries.

## Repository Structure

```
solar-geoengineering-india/
├── README.md                              this file
├── analyze_scopus_export.R                main analysis script (R)
├── data/
│   └── scopus_export_screened_final.csv   final, screened dataset (900 publications)
├── results/
│   ├── country_publication_counts.csv     publication counts by country
│   ├── country_percapita_comparison.csv   publications per capita by country
│   ├── india_top_institutions.csv         leading Indian institutions by publication count
│   └── ndgain_map_data.csv                ND-GAIN climate vulnerability scores by country
```

## How to Rerun This Analysis

This script is built to run on any computer with R installed, without much setup.

1. Download this repository (or just `analyze_scopus_export.R` and `data/scopus_export_screened_final.csv`).
2. Put both files in the same folder on your computer.
3. Open `analyze_scopus_export.R` in RStudio.
4. Set your working directory to that folder: Session > Set Working Directory > To Source File Location.
5. Click Source (or run the script). Required R packages (dplyr, stringr, readr, ggplot2, tidyr) will install automatically if they're not already present.
6. The script will detect the CSV file in the folder on its own, run the full analysis, and save all output files (matching what's in the `results/` folder) into the same directory.

No manual file path editing is needed. The script finds its own input file and doesn't depend on any particular computer's folder setup.

## Visualizations

Final charts (built in Datawrapper and Flourish, using the data in `results/`) are linked in the written report:

- Solar geoengineering publications by country (bar chart)
- Publications per capita by country (bar chart)
- Publication trend over time, 2009 to 2025 (interactive line chart)
- Climate vulnerability by country (choropleth map)

## Known Limitations

The 900-publication dataset reflects manual screening of the full search results. A random sample of 150 publications came back around 95% relevant, but screening decisions do involve some subjectivity.

Population and vulnerability figures come from current published sources (World Bank population estimates, ND-GAIN Country Index). These are recent estimates, not exact figures for the study period.

This analysis measures publication counts as a stand-in for research contribution. It doesn't account for research quality, citation impact, or funding levels.

## Author

Dhanvi Doshi, NYU MSPP, Summer 2026
