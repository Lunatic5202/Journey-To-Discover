# Journey To Discover
**Google Review Counts & Hidden-Gem Insight**  
Trio Explorers · CSE DS

---

## Team
| Name | Roll No | Autonomy No |
|---|---|---|
| Sanchayan Adhya | 2462048 | 12624019043 |
| Aryan Kumar | 2462065 | 12624019016 |
| Aditi Shambhavi | 2462045 | 12624019001 |
| Rahul Mahato | 2462077 | 12625019080 |

---

## Project Structure
```
journey_to_discover/
├── journey_to_discover.Rproj     ← open in RStudio
├── README.md
├── data/
│   ├── travel_dataset.csv        ← Dataset 1 (325 places, real data)
│   ├── india_tourism_dataset.json← Dataset 2 (100 destinations, real data)
│   ├── dataset_schema.json
│   └── destination_names.txt
├── R/
│   ├── install_packages.R        ← run once
│   └── analysis.R                ← full standalone script
├── report/
│   └── journey_to_discover.Rmd  ← knit to HTML/PDF
└── output/                       ← all plots saved here
    ├── fig1_review_distribution.png
    ├── fig2_popularity_tiers.png
    ├── fig3_reviews_vs_rating.png
    ├── fig4_reviews_by_zone.png
    ├── fig5_top15_reviewed.png
    ├── fig6_hidden_gems_by_state.png
    └── fig7_budget_vs_popularity.png
```

---

## Setup
```r
# 1. Open journey_to_discover.Rproj in RStudio
# 2. Install packages (once)
source("R/install_packages.R")
# 3a. Run full script
source("R/analysis.R")
# 3b. Or knit the report
rmarkdown::render("report/journey_to_discover.Rmd")
```

---

## Datasets (real data included)
- **Dataset 1** — `travel_dataset.csv`: 325 Indian places with Google review counts, ratings, zones, types, significance, entrance fees etc.
- **Dataset 2** — `india_tourism_dataset.json`: 100 curated destinations with popularity scores, hidden gems lists, budget ranges, safety ratings, accessibility info.
- Both joined on **State**.

## Key Insights
1. **Low-review majority** — most destinations have < 0.5L reviews; visibility is highly concentrated.
2. **Hidden gems link** — Dataset 2 validates low-visibility destinations as hidden gems.
3. **Recommendation value** — popularity alone is a poor guide; high-rated spots span all review tiers.

> *Underrated ≠ Inferior.*


---

## Running the Shiny App

```r
# Option 1 — from RStudio console (with project open)
shiny::runApp("R/shiny_app.R")

# Option 2 — from any R session
setwd("path/to/journey_to_discover")
shiny::runApp("R/shiny_app.R")
```

The app opens in your browser automatically. It has four tabs:

| Tab | What's there |
|---|---|
| **Hidden Gem Explorer** | Search/filter 100 destinations by region, season, budget, gem score, trip type. Cards show all hidden gems, review quote, budget range. |
| **Review Analysis** | Interactive histogram, box plot, tier breakdown, reviews vs rating scatter, zone violin plots, top 15 bar chart. |
| **Hidden Gem Deep Dive** | Gem count by state (coloured by popularity), budget vs popularity bubble chart, full sortable/filterable data table. |
| **Key Insights** | Team info, the three findings, and the gem score formula explained. |

### Required packages
```r
source("R/install_packages.R")
```
Packages needed: `shiny`, `bslib`, `tidyverse`, `jsonlite`, `janitor`, `DT`, `plotly`, `scales`, `here`
