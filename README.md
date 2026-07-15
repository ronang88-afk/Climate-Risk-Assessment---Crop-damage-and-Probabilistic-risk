# 🌧️ Climate Risk Assessment using Extreme Value Theory

> A climate risk assessment pipeline that combines **Extreme Value Theory (Generalized Pareto Distribution)**, **GIS analysis**, and **economic risk modelling** to estimate agricultural flood losses under future climate scenarios in Zuid-Holland, Netherlands.

![R](https://img.shields.io/badge/R-276DC3?style=flat-square&logo=r)
![QGIS](https://img.shields.io/badge/QGIS-589632?style=flat-square&logo=qgis)
![MIT License](https://img.shields.io/badge/License-MIT-green.svg)

---

# Overview

This repository contains the **risk modelling stage** of a two-part climate analysis pipeline.

Using precipitation data processed in the companion NetCDF repository, the project:

- Models extreme precipitation using **Peaks Over Threshold (POT)** and the **Generalized Pareto Distribution (GPD)**
- Produces return levels for multiple climate scenarios
- Maps flood inundation using high-resolution terrain data
- Estimates **Expected Annual Damage (EAD)** for agricultural crops
- Compares historical and future climate scenarios

The project was developed as part of an MSc thesis investigating climate-related financial risk in Dutch agriculture.

---

# Repository Pipeline

```text
NetCDF Climate Data
        │
        ▼
Part 1: Climate Processing
(NetCDF → CSV)
        │
        ▼
Extreme Value Analysis
(POT + GPD)
        │
        ▼
Return Level Estimation
        │
        ▼
QGIS Terrain Analysis
        │
        ▼
Flood Depth
        │
        ▼
Depth-Damage Curves
        │
        ▼
Expected Annual Damage (€)
```

# Disclaimer
This page is for displaying the code written during my MSc thesis. This project is ongoing, the final product will include a struced output and will be in a different repo. 





# Methodology

The workflow consists of four stages.

## 1. Climate Processing

Using the companion repository:

- Process NetCDF precipitation
- Convert units
- Spatial aggregation
- Export daily time series

---

## 2. Extreme Value Analysis

Using **Peaks Over Threshold (POT)**:

- 3-day declustering
- Threshold selection
- GPD fitting
- Likelihood Ratio Tests
- Return level estimation

Return periods:

- 10 years
- 20 years
- 50 years
- 100 years
- 250 years
- 500 years
- 1000 years

---

## 3. GIS Analysis

Using **QGIS** and a **2 m Digital Terrain Model**:

- Fill sinks
- Identify depressions
- Calculate inundation depth
- Extract zonal statistics
- Produce crop-level exposure

---

## 4. Risk Modelling

Expected Annual Damage is calculated as

```text
EAD = Σ(P × V × E)
```

Where

- **P** = Annual exceedance probability
- **V** = Vulnerability (depth-damage function)
- **E** = Exposure (yield × price × area)

---

# Climate Scenarios

| Scenario | Period |
|-----------|--------|
| Historical | 1971–2024 |
| RCP 2.6 | 2025–2080 |
| RCP 4.5 | 2025–2080 |
| RCP 8.5 | 2025–2080 |

---

# Data Sources

| Dataset | Source |
|---------|--------|
| Climate Data | Copernicus Climate Data Store |
| Terrain Model | PDOK / Kadaster |
| Crop Polygons | BGT Zuid-Holland |
| Crop Yield | CBS |
| Crop Prices | Eurostat |
| Damage Curves | Joint Research Centre (JRC) |

---

# Key Findings

### Expected Annual Damage Increase

| Crop | Historical (€) | RCP 8.5 (€) | Increase |
|------|---------------:|------------:|----------:|
| Potatoes | 481,869 | 553,246 | **+14.8%** |
| Sugar Beets | 132,783 | 150,880 | **+13.6%** |
| Wheat | 19,766 | 21,560 | **+9.1%** |

### Main Results

- 🌧️ RCP 8.5 produces the largest increase in agricultural flood risk.
- 🥔 Potatoes show the highest financial vulnerability despite relatively shallow flooding.
- 🌍 Nearly **69%** of agricultural land lies within topographic depressions.
- 📈 In this model financial exposure drives risk more strongly than flood depth alone.

---

# Running the Project

## Requirements

R packages:

```r
POT
extRemes
data.table
dplyr
ggplot2
```

## Required Input Files

```
Combined_Historical_Data_1971-2024.csv
Combined_RCP2.6_2025-2080.csv
Combined_RCP4.5_2025-2080.csv
Combined_RCP8.5_2025-2080.csv
```

Run:

```r
source("risk_calculation.R")
```

---

# Outputs

| File | Description |
|------|-------------|
| final_results_summary.csv | Scenario summaries |
| final_results_detailed.csv | Plot-level risk |
| scenario_outputs_2.txt | Model diagnostics |

---

# Limitations

- Static threshold selection
- Uniform agricultural damage curves
- No pumping infrastructure modelled
- Stationary EVT assumptions

Future work includes:

- Dynamic thresholds
- Crop-specific vulnerability curves
- Drainage infrastructure
- Insurance pricing
- Precision agriculture risk mapping

---

# Related Repository

This project is the second stage of the workflow.

**Part 1**
> NetCDF Processing Pipeline → Climate Time Series Generation

---

# References

- Coles (2001) *An Introduction to Statistical Modeling of Extreme Values*
- Hosking & Wallis (1987)
- Gilleland & Katz (2016)
- Huizinga et al. (2017)
- KNMI Climate Scenarios (2023)

---

# License

MIT License

---

# Acknowledgements

- Copernicus Climate Change Service
- KNMI
- UK Met Office
- PDOK
- CBS
- Eurostat
- Joint Research Centre (JRC)

---

## Author

Created as part of an MSc thesis on **climate risk and sustainable finance**.

Feel free to open an Issue or submit a Pull Request.
