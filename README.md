# Fire Adapted Forests: Evidence for Carbon Durability via Wildfire Severity Reduction  

Welcome to the repository accompanying the **"Fire Adapted Forests"** publication, which provides evidence for the role of forest treatments in stabilizing carbon storage, reducing wildfire severity, and enhancing resilience in mixed-conifer forests of the Central Sierras.

This repository contains code, data, and workflows for reproducing the analyses presented in the publication, leveraging **dynamic performance baselines** and **natural experimental design** techniques.


## 📝 Citing the Publication

If you use this repository in your work, please cite:

[“Up in Smoke?  Evidence for Stabilized Carbon Storage via Operational Resilience in Frequent Fire Forests”](https://osf.io/72rj4)

Authors:
Ethan Yackulic, Micah Elias, Joe Shannon, Sophie Gilbert, Michael Koontz, Spencer Plumb, Matthew Sloggy, Katharyn Duffy 

Journal: Frontiers in Forests and Global Change

Pre-print DOI: [10.17605/OSF.IO/3UNGR](https://osf.io/3ungr/)


---

## 🚩 Repository Overview

This repository includes:
- **Code**: Scripts for data processing, statistical analysis, and visualization.
- **Data**: Publicly available datasets, including pre-processed input files for modeling.
- **Figures**: Scripts to reproduce the publication figures and additional exploratory visualizations.
- **Documentation**: Detailed instructions for running the analyses and adapting them for other regions.

---

## 🔍 Key Features
- **Dynamic Performance Benchmarks**: Methods for evaluating forest treatment effectiveness over time, using difference-in-differences and census-based reference regions.
- **Wildfire Severity**: Tools to analyze burn severity and its impact on carbon stability.
- **Carbon Stability Assessment**: Framework for quantifying carbon loss and recovery post-treatment and post-wildfire.
- **Scalable Workflow**: Methods adaptable to other forest types and disturbance regimes.

---

## 📂 Directory Structure



---

## 🛠️ Getting Started

### 1. Clone the Repository

```bash
git clone https://github.com/Vibrant-Planet-Open-Science/Fire-Adapted-Forests.git
cd fire-adapted-forests
```

### 2. Install Required Dependencies

Ensure you have R (version 4.1 or later) and the following R packages:
	•	data.table
	•	tidyverse
	•	bayestestR
	•	ggplot2

Install them via CRAN:

``` 
install.packages(c("data.table", "tidyverse", "bayestestR", "ggplot2"))
```
