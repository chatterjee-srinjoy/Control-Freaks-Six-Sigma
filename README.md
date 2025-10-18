# 🐝 MedHive: Nursing Home Medication Waste Dashboard

**Team:** MedHive  
**Event:** Cornell Six Sigma Hackathon 2025  
**Category:** Health Systems (Prompt 4)

---

## 🧩 Overview

**MedHive** is a quality-control dashboard built in **R (Shiny)** to track and reduce **prescription medication waste** in nursing homes.  
It helps staff identify preventable causes, visualize trends across medications and prescribers, and test “first-fill” policy simulations to reduce waste costs.

---

## ⚙️ Features

- **Charts:** Pareto, distributions, and SPC-style time series  
- **Cause breakdowns:** By medication, prescriber, and waste reason  
- **Operational alerts:** Flags preventable or process-related waste  
- **Policy simulator:** Estimates savings from shorter first fills  
- **Ready to demo:** Works with included synthetic datasets

---

## 📦 Files

| File | Description |
|------|-------------|
| `app.R` | Shiny dashboard (main file to run). |
| `CODEBOOK.md` | Variables, units, and dataset descriptions. |
| `medhive_facility_A_waste_with_reason.csv` | High-waste facility dataset. |
| `medhive_facility_B_waste_with_reason.csv` | Low-waste facility dataset. |
| `medhive_facility_C_waste_with_reason.csv` | Balanced facility dataset. |

---

## 🧠 How to Run the Dashboard

### Option 1: In **RStudio (Local)**

To install required packages and launch the dashboard:

```r
# install dependencies
install.packages(c(
  "shiny", "tidyverse", "lubridate", "scales",
  "DT", "janitor", "bslib"
))

# run the app
shiny::runApp("app.R")
```

Then upload one or more of the provided CSVs when prompted.

---

### Option 2: In **Posit Cloud**

```r
# steps:
# 1. Create a new R project
# 2. Upload app.R and all CSVs
# 3. Install required packages
install.packages(c(
  "shiny", "tidyverse", "lubridate", "scales",
  "DT", "janitor", "bslib"
))

# 4. Run the app
shiny::runApp("app.R")
```

---

## 📊 Example Insights

- **Top Prescribers (Waste):** Shows clinicians with highest preventable waste cost  
- **Operational Alerts:** Flags causes like refills after death or transfer-related waste  
- **Pareto Chart:** Highlights medications contributing most to total waste  
- **Policy Simulator:** Models savings from reducing first-fill fractions

---

## 📂 Data Reference

See **[`CODEBOOK.md`](./CODEBOOK.md)** for:
- Dataset descriptions for Facilities A, B, and C  
- Full variable dictionary with units and controlled vocabularies  

> All weights (`initial_weight`, `weight_at_stop`) are in **grams (g)**.  
> All costs (`cost_usd`) are in **USD ($)**.

---

## 🧾 License & Attribution

- Synthetic data for educational demonstration  
- © 2025 **Team MedHive**, Cornell Systems Engineering M.Eng.  
- Licensed under **CC BY 4.0**

---

### 🐝 “Smarter Prescriptions. Less Waste.”
