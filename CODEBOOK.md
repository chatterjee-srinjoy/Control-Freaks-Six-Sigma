# MedHive Codebook  
_Last updated: 2025-10-18_

---

## Files

All CSV files share the same structure and variables.

| File | Description |
|------|--------------|
| `medhive_facility_A_waste_with_reason.csv` | Facility with **high waste** rates (frequent transfers and medication changes). |
| `medhive_facility_B_waste_with_reason.csv` | Facility with **low waste** rates (more stable chronic therapies). |
| `medhive_facility_C_waste_with_reason.csv` | Facility with **balanced** waste and more end-of-life cases. |

Each file can be analyzed independently or together in the **MedHive Dashboard**.

---

## Variable Dictionary

| Variable | Type | Unit / Format | Description |
|-----------|------|---------------|--------------|
| `prescription_id` | string | text | Unique identifier for the prescription fill. |
| `patient_id` | string | text | De-identified patient code (one patient may have multiple prescriptions). |
| `medication` | string | text | Medication or drug class name. |
| `scheduled_date` | date | YYYY-MM-DD | Intended start or dispense date. |
| `refill_date` | date | YYYY-MM-DD | Actual refill or dispense date. |
| `supply_length` | integer | days | Length of medication supply period. |
| `cost_usd` | numeric | USD ($) | Total cost of the dispensed medication. |
| `prescriber` | string | text | Name of the prescribing clinician. |
| `initial_weight` | numeric | **grams (g)** | Total mass of medication dispensed. |
| `weight_at_stop` | numeric | **grams (g)** | Mass of unused medication remaining when therapy stopped or changed. |
| `percent_wasted` | numeric | percent (0–100) | Portion of dispensed medication wasted: `(weight_at_stop / initial_weight) × 100`. |
| `waste_reason` | categorical | controlled vocabulary | Cause of medication waste (e.g., “Medication change”, “Non-adherence”, “Deceased”). |
| `transfer_type` | string | text | Type of transfer event (if applicable). |
| `transfer_date` | date | YYYY-MM-DD | Date of transfer (if applicable). |

---
