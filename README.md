# REDS Data Analysis

Analysis scripts for REDS (Rural Economic and Demographic Survey) data quality assessment.

## Data Coverage

| Dataset | States | Districts | Villages | Observations |
|---------|--------|-----------|----------|--------------|
| SEPRI1 | 8 (MP, RJ, HR, BR, JH, CG, AP, TN) | 44 | 102 | 53,229 |
| SEPRI2 | 5 (MH, GJ, UP, WB, OR) | 39 | 90 | 39,767 |
| **Combined** | **13** | **83** | **192** | **92,996** |

No state overlap between SEPRI1 and SEPRI2.

## Scripts

| Script | Description | Output |
|--------|-------------|--------|
| `01_obs_per_village.R` | Village-level counts (clean_reds.csv) | `figs/obs_per_village_hist.png` |
| `02_obs_per_village_full.R` | Village/panchayat distributions | `figs/obs_per_village_full_hist.png`, `figs/obs_per_panchayat_full_hist.png` |
| `03_interviewer_analysis.R` | Interviewer patterns, times, durations | `figs/interviewer_time_analysis.png` |
| `04_data_quality.R` | Missing data, impossible values, duplicates | `figs/data_quality.png` |
| `05_temporal_patterns.R` | Fieldwork timeline, daily volume, rushing | `figs/temporal_patterns.png` |
| `06_interviewer_effects.R` | Heaping, caste distributions, quality | `figs/interviewer_effects.png` |
| `07_panel_attrition.R` | Panel attrition (SEPRI2 only) | `figs/panel_attrition.png` |
| `08_shrug_merge.R` | SHRUG census linkage and validation | `figs/shrug_validation.png`, `data/reds_shrug_matched.csv` |
| `09_interviewer_fe.R` | Interviewer fixed effects / variance decomposition | `figs/interviewer_fe.png` |
| `10_inference_robustness.R` | HC1 vs cluster SE comparison, wild bootstrap | `figs/inference_robustness.png` |

## Key Findings

### Data Quality Issues

**Impossible Values:**
- 505 interviews (0.54%) with impossible duration (<0 or >8 hours)
- Mostly in SEPRI2 (442 vs 63 in SEPRI1)
- No duplicates found

**Missing Data:**
- Land ownership (q1_10): 6.6% missing overall
- Caste/religion: <0.2% missing
- Many "3rd choice" variables 99%+ missing (by design)

### Digit Heaping (Land Ownership)

Strong evidence of rounding:
- **40.4% whole numbers** (expected ~10%)
- **48.8% multiples of 0.5 acres** (expected ~20%)
- 36.6% of interviewers have >60% whole number responses

### Interviewer Patterns

**Name Sharing:**
- SEPRI1: 3,530 names, 130 represent 2+ people, 12 represent 4+ people
- SEPRI2: 2,171 names, 112 represent 2+ people, 10 represent 4+ people

**Productivity:**
- Median 3 interviews/interviewer-day (both datasets)
- After time-overlap adjustment, max ~17 interviews/person/day

**Duration:**
- SEPRI1: median 105 min
- SEPRI2: median 120 min
- End-of-day rushing: duration drops to ~60 min after 5pm

### Duration vs Quality

Shorter interviews have MORE missing data:
- SEPRI1 <60 min: 15.2% missing land; 90-120 min: 4.6%
- SEPRI2 <60 min: 8.5% missing land; 90-120 min: 2.1%
- Correlation: r = -0.37 (SEPRI1), r = -0.19 (SEPRI2)

### Panel Attrition (SEPRI2 only)

- Only 2.4% of households are panel HH
- 0.3% locked houses overall
- Non-interview reasons: 69% travelling, 24% migrated out

### Interviewer Fixed Effects (Variance Decomposition)

How much variance do interviewers explain beyond village-level differences?

| Variable | ICC (Interviewer) | R² Added After Village FE | Interpretation |
|----------|-------------------|---------------------------|----------------|
| Land (SEPRI1) | 4.1% | 3.4% | Acceptable |
| Land (SEPRI2) | 1.8% | 2.1% | Acceptable |
| **SC/ST (SEPRI1)** | **14.5%** | **10.5%** | **Concerning** |
| **SC/ST (SEPRI2)** | **16.4%** | **8.9%** | **Concerning** |
| OBC (SEPRI1) | 11.7% | - | Elevated |
| OBC (SEPRI2) | 10.4% | - | Elevated |

**Key finding:** Land ownership shows low interviewer effects (expected for objective measures). However, **caste variables show high interviewer effects** (14-16% ICC), meaning interviewers within the same village get systematically different caste distributions. Possible explanations:
- Caste boundaries are subjective (esp. OBC vs General)
- Non-random HH assignment within villages (caste-segregated hamlets)
- Interviewer bias or fabrication

### Inference Robustness (Clustering Sensitivity)

Village sizes range from 2 to 5,134 HHs (2,567x ratio). With 193 villages across 13 states, proper clustering is critical.

**Cluster structure:** 92,996 obs, 193 villages, 83 districts, 13 states

| Regression | t(HC1) | t(Village) | t(State) | Bootstrap p (State) |
|------------|--------|------------|----------|---------------------|
| Land ~ SC/ST | -54.3 | -5.9 | -3.6 | Sig* |
| Land ~ OBC | 29.4 | 3.2 | 2.1 | Sig* |
| Land ~ General | 15.2 | 1.7 | 1.1 | 0.30 |
| Has Land ~ SC/ST | -41.3 | -5.2 | -3.4 | Sig* |
| Has Land ~ OBC | 32.5 | 4.0 | 3.2 | Sig* |
| SC/ST ~ Land | -55.3 | -6.8 | -4.3 | Sig* |
| OBC ~ Land | 29.5 | 3.4 | 2.2 | Sig* |
| Land ~ Hindu | 26.2 | 2.7 | 2.4 | Sig* |
| **Land ~ Muslim** | **-20.5** | **-2.0** | **-1.6** | **0.20** |
| **SC/ST ~ Hindu** | **33.0** | **2.1** | **1.4** | **0.21** |
| **Has Land ~ Hindu** | **14.5** | **1.3** | **0.9** | **0.42** |

**Key findings:**

1. **Massive SE inflation from clustering:** Village clustering inflates SEs by 8-15x; state clustering by 10-25x. Results that look highly significant (t=15-55) under HC1 can become marginal or non-significant with proper clustering.

2. **HC1 vs Wild Bootstrap gap is enormous:** This is critical. For "Land ~ Muslim" the t-stat is -20.5 under HC1 (p < 0.0001), but wild cluster bootstrap at state level gives p=0.20. That's a result that looks like a 4-sigma finding turning into a null. Similarly, "SC/ST ~ Hindu" has t=33.0 under HC1 but bootstrap p=0.21. HC1 assumes independent observations, which is grossly violated with clustered sampling.

3. **Robust results (survive all methods):** Core caste-land relationships (SC/ST, OBC) remain highly significant even with state-level clustering and wild bootstrap:
   - Land ~ SC/ST: t=-3.6 at state level
   - SC/ST ~ Land: t=-4.3 at state level
   - Land ~ Hindu: t=2.4 at state level (survives)

4. **Fragile results (flip under clustering):** 4 of 11 specifications lose significance:
   - Land ~ General: t=15.2 (HC1) → t=1.1 (state)
   - Land ~ Muslim: t=-20.5 (HC1) → p=0.20 (bootstrap)
   - SC/ST ~ Hindu: t=33.0 (HC1) → p=0.21 (bootstrap)
   - Has Land ~ Hindu: t=14.5 (HC1) → p=0.42 (bootstrap)

**Recommendation:** For REDS data, always cluster at village level minimum. For relationships that could vary by state (religion, policy-related), use state clustering with wild bootstrap given only 13 clusters.

### SHRUG Census Linkage

**Matching Results (SEPRI2 → SHRUG via LGD):**
- 78 of 90 villages fuzzy-matched to LGD (86.7%)
- 72 villages successfully linked to SHRUG (80%)
- Match quality: 31 exact, 27 distance=1, 13 distance=2, 7 distance=3

**Validation Correlations:**
| Comparison | Correlation |
|------------|-------------|
| REDS sample size vs SHRUG households | r = 0.43 |
| REDS SC/ST % vs Census SC/ST % | r = 0.21 |

**Matched Village Characteristics (Census 2011):**
- Median population: 1,524
- Median SC/ST %: 22.8% (REDS: 26.8%)
- Median literacy rate: available in `data/reds_shrug_matched.csv`

## Geographic Identifiers

**For linking to census/SHRUG:**

| Variable | Type | Notes |
|----------|------|-------|
| `state` | Numeric | Standard census codes (e.g., 6=Rajasthan, 11=Bihar) |
| `district` | Numeric | REDS-internal codes (not census codes) |
| `village` | Numeric | REDS-internal codes (not census codes) |
| `village_name` | String | Available in Village-level files (SEPRI2/Village/) |

**Matching strategy:**
1. Use state codes directly (standard)
2. Fuzzy match village names within state to SHRUG village names
3. Or use REDS documentation for district/village code crosswalk if available

## Dependencies

```r
library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(stringdist)  # for SHRUG fuzzy matching
library(lme4)        # for interviewer FE analysis
library(fixest)      # for fast FE estimation
library(fwildclusterboot)  # for wild cluster bootstrap
library(sandwich)    # for robust SEs
library(lmtest)      # for coefficient tests
```

## Usage

```bash
cd reds
Rscript scripts/02_obs_per_village_full.R
Rscript scripts/03_interviewer_analysis.R
Rscript scripts/04_data_quality.R
Rscript scripts/05_temporal_patterns.R
Rscript scripts/06_interviewer_effects.R
Rscript scripts/07_panel_attrition.R
Rscript scripts/08_shrug_merge.R  # requires ../quota/data/lgd/ and ../quota/data/shrug/
Rscript scripts/09_interviewer_fe.R
Rscript scripts/10_inference_robustness.R
```

Outputs saved to `figs/` and `data/`.
