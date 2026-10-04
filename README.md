# REDS Data Analysis

Analysis scripts for REDS (Rural Economic and Demographic Survey) data quality assessment.

## Data Coverage

| Dataset | States | Districts | Villages | Observations |
|---------|--------|-----------|----------|--------------|
| SEPRI1 | 8 (MP, RJ, HR, BR, JH, CG, AP, TN) | 44 | 102 | 53,229 |
| SEPRI2 | 5 (MH, GJ, UP, WB, OR) | 39 | 90 | 39,767 |
| **Combined** | **13** | **83** | **192** | **92,996** |

No state overlap between SEPRI1 and SEPRI2.

## Key Findings

### Sample Structure
[Script](scripts/02_obs_per_village_full.R) | [Figure](figs/obs_per_village_full_hist.png)

Village sample sizes range from 2 to 5,134 HH (2,567x ratio). This variation matters for weighting and clustered inference.

**Cluster structure:** 92,996 obs across 193 villages, 83 districts, 13 states

### Panel Attrition (SEPRI2 only)
[Script](scripts/07_panel_attrition.R) | [Figure](figs/panel_attrition.png)

- Only 2.4% of households are panel HH
- 0.3% locked houses overall
- Non-interview reasons: 69% travelling, 24% migrated out

### Data Quality Issues
[Script](scripts/04_data_quality.R) | [Figure](figs/data_quality.png)

**Duration flags:**
- 990 interviews (1.06%) with recorded duration below 5 minutes or above 4 hours
- Mostly in SEPRI2 (779 vs 211 in SEPRI1)
- No duplicates found

**Missing Data:**
- Land ownership (q1_10): 6.6% missing overall
- Caste/religion: <0.2% missing
- Many "3rd choice" variables 99%+ missing (by design)

### Interviewer Patterns
[Script](scripts/03_interviewer_analysis.R) | [Figure](figs/interviewer_time_analysis.png)

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

### Digit Heaping (Land Ownership)
[Script](scripts/06_interviewer_effects.R) | [Figure](figs/interviewer_effects.png)

Strong evidence of rounding:
- **40.4% whole numbers** (expected ~10%)
- **48.8% multiples of 0.5 acres** (expected ~20%)
- 36.6% of interviewers have >60% whole number responses

### Duration vs Quality
[Script](scripts/06_interviewer_effects.R) | [Figure](figs/interviewer_effects.png)

Shorter recorded interviews have more missing land responses:
- SEPRI1 <60 min: 15.2% missing land; 90-120 min: 4.6%
- SEPRI2 <60 min: 8.5% missing land; 90-120 min: 2.1%
- Correlation: r = -0.37 (SEPRI1), r = -0.19 (SEPRI2)

### Interviewer Fixed Effects (Variance Decomposition)
[Script](scripts/09_interviewer_fe.R) | [Figure](figs/interviewer_fe.png)

How much variance do interviewers explain beyond village-level differences?

| Variable | ICC (Interviewer) | R² Added After Village FE |
|----------|-------------------|---------------------------|
| Land (SEPRI1) | 4.1% | 3.4% |
| Land (SEPRI2) | 1.8% | 2.1% |
| **SC/ST (SEPRI1)** | **14.5%** | **10.5%** |
| **SC/ST (SEPRI2)** | **16.4%** | **8.9%** |
| OBC (SEPRI1) | 11.7% | 7.8% |
| OBC (SEPRI2) | 10.4% | 7.6% |

**Key finding:** Land ownership shows low interviewer effects (expected for objective measures). However, **caste variables show high interviewer effects** (14-16% ICC), meaning interviewers within the same village get systematically different caste distributions. Possible explanations:
- Caste boundaries are subjective (esp. OBC vs General)
- Non-random HH assignment within villages (caste-segregated hamlets)
- Differences in classification, recording or household assignment that require record-level follow-up

### Inference Robustness (Clustering Sensitivity)
[Script](scripts/10_inference_robustness.R) | [Figure](figs/inference_robustness.png)

With 193 villages across 13 states and highly unequal cluster sizes, proper clustering is critical.

| Regression | t(HC1) | t(Vill) | t(State) | p(Vill Boot) | p(State Boot) |
|------------|--------|---------|----------|--------------|---------------|
| Land ~ SC/ST | -54.3 | -5.9 | -3.6 | <0.001 | 0.003 |
| Has Land ~ SC/ST | -41.3 | -5.2 | -3.4 | <0.001 | 0.003 |
| SC/ST ~ Land | -55.3 | -6.8 | -4.3 | <0.001 | 0.004 |
| Has Land ~ OBC | 32.5 | 4.0 | 3.2 | <0.001 | 0.018 |
| Land ~ Hindu | 26.2 | 2.7 | 2.4 | 0.010 | 0.047 |
| **Land ~ OBC** | **29.4** | **3.2** | **2.1** | **0.001** | **0.061** |
| **OBC ~ Land** | **29.5** | **3.4** | **2.2** | **0.001** | **0.065** |
| **Land ~ General** | **15.2** | **1.7** | **1.1** | **0.103** | **0.310** |
| **Land ~ Muslim** | **-20.5** | **-2.0** | **-1.6** | **0.062** | **0.230** |
| **SC/ST ~ Hindu** | **33.0** | **2.1** | **1.4** | **0.070** | **0.207** |
| **Has Land ~ Hindu** | **14.5** | **1.3** | **0.9** | **0.195** | **0.394** |

Significance drops from 11/11 (HC1) to 5/11 (state bootstrap). Only 5 specs survive all methods; 6 flip under wild bootstrap. Results should survive both village-level (193 clusters) and state-level (13 clusters) wild bootstrap to be trusted.

### SHRUG Census Linkage
[Script](scripts/08_shrug_merge.R) | [Figure](figs/shrug_validation.png)

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

## Replications

Re-analysis of published papers using wild cluster bootstrap to assess inference robustness with few clusters.

### Jensenius (2015)
[Script](scripts/11_jensenius_replication.R) | [Figure](figs/jensenius_replication.png)

Re-analyzed "Development from Representation? A Study of Quotas for the Scheduled Castes in India" (AEJ:Applied, Vol. 7, No. 3, pp. 196-220) with wild cluster bootstrap. With only 15 state clusters, **4 of 10 HC1-significant results flip to non-significant:**

| Outcome | Coef | t(HC1) | t(State) | p(Boot) |
|---------|------|--------|----------|---------|
| % SC population | 7.7 | 17.5 | 6.9 | <0.001 |
| Literacy rate | -2.4 | -3.9 | -2.1 | 0.034 |
| Electricity in village | -2.6 | -2.6 | -2.2 | 0.047 |
| School in village | -1.1 | -2.6 | -2.4 | 0.012 |
| Literacy gap | -1.9 | -4.4 | -3.3 | 0.006 |
| Agri laborer gap | 1.3 | 3.5 | 2.8 | 0.008 |
| **Agricultural laborers** | **0.7** | **2.1** | **1.7** | **0.110** |
| **Medical facility** | **-3.2** | **-2.5** | **-2.0** | **0.067** |
| **Communication channel** | **-3.3** | **-2.9** | **-2.3** | **0.060** |
| **Employment gap** | **0.6** | **3.0** | **1.8** | **0.100** |
| Employment rate | 0.2 | 0.4 | 0.3 | 0.785 |

With only 15 clusters, standard clustered SEs are unreliable. Wild bootstrap is essential.

### Munshi & Rosenzweig (2015)
[Script](scripts/12_munshi_rosenzweig_replication.R) | [Figure](figs/munshi_rosenzweig_replication.png)

Re-analyzed "Networks and Misallocation: Insurance, Migration, and the Rural-Urban Wage Gap" (AER, 2015). The original paper uses wild cluster bootstrap for Table 8a (15 state clusters) and standard bootstrap for Table 6 (148 caste clusters).

**Table 8a (15 state clusters):**

| Specification | Coef | t(HC1) | t(State) | p(Boot) |
|--------------|------|--------|----------|---------|
| Outmig10 ~ own inc | 0.0000 | 1.3 | 3.3 | 0.086 |
| Outmig10 ~ jati inc | -0.0000 | -2.0 | -3.6 | 0.045 |
| Outmig5 ~ own inc | 0.0000 | 1.2 | 2.0 | 0.054 |
| Outmig5 ~ jati inc | -0.0000 | -1.4 | -2.5 | 0.037 |

**Table 6 (148 caste clusters):**

| Specification | Coef | t(HC1) | t(Caste) | p(Boot) |
|--------------|------|--------|----------|---------|
| Mig ~ own inc (1) | 0.006 | 2.2 | 3.2 | 0.054 |
| Mig ~ jati inc (1) | -0.016 | -6.9 | -3.8 | 0.012 |
| Mig ~ own inc (2) | 0.005 | 1.9 | 2.6 | 0.111 |
| Mig ~ jati inc (2) | -0.018 | -7.6 | -3.9 | 0.014 |
| Mig ~ own inc + vill FE | 0.002 | 0.8 | 0.7 | 0.502 |
| Mig ~ jati inc + vill FE | -0.017 | -1.4 | -1.5 | 0.177 |

The negative jati income effect on migration is robust across specifications and inference methods.

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
| `11_jensenius_replication.R` | Replication of Jensenius (2015) with wild bootstrap | `figs/jensenius_replication.png` |
| `12_munshi_rosenzweig_replication.R` | Replication of Munshi & Rosenzweig (2015) with wild bootstrap | `figs/munshi_rosenzweig_replication.png` |

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
