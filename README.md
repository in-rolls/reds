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
| `11_jensenius_replication.R` | Replication of Jensenius (2015) with wild bootstrap | `figs/jensenius_replication.png` |
| `12_munshi_rosenzweig_replication.R` | Replication of Munshi & Rosenzweig (2015) with wild bootstrap | `figs/munshi_rosenzweig_replication.png` |

## Key Findings

### Data Quality Issues
[Script](scripts/04_data_quality.R) | [Figure](figs/data_quality.png)

**Impossible Values:**
- 990 interviews (1.06%) with impossible duration (<5 min or >4 hours)
- Mostly in SEPRI2 (779 vs 211 in SEPRI1)
- No duplicates found

**Missing Data:**
- Land ownership (q1_10): 6.6% missing overall
- Caste/religion: <0.2% missing
- Many "3rd choice" variables 99%+ missing (by design)

### Digit Heaping (Land Ownership)
[Script](scripts/06_interviewer_effects.R) | [Figure](figs/interviewer_effects.png)

Strong evidence of rounding:
- **40.4% whole numbers** (expected ~10%)
- **48.8% multiples of 0.5 acres** (expected ~20%)
- 36.6% of interviewers have >60% whole number responses

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

### Duration vs Quality
[Script](scripts/06_interviewer_effects.R) | [Figure](figs/interviewer_effects.png)

Shorter interviews have MORE missing data:
- SEPRI1 <60 min: 15.2% missing land; 90-120 min: 4.6%
- SEPRI2 <60 min: 8.5% missing land; 90-120 min: 2.1%
- Correlation: r = -0.37 (SEPRI1), r = -0.19 (SEPRI2)

### Panel Attrition (SEPRI2 only)
[Script](scripts/07_panel_attrition.R) | [Figure](figs/panel_attrition.png)

- Only 2.4% of households are panel HH
- 0.3% locked houses overall
- Non-interview reasons: 69% travelling, 24% migrated out

### Interviewer Fixed Effects (Variance Decomposition)
[Script](scripts/09_interviewer_fe.R) | [Figure](figs/interviewer_fe.png)

How much variance do interviewers explain beyond village-level differences?

| Variable | ICC (Interviewer) | R² Added After Village FE | Interpretation |
|----------|-------------------|---------------------------|----------------|
| Land (SEPRI1) | 4.1% | 3.4% | Acceptable |
| Land (SEPRI2) | 1.8% | 2.1% | Acceptable |
| **SC/ST (SEPRI1)** | **14.5%** | **10.5%** | **Concerning** |
| **SC/ST (SEPRI2)** | **16.4%** | **8.9%** | **Concerning** |
| OBC (SEPRI1) | 11.7% | 7.8% | Elevated |
| OBC (SEPRI2) | 10.4% | 7.6% | Elevated |

**Key finding:** Land ownership shows low interviewer effects (expected for objective measures). However, **caste variables show high interviewer effects** (14-16% ICC), meaning interviewers within the same village get systematically different caste distributions. Possible explanations:
- Caste boundaries are subjective (esp. OBC vs General)
- Non-random HH assignment within villages (caste-segregated hamlets)
- Interviewer bias or fabrication

### Inference Robustness (Clustering Sensitivity)
[Script](scripts/10_inference_robustness.R) | [Figure](figs/inference_robustness.png) | [Village Size Distribution](scripts/02_obs_per_village_full.R)

Village sizes range from 2 to 5,134 HHs (2,567x ratio). With 193 villages across 13 states, proper clustering is critical.

**Cluster structure:** 92,996 obs, 193 villages, 83 districts, 13 states

| Regression | t(HC1) | t(Vill) | t(State) | p(Vill Boot) | p(State Boot) | Verdict |
|------------|--------|---------|----------|--------------|---------------|---------|
| Land ~ SC/ST | -54.3 | -5.9 | -3.6 | <0.001 | 0.003 | ROBUST |
| Has Land ~ SC/ST | -41.3 | -5.2 | -3.4 | <0.001 | 0.003 | ROBUST |
| SC/ST ~ Land | -55.3 | -6.8 | -4.3 | <0.001 | 0.004 | ROBUST |
| Has Land ~ OBC | 32.5 | 4.0 | 3.2 | <0.001 | 0.018 | ROBUST |
| Land ~ Hindu | 26.2 | 2.7 | 2.4 | 0.010 | 0.047 | ROBUST |
| **Land ~ OBC** | **29.4** | **3.2** | **2.1** | **0.001** | **0.061** | **FRAGILE** |
| **OBC ~ Land** | **29.5** | **3.4** | **2.2** | **0.001** | **0.065** | **FRAGILE** |
| **Land ~ General** | **15.2** | **1.7** | **1.1** | **0.103** | **0.310** | **FRAGILE** |
| **Land ~ Muslim** | **-20.5** | **-2.0** | **-1.6** | **0.062** | **0.230** | **FRAGILE** |
| **SC/ST ~ Hindu** | **33.0** | **2.1** | **1.4** | **0.070** | **0.207** | **FRAGILE** |
| **Has Land ~ Hindu** | **14.5** | **1.3** | **0.9** | **0.195** | **0.394** | **FRAGILE** |

**Key findings:**

1. **Significance drops dramatically:** HC1: 11/11 sig → Village cluster: 8/11 → State cluster: 7/11 → Village bootstrap: 7/11 → State bootstrap: 5/11

2. **Wild bootstrap matters even with 193 clusters:** 4 specs flip from HC1 sig to village bootstrap non-sig (Land ~ General, SC/ST ~ Hindu, Land ~ Muslim marginally, Has Land ~ Hindu).

3. **Robust results (survive all 5 methods):** Only 5 specs survive:
   - Land ~ SC/ST: p=0.003 (state boot)
   - Has Land ~ SC/ST: p=0.003 (state boot)
   - SC/ST ~ Land: p=0.004 (state boot)
   - Has Land ~ OBC: p=0.018 (state boot)
   - Land ~ Hindu: p=0.047 (state boot)

4. **Fragile results (flip under bootstrap):** 6 specs fail proper inference:
   - Land ~ OBC: survives village boot (p=0.001) but NOT state boot (p=0.061)
   - Land ~ General: fails both (village p=0.103, state p=0.310)
   - Land ~ Muslim: fails both (village p=0.062, state p=0.230)
   - SC/ST ~ Hindu: fails both (village p=0.070, state p=0.207)

**Recommendation:** For REDS data, results should survive BOTH village-level (193 clusters) AND state-level (13 clusters) wild bootstrap to be trusted.

### Jensenius (2015) Replication
[Script](scripts/11_jensenius_replication.R) | [Figure](figs/jensenius_replication.png)

Re-analyzed "Development from Representation? A Study of Quotas for the Scheduled Castes in India" (AEJ:Applied, Vol. 7, No. 3, pp. 196-220) with wild cluster bootstrap. With only 15 state clusters, **4 of 10 HC1-significant results flip to non-significant:**

| Outcome | Coef | t(HC1) | t(State) | p(Boot) | Verdict |
|---------|------|--------|----------|---------|---------|
| % SC population | 7.7 | 17.5 | 6.9 | <0.001 | ROBUST |
| Literacy rate | -2.4 | -3.9 | -2.1 | 0.034 | ROBUST |
| Electricity in village | -2.6 | -2.6 | -2.2 | 0.047 | ROBUST |
| School in village | -1.1 | -2.6 | -2.4 | 0.012 | ROBUST |
| Literacy gap | -1.9 | -4.4 | -3.3 | 0.006 | ROBUST |
| Agri laborer gap | 1.3 | 3.5 | 2.8 | 0.008 | ROBUST |
| **Agricultural laborers** | **0.7** | **2.1** | **1.7** | **0.110** | **FRAGILE** |
| **Medical facility** | **-3.2** | **-2.5** | **-2.0** | **0.067** | **FRAGILE** |
| **Communication channel** | **-3.3** | **-2.9** | **-2.3** | **0.060** | **FRAGILE** |
| **Employment gap** | **0.6** | **3.0** | **1.8** | **0.100** | **FRAGILE** |
| Employment rate | 0.2 | 0.4 | 0.3 | 0.785 | (not sig) |

**Implication:** With only 15 clusters, standard clustered SEs are unreliable. Wild bootstrap is essential.

### Munshi & Rosenzweig (2015) Replication
[Script](scripts/12_munshi_rosenzweig_replication.R) | [Figure](figs/munshi_rosenzweig_replication.png)

Re-analyzed "Networks and Misallocation: Insurance, Migration, and the Rural-Urban Wage Gap" (AER, 2015). The original paper uses wild cluster bootstrap for Table 8a (15 state clusters) and standard bootstrap for Table 6 (148 caste clusters).

**Table 8a (15 state clusters):**

| Specification | Coef | t(HC1) | t(State) | p(Boot) | Verdict |
|--------------|------|--------|----------|---------|---------|
| Outmig10 ~ own inc | 0.0000 | 1.3 | 3.3 | 0.086 | Not sig |
| Outmig10 ~ jati inc | -0.0000 | -2.0 | -3.6 | 0.045 | ROBUST |
| Outmig5 ~ own inc | 0.0000 | 1.2 | 2.0 | 0.054 | Not sig |
| Outmig5 ~ jati inc | -0.0000 | -1.4 | -2.5 | 0.037 | Not sig |

**Table 6 (148 caste clusters):**

| Specification | Coef | t(HC1) | t(Caste) | p(Boot) | Verdict |
|--------------|------|--------|----------|---------|---------|
| Mig ~ own inc (1) | 0.006 | 2.2 | 3.2 | 0.054 | FRAGILE |
| Mig ~ jati inc (1) | -0.016 | -6.9 | -3.8 | 0.012 | ROBUST |
| Mig ~ own inc (2) | 0.005 | 1.9 | 2.6 | 0.111 | Not sig |
| Mig ~ jati inc (2) | -0.018 | -7.6 | -3.9 | 0.014 | ROBUST |
| Mig ~ own inc + vill FE | 0.002 | 0.8 | 0.7 | 0.502 | Not sig |
| Mig ~ jati inc + vill FE | -0.017 | -1.4 | -1.5 | 0.177 | Not sig |

**Key findings:**
- Table 8a: Authors already use wild bootstrap (appropriate for 15 clusters)
- Table 6: With 148 caste clusters, wild bootstrap is less critical but 1 result flips (Mig ~ own inc: HC1 sig → bootstrap not sig)
- The negative jati income effect on migration is robust across specifications and inference methods

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
