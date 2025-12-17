# Dataset Classification Scheme for vcdExtra Package

## Overview

This document outlines a classification scheme for the 45 datasets in the vcdExtra R package using roxygen `@concept` tags. These datasets are designed to illustrate statistical and graphical methods for categorical data analysis.

## Analysis Summary

The datasets in `R/data.R` form a comprehensive collection focused on:
- **Contingency table analysis** - Nearly all datasets are cross-tabulations
- **Graphical representation** - Heavy emphasis on mosaic displays and related visualizations
- **Loglinear modeling** - Core statistical method across most datasets
- **Ordinal variables** - Many datasets feature ordered factors
- **Classical datasets** - Mix of historical statistical classics and modern applied examples
- **Diverse applications** - Wide range from social mobility to medical data to consumer preferences
- **Educational value** - Well-documented with extensive examples of model fitting and visualization

---

## Recommended @concept Classification Scheme

### 1. **Statistical Methods** (primary analysis techniques)
- `loglinear-models` - Datasets for loglinear model analysis (~40+ datasets)
- `logistic-regression` - Binary/multinomial logistic models (Donner, ICU, Accident, Titanicp)
- `poisson-regression` - Poisson GLM applications (Accident, Cormorants, DaytonSurvey)
- `correspondence-analysis` - CA methods (AirCrash, Asbestos, HairEyePlace, HospVisits, Mental)
- `association-models` - Quasi-independence, RC models (Hauser79, Yamaguchi87, JobSat)
- `observer-agreement` - Agreement/reliability analysis (Mammograms)
- `count-models` - Zero-inflated, negative binomial (PhdPubs, CyclingDeaths, Depends)

### 2. **Data Structures**
- `two-way-table` - 2D contingency tables (Glass, JobSat, Mammograms, Mobility)
- `multi-way-table` - 3+ dimensional tables (Abortion, Bartlett, Caesar, Dyke, Heckman)
- `square-table` - Same categories for rows/columns (Glass, Hauser79, Mobility, Yamaguchi87)
- `stratified-table` - Layered analysis (Fungicide, Heart, HairEyePlace, Yamaguchi87)
- `ordered-factors` - Ordinal variables (Accident, Asbestos, JobSat, Mental, Toxaemia)
- `case-form-data` - Individual-level data frames (Donner, ICU, Titanicp)

### 3. **Graphical Methods**
- `mosaic-plot` - Mosaic display examples (~40+ datasets)
- `fourfold-plot` - 2×2 table visualizations (Abortion, Bartlett, Fungicide, Vote1980, WorkerSat)
- `doubledecker-plot` - Conditional independence plots (Dyke, TV)
- `sieve-diagram` - Sieve plots (Mammograms, Mental)
- `rootogram` - Goodness-of-fit plots (CyclingDeaths, PhdPubs)

### 4. **Special Data Features**
- `sparse-data` - Many zero/small cells (Alligator, DaytonSurvey, Fungicide)
- `structural-zeros` - Known impossible combinations (Caesar)
- `sampling-zeros` - Rare unobserved events (Vote1980)

### 5. **Application Domains** (helps users find relevant examples)
- `social-mobility` - Occupational mobility (Glass, Hauser79, Mobility, Yamaguchi87)
- `medical-health` - Health outcomes/epidemiology (Cancer, Caesar, Heart, ICU, Toxaemia)
- `survey-data` - Opinion/attitude surveys (DaytonSurvey, GSS, Vietnam, Vote1980)
- `demographics` - Population characteristics (Burt, Donner, Geissler, Titanicp)
- `consumer-behavior` - Preferences/satisfaction (Detergent, HouseTasks, JobSat, WorkerSat)

### 6. **Response Variable Type** (for modeling focus)
- `binary-response` - 0/1 outcomes (Bartlett, Donner, Heart, ICU, Titanicp)
- `count-response` - Count data (Cormorants, CyclingDeaths, PhdPubs)
- `categorical-response` - Multinomial outcomes (Alligator, Caesar, Hoyt)

---

## Implementation Guidelines

### Multiple Tags Per Dataset
Most datasets should have 3-6 concepts. For example, the Abortion dataset might be tagged:
- `loglinear-models`
- `multi-way-table`
- `mosaic-plot`
- `fourfold-plot`
- `binary-response`

### Tag Priority
Prioritize tags in this order:
1. **Statistical methods** - What analysis to demonstrate
2. **Data structure** - What type of table
3. **Graphical methods** - What plots work well
4. **Domain** - For finding relevant examples

### Common Tag Combinations
- **Square tables** → `social-mobility` + `association-models`
- **Ordered factors** → `correspondence-analysis` + `ordered-factors`
- **Medical data** → `sparse-data` + `logistic-regression`

---

## Complete Dataset Inventory

### Dataset List (45 datasets)

1. **Abortion** - Opinions about abortion by gender and SES (2×2×2)
2. **Accident** - Traffic accident victims in France 1958 (5×2×4×2)
3. **AirCrash** - Fatal commercial airplane crashes 1993-2015
4. **Alligator** - Primary food choices of alligators in Florida lakes
5. **Asbestos** - Occupational exposure and asbestosis severity (5×4)
6. **Bartlett** - Plum root cuttings survival data (2×2×2)
7. **Burt** - Hair, eyes, head and stature of 100 Liverpool residents
8. **Caesar** - Risk factors for infection in Caesarian births (2×2×2×3)
9. **Cancer** - Three-year breast cancer survival (2×2×2)
10. **Cormorants** - Advertising behavior by male cormorants
11. **CyclingDeaths** - Cycling deaths in London 2005-2012
12. **DaytonSurvey** - Substance use among high school seniors 1992
13. **Depends** - R package dependencies distribution (CRAN 2014)
14. **Detergent** - Consumer preferences between detergent brands (2×2×2×3)
15. **Donner** - Survival in the Donner Party 1846
16. **Draft1970** - USA 1970 Draft Lottery results
17. **Draft1970table** - 1970 Draft Lottery contingency table (12×3)
18. **Dyke** - Sources of knowledge of cancer (2^5)
19. **Fungicide** - Carcinogenic effects of fungicide in mice
20. **Geissler** - Distribution of boys/girls in Saxony families 1876-1885
21. **Gilby** - Clothing and intelligence rating of children (4×6)
22. **Glass** - British social mobility from Glass 1954 (5×5)
23. **GSS** - General Social Survey: Sex and party affiliation 1991
24. **HairEyePlace** - Hair and eye color in Scottish locations (4×5×2)
25. **Hauser79** - Social mobility: son's vs father's occupation USA 1973 (5×5)
26. **Heart** - Sex, occupation and heart disease (2×2×3)
27. **Heckman** - Labor force participation of married women 1967-1971 (2^5)
28. **HospVisits** - Hospital visits and length of stay for schizophrenic patients (3×3)
29. **HouseTasks** - Household tasks by husbands and wives (13×4)
30. **Hoyt** - Minnesota high school graduates June 1930 (4×3×7×2)
31. **ICU** - Intensive care unit patient outcomes (200 subjects)
32. **JobSat** - Job satisfaction by income (4×4)
33. **Mammograms** - Mammogram ratings by two readers (4×4)
34. **Mental** - Mental impairment and parents' SES (6×4)
35. **Mice** - Mice depletion data by litter size and treatment
36. **Mobility** - Social mobility: occupational categories (5×5)
37. **PhdPubs** - Publications by PhD candidates in biochemistry
38. **ShakeWords** - Shakespeare's word type frequencies
39. **Titanicp** - Passengers on the RMS Titanic (1309 individuals)
40. **Toxaemia** - Toxaemia symptoms in pregnancy (13,384 mothers)
41. **TV** - TV viewing data Nielsen 1995 (5×11×3)
42. **Vietnam** - Student opinion about Vietnam War UNC 1967
43. **Vote1980** - Race and politics in 1980 presidential vote
44. **WorkerSat** - Worker satisfaction: Danish blue-collar workers 1968
45. **Yamaguchi87** - Occupational mobility in USA, UK, Japan (5×5×3)

---

## Next Steps

- [ ] Review and refine concept categories
- [ ] Create comprehensive tagging for all 45 datasets
- [ ] Apply @concept tags to R/data.R
- [ ] Update package documentation index
- [ ] Test concept-based help searches
