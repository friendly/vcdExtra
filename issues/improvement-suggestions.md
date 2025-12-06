# Suggested Topics for Improvement - vcdExtra Package

**Date:** 2025-12-05
**Current Version:** 0.8-7
**Prepared for:** Next release planning

---

## 1. Documentation & Consistency Issues

### Version Mismatch
- **Issue:** README.md shows Version 0.8-6 while DESCRIPTION shows 0.8-7
- **Action:** Update README.md to reflect current version
- **Files:** `README.md` line 18

### Deprecated Function Cleanup
- **Issue:** The `Summarise.R` file contains duplicated code identical to `LRstats.R`
- **Action:** Consider one of:
  - Formally deprecating `Summarise()` in favor of `LRstats()` (add `.Deprecated()` call)
  - Remove the duplicate code and make `Summarise` simply an alias/wrapper
  - Currently `summarise()` deprecation is commented out in `vcdExtra-deprecated.R`
- **Files:** `R/Summarise.R`, `R/LRstats.R`, `R/vcdExtra-deprecated.R`

### Function Documentation
- **Action:** Add @family tags to related functions for better cross-referencing in help pages
- **Benefit:** Improved navigation and discoverability

---

## 2. Code Quality & Technical Debt

### TODO Items in Code
Address outstanding TODO/FIXME comments:

#### CMHtest.R
- Better handling of p-value printing (line 357)
- Determine score types (integer, midrank) for heading (line 374)

#### mosaic3d.R
- Provide formula interface
- Handle zero margins (causes display to be erased in shapelist3d)
- Allow alpha to control transparency of side walls
- Fix kludge for interline gap between level labels and variable name
- How to pass & extract labeling_args

#### mosaic.glm.R
- Move utility functions to separate utility.R file (line 10)

#### mosaic.glmlist.R
- Use `abbreviate()` for better model name display

#### GKgamma.R
- Add tests for matrix or table inputs

### Code Duplication
- **Issue:** `Summarise.R` and `LRstats.R` contain identical code
- **Action:** Consolidate or properly deprecate to reduce maintenance burden

---

## 3. Testing & Quality Assurance

### Expand Test Coverage
Currently only 1 test file (`test-CMHtest.R`) exists. Priority areas for testing:

#### High Priority
- Core mosaic plotting functions: `mosaic.glm`, `mosaic.glmlist`, `mosaic3d`
- Model utilities: `Kway`, `seq_loglm`, `LRstats`
- Statistical tests: `GKgamma`, `HLtest`, `zero.test`

#### Medium Priority
- Data manipulation functions: `expand.dft`, `collapse.table`, `cutfac`
- Loglinear model utilities: functions in `loglin-utilities.R`
- Edge cases: structural zeros, sparse tables, missing data

### Integration Testing
- Add tests comparing results with established packages
- Follow the excellent SAS comparison pattern used in `test-CMHtest.R`
- Verify consistency with vcd package results where applicable

---

## 4. Enhanced Functionality

### CMHtest Improvements
- Better print formatting for p-values (currently noted as TODO)
- More informative output showing which score types were used
- Potential performance optimization for large stratified tables
- Consider adding a `summary()` method

### mosaic3d Enhancements
- Implement formula interface (currently TODO)
- Fix handling of zero margins
- Add alpha transparency control for 3D walls
- Improve labeling argument handling
- Better integration with rgl device options

### Modernization Options
Consider adding:
- `tidy()` methods compatible with the broom package
- Better integration with tidyverse workflows (already imports dplyr, tidyr, etc.)
- Native pipe operator `|>` support in examples (for R >= 4.1.0)
- Optional ggplot2-based alternatives for key visualizations

---

## 5. Package Infrastructure

### GitHub Actions/CI
- Set up continuous integration for automated testing
- Test on multiple R versions (oldrel, release, devel)
- Test on multiple platforms (Windows, macOS, Linux)
- Add R-CMD-check workflow
- Add test coverage reporting

### Dependency Management
- Review the large Suggests list (19 packages) - are all still needed?
- Document minimum versions for key dependencies beyond gnm
- Consider moving some rarely-used Suggests to Enhances
- Current Suggests list:
  ```
  gmodels, Fahrmeir, effects, VGAM, plyr, lmtest, nnet, ggplot2,
  Sleuth2, car, lattice, stats4, rgl, AER, coin, Hmisc, knitr,
  rmarkdown, seriation, testthat, tibble
  ```

### NAMESPACE Review
- Review exported vs internal functions
- Some utilities might be better as internal-only functions
- Ensure all exported functions are documented and tested

---

## 6. Documentation Improvements

### Vignette Enhancements

#### New Vignettes
- Add a "quick start" vignette for new users
- Create a "common workflows" vignette
- Add vignette on RC models and structured associations

#### Existing Vignettes
- Expand the `tidyCats.Rmd` vignette (if incomplete)
- Add cross-references between vignettes and relevant functions
- Update vignette examples to use more recent R features
- Ensure all code chunks run successfully

#### Current Vignettes
- a1-creating.Rmd: Creating and manipulating frequency tables
- a2-tests.Rmd: Tests of Independence
- a3-loglinear.Rmd: Loglinear Models
- a4-mosaics.Rmd: Mosaic plots
- a5-demo-housing.Rmd: Demo - Housing Data
- a6-mobility.Rmd: Mobility tables
- a7-continuous.Rmd: Continuous predictors
- datasets.Rmd: Datasets for categorical data analysis
- tidyCats.Rmd: (status unclear)

### README.Rmd Updates
- Update version number (currently shows 0.8-6)
- Add more badges:
  - Test coverage
  - Lifecycle status (already present)
  - CRAN downloads (already present)
  - R-CMD-check status
- Consider adding a "Citation" section
- Add recent changes/highlights section
- Fix installation example (line 48 has wrong package name "mvinfluence")

### Function Documentation
- Add more @examples showing real-world use cases
- Include @seealso sections for related functions
- Add @family tags for function grouping
- Include references to DDAR book chapters where relevant
- Ensure all parameters are documented
- Add return value descriptions for all functions

---

## 7. Performance & Efficiency

### Large Table Handling
- Test and optimize functions for large contingency tables
- Particularly important for: `mosaic3d`, `CMHtest` with many strata
- Add benchmarking suite for key functions
- Consider optional sparse matrix support for very large tables

### Memory Efficiency
- Review memory usage in table manipulation functions
- Profile memory allocation in `expand.dft` for large datasets
- Optimize `Kway` for models with many factors

---

## 8. User Experience

### Error Messages
Improve error messages with more actionable guidance:

#### Current Issues
- CMHtest gives basic dimension errors - could suggest fixes
- mosaic.glm errors could be more informative about data requirements
- Generic "can't find X" messages don't suggest alternatives

#### Suggested Improvements
- Provide clear error messages with hints
- Include examples of correct usage in error messages
- Validate inputs early with informative messages
- Add class checks with helpful suggestions

### Progress Indicators
- For long-running operations (e.g., `Kway` with many factors), add optional progress bars
- Use existing progress bar packages (progress, progressr)
- Make progress reporting optional via argument

### Return Values
- Ensure all functions return consistent, well-structured objects
- Add proper class attributes to all return objects
- Document return value structure clearly
- Consider adding `str()` examples in documentation

### Function Naming
- Review function naming consistency
- Some functions use camelCase, others use dots
- Consider a consistent naming convention for new functions

---

## 9. Website & pkgdown

### pkgdown Site Enhancement

#### Content Additions
- Add a "News" page linking to NEWS.md
- Include gallery of example plots
- Add articles showing advanced use cases
- Add "Getting Started" landing page
- Include citation information

#### Organization
- Better organization of function reference (partially done in `_pkgdown.yml`)
- Group related functions more clearly
- Add search functionality
- Include package logo consistently

#### Current _pkgdown.yml Structure
- Mosaics (10 functions)
- Loglinear models (11 functions)
- Statistical tests (13 functions)
- Distributions (4 functions)
- Utility (6 functions)
- Other (6 functions)
- Data (45 datasets)

### URL Configuration
- Current URL: http://friendly.github.io/vcdExtra
- Consider HTTPS configuration
- Ensure all internal links work correctly

---

## 10. Datasets

### Dataset Documentation
- Ensure all 45 datasets have complete `\concept{}` tags (started in v0.8-3)
- Add more detailed sources and references
- Include suggested analyses in dataset help pages
- Add DOI references where available
- Include original data sources

### Dataset Consistency
- Standardize variable naming conventions
- Ensure all datasets have proper class attributes
- Add metadata about dataset origin and purpose
- Consider creating a dataset classification vignette

### New Datasets
Consider adding example datasets that demonstrate:
- Sparse tables with structural zeros
- Large-scale categorical data
- Modern applications (social media, web analytics, etc.)
- Time series of categorical data
- Multilevel categorical data

### Current Datasets (45 total)
Abortion, Accident, AirCrash, Alligator, Asbestos, Bartlett, Burt, Caesar, Cancer, Cormorants, CyclingDeaths, DaytonSurvey, Depends, Detergent, Donner, Draft1970, Draft1970table, Dyke, Fungicide, GSS, Geissler, Gilby, Glass, HairEyePlace, Hauser79, Heart, Heckman, HospVisits, HouseTasks, Hoyt, ICU, JobSat, Mammograms, Mental, Mice, Mobility, PhdPubs, ShakeWords, TV, Titanicp, Toxaemia, Vietnam, Vote1980, WorkerSat, Yamaguchi87

---

## 11. Compatibility & Future-Proofing

### R Version Requirements
- Package currently requires R >= 3.5.0
- Consider if this can be updated to R >= 4.0.0 or 4.1.0
- Benefits of newer R version:
  - Native pipe operator `|>`
  - Better string handling
  - Improved performance
  - rawConnection improvements
- Decision depends on user base and CRAN policies

### Deprecated R Functions
- Review use of any R functions that are deprecated in newer R versions
- Check for use of deprecated argument names
- Update to use current best practices

### Long-term Dependencies
- Ensure compatibility with upcoming versions of:
  - vcd (core dependency)
  - gnm (core dependency)
  - MASS (used in CMHtest)
  - tidyverse packages (dplyr, tidyr, etc.)
- Monitor deprecation warnings from dependencies

### Platform Support
- Ensure package works on all major platforms
- Test on M1/M2 Macs
- Test on Windows with Rtools
- Consider containerized testing

---

## 12. Community & Contribution

### CONTRIBUTING.md
- Add file with guidelines for contributors
- Include:
  - How to set up development environment
  - Coding style guidelines
  - Testing requirements
  - Documentation standards
  - Pull request process

### CODE_OF_CONDUCT.md
- Add standard code of conduct
- Use Contributor Covenant or similar
- Specify enforcement mechanisms

### Issue Templates
- Create GitHub issue templates for:
  - Bug reports
  - Feature requests
  - Documentation improvements
  - Dataset contributions
- Include required information in templates

### Developer Documentation
- Add notes about package architecture
- Document key design decisions
- Explain relationship with vcd package
- Include contribution examples

### Community Building
- Consider creating a discussion forum
- Add examples of papers/projects using vcdExtra
- Create a user showcase
- Regular blog posts about new features

---

## Priority Recommendations

### High Priority (Next Release)
1. Fix README version mismatch
2. Resolve Summarise/LRstats duplication
3. Expand test coverage (at least basic tests for all exported functions)
4. Address critical TODO items in mosaic3d
5. Set up GitHub Actions CI

### Medium Priority (Following Release)
1. Improve error messages across all functions
2. Complete TODO items in CMHtest
3. Add new vignettes (quick start, common workflows)
4. Enhance pkgdown website
5. Review and optimize dependency list

### Lower Priority (Future Releases)
1. Additional dataset examples
2. Performance optimization for large tables
3. Tidyverse integration enhancements
4. Additional statistical test implementations
5. Extended gallery of examples

---

## Implementation Strategy

### Phase 1: Foundation (Immediate)
- Fix version inconsistencies
- Set up CI/CD
- Expand test coverage to >50%
- Resolve code duplication

### Phase 2: Enhancement (1-2 months)
- Address TODO items
- Improve documentation
- Enhance error messages
- Website improvements

### Phase 3: Extension (3-6 months)
- New features and methods
- Additional vignettes
- Performance optimization
- Community building

---

## Notes
- This document created: 2025-12-05
- Based on analysis of version 0.8-7 codebase
- Review and update priorities as development progresses
- Consider user feedback and issue reports when prioritizing
