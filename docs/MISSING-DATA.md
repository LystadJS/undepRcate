# Missing Data practice

In **Practice**, select **Missing Data** under **Skill category**. The drill menu groups the material into six stages. Choose a named drill to repeat one operation or choose **Mixed drills in this category** to practice across the stages. Each drill has three levels and generates new records for every repetition.

| Stage | What to practice | What to look for |
| --- | --- | --- |
| Inspect the records | Import quoted CSV without losing leading zeroes or sentinel codes. Preserve raw values and reasons. Profile fields and records, identify who was eligible, compare expected visits with actual records, and audit failed joins. | An unanswered question, a question a person was never asked, and a record lost in a join require different actions. |
| Diagnose missingness | Tabulate joint patterns and co-absence, distinguish intermittent from trailing gaps, plot rates by site and wave, and check respondent coverage of a target population. Compare specified MCAR, MAR and MNAR mechanisms and observationally equivalent scenarios. | A clear association with a recorded variable can guide an imputation model. The observed records alone generally cannot distinguish MAR from MNAR. |
| Recover values and assess exclusions | Resolve conflicting sources under an explicit source-priority rule, reconstruct counts from a verified identity, bound time-limited carries, quantify complete-case attrition, and use inverse observation-probability weights under their stated assumptions. | Preserve provenance. Check whether the rule applies to each record and whether respondents represent every target stratum. |
| Build and check imputations | Choose methods by field type, design predictors, mark eligible cells, run chained equations, extract completed data, inspect chains and distributions, fit the same model in each completion, and pool estimates and uncertainty. | Include information needed by the analysis model. Audit impossible imputations, sparse categories and logged events. Chain stability alone cannot establish that an imputation model is appropriate. |
| Validate predictions | Estimate replacement values using training records and apply those values to held-out records. Compare predictions with intentionally masked measurements, including subgroup errors and interval coverage. | Use the original missingness indicators. A masked observation supplies known truth for that validation case. The value of a genuinely missing observation remains unknown. |
| Assess sensitivity and report | Calculate binary-outcome bounds, shift assumptions for unobserved outcomes, locate tipping points, and compare estimates with paired uncertainty. | Report the target population, eligible denominators, assumed response process, method, diagnostics, uncertainty, and conclusions that changed under sensitivity analysis. |

For example, if follow-up scores are often missing at one site, start with **Set the eligible denominator** and **Distinguish intermittent and trailing gaps**. A site might have a different visit schedule, incomplete linkage, or genuinely lower response. Establishing which records were due changes both the numerator and denominator of the rate. For the remaining gaps, compare **Audit complete-case eligibility and attrition** with **Pool fitted models and uncertainty**, then use **Find a missing-outcome tipping point** to examine departures from the working assumptions.

The mechanism drills supply a complete hypothetical response law when they ask you to classify it. For a particular missingness pattern, MAR requires the probability of that pattern to stay constant as its unobserved values vary while the observed values stay fixed. Likelihood ignorability also needs conditions on the parameters of the response and data models. [Seaman et al., *What Is Meant by “Missing at Random”?*](https://doi.org/10.1214/13-STS415)

## Reading the R interfaces

The tidyverse is attached in practice. Use `tidyr::complete()` to make absent combinations explicit. Use `mice::complete()` to extract data from a fitted imputation object. The Missing Data category loads the `mice` namespace so that `with()` can fit a model in every completion. The reference solutions use explicit `mice::` calls for its imputation tools. [tidyr completion](https://tidyr.tidyverse.org/reference/complete.html) · [mice completion](https://amices.org/mice/reference/complete.mids.html)

In `mice()`, `m` sets the number of completed datasets and `maxit` sets the number of chain iterations. A row of `predictorMatrix` represents an imputation target, and its columns are candidate predictors. A true cell in `where` requests an imputation at that position. The observation indicator used for analyzing response has the opposite convention at an eligible missing cell. A structural skip may be absent and still be deliberately excluded from imputation. [mice configuration](https://amices.org/mice/reference/mice.html)

The following translation applies to a single scalar coefficient in the pooling drills. Dimensions and reported values refer to one fitted exercise, conditional on its realized input and completed-data analyses.

| Software field → preferred notation | Object and meaning | Dimensions | Status | Conditioning |
| --- | --- | --- | --- | --- |
| `m` → $M$ | Number of completed datasets | Scalar, 1 × 1 | Fixed imputation setting | Chosen for this analysis |
| Positive `.imp` → $m$ | Completion index from one through $M$ | Scalar, 1 × 1 | Index | Fixed collection of $M$ completions. In long output, zero identifies the original incomplete records. |
| `estimate` → $\hat\theta_{\mathrm{MI}}$ | Pooled estimate of the chosen model coefficient | Scalar, 1 × 1 | Realized point estimate | Completed-data fits for that coefficient |
| `ubar` → $\bar U_{\mathrm{MI}}$ | Average estimated within-imputation variance | Scalar, 1 × 1 | Deterministic summary | Variance estimates from the completed-data fits |
| `b` → $B_{\mathrm{MI}}$ | Between-imputation variance of the coefficient estimates | Scalar, 1 × 1 | Deterministic summary | Coefficient estimates across the $M$ fits |
| `t` → $T_{\mathrm{MI}}$ | Total estimated variance under Rubin's rule | Scalar, 1 × 1 | Deterministic summary | Within- and between-imputation components and $M$ |
| `std.error` → $\widehat{\mathrm{SE}}_{\mathrm{MI}}$ | Estimated pooled standard error, the square root of total variance | Scalar, 1 × 1 | Deterministic summary | Total estimated variance for this coefficient |
| `lambda` → $\lambda_{\mathrm{MI}}$ | Proportion of total variance attributable to missingness | Scalar, 1 × 1 | Deterministic summary | Between-imputation variance, total variance and $M$ |

The pooling exercise uses a frequentist regression with fixed, unknown coefficients. Its reported estimates and uncertainty are realized summaries. The software also reports `fmi`, a degrees-of-freedom-adjusted fraction of missing information, which can differ from `lambda`. [mice pooling rules](https://amices.org/mice/reference/pool.html)
