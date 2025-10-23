This project focuses on building predictive models for the expected number of shots in soccer as a function of the distance to the goal. The core methodology involves fitting a series of Generalized Linear Models (GLMs) to model the conditional mean of shot counts.

**Methodology:**

* Model Framework: We model the conditional mean of shot counts using different GLM families and a non-parametric alternative for comparison.

* Model Comparison: The following models are implemented and compared:
  * Poisson GLM with log link: First with the single predictor x (distance), then with x and x².
  * Negative Binomial GLM with log link, using x and x² to handle potential overdispersion.
  * Gaussian GLM with log link, using x and x².
  * Generalized Additive Model (GAM): Used as a flexible, non-parametric benchmark to compare against the parametric GLMs.
    
* Statistical Analysis: For each GLM, we perform a comprehensive analysis including:
  * Calculation of parameter estimates.
  * Construction of confidence intervals for the parameters.
  * Hypothesis testing for model comparison (e.g., Likelihood Ratio Tests).

* Visualization: The analysis is supported by various graphical representations to illustrate model fits, confidence bands, and residual diagnostics.
