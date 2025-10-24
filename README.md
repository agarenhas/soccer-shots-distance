# ⚽ Predictive Modeling of Soccer Shots vs. Distance

## 📋 Project Overview
This project focuses on building predictive models for the expected number of shots in soccer as a function of the distance to the goal. The core methodology involves fitting a series of Generalized Linear Models (GLMs) to model the conditional mean of shot counts.

---

## 🧮 Methodology

### 🎯 Model Framework
We model the conditional mean of shot counts using different GLM families and a non-parametric alternative for comparison.

### 📊 Model Comparison
The following models are implemented and compared:

| Model Type | Features | Link Function | Purpose |
|------------|----------|---------------|---------|
| **Poisson GLM** | Single predictor (x) | Log link | Baseline count data model |
| **Poisson GLM** | x + x² | Log link | Capture non-linear effects |
| **Negative Binomial GLM** | x + x² | Log link | Handle overdispersion |
| **Gaussian GLM** | x + x² | Log link | Alternative approach |
| **GAM** | Non-parametric | Flexible | Benchmark comparison |

---

## 🔍 Statistical Analysis

For each GLM, we perform a comprehensive analysis including:

- 📈 **Parameter Estimates**: Calculation of model coefficients
- 📏 **Confidence Intervals**: Construction of parameter confidence intervals
- 🎯 **Hypothesis Testing**: Model comparison using Likelihood Ratio Tests
- 📊 **Model Diagnostics**: Residual analysis and goodness-of-fit assessment

---

## 📊 Visualization

The analysis is supported by various graphical representations:

- 📈 **Model Fits**: Comparison of fitted curves across models
- 🎯 **Confidence Bands**: Uncertainty intervals around predictions
- 🔍 **Residual Diagnostics**: Plots for model validation
- 📉 **Performance Metrics**: Visual model comparison

---
