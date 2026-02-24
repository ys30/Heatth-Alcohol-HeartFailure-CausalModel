# Alcohol-SES-HeartFailure

Statistical modelling of the association between alcohol consumption, socioeconomic status, and congestive heart failure among U.S. adults.

## Overview

This project investigates whether alcohol consumption is associated with congestive heart failure (CHF) and how socioeconomic status (SES) may confound or modify this relationship.

Rather than relying purely on predictive modelling, this analysis follows a causal inference framework, combining:

  - Directed Acyclic Graph (DAG) specification

  - Logistic regression modelling

  - Confounder adjustment

  - Interaction analysis

  - Sensitivity checks

The goal is to estimate the adjusted association between alcohol use and heart failure while accounting for socioeconomic and demographic factors.

## Research Questions

1. Is alcohol consumption associated with congestive heart failure?

2. Does socioeconomic status confound or modify this relationship?

3. How does adjustment for demographic and health covariates change the estimated effect?

## Modelling Framework
Step 1 – DAG Specification

Defines hypothesized causal relationships.

Step 2 – Unadjusted Logistic Model

Alcohol → CHF

Step 3 – Adjusted Model

Alcohol + SES + confounders → CHF

Step 4 – Interaction Model

Alcohol × SES interaction term

## Methods

Logistic regression

Odds ratios with 95% CI

Model comparison (AIC)

Sensitivity checks

## Key Findings (You fill based on output)

🔹 Alcohol and CHF

The unadjusted model suggested a weak association between alcohol use and CHF.

After adjusting for SES and demographic covariates, the magnitude of the association changed, indicating potential confounding.

🔹 Role of Socioeconomic Status

SES was independently associated with CHF risk.

Adjustment for SES attenuated the alcohol effect estimate, suggesting SES partially confounds the relationship.

🔹 Interaction Effects

Interaction analysis indicated that the association between alcohol and CHF varied across SES levels.

In lower SES groups, the estimated effect of alcohol on CHF appeared stronger compared to higher SES groups.

🔹 Model Comparison

Fully adjusted models demonstrated improved model fit (lower AIC).

Effect size stability increased after covariate adjustment.

Overall, results highlight the importance of socioeconomic context when evaluating alcohol-related cardiovascular risk.
