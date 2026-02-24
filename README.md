# Alcohol-SES-HeartFailure

Statistical modelling of the association between alcohol consumption, socioeconomic status, and congestive heart failure among U.S. adults.

## Overview

This project investigates whether alcohol consumption is associated with congestive heart failure (CHF), and how socioeconomic status (SES) modifies or confounds this relationship.

The analysis combines:

Logistic regression modelling

Directed Acyclic Graph (DAG) specification

Confounder adjustment

Sensitivity analyses

## Research Questions

Is alcohol consumption associated with heart failure?

Does socioeconomic status modify this association?

What covariates should be adjusted based on causal reasoning?

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

Alcohol showed [positive/negative/no] association with CHF.

SES significantly [modified/confounded] the relationship.

Adjusted models changed effect size by ___%.
