# 01_analysis_main.R
# Source: refactored from 000summary code.R

source(file.path('scripts','00_setup.R'))

############################################################
############################################################
# PART 1: DATA INSTALLATION, PREP, AND CLEANING (NHANES)
############################################################
############################################################

############################################################
# 1. PACKAGES
############################################################

# install.packages(c(
#   "tidyverse","haven","curl","broom","MatchIt","tableone",
#   "dagitty","ggdag","janitor"
# ), dependencies = TRUE)

library(tidyverse)
library(haven)     # read_xpt
library(curl)      # curl_download
library(dplyr)
library(broom)
library(MatchIt)
library(tableone)
library(dagitty)
library(ggdag)
library(janitor)

# NOTE: setwd() removed for GitHub reproducibility. Use relative paths instead.


############################################################
# 2. NHANES 2017–March 2020 FILE URLS (Cycle P_)
############################################################

url_demo   <- "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2017/DataFiles/P_DEMO.XPT"
url_alq    <- "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2017/DataFiles/P_ALQ.XPT"
url_mcq    <- "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2017/DataFiles/P_MCQ.XPT"
url_bmx    <- "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2017/DataFiles/P_BMX.XPT"
url_smq    <- "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2017/DataFiles/P_SMQ.XPT"
url_smqrtu <- "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2017/DataFiles/P_SMQRTU.XPT"
url_paq    <- "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2017/DataFiles/P_PAQ.XPT"
url_dr1    <- "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2017/DataFiles/P_DR1TOT.XPT"
url_dr2    <- "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2017/DataFiles/P_DR2TOT.XPT"

# New comorbidity modules
url_diq <- "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2017/DataFiles/P_DIQ.XPT"
url_kiq <- "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2017/DataFiles/P_KIQ_U.XPT"


############################################################
# 3. HELPER FUNCTION: DOWNLOAD + READ XPT
############################################################

load_nhanes <- function(url) {
  tmp <- tempfile(fileext = ".xpt")
  curl_download(url, tmp, quiet = TRUE)
  read_xpt(tmp)
}


############################################################
# 4. DOWNLOAD DATASETS
############################################################

demo <- load_nhanes(url_demo)
alq  <- load_nhanes(url_alq)
mcq  <- load_nhanes(url_mcq)
bmx  <- load_nhanes(url_bmx)
smq  <- load_nhanes(url_smq)
smrt <- load_nhanes(url_smqrtu)   # detailed smoking file
paq  <- load_nhanes(url_paq)
dr1  <- load_nhanes(url_dr1)
dr2  <- load_nhanes(url_dr2)

diq  <- load_nhanes(url_diq)      # diabetes
kiq  <- load_nhanes(url_kiq)      # kidney


############################################################
# 5. MERGE ALL MODULES BY SEQN
############################################################

nhanes <- demo %>%
  left_join(alq,  by = "SEQN") %>%
  left_join(mcq,  by = "SEQN") %>%
  left_join(bmx,  by = "SEQN") %>%
  left_join(smq,  by = "SEQN") %>%
  left_join(smrt, by = "SEQN") %>%
  left_join(paq,  by = "SEQN") %>%
  left_join(dr1,  by = "SEQN") %>%
  left_join(dr2,  by = "SEQN") %>%
  left_join(diq,  by = "SEQN") %>%
  left_join(kiq,  by = "SEQN")

# Quick check: confirm new variables exist
grep("DIQ010|KIQ022|MCQ160L|MCQ220", names(nhanes), value = TRUE)


############################################################
# PART 2: CLEANING + VARIABLE CODING
############################################################

# Notes:
# - Exposure: Alcohol (ALQ121), recode to binary Low vs High
# - Outcomes: Congestive Heart Failure (MCQ160B)
# - new variables: diabetes (DIQ010), liver (MCQ160L), cancer (MCQ220), kidney (KIQ022)
# - SES: INDFMPIR (PIR) categorized into Low/Middle/High
# - BMI: continuous + categorical
# - Covariates: age, gender, race, education, smoking, physical activity, fiber intake
# - Keep survey design vars: SDMVPSU, SDMVSTRA, WTINTPRP

nhanes_clean <- nhanes %>%
  filter(
    RIDAGEYR >= 21,
    !is.na(ALQ121),
    !is.na(INDFMPIR),
    !is.na(BMXBMI)
  ) %>%
  mutate(
    ########################################################
    # Exposure: Alcohol
    ########################################################
    alcohol_raw = ifelse(ALQ121 %in% c(77, 99), NA, ALQ121),
    alcohol = case_when(
      alcohol_raw %in% c(1, 2, 3, 4)  ~ 1,
      alcohol_raw %in% c(8, 9, 10)    ~ 0,
      TRUE ~ NA_real_
    ),
    alcohol_f = factor(alcohol, levels = c(0, 1), labels = c("Low", "High")),
    
    
    ########################################################
    # OUTCOME: Congestive Heart Failure (MCQ160B)
    ########################################################
    #creating a column called cvd 
    #1 if MCQ160B = 1 (told had CHF)
    #0 if MCQ160B = 2 (not told)
    
    cvd = case_when(
          MCQ160B == 1 ~ 1,
          MCQ160B == 2 ~ 0,
          TRUE ~ NA_real_
        ),
    
    ########################################################
    # SES (PIR)
    ########################################################
    ses_cat = case_when(
      INDFMPIR < 1 ~ "Low",
      INDFMPIR >= 1 & INDFMPIR < 4 ~ "Middle",
      INDFMPIR >= 4 ~ "High"
    ),
    ses_cat = factor(ses_cat, levels = c("Low", "Middle", "High")),
    
    ########################################################
    # BMI (continuous + categorical)
    ########################################################
    bmi = BMXBMI,
    bmi_cat = case_when(
      bmi < 18.5 ~ "Underweight",
      bmi >= 18.5 & bmi < 25 ~ "Normal",
      bmi >= 25   & bmi < 30 ~ "Overweight",
      bmi >= 30 ~ "Obese",
      TRUE ~ NA_character_
    ),
    bmi_cat = factor(bmi_cat, levels = c("Underweight", "Normal", "Overweight", "Obese")),
    
    ########################################################
    # Single-disease outcomes (0/1)
    ########################################################
    diabetes = case_when(DIQ010 == 1 ~ 1, DIQ010 == 2 ~ 0, TRUE ~ NA_real_),
    liver    = case_when(MCQ160L == 1 ~ 1, MCQ160L == 2 ~ 0, TRUE ~ NA_real_),
    cancer   = case_when(MCQ220  == 1 ~ 1, MCQ220  == 2 ~ 0, TRUE ~ NA_real_),
    kidney   = case_when(KIQ022  == 1 ~ 1, KIQ022  == 2 ~ 0, TRUE ~ NA_real_),
    
    ########################################################
    # Composite comorbidity indicator (optional)
    ########################################################
    any_comorb = if_else(
      diabetes == 1 | liver == 1 | cancer == 1 | kidney == 1,
      1, 0, missing = NA_real_
    ),
    
    ########################################################
    # Covariates
    ########################################################
    age    = RIDAGEYR,
    gender = factor(RIAGENDR, levels = c(1, 2), labels = c("Male", "Female")),
    race   = factor(RIDRETH3),
    educ   = factor(DMDEDUC2),
    
    smoke_100 = case_when(SMQ020 == 1 ~ 1, SMQ020 == 2 ~ 0, TRUE ~ NA_real_),
    
    pa_mod = case_when(
      PAQ620 == 1 | PAQ665 == 1 ~ 1,
      PAQ620 == 2 & PAQ665 == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    
    fiber_mean = rowMeans(cbind(DR1TFIBE, DR2TFIBE), na.rm = TRUE),
    fiber_mean = ifelse(is.nan(fiber_mean), NA, fiber_mean)
  ) %>%
  filter(
    !is.na(alcohol_f),
    !is.na(ses_cat),
    !is.na(bmi_cat),
    !is.na(cvd)
  )

# Final analysis dataset
nhanes_final <- nhanes_clean %>%
  select(
    SEQN,
    cvd,
    alcohol_f, ses_cat,
    age, gender, race, educ,
    bmi, bmi_cat,
    smoke_100, pa_mod, fiber_mean,
    diabetes, liver, cancer, kidney,
    any_comorb,
    SDMVPSU, SDMVSTRA, WTINTPRP
  )

str(nhanes_final)

############################################################
# PART 3: BIVARIATE ANALYSIS (SCREENING)
############################################################

# For screening, we use complete cases for the main exposure/outcome/covariates.
# ( outcome is CHF/cvd )
outcome_var <- "cvd"

biv_data <- nhanes_final %>%
  drop_na(
    alcohol_f, ses_cat,
    age, gender, race, educ,
    bmi, smoke_100, pa_mod, fiber_mean,
    any_comorb, diabetes, liver, cancer, kidney
  ) %>%
  drop_na(!!sym(outcome_var))


############################################################
# 3.1 Continuous variables vs exposure and outcome
############################################################

t.test(age ~ alcohol_f, data = biv_data)
t.test(bmi ~ alcohol_f, data = biv_data)
t.test(fiber_mean ~ alcohol_f, data = biv_data)

t.test(age ~ biv_data[[outcome_var]], data = biv_data)
t.test(bmi ~ biv_data[[outcome_var]], data = biv_data)
t.test(fiber_mean ~ biv_data[[outcome_var]], data = biv_data)


############################################################
# 3.2 Categorical variables vs exposure and outcome
############################################################

chisq.test(table(biv_data$gender,    biv_data$alcohol_f))
chisq.test(table(biv_data$ses_cat,   biv_data$alcohol_f))
chisq.test(table(biv_data$smoke_100, biv_data$alcohol_f))
chisq.test(table(biv_data$pa_mod,    biv_data$alcohol_f))

chisq.test(table(biv_data$gender,    biv_data[[outcome_var]]))
chisq.test(table(biv_data$ses_cat,   biv_data[[outcome_var]]))
chisq.test(table(biv_data$smoke_100, biv_data[[outcome_var]]))
chisq.test(table(biv_data$bmi_cat, biv_data[[outcome_var]]))
chisq.test(table(biv_data$pa_mod,    biv_data[[outcome_var]]))
chisq.test(table(biv_data$alcohol_f, biv_data[[outcome_var]]))
chisq.test(table(biv_data$any_comorb, biv_data[[outcome_var]]))

# Comorbidities vs outcome
chisq.test(table(biv_data$diabetes, biv_data[[outcome_var]]))
chisq.test(table(biv_data$liver,    biv_data[[outcome_var]]))
chisq.test(table(biv_data$cancer,   biv_data[[outcome_var]]))
chisq.test(table(biv_data$kidney,   biv_data[[outcome_var]]))

############################################################
# 3.3 Automated bivariate p-value summary table
############################################################

biv_test <- function(var, exposure, outcome, data) {
  
  x <- data[[var]]
  e <- data[[exposure]]
  y <- data[[outcome]]
  
  p_exp <- if (is.numeric(x)) {
    t.test(x ~ e)$p.value
  } else {
    chisq.test(table(x, e))$p.value
  }
  
  p_out <- if (is.numeric(x)) {
    t.test(x ~ y)$p.value
  } else {
    chisq.test(table(x, y))$p.value
  }
  
  tibble(
    variable   = var,
    p_exposure = p_exp,
    p_outcome  = p_out
  )
}

vars <- c("age", "gender", "race", "educ", "bmi", "smoke_100", "pa_mod", "fiber_mean", "ses_cat",
          "any_comorb", "diabetes", "liver", "cancer", "kidney","bmi_cat")

biv_summary <- bind_rows(
  lapply(
    vars,
    biv_test,
    exposure = "alcohol_f",
    outcome  = outcome_var,
    data     = biv_data
  )
)

biv_summary_clean <- biv_summary %>%
  mutate(
    p_exposure = round(p_exposure, 3),
    p_outcome  = round(p_outcome, 3),
    include = ifelse(p_exposure < 0.20 | p_outcome < 0.20, "Yes", "No")
  )

biv_summary_clean


############################################################
# 4 VIF / GVIF CHECK (use bmi_cat + any_comorb)
############################################################

library(dplyr)
library(tidyr)
library(car)

# 1) Build analysis dataset for VIF (complete cases)
dat_adj <- nhanes_final %>%
  mutate(
    # make sure outcome is numeric 0/1 (or keep as-is if already)
    cvd = as.numeric(cvd),
    
    # make sure exposure is factor with desired baseline
    alcohol_f = factor(alcohol_f, levels = c("Low", "High")),
    
    # SES baseline
    ses_cat = factor(ses_cat, levels = c("Low", "Middle", "High")),
    
    # BMI categorical baseline (Normal is typical reference)
    bmi_cat = factor(bmi_cat, levels = c("Normal", "Underweight", "Overweight", "Obese")),
    
    # binary covariates to factor (optional but recommended for clarity)
    smoke_100 = factor(smoke_100, levels = c(0, 1), labels = c("No", "Yes")),
    pa_mod    = factor(pa_mod,    levels = c(0, 1), labels = c("No", "Yes")),
    
    # comorbidity as factor 0/1
    any_comorb = factor(any_comorb, levels = c(0, 1), labels = c("No", "Yes")),
    
    # fix possible NaN in fiber_mean
    fiber_mean = ifelse(is.nan(fiber_mean), NA, fiber_mean)
  ) %>%
  select(
    cvd, alcohol_f, ses_cat,
    age, gender, race, educ,
    bmi_cat,
    smoke_100, pa_mod, fiber_mean,
    any_comorb
  ) %>%
  drop_na()

# 2) Fit logistic regression (unweighted) for VIF diagnostics
fit_adj <- glm(
  cvd ~ alcohol_f + ses_cat + age + gender + race + educ +
    bmi_cat + smoke_100 + pa_mod + fiber_mean + any_comorb,
  data = dat_adj,
  family = binomial()
)

# 3) GVIF / VIF
v <- vif(fit_adj)
v

# 4) Adjusted GVIF (comparable scale)
# If factors exist, vif() returns a matrix with GVIF + Df
if (is.matrix(v)) {
  gvif_adj <- v[, "GVIF"]^(1 / (2 * v[, "Df"]))
  gvif_adj
} else {
  # If no factors (rare here), vif() returns a named vector already
  v
}


############################################################
############################################################
############################################################
############################################################
############################################################
############################################################
############################################################


############################################################
############################################################
#PART 2: MODELS (Unweighted + Survey-weighted) 
############################################################
############################################################

############################################################
# PACKAGES
############################################################
library(dplyr)
library(tidyverse)
library(haven)
library(curl)
library(broom)
library(tableone)
library(janitor)
library(MatchIt)
library(survey)

############################################################
# PART 5: MODELs
############################################################
dat_adj <- nhanes_final %>%
  tidyr::drop_na(
    cvd, alcohol_f, ses_cat,
    age, gender, race, educ,
    bmi_cat, smoke_100, pa_mod,
    any_comorb
  )

# 5.1 Unweighted crude
m_crude <- glm(cvd ~ alcohol_f, data=nhanes_final, family=binomial)

# 5.2 Unweighted adjusted (main covariate-adjusted)
dat_adj <- nhanes_final %>%
  select(cvd, alcohol_f, ses_cat, age, gender, race, educ, bmi_cat, smoke_100, pa_mod, any_comorb) %>%
  tidyr::drop_na()

m_adj <- glm(
  cvd ~ alcohol_f + ses_cat + age + gender + race + educ + bmi_cat + smoke_100 + pa_mod + any_comorb,
  data=dat_adj, family=binomial
)

# 5.3 Unweighted SES-stratified adjusted (your “Model 1”)
# SES-stratified adjusted (High vs Low within each SES)
stratified_models <- dat_adj %>%
  group_by(ses_cat) %>%
  group_modify(~{
    fit <- glm(
      cvd ~ alcohol_f +
        age + gender + race + educ +
        bmi_cat + smoke_100 + pa_mod +
        any_comorb,
      data = .x, family = binomial
    )
    
    tidy(fit, exponentiate = TRUE, conf.int = TRUE) %>%
      filter(term == "alcohol_fHigh") %>%
      mutate(
        Model  = "SES-stratified (adjusted)",
        Design = "Unweighted",
        SES    = as.character(.y$ses_cat)  
      )
  }) %>%
  ungroup()

# 5.4 Survey-weighted adjusted (your “Model 2”)
# Survey design object (IMPORTANT: use complete cases for model vars)
library(survey)

dat_wt <- nhanes_final %>%
  tidyr::drop_na(
    cvd, alcohol_f, ses_cat,
    age, gender, race, educ,
    bmi_cat, smoke_100, pa_mod,
    any_comorb,
    SDMVPSU, SDMVSTRA, WTINTPRP
  )

nhanes_svy <- svydesign(
  ids     = ~SDMVPSU,
  strata  = ~SDMVSTRA,
  weights = ~WTINTPRP,
  nest    = TRUE,
  data    = dat_wt
)

# Survey-weighted crude
m_crude_wt <- svyglm(
  cvd ~ alcohol_f,
  design = nhanes_svy,
  family = quasibinomial()
)

# Survey-weighted adjusted
m_adj_wt <- svyglm(
  cvd ~ alcohol_f + ses_cat +
    age + gender + race + educ +
    bmi_cat + smoke_100 + pa_mod +
    any_comorb,
  design = nhanes_svy,
  family = quasibinomial()
)




############################################################
# FINAL "ULTIMATE RESULTS TABLE"
############################################################
# ---- helper: extract OR for alcohol_fHigh ----
extract_or <- function(model, term = "alcohol_fHigh") {
  tidy(model, exponentiate = TRUE, conf.int = TRUE) %>%
    filter(term == !!term) %>%
    transmute(
      OR = estimate,
      CI = sprintf("(%.2f, %.2f)", conf.low, conf.high),
      p_value = p.value
    )
}

# overall: unweighted crude/adjusted
res_crude <- extract_or(m_crude) %>%
  mutate(Model="Crude", Design="Unweighted", SES="Overall")

res_adj <- extract_or(m_adj) %>%
  mutate(Model="Adjusted", Design="Unweighted", SES="Overall")

# overall: survey-weighted crude/adjusted
res_wt_crude <- extract_or(m_crude_wt) %>%
  mutate(Model="Crude", Design="Survey-weighted", SES="Overall")

res_wt_adj <- extract_or(m_adj_wt) %>%
  mutate(Model="Adjusted", Design="Survey-weighted", SES="Overall")

# SES-stratified adjusted (already tidy)
res_strat <- stratified_models %>%
  transmute(
    Model, Design, SES,
    OR = estimate,
    CI = sprintf("(%.2f, %.2f)", conf.low, conf.high),
    p_value = p.value
  )

final_results_table <- bind_rows(
  res_crude, res_adj,
  res_wt_crude, res_wt_adj,
  res_strat
) %>%
  mutate(
    OR = round(OR, 3),
    p_value = signif(p_value, 3)
  ) %>%
  select(Model, Design, SES, OR, CI, p_value)

final_results_table

############################################################
# PART 6: CELL COUNT CHECK (by SES) + EVENT COUNTS
############################################################
# 6.1 2x2 table within each SES (original)
by(nhanes_final, nhanes_final$ses_cat, function(d){
  cat("\nSES =", unique(d$ses_cat), "\n")
  print(table(d$cvd, d$alcohol_f))
})

# 6.2 event counts helper
count_events <- function(data, label, ses = "Overall") {
  tibble(
    Model = label,
    SES = ses,
    N_total = nrow(data),
    Events_cvd  = sum(data$cvd == 1, na.rm = TRUE),
    Events_High = sum(data$cvd == 1 & data$alcohol_f == "High", na.rm = TRUE),
    Events_Low  = sum(data$cvd == 1 & data$alcohol_f == "Low",  na.rm = TRUE)
  )
}

event_summary <- bind_rows(
  # Overall
  count_events(nhanes_final, "Full sample (unweighted)", "Overall"),
  
  # By SES (named with .y )
  nhanes_final %>%
    group_by(ses_cat) %>%
    group_modify(~ count_events(.x, "Full sample (unweighted)", ses = as.character(.y$ses_cat))) %>%
    ungroup()
)

event_summary
