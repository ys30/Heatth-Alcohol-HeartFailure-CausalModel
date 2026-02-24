# 02_dag_model.R
# Source: refactored from 000Dag.R

source(file.path('scripts','00_setup.R'))

library(dagitty)
library(ggdag)
library(ggplot2)

dag_total <- dagitty("
dag {
  alcohol_f [exposure]
  cvd [outcome]

  ses_cat
  age
  gender
  race
  educ
  smoke_100
  pa_mod
  fiber_mean

  alcohol_f -> cvd

  ses_cat -> alcohol_f
  ses_cat -> cvd
  age -> alcohol_f
  age -> cvd
  gender -> alcohol_f
  gender -> cvd
  race -> ses_cat
  race -> cvd
  educ -> alcohol_f
  educ -> cvd

  smoke_100 -> alcohol_f
  smoke_100 -> cvd
  pa_mod -> alcohol_f
  pa_mod -> cvd
  fiber_mean -> alcohol_f
  fiber_mean -> cvd
}
")

exposures(dag_total) <- "alcohol_f"
outcomes(dag_total)  <- "cvd"

adjustmentSets(dag_total, exposure="alcohol_f", outcome="cvd")

coordinates(dag_total) <- list(
  x = c(race=-2, ses_cat=-1.5, educ=-1.0, age=-1.5, gender=-1.5,
        smoke_100=-0.2, pa_mod=-0.2, fiber_mean=-0.2,
        alcohol_f=0.8, cvd=2.2),
  y = c(race=1.0, ses_cat=0.3, educ=1.0, age=-0.3, gender=-0.9,
        smoke_100=0.9, pa_mod=0.0, fiber_mean=-0.9,
        alcohol_f=0, cvd=0)
)

ggdag(dag_total, text = FALSE) +
  theme_dag() +
  geom_dag_point(size=5) +
  geom_dag_text(color="white", size=3.2) +
  ggtitle("DAG (Total effect): alcohol_f → CHF (cvd)")
