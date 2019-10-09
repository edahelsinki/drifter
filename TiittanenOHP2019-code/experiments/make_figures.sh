#!/bin/sh

Rscript --vanilla make_figures.R results/performance/svm_lm_airquality_10_0_2_2.rds figures/fig3aq.pdf figures/fig3aq_roc.pdf figures/fig3aq_c.pdf 0

Rscript --vanilla make_figures.R results/performance/randomforest_lm_airline_2_0_2_2.rds figures/fig3airline.pdf figures/fig3airline_roc.pdf figures/fig3airline_c.pdf 0

Rscript --vanilla make_figures.R results/performance/lm_lm_synthetic_04_60_0_2_2.rds figures/fig3slm.pdf figures/fig3slm_roc.pdf figures/fig3slm_c.pdf 0

Rscript --vanilla make_figures.R results/performance/svm_lm_synthetic_04_60_0_2_2.rds figures/fig3ssvm.pdf figures/fig3ssvm_roc.pdf figures/fig3ssvm_c.pdf 0

Rscript --vanilla make_figures.R results/performance/randomforest_lm_synthetic_04_60_0_2_2.rds figures/fig3srf.pdf figures/fig3srf_roc.pdf figures/fig3srf_c.pdf 0

Rscript --vanilla make_figures.R results/performance/lm_lm_bike_4_0_2_2.rds figures/fig3bike.pdf figures/fig3bike_roc.pdf figures/fig3bike_c.pdf 1

Rscript --vanilla make_figures.R results/performance/lm_lm_bike_current_4_0_2_2.rds figures/fig3bike_current.pdf figures/fig3bike_current_roc.pdf figures/fig3bike_current_c.pdf 1
