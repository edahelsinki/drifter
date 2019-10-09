#!/bin/bash

# process results
Rscript --vanilla analyse_scalability.R results/scalability_results figures
for dataset_id in 1 3 4 5 6
do
    Rscript --vanilla combine_performance_results.R results/performance/ $dataset_id results
done
Rscript --vanilla final_results.R
bash make_figures.sh
