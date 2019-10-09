#!/bin/bash
#SBATCH --partition short

#SBATCH --time=12:00:00
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=30000
#SBATCH --array=1-810

module load R

OUTPUTDIR=results/performance

for dataset_id in 1 3 4 5 6
do
    srun Rscript --vanilla experiment_test_drifters.R ../data $dataset_id $SLURM_ARRAY_TASK_ID $OUTPUTDIR
done
