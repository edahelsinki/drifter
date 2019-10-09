#!/bin/bash
#SBATCH --partition short

#SBATCH --time=20:00:00
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=30000
#SBATCH --array=1-540

module load R

OUTPUTDIR=results/scalability_results

srun Rscript --vanilla experiment_scalability.R 2 $SLURM_ARRAY_TASK_ID $OUTPUTDIR
