#!/bin/bash
#SBATCH --partition short

#SBATCH --time=20:00:00
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=30000
#SBATCH --array=1-18

module load R 

OUTPUTDIR=models

srun Rscript --vanilla train_models.R $SLURM_ARRAY_TASK_ID $OUTPUTDIR
