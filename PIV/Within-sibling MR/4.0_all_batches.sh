#!/bin/bash
#SBATCH --account=p805
#SBATCH --job-name=array_job
#SBATCH --output=logs/output_%A_%a.out
#SBATCH --error=logs/error_%A_%a.err
#SBATCH --array=1-1397
#SBATCH --time=00:30:00
#SBATCH --mem-per-cpu=5G
#SBATCH --cpus-per-task=1

# Get the array task ID (1–140)
arg1=${SLURM_ARRAY_TASK_ID}

# Run your script with the arguments
./4.0_unified_regression.sh $arg1 Education_std
