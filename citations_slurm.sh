#!/bin/bash
#SBATCH -J tc                           # Job name
#SBATCH -o slurm_output/output_%j.txt                # Output file (%j will be replaced by the job ID)
#SBATCH -e slurm_output/error_%j.txt                 # Error file (%j will be replaced by the job ID)
#SBATCH --ntasks=1                      # Number of tasks (processes)
#SBATCH --cpus-per-task=16              # Number of CPU cores per task
#SBATCH --mem=100G                      # Memory per node (450 GB)
#SBATCH --partition=day
#SBATCH --time=8:00:00

echo '-------------------------------'
cd ${SLURM_SUBMIT_DIR}                  # Change directory to slurm submit directory
echo ${SLURM_SUBMIT_DIR}
echo Running on host $(hostname)
echo Time is $(date)
echo SLURM_NODES are $(echo ${SLURM_NODELIST})
echo '-------------------------------'
echo -e '\n\n'

# Load any necessary modules
module load R/4.3.0-foss-2020b                        

# Your R script execution command
Rscript "Code/main.R"
