#!/bin/bash
#SBATCH --job-name=lire_fichier
#SBATCH --nodes=1
#SBATCH --ntasks=4
#SBATCH --time=00:01:00
#SBATCH --output=lire_fichier-%j.out
#SBATCH --error=lire_fichier-%j.out
#SBATCH --hint=nomultithread
#RESERVATION

cd ${SLURM_SUBMIT_DIR}
 
module purge
 
module load intel-all/2020.4
set -x
 
srun ./lire_fichier
