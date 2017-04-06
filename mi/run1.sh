#!/bin/bash

#SBATCH --mem=4g
#SBATCH -t 1:00:00 # 1 hour max run time
#SBATCH --mail-type=END,FAIL # notifications for job done & fail
#SBATCH --mail-user=vonholle@email.unc.edu # send-to address

module add sas
sas wts_mi_code_avh_check.sas
