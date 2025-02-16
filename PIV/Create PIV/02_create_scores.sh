#!/bin/bash
#SBATCH --job-name=EA_score_MR
#SBATCH --account=p805_tsd
#SBATCH --time=12:00:00
#SBATCH --ntasks=1
#SBATCH --mem-per-cpu=15G
#SBATCH --cpus-per-task=10

module load plink/1.90b6.2

bfile_ref="/tsd/p805/cluster/data/genetics/reference_panels/1kg_eur"
bfile_moba="/tsd/p805/cluster/data/genetics/MoBaPsychGen_v1/MoBaPsychGen_v1-ec-eur-batch-basic-qc"
base_sumstats="/tsd/p805/data/durable/projects/EQOP/MR_Income/EA_allele_score_for_MR/EA4_SNPs_shared_Moba.txt"
output_dir="/tsd/p805/data/durable/projects/EQOP/MR_Income/EA_allele_score_for_MR/"

# --score arguments are file SNPID effect_allele effect_size header
plink \
    --bfile ${bfile_moba} \
    --score ${base_sumstats} 3 5 17 header \
    --extract ${output_dir}EA.valid.snp \
    --out ${output_dir}EA_scores
    
# I actually ran this interactively and it ran so fast!! 