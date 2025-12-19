#!/bin/bash
#SBATCH --job-name=clump
#SBATCH --account=p805
#SBATCH --time=04:00:00
#SBATCH --mem-per-cpu=5G

module load PLINK/1.90b_6.2-x86_64

bfile_ref="/ess/p805/data/durable/data/genetics_test/refPanels/1kg_eur"
base_sumstats="/ess/p805/data/durable/projects/EQOP/MR_Income/WithinsibGWAS/EA_WF_Moba_std.sumstats"
output_dir="/ess/p805/data/durable/projects/EQOP/MR_Income/WithinsibGWAS/"
bfile_moba="/ess/p805/data/durable/data/genetics/MoBaPsychGen_v1/MoBaPsychGen_v1-ec-eur-batch-basic-qc"

plink \
--bfile ${bfile_moba} \
--clump-p1 1e-5 \
--clump-r2 0.001 \
--clump-kb 10000 \
--clump ${base_sumstats} \
--clump-snp-field SNP \
--clump-field P \
--out ${output_dir}EA_wf_std 