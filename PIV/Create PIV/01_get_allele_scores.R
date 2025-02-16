# Create allele scores to use for MR, for educational attainment

# Set up ###############
# Packages 
library(data.table)

# Summary statistics
# EA4 without Moba and without 23andMe
ea <- fread("N:/durable/data/genetics/GWAS_sumstats/EA4_excl_23andMe_exclMOBA_2022_04_04.meta.gz")
head(ea)


# Identify independent variants significantly associated with EA ####### 
# This uses the EA4 summary statistics 

## Select variants associated with lower than p 5*10-8######
ea_sig <- ea[ea$P<5e-8,]

## Exclude variants not present in Moba from the EA4 sumstats ######
# doing this step before the clumping is allowing to maximizing the number of shared snps between the two samples. 
moba_snplist <- fread("N:/durable/data/genetics/MoBaPsychGen_v1/MoBaPsychGen_v1-ec-eur-batch-basic-qc.snpstats") 
tail(moba_snplist)
ea_sig_moba <- ea_sig[ea_sig$rsID %in% moba_snplist$SNP, ]
# 49653 SNPs 

write.table(ea_sig_moba,
            "N:/durable/projects/EQOP/MR_Income/EA_allele_score_for_MR/EA4_SNPs_shared_Moba.txt",
            sep=" ", quote=F, row.names=F)


## Clumping: #####
# Use the current cutoff suggested by TwoSampleMR package: r=0.001 and 10000kb
# run interactively in Putty on TSD 

module load plink/1.90b6.2

bfile_ref="/tsd/p805/cluster/data/genetics/reference_panels/1kg_eur"
bfile_moba="/tsd/p805/cluster/data/genetics/MoBaPsychGen_v1/MoBaPsychGen_v1-ec-eur-batch-basic-qc.bim"
base_sumstats="/tsd/p805/data/durable/projects/EQOP/MR_Income/EA_allele_score_for_MR/EA4_SNPs_shared_Moba.txt"
output_dir="/tsd/p805/data/durable/projects/EQOP/MR_Income/EA_allele_score_for_MR/"

plink \
--bfile ${bfile_ref} \
--clump-p1 5e-8 \
--clump-r2 0.001 \
--clump-kb 10000 \
--clump ${base_sumstats} \
--clump-snp-field rsID \
--clump-field P \
--out ${output_dir}EA 

# 335 clumps 

# extract the column with the snp rsid 
awk 'NR!=1{print $3}' ${output_dir}EA.clumped >  ${output_dir}EA.valid.snp

# Create the scores using plink ####
# still using plink but submitting job as this is more demanding
# job is 02_create_scores.sh

