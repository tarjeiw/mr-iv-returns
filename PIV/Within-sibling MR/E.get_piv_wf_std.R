# Create PIV with within sib EA from Moba 
# Perline Demange 
------------------------------------------
  
rm(list=ls())
library(data.table)
library(tidyverse)

# 1. Using the hits found in EA4, as done previously #####
hits <- fread("../EA_allele_score_for_MR/EA.clumped")
gwas_wf <- fread("EA_WF_Moba_std.sumstats")
head(hits)
hits <- hits[, c(1:6)]
head(gwas_wf)
piv_wf <- merge(hits, gwas_wf, by=c("CHR", "BP"))
tst <- merge(hits, gwas_wf, by="SNP")
#same
rm(tst)
colnames(piv_wf) <- c("CHR","BP","F","SNP.EA4","P.EA4", "TOTAL", "V1","A1", "A2",
                      "N_REG",  "BETA",  "SE", "P","SNP",   "AF1")

#quick check of the instruments
plot(piv_wf$P.EA4, piv_wf$P)
cor(piv_wf$P.EA4, piv_wf$P) 
#0.12, no correlation.. 
hist(piv_wf$P)
hist(piv_wf$BETA)

#save this bit as sumstats to make the score 
piv_wf <- piv_wf[, c("CHR", "BP", "SNP", "A1", "A2", "BETA", "SE", "P")]
write.table(piv_wf, file = "EA_WF_Moba_335SNPs_std.sumstats", sep = "\t", 
            row.names = FALSE, quote = FALSE)
#piv_wf <- fread("EA_WF_Moba_335SNPs.sumstats")

#run interactively 
module load PLINK/1.90b_6.2-x86_64

bfile_moba="/ess/p805/data/durable/data/genetics/MoBaPsychGen_v1/MoBaPsychGen_v1-ec-eur-batch-basic-qc"
base_sumstats="/ess/p805/data/durable/projects/EQOP/MR_Income/WithinsibGWAS/EA_WF_Moba_335SNPs_std.sumstats"
output_dir="/ess/p805/data/durable/projects/EQOP/MR_Income/WithinsibGWAS/"
original_dir="/ess/p805/data/durable/projects/EQOP/MR_Income/EA_allele_score_for_MR/"

# --score arguments are file SNPID effect_allele effect_size header
plink \
--bfile ${bfile_moba} \
--score ${base_sumstats} 3 5 6 header \
--extract ${original_dir}EA.valid.snp \
--out ${output_dir}EA_wf_std_335_scores

## 1.2 Keep only unrelated samples #####
ea_wf_scores <- fread("EA_wf_std_335_scores.profile")
#only keep the unrelated people to the gwas sample 
unrelated <- fread("wf_unrelated.txt", header = F)
colnames(unrelated) <- c("FID", "IID")
ea_wf_scores_unrelated <- merge(unrelated, ea_wf_scores, by= c("FID", "IID"))
#write over . profile to avoid issues 
write.table(ea_wf_scores_unrelated, file = "EA_wf_std_335_scores.profile", sep = "\t", 
            row.names = FALSE, quote = FALSE)

## 1.3  Check scores ##
hist(ea_wf_scores$SCORE)
#check prediction 
unrelatedea <- fread("wf_unrelated_withEA.txt")
ea_wf <- merge(ea_wf_scores, unrelatedea, by = c("FID", "IID"))
head(ea_wf)
cor.test(ea_wf$SCORE, ea_wf$EduYearsNO_2023)
plot(ea_wf$SCORE, ea_wf$EduYearsNO_2023)
#significant negative correlation 0.06 

# correlation between the two SCORES
ea_scores <- fread("N:/durable/projects/EQOP/MR_Income/EA_allele_score_for_MR/EA_scores.profile")
head(ea_scores)
colnames(ea_scores) <- c("FID","IID", "PHENO",   "CNT",  "CNT2", "SCORE_EA4")
head(ea_wf)
combined <- merge(ea_scores, ea_wf, by = c("FID", "IID"))
plot(combined$SCORE, combined$SCORE_EA4)
cor.test(combined$SCORE, combined$SCORE_EA4)
#-0.39, highly significant 

# 2. Using within sib GWAS hits ######
rm(list=ls())
gwas_wf <- fread("EA_WF_Moba_std.sumstats")
write.table(gwas_wf, file = "EA_WF_Moba_std.sumstats", sep = "\t", 
            row.names = FALSE, quote = FALSE) #needed to do this or the data was not properly read and i was getting not results 

# Identify significant SNPs #
gwas_wf[gwas_wf$P <5e-8, ] # no hits with usual threshold 
gwas_wf[gwas_wf$P <1e-5, ] #87 non clumped SNPs # threshold as in Demange et al 2024 

# check merging with 1kg #
# ref <- fread("N:/durable/data/genetics_test/refPanels/1kg_eur.bim")
# head(ref)
# colnames(ref) <- c("CHR", "rsid", "cM", "BP", "A1", "A2")
# gwas_wf <- merge(gwas_wf, ref, by=c("CHR", "BP"))
# gwas_wf[gwas_wf$P <1e-5, ] #10 "significant" snps once merged.... 



module load PLINK/1.90b_6.2-x86_64

base_sumstats="/ess/p805/data/durable/projects/EQOP/MR_Income/WithinsibGWAS/EA_WF_Moba_std.sumstats"
output_dir="/ess/p805/data/durable/projects/EQOP/MR_Income/WithinsibGWAS/"
bfile_moba="/ess/p805/data/durable/data/genetics/MoBaPsychGen_v1/MoBaPsychGen_v1-ec-eur-batch-basic-qc"

check <- fread("EA_WF_Moba_rsidfrom1kg.sumstats")
plink \
--bfile ${bfile_Moba} \
--clump-p1 1e-5 \
--clump-r2 0.001 \
--clump-kb 10000 \
--clump ${base_sumstats} \
--clump-snp-field SNP \
--clump-field P \
--out ${output_dir}EA_wf_std 
awk 'NR!=1{print $3}' ${output_dir}EA_wf_std.clumped >  ${output_dir}EA_wf_std.valid.snp
gwas_wf_clumped <- fread("EA_wf_std.clumped")
gwas_wf_clumped # 24 hits 


piv_wf <- merge(gwas_wf_clumped, gwas_wf, by=c("CHR", "BP"))

#rename to save rsid in this file and save this bit as sumstats to make the score 
piv_wf <- piv_wf[, c("CHR", "BP", "SNP.x", "A1", "A2", "BETA", "SE", "P.y")]
colnames(piv_wf) <- c("CHR", "BP", "SNP", "A1", "A2", "BETA", "SE", "P")
write.table(piv_wf, file = "EA_WF_Moba_std_24hits.sumstats", sep = "\t", 
            row.names = FALSE, quote = FALSE)



#run interactively 
module load PLINK/1.90b_6.2-x86_64

bfile_moba="/ess/p805/data/durable/data/genetics/MoBaPsychGen_v1/MoBaPsychGen_v1-ec-eur-batch-basic-qc"
base_sumstats="/ess/p805/data/durable/projects/EQOP/MR_Income/WithinsibGWAS/EA_WF_Moba_std_24hits.sumstats"
output_dir="/ess/p805/data/durable/projects/EQOP/MR_Income/WithinsibGWAS/"

# --score arguments are file SNPID effect_allele effect_size header
plink \
--bfile ${bfile_moba} \
--score ${base_sumstats} 3 4 6 header \
--extract ${output_dir}EA_wf_std.valid.snp \
--out ${output_dir}EA_wf_std_24hits_scores


# check scores 
ea_wf_scores <- fread("EA_wf_std_24hits_scores.profile")
#only keep the unrelated people to the gwas sample 
unrelated <- fread("wf_unrelated.txt", header = F)
colnames(unrelated) <- c("FID", "IID")
ea_wf_scores_unrelated <- merge(unrelated, ea_wf_scores, by= c("FID", "IID"))
#write over . profile to avoid issues 
write.table(ea_wf_scores_unrelated, file = "EA_wf_std_24hits_scores.profile", sep = "\t", 
            row.names = FALSE, quote = FALSE)

# Check distribution ###
hist(ea_wf_scores$SCORE)

#check prediction 
unrelatedea <- fread("wf_unrelated_withEA.txt")
ea_wf <- merge(ea_wf_scores, unrelatedea, by = c("FID", "IID"))
head(ea_wf)
cor.test(ea_wf$SCORE, ea_wf$EduYearsNO_2023)
plot(ea_wf$SCORE, ea_wf$EduYearsNO_2023)
#significant positive correlation 0.006 
#why change of direction between the two??? 

# correlation between the SCORES
ea_scores <- fread("N:/durable/projects/EQOP/MR_Income/EA_allele_score_for_MR/EA_scores.profile")
ea_wf_scores_335 <- fread("EA_wf_std_335_scores.profile")
head(ea_scores)
head(ea_wf_scores_335)
colnames(ea_scores) <- c("FID","IID", "PHENO",   "CNT",  "CNT2", "SCORE_EA4")
colnames(ea_wf_scores_335) <- c("FID","IID", "PHENO",   "CNT",  "CNT2", "SCORE_335")
head(ea_wf)
combined <- merge(ea_scores, ea_wf, by = c("FID", "IID"))
combined <- merge(combined, ea_wf_scores_335, by = c("FID", "IID"))
plot(combined$SCORE, combined$SCORE_EA4)
cor.test(combined$SCORE, combined$SCORE_EA4)
#0.017, highly significant 
cor.test(combined$SCORE, combined$SCORE_335)
# -0.03 highly significant 
# OK so PGI are both predictive (335 a bit more), but opposite direction!! 
