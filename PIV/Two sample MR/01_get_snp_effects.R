# Get SNP effects to run Two sample MR
# Version: 
# - update on 23/07/24: to use income data on new 2022 data

library(data.table)
library(tidyverse)

# A. EXPOSURE #######
# 1. for EA4 #######
# As exposure GWAS we used EA4
# Use the same SNPs that we used to create the allele score in One sample MR
# p<5e-8 r=0.001 and 10000kbO

ea <- fread("N:/durable/data/genetics/GWAS_sumstats/EA4_excl_23andMe_exclMOBA_2022_04_04.meta.gz")
head(ea)

snps.valid <- fread("../EA_allele_score_for_MR/EA.valid.snp", header=F)
tail(snps.valid)

EA4.instruments <- merge(ea, snps.valid, by.x="rsID", by.y="V1")
#335 instruments 
rm(ea)
# save in order to export and run the TwoSample MR using the TwoSampleMR package outside TSD 
write.csv2(EA4.instruments, "EA4_instruments.csv", row.names=F)
write.table(EA4.instruments$rsID, "instruments.txt",quote=F,col.names = F, row.names=F)


#B. OUTCOME ######
# for the outcome, we need the associations of these SNPs with income in a different sample
# we decided to run these associations in our Moba sample
# this has the benefit to have 0 sample overlap and
# be highly comparable with our other analyses done in Moba (same measure of income, same sample)

# 1. Get the phenotype #####
# File created by Tarjei 
# This includes the main income variable used in our analyses now. 
# It is the average of the three top earnings years ages 34 to 40 
#(following Markussen & Rxed, 2022) (wage inflation adjusted to 2021 
# levels following Bhuller et al. (2018) prior to taking the average) and
# leaving out "extreme" earners (top 1%). Years of schooling (yos) is 
# educational attainment by age 33.

library(haven)
outcome <- read_dta("N:/durable/projects/EQOP/MR_Income/temp/income_for_twosample.dta")
head(outcome)

hist(outcome$log_income)
summary(outcome$log_income)


# 2. Merge with genetic data and covariate datasets ##### 

## 2.1 get Moba IDs #####
linkage <- fread("N:/durable/data/moba/linkage/PDB2601_kobling_SSB_v12.csv")
head(linkage)

income_data <- merge(linkage, outcome, by = "w19_0634_lnr")
head(income_data)#165958

## 2.2 get Moba genetic ID (SENTRIX) #######
child_sentrix <- fread("N:/durable/data/moba/MoBaGenetics/key_CENTRIX_ID_Aug_22/PDB2601_MoBaGeneticsTot_Child_20220829.csv")
father_sentrix <- fread("N:/durable/data/moba/MoBaGenetics/key_CENTRIX_ID_Aug_22/PDB2601_MoBaGeneticsTot_Father_20220829.csv") 
mother_sentrix <- fread("N:/durable/data/moba/MoBaGenetics/key_CENTRIX_ID_Aug_22/PDB2601_MoBaGeneticsTot_Mother_20220829.csv") 
head(child_sentrix)

parents <- rbind(mother_sentrix[,-1], father_sentrix[,-1])

head(parents)
table(income_data$rolle) # no child, makes sense as children do not have an income 
table(father_sentrix$ROLE)
income_data <- income_data %>%
  mutate(rolle = case_when(
    rolle == "SU2PT_FATHER" ~ "Father",
    rolle == "SU2PT_MOTHER" ~ "Mother"
  )) 
income_data <- income_data %>%
  rename(ROLE = rolle)

income_data_sentrix <- inner_join(income_data, parents, by = c("PREG_ID_2601","ROLE"))

# We want to keep those that passed QC. The covariate file has the ID of who passed qcs 
covariate <- fread("N:/durable/data/genetics/MoBaPsychGen_v1/MoBaPsychGen_v1-ec-eur-batch-basic-qc-cov-noMoBaIDs.txt")
income_data_sentrix <- merge(income_data_sentrix, covariate, by.x = "SENTRIX_ID", by.y = "SENTRIXID") 

table(duplicated(income_data_sentrix$w19_0634_lnr)) # some duplicates why? 
table(duplicated(income_data_sentrix$SENTRIX_ID)) # same number of duplicate for sentrix id 
income_data_sentrix[duplicated(income_data_sentrix$w19_0634_lnr)== T, ] # looks like it is parents with several kids so different PREG_ID 
#can safely drop one of the duplicated at random
income_data_sentrix <- income_data_sentrix[!duplicated(income_data_sentrix$SENTRIX_ID), ]

# N = 112162


## 2.2 Adapt for GCTA #####
income.gcta <- income_data_sentrix[, c("FID", "IID", "log_income")]
write.table(income.gcta, "income.gcta", quote=F,col.names = F, row.names=F)


# 3. Run the GWAS ######
# 3.1 Add birthyear in covariate files #####
cov <- fread("N:/durable/projects/Perline/Key_scripts/20PCs.qcov")
head(cov)
head(income_data_sentrix)
yob <- income_data_sentrix[, c("FID", "IID", "birthyear")]
covyob  <- merge(cov, yob, by.x = c("V1", "V2"), by.y=c("FID","IID"))
write.table(covyob, "20PCS_yob.qcov", quote=F,col.names = F, row.names=F)

# 3.2 Run GWAS in terminal #### 
# RUN IN TERMINAL
covariate="/tsd/p805/data/durable/projects/Perline/Key_scripts"

/tsd/p805/cluster/software/gcta/gcta-1.94.1 \
--fastGWA-mlm \
--bfile /tsd/p805/data/durable/data/genetics/MoBaPsychGen_v1/MoBaPsychGen_v1-ec-eur-batch-basic-qc \
--grm-sparse /tsd/p805/data/durable/data/genetics/MoBaPsychGen_v1/MoBaPsychGen_v1-ec-eur-batch-basic-qc_regenie_500k_snps_sparse0.05 \
--pheno income.gcta \
--qcovar 20PCS_yob.qcov \
--covar ${covariate}/sex_batches.cov \
--est-vg HE \
--threads 10 \
--out gwas_mr_income
#removed the option to extract the instruments because it was leading to some issues with getting 2000snps for tunning (weird it worked before)

income_gwas <- fread("gwas_mr_income.fastGWA")
write.csv(income_gwas, "gwas_mr_income.csv", row.names=F, quote=F)

## 3.3 Check and save this GWAS based on 335 SNPs ######
instruments <- fread("instruments.txt", header=F)
head(income_gwas)
head(instruments)
income_gwas_335 <- merge(income_gwas, instruments, by.x = "SNP", by.y = "V1")
hist(income_gwas_335$P)
hist(income_gwas_335$BETA)

write.csv(income_gwas_335, "gwas_mr_income_335snps.csv", row.names=F, quote=F)

#4. Get GWAS in unrelated sample! to run two sample with within sib GWAS ####
income <- fread("wf_unrelated_with_income.txt")
write.table(income, "income_unrelated.gcta", quote=F,col.names = F, row.names=F)

hist(income$log_income)
# RUN IN TERMINAL 

covariate="/tsd/p805/data/durable/projects/Perline/Key_scripts"

/tsd/p805/cluster/software/gcta/gcta-1.94.1 \
--fastGWA-mlm \
--bfile /tsd/p805/data/durable/data/genetics/MoBaPsychGen_v1/MoBaPsychGen_v1-ec-eur-batch-basic-qc \
--grm-sparse /tsd/p805/data/durable/data/genetics/MoBaPsychGen_v1/MoBaPsychGen_v1-ec-eur-batch-basic-qc_regenie_500k_snps_sparse0.05 \
--pheno income_unrelated.gcta \
--qcovar 20PCS_yob.qcov \
--covar ${covariate}/sex_batches.cov \
--est-vg HE \
--threads 10 \
--out gwas_mr_income_unrelated


income_gwas <- fread("gwas_mr_income_unrelated.fastGWA")
write.csv(income_gwas, "gwas_mr_income_unrelated.csv", row.names=F, quote=F)

# Check and save this GWAS based on 335 SNPs 
instruments <- fread("instruments.txt", header=F)
head(income_gwas)
head(instruments)
income_gwas_335 <- merge(income_gwas, instruments, by.x = "SNP", by.y = "V1")
hist(income_gwas_335$P)
hist(income_gwas_335$BETA)

write.csv(income_gwas_335, "gwas_mr_income_335snps_unrelated.csv", row.names=F, quote=F)

 