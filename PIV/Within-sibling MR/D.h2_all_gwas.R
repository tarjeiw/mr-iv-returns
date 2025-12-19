# Check heritabilities and genetic correlations of all the GWASes used in this paper 
# using ldsc in genomic SEM 
library(GenomicSEM)
library(data.table)

# GWAS - clean and reformat if needed ######

# EA4 excluding 23andme and moba
ea <- fread("N:/durable/data/genetics/GWAS_sumstats/EA4_excl_23andMe_exclMOBA_2022_04_04.meta.gz")

# income in all of moba 
income_gwas <- fread("N:/durable/projects/EQOP/MR_Income/Two_sample_MR/gwas_mr_income.csv")
# income in unrelated samples of moba 

# within-sibling moba EA
gwas_wf <- fread("N:/durable/projects/EQOP/MR_Income/WithinsibGWAS/EA_WF_Moba.sumstats")
#need to calculate effective sample
#need to get MAF 
# module load PLINK/1.90b_6.2-x86_64
# bfile_raw="/ess/p805/data/durable/projects/Perline/TEMP_GENDATA_withinsibGWAS/subset_plink_siblings_filtered_nodup"
# plink --bfile ${bfile_raw} --freq --out sib_maf
# THIS Y WORKS FOR FOUNDERS; AND I DONT hAVE FOUNDERS AS THEY ARE Sib
#I am gonna take the AF1 calculated for the income gwas with fastgwa... 
income_gwas_maf <- income_gwas[,c("CHR", "SNP", "POS", "AF1")]
gwas_wf <- gwas_wf[,-c("SNP")]
gwas_wf <- merge(gwas_wf, income_gwas_maf, by.x=c("CHR","BP"), by.y=c("CHR","POS"))
#also need to rename columns 
colnames(gwas_wf) <- c("CHR", "BP","A1","A2","N_REG","BETA", "SE", "P","SNP","AF1")
write.table(gwas_wf, file="N:/durable/projects/EQOP/MR_Income/WithinsibGWAS/EA_WF_Moba_newcolnames.sumstats")

  # Restrict to MAF between 0.1 and 0.4
  factor_sub <- subset(gwas_wf, AF1 <= 0.4 & AF1 >= 0.1)
  # Calculate Effective_N
  Effective_N <- mean((((factor_sub$BETA_WF/factor_sub$SE_BETA_WF) /
                          factor_sub$BETA_WF)^2) /
                        (2 * factor_sub$AF1 * (1 - factor_sub$AF1)))
#816....
  
# within-sibling moba ea with only EA4 hits 
piv_wf <- fread("EA_WF_Moba_335SNPs.sumstats")
head(piv_wf)
piv_wf <- piv_wf[,-c("SNP")]
piv_wf <- merge(piv_wf, income_gwas_maf, by.x=c("CHR","BP"), by.y=c("CHR","POS"))
#piv_wf$AF1 <- 0.5 - piv_wf$AF1
# Restrict to MAF between 0.1 and 0.4
factor_sub <- subset(piv_wf, AF1 <= 0.4 & AF1 >= 0.1)
# Calculate Effective_N
Effective_N <- mean((((factor_sub$BETA/factor_sub$SE) /
                        factor_sub$BETA)^2) /
                      (2 * factor_sub$AF1 * (1 - factor_sub$AF1)))
#816 as well # I checked in case the AF1 is about the wrong allele and it only increase the effective n to 1051 


# Within siblign in Moba BUT education was std before runing gwas #
gwas_wf_std <- fread("N:/durable/projects/EQOP/MR_Income/WithinsibGWAS/results/05/MobaEQOP.sumstats.Education_std.txt.gz")
head(gwas_wf_std)
gwas_wf_std <- gwas_wf_std[, c("CHR", "SNP","BP","A1","A2","N_REG","BETA_WF", "SE_BETA_WF", "P_BETA_WF")]
# Get MAF and calculate effective sample size 
#I am gonna take the AF1 calculated for the income gwas with fastgwa... 
#I assume the MAF in the sibling pop is the same as in the rest of Moba 
income_gwas_maf <- income_gwas[,c("CHR", "SNP", "POS", "AF1")]
gwas_wf_std <- gwas_wf_std[,-c("SNP")]
gwas_wf_std <- merge(gwas_wf_std, income_gwas_maf, by.x=c("CHR","BP"), by.y=c("CHR","POS"))

# Restrict to MAF between 0.1 and 0.4
factor_sub <- subset(gwas_wf_std, AF1 <= 0.4 & AF1 >= 0.1)
# Calculate Effective_N
Effective_N <- mean((((factor_sub$BETA_WF/factor_sub$SE_BETA_WF) /
                        factor_sub$BETA_WF)^2) /
                      (2 * factor_sub$AF1 * (1 - factor_sub$AF1)))
Effective_N
#8519 So basically x10 previously so maybe indeed not standardizing EA leads to quite some scaling effects.. 
#Makes more sense as this is about the sample divided by a bit more than 2.. 
summary(gwas_wf_std$N_REG)#max 21176

#also need to rename columns 
colnames(gwas_wf_std) <- c("CHR", "BP","A1","A2","N_REG","BETA", "SE", "P","SNP","AF1")
write.table(gwas_wf_std, file="N:/durable/projects/EQOP/MR_Income/WithinsibGWAS/EA_WF_Moba_std.sumstats", row.names=F)

# MUNGE ####

library(GenomicSEM)

files <- c("N:/durable/data/genetics/GWAS_sumstats/EA4_excl_23andMe_exclMOBA_2022_04_04.meta.gz", 
            "N:/durable/projects/EQOP/MR_Income/WithinsibGWAS/EA_WF_Moba_newcolnames.sumstats")
names <- c("EA4",
           "MobaEA")
N <- c(764661, #check sample size EA4 765,283 - 622 = 764661
       815 ) # Howe et al used effective sample size, calculated above 

munge(files=files, 
      hm3 = "N:/durable/data/genetics/ld/w_hm3.noMHC.snplist",
      trait.names=names,
      N=N)

files <- c("N:/durable/projects/EQOP/MR_Income/WithinsibGWAS/EA_WF_Moba_newcolnames.sumstats")
N <- c(21182) # try with actual sample size 
names <- c("MobaEAsample")
munge(files=files, 
      hm3 = "N:/durable/data/genetics/ld/w_hm3.noMHC.snplist",
      trait.names=names,
      N=N)

files <- c("N:/durable/projects/EQOP/MR_Income/WithinsibGWAS/EA_WF_Moba_std.sumstats")
N <- c(8519) # effective sample
names <- c("MobaEAstd")
munge(files=files, 
      hm3 = "N:/durable/data/genetics/ld/w_hm3.noMHC.snplist",
      trait.names=names,
      N=N)


files <- c("N:/durable/projects/EQOP/MR_Income/Two_sample_MR/gwas_mr_income.fastGWA",
           "N:/durable/projects/EQOP/MR_Income/Two_sample_MR/gwas_mr_income_unrelated.fastGWA")

N <- c(112162, 
       91892) 
names <- c("income", "income_unrelated")
munge(files=files, 
      hm3 = "N:/durable/data/genetics/ld/w_hm3.noMHC.snplist",
      trait.names=names,
      N=N)


# Run ldsc #####

ld <- "N:/durable/data/genetics/ld/eur_w_ld_chr/"
wld <- "N:/durable/data/genetics/ld/eur_w_ld_chr/"
names <- c("EA4",
           "MobaEA",
           "MobaEAsample", 
           "MobaEAstd",
           "income", 
           "income_unrelated")
traits <- paste0(names, ".sumstats.gz")
sample.prev <- c(NA,NA, NA, NA, NA, NA)
population.prev <- c(NA,NA,NA, NA, NA, NA)
rg <- ldsc(traits, sample.prev, population.prev, ld, wld, trait.names=names) 
save(rg, file="rg_h2.RData")
rg
