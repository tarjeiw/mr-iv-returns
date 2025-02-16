# Get SNP effects to run Two sample MR

setwd("N:/durable/projects/EQOP/MR_Income")

library(data.table)

# EXPOSURE #######
# As exposure GWAS we used EA4
# Use the same SNPs that we used to create the allele score in One sample MR
# p<5e-8 r=0.001 and 10000kb

ea <- fread("N:/durable/data/genetics/GWAS_sumstats/EA4_excl_23andMe_exclMOBA_2022_04_04.meta.gz")
head(ea)

snps.valid <- fread("N:/durable/projects/EQOP/MR_Income/EA_allele_score_for_MR/EA.valid.snp", header=F)
tail(snps.valid)

EA4.instruments <- merge(ea, snps.valid, by.x="rsID", by.y="V1")
#335 instruments 
rm(ea)

# OUTCOME ######
# for the outcome, we need the associations of these SNPs with income in a different sample
# we decided to run these associations in our Moba sample
# this has the benefit to have 0 sample overlap
# be highly comparable with our other analyses done in Moba (same measure of income, same sample)

# 1. Get the phenotype #####
# File created by Tarjei 
# This includes the main income variable used in our analyses now. 
# It is the average of the three top earnings years ages 34 to 40 
#(following Markussen & Røed, 2022) (wage inflation adjusted to 2021 
# levels following Bhuller et al. (2018) prior to taking the average) and
# leaving out "extreme" earners (top 1%). Years of schooling (yos) is 
# educational attainment by age 33.

library(haven)
outcome <- read_dta("./temp/income_yos_for_gwas.dta")
head(outcome)
View(outcome)
View(EA4.instruments)
View(snps.valid)

hist(outcome$income)
summary(outcome$income)

# Should I standardize this outcome somehow? 
# What did Kweon et al did in their new gwas of income? 
# Note: Kweon et al. use log income. 
outcome$log_income <- log(outcome$income)
View(outcome)
hist(outcome$log_income)
colSums(outcome==0)
# remove 0's
library(dplyr)
outcomev2 <- filter(outcome, income > 0)
colSums(outcomev2==0)
outcomev2$log_income <- log(outcomev2$income)
hist(outcomev2$log_income)

# Two sample MR
install.packages("TwoSampleMR")
library(TwoSampleMR)
# Warning in install.packages :
# package 'TwoSampleMR' is not available for this version of R

install.packages("MendelianRandomization")
library(MendelianRandomization)

# When we have SNP-income b and SE, 
# and combined these with SNP-yos and SE, then 
mr_ml_estimate(b_exp = snp_yos,
            b_out = snp_income,
            se_exp = yos_se,
            se_out = income_se)
mr_ml_estimate

