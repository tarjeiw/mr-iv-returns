#reformat covariate file for Howe pipeline

# Covariate file format.
# 
# A covariate file should be provided satisfying the following requirements:
#   
#   a) First two columns should be FID/IID.
# b) One column should contain Age (years), defined as 2019 minus YoB and labelled as "Age".
# c) One column should contain Sex, labelled as "Sex". Males should be coded as 1 and females as 0.
# d) Twenty columns containing the first 20 genomic principal components. Note that it is potentially possible to alter the primary analysis to include less principal components for computational efficiency.
# e) Tab delimited.

cov <- fread("SIBEA.pheno")
# Get sex from the fam file of the full genotype data 
genedir = "N:\\durable\\data\\genetics\\MoBaPsychGen_v1"
genefile = "MoBaPsychGen_v1-ec-eur-batch-basic-qc"
fam = read.table(paste0(genedir, "\\", genefile, ".fam"), sep = " ", na.strings = "0",
                 col.names =  c("FID", "IID", "father", "mother", "sex", "phenotype"))
head(fam)
fam <- fam[,c("IID", "sex")]
sex <- merge(cov, fam, by= "IID")


covariate <- fread("N:/durable/data/genetics/MoBaPsychGen_v1/MoBaPsychGen_v1-ec-eur-batch-basic-qc-cov-noMoBaIDs.txt")
head(covariate)
covariate <- covariate[, c(3,14:33 )]

covariate <- merge(sex, covariate, by="IID")
covariate <- covariate[,c(2,1,4, 5:24)]

#Get YoB. I dont get why they use a centering on 2019 in howe et al, so I am just going to use yob 
linkage <- fread("N:/durable/data/moba/linkage/merged_IDs/MoBa_SSB_IDs_20250317.csv")
linkage <- linkage[, c(3,6)]
colnames(linkage) <- c("w19_0634_lnr", "IID")
basic <- fread("N:/durable/data/registers/SSB/01_data/data_v5.0/CORE/csv/POPULATION_FASTE_OPPLYSNINGER_reduced.csv", 
                           strip.white=TRUE)
head(basic)
basic <- basic[, c(1,4)]
cov <- left_join(covariate, linkage, by="IID")
table(duplicated(cov$IID))
table(duplicated(cov$w19_0634_lnr))
cov <- cov %>%
  group_by(IID) %>% 
  sample_n(size=1) %>%
  ungroup
cov <- left_join(cov, basic, by = "w19_0634_lnr")

cov <- extract(cov, foedsels_aar_mnd, into = c("birthyear", "birthmonth"), "(.{4})(.{2})", remove=FALSE)
cov <- cov[, c(1:23, 26)] #remove unnecessayr rows
names(cov)[names(cov) == "birthyear"] <- "Age"
names(cov)[names(cov) == "sex"] <- "Sex"
write.table(cov, file = "SIBEA.cov", sep = "\t", row.names = FALSE, quote = FALSE)
