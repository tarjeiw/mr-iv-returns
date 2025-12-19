# Identify sibling sample with genetic data and EA
# and unrelated sample
# Perline Demange 
# Mai 2025

rm(list = ls())
library(data.table)

#1. For education, keep only people who were more than 25 in 2023  ##################

#EduYears data in the pop registry 
#see EDUCATION.R
load("educational_attainement_250517.rda")

# Keep only people who were more than 25 in 2023 
edu <- edu[2023 - edu$birth_year >= 25, ]  #doing the threshold at 33 removes only 6 people here, so likely no one in the gwas. 
table(edu$EduYears11_2023)

head(edu)
edu <- edu[, c("w19_0634_lnr", "EduYears11_2023","EduYearsNO_2023", "kjoenn", "birth_year")]
hist(edu$EduYears11_2023)
hist(edu$EduYearsNO_2023)

# 2. Merge with genetic id #################
linkage <- fread("N:/durable/data/moba/linkage/merged_IDs/MoBa_SSB_IDs_20250317.csv")
# Remove SENTRIX_ID that did not pass QC 
# Use one QCd file (here covariates) to clean up 
covariate <- fread("N:/durable/data/genetics/MoBaPsychGen_v1/MoBaPsychGen_v1-ec-eur-batch-basic-qc-cov-noMoBaIDs.txt")
QC_IDs <- covariate$SENTRIXID
qcd <- linkage[linkage$SENTRIX_ID %in% QC_IDs, ]
#add FID and IID to linkage 
qcd <- merge(qcd, covariate[, c(1,2,3)], by.x="SENTRIX_ID", by.y="SENTRIXID")                                                                                                                                                                 

# As mentioned by Clara, duplicated sentrix in linkage file are individual with several preg_id 
# I remove duplicated individuals here already
qcd <- qcd %>%
  group_by(SENTRIX_ID) %>%
  sample_n(size=1) %>%
  ungroup


head(qcd)
qcd <- qcd[,c(4,7,8)]

edu <- merge(edu, qcd, by = "w19_0634_lnr")

#remove when no sentrix id 
edu <- edu[!is.na(edu$IID), ]
#128046 


# 3. Merge with sibling file ######
output1 <- fread("Siblings-FID.fam")
colnames(output1) <- c("FID2", "IID")

sibEA <- merge(edu, output1, by="IID")
#21576
table(sibEA$EduYears11_2023)

# Count occurrences of each FID
fid_counts <- table(sibEA$FID2)

# Keep only rows with FIDs that occur at least twice
sibEA <- sibEA[sibEA$FID2 %in% names(fid_counts[fid_counts >= 2]), ]
#those are the individuals that will be used in the within sib gwas 
#21182

sibEA11 <- sibEA[, c("FID2", "IID", "EduYears11_2023")]
colnames(sibEA11) <- c("FID", "IID", "Education")
write.table(sibEA11, file = "SIBEA.pheno", sep = "\t", row.names = FALSE, quote = FALSE)

sibEA11 <- fread("SIBEA.pheno")
head(sibEA11)
anyNA(sibEA11$Education)
summary(sibEA11$Education) #there is 3 NAs weirdly, this could explain some small of the sample mismatchs
mean(sibEA11$Education, na.rm=T)
sd(sibEA11$Education, na.rm=T) #3.23

# The checks from Howe et al says some families are not complete...
update <- fread("results/01/updated_phenotypes.txt")
summary(update$Education) #Nas were correctly set for the missing families 

## standardize EA in the sibling pop and remove the missing ####
sibEA11 <- fread("SIBEA.pheno")
sibEA11 <- sibEA11[!is.na(sibEA11$Education), ]
fid_counts <- table(sibEA11$FID)
sibEA11 <- sibEA11[sibEA11$FID %in% names(fid_counts[fid_counts >= 2]), ] #21776 
sibEA11$Education <- scale(sibEA11$Education)
hist(sibEA11$Education)
sd(sibEA11$Education, na.rm=T)
colnames(sibEA11) <- c("FID", "IID", "Education_std")
write.table(sibEA11, file = "SIBEA_std.pheno", sep = "\t", row.names = FALSE, quote = FALSE)

# create file to run gwas on NO system EA #Not Done 
sibEANO <- sibEA[, c("FID2", "IID", "EduYearsNO_2023")]
colnames(sibEANO) <- c("FID", "IID", "EducationNO_std")
sibEANO <- sibEANO[!is.na(sibEANO$Education), ]
fid_counts <- table(sibEANO$FID)
sibEANO <- sibEANO[sibEANO$FID %in% names(fid_counts[fid_counts >= 2]), ] #21776 
sd(sibEANO$Education, na.rm=T) #2.81
sibEANO$Education <- scale(sibEANO$Education)
hist(sibEANO$Education)
write.table(sibEANO, file = "SIBEANO_std.pheno", sep = "\t", row.names = FALSE, quote = FALSE)

## bonus: Get income for siblings and check how different the samples are ##### 
#can be used to run a wihtinsib gwas of income if useful #not done
income <- fread("N:/durable/projects/EQOP/MR_Income/Two_sample_MR/income.gcta")
head(income)
colnames(income)<-c("FID","IID","log_income")
head(sibEA) 
sibincome <- merge(sibEA, income, by=c("FID","IID")) #21182 for EA, now 19396 (tbh no sure why we dont have income for some people)
# Count occurrences of each FID
fid_counts <- table(sibincome$FID2)
# Keep only rows with FIDs that occur at least twice
sibincome <- sibincome[sibincome$FID2 %in% names(fid_counts[fid_counts >= 2]), ]
#18269
sibincome <- sibincome[, c("FID2", "IID", "log_income")] #get the fid from howe pipeline if I want to run the gwas
colnames(sibincome) <- c("FID", "IID", "log_income")
write.table(sibincome, file = "SIBincome.pheno", sep = "\t", row.names = FALSE, quote = FALSE)

# 4. Identify unrelated participants (based on sibEA) ####
## From KING, identify genetic relatedness
relatedness <- fread("N:/durable/data/genetics/MoBaPsychGen_v1/MoBaPsychGen_v1-ec-eur-batch-basic-qc-rel.kin")
head(relatedness)
relatedness$relation[relatedness$Kinship > (1/(2^(3/2)))] <- "mztwins"
relatedness$relation[relatedness$Kinship < (1/(2^(3/2))) & relatedness$Kinship > (1/(2^(5/2))) & relatedness$IBS0 < 0.0012] <- "parents"
relatedness$relation[relatedness$Kinship < (1/(2^(3/2))) & relatedness$Kinship > (1/(2^(5/2))) & relatedness$IBS0 > 0.0012] <- "fullsibs"
relatedness$relation[relatedness$Kinship < (1/(2^(5/2))) & relatedness$Kinship > (1/(2^(7/2)))] <- "deg2"
relatedness$relation[relatedness$Kinship < (1/(2^(7/2))) & relatedness$Kinship > (1/(2^(9/2)))] <- "deg3"
relatedness$relation[relatedness$Kinship < (1/(2^(9/2)))] <- "unrelated" 
relatedness <- relatedness[which(!relatedness$relation == "unrelated"),] 
table(relatedness$relation)
relatedness[relatedness$relation == "mztwins",]
#everyone in relatedness file is related to someone 

#now I want to keep people NOT related to someone in sibEA 

sibs <- sibEA$IID
related <- rbind(relatedness[relatedness$ID1 %in% sibs,], relatedness[relatedness$ID2 %in% sibs,])
table(related$relation)
#all these people are related to someone in sibEA 
#remove them from full sample, I will base my full sample on covariate file 

relatedlist <- c(related$ID1, related$ID2)
unrelated <- covariate[, c(1,2,3)][!(covariate$IID %in% relatedlist), ]
# 165832

#5. Identify unrelated with income data #############
income <- fread("../Two_sample_MR/income.gcta")
colnames(income) <- c("FID", "IID", "log_income")

income <- merge(unrelated, income, by="IID")
summary(income$log_income)
# 91892 
head(income)

# head(linkage)
# #to be able to identify the people in the original plink I need to original FID 
# income <- income[,c("IID", "log_income")]
# plinkFAM <- read.delim2("N:/durable/data/genetics/MoBaPsychGen_v1/MoBaPsychGen_v1-ec-eur-batch-basic-qc.fam", 
#                         header=F, sep=" ", as.is=T)
# plinkFAM <- plinkFAM[, c(1,2)]
# colnames(plinkFAM) <- c("FID", "IID")
# 
# income <- merge(income, plinkFAM, by = "IID")
income <- income[, c("FID.x", "IID", "log_income")]
colnames(income) <- c("FID", "IID", "log_income")
write.table(income, file = "wf_unrelated_with_income.txt", sep = "\t", 
            row.names = FALSE, quote = FALSE)

income <- income[, c("FID", "IID")]
write.table(income, file = "wf_unrelated.txt", sep = "\t", 
            row.names = FALSE, quote = FALSE, col.names=F)

unrelated <- unrelated[, c("FID", "IID")]
write.table(unrelated, file = "wf_unrelated_all.txt", sep = "\t", 
            row.names = FALSE, quote = FALSE, col.names=F)


# 6. Get EA pheno for unrelated samples ####
head(edu)
unrelated <- fread("wf_unrelated_all.txt", header=F)
colnames(unrelated) <- c("FID", "IID")
unrelated <- merge(unrelated, edu, by = c("FID", "IID")) #105921
write.table(unrelated, file = "wf_unrelated_withEA.txt", sep = "\t", 
            row.names = FALSE, quote = FALSE)


