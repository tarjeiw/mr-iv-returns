#subset plink files to siblings only to go faster

keep <- fread("SIBEA.pheno")
write.table(keep[, 1:2], "keep.txt", sep = "\t", row.names = FALSE, quote = FALSE, col.names=F)


#In putty, in the directory TEMP:GENDATA 
module load PLINK/1.90b_6.2-x86_64

bfile_moba="/ess/p805/data/durable/projects/Perline/TEMP_GENDATA_withinsibGWAS/MoBaPsychGen_v1-ec-eur-batch-basic-qc"

plink --bfile ${bfile_moba} --keep keep.txt --make-bed --out subset_plink_siblings

