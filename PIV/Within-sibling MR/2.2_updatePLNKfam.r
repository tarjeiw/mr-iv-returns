# R script to update FID, keeping the order of individuals the same as the original PLINK file.

# Input
sibFID <- read.delim2("Siblings-FID.fam", header=F, sep=" ", as.is=T)
plinkFAM <- read.delim2("N:/durable/data/genetics/MoBaPsychGen_v1/MoBaPsychGen_v1-ec-eur-batch-basic-qc.fam", 
                        header=F, sep=" ", as.is=T)


# Set order - must keep the same order as in the PLINK file
plinkFAM$id <- 1:nrow(plinkFAM)

# Merge
merged <- merge(x = sibFID, y = plinkFAM, by = "V2", sort = FALSE, all=T)

# Order
ordered <- merged[order(merged$id), ]

# Check dim
dim(plinkFAM)
dim(ordered)

# Format the file
ordered$V1.y <- NULL
ordered$id <- NULL

# Add IID as FID for missing
ordered$V1.x[is.na(ordered$V1.x)] <- ordered$V2[is.na(ordered$V1.x)]

ordered <- ordered[, c(2,1,3,4,5,6)]
# Output
write.table(ordered, "update.fam", quote=F, row.names=F, col.names=F, sep=" ")
