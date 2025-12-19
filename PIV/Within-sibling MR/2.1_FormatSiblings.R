#Following pipeline by Lawrence Howe for within sibling GWAS 
# Format sibling file from KING 

#Format sibling file from long to wide

# Load dependencies
require(data.table)

#Read in file and restrict to siblings

kin <- read.delim('N:/durable/data/genetics/MoBaPsychGen_v1/MoBaPsychGen_v1-ec-eur-batch-basic-qc-rel.kin',
                  header=T, as.is=T)


# Restrict to full siblings
kin2 <- kin[kin$InfType=='FS',]

#Converting from pairwise format to individual level format
#sibs<-input2[,c(1:3)]
sibs <- kin2[,c("FID", "ID1", "ID2")]
names(sibs) <- c("FID", "IID", "Sibling")
sibs$FID <- seq.int(nrow(sibs))

#sibs2<-input2[,c(1:3)]
sibs2 <- kin2[,c("FID", "ID1", "ID2")]
names(sibs2) <- c("FID", "Sibling", "IID")
sibs2$FID <- seq.int(nrow(sibs2))

sibs3<-rbind(sibs,sibs2)
sibs4<-sibs3[order(sibs3$FID),]

#Generate duplicated and non-duplicated sets of siblings
nodup<-sibs4[!duplicated(sibs4$IID) & !duplicated(sibs4$Sibling),]
dup<-sibs4[duplicated(sibs4$IID) | duplicated(sibs4$Sibling),]
dup$FID<-NULL
names(dup)<-c("IID", "Sibling2")

#Merge to get same family IDs for siblings
merge<-merge(nodup,dup,by="IID")
merge2<-merge[order(merge$FID),]
merge3<-merge2[,c(2,3)]
merge4<-merge2[,c(2,4)]
names(merge4)<-c("FID", "Sibling")
merge5<-rbind(merge3,merge4)
names(merge5)<-c("FID", "IID")
sibtrios<-merge5[!duplicated(merge5),]

#Merge everything together
nodup$Sibling<-NULL
final<-rbind(nodup,sibtrios)
final2<-final[order(final$FID),]
final3<-final2[!duplicated(final2),]

#Output
output1<-final3[,c(1,2)]
names(output1)<-c("FID", "IID")
output2<-final3[,c(2,2)]
names(output2)<-c("FID", "IID")
write.table(output1, "Siblings-FID.fam", quote=F, row.names=F, col.names=F, sep=" ")
