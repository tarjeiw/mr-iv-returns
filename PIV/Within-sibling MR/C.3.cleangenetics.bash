cd /ess/p805/data/durable/projects/Perline/TEMP_GENDATA_withinsibGWAS/
awk '{
if (($5 == "A" || $5 == "T" || $5 == "C" || $5=="G") && ($6 == "A" || $6 == "T" || $6 == "C" || $6=="G"))
print $1, "chr"$1":"$4":SNP", $3, $4, $5, $6;
else
print $1, "chr"$1":"$4":INDEL", $3, $4, $5, $6;
}' subset_plink_siblings.bim > subset_plink_siblings_filtered.bim


awk '{
if (++dup[$2] > 1) {
print $1, $2".duplicate."dup[$2], $3, $4, $5, $6
} else {
print $0 }
}' subset_plink_siblings_filtered.bim > subset_plink_siblings_filtered_nodup.bim

mv subset_plink_siblings.bed subset_plink_siblings_filtered_nodup.bed
mv subset_plink_siblings.fam subset_plink_siblings_filtered_nodup.fam