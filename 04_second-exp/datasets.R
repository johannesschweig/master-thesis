# DATASETS
# classification
nrow(subset(cleaned, cal<=13 & g<=16)) + nrow(subset(cleaned, cal>13 & g<=16)) + nrow(subset(cleaned, cal<=13 & g>16)) + nrow(subset(cleaned, cal>13 & g>16))
# [1] 227328
## old+new cals, old gl
write.table(cleaned[cleaned$g<=16, c(7,8,9,10,4)], paste0(getwd(), "/in/class_val_old.txt"), sep=";", quote=F, row.names=F)
## old+new cals, new gl
write.table(cleaned[cleaned$g>16, c(7,8,9,10,4)], paste0(getwd(), "/in/class_val_new.txt"), sep=";", quote=F, row.names=F)


# regression
## dataset
write.table(cleaned[,c(4,5,6,9,10)], paste0(getwd(), "/in/corr_val.txt"), sep=";", quote=F, row.names=F)

