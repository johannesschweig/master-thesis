#CLASSIFICATION
## prepare datasets for training/test

##load data
cleaned = read.csv(paste0(getwd(), "/in/cleaned.txt"), sep=";", header=T, stringsAsFactors=F, colClasses = rep("numeric", 10), comment.char="#")

##GENERAL DATASET
temp = cleaned[c(7,8,9,10,4)]
set.seed(123)
train <- sample.int(nrow(temp), floor(.8*nrow(temp)), replace = F)
write.table(temp[train,], paste0(getwd(), "/in/train.txt"), sep=";", quote=F, row.names=F)
write.table(temp[-train,], paste0(getwd(), "/in/test.txt"), sep=";", quote=F, row.names=F)

# ##EQUAL DATASET
# ## same amount of each class
# temp = cleaned[c(7,8,9,10,4)]
# table(temp$dpt)
# bl = which(temp$dpt==0)
# set.seed(123)
# r = bl[sample.int(length(bl), length(bl)-25000)]
# temp = temp[-r,]
# table(temp$dpt)
# rm(bl,r)
# 
# set.seed(123)
# train <- sample.int(nrow(temp), floor(.8*nrow(temp)), replace = F)
# write.table(temp[train,], paste0(getwd(), "/in/train_eq.txt"), sep=";", quote=F, row.names=F)
# write.table(temp[-train,], paste0(getwd(), "/in/test_eq.txt"), sep=";", quote=F, row.names=F)
