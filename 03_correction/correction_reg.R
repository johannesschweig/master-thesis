##3. correction regression
## dataset
set.seed(123)
train <- sample.int(nrow(cleaned), floor(.8*nrow(cleaned)), replace = F)

#TRAIN MODELS
##degree: 1
# regX1 = lm(offsetX ~ X:dpt, data=cleaned[train,])
# regY1 = lm(offsetX ~ I(dpt^2), data=cleaned[train,])
##degree: 2
regX2 = lm(offsetX ~ dpt + X:dpt, data=cleaned[train,])
regY2 = lm(offsetY ~ I(dpt^2) + Y:dpt, data=cleaned[train,])
##degree: 3
# regX3 = lm(offsetX ~ dpt + X:dpt + I(Y^2), data=cleaned[train,])
# regY3 = lm(offsetX ~ I(dpt^2) + Y:dpt + dpt, data=cleaned[train,])
##degree: 4
# regX4 = lm(offsetX ~ dpt + X:dpt + I(Y^2) + X:Y, data=cleaned[train,])
# regY4 = lm(offsetX ~ I(dpt^2) + Y:dpt + dpt + X:dpt, data=cleaned[train,])
##degree: 5 full model, R^2>.95
regX5 = lm(offsetX ~ polym(X, Y, dpt, degree=5, raw=T), data=cleaned[train,])
regY5 = lm(offsetY ~ polym(X, Y, dpt, degree=5, raw=T), data=cleaned[train,])
##degree: 9 full model, R^2>.95
regX9 = lm(offsetX ~ polym(X, Y, dpt, degree=9, raw=T), data=cleaned[train,])
regY9 = lm(offsetY ~ polym(X, Y, dpt, degree=9, raw=T), data=cleaned[train,])

#SAVE MODELS
# save(regX1, regY1, regX2, regY2, regX3, regY3, regX4, regY4, file=paste0(getwd(), "/out/reg.mdl"))
save(regX2, regY2, file=paste0(getwd(), "/out/reg.mdl"))

##train
## 80% of all cleaned trials
corrected = cleaned[train,]
## just the glasses trials 
corrected = corrected[corrected$g!=0,]
##test
## 20% of all cleaned trials
corrected = cleaned[-train,]
## just the glasses trials 
corrected = corrected[corrected$g!=0,]

#PERFORMANCE
corrected$corrX = NULL
corrected$corrY = NULL
corrected$corrX = corrected$X - predict(regX5, newdata=corrected)
corrected$corrY = corrected$Y - predict(regY5, newdata=corrected)
performance()  