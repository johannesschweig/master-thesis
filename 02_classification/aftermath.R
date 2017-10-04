# KNN
## load summary
knn = read.csv("out/knn_summary.txt", sep=";", header=T, stringsAsFactors = F, comment.char="#")
## max test_acc
knn_sum[which(knn$test==max(knn$test)),]
maxX = sqrt(knn$k[which(knn$test==max(knn$test))])
maxY = (knn$train[which(knn$test==max(knn$test))] + knn$test[which(knn$test==max(knn$test))])/2
# [1] 0.99038
# lookup: k=4, row: 2

## plot
par(mar=c(5,5,1,1)) # default: c(5,4,4,2) + .1; bottom, left, top, right
plot(sqrt(knn$k), knn$train, xlab="k", ylab="F-score", pch=16, xaxt="n", cex.lab=1.5, cex.axis=1.3)
points(sqrt(knn$k), knn$test, pch=16, col="red")
labels = c(parse(text=paste0(1,"^2")),NA,NA,NA,parse(text=paste0(5,"^2")),NA,NA,NA,NA,parse(text=paste0(10,"^2")),NA,NA,NA,NA,parse(text=paste0(15,"^2")),NA,NA,NA,NA,parse(text=paste0(20,"^2")),NA,NA,NA,NA,parse(text=paste0(25,"^2")),NA,NA,NA,NA,parse(text=paste0(30,"^2")),NA,NA,NA,NA,parse(text=paste0(35,"^2")))
axis(1, at=1:35, labels=labels, cex.axis=1.5)
rect(maxX-0.5,maxY-0.003, maxX+0.5,maxY+0.003, border="grey")
legend("topright", legend=c("train", "test"), pch=16, col=c("black", "red"), cex=1.3)
### saved as knn_sum.png (500x500)

## range acc
range(knn$fscore)
# [1] 0.942779 0.973193




##SVM
svm = read.csv("out/svm_summary.txt", sep=";", header=T, stringsAsFactors = F, comment.char="#")
library(ggplot2)
## max
svm[which(svm$test == max(svm$test)),]
maxX = svm$c[which(svm$test==max(svm$test))]
maxY = svm$gamma[which(svm$test==max(svm$test))]
## plot
ggplot(svm, aes(x=c,y=gamma, col=test)) + labs(x="C", y="gamma", col="F-score") + geom_text(label = round(svm$test,1)) + scale_y_log10() + scale_x_log10() + scale_colour_gradient(low = "grey", high = "black") + theme_bw(base_size=18)
### saved as svm_sum.png (500x500)

##LinearSVM
lsvm = read.csv("out/lsvm_summary.txt", sep=";", header=T, stringsAsFactors = F, comment.char="#")
## plot
## max test_acc
lsvm[which(lsvm$test==max(lsvm$test)),]
maxX = log2(lsvm$c[5])
maxY = (lsvm$train[5] + lsvm$test[5])/2
plot(log2(lsvm$c), lsvm$train, pch=16, xlab="C", ylab="Fscore", xaxt="n", cex.axis=1.3, cex.lab=1.5)
points(log2(lsvm$c), lsvm$test, pch=16, col="red")
rect(maxX-0.5,maxY-0.003, maxX+0.5,maxY+0.003, border="grey")
axis(1, at=log2(lsvm$c), labels=parse(text=paste0("2^",log2(lsvm$c))), cex.axis=1.3)
legend("topright", legend=c("train", "test"), pch=16, col=c("black", "red"), cex=1.3)
### saved as lsvm_sum.png (500x500)


##MLP
mlp = read.csv("out/mlp_summary.txt", sep=";", header=T, stringsAsFactors = F, comment.char="#")
#max
maxX = mlp$h[which(mlp$test==max(mlp$test))]
maxY = mlp$test[which(mlp$test==max(mlp$test))]
#plot
plot(mlp$h, mlp$train, pch=19, ylab="Fscore", xlab="hidden layer size", cex.axis=1.3, cex.lab=1.5)
points(mlp$h+1, mlp$test, pch=19, col="red")
rect(maxX-1.5,maxY-0.015, maxX+1.5,maxY+0.015, border="grey")
legend("bottomright", legend=c("train", "test"), pch=16, col=c("black", "red"), cex=1.3)
### saved as mlp_sum.png (500x500)

rm(maxX, maxY, labels)














