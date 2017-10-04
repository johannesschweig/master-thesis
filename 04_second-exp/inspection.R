#INSPECTION

#DESCRIPTIVES
summary(stats$sdX)
summary(stats$sdY)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.418   2.633   3.520   4.398   4.816  18.310 
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2.154   4.136   6.378   9.023  10.430  39.090
vang(4.398)
vang(9.023)
# [1] 0.109403
# [1] 0.2244527


#ALL DATA
## glasses
library(ggplot2)
ggplot(cleaned[cleaned$g!=0,], aes(x=X, y=Y, col=as.factor(dpt))) + geom_point() + scale_y_reverse()
## saved as glasses-overview.png (1700x1000)
## bl trials
ggplot(cleaned[cleaned$g==0,], aes(x=X, y=Y, col=as.factor(bl))) + geom_point() + scale_y_reverse()
## saved as bl-overview.png (1700x1000)
##exp1 vs exp2
cleaned$dpt = as.factor(cleaned$dpt)
st2 = read.csv(paste0(gsub("04_second-exp", "", getwd()), "01_data cleaning/logs/stats.txt"), sep=";", header=T, comment.char="#")
st2$dpt = as.factor(st2$dpt)
# ggplot(cleaned[cleaned$g!=0,], aes(x=X, y=Y, col=dpt)) + scale_x_continuous(breaks=c(seq(0,1680,length.out=7)), minor_breaks=NULL) + scale_y_reverse(breaks=c(seq(0,1050,length.out=7)), minor_breaks=NULL) + theme_bw() + annotate("rect", xmin=0, xmax=1680, ymin=0, ymax=1050, fill="darkgrey", alpha=.3)+ geom_point() + geom_point(data=cl2[cl2$cal %in% cal_points & cl2$g %in% glasses,], aes(col=dpt), alpha=.03)
stats$dpt = as.factor(stats$dpt)
calp = data.frame(x=rep(NA, 15), y=rep(NA, 15), stringsAsFactors=F)
for(i in 1:15){
  calp$x[i] = cp(i, "x")
  calp$y[i] = cp(i, "y")
}
ggplot(stats[stats$g!=0,], aes(x=meanX, y=meanY, col=dpt)) + scale_x_continuous(breaks=c(seq(0,1680,length.out=7)), minor_breaks=NULL) + scale_y_reverse(breaks=c(seq(0,1050,length.out=7)), minor_breaks=NULL) + theme_bw(base_size=24) + annotate("rect", xmin=0, xmax=1680, ymin=0, ymax=1050, fill="darkgrey", alpha=.3)+ geom_point(shape=16, size=5) +
  geom_point(data=st2[st2$cal %in% cal_points & st2$g %in% glasses,], mapping=aes(x=meanX, y=meanY, col=dpt), shape=17, size=5) + #old data as triangles
  geom_point(data=calp, aes(x=x, y=y), colour="grey", shape=43, size=7) + #calibration points in grey
  guides(colour=guide_legend(override.aes = list(size=6))) 
## saved as exp1-exp2.png (1700x1000)

#OFFSET ILLUSTRATION
table(stats$dpt, stats$g)
#     0 12 13 15 16 17 18
# -4  0  0  0  0  8  0  0
# -2  0  0  0  0  0  8  0
# -1  0  0  0  8  0  0  0
# 0  56  0  0  0  0  0  0
# 1   0  8  0  0  0  0  0
# 2   0  0  8  0  0  0  0
# 3   0  0  0  0  0  0  8
## save all offsetPlots
for(c in cal_points){
  offsetPlot(c, T)
}
## saved as x.png (600x600) in /offset/
offsetPlotAll(T)
## saved as offset/offset-all-exp2.png (1800x1200)

# INDEPENDENCE
## independence of featurescleaned$dpt = as.numeric(as.character(cleaned$dpt))
round(cor(cleaned[c(7,8,9,10,4)]),3)
#          refX   refY offsetX offsetY    dpt
# refX     1.000 -0.054   0.062   0.023  0.005
# refY    -0.054  1.000  -0.004   0.069 -0.013
# offsetX  0.062 -0.004   1.000   0.235  0.109
# offsetY  0.023  0.069   0.235   1.000 -0.118
# dpt      0.005 -0.013   0.109  -0.118  1.000



