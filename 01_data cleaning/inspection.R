# INSPECT CLEANED DATA
## import data
stats = read.csv(paste(getwd(), "/logs/stats.txt", sep=""), sep=";", header=T, stringsAsFactors=F, comment.char="#")
stats0 = read.csv(paste(getwd(), "/logs/stats_0cleaned.txt", sep=""), sep=";", header=T, stringsAsFactors=F, comment.char="#")

#DESCRIPTIVES
##n datasets
nrow(cleaned) #full
floor(nrow(cleaned)*.8) #train
nrow(cleaned) - floor(nrow(cleaned)*.8) # test
# [1] 386398
# [1] 309118
# [1] 77280
##clean
table(data$clean)
round(table(data$clean)/nrow(data),3)
#     0       ex      low moutlier       ok  outlier
# 27150   547405        3     5855   386398    60030
# 0.026    0.533    0.000    0.006    0.376    0.058
##sd
summary(stats$sdX)
summary(stats$sdY)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 1.387   2.536   3.332   4.584   5.020  47.540
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 1.992   4.598   6.645   9.550  10.560  61.320
vang(4.584)
vang(9.550)
# [1] 0.1140299
# [1] 0.2375622
##obs
summary(stats$obs)
sd(stats$obs)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 306    1910    2204    2159    2412    2952
# [1] 370.3086

#OFFSET MAGNITUDE
##are offsets larger at the edges of the screen?
dist = vang(sqrt(abs(1680/2-sapply(stats$cal[stats$g!=0], function(x) cp(x, "x")))**2 + abs(1050/2-sapply(stats$cal[stats$g!=0], function(x) cp(x, "y")))**2))
off = vang(sqrt(abs(stats$offsetX[stats$g!=0])**2 + abs(stats$offsetY[stats$g!=0])**2))
plot(dist, off, xlab="distance from cal in °", ylab="offset in °")
reg = lm(off~dist)
abline(reg, col="red")
summary(reg)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)   1.5733     0.7671   2.051   0.0439 *
#   dist          0.1689     0.0643   2.627   0.0105 *
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 2.681 on 73 degrees of freedom
# Multiple R-squared:  0.0864,	Adjusted R-squared:  0.07388
# F-statistic: 6.903 on 1 and 73 DF,  p-value: 0.01048
rm(dist, off, reg)

#ALL DATA
## reorder levels
cleaned$dpt = factor(cleaned$dpt, levels=c("-4.0", "-3.0", "-1.0", "0", "1.0", "2.0", "5.0"))
## glasses
library(ggplot2)
calp = data.frame(x=rep(NA, 15), y=rep(NA, 15), stringsAsFactors=F)
for(i in 1:15){
  calp$x[i] = cp(i, "x")
  calp$y[i] = cp(i, "y")
}
ggplot(cleaned[cleaned$g!=0,], aes(x=X, y=Y, col=dpt))  + scale_x_continuous(breaks=c(seq(0,1680,length.out=7)), minor_breaks=NULL) + scale_y_reverse(breaks=c(seq(0,1050,length.out=7)), minor_breaks=NULL) + theme_bw(base_size=24) + annotate("rect", xmin=0, xmax=1680, ymin=0, ymax=1050, fill="darkgrey", alpha=.3) + geom_point() + guides(colour=guide_legend(override.aes = list(size=6)))
  geom_point(data=calp, aes(x=x, y=y), colour="grey", shape=43, size=7) #calibration points in grey
## saved as glasses-overview.png (1700x1000)
## bl trials
cleaned$bl = as.factor(cleaned$bl)
ggplot(cleaned[cleaned$g==0,], aes(x=X, y=Y, col=bl)) + scale_x_continuous(breaks=c(seq(0,1680,length.out=7)), minor_breaks=NULL) + scale_y_reverse(breaks=c(seq(0,1050,length.out=7)), minor_breaks=NULL) + theme_bw(base_size=24) + annotate("rect", xmin=0, xmax=1680, ymin=0, ymax=1050, fill="darkgrey", alpha=.3) + geom_point() + scale_colour_brewer(palette="Dark2") + guides(colour=guide_legend(title="baseline", override.aes = list(size=6))) +
  geom_point(data=calp, aes(x=x, y=y), colour="grey", shape=43, size=7) #calibration points in grey
## saved as bl-overview.png (1700x1000)
rm(calp,i)

#BASELINE SHIFT
par="Y" # X or Y
col=rainbow(13)
plot(NULL, type="l", xlab="baseline", ylab=paste0("offset",par," in °"), xlim=c(1,19), ylim=c(-2,2), xaxt="n", yaxt="n")
axis(1, at=(1:17))
axis(2, at=seq(-2.1, 2.1, length.out=7))
abline(0, 0, col="darkgrey")
off = c()
for(c in 1:13){
  m = stats0[[paste0("mean", par)]][stats0$cal==c & stats0$g==0 & stats0$bl==1]
  x = vang(stats0[[paste0("mean", par)]][stats0$cal==c & stats0$g==0] - m)
  off = c(off, x)
  lines(1:17, x, col=col[c])
}
legend(17.4,2.1, legend=1:13, title="calPoint", col=col, pch=19, cex=0.8)
summary(off)
rm(c,m,x,col,par,off)
## saved as shiftX/Y.png (700x400)
##offsets baseline trials
# shift X
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
# -1.94386 -0.09587  0.00000  0.13712  0.14256  1.60930
# shift Y
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
# -2.12341 -0.14065  0.01871  0.16055  0.39679  2.05177
##precision baseline trials
summary(stats0$sdX[stats0$g==0])
summary(stats0$sdY[stats0$g==0])
vang(4.547)
vang(11.043)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 1.456   2.282   3.172   4.547   4.288 115.311
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 1.992   4.399   6.645  11.043  10.648 116.788
# [1] 0.1131095
# [1] 0.2747015
vang(mean(stats0$sdY[stats0$cal==12]))
# [1] 41.29694
# [1] 1.027287
vang(mean(stats0$sdX[stats0$cal==12]))
# [1] 11.78512
# [1] 0.2931622

##DISTANCE ILLUSTRATION
hist(stats$dist)
## distance ~ g(dpt)
dpt = c(0, rep(-3,4), rep(1,8), 2, 5, -1, -4)
plot(jitter(stats$g), stats$dist, xaxt="n", xlab="diopters in order of glasses", ylab="distance in cm")
axis(1, at=0:16, labels=dpt)
## saved as dist_dpt_g.png (700x500) in /distance/

## distance ~ dpt
reg = lm(stats$dist ~ stats$dpt)
summary(reg)
#             Estimate Std. Error t value Pr(>|t|)
# (Intercept)  60.3509     0.1723  350.33   <2e-16 ***
# stats$dpt    -3.1969     0.1107  -28.88   <2e-16 ***
plot(jitter(stats$dpt,1.2), stats$dist, xlab="diopter", ylab="distance in cm", xaxt="n", cex.axis=1.5, cex.lab=1.8)
axis(1, at=unique(stats$dpt), cex.axis=1.5)
abline(reg, col="red", lwd=2)
## saved as dist_dpt.png (700x500) in /distance/

## distance ~ dpt(EI, single vision)
reg = lm(stats$dist[stats$g %in% c(0,13:16)] ~ stats$dpt[stats$g %in% c(0,13:16)])
summary(reg)
#                                        Estimate Std. Error t value Pr(>|t|)
#   (Intercept)                          62.2743     0.1458   427.2   <2e-16 ***
#   stats$dpt[stats$g %in% c(0, 13:16)]  -2.9373     0.1125   -26.1   <2e-16 ***
plot(jitter(stats$dpt[stats$g %in% c(0,13:16)],1.2), stats$dist[stats$g %in% c(0,13:16)], xlab="diopters", ylab="distance in cm", xaxt="n")
axis(1, at=unique(stats$dpt[stats$g %in% c(0,13:16)]))
abline(reg, col="red")
## saved as dist_dpt_sv.png (700x500) in /distance/


#OFFSET ILLUSTRATION
table(stats$dpt, stats$g)
#      0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16
# -4   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0  13
# -3   0  13  13  13  13   0   0   0   0   0   0   0   0   0   0   0   0
# -1   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0  13   0
# 0  221   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
# 1    0   0   0   0   0  13  13  13  13  13  13  13  13   0   0   0   0
# 2    0   0   0   0   0   0   0   0   0   0   0   0   0  13   0   0   0
# 5    0   0   0   0   0   0   0   0   0   0   0   0   0   0  13   0   0
## save all offsetPlots
for(cal in 1:13){
  offsetPlot(cal, T)
}
## saved as x.png (600x600) in /offset/

offsetPlotAll(T)
## saved as offset-all/offset-all.png (1800x1200)

#SD ILLUSTRATION -> save
stats[which(vang(stats$sdX)>3),]
#     cal  g  bl    meanX     sdX   meanY     sdY  obs dist dpt
# 373  12 10 10  731.874 210.612 802.526 277.711 2333   51   1
# 400  13  4  4 1062.219 130.829 559.046 144.330 2562   72  -3
# 406  13 10 10  903.035 161.195 461.840 111.295 2885   51   1
stats[which(vang(stats$sdY)>3),]
#     cal  g  bl    meanX     sdX   meanY     sdY  obs dist dpt
# 373  12 10 10  731.874 210.612 802.526 277.711 2333   51   1
# 400  13  4  4 1062.219 130.829 559.046 144.330 2562   72  -3

plot(NULL, xlim=c(1, 16), ylim=c(0,3), xlab="glasses", ylab="sd in °", xaxt="n", cex.lab=1.5, cex.axis=1.2)
axis(1, at=1:16, cex.axis=1.2)
abline(0.5,0,col="grey")
for(g in 1:16){
  col = switch(g, "red", "black", "black", "black", "black", "black", "black", "black", "black", "black", "black", "red", "red", "red", "red", "red")

  ## plot sdX
  points(rep(g-0.1,13), stats0$sdXang[stats0$g==g], pch=0, col=col)
  ## plot sdY
  points(rep(g+0.1,13), stats0$sdYang[stats0$g==g], pch=1, col=col)
  ## plot >3
  lenX = sum(stats0$sdXang[stats0$g==g]>3, na.rm=T) ## outside plotting region
  lenY = sum(stats0$sdYang[stats0$g==g]>3, na.rm=T) ## outside plotting region
  if(lenX>0){
    points(rep(g-0.1,lenX), rep(3,lenX), pch="*")
  }
  if(lenY>0){
    points(rep(g+0.1,lenY), rep(3,lenY), pch="*")
  }
}
rect(14.5, 2.35, 17, 3.2, border="black")
legend(12.7, 3.2, legend=rep("",3), pch=c(0,1,42), bty="n", cex=1.3) ## 42 <= num("*")
legend(11, 3.2, legend=c("sdX", "sdY", ">3°"), bty="n", cex=1.3) ## 42 <= num("*")
## saved as glasses-sd.png (1000x700)

## 3-4 high sds, 1-2 acceptable sds
## 10, 9, 8 high sdX, 11, 10, 5, 9, 7 high sdY; 6, 12 acceptable sds
## saved as sdX1-4.png, sdY1-4.png, sdX5-12.png, sdY5-12.png (700x400)

## how many values > 0.5 does each pair of glasses have?
for(g in 1:16){
  print(paste0("g:", g," x", sum(stats0$sdXang[stats0$g==g]>0.5, na.rm=T), " y", sum(stats0$sdYang[stats0$g==g]>0.5, na.rm=T)))
}
# [1] "g:1 x0 y2" 2
# [1] "g:2 x2 y2" 4
# [1] "g:3 x5 y5" 10
# [1] "g:4 x3 y2" 5

# [1] "g:5 x0 y3" 3
# [1] "g:6 x0 y1" 1
# [1] "g:7 x0 y2" 2
# [1] "g:8 x1 y3" 4
# [1] "g:9 x1 y2" 3
# [1] "g:10 x4 y6" 10
# [1] "g:11 x0 y1" 1
# [1] "g:12 x0 y1" 1

# [1] "g:13 x1 y2" 3
# [1] "g:14 x2 y1" 3
# [1] "g:15 x0 y2" 2
# [1] "g:16 x3 y0" 3

## look at mean
for(g in 1:16){
  print(paste(g,"x", round(vang(mean(stats0$sdX[stats0$g==g], na.rm=T)),3), "y", round(vang(mean(stats0$sdY[stats0$g==g], na.rm=T)),3)))
}
# [1] "1 x 0.149 y 0.256"
# [1] "2 x 0.221 y 0.258"
# [1] "3 x 0.577 y 0.711"
# [1] "4 x 0.461 y 0.513"

# [1] "5 x 0.121 y 0.447"
# [1] "6 x 0.118 y 0.286"
# [1] "7 x 0.09 y 0.36"
# [1] "8 x 0.122 y 0.349"
# [1] "9 x 0.143 y 0.388"
# [1] "10 x 0.883 y 1.133"
# [1] "11 x 0.101 y 0.28"
# [1] "12 x 0.102 y 0.258"

# [1] "13 x 0.164 y 0.261"
# [1] "14 x 0.227 y 0.356"
# [1] "15 x 0.109 y 0.299"
# [1] "16 x 0.291 y 0.173"

## examine too high sds from chosen glasses 1, 12:16
for(gl in c(1,12:16)){
  sel = subset(stats, stats$g==gl, select=c(cal, bl, sdXang, sdYang))
  print(gl)
  print(sel[sel$sdXang > .5 | sel$sdYang > .5,])
}
# [1] 1
#     cal bl sdXang sdYang
# 199   7  1  0.389  1.166
# 364  12  1  0.277  0.588
# [1] 12
#     cal bl sdXang sdYang
# 210   7 12  0.093  0.538
# [1] 13
#     cal bl sdXang sdYang
# 178   6 13  0.154  0.658
# 277   9 13  1.183  0.580
# [1] 14
#     cal bl sdXang sdYang
# 47     2 14  0.104  0.558
# 212    7 14  0.591  0.470
# 245    8 14  0.580  0.172
# [1] 15
#     cal bl sdXang sdYang
# 180   6 15  0.075  1.255
# 213   7 15  0.308  1.040
# [1] 16
#     cal bl sdXang sdYang
# 214   7 16  0.733  0.135
# 280   9 16  0.603  0.165
# 412  13 16  0.741  0.238

# OUTLIERS
outlierPlot(2,14,14)

# SD BASELINE
stats[stats$dpt==0 & (vang(stats$sdX)>.5 | vang(stats$sdY)>.5),]
#     cal g bl    meanX     sdX   meanY     sdY  obs dist dpt offsetX offsetY
# 183   6 0  2 1429.099   5.158 527.865  20.761 2688   63   0      NA      NA
# 380  12 0  1  576.816   5.178 712.517  61.317 2277   62   0      NA      NA
# 381  12 0  2  577.983   2.109 763.365  29.301 2658   61   0      NA      NA
# 391  12 0 12  568.112   1.950 764.770  38.551 2691   61   0      NA      NA
# 392  12 0 13  567.694   1.914 774.685  30.763 2561   61   0      NA      NA
# 393  12 0 14  567.195   2.023 774.305  28.736 2579   61   0      NA      NA
# 394  12 0 15  566.543   1.943 774.438  28.733 2561   61   0      NA      NA
# 395  12 0 16  565.563   5.210 655.060  54.889 2620   61   0      NA      NA
# 396  12 0 17  567.029   1.923 776.260  26.635 2577   61   0      NA      NA
# 425  13 0 13 1051.487 113.959 592.706 113.908 2697   63   0      NA      NA

# SD-DIOPTER: precision loss for higher dpt
plot(abs(stats$dpt[stats$g!=0]), stats$sdX[stats$g!=0])
cor(stats$sdX[stats$g!=0], abs(stats$dpt[stats$g!=0]))
plot(abs(stats$dpt[stats$g!=0]), stats$sdY[stats$g!=0])
cor(stats$sdY[stats$g!=0], abs(stats$dpt[stats$g!=0]))
# [1] 0.2398133
# [1] 0.0332738
# dpt-offset-sd
for(dpt in c(-4,-3,-1,0,1,2,5)){
  # old calculation
  # off = vang(sqrt(mean(abs(stats$offsetX[stats$dpt==dpt]))**2 + mean(abs(stats$offsetY[stats$dpt==dpt]))**2))
  # sd = vang(sqrt(mean(stats$sdX[stats$dpt==dpt])**2 + mean(stats$sdY[stats$dpt==dpt])**2))
  off = vang(mean(sqrt(stats$offsetX[stats$dpt==dpt]**2 + stats$offsetY[stats$dpt==dpt]**2)))
  sd = vang(mean(sqrt(stats$sdX[stats$dpt==dpt]**2 + stats$sdY[stats$dpt==dpt]**2)))
  print(paste(dpt, round(off,2), round(sd,2)))
}
# [1] "-4 4.29 0.27"
# [1] "-3 2.48 0.23"
# [1] "-1 2.03 0.28"
# [1] "0 NA 0.26"
# [1] "1 2.91 0.28"
# [1] "2 3.33 0.33"
# [1] "5 6.22 0.46"
##correlation |offset|~|diopter|
cor.test(sqrt(stats$sdX**2+stats$sdY**2), abs(stats$dpt))
# [1] 0.125
# t = 1.6789, df = 177, p-value = 0.09494
## positive
cor.test(sqrt(stats$sdX[stats$dpt>0]**2+stats$sdY[stats$dpt>0]**2), abs(stats$dpt[stats$dpt>0]))
# [1] 0.306
# t = 1.8725, df = 34, p-value = 0.06976
## negative
cor.test(sqrt(stats$sdX[stats$dpt<0]**2+stats$sdY[stats$dpt<0]**2), abs(stats$dpt[stats$dpt<0]))
# mean(sqrt(stats$sdX[stats$dpt==-4]**2+stats$sdY[stats$dpt==-4]**2))
# mean(sqrt(stats$sdX[stats$dpt==-3]**2+stats$sdY[stats$dpt==-3]**2))
# mean(sqrt(stats$sdX[stats$dpt==-1]**2+stats$sdY[stats$dpt==-1]**2))
# [1] -0.038
# t = -0.22879, df = 37, p-value = 0.8203

# INDEPENDENCE
## independence of features
cleaned$dpt = as.numeric(as.character(cleaned$dpt))
round(cor(cleaned[c(7,8,9,10,4)]),3)
#           refX   refY offsetX offsetY    dpt
# refX     1.000 -0.034  -0.028  -0.096  0.031
# refY    -0.034  1.000   0.066  -0.112  0.033
# offsetX -0.028  0.066   1.000   0.240  0.050
# offsetY -0.096 -0.112   0.240   1.000 -0.302
# dpt      0.031  0.033   0.050  -0.302  1.000

for(i in 1:6){
  c = combn(7:10,2)
  f1 = c[1,i]# factor1
  f2 = c[2,i]
  nf1 = colnames(cleaned[f1]) # name f1
  nf2 = colnames(cleaned[f2]) # name f1
  png(paste0(getwd(), "/img/feat-cor/" , nf1, "-",nf2, ".png"),600,600)
  plot(cleaned[[f1]], cleaned[[f2]], xlab=nf1, ylab=nf2)
  abline(lm(cleaned[[f2]] ~ cleaned[[f1]]), col="red")
  dev.off()
}
rm(c,f1,f2,nf1,nf2)

# RELATION PREDICTORS-LABEL
for(f in 7:10){
  nf = colnames(cleaned[f])
  png(paste0(getwd(), "/img/feat-label/" , nf, "-dpt.png"),600,600)
  plot(cleaned[[f]], cleaned$dpt, xlab=nf, ylab="dpt")
  reg = lm(cleaned$dpt ~ cleaned[[f]])
  abline(reg, col="red")
  dev.off()
}
rm(f,nf,reg)

# absolute relation offsetX/Y-dpt
for(f in 9:10){
  nf = colnames(cleaned[f])
  png(paste0(getwd(), "/img/feat-label/abs(" , nf, ")-dpt.png"),600,600)
  plot(abs(cleaned[[f]]), cleaned$dpt, ylab="dpt", xlab=paste0("|", nf, "|"))
  abline(lm(cleaned$dpt ~ abs(cleaned[[f]])), col="red")
  dev.off()
}
rm(f,nf)
# [1] "offsetX"
# Coefficients:
#   (Intercept)  abs(cleaned[[f]])
# -0.022267          -0.001406
#
# [1] "offsetY"
# Coefficients:
#   (Intercept)  abs(cleaned[[f]])
# -0.323831           0.005543


# library(scatterplot3d)
# scatterplot3d(cleaned$offsetX, cleaned$offsetY, cleaned$cal, highlight.3d=T, xlab="offsetX", ylab="offsetY", zlab="cal")


