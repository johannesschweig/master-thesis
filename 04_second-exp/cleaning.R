# load data
data = read.csv(paste0(getwd(), "/logs/merged.txt"), sep=";", header=T)
cal_points = unique(data$cal)
glasses = unique(data$g)[2:length(unique(data$g))]
baseline = unique(data$bl)

# plot data
library(ggplot2)
## glasses
ggplot(data=subset(merged, g!=0), aes(x=X, y=Y, col=g)) + geom_point()
## bl
ggplot(data=subset(merged, g==0), aes(x=X, y=Y, col=bl)) + geom_point()

#CLEANING
## cleaning column
## ok=ok, 0=x or y is zero, outlier=observation is an outlier in the trial, low=observations belongs to trial with too few obs (<10)
data$clean = "ok"

## Clean all 0 in data
data$clean[data$X==0 | data$Y==0] = "0"

# Clean outliers (Warning: takes time!)
## Clean all trials with glasses
for(ca in cal_points){
  print(paste("cal:", ca, sep=" "))
  for(ga in glasses){
    ba = ga
    ulx = quantile(data$X[data$cal==ca & data$g==ga & data$bl==ba & data$clean=="ok"], probs=.75)[[1]] + 1.5*IQR(data$X[data$cal==ca & data$g==ga & data$bl==ba & data$clean=="ok"])
    llx = quantile(data$X[data$cal==ca & data$g==ga & data$bl==ba & data$clean=="ok"], probs=.25)[[1]] - 1.5*IQR(data$X[data$cal==ca & data$g==ga & data$bl==ba & data$clean=="ok"])
    uly = quantile(data$Y[data$cal==ca & data$g==ga & data$bl==ba & data$clean=="ok"], probs=.75)[[1]] + 1.5*IQR(data$Y[data$cal==ca & data$g==ga & data$bl==ba & data$clean=="ok"])
    lly = quantile(data$Y[data$cal==ca & data$g==ga & data$bl==ba & data$clean=="ok"], probs=.25)[[1]] - 1.5*IQR(data$Y[data$cal==ca & data$g==ga & data$bl==ba & data$clean=="ok"])
    
    data$clean[data$cal==ca & data$g==ga & data$bl==ba & data$clean=="ok" & data$X > ulx] = "outlier"
    data$clean[data$cal==ca & data$g==ga & data$bl==ba & data$clean=="ok" & data$X < llx] = "outlier"
    data$clean[data$cal==ca & data$g==ga & data$bl==ba & data$clean=="ok" & data$Y > uly] = "outlier"
    data$clean[data$cal==ca & data$g==ga & data$bl==ba & data$clean=="ok" & data$Y < lly] = "outlier"
  }
}
## Clean all trials with baseline
for(ca in cal_points){
  print(paste("cal:", ca, sep=" "))
  for(ba in baseline){
    ga = 0
    ulx = quantile(data$X[data$cal==ca & data$g==ga & data$bl==ba & data$clean=="ok"], probs=.75)[[1]] + 1.5*IQR(data$X[data$cal==ca & data$g==ga & data$bl==ba & data$clean=="ok"])
    llx = quantile(data$X[data$cal==ca & data$g==ga & data$bl==ba & data$clean=="ok"], probs=.25)[[1]] - 1.5*IQR(data$X[data$cal==ca & data$g==ga & data$bl==ba & data$clean=="ok"])
    uly = quantile(data$Y[data$cal==ca & data$g==ga & data$bl==ba & data$clean=="ok"], probs=.75)[[1]] + 1.5*IQR(data$Y[data$cal==ca & data$g==ga & data$bl==ba & data$clean=="ok"])
    lly = quantile(data$Y[data$cal==ca & data$g==ga & data$bl==ba & data$clean=="ok"], probs=.25)[[1]] - 1.5*IQR(data$Y[data$cal==ca & data$g==ga & data$bl==ba & data$clean=="ok"])
    
    data$clean[data$cal==ca & data$g==ga & data$bl==ba & data$clean=="ok" & data$X > ulx] = "outlier"
    data$clean[data$cal==ca & data$g==ga & data$bl==ba & data$clean=="ok" & data$X < llx] = "outlier"
    data$clean[data$cal==ca & data$g==ga & data$bl==ba & data$clean=="ok" & data$Y > uly] = "outlier"
    data$clean[data$cal==ca & data$g==ga & data$bl==ba & data$clean=="ok" & data$Y < lly] = "outlier"
  }
}
rm(llx, lly, ulx, uly, bl, cal, g)

# Save all outlier plots (Warning! takes a while)
for(ca in cal_points){
  for(ga in glasses){
    outlierPlot(ca,ga,ga,outlier=T, save=T)
  }
  for(ba in baseline){
    outlierPlot(ca,0,ba, outlier=T, save=T)
  }
}

# Clean trials with too few observations
min(stats$obs)
# [1] 989
# -> all trials over 50 obs

# Manual outlier cleaning: trials sdX/sdY > 0.5°
stats[which(vang(stats$sdX)>0.5),]
# 12.16.16
stats[which(vang(stats$sdY)>0.5),]
# 3.15.15, 3.17.17, 8.0.13, 8.0.16, 8.0.19, 8.13.13, 8.16.16, 12.15.15, 12.16.16, 12.17.17, 14.18.18

# 12.16.16 - dual clouds - done
outlierPlot(12,16,16)
sum(df(12,16,16, "X")>400)
# [1] 800
sum(df(12,16,16, "X")<400)
# [1] 2116
data$clean[data$X > 400 & data$cal==12 & data$g==16 & data$bl==16 & data$clean=="ok"] = "moutlier"
# 3.15.15 - dual clouds, close and noisy
outlierPlot(3,15,15)
sum(df(3,15,15, "X") > 1510)
# [1] 1730
sum(df(3,15,15, "X") <= 1510)
# [1] 1250
# 3.17.17 - big scatterplot
outlierPlot(3,17,17)
# 8.0.13 - big scatterplot
outlierPlot(8,0,13)
# 8.0.16 - big y spread
outlierPlot(8,0,16)
# 8.0.19 - big y spread
outlierPlot(8,0,19)
# 8.13.13 - some noise downwards - done
outlierPlot(8,13,13)
sum(df(8,13,13,"Y",T)>560)
data$clean[data$Y > 560 & data$cal==8 & data$g==13 & data$bl==13 & data$clean=="ok"] = "moutlier"
# 8.16.16 - big y spread
outlierPlot(8,16,16)
# 12.15.15 - big y spread
outlierPlot(12,15,15)
# 12.16.16 - already handled above
outlierPlot(12,16,16)
# 12.17.17 - big y spread
outlierPlot(12,17,17)
# 14.18.18 - big y spread
outlierPlot(14,18,18)

## Write new txt
export("cleaning.txt", data)


## Write new txt cleaned
cleaned = data[data$clean=="ok",]
cleaned$clean = NULL
cleaned$time = NULL

### calculate offset
cleaned$cal = as.numeric(as.character(cleaned$cal)) # ATTENTION: else apply returns weird stuff bc not all columns are numeric
cleaned$g = as.numeric(as.character(cleaned$g))
cleaned$bl = as.numeric(as.character(cleaned$bl))

### add reference point (mean of baseline)
#### input: cal, bl (g=0)
cleaned$refX = apply(cleaned[,c(1,3)], 1, function(x){st(x[1], 0, x[2], "meanX")})
cleaned$refY = apply(cleaned[,c(1,3)], 1, function(x){st(x[1], 0, x[2], "meanY")})

### check reference point
for(ca in cal_points){
  x = round(range(cleaned$refX[cleaned$cal==ca]))
  y = round(range(cleaned$refY[cleaned$cal==ca]))
  print(paste0(ca,": ", x[1],"<",cp(ca,"x"),"<",x[2]," | ", y[1],"<",cp(ca,"y"),"<", y[2]))
}
# [1] "1: 290<280<307 | 187<175<285"
# [1] "3: 1370<1400<1405 | 156<175<197"
# [1] "5: 827<840<842 | 528<525<552"
# [1] "8: 841<840<849 | 873<875<910"
# [1] "12: 545<560<564 | 700<700<735"
# [1] "13: 1112<1120<1143 | 696<700<714"
# [1] "14: 559<560<569 | 522<525<545"
# [1] "15: 1123<1120<1126 | 516<525<532"
## cal 1: y very offseted
stats[stats$cal==1 & stats$g==0,]


### add offset (X - refX, Y - refY)
cleaned$offsetX = apply(cleaned[,c(5,7)], 1, function(x){ x[1] - x[2]})
cleaned$offsetY = apply(cleaned[,c(6,8)], 1, function(x){ x[1] - x[2]})
cleaned$offsetX = as.numeric(round(cleaned$offsetX, 3))
cleaned$offsetY = as.numeric(round(cleaned$offsetY, 3))

### cross-check
hist(cleaned$offsetX)
hist(cleaned$offsetY)
range(cleaned$offsetX)
# [1] -368.245  216.853
range(cleaned$offsetY)
# [1] -594.709  240.320

export("cleaned.txt", cleaned, F)
rm(cleaned)