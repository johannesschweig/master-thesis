#load data
cc = c("numeric", "factor", "factor", "factor", "factor", rep("numeric",10))
#data = read.csv(paste(getwd(), "/main_session_full/merged.txt", sep=""), sep=";", header=T, stringsAsFactors=F, colClasses = cc, comment.char="#")
data = read.csv(paste(getwd(), "/logs/cleaning.txt", sep=""), sep=";", header=T, stringsAsFactors=F, colClasses = c(cc, "factor"), comment.char="#")


#CORRECTION
## wrong baseline_count in cal 7&11
levels(data$baseline_count)[levels(data$baseline_count)==0] <- 1
## reorder factors
data$calPoint = factor(data$calPoint, levels = 1:13)
data$glasses_number = factor(data$glasses_number, levels = 0:16)
data$baseline_count = factor(data$baseline_count, levels = 1:17)
##reorder columns: calPoint to front
data = data[,c(1,5,2,3,4,6:16)]

#CLEANING
## cleaning column
## ok=ok, 0=x or y is zero, outlier=observation is an outlier in the trial, low=observations belongs to trial with too few obs (<10)
data$clean = "ok"

## Clean all 0 in data
data$clean[data$X==0 | data$Y==0] = "0"

# Clean outliers (Warning: takes time!)
## Clean all trials with glasses

for(cal in 1:13){
  print(paste("cal:", cal, sep=" "))
  for(g in 1:16){
    bl = g
    # cal=12
    # g =9
    # bl =9
    ulx = quantile(data$X[data$calPoint==cal & data$glasses_number==g & data$baseline_count==bl & data$clean=="ok"], probs=.75)[[1]] + 1.5*IQR(data$X[data$calPoint==cal & data$glasses_number==g & data$baseline_count==bl & data$clean=="ok"])
    llx = quantile(data$X[data$calPoint==cal & data$glasses_number==g & data$baseline_count==bl & data$clean=="ok"], probs=.25)[[1]] - 1.5*IQR(data$X[data$calPoint==cal & data$glasses_number==g & data$baseline_count==bl & data$clean=="ok"])
    uly = quantile(data$Y[data$calPoint==cal & data$glasses_number==g & data$baseline_count==bl & data$clean=="ok"], probs=.75)[[1]] + 1.5*IQR(data$Y[data$calPoint==cal & data$glasses_number==g & data$baseline_count==bl & data$clean=="ok"])
    lly = quantile(data$Y[data$calPoint==cal & data$glasses_number==g & data$baseline_count==bl & data$clean=="ok"], probs=.25)[[1]] - 1.5*IQR(data$Y[data$calPoint==cal & data$glasses_number==g & data$baseline_count==bl & data$clean=="ok"])
    
    data$clean[data$calPoint==cal & data$glasses_number==g & data$baseline_count==bl & data$clean=="ok" & data$X > ulx] = "outlier"
    data$clean[data$calPoint==cal & data$glasses_number==g & data$baseline_count==bl & data$clean=="ok" & data$X < llx] = "outlier"
    data$clean[data$calPoint==cal & data$glasses_number==g & data$baseline_count==bl & data$clean=="ok" & data$Y > uly] = "outlier"
    data$clean[data$calPoint==cal & data$glasses_number==g & data$baseline_count==bl & data$clean=="ok" & data$Y < lly] = "outlier"
  }
}
## Clean all trials with baseline
for(cal in 1:13){
  print(paste("cal:", cal, sep=" "))
  for(bl in 1:17){
    g = 0
    ulx = quantile(data$X[data$calPoint==cal & data$glasses_number==g & data$baseline_count==bl & data$clean=="ok"], probs=.75)[[1]] + 1.5*IQR(data$X[data$calPoint==cal & data$glasses_number==g & data$baseline_count==bl & data$clean=="ok"])
    llx = quantile(data$X[data$calPoint==cal & data$glasses_number==g & data$baseline_count==bl & data$clean=="ok"], probs=.25)[[1]] - 1.5*IQR(data$X[data$calPoint==cal & data$glasses_number==g & data$baseline_count==bl & data$clean=="ok"])
    uly = quantile(data$Y[data$calPoint==cal & data$glasses_number==g & data$baseline_count==bl & data$clean=="ok"], probs=.75)[[1]] + 1.5*IQR(data$Y[data$calPoint==cal & data$glasses_number==g & data$baseline_count==bl & data$clean=="ok"])
    lly = quantile(data$Y[data$calPoint==cal & data$glasses_number==g & data$baseline_count==bl & data$clean=="ok"], probs=.25)[[1]] - 1.5*IQR(data$Y[data$calPoint==cal & data$glasses_number==g & data$baseline_count==bl & data$clean=="ok"])
    
    data$clean[data$calPoint==cal & data$glasses_number==g & data$baseline_count==bl & data$clean=="ok" & data$X > ulx] = "outlier"
    data$clean[data$calPoint==cal & data$glasses_number==g & data$baseline_count==bl & data$clean=="ok" & data$X < llx] = "outlier"
    data$clean[data$calPoint==cal & data$glasses_number==g & data$baseline_count==bl & data$clean=="ok" & data$Y > uly] = "outlier"
    data$clean[data$calPoint==cal & data$glasses_number==g & data$baseline_count==bl & data$clean=="ok" & data$Y < lly] = "outlier"
  }
}

# Clean trials with too few observations
stats[stats$obs < 50,]
# cal  g bl obs
# 14    1 14 14   1
# 113   4 14 14   0
# 179   6 14 14   2
data$clean[data$calPoint==1 & data$glasses_number==14 & data$baseline_count==14 & data$clean=="ok"] = "low"
data$clean[data$calPoint==6 & data$glasses_number==14 & data$baseline_count==14 & data$clean=="ok"] = "low"
table(data$clean)

# Exclude obs. from glasses that were not chosen
#data$clean = as.character(data$clean)
#data$clean = as.factor(data$clean)
data$clean[data$glasses_number %in% c(2:11) & data$clean=="ok"] = "ex"

# Exclude obs. from baseline_count that is not relevant anymore
data$clean[data$baseline_count %in% c(3:11) & data$clean=="ok"] = "ex"

# Manual outlier cleaning
library(ggplot2)

## baseline trials
## 12.0.1 - dense scatterplot, nothing to do
outlierPlot(12,0,1,F)
## 12.0.2 - dense scatterplot, nothing to do
outlierPlot(12,0,2,F)
## 12.0.13 - dense scatterplot, nothing to do
outlierPlot(12,0,13,F)
## 12.0.14 - dense scatterplot, nothing to do
outlierPlot(12,0,14,F)
## 12.0.15 - dense scatterplot, nothing to do
outlierPlot(12,0,15,F)
## 12.0.16 - dense scatterplot, nothing to do
outlierPlot(12,0,16,F)
## 12.0.17 - dense scatterplot, nothing to do
outlierPlot(12,0,1,F)
## 13.0.13 - dual centres, cleaned
outlierPlot(13,0,13)
st(13,0,13, "obs")
# [1] 2697
sum(df(13,0,13, "X")>1000)
# [1] 1916
sum(df(13,0,13, "X")<1000)
# [1] 781
data$clean[data$X < 1000 & data$calPoint==13 & data$glasses_number==0 & data$baseline_count==13 & data$clean=="ok"] = "moutlier"

## glasses trials
## 7.1.1 - sdY - done
outlierPlot(7,1,1)
sum(df(7,1,1,T)$Y > 1000)
# [1] 792
sum(df(7,1,1,T)$Y < 1000)
# [1] 1839
abline(1000,0)
data$clean[data$Y > 1000 & data$calPoint==7 & data$glasses_number==1 & data$baseline_count==1 & data$clean=="ok"] = "moutlier"

## 12.1.1 - sdY - weird form, not differentiable
outlierPlot(12,1,1, F)
ggplot(data[data$calPoint==12 & data$glasses_number==1 & data$baseline_count==1 & data$clean=="ok",], aes(x=X, y=Y)) + geom_density2d()

## 7.12.12 - sdY - comment: "noise rechts" - kinda noisy on one side...
outlierPlot(7,12,12, F)
ggplot(data[data$calPoint==7 & data$glasses_number==12 & data$baseline_count==12 & data$clean=="ok",], aes(x=X, y=Y)) + geom_density2d()

## 6.13.13 - sdY - dense scatterplot, nothing to do
outlierPlot(6,13,13)

## 9.13.13 - sdX+Y - dual centers, n and sd comparable
outlierPlot(9,13,13)
sum(df(9,13,13,T)$X < 1225)
sum(df(9,13,13,T)$X > 1225)
sd(data$X[data$calPoint==9 & data$glasses_number==13 & data$baseline_count==13 & data$clean=="ok" & data$X < 1225])
sd(data$X[data$calPoint==9 & data$glasses_number==13 & data$baseline_count==13 & data$clean=="ok" & data$X > 1225])

## 2.14.14 - sdY - comment: rechts(on/off) Horror - scatterplot with some noise around
outlierPlot(2,14,14)

## 7.14.14 - sdX - comment: rechtes Auge erkannt - scatterplot with noise on the left
outlierPlot(7,14,14)

## 8.14.14 - sdX - very few obs and very scattered
outlierPlot(8,14,14)
st(8,14,14,"obs")
# [1] 634
st(7,14,14,"obs")

## 6.15.15 - sdY - dense scatterplot
outlierPlot(6,15,15,F)

## 7.15.15 - sdY - comment: obere Wolke = L+R, untere = L - done
outlierPlot(7,15,15,T)
abline(975,0)
sum(df(7,15,15,T)$Y < 975)
# [1] 1087
sum(df(7,15,15,T)$Y > 975)
# [1] 1801
data$clean[data$Y > 975 & data$calPoint==7 & data$glasses_number==15 & data$baseline_count==15 & data$clean=="ok"] = "moutlier"

## 7.16.16 - sdX - scatterplot with some noise on the right - done
outlierPlot(7,16,16)
lines(x=c(80,80), y=c(910,860))
sum(df(7,16,16,T)$X < 80)
# [1] 2146
sum(df(7,16,16,T)$X > 80)
# [1] 760
data$clean[data$X > 80 & data$calPoint==7 & data$glasses_number==16 & data$baseline_count==16 & data$clean=="ok"] = "moutlier"

## 9.16.16 - sdX - comment: EK links - done
outlierPlot(9,16,16)
lines(x=c(1560,1560), y=c(2000,1000))
lines(x=c(1510,1510), y=c(2000,1000))
sum(df(9,16,16,T)$X < 1510)
# [1] 1
sum(df(9,16,16,T)$X < 1560)
# [1] 1819
sum(df(9,16,16,T)$X > 1560)
# [1] 886
data$clean[data$X > 1560 & data$calPoint==9 & data$glasses_number==16 & data$baseline_count==16 & data$clean=="ok"] = "moutlier"
data$clean[data$X < 1510 & data$calPoint==9 & data$glasses_number==16 & data$baseline_count==16 & data$clean=="ok"] = "moutlier"

## 13.16.16 - sdX - dual centers - done
outlierPlot(13,16,16)
lines(x=c(925,925), y=c(0,1000))
sum(df(13,16,16,T)$X < 925)
# [1] 834
sum(df(13,16,16,T)$X > 925)
# [1] 2045
data$clean[data$X < 925 & data$calPoint==13 & data$glasses_number==16 & data$baseline_count==16 & data$clean=="ok"] = "moutlier"

## Write new txt
export("cleaning.txt", data)


## Write new txt cleaned
cleaned = data[data$clean=="ok",]
cleaned$time = NULL
cleaned$x_L = NULL
cleaned$y_L = NULL
cleaned$x_R = NULL
cleaned$y_R = NULL
cleaned$x_B = NULL
cleaned$y_B = NULL
cleaned$clean = NULL
### change colnames
colnames(cleaned) = c("cal", "g", "bl", "dpt", "calX", "calY", "X", "Y")

### calculate offset
cleaned$cal = as.numeric(as.character(cleaned$cal)) # ATTENTION: else apply returns weird stuff bc not all columns are numeric
cleaned$g = as.numeric(as.character(cleaned$g))
cleaned$bl = as.numeric(as.character(cleaned$bl))

### add reference point (mean of baseline)
cleaned$refX = 0
cleaned$refY = 0
cleaned$refX = apply(cleaned[,c(1,3)], 1, function(x){st(x[1], 0, x[2], "meanX")})
cleaned$refY = apply(cleaned[,c(1,3)], 1, function(x){st(x[1], 0, x[2], "meanY")})

### cross-check -100 - +100 ok
max(cleaned$refX - cleaned$calX)
min(cleaned$refX - cleaned$calX)
max(cleaned$refY - cleaned$calY)
min(cleaned$refY - cleaned$calY)

### add offset (X - refX, Y - refY)
cleaned$offsetX = apply(cleaned[,c(7,9)], 1, function(x){ x[1] - x[2]})
cleaned$offsetY = apply(cleaned[,c(8,10)], 1, function(x){ x[1] - x[2]})
cleaned$offsetX = as.numeric(round(cleaned$offsetX, 3))
cleaned$offsetY = as.numeric(round(cleaned$offsetY, 3))

### cross-check
max(cleaned$offsetX)
# which(cleaned$offsetX > 300)
# cleaned[194272,]
# 7/14/14 ok
min(cleaned$offsetX)
max(cleaned$offsetY)
min(cleaned$offsetY)
# which(cleaned$offsetY < -500)
# cleaned[224012,]
# 8/14/14 ok

### remove calX, calY
cleaned$calX = NULL
cleaned$calY = NULL

export("cleaned.txt", cleaned, F)
rm(cleaned)

# Save all outlier plots (Warning! takes a while)
for(cal in 1:13){
  for(g in 1:16){
    outlierPlot(cal,g,g,T,T)
  }
  for(bl in 1:17){
    outlierPlot(cal,0,bl,T,T)
  }
}
rm(bl,cal,g)

## check dual trials
b = data[502721:504897,]
c = data[504898:507345,]
plot(NULL, xlim=c(200,400), ylim=c(50,400))
plot(b$X, b$Y)
plot(c$X, c$Y)
points(b$X, b$Y, col="green")
points(c$X, c$Y, col="red")

## check for invalid jumps in time slot
last = 0
for(i in 1:nrow(data)){
  now = data$time[i]
  if(abs(now - last) > 1 & now > 0.1){
    print(paste(now, i, sep="-"))
  }
  last = now
}
## NONE :)

## how many trials after cleaning
nrow(stats[stats$g!=0,])
# [1] 75
nrow(stats[stats$g==0,])
# [1] 104