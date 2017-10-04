#CORRECTION

##load data
stats_nonp = read.csv(paste(getwd(), "/in/stats.txt", sep=""), sep=";", header=T, stringsAsFactors=F, comment.char="#")
cleaned = read.csv(paste(getwd(), "/in/cleaned.txt", sep=""), sep=";", header=T, stringsAsFactors=F, comment.char="#")


##create corrected
set.seed(123)
train <- sample.int(nrow(cleaned), floor(.8*nrow(cleaned)), replace = F)
corrected = cleaned[train,]


#STATS_NONP
stats_nonp = stats_nonp[,1:3]
stats_nonp$meanX = NA
stats_nonp$meanY = NA

for(i in 1:nrow(stats_nonp)){
  cal = stats_nonp[i,]$cal
  g = stats_nonp[i,]$g
  bl = stats_nonp[i,]$bl
  
  stats_nonp[i,]$meanX = round(mean(co(cal,g,bl, "X")),3)
  stats_nonp[i,]$meanY = round(mean(co(cal,g,bl, "Y")),3)
}
rm(cal,g,bl,i)

##position import because of ref-trials
corrected = corrected[corrected$dpt!=0,]

#PLANE
### features mean offsetX/Y and dpt per trial
r = nrow(stats_nonp[stats_nonp$g!=0,]) #75
rm(plane)
plane = data.frame(cal=rep(NA,r), refX=rep(NA,r), refY=rep(NA,r), dpt=rep(NA,r), offsetX=rep(NA,r), offsetY=rep(NA,r), stringsAsFactors = F)
i = 0
for(cal in 1:13){
  for(g in c(1,12:16)){
    if(nrow(st_np(cal,g,g))!=0){
      i = i + 1
      
      plane[i,]$cal = cal
      plane[i,]$refX = st_np(cal, 0, g, "meanX")
      plane[i,]$refY = st_np(cal, 0, g, "meanY")
      plane[i,]$dpt = switch(g, -3, -3, -3, -3, 1, 1, 1, 1, 1, 1, 1, 1, 2, 5, -1, -4)
      
      plane[i,]$offsetX = st_np(cal, g, g, "meanX") - st_np(cal, 0, g, "meanX")
      plane[i,]$offsetY = st_np(cal, g, g, "meanY") - st_np(cal, 0, g, "meanY")
    }
  }
}
plane = plane[order(plane$cal, plane$dpt),]
export(plane, "plane.txt")
rm(cal,g,i,r)




##1. weighted correction plane
p = apply(corrected[, c("X", "Y", "dpt")], 1, function(i) corrWeighted(i[1], i[2], i[3]))
corrected$corrX = round(p[1,],3)
corrected$corrY = round(p[2,],3)
performance()
rm(p)

##1a. weighted correction plane
p = apply(corrected[, c("X", "Y", "dpt")], 1, function(i) corrWeighted(i[1], i[2], i[3], pow=2))
corrected$corrX = round(p[1,],3)
corrected$corrY = round(p[2,],3)
performance()
rm(p)

##1b. weighted correction plane
p = apply(corrected[, c("X", "Y", "dpt")], 1, function(i) corrWeighted(i[1], i[2], i[3], pow=10))
corrected$corrX = round(p[1,],3)
corrected$corrY = round(p[2,],3)
performance()
rm(p)


##2. specific correction plane
corrected$corrX = NA
corrected$corrY = NA
p = apply(corrected[, c("X", "Y", "dpt")], 1, function(i) corrNear(i[1], i[2], i[3]))
corrected$corrX = round(p[1,],3)
corrected$corrY = round(p[2,],3)
performance()
rm(p)



# PERFORMANCE
## compute mean corrected
performance = function(){
  stats_nonp$meanCorrX = NA
  stats_nonp$meanCorrY = NA
  for(c in 1:13){
    for(gl in unique(corrected$g)){ #1,12:16
      stats_nonp$refX[stats_nonp$cal==c & stats_nonp$g==gl & stats_nonp$bl==gl] = st_np(c, 0, gl, "meanX")
      stats_nonp$refY[stats_nonp$cal==c & stats_nonp$g==gl & stats_nonp$bl==gl] = st_np(c, 0, gl, "meanY")
      stats_nonp$meanCorrX[stats_nonp$cal==c & stats_nonp$g==gl & stats_nonp$bl==gl] = mean(corrected$corrX[corrected$cal==c & corrected$g==gl])
      stats_nonp$meanCorrY[stats_nonp$cal==c & stats_nonp$g==gl & stats_nonp$bl==gl] = mean(corrected$corrY[corrected$cal==c & corrected$g==gl])
    }
  }
  ## error before correction: mean raw X/Y - refX/Y
  err_x_before = mean(abs(stats_nonp$meanX[stats_nonp$g!=0] - stats_nonp$refX[stats_nonp$g!=0]))
  print("before")
  print(round(c(err_x_before, vang(err_x_before)), 3))
  err_y_before = mean(abs(stats_nonp$meanY[stats_nonp$g!=0] - stats_nonp$refY[stats_nonp$g!=0]))
  print(round(c(err_y_before, vang(err_y_before)), 3))
  
  ## error after correction: meancorrected X/Y - refX/Y
  err_x_after = mean(abs(stats_nonp$meanCorrX[stats_nonp$g!=0] - stats_nonp$refX[stats_nonp$g!=0]))
  print("after")
  print(round(c(err_x_after, vang(err_x_after)), 3))
  err_y_after = mean(abs(stats_nonp$meanCorrY[stats_nonp$g!=0] - stats_nonp$refY[stats_nonp$g!=0]))
  print(round(c(err_y_after, vang(err_y_after)), 3))
  rm(err_x_after, err_y_after)
}

##Nomenclature
# [X] px °
# [Y] px °

##TRAIN
# [1] "before"
# [1] 85.152  2.118
# [1] 93.764  2.332
#1. weighted plane
# [1] 57.093  1.420
# [1] 63.159  1.571
#1a. weighted squared
# [1] 35.562  0.885
# [1] 46.083  1.146
#1b. weighted pow10
# [1] 21.552  0.536
# [1] 33.906  0.843
#2. specific plane
# [1] 0.140 0.003
# [1] 0.956 0.024
#3. regression
# [1] 61.318  1.525
# [1] 72.900  1.813
#3a. regression deg5full
# [1] 30.718  0.764
# [1] 44.552  1.108
#3b. regression (deg9full)
# [1] 1.087 0.027
# [1] 2.630 0.065


##TEST
# [1] "before"
# [1] 85.047  2.116
# [1] 93.854  2.335
#1. weighted plane
# [1] 56.962  1.417
# [1] 63.10  1.57
#1a. weighted correction plane
# [1] 35.413  0.881
# [1] 46.116  1.147
#1b. weighted correction plane
# [1] 21.757  0.541
# [1] 34.482  0.858
#2. specific correction plane
# [1] 0.577 0.014
# [1] 1.759 0.044
#3. regression
# [1] 61.201  1.522
# [1] 73.028  1.817
#3a. regression deg5full
# [1] 30.555  0.760
# [1] 44.711  1.112
#3b. regression (deg9full)
# [1] 1.136 0.028
# [1] 3.046 0.076