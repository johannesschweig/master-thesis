#CORRECTION

#PLANE
plane = read.csv(paste(getwd(), "/in/plane.txt", sep=""), sep=";", header=T, stringsAsFactors=F, comment.char="#")

##create corrected
### all cals, old gl
corrected = cleaned[cleaned$dpt!=0 & cleaned$g!=17 & cleaned$g!=18,]
### new cals, old gl
corrected = cleaned[cleaned$cal>13 & cleaned$dpt!=0 & cleaned$g!=17 & cleaned$g!=18,]

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


performance = function(){
  stats$meanCorrX = NA
  stats$meanCorrY = NA
  for(c in unique(corrected$cal)){
    for(gl in unique(corrected$g)){ #1,12:16
      stats$refX[stats$cal==c & stats$g==gl & stats$bl==gl] = st(c, 0, gl, "meanX")
      stats$refY[stats$cal==c & stats$g==gl & stats$bl==gl] = st(c, 0, gl, "meanY")
      stats$meanCorrX[stats$cal==c & stats$g==gl & stats$bl==gl] = mean(corrected$corrX[corrected$cal==c & corrected$g==gl])
      stats$meanCorrY[stats$cal==c & stats$g==gl & stats$bl==gl] = mean(corrected$corrY[corrected$cal==c & corrected$g==gl])
    }
  }
  ## error before correction: mean raw X/Y - refX/Y
  err_x_before = mean(abs(stats$meanX[stats$g %in% unique(corrected$g) & stats$cal %in% unique(corrected$cal)] - stats$refX[stats$g %in% unique(corrected$g) & stats$cal %in% unique(corrected$cal)]))
  print("before")
  print(round(c(err_x_before, vang(err_x_before)), 3))
  err_y_before = mean(abs(stats$meanY[stats$g %in% unique(corrected$g) & stats$cal %in% unique(corrected$cal)] - stats$refY[stats$g %in% unique(corrected$g) & stats$cal %in% unique(corrected$cal)]))
  print(round(c(err_y_before, vang(err_y_before)), 3))
  
  ## error after correction: meancorrected X/Y - refX/Y
  err_x_after = mean(abs(stats$meanCorrX[stats$g %in% unique(corrected$g) & stats$cal %in% unique(corrected$cal)] - stats$refX[stats$g %in% unique(corrected$g) & stats$cal %in% unique(corrected$cal)]))
  print("after")
  print(round(c(err_x_after, vang(err_x_after)), 3))
  err_y_after = mean(abs(stats$meanCorrY[stats$g %in% unique(corrected$g) & stats$cal %in% unique(corrected$cal)] - stats$refY[stats$g %in% unique(corrected$g) & stats$cal %in% unique(corrected$cal)]))
  print(round(c(err_y_after, vang(err_y_after)), 3))
  
  ##Nomenclature
  # [X] px °
  # [Y] px °
  
  rm(err_x_after, err_y_after)
}

#OUTPUT:
### all cals, old gl
# [1] "before"
# [1] 75.260  1.872
# [1] 103.586 2.577
##1. weighted correction plane
# [1] 53.364  1.327
# [1] 88.506  2.202
##1a. weighted correction plane
# [1] 40.988  1.020
# [1] 90.587  2.253
##1b. weighted correction plane
# [1] 40.973  1.019
# [1] 101.489 2.525
##2. specific correction plane
# [1] 51.653  1.285
# [1] 99.743  2.481

### new cals, old gl
# [1] "before"
# [1] 56.448  1.404
# [1] 50.178  1.248
##1. weighted correction plane
# [1] 34.675  0.863
# [1] 41.947  1.043
##1a. weighted correction plane
# [1] 34.452  0.857
# [1] 56.624  1.409
##1b. weighted correction plane
# [1] 47.563  1.183
# [1] 76.160  1.895
##2. specific correction plane
# [1] 25.636  0.638
# [1] 83.069  2.066
