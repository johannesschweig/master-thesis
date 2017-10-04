#UTILS
#You can run this whole file without issues.

## plots a 3d scatterplot with a regression plane
scatterPlane = function (x, y, z, xlab="x", ylab="y", zlab="z"){
  #Scatterplot with regression plane
  ## fit reg
  fit = lm(z ~ x + y)
  print(paste("R^2:", summary(fit)$r.squared))
  coefs = coef(fit)
  print(coefs)
  ## plot 3d and plane
  plot3d(x, y, z, point.col = "blue", surface = F, xlab=xlab, ylab=ylab, zlab=zlab)
  planes3d(a=coefs["x"], b=coefs["y"], c=-1, d=coefs["(Intercept)"], alpha=.5)
  return(fit)
}

#nearest neighbor correction
## computs the corrected value based on the nearest offset
corrNear = function(x, y, dpt){
  # distance between the new point and the offseted points in the plane
  ## BEWARE OF MISSING CAL
  if(!(dpt %in% unique(plane$dpt))){
    stop(paste0("dpt ", dpt, " is not present in data"))
  }
  dist = apply(plane[plane$dpt==dpt,c("cal", "offsetX", "refX", "offsetY", "refY")], 1, function(i){
    return(c(i[1], abs(x-i[2]-i[3])+abs(y-i[4]-i[5])))
  })
  dist = unname(dist)
  # find minimum distance
  ca = dist[1,which.min(dist[2,])]
  resX = plane$offsetX[plane$dpt==dpt & plane$cal==ca]
  resY = plane$offsetY[plane$dpt==dpt & plane$cal==ca]
  
  return(c(x-resX, y-resY))
}


#Weighted mean correction
## computes the corrected value based on all reported offsets (usually cal: 13) with weighted mean (distance to x/y)
## pow: power of distance (larger: more impact of distance on weighting)
corrWeighted = function(x, y, dpt, pow=1){
  # res = (sum weight * value)/(sum weight)
  # weight: 1/dist
  # value: offset
  offsetX = plane$offsetX[plane$dpt==dpt]
  offsetY = plane$offsetY[plane$dpt==dpt]
  trials = length(offsetX)
  # dist = sqrt((x1-x2)^2 + (y1-y2)^2)
  dist = sqrt((rep(x,trials)-plane$refX[plane$dpt==dpt])^2 + (rep(y,trials)-plane$refY[plane$dpt==dpt])^2) ** pow
  resX = sum(offsetX*1/dist)/sum(1/dist)
  resY = sum(offsetY*1/dist)/sum(1/dist)
  # resX, resY: corrected X, Y values
  return(c(x-resX, y-resY))
}

## get statistics
st = function(cal, g, bl, par){
  return(stats[par][stats$cal==cal & stats$g==g & stats$bl==bl,])
}

## get statistics (nonparametric training statistics)
st_np = function(cal, g, bl, par){
  return(stats_nonp[par][stats_nonp$cal==cal & stats_nonp$g==g & stats_nonp$bl==bl,])
}

##get corrected
co = function(cal, g, bl, par){
  return(corrected[par][corrected$cal==cal & corrected$g==g & corrected$bl==bl,])
}

## get x/y of calibration points
cp = function(cal, par="both"){
  cal_points = list(c(1680 / 6, 1050 / 6),  # top left
                    c(1680 / 2, 1050 / 6),  # top mid
                    c(1680 * 5/6, 1050 / 6),  # top right
                    c(1680 / 6, 1050 / 2),  # center left
                    c(1680 / 2, 1050 / 2),  # center mid
                    c(1680 * 5/6, 1050 / 2),  # center right
                    c(1680 / 6, 1050 * 5/6),  # bottom left
                    c(1680 / 2, 1050 * 5/6),  # bottom mid
                    c(1680 * 5/6, 1050 * 5/6),  # bottom right
                    c(1680 / 3, 1050 / 3),  # top left corner
                    c(1680 * 2/3, 1050 / 3),  # top right corner
                    c(1680 / 3, 1050 * 2/3),  # bottom left corner
                    c(1680 * 2/3, 1050 * 2/3))  # bottom right corner
  
  if(par=="x"){
    return(cal_points[[cal]][1])
  }else if(par=="y"){
    return(cal_points[[cal]][2])
  }else{
    return(cal_points[[cal]])
  }
}

## returns subset of dataset
df = function(c, gl, b){
  return(subset(cleaned, cal==c & g==gl & bl==b))
}

## export data.frame
export = function(object, file, saveRData=T){
  con <- file(paste(getwd(), "/in/", file, sep=""), open="wt")
  writeLines(paste("# created on", Sys.time()), con)
  write.table(object, con, sep=";", quote=F, row.names=F)
  close(con)
  if(saveRData)
    save.image(".RData") ## save new data.frame to .RData
}

## visual angle
### turns px into degrees of visual angle
vang = function(px){
  return(px/40.2)
}