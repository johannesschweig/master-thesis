#UTILS
#You can run this whole file without issues.

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

## export data.frame
export = function(object, file, saveRData=T){
  con <- file(paste(getwd(), "/in/", file, sep=""), open="wt")
  writeLines(paste("# created on", Sys.time()), con)
  write.table(object, con, sep=";", quote=F, row.names=F)
  close(con)
  if(saveRData)
    save.image(".RData") ## save new data.frame to .RData
}

## get statistics
st = function(cal, g, bl, par){
  return(stats[par][stats$cal==cal & stats$g==g & stats$bl==bl,])
}

## subset data
df = function(ca, ga, ba, par, clean=F){
  if(clean){
    return(data[par][data$cal==ca & data$g==ga & data$bl==ba & data$clean=="ok",])
  }else{
    return(data[par][data$cal==ca & data$g==ga & data$bl==ba,])
  }
}

## outlier plot
### view outlier plot for one combination of parameters
outlierPlot = function(ca, ga, ba, outlier=T, save=F){ # oulier: whether to show/hide outliers, save: save the plot to disk
  if(save){
    png(paste0(getwd(), "/img/outlier/out_", ca, "_", ga, "_", ba, ".png"),600,600)
  }
  if(outlier){
    sel = subset(data, cal==ca & g==ga & bl==ba & data$clean!="0")
  }else{
    sel = subset(data, cal==ca & g==ga & bl==ba & data$clean=="ok")
  }
  if(nrow(sel)!=0){ # if trial contains no valid data
    if(save){
      plot(sel$X, sel$Y, ylim=rev(range(sel$Y)), col = ifelse(sel$clean=="ok",'black','red'), xlab="X", ylab="Y", pch=20)
    }else{
      plot(sel$X, sel$Y, main=paste("cal:", ca, "g:", ga, "bl:", ba, sep=" "), ylim=rev(range(sel$Y)), col = ifelse(sel$clean=="ok",'black',ifelse(sel$clean=="outlier", "red", "darkred")), xlab="X", ylab="Y", pch=20)
    }
    legend("topright", legend=c("normal", "outlier", "moutlier"), pch=20, col=c("black", "red", "darkred"))
  }else{
    print(paste0(ca, ".", ga, ".", ba, " | no valid data found"))
  }
  if(save){
    dev.off()
  }
}

## visual angle
### turns px into degrees of visual angle
vang = function(px){
  return(px/40.2)
}

##get x/y of calibration points
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
                    c(1680 * 2/3, 1050 * 2/3),  # bottom right corner
                    c(1680 / 3, 1050 / 2), # new: center left
                    c(1680 * 2/3, 1050 / 2)) # new: center right
  
  if(par=="x"){
    return(cal_points[[cal]][1])
  }else if(par=="y"){
    return(cal_points[[cal]][2])
  }else{
    return(cal_points[[cal]])
  }
}

## offset plot
### view offset plot for one calibration point
offsetPlot = function(cal, save=F){
  library(plotrix)
  
  xlim = c(min(stats$meanX[stats$cal==cal & stats$g!=0], na.rm=T) - 30, max(stats$meanX[stats$cal==cal & stats$g!=0], na.rm=T) + 30)
  ylim = c(max(stats$meanY[stats$cal==cal & stats$g!=0], na.rm=T) + 30, min(stats$meanY[stats$cal==cal & stats$g!=0], na.rm=T) - 30) # reversed
  if(save){
    png(paste0(getwd(), "/img/offset/", cal, ".png"),600,600)
    plot(NULL, xlim=xlim, ylim=ylim, xlab="X", ylab="Y")
  }else{
    plot(NULL, xlim=xlim, ylim=ylim, xlab="X", ylab="Y", main=paste0("cal: ", cal))
  }
  
  glr = unique(stats$g[stats$cal==cal])# glasses available in this calibration point
  glr = glr[-which(glr==0)] # remove 0
  for(g in glr){
    draw.ellipse(st(cal,g,g, "meanX"), st(cal,g,g,"meanY"), a=st(cal,g,g, "sdX"), b=st(cal,g,g, "sdY"), border=gpar(g, "col")) # g ellipse
    arrows(st(cal,0,g,"meanX"), st(cal,0,g,"meanY"), st(cal,g,g,"meanX"), st(cal,g,g,"meanY"), length=0.05, lwd=gpar(g, "lwd"), lty=gpar(g, "lty"), col=gpar(g, "col")) # arrow
  }
  legend("topright", title="dpt", legend=sort(unique(stats$dpt))[-4], col=c(rep("blue", 3), rep("red", 3)), lty=c(1,1,2,2,1,1), lwd=c(2,1,1,1,1,2), cex=1)
  if(save){
    dev.off()
  }
}

##offsetPlot all
### view offset plot for all cals
offsetPlotAll = function(save=F){
  library(plotrix)
  
  xlim = c(min(stats$meanX[stats$g!=0], na.rm=T) - 30, max(stats$meanX[stats$g!=0], na.rm=T) + 30 + 50)
  ylim = c(max(stats$meanY[stats$g!=0], na.rm=T) + 30, min(stats$meanY[stats$g!=0], na.rm=T) - 30) # reversed
  if(save){
    png(paste0(getwd(), "/img/offset/offset-all-exp2.png"),1800,1200)
    par(mar=c(5,6,2,2))
  }
  plot(NULL, xlim=xlim, ylim=ylim, xlab="X", ylab="Y", cex.axis=2.3, cex.lab=3)
  
  glr = unique(stats$g)# glasses available
  glr = glr[-which(glr==0)] # remove 0
  for(cal in cal_points){
    for(g in glr){
      if(nrow(st(cal,g,g))!=0){ #check for no values
        draw.ellipse(st(cal,g,g, "meanX"), st(cal,g,g,"meanY"), a=st(cal,g,g, "sdX"), b=st(cal,g,g, "sdY"), border=gpar(g, "col")) # g ellipse
        arrows(st(cal,0,g,"meanX"), st(cal,0,g,"meanY"), st(cal,g,g,"meanX"), st(cal,g,g,"meanY"), length=0.05, lwd=gpar(g, "lwd"), lty=gpar(g, "lty"), col=gpar(g, "col")) # arrow
      }
    }
  }
  legend("topright", title="dpt", legend=sort(unique(stats$dpt))[-4], col=c(rep("blue", 3), rep("red", 3)), lty=c(1,1,2,2,1,1), lwd=c(2,1,1,1,1,2), cex=2.3)
  if(save){
    dev.off()
  }
}


## graphical parameters
### returns graphical parameters for glasses
gpar = function(g, par){
  comb = data.frame(
    g=c(0:18),
    dpt=c(0, rep(-3,4), rep(1, 8), 2, 5, -1, -4, -2, 3),
    col=c("darkgrey", rep("blue", 4), rep("red", 10), rep("blue", 3), "red"),
    lwd=c(rep(1,14), 2, 1, 2, 1, 2),
    lty=c(rep(1,5), rep(2,8), rep(1,2),2,1,1,1)
  )
  comb$col = as.character(comb$col)
  return(comb[par][comb$g==g,])
}