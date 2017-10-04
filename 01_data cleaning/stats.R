# COMPUTE STATS
## NOTE: stats are computed with cleaned measures
## View number of observations per trial (clean)

stats <- data.frame(cal=rep(NA, 429), g=rep(NA, 429), bl=rep(NA, 429), meanX=rep(NA, 429), sdX=rep(NA, 429), meanY=rep(NA, 429), sdY=rep(NA, 429), obs=rep(NA, 429),stringsAsFactors=FALSE)

## Fill: obs, mean, sd
i = 0
for(cal in 1:13){ # 429 trials
  for(g in 1:16){
    i = i + 1
    
    stats[i,]$cal=cal
    stats[i,]$g=g
    stats[i,]$bl=g
    
    stats[i,]$meanX = round(mean(df(cal,g,g,"X",T)),3)
    stats[i,]$sdX = round(sd(df(cal,g,g,"X",T)),3)
    stats[i,]$meanY = round(mean(df(cal,g,g,"Y",T)),3)
    stats[i,]$sdY = round(sd(df(cal,g,g,"Y",T)),3)
    stats[i,]$obs = nrow(df(cal,g, g, clean=T))
  }
  for(bl in 1:17){
    i = i + 1
    
    stats[i,]$cal=cal
    stats[i,]$g=0
    stats[i,]$bl=bl
    
    stats[i,]$meanX = round(mean(df(cal,0,bl,"X",T)),3)
    stats[i,]$sdX = round(sd(df(cal,0,bl,"X",T)),3)
    stats[i,]$meanY = round(mean(df(cal,0,bl,"Y",T)),3)
    stats[i,]$sdY = round(sd(df(cal,0,bl,"Y",T)),3)
    stats[i,]$obs = nrow(df(cal,0, bl, clean=T))
  }
}

#ADD DISTANCE MEASURES
stats$dist = NA
dist = c(61,61,62,60,61,61,61,58,62,58,60,60,59,59,58,61,58,67,72,65,70,57,52,51,53,52,49,50,55,53,NA,67,74,63,62,63,62,62,63,64,62,62,63,63,63,64,64,63,63,62,69,67,69,69,54,57,52,54,53,51,55,54,56,60,67,76,62,62,62,62,62,62,62,62,62,62,63,62,62,62,62,62,62,70,67,73,70,54,54,50,55,51,52,54,54,56,NA,68,71,58,58,58,58,58,58,62,62,58,58,61,60,62,58,62,62,58,64,67,63,68,55,51,50,55,NA,48,50,51,52,NA,63,69,63,63,63,62,63,63,63,63,63,63,63,63,64,63,64,63,62,67,69,65,66,54,53,52,54,54,51,53,54,55,46,69,74,62,63,63,62,63,62,62,62,62,63,64,62,62,62,63,63,62,65,66,61,68,55,52,52,55,53,51,53,54,56,NA,60,71,62,62,60,60,60,60,60,60,60,60,60,60,59,59,59,60,60,67,64,69,67,51,53,50,52,51,NA,50,52,52,50,63,76,71,70,71,71,67,65,71,65,65,66,66,67,64,64,64,64,64,68,72,71,66,55,57,54,56,54,51,54,56,54,NA,66,75,63,63,63,62,62,63,63,63,63,63,62,63,63,62,63,63,62,70,70,72,69,55,53,51,54,53,53,52,54,55,45,69,71,62,62,61,62,62,62,62,62,62,62,62,62,62,62,62,62,62,68,68,71,76,54,52,52,55,54,51,53,53,55,NA,67,76,64,63,63,63,63,63,64,62,62,62,64,64,64,64,64,63,64,62,67,69,72,55,53,52,55,53,51,52,54,55,47,67,77,62,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,65,66,67,67,57,53,54,55,56,51,NA,56,56,48,64,72,64,64,64,64,63,65,63,63,66,65,65,63,63,66,66,66,66,66,70,68,72,55,53,52,55,53,51,53,54,55,45,68,76)
i = 0
for(cal in 1:13){
  for(bl in 1:17){
    i = i + 1
    stats$dist[stats$cal==cal & stats$g==0 & stats$bl==bl] = dist[i]
  }
  for(g in 1:16){
    i = i + 1
    stats$dist[stats$cal==cal & stats$g==g & stats$bl==g] = dist[i]
  }
}

#ADD DPT MEASURE
library(car)
stats$dpt = NA
stats$dpt = recode(stats$g, "1=-3;2=-3;3=-3;4=-3;5=1;6=1;7=1;8=1;9=1;10=1;11=1;12=1;13=2;14=5;15=-1;16=-4")

#COMPUTE sdX and sdY as visual angle in degrees
#stats$sdXang = round(vang(stats$sdX),3)
#stats$sdYang = round(vang(stats$sdY),3)

#REMOVE NA rows (dpt may still be NA)
stats = stats[!is.na(stats$meanX),]

#COMPUTE OFFSET VALUES
glr = unique(stats$g)[-which(unique(stats$g)==0)]# glasses available - 0
for(cal in 1:13){
  for(g in glr){ # 1 12 13 15 16 14
    if(nrow(st(cal,g,g))!=0){ #check for no values
      # offset => (X - refX, Y - refY)
      stats$offsetX[stats$cal==cal & stats$g==g] = st(cal,g,g,"meanX") - st(cal,0,g, "meanX")
      stats$offsetY[stats$cal==cal & stats$g==g] = st(cal,g,g,"meanY") - st(cal,0,g, "meanY")
    }
  }
}
rm(glr,cal,g)    
##cross-check is possible with offset-vales from cleaned -> looks legit
mean(dfc(1,1,1, "offsetX"))
st(1,1,1,"offsetX")
mean(dfc(13,1,1, "offsetY"))
st(13,1,1,"offsetY")

## Write to file
export("stats.txt", stats)

## stats0 cleaned
stats0 = read.csv(paste0(getwd(), "/logs/stats_0cleaned.txt"), header=T, stringsAsFactors=F, sep=";", comment.char="#")
