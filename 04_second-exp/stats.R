# COMPUTE STATS
## NOTE: stats are computed with cleaned measures
## View number of observations per trial (clean)

stats <- data.frame(cal=rep(NA, 104), g=rep(NA, 104), bl=rep(NA, 104), meanX=rep(NA, 104), sdX=rep(NA, 104), meanY=rep(NA, 104), sdY=rep(NA, 104), obs=rep(NA, 104), stringsAsFactors=FALSE)

## Fill: obs, mean, sd
i = 0
for(cal in cal_points){ # 104 trials
  for(bl in baseline){
    i = i + 1
    
    stats[i,]$cal=cal
    stats[i,]$g=0
    stats[i,]$bl=bl
    
    stats[i,]$meanX = round(mean(df(cal,0,bl,"X", T)),3)
    stats[i,]$sdX = round(sd(df(cal,0,bl,"X", T)),3)
    stats[i,]$meanY = round(mean(df(cal,0,bl,"Y", T)),3)
    stats[i,]$sdY = round(sd(df(cal,0,bl,"Y", T)),3)
    stats[i,]$obs = nrow(df(cal,0, bl, T))
  }
  for(g in glasses){
    i = i + 1
    
    stats[i,]$cal=cal
    stats[i,]$g=g
    stats[i,]$bl=g
    
    stats[i,]$meanX = round(mean(df(cal,g,g, "X",T)),3)
    stats[i,]$sdX = round(sd(df(cal,g,g,"X",T)),3)
    stats[i,]$meanY = round(mean(df(cal,g,g,"Y",T)),3)
    stats[i,]$sdY = round(sd(df(cal,g,g,"Y",T)),3)
    stats[i,]$obs = nrow(df(cal,g, g, T))
  }
}
rm(cal,g,bl,i)

#ADD DISTANCE MEASURES
stats$dist = c(59,59,59,59,57,69,56,53,55,63,75,66,55,65,65,65,65,65,66,65,57,59,73,NA,78,56,62,61,61,61,61,61,61,54,55,66,80,70,54,60,60,60,61,60,61,61,52,54,65,67,66,52,59,59,60,60,60,60,60,51,52,62,76,67,52,62,62,62,62,62,62,62,53,55,67,72,69,53,58,58,59,58,58,58,58,51,53,64,72,65,54,62,62,62,62,62,62,62,53,57,64,70,70,54)

#ADD DPT MEASURE
library(car)
stats$dpt = recode(stats$g, "1=-3;2=-3;3=-3;4=-3;5=1;6=1;7=1;8=1;9=1;10=1;11=1;12=1;13=2;14=5;15=-1;16=-4;17=-2;18=3")

#REMOVE NA rows (dpt may still be NA)
sum(is.na(stats$meanX))
# no NA rows

## Write to file
export(stats, "stats.txt")

