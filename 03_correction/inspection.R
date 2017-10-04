#INSPECTION

# correction regression
## read summary
regX_sum = read.csv("out/regX_summary_r2.txt", sep=";", header=T, stringsAsFactors = F, comment.char="#")
regY_sum = read.csv("out/regY_summary_r2.txt", sep=";", header=T, stringsAsFactors = F, comment.char="#")
### number of features (incl. intercept) for polynomial degree 1 to 20
d = c(4, 10, 20, 35, 56, 84, 120, 165, 220, 286, 364, 455, 560, 680, 816, 969, 1140, 1330, 1540, 1771)
## plot data
par(mar=c(5,5,2,2))
plot(regX_sum$d, regX_sum$r, pch=0, xlab="degree", ylab=parse(text="R^2"), xaxt="n", cex.axis=1.3, cex.lab=1.5)
points(regY_sum$d, regY_sum$r, pch=1)
axis(1, at=seq(1,20,3), labels=seq(1,20,3), cex.axis=1.3)
legend("bottomright", legend=c("offsetX", "offsetY"), pch=c(0,1), cex=1.3)
### saved as poly_sum.png (500x500)

##relation fit-number of terms
###best ratio for deg2
round(regX_sum$r/d,3)
# [1] 0.011 0.052 0.032 0.021 0.015 0.011 0.008 0.006 0.004 0.003 0.003 0.002 0.002 0.001 0.001 0.001 0.001 0.001 0.001 0.001
round(regY_sum$r/d,3)
# [1] 0.032 0.044 0.025 0.017 0.012 0.009 0.007 0.006 0.004 0.003 0.003 0.002 0.002 0.001 0.001 0.001 0.001 0.001 0.001 0.001


## final regression
plot(cleaned$dpt, cleaned$offsetX)
abline(regX, col="red")
library("car")
library("rgl")
scatter3d(x=cleaned$dpt, y=cleaned$offsetX, z=cleaned$dpt*cleaned$X)

# correction plane
# plot data and corrected data
xl = c(0, max(cleaned$X[cleaned$dpt!=0]))
yl = c(max(cleaned$Y[cleaned$dpt!=0]), min(cleaned$Y[cleaned$dpt!=0]))
plot(NA, NA, xlim=xl, ylim=yl, xlab="X", ylab="Y")
## original data
points(cleaned$X[cleaned$dpt!=0], cleaned$Y[cleaned$dpt!=0], col="black")
## corrected data: 159943*2
# p = apply(cleaned[cleaned$dpt!=0,c("X", "Y", "dpt")], 1, function(i) corrWeighted(i[1], i[2], i[3]))
# points(p[1,], p[2,], col="green")

p = apply(cleaned[cleaned$dpt!=0,c("X", "Y", "dpt")], 1, function(i) corrNear(i[1], i[2], i[3]))
p = array(as.numeric(unlist(p)), dim=c(2,length(p)))
points(p[1,], p[2,], col="green")
## cross for baseline trial
apply(stats[stats$dpt==0,c("meanX", "meanY")], 1, function(i) lines(x=c(i[1]-10,i[1]+10), y=c(i[2],i[2]), col="grey", lwd=2))
apply(stats[stats$dpt==0,c("meanX", "meanY")], 1, function(i) lines(x=c(i[1],i[1]), y=c(i[2]-10,i[2]+10), col="grey", lwd=2))
rm(p,xl,yl)

# regression correction
library("rgl")
library("car")
scatter3d(x=corrected$dpt, y=corrected$offsetX, z=corrected$refX, point.col = "blue", fit="smooth", xlab = "dpt", ylab="offsetX", zlab="refX")
## saved as regX_smooth_ray.png
scatter3d(x=corrected$dpt, y=corrected$offsetX, z=corrected$refX, point.col = "blue", fit="quadratic", xlab = "dpt", ylab="offsetX", zlab="refX")
## saved as regX_quadratic.png

#Scatterplot with regression plane
regX = scatterPlane(corrected$dpt, corrected$refX, corrected$offsetX, "dpt", "refX", "offsetX")
# [1] "R^2: 0.00438837597586739"
regY = scatterPlane(corrected$dpt, corrected$refY, corrected$offsetY, "dpt", "refY", "offsetY")
# [1] "R^2: 0.149830852101334"





##offset ~ dpt
regX = lm(abs(plane$offsetX) ~ abs(plane$dpt))
summary(regX)
plot(abs(plane$dpt), abs(plane$offsetX))
abline(regX, col="red")
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)      35.481     14.743   2.407 0.018746 *  
#   abs(plane$dpt)   20.412      5.204   3.923 0.000202 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 61.12 on 70 degrees of freedom
# Multiple R-squared:  0.1802,	Adjusted R-squared:  0.1685 
# F-statistic: 15.39 on 1 and 70 DF,  p-value: 0.0002021

regY = lm(abs(plane$offsetY) ~ abs(plane$dpt))
summary(regY)
plot(abs(plane$dpt), abs(plane$offsetY))
abline(regY, col="red")
# Estimate Std. Error t value Pr(>|t|)  
# (Intercept)      45.798     21.900   2.091   0.0401 *
#   abs(plane$dpt)   16.660      7.729   2.155   0.0346 *
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 90.78 on 70 degrees of freedom
# Multiple R-squared:  0.06224,	Adjusted R-squared:  0.04884 
# F-statistic: 4.646 on 1 and 70 DF,  p-value: 0.03457

##offset ~ distance to eye tracker
regX = lm(abs(plane$offsetX) ~ plane$distX)
summary(regX)
plot(plane$distX, abs(plane$offsetX))
abline(regX, col="red")
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 41.76514   12.97085   3.220 0.001945 ** 
#   plane$distX  0.13058    0.03199   4.082 0.000117 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 60.66 on 70 degrees of freedom
# Multiple R-squared:  0.1923,	Adjusted R-squared:  0.1807 
# F-statistic: 16.66 on 1 and 70 DF,  p-value: 0.0001169

regY = lm(abs(plane$offsetY) ~ plane$distY)
summary(regY)
plot(plane$distY, abs(plane$offsetY))
abline(regY, col="red")

# Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 122.30798   24.42295   5.008 3.96e-06 ***
#   plane$distY  -0.06823    0.04227  -1.614    0.111    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 92.05 on 70 degrees of freedom
# Multiple R-squared:  0.03589,	Adjusted R-squared:  0.02212 
# F-statistic: 2.606 on 1 and 70 DF,  p-value: 0.111

summary(lm(abs(plane$offsetX) ~ plane$distX + plane$distY)) #distX sig., rest n.s.
summary(lm(abs(plane$offsetY) ~ plane$distX + plane$distY)) # n.s.
