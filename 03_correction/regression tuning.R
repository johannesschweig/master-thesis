#exhaustive search
library("leaps")
##offsetX
str = "offsetX ~ X*Y*dpt+I(X^2)+I(Y^2)+I(dpt^2)"
reg = regsubsets(as.formula(str), data=cleaned, method="exhaustive", nbest=1, really.big=T)
summary(reg)
summary(reg)$adjr2
#          X   Y   dpt I(X^2) I(Y^2) I(dpt^2) X:Y X:dpt Y:dpt X:Y:dpt
# 1  ( 1 ) " " " " " " " "    " "    " "      " " "*"   " "   " "    
# 2  ( 1 ) " " " " "*" " "    " "    " "      " " "*"   " "   " "    
# 3  ( 1 ) " " " " "*" " "    "*"    " "      " " "*"   " "   " "    
# 4  ( 1 ) " " " " "*" " "    "*"    " "      "*" "*"   " "   " "    
# 5  ( 1 ) " " " " "*" "*"    "*"    " "      "*" "*"   " "   " "    
# 6  ( 1 ) " " " " "*" " "    "*"    " "      "*" "*"   "*"   "*"    
# 7  ( 1 ) "*" " " "*" " "    "*"    " "      "*" "*"   "*"   "*"    
# 8  ( 1 ) "*" " " "*" "*"    "*"    " "      "*" "*"   "*"   "*" 
# [1] 0.06628665 0.48072261 0.49546120 0.51041318 0.51639886 0.52189885 0.53290629 0.5329365
##offsetY
str = "offsetY ~ X*Y*dpt+I(X^2)+I(Y^2)+I(dpt^2)"
reg = regsubsets(as.formula(str), data=cleaned, method="exhaustive", nbest=1)
summary(reg)
summary(reg)$adjr2
rm(reg,str)
#          X   Y   dpt I(X^2) I(Y^2) I(dpt^2) X:Y X:dpt Y:dpt X:Y:dpt
# 1  ( 1 ) " " " " " " " "    " "    "*"      " " " "   " "   " "    
# 2  ( 1 ) " " " " " " " "    " "    "*"      " " " "   "*"   " "    
# 3  ( 1 ) " " " " "*" " "    " "    "*"      " " " "   "*"   " "    
# 4  ( 1 ) " " " " "*" " "    " "    "*"      " " "*"   "*"   " "    
# 5  ( 1 ) " " " " "*" "*"    " "    "*"      " " "*"   "*"   " "    
# 6  ( 1 ) " " "*" "*" " "    " "    "*"      "*" "*"   "*"   " "    
# 7  ( 1 ) " " "*" "*" "*"    " "    "*"      "*" "*"   "*"   " "    
# 8  ( 1 ) "*" " " "*" "*"    "*"    "*"      "*" "*"   "*"   " "    
# [1] 0.2044442 0.3758753 0.3989690 0.4229613 0.4338762 0.4357400 0.4362682 0.4368868

##final model
regX = lm(offsetX ~ dpt + X:dpt, data=cleaned)
summary(regX)$r.squared
# [1] 0.4807253

regY = lm(offsetY ~ I(dpt^2) + Y:dpt, data=cleaned)
summary(regY)$r.squared
# [1] 0.3758786