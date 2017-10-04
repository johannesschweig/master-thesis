##load models
load(file=paste0(getwd(), "/in/reg.mdl"))

##all cals
corrected = cleaned[cleaned$g!=0,]
##new cals
corrected = cleaned[cleaned$cal>13 & cleaned$g!=0,]

corrected$corrX = NULL
corrected$corrY = NULL
corrected$corrX = corrected$X - predict(regX2, newdata=corrected)
corrected$corrY = corrected$Y - predict(regY2, newdata=corrected)
performance()


### all cals
# [1] "before"
# [1] 69.427  1.727
# [1] 98.319  2.446
#regression d2
# [1] 37.429  0.931
# [1] 90.036  2.240
#regression d5full
# [1] 71.338  1.775
# [1] 126.119 3.137
#regression d9full
# [1] 464.261  11.549
# [1] 658.031  16.369
### new cals
# [1] "before"
# [1] 55.760  1.387
# [1] 54.202  1.348
#regression d2
# [1] 19.354  0.481
# [1] 64.015  1.592
#regression d5full
# [1] 50.685  1.261
# [1] 98.735  2.456
#regression d9full
# [1] 234.647   5.837
# [1] 274.590   6.831