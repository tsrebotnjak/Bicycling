### Bicycle Analysis ###

# read in the data

path <- file.choose()
data <- read.csv(paste(path), header=T)

dim(data)
# [1] 458 129
colnames(data)

# Gender distribution

table(data$GENDER) # 1=female, 2=male
#1   2     3   4 
#244 210   1   2 

# remove "Other" and "PNS"

data1 <- data[data$GENDER !=3,]
data1 <- data1[data1$GENDER !=4,]

## bike use analysis

table(data1$BK_YR) # bike ridden in past year, 1=yes, 0=no
#0   1 
#133 321
tabl <- table(data1$GENDER, data1$BK_YR)
chisq.test(tabl)
# significant diff. b/w men and women (more men rode bike than women)

table(data1$BK_STATUS) # bike ownership
#0   1   2   3   4   5   6   7 
#48 244  11  26   7  18   2   6 
tabl <- table(data1$GENDER, data1$BK_STATUS)
chisq.test(tabl)
# not significant diff. b/w men and women

hist(data1$BKNUM) # average bike use days/week
t.test(data1$BKNUM[data1$GENDER==1], data1$BKNUM[data1$GENDER==2], alternative="two.sided")
# t = -3.6467, df = 411.39, p-value = 0.0002997
# significant diff. b/w men and women (men on average almost double the use of women)
mean(data1$BKNUM[data1$GENDER==1], na.rm=T) # women
#[1] 1.237377
mean(data1$BKNUM[data1$GENDER==2], na.rm=T) # men
#[1] 2.01781

hist(data1$BKNUM_WRK) # average bike use days/week for work
t.test(data1$BKNUM_WRK[data1$GENDER==1], data1$BKNUM_WRK[data1$GENDER==2], alternative="two.sided")
# t = -1.9447, df = 402.23, p-value = 0.05251
# almost significant diff. b/w men and women (men on average almost double the use of women)
mean(data1$BKNUM_WRK[data1$GENDER==1], na.rm=T) # women
#[1] 0.4477869
mean(data1$BKNUM_WRK[data1$GENDER==2], na.rm=T) # men
#[1] 0.7137143

hist(data1$BKNUM_RF) # average bike use days/week for recreation/fitness
t.test(data1$BKNUM_RF[data1$GENDER==1], data1$BKNUM_RF[data1$GENDER==2], alternative="two.sided")
# t = -3.4817, df = 387.78, p-value = 0.0005549
# strongly significant diff. b/w men and women (men on average almost double the use of women)
mean(data1$BKNUM_RF[data1$GENDER==1], na.rm=T) # women
#[1] 0.917377
mean(data1$BKNUM_RF[data1$GENDER==2], na.rm=T) # men
#[1] 1.565857

table(data1$ST_BK_LN) # rides on streets with bike lanes, 1=yes, 0=no
#0   1 
#240 214
tabl <- table(data1$GENDER, data1$ST_BK_LN)
chisq.test(tabl)
# significant diff. b/w men and women (more men rode bike on streets w. bike lanes than women)

table(data1$X0_BK_LN) # rides on streets without bike lanes, 1=yes, 0=no
#0   1 
#260 194 
tabl <- table(data1$GENDER, data1$X0_BK_LN)
chisq.test(tabl)
# significant diff. b/w men and women (more men rode bike on streets w/o bike lanes than women)

table(data1$BK_RTS) # rides on bike routes, 1=yes, 0=no
#0   1 
#382  72  
tabl <- table(data1$GENDER, data1$BK_RTS)
chisq.test(tabl)
# not significant diff. b/w men and women 

table(data1$SDWLK) # rides on sidewalk, 1=yes, 0=no
#0   1 
#367  87  
tabl <- table(data1$GENDER, data1$SDWLK)
chisq.test(tabl)
# not significant diff. b/w men and women 

levels(data1$WISH_BK_MORE)<-c(1,2,3,4) # recoding of Wishes to Bike More
data1$WISH_BK_MORE <- as.numeric(data1$WISH_BK_MORE)
data1$WISH_BK_MORE[data1$WISH_BK_MORE==1] <- NA
data1$WISH_BK_MORE[data1$WISH_BK_MORE==4] <- 2
table(data1$WISH_BK_MORE)

tabl <- table(data1$GENDER, data1$WISH_BK_MORE) # 2=no, 3=yes
chisq.test(tabl)
# not significant diff. b/w men and women

## Barriers

table(data1$BAR_A) # barrier: does not own bike, yes=1, 0=no
tabl <- table(data1$GENDER, data1$BAR_A)
chisq.test(tabl)
# significant diff. b/w men and women (nearly twice as many women report not owning bike)

table(data1$BAR_B) # barrier: weather, yes=1, 0=no
tabl <- table(data1$GENDER, data1$BAR_B)
chisq.test(tabl)
# no significant diff. b/w men and women 

table(data1$BAR_C) # barrier: safety concerns, yes=1, 0=no
tabl <- table(data1$GENDER, data1$BAR_C)
chisq.test(tabl)
# strong significant diff. b/w men and women (women far more likely to be concerned)

table(data1$BAR_D) # barrier: lack of bike routes, yes=1, 0=no
tabl <- table(data1$GENDER, data1$BAR_D)
chisq.test(tabl)
# no significant diff. b/w men and women

table(data1$BAR_E) # barrier: destination too far, yes=1, 0=no
tabl <- table(data1$GENDER, data1$BAR_E)
chisq.test(tabl)
# no significant diff. b/w men and women

table(data1$BAR_F) # barrier: family/job obligations require car, yes=1, 0=no
tabl <- table(data1$GENDER, data1$BAR_F)
chisq.test(tabl)
# no significant diff. b/w men and women

table(data1$BAR_G) # barrier: uncomfortable on road, yes=1, 0=no
tabl <- table(data1$GENDER, data1$BAR_G)
chisq.test(tabl)
#  significant diff. b/w men and women (almost twice as many women uncomfortable than men)

table(data1$BAR_H) # barrier: does not own bike, yes=1, 0=no
tabl <- table(data1$GENDER, data1$BAR_H)
chisq.test(tabl)
# no significant diff. b/w men and women

table(data1$BAR_I) # barrier: bike not functional, yes=1, 0=no
tabl <- table(data1$GENDER, data1$BAR_I)
chisq.test(tabl)
# no significant diff. b/w men and women

table(data1$BAR_J) # barrier: concerned about car pollution, yes=1, 0=no
tabl <- table(data1$GENDER, data1$BAR_J)
chisq.test(tabl)
# no significant diff. b/w men and women

table(data1$BAR_L) # barrier: lack of racks for bike parking, yes=1, 0=no
tabl <- table(data1$GENDER, data1$BAR_L)
chisq.test(tabl)
# no significant diff. b/w men and women

table(data1$BAR_M) # barrier: not confident in biking ability, yes=1, 0=no
tabl <- table(data1$GENDER, data1$BAR_M)
chisq.test(tabl)
# highly significant diff. b/w men and women (women 3-4 times as likely to not be confident)

table(data1$BAR_OTA) # barrier: other barrier, yes=1, 0=no
tabl <- table(data1$GENDER, data1$BAR_OTA)
chisq.test(tabl)
# no significant diff. b/w men and women

table(data1$BAR_TOT) # barrier: total # of barriers, yes=1, 0=no
tabl <- table(data1$GENDER, data1$BAR_TOT)
chisq.test(tabl)
# weakly significant diff. b/w men and women (women report somewhat more barriers than men)

## Improvements

table(data1$RATE_A) # 
tabl <- table(data1$GENDER, data1$RATE_A)
chisq.test(tabl)

table(data1$RATE_B) # 
tabl <- table(data1$GENDER, data1$RATE_B)
chisq.test(tabl)

table(data1$RATE_C) # repair of road conditions
tabl <- table(data1$GENDER, data1$RATE_C)
chisq.test(tabl)
# strongly significant diff. b/w men and women (women more concerned)

table(data1$RATE_D) # 
tabl <- table(data1$GENDER, data1$RATE_D)
chisq.test(tabl)

table(data1$RATE_E) # lighting on bike routs
tabl <- table(data1$GENDER, data1$RATE_E)
chisq.test(tabl)
# weakly significant diff. b/w men and women (women more concerned)

table(data1$RATE_F) # 
tabl <- table(data1$GENDER, data1$RATE_F)
chisq.test(tabl)

table(data1$RATE_G) # 
tabl <- table(data1$GENDER, data1$RATE_G)
chisq.test(tabl)

table(data1$RATE_H) # 
tabl <- table(data1$GENDER, data1$RATE_H)
chisq.test(tabl)

table(data1$RATE_OTA) # 
tabl <- table(data1$GENDER, data1$RATE_OTA)
chisq.test(tabl)

## Political Participation

table(data1$POL_A) # 
tabl <- table(data1$GENDER, data1$POL_A)
chisq.test(tabl)

table(data1$POL_B) # 
tabl <- table(data1$GENDER, data1$POL_B)
chisq.test(tabl)

table(data1$POL_C) # attending city hall meetings
tabl <- table(data1$GENDER, data1$POL_C)
chisq.test(tabl)
# strongly significant diff. b/w men and women (more men attend)

table(data1$POL_D) # 
tabl <- table(data1$GENDER, data1$POL_D)
chisq.test(tabl)

table(data1$POL_E) # contacting non-elected city hall staff member
tabl <- table(data1$GENDER, data1$POL_E)
chisq.test(tabl)
# marginally significant diff. b/w men and women (more men than women do)

table(data1$POL_F) # 
tabl <- table(data1$GENDER, data1$POL_F)
chisq.test(tabl)

table(data1$POL_G) # 
tabl <- table(data1$GENDER, data1$POL_G)
chisq.test(tabl)

table(data1$POL_H) # 
tabl <- table(data1$GENDER, data1$POL_H)
chisq.test(tabl)

table(data1$POL_OTA) # 
tabl <- table(data1$GENDER, data1$POL_OTA)
chisq.test(tabl)

## Barriers to Political Participation

table(data1$POL_BAR_A) # don't know how to get involved
tabl <- table(data1$GENDER, data1$POL_BAR_A)
chisq.test(tabl)
# strongly significant diff. b/w men and women (more women don't know)

table(data1$POL_BAR_B) # lack of interest
tabl <- table(data1$GENDER, data1$POL_BAR_B)
chisq.test(tabl)
# significant diff. b/w men and women (more men are not interested)

table(data1$POL_BAR_C) # 
tabl <- table(data1$GENDER, data1$POL_BAR_C)
chisq.test(tabl)

table(data1$POL_BAR_D) # 
tabl <- table(data1$GENDER, data1$POL_BAR_D)
chisq.test(tabl)

table(data1$POL_BAR_E) # 
tabl <- table(data1$GENDER, data1$POL_BAR_E)
chisq.test(tabl)

table(data1$POL_BAR_F) # 
tabl <- table(data1$GENDER, data1$POL_BAR_F)
chisq.test(tabl)

table(data1$POL_BAR_G) # insufficient knowledge of political situation
tabl <- table(data1$GENDER, data1$POL_BAR_G)
chisq.test(tabl)
# strongly significant diff. b/w men and women (far more women say they don't know)

table(data1$POL_BAR_H) # 
tabl <- table(data1$GENDER, data1$POL_BAR_H)
chisq.test(tabl)

table(data1$POL_BAR_I) # 
tabl <- table(data1$GENDER, data1$POL_BAR_J)
chisq.test(tabl)

table(data1$POL_BAR_J) # 
tabl <- table(data1$GENDER, data1$POL_BAR_J)
chisq.test(tabl)

table(data1$POL_BAR_OTA) # 
tabl <- table(data1$GENDER, data1$POL_BAR_OTA)
chisq.test(tabl)