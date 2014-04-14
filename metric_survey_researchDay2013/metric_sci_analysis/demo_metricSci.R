#2014 Feb 2, teaching R using the metric sci survey dataset. 

rm(list=ls())
tb = read.csv("scored_survey_data20130411.csv")

# The survey questions have 3 types: scientific literacy, attitude toward science, and metric proficiency
# We will sum up the score of teach 

sciLiteracy = c("light", "fossil", "food", "electronCharge", 
                "earlyHuman", "laser", "continents", "antibiotics", "electronSize", "earthCenter")
tb$SciLitScore = apply( tb[, sciLiteracy], MARGIN=1, FUN=sum ) #by row
hist(tb2$SciLitScore, br=20)

sciAttitude = c("religiousView", "dailyLife", "SciOnLife", "SciEffect")
#Attitude total score
tb$SciAttitude = apply( tb[, sciAttitude], MARGIN=1, FUN=sum)
hist(tb2$SciAttitude, br=20)

metrics = c("shaq", "kilo", "mm", "inseam", "weather")
#metric total score
tb$metric = apply( tb[, metrics], MARGIN=1, FUN=sum )
hist(tb$metric, br=20)

summary(tb)
str(tb); 

pairs(tb[, c("metric", "SciLitScore", "SciAttitude")])
summary(lm(tb$SciLitScore ~ tb$metric )) #significant
summary(lm(tb$SciAttitude ~ tb$metric )) #significant
summary(lm(tb$SciAttitude ~ tb$SciLitScore + tb$metric )) #significant
## metric -> SciAttitude and SciLitScore

summary(lm(tb$SciLitScore ~ tb$country)) #not significant
summary(lm(tb$SciLitScore ~ tb$metric + tb$age + tb$gender + tb$country  )) #only metric is significant


plot( tb2$SciLitScore ~ jitter(tb2$metric), xlab='Metric Proficiency', ylab='Scientific Literacy', ylim=c(2,10) )
m1 = lm(tb2$SciLitScore ~ tb2$metric )
abline(m1, col='red')
summary(m1)
text(2, 2.5, "SciLit ~ Metric, R2=0.28, p=1.5E-15", col="red", pos=4)
#abline(m2, col='blue')
summary(m2)
m2 = lm(tb2$SciLitScore ~ tb2$metric + tb2$age)
anova(m1, m2)
m3 = lm(tb2$SciLitScore ~ tb2$metric + tb2$age + tb2$gender)
summary(m3)
anova(m2,m3)
m4 = lm(tb2$SciLitScore ~ tb2$metric + tb2$age + tb2$country)
anova(m2, m4)
#text(2, 2, "SciLit ~ Metric + Age, R2=0.29, p=2.8E-14", col="blue", pos=4)

plot( tb2$SciAttitude ~ jitter(tb2$metric), ylim=c(0.5,4), xlab='Metric Proficiency', ylab='Attitude toward Science' )
m1 = lm( tb2$SciAttitude ~ tb2$metric )
m2 = lm( tb2$SciAttitude ~ tb2$metric + tb2$age )
abline(m1, col='red')
abline(m2, col='blue')
summary(m1)
summary(m2)
anova(m1, m2)
text(2, 0.9, "SciAttitude ~ Metric , R2=0.18, p=1.0E-9", col="red", pos=4)
text(2, 0.7, "SciAttitude ~ Metric + Age, R2=0.24, p=4.7E-12", col="blue", pos=4)

plot( tb2$SciAttitude ~ jitter(tb2$age), ylab='Attitude toward Science', xlab='Age')
m2 = lm( tb2$SciAttitude ~ tb2$age + tb2$metric)
abline(m2, col='blue')
text(30, 1.7, "SciAttitude ~ Metric + Age, R2=0.24, p=4.7E-12", col="blue", pos=4)

summary(lm(tb2$SciAttitude ~ tb2$metric + tb2$age + tb2$gender + tb2$country  )) #age is signicant!!!
#but this might be a bias in the sample
# 1) there is many faculty
# 2) people took the sample may be interested in the metric and science at the first place?!

summary(lm(tb2$SciAttitude ~ tb2$metric + tb2$age + tb2$gender + tb2$country + tb2$degree  )) #age is signicant!!!

summary(lm(tb2$SciAttitude ~ tb2$SciLitScore))
summary(lm(tb2$SciAttitude ~ tb2$SciLitScore + tb2$metric))
###########
# remove phD from the samples
#
summary(tb2[, 1:5])
tb3 = tb2[ - grep('Ph.D.', tb2$degree)  , ]
summary(tb3)
summary(lm(tb3$SciAttitude ~ tb3$metric + tb3$age + tb3$gender + tb3$country + tb3$degree  )) 
#age is still signicant after PhD are removed from the sample


########test 
testTwoFactorTb2 = function( fac1, fac2) {
  tbTwo = table( tb2[,fac1], tb2[,fac2] )
  print(tbTwo)
  f = fisher.test(tbTwo)
}

#metrics = c("shaq", "kilo", "mm", "inseam", "weather")
#sciLiteracy = c("light", "fossil", "food", "electronCharge", 
#                "earlyHuman", "laser", "continents", "antibiotics", "electronSize", "earthCenter")
#sciAttitude = c("religiousView", "dailyLife", "SciOnLife", "SciEffect")

f = testTwoFactorTb2( "shaq", "religiousView"); f
f = testTwoFactorTb2( "shaq", "dailyLife"); f
f = testTwoFactorTb2( "shaq", "SciOnLife"); f

f = testTwoFactorTb2( "shaq", "SciEffect"); f #significant effect!!!!
f = testTwoFactorTb2( "kilo", "SciEffect"); f #significant effect!!!
f = testTwoFactorTb2( "mm", "SciEffect"); f #significant effect!!!
f = testTwoFactorTb2( "inseam", "SciEffect"); f #significant effect!!!
f = testTwoFactorTb2( "weather", "SciEffect"); f #p=0.078
f = testTwoFactorTb2( "country", "SciEffect"); f #p=0.24

summary(lm(tb2$SciEffect ~ tb2$kilo + tb2$country + tb2$gender + tb2$age + tb2$degree )) #significant kilo 
summary(lm(tb2$SciOnLife ~ tb2$kilo + tb2$country + tb2$gender + tb2$age + tb2$degree )) #no effect
summary(lm(tb2$religiousView ~ tb2$kilo + tb2$country + tb2$gender + tb2$age + tb2$degree )) #age effect
summary(lm(tb2$dailyLife ~ tb2$kilo + tb2$country + tb2$gender + tb2$age + tb2$degree )) #gender

summary(lm(tb2$religiousView ~ tb2$kilo + tb2$country + tb2$gender + tb2$age + tb2$degree )) #significant age, gender 
summary(lm(tb2$SciOnLife ~ tb2$kilo + tb2$country + tb2$gender + tb2$age + tb2$degree )) #no effect
summary(lm(tb2$dailyLife ~ tb2$kilo + tb2$country + tb2$gender + tb2$age + tb2$degree )) #gender effect, education

summary(lm(tb2$SciEffect ~ tb2$mm + tb2$country + tb2$gender + tb2$age + tb2$degree )) #no effect
summary(lm(tb2$SciEffect ~ tb2$inseam + tb2$country + tb2$gender + tb2$age + tb2$degree )) #random
summary(lm(tb2$SciEffect ~ tb2$shaq + tb2$country + tb2$gender + tb2$age + tb2$degree )) #p=0.066 shaq
summary(lm(tb2$SciEffect ~ tb2$weather + tb2$country + tb2$gender + tb2$age + tb2$degree )) #no effect


f = testTwoFactorTb2("country", "shaq")
f

f = testTwoFactorTb2( "country", "shaq")
f


