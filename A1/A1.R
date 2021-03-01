# packages
install.packages("tidyverse")
install.packages("rmarkdown")
install.packages("gdata")
install.packages("boot")
install.packages("xtable")
install.packages("MASS")
install.packages("moments")
install.packages("mvtnorm")

library(tidyverse)
# set working directory
setwd("/Users/alessahuang/Desktop/Econ613/Assignments/A1/dat")

# load data
datstu = read.csv("datstu.csv")
datjss = read.csv("datjss.csv")
datsss = read.csv("datsss.csv")

####################################################


# Part 1
# EX 1
# number of students
nrow(datstu)

# number of schools
schoolcodes = select(datstu, starts_with("schoolcode"))
n_distinct(flatten(schoolcodes), na.rm = TRUE)

# number of programs
programs = select(datstu, starts_with("choicepgm"))
n_distinct(flatten(programs), na.rm = TRUE)

# number of choices
schools_all = flatten(schoolcodes)
programs_all = flatten(programs)

sp1 = datstu %>% group_by(schoolcode1,choicepgm1) %>% summarise(Count = n())
sp2 = datstu %>% group_by(schoolcode2,choicepgm2) %>% summarise(Count = n())
sp3 = datstu %>% group_by(schoolcode3,choicepgm3) %>% summarise(Count = n())
sp4 = datstu %>% group_by(schoolcode4,choicepgm4) %>% summarise(Count = n())
sp5 = datstu %>% group_by(schoolcode5,choicepgm5) %>% summarise(Count = n())
sp6 = datstu %>% group_by(schoolcode6,choicepgm6) %>% summarise(Count = n())
sp = bind_rows(sp1, sp2, sp3, sp4, sp5, sp6)
num_sp = sp %>% group_by(schoolcode1, choicepgm1) %>% summarise()
num_sp = num_sp %>% rename(schoolcode = schoolcode1, choicepgm = choicepgm1)
nrow(num_sp)

# number of missing test score
sum(is.na(datstu$score))

# number of students who apply to the same school(different programs) 
same_school = schoolcodes %>% filter(schoolcode1 == schoolcode2 |
                                       schoolcode1 == schoolcode3|
                                       schoolcode1 == schoolcode4|
                                       schoolcode1 == schoolcode5|
                                       schoolcode1 == schoolcode6|
                                       schoolcode2 == schoolcode3|
                                       schoolcode2 == schoolcode4|
                                       schoolcode2 == schoolcode5|
                                       schoolcode2 == schoolcode6|
                                       schoolcode3 == schoolcode4|
                                       schoolcode3 == schoolcode5|
                                       schoolcode3 == schoolcode6|
                                       schoolcode4 == schoolcode5|
                                       schoolcode4 == schoolcode6|
                                       schoolcode5 == schoolcode6
                                       )

nrow(same_school)
# apply to less than 6 choices
less = schoolcodes %>% filter(is.na(schoolcode1)| is.na(schoolcode2) 
                       | is.na(schoolcode3) | is.na(schoolcode4)| 
                         is.na(schoolcode5) |is.na(schoolcode6))
nrow(less)


# EX 2
ssss = datsss %>% select(schoolcode, sssdistrict, ssslong, ssslat)%>% distinct()
ssss = ssss[complete.cases(ssss), ]
school_data = left_join(num_sp, ssss, by = "schoolcode")

rank1 = datstu %>% filter(rankplace == 1)
rank1 = rank1 %>% select(X, agey, male, score, 
                         schoolcode1, 
                         choicepgm1, 
                         jssdistrict)%>% rename(ad.schoolcode = schoolcode1, 
                                                    ad.choicepgm = choicepgm1)

rank2 = datstu %>% filter(rankplace == 2)
rank2 = rank2 %>% select(X,agey, male, 
                         score, schoolcode2, 
                         choicepgm2, jssdistrict) %>% rename(
                           ad.schoolcode = schoolcode2, 
                           ad.choicepgm = choicepgm2)


rank3 = datstu %>% filter(rankplace == 3)
rank3 = rank3 %>% select(X, agey, male, 
                         score, schoolcode3, 
                         choicepgm3, jssdistrict)%>% rename(
                           ad.schoolcode = schoolcode3, 
                           ad.choicepgm = choicepgm3)

rank4 = datstu %>% filter(rankplace == 4)
rank4 = rank4 %>% select(X, agey, male, 
                         score, schoolcode4, 
                         choicepgm4, jssdistrict)%>% rename(
                           ad.schoolcode = schoolcode4, 
                           ad.choicepgm = choicepgm4)

rank5 = datstu %>% filter(rankplace == 5)
rank5 = rank5 %>% select(X, agey, male, 
                         score, schoolcode5, 
                         choicepgm5, jssdistrict)%>% rename(
                           ad.schoolcode = schoolcode5, 
                           ad.choicepgm = choicepgm5)

rank6 = datstu %>% filter(rankplace == 6)
rank6 = rank6 %>% select(X, agey, male, 
                         score, schoolcode6, 
                         choicepgm6, jssdistrict)%>% rename(
                           ad.schoolcode = schoolcode6, 
                           ad.choicepgm = choicepgm6)

admitted = bind_rows(rank1, rank2, rank3, rank4, rank5, rank6)
addata = admitted %>% group_by(ad.schoolcode, 
         ad.choicepgm) %>% summarise(cutoff = min(score), 
                                     quality = mean(score), size = n())


school_data = left_join(school_data, addata, by = c("schoolcode" = "ad.schoolcode",
                                                    "choicepgm" = "ad.choicepgm"))

# EX3
admitted = left_join(admitted, datjss%>%select(-X), by = "jssdistrict")

admitted = left_join(admitted, ssss%>% select(-sssdistrict), 
                     by = c("ad.schoolcode"="schoolcode"))

admitted = admitted %>% rename(jsslong = point_x, jsslat = point_y)

admitted = admitted %>% mutate(distance= sqrt( 
  (69.172 * (ssslong - jsslong)* cos(jsslat/57.3))^2 +
    (69.172*(ssslat - jsslat))^2 ) )

dis = admitted %>% group_by(ad.schoolcode, ad.choicepgm) %>%
  summarise(avg_distance = mean(distance))

school_dis = left_join(school_data, dis, by = c("schoolcode" = "ad.schoolcode",
                                                    "choicepgm" = "ad.choicepgm"))


# EX4
# rank 1 choice
sp1 = left_join(sp1, 
                school_dis %>% select(
                  schoolcode, choicepgm, cutoff, quality, avg_distance),
                by = c("schoolcode1" = "schoolcode","choicepgm1" = "choicepgm"))

# cutoff
cmean1 = mean(sp1[,"cutoff"][!is.na(sp1[,"cutoff"])])
csd1 = sd(sp1[,"cutoff"][!is.na(sp1[,"cutoff"])])
# quality
qmean1 = mean(sp1[,"quality"][!is.na(sp1[,"quality"])])
qsd1 = sd(sp1[,"quality"][!is.na(sp1[,"quality"])])
# distance
dmean1 = mean(sp1[,"avg_distance"][!is.na(sp1[,"avg_distance"])])
dsd1 = sd(sp1[,"avg_distance"][!is.na(sp1[,"avg_distance"])])



# rank 2 choice
sp2 = left_join(sp2, 
                school_dis %>% select(
                  schoolcode, choicepgm, cutoff, quality, avg_distance),
                by = c("schoolcode2" = "schoolcode","choicepgm2" = "choicepgm"))

# cutoff
cmean2 = mean(sp2[,"cutoff"][!is.na(sp2[,"cutoff"])])
csd2 = sd(sp2[,"cutoff"][!is.na(sp2[,"cutoff"])])
# quality
qmean2 = mean(sp2[,"quality"][!is.na(sp2[,"quality"])])
qsd2 = sd(sp2[,"quality"][!is.na(sp2[,"quality"])])
# distance
dmean2 = mean(sp2[,"avg_distance"][!is.na(sp2[,"avg_distance"])])
dsd2 = sd(sp2[,"avg_distance"][!is.na(sp2[,"avg_distance"])])

# rank 3 choice
sp3 = left_join(sp3, 
                school_dis %>% select(
                  schoolcode, choicepgm, cutoff, quality, avg_distance),
                by = c("schoolcode3" = "schoolcode","choicepgm3" = "choicepgm"))

# cutoff
cmean3 = mean(sp3[,"cutoff"][!is.na(sp3[,"cutoff"])])
csd3 = sd(sp3[,"cutoff"][!is.na(sp3[,"cutoff"])])
# quality
qmean3 = mean(sp3[,"quality"][!is.na(sp3[,"quality"])])
qsd3 = sd(sp3[,"quality"][!is.na(sp3[,"quality"])])
# distance
dmean3 = mean(sp3[,"avg_distance"][!is.na(sp3[,"avg_distance"])])
dsd3 = sd(sp3[,"avg_distance"][!is.na(sp3[,"avg_distance"])])

# rank 4
sp4 = left_join(sp4, 
                school_dis %>% select(
                  schoolcode, choicepgm, cutoff, quality, avg_distance),
                by = c("schoolcode4" = "schoolcode","choicepgm4" = "choicepgm"))

# cutoff
cmean4 = mean(sp4[,"cutoff"][!is.na(sp4[,"cutoff"])])
csd4 = sd(sp4[,"cutoff"][!is.na(sp4[,"cutoff"])])
# quality
qmean4 = mean(sp4[,"quality"][!is.na(sp4[,"quality"])])
qsd4 = sd(sp4[,"quality"][!is.na(sp4[,"quality"])])
# distance
dmean4 = mean(sp4[,"avg_distance"][!is.na(sp4[,"avg_distance"])])
dsd4 = sd(sp4[,"avg_distance"][!is.na(sp4[,"avg_distance"])])

# rank 5
sp5 = left_join(sp5, 
                school_dis %>% select(
                  schoolcode, choicepgm, cutoff, quality, avg_distance),
                by = c("schoolcode5" = "schoolcode","choicepgm5" = "choicepgm"))

# cutoff
cmean5 = mean(sp5[,"cutoff"][!is.na(sp5[,"cutoff"])])
csd5 = sd(sp5[,"cutoff"][!is.na(sp5[,"cutoff"])])
# quality
qmean5 = mean(sp5[,"quality"][!is.na(sp5[,"quality"])])
qsd5 = sd(sp5[,"quality"][!is.na(sp5[,"quality"])])
# distance
dmean5 = mean(sp5[,"avg_distance"][!is.na(sp5[,"avg_distance"])])
dsd5 = sd(sp5[,"avg_distance"][!is.na(sp5[,"avg_distance"])])

# rank 6
sp6 = left_join(sp6, 
                school_dis %>% select(
                  schoolcode, choicepgm, cutoff, quality, avg_distance),
                by = c("schoolcode6" = "schoolcode","choicepgm6" = "choicepgm"))

# cutoff
cmean6 = mean(sp6[,"cutoff"][!is.na(sp6[,"cutoff"])])
csd6 = sd(sp6[,"cutoff"][!is.na(sp6[,"cutoff"])])
# quality
qmean6 = mean(sp6[,"quality"][!is.na(sp6[,"quality"])])
qsd6 = sd(sp6[,"quality"][!is.na(sp6[,"quality"])])
# distance
dmean6 = mean(sp6[,"avg_distance"][!is.na(sp6[,"avg_distance"])])
dsd6 = sd(sp6[,"avg_distance"][!is.na(sp6[,"avg_distance"])])

# make table
describe_rank = matrix(c(cmean1, csd1, qmean1, qsd1, dmean1, dsd1,
                       cmean2, csd2, qmean2, qsd2, dmean2, dsd2,
                       cmean3, csd3, qmean3, qsd3, dmean3, dsd3,
                       cmean4, csd4, qmean4, qsd4, dmean4, dsd4,
                       cmean5, csd5, qmean5, qsd5, dmean5, dsd5,
                       cmean6, csd6, qmean6, qsd6, dmean6, dsd6),
                       ncol = 6, byrow = TRUE)
colnames(describe_rank) = c("cutoff_mean", "cutoff_sd", "quality_mean",
                            "quality_sd", "distance_mean", "distance_sd")
rownames(describe_rank) = c("rank1", "rank2", "rank3", "rank4", "rank5",
                            "rank6")

# compute the quantile of student test scores
quantile(datstu[,'score'], probs = seq(0, 1, 0.25), na.rm = TRUE)

# quantile
q1 = filter(admitted, score <= 252)
q2 = filter(admitted, score > 252 & score <=283)
q3 = filter(admitted, score > 283 & score <= 324)
q4 = filter(admitted, score > 324)

# table
des_quantile = matrix(c(min(q1[,"score"]), mean(q1[,"score"]), sd(q1[,"score"]),
                      mean(q1[,"distance"]), sd(q1[,"distance"]),
                      min(q2[,"score"]), mean(q2[,"score"]), sd(q2[,"score"]),
                      mean(q2[,"distance"], na.rm = TRUE), sd(q2[,"distance"], na.rm = TRUE),
                      min(q3[,"score"]), mean(q3[,"score"]), sd(q3[,"score"]),
                      mean(q3[,"distance"]), sd(q3[,"distance"]),
                      min(q4[,"score"]), mean(q4[,"score"]), sd(q4[,"score"]),
                      mean(q4[,"distance"]), sd(q4[,"distance"])
                      ), ncol = 5, byrow = TRUE)

colnames(des_quantile) = c("cutoff", "quality_mean", "quality_sd", "distance_mean",
                           "distance_sd")
rownames(des_quantile) = c("Q1", "Q2", "Q3", "Q4")



######################################################

# Part 2

# EX 5
# setting a seed
set.seed(1233)
X1 = runif(10000, 1, 3)
X2 = rgamma(10000, 3, scale = 2)
X3 = rbinom(10000, 1, 0.3)
eps = rnorm(10000, 2, 1)
# Y variables
Y = 0.5 + 1.2 * X1 - 0.9 * X2 + 0.1 * X3 + eps
ydum = as.numeric(Y > mean(Y))

# EX 6
ones = seq(from = 1, to  = 1, length.out = 10000)
# Y ~ X1
new_X1 = cbind(ones, X1) # add ones to first col of X1
beta1 = solve(t(new_X1) %*% new_X1) %*% t(new_X1) %*% Y

# The coefficient beta is 1.19, smaller than 1.2. This may due to the number of
# sample we have, i.e. if we increase the number of observations from 10,000 to
# 100,000 or 1 000,000 the beta_hat we computed will be closer and closer to 1.2.
# The difference might also due to the residual, but it will not have big impact.

# Y ~ (1, X1, X2, X3)
X = cbind(new_X1, X2, X3)
# beta
beta = solve(t(X) %*% X) %*% t(X) %*% Y
# std error
Y_hat = X %*% beta
sigma2 = sum((Y-Y_hat)^2)/10000
var_beta = sigma2 * solve(t(X) %*% X)
std_err = sqrt(diag(var_beta))

# EX 7
# linear probability model
# P[ydum_i=1|X_i=x_i] = x_i'(beta)
# log likelihood
result1 = solve(t(X) %*% X) %*% t(X) %*% ydum
# F test
p = 3
hat_ydum1 = X %*% result1
SSE1 = sum((ydum - hat_ydum1)^2)
SSR1 = sum((hat_ydum1 - mean(ydum))^2)
f1 = (SSR1/p)/(SSE1/(10000-p-1))
f1 > qf(.99, df1=3, df2=10000-4)


# probit model
# P[ydum_i=1|X_i=x_i] = Phi(x_i'(beta))
# log likelihood
log.neg_llh2 = function(X, par2){
  -sum(ydum * log(pnorm(X %*% par2)) + (1 - ydum) * log(1 - pnorm(X %*% par2)) )
}
result2 = optim(par = c(0,0,0,0), fn = log.neg_llh2, X = X)

# hypothesis test(F-test)
p = 3
hat_par2 = result2$par
hat_ydum2 = X %*% hat_par2
SSE2 = sum((ydum - hat_ydum2)^2)
SSR2 = sum((hat_ydum2 - mean(ydum))^2)
f2 = (SSR2/p)/(SSE2/(10000-p-1))
f2 > qf(.99, df1=3, df2=10000-4)

# logit model
# P[ydum_i=1|X_i=x_i] = 1/(1 + exp[-x_i'(beta)])
# log likelihood
log.neg_llh3 = function(X, par3){
  -sum(ydum * log(1/(1+exp(-X%*%par3))) + (1 - ydum) * log(1 -1/(1+exp(-X%*%par3)) ))
}
result3 = optim(par = c(0,0,0,0), fn = log.neg_llh3, X = X)

# significance (F-test)
p = 3
hat_par3 = result3$par
hat_ydum3 = X %*% hat_par3
SSE3 = sum((ydum - hat_ydum3)^2)
SSR3 = sum((hat_ydum3 - mean(ydum))^2)
f3 = (SSR3/p)/(SSE3/(10000-p-1))
f3 > qf(.99, df1=3, df2=10000-4)


# EX 8
# probit model
# marginal effect(average marginal effect)
me_probit = exp(-(X%*%hat_par2)^2/2) %*% t(hat_par2) / sqrt(2 * pi)
avg_me_probit = colMeans(me_probit)


# logit model
# marginal effect(average marginal effect)
f = 1/(1+exp(- X%*% hat_par3))
me_logit = (f * (1 - f)) %*% t(hat_par3)
avg_me_logit = colMeans(me_logit)

# std error
set.seed(123)
R = 999
nvar = length(hat_par2)
outs2 = mat.or.vec(R,nvar)
outs3 = mat.or.vec(R,nvar)

for (i in 1:R)
{
  samp = sample(1:10000,200,rep=TRUE)
  X_samp = X[samp,]
  
  probit = exp(-(X_samp %*% hat_par2)^2/2) %*% t(hat_par2) / sqrt(2 * pi)
  outs2[i,] = colMeans(probit)
  
  f_boo = 1/(1+exp(- X_samp %*% hat_par3))
  logit = (f_boo * (1 - f_boo)) %*% t(hat_par3)
  outs3[i,] = colMeans(logit)
}

sd_probit = apply(outs2, 2, sd)
sd_logit = apply(outs3, 2, sd)
