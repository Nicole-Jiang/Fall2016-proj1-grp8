data("ChickWeight")
attach(ChickWeight)

#Question a-1
dim(ChickWeight)
sum(complete.cases(ChickWeight)) # 578 complete cases, which is the same as the dim of data set
plot(Time, weight)

#Question a-3
mean(weight)
median(weight)
range(weight)
quantile(weight)
sd(weight)
var(weight)

#Question a-4
hist(weight)
boxplot(weight, main= "Box-plot of weight")
qqnorm(weight)
qqline(weight)

#Question b-1
# Jackknife for standard deviation
sum_sd= 0
n= length(weight)
for(i in 1:n){
  sum_sd= sum_sd + sd(weight[-i])
}
B_jack_sd= (n-1)*(sum_sd/n-sd(weight))

# Jackknife for median
sum_median=0
for(i in 1:n){
  sum_median = sum_median + median(weight[-i])
}
B_jack_median= (n-1)*(sum_median/n-median(weight))

fit.lm <- lm(weight ~ Time, data= ChickWeight)
plot(Time, weight)
abline(fit.lm)

#bootstrap for sd
N=1000
boot_sd_bias= mat.or.vec(1,N)
for(i in 1:N){
  temp= sample(weight,n,replace= T)
  boot_sd_bias[i]=sd(temp)-sd(weight)
}
mean(boot_sd_bias)

#bootstrap for median
N=1000
boot_median_bias= mat.or.vec(1,N)
for(i in 1:N){
  temp= sample(weight,n,replace= T)
  boot_median_bias[i]=median(temp)-median(weight)
}
mean(boot_median_bias)


