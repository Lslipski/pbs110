#(1)
numreps = 100
subjs_per_group = 24
cohen_d = 0.6

group = c(rep(1,24),rep(2,24))
group = as.factor(group)
dv = rep(0,48)

ps = c(rep(0, numreps))
sig = 0
bonferroni = 0
fdr = 0

for (rep in 1:numreps){
    # sample 24 times from normal distribution with mean 0 and std 1
    x = sort(rnorm(24, 0, 1))
    # sample 24 times from normal distribution with mean 0.6 and std 1 to get effect size 0.6
    y = sort(rnorm(24, cohen_d, 1))

    dv[1:24] = x
    dv[25:48] = y
    
    p_val = t.test(dv~group)$p.value
    ps[rep] = p_val
    if (p_val < 0.05) {sig = sig + 1}

}

bonferroni = p.adjust(ps, method = 'bonferroni', n = numreps)
fdr = p.adjust(ps, method = 'fdr', n = numreps)

bonferroni_sig = length(bonferroni[bonferroni < 0.05])
fdr_sig = length(fdr[fdr < 0.05])

print(sig)
print(bonferroni_sig)
print(fdr_sig)



#(2)
sal = read.csv('data_files//salary.csv')
head(sal)
dim(sal)

#scale continuous vars and convert categorical variables to factors
sal$years = scale(sal$years)
sal$age = scale(sal$age)
sal$gender = as.factor(sal$gender)
sal$departm = as.factor(sal$departm)

# take a look at univariate distributions
par(mfrow=c(3,2))
hist(sal$salary)
plot(sal$gender)
plot(sal$departm)
hist(sal$age)

# take a look at univariate distributions
par(mfrow=c(3,2))
hist(sal$salary)
plot(sal$gender)
plot(sal$departm)
hist(sal$age)
hist(sal$years)

# take a look at basic correlations btwn variables and possible interactions
par(mfrow=c(4,2))
plot(sal$years, sal$salary)
plot(sal$age, sal$salary)
plot(sal$gender, sal$salary)
plot(sal$departm, sal$salary)
plot(sal$departm, sal$gender)
plot(sal$years, sal$gender)
plot(sal$age, sal$gender)

# make more clear plot of salary by gender for best argument
library(ggplot2)
qplot(gender, salary, data=sal, geom=c("boxplot"),
   fill=gender, main="Salary by Gender",
   xlab="Gender", ylab="Salary")
hist(sal$years)

#t-test of the 2 means (men and women), not controlling for edlevel and age
t.test(salary ~ gender, sal)

summary(lm(salary~gender+departm+age+years, data=sal))


