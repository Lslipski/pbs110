# Problem 1
library('bootES')
library('pwr')

# calculate sample size based on 3 different effect sizes
ss_1 = pwr.t.test(n=, d=2.69, power=0.8,sig.level=.05, type='two.sample')
ss_2 = pwr.t.test(n=, d=1.54, power=0.8,sig.level=.05, type='two.sample')
ss_3 = pwr.t.test(n=, d=1.64, power=0.8,sig.level=.05, type='two.sample')

print(ss_1)
print(ss_2)
print(ss_3)

#assume effect size is inflated by 50%
# calculate sample size based on 3 different effect sizes
ss_1_50 = pwr.t.test(n=, d=1.35, power=0.8,sig.level=.05, type='two.sample')
ss_2_50 = pwr.t.test(n=, d=0.77, power=0.8,sig.level=.05, type='two.sample')
ss_3_50 = pwr.t.test(n=, d=0.82, power=0.8,sig.level=.05, type='two.sample')

print(ss_1_50)
print(ss_2_50)
print(ss_3_50)

# Problem 2
h4 = read.csv('data_files/h4.csv')
head(h4)

# cast group as a factor
h4$group = as.factor(h4$group)

aggregate(hcvknowl~group, h4, mean)
aggregate(hcvknowl~group, h4, var)

t.test(hcvknowl~group, data=h4, var.equal = T)
t.test(hcvknowl~group, data=h4, var.equal = F)

#test both means against 0
t.test(h4[which(h4$group == 1),]$hcvknowl, mu=0)
t.test(h4[which(h4$group == 2),]$hcvknowl, mu=0)


# Problem 3
cs = read.csv('data_files/class_screens.csv')
head(cs)
dim(cs)

# cast condition as a factor
cs$condition = as.factor(cs$CONDTION)

aggregate(NUM_SCREENS~condition, data=cs, mean)
aggregate(NUM_SCREENS~condition, data=cs, var)

hist(cs$NUM_SCREENS)
summary(cs$NUM_SCREENS)


# fit glm for negative binomial distributions
library(MASS)
nb = glm.nb(NUM_SCREENS~condition, data=cs)
summary(nb)

# compare against poisson just for fun
poisson = glm(NUM_SCREENS~condition, data = cs,  family = poisson())
summary(poisson)