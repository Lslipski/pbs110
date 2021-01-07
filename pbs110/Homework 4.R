#(1)
dat = read.csv('data_files/hw4_q1.csv')
colnames(dat) = c('experiment', 'control')
par(mfrow=c(1,2))
hist(dat$experiment)
hist(dat$control)

#min, max, mean, median, quartiles for experimental distribution
summary(dat$experiment)
var(dat$experiment)

#min, max, mean, median, quartiles for control distribution
summary(dat$control)
var(dat$control)

#compare means and variances
means = c(mean(dat$experiment), mean(dat$control))
sds = c(sd(dat$experiment), sd(dat$control))
plot(means, sds)



#(2)
# From HW2 K = 30
# get probabilities for each value in distribution
iter = c(1:5)
for (i in iter) {
    iter[i] = (2*i)/30
}
print(paste('Probabilities of each value: ',iter))

# multiply values by their probabilities
dist = c(1:5)* iter

#mean is the sum of each value times its probability
print(paste('mean of distribution: ',sum(dist)))

# variance is the sum of squared distance of each value from the mean times its probability
vals = rep(0,5)
for (val in c(1:5)){
    sqrd_d_from_mean = ((val - dist[val])^2) * iter[val]
    vals[val] = sqrd_d_from_mean
}
print(paste('variance of distribution: ', sum(vals)/5))


# sample 16 times from distribution and calculate mean and variance of sample
samp_means = rep(0,100)
samp_vars = rep(0,100)
n = 16

for (samp in 1:100){
    new_sample = (sample(c(1:5),n, prob = iter, replace = T))
    samp_means[samp] = mean(new_sample)
    samp_vars[samp] = var(new_sample) #(sd(new_sample)^2) / sqrt(n)
}
print(paste('mean of sample: ', mean(samp_means)))
print(paste('variance of sample: ', mean(samp_vars)))



#(3)
#answers in latex. Available in notebook and hw4_prob3.png


#(4)
# to get value of 90th percentile
mean = 650
sd = 60
zval = qnorm(.9, mean, sd)
zval

#(5)
#Algebra in notebook