#(1)
die = c(1, 2, 3, 4, 5, 6)
roll = sample(die, size=1, replace=TRUE)
roll

roll_two_fours = function(){
    die = c(1, 2, 3, 4, 5, 6)
    fours = 0
    loops = 0
    num_games = 10
    hours = rep(0, num_games)

    i = 1
    while (i == 1) {
        roll = sample(die, size=1, replace=TRUE)
        if (roll == 4) {fours = fours + 1}
        loops = loops + 1

        if (fours == 2){i = 0}
    }
    return(loops)
}

num_games = 100000
hours = rep(0, num_games)

for (i in 1:num_games){
    x = roll_two_fours()
    hours[i] = x
}

#sample 13 students 10000 times from the 100,000 runs above
num_students = 13
num_samples = 10000
samp_means = rep(0, num_samples)
samp_variances = rep(0, num_samples)
samp_stderrs = rep(0, num_samples)

for (p in 1:num_samples){
    students = sample(hours, size = num_students, replace=TRUE)
    samp_means[p] = mean(students)
    samp_variances[p]=var(students)
    samp_stderrs[p] = sd(students) / sqrt(length(students))
    
}

library('psych')
describe(hours)
var(hours)

mean(samp_means)
mean(samp_variances)
mean(samp_stderrs)




#(2)
# A = being a terrorist
# B = positive test result 
#P(A|B) = P(A)*P(B|A) / P(B)

#probability of being a terrorist P(A)
p_terror = 88 / ((200000*365) - 88)
paste('p_terror: ', p_terror)

#probability of a positive test, regardless of terrorist value P(B)
num_terrorists = 88
num_non_terrorists = (200000*365) - 88
num_positive = (.88 * num_terrorists) + (.03 * num_non_terrorists)
p_positive = num_positive / (200000*365)
paste('p_positive: ', p_positive)

#probability of positive test given terrorist P(B|A)
p_positive_terror = 0.88
paste('p_positive_terror: ', p_positive_terror)

#probability that terrorist given a positive test P(A|B)
p_terror_positive = (p_terror * p_positive_terror) / p_positive
paste('p_terror_positive: ', p_terror_positive)




#(3)
mov = read.csv('data_files/movies.csv')
head(mov)
dim(mov)

# take a look at univariate distributions
par(mfrow=c(4,1))
hist(mov$recall)
plot(mov$gender,mov$recall)
plot(mov$genre, mov$recall)
plot(mov$gender~mov$genre)

mov$gender = as.factor(mov$gender)
mov$genre = as.factor(mov$genre)

# create a linear model with recall as dv and gender as ids
lm_1 = lm(recall~gender, data=mov)
summary(lm_1)

# create a linear model with recall as dv and gender and genre as ids
lm_1 = lm(recall ~ gender + genre, data=mov)
summary(lm_1)

#Hypothesis: Men have better recall than women
con_male = c(-1, 1)
contrasts(mov$gender) = cbind(con_male)

#Hypothesis: Action best recall, then comedy, then drama
con_genre = c(1, 0, -1)
contrasts(mov$genre) = cbind(con_genre)

# include both contrasts in a linear model to test hypotheses
con_model = lm(recall ~ gender * genre, data = mov)
summary(con_model)

# compute effect size, assume it is inflated by 25%
library(effectsize)
library(pwr)

cd = cohens_d(recall~gender, data=mov)
pwr.t.test(d=cd$Cohens_d*0.75,n=, power=0.8, sig.level=0.05,type="two.sample",alternative="two.sided")





#(4)
fight = read.csv('data_files/school_fighting.csv')
head(fight)
dim(fight)

fight$gender = as.factor(fight$gender)
fight$grade = as.factor(fight$grade)

aggregate(fights~grade, fight, mean)
aggregate(fights~grade, fight, var)

aggregate(fights~gender, fight, mean)
aggregate(fights~gender, fight, var)
hist(fight$fights)

par(mfrow=c(2,2))
hist(fight[fight$grade == 1,3])
hist(fight[fight$grade == 2,3])
hist(fight[fight$grade == 3,3])
hist(fight[fight$grade == 4,3])

# fit glm for negative binomial distributions
library(MASS)
nb = glm.nb(fights~gender + grade, data=fight)
summary(nb)

# compare against poisson just for fun
poisson = glm(fights~gender+grade, data = fight,  family = poisson())
summary(poisson)

con_grade = c(-3, -1, 1, 3)
contrasts(fight$grade) = cbind(con_grade)
nb = glm.nb(fights~gender + grade, data=fight)
summary(nb)

