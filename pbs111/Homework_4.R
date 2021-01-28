library(car)
library(ez)
library(tidyr)
library(nlme)
library(lme4)
library(lmerTest)
library(lattice)
library(dplyr)

chol_wide = read.csv('data_files//chlo2_dat.txt')
chol_wide$ID = as.factor(chol_wide$ID)
chol_wide$Margarine = as.factor(chol_wide$Margarine)
head(chol_wide)
dim(chol_wide)

# make long format
chol_long <- gather(chol_wide, phase, measurement, Before:After8weeks, factor_key=TRUE)
chol_long$phase = as.numeric(chol_long$phase)
head(chol_long)
hist(chol_long$measurement)
plot(chol_long$measurement ~ chol_long$phase)
boxplot(measurement ~ Margarine, data=chol_long)

# look at covariance structure across trials
var(chol_wide[2:4])
cor(chol_wide[2:4])
dim(chol_long)

par(cex = .6)
xyplot(measurement ~ phase | Margarine, data = chol_long, groups = ID,
       type = "o", panel = panel.superpose)

#looks like this might be exponential decay
par(cex = .6)
xyplot(measurement ~ exp(-1/chol_long$phase) | Margarine, data = chol_long, groups = ID,
       type = "o", panel = panel.superpose)

# where we left off last time
# run a linear mixed effects model allowing for random intercepts and slopes per subject
marg_lmer = lmer(measurement ~ as.numeric(phase) * Margarine + (1 |ID) , data = chol_long, REML = F)  # REML = T for comparison)
summary(marg_lmer)


# looks more linear after the exponential transformation, so we'll transform it
chol_long$phase <- exp(-1/chol_long$phase)

model_exp <- lmer(measurement ~ phase * Margarine + (1 + phase|ID) , data = chol_long, REML = F)
summary(model_exp)





#2

rat = read.csv('data_files//birthweight.csv')
dim(rat)
head(rat)

boxplot(weight ~ Treatment, data = rat)
boxplot(weight ~ sex, data = rat)
boxplot(weight ~ Lsize, data = rat)
hist(rat$weight)

# look at means across groups
aggregate(weight ~ Lsize, data=rat, mean)
aggregate(weight ~ sex, data=rat, mean)
aggregate(weight ~ Treatment, data=rat, mean)


#variances
# look at means across groups
aggregate(weight ~ Lsize, data=rat, var)
aggregate(weight ~ sex, data=rat, var)
aggregate(weight ~ Treatment, data=rat, var)


rat %>% count(Treatment)

# linear model with DV weight, sex, Litter, and Treatment as factors as IVs, and Lsize as continuous covariate
contrasts(rat$Treatment) = contr.poly(3)
rat_mod = lm(weight ~ Treatment + as.factor(sex) + Litter + as.numeric(Lsize), data=rat)
summary(rat_mod)


