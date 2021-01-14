library(car)
library(ez)
library(tidyr)

learn = read.csv('data_files/percep_learn.csv')
learn$trial = as.factor(learn$trial)
learn$group = as.factor(learn$group)
learn$subj = as.factor(learn$subj)
head(learn)

hist(learn$per_corr)

boxplot(learn$per_corr ~ learn$group)

aggregate(per_corr ~ group, data=learn, mean)
aggregate(per_corr ~ group, data=learn, var)

# perform anova with no corrections
learn_aov = aov(per_corr ~ group * trial + Error(subj), data = learn)   # uses Type II SS, can't change
summary(learn_aov)

# create contrasts of interest
low2high = c(-1,-0.5,0, 0.5, 1)
lowvhigh = c(-1, -1, 0, 1, 1)
contrasts(learn$trial) <- cbind(low2high, lowvhigh)
contrasts(learn$group) <- contr.sum(2)

# perform anova using GG and HF corrections
learn.ez <- ezANOVA(data = learn, dv = .(per_corr), wid = .(subj),
                  between = .(group), within = .(trial), type = 3, detailed = T, return_aov = TRUE)
print(learn.ez)

## Now Using MANOVA
# convert to wide data
learn_wide <- spread(learn, trial, per_corr)
# # Rename 1 to first, and 2 to second
names(learn_wide)[names(learn_wide)=="1"] <- "first"
names(learn_wide)[names(learn_wide)=="2"] <- "second"
names(learn_wide)[names(learn_wide)=="3"] <- "third"
names(learn_wide)[names(learn_wide)=="4"] <- "fourth"
names(learn_wide)[names(learn_wide)=="5"] <- "fifth"
head(learn_wide)

# create contrasts of interest
low2high = c(-1,-0.5,0, 0.5, 1)
confactor = as.factor(low2high)
conframe = data.frame(confactor)
trialbind = cbind(learn_wide$first,learn_wide$second,learn_wide$third,learn_wide$fourth, learn_wide$fifth)
model <- lm(trialbind ~ learn_wide$group)
analysis <- Anova(model, idata = conframe, idesign = ~confactor)
summary(analysis)




# 2.)
chol_wide = read.csv('data_files//chlo2_dat.txt')
chol_wide$ID = as.factor(chol_wide$ID)
chol_wide$Margarine = as.factor(chol_wide$Margarine)
head(chol_wide)

# The arguments to gather():
# - data: Data object
# - key: Name of new key column (made from names of data columns)
# - value: Name of new value column
# - ...: Names of source columns that contain values
# - factor_key: Treat the new key column as a factor (instead of character vector)
chol_long <- gather(chol_wide, phase, measurement, Before:After8weeks, factor_key=TRUE)
head(chol_long)
hist(chol_long$measurement)
plot(chol_long$measurement ~ chol_long$phase)

# perform anova with no corrections
chol_aov = aov(measurement ~ Margarine * phase + Error(ID), data = chol_long)   # uses Type II SS, can't change
summary(chol_aov)

# create contrasts of interest
decreasing = c(0.5, 0, -0.5)
increasing = c(-0.5, 0, 0.5)
parabolic = c(0.5, 0, 0.5)

contrasts(chol_long$phase) <- cbind(decreasing, increasing, parabolic)
contrasts(chol_long$Margarine) <- contr.sum(2)

# perform anova using GG and HF corrections
chol.ez <- ezANOVA(data = chol_long, dv = .(measurement), wid = .(ID),
                  between = .(Margarine), within = .(phase), type = 3, detailed = T, return_aov = TRUE)
print(chol.ez)

## Now Using MANOVA
head(chol_wide)

trial <- c(1, 2, 3)
trialfactor <- as.factor(trial)
trial_frame <- data.frame(trialfactor)
trial_bind <- cbind(chol_wide$Before,chol_wide$After4weeks,chol_wide$After8weeks)
model <- lm(trial_bind ~ chol_wide$Margarine)
analysis <- Anova(model, idata = trial_frame, idesign = ~trialfactor)
summary(analysis)