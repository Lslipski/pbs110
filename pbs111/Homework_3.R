library(car)
library(ez)
library(tidyr)
library(nlme)
library(lme4)
library(lmerTest)
library(lattice)

plearn = read.csv('data_files/percep_learn.csv')
plearn$trial = as.factor(plearn$trial)
head(plearn)

boxplot(per_corr ~ group, data=plearn)

# look at covariance structure across trials

# convert to wide data
plw <- spread(plearn, trial, per_corr)
# # Rename 1 to first, and 2 to second
names(plw)[names(plw)=="1"] <- "first"
names(plw)[names(plw)=="2"] <- "second"
names(plw)[names(plw)=="3"] <- "third"
names(plw)[names(plw)=="4"] <- "fourth"
names(plw)[names(plw)=="5"] <- "fifth"
var(plw[3:7])
cor(plw[3:7])


# fit assuming compound symmetry
fit.plearn <- gls(per_corr ~ group * as.factor(trial), data = plearn, corr = corCompSymm(, form= ~ 1 | subj), method = "ML" )
summary(fit.plearn)
anova(fit.plearn)
anova(fit.plearn,type = "marginal")
coef(fit.plearn)
plot(fit.plearn)


fit.plearnar1 <- gls(per_corr ~ group * trial, data = plearn, corr = corAR1(, form= ~ 1 | subj), method = "ML" )
anova(fit.plearn, fit.plearnar1)


fit.plearnun <- gls(per_corr ~ group * trial, data = plearn, corr = corSymm(, form= ~ 1 | subj), method = "ML" )
anova(fit.plearnar1, fit.plearnun)

summary(fit.plearnar1)

#Now treat trial as continuous and run lmer
plearn$trial = as.numeric(plearn$trial)
# run a linear mixed effects model allowing for random intercepts and slopes per subject
plearn_factor = lmer(per_corr ~ as.numeric(trial) * group + (1 + as.numeric(trial)|subj) , data = plearn, REML = F)  # REML = T for comparison)
summary(plearn_factor)
anova(plearn_factor)
coef(plearn_factor)



#Now with the Margarine data
chol_wide = read.csv('data_files//chlo2_dat.txt')
chol_wide$ID = as.factor(chol_wide$ID)
chol_wide$Margarine = as.factor(chol_wide$Margarine)
head(chol_wide)
dim(chol_wide)

# make long format
chol_long <- gather(chol_wide, phase, measurement, Before:After8weeks, factor_key=TRUE)
chol_long$phase = as.factor(chol_long$phase)
head(chol_long)
hist(chol_long$measurement)
plot(chol_long$measurement ~ chol_long$phase)
boxplot(measurement ~ Margarine, data=chol_long)

# look at covariance structure across trials
var(chol_wide[2:4])
cor(chol_wide[2:4])
dim(chol_long)

# fit assuming compound symmetry
fit.marg <- gls(measurement ~ Margarine * phase, data = chol_long, corr = corCompSymm(, form= ~ 1 | ID), method = "ML" )
summary(fit.marg)
anova(fit.marg)
anova(fit.marg,type = "marginal")
coef(fit.marg)
plot(fit.marg)


fit.margar1 <- gls(measurement ~ Margarine * phase, data = chol_long, corr = corAR1(, form= ~ 1 | ID), method = "ML" )
anova(fit.marg, fit.margar1)
summary(fit.margar1)

fit.margun <- gls(measurement ~ Margarine * phase, data = chol_long, corr = corSymm(, form= ~ 1 | ID), method = "ML" )
anova(fit.margar1, fit.margun)
summary(fit.margun)

# specify contrasts for 
b4_vs_after = c(-2, 1, 1)
decrease = c(-1, 0, 1)
contrasts(chol_long$phase) <- cbind(b4_vs_after,decrease)


# run a linear mixed effects model allowing for random intercepts and slopes per subject
marg_lmer = lmer(measurement ~ as.numeric(phase) * Margarine + (1 |ID) , data = chol_long, REML = F)  # REML = T for comparison)
summary(marg_lmer)
anova(marg_lmer)
coef(marg_lmer)

