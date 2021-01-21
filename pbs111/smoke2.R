# loaded lme4 (Bates et al. paper)

# show Rich and describe Rich's data

#read in two versions of Rich Lopez's data.  the first only uses subjects with no missing data
# the second version uses all data, missing cells or otherwise

# some analyses are bothered by missing data.  So for one vesion of dataset
# I eliminated any subjecy with any missing data  "rich_smoke__nomiss.csv"

smoke_nomiss <- read.csv("a 111/rich_smoke__nomiss.csv")  
## Convert variables to factor
  smoke_nomiss$group <- factor(smoke_nomiss$group)
  smoke_nomiss$time <- factor(smoke_nomiss$time)
  smoke_nomiss$id <- factor(smoke_nomiss$id)

table(smoke_nomiss$group)  # eliminating those subjects leads to unequal n's
hist(smoke_nomiss$cpd,15,col = "purple")
boxplot(smoke_nomiss$cpd~smoke_nomiss$group)

smoke <- read.csv("a 111/rich_smoke_data.csv")  # dataset with all data
## Convert variables to factor
smoke <- within(smoke, {
  group <- factor(group)
  id <- factor(id)
  time <- as.numeric(time)  # importantly I leave time as a continuous variable
})
table(smoke$group)

library("ggplot2")
line <- ggplot(smoke_nomiss, aes(time, cpd, colour = group))
line + stat_summary(fun = mean, geom = "line", size = 2, aes(group=group))

# as with all linear models, we need to specify 0 sum othoganal contrast
cont_vs_treatment <- c(2,-1,-1)  # control versus the 2 exper conditions
tr2_vs_tr3 <- c(0,.5,-.5)   # compare exper conditions
contrasts(smoke_nomiss$group) <- cbind(cont_vs_treatment,tr2_vs_tr3 )
contrasts(smoke_nomiss$time) = contr.poly(5)  # we expect a linear or exponential decrease

library("ez")
smoke.ez <- ezANOVA(data = smoke_nomiss, dv = .(cpd), wid = .(id), between = .(group), within = .(time), type = 3, detailed = T)
smoke.ez   # let's look at 'old fashioned analysis"

mat <- with(smoke_nomiss, matrix(c(cpd[time==1], cpd[time==2], cpd[time==3], cpd[time==4], cpd[time==5]), ncol = 5))
var(mat, na.rm = T)   # see if variances are =
cor(mat, use = "pairwise.complete.obs")  # see if there is a pattern to cov

# let's look at an example of AR1  from cor matrix look how well first row fits AR1
.875^2  # 0.765625
.875^3  # 0.6699219
.875^4  # 0.5861816

library("nlme")

smoke.cs <- gls(cpd ~ group * time, data = smoke_nomiss, corr = corCompSymm(, form= ~ 1 | id), method = "ML" )
summary(smoke.cs)
anova(smoke.cs, type = "m") #  This should yield approximately same F's as exANOVA

smoke.ar1 <- gls(cpd ~ group * time, data = smoke_nomiss, corr = corAR1(, form= ~ 1 | id), method = "ML" )
summary(smoke.ar1)
anova(smoke.ar1, type = "m" )  # this should lead to lower AIC as correltions look like AR1
anova(smoke.cs,smoke.ar1)

smoke.ar1un <- gls(cpd ~ group * time, data = smoke_nomiss, corr = corAR1(, form= ~ 1 | id), weights = varIdent(form = ~ 1 | time), method = "ML" )
anova(smoke.ar1un, type = "m" )  # shouldn't be better because variances don't look unequal
anova(smoke.ar1,smoke.ar1un)

smoke.un <- gls(cpd ~ group * time, data = smoke_nomiss,
              corr=corSymm(form = ~ 1 | id),
              weights = varIdent(form = ~ 1 | time), method = "ML" )
summary(smoke.un)
anova(smoke.un, type = "m")  # shouldn't be better than AR1 based on var/cov matrix
anova(smoke.ar1,smoke.un)

# spaghetti plots

library("lattice")
par(cex = .6)
xyplot(cpd ~ time | group, data = smoke_nomiss, groups = id,
       type = "o", panel = panel.superpose)

smoke$time <- as.numeric(smoke$time)  # just make sure time is't a factor anymore
smoke$time_center <- smoke$time - mean(smoke$time) # gonna play with mean centering (watch out)

cont_vs_treatment <- c(2,-1,-1)
tr2_vs_tr3 <- c(0,1,-1)
contrasts(smoke$group) <- cbind(cont_vs_treatment,tr2_vs_tr3)

library("lme4")
library("lmerTest")

# just for calbration let's keep treating time a a factor and compare
smoke_time_int_factor <- lmer(cpd ~ as.factor(time) * group + (1|id) , data = smoke_nomiss, REML = T)  # REML = T for comparison)
summary(smoke_time_int_factor)
anova(smoke_time_int_factor) #  This should yield approximately same F's as exANOVA, but must use REML

# now let's do 2 things: make time continuous and add back in all S's with missing data, also use "ML"

smoke_time_int <- lmer(cpd ~ time * group + (1|id) , data = smoke, REML = F)
summary(smoke_time_int)  # no random slopes
anova(smoke_time_int)
coef(smoke_time_int)

smoke_time_slope <- lmer(cpd ~ time * group + (1 + time|id) , data = smoke, REML = F)
summary(smoke_time_slope)  # add random slopes
anova(smoke_time_slope)
coef(smoke_time_slope)
ranef(smoke_time_slope)

# what is being tested?  what the heck?

anova(smoke_time_int,smoke_time_slope)  # does random slopes improve fit sig.

library(sjPlot)   # try a  plotting package
library(sjmisc)
plot_model(smoke_time_slope, type = "est")
plot_model(smoke_time_slope, type = "re")
tab_model(smoke_time_slope)        # nice way to get tables
confint(smoke_time_slope)

# what happens if we mean center the 'time' variable (pretty interesting)
time_center_slope <- lmer(cpd ~ time_center * group + (1 + time_center|id) , data = smoke, REML = F)
summary(time_center_slope)
anova(time_center_slope)

anova(smoke_time_slope,time_center_slope)

# slopes don't look very linear
# looks a lot like exponential decay, let's try it

smoke$etime <- exp(-1/smoke$time)
smoke_etime_slope <- lmer(cpd ~ etime * group + (1 + etime|id) , data = smoke, REML = F)
summary(smoke_etime_slope) 

anova(smoke_time_slope,smoke_etime_slope)

# our time periods 1-5 weren't equally spaced.  Let's look at correct spacing

# Convert to actual time elapsed in weeks
for (i in 1:length(smoke$id)) {
  if (smoke$time[i] == 1) {smoke$timewk[i] <- 0}
  if (smoke$time[i] == 2) {smoke$timewk[i] <- 1}
  if (smoke$time[i] == 3) {smoke$timewk[i] <- 2}
  if (smoke$time[i] == 4) {smoke$timewk[i] <- 4}
  if (smoke$time[i] == 5) {smoke$timewk[i] <- 8}
}

smoke$etimewk <- exp(-1/smoke$timewk)

table(smoke$timewk)
table(smoke$etimewk)
boxplot(smoke$cpd ~ smoke$etimewk)
line <- ggplot(smoke, aes(etimewk, cpd, colour = group))
line + stat_summary(fun = mean, geom = "line", size = 2, aes(group=group))



smoke_etimewk_eslope <- lmer(cpd ~ group * etimewk + (1 + etimewk|id) , data = smoke, REML = F)
summary(smoke_etimewk_eslope)
anova(smoke_etimewk_eslope)
anova(smoke_etime_slope,smoke_etimewk_eslope)  #so switching to real intervals didn't do much

plot_model(smoke_etimewk_eslope, type = "re")
tab_model(smoke_etimewk_eslope)


# mising data

