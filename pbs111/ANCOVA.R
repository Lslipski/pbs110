# material on covariance and its dangers

cho <- read.csv("a 111/cholesterol.csv")
cor(cho$chol,cho$age)    # see if chol is f(age)
cho$state <- as.factor(cho$state)

zerosum <- c(-.5,.5)
contrasts(cho$state) = zerosum

aggregate(chol~state,cho,mean)
aggregate(age~state,cho,mean)

mod1 <- lm(chol ~  state, data = cho)             # model w/o covar
summary(mod1)

t.test(cho$age~cho$state)                         # see if age varies by state
mod2 <- lm(chol ~ age + state, data = cho)        # model with covar
summary(mod2)

library(emmeans)                             # estimated marginal means (adjusted means)
emmeans(mod1,"state")                        # what the hell are setimated means
emmeans(mod2,"state")

goats <- read.csv("a 111/goats.csv")

goats$Treatment <- as.factor(goats$Treatment)
contrasts(goats$Treatment) <- zerosum

cor(goats$gain,goats$Initial_wt)               # see if amt gained is f(initial weight)
t.test(goats$Initial_wt~goats$Treatment)       # see if initial weight differs by group
mod1 <- lm(gain ~ Treatment, data = goats)     # model w/o covar
summary(mod1)
mod2 <- lm(gain ~ Initial_wt + Treatment, data = goats)   # model with covar
summary(mod2)

emmeans(mod1,"Treatment")                      # estimated marginal means (adjusted means)
emmeans(mod2,"Treatment")

library(effects)                              # a new function for estimated marginal means 
summary(effect("Treatment",mod1))             # this covar reduced within group var, a good thing
summary(effect("Treatment",mod2))             # critically groups didn't differ in initial weight



cov <- read.csv("a 111/cov_test_data.csv")    # watch how bad it can get!

cov$iv <- as.factor(cov$iv)
contrasts(cov$iv)  <- zerosum

mod_base <- lm(diff ~ iv, data = cov)
summary(mod_base)

aggregate(diff~iv,cov,mean)                  # mean diff as f(iv)
emmeans(mod_base, "iv")                      # no covar, no change

t.test(cov$time1_1~cov$iv)
mod_cov1 <- lm(diff ~ time1_1 + iv, data = cov)
summary(mod_cov1)
emmeans(mod_cov1, "iv")                      # almost no change, satisfied constraint of no differences at t1

t.test(cov$time1_2~cov$iv)
mod_cov2 <- lm(diff ~ time1_2 + iv, data = cov)
summary(mod_cov2)
emmeans(mod_cov2,"iv")

mod_cov3 <- lm(diff ~ time1_3 + iv, data = cov)
summary(mod_cov3)
emmeans(mod_cov3,"iv")

emmeans(mod_cov1,"iv")
emmeans(mod_cov2,"iv")
emmeans(mod_cov3,"iv")

# wow


