
# Graphing log and exponential functions

x <- 1:10
plot(x,log(x))
plot(x,exp(x))
plot(x,exp(-x))
plot(x,1/exp(-x))
plot(x,1-exp(-x))


# trauma study

#first ignore therapists as random variable
web <- read.csv("stat consulting/cohen data/pittsburgh.csv",header = T)
web$ucla_diff = web$T2.UCLA.TOT - web$T1.UCLA.TOT
(mean((web$T1.UCLA.TOT), na.rm = T) + mean((web$T2.UCLA.TOT), na.rm = T))/2
t.test(web$ucla_diff~web$Condition)

# use lmm include therapist and id with random intercepts only
library(lme4)
library(lmerTest)
library(sjPlot)

web_long <- read.csv("stat consulting/cohen data/pittsburgh_long.csv",header = T)
table(web_long$Condition)
mean(web_long$UCLA,na.rm = T)

aggregate(UCLA ~ Condition * Time, web_long, mean)

# I've already made time and condition 0 sum

null <- lmer(UCLA ~ Condition*Time + (1|ID), data = web_long, REML = "F")
summary(null)

ucla <- lmer(UCLA ~ Condition*Time + (1 |ID), data = web_long, REML = "F")
summary(ucla)
coef(ucla)
anova(null,ucla)

ucla_ther <- lmer(UCLA ~ Condition*Time + (1 |ID) + (1|THERAPIST), data = web_long, REML = "F")
summary(ucla_ther)
coef(ucla_ther)
anova(ucla,ucla_ther)

# now add slopes for therapists
ucla_slopes<- lmer(UCLA ~ Condition*Time  + (0 + Time|THERAPIST) + (1 |ID), data = web_long, REML = "F")
summary(ucla_slopes)
coef(ucla_slopes)
anova(ucla,ucla_slopes)

ucla_noint<- lmer(UCLA ~ Condition*Time  + (0 + Time|THERAPIST) + (1|ID), data = web_long, REML = "F")
summary(ucla_noint)
coef(ucla_noint)
ranef(ucla_noint)
plot_model(ucla_noint, type = "re")
table(web_long$THERAPIST)
table(web_long$THERAPIST,web_long$Condition)
anova(null,ucla_noint)

#okay, let's add number of clients treated
ucla_num<- lmer(UCLA ~ Condition*Time + number  + (0 + Time|THERAPIST) + (1|ID), data = web_long, REML = "F")
summary(ucla_num)

# now add slopes fort both therapists and clients (way tgoo many parameters)
ucla_double<- lmer(UCLA ~ Condition*Time  + (0 + Time|THERAPIST) + (0+Time|ID), data = web_long, REML = "F")
summary(ucla_double)
coef(ucla_double)

