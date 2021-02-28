#problem 1

library(lme4)
library(lmerTest)
library(sjPlot)

holly <- read.csv("a 111/holly_map_data.csv")
holly[1:10,]
holly$pnum <- as.factor(holly$pnum)
holly$year <- as.numeric(holly$year)   # could have used as factor and specified Poly contrasts
aggregate(DB ~ year, data = holly, mean)
aggregate(DB ~ year, data = holly, var)

# what the heck are DE and DB
# DE is absolute |error| and represents inaccuracy
# DB is unsigned error and represents bias

hist(holly$DB, 20, col = "brown")  #brown is Tuft's color

colors()
boxplot(DB ~ year, col = c("blue4", "blue3", "blue2", "blue1"), holly)  # seeing the lightness
boxplot(DB ~ landmark, col = "yellow", holly)

model_db <- lmer(DB ~ year + (1|pnum) + (1|landmark), data = holly, REML = F)
summary(model_db)
anova(model_db)

model_db_randslopes <- lmer(DB ~ year + (1|pnum) + (1+year|landmark), data = holly, REML = F)
summary(model_db_randslopes)

anova(model_db,model_db_randslopes)
# beware of - AICs (more + is bad; more - is good)

model_db_randslopesi <- lmer(DB ~ year + (1|pnum) + (0+year|landmark), data = holly, REML = F)
summary(model_db_randslopesi)
anova(model_db,model_db_randslopesi)

coef(model_db)
coefficients(model_db)
plot_model(model_db, type = "re", grid = F, sort.est = T)
tab_model(model_db, digits = 4, digits.p = 4, digits.re = 4)

# bottom line is buildings get closer and closer!

# not assigned but could use DE as independent variable

model_de <- lmer(DE ~ year + (1|year:pnum) + (1|landmark), data = holly, REML = F)
summary(model_de)

model_de_randslopes <- lmer(DE ~ year + (1|year:pnum) + (1+year|landmark), data = holly, REML = F)
summary(model_de_randslopes)

anova(model_de,model_de_randslopes)

model_de_randslopesi <- lmer(DE ~ year + (1|year:pnum) + (0+year|landmark), data = holly, REML = F)
summary(model_de_randslopesi)

plot_model(model_de, type = "re", grid = F, sort.est = T, digits = 4, digits.p = 4, digits.re = 4)

# DB is much better and sor of understandable

# problem 2

v2_expert <- read.csv('a 111/jesse_search_data.csv')
v2_expert <- within(v2_expert, {
  set_size <- as.numeric(set_size)
  orientation <- factor(orientation)
  presence <- factor(presence)
  identity <- factor(identity)
  sub <- factor(sub)
})

present <- subset(v2_expert, presence == 1)
absent <- subset(v2_expert, presence == 2)

zerosum = c(-.5,.5)
contrasts(present$orientation) = zerosum

# for today let's just look at 'present'

hist(present$rt, 20, col = "magenta")

boxplot(rt ~ set_size * presence,col=c("blue1","blue2","blue3"),v2_expert)
boxplot(rt ~ presence * orientation,col=c("blue","red"),v2_expert)
boxplot(rt ~ set_size * orientation,col=c("blue","red","yellow"),v2_expert)

library(ez)

jesse_ez <- ezANOVA(data = present, dv = .(rt), wid = .(sub), within = .(set_size, orientation),
                    type = 3, detailed = T)

library('lme4')

present_model <- lmer(rt ~ scale(set_size) * orientation + (1|sub) + (1|identity) , data= present, REML = F)
summary(present_model)

# could try a whole slew of random slopes but will just cut to the chase

present_model_slopes <- lmer(rt ~ scale(set_size) * orientation + (1+orientation|sub) + (1|identity) , data= present, REML = F)
summary(present_model_slopes)

anova(present_model, present_model_slopes)

plot_model(present_model_slopes, type = "re", grid = F, sort.est = T)


tab_model(present_model_slopes, digits = 4, digits.p = 4, digits.re = 4)  # need to specify # digits or get all 0's
