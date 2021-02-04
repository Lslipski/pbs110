#imports
library(car)
library(ez)
library(tidyr)
library(nlme)
library(lme4)
library(lmerTest)
library(lattice)
library(dplyr)


# 1


#load data
map = read.csv('./data_files//holly_map_data.csv')
head(map,20)


boxplot(DB ~ year, data=map)

hist(map$DB)

# make factors
map$year = as.numeric(map$year)
map$landmark = as.numeric(map$landmark)

# start with random intercepts per participant
lmm_mod_randsub = lmer(DB ~ year + landmark + (1|pnum), data =map, REML = F)
summary(lmm_mod_randsub)

# add in random intercepts for landmarks
lmm_mod_randsub_randmark = lmer(DB ~ year + landmark + (1|pnum) + (1|landmark), data =map, REML = F)
summary(lmm_mod_randsub_randmark)

# add in random intercept per year
lmm_mod_randsub_randmark_randyear = lmer(DB ~ year + landmark + (1|pnum) + (1|landmark) + (1|year), data =map, REML = F)
summary(lmm_mod_randsub_randmark)
#singular

# allow for random slopes of participants within landmarks
randslope_pnuminlandmark = lmer(DB ~ year + landmark + (1|pnum) + (1 + pnum|landmark), data =map, REML = F)
summary(randslope_pnuminlandmark)
#singular





#2

#load data
rt = read.csv('./data_files//jesse_search_data.csv')
dim(rt)
head(rt, 20)

#limit to present condition
rt_present = rt[rt$presence == 1,]
dim(rt_present)

boxplot(rt ~ set_size, data=rt_present)

hist(rt_present$rt)

#going to transform for exponential decay
rt_present$rt = exp(-rt_present$rt)

hist(rt_present$rt)


#sub	rt	set_size	identity	presence	orientation
# is the set size effect different for upright versus inverted items
# create model with random intercepts for subject and identity along with the interaction term of set size with
# orientation
lmer1 = lmer(rt ~ set_size*orientation + (1|sub) + (1|identity), data=rt_present, REML = F)
summary(lmer1)


# test for interaction, but also allow for random intercepts for subject and identity
# then allow for random slopes for identity within each subject, since all subjects have all identities
lmer1 = lmer(rt ~ set_size*orientation + (1+identity|sub) + (1|identity), data=rt_present, REML = F)
summary(lmer1)


lmer1 = lmer(rt ~ set_size*orientation + (1|sub) + (1|identity) + (1|orientation), data=rt_present, REML = F)
summary(lmer1)
#singular








