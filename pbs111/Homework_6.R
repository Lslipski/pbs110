library(car)
library(ez)
library(tidyr)
library(nlme)
library(lme4)
library(lmerTest)
library(lattice)
library(dplyr)


#1
#Redo Problem 2 (Jesse) from last week using glmer. You already have the datafile.



#load data
rt = read.csv('./data_files//jesse_search_data.csv')
dim(rt)
head(rt, 20)

#limit to present condition
rt_present = rt[rt$presence == 1,]
dim(rt_present)

hist(rt_present$rt)

# old lmer model
lmer_mod <- lmer(rt ~ scale(set_size) * orientation + (1|sub) + (1|identity) , data= rt_present, REML = F)
summary(lmer_mod)

#instead of transforming, let's use a non-normal function to model the DV
glmer1 = glmer(rt ~ scale(set_size)*orientation + (1|sub), family=Gamma(link=log),data=rt_present)
summary(glmer1)

#instead of transforming, let's use a non-normal function to model the DV
glmer2 = glmer(rt ~ scale(set_size)*orientation + (1|sub) + (1|identity), family=Gamma(link=log),data=rt_present)
summary(glmer2)

# try random slopes of subject within orientation
glmer3 = glmer(rt ~ scale(set_size)*orientation + (1 + orientation|sub) + (1 + orientation|identity), family=Gamma(link=log),data=rt_present)
summary(glmer3)




#2
#A former grad student ran a study here (well after graduating) on eye-movements as a predictor of face familiarity. Here are two #eye-movement parameters: probability of a leftward movement and the number of clusters. Were those two predictor variables, or #IVs, significantly related to familiarity? (Data in frank_trial_data)



eye = read.csv('data_files//frank_trial_data.csv')
dim(eye)
head(eye, 10)

aggregate(Pleft ~ Class, eye,  mean)
aggregate(Pleft ~ Class, eye,  sd)


aggregate(NumClus ~ Class, eye,  mean)
aggregate(NumClus ~ Class, eye,  sd)

# treat as normal with simple lmer and random subjects
m_norm = lmer(as.numeric(Class) ~ Pleft * NumClus + (1|subj_id), data=eye, REML=FALSE)
summary(m_norm)


# treat Class with a logit function (logistic regression) and subject as a random effect
m_gamma = glmer(Class ~ Pleft * NumClus + (1|subj_id), data=eye, family = binomial(link = logit))
summary(m_gamma)



















