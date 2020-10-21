#(1)
resil = read.csv('data_files//resil_data_cleaned.csv')

head(resil)

dim(resil)

# first create a logistic model to predict depression_yn from ptsd and total_traumas
logmodel_1 = glm(depression_yn ~ PTSD + total_traumas, data=resil, family=binomial(logit))
summary(logmodel_1)

# now create logistic model to predict depression_yn including TOTAL_RESILIENCY to compare to original model
logmodel_2 = glm(depression_yn ~ PTSD + total_traumas + TOTAL_RESILIENCY, data=resil, family=binomial(logit))
summary(logmodel_2)

# use stepAIC to determine if adding the total_resiliency improves the model.
library(MASS)
logistic_AIC = glm(depression_yn ~ PTSD + total_traumas + TOTAL_RESILIENCY, data=resil, family=binomial(logit))
log_step = stepAIC(logistic_AIC, direction='both')
log_step$anova


(2)
mt = read.csv('data_files//minnesota_twins.csv')
head(mt)
dim(mt)

#mycors = cor(na.omit(myvars))
mycors = cor(na.omit(mt))
#plot corr matrix
library(corrplot)
corrplot(mycors, method='circle')

# create var for determining holdout data
mt$r = rbinom(nrow(mt),1, .5)

# determine the model parameters on only those rows where r = 1
library(MASS)
model_train1 = lm(vocab ~ as.factor(sex) + as.factor(zygosity) + moed + faed + faminc, r ==1, data=mt)
step = stepAIC(model_train1, direction='both')
step$anova

# train the model with only parameters of interest on rows where r==1
model_train_final = lm(vocab ~ moed + faed + faminc, r==1, data=mt)
summary(model_train_final)

# now test on the holdout data and see how model performs
mt_holdout = subset(mt, r==0)
y = predict(model_train_final, mt_holdout)
model = lm(vocab ~ y, data=mt_holdout)
summary(model)

