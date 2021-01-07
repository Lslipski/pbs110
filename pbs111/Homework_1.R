library(car)
library(emmeans)  

##### 1
reed = read.csv('data_files//read2007.csv')
head(reed)

# Compute 'readgrade' (overall reading ability) which is average of word_id and word_atk
reed['readgrade'] = rowMeans(reed[c("word_id","word_atk")])
head(reed)

hist(reed$readgrade)

# Divide the students into three groups with rougly equal numbers in poor, average, and good based on readgrade (you can use two groups if you prefer)
# 1 = poor; 2 = average; 3 = good
library(ggplot2)
reed$rgroup = as.numeric(cut_number(reed$readgrade,3))
table(reed$rgroup)
head(reed)

#Carry out a three-group (or two group) ANOVA with snd_same as the DV
reed$rgroup = as.factor(reed$rgroup)
size_linear = c(-1, 0, 1)
size_quadratic = c(-1, 2, -1)
#check orthogonality
print('Zero:')
print(sum(size_linear * size_quadratic))

#assign contrasts to variables
contrasts(reed$rgroup) = cbind(size_linear, size_quadratic)

# run one way test on color
anova1 = aov(snd_same ~ rgroup, data=reed)
anova1


Anova(anova1, type="III")
emmeans(anova1, "rgroup")

# Now compare that three-group ANOVA to doing a
# simple regression using snd_same as DV and readgrade as the predictor variable.
reg = lm(snd_same ~ readgrade, data=reed)
summary(reg)

###### 2
# see if iq differs by group
x = aov(reed$iq ~ reed$rgroup)
summary(x)


# ANOVA with IQ as covariate
# run one way test on color

anova_iq = aov(snd_same ~ iq + rgroup, data=reed)
Anova(anova_iq, type="III")

# calculate adjusted means
#install.packages('emmeans')
emmeans(anova_iq, "rgroup")

# regression with IQ as covariate
reg_iq = lm(snd_same ~ iq + readgrade, data=reed)
summary(reg_iq)

##### 3
# dvs: snd_same, cat_sam, look_sam, mean_sam
# IV: rgroup
reed$rgroup = as.factor(reed$rgroup)
dvs = cbind(reed$snd_same, reed$cat_sam, reed$look_sam, reed$mean_sam)

man_fit = manova(dvs ~ rgroup, data=reed)

summary(man_fit, test="Wilks")

summary.aov(man_fit)



























