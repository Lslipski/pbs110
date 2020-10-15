datadir = 'data_files/'

twins = read.csv(file.path(datadir,'/minnesota_twins.csv'))
head(twins)

# add a within pair twin identifier
twins$pair_id = rep(c(0,1), dim(twins)[1]/2)

#keep only relevent vars
col_keep = c("pairnum", "pair_id", "zygosity", "math")
twins = twins[col_keep]
dim(twins)
head(twins)


# get difference in math scores between each pair of twins
df.wide <- reshape(twins, direction='wide', idvar='pairnum', timevar='pair_id')
df.wide$math_diff = abs(df.wide$math.0 - df.wide$math.1)
df.wide['rowmax'] = rowmax = ifelse(df.wide$math.0 > df.wide$math.1, df.wide$math.0, df.wide$math.1)
df.wide['rowmin'] = rowmax = ifelse(df.wide$math.0 < df.wide$math.1, df.wide$math.0, df.wide$math.1)

#dim(df.wide)
head(df.wide)

t.test(df.wide$rowmax, df.wide$rowmin, paired= TRUE, alternative="two.sided")

# make sure one sample and 2 sample give same answer
t.test(df.wide$math_diff, mu=0)

# make sure both methods of calculating math score difference give same answer
diff_model = lm(df.wide$math_diff ~ 1)
summary(diff_model)

#use linear model to determine if zygosity affects relationship
#same linear model as above, but with zygosity as a predictor
diff_model_zyg = lm(math_diff ~ zygosity.0, df.wide)
summary(diff_model_zyg)


boxplot(math_diff ~ zygosity.0, df.wide)

bank = read.csv(file.path(datadir,'bank_salaries.csv'))
head(bank)

# plot salbeg by sex
boxplot(salbeg ~ sex, notch=TRUE, bank)

#t-test of the 2 means (men and women), not controlling for edlevel and age
t.test(salbeg ~ sex, bank)

# first create interaction terms
int_sex_edlevel = scale(bank$sex) * scale(bank$edlevel)
int_sex_age = scale(bank$sex) * scale(bank$age)

# create linear model and control for sex
lm_salbeg = lm(salbeg ~ age + edlevel + sex + int_sex_edlevel + int_sex_age, bank)
summary(lm_salbeg)


