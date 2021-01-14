
emote <- read.csv("a 111/emotion.csv")
emote[1:10,]
cor(emote)

# why repeated measures?

# most, but not quite all, repeated measures anova require a 'long' format
# to date, we have used the 'wide' format, everything belonging to a subject on one line

# I use melt() and cast() from reshape2.  can use gather() and spread() from tidyr
# but syntax is wee bit different

library(reshape2)
emote_long <- melt(emote, id = c("subj","group"), measured = c("anger", "fear", "neutral", "ribald"))
names(emote_long) <- c("subj","group", "emotion", "galvanic")
emote_long <- emote_long[order(emote_long$subj),] 
emote_long[1:10,]

## Convert variables to factor
emote_long <- within(emote_long, {
  emotion <- factor(emotion)
  group <- factor(group)
  subj <- factor(subj)
})


hist(emote_long$galvanic,20, col = "pink")

boxplot(emote_long$galvanic ~ emote_long$group*emote_long$emotion)
boxplot(emote_long$galvanic ~ emote_long$emotion)
boxplot(emote_long$galvanic ~ emote_long$group)

aggregate(galvanic ~ group * emotion, emote_long, mean )

# different ways of analyzing repeated measures anova  (must specify contrasts!!!!!)

neg_vs_other <- c(1,1,-1,-1)
ang_vs_fear <- c(1,-1,0,0)
neu_vs_happy <- c(0,0,1,-1)
contrasts(emote_long$emotion) <- cbind(neg_vs_other,ang_vs_fear,neu_vs_happy)
contrasts(emote_long$group) <- contr.sum(2)

# do analysis using aov
library(car)
emo <- aov(galvanic ~ group * emotion + Error(subj), data = emote_long)   # uses Type II SS, can't change
summary(emo)
summary.lm(emo)
table(emote_long$subj)

# eta^2 becomes partial eta^2

# now for the good stuff, modelling the covariance structure
mat <- with(emote_long, matrix(c(galvanic[emotion=="anger"], galvanic[emotion=="fear"],
                                 galvanic[emotion=="neutral"], galvanic[emotion=="happy"]), ncol = 4))
var(mat)  # let's look at var/cov matrix
cor(mat)  # look at correlations

# redo analyses using ezANOVA (gives us Mauchly's test, and GG and HF corrections)
library("ez")
emo.ez <- ezANOVA(data = emote_long, dv = .(galvanic), wid = .(subj),
                  between = .(group), within = .(emotion), type = 3, detailed = T, return_aov = TRUE)
print(emo.ez)

1-pf(3.4753966, df1=1.803951, df2=32.47112) 

# gonna try a new package
library(MANOVA.RM)

k_mod <- RM(galvanic ~ group*emotion, data = emote_long, subject = "subj", no.subf = 1, iter = 1000, 
            resampling = "paramBS", CPU = 1, seed = 1234)
summary(k_mod)

# truly bizarre code for going the multivartiate approach to repeated measures
# source -- http://rtutorialseries.blogspot.com/2011/02/r-tutorial-series-one-way-repeated.html
# also provides all of the stuff that ezANOVA does
# need to use the wide version of emotion
library(car)
emo_levels <- c("anger", "fear", "neutral", "happy")
emofactor <- as.factor(emo_levels)
contrasts(emofactor) <- cbind(neg_vs_other,ang_vs_fear,neu_vs_happy)
emo_frame <- data.frame(emofactor)
emo_bind <- cbind(emote$anger,emote$fear,emote$neutral,emote$happy)
emo_model <- lm(emo_bind ~ emote$group)
analysis <- Anova(emo_model, idata = emo_frame, idesign = ~emofactor)
summary(analysis)

# now for the good stuff, modelling the covariance structure
mat <- with(emote_long, matrix(c(galvanic[emotion=="anger"], galvanic[emotion=="fear"],
                                 galvanic[emotion=="neutral"], galvanic[emotion=="happy"]), ncol = 4))
var(mat)  # let's look at var/cov matrix
cor(mat)  # look at correlations

# do analyses using gls()
library(nlme)
# "don't see the point of this"    longg <- groupedData(galvanic ~ as.numeric(group) * as.numeric(emotion) | subj, data = emote_long)
fit.cs <- gls(galvanic ~ group * emotion, data = emote_long, corr = corCompSymm(, form= ~ 1 | subj), method = "ML" )
summary(fit.cs)
anova(fit.cs)
anova(fit.cs,type = "marginal")   # to be preferred (similar to SS Type = III)
anova(fit.cs,type = "sequential")
coef(fit.cs)
plot(fit.cs)

fit.ar1 <- gls(galvanic ~ group * emotion, data = emote_long, corr = corAR1(, form= ~ 1 | subj), method = "ML" )
summary(fit.ar1)
anova(fit.ar1)
anova(fit.ar1,type = "marginal")
anova(fit.cs,fit.ar1)

fit.ar1un <- gls(galvanic ~ group * emotion, data = emote_long, corr = corAR1(, form= ~ 1 | subj), weights = varIdent(form = ~ 1 | emotion), method = "ML" )
summary(fit.ar1un)
anova(fit.ar1un)
anova(fit.ar1un,type = "marginal")
anova(fit.ar1,fit.ar1un)

fit.un <- gls(galvanic ~ group * emotion, data = emote_long, corr=corSymm(form = ~ 1 | subj),
                 weights = varIdent(form = ~ 1 | emotion), method = "ML" )
summary(fit.un)
anova(fit.un)
anova(fit.ar1un,fit.un)

# __________________________________________________________________________________________________
