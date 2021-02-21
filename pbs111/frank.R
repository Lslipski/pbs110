# go over datasets for homework 7

#
frank <- read.csv("a 111/frank_eyemovedata.csv")
frank[1:10,]
attach(frank)

y <- cbind(NumClus,FstRtnFix,S1,S2,Pleft,NumFix)

manova.fit <- manova(y ~ Class)              # check for overall significance
summary(manova.fit, test = "Wilks")
summary.aov(manova.fit)                      # look for sig of individual predictors

#must install and load MASS to use lda() and klaR to use greedy.wilks
library("MASS")
library("klaR")

# If you decide to run this, your results will depend on split into groups and may differ from mine

frank$t <- rbinom(length(frank$S1),1,.5)  # generate a random binary variable to select traing and test samples
frank_train <- subset(frank, t == 1)
frank_holdout <- subset(frank, t == 0)

# use stepwise selection to choose predictors ( step_obj and greedy.wilks are two different techniques)

frank_d <- frank_train[,1:6]  # the data  
cor(frank_d)
frank_c <- frank_train[,7]    # the classes 

step_obj <- stepclass(frank_d, frank_c, "lda", improvement = .02)

step_objform =  stepclass(Class ~ NumClus + FstRtnFix + S1 + S2 + Pleft + NumFix, data = frank_train, "lda", improvement = .02)

stepmodel <- greedy.wilks(Class ~ NumClus + FstRtnFix + S1 + S2 + Pleft + NumFix, data = frank_train,niveau = 0.2 )
stepmodel

# we can plot group by IV scatterplot.  In an ideal world, upper left would be all green
#    and lower right would be all red
library(ggplot2)
ggplot(frank_train,aes(NumClus,Pleft,colour=Class))+geom_point()  # ain't very pretty


# first you must enter the variables from greedy wilks or whatever in equation below
# let's get weights for LDA on training sample
lda.TrainT <- lda(Class ~ NumClus + Pleft, data = frank_train, na.action = na.omit, CV = T)  

# these routines show how well we did on training sample
ct <- table(frank_train$Class, lda.TrainT$class)    # this output uses leave-one-out
diag(prop.table(ct, 1))                             # first % is sensitivity, next is specificity
# total percent correct
sum(diag(prop.table(ct))) 

0.7142857*77
binom.test(55, 77, p = 0.5)   #prediction is statistically better than chance, but!!!!


# CV = T or F determines whether leave-out-one is used, some things only work with one or the other

# crucial to choose predictors on training sample alone

lda.trainF <- lda(Class ~ NumClus + Pleft, data = frank_train, na.action = na.omit, CV = F)  #for predict only
no_loo <- predict(lda.trainF, newdata = frank_holdout)

ct <- table(frank_holdout$Class, no_loo$class)       # this output doesn't use leave-one-out
diag(prop.table(ct, 1))                            # first % is sensitivity, next is specificity
# total percent correct
sum(diag(prop.table(ct))) 


#three ways of graphing ROC curve to assess quality of prediction
# must install and load ROCR and pROC

library("ROCR")
pred <- prediction(no_loo$posterior[,2], frank_holdout$Class)
perf <- performance(pred,"tpr","fpr")
plot(perf, colorize = TRUE)
#compute area under the ROC curve
attributes(performance(pred, "auc"))$y.values[[1]]

#trying a new package, pROC (which I like much better)
library("pROC")
roc_lda <-roc(frank_holdout$Class,no_loo$posterior[,2], ci=T, plot = T)
roc_lda
ci(roc_lda)

plot.roc(frank_holdout$Class,no_loo$posterior[,2], type="b", pch=21, col="blue", bg="grey")
# Plotting a shape. We need more
ci.sp.obj <- ci.sp(roc_lda, sensitivities=seq(0, 1, .01), boot.n=100)
plot(roc_lda) # restart a new plot
plot(ci.sp.obj, type="shape", col="blue")


# demonstration of strong need to find both which variables to use and their weights
# on training sample only

rand <- read.csv("a 111/random discriminant.csv")
rand

rand.d <- rand[,2:21]  # the data    
rand.c <- rand[,1]    # the classes 
step_obj <- stepclass(rand.d, rand.c, "lda", improvement = .02)
step_obj

lda.randT <- lda(group~iv2, data = rand, CV = T)
lda.randF <- lda(group~iv2, data = rand, CV = F)

no_loo <- predict(lda.randF, newdata = rand)

# Assess the accuracy of the prediction            #no leave-out-one
# percent correct for each category of G
ct <- table(rand$group, no_loo$class)
diag(prop.table(ct, 1))
# total percent correct
sum(diag(prop.table(ct)))

# Assess the accuracy of the prediction            #with leave-out-one
ct <- table(rand$group, lda.randT$class)
diag(prop.table(ct, 1))
# total percent correct
sum(diag(prop.table(ct)))


# what if we use all 20 predictors w and w/o loo

rand.fitT <- lda(group ~ iv1+iv2+iv3+iv4+iv5+iv6+iv7+iv8+iv9+iv10+iv11+iv12+iv13+iv14+iv15+iv16+iv17+iv18, data = rand, na.action = na.omit, CV = T)
lda.fitF <- lda(group ~ iv1+iv2+iv3+iv4+iv5+iv6+iv7+iv8+iv9+iv10+iv11+iv12+iv13+iv14+iv15+iv16+iv17+iv18, data = rand, na.action = na.omit, CV = F)
rand_all_no_loo <- predict(lda.fitF, newdata = rand)


# Assess the accuracy of the prediction            #leave-out-one  (not so great!)
# percent correct for each category of G
ct <- table(rand$group, rand.fitT$class)
diag(prop.table(ct, 1))
# total percent correct
sum(diag(prop.table(ct)))

# Assess the accuracy of the prediction    #no leave out one   (perfect!!)
# percent correct for each category of G
ct <- table(rand$group, rand_all_no_loo$class)
diag(prop.table(ct, 1))
# total percent correct
sum(diag(prop.table(ct)))


#now try using logistic regression to set the weights

library(sjPlot)
library(sjmisc)
# need to make class variable 0,1 instead of F,U
frank_train$ClassNum <- ifelse(frank_train$Class == "F",1,0)
frank_holdout$ClassNum <- ifelse(frank_holdout$Class == "F",1,0)
logit.model <- glm(ClassNum ~ NumClus + Pleft, data = frank_train, family = binomial())
summary(logit.model)
tab_model(logit.model)

hist(fitted(logit.model),30)
predict(logit.model, type="response")

frank_train$prob <- fitted(logit.model)
frank_train$pred <- ifelse(frank_train$prob > .5, 1,0)

ct <- table(frank_train$ClassNum, frank_train$pred)
diag(prop.table(ct, 1))
# total percent correc
sum(diag(prop.table(ct)))

#graph ROC curve to assess quality of prediction
# must install and load ROCR
pred <- prediction(fitted(logit.model), frank_train$ClassNum)
perf <- performance(pred,"tpr","fpr")
plot(perf, colorize = T)

frank_holdout$prob <- predict(logit.model, frank_holdout, type = "response")

frank_holdout$pred <- ifelse(frank_holdout$prob > .5, 1,0)

ct <- table(frank_holdout$ClassNum, frank_holdout$pred)
diag(prop.table(ct, 1))
# total percent correc
sum(diag(prop.table(ct)))



