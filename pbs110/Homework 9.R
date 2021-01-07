#(1)
room = read.csv('data_files//hw9_q1.csv')
head(room)

boxplot(anxiety ~ color, data=room)
boxplot(anxiety ~ size, data=room)

room$size = as.factor(room$size)
room$color = as.factor(room$color)

# set size contrasts
size_linear = c(-1, 0, 1)
size_quadratic = c(-1, 2, -1)
#check orthogonality
print(sum(size_linear * size_quadratic))

# set color contrasts
GB_v_RY = c(-1, -1, 1, 1)
GB_v_none = c(-1, 1, 0, 0)
RY_v_none = c(0, 0, -1, 1)
#check orthogonality
print(sum(GB_v_RY*GB_v_none*RY_v_none))

#assign contrasts to variables
contrasts(room$size) = cbind(size_linear, size_quadratic)
contrasts(room$color) = cbind(GB_v_RY, GB_v_none, RY_v_none)

# run one way test on size
oneway.test(anxiety ~ size, data=room)

# run one way test on color
oneway.test(anxiety ~ color, data=room)

#run anova using both predictor variables
model_aov = aov(anxiety ~ color*size, data = room)
summary(model_aov)

library(car)
Anova(model_aov, type=3)

#total SS
ss = 30974.63 + 563.95 + 3159.72 + 7784.67

color_eta2 = 11881.00 / ss
size_eta2 = 130.67 / ss
int_eta2 = 3159.72 / ss

print(color_eta2)
print(size_eta2)
print(int_eta2)

lm_model = lm(anxiety ~ color*size, data = room)
summary(lm_model)

#(2)
arrow = read.csv('data_files//arrows.csv')
head(arrow)
dim(arrow)

#first plot pntrt by the variables of interest
par(mfrow=c(3,2))
plot(pntrt ~ arrowhead, data=arrow)
plot(pntrt ~ clothFit, data=arrow)
plot(pntrt ~ clothType, data=arrow)

arrow$arrowhead = as.factor(arrow$arrowhead)
arrow$clothFit = as.factor(arrow$clothFit)
arrow$clothType = as.factor(arrow$clothType)

bullet_v_arrow = c(-2, 1, 1)
contrasts(arrow$arrowhead) = cbind(bullet_v_arrow)

#run linear model to test hypothesis 1
bullet_arrow_model = lm(pntrt ~ arrowhead + clothFit + clothType, data=arrow)
summary(bullet_arrow_model)

# run anova as well
bullet_arrow_aov = aov(pntrt ~ clothFit*clothType, data= arrow)
Anova(bullet_arrow_aov, type=3)

