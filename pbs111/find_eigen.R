


cormatrix <- matrix(c(1,.85,.5,0,0,.85,1,.55,0,.05,.5,.55,1,.6,.72,0,0,.6,1,.77,0,.05,.72,.77,1), nrow = 5)

eigen_base <- matrix(c(.5,.5,.5,.5,.5), nrow = 5)
ones <- matrix(c(1,1,1,1,1), nrow = 1)

x1 <- cormatrix %*% eigen_base
eigen1 <- 1/1.685 * x1

x2 <- cormatrix %*% eigen1
eigen2 <- 1/2.713145 * x2
d <- abs(eigen1 - eigen2)
distance <- (ones %*% d)/5

x3 <- cormatrix %*% eigen2
eigen3 <- 1/2.682190 * x3
d <- abs(eigen2 - eigen3)
distance <- (ones %*% d)/5

# length of eigenvector = sqrt of sum of squared elements in eigen3
len <- sqrt(t(eigen3) %*% eigen3)

eigen <- 1/len[1,1] * eigen3


#eigen value = (eigen' * cormatrix * eigen)
corx = cormatrix %*% eigen
eigenvalue = t(eigen) %*% corx
eigenvalue

percent_var = eigenvalue[1,1]/5
percent_var

factor1_loadings <- sqrt(eigenvalue[1,1]) * eigen
factor1_loadings

(2.68 + 1.79)/5

library(psych)
fa.parallel(cormatrix)
full.fit <- principal(cormatrix, nfactors=5, rotate = "none")
print(full.fit)
full.fit$values
fit.two.norot <- principal(cormatrix, nfactors=2, rotate = "none")
print.psych(fit.two.norot)

fit.two.rot <- principal(cormatrix, nfactors=2, rotate = "varimax")
print.psych(fit.two.rot)

# Plot variable space w/o rotation
xu = c(.61,.64,.95,.68,.74)
yu = c(.74,.72,-.05,-.62,-.6)
plot(xu,yu)

# Plot variable space with rotation
xr = c(.01,.05,.77,.91,.95)
yr = c(.95,.96,.56,-.05,.0)
plot(xr,yr)

