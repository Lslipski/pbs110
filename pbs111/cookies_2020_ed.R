cookies <- read.csv("a 111/cookie_2020.csv")  #read in cookie data
cookies[,2:13] <- 11 - cookies[,2:13]  #change from similarities to distances

# create distance matrix -- use only one of the d statements below
cookie_d_euc <- dist(cookies[,2:13], method = "euclidean") # distance matrix
cookie_d_nat <- as.dist(cookies[,2:13])  #leave matrix as it stands

# Nonmetric MDS
# N rows (objects) x p columns (variables)
# each row identified by a unique row name
library(MASS)
fit1 <- isoMDS(cookie_d_nat, k=1) # k is the number of dim --- let's see the one-dimensional solution
fit1 # view results
# plot solution 
x <- fit1$points[,1]
y <- rep(0,12)  #  "kluge"
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", main="Nonmetric MDS", type="n")
text(x, y, labels = c("oatcrumapp","petit-eco","caramelapp","gingerman",
                      "tahoe","raspberry","shortbread","milano",
                      "choc_chip","peanutbutter","geneva","strawberry_cheese"), cex=.7)

fit2 <- isoMDS(cookie_d_nat, k=2) # k is the number of dim  ---- let's see the two-dimensional solution
fit2 # view results
# plot solution 
x <- fit2$points[,1]
y <- fit2$points[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", 
     main="Nonmetric MDS", type="n")
text(x, y, labels = c("oatcrumapp","petit-eco","caramelapp","gingerman",
                      "tahoe","raspberry","shortbread","milano",
                      "choc_chip","peanutbutter","geneva","strawberry_cheese"), cex=.7)

s <- rep(0,11)
for (i in 1:11) {
  fit <- isoMDS(cookie_d_nat, k=i) # k is the number of dim  ---- let's see the two-dimensional solution
  fit # view results
  s[i] <- fit$stress}
plot(1:11,s,xlab = " Number of dimensions", ylab = "Stress")   # plot stress ~ # of dimensions 

fit3 <- isoMDS(cookie_d_nat, k=3) # k is the number of dim  ---- let's see the two-dimensional solution
fit3 # view results
# plot solution 
x <- fit3$points[,1]
y <- fit3$points[,3]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", 
     main="Nonmetric MDS", type="n")
text(x, y, labels = c("oatcrumapp","petit-eco","caramelapp","gingerman",
                      "tahoe","raspberry","shortbread","milano",
                      "choc_chip","peanutbutter","geneva","strawberry_cheese"), cex=.7)


# Ward Hierarchical Clustering
fit <- hclust(cookie_d_nat, method="ward.D") 
plot(fit) # display dendogram
groups <- cutree(fit, k=3) # cut tree into 5 clusters
# draw dendogram with red borders around the 3 clusters 
rect.hclust(fit, k=3, border="red")

# Ward Hierarchical Clustering with Bootstrapped p values
library(pvclust)
fit <- pvclust(cookies[,2:13], method.hclust="ward.D",
               method.dist="euclidean")
plot(fit) # dendogram with p values
# add rectangles around groups highly supported by the data
pvrect(fit, alpha=.95)


