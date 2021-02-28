letter <- read.csv("a 111/letter.csv")  #read in my old letter data

# create distance matrix -- use only one of the d statements below
d_euc <- dist(letter[,2:21], method = "euclidean") # create euclidean distance matrix
d_nat <- as.dist(letter[,2:21])  #leave sorting matrix as it stands

# Nonmetric MDS
# N rows (objects) x p columns (variables)
# each row identified by a unique row name
library(MASS)
fit_nat1 <- isoMDS(d_nat, k=1) # k is the number of dim --- let's see the one-dimensional solution
fit_nat1 # view results
# plot solution 
x <- fit_nat1$points[,1]
y <- rep(0,20)  #  "kluge"
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", 
     main="Nonmetric MDS", type="n")
text(x, y, labels = c("B","C","D","F","G","H","J","K","L","M","N","P","Q","R","S","T","V","W","X","Z"), cex=.7)

fit_euc1 <- isoMDS(d_euc, k=1) # k is the number of dim --- let's see the one-dimensional solution
fit_euc1 # view results
# plot solution 
x <- fit_euc1$points[,1]
y <- rep(0,20)  #  "kluge"
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", 
     main="Nonmetric MDS", type="n")
text(x, y, labels = c("B","C","D","F","G","H","J","K","L","M","N","P","Q","R","S","T","V","W","X","Z"), cex=.7)

s <- rep(0,18) #stress as f() of # of dimensions
for (i in 1:18) {
  fit <- isoMDS(d_nat, k=i) # k is the number of dim  ---- let's see the i-dimensional solution
  fit # view results
  s[i] <- fit$stress}
plot(1:18,s,xlab = " Number of dimensions", ylab = "Stress")   # plot stress ~ # of dimensions 


#review two-dimensional plot for letters

fit_nat2 <- isoMDS(d_nat, k=2) # k is the number of dim  ---- let's see the two-dimensional solution
fit_nat2 # view results
# plot solution 
x <- fit_nat2$points[,1]
y <- fit_nat2$points[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", 
     main="Nonmetric MDS", type="n")
text(x, y, labels = c("B","C","D","F","G","H","J","K","L","M","N","P","Q","R","S","T","V","W","X","Z"), cex=.7)

# show YB's three dimensional plot

# Anne Krendl used Indscale  (control big for older adults with cog impairments)

# show Andy's indscale


# mds best at long distances, clustering best at short distances

# Ward Hierarchical Clustering
fit_nat <- hclust(d_nat, method="ward.D2") # lots of method options
plot(fit_nat) # display dendogram
groups <- cutree(fit_nat, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters 
rect.hclust(fit_nat, k=5, border="red")

# Ward Hierarchical Clustering with Bootstrapped p values
# constructs its own similarity matrix (a little weird)
library(pvclust)
fit_pv <- pvclust(letter[,2:21], method.hclust="ward.D2",
               method.dist="euclidean")
plot(fit_pv) # dendogram with p values
# add rectangles around groups highly supported by the data
pvrect(fit_pv, alpha=.95)


# Let's get cute'

# link to dendextend package
# https://cran.r-project.org/web/packages/dendextend/vignettes/Cluster_Analysis.html

library(dendextend)
dend <- as.dendrogram(fit_nat)
dend <- color_branches(dend, k=5) # identify 5 clusters as above
plot(dend)
# Requires that the circlize package will be installed
library(circlize)
par(mar = rep(0,4))
circlize_dendrogram(dend)

# used mds and clustering to examine errors for visual vs acoustic confusions

# Many users combine MDS and clustering in one plot


