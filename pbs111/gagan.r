# two functions for accessing how well a PCA is likely to work (Of course, R functions for those are available)

kmof <- function(x) {
  x <- subset(x, complete.cases(x))     # Omit missing values
  r <- cor(x)                           # Correlation matrix
  r2 <- r^2                             # Squared correlation coefficients
  i <- solve(r)                         # Inverse matrix of correlation matrix
  d <- diag(i)                          # Diagonal elements of inverse matrix
  p2 <- (-i/sqrt(outer(d, d)))^2        # Squared partial correlation coefficients
  diag(r2) <- diag(p2) <- 0             # Delete diagonal elements
  KMO <- sum(r2)/(sum(r2)+sum(p2)) 
  MSA <- colSums(r2)/(colSums(r2)+colSums(p2)) 
  return(list(KMO=KMO, MSA=MSA))
}

Bartlett.sphericity.test <- function(x) {
  method <- "Bartlett’s test of sphericity" 
  data.name <- deparse(substitute(x))
  x <- subset(x, complete.cases(x))     # Omit missing values 
  n <- nrow(x) 
  p <- ncol(x) 
  chisq <- (1-n+(2*p+5)/6)*log(det(cor(x))) 
  df <- p*(p-1)/2 
  p.value <- pchisq(chisq, df, lower.tail=FALSE) 
  names(chisq) <- "X-squared" 
  names(df) <- "df" 
  return(structure(list(statistic=chisq, parameter=df, p.value=p.value,
                    method=method, data.name=data.name), class="htest"))
}


wig <- read.csv("a 111/gagan.csv")
wig[1:10,]
summary(wig)
hist(wig$full_iq,20,col = "orange")

# Use homemade functions
kmof(wig)                                 # would prefer values > .8 but you get what you get
Bartlett.sphericity.test(wig)            # really want the data to violate sphericity
cor(wig)                                 # look at raw correlations among measures                            

# use already available functions
library(psych)
KMO(wig)              # fairly close to homemade function
cortest.bartlett(wig)  # not sure if same as above

# Principal Components Analysis
# entering raw data and extracting PCs 
# from the correlation matrix 

fa.parallel(wig)                         # show scree plot


# princomp() is a built-in (base) function.  I don't like it.  principal() is from 'psych' and is easier to use
wig.all <- princomp(wig, cor=TRUE)         # i recommend skipping this section (so why do I have it here!!) 
summary(wig.all) # print variance accounted for 
wig.all$sdev^2
loadings(wig.all) # pc loadings 
plot(wig.all,type="lines") # scree plot 
wig.all$scores # the principal components

# let's use principal()
# Varimax Rotated Principal Components
# retaining 4 components 
wig.four <- principal(wig, nfactors=4, rotate="varimax")  #four factors had eigenvalues > 1
wig.four # print results
wig.four$values  # eigenvalues  

# the value indicates how many factors a particular item is related to
# ideally the c score would be close to 1
complexity = sum(.21^2 + .03^2 + .86^2 + .04^2)^2/sum(.21^4 + .03^4 + .86^4 + .04^4) # who knew

# factor scores are each subject's score on each factor
wig.four$scores                          # show the 4 factor scores for each subject
wig.four$scores[,3]                      # these are the memory factor scores used in pub
mean(wig.four$scores[,3])                # should be close to 0
cor.test(wig.four$scores[,3],wig$face_nm_cor)  # should be high
cor(wig.four$scores)  # correlations should be essentially 0

wig <- read.csv("a 111/gagan_newlabels.csv")  # I just shortened variable names

# cool graphs that help clarify which components load on which factors
fa.diagram(wig.four)

library("qgraph")

floadings <- loadings(wig.four)
qgraph.loadings(floadings,rotation="varimax",minimum=0.3,
                cut=0.4,vsize=c(5.0,15),borders=T,vTrans=200, label.cex= 2)

biplot(wig.four)












