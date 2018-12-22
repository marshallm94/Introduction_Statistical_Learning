suppressPackageStartupMessages(library(ggplot2))

################################################################################
######################## PRINCIPAL COMPONENTS ANALYSIS #########################
################################################################################

states <- rownames(USArrests)
states
names(USArrests)

apply(USArrests, 2, mean)
apply(USArrests, 2, var)

# get principal components
pr.out <- prcomp(USArrests, scale. = TRUE)
names(pr.out)

# original attributes mean and standard deviation
pr.out$center
pr.out$scale

# although the output of prcomp() stores the values of the data projected onto
# the principal components, it is shown below that after scaling the original
# data set, one can perform matrix multiplication between the principal 
# component loadings and the scaled data set to obtain the same result
# 
# Note: Looking at the equation for the first principal component on page 375 
# (hardcopy) of ISLR, it is shown that x_i,j of the principal component matrix
# is a linear combination (aka dot product - essence of matrix multiplication)
# of the loadings for each principal component and the values of the ith
# observation of the original data set. 

pc.matrix <- matrix(pr.out$rotation, nrow = 4, ncol = 4)
scaled.x.matrix <- as.matrix(scale(USArrests))

(scaled.x.matrix %*% pc.matrix == pr.out$x)

# for a more concrete example, one can take the first column of pr.out$rotation
# and perform the sum of the element wise products of the values of the first 
# observation of the scaled data set and the loading for the first principal 
# components, shown below:

pc1.loadings <- pc.matrix[,1]
first.observation <- scaled.x.matrix[1,]

first.value <- 0
for (i in 1:length(pc1.loadings)) {
    first.value <- first.value + (pc1.loadings[i] * first.observation[i])
}

# showing that they are all equal:
first.value
pr.out$x[1,1]
(pc1.loadings %*% first.observation)

# plot of first two principal components
biplot(pr.out, scale = 0)

pr.out$rotation <- -pr.out$rotation
pr.out$x <- -pr.out$x
biplot(pr.out, scale = 0)

# standard deviation of of each principal component
pr.out$sdev

pr.var <- pr.out$sdev^2
pr.var

# proportion of variance explained
pve <- pr.var/sum(pr.var)
pve

ggplot(data.frame(y = pve,
                  x = 1:length(pve),
                  z = cumsum(pve)), aes(x, y, color = 'PVE')) +
    geom_line() +
    geom_line(aes(y = z, color = "Cumulative PVE")) +
    scale_y_continuous(labels = scales::percent) +
    theme(legend.title = element_blank()) +
    xlab("Principal Component Number") +
    ylab("Variance Explained") +
    ggtitle("Variance Explained with Principal Components")

################################################################################
################################### K-MEANS ####################################
################################################################################

# data setup
set.seed(2)
x <- matrix(rnorm(50*2), ncol = 2)
x[1:25, 1] <- x[1:25, 1] + 3
x[1:25, 2] <- x[1:25, 2] - 4

# k-means with k=2
km.out <- kmeans(x, 2, nstart = 20)
km.out$cluster

display.df <- data.frame(x = x,
                         y = factor(km.out$cluster,
                                    levels = c(1, 2)))

ggplot(display.df, aes(x.1, x.2, color = y)) +
    geom_point() +
    labs(color = "Cluster\n") +
    xlab("X1") +
    ylab("X2") +
    ggtitle("K-Means where K=2")

# k-means with k=3
set.seed(4)
km.out <- kmeans(x, 3, nstart = 20)
km.out

display.df <- data.frame(x = x,
                         y = factor(km.out$cluster,
                                    levels = c(1, 2, 3)))

ggplot(display.df, aes(x.1, x.2, color = y)) +
    geom_point() +
    labs(color = "Cluster\n") +
    xlab("X1") +
    ylab("X2") +
    ggtitle("K-Means where K=3")

################################################################################
######################### HIERARCHICAL CLUSTERING ##############################
################################################################################

# compute all pairswise distances of observations
hc.complete <- hclust(dist(x), method = 'complete')
hc.average <- hclust(dist(x), method = 'average')
hc.single <- hclust(dist(x), method = 'single')

# plot dendrograms
par(mfrow = c(1,3))
plot(hc.complete, main = 'Complete Linkage', xlab = "", sub = "", cex = 0.9)
plot(hc.average, main = 'Average Linkage', xlab = "", sub = "", cex = 0.9)
plot(hc.single, main = 'Single Linkage', xlab = "", sub = "", cex = 0.9)

# "divide" the dendrogram into 2 groups
cutree(hc.complete, 2)
cutree(hc.average, 2)
cutree(hc.single, 2)

xsc <- scale(x)
par(mfrow = c(1,1))
plot(hclust(dist(xsc), method = 'complete'),
     main = "Hierarchical Clustering with Scaled Features")

x <- matrix(rnorm(30 * 3), ncol = 3)
# compute the distance using one minuse the correlation of the transpose of X
dd <- as.dist(1 - cor(t(x)))
plot(hclust(dd, method = 'complete'),
     main = "Complete Linkage with Correlation-Based Distance",
     xlab = "", sub = "")

################################################################################
############################# NC160 DATA EXAMPLE ###############################
################################################################################

suppressPackageStartupMessages(library(ISLR))

# cancer cell gene expression data
nci.labs <- NCI60$labs
nci.data <- NCI60$data

dim(nci.data)
table(nci.labs)

pr.out <- prcomp(nci.data, scale = TRUE)

cols <- function(vec) {
    cols <- rainbow(length(unique(vec)))
    return(cols[as.numeric(as.factor(vec))])
}

# plot of principal components
par(mfrow = c(1,2))
plot(pr.out$x[,1:2], col=cols(nci.labs), pch = 19, xlab = "Z1", ylab = "Z2")
plot(pr.out$x[,c(1,3)], col=cols(nci.labs), pch = 19, xlab = "Z1", ylab = "Z2")

summary(pr.out)
par(mfrow = c(1,1))
plot(pr.out)

pve <- pr.out$sdev^2/sum(pr.out$sdev^2)
ggplot(data.frame(y = pve,
                  x = 1:length(pve),
                  z = cumsum(pve)), aes(x, y, color = 'PVE')) +
    geom_line() +
    geom_line(aes(y = z, color = "Cumulative PVE")) +
    scale_y_continuous(labels = scales::percent) +
    theme(legend.title = element_blank()) +
    xlab("Principal Component Number") +
    ylab("Variance Explained") +
    ggtitle("Variance Explained with Principal Components")

sd.data <- scale(nci.data)

# hierarchical clustering of N160 data
par(mfrow = c(1, 3))
data.dist <- dist(sd.data)

plot(hclust(data.dist, method = 'complete'),
     labels = nci.labs,
     main = 'Complete Linkage',
     xlab = '',
     ylab = '',
     sub = '')

plot(hclust(data.dist, method = 'average'),
     labels = nci.labs,
     main = 'Average Linkage',
     xlab = '',
     ylab = '',
     sub = '')

plot(hclust(data.dist, method = 'single'),
     labels = nci.labs,
     main = 'Single Linkage',
     xlab = '',
     ylab = '',
     sub = '')


hc.out <- hclust(dist(sd.data), method = 'complete')
hc.clusters <- cutree(hc.out, 4)
table(hc.clusters,  nci.labs)

par(mfrow = c(1,1))
plot(hc.out, labels = nci.labs)
abline(h=139, col = 'red')

# K-Means on N160 data
set.seed(2)
km.out <- kmeans(sd.data, 4, nstart = 20)
km.clusters <-  km.out$cluster
table(km.clusters, hc.clusters)

hc.out <- hclust(dist(pr.out$x[,1:5]))
plot(hc.out,
     labels = nci.labs,
     main = 'Hier. Clust. on First Five Principal Components')
table(cutree(hc.out, 4), nci.labs)
