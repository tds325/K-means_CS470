setwd("C:/Users/Tayler/Documents/470/finalproject")
label.input.filename <- "y_train.txt"
feature.input.filename <- "X_train.txt"

library(ggplot2)
library(data.table)
# convert labels given to vector
label.vec <- as.vector(data.table::fread(label.input.filename)[[1]])
# input features as a data.table
feature.dt <- data.table::fread(feature.input.filename)

# actual file only has 561 columns not 571 - conversion unnecessary
#core.featureset <- 1:561
#core.feature.dt <- feature.dt[,..core.featureset]

# take in two observations and calculate the euclidean distance between them
pairwise_distance <- function(obs1, obs2){
  obs1 = as.vector(obs1[[1]])
  obs2 = as.vector(obs2[[1]])
  sq.result.sum <- sum((obs1 - obs2)^2)
  return(sqrt(sq.result.sum))
} 
# set num of clusters & make random assignment
set.seed(1)
K.clusters <- 5
# take samples of K.clusters random observations and set the initial mean values to the sample values
initial.center.assignment.indices <- sample(length(feature.dt[1,]), K.clusters)
init.assmt.means <- as.vector(feature.dt[initial.center.assignment.indices,])

cluster.assmt.matrix <- matrix(0, nrow = nrow(feature.dt), ncol = K.clusters) 
dist.from.mean.vec <- vector(mode = "numeric", length = K.clusters)
iterations <- 10
for(iter in 1:iterations){
  str(iter)
  print(head(init.assmt.means[1,1:4]))
  print((init.assmt.means[2,1:4]))
  print((init.assmt.means[3,1:4]))
  print((init.assmt.means[4,1:4]))
  # loop through each observation in the data
  for(obs.n in 1:nrow(feature.dt)){
    dist.from.mean.vec = 0
    # for each observation, compute the euclidean distance from each mean value
    for(k in 1:K.clusters){
      dist.from.mean.vec[k] = pairwise_distance(init.assmt.means[k], feature.dt[obs.n])
    }
    # assign the observation to the cluster with the closest mean
    assmt.index <- which.min(dist.from.mean.vec)
    cluster.assmt.matrix[obs.n, ] = 0
    cluster.assmt.matrix[obs.n, assmt.index] = 1
  }
  
  # recompute cluster means
  cluster.mean.list <- list()
  for(k in 1:K.clusters){
    cluster.mean.list [[paste(k)]] <- (colMeans(feature.dt[which(cluster.assmt.matrix[,k]==1),]))
  }
  init.assmt.means <- data.table()
  init.assmt.means <- do.call(rbind, cluster.mean.list)
} 

# returns a data.table of cluster assignments given the assignment matrix
compute_assignment <- function(cluster.matrix){
  assmt.vec <- vector(mode = "numeric",length=nrow(cluster.matrix))
  for(n in 1:length(assmt.vec)){
    assmt.vec[n] = (which(cluster.matrix[n,]==1))
  }
  return(assmt.vec)
}

cluster.assmt.vec <- compute_assignment(cluster.assmt.matrix)
ggplot()+
  geom_point(aes(
    x=V1,
    y=V3,
    color=cluster.assmt.vec
  ),data=feature.dt)+
  geom_point(aes(
    x=V1,
    y=V3,
  ),data=as.data.table(init.assmt.means), color = "#FF0000",size = 2)















# which(cluster.assmt.matrix[,k]==1) vector of indexes of cluster
# feature.dt[which(cluster.assmt.matrix[,k]==1),] data of these observations
# colMeans(feature.dt[which(cluster.assmt.matrix[,k]==1),]) mean of columns in cluster

#cluster.mean.list <- list()
#for(k in 1:K.clusters){
#  assmt.vec <- which(cluster.assmt.matrix[,k]==1)
#  cluster.obs <- feature.dt[assmt.vec,]
#  cluster.mean.list[[paste(k)]] <- data.table(k ,colMeans(cluster.obs))
#}
#temp.assmt.means <- do.call(rbind, cluster.mean.list)
#init.assmt.means <- data.table()
# 
#for(k in 1:K.clusters){
#  init.assmt.means[k,] = data.table(temp.assmt.means[k==k,2])
#}

