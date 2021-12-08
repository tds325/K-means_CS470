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

# returns a data.table of cluster assignments given the assignment matrix
compute_assignment <- function(cluster.matrix){
  assmt.vec <- vector(mode = "numeric",length=nrow(cluster.matrix))
  for(n in 1:length(assmt.vec)){
    assmt.vec[n] = (which(cluster.matrix[n,]==1))
  }
  return(assmt.vec)
}

# set num of clusters & make random assignment
set.seed(1)
# k values to loop through given the prompt
K.iter.vals <- c(4:7)
clustering.result.list <- list()
MSE.outer.list <- list()
# K.iter.vals <- 2
count <- 0
MSE.total <- 0

for(K.outer in K.iter.vals){
  # take samples of K.clusters random observations and set the initial mean values to the sample values
  initial.center.assignment.indices <- sample(length(feature.dt[1,]), K.outer)
  init.assmt.means <- as.vector(feature.dt[initial.center.assignment.indices,])
  
  # create variable to compare and stop running kmeans when they are equal 
  # (initializing them to not be equivalent in order for the loop to begin)
  previous.means <- init.assmt.means[-1]
  
  cluster.assmt.matrix <- matrix(0, nrow = nrow(feature.dt), ncol = K.outer) 
  dist.from.mean.vec <- vector(mode = "numeric", length = K.outer)
  
  different.k.list <- list()
  MSE.inner.list <- list()
  cluster.assmt.list <- list()
  cluster.iter.mean.list <- list()
  
  # upper bound for iterations
  iterations <- 15
  count = 0 # actual iterations carried out (needed for plot data.table creation below)
  for(iter in 1:iterations){
    if(identical(previous.means, init.assmt.means)){
      break
    }
    MSE.total = 0
    # loop through each observation in the data
    for(obs.n in 1:nrow(feature.dt)){
      dist.from.mean.vec = 0
      # for each observation, compute the euclidean distance from each mean value
      for(k in 1:K.outer){
        dist.from.mean.vec[k] = pairwise_distance(init.assmt.means[k], feature.dt[obs.n])
      }
      # assign the observation to the cluster with the closest mean
      assmt.index <- which.min(dist.from.mean.vec)
      cluster.assmt.matrix[obs.n, ] = 0
      cluster.assmt.matrix[obs.n, assmt.index] = 1
      MSE.total = MSE.total + (dist.from.mean.vec[assmt.index]^2)
    }
    # recompute cluster means
    cluster.mean.list <- list()
    for(k in 1:K.outer){
      cluster.mean.list [[paste(k)]] <- (colMeans(feature.dt[which(cluster.assmt.matrix[,k]==1),]))
    }
    previous.means <- init.assmt.means
    init.assmt.means <- data.table()
    init.assmt.means <- do.call(rbind, cluster.mean.list)
    
    # list to store all means we compute along the way for future reference
    cluster.iter.mean.list[[paste(iter)]] <- data.table(
      iter = iter,
      means = init.assmt.means)
    # list to store all assignments to clusters 
    cluster.assmt.list[[paste(iter)]] <- data.table(
      iter = iter,
      obs.assignments = compute_assignment(cluster.assmt.matrix))
    # list to keep track of mean squared error
    MSE.inner.list[[paste(iter)]] <- data.table(K.outer, iter, MSE.total)
    
    count = count + 1
  } 
  cluster.assmt.dt <- do.call(rbind, cluster.assmt.list)
  cluster.means.dt <- do.call(rbind, cluster.iter.mean.list)
  
  clustering.result.list[[paste(K.outer)]] <- as.list(c(
    assmt = cluster.assmt.dt[iter == count]$obs.assignments,
    means = cluster.means.dt[iter == count]))
  
  MSE.outer.list[[paste(K.outer)]] <- MSE.inner.list
}

#`plotting` data table for showing each iteration with a single k value
##cluster.assmt.vec <- compute_assignment(cluster.assmt.matrix)
plotting.dt <- feature.dt[rep(1:nrow(feature.dt), count),]
plotting.dt[,iter := cluster.assmt.dt$iter]
plotting.dt[,obs.assignments := cluster.assmt.dt$obs.assignments]


ggplot()+
  geom_point(aes(
    x=V1,
    y=V3,
    color=obs.assignments
  ),data=plotting.dt)+
  geom_point(aes(
    x=means.V1,
    y=means.V3,
  ),data=cluster.means.dt, color = "#FF0000",size = 2)+
  facet_grid(iter ~ ., scales = "free")


