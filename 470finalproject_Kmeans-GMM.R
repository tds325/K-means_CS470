setwd("C:/Users/Tayler/Documents/470/finalproject")
label.input.filename <- "y_train.txt"
feature.input.filename <- "X_train.txt"

library(ggplot2)
library(plyr)
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
  obs1 = obs1[!is.na(obs1)]
  obs2 = as.vector(obs2[[1]])
  obs2 = obs2[!is.na(obs2)]
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

compute_colmeans <- function(data.dt, label.vector){
  mean.list <- list()
  main.data.table <<- data.dt
  for(label in 1:max(label.vector)){
    mean.list[[paste(label)]] <- 
      colMeans(feature.dt[which(label.vector == label),])
  }
  main.data.table <- do.call(rbind, mean.list)
  main.data.table <- as.data.table(main.data.table)
  return(main.data.table)
}

compute_MSE <- function(data.dt, label.vector){
  mean.dt <- compute_colmeans(data.dt, label.vector)
  #mean.dt[,k := .I]
  total.mse <- 0
  for(label in 1:max(label.vector)){
    temp.dt <- feature.dt[which(label.vector == label), ]
    for(n in 1:nrow(temp.dt)){
      total.mse = total.mse + (pairwise_distance(mean.dt[label,], temp.dt[n,]))^2
      str(total.mse)
    }
  } 
  str(total.mse)
  return(total.mse)
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
K.iter.vals = 6
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
  iterations <- 100
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
#plotting.dt <- feature.dt[rep(1:nrow(feature.dt), count),]
#plotting.dt[,iter := cluster.assmt.dt$iter]
#plotting.dt[,obs.assignments := cluster.assmt.dt$obs.assignments]

# ggplot for showing each iteration for a single k
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

# create data structure for plotting clusters with different k values
for(k in K.iter.vals){
  expr.string <- "clustering.result.list$`"
  expr.string <- paste(expr.string, k, sep = "")
  expr.string <- paste(expr.string, "`", sep = "")
  final.res.list[[paste(k)]] <- data.table(
    k, 
    label = as.numeric(unlist(eval(parse(text = expr.string))))[1:nrow(feature.dt)]) 
}
final.res.dt <- do.call(rbind, final.res.list)
plotting.dt <- data.table()
plotting.dt <- feature.dt[rep(1:nrow(feature.dt), length(K.iter.vals)),]
plotting.dt[, k := final.res.dt$k]
plotting.dt[, label := final.res.dt$label]

# create data structure for plotting means
# refusing to cooperate
#plot.mean.list <- list()
#for(k in K.iter.vals){
#  #str(k)
#  mean.dt <- compute_colmeans(feature.dt, final.res.dt[k==k]$label)
#  #str(mean.dt)
#  plot.mean.list[[paste(k)]] <- mean.dt
#}
#plot.mean.dt <- do.call(rbind, plot.mean.list)

plot.mean.list[[paste(4)]] <- compute_colmeans(feature.dt, final.res.dt[k==4]$label)
plot.mean.list[[paste(5)]] <- compute_colmeans(feature.dt, final.res.dt[k==5]$label)
plot.mean.list[[paste(6)]] <- compute_colmeans(feature.dt, final.res.dt[k==6]$label)
plot.mean.list[[paste(7)]] <- compute_colmeans(feature.dt, final.res.dt[k==7]$label)
plot.mean.dt <- do.call(rbind, plot.mean.list)
k.vec <- vector(mode = "numeric")
for(k in K.iter.vals){
  k.vec <- c(k.vec, rep(k, each = k))
}
plot.mean.dt[, k := k.vec]

# plot for k = 4:7 showing labeling and mean centers
ggplot()+
  geom_point(aes(
    x = V1,
    y = V3,
    color = label,
  ),data=plotting.dt)+
  geom_point(aes(
    x = V1, 
    y = V3,
  ),data = plot.mean.dt, color = "red", size = 2)+
  facet_grid(k ~ ., scales = "free")
# the plot itself is not completely representative of the data, as there are 561 
# dimensions and we are plotting two of them

# ggplot of original labeling
orig.means <- compute_colmeans(feature.dt, label.vec)
ggplot()+
  geom_point(aes(
    x = V1,
    y = V3,
    color = label.vec,
  ),data = feature.dt)+
  geom_point(aes(
    x = V1,
    y = V3
  ),data = orig.means, color = "red")

MSE.plot.list <- list()
MSE.plot.list[[paste(4)]] <- as.data.table(MSE.outer.list$`4`$`25`)
MSE.plot.list[[paste(5)]] <- as.data.table(MSE.outer.list$`5`$`24`)
MSE.plot.list[[paste(6)]] <- as.data.table(MSE.outer.list$`6`$`27`)
MSE.plot.list[[paste(7)]] <- as.data.table(MSE.outer.list$`7`$`68`)
MSE.plot.dt <- do.call(rbind.fill, MSE.plot.list)
#given.mse <- compute_MSE(feature.dt, label.vec)
given.mse <- MSE.plot.list$
mse.list <- list()
for(k in K.iter.vals){
  mse.list[k] = as.numeric(compute_MSE(feature.dt, final.res.dt[k==k]$label))
}
mse.dt <- do.call(rbind, mse.list)

mse.dt[1,] = compute_MSE(feature.dt, final.res.dt[k==4]$label)
mse.dt[2,] = compute_MSE(feature.dt, final.res.dt[k==5]$label)
mse.dt[3,] = compute_MSE(feature.dt, final.res.dt[k==6]$label)
mse.dt[4,] = compute_MSE(feature.dt, final.res.dt[k==7]$label)
colnames(mse.dt) = c("mse")
mse.dt <- as.data.table(mse.dt)
mse.dt[, data.i := .I]
ggplot()+
  geom_path(aes(
    x = data.i, 
    y = mse,
  ),data=mse.dt)+
  geom_hline(aes(
    yintercept = given.mse,
    color = "red"
  ))
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Finding train and test subsets
tsize <- as.integer(nrow(feature.dt) * .9)
train.subset.indices <- sample(1:nrow(feature.dt), size = tsize)
train.subset <- feature.dt[train.subset.indices,]
test.subset.indices <- which(!((1:nrow(feature.dt))%in%train.subset.indices))
test.subset <- feature.dt[test.subset.indices,]
# ran train.subset on kmeans above

# add features to the data.tables so we can keep track of cluster assignments
K.neighbors <- 3
train.subset[,assmt := cluster.assmt]
test.subset[, assmt := K.neighbors]

# initialize variables for K.n.n
temp.list <- list()
temp.dt <- data.table()

# for each observation in the test subset, find the k closest points.
# then using the tabulate function find the index of the 
# maximum value and assign it to that cluster

for(obs.n.test in 1:nrow(test.subset)){
  for(obs.n.train in 1:nrow(train.subset)){
    # design pattern of creating a list and then using rbind to get the results
    # here we are storing the original position of test & train, 
    # the cluster assignment #, and the distance between the test and 
    # train points
    temp.list[[paste(obs.n.train)]] <- 
      data.table(obs.n.test, obs.n.train, assmt = 0, dist = pairwise_distance(test.subset[obs.n.test,-"assmt"], 
                        test.subset[obs.n.train,-"assmt"]))
  }
  temp.dt <- do.call(rbind, temp.list)
  temp.dt <- temp.dt[order(temp.dt$dist),]
  temp.dt <- temp.dt[1:K.neighbors,]
  
  # for each item in the data.table, find which cluster assignment is represented the most
  for(item in 1:nrow(temp.dt)){
    temp.dt[item,]$assmt = train.subset$assmt[temp.dt$obs.n.train[item]]
  }
  test.subset[obs.n.test,]$assmt = which.max(tabulate(temp.dt$assmt))
  
}

