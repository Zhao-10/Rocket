## Group Member Information

## Group Number: 24
## 1. Jiaxuan Zhou__s2481783
## 2. Jingyu Zhao__s2493697
## 3. Yanfu(Billy) Ding__s2558622

## Address of Github repo: 
## https://github.com/Zhao-10/Rocket

## Contribution:
## 'netup': Jiaxuan Zhou
## 'forward': Yanfu (Billy) Ding
## 'backward': Jingyu Zhao
## the rest of the content: jointly completed by all members

## Contribution of work
## Jiaxuan Zhou = Jingyu Zhao = Yanfu (Billy) Ding = 1/3 

################################################################################
# Overview: 
#
# This script is related to the construction and application of a simple neural 
# network. Consider a L-layer simple neural network with a vector d giving the 
# number of nodes in each layer. Basically, the network contain values of each 
# node. Denoted h_j^l the value of the j-th node of the l-th layer. Setting 
# h_j^1 as the values of input data, we use the network to combine and transform 
# values in each layer to produce the values at the next layer, until we reach 
# the output layer, h_j^L. In this script, each node value is computed by 
# ReLU-transforming a linear combination of the previous nodes' values. 
# Specifically, for the j-th node in layer l, we have
# 
# h_j^{l+1} = max(0, W_j^l h_l + b_j^l),
#
# where h^l is a vector of node values for layer l, W_j^l is the j-th row of
# the weight parameter matrix W^l linking layer l to layer (l+1) and b^l is a 
# vector of offset parameters linking layer l to layer (l+1). Note that the
# dimension of W_l and b_l is d[l+1] * d[l] and d[l] * 1 respectively.
#
# Now, consider a classification problem, in which h^L is used to predict what 
# class an obs belongs to. Define
#
# p_k = exp(h_k^L) / sum(exp(h^L))
#
# to be the prob that the output variable is in class k. 
# Define the loss function of the training data
#
# L = - sum(log(p_k)) / n
#
# The basic idea is to find the parameters of the network (W_l, b_l) that 
# minimise L, using Stochastic Gradient Descent approach. Specifically, we 
# repeatedly find the gradient of L w.r.t W_l and b_l for a small randomly 
# chosen subsets of the training data, and to adjust the parameters by taking
# a small step in the direction of the negative gradient.
#
# For each iteration, suppose we have a random sample of size mb. Then, the 
# corresponding gradient of L is the average of the individual gradients of L_i,
# where i = 1,2,3,...,mb. One can show that for each datum (x_i, k_i), the 
# derivative of L for k_i w.r.t. h_j^L is
#
#               |  exp(h_j^L) / sum(exp(h^L))     , j != k_i
# dL_i/dh_j^L = |
#               |  exp(h_j^L) / sum(exp(h^L)) - 1 ,  j = k_i
#
# Then we can compute the derivatives of L_i w.r.t. the other h values by 
# back-propagation:
#                                            | dL_i/dh_j^{l+1}, h_j^{l+1} > 0
# dL_i/dh^l = t(W^l) d^{l+1}, where d^{l+1} =| 
#                                            | 0.             , h_j^{l+1} <= 0
# and 
#
# dL_i/db^l = d^{l+1}, 
# dL_i/dW^l = d^{l+1} t(h^l)
#
# Therefore, the gradient of L of the sample can be computed by
# 
# dL/dW^l = mean(dL_i/dW^l), and dL/db^l = mean(dL_i/db^l).
#
# Hence, the update of the parameters in each iteration is 
# 
# W_l <- W_l - eta * dL_i/dW^l
# b_l <- b_l - eta * dL_i/db^l
#
# where eta is a step length.
#
#
# In this practical, functions `netup` and `forward` are first constructed for 
# building a network, with `netup` setting up the NN and `forward` computing all
# the node values according to an input datum. Then, the `backward` function is 
# written to compute the derivative of L related to the output class k for the 
# NN returned from `forward`. Next, the function `train` is constructed to train  
# NN, returning an NN with optimal W and b. Finally, a 4-8-7-3 network is 
# trained to classify irises to species based on the characteristics in the iris
# dataset, with the misclassification rate for the test set given.
################################################################################






################################################################################
## Functions for Neural Network Constructing  
################################################################################

netup = function(d){
  ## INPUT
  ## d: a vector giving the number of nodes in each layer of a network
  
  ## storage for h, W, b
  ## h: a list of node values for each layer
  ## W: a list of weight matrices
  ## b: a list of offset vectors
  n = length(d)
  h = vector('list', n)
  W = vector('list', n-1)
  b = vector('list', n-1)
  
  ## initialisation of h, W and b
  for (i in 1:n){
    h[[i]] = numeric(d[i])
    if (i<n){
      
      W[[i]] = matrix(runif(d[i]*d[i+1], min=0, max=0.2),nrow=d[i+1],ncol=d[i])
      b[[i]] = runif(d[i+1], min=0, max=0.2)
    }
  }
  
  ## OUTPUT: a list of h, W, b
  return(list(h=h, W=W, b=b, d=d))
}


forward = function(nn, inp){
  ## INPUT: 
  ## nn: a network list as returned by `netup`
  ## inp: a vector of input values for the 1st layer
  
  d = nn$d 
  n = length(d) ## number of layers
  h = nn$h
  W = nn$W
  b = nn$b
  h[[1]] = inp ## the first layer's node values are set to be inp
  
  
  ## use ReLU to compute the node values of the rest of the layers
  for (i in 2:n){
    hn = W[[i-1]] %*% h[[i-1]] + nn$b[[i-1]]
    hn[hn < 0] = 0
    h[[i]] = hn
  }
  
  ## OUTPUT: a list of updated h, as well as W, b and d.
  return(list(h = h, W = W, b = b, d=d))
}


################################################################################
## Constructing `backward`
################################################################################


backward = function(nn, k){
  ## INPUT: 
  ## nn: a network list as returned by `forward`
  ## k: a specific output class
  
  d = nn$d
  n = d[length(d)] ## the number of potential output classes
  h_L = nn$h[[length(d)]] ## node values of the output layer
  
  ## derivatives of the loss for class k, w.r.t the nodes
  probs = exp(h_L) / sum(exp(h_L))  
  dL_dh = probs
  dL_dh[k] = dL_dh[k] - 1
  
  ## storage for dh, dW, db
  ## dh, dW, db: lists of derivatives w.r.t the nodes, weights, and offsets
  dh = vector("list", length(nn$h))
  dW = vector("list", length(nn$W))
  db = vector("list", length(nn$b))
  
  ## compute dh, dW, db using back-propagation
  for(i in length(nn$h):2){
    dh[[i]] = dL_dh
    dl_1 = dL_dh * (nn$h[[i]] > 0) ## the vector d^{l+1}
    dW[[i-1]] = dl_1 %*% t(nn$h[[i-1]])
    db[[i-1]] = dl_1
    dL_dh = t(nn$W[[i-1]]) %*% dl_1 ## compute dh for the previous layer
    
  }
  
  ## OUTPUT: a list of dh, dW, db
  return(list(dh=dh, dW=dW, db=db))
}

################################################################################
## Constructing `train` 
################################################################################

train = function(nn, inp, k, eta = 0.01, mb = 10, nstep = 10000){
  ## INPUT: 
  ## nn: a network list as returned by `forward`
  ## inp: matrix of input data
  ## k: vector of labels corresponding to 'inp'
  ## eta: step size 
  ## mb: the number of data randomly sampled to compute the gradient
  ## nstep: the number of iterations (optimisation steps)
  
  d = nn$d
  
  ## update W and b for each iteration
  for(i in 1:nstep){
    gradient_sample = sample(nrow(inp), mb)
    
    ## storage of mean_dW and mean_db
    ## mean_dW: a list of gradients of L of the sample w.r.t. W for each layer
    ## mean_db: a list of gradients of L of the sample w.r.t. b for each layer          
    mean_dW = lapply(1:length(nn$W), function(j) matrix(0, d[j+1], d[j]))
    mean_db = lapply(1:length(nn$b), function(j) numeric(d[j+1]))
    
    ## compute the mean of dW, db of each sample    
    for(j in gradient_sample){
      fnn = forward(nn, as.numeric(inp[j,]))
      bnn = backward(fnn, k[j])
      
      ## iterative process of addition:
      ## the gradients (dW and db) for each layer are computed and accumulated 
      ## in mean_dW and mean_db.
      for(l in 1:length(bnn$dW)){
        mean_dW[[l]] = mean_dW[[l]] + bnn$dW[[l]]/mb
        mean_db[[l]] = mean_db[[l]] + bnn$db[[l]]/mb
      }
    }
  
    
    ## update network weights and biases
    for(l in 1:length(nn$W)){
      nn$W[[l]] = nn$W[[l]] - eta * mean_dW[[l]]
      nn$b[[l]] = nn$b[[l]] - eta * mean_db[[l]]
    }
  }
  
  
  ## OUTPUT: the network after training
  return(nn)
}

################################################################################
## Training and Testing NN on "iris" dataset 
################################################################################

## Part 1: Training

## Converts the 'Species' column from a categorical factor to a numeric format.
iris$Species = as.numeric(as.factor(iris$Species))  

## divide iris into training and test data
iris = as.matrix(iris)
test_index = seq(from = 5, to = nrow(iris), by = 5) ## index of test data
test_data = iris[test_index, 1:4]
train_data = iris[-test_index, 1:4]
test_species = iris[test_index, 5]
train_species = iris[-test_index, 5]


d = c(4, 8, 7, 3) ## specify the number of nodes in each layer

## initialise an NN w.r.t. 'd' after setting the seed
set.seed(124); nn1 = netup(d) 

## train_nn: the corresponding network after training
train_nn = train(nn = nn1 , inp = train_data, 
                 k = train_species, eta = 0.01, mb = 10, nstep = 10000)

################################################################################
## Part 2: Testing -- misclassification rate

test_pred = rep(0,nrow(test_data)) ## storage of predictions
for(i in 1:nrow(test_data)){
  ## apply 'forward' to get the node values of the output layer
  test_nn = forward(train_nn, as.numeric(test_data[i,]))
  test_h = test_nn$h[[length(train_nn$d)]]
  
  ## determine the class with the highest output value
  test_pred[i] = which.max(test_h)
}

## calculate the misclassification rate
mis_rate = sum(test_species != test_pred) / length(test_pred); mis_rate









