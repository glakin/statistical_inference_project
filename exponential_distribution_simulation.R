lambda = 0.2
n = 40
expMeans <- rep(NA,1000)

# Simulate the means
for(i in 1:1000){
    expMeans[i] = mean(rexp(n, lambda))
}

# Expected mean of the exponential distribution is 1/lambda
1/lambda

# Average simulated mean
mean(expMeans)

# Expected variance under the CLT is s^2/n. For exponential distribution s^2 = 1/(lambda^2)
1/(lambda^2*n)

# Variance of simulated means
var(expMeans)

# Compare the distribution of the means to a distribution of random variables 
# from the exponential distribution
exps <- rexp(1000, lambda)

library(ggplot2)
exps <- rexp(1000, lambda)

g1 <- ggplot(data.frame(exps = exps), aes(x = exps)) 
g1 + geom_histogram(color = "black", fill = "thistle", binwidth = 1) +
    ggtitle("Distribution of 1000 random variables taken from the exponential distribution with lambda = 0.2")

# Histogram of simulated means
g2 <- ggplot(data.frame(expMeans = expMeans), aes(x = expMeans)) +
      geom_histogram(color = "black", fill = "thistle", binwidth = 0.25)
g2

muPred <- 1/lambda 
muSim <- mean(expMeans)
muPred
muSim
(muPred - muSim)/muSim

varPred <- 1/(lambda^2*n)
varSim <- var(expMeans)
varPred
varSim
(varSim - varPred)/varPred
