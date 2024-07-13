# groups differ but there is commonality, common influence
# multilevel models capture hierarchy

# whether decision quality depends on quality
# what effect has age?
# in which differences in numeracy are bigger? do they differ?

# assume: decision quality could be normally distributed
# before: each observation is influenced by same mean and sd

# groups: people systematically differ from each other
# observations in groups have more similarities that between groups
# then errors are not normally distributed, they're correlated in groups
# means of groups are different

# group means are drown from overall distribution
# partial pooling (=multilevel models) - assume differences, but they're not compeltely different from each other

# mean is clustered withing the group
# fixed sd that influences all observations

# group vector with 1 and 2, use them to generate decision quality data rnorm
# mean argument in rnorm is dependent on group

library(rethinking)

set.seed(99389)
group <- sample(c(1, 2), 1000, replace = TRUE)

quality <- function(group) {
    # group 1 has better dc than group 2
    rnorm(1000, mean = ifelse(group == 1, 0.7, 0.3), sd = 0.1)
}
qual <- quality(group)

data <- list(
    quality = qual,
    group = group
)

m1 <- ulam(
  alist(
    
    Q1 ~ dnorm(0.7, 0.1), # likelihood
    Q2 ~ dnorm(0.3, 0.1),

  ), data = data, chains = 4, cores = 4
)

# solution to exercise 1

# generative model

sim_quality_1 <- function(G) {
  N <- length(G)
  quality <- ifelse(G == 1, .75, .6) + rnorm(N, 0, .2) # adding random noise
  # or: rnorm(N, mean = ifelse(G==1, .75, .6), sd = .2)
  data.frame(G, quality)
}

# simulate data
G <- as.factor(sample(c(1, 2), 1e3, replace = T))
d1 <- sim_quality_1(G)

# test model

## complete pooling
mQCP1 <- ulam(
  alist(
    quality ~ dnorm(mu, sigma), # likelihood
    mu ~ dnorm(.5, .2), # prior, assume random choice
    sigma ~ dexp(.5) # prior
  ) , data = d1, chains = 4, cores = 4, log_lik = TRUE
)
precis(mQCP1)

# log_lik computes fit to the data

# good idea to plot priors beforehand

plot(density(rnorm(1000, 0.5, 0.2)))
plot(density(rexp(1000, 0.5))) # bad one, better 10

## no pooling
mQNP1 <- ulam(
  alist(
    quality ~ dnorm(mu, sigma),
    mu <- a[G], # group differences
    a[G] ~ dnorm(.5, .2),
    sigma ~ dexp(.5)
  ) , data = d1, chains = 4, cores = 4, log_lik = TRUE
)
precis(mQNP1, depth = 2)

# partial pooling

# remove assumption of complete indifference
# group means are drawn from one hyper distribution
# estimating hyper distribution too
# observations are clustered into a group
# they're influenced by group-specific mean and sd
# group-specific mean ist influenced by hyperparameters
# common ifluence + on top group-specific effect
# partial pooling helps with overfitting ie with narrow prior
# the narrower hyper distribution - the stronger is effect between groups
# hyperparameters in mlm usually from normal distribution but it can be any other
# judge how likely is each mean in hyperdistribution
# entire chain - maximum likelihood, good compromise

# cross-validation

# take in-sample fit, use to predict the out-sample fit
# informaitonal criterions try to estimate out-sample fit
# WAIC widely applicable IC - takes in-sample fit, probability of fit given entire posterior
# smallest IC, lower deviance between prediction and error by maximizing likelihood
# log-like is in-sample fit

# partial pooling

mQPP1 <- ulam(
  alist(
    quality ~ dnorm(mu, sigma),
    K ~ dnorm(0.5, 0.2),
    T ~ dnorm(0, 0.1),
    mu <- a[G],
    a[G] ~ dnorm(K, T),
    sigma ~ dexp(.5)
  ) , data = d1, chains = 4, cores = 4, log_lik = TRUE
)
precis(mQPP1, depth = 2)

compare(mQCP1, mQNP1, mQPP1)

# partial pooling is punished more, fit to the model is a bit better
# WAIC prefers the simpler model

set.seed(1100)
G2 <- as.factor(sample(c(1, 2), 1e3, replace = T))

no_samples <- extract.samples(mQNP1)
complete_samples <- extract.samples(mQCP1)
partial_samples <- extract.samples(mQPP1)

# different kinds of multi level models
# we can assume one intercept for all groups
# and assume that slopes are different and drawn from hyperdistibution - influence on mood with different direction
# or same effect but different averages - dra intercepts from hyper - one direction, but pre-mood was different

# exercise 3

# generative model

sim_quality_3 <- function(G, a, b, oa, ob) { 
    # groups, fixed intercept and slope, oa, ob - deviation from overall mean
  N <- length(G)
  numeracy <- rnorm(N, .5, .2) # predictor
  quality <- a + oa[G] + (b + ob[G]) * numeracy + rnorm(N, 0, .1)
  data.frame(G, numeracy, quality)
}

# simulate data

G <- as.factor(sample(1:20, 1e4, replace = T)) # 20 groups
oa <- round(sample(seq(-.3, .3, .05), 20, replace = T), 2) # deviation from -.3 to .3
ob <- round(sample(seq(-.2, .2, .05), 20, replace = T), 2)
d3 <- sim_quality_3(G, a = .5, b = 0, oa = oa, ob = ob)

head(tibble(oa = .5 + oa, ob = ob), 5)

# test model

## partial pooling 

mQPP3 <- ulam( 
  alist(

    quality ~ dnorm(mu, sigma),
    mu <-  a[G] + b[G] * numeracy,

    a[G] ~ dnorm(a_bar, tau_a),
    b[G] ~ dnorm(b_bar, tau_b),
    a_bar ~ dnorm(.5, .2),
    b_bar ~ dnorm(0, .15),
    tau_a ~ dexp(.5),
    tau_b ~ dexp(.5),
    sigma ~ dexp(1)
  ) , 
  data = d3,
  chains = 4,
  cores = 4,
  log_lik = TRUE
)

precis(mQPP3, depth = 2)
plot(precis(mQPP3, depth = 2))


mQCP3 <- ulam( 
  alist(
    quality ~ dnorm(mu, sigma) , 
    mu <-  a + b * numeracy ,
    a ~ dnorm(.5, .2) , 
    b ~ dnorm(0, .15),
    sigma ~ dexp(1)
  ) , 
  data = d3, 
  chains = 4, 
  cores = 4, 
  iter = 4000,
  log_lik = TRUE
)

precis(mQCP3, depth = 2)

compare(mQPP3, mQCP3)