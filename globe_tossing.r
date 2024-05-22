# vector of water and land
# vector of candidates
# apply them

library(stringi)

n <- 1000
step <- 0.1

globe_tossing <- function() {
   set.seed(16)
   globe <- stri_rand_strings(n, 1, pattern = "[LW]")
   proportions <- seq(from = 0, to = 1, by = step)
}

data_counts <- function() {
    globe_tossing()

}

# solution

sim_tosses <- function(n, p){
  sample(c("L", "W"), size=n, replace=TRUE, prob=c(p, 1-p))
}

counter <- function(data, cp){ 

  sides <- length(cp)-1

  L <- sum(data == "L")
  W <- sum(data == "W")
  ways <- (cp * sides)^L * ((1 - cp) * sides)^W
  data.frame(cp, ways)
}

n <- 10
p <- 0.1

# counting

counter(sim_tosses(n, p), cp = seq(0, 1, .1))

counter(c("L", "W", "L"), cp = seq(0, 1, .25)) # example from slides
counter(c("L", "W", "L", "W"), cp = seq(0, 1, .25)) # example from slides

old_data <- c("L", "W", "L")
old_ways <- counter(old_data, cp = seq(0, 1, .25))

new_data <- sim_tosses(n, p)
new_ways <- counter(new_data, cp = seq(0, 1, .25))

data.frame(cp = old_ways$cp,
old = old_ways$ways,
new = new_ways$ways,
total = old_ways$ways * new_ways$ways)

# probability of getting land

sim_tosses <- function(n){
  sample(c("L", "W"), size=n, replace=TRUE, prob=NULL) # random sample
}

likelihood <- function(data, cp) {
  L <- sum(data == "L")
  W <- sum(data == "W")

  prob <- dbinom(cp, data)
  data.frame(cp, prob)
}

likelihood(data, cp)

posterior <- function() {
  prior * likelihood
}

n <- 10
flat_prior <- 1/n
cp <- seq(0, 1, .1)

data <- sim_tosses(n)
prior <- flat_prior
likelihood <- likelihood(data, cp)
posterior()

# solution

compute_post <- function(data, candidates, n) {
  L <- sum(data=="L")
  likelihood <- dbinom(L, n, prob=candidates$cp)
  posterior <- likelihood * candidates$prior # updating
  posterior_norm <- posterior / sum(posterior) # standardization
  data.frame(candidates, lh=round(likelihood, 3), post=round(posterior_norm, 3))
}

n <- 100
data <- sim_tosses(n)
cp <- seq(0, 1, .05)
flat_prior <- rep(1/length(cp), length(cp))

candidates <- tibble(cp, prior = flat_prior)
compute_post(data, candidates, n)

# generate samples from posterior distribution to calculate statistics
n <- 100
data <- sim_tosses(n)
cp <- seq(0, 1, .01)
flat_prior <- rep(1/length(cp), length(cp))
candidates <- tibble(cp, prior = flat_prior)
posterior <- compute_post(data, candidates, n)

mode <- function(x) {
   return(as.numeric(names(which.max(table(x)))))
}

samples_n <- 1000
sample <- sample(posterior$cp, samples_n, replace=TRUE, prob=posterior$post)

mean(sample)
sd(sample)
mode(sample)
quantile(sample, probs = .10)
quantile(sample, probs = .90)