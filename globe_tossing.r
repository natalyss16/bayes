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