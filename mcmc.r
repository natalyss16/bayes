library(rethinking)
library(ggplot2)

# King Markov travel

weeks <- 1000
islands <- 10

start_island <- floor(runif(1, min = 1, max = islands))
visits <- c(start_island)

for(i in 1:weeks) {
    direction <- sample(c(-1, 1), 1, prob=c(0.5, 0.5), replace = TRUE)
    proposal_island <- start_island + direction
    if(proposal_island > start_island && proposal_island <= islands) {
        start_island <- proposal_island
        visits <- append(visits, start_island)
    }
}
visits

# correct solution
num_weeks <- 1e5 
positions <- rep(0, num_weeks)
current <- 10
for (i in 1:num_weeks) {
  # record current position
  positions[i] <- current
  # flip coin to generate proposal
  proposal <- current + sample(c(-1, 1), size = 1)
  # now make sure he loops around the archipelago
  if (proposal < 1) proposal <- 10
  if (proposal > 10) proposal <- 1
  prob_move <- proposal / current
  # if the prob_move > runif(1) then stay
  current <- ifelse(runif(1) < prob_move, proposal, current)
}

positions[1:100]

data <- data.frame(position = positions, move = 1:1e5)
ggplot(data, aes(x=move, y = position)) +
  geom_line(size = 1, color = "#c80895") +
  scale_x_continuous(limits = c(0,1000))

ggplot(data, aes(x=position)) +
  geom_bar(color = "#c80895", fill = "#c80895", alpha = .5) + 
  scale_x_continuous(breaks = seq(1,10,1))

# ulam instead of quap

shaq <- read_csv("data/shaq.csv")
dat <- list(FGA = shaq$FGA,
            FTA = shaq$FTA,
            PTS = shaq$PTS,
            MIN = shaq$Minutes,
            FTA_bar = round(mean(shaq$FTA), 0),
            FGA_bar = round(mean(shaq$FGA), 0),
            MIN_bar = round(mean(shaq$FGA), 0))

m5_shaq <- ulam(
  alist(
    pts ~ dnorm(mu, sd),
    mu <- a + b_1 * (MIN - MIN_bar) + b_2 * (FGA-FGA_bar) * 2 + b_3 * (FTA-FTA_bar),
    a ~ dnorm(20, 8),
    b_1 ~ dnorm(0, 2),
    b_2 ~ dunif(0, 2),
    b_3 ~ dunif(0, 1),
    sd ~ dunif(0, 10)
  ),
  data = dat, chains = 4, cores = 4, iter = 4000)
precis(m5_shaq)