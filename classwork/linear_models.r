library(rethinking)

# Write a simulation that generates points for 1000 games,
# based on field goal attempts (FGA), free throw attempts
# (FTA), and the shot accuracies for FGA and FTA.

# function that simulates number of points for a single game
sim_pts <- function(FGA, FTA, hFG, hFT) {
  # FTA
  # simulate 1 value (bernoulli process) when we shoot the ball FT times
  # each shot hits the basket with hFT probability
  FT <- rbinom(1, FTA, prob = hFT)
  # 2PA
  # same for field goals, 2 - number of points
  FG <- rbinom(1, FGA, prob = hFG) * 2
  # + unobserved random influence
  FT + FG + rnorm(1, mean = 0, sd = 4)
}

N_games <- 1e3
hFG <-  .7
hFT <-  .4
# simulate FGA and FTA
# gamma fn - simulates values, positive, 20 - mean, 1 - shape (is skewed or not)
FGA <- round(rgamma(N_games, 20, 1), 0)
FTA <- round(rgamma(N_games, 5, 1), 0)
pts <- vector("numeric", length = N_games)

# simulating 1000 games
set.seed(16)
for (i in seq_along(1:N_games)) {
  pts[i] <-  sim_pts(FGA[i], FTA[i], hFG, hFT)
}

dat <- tibble(FGA, FTA, pts)

ggplot(dat, aes(x = pts)) +
  geom_histogram(fill = "#ff02ff", alpha = .5, color = "#ff02ff", bins = 30) +
  labs(x = "PTS",
       y = "Frequency")

# Use the quap() function to estimate a multiple linear
# regression model on the simulated data that estimates
# the two shot accuracies from the points, FGA and FTA.

m1_shaq <- quap(
  alist(
    # likelihood
    pts ~ dnorm(mu, sd), # model
    mu <- a + b_1 * FGA * 2 + b_2 * FTA,
    # prior
    a ~ dnorm(20, 8),
    b_1 ~ dunif(0, 1), # shot accuracy
    b_2 ~ dunif(0, 1), # throw accuracy
    sd ~ dunif(0, 8)
  ),
  data = dat)
precis(m1_shaq)

plot(m1_shaq)
pairs(m1_shaq, pars = c("a", "b_1", "b_2", "sd"))

m1_simple <- quap(
  alist(
    pts ~ dnorm(mu, sd),
    mu <- a + b_1 * FGA * 2,
    a ~ dunif(0, 10),
    b_1 ~ dunif(0, 1),
    sd ~ dunif(0, 8)
  ),
  data = dat
)

# mean-centering - only when there's enough data
FGA_bar <- round(mean(dat$FGA), 0)
FTA_bar <- round(mean(dat$FTA), 0)

m2_shaq <- quap(
  alist(
    pts ~ dnorm(mu, sd),
    mu <- a + b_1 * (FGA-FGA_bar) * 2 + b_2 * (FTA-FTA_bar),
    a ~ dnorm(20, 8),
    b_1 ~ dunif(0, 1),
    b_2 ~ dunif(0, 1),
    sd ~ dunif(0, 8)
  ),
  data = dat)
precis(m2_shaq)

plot(m2_shaq)
pairs(m2_shaq, pars = c("a", "b_1", "b_2", "sd"))

# we recommend running this is a fresh R session or restarting your current session
install.packages("cmdstanr", repos = c("https://stan-dev.r-universe.dev", getOption("repos")))

cmdstanr::install_cmdstan()

install.packages(c("coda","mvtnorm","devtools","loo","dagitty","shape"))
devtools::install_github("rmcelreath/rethinking")