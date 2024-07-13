# in glm data must not be normally distributed, could be more contraint

# binary responses, categorical predictors
# grouped data

# before we assumed outcomes to follow normal distribution
# but that doesn't work for all variables

# we want to predict parameters for distribution instead of mean in a normal one
# for each probability distribution we choose link functions

# logistic takes real values and tranforms to probabilityh

# prior distribution for alpha

prior <- rexp(10000, rate=1)

prior_sample = sample(prior, 1000, replace=TRUE)
head(prior_sample)

theta = exp(prior_sample) / (1 + exp(prior_sample))
head(theta)

pr <- data.frame(theta)

ggplot(pr) +
    geom_density(aes(x = theta))

# solution

dat <- tibble(logit = seq(-6,6, length.out = 100), logistic = inv_logit(logit))

# plotting data

dat %>% 
  ggplot(aes(logit, logistic)) +
  geom_line(size = 2, color = "#ff02ff") +
  labs(x = "Logit",
       y = "Logistic")

# different priors

dat <-  tibble(a1 = rnorm(1e4, 0, 5),
               a2 = rnorm(1e4, 0, 1),
               a3 = rnorm(1e4, 0, .1),
               p1 = inv_logit(a1), # = exp / 1 - exp
               p2 = inv_logit(a2),
               p3 = inv_logit(a3)) %>%
  pivot_longer(cols = a1:p3, names_to = "scale", values_to = "value")

# p1 p2 p3 - resulting priors

dat %>% 
  ggplot(aes(x=value)) +
  geom_density(size = 1, color = "#ff02ff") + 
  facet_wrap(~scale, scales = "free") + 
  labs(x = "Logit Prediction",
       y = "Density")

# binary predictors - index methods
# model intercept for each gender
# specify only one prior assuming all are the same

# model gender and department

# simulation without direct discrimination

N <- 1000 # number of applicants 
G <- sample(1:2, size = N, replace = TRUE) # genders
D <- rbern(N, ifelse(G==1, .3, .8)) + 1 # departments 1 and 2, 30% for women in dep1 and 80% in dep2
accept_rate <- matrix( c(.1, .3, .1, .3), nrow = 2) # no discrimination, diff acc rates for diff departments
A <- rbern(N, accept_rate[D, G])

table(G, D) # genders and application to departments
table(G, A) # acceptance

# total effect of gender

dat <- list(A = A, D = D, G = G)

m1 <- ulam(
  alist(
    A ~ dbern(p), # likelihood
    logit(p) <- a[G], # predicting logit of p
    a[G] ~ dnorm(0,1) # prior
  ), data = dat, chains = 4, cores = 4
)

traceplot(m1)
precis(m1, depth = 2) # coefficients on log odds scale
inv_logit(coef(m1)) # interpret as probability
# different AR for different genders

m2 <- ulam(
  alist(
    A ~ dbern(p),
    logit(p) <- a[G, D],
    matrix[G, D]:a ~ dnorm(0,1)
  ) , data = dat, chains = 4, cores = 4
)

precis(m2, depth = 3)
inv_logit(coef(m2))
traceplot(m2)

# Berkeley data

data("UCBadmit", "UCBadmit_long")
dat_wide <- UCBadmit
dat_long <- UCBadmit_long
head(dat_wide)
head(dat_long)

acceptance_rates <- dat_wide %>%
    group_by(dept, applicant.gender) %>%
    mutate(rate = admit / applications)

dat <- list(
  A = dat_wide$admit, 
  N = dat_wide$applications,
  G = ifelse(dat_wide$applicant.gender=="female", 1, 2),
  D = as.integer(dat_wide$dept)
  )

# total difference
m5 <- ulam(
  alist(
    A ~ dbinom(N, p),
    logit(p) <- a[G],
    a[G] ~ dnorm(0,1)
  ) , data = dat, chains = 4, cores = 4
)

precis(m5, depth = 2)
inv_logit(coef(m5))
traceplot(m5)

# samples from posterior distribution of differences

post1 <- extract.samples(m5)

posterior <-  tibble(p1 = inv_logit(post1$a[,1]) , 
                     p2 = inv_logit(post1$a[,2]) , 
                     diff = p1 - p2)


ggplot(posterior, aes(diff)) + 
  geom_density(color = "#ff02ff", linewidth = 2) +
  labs(x="Admission Probability G1 - Admission Probability G2" , 
       y="Density")

# gender diff

m6 <- ulam(
  alist(
    A ~ dbinom(N, p),
    logit(p) <- a[G, D],
    matrix[G, D]:a ~ dnorm(0,1)
  ) , data = dat, chains = 4, cores = 4
)

precis(m6, depth = 3)
inv_logit(coef(m6))
traceplot(m6)

post2 <- extract.samples(m6)

PrA <- inv_logit(post2$a)

diff_prob_D <- sapply(1:6, function(i) PrA[, 1, i] - PrA[, 2, i])
diffs <-  tibble(D1 = PrA[, 1, 1] - PrA[, 2, 1] , 
                 D2 = PrA[, 1, 2] - PrA[, 2, 2] , 
                 D3 = PrA[, 1, 3] - PrA[, 2, 3] , 
                 D4 = PrA[, 1, 4] - PrA[, 2, 4] , 
                 D5 = PrA[, 1, 5] - PrA[, 2, 5] ,
                 D6 = PrA[, 1, 6] - PrA[, 2, 6]) %>% 
  pivot_longer(cols = D1:D6, names_to = "Department", values_to = "Diff")
  

diffs %>% 
  ggplot(aes(Diff, color = Department)) + 
  geom_density(alpha = .3, linewidth = 2) +
  labs(x="Admission Probability G1 - Admission Probability G2" , 
       y="Density") +
  scale_color_viridis_d(option = "C")


ggplot(posterior2, aes(diff)) + 
  geom_density(color = "#ff02ff", linewidth = 2) +
  labs(x="Admission Probability G1 - Admission Probability G2" , 
       y="Density")

# gender differences completely come from differences in departments applications       

dat2 <- list(
    A = dat_long$admit,
    G = ifelse(dat_long$applicant.gender=="female", 1, 2),
    D <- as.integer(dat_long$dept)
)

m_long <- ulam(
    alist(
        A ~ dbern(p),
        logit(p) <- a[G],
        a[G] ~ dnorm(0,1)
    ), data = dat2, chains = 4, cores = 4
)
