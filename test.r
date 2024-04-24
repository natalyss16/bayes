v1 <- c(1:5, T)
v1*1

dat[1,] # first row
dat[,1] # first column
dat[,1] + 2 # add 2 to each value
dat[c(1,2), c(1,2)] # select a11, a12, a21, a22
dat$X # by column name

fn1 <- function(INPUT) {
    x <- INPUT + 1
    return x
}

fn1(5)

df <- data.frame(x1=c(4,8,9,2,4), x2=c(4,0,8,22,1), x3=c(5,9,21,5,4), x4=c(6,6,77,32,4), x5=c(3,1,22,0,0))
print(df)

vec <- c()
for (i in length(df[,i])) {
    vec <- c(vec, mean(df[,i]))
}

vec

n_obs <- 5
dat <- data.frame(
    v1 = rnorm(n_obs, mean=1),
    v2 = rnorm(n_obs, mean=2),
    v3 = rnorm(n_obs, mean=3),
    v4 = rnorm(n_obs, mean=4),
    v5 = rnorm(n_obs, mean=5)
)

sapply(dat, mean)

colMeans(dat)

c(
    mean(dat[,1]), 
    mean(dat[,2]), 
    mean(dat[,3]), 
    mean(dat[,4]), 
    mean(dat[,5])
    )

length(dat)

install.packages("tidyverse")
library(tidyverse)

install.packages("ggplot2")
library(ggplot2)

group_n <- 1000 # number of observations per group
group <- c( rep("Group 1", group_n), rep("Group 2", group_n) ) # create grouping variable
value <-  c( rnorm(group_n, mean = 0), rnorm(group_n, mean=4)) # generate random values for both groups
data <- data.frame(group, value) # store variables in a data frame


# Represent it
ggplot(data, mapping = aes(x=value, fill = group)) + # data that should be plotted
  geom_histogram(position = "identity", alpha = .5, color = "gray", bins = 100) + # histogram & settings
  scale_fill_manual(values=c("#69b3a2", "#404080")) + #fill color of histogram bars
  theme_dark() + # plot theme
  facet_wrap(vars(group), nrow = 2) +
  labs(title = "Random numbers") +
  xlab("values") +
  ylab("counts")

data$value_2 <- c(data[1:1000, "value"]*2 + rnorm(group_n, mean = 0), data[1001:2000, "value"]*-.5 + rnorm(group_n, mean = 0))

ggplot(data, mapping = aes(x=value, y = value_2, color = group)) +
    geom_point(position = "identity") +
    facet_wrap(vars(group)) +
    xlim(-5,7.5) +
    ylim(-8,8) +
    labs(title = "Distribution of Group 1 to Group 2")