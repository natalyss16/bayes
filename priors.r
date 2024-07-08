# mean and sd of income of Los Angeles inhabitants
# not normal
mean_LA <- 5000
sd_LA <- 10000

x_LA <- seq(0, 100000, by = 100)
y_LA <- dnorm(x_LA, mean = mean_LA, sd = sd_LA)
plot(x_LA, y_LA)

# mean and standard deviation of daily temperature in Nuuk
# also not normal

mean_Nuuk <- 10
sd_Nuuk <- 7

x_Nuuk <- seq(-50, 30, by = 1)
y_Nuuk <- dnorm(x_Nuuk, mean = mean_Nuuk, sd = sd_Nuuk)
plot(x_Nuuk, y_Nuuk)

# percent alcohol and gram/liter sugar in a Barolo (dry red wine) 

# should use beta distribution
mean_wine_alco <- 12
sd_wine_alco <- 1

x_wine_alco <- seq(3, 10, by = 0.2)
y_wine_alco <- dnorm(x_wine_alco, mean = mean_wine_alco, sd = sd_wine_alco)
plot(x_wine_alco, y_wine_alco)

mean_wine_sugar <- 0.5
sd_wine_sugar <- 1

x_wine_sugar <- seq(0, 10, by = 0.1)
y_wine_sugar <- dnorm(x_wine_sugar, mean = mean_wine_sugar, sd = sd_wine_sugar)
plot(x_wine_sugar, y_wine_sugar)

# mean and standard deviation in body height in a population

mean_height <- 165
sd_height <- 20

x_height <- seq(40, 210, by = 5)
y_height <- dnorm(x_height, mean = mean_height, sd = sd_height)
plot(x_height, y_height)

# solution on GitHub