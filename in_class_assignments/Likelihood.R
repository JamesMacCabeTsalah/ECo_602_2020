# dpois : poisson distribution, gives probability mass 

# Likelihood of particular counts is the product of individual likelihoods
  # vector = c(x, y)
  # sum(log(dpois(x = vector, lambda = ))) ****

# ---- Q1 ----

wiwa_counts = c(2,6)
sum(log(dpois(x = wiwa_counts, lambda = 4)))

# ---- Q2 ----

dat_bird = read.csv(here::here("data", "bird.sta.csv"))
dat_habitat = read.csv(here::here("data", "hab.sta.csv"))
dat_all = merge(dat_bird, dat_habitat)

hist(dat_all$WIWR,
     breaks = 6,
     main = "Histogram of Winter Wren (WIWR) Observation Frequency",
     xlab = "WIWR Observations")
sum(log(dpois(x = dat_all$WIWR, lambda = 1.5)))

# ---- Q3 ----
n = (length(wiwr_counts))
length(na.omit(dat_all$WIWR)) #if you want to omit NA

sum(log(dbinom(wiwr_counts, 1046, .001)))

?dpois()


