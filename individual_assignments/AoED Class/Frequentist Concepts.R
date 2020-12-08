# Individual Assignment: Frequentist Concepts #


# pbinom: the cumulative mass function
# pnorm: the cumulative density function
  # pnorm gives us the probability of observing a value of x or less

# dbinom: the probability mass function
# dnorm: the probability density function

# qbinom: the quantile function
# qnorm: the quantile function

# ---- Q1 ----

dbinom(3, 4, 0.75)
# Output = 0.421876

# ---- Q2 ----

pbinom(3, 4, 0.75)
# Output = 0.6835937

# ---- Q3 ----

pbinom(3, 5, 0.75)
# Output = 0.3671875

1 - pbinom(3, 5, 0.75)
# Output = 0.6328125

# ---- Q4 ----

pnorm(1.2, 2, 2)
# Output = 0.3445783

# ---- Q5 ----

1 - pnorm(1.2, 2, 2)
# Output = 0.6554217

# ---- Q6 ----

pnorm(3.2, 2, 2) - pnorm(1.2, 2, 2)
# Output = 0.3811686


















