# dnorm = probability density
# dnorm(x, mean = 0, sd = 1, log = FALSE)

# pnorm = cumulative probability density
# pnorm(q, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)

# qnorm = quantile function
# qnorm(p, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)

# rnorm = function to generate random, normally-distributed numbers
# rnorm(n, mean = 0, sd = 1)

  # For binomial distributions 
# dbinom()
# pbinom()
# qbinom()
# rbinom()
# ---------------------------------------------------------------------------------- #

# Plotting a PDF - Probability Density Function
# 1. Generate a vector of x-values
x = seq(-3, 3, length.out = 1000)
  # length.out = desired length of the sequence = how many times you divide the given range into 
y = dnorm(x)
  # ?dnorm()
    # y = density: created via 100 randomly sequenced 
    # mean is by default 0
      # mean = ___ : can specify mean (center of distribution)
    # sd = 1 by default
      # sd = ___ : can specify sd

# 2. Plot!
plot(x, y, main = "Normal PDF", type = "l")
abline(h = 0)
  # ?abline()
    # abline() used to add a straight line to a plot
    # h = y-values for horizontal lines, v = x-values of vertical lines

# ---------------------------------------------------------------------------------- #

    # Lab Questions #

  # Q1
u = 10.4
standev = 2.4
set.seed(123)

norm_17 = rnorm(17, mean = u, sd = standev)
norm_17

norm_30 = rnorm(30, mean = u, sd = standev)
norm_30

norm_300 = rnorm(300, mean = u, sd = standev)
norm_300

# ---------------------------------------------------------- #
  # Q2
?png()
png("lab_04_hist_01.png", 
    width = 700, 
    height = 1400, 
    units = "px", 
    pointsize = 12,
    res = 180)

par(mfrow = c(3,1)) # c(rows, columns)

hist(norm_17,
     main = "Random Normal Distribution of 17 Data Points",
     xlab = "Distribution",
     ylab = "Frequency",
     ylim = c(0, 4),
     xlim = c(6, 16),
     col = "palegoldenrod")

hist(norm_30,
     main = "Random Normal Distribution of 30 Data Points",
     xlab = "Distribution",
     ylab = "Frequency",
     col = "palegoldenrod")

hist(norm_300,
     main = "Random Normal Distribution of 300 Data Points",
     xlab = "Distribution",
     ylab = "Frequency",
     ylim = c(0, 60),
     xlim = c(0, 20),
     col = "palegoldenrod")

# dev.off() will clear the current plot being plotted!
# dev.off() also is needed for png() to function - because this tells png() where to finish looking for code
dev.off()

# ---------------------------------------------------------- #
  # Q3
# Q3.1: In rnorm17, there are no data points at 13, but then a lot of points at 15 - which is not very normal. 
# It gets better in rnorm30, but there is the same pattern of lower frequencies which then increase where they should be 
# in less occupied standard deviations. rnorm30 drops off from a frequency of 10 in ranges between 8-10, down to 4 between 1-12. 
# It then spikes back up to a frequency of 8 between 12-14. 
# The normalized bell curve smooths out the most in rnorm300 because there are no patterns of sudden drops and spikes as seen in rnorm17 & 30. 

# Q3.2: As the sample size got larger, the normal distribution curve became more normally distributed.
# This is because with larger sample sizes comes more statistical power, and thus a more normally distributed
# curve which contains more and more normal data points that overwelm the outliers and non-normal conforming points.
# This is seen when comparing the histograms of sample sizes of 17 and 30 to 300! 
# Histograms norm_17 and norm_30 have columns which abnormally drop and rise - which is uncharacteristic of the normal distribution. 
# The histogram should aim to be bell shaped because of the gradual nature of the normal bell curve.

# ---------------------------------------------------------- #
    # Q4
png(filename = "norm_1.png", 
    width = 1280, 
    height = 720, 
    units = "px", 
    pointsize = 12,
    res = 180) 

u = 10.4
standev = 2.4

x = seq(0, 20, length.out = 1000)
y = dnorm(x, mean = u, sd = standev, log = FALSE)

plot(x = x, y = y, main = "Normal Distribution: Mean = 10.4, Standev. = 2.4", type = "l")
abline(h = 0)

dev.off()
          
# Q4.1: The parameters are between values of 0 and 20, also the mean and standev.

# ---------------------------------------------------------- #
  # Line-Point-Slope Function
# Calculates the value of y for a linear function, given the coordinates
# of a known point (x1, y1) and the slope of the line.
line_point_slope = function(x, x1, y1, slope)
{
  get_y_intercept = 
    function(x1, y1, slope) 
      return(-(x1 * slope) + y1)
  
  linear = 
    function(x, yint, slope) 
      return(yint + x * slope)
  
  return(linear(x, get_y_intercept(x1, y1, slope), slope))
}

# ---------------------------------------------------------- #
    # Q5
png(filename = "sim_data_scatterplots.png", 
    width = 1920, 
    height = 1080, 
    units = "px", 
    pointsize = 12,
    res = 180) 

par(mfrow = c(2,2)) # c(rows, columns)

# ---------------------------------------------------------- #
    # Q6 & Q7
# png(filename = "sim_data_fitted_scatterplot.png", 
   # width = 1280, 
   # height = 720, 
   # units = "px", 
   # pointsize = 12,
   # res = 180) 

set.seed(234) # set.seed() forces R to generate the same sequence when using a random number generator 
n_pts = 10
x_min = 1
x_max = 10
x = runif(n = n_pts, min = x_min, max = x_max)

guess_x = 7
guess_y = 0
guess_slope = 0.2

y_observed = rnorm(n_pts)

y_predicted = line_point_slope(x, 
                               guess_x, 
                               guess_y, 
                               guess_slope)

resids = y_observed - y_predicted

dat = data.frame(x = x, 
                 y_observed,
                 y_predicted,
                 resids)
dat


sum(resids) # Equals to 0
abs(resids) # Error: non-numeric argument to mathematical function!

plot(y_observed ~ x, data = dat, pch = 8, main = "sim_data_fitted_scatterplot (10 obs.)")
curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T)

# dev.off()

# ---------------------------------------------------------- #
# Plot #2
set.seed(456) # set.seed() forces R to generate the same sequence when using a random number generator 
n_pts_2 = 25
x_min_2 = 1
x_max_2 = 25
x_2 = runif(n = n_pts_2, 
            min = x_min_2, 
            max = x_max_2)

guess_x_2 = 7
guess_y_2 = 0
guess_slope_2 = 0.2

y_observed_2 = rnorm(n_pts_2)

y_predicted_2 = line_point_slope(x_2, 
                                 guess_x_2, 
                                 guess_y_2, 
                                 guess_slope_2)

dat_2 = data.frame(x = x_2, 
                   y_observed_2, 
                   y_predicted_2)
dat_2

plot(y_observed_2 ~ x_2, data = dat_2, pch = 8, main = "sim_data_fitted_scatterplot (25 obs.)")

# --------------------------------------------- #
# Plot #3
set.seed(789) # set.seed() forces R to generate the same sequence when using a random number generator 
n_pts_3 = 50
x_min_3 = 1
x_max_3 = 50
x_3 = runif(n = n_pts_3, 
            min = x_min_3, 
            max = x_max_3)

guess_x_3 = 7
guess_y_3 = 0
guess_slope_3 = 0.2

y_observed_3 = rnorm(n_pts_3)

y_predicted_3 = line_point_slope(x_3, 
                                 guess_x_3, 
                                 guess_y_3, 
                                 guess_slope_3)

dat_3 = data.frame(x = x_3, 
                   y_observed_3, 
                   y_predicted_3)
dat_3

plot(y_observed_3 ~ x_3, data = dat_3, pch = 8, main = "sim_data_fitted_scatterplot (50 obs.)")

# --------------------------------------------- #
#Plot #4
set.seed(101112) # set.seed() forces R to generate the same sequence when using a random number generator 
n_pts_4 = 75
x_min_4 = 1
x_max_4 = 75
x_4 = runif(n = n_pts_4, 
            min = x_min_4, 
            max = x_max_4)

guess_x_4 = 7
guess_y_4 = 0
guess_slope_4 = 0.2

y_observed_4 = rnorm(n_pts_4)

y_predicted_4 = line_point_slope(x_4, 
                                 guess_x_4, 
                                 guess_y_4, 
                                 guess_slope_4)

dat_4 = data.frame(x = x_4, 
                   y_observed_4, 
                   y_predicted_4)
dat_4

plot(y_observed_4 ~ x_4, data = dat_4, pch = 8, main = "sim_data_fitted_scatterplot (75 obs.)")

dev.off()
# ---------------------------------------------------------------------------------- #
plot_resids = function(x, y, guess_x, guess_y, guess_slope)
{
  for (i in 1:n)
  {
    segments(
      x0 = x[i], 
      y0 = line_point_slope(
        x = x[i], guess_x, guess_y, guess_slope), 
      x1 = x[i],
      y1 = y[i], 
      col = "red", lwd = 2)
  }
}
# ---------------------------------------------------------------------------------- #
