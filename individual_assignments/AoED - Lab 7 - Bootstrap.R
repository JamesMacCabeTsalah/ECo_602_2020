# Lab 7 - Bootstrapping 
  # https://michaelfrancenelson.github.io/eco_602_634_2020/assignments/eco_634/lab_07.html

# Bootstrapping is a resampling technique by which we use sampling with replacement 
# of a single sample of observations to simulate the process of taking many samples
# from a population!

# Most importantly used for calculating non-parametric CI's for parameter estimates

require(here)
getwd()

moths = read.csv(here("data", "moths.csv"))
head(moths)
hist(moths$anst)

# ---- parametric_anst_t-dist ----
# t-dist ≈ normal-dist when population-variance is unknown & thus must be estimated from sample
  # t-dist generally has heavier tails (more conservative) than the normal-dist 
    # but when n >30 it's close to normal!

alpha = 0.05 # significance level ≈ critical value
anst = moths$anst
n = sum(!is.na(anst))
t_crit = abs(qt(alpha / 2, df = n - 1))

sse = sd(anst, na.rm = TRUE) / sqrt(n)

sample_mean = mean(anst, na.rm = TRUE)
ci_parametric = sse * t_crit

confidence_intervals = 
  data.frame(
    technique = c("parametric: t-dist"),
    mean = sample_mean,
    ci_radius = sse * t_crit,
    lower = sample_mean - ci_parametric,
    upper = sample_mean + ci_parametric
  )

# ---- non_parametric_anst_t-dist ----

  # 1) Create an empty vector to hold the bootstrap sample means
m = 10000

# numeric() creates an vector of length m with all values initiailized to zero
result = numeric(m)
head(result)

  # 2) Create the bootstrap = resampled data sets and calculate the means
for(i in 1:m)
{
  result[i] = mean(sample(anst, replace=TRUE))
}

  # 3) Calculate the CI from the quantiles of the resulting bootstrap means
mean(result)
quantile(result,c(0.025,0.975)) # c((1 - CI%), CI%)

# ---- boot() ----
install.packages("boot")
  require(boot)

?boot()
# boot(data, statistic, R)
  # data is the data object you want to resample. 
    # It can be a vector, matrix, or data.frame.
  # statistic is a function that returns the statistic of interest.
  # R is the number of resamplings you want to perform

# ~ boot() comes with restrictions, so we need functions to meet those restrictions ~

# We have to create a modified version of mean() = bootmean() instead of mean()!
boot_mean = function(x, i)
{
  return(mean(x[i]))
}
  # Necessary because the first argument must be 'input data' = x 
  # Also because the 2nd argument is an index (a vector of subscripts) 
    # that is used within boot() to select random assortments of x.

myboot = 
  boot(
    data = anst,
    statistic = boot_mean,
    R = 10000)
print(myboot)
str(myboot)
  # original is the original mean of the whole sample: mean(anst).
  # bias is the difference between the original mean and the mean of the bootstrapped samples.
  # std.error is the standard deviation of the simulated values.

# Extract CI
quantile(
  myboot$t,
  c(0.025, 0.975))

# ---- Rarefaction_Curve_Info ----

# The principle behind rarefaction is that the # of species detected is related to sampling intensity
  # The greater the sampling intensity (e.g., number of sample plots or sampling area) the greater the species richness ***

# A Species-Area relationship can be interpreted differently - where area is replaced by sampling effort! ***
  # When the sampling intensity varies among sampling units...
    # rarefaction can be used to adjust the species richness estimates so that they are comparable! ***

# A rarefaction curve shows the species richness as a function of sampling intensity ***

# We can use the bootstrap curve to calculate the species richness and the confidence interval!

# ~ Interpretation: Each row is a separate bootstrap iteration, and each column represents the size of the bootstrap sample ~

# We can calculate the mean and CI% quantiles of the bootstrapped species richness for each sampling intensity apply()

rare_mean = apply(rarefact, 2, mean)
rare_quant = apply(rarefact, 2, quantile, probs=c((1-0.CI%), 0.CI%)))
rare = t(rbind(rare_mean, rare_quant))

# ---- apply() ----
# The apply functions attempt to replace the need for for-loops by providing a function-based set of alternatives
# apply() works on data frames or other 2-dimensional arrays of data, such as matrices.
# It applies a function to either the rows or columns of the input data.

?apply()
  # apply(X, MARGIN, FUN, (OPTIONAL FUN. ARGUMENTS))
# X = 2-dimensional data, usually a data frame or a matrix
# MARGIN determines whether to apply the function to rows or columns
# rows : (MARGIN = 1)
# columns (MARGIN = 2)
# FUN The function to apply to the rows or columns

# Resources: https://www.datacamp.com/community/tutorials/r-tutorial-apply-family

# ---- Q3_Debugging_Rarefaction ----
# This clears the current R session's environment
rm(list = ls())

# Re-read my data:
moths = read.csv(here::here("data", "moths.csv"))
moth_dat = moths[,-1]

rarefaction_sampler = function(input_dat, n_iterations)
{
  n_input_rows = nrow(input_dat)
  
  results_out = matrix(
    nrow = n_iterations,
    ncol = n_input_rows)
  
  # The outer loop: runs once for each bootstrap iteration.  index variable is i
  for(i in 1:n_iterations)
  {
    # The inner loop: simulates increasing sampling intensity
    # Sampling intensity ranges from 1 site to the complete count of 
    # sites in the input data (n)
    for(j in 1:n_input_rows)
    {
      # sample the input data row indices, with replacement
      rows_j = sample(n_input_rows, size = j, replace=TRUE)
      
      # Creates a new data matrix
      t1 = input_dat[rows_j, ]
      
      # Calculates the column sums
      t2 = apply(t1, 2, sum)
      
      # Counts the number of columns in which any moths were observed
      results_out[i, j] = sum(t2 > 0)
    }
  }
  return(results_out)
}

rarefact_100 = rarefaction_sampler(moth_dat, 100)
head(rarefact_100)

rarefact_10000 = rarefaction_sampler(moths[,-1], 10000)
head(rarefact_10000)

# ---- Q4_Plotting_Rarefaction_Curve ----

# create a dataset of moth data w/out useless first column
moth_dat = moths[,-1]
head(moth_dat)

  # Gives us key CI information!
rare_mean = apply(rarefact_10000, 2, mean)
rare_quant = apply(rarefact_10000, 2, quantile, probs=c(0.025, 0.975))
rare = t(rbind(rare_mean, rare_quant))

# We want to plot the rarefact curve!
?matplot()
  # Plot the columns of one matrix against the columns of another.
matplot(
  rare, # your rarefaction computation
  type='l',
  xlab='Number of sampling plots',
  ylab='Species richness', 
  main='Bootstrap Rarefaction Curve of 10 rare Massachusetts Moth Species')

legend(
  'bottomright',
  legend=c('mean','2.5%','97.5%'),
  lty=c(1,2,3),col=c(1,2,3), inset=c(.1,.1))

# ---- Q1 ----

require(palmerpenguins)
# Extract only the Gentoo penguin data from the main penguins data frame
dat_pen = subset(penguins, species == "Gentoo")
billL = dat_pen$bill_length_mm

# calculate n
n = sum(!is.na(billL))
# Output: n = 123

# calculate the critical t-values
alpha = 0.05 # significance level
t_crit = abs(qt(alpha / 2, df = n - 1))
# Output: t_crit = 1.98

# calculate the sample mean
sample_mean = mean(billL, na.rm = TRUE)
# Output: sample_mean = 47.5

# calculate the standard deviation
sample_sd = sd(billL, na.rm = TRUE)
# Output: sample_sd = 3.08

# calculate the standard error
sse = sample_sd / sqrt(n)
# Output: sse = 0.27788

# calculate the CI
ci_parametric = sse * t_crit
# Output: ci_parametric = 0.55

# ---- Q2 ----
require(boot)

# ~ boot() comes with restrictions, so we need functions to meet those restrictions ~
# We have to create a modified version of mean() = bootmean() instead of mean()!
boot_mean = function(x, i)
{
  return(mean(x[i], na.rm = TRUE))
}

myboot = 
  boot(
    data = billL, # data is the data object you want to resample. 
    statistic = boot_mean, # statistic is a function that returns the statistic of interest.
    R = 10000) # R is the number of resamplings you want to perform

# Extract CI
quantile(
  myboot$t,
  c(0.025, 0.975),
  na.rm = TRUE)





















