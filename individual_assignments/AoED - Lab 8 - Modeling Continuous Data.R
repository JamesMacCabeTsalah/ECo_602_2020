# Lab 8 - Modeling Continuous Data
  # https://michaelfrancenelson.github.io/eco_602_634_2020/assignments/eco_634/lab_08.html
  # We’ll be using bootstrap resampling to explore sampling distributions of model parameters 
  # in the context of one and two sample difference of means tests.

rm(list = ls())

# t-tests are useful when we ahve a categorical predictor with two levels and a continuous response variabl

# ---- simpleboot() ----
install.packages("simpleboot")
require(simpleboot)

# ?two.boot()

require(palmerpenguins)
pen_dat = droplevels(subset(penguins, species != "Gentoo"))
pen_boot = two.boot(
  subset(pen_dat, species == "Adelie")$flipper_length_mm, 
  subset(pen_dat, species == "Chinstrap")$flipper_length_mm,
  FUN = mean, 
  R = 10000,
  na.rm = TRUE)

hist(pen_boot,
     main = "Histogram of 10,000 Bootstrap Differences in mean Penguin Flipper Length",
     xlab = "Difference in mean Flipper Length (mm) of Adelie and Chinstrap Penguins",
     breaks = 7)
# --- --- --- --- --- --- --- --- --- --- #
veg = read.csv(here::here("data", "vegdata.csv"))

dat_tree = droplevels(subset(veg, treatment %in% c("control", "clipped")))
  #  %n% : 

length(subset(dat_tree, treatment == "clipped"))
length(subset(dat_tree, treatment == "control"))

# ---- nonparametric_two_sample_test ----

# Wilcoxon Rank Sum Test on the difference in means between treatments

# sum(tree_boot$t >= 0)
# sum(tree_boot$t < 0)

# boot.ci() : nonparametric boostrap confidence intervals
  ?boot.ci()

# quantile(x, CI%)

# ---- Resampling_Simple_Linear_Regression ----

dat_bird = read.csv(here::here("data", "bird.sub.csv"))
dat_habitat = read.csv(here::here("data", "hab.sub.csv"))
  # subbasin files (.sub) are aggregated by sub-basin and standardized from the full bird census count data (.sta)

dat_all = merge(
  dat_bird, 
  dat_habitat,
  by = c("basin", "sub")) 
    # basin (3 unique basins)
    # sub (10 unique sub-basins in each basin).

# Simpson's Diversity Index is a measure of diversity which takes into account species richness and evenness 
# As species richness and evenness increase, so diversity increases.
  # b.sidi = Simpson’s diversity index for breeding birds ***
  # s.sidi = Simpson’s diversity index for vegetation cover types ***

# fit = lm(x ~ y, data = data.frame)
  # lm() is used to fit linear models assuming that the errors are normally distributed about the mean ***
?lm()
# coef(fit)
?coef()

{ # We can fit a simple linear regression using a Least Squares criterion.
fit_1 = lm(b.sidi ~ s.sidi, data = dat_all) # Slope coefficient = s.sidi since that is the predictor we specify in the model.
coef(fit_1) # Model coefficients = coef() 

slope_observed = coef(fit_1)[2] # We want to save the slope for later use
}

{ # Plot with simple linear regression
plot(
  b.sidi ~ s.sidi, data = dat_all,
  main = "Simpson's diversity indices",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")
abline(fit_1) #abline is based on simple, assumed to be normally distributed linear regression
}

# bird diversity declines as the vegetation diversity increases.

# ---- Monte_Carlo_Resampling ----
# How likely it is for us to observe a slope coefficient this large and negative if our null hypothesis is true?
  # lm() is used to fit linear models assuming that the errors are normally distributed about the mean = not always TRUE

# Monte Carlo Randomization: breaks up the associations by sampling random values from each column, in stead of keeping rows intact

{
# (1) Extract just the two variables we need for this exercise
dat_1 = 
  subset(
    dat_all,
    select = c(b.sidi, s.sidi)) # select indicates columns to select from a data frame

# (2) Create Monte Carlo Variables - We can create two vectors of randomly generated row indices ***
index_1 = sample(nrow(dat_1), replace = TRUE)
index_2 = sample(nrow(dat_1), replace = TRUE)

# (3) Then we can use these to create two new vectors of bird and vegetation diversity indices.
dat_resampled_i = 
  data.frame(
    b.sidi = dat_1$b.sidi[index_1],
    s.sidi = dat_1$s.sidi[index_2]
  )

# (4) Results
fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
slope_resampled_i = coef(fit_resampled_i)[2] # [2] sources the slope intercept, while [1] sources the y-intercept

print(slope_resampled_i)

# (5) Recreate scatterplot w/ regression line
plot(
  b.sidi ~ s.sidi, data = dat_resampled_i,
  main = "Simpson's diversity indices",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")
abline(fit_resampled_i) #abline is now based on Monte Carlo trendline
}

# ---- MC_Randomization_Loop ----

m = 10000 
result = numeric(m) # we can pre-allocate a vector to hold the results using numeric()
?numeric()

for(i in 1:m)
{
 # (1) Create Monte Carlo Variables - We can create two vectors of randomly generated row indices  
  index_1 = sample(nrow(dat_1), replace = TRUE)
  index_2 = sample(nrow(dat_1), replace = TRUE)
  
  # (2) Then we can use these to create two new vectors of bird and vegetation diversity indices.
  dat_resampled_i = 
    data.frame(
      b.sidi = dat_1$b.sidi[index_1],
      s.sidi = dat_1$s.sidi[index_2]
    )
  
  fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
  result[i] = coef(fit_resampled_i)[2] # [2] sources the slope intercept, while [1] sources the y-intercept 
} 

# The loop output is a collection of regression slope parameters that were calculated on randomized and resampled data.

# This loop takes a pre-allocated vector 'result' - which is empty 10,000 numerics - 
# and fills each numeric with every iteration of the loop.
  # As i increases by 1 each iteration, i acts as a mapping tool for the newly iterated monte carlo
  # resample to be input into 'result'

# ---- Null_Distribution ----

hist(result, main = "Null Distribution of Regression Slope", xlab = "Slope Parameter")
abline(v = slope_observed, lty = 2, col = "red", lwd = 2)

# ---- Critical_Slope_Value ----

quantile(result, 0.05)

# Do you remember what the observed slope of the real data was?
  
  # How does it compare to the lower critical value?
  
# How many slopes from the Monte Carlo randomization were equal to or less than the observed slope?

# ---- Q2 ----
# 95% Confidence interval on the difference in mean flipper lengths
quantile(pen_boot$t, 0.95)
# ---- Q3 ----
# Create a distribution function from pen_boot using ecdf() 
pen_ecdf = ecdf(pen_boot$t)

# ecdf() = Empirical Cumulative Distribution Function
?ecdf()
# ---- Q4 ----
# There is no difference between mean flipper length between Adelie and Chinstrap
# ---- Q5 ----
pen_ecdf(-4,5)
pen_ecdf(0)
# ---- Q6 ----
wilcox.test(pine ~ treatment, data = dat_tree, alternative = "two.sided")
# ---- Q7 ----
# Use two.boot() to create a bootstrapped data set of the differences in mean tree count between the clipped and control treatments.
veg = read.csv(here::here("data", "vegdata.csv"))
dat_tree = droplevels(subset(veg, treatment %in% c("control", "clipped")))

tree_boot = 
  two.boot(
    subset(dat_tree, treatment == "clipped")$pine,
    subset(dat_tree, treatment == "control")$pine,
    FUN = mean,
    R = 10000,
    na.rm = TRUE
  )

# Use quantile() to find a 95% CI.
quantile(tree_boot$t, 0.95)

# Q7.1: what is an endpoint???
# Q7.2: ???
  
# ---- Q8 ----
# Code a loop to resample the slope parameter of a simple linear regression of the Simpson’s diversity indices for vegetation and birds.

# Setup Data
dat_bird = read.csv(here::here("data", "bird.sub.csv"))
dat_habitat = read.csv(here::here("data", "hab.sub.csv"))
dat_all = merge(
  dat_bird, 
  dat_habitat,
  by = c("basin", "sub")) 






# ??????????????????????????

# ---- Q9 ----
{
  # Extract just the two variables we need for this exercise
  dat_1 = 
    subset(
      dat_all,
      select = c(b.sidi, s.sidi)) # select indicates columns to select from a data frame
  
  m = 10000 
  result = numeric(m) # we can pre-allocate a vector to hold the results using numeric()
  
  for(i in 1:m)
  {
    # (1) Create Monte Carlo Variables - We can create two vectors of randomly generated row indices  
    index_1 = sample(nrow(dat_1), replace = TRUE)
    index_2 = sample(nrow(dat_1), replace = TRUE)
    
    # (2) Then we can use these to create two new vectors of bird and vegetation diversity indices.
    dat_resampled_i = 
      data.frame(
        b.sidi = dat_1$b.sidi[index_1],
        s.sidi = dat_1$s.sidi[index_2]
      )
    
    fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
    result[i] = coef(fit_resampled_i)[2] # [2] sources the slope intercept, while [1] sources the y-intercept 
  } 
}
# ---- Q10 ----
quantile(result, 0.05)
# Q10.1.1: Critical Value = -0.0132565
# Q10.1.2: The observed slope of ____ was ____ than the critical value of -0.0132565

# Q10.2: My conclusion is that




