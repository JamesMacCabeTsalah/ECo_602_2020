# Lab 6 - Resampling Methods

rm(list = ls()) # clears global environment w/out unloading packages

# ---- SSE ----
require(palmerpenguins)

{
  pbill_depth = subset(penguins$bill_depth_mm, subset = is.na(penguins$bill_depth_mm) != TRUE) # ! means NOT
  pbill_depth_sd = sd(pbill_depth)
  
  n_bill_depth = length(penguins$bill_depth_mm)
  
  sse_mean = function(sd, n)
  { 
    return(sd / sqrt(n))
  }
  
  sse_mean(pbill_depth_sd, n_bill_depth)
}

# ---- droplevels() ----
# droplevels() removes unused factor levels from a data.frame
?droplevels()
dat_pen = droplevels(subset(penguins, species != "Gentoo"))

# ---- ttest() ----
?ttest()
# t.test(x ~ y)
  # you can use $ to retrieve a specific object to t-test!
    # use str() to see what objects can be summoned with $
  # t_test$estimate : estimate is an output of the t-test that tells us estimated means(?)

# sample() can resample WITH or WITHOUT replacement - and can be paired with set.seed(###) 
  # sample(...., replace = TRUE/FALSE)

# when using mean() - na.rm = TRUE is helpful! 
  # na.rm = "NA Removed"

# diff() gives the difference between values of set intervals
?diff()

# tell the difference between means!
diff_observed = round(diff(t_test$estimate), digits = 3)
print(diff_observed, digits = 3)

# ---- aggregate() ----
# aggregate() subsets an R-object and calcualtes a summary statistic for each subset
?aggregate()
  # aggregate(x ~ y, data = dat, FUN = insert_function, na.rm = TRUE)

# For example, aggregate() can be used to calculate the difference in means!

# plug your aggregate means into diff() to put everything into a single output!
diff_observed = diff(agg_means[, 2])

# ---- two_group_resample ----
{
x = 
n_1 = 
n_2 = 

two_group_resample = function(x, n_1, n_2)
{
  dat_1 = sample(x, n_1, replace = TRUE)
  dat_2 = sample(x, n_2, replace = TRUE)
}
}

two_group_resample()

diff_simulated = mean(dat_1, na.rm = TRUE) - mean(dat_2, na.rm = TRUE)

# ---- resampling ----

n = 
mean_differences = c()
for (i in 1:n)
{
  mean_differences = c(
    mean_differences,
    two_group_resample(x, n_1, n_2)
  )
}
hist(mean_differences)

sum(abs(mean_differences) >= diff_observed) 

# ---- Lab 6 ----
# ---- Q1 ----
sse_mean = function(sd, n)
{ 
  return(sd / sqrt(n))
}

# ---- Q2 ----

{
  x = 
  n_1 = 
  n_2 = 
  
    two_group_resample = function(x, n_1, n_2)
    {
      dat_1 = sample(x, n_1, replace = TRUE)
      dat_2 = sample(x, n_2, replace = TRUE)
      diff_simulated = mean(dat_1, na.rm = TRUE) - mean(dat_2, na.rm = TRUE)
      diff_simulated
    }
}

two_group_resample() # default function call

two_group_resample(dat_pen$flipper_length_mm, 68, 152) # penguin data input

# ---- Q3 ----
{
require(palmerpenguins)

dat_pen = droplevels(subset(penguins, species != "Gentoo")) # removed Gentoo Penguins from the species list!
table(dat_pen$species) # tells us n!

  x = dat_pen$flipper_length_mm
  n_1 = 68 # Chinstrap
  n_2 = 152 # Adelie
  
  two_group_resample = function(x, n_1, n_2)
  {
    dat_1 = sample(x, n_1, replace = TRUE)
    dat_2 = sample(x, n_2, replace = TRUE)
    diff_simulated = mean(dat_1, na.rm = TRUE) - mean(dat_2, na.rm = TRUE) 
    diff_simulated
  }

n = 2000 # how many times to repeat sampling!
  mean_differences = c()
for (i in 1:n)
{
  mean_differences = c(
    mean_differences,
    two_group_resample(x, n_1, n_2))
}
hist(mean_differences,
     main = "Mean Differences of Adelie and Chinstrap Flipper Length (mm)")
}
# ---- Q4 ----

# t-test is good for comparing the measn of two groups because it is a part of the output!
t_test_flipper = t.test(dat_pen$flipper_length_mm ~ dat_pen$species) 

flip_diff_observed = round(diff(t_test_flipper$estimate), digits = 3)
sum(abs(mean_differences) >= flip_diff_observed)

sum(abs(mean_differences) >= 5.8) # answer

# ---- Q5 ----

# Answer: Given a p-value of less than 1 per 10 million, you'd need to run
# 10 million simulations to find just 1 simulation with a flipper length >= 5.8!

# ---- Q6 ----

  # Q6.1
# boxplot of bill length as it pertains to species!
boxplot(dat_pen$bill_length_mm ~ dat_pen$species)

  # Q6.2
# group means
group_means_bl = aggregate(
  bill_length_mm ~ species,
  data = dat_pen,
  FUN = mean,
  na.rm = TRUE)

# difference in means 
diff_crit = diff(group_means_bl[, 2]) 

diff_crit # gives difference in means 

# Group Means: Adelie = 38.79, Chinstrap = 48.83
# Difference between Means = 10.04

# ---- Q7 ----

t_test_billL = t.test(dat_pen$bill_length_mm ~ dat_pen$species) 
str(t_test_billL)
  # t_test_billL$estimate # gives means of both species!
t_test_billL$p.value

# The output P-value = 2.9 • 10^-41: meaning that if there was no correlation between the two variables,
# we would observe our dataset in 1 / (2.9 • 10^41) samples of the population!

# ---- Q8 ----
{
  require(palmerpenguins)
  
  dat_pen = droplevels(subset(penguins, species != "Gentoo")) # removed Gentoo Penguins from the species list!
  
  x = dat_pen$bill_length_mm
  n_1 = 68 # Chinstrap
  n_2 = 152 # Adelie
  
  two_group_resample = function(x, n_1, n_2)
  {
    dat_1 = sample(x, n_1, replace = TRUE)
    dat_2 = sample(x, n_2, replace = TRUE)
    diff_simulated = mean(dat_1, na.rm = TRUE) - mean(dat_2, na.rm = TRUE) 
    diff_simulated
  }
  
  n = 1000 # how many times to repeat sampling!
  mean_differences = c()
  for (i in 1:n)
  {
    mean_differences = c(
      mean_differences,
      two_group_resample(x, n_1, n_2))
  }
  hist(mean_differences,
       main = "Mean Differences of Adelie and Chinstrap Bill Length (mm)")  
}

sum(abs(mean_differences) >= diff_crit, na.rm = TRUE)





































