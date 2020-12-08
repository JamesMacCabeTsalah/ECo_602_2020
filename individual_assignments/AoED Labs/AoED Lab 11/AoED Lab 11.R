# ---- Lab 11 - Frameworks ----
  # https://michaelfrancenelson.github.io/eco_602_634_2020/assignments/eco_634/lab_11.html

# Simulation ≈ Forward Modeling 
  # You pick a model and parameters and work forward to predict patterns in the data.

# Simulation is a way to test whether you can correctly estimate the parameters of an environmental system.
# We can simulate the system using our model and see what kind of range of data we might observe.
# This can give us great insight into the level of certainty we have in our model.

#This is the theoretical basis for the Frequentist approach to statistical inference, 
  # in which we evaluate the likelihood of our data given the model, 
  # which implicitly means how likely would we observe our original data if we were to repeatedly sample the system.

# This is exactly what simulation allows us to do: repeatedly sample the environmental system under the assumption that the model is the truth.


rm(list = ls())

bird.sub = read.csv(here::here("data", "bird.sub.csv"))
hab.sub = read.csv(here::here("data", "hab.sub.csv"))
birdhab = merge(bird.sub, hab.sub, by = c("basin", "sub"))
dim(birdhab)

# ---- PA_linear_sim_1 ----

# Power analysis using simulation models allows you to explore how different experimental designs and sample sizes 
# might affect your ability to get a reasonably precise estimate of your parameters.

fit_1 = lm(BRCR ~ ls, data = birdhab)

plot(BRCR ~ ls, data = birdhab)
abline(fit_1)

# To generate data based on a Group 1 Simple Linear Regression, we need to know...
  # Parameters for the deterministic model:
    # y-intercept
    # slope coefficient
  # Parameter for the stochastic model: σ

linear = function(x, y_int, slope)
{
  return(y_int + slope*x)
}

# Will generate random data based on a linear deterministic function with normally-distributed errors
  # Generate the y-values on the line.
  # Add normally-distributed errors and return.

linear_simulator = function(x, y_int, slope, st_dev)
{
  return(y_int + slope*x + rnorm(x, sd = st_dev))
}

# To simulate new data, you need to retrieve the intercept, slope, and standard deviation from the model you fit to the data. 
  # You can use coefficients() to extract the intercept and slope values. 
  # Use str() to examine the output of coefficients() 
fit_1_coefs = coefficients(fit_1)
str(fit_1_coefs)
  # To extract standard deviation data, you must summon sigma from the summary table of your fit_1
fit_1_summary = summary(fit_1)
fit_1_summary$sigma # sigma = standev in the summary table

int_obs = fit_1_coefs[1]
slope_obs = fit_1_coefs[2]
sd_obs = fit_1_summary$sigma

# The first goal of the simulation is to generate some new “data” using our model.
  # We have several options:
    # Allow x (the late successional forest extent) vary randomly between 1 and 100.
    # Use the observed x values.
    # Resample the observed x values

# ~ we are going to keep the original values of x and let brown creeper abundance vary among simulations ~ #

# ---- PA_visual_analysis ----
# if you want to distinguish between the simulation and the orignal data, you can run them separately
  # otherwise plot with x = birdhab$ls, and y = linear_simulator(...)
plot(
  birdhab$ls, birdhab$BRCR, 
  xlab = "late-successional forest extent",
  ylab = "Brown Creeper abundance",
  pch = 19)


points(
  x = birdhab$ls, 
  y = linear_simulator(
    x = birdhab$ls,
    y_int = int_obs,
    slope = slope_obs,
    st_dev = sd_obs),
  col = adjustcolor("red", alpha = 0.3),
  pch = 16)

# notes: One problem with the use of the normal distribution is that it is unbounded on the lower limit. 
  # Thus, negative values are possible *** (no bueno)

# ---- PA_linear_sim_2 ----

# How do the quality and quantity of the data and the true properties (parameters) of the environmental system 
# affect the quality and of the answers to our questions about environmental systems? ***

# 1) Simulate a dat set with a given intercept and slope, and number of data points; 
# 2) Run a linear regression; 
# 3) Extract the p-value 
  # This represents the probability of observing our data if in fact it came from distribution described by the null model, 
  # which in this case means that brown creeper abundance is independent of ls or has no relationship to ls).
# 4) Determine whether it is less than our specified alpha criterion (usually 0.05)

y_sim = linear_simulator(
  x = birdhab$ls,
  y_int = int_obs,
  slope = slope_obs,
  st_dev = sd_obs
)

fit_sim = lm(y_sim ~ birdhab$ls)
summary(fit_sim)$coefficients

# To estimate the probability of successfully rejecting the null hypothesis when it is false (the power), 
# we have to repeat this procedure many times and calculate the proportion of the time that we reject the null hypothesis.

# 1) Specify the number of simulations to run and set up a vector to hold the p-value for each simulation.
# 2) Repeat what we did above, each time saving the p-value in the storage vector
  # (without redefining some of the objects that have not changed, such as x_vals, a, b, and y.error)
# 3) Calculate the power by = ∑rejected null hypothesis @ alpha-level /  number of simulations 
  # this converts it to a proportion 

n_sims = 1000 # Step 1
p_vals = numeric(n_sims) # numeric() creates an vector of length n_sims with all values initiailized to zero
for(i in 1:n_sims)
{ 
  y_sim = linear_simulator(
    x = birdhab$ls,
    y_int = int_obs,
    slope = slope_obs,
    st_dev = sd_obs
  )
  fit_sim = lm(y_sim ~ birdhab$ls)
  
  p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)'] # Step 2
}
sum(p_vals < 0.05) / n_sims # Step 4

# Agile Power Analysis Function Skeleton
linear_sim_fit = function(x, y_int, slope, st_dev)
{
  y_sim = linear_simulator(
    x = x,
    y_int = y_int,
    slope = slope,
    st_dev = st_dev)
  return(lm(y_sim ~ x))
}

# ---- PA_single_param ----
# We want to know how the power changes as we change some aspect of the design - 
  # such as the sample size or the effect size (slope, in this case).
# Thus, we have to repeat the entire procedure multiple times, 
  # each time changing some parameter of the simulation such as the slope or the sample size.

alpha = 0.05
n_sims = 10
p_vals = numeric(n_sims)
effect_size = seq(-.01, .01, by = 0.001) # vector of slope values
  # larger = smoother graph
effect_power = numeric(length(effect_size)) # storage vector to hold the results
  # 1 - false negative rate

for(j in 1:length(effect_sizes)) # creates a vector of power values for increasing values of slope (each outer loop iteration)
{
  for(i in 1:n_sims)
  {
    fit_sim = linear_sim_fit(
      x = birdhab$ls,
      y_int = int_obs,
      slope = effect_size[j],
      st_dev = sd_obs)
    
    p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
  }
  effect_power[j] = sum(p_vals < alpha) / n_sims
}

plot(effect_sizes, effect_power, type = 'l', xlab = 'Effect size', ylab = 'Power')
abline(v = coef(fit_1)[2], lty = 2, col = 'red')

# We can do this for a gradient of sample sizes! (one coefficient at a time)
alpha = 0.05
n_sims = 100
p_vals = numeric(n_sims)
sample_sizes = seq(10, 50)
sim_output_1 = numeric(length(sample_sizes))

for(j in 1:length(sample_sizes))
{
  x_vals = seq(0, 100, length.out = sample_sizes[j])
  
  for(i in 1:n_sims)
  {
    fit_sim = linear_sim_fit(
      x = x_vals,
      y_int = int_obs,
      slope = slope_obs,
      st_dev = sd_obs
    )
    p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
  }
  sim_output_1[j] = sum(p_vals < alpha) / n_sims
}

plot(sample_sizes, sim_output_1,  type = 'l', xlab = 'Sample size', ylab = 'Power')
abline(v = nrow(birdhab), lty = 2, col = 'red')

# This shows us that lowering sample sizes from 30 down to 20 doesn't do very much! ***

# ---- PA_multi_param ----
# We can do these power analysis simulations for multiple parameters at a time!
  # Only difference in this code is that we added a third outer loop and created a matrix to store the results, 
  # since we have a power result for each combination of slope and sample size.
alpha = 0.05
n_sims = 10
p_vals = numeric(n_sims)
effect_sizes = seq(-.01, .01, by = 0.001)
sample_sizes = seq(10, 50)

sim_output_2 = matrix(nrow = length(effect_sizes), ncol = length(sample_sizes))

for(k in 1:length(effect_sizes))
{
  effect_size = effect_sizes[k]
  for(j in 1:length(sample_sizes))
  {
    x_vals = seq(0, 100, length.out = sample_sizes[j])
    
    for(i in 1:n_sims)
    {
      fit_sim = linear_sim_fit(
        x = x_vals,
        y_int = int_obs,
        slope = effect_size,
        st_dev = sd_obs)
      p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
    }
    sim_output_2[k, j] = sum(p_vals < alpha) / n_sims
  }
}

# ---- plotting_types ----
image(sim_output_2)
  # image() is a quick way to plot a matrix as if it were raster data, 
  # that is to plot a grid in which the pixel color is determined by the value of the matrix element.

# contour(x = effect_size, 
        # y = sample_sizes,
        # z = sim_output)
# Contour plots are similar to topographic maps.
# The [iso]lines show interpolated lines at which the value is the same.
  # x-axis = effect size
  # y-axis to be sample size.
  # z-coordinates are a matrix in which cells represent the values for which to create the contours

persp(
  x = effect_sizes, y = sample_sizes, z = sim_output_2,
  col = 'lightblue',
  theta = 30, phi = 30, expand = .75,
  ticktype = 'detailed')

install.packages("rgl")
require("rgl")
persp3d(
  x = effect_sizes, y = sample_sizes, z = sim_output_2,
  col = 'lightblue',
  theta = 30, phi = 30, expand = .75,
  ticktype = 'detailed')

# --- saving_R_data_objects ----

save(
  sim_output_2, 
  file = here::here("data", "sample_size_effect_size_power_sim.Rdata"))

load(file = here::here("data", "sample_size_effect_size_power_sim.Rdata"))

# write.csv()

# ---- pop_dispersion_analysis_single -----

alpha = 0.05
n_sims = 100
p_vals = numeric(n_sims)
sd_obs = fit_1_summary$sigma
  # sd = 0.1412668

n_sds = 100
pop_sds = seq(from = 0.05, to = 1.5, length.out = n_sds) # vector of standevs
pop_sd_power = numeric(length(pop_sds)) # storage vector to hold the results 

for(j in 1:length(pop_sds))
{
  pop_sd_j = pop_sds[j]
  for(i in 1:n_sims)
  {
    fit_sim = linear_sim_fit(
      x = birdhab$ls,
      y_int = int_obs,
      slope = slope_obs,
      st_dev = pop_sd_j) # pop_sd_j changes with each iteration    
    
    p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
  }
  pop_sd_power[j] = sum(p_vals < alpha) / n_sims
}

sim_output_dispersion = data.frame(sd = pop_sds, power = pop_sd_power)
write.csv(sim_output_dispersion, file = "sim_output_dispersion.csv")

sim_output_dispersion = read.csv(here::here("data", "sim_output_dispersion.csv"))

plot(pop_sds, pop_sd_power, type = 'l', 
     xlab = 'Population Standard Deviation', ylab = 'Power',
     main = "Dispersion Power Plot")
abline(v = sd_obs, lty = 2, col = 'red')

# ---- pop_dispersion_analysis_multi -----

alpha = 0.05
n_sims = 100
p_vals = numeric(n_sims)
n_sds = 10

pop_sds = seq(from = 0.05, to = 1.5, length.out = n_sds) # vector of standevs
sample_sizes = seq(50, 150)

sim_output_3 = matrix(nrow = length(pop_sds), ncol = length(sample_sizes))

for(k in 1:length(pop_sds))
{
  pop_sd_k = pop_sds[k]
  for(j in 1:length(sample_sizes))
  {
    x_vals = seq(0, 100, length.out = sample_sizes[j])
    
    for(i in 1:n_sims)
    {
      fit_sim = linear_sim_fit(
        x = x_vals,
        y_int = int_obs,
        slope = slope_obs,
        st_dev = pop_sd_k) # pop_sd_k changes with each iteration
      
      p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
    }
    sim_output_3[k, j] = sum(p_vals < alpha) / n_sims
  }
}
image(sim_output_3)

# ---- Q2 ----

persp3d(
  x = pop_sds, y = sample_sizes , z = sim_output_3,
  col = 'yellow',
  theta = 100, phi = 100, expand = 0.75,
  ticktype = 'detailed')

rgl::writeWebGL(
  dir = here::here("data", "webGL"), 
  filename = here::here(
    "data", "webGL",
    "sds_sample_power_sim_plot.html"),
  width = 1200, height = 1200
)

# ---- Q3 ----
contour(x = pop_sds, 
        y = sample_sizes,
        z = sim_output_3,
        xlab = "pop_sds",
        ylab = "sample_sizes",
        main = "Contour Plot of Sample Size and Population Dispersion Power Simulation")


