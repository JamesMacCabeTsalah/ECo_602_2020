# The Ricker Function
  # a = determines initial slope
  # b = determines x-coordinate of peak
  # a & b = determine y-value of peak!

ricker_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}

curve(
  ricker_fun(x, 1, 1), 
  from = 0, to = 5, add = FALSE, 
  main = "Ricker function: a = 1, b = 1",
  ylab = "f(x)", xlab = "x")

# curve()
  # from = __, to = __ : tells us range of where to draw curve
  # add = FALSE : creates a new plot
    # add = TRUE : adds to current plot

# ---------------------------------------------------------- #
exp_fun = function(x, a, b)
{
  return(a * exp(b * x))
}

curve(
  exp_fun(x, 0.1, 0.5), 
  from = 0, to = 10, add = FALSE, 
  main = "Exponential function",
  ylab = "f(x)", xlab = "x")

# ---------------------------------------------------------- #
# Normally-Distributed Errors

set.seed(1234567)
n_pts = 50
x_min = 2
x_max = 10

x_sim = runif(n_pts, min = x_min, max = x_max)
# runif() = random uniform distribution!

# Next, we can choose an intercept and slope for our determinsitic model and generate the ‘predicted’ y values:
  
param_intercept = 2.3
param_slope = 0.67
y_pred = param_intercept + x_sim * param_slope
plot(x_sim, y_pred)

# Add Normally-Distributed NOISE to generate our 'observed' values
error_mean = 0
error_sd = 0.25

y_observed = y_pred + rnorm(n = n_pts, 
                            mean = error_mean, 
                            sd = error_sd) # you can make this more sophisticated by adding x_sim to error_sd!

plot(x_sim, y_observed)

# ---------------------------------------------------------- #
# Exponentially-Distributed Errors

# rexp() : generates exponentially-distributed errors

# ---------------------------------------------------------- #
# When looking at a scatterplot, looking at the histogram of residuals is helpful!
  # hist(y_observed - y_pred,..., xlab = "observed y-values")
    # a.k.a hist(residuals,..., xlab = "observed y-values")

# ---------------------------------------------------------- #
# Lab 5 Questions

rm(list = ls()) # clears global environment w/out unloading packages

dev.off()

# Plot the relationship between juvenile dispersal (disp.rate.ftb) and distance (dist.class).
plot(dat_dispersal$disp.rate.ftb ~ dat_dispersal$dist.class, 
     xlab = "Distance Class", 
     ylab = "Dispersion Rate")

# ---- Q1 ----
exp_fun = function(x, a, b)
{
  return(a*exp(-b*x))
}

{
  # curve 1: a = 1.9, b = 0.1, line color = black, line texture = solid
  curve(exp_fun(x, 1.9, 0.1),
        from = 0, to = 35,
        main = "Exponential Functions",
        y = "f(x)",
        col = "black", lty = "solid",
        add = F)
  # curve 2: a = 1.9, b = 0.3, line color = black, line texture = dotted
  curve(exp_fun(x, 1.9, 0.3), 
        col = "black", lty = "dashed",
        add = T)
  # curve 3: a = 1.2, b = 0.2, line color = red, line texture = solid
  curve(exp_fun(x, 1.2, 0.2),
        col = "red", lty = "solid",
        add = T)
  # curve 4: a = 1.2, b = 0.4, line color = red, line texture = dotted
  curve(exp_fun(x, 1.2, 0.4),
        col = "red", lty = "dashed",
        add = T)
}

# ---- Q3 ----

ricker_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}

{
  # curve 1: a = 25, b = 0.1, line color = black, line texture = solid
  curve(ricker_fun(x, 25, 0.1),
        from = 0, to = 60,
        main = "Ricker Functions",
        ylab = "f(x)",
        add = F) # add = F because it is the first curve - thus it isn't adding to anything 
  # curve 2: a = 20, b = 0.2, line color = black, line texture = dotted
  curve(ricker_fun(x, 20, 0.2),
        col = "black", lty = "dashed",
        add = T)
  # curve 3: a = 10, b = 0.2, line color = black, line texture = dotted
  curve(ricker_fun(x, 10, 0.2),
        col = "black", lty = "dashed",
        add = T)
  # curve 4: a = 75, b = 0.3, line color = red, line texture = solid
  curve(ricker_fun(x, 75, 0.3),
        col = "red", lty = "solid",
        add = T)
  # curve 5: a = 50, b = 0.3, line color = red, line texture = dotted
  curve(ricker_fun(x, 50, 0.3),
        col = "red", lty = "dashed",
        add = T)
  # curve 6: a = 40, b = 0.3, line color = red, line texture = dotted
  curve(ricker_fun(x, 40, 0.3),
        col = "red", lty = "dashed",
        add = T)
}

# ---- Q5 ----

sal_dispersal = read.csv("https://michaelfrancenelson.github.io/eco_602_634_2020/data/salamander_dispersal.csv")
  # dist.class = distance class, based on 100 m intervals;
  # disp.rate.ftb = standardized dispersal rate for first-time breeders = relative dispersal probability.
  # disp.rate.eb = standardized dispersal rate for experienced breeders = relative dispersal probability.

dat_dispersal = data.frame(sal_dispersal)
dat_dispersal

plot(dat_dispersal$disp.rate.ftb ~ dat_dispersal$dist.class,
     main = "Juvenile Marbled Salamander Dispersion",
     xlab = "Distance Class (m)",
     ylab = "Juveline Dispersion Rate")

# ---- Q6 ----
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

{
plot(dat_dispersal$disp.rate.ftb ~ dat_dispersal$dist.class,
     main = "Juvenile Marbled Salamander Dispersion",
     xlab = "Distance Class (m)",
     ylab = "Juveline Dispersion Rate")
curve(line_point_slope(x, 1600, 0.06, -0.00007), add = T)
}

# ---- Q7 ----
{
plot(dat_dispersal$disp.rate.ftb ~ dat_dispersal$dist.class,
     main = "Juvenile Marbled Salamander Dispersion",
     xlab = "Distance Class (m)",
     ylab = "Juveline Dispersion Rate")
curve(exp_fun(x, 0.7, 0.002), add = T)
}

# ---- Q8 ----
{
plot(dat_dispersal$disp.rate.ftb ~ dat_dispersal$dist.class,
     main = "Juvenile Marbled Salamander Dispersion",
     xlab = "Distance Class (m)",
     ylab = "Juveline Dispersion Rate")
curve(ricker_fun(x, 0.01, 0.006), add = T)
}

# ---- Q9 ----
{
  resids_linear = 
    dat_dispersal$disp.rate.ftb -
    line_point_slope(
      dat_dispersal$dist.class, 
      1600, 0.06,
      -0.0007)  
    
    hist(resids_linear, main = "residuals: linear model")
}
# ---- Q10 ----
# Q10
{
  resids_exp =
    dat_dispersal$disp.rate.ftb -
    exp_fun(
      dat_dispersal$dist.class, 
      0.07, 0.002) 
    
    hist(resids_exp)
}
# ----- Q11 ----
# Q11
{
  resids_ricker = 
    dat_dispersal$disp.rate.ftb -
    ricker_fun(
      dat_dispersal$dist.class, 
      0.01, 0.006) 
  
  hist(resids_ricker)
}

# residuals = observed - predicted


# You can source scripts from other R-scripts using source()
# source(package::here(insert_R_script_to_reference)
# package:: = require(package)
