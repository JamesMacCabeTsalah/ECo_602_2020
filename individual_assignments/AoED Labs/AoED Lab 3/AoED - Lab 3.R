# We first want to read our data so that R registers it as a dataset!
read.csv("https://michaelfrancenelson.github.io/eco_602_634_2020/data/hab.sta.csv")
# We can then input this read dataset into a vector so we can use it for processing!
dat_habitat <- read.csv("https://michaelfrancenelson.github.io/eco_602_634_2020/data/hab.sta.csv")
read.csv("https://michaelfrancenelson.github.io/eco_602_634_2020/data/bird.sta.csv")
dat_birds <- read.csv("https://michaelfrancenelson.github.io/eco_602_634_2020/data/bird.sta.csv")

# You can also use the 'here' package to download and read data files into R from your local drive
  # here() works from your working directory, and you need to input subsequent coordinates within that working directory for it to identify it
  # Your working directory is by default based off of the folder which holds your R-script file!
    # getwd() - tells you what your current working directory is
  # You can also manually set your working directory with setwd() - this is unsafe because your wd can always change

getwd()

require(here)
here("AoED Data", "AoED Birds", "bird.sta.csv")
here("AoED Data", "AoED Birds", "hab.sta.csv")

read.csv(here("AoED Data", "AoED Birds", "bird.sta.csv"))
read.csv(here("AoED Data", "AoED Birds", "hab.sta.csv"))

# file.exists()  tells you if you're looking in the right spot for your file
file.exists(here("AoED Data", "AoED Birds", "bird.sta.csv"))
file.exists(here("AoED Data", "AoED Birds", "hab.sta.csv"))
# If the file truly exists, it will output : TRUE!


# Library() and require() allow you to load extra packages that you;ve installed
# Library() will re-load already loaded packages, while require() will check if it's already loaded and will decide whether or not it needs to activate it (does not reactivate)
# Using this at the beginning of your script is helpful!
library(name_of_package)
require(name_of_package)
# Require() is much more streamlined because some packages take a long time to load!

# ------------------------------------------------------------------------------------------------------------------- #
        # In-Class R-Script! #

pairs(dat_habitat[, c("slope", "aspect")])
                    # pairs() creates multiple scatterplots for multiple variables - and most importantly for 
                    # each combination of each variable! In this case we plotted just slope and aspect, but 
                    # it is possible to add more variables which will all be plotted against each other.
                    
                    # Next, for using the his() script, we first need to find the maximum data value.
                    # This is to allow the break argument to function properly.
                    
max(dat_birds[,"AMRO"])
                    
                    # Output: [1] 4
                    # This means that there is a maximum value of 4
                    # In the context of the dat_birds dataset, this means that the most amount of 
                    # American Robins (AMRO) observed at a specific site is 4!
                    
                    # This tells us our minimum break range.
                    # The break argument requires that the range be at least the
                    #  total number of observations.
                  
                    # Now to use the hist() function to plot a histogram in R!
                    
hist(dat_birds[,"AMRO"],breaks = 0:5 - .5, xlab = "Number of Birds Counted", ylab = "Frequency", main = "Histogram of American Robin")
                    
                    # In the dataset there is a max of [4] American Robins observations at a single site, 
                    # which would initially make you want to have a break range of 0:4. 
                    # Though this works, it doesn’t work well for discrete data where a break = 0:4
                    # will give you a histogram with x-values depicted as ranges from 0-1, 1-2, 2-3 etc..
                    # So in this case, we can fix this by adding 1 to the break range so you are able to more 
                    # accurately describe the discrete nature of the data! By adding the 5th break range, 
                    # you isolate each value as its own discrete (non-continuous) value!

# ------------------------------------------------------------------------------------------------------------------- #
        # Individual Assignment Portion #

?par()
# par() allows us to view plots side by side
# First number is number of rows to arrange
# Second number is number of columns to arrange

dat_all = merge(dat_birds, dat_habitat)
?hist()
par(mfrow = c(1,3))
hist(dat_all$elev, 
     xlab = "Elevation (m)", 
     main = "Frequency of Elevation (m) Observed",
     xlim = c(0, 1000))

hist(dat_all$slope, 
     main = "Frequency of Slope (%) Observed", 
     xlab = "Slope (%)",
     xlim = c(0, 120),
     ylim = c(0, 175))

hist(dat_habitat$aspect, 
     xlab = "Aspect (degrees)", 
     main = "Frequency of Aspect (degrees) Observed",
     ylim = c(0, 175),
     xlim = c(0, 400)) 


# dev.off() will clear the current plot being plotted!
dev.off()

# ---------------------------------------------------------------------------------- #
# Calculates the value of x for a linear function, given the coordinates
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

# x1 and y1 are the coordinates which anchors your slope! It can be anywhere on the graph.
curve(line_point_slope(x, x1 = , y1 = , slope = ), add = TRUE)
# ---------------------------------------------------------------------------------- #

par(mfrow = c(1,3))
# You can combine this w/ the mtext() and line() to label! 
plot(dat_all$elev, y = dat_all$ba.tot, 
     xlab = "Elevation (m)", 
     ylab = "Basal Area (m^2 per ha)", 
     main = "Basal Area versus Elevation") 
curve(line_point_slope(x, x1 = 250, y1 = 30, slope = -0.01), add = TRUE)

plot(x = dat_all$slope, y = dat_all$ba.tot, 
     xlab = "Slope (%)", 
     ylab = "Basal Area (m^2 per ha)", 
     main = "Basal Area versus Slope") 
curve(line_point_slope(x, x1 = 10, y1 = 20, slope = -0.06), add = TRUE)
# Slope is the steepness or the degree of incline of a surface

plot(dat_all$aspect, y = dat_all$ba.tot, 
     xlab = "Aspect (degrees)", 
     ylab = "Basal Area (m^2 per ha)", 
     main = "Basal Area versus Aspect") 
curve(line_point_slope(x, x1 = 0, y1 = 20, slope = 0), add = TRUE)
# Aspect is the orientation of slope, measured clockwise in degrees from 0 to 360
# Where 0 is north-facing, 90 is east-facing, 180 is south-facing, and 270 is west-facing

# ------------------------------------------------------------------------------------------------------------------- #
        # Lab 3 #

  # Create pair plots using psych to plot slope, elevation and aspect!
# Use the pair plot function from psych to create a pair plot 
# of the three terrain variables and basal area from the lecture questions.
# psych package helps make pretty pair plots
?pairs.panels
require(psych)
env_var = data.frame(dat_all$slope, dat_all$elev, dat_all$aspect, dat_all$ba.tot)
env_var
pairs.panels(env_var)

# ---------------------------------------------------------------------------------- #
# We want to merge dat_habitat and dat_birds 
# How do we know if there are the same number of rows in both data frames?
# How can we be sure that we associate the correct row of dat+habitat/birds

dat_all = merge(dat_birds, dat_habitat)
plot(ba.tot ~ elev, data = dat_all)

# How to use R to to calculate the total number of waxwings 
CEWA_sum = sum(dat_birds[, "CEWA"])
CEWA_sum

# Converting to Presence/Absence using as.numeric *********
CEWA_boolean = dat_birds[, "CEWA"] >= 1
CEWA_boolean
# You could also have inputted the CEWA_boolean vector script directly into the as.numeric() function
CEWA_present_absent = as.numeric(CEWA_boolean)
CEWA_present_absent

# This code creates a binary plot like in the McGarigal reading!
# You can transform these well with logistic models
plot(x = dat_all$elev, y = CEWA_present_absent)

# ---------------------------------------------------------------------------------- #
# Function to calculate the logistic parameter a given the slope and midpoint
get_logistic_param_a = function(slope, midpoint)
{
  b = slope / 4
  return (-midpoint * (slope / 4))
}

# Function to calculate the logistic parameter b given the slope
get_logistic_param_b = function(slope)
{
  return (slope / 4)
}


# Calculate the value of the logistic function at x, given the parameters a and b.
logistic = function(x, a, b)
{
  val = exp(a + b * x)
  return(val / (1 + val))
}

# Calculate the value of the logistic function at x, given a slopoe and midpoint.
logistic_midpoint_slope = function(x, midpoint, slope)
{
  b = get_logistic_param_b(slope)
  a = get_logistic_param_a(slope, midpoint)
  return(logistic(x, a, b))
}

plot(x = dat_all$elev, y = CEWA_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = -0.05), add = TRUE)

# ---------------------------------------------------------------------------------- #

  # Question 1 & 2: Choose Two Bird Species and create plots of presence/absence (y) and basal area (x)
# I chose the American Goldfinch (AMGO) and Black-cap Chickadee (BCCH)
AMGO_boolean = dat_birds[, "AMGO"] >= 1
AMGO_present_absent = as.numeric(AMGO_boolean)
plot(x = dat_habitat$ba.tot, y = AMGO_present_absent,
     main = "Total Basal Area versus American Goldfinch Observations",
     xlab = "Total Basal Area (m^2 per ha)",
     ylab = "Presence/Absence of American Goldfinch")
curve(logistic_midpoint_slope(x, midpoint = 12, slope = -0.8), add = TRUE)

BCCH_boolean = dat_birds[, "BCCH"] >= 1
BCCH_present_absent = as.numeric(BCCH_boolean)
plot(x = dat_habitat$ba.tot, y = BCCH_present_absent,
     main = "Total Basal Area versus Black Cap Chickadee Observations",
     xlab = "Total Basal Area (m^2 per ha)",
     ylab = "Presence/Absence of Black-cap Chickadee")
curve(logistic_midpoint_slope(x, midpoint = 25, slope = -1), add = TRUE)


# Question 3: Sum of Observed Grey Jays
GRJA_sum = sum(dat_all[, "GRJA"])
GRJA_sum

# Question 4: Total number of sampling sites in which Gray Jay were observed
GRJA_sites = sum(dat_all[, "GRJA"] > 0)
GRJA_sites












                    