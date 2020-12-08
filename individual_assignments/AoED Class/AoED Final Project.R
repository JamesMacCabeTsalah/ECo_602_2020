# In-Class Assignment:  Final Project Data

rm(list = ls())

delomys = read.csv(here::here("data","delomys.csv"))

# ---- graphical_exploration ----

# Plot a histogram of body mass.
hist(delomys$body_mass)
# Plot a histogram of body length.
hist(delomys$body_length)
# Plot a scatterplot of body length (on the y-axis) and body mass (on the x-axis)
plot(delomys$body_length ~ delomys$body_mass)

# ---- Questions ----

# Which data columns hold continuous data?
  # Body mass
  # Body length
  # Longitude
  # Latitude

# Which data columns hold categorical data?
  # X
  # Genus
  # Binomial
  # Age
  # Sex
  # Status

# Conditional boxplot of body mass grouped by sex
boxplot(delomys$body_mass ~ delomys$sex,
        xlab = 'Sex',
        ylab = 'Body Mass (g)',
        main = "Boxplot of Body Mass by Sex of Delomys spp.")
# Conditional boxplot of body mass grouped by species.
boxplot(delomys$body_mass ~ delomys$binomial, 
        xlab = 'Species',
        ylab = 'Body Mass (g)',
        main = "Boxplot of Body Mass by Species of Delomys spp.")
# Conditional boxplot of body mass grouped by species and sex.
boxplot(delomys$body_mass ~ delomys$binomial * delomys$sex,
        names = c("Female D. dorsalis", "Male D. dorsalis",
                  "Female D. sublineatus", "Male D. sublineatus"),
        xlab = "Species and Sex",
        ylab = "Body Mass (g)")





