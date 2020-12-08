# Model Concepts II
  # https://michaelfrancenelson.github.io/eco_602_634_2020/assignments/eco_602/individual/week_10_using_models_2.html

# Review 1- and 2-sample t-tests
# Perform a 1-way Analysis of Variance (ANOVA)
# Perform a simple- and multiple- linear regressions
# Interpret model coefficient tables
# Interpret ANOVA tables

rm(list = ls())

# ---- one_sample_ttest ----

# T-tests are univariate tests that we can use to determine whether we have good evidence that:
  # One sample: The mean of one sample is different from a fixed value.

# One Tailed T-Tests use 'Alternative' script to choose which side to impose alpha 
  # alternative = "greater" : makes the t.test one sided if we think the observed data will be greater than the null hypothesis
  # alternative = "less" : makes the t.test one sided if we think the observed data will be less than the null hypothesis 

# alternative hypothesis: Gentoo penguin flippers are smaller than 218 mm
require(palmerpenguins)
t.test(
  x = subset(penguins, species == "Gentoo")$flipper_length_mm,
  mu = 218,  # sets a mean value to compare to, that isn't default = 0
  alternative = "less" 
)

# ---- two_sample_ttest ----

# Two sample: The means of two samples are different from each other.

t.test(flipper_length_mm ~ species, data = subset(penguins, species != "Chinstrap"))

# how could I modify the code to test the alternative hypothesis that Adelie penguins have shorter than Gentoo penguins?

t.test(flipper_length_mm ~ species, 
       data = subset(penguins, species != "Chinstrap"),
       alternative = "less")

# ---- 1_ANOVA_Graphical ----

# 1) Perform graphical and numerical data exploration

  # Graphical
    # We can explore normality using histograms and density plots:
par(mfrow = c(1, 2))
hist(penguins$body_mass_g, breaks = 80, main = "histogram of body mass", xlab = "body mass (g)")
plot(density(penguins$body_mass_g, na.rm = TRUE), main = "density plot of body mass")
      # Looks right skewed!

    # Conditional boxplots are great for categorical variables:
boxplot(body_mass_g ~ species, data = penguins) 
      # Gentoo have a higher average weight than adelie and chinstrap, who are quite close

  # Numerical
    # Testing for Normality for t-test assumptions
      # To test whether within-group data are normally-distributed, we need to do some data massaging:
        # 1) Extract the measurements for each species
dat_chinstrap = subset(penguins, species == "Chinstrap")
dat_adelie = subset(penguins, species == "Adelie")
dat_gentoo = subset(penguins, species == "Gentoo")

        # 2)  Calculate the mean body mass for each species
mean(dat_chinstrap$body_mass_g, na.rm = TRUE)
  # 3733.088
mean(dat_adelie$body_mass_g, na.rm = TRUE)
  # 3700.662
mean(dat_gentoo$body_mass_g, na.rm = TRUE)
  # 5076.016

        # 3) Conduct Shapiro tests on each species’ body mass
          # Shapiro test null hypothesis: “The data are drawn from a normally-distributed population.”
shapiro.test(dat_chinstrap$body_mass_g)
  # p-value = 0.5605
shapiro.test(dat_adelie$body_mass_g)
  # p-value = 0.0324
shapiro.test(dat_gentoo$body_mass_g)
  # p-value = 0.2336

# ---- 2_ANOVA_lm() ----

# 2) Fit a linear model using lm()
  
fit_species = lm(body_mass_g ~ species, data = penguins)

# ---- 3_ANOVA_summary() ----

# 3) Examine the model coefficient table using summary()

summary(fit_species)

# Model Coefficient Table: Shows coefficients for each x
  # Interpretation for linear model coefficients for a categorical variable:
    # The base case is the intercept 
    # The ‘slope’ coefficients are the adjustments you need to make to the base case to arrive at the means for the other levels of the factor.
  # Interpretation of p-values
    # The p-value in each row of the table is a significance test for whether the coefficient in that row is different from zero.
    # These p-values DO NOT tell us whether any of the coefficients are significantly different from each other!

# ---- 4_ANOVA_anova() ----

# 4) Conduct the Analysis of Variance using anova()

anova(fit_species)

# ANOVA Tables: output combines all the species into a single row
  # Low p-value ≈ adding the predictor creates a significantly better model than leaving it out
  # DOF
  # Sum Sq & Mean Sq tell us how much variability the predictor variable is able to explain  
    # Sum of Squares:tell us about how much of the total data variability is explained by each of the predictor variables.
      # The residuals sum of squares contains information about the variation that our model couldn’t explain.
      # Total Sum of Squares = ∑ numbers in the Sum sq. column
    # Mean Squares: allows us to compare the relative amount of information that each factor explains (when you have more than one predictor variable)
      # Mean Sq = Sum Sq adjusted by the degrees of freedom associated with each predictor variable
  # F-statistics is a measure of how much a adding a variable to the model improves the model fit
    # Null hypothesis: “Adding predictor x to the model does not improve the model fit.”

# ---- two_way_ANOVA ----

# Conditional Boxplot Example
boxplot(body_mass_g ~ sex + species, # predictor + predictor : this makes it a two-way!
        data = penguins)

fit_both = lm(formula = body_mass_g ~ sex + species, data = penguins)
summary(fit_both)

dat_chinstrap = subset(penguins, species == "Chinstrap")
female_chinstrap = subset(dat_chinstrap, sex == "female")
mean(female_chinstrap$body_mass_g, na.rm = TRUE)


     







