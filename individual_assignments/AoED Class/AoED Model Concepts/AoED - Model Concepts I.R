# Model Concepts I
  # https://michaelfrancenelson.github.io/eco_602_634_2020/assignments/eco_602/individual/week_08_using_models_1.html

rm(list = ls())

catrate = read.csv(here::here("data", "catrate.csv"))
head(catrate)

  # Q1
hist(catrate$cat.rate,
     main = "Salamander Reproduction Catastrophe Rates",
     xlab = "Catastrophe Rate",
     ylab = "Frequency")

# ---- Shapiro_Wilk_Test ----
# Shapiro Test determines whether or not check whether your data are likely to have been drawn from a Normal distribution
  # Null hypothesis is that the data is normally distributed!

  # Q2
shapiro.test(catrate$cat.rate)
# W = 0.86202, p-value = 0.04097

  # Q3.1
# Answer: The data is not normally distributed.
  # Q3.2
# Answer: The p-value is less than 0.05, and thus it is unlikely that the null hypothesis is true.

# ---- Other_Normality_Tests ----

install.packages("nortest")
require(nortest) # this package contains many normal testing functions!

ad.test()

lillie.test()

# ---- Parametric_One_Sided_t-test ----

# If the data are normally distributed, we can use the Student’s t test.

# For a one-sample test, the null hypothesis is: 
  # “The mean of population from which the data were collected is not different from x”

  # Q4.1
# Answer: The rate of salamander reproduction catastrophe is not due to late pond-filling in the fall
  # Q4.2
t.test(catrate$cat.rate, mu = 2/7)
  # mu is the null hypothesis you want to test against - what we expect the system to be doing if there was no association

  # Q5.1
# t = 2.9595 | p-value = 0.01193
  # Q5.2 
# Answer:
  # Q5.3
# ANSWER: The t.test tells us that the p-value is small enough that we can reject the null hypothesis 
  # A t-value of 0 indicates that the sample results exactly equal the null hypothesis. 
    # As the difference between the sample data and the null hypothesis increases, the absolute value of the t-value increases
  # So the t-value also tells us that we aren't entirely within the range of the null hypothesis!


# t.test cares about the mean value! 
  # In one sided t-tests, we determine whether the observed mean is significantly different from a specified, expected value
  # Note, the default t test is a two-sided alternative
    # i.e., the alternative hypothesis is that the observed mean ≠ expected mean: which can mean less than or greater than the expected mean

# ---- One_Sided_Alt_Hypothesis ----

# If we have a good reason to believe that the observed mean should be greater than the null hypothesis mean...
  # you can use a one-tailed test ! ***
  # note: it is not valid to retroactively specify a 1-tailed alternative hypothesis ***

t.test(catrate$cat.rate, mu = 2/7, alternative = "greater")
  # alternative = "greater" : makes the t.test one sided if we think the observed data will be greater than the null hypothesis
  # alternative = "less" : makes the t.test one sided if we think the observed data will be less than the null hypothesis 

# ---- Non-Parametric_One-Sample_Wilcoxon ----
# The null hypothesis of the Wilcoxon test is that the two populations have the same distribution with the same median. 
# If we reject the null, that means we have evidence that one distribution is shifted to the left or right of the other. 
# Since we’re assuming our distributions are equal, rejecting the null means we have evidence that the medians of the two populations differ.
  # if p-value < 0.05: the medians of these two distributions differ 
    # Alternative hypothesis: “true location shift is not equal to 0” = “the distribution of one population is shifted to the left or right of the other,” which implies different medians. 

  # Q6
wilcox.test(catrate$cat.rate, mu = 2 / 7, alternative = "greater")

  # Q7.1
# V = 85, p-value = 0.003137
# alternative hypothesis: true location is greater than 0.2857143
  # Q7.2
# Answer: The p-value is 0.003137, which is below 0.05 and thus there is strong evidence to reject the null hypothesis.

  # Q8.1
# Answer: The Wilcox test is nonparametric while the t-test is parametric. ********
# Wilcox and t-tests assume that 
  # the two samples are independent of one another
  # the two populations have equal variance or spread
# But where they differ is that the wilcox test does not require the two populations to be normally distributed while a t-test does -
# and thus does not assume our data have a known distribution. 
# Additionally, the alternative hypotheses of the wilcox and t-test tests is that the the two datasets have different medians and means respectively.
# So based on this information, I believe that because the data is close to normally distributed,
# there is no strong difference between the median and mean - and therefore the results of the two tests.
  # Q8.2
# Answer: The alternative hypotheses of the wilcox and t-test tests is that the the two datasets have different medians and means respectively.
# This means that in a wilcox test you can determine whether the center of the data is different, 
# while in a t-test you can determine whether the average of the data are different. 
# These are distinct differences which will not give you the same output if the data contain outliers, or the data is highly skewed!

# ---- Comparing_Two_Sample_Means ----

# The null hypothesis in a two-sample test is: 
  # “There is no difference in mean values between the two groups.” 
  # "The values in the two groups were drawn from the same population."

require(palmerpenguins)
penguin_dat = droplevels(subset(penguins, species != "Gentoo"))
summary(penguin_dat)
boxplot(flipper_length_mm ~ species, data = penguin_dat) # can show us how different the data means are visually

dat_adelie = subset(penguin_dat, species == "Adelie")
  # Q9.1
shapiro.test(dat_adelie$flipper_length_mm)
# W = 0.99339 | p-value = 0.72
  # Q9.1
dat_chinstrap = subset(penguin_dat, species == "Chinstrap")
shapiro.test(dat_chinstrap$flipper_length_mm)
# W = 0.98891 | p-value = 0.8106

  # Q9.2
# Answer: The null hypothesis for this test is that the data are normally distributed,
# and the p-values of these tests are well above 0.05 - and thus there is a 72% and 81% probability of observing this data if the null was true.
# This means that the data are likely to be normally distributed. 

  # Q10
{
png("Adelie_Chinstrap_Flipper_Length.png", 
    width = 1920, 
    height = 1080, 
    units = "px", 
    pointsize = 12,
    res = 180)

par(mfrow = c(1,2))  # c(rows, columns)

hist(dat_adelie$flipper_length_mm,
     main = "Adelie Penguin Flipper Length",
     xlab = "Flipper Length")

hist(dat_chinstrap$flipper_length_mm,
     main = "Chinstrap Penguin Flipper Length",
     xlab = "Flipper Length")
dev.off()
}

# Q11.1: State the alternative hypothesis of the test, in plain nontechnical English.
  # Consider whether you used a one- or two- tailed test.
# Answer: The null hypothesis in a two-sample test is that “there is no difference in mean values between the two groups”, or 
# "the values in the two groups were drawn from the same population." This means that in the alternative hypothesis,
# the two values have different mean values 

  #  Q11.2
t.test(flipper_length_mm ~ species, data = penguin_dat)
wilcox.test(flipper_length_mm ~ species, data = penguin_dat)

# The output coefficients of the test are in relation to a 'base level' factorial that R has chosen!
  # You can use levels() to find out which species is the 'base level'
levels(penguin_dat$species)
# Adelie is the base level because it is ranked first!

# categorical ≈ factorial data
  # more formally, categorical variables = theoretical statistical


# ---- W_test ----

# W-test: how often would you expect the values of two groups to be more positive or negative than eachother
  # W = sum of all of the positive and negative slopes made between compared groups
