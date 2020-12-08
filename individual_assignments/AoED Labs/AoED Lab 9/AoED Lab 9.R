# Lab 10 - Modeling Continuous Data 2
  # https://michaelfrancenelson.github.io/eco_602_634_2020/assignments/eco_634/lab_09.html

rm(list = ls())

# ---- binom_proportions_test ----

catrate = read.csv(here::here("data", "catrate.csv"))
  # pond: the ID of the pond
  # success: The number of years in which successful reproduction occurred at the pond
  # years: The total number of years that the pond was observed.
  # cat.rate: the ratio of successes to total observation years.

?binom.test()
# Binomial Test: Suitable for data consisting of a single trial in which each observation is 0/1
  # Question: What is the evidence that 1 is more likely than 0? 
  # Answer: Two-Sided Binomial Test! 
    # Note: The default test is a two-sided alternative ***

# How likely is a response of 33/61 if the reproductive success and failure are equally likely?
  # Pr(success) = 0.5 is the default probability for binom.test()! ***
success = sum(catrate$success)
years = sum(catrate$years)
binom.test(success,years) # default is two sided alt test w/ prob = 0.5

# What is the evidence that reproductive success is more or less frequent than the late-filling rate?
  # In this scenario, we expect successful reproduction in approximately 5 of every 7 years
binom.test(success, years, p = 5/7)
  # p = probability = 0.___

binom.test(success, years, p = 5/7, alternative ='less') # to make binom.test() one sided, use 'alternative = "less/greater"

# What do these binomial tests say about the success (or catastrophe) rate?
  # Do these results agree with the Student’s t test and/or Wilcoxon’s signed rank test?
    # Answer: binom.test output p-value = 0.002984, which rejects the null hypothesis   
  # Which test do think is most appropriate?
    # binom.test() because we are working with binomial data and not continuous data

# Bernoulli Trial treats each pond-year as a separate observation with a binary outcome 
  # binom.test() treats each pond an observation and the dependent variable is the frequency of successful years out of the total number of years

# ---- two_samples ----

# The classical tests for two samples include:
  # comparing two variances
  # comparing two sample means or medians
  # tests for paired samples
  # correlating two variables
  # comparing two distributions
  # comparing two (or more) proportions
  # testing for independence of two variables in a contingency table

# ---- fisher_f_variance_test ----

# Fisher’s F test, based on the F-statistic.
  # The F-statistic represents the ratio between two variances 
    # For unequal variances, the test assumes that the data are normally distributed ***
  # If the variances of the two samples are the same, then the ratio of the variances will be 1

veg = read.csv(here::here("data", "vegdata.csv"), header=TRUE)

# you can look at variances visually
boxplot(pine ~ treatment, data = veg)

# or you can do a variance test!
var.test(
  pine ~ treatment,
  data = veg,
  subset = treatment %in% c('control','clipped'))

# The Fisher F Test tells us that an F = 119.2951 is far above 1, and thus the variances are not the same or similar

# Always good to check for normality since it is an assumption made by the Fisher F Test
  # Shaprio null hypothesis is that the data is normally distributed!
shapiro.test(veg$pine[veg$treatment=="control"])
  # p-value = 0.2547
  # Normally Distributed
shapiro.test(veg$pine[veg$treatment=="clipped"])
  # p-value = 0.04452
  # Not normally distributed

# If data are non-normal, then we should use a non-parametric test of homogeneity of variances
  # Fligner-Killeen test

# ---- nonparametric_variance_test ----

# The preferred non-parametric alternative test is called the Fligner-Killeen test
  # can do n variances (not just two like shown here!)
  # null = all variances are equal
fligner.test(
  pine ~ treatment,
  data = veg,
  subset = treatment %in% c('control','clipped')) # just control and clipped groups
  # p-value = 0.003818
  # Variances are not equal

fligner.test(pine ~ treatment, data = veg) # all groups
  # p-value = 0.0001952p-value = 0.0001952
  # Variances are not equal 

# The ksample parametric test is called Bartlett’s test, which is used to test for homogeneity of variances among more than two groups
  # null = all variances are equal
bartlett.test(pine ~ treatment, data = veg) # must do response variable ~ categorical variable
  # p-value = 9.323e-14
  # Variances are not equal

# The Fligner-Killeen Test and the Bartlett's Test agree that the variances are not equal!

# ---- T-Test ----

# How likely is it that our sample means were drawn from populations with the same mean?
  # Highly likely = means are not significantly different
  # Highly UN-likely = means are significantly different

# If this probability is very low (<5%), then we can be reasonably certain (95%<) that the means really are different from one another

# T-test is good when:
  # Samples are independent
  # Variances constant
  # Errors are normally distributed

# T-test Null: The means are the same/similar because they are drawn from the same population
t.test(pine~treatment,
       data=veg,subset=treatment %in% c('control','clipped'), 
       conf.int = TRUE) # 95% CI by default on the difference between sample means
                          # CI = 0 indicates that the sample means are not significantly different
# Output: 95% CI = -0.5204871, 32.5204871
# This means that ?????????********

# ---- wilcox_test ----

# The Wilcoxon’s rank-sum test is appropriate when the samples are independent but the errors are not normally distributed.

wilcox.test(pine~treatment,
            data=veg,
            subset=treatment %in% c('control','clipped'), 
            conf.int=TRUE)

# Do the results agree with the Student’s t test? Which test result to you have more confidence in?
  # Answer: T-Test ouputs p-value = 0.05582, while the Wilcox test has a p-value of 0.1005
  # Whereas the null hypothesis of the two-sample t-test is equal means, the  Wilcoxon null is  equal medians. 
  # Alternative hypothesis is that they have different means/medians.
  # Both favor the null hypothesis, which says that they have equal means/medians.
  # I trust the wilcox more because of the failure to meet the underlying assumptions of constant variance and normally distributed data. ***

# ---- tests_for_paired_samples ----

# Strong correlations between two measurement variables will affect the test for significant differences between means/medians

# A positive covariance between the two samples reduces the variance of the difference between means, 
# which makes it easier to detect significant differences between the means

# If you can do a paired t test, then you should always do the paired test ***
  # Accounting for covariance by using a paired t test is advantageous 

control = veg$pine[veg$treatment=='control']
clipped = veg$pine[veg$treatment=='clipped']

t.test(control, clipped, paired = TRUE) # paired = TRUE makes it a paired t-test
# Do the results differ from the unpaired t test?
  # Answer: Very slightly, with the unpaired t-test outputting a p-value of 0.05582 and and the paired 0.05417

wilcox.test(control, clipped, paired = TRUE)
# Output p-value = 0.03461 for paired, while it was 0.1005 in the unpaired. 
# It favors the null hypothesis in the unpaired, and the alternative hypothesis in the paired 

# ---- cor.test() ---- 

disp = read.csv(here::here("data", "dispersal.csv"), header=TRUE)
disp

# parametric cor.test()
cor.test(disp$disp.rate.ftb, disp$disp.rate.eb,
         use = 'complete.obs')

# The default correlation test statistic is based on Pearson’s product-moment correlation coefficient (r) cor(x,y) 
# which follows a t-distribution with length(x)-2 DOF if the samples follow independent normal distributions
  
# If the data are non-normal, then a non-parametric rank-based measure of association is more appropriate.
  # helpful if the data do not necessarily come from a bivariate normal distribution.

# non-parametric cor.test()
cor.test(disp$disp.rate.ftb, disp$disp.rate.eb,
  use = 'complete.obs', # addresses the missing values, so it won't fail due to empty data 
  method ='spearman') # you can specify Spearman's Rho statistic or Kendall's Tau statistic

# What does this test say about the correlation between dispersal-distance functions? 
  # +1 = perfect association of ranks
  # zero = no association between ranks 
  # -1 = perfect negative association of ranks.
  #  Answer: The output Rho value of 0.77 indicates that there is a positive association between dispersal-distance
# Does it agree with the parametric test? 
  # Answer: No because the data isn't parametric(???)
# Which do you trust more with this data set?
  # Answer: The parametric test output an r of 0.329, while the non-parametric test ouptut rho = 0.77.
  # Because this data is non-parametric, I trust the non-parametric spearman rho value more!

# ---- comparing_distributions_ks.test() ----

# Kolmogorov-Smirnov test,asks one of two different questions using the ECDF functions:
  # Are two sample distributions the same, or are they significantly different from one another in one or more (unspecified) ways?
    # Null hypothesis: The two distributions are the same, and not significantly different
    # two distributions with the same mean could be significantly different if they differed in variance, or in skew or kurtosis, or both
  # Does a particular sample distribution arise from a particular hypothesized theoretical distribution?
    # Null hypothesis: There is no difference between the two distributions

# Empirical Cumulative Distribution Function gives the probability that a randomly selected value of X is less than or equal to x.

# We can view them visually in a plot
plot(
  ecdf(disp$disp.rate.ftb),
  verticals=TRUE)
plot(
  ecdf(disp$disp.rate.eb),
  verticals=TRUE,
  lty=3,
  add=TRUE)

# We can then test them using ks.test()
ks.test(disp$disp.rate.ftb,disp$disp.rate.eb)
  # p-value = 0.6172
  # The test statistic is the maximum difference in value of the cumulative distribution functions
    # ≈ maximum vertical difference in the curves for a given value of X

# Is there enough evidence to suggest that the dispersal-distance relationship differs between first-time breeders and experienced breeders?
  # Answer: Ouput p-value = 0.6172, which means that the null is supported = the two distributions are the same, and not significantly different

# ---- comparing_2+_proportions_prop.test() ----

# prop.test(c(# of events for A, # of events for B), c(total # of events from A, total # of events from B)
  # example: prop.test(c(# of female mortalities, # of male mortalities), c(total # of females, total # of males))
prop.test(c(4,16),c(40,250))
  # p-value = 0.6183 > .05 = reject null

# p-value < .05 = proportions are different between samples and are unlikely to have been drawn from the same underlying population
# p-value > .05 = insufficient evidence to reject the null hypothesis, and proportions are not statistically different

# Do females suffer a higher mortality rate crossing the road or is the observed difference merely a sampling artifact?
  # Answer: It seems to just be an artifact of small sampling size, seeing as the null was rejected.
# How would the result change (p-value) if the sample size was doubled but the proportions killed remained the same (i.e., 8 out of 80 females and 32 out of 500 males)?
prop.test(c(8,32),c(80,500))
  # p-value = 0.3461 > .05 = reject null
  # Answer: The p-value got smaller, but in this case it's not enough to make it less than .05.

# ---- chisq.test() ----

# A contingency table shows the counts of how many times each of the contingencies actually happened in a particular sample

# We want to know whether the observed counts differ from what we would expect if presence/absence was independent of a categorical variable
  # Pearson’s chi-squared test expects the expected cell values to be large, generally greater than 4 or 5 
    # Do not use when one or more of the expected frequencies is less than 4 (or 5) - instead use the fisher.test() ***
      # (small expected values inflate the test statistic value)
owls = matrix(c(16, 9, 4, 11), nrow=2)
rownames(owls) = c("present", "absent")
colnames(owls) = c("old", "young")
owls

chisq.test(owls)
  # p-value = 0.05004
  # Null Hypothesis: no relationship exists on the categorical variables in the population, they are independent.

# Are barred owls present more frequently than expected in old forest stands?
  # Answer: ????

# ---- fisher.test() ----

# When one or more of the expected frequencies is less than 4 (or 5) -  use the fisher.test() > chisq.test
fisher.test(owls)
  # p-value = 0.04837

# The conclusion is about the same as the chisq.test


# ---- Q1 ----

require(penguins)
dat_pen = penguins

# The ksample parametric test is called Bartlett’s test, which is used to test for homogeneity of variances among more than two groups
# null = all variances are equal
bartlett.test(body_mass_g ~ species, data = dat_pen)

# Q1 Answer: p-value = 0.05005

# ---- Q2 ----

bartlett.test(body_mass_g ~ sex, data = dat_pen)

# Q2 Answer: p-value = 0.03194

# ---- Q3 ----

dat_groups = aggregate(
  body_mass_g ~ species,
  data = penguins,
  FUN = c)
str(dat_groups)

par(mar = c(8, 4, 2, 2))
boxplot(
  body_mass_g ~ sex * species,
  data = penguins,
  las = 2, 
  xlab = NULL,
  ylab = "body mass (g)")

bartlett.test(body_mass_g ~ interaction(species, sex), data = penguins)

# Q3 Answer: p-value = 0.1741

# ---- Q4 ----

birds = read.csv(here::here("data", "bird.sta.csv"), header = TRUE)
hab = read.csv(here::here("data", "hab.sta.csv"), header = TRUE)
birdhab = merge(birds, hab, by = c("basin", "sub", "sta"))

# Create a contingency table for edge/interior and brown creeper presence/absence
table(birdhab$s.edge, birdhab$BRCR > 0)
  # used the table() function to compute the cross-classified counts

# set the presence to be in the first column
br_creeper_table = table(birdhab$s.edge, birdhab$BRCR > 0)[, 2:1] 
  # this switches the order of the columns so that the present counts were in the first column and the absent counts were in the second column
  # necessary to use prop.test()

# Conduct a Chi-square test on the contingency table of Brown Creeper presence/absence in edge and interior habitats.
chisq.test(br_creeper_table)
  # p-value = 1.386e-06

prop.test(c(29,314),c(173,873))
  # p-value = 1.386e-06
  # Proportions are different between samples and are unlikely to have been drawn from the same underlying population

# What is the null hypothesis for your test?
  # Make sure you state the null hypothesis in terms of Brown Creeper presence/absence and edge/interior habitats.
  # Are brown creepers present more or less frequently than expected in forest interiors versus forest edges and is the difference significant?
# Answer: The null hypothesis is that brown creeper presence/absence is unaffected by the cateogrical variables of edge/interior habitats - they are independent and unrelated. 

# Explain whether you think that Brown Creepers show a significant habitat preference.
  # Make sure your use the output of your statistical test to support your answer.
# Answer: The Chi-Square Test output a p-value = 1.386e-06, which rejects the null hypothesis and thus favors the alternative hypothesis.
# The alternative hypothesis is that presence/absence of brown creepers show habitat preference for forest edge and interior spaces.
# A prop.test() of the data output a p-value = 1.386e-06, which means that the proportions are different between samples and are unlikely to have been drawn from the same underlying population.
# This inspires confidence that the chi-squared output isn't just an artifact of random chance. 
  
  
