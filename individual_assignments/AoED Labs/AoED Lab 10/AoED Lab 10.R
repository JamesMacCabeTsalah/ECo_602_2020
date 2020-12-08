# Lab 10 - 1-way ANOVA
  # https://michaelfrancenelson.github.io/eco_602_634_2020/assignments/eco_634/lab_10.html

rm(list = ls())

rope = read.csv(here::here("data", "rope.csv"))
  # predictor: rope type
  # response: percent rope cut

# ---- ANOVA_by_hand ----

# Dr.Kane would like to understand the statistical power of experiments in relation to varying sample size and error rates (ANOVA)

# 1) We need to know how many levels there are to our independent treatment factor, rope.type, and the total number of observations (sample size)
factor(rope, levels = "rope.type")
length(levels(rope$rope.type))

# 2) Determine total number of observations made (n_obs) and number of groups (n_groups)
dim(rope)
n_obs = nrow(rope)
n_groups = 6
  # ncol(rope)

# 3) Partitioning Variance: Total 
  # Among-group
  # Within-group: The sum of each individual group’s sum of squares is the within-group sum of squares for the ANOVA.
    # (for test statistic ratio)

# Total variance = ∑among-group variance + ∑within-group variance
ss_tot = sum((rope$p.cut - mean(rope$p.cut))^2)
  # mean(rope$p.cut) = Grand Mean: the mean of the dataset without adding groups
    # Comparing grouped means (including groups) to the grand mean lets us see if adding models improves our model
      # Group means should be < grand mean

# 4) Partitioning Variance: Within-group
  # For each group we must:
    # Calculate the group mean
    # Calculate the group SS
    # Sum the group SSs

agg_sq_resids = aggregate(
  x = rope$p.cut, 
  by = list(rope$rope.type), 
  FUN = function(x) (sum((x - mean(x))^2))) # a function of the sum of squares for every type of ropes percent rope cut
      # function(x) is an "anonymouse function" and disappears after aggregate() finishes

str(agg_sq_resids)

ss_within = sum(agg_sq_resids$x)
  # 115 DOF ***

# 5) Partitioning Variance: Amongst Groups (SSA)

ss_among = ss_tot - ss_within
  # This is the SS that are are found by calculating residuals based on the mean of all groups and the means of each individual group
    # how much of the total error is accounted for by the within groups
  # ss_among requires all groups be the same length, and because this is unrealistic - it requires very complicated math to reach
    # by subtracting ss_among + ss_within = ss_tot, so subtracting ss_within from ss_tot we easily get ss_among!!!! ***
  # There are 6 rope types, so there are 6 − 1 = 5 DOF ***

# The extent to which ss_within is < ss_tot is a reflection of the magnitude of the differences between the means.
  # If ss_within << ss_tot: most of the variation in the response is due to differences among groups (or levels of the independent factor).
  # As the ratio of ss_among:ss_within increases, an increasing amount of the variation is due to differences among groups.

# 6) Normalizing
# We can’t compare the sums of squares directly because the numbers of groups are different than the total number of observations.
  # Larger samples will have larger sums of squared residuals - even if the variance is the same!
# We need to adjust the sums of squares to reflect the DOF available given the number of treatments (or levels of the independent factor) and the number of replicates per treatment.

df_tot = n_obs - 1
  # ss_within = 115 DOF , ss_among = 5 DOF : 115 + 5 = 120
    # n_obs = 121, so 121 - 1 = 120
      # We lose 1 DOF because in calculating ss_tot we had to estimate one parameter from the data in advance: the grand mean.
df_within = n_obs - n_groups
# df_within = number of observations minus the number of groups ***
  # This is because each group contains a treatment mean which is used in calculating ss_within
df_among = 6 - 1

# Mean Squares = Sum of Squares / DOF
ms_among  =  ss_among / (n_groups - 1)
ms_within = ss_within / (n_obs - n_groups)

# 7) F-Statistic
  # Null Hypothesis = treatment means are all the same
    # big number = reject the null hypothesis
    # small number = fail to reject the null hypothesis
  # Alt Hypothesis = at least one of the means is significantly different from the others.
f_ratio = ms_among/ms_within

# We want to know the Type 1 error rate (p-value)
  # Use pf() for cumulative probabilities of the F distribution

?pf() # pf(f_obs, df1, df2, lower.tail = FALSE)
f_pval = pf(f_ratio, 5, 115, lower.tail = FALSE)
  # df1 = 5 = DOF of df_among, df2 = 115 = DOF of df_within
  # We want the upper-tail p-value, so set lower.tail = FALSE (same as 1 - pf())
  # q argument in pf() is the observed value of F for which you want to calculate a likelihood of observing
    # q = x-axis = observed value of F

# qf(p = f-value, df1, df2) = tells you the mean square value at that f-value
qf(p = 2.2312, 5, 115)
  # Output: NaN ??? 

# ---- self_test ----
# number comparison tolerance
digits_check = 5

# Build the reference model using R functions
fit_1 = lm(p.cut ~ rope.type, data=rope)
anova(fit_1)
anova_fit_1 = anova(fit_1)

# Check degrees of freedom
anova_fit_1$Df == c(df_among, df_within)

# Check sums of squares
round(anova_fit_1$`Sum Sq`, digits = digits_check) == round(c(ss_among, ss_within), digits = digits_check)

# Check mean squares
round(anova_fit_1$`Mean Sq`, digits = digits_check) == round(c(ms_among, ms_within), digits = digits_check)

# Check the F-ratio
round(anova_fit_1$`F value`[1], digits = digits_check) == round(f_ratio, digits = digits_check)

# Check the F test statistic p-value
round(anova_fit_1$`Pr(>F)`[1], digits = digits_check) == round(f_pval, digits = digits_check)

