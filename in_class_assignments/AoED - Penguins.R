install.packages("palmerpenguins")
require(palmerpenguins)
require(here)
# Quotation marks are optional when loading installed packages, even though it seems like you should need them. 

class(penguins)
penguins = data.frame(penguins)

mean(penguins$bill_length_mm)
# This doesn't work because theres NA data
# Some functions don't work if theres missing data!
head(penguins)
?mean()
# na.rm : indicates whether NA values should be stripped before the computation proceeds.
# Must set na.rm = TRUE if there is NA or NULL  data
mean(penguins$bill_length_mm, na.rm = TRUE)

pairs(penguins)
plot(penguins)
hist(penguins$bill_length_mm)

boxplot(bill_depth_mm ~ sex, data = penguins)
?boxplot()
# y ~ grp, where y is a numeric vector of data values to be split into groups according to the grouping variable grp (usually a factor)
# TILDE 
# y = bill_depth_mm, and it was split into cateogries of M/F

coplot(body_mass_g ~ bill_depth_mm | sex, data = penguins)
# body_mass_g grouped by bill_depth_mm and sex!
?coplot()
# y ~ x | a indicates that plots of y versus x should be produced conditional on the variable a.
# Conditioning variable is a! 

?png()
png(filename = here("basic_histogram.png"), width = 800, height = 600)
# png(filename = "x", width = y, height = z, units = px/in/mm....)
# There are many specifications you can make to your export!
hist(penguins$body_mass_g)
dev.off()





