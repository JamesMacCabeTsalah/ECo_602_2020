install.packages("ggplot2")
library(ggplot2)

data(iris)
head(iris)
iris$Sepal.Width
mean(iris$Sepal.Length)
sd(iris$Sepal.Length)

plot(iris)
plot(x = iris$Sepal.Width, y = iris$Sepal.Length)
points(x = data_center_x, y = data_center_y, col = "red")

# Calculate the mean of the Sepal.Width/Length to determine the center!
# This tells you the coordinates of the primary cluster.
data_center_x = mean(iris$Sepal.Width)
data_center_y = mean(iris$Sepal.Length)
c(data_center_x, data_center_y)

# Loading a Custom Function into R's Workspace! 
# This calculates the coordinates of points on a line given 
# the x- and y- coordinates of a known point and a slope.
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

plot(x = iris$Sepal.Width, y = iris$Sepal.Length)
points(x = data_center_x, y = data_center_y, col = "red")
curve(
  line_point_slope(
    x, 
    data_center_x, 
    data_center_y,
    -0.1), 
  add = TRUE)

# ------------------------------------------------------------------ #
my_vec = rep(1:3, 5)
my_vec

my_bool_vec = my_vec == 3
my_bool_vec

my_vec[my_bool_vec == TRUE]

data_frame = data.frame(my_vec, my_bool_vec)
data_frame

subset(data_frame, subset = my_bool_vec == TRUE)




