# Question 1

?sample()
?paste0()

n = 12345
vec_1 = sample(12, n, replace = TRUE)
head(vec_1)

vec_2 = vec_1 == 3
vec_2

vec_1[vec_2]
--------------------------------------
# Question 2

    n = 12345
vec_1 = sample(12, n, replace = TRUE)
head(vec_1)

length(vec_1)
sum(vec_11 == 3)

n = 10
vec_1 = sample(12, n, replace = TRUE)
paste0("Sum of elements with value 3: ", sum(vec_1 == 3))
--------------------------------------
 # Question 3
   
  for (i in 1:10)
{ 
  print(paste0("This is loop iteration: ", i))
  }
--------------------------------------
# Question 4

    n = 5
for(i in 1:n)
{
  print(i)
}
--------------------------------------
# Question 5

  n = 17
vec_1 = sample(1:10, size = n, replace = TRUE)
vec_1
for(i in 1:n)
{
  print(
    paste0("The element of vec_1 at index ", i, " is ", vec_1[i]))
}
# sample(range of numbers to select from, how many times you want to randomly selection from that range of numbers, replace = TRUE/FALSE)
# for(index variable, range in which you want to repeat/loop)
# every time you repeat, you increase the value within the range by one! 
# ex: for(i, 1:5) will repeat the loop body code 5 times, increasing by one each repeat! 
# 1:5 = 1,2,3,4,5 - so it's pretty much each value being put into the function
--------------------------------------
# Question 6

  create_and_print_vec(10, min = 100, max = 2000)
  {
    n = 100:2000
    vec_1 = sample(n, size = 10, replace = TRUE)
    vec_1
    for(i in 1:10)
    {print(paste0("The element of vec_1 at index ", i, " is ", vec_1[i]))}
  }










