n = 12345
vec_1 = sample(12, n, replace = TRUE)
head(vec_1)
n = 12345
vec_2 = sample(12, n, replace = TRUE)
vec_2 = vec_1 == 3
vec_1[vec_2]
n = 12345
vec_1 = sample(12, n, replace = TRUE)
head(vec_1)
length(vec_1)
sum(vec_1 == 3)
n = 10
vec_1 = sample(12, n, replace = TRUE)
paste0("Sum of elements with value 3: ", sum(vec_1 == 3))
n = 13
for (i in 1:n)
{
  print(paste0("This is loop iteration: ", i))
}

n = 17
vec_1 = sample(17, n, replace = TRUE)
for (i in 1:n)
{
  print(paste0("The element of vec_1 at index ", i, " is ", vec_1[[i]]))
}

n = 10
min = 100
max = 2000

lizvec = sample(min:max, n, replace = TRUE)
create_and_print_vec = function(n, min =  100, max = 2000)
{
  lizvec = sample(min:max, n, replace = TRUE)
  for (i in 1:n)
  {
    print(paste0("The element of vec_1 at index ", i, " is ", lizvec[[i]]))
  }
}
create_and_print_vec(10, min = 100, max = 2000)
