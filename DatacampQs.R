a <- "Liz"
b1 <- 45.6
b2 <- "45.6"
c1 <- c(0, 1 ,2, 3)
str("Liz")

v1 <-  c(-2, -1,  0,  1,  2)
v1
v2 <- v1 * 3
v2
vec_4 <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
mat_1 <- matrix(vec_4, nrow = 3, ncol = 4, byrow = TRUE)
head(mat_1)
mat_2 <- matrix(vec_4, nrow = 3, ncol = 4, byrow = FALSE)
mat_2

my_list_1 <- list("two" = 5.2, "one" = "five point two", "three" = c(0, 1, 2, 3, 4, 5))
my_list_1$one
my_vec = rep(1:3, 5)
my_bool_vec <- my_vec == 3
data.frame(my_vec, my_bool_vec)

my_vec[my_bool_vec]

