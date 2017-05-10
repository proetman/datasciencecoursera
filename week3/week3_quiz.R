Pass with 5/5


# --- --- ---
# Question 1
# --- --- ---
rm(list=ls())
library(datasets)
data(iris)

s <- split(iris,  iris$Species)
s_v <- s$virginica
mean(s_v$Sepal.Length)
round(mean(s_v$Sepal.Length))

# --- --- ---
# Question 2
# --- --- ---

# apply(iris, 1, mean)

# rowMeans(iris[, 1:4])

# colMeans(iris)

# apply(iris, 2, mean)

apply(iris[, 1:4], 2, mean)

# apply(iris[, 1:4], 1, mean)

# --- --- ---
# Question 3
# --- --- ---

rm(list=ls())
library(datasets)
data(mtcars)
?mtcars

# tapply(mtcars$cyl, mtcars$mpg, mean)

# split(mtcars, mtcars$cyl)

# mean(mtcars$mpg, mtcars$cyl)

#  lapply(mtcars, mean)

# sapply(mtcars, cyl, mean)

with(mtcars, tapply(mpg, cyl, mean))

tapply(mtcars$mpg, mtcars$cyl, mean)

sapply(split(mtcars$mpg, mtcars$cyl), mean)

# apply(mtcars, 2, mean)


# --- --- ---
# Question 4
# --- --- ---
# what is the absolute difference between the average horsepower of 4-cylinder cars and the average horsepower of 8-cylinder cars?
x <- tapply(mtcars$hp, mtcars$cyl, mean)
round(x[['8']] - x[['4']])
209.21429 -  82.63636
round(126.5779)
# --- --- ---
# Question 5
# --- --- ---

# what happens if you enter: debug(ls)

# The 'ls' function will return an error.

Execution of 'ls' will suspend at the beginning of the function and you will be in the browser.

#You will be prompted to specify at which line of the function you would like to suspend execution and enter the browser.

# The 'ls' function will execute as usual.
