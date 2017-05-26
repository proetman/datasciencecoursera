print('question 1')

cube <- function(x, n) {
        x^3
}

print('question 2')

x <- 1:10
if(x > 5) {
        x <- 0
}

print('question 3')
rm(list=ls())

f3 <- function(x) {
        g <- function(y) {
                y + z
        }
        z <- 4
        print(z)
        x + g(x)
}

z <- 10
print(f3(3))

print('question 4')


h <- function(x, y = NULL, d = 3L) {
        z <- cbind(x, d)
        if(!is.null(y))
                z <- z + y
        else
                z <- z + f
        g <- x + y / z
        if(d == 3L)
                return(g)
        g <- g + 10
        g
}

# f, z, d, L, g
