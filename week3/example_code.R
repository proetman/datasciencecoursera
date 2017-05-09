makeVector <- function(x = numeric()) {
        m <- NULL

        set <- function(y) {
                print('run set')
                x <<- y
                m <<- NULL
        }
        get <- function()
        {
                print('run get')
                x
        }

        setmean <- function(mean)
        {
                print('run setmean')
                m <<- mean
        }

        getmean <- function()
        {
                print('run getmean')
                m
        }

        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}

cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}
