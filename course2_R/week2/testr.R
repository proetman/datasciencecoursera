x_list = list()
x_list[['1']] = 'a'
x_list[['3']] = 'b'
x_list[['5']] = 'c'
names(x_list)
for (x in names(x_list)) {
        print(x)
}

toto <- list(a="my name is",b="I'm called",c="name:")
myfun <- function(key,value) paste(value,key)
for( key in names(toto) ) print(paste(c('key is ', key)))
for( key in names(toto) ) toto[key] <- myfun(key,toto[[key]])
