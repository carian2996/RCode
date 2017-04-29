# Author: Arturo Erdely  # Date: 01/08/2017
# Goal: (Original text) Crear una función cuya fórmula quede oculta. 
#       Create a function whose formula is hidden.
crear.fn <- function(){
    expr <- expression(2*(x^2) + 1) # fórmula que deseo ocultar
    f <- function(x){
        x <- x
        eval(expr)
    }
    return(f)
}

# Example 1
f <- crear.fn()
rm(crear.fn)
f(1:3)

f

# Example 2
load("FnSecreta.RData")

# Author: Ian Castillo # Date: 01/09/2017
# Goal: Investigate the structure and definition 
#       of functions in other environments

str(object = f)     # display the internal structure of an R object
get(x = 'f')        # return the value of a named object 
exists(x = 'f', envir = environment(f))     # is x an object defined?
as.list(x = environment(f), all.names=TRUE)     # list all object from x environment

