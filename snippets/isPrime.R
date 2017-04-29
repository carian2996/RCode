# Ian Castillo
# Mexico City

require(primes)

# Several ways to present a function to determine if a number is prime

# First attemp

isPrimeV1 <- function(n) {
    if (n == 2) return(TRUE)
    divisor <- c()
    for (i in 2:(n-1)) divisor <- c(divisor, n%%i == 0)
    return(sum(divisor) == 0)
}

# Testing for several primes. You can generate from any number to another.
# Be careful, it could be a slow function with big primes.
primes <- generate_primes(min = 0, max = 1000)

# Functionality
for (i in 1:length(primes)) if (!isPrimeV1(primes[i])) print('Error en la prueba')

# Speed
system.time({
    for (i in 1:length(primes)) isPrimeV1(primes[i])
})


system.time({
    for (i in 1:length(primes)) is_prime(primes[i])
})


# Optimized way (01/04/17)

isPrimeV3 <- function(n){
    
    # ===== INPUT =====
    # n = natural number greater than 1.
    
    # ===== FUNCTION =====
    # isPrimeV3 function determine if n is a prime number
    # More info abour prime numbers: http://en.wikipedia.org/wiki/Prime_number
    
    # ===== OUTPUT =====
    # True = n is prime
    # False = n is not prime
    
    n == 2L || all(n %% 2L:ceiling(sqrt(n)) != 0)
    
    }

# ===== Notes =====
# According to R Project documentation:
# There are, however, times when we would like to explicitly create an integer 
# value for a constant. We can do this by calling the function as.integer or 
# using various other techniques. But perhaps the simplest approach is to 
# qualify our constant with the suffix character ‘L’.


