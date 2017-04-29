# cumstats

# Cumulative descriptive statistics similar to R base functions cumsum, cumprod, cummax, cummin

cummean <- function(x) cumsum(as.numeric(x))/seq_along(x) # mean
cumgmean <- function(x) exp(cummean(log(x))) # geometric mean
cumhmean <- function(x) 1/(cummean(1/x)) # harmonic mean
cummedian <- function(x) sapply(seq_along(x), function(k, z) median(z[1:k]), z = x)  # median
cumquant <- function(x, p, type = 7) sapply(seq_along(x), function(k, z) quantile(z[1:k], probs = p, names = F, type = type), z = x)
cumvar <- function(x) sapply(seq_along(x), function(k, z) var(z[1:k]), z = x)  # variance
skewness <- function(x) sqrt(length(x))*sum((x - mean(x))^3)/(sum((x - mean(x))^2)^(3/2))
cumskew <- function(x) sapply(seq_along(x), function(k, z) skewness(z[1:k]), z = x)  # skewness
kurtosis <- function(x) length(x)*sum((x - mean(x))^4)/(sum((x - mean(x))^2)^2)
cumkurt <- function(x) sapply(seq_along(x), function(k, z) kurtosis(z[1:k]), z = x)  # kurtosis

# Examples:
y <- c(9, 1, 3, 0)
cummean(y)
cumgmean(y)
cumhmean(y)
cummedian(y)
cumquant(y, 0.5)
cumvar(y)
cumskew(y)
cumkurt(y)

Mode <- function(x){    
    counts <- tabulate(match(x, unique(x)))    
    list(Values = unique(x)[counts == max(counts)], Frequency = max(counts))
}
cummode <- function(x) sapply(seq_along(x), function(k, z) Mode(z[1:k])$Values, z = x)
# Examples:
y <- c(rep(1, 2), rep(12, 5), rep(44, 3), rep(8, 5), 55)
Mode(y)
cummode(y)
Mode(runif(5))
cummode(runif(5))


