decoder <- function (matrix) 
{
    message <- apply(matrix, 1, function(x) packBits(as.integer(x)))
    message <- rawToChar(message)
    return(message)
}
