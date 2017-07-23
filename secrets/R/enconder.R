enconder <- function (message, txt = FALSE) 
{
    raws <- charToRaw(message)
    ascii_matrix <- matrix(raws, nrow = 1)
    output <- t(apply(ascii_matrix, 2, rawToBits))
    if (txt == T) {
        output <- paste("matrix(c(", paste(t(output), collapse = ", "), "), ncol = 8, byrow = TRUE)", sep = "")
    }
    return(output)
}
