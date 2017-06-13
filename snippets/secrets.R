# Ian C
# Rocio A
# 06/08/17

# This two little functions encode and decode messages
# with the following logic:
# The official alphabet conrains 26 letters that we can map into
# the first 26th numbers. Then we can convert each integer to the 
# binary system put it into a matrix and sent the message.
# Spaces are represented as zero rows.

function(message, matrix=TRUE) {
    
    message <- "listo!! ya quedo el tuyo?"
    
    # message (character): The message to encode
    # matrixx (boolean): Should the result be printed as a matrix?
    #                    If not, printed so you can converted as a vector.
    
    if (is.character(message)) {
        
        alphabet <- c(letters, 'kkkk', ',', '.', '?', '!')
        
        # Split the message into a vector of lower letters
        letts <- strsplit(message, "")[[1]]
        letts <- tolower(letts)
        
        # Match every letter with the position in the alphabet
        ints <- match(letts, alphabet)
        # Replace spaces with zeros
        ints[is.na(ints)] <- 0
        
        # Convert each integer into a binary number 
        matriz <- sapply(ints, function(x){ rev(as.integer(intToBits(x))[1:5]) })
        
        if (matrix == TRUE) return(t(matriz))
        else return(as.vector(matriz))
        
    } else {
        return("I can only convert character messages")
    }
}

decoder <- function(secret) {
    
    # secret (matrix): Encode message to decode
    
    # If the matrix doesn't have the conditions established return error
    if((ncol(secret) > 5 | !is.matrix(secret))) {
        return("Your matrix has something strange")
        
    } else {
        # Convert binaries and put them into a vector
        ints <- unlist(apply(secret, 1, function(x) as.integer(packBits(as.raw(c(rev(x), rep(0, 3)))))))
        # Spaces
        ints[ints == 0] <- 27
        
        # Return the message
        alphabet <- c(letters, " ", ',', '.', '?', '!')
        return(paste(alphabet[ints], collapse = ""))
    }
}
