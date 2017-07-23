\name{enconder}
\alias{enconder}
\title{Encodes a message (only ASCII characters)}
\description{
Encondes the phrase 'Hello, world!' within a secret matrix that you can share with your friends.
}
\usage{
enconder(message, txt = FALSE)
}
\arguments{
\item{message}{A string using ASCII characters to encode into a secret matrix.}
\item{txt}{desired type of output. If \code{txt = TRUE}, you can share it more easily.}
}
\details{
\code{encoder} finds the position in ASCII table for each character of the message, then turn into binary code and returns a 8-column matrix with the number of rows according to total of characters in the message.

If \code{txt = TRUE}, the output is an instruction to create the secret matrix, so you can easily share.
}
\value{
A matrix with 8 columns to represent each row as a character in your message.
If \code{txt = TRUE}, a manual instruction to create the secret matrix.
}
\examples{
enconder(message = 'Hello, world!') # Print the secret matrix
enconder(message = 'Hello, world!', txt = TRUE) # Share this with a friend
}
