\name{decoder}
\alias{decoder}
\title{Decodes a message encoded by this package}
\description{
Decodes a secret message encoded by this package.
}
\usage{
decoder(matrix)
}
\arguments{
\item{matrix}{A secret matrix created by \code{encoder}}
}
\details{
\code{decoder} use the reverse technique to create a secret matrix.
}
\value{
The message encoded previously by \code{encoder}.
}
\seealso{
[encoder()]
}
\examples{
secret <- matrix(c(00, 00, 00, 01, 00, 00, 01, 00, 
                   01, 00, 00, 01, 00, 01, 01, 00), 
                 ncol = 8, byrow = TRUE)

decoder(matrix = secret)
}
