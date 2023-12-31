\name{Cronbach's alpha}
\alias{cronbach}
\title{
Cronbach's alpha
}

\description{
Cronbach's alpha.
}

\usage{
cronbach(x)
}

\arguments{
\item{x}{
A numerical matrix with the data.
}
}

\details{
This function computes Cronbach's alpha internal reliability coefficient.
}

\value{
The Cronbach's alpha.
}

\references{
Cronbach L. (1951). "Coefficient alpha and the internal structure of tests".
Psychometrika, 16(3): 297-334.
}

\author{
Michail Tsagris.

R implementation and documentation: Michail Tsagris \email{mtsagris@uoc.gr}.
}

\seealso{
\code{ \link{cron.ci}, \link{cronfree.ci}
}
}

\examples{
## silly example
x <- as.matrix( iris[1:50, 1:4] )
Cronbach::cronbach(x)
}


