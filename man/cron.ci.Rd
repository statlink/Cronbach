\name{Confidence intervals for Cronbach's alpha}
\alias{cron.ci}
\title{
Confidence intervals for Cronbach's alpha
}

\description{
Confidence intervals for Cronbach's alpha.
}

\usage{
cron.ci(x, conf = 0.95, type = "logit", B = 1000)
}

\arguments{
\item{x}{
A numerical matrix with the data.
}
\item{conf}{
The confidence level of the interval
}
\item{type}{
The type of the confidence interval, "logit", "zyl" or "boot". See details for more information.
}
\item{B}{
This relevant only for the bootstrap based confidence interval. It is the number of bootstrap samples to generate.
}
}

\details{
This function contains some confidence intervals for Cronbach's alpha as described in Tsagris, Frangos and Frangos (2013). In particular, the type "zyl" refers to estimating the variance of alpha as suggested by van Zyl J., Neudecker H. and Nel D. (2000) and then constructing the classical confidence interval. The type "logit" computes the logit transformation and then approximates the variance based on the Delta method. This makes use of the variance as computed by van Zyl J., Neudecker H. and Nel D. (2000). Finally, the type "boot" computes BCa confidence intervals.
}

\value{
A confidence interval for the true value of alpha.
}

\references{
Tsagris M., Frangos C.C. and Frangos C.C. (2013). "Confidence intervals for Cronbach's reliability coefficient".
Recent Techniques in Educational Science, 14-16 May, Athens, Greece.
http://www.academia.edu/download/32613010/Cronbachs_alpha.pdf

van Zyl J., Neudecker H. and Nel D. (2000). "On the distribution of the maximum likelihood
estimator of Cronbach's alpha". Psychometrika, 65(3): 271-280

Efron B. and Tibshirani, R.J. (1993). "An introduction to the bootstrap". Chapman & Hall CRC.

Cronbach L. (1951). "Coefficient alpha and the internal structure of tests".
Psychometrika, 16(3): 297-334.
}

\author{
Michail Tsagris, Christos Frangos and Constantinos Frangos.

R implementation and documentation: Michail Tsagris \email{mtsagris@uoc.gr}, Christos Frangos \email{cfragos@teiath.gr} and Constantinos Frangos \email{kfragkos@outlook.com}.
}

\seealso{
\code{ \link{cronfree.ci}, \link{cronbach}
}
}

\examples{
## silly example
x <- as.matrix( iris[1:50, 1:4] )
Cronbach::cron.ci(x)
}


