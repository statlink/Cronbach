\name{Confidence intervals for Cronbach's alpha wihtout the data}
\alias{cronfree.ci}
\title{
Confidence intervals for Cronbach's alpha wihtout the data
}

\description{
Confidence intervals for Cronbach's alpha wihtout the data.
}

\usage{
cronfree.ci(a, p, n, conf = 0.95, type = "kf")
}

\arguments{
\item{a}{
The Cronbach's alpha.
}
\item{p}{
The number of items or variables.
}
\item{n}{
The sample size of the data.
}
\item{conf}{
The confidence level of the interval
}
\item{type}{
The type of confidence interval to compute, "kf" or "whalen". See details for more information.
}
}

\details{
This function contains some confidence intervals for Cronbach's alpha. The "kf" was described in Tsagris, Frangos and Frangos (2013) and stands for the method of Koning A. and Franses H. P. (2006). The "whalen" stands for the method of Rodriguez and Maeda (2006). The difference with the confidence intervals produced by \code{ \link{cron.ci}} is that with these methods the data are not necessary.
}

\value{
A confidence interval for the true value of alpha.
}

\references{
Tsagris M., Frangos C.C. and Frangos C.C. (2013). "Confidence intervals for Cronbach's reliability coefficient". Recent Techniques in Educational Science, 14-16 May, Athens, Greece.
http://www.academia.edu/download/32613010/Cronbachs_alpha.pdf

Rodriguez M. C. and Maeda Y. (2006). "Meta-analysis of coefficient alpha".
Psychological methods, 11(3): 306-322.

Koning A. and Franses H. P. (2006). "Confidence intervals for Cronbach's coefficient alpha
values". Technical report, Erasmus Research Institute of Management-ERIM.

Cronbach L. (1951). "Coefficient alpha and the internal structure of tests".
Psychometrika, 16(3): 297-334.
}

\author{
Michail Tsagris, Christos Frangos and Constantinos Frangos.

R implementation and documentation: Michail Tsagris \email{mtsagris@uoc.gr}, Christos Frangos \email{cfragos@teiath.gr} and Constantinos Frangos \email{kfragkos@outlook.com}.
}

\seealso{
\code{ \link{cron.ci}, \link{cronbach}
}
}

\examples{
## silly example
x <- as.matrix( iris[1:50, 1:4] )
a <- Cronbach::cronbach(x)
Cronbach::cronfree.ci(a, 4, 150)
}


