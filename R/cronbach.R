cronbach <- function(x) {

  s <- cov(x)
  p <- dim(s)[1]
  p / (p - 1) * (1 - sum( diag(s) ) / sum(s) )

}
