cron.ci <- function(x, conf = 0.95, type = "logit", B = 1000){

  za <- qnorm( conf + 0.5 * (1 - conf) )
  s <- cov(x)
  dm <- dim(x)
  n <- dm[1]   ;  p <- dm[2]
  trs <- sum( diag(s) )
  sums <- sum(s)
  a <- p / (p - 1) * (1 - trs / sums)

  if ( type == "zyl" ) {
    ss <- s %*% s
    om <- sums * ( sum( diag(ss) ) + trs^2 ) - 2 * trs * sum(ss)
    va <- 2 * p^2 / ( n * (p - 1)^2 ) * om / sums^3
    ci <- c( a - za * sqrt(va), a + za * sqrt(va) )

  } else if ( type == "logit" ) {
    ss <- s %*% s
    om <- sums * ( sum( diag(ss) ) + trs^2 ) - 2 * trs * sum(ss)
    va <- 2 * p^2 / ( n * (p - 1)^2 ) * om / sums^3
    theta <- log( a / (1 - a) )
    vtheta <- ( 1/a + 1/ (1 - a) )^2 * va
    ci <- c( theta - za * sqrt(vtheta), theta + za * sqrt(vtheta) )
    ci <- exp(ci) / ( 1 + exp(ci) )

  } else if ( type == "boot" ) {

    aboot <- function(x, i) {
      s <- cov(x[i, ])
      p <- dim(s)[1]
      p / (p - 1) * (1 - sum( diag(s) ) / sum(s) )
    }
    x <- as.data.frame(x)
    b <- boot::boot(x, aboot, R = B)
    ci <- boot::boot.ci(b, type = "bca")$bca[4:5]
  }

  names(ci) <- c( paste( 0.5 * (1 - conf), "%", sep = "" ), 
                  paste( conf + 0.5 * (1 - conf), "%", sep = "" ) )
  ci

}
