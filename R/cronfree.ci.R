cronfree.ci <- function(a, p, n, conf = 0.95, type = "kf"){

  za <- qnorm( conf + 0.5 * (1 - conf) )

  if ( type == "kf" ) {
    low <- exp( za * sqrt( 0.5 * p / (p - 1) / n ) )
    up <- exp( -za * sqrt( 0.5 * p / (p - 1) / n ) )
    ci <- c( 1 - (1 - a) * low, 1 - (1 - a) * up )

  } else if ( type == "whalen" ) {
    tau <- (1 - a)^(1/3)
    up <- 18 * p * (n - 1) (1 - a) ^ ( 2/3 )
    denom <- ( p - 1) * (9 * n - 11)^2
    se <- sqrt( up / denom )
    ci <- c( tau - za * se, tau + za * se )
    ci <- abs(1 - ci^3)
    ci[ci < 0] <- 0    ;   ci[ci > 1] <- 1
  }

  names(ci) <- c( paste( 0.5 * (1 - conf), "%", sep = "" ), 
                  paste( conf + 0.5 * (1 - conf), "%", sep = "" ) )
  ci

}
