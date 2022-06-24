#' @title Calculating the Effect Size of Cohen's d & Hedges'g with the density curve and mean's line
#' @description \code{ESizeCoHedg} calculate the Effect Size & draw the density curve and mean's line
#'
#' @importFrom stats na.omit
#' @importFrom stats sd
#' @importFrom stats dnorm
#' @importFrom utils read.csv
#' @importFrom graphics curve
#' @importFrom graphics legend
#' @return Effect Size of Cohen's d & Hedges'g
#' @export
#' @examples
#' # ESizeCoHedg.dnsty02()

ESizeCoHedg.dnsty02 <- function(){

  dat <- read.csv(file.choose(), header=T)

  d1 <- na.omit(dat[,2])
  d2 <- na.omit(dat[,3])
  m1 <- mean(d1)
  m2 <- mean(d2)
  n1 <- length(d1)
  n2 <- length(d2)
  s1 <- sd(d1)
  s2 <- sd(d2)

  sp1 <- sqrt(((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2))
  d <- abs(m1 - m2)/sp1

  sp2 <- sqrt(((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2))
  g <- abs(m1 - m2)/sp2

  # draw the density curve
  plot(density(d2), col="red")
  lines(density(d1), col="blue")

  # add the mean's line
  abline(v=mean(d1), col="blue")
  abline(v=mean(d2), col="red")
  legend("topleft",
         legend=c("Density 2","Density 1"),
         col=c("red", "blue"),
         lty=1

  )
  return(list(Cohens_d=d, Hedges_g=g))
}


