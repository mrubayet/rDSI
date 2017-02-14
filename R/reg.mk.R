#'Regional Mann-Kendall Test

#'
#'

reg.mk<-function(x){
    mk <- mk.tfpw(x)
    slopes<- NULL
    for (m in 1:ncol(x)) {
      xm <- na.omit(x[,m])
      tm <- time(xm)
      outr <- outer(xm, xm, "-")/outer(tm, tm, "-")
      slopes.m <- outr[lower.tri(outr)]
      slopes <- c(slopes, slopes.m)     
    }
    sen.slope <- median(slopes, na.rm = TRUE)
    S <- sum(mk[, "S"])
    S_avg<-mean(mk[, "S"])
    varS <- sum(mk[, "varS"])
    Z <- (S - sign(S))/sqrt(varS)
    p.value <- 2 * pnorm(-abs(Z))
    ans<-c(sen.slope = sen.slope, p.value = p.value,S.avg=S_avg, Z = round(Z,3))
    return(ans)
  }
