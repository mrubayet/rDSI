#'Trend Free Prewhitenned Mann-Kendall Test

#' This function performs trend free prewhitenned Mann-kendall test 
#' @param x (Numeric) 'x' can be a vector, matrix, or data.frame.
#' @return Returns a list containing Sen's slope, Mann-kendall results and auto-correlation value.

mk.tfpw<-function (x){
  if (!is.numeric(x) && !is.matrix(x) && !is.data.frame(x)) 
    stop("'x' must be a vector, matrix, or data.frame")
  if (!is.null(ncol(x)) && is.null(colnames(x))) 
    colnames(x) <- paste("series_", 1:ncol(x), sep = "")
  y <- x[!is.na(x)]
  n <- length(y)
  if(n<10)
    stop("Length of timeseries must be greater than 10")	
  
  mk<-function(x){
    t <- time(x)[!is.na(x)]
    outr <- outer(y, y, "-")/outer(t, t, "-")
    sen.slope <- median(outr[lower.tri(outr)])
    yy<-y-t*sen.slope
    acf_yy<-round(acf(yy,plot=FALSE,lag.max=1)$acf[2],digit=2)
    c1<-round((-1+1.96*(sqrt(n-2)))/n,digit=2)
    c2<-round((-1-1.96*(sqrt(n-2)))/n,digit=2)

if (acf_yy<c1 & acf_yy > c2){
  y <- y
  y = as.vector(na.omit(y))
  outr <- sign(outer(y, y, "-")/outer(t, t, "-"))
} else {
  yyy <- as.numeric(sapply(seq(2,n,1),function(xx) yy[xx]-acf_yy*yy[xx-1]))
  y = yyy + t[-1]*sen.slope
  y = as.vector(na.omit(y))
  outr <- sign(outer(y, y, "-")/outer(t[-1], t[-1], "-"))
}

S <- sum(outr[lower.tri(outr)])
ties <- rle(sort(y))$lengths
n <- length(y)
t1 <- n * (n - 1) * (2 * n + 5)
t2 <- sum(ties * (ties - 1) * (2 * ties + 5))
varS <- (t1 - t2)/18
if (n > 10 || any(ties > 1)) {
  Z <- (S - sign(S))/sqrt(varS)
  p.value <- 2 * pnorm(-abs(Z))
}

c(sen.slope = sen.slope, p.value = p.value, S = S, varS = varS, Z = round(Z, 3),acf=acf_yy)						
  }
  if (is.null(dim(x))){
    return(as.list(mk(as.vector(x))))
  } else{
    if (ncol(x) == 1){
      return(as.list(mk(x[, 1])))
    } else{
      ans <- t(sapply(1:ncol(x), function(i) mk(x[, i])))
      rownames(ans) <- colnames(x)
      return(ans)
    }
  }		
}
