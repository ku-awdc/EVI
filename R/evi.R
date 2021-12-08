#' Epidemic Volatility Index calcuation
#' Calculates the relative change of standard deviation between two consecutive rolling windows

#' @param rollsd numeric vector - returned from the rollsd() function
#'
#'
#' @examples
#' cases <- rbinom(100,10,0.5)
#' a <- rollsd(cases)
#' evi(a)
#' NA NA -0.183503419  0.000000000 -0.051316702  2.399346342 -0.026448667  0.049499536 ….

#'
#' @export
#' evi: numeric vector - relative change of sd
evi = function(rollsd) {
  evi=rep(NA,length(rollsd))
  for (i in 2:length(rollsd)){
    evi[i]= (rollsd[i]-rollsd[i-1])/rollsd[i-1]
  }
  evi[is.nan(evi)]<-0
  return(evi)
}



