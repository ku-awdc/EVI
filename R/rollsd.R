#' Rolling standard deviation
#' -A function for computing the rolling standard deviation (between two sd’s) for the time series epidemic data
#' @param cases numeric vector - number of cases per day (or any other time interval)
#' @param lag_t rolling window size (lag_t=7{default})
#'
#' @examples
#' First example
#' cases=c(2,2,1,4,9,10,23,10,9,10,14,12,10)
#' rollsd(cases,lag_t)
#' NA 0.0000000 0.5773503 1.2583057
#' 3.2093613 3.8815804 7.7827648 7.4578179
#' 6.9006556 5.8227796 5.0803075 4.8941170
#' 4.8941170
#' 
#' 
#' Second example
#' cases = rbinom(100,10,0.5)
#' rollsd(cases,30)
#' 
#' 
#' @export
#' numeric vector - Rolling standard deviation
rollsd = function(cases, lag_t=7) {
  rollsd=rep(NA,length(cases))
  for (i in 1:lag_t){
    rollsd[i]=medvol(cases[1:i])
  }
  for (i in (lag_t+1):length(cases)){
    rollsd[i]=medvol(cases[(i-(lag_t-1)):i])
  }
  return(rollsd)
}


