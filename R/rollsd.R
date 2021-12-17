#' Rolling standard deviation
#'
#' -A function to compute the rolling standard deviation for a time series
#' 
#' @param cases numeric vector with a number of cases per unit of time (i.e., daily)
#' @param lag_t integer - the size of the rolling window for which the rolling standard deviation is calculated (minimum/default value = 7, maximum recommended value = 30)
#'
#' @examples
#' data("Italy")
#' cases = mova(cases=Italy$Cases, r_a=7)
#' roll = rollsd(cases=cases,lag_t=7)
#'
#'
#' @export
rollsd = function(cases, lag_t) {
  rollsd=rep(NA,length(cases))
  for (i in 1:lag_t){
    rollsd[i]=medvol(cases[1:i])
  }
  for (i in (lag_t+1):length(cases)){
    rollsd[i]=medvol(cases[(i-(lag_t-1)):i])
  }
  return(rollsd)
}

