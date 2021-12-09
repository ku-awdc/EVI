#' Example function and help file for mova.r
#' Moving Average
#' Calculate the moving average for the time series epidemic data
#'
#' @param cases time series data
#' @param int rolling window/time interval on which the moving average will be calculated
#'
#' @examples
#' cases=c(2,2,1,4,9,10,23,10,9,10,14,12,10)
#' mova=mova(cases, 7)
#' 2.000000  2.000000  1.666667  2.250000
#' 3.600000  4.666667  7.285714  8.428571
#' 9.428571 10.714286 12.142857 12.571429
#' 12.571429
#' @export
mova=function(cases, int=7){
  ncases=rep(NA, length(cases))
  for (i in 1:length(cases)){
    ncases[i]=mean(cases[((i+1)-min(int,i)):i])
  }
  return(ncases)
}

