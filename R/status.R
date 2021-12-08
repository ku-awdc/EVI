#'  Case definition 
#'  ### The 0 and 1 outcomes from the status and indic function are used to create the 2x2 table - see pdf file
#A rise in the mean of the number of cases between two consecutive weeks that exceeds a threshold r

#' @param cases numeric vector - number of new cases per day/any other time interval
#' @param w_s  time interval/validation time w_s=7{default}
#' @param ratio threshold value for the case definition (ratio = 1/1.2 {default})
#'
#'
#' @examples
#' cases <- rbinom(100,10,0.5)
#' w_s <- 7
#' ratio <- 1/1.2
#' status(cases, w_s, ratio)
#' NA  0  0  0  0  0  0  0  1  1
#' @export
#' Numeric vector of 0 and 1 - that contain the cases
#' 
#' 
status = function(cases, w_s=7, ratio=1/1.2) {
  status=rep(NA,length(cases))
  status[1]=NA
  for (i in 2:(length(cases)-w_s)){
    if (mean(cases[(i-min((i-1),w_s)):(i-1)])<=ratio*mean((cases[i:(i+min(i,(w_s-1)))])))
    {status[i]=1}
    else
    {status[i]=0}
  }
  return(status)
}
