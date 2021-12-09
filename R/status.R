#'  Case definition
#' Case definition
#' ### The 0 and 1 outcomes from the status and indic function are used to create the 2x2 table
#  A rise in the mean of the number of cases between two consecutive weeks that exceeds a threshold r

#' @param cases numeric vector - moving average for the time series epidemic data - obtained and stored as cases from the mova function
#' @param w_s  (Cannot be changed) time interval - validation time w_s=7{default}
#' @param r Threshold value (0<=r<=1, r=0.2{default}) for the minimum increase in the mean number of cases between two consecutive weeks that if present defines a case
#'
#'
#' @examples
#' data(Italy)
#' cases = mova(Italy$ncases)
#' status = status(cases, 7, 0.2)
#' @export
#'
#'
#'
status = function(cases, w_s=7, r=0.2) {
  ratio = 1/(1+r)
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
