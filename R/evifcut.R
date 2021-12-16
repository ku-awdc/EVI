#' Function that calculates the sensitivity and specificity at a specific cut-off value
#'
#'  @param evi numeric vector - object (obtained from the evi function and stored as ev)
#' @param cases moving average for the time series epidemic data - obtained and stored as cases from the mova function
#' @param cut threshold value 0<=c<=1 - expetation in the future number of cases
#' @param w_s (Cannot be changed) time interval - validation time w_s=7{default}
#' @param r Threshold value (0<=r<=1, r=0.2{default}) for the minimum increase in the mean number of cases between two consecutive weeks that if present defines a case
##'
#' @examples
#'#' data("Italy")
#' cases = mova(Italy$Cases)
#' roll = rollsd(cases)
#' ev = evi(roll)
#' evifcut(ev, cases, 0.01, 7, 0.2)
#' # rate = cut

#' @export

evifcut = function(evi, cases, cut, w_s, ratio) {

  #ratio = 1/(1+r)
  test_p=rep(NA, length(cases))
  true_p=rep(NA, length(cases))



  for (i in w_s:(length(cases)-w_s)){
    if (evi[i]>=cut && cases[i]>mean(cases[i:(i-6)]))
    {test_p[i]=1}
    else
    {test_p[i]=0}

    if (mean(cases[(i-(w_s-1)):i])<=ratio*mean(cases[(i+1):(i+w_s)]))
    {true_p[i]=1}
    else
    {true_p[i]=0}

  }

  sens=length(which(test_p==1 & true_p==1))/length(which(true_p==1))
  spec=length(which(test_p==0 & true_p==0))/length(which(true_p==0))
  sens[is.nan(sens)]<-0
  spec[is.nan(spec)]<-0
  testsin=length(which(test_p==1))/(length(cases)-w_s)
  prev=length(which(true_p==1))/(length(cases)-w_s)

  evifcut<-list(sens=sens, spec=spec, testsin=testsin, prev=prev)
  return(evifcut)
}



