#' Function that calculates the sensitivity and the specificity for each cut-off value and rolling window size
#'
#' @param evi numeric vector - object (obtained from the evi function and stored as ev) that corresponds to the relative change in the standard deviation
#' @param cases numeric vector with a number of cases per unit of time (i.e., daily)
#' @param cut threshold value (0<=c<=0.5) for issuing an early warning. If evi >= c an early warning is issued and otherwise is not.
#' @param r 0<=r<=1, r=0.2 {default}) Definition for the minimum increase in the mean number of cases, one week before and after each time point that if present should be detected (i.e., defines a case). The default is 0.2. This means that we have a case if the mean number of the newly observed cases in the next 7 days is at least 20% higher than the mean number of the newly observed cases in the past 7 days
##'
#' @examples
#'#' data("Italy")
#' cases = mova(cases=Italy$Cases)
#' roll = rollsd(cases=cases)
#' ev = evi(rollsd=roll)
#' evifcut(evi=ev, cases=cases, cut=0.01, r=0.2)

#' @export

evifcut = function(evi, cases, cut, r) {
  
  w_s=7
  ratio = 1/(1+r)
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



