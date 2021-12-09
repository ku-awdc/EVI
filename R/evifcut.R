#' Function that calculates the sensitivity and specificity in a range of cut-off values so that to find the cut-off value that maximizes the Youden Index (Se + Sp -1)
#' @param evi numeric vector as returned from the evi function
#' @param cases numeric vector - number of new cases per day/any other time interval
#' @param rate threshold value for model prediction - test outcome # defined as c in the indic function
#' @param w_s time interval - validation time w_s=7{default}
#' @param ratio threshold value for case definition ratio=1/1.2{dedault}
##'
#' @examples
#' evi: numeric vector as returned from the evi function
#' cases <- rbinom(100,10,0.5)
#' rate = 0.01
#' w_s = 7
#' ratio = 1/1.2
#' evifcut(evi, cases, rate, w_s, ratio)
#' $sens
#' 0.1333333
#' $spec
#' 0.8611111
#' $testsin
#' 0.1290323
#' $prev
#' 0.1612903
#'
#' @export
#' evifcut

evifcut = function(evi, cases, rate, w_s=7, ratio=1/1.2) {

  test_p=rep(NA, length(cases))
  true_p=rep(NA, length(cases))



  for (i in w_s:(length(cases)-w_s)){
    if (evi[i]>=rate && cases[i]>mean(cases[i:(i-6)]))
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
