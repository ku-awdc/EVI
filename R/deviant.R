#' This function  produces the Epidemic Volatility index based output data
#'
#' This is the main function of the EVI package that utilizes all secondary functions and gives as an output a data frame aaaaaaa
#' with each row corresponding to each time point. For each time point the stored variables are:
#' dates the date for each time point (with origin 01-01-1970)
#' Days the serial number of the time point
#' EVI the estimate EVI at this time point
#' Cases the rolling average of the newly observed cases at this time point. A 7-day rolling average is calculated by default and the user has the option to change this by modifying r_a
#' Index takes values 1 or 0 for the issuance of an early warning or not
#' pvs the positive predictive value at this time point
#' pvn the negative predictive value at this time point
#' lag_all the selected rolling window size for EVI calculation at this time point
#' c_all the selected cut-off (c_all) for issuing an early warning at this time point
#' se_all the sensitivity (Se) and specificity (Sp) of EVI up to this time point
#' sp_all  the sensitivity (Se) and specificity (Sp) of EVI up to this time point
#' the positive and negative predictive value (ppv and npv, respectively) at this time point

#' @param new_cases time series data
#' @param cum True of False; True if the time series data are stored as cummulative data cum=FALSE {default}
#' @param r_a rolling window size/time interval on which the moving average will be calculated - number of consecutive observations per rolling window. Usually the 7-day moving average rather than the actually observed cases are analyzed
#' @param r Threshold value (0<=r<=1, r=0.2{default}) for the minimum increase in the mean number of cases between two consecutive weeks that if present defines a case
#' @param start_cases integer - first observations "burn-in" of the time series
#' @param lag_1 (Cannot be changed) initial minimum value for the rolling windows size, lag_1=7
#' @param lag_max (Cannot be changed) integer maximum value for the number of consecutive rolling windows
#' @param c_1 (Cannot be changed) initital threshold value for the expetation in the future number of cases, c_1=0.1{default}
#' @param w_s (Cannot be changed) desired detection difference - time interval - validation time w_s=7{default}
#' @param c_s (Cannot be changed) cut off point range, c_s=seq(0.01,0.5, 0.01)
#'
#'
#' @examples
#' data("sub_Italy")
#' deviant(sub_Italy$ncases, cum = FALSE, 0.2, 14, 30)
#' #This step should take some time and the time elapsed will be printed
#'
#' @export

deviant=function(new_cases, cum = FALSE){
  source("mova.r")
  source("medvol.r")
  source("evi.r")
  source("evifcut.r")
  source("indic.r")
  source("status.r")
  source("rollsd.r")
  
 
  
  
  start_time = Sys.time()
  start_cases=14
  r=0.2
  ratio=1/(1+r)
  lag_1=7
  c_1=7
  r_a = 7
  w_s =7
  lag_max=30
  


  if (cum == TRUE) new_cases = c(new_cases[1], diff(new_cases))


  #calculate the moving average of new confrimed cases
  cases=mova(new_cases,r_a)

  roll=rollsd(cases[1:start_cases],lag_1)
  ev=evi(roll)
  ind=indic(ev,c_1, cases[1:start_cases])
  status=status(cases[1:start_cases],w_s,ratio)

  #initiate chain for positive predictive value
  pvs=rep(NA, length(cases))

  #initiate chain for negative predictive value
  pvn=rep(NA, length(cases))

  lag_all=rep(NA, start_cases)
  c_all=rep(NA, start_cases)

  se_all=rep(NA, start_cases)
  sp_all=rep(NA, start_cases)


  lag_all[1:start_cases]=lag_1
  c_all[1:start_cases]=c_1


  for (i in (start_cases+1): length(cases)){

    case_t=cases[1:i]
    #case_t=cases[max(1,(i-33)):i]
    #lag_s=7
    lag_s=seq(lag_1,min(lag_max,(length(case_t)-1)), 1)
    #lag_s=seq(lag_1,min(length(case_t),50), 1)
    c_s=seq(0.01,0.5, 0.01)
    #all_j=NA

    all_lag=NA
    all_cut=NA
    all_se=NA
    all_sp=NA



    for (j in lag_s){
      roll_t=rollsd(case_t,j)
      ev_t=evi(roll_t)
      for (l in c_s){
        evicut_t=evifcut(ev_t, case_t, l, w_s, ratio)
        new_j=j
        new_l=l
        new_se=evicut_t$sens
        new_sp=evicut_t$spec
        all_lag[[length(all_lag) + 1]] <- new_j
        all_cut[[length(all_cut) + 1]] <- new_l
        all_se[[length(all_se) + 1]] <- new_se
        all_sp[[length(all_sp) + 1]] <- new_sp


      }
    }



    sesp=as.data.frame(cbind(all_lag,all_cut,all_se,all_sp))




    #Select the row with the right window and cut
    index=which.max(sesp$all_se+sesp$all_sp-1)

    #index=sesp[which(sesp$all_sp>0.80),]
    #index=which.max(index$all_se)
    #index=which(sesp$all_se==1 & sesp$all_sp>=0.95),1)
    #if (i>40)
    #   {index1=sesp[which(sesp$all_sp>0.95),]
    #  index=which.max(index1$all_se)
    #   }
    #else
    #{index=which.max(sesp$all_se+sesp$all_sp-1)}


    #index=which(sesp$se>=0.6 & sesp$sp>0.9)
    print(i)
    print(sesp[index,])



    #estimate the parameters for the last observed case
    lag_n=sesp$all_lag[index]
    c_n=sesp$all_cut[index]

    roll_n=rollsd(cases[1:i],lag_n)

    ev_n=evi(roll_n)
    ind_n=indic(ev_n,c_n, case_t)
    evicut_n=evifcut(ev_n, case_t, c_n, w_s, ratio)

    roll=c(roll,roll_n[i])
    ev=c(ev,ev_n[i])
    ind=c(ind, ind_n[i])

    lag_all=c(lag_all,lag_n)
    c_all=c(c_all,c_n)

    se_all=c(se_all,all_se[index])
    sp_all=c(sp_all,all_sp[index])

    pvs[i]=evicut_n$prev*all_se[index]/
      (evicut_n$prev*all_se[index]+(1-evicut_n$prev)*(1-all_sp[index]))

    pvn[i]=(1-evicut_n$prev)*all_sp[index]/
      ((1-evicut_n$prev)*all_sp[index]+evicut_n$prev*(1-all_se[index]))



  }


  Days=(1:length(cases))
  EVI=ev
  Cases=cases
  Index=ind

  end_time = Sys.time()

  time_elapsed = end_time - start_time

  EVI_out=as.data.frame(cbind(Days, EVI, Cases, Index, pvs, pvn,
                              lag_all, c_all, se_all, sp_all))
  EVI_output<<-(EVI_out)

  total_time = c("The elapsed time was", time_elapsed)

  return(c(EVI_output, total_time))

}
