#' This function is used after first running the ceviant function
#'
#' Once the \code{\link[EVI:ceviant]{ceviant()}} function has been used to analyze the already observed time series,
#' the ceviant_update() function is used to obtain the EVI output and early warnings for the new cases that are recorded.
#' After running the ceviant_update() function the output of the ceviant function is also updated with a new row of data for each newly observed time point.


#' @param new_cases the time series of the previous and newly observed cases per unit of time (ideally per day).
#' @param cum TRUE if the time series is recorded as the cumulative number of the reported cases and FALSE (the default) if newly reported cases per unit of time are recorded.
#' @param r_a The window size for the moving average that will be analyzed. If set to 1 the actual observations are analyzed. However, due to the variability of the reported cases between working days and weekends it is recommended that the 7-day moving average is analyzed (i.e. r_a = 7), which is the default for this argument. Users could prefer a longer interval of 14 days or one month (i.e., r_a=14 or 30, respectively).
#' @param r Definition for the minimum difference in the mean number of cases, one week before and after each time point that, if present, should be detected. This is the case definition and the default is 0.2 (with 0 <= r <= 1). A value of r=0.2 means that we have a case when the mean number of the newly observed cases in the next 7 days is at least 20% higher than the mean number of the newly observed cases in the past 7 days.
#' @param lag_max Integer. Restriction of the maximum window size for the rolling window size. The default is set to one month (lag_max=30) to prevent excess volatility of past epidemic waves from affecting the most recent volatility estimates and the ability of EVI to warn for upcoming waves that may be smaller and of lower volatility than previous ones.
#'
#'
#' @examples
#' # If we have first observed only the 148 cases from the Italian data we run the ceviant function on these cases:
#'
#' data("Italy")
#' ceviant(new_cases=Italy$Cases[1:148], cum=FALSE, r_a=7, r=0.2, lag_max=30)
#'
#' # When the number of cases for the next day is observed we want to obtain the estimates for this day without having to reanalyze the entire time series. This is done by using the ceviant_update function:
#'
#' ceviant_update(new_cases=Italy$Cases[1:149], cum=FALSE, r_a=7, r=0.2, lag_max=30)
#'
#' # The result of running the ceviant_update function is to update the output of the ceviant_function by adding an additional row with estimates for the new data.
#' # In this example the cEVI_output file will now have 149 rows. If two additional days are analyzed two additional rows will be added and so on.

#'
#' @export
#'
#' @references
#' Kostoulas, P., Meletis, E., Pateras, K. et al. The epidemic volatility index, a novel early warning tool for identifying new waves in an epidemic. Sci Rep 11, 23775 (2021). \doi{10.1038/s41598-021-02622-3}


ceviant_update=function(new_cases, cum = FALSE, r_a=7, r=0.2, lag_max=30){
  #source("mova.r")
  #source("medvol.r")
  #source("evi.r")
  #source("evifcut.r")
  #source("indic.r")
  #source("status.r")
  #source("rollsd.r")


  start_time = Sys.time()
  start_cases=18
  lag_1=3
  c_1=0.001
  w_s=7
  if (cum == TRUE){
    new_cases = c(new_cases[1], diff(new_cases))
  }
  cases=mova(new_cases, r_a)
  #cases=new_cases
  cevi=cEVI_fun(cases = cases[1:(start_cases)],lag_n = lag_1, c_n = c_1)
  ind=indic_cEVI(cevi=cevi, cases=cases[1:start_cases])
  status=status(cases[1:start_cases],r)

  #initiate chain for positive predictive value
  ppv=rep(NA, length(cases))
  #initiate chain for negative predictive value
  npv=rep(NA, length(cases))

  lag_all=rep(NA, start_cases)
  c_all=rep(NA, start_cases)
  se_all=rep(NA, start_cases)
  sp_all=rep(NA, start_cases)
  lag_all[1:start_cases]=lag_1
  c_all[1:start_cases]=c_1

  diff= length(cases)-(nrow(cEVI_output) +1)

  for (i in (nrow(cEVI_output)+1): length(cases)){

      case_t=cases[1:i]
      lag_s=seq(lag_1,min(lag_max,(i-i/2-4)), 2)
      c_s=seq(0.001,0.5, 0.06)
      all_lag=NA
      all_cut=NA
      all_se=NA
      all_sp=NA

      for (l in c_s) {
        for (j in lag_s) {
          # roll_t <- rollsd(case_t,j)
          #  ev_t <- evi(roll_t)
          cevi <- rep(NA, length(case_t))
          for(k in (j+1):(length(case_t)-(j+1))){
            enu=mean(case_t[(k+2):(k+j+1)]-case_t[(k):(k-(j-1))],na.rm = T)
            den1=sd(case_t[(k):(k-(j-1))])^2/(length(case_t[(k):(k-(j-1))]))
            den2=sd(case_t[(k+2):(k+j+1)])^2/(length(case_t[(k+2):(k+j+1)]))
            # Spectral variances more appropriate but more time consuming
            #den1=spectrum0.ar(case_t[(i+1):(i+w_s)])$spec/(length(case_t[(i+1):(i+w_s)])) # Spectral variances
            #den2=spectrum0.ar(case_t[(i):(i-(w_s-1))])$spec/(length(case_t[(i):(i-(w_s-1))]))
            test=enu/sqrt(den1+den2)
            cevi[k+j+1]=as.numeric((1-pnorm(test))<=l)#*as.numeric(evi[i] >= rate)
          }
          evicut_t <- evifcut_cEVI(cevi=cevi,cases = case_t, r = r)
          all_lag[[length(all_lag) + 1]] <- j
          all_cut[[length(all_cut) + 1]] <- l
          all_se[[length(all_se) + 1]] <- evicut_t[[1]]
          all_sp[[length(all_sp) + 1]] <- evicut_t[[2]]
        }
      }

      sesp=as.data.frame(cbind(all_lag,all_cut,all_se,all_sp))
      index=which.max(sesp$all_se+sesp$all_sp-1)

      print(i)
      print(sesp[index,])

      lag_n=sesp$all_lag[index]
      c_n=sesp$all_cut[index]

      cevi=cEVI_fun(cases = cases[1:i],lag_n = lag_n, c_n = c_n) #
      ind_n=indic_cEVI(cevi = cevi, cases = case_t) #
      evicut_n=evifcut_cEVI(cevi = cevi, cases = case_t, r = r) #

      ind=c(ind, ind_n[i])
      lag_all=c(lag_all,lag_n)
      c_all=c(c_all,c_n)

      se_all=c(se_all,all_se[index])
      sp_all=c(sp_all,all_sp[index])

      ppv[i]=evicut_n$prev*all_se[index]/
        (evicut_n$prev*all_se[index]+(1-evicut_n$prev)*(1-all_sp[index]))
      npv[i]=(1-evicut_n$prev)*all_sp[index]/
        ((1-evicut_n$prev)*all_sp[index]+evicut_n$prev*(1-all_se[index]))
    }

  Days=((length(cases)-diff):length(cases))
  EVI=cevi[((length(cevi)-diff):length(cevi))]
  Cases=cases[((length(cases)-diff):length(cases))]
  Index=ind[((length(ind)-diff):length(ind))]
  ppv=ppv[((length(ppv)-diff):length(ppv))]
  npv=npv[((length(npv)-diff):length(npv))]
  lag_all=lag_all[((length(lag_all)-diff):length(lag_all))]
  c_all=c_all[((length(c_all)-diff):length(c_all))]
  se_all=se_all[((length(se_all)-diff):length(se_all))]
  sp_all=sp_all[((length(sp_all)-diff):length(sp_all))]


  cEVI_out_add=as.data.frame(cbind(Days, EVI, Cases, Index, ppv, npv,
                                  lag_all, c_all, se_all, sp_all))

  cEVI_output=rbind(cEVI_output,cEVI_out_add)

  cEVI_output<<-(cEVI_output)

  return(cEVI_output)

}
