#' Combining multiple epidemic volatility indices into a multi-early warning index plot
#'
#' This function returns an evi.graph containing two indices.
#' 1. If index number 1 results in an early warning,then a yellow point will be shown.
#' 2. If index number 2 results in an early warning, then a orange point will be shown.
#' 3. If both indexes result in early warnings, then a red point will be shown.
#'
#' For each time point the stored variables are:
#'@return \itemize{
#' \item{evi.compare: A graph with two EVI indexes shown simulteounsly.}}

#' @param EVI1_output e.g. output of the \code{\link[EVI:deviant]{deviant()}} function
#' @param EVI2_output e.g. output of the \code{\link[EVI:deviant]{deviant_cEVI()}} function
#' @param ln  TRUE or FALSE; If TRUE (the default) the output of the graph will be presented on the logarithmic scale. IF FALSE the output data will be presented on the original scale.
#' @param type By default, points are plotted on EVI graphs (type="p"). In cases where, changes are very sudden or data sparsely available, type="l" introduces lines on top of points for the "EVI" type of graph.
#' @param size.evi Argument that controls the size of the evi lines (numeric). Default size.evi=1.
#' @param gray If TRUE then grayscale alternatives of the EVI.graphs will be ploted.
#' @param lg If TRUE then add the names of the 2 indexes in EVI1.lab and EVI2.lab and their combination in EVI3.lab. If FALSE, then the name of the country will be printed.
#' @param EVI1.lab Add the name of the first index as character.
#' @param EVI2.lab Add the name of the second index as character.
#' @param EVI3.lab Add the name of the combination of the first and second index as character.
#'
#' @examples
#'
#' # Run basic functions to acquire early warnings results
#' EVI_output=deviant(new_cases=Italy$Cases, cum=FALSE)
#' cEVI_output=deviant_cEVI(new_cases=Italy$Cases, cum=FALSE)
#'
#' # Plot the EVI combination graph
#' evi.compare(EVI1_output=EVI_output, EVI2_output=cEVI_output, ln=T)
#' evi.compare(EVI1_output=EVI_output, EVI2_output=cEVI_output, ln=T, type="l") # For the line EVI plot
#' @export
#'
#' @import ggplot2
#' @import cowplot
#'
#'
#' @references
#' Pateras K, Meletis E, Denwood M, Paolo E, Kostoulas P, The convergence epidemic volatility index an early warning tool for identifying waves in an epidemic, 2022


evi.compare <- function(EVI1_output,EVI2_output, ln=T, type="p",size.evi=1,lg=T,gray=F,
                        EVI1.lab="EVI1",EVI2.lab="EVI2",EVI3.lab="EVI+",EVI.country=NULL) {
  
  #EVI_output=temp
  EVI1_output$Index=EVI1_output$Index
  EVI2_output$Index=EVI2_output$Index*2
  EVI_output=EVI1_output
  EVI_output$Index=EVI_output$Index+EVI2_output$Index
  EVI_output$cases_1=EVI_output$Cases*EVI_output$Index
  EVI_output$cases_1[EVI_output$cases_1 == 0] <- NA
  EVI_output$cases_0=EVI_output$Cases*(1-EVI_output$Index)
  EVI_output$cases_0[EVI_output$cases_0 == 0] <- NA
  
  EVI_output$npv=EVI_output$npv*(1-EVI_output$Index)
  EVI_output$npv[EVI_output$npv == 0] <- NA
  EVI_output$ppv=EVI_output$ppv*EVI_output$Index
  EVI_output$ppv[EVI_output$ppv == 0] <- NA
  EVI_output$variable<-"x"
  EVI_output$Index[is.na(EVI_output$Index)]<-0
  EVI_output$Index<-factor(EVI_output$Index,labels = c("No warning",paste(EVI1.lab,"alone"),paste(EVI2.lab,"alone"),EVI3.lab))
  if (ln==F) {
    sp3<-ggplot(EVI_output, aes_string(x="Days",group="variable"))+
      list(
        geom_point(aes_string(y=("Cases"), color="Index"), size=size.evi),
        if (gray==F) scale_color_manual(values=c("grey69", "yellow3", "orange3", "red4")),
        if (gray==T) scale_colour_grey(start = 1,end = 0),        theme(legend.position = "none",plot.title = element_text(hjust = 0.5)),
        if (lg==T) labs(title = paste0("Graph combining outputs ",EVI1.lab,", ", EVI2.lab," and ", EVI3.lab," - ",EVI.country), y = "Cases", x="Days"),
        if (lg==F) labs(title = paste(EVI.country)),
        theme(legend.position = "bottom",
              legend.title = element_blank(),
              legend.text = element_text(size=8),
              legend.key.height = unit(0, 'cm')),
        if (type=="l")  geom_path(aes_string(y="Cases",colour="Index"),size=size.evi)
      )
  }
  
  if (ln==T) {
    sp3<-ggplot(EVI_output, aes_string(x="Days",group="variable"))+
      list(
        geom_point(aes_string(y="log(Cases)", color="Index"), size=size.evi),
        scale_color_manual(values=c("grey69", "yellow3", "orange3", "red4")),
        theme(legend.position = "none",plot.title = element_text(hjust = 0.5)),
        if (gray==F) scale_color_manual(values=c("grey69", "yellow3", "orange3", "red4")),
        if (gray==T) scale_colour_grey(start = 1,end = 0),
        if (lg==T) labs(title = paste0("Graph combining outputs ",EVI1.lab,", ", EVI2.lab," and ", EVI3.lab," - ",EVI.country), y = "Cases", x="Days"),
        if (lg==F) labs(title = paste(EVI.country)),
        theme(legend.position = "bottom",
              legend.title = element_blank(),
              legend.text = element_text(size=8),
              legend.key.height = unit(0, 'cm')),
        if (type=="l")  geom_path(aes_string(y="log(Cases)",colour="Index"), size=size.evi)
      )
  }
  print(sp3)
  
}
