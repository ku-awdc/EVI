#'  This function  produces descriptive figures for the Epidemic Volatility index based output data
#'
#' Three types of graphs can be plotted with the data presented either on the original scale or the logarithmic case.
#' The first type of graph is the confirmed cases represented as dots. Red dots correspond to time points when an early warning was issued and indicate that, according to the defined criterion, an increase in the mean of expected cases equal or higher to twenty percent is expected in the coming week. 
#' Grey dots correspond to time points without an early warning indication.
#' The second and third type of graphs, respectively, are the positive and negative predictive values at each time point, depending on whether or not an early warning was issued. 
#' Higher color intensity corresponds to predictive values closer to the value of 1..
#' 
#' 
#' @param graph Type of graph to be plotted. Options: "cases", "pv_plus", "pv_minus"; If "cases" is selected{defalut} the daily number of confirmed cases is plotted. If "pv_plus" or "pv_minus" is selected the positive or negative predictive values are plotted, respectively.
#' @param ln TRUE or FALSE; If TRUE{default} the output of the graph will be presented on the logarithmic scale. IF FALSE the output data will be presented on the original scale.
#'
#'
#' @examples
#' evi.graphs(EVI_output)
#'
#' @export

evi.graphs=function(EVI_output,graph=c("cases"), ln=T) {

  list.of.packages <- c("ggplot2", "cowplot")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  require(ggplot2)
  require(cowplot)

  EVI_output$cases_1=EVI_output$Cases*EVI_output$Index
  EVI_output$cases_1[EVI_output$cases_1 == 0] <- NA
  EVI_output$cases_0=EVI_output$Cases*(1-EVI_output$Index)
  EVI_output$cases_0[EVI_output$cases_0 == 0] <- NA

  EVI_output$pvn=EVI_output$pvn*(1-EVI_output$Index)
  EVI_output$pvn[EVI_output$pvn == 0] <- NA
  EVI_output$pvs=EVI_output$pvs*EVI_output$Index
  EVI_output$pvs[EVI_output$pvs == 0] <- NA

  if (graph=="cases" && ln==F) {
    sp3<-ggplot(EVI_output, aes(x=Days))+
      geom_point(aes(y=(Cases), color=Index>0), size=0.5)+
      scale_color_manual(values=c("grey69", "red3"))+
      theme(legend.position = "none")+
      labs(y = "Cases", x="Days")
      }

  if (graph=="cases" && ln==T) {
    sp3<-ggplot(EVI_output, aes(x=Days))+
      geom_point(aes(y=log(Cases), color=Index>0), size=0.5)+
      scale_color_manual(values=c("grey69", "red3"))+
      theme(legend.position = "none")+
      labs(y = "ln(Cases)", x="Days")
     }

  if (graph=="pv_plus" && ln==F) {
    sp3<-ggplot(EVI_output, aes(x=Days))+
      geom_point(aes(y=(cases_1), col=pvs), size=0.5)+
      geom_point(aes(y=(cases_0)), col="grey69", size=0.5)+
      labs(y = "Cases", x="")+
      scale_color_gradient(low = "green", high = "red", limits=c(0, 1))+
      labs(color= "PPV")+
      theme(legend.position = c(0.95, 0.3),
            legend.title = element_text(size=10),
            legend.text = element_text(size=8),
            legend.key.height = unit(0.5, 'cm'))
      }

  if (graph=="pv_plus" && ln==T) {
    sp3<-ggplot(EVI_output, aes(x=Days))+
      geom_point(aes(y=log(cases_1), col=pvs), size=0.5)+
      geom_point(aes(y=log(cases_0)), col="grey69", size=0.5)+
      labs(y = "ln(Cases)", x="")+
      scale_color_gradient(low = "green", high = "red", limits=c(0, 1))+
      labs(color= "PPV")+
      theme(legend.position = c(0.95, 0.3),
            legend.title = element_text(size=10),
            legend.text = element_text(size=8),
            legend.key.height = unit(0.5, 'cm'))
  }

  if (graph=="pv_minus" && ln==F) {
    sp3<-ggplot(EVI_output, aes(x=Days))+
      geom_point(aes(y=(cases_0), col=pvn), size=0.5)+
      geom_point(aes(y=(cases_1)), col="grey69", size=0.5)+
      labs(y = "Cases")+
      scale_color_gradient(low = "green", high = "red", limits=c(0, 1))+
      labs(color= "NPV")+
      theme(legend.position = c(0.95, 0.3),
            legend.title = element_text(size=10),
            legend.text = element_text(size=8),
            legend.key.height = unit(0.5, 'cm'))
  }


  if (graph=="pv_minus" && ln==T) {
    sp3<-ggplot(EVI_output, aes(x=Days))+
      geom_point(aes(y=log(cases_0), col=pvn), size=0.5)+
      geom_point(aes(y=log(cases_1)), col="grey69", size=0.5)+
      labs(y = "ln(Cases)")+
      scale_color_gradient(low = "green", high = "red", limits=c(0, 1))+
      labs(color= "NPV")+
      theme(legend.position = c(0.95, 0.3),
            legend.title = element_text(size=10),
            legend.text = element_text(size=8),
            legend.key.height = unit(0.5, 'cm'))
  }

  print(sp3)

  }












