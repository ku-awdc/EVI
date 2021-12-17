#'  This function  produces  plots of the time series data with the EVI predictions.
#'
#' Three types of plots are generated: 
#' (i) A plot of the confirmed cases with red dots corresponding to time points that an early warning was issued and grey dots corresponding to time points without an early warning indication. 
#' (ii) A plot of the confirmed cases with colored dots corresponding to time points with an early warning. Color intensity is increasing for higher positive predictive value (PPV). 
#' (iii) A plot of the confirmed cases with colored dots corresponding to time points without an early warning. Color intensity is increasing for higher negative predictive value (NPV).#' 
#' 
#' @param graph Type of graph to be plotted. Options: "EVI", "PPV", "NPV"; "EVI" {default} is giving a plot of the confirmed cases, with red dots corresponding to time points that an early warning was issued and grey dots corresponding to time points without an early warning indication. "PPV" is giving a plot of the confirmed cases with colored dots corresponding to time points with an early warning. Color intensity is increasing for higher PPV. "NPV" is giving a plot of the confirmed cases with colored dots corresponding to time points without an early warning. Color intensity is increasing for higher NPV. 
#' @param ln  TRUE or FALSE; If TRUE{default} the output of the graph will be presented on the logarithmic scale. IF FALSE the output data will be presented on the original scale.
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

  if (graph=="EVI" && ln==F) {
    sp3<-ggplot(EVI_output, aes(x=Days))+
      geom_point(aes(y=(Cases), color=Index>0), size=0.5)+
      scale_color_manual(values=c("grey69", "red3"))+
      theme(legend.position = "none")+
      labs(y = "Cases", x="Days")
      }

  if (graph=="EVI" && ln==T) {
    sp3<-ggplot(EVI_output, aes(x=Days))+
      geom_point(aes(y=log(Cases), color=Index>0), size=0.5)+
      scale_color_manual(values=c("grey69", "red3"))+
      theme(legend.position = "none")+
      labs(y = "ln(Cases)", x="Days")
     }

  if (graph=="PPV" && ln==F) {
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

  if (graph=="PPV" && ln==T) {
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

  if (graph=="NPV" && ln==F) {
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


  if (graph=="NPV" && ln==T) {
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












