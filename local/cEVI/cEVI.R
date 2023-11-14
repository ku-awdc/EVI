# Load functions and rest of EVI package
remotes::install_github("ku-awdc/EVI",force=T)
require(EVI)
data("Italy")
document()

tmp_EVI_at=deviant(new_cases = Italy$Cases,past = 40)
tmp_cEVI_at=deviant(new_cases = Italy$Cases,lag_max = 40,method = "cEVI",past = 40)
evirlap(Index1 = tmp_EVI_at,Index2 = tmp_cEVI_at)

tmp_EVI_at=deviant_update(EVI_input = tmp_EVI_at, new_cases = Italy$Cases,past = 40,method = "EVI")
tmp_cEVI_at=deviant_update(EVI_input = tmp_cEVI_at, new_cases = Italy$Cases,lag_max = 40,past = 40,method = "cEVI")
evirlap(Index1 = tmp_EVI_at,Index2 = tmp_cEVI_at)


# Load the mot example
data("Italy")
library(readxl)
Austria <- read_excel("EVI_official/EVI/local/cEVI/Austria_150.xlsx")

# Run cEVI for the first cases of Italy
tmp_EVI_at=deviant(new_cases = Austria$ncases)
tmp_cEVI_at=deviant(new_cases = Austria$ncases,lag_max = 40,method = "cEVI")
evirlap(Index1 = tmp_EVI_at,Index2 = tmp_cEVI_at)
# 2 min for Austria 150
# 2 min for Italy 150

#tmp_EVI_ita=deviant(new_cases = Italy$Cases)
#tmp_cEVI_ita=deviant_plus(new_cases = Italy$Cases,lag_max = 40)
# 1.60 min for Austria 150
# 1.61 min for Italy 150

# Plot Austria example first cases
pdf("EVI_Austria150.pdf",width=6,height=6)
evi.graphs(tmp_EVI_at,ln = T,type = "l")
dev.off()

pdf("cEVI_Austria150.pdf",width=6,height=6)
evi.graphs(tmp_cEVI_at,ln = T,type = "l")
dev.off()

# Plot Italy example first cases
pdf("EVI_Italy150.pdf",width=6,height=6)
evi.graphs(tmp_EVI_ita,ln = T,type = "l")
dev.off()

pdf("cEVI_Italy150.pdf",width=6,height=6)
evi.graphs(tmp_cEVI_ita,ln = T,type = "l")
dev.off()


# Ex_other example ####
library(readr)
dg <- data.frame(read_csv("~/GitHub/dg.csv"))
tmp_EVI_dg<-deviant(new_cases = dg$total_cases[1:250])
tmp_cEVI_dg<-deviant_plus(new_cases = dg$total_cases[1:250],lag_max = 40)
pdf("ex_other_EVI_250.pdf",width=6,height=6)
evi.graphs(tmp_EVI_dg,ln = T, type="l")
dev.off()

pdf("ex_other_cEVI_250.pdf",width=6,height=6)
evi.graphs(tmp_cEVI_dg,ln = T, type="l")
dev.off()


tmp_EVI_dg_all<-deviant(new_cases = dg$total_cases)
tmp_cEVI_dg_all<-deviant_plus(new_cases = dg$total_cases,lag_max = 50)
pdf("ex_other_all_EVI_500.pdf",width=6,height=6)
evi.graphs(tmp_EVI_dg_all,ln = T, type="l")
dev.off()

pdf("ex_other_all_cEVI_500.pdf",width=6,height=6)
evi.graphs(tmp_cEVI_dg_all,ln = T, type="l")
dev.off()
Austria<-Austria_150
names(Austria)<-c("Date","Cases","Cum_Cases")
save("Austria",file = "data/Austria.rda")


EVI_output0<-deviant_update(new_cases = Italy$Cases[149],
                            EVI_input=EVI_out,
                            cum=FALSE, r_a=7, r=0.2, lag_max=30)




EVI_output<-deviant(new_cases=Italy$Cases[1:49], cum=FALSE,
                    r_a=7, r=0.2, lag_max=30,method = "EVI")
EVI_output2<-deviant_update(all_cases = c(Italy$Cases[1:50],34,234,5,2,3,234,12),
                            EVI_input=EVI_output,method = "EVI")
