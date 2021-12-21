# Load functions and rest of EVI package
source("cEVI/deviant_cEVI.R")
source("cEVI/evifcut_cEVI.R")
source("cEVI/indic_cEVI.R")
require(EVI)

# Load the mot example
data("Italy")

library(readxl)
Austria <- read_excel("cEVI/Austria_200.xlsx")
Italy_300<-Italy_613[1:300,]
View(Italy_300)

# Run cEVI for the first cases of Italy
tmp_cEVI=deviant_plus(new_cases = Italy_300$ncases,method = "cEVI")

# Plot Italy example first cases
evi.graphs(tmp_cEVI)
