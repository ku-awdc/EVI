# Load functions and rest of EVI package
source("cEVI/deviant_cEVI.R")
source("cEVI/evifcut_cEVI.R")
source("cEVI/indic_cEVI.R")
require(EVI)

# Load the mot example
data("Italy")

# Run cEVI for the first 150 cases of Italy
tmp_cEVI=deviant_plus(new_cases = Italy$Cases,method = "cEVI")

# Plot Italy example first 150 cases
evi.graphs(tmp_cEVI)
