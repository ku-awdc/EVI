# EVI
EVI: the Epidemic Volatility Index as an early-warning tool for epidemic waves

## Installation

To install the current source from GitHub use:

    install.packages(c("devtools", "remotes")
    require(devtools)
    require(remotes)
    remotes::install_github("ku-awdc/EVI")
    

To install a stable version from the drat repository use:

    ## Will be added once a stable version is available

## Basic functions of EVI package

    require(EVI)

To load some example data:

    data("Italy")
    
To run EVI analysis on the example data:

    deviant(Italy$ncases)

To create a plot of the analysed data: 

    evi.graphs(Italy)
    
The basic two functions of the EVI analysis are deviant() and evi.graphs(). For help on these functions type:  
    
    ?deviant
    ?evi.graphs

