## Compare R to C++ code:

library("EVI")

# Get a large dataset:
data(Italy)

repit <- lapply(1:1e5, function(x) Italy$Cases) |> unlist()
repit[1] <- 1

options(EVI_disable_cpp=FALSE)
system.time({
  cpp <- mova(cases = repit, r_a = 7)
})
# On my system: 0.15 seconds

options(EVI_disable_cpp=TRUE)
system.time({
  r <- mova(cases = repit, r_a = 7)
})
# On my system: 25 seconds

stopifnot(all(abs(cpp-r) < .Machine$double.eps^0.5))
cbind(cpp, r)[2:14,]

# Note that the C++ implementation leaves floating point error on zero -
# I've added a potential fix for this to the R code (but commented out)
