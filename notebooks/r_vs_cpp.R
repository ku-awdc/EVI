## Compare R to C++ code:

library("EVI")

# Get a large dataset:
data(Italy)

repit <- lapply(1:1e5, function(x) Italy) |> dplyr::bind_rows()
repit$Cases[1] <- 1

options(EVI_disable_cpp=FALSE)
system.time({
  cpp <- mova(cases = repit$Cases, r_a = 7)
})
# On my system: 0.1 seconds

options(EVI_disable_cpp=TRUE)
system.time({
  r <- mova(cases = repit$Cases, r_a = 7)
})
# On my system: 24 seconds

stopifnot(all(abs(cpp-r) < .Machine$double.eps^0.5))

