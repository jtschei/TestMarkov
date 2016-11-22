# test a markov model with dice
library(markovchain)

dieState <- c("1","2","3","4","5","6")
byRow <- TRUE
dieMatrix <- matrix(data = rep((1/6),36), byrow = byRow, nrow = 6, dimnames = list(dieState, dieState))
mcDie <- new("markovchain", states = dieState, byrow = byRow, transitionMatrix = dieMatrix, name = "Die")
initialState <- c(1,0,0,0,0,0)
after7Rolls <- (t(mcDie) ^ 7) * initialState
after7Rolls
show(mcDie)
plot(mcDie)
rolls <- rmarkovchain(n = 10, object = mcDie, t0 = "1")
#missing example of how to fit model to data