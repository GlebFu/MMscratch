rm(list = ls())

source("SimSource.R")

df <- gen_data(n.students = 3000, 
               X_PS.cor = 0, 
               X.m = 0, 
               X.sd = 1, 
               M.m = 0, 
               e.m = 0, 
               e.sd = 1, 
               tripleRate = .3, 
               n.schools = 100, 
               u0.m = 0, 
               u0.sd = .5, 
               genMod = (~ 1 + X + M + wMj + wXj + w.u0 + e), 
               gmc = T)

debug(gen_data)
debug(moveSch)
