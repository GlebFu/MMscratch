library(MASS)
library(parallel)

rm(list = ls())

source("SimSource.R")

#----------------------
# Conditions - Real
#----------------------

cond <- list(n.students = 3000,
             X_PS.cor = c(0:9/10),
             X.m = 0,
             X.sd = 1,
             M.m = .25,
             e.m = 0,
             e.sd = 1,
             tripleRate = .375,
             n.schools = 100,
             u0.m = 0,
             u0.sd = 1) %>%
  expand.grid()


sim_driver <- function(cond, reps) {
  source("SimSource.R")
  cond <- data.frame(t(cond))

  results <- replicate(reps, 
                       run_sim(cond$n.students, 
                               cond$X_PS.cor, 
                               cond$X.m, 
                               cond$X.sd, 
                               cond$M.m, 
                               cond$e.m, 
                               cond$e.sd, 
                               cond$tripleRate, 
                               cond$n.schools, 
                               cond$u0.m, 
                               cond$u0.sd, 
                               cond$genMod), 
                       simplify = F) %>%
    bind_rows() %>% 
    data.frame()  %>% 
    cbind(cond)
  
  return(results)

}

# Calculate the number of cores
no_cores <- detectCores() - 1

# Initiate cluster
cl <- makeCluster(no_cores)

# seed <- runif(1,0,1)*10^8
set.seed(42987117)

reps <- 10

runtime <- system.time(results <- parApply(cl, cond, 1, sim_driver, reps))

stopCluster(cl)


save(runtime, reps, no_cores, file = "Data/ParTimeClean.rdata")
load("Data/ParTimeClean.rdata")

avgRun <- runtime/reps/no_cores
avgRun * 1000 / 60 / 60 # Hours
avgRun * 1000 / 60      # Minutes