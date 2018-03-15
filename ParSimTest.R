library(MASS)

rm(list = ls())

source("SimSource.R")

#----------------------
# SimDriver
#----------------------
driver <- function(reps, n.students, X.mean, X.sd, X.M.Bs, e.mean, e.sd, tripleRate, n.schools, RIM.Bs, u0.mean, u0.sd, ICC) {
  source("SimSource.R")
  
  results <- replicate(reps, 
            run_sim(n.students, 
                    X.mean, 
                    X.sd, 
                    X.M.Bs, 
                    e.mean, 
                    e.sd, 
                    tripleRate, 
                    n.schools, 
                    RIM.Bs,
                    u0.mean,
                    u0.sd,
                    ICC), 
            simplify = F) %>%
    bind_rows() %>% 
    data.frame()
  
  return(results)
}


#----------------------
# Conditions - Real
#----------------------
# n.schools <- 100
# n.students <- 3000
# 
# mobRate <- .25
# tripleRate <- .375
# 
# X.mean <- 60
# X.sd <- 10
# X.M.cor <- -.05
# 
# e.mean <- 0
# e.sd <- sqrt(240)
# 
# ICC <- .15
# 
# u0.mean <- 0
# u0.sd <- sqrt((e.sd^2 * ICC) / (1 - ICC))
# 
# X.M.Bs <- round(gen_Mob_Coef(stand(rnorm(100000, X.mean, X.sd)), X.M.cor, mobRate, range = c(-1, 1) * 200), 2)
# 
# RIM.Bs <- c(B0 = 125, X = 1, M = .1, M.S = -2, X.S = .2, u0 = 1, e = 1)

cond <- list(n.schools = 100,
             n.students = 3000,
             mobRate = .25,
             tripleRate = .375,
             X.mean = 60,
             X.sd = 10,
             X.M.cor = c(0, .5),
             e.mean = 0,
             e.sd = sqrt(240),
             ICC = .15,
             u0.mean = 0) %>%
  expand.grid() %>%
  mutate(u0.sd = sqrt((e.sd^2 * ICC) / (1 - ICC)))

cond %>%
  select(X.mean, X.sd, X.M.cor, mobRate) 


X.M.Bs = round(gen_Mob_Coef(stand(rnorm(100000, X.mean, X.sd)), X.M.cor, mobRate, range = c(-1, 1)), 2)




RIM.Bs <- c(B0 = 125, X = 1, M = .1, M.S = -2, X.S = .2, u0 = 1, e = 1)


#----------------------
# Sim - Real
#----------------------
library(parallel)

# Calculate the number of cores
no_cores <- detectCores() - 1

# Initiate cluster
cl <- makeCluster(no_cores)

# seed <- runif(1,0,1)*10^8
set.seed(42987117)

reps = 150

runtime <- system.time(results <- parLapply(cl = cl,
                                            X = rep(reps, no_cores),
                                            fun = driver,
                                            n.students,
                                            X.mean,
                                            X.sd,
                                            X.M.Bs,
                                            e.mean,
                                            e.sd,
                                            tripleRate,
                                            n.schools,
                                            RIM.Bs,
                                            u0.mean,
                                            u0.sd,
                                            ICC))

stopCluster(cl)


save(runtime, reps, no_cores, file = "Data/ParTimeReal.rdata")
load("Data/ParTimeReal.rdata")

avgRun <- runtime/reps/no_cores
avgRun * 1000 / 60 / 60 # Hours
avgRun * 1000 / 60      # Minutes


results <- bind_rows(results) %>% data.frame

save(results, file = "Results/results_real_rim.rdata")

#----------------------
# Conditions - clean
#----------------------

source("SimSource.R")

n.schools <- 100
n.students <- 3000

mobRate <- .25
tripleRate <- .375

X.mean <- 0
X.sd <- 1
X.M.cor <- 0

e.mean <- 0
e.sd <- sqrt(1)

ICC <- .15

u0.mean <- 0
u0.sd <- sqrt((e.sd^2 * ICC) / (1 - ICC))

X.M.Bs <- round(gen_Mob_Coef(stand(rnorm(100000, X.mean, X.sd)), X.M.cor, mobRate, range = c(-1, 1) * 5), 2)

RIM.Bs <- c(B0 = 0, X = 1, M = 1, M.S = 1, X.S = 1, u0 = 1, e = 1)

#----------------------
# Sim - Clean
#----------------------
library(parallel)

# Calculate the number of cores
no_cores <- detectCores() - 1

# Initiate cluster
cl <- makeCluster(no_cores)

# seed <- runif(1,0,1)*10^8
set.seed(42987117)

reps = 150

runtime <- system.time(results <- parLapply(cl = cl,
                                            X = rep(reps, no_cores),
                                            fun = driver,
                                            n.students,
                                            X.mean,
                                            X.sd,
                                            X.M.Bs,
                                            e.mean,
                                            e.sd,
                                            tripleRate,
                                            n.schools,
                                            RIM.Bs,
                                            u0.mean,
                                            u0.sd,
                                            ICC))

stopCluster(cl)


save(runtime, reps, no_cores, file = "Data/ParTimeClean.rdata")
load("Data/ParTimeClean.rdata")

avgRun <- runtime/reps/no_cores
avgRun * 1000 / 60 / 60 # Hours
avgRun * 1000 / 60      # Minutes


results <- bind_rows(results) %>% data.frame

save(results, file = "Results/results_clean_rim.rdata")


