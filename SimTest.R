rm(list = ls())

source("SimSource.R")

# ecls <- read.csv("eqData SCL.csv")
# cor(ecls$Mob, ecls$PRE)

#----------------------
# Conditions
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
# # u0.sd <- sqrt(46)
# 
# # ICC <- u0.sd^2 / (e.sd^2 + u0.sd^2)
# 
# X.M.Bs <- round(gen_Mob_Coef(stand(rnorm(100000, X.mean, X.sd)), X.M.cor, mobRate, range = c(-1, 1) * 200), 2)
# 
# RIM.Bs <- c(B0 = 125, X = 1, M = .1, M.S = -2, X.S = .2, u0 = 1, e = 1)
# # RIM.Bs <- c(B0 = 1, X = 1, M = 1, M.S = -1, X.S = 1, u0 = 1, e = 1)

#----------------------
# Testrun
#----------------------

# test_df <- gen_data(n.students, X.mean, X.sd, X.M.Bs, e.mean, e.sd, tripleRate, n.schools, RIM.Bs)
# 
# test_mods <- run_RIM(test_df)
# test_res <- bind_rows(lapply(test_mods, getCoefs)) %>%
#   mutate(Est = paste(EstEqual, EstRandom, sep = "_"))
# 
# test_res %>%
#   ggplot(aes(x = vars, y = estimates, color = Est)) +
#   geom_point() +
#   geom_point(aes(y = true), shape = "x", size = 4, color = "black") +
#   facet_wrap(~ Gen)
# 
# test_res %>%
#   mutate(dist = estimates - true,
#          upper = error,
#          lower = -error) %>%
#   ggplot(aes(x = Est, y = estimates, color = Est)) +
#   geom_point() +
#   geom_errorbar(aes(ymin = estimates - error, ymax = estimates + error)) +
#   # geom_hline(aes(yintercept = 0)) + 
#   # geom_hline(aes(yintercept = true + true * c(-.1, .1)), linetype = "dashed") +
#   facet_wrap(Gen ~ vars, scales = "free_y")

#----------------------
# Conditions - real
#----------------------
# n.schools <- 100
# n.students <- 3000
# 
# # n.schools <- 10
# # n.students <- 1000
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


#----------------------
# Conditions - clean
#----------------------
rm(list = ls())

source("SimSource.R")

# n.schools <- 100
# n.students <- 3000

n.schools <- 10
n.students <- 100

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
# undebug(gen_Mob_Coef)
# undebug(flip)

RIM.Bs <- c(B0 = 0, X = 1, M = 1, M.S = 1, X.S = 1, u0 = 1, e = 1)

#----------------------
# SimDriver
#----------------------
driver <- function(reps, n.students, X.mean, X.sd, X.M.Bs, e.mean, e.sd, tripleRate, n.schools, RIM.Bs, u0.mean, u0.sd, ICC) {
  source("SimSource.R")
  
  replicate(reps, 
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
            simplify = F)
}

# driver(1,
#        n.students,
#        X.mean,
#        X.sd,
#        X.M.Bs,
#        e.mean,
#        e.sd,
#        tripleRate,
#        n.schools,
#        RIM.Bs,
#        u0.mean,
#        u0.sd, ICC)


#----------------------
# Run Sim
#----------------------

# # seed <- runif(1,0,1)*10^8
# set.seed(42987117)
# 
# reps = 1
# 
# runtime <- system.time(results <- driver(reps,
#                                          n.students,
#                                          X.mean,
#                                          X.sd,
#                                          X.M.Bs,
#                                          e.mean,
#                                          e.sd,
#                                          tripleRate,
#                                          n.schools,
#                                          RIM.Bs,
#                                          u0.mean,
#                                          u0.sd, ICC))
# 
# # undebug(gen_data)
# 
# save(runtime, file = "Data/Sim Test Runtime.rdata")
# load("Data/Sim Test Runtime.rdata")
# 
# avgRun <- runtime/reps
# avgRun * 1000 / 60 / 60 # Hours
# avgRun * 1000 / 60      # Minutes
# 
# 
# results <- bind_rows(results) %>% data.frame 
# 
# save(results, file = "Results/results_clean_rim.rdata")

#----------------------
# Run Sim Parallel
#----------------------
library(parallel)

# Calculate the number of cores
no_cores <- detectCores() - 1

# Initiate cluster
cl <- makeCluster(no_cores)

# clusterExport(cl, c("n.students",
#                     "X.mean",
#                     "X.sd",
#                     "X.M.Bs",
#                     "e.mean",
#                     "e.sd",
#                     "tripleRate",
#                     "n.schools",
#                     "RIM.Bs"))

# seed <- runif(1,0,1)*10^8
set.seed(42987117)

reps = 10

runtime <- system.time(results <- parLapply(cl = cl,
                                            X = rep(1, no_cores),
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


save(runtime, file = "Data/Sim Test Runtime.rdata")
load("Data/Sim Test Runtime.rdata")

avgRun <- runtime/reps/no_cores
avgRun * 1000 / 60 / 60 # Hours
avgRun * 1000 / 60      # Minutes


results <- bind_rows(results) %>% data.frame

save(results, file = "Results/results_clean_rim.rdata")


