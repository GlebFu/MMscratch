library(parallel)

rm(list = ls())

source("SimSource.R")

#----------------------
# Conditions - Real
#----------------------

# ICC
# e.sd <- 1
# ICC <- .25
# sqrt((e.sd^2 * ICC) / (1 - ICC))


cond <- list(n.students = 3000,
             # X_PS.cor = c(0:9/10),
             X_PS.cor = c(0, .25, .5),
             X.m = 0,
             X.sd = 1,
             M.m = c(.10, .25),
             e.m = 0,
             e.sd = 1,
             tripleRate = .375,
             n.schools = 100,
             u0.m = 0,
             u0.sd = c(.42, .577),
             gen_frm = list(~ 1 + X + M + w.u0 + e),
             Bs = list(c(0, 1, 1, 1, 1)),
             est_frm = list(Y ~ 1 + X + M + (1 | mm1) + (1 | ID))) %>%
  expand.grid() %>%
  mutate(ICC = u0.sd^2 / (u0.sd^2 + e.sd^2),
         CID = 1:n()) %>%
  as.tibble()

cond %>% mutate_all(as.character) %>% write.csv("Results/2018-05-03/conditions.csv")

# cond <- cond[1:12,]
# cond <- cond[13:24,]

sim_driver <- function(cond) {
  source("SimSource.R")
  # debug(gen_Y)
  
  # return(cond$reps)
  results <- replicate(cond$reps,
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
                               cond$gen_frm,
                               cond$Bs,
                               cond$est_frm,
                               w.sd = 0),
                       simplify = F) %>%
    bind_rows() %>%
    data.frame()

  return(results)

}

# Calculate the number of cores
# cond$reps <- 1
# sim_driver(cond[1,])
# # undebug(sim_driver)

no_cores <- detectCores() - 1

cond <- bind_rows(replicate(no_cores, cond, simplify = FALSE))

minreps <- 100
reps <- (minreps + (no_cores - minreps %% no_cores)) / no_cores
cond$reps <- reps

# Initiate cluster
cl <- makeCluster(no_cores)

# seed <- runif(1,0,1)*10^8
set.seed(60240601)


# runtime <- system.time(results <- parApply(cl, cond, 1, sim_driver))
runtime <- system.time(results <- parApply(cl, cond[1,], 1, sim_driver))

stopCluster(cl)



save(runtime, reps, no_cores, file = "Results/2018-05-03/time.rdata")
load("Results/2018-05-03/time.rdata")

avgRun <- runtime / (reps * no_cores)
round(avgRun * 200 / 60 / 60, 2) # Hours
round(avgRun * 200 / 60, 2)      # Minutes

save(results, cond, seed, file = "Results/2018-05-03/full.rdata")

cond <- mutate(cond, RID = 1:n())

for(i in 1:nrow(cond)) {
  cbind(cond[i,] %>% mutate_if(is.list, as.character), results[[i]]) %>%
    write_csv(paste("Results/2018-05-03/", cond[i,]$CID, "-", cond[i,]$RID, " Results.csv", sep = ""))
}


load("Results/2018-05-03/full.rdata")