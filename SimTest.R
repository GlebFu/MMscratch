library(stringr)
library(R2MLwiN)
options(MLwiN_path="C:/Program Files/MLwiN v2.36/")

rm(list = ls())

source("SimSource.R")

# ecls <- read.csv("eqData SCL.csv")
# cor(ecls$Mob, ecls$PRE)

#----------------------
# Conditions
#----------------------
n.schools <- 100
n.students <- 3000

mobRate <- .25
tripleRate <- .375

X.mean <- 60
X.sd <- 10
X.M.cor <- -.05

e.mean <- 0
e.sd <- sqrt(240)

ICC <- .15

u0.mean <- 0
u0.sd <- sqrt((e.sd^2 * ICC) / (1 - ICC))
# u0.sd <- sqrt(46)

# ICC <- u0.sd^2 / (e.sd^2 + u0.sd^2)

X.M.Bs <- round(gen_Mob_Coef(stand(rnorm(100000, X.mean, X.sd)), X.M.cor, mobRate, range = c(-1, 1) * 200), 2)

RIM.Bs <- c(B0 = 125, X = 1, M = .1, M.S = -2, X.S = .2, u0 = 1, e = 1)
# RIM.Bs <- c(B0 = 1, X = 1, M = 1, M.S = -1, X.S = 1, u0 = 1, e = 1)

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
# Conditions
#----------------------
n.schools <- 100
n.students <- 3000

# n.schools <- 10
# n.students <- 1000

mobRate <- .25
tripleRate <- .375

X.mean <- 60
X.sd <- 10
X.M.cor <- -.05

e.mean <- 0
e.sd <- sqrt(240)

ICC <- .15

u0.mean <- 0
u0.sd <- sqrt((e.sd^2 * ICC) / (1 - ICC))

X.M.Bs <- round(gen_Mob_Coef(stand(rnorm(100000, X.mean, X.sd)), X.M.cor, mobRate, range = c(-1, 1) * 200), 2)

RIM.Bs <- c(B0 = 125, X = 1, M = .1, M.S = -2, X.S = .2, u0 = 1, e = 1)

#----------------------
# Run Sim
#----------------------

# seed <- runif(1,0,1)*10^8
set.seed(42987117)

reps = 500

runtime <- system.time(results <- replicate(reps, run_sim(n.students, X.mean, X.sd, X.M.Bs, e.mean, e.sd, tripleRate, n.schools, RIM.Bs), simplify = F))

save(runtime, file = "Data/Sim Test Runtime.rdata")
load("Data/Sim Test Runtime.rdata")

avgRun <- runtime/reps
avgRun * 1000 / 60 / 60 # Hours
avgRun * 1000 / 60      # Minutes


results <- bind_rows(results) %>% data.frame 

save(results, file = "Data/results.rdata")

load("Data/results.rdata")

#----------------------
# Explore Data
#----------------------

# df %>%
#   select(ID, estimation, S1:S3, w1:w3) %>%
#   gather(key = "Var.Time", value = "value", S1:S3, w1:w3) %>%
#   mutate(var = str_sub(Var.Time, end = 1),
#        time = paste("t", str_sub(Var.Time, start = 2), sep = "")) %>%
#   select(-Var.Time) %>%
#   spread(key = var, value = value) %>%
#   group_by(ID, estimation, S) %>%
#   mutate(w2 = sum(w))
# 
# df %>%
#   select(nSchools, w1:w3, estimation) %>%
#   gather(key = time, value = weight, -nSchools, -estimation) %>%
#   filter(weight != 0) %>%
#   ggplot(aes(x = weight, color = time)) +
#   geom_density() +
#   facet_grid(estimation~nSchools)
# 
# df %>%
#   select(estimation, nSchools, w1:w3) %>%
#   gather(key = time, value = weight, -nSchools, -estimation) %>%
#   filter(weight != 0) %>%
#   group_by(estimation, nSchools, time) %>%
#   summarise(m = mean(weight),
#             s = sd(weight),
#             n = n())
# 
# 
# df %>%
#   select(M, S1, S2, S3, estimation) %>%
#   filter(estimation == "Fixed_Equal") %>%
#   gather(key = Time, value = ID, -M, -estimation) %>%
#   ggplot(aes(x = ID, fill = as.factor(M))) +
#   geom_bar() +
#   facet_wrap(estimation~Time)
# 
# set.seed(0115)
# 
# df %>%
#   filter(M == 1, isRandom) %>%
#   group_by(estimation, nSchools) %>%
#   sample_n(20) %>%
#   select(ID, w1:w3, estimation, nSchools) %>%
#   gather(key = time, value = weight, w1:w3) %>%
#   ungroup() %>%
#   mutate(ID = as.factor(ID),
#          time = forcats::fct_rev(time)) %>%
#   ggplot(aes(x = ID, y = weight, fill = time)) +
#   geom_bar(stat = "identity") +
#   geom_hline(yintercept = c(1/3, 2/3, 1)) +
#   coord_flip() +
#   scale_fill_brewer(type = "qual", palette = 4, direction = -1) +
#   facet_wrap(nSchools ~ estimation, ncol = 2, scale = "free_y")




