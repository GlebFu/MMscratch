rm(list = ls())

source("SimSource.R")

cond <- list(n.students = 3000,
             # X_PS.cor = c(0:9/10),
             X_PS.cor = .25,
             X.m = 0,
             X.sd = 1,
             M.m = c(0:9/10),
             e.m = 0,
             e.sd = 1,
             tripleRate = .375,
             n.schools = 100,
             u0.m = 0,
             u0.sd = c(.5, 1),
             gen_frm = list(~ 1 + X + M + w.u0 + e),
             Bs = list(c(0, 1, 1, 1, 1)),
             est_frm = list(Y ~ 1 + X + M + (1 | mm1) + (1 | ID))) %>%
  expand.grid() %>%
  mutate(ICC = u0.sd^2 / (u0.sd^2 + e.sd^2),
         CID = 1:n())


index = 1

genMod <- list(frm = ~ 1 + X + M + w.u0 + e,
               Bs = c(0, 1, 1, 1, 1))

seed <- runif(1,0,1)*10^8
set.seed(seed)

df <- gen_data(n.students = 3000, 
               X_PS.cor = .25, 
               X.m = 0, 
               X.sd = 1, 
               M.m = .25, 
               e.m = 0, 
               e.sd = 1, 
               tripleRate = .375, 
               n.schools = 100, 
               u0.m = 0, 
               u0.sd = .5, 
               genMod, 
               gmc = T, 
               w.sd = 0)

debug(gen_L2)
debug(gen_Y)

mods <- df %>%
  run_estimation(frm = Y ~ 1 + X + M + (1 | mm1) + (1 | ID))

df %>%
  filter(nSchools == 2) %>%
  ggplot(aes(x = ww1 - w1)) +
  geom_histogram() +
  facet_wrap(~ estimation)

res <- mods %>%
  mutate(coefs = map(model, getCoefs)) %>%
  dplyr::select(-data, -model) %>%
  unnest(coefs)

res %>%
  filter(stat == "Estimate") %>%
  gather(key = Var, value = val, FE_Int:RE_ei)

seed

