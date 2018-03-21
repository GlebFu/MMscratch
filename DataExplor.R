library(MASS)
library(parallel)

rm(list = ls())

source("SimSource.R")

genMod <- list(frm = ~ 1 + X + M + wMj + wXj + w.u0 + e,
               Bs = c(1, 1, 1, 1, 1, 1, 1))


orig <- gen_data(n.students = 3000,
               X_PS.cor = .25,
               X.m = 0,
               X.sd = 1,
               M.m = .25,
               e.m = 0,
               e.sd = 1,
               tripleRate = .375,
               n.schools = 100,
               u0.m = 0,
               u0.sd = 1, 
               genMod, 
               gmc = F) 

df <- orig %>%
  select(ID, estimation, X:M, nSchools: S3, w1:w3, wMj, wXj, Y_EQ, Y_UQ) %>%
  filter(estimation == "Fixed_Equal") %>%
  gather(key = "Gen", value = "Y", Y_EQ, Y_UQ)

df %>%
  ggplot(aes(x = X, y = Y, color = M)) +
  geom_point() +
  facet_wrap(~Gen)


df %>%
  ggplot(aes(x = X, y = Y, group = as.factor(S1))) +
  geom_line(stat = "smooth", method = lm, se = F, size = .1, alpha = .5) +
  facet_wrap(~Gen)

df %>%
  filter(Gen == "Y_EQ") %>%
  group_by(S1) %>%
  mutate(Mj = mean(M)) %>%
  ggplot(aes(x = X, y = Y, color = Mj, group = Mj)) +
  geom_point(size = .1, alpha = .5) +
  geom_line(stat = "smooth", method = "lm", se = F)

df %>%
  filter(Gen == "Y_EQ") %>%
  group_by(S1) %>%
  mutate(Mj = mean(M)) %>%
  ggplot(aes(x = X, y = Y, color = M, group = S1)) +
  geom_point(size = .1, alpha = .5) +
  geom_line(stat = "smooth", method = "lm", se = F)

