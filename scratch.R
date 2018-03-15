library(MASS)
library(tidyverse)

logit <- function(p) log(p/(1 - p))
expit <- function(x) exp(x) / (exp(x) + 1)
flip <- function(ps) rbinom(length(ps), 1, ps)
stand <- function(x) (x - mean(x)) / (sum((x - mean(x))^2) / length(x))
GMC <- function(x) x - mean(x)


test <- function(XPScor, X.m = 0, X.sd = 1, M.m = .5) {
  n <- 10^3
  
  vars <- mvrnorm(n = n, mu = c(X.m,logit(M.m)), Sigma = matrix(c(X.sd, XPScor, XPScor, 1), 2, 2), empirical = T)
  
  x <- vars[,1]
  p <- pnorm(vars[,2])
  ps <- expit(vars[,2])
  m <- flip(ps)
  data.frame(x, p, ps, m)
}

results <- expand.grid(list(XPScor = 0:10/10, X.m = c(-1, 0, 1), M.m = c(.25, .5, .75))) %>%
  plyr::mdply(test)

results %>%
  filter(X.m == 0) %>%
  ggplot(aes(x = ps, y = x, color = m)) +
  geom_point(alpha = .5) +
  facet_grid(M.m~XPScor)

results %>%
  group_by(XPScor, X.m, M.m) %>%
  summarise_all(mean) %>%
  gather(key = newVar, value = value, p:m) %>%
  ggplot(aes(x = XPScor, y = value, color = newVar)) +
  geom_point() +
  geom_line() +
  geom_hline(aes(yintercept = M.m)) +
  facet_grid(M.m ~ X.m)

library(modelr)

mod <- function(df) {
  lm(m ~ x, df)
}

revert <- results %>%
  group_by(XPScor, X.m, M.m) %>%
  nest() %>%
  mutate(model = map(data, mod),
         value = map(model, coef)) %>%
  unnest(value) %>%
  mutate(coef = rep(c("b0", "b1"), n()/2)) %>%
  spread(key = coef, value = value)

revert

test2 <- function(XPScor, X.m = 0, X.sd = 1, M.m = .5, b0, b1) {
  n <- 10^3
  
  x <- rnorm(n, X.m, X.sd)
  ps <- expit(b0 + b1 * x)
  m <- flip(ps)
  
  data.frame(x, ps, m)
}

results2 <- revert %>%
  plyr::mdply(test2)

# revert[1,] %>%
#   plyr::mdply(test2) %>%
#   summarise_all(mean)

results2 %>%
  filter(X.m == 0) %>%
  ggplot(aes(x = ps, y = x, color = m)) +
  geom_point(alpha = .5) +
  facet_grid(M.m~XPScor)

results2 %>%
  group_by(XPScor, X.m, M.m) %>%
  summarise_all(mean) %>%
  gather(key = newVar, value = value, p:m) %>%
  ggplot(aes(x = XPScor, y = value, color = newVar)) +
  geom_point() +
  geom_line() +
  geom_hline(aes(yintercept = M.m)) +
  facet_grid(M.m ~ X.m)
