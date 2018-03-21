library(MASS)
library(tidyverse)

logit <- function(p) log(p/(1 - p))
expit <- function(x) exp(x) / (exp(x) + 1)
flip <- function(ps) rbinom(length(ps), 1, ps)
stand <- function(x) (x - mean(x)) / (sum((x - mean(x))^2) / length(x))
GMC <- function(x) x - mean(x)

# data.frame(x = rnorm(10^5)) %>% 
#   mutate(p = pnorm(x), b = qbinom(p, 1, .75)) %>% 
#   summarise(x.m = mean(x), m.m = mean(b), cor = cor(x,b))

test <- function(XPScor, X.m = 0, X.sd = 1, M.m = .5) {
  n <- 10^3
  
  vars <- mvrnorm(n = n, mu = c(X.m,logit(M.m)), Sigma = matrix(c(X.sd, XPScor, XPScor, 1), 2, 2), empirical = T)
  
  data.frame(x = vars[,1],
             p = pnorm(vars[,2]),
             ps = expit(vars[,2])) %>%
    mutate(m_ps = flip(ps),
           m_p = flip(p),
           m_b = qbinom(p, 1, M.m),
           m_b = qbinom(ps, 1, M.m)) %>% 
    arrange(ps) %>%
    mutate(rank = n():1,
           m_ps_2 = as.numeric(rank <= n() * M.m)) %>%
    arrange(p) %>%
    mutate(rank = n():1,
           m_p_2 = as.numeric(rank <= n() * M.m)) %>%
    dplyr::select(-rank)
}

results <- expand.grid(list(XPScor = 0:10/10, X.m = c(-1, 0, 1), M.m = c(.25, .5, .75))) %>%
  plyr::mdply(test)

results %>%
  filter(X.m == 0) %>%
  ggplot(aes(x = ps, y = x, color = m_ps)) +
  geom_point(alpha = .2) +
  facet_grid(M.m~XPScor)


results %>%
  filter(X.m == 0, M.m == .5, XPScor %in% c(0, .2, .5, .8, 1)) %>%
  gather(key = mtype, value = m, m_ps:m_p_2) %>%
  ggplot(aes(x = ps, y = x, color = m)) +
  coord_flip() + 
  geom_point(alpha = .2) +
  # facet_grid(mtype~XPScor)
  facet_grid(XPScor~mtype)

ggsave(file = "image2.jpeg")


results %>%
  gather(key = mtype, value = m, m_ps:m_p_2) %>%
  group_by(XPScor, X.m, M.m, mtype) %>%
  summarise(cor = cor(x, m)) %>%
  # filter(mtype %in% c("m_p_2", "m_ps_2")) %>%
  ggplot(aes(x = XPScor, y = cor, color = mtype, shape = as.factor(M.m))) +
  geom_line() +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  geom_abline(intercept = 0, slope = .5, linetype = "dashed") +
  facet_wrap(~X.m)

results %>%
  gather(key = mtype, value = m, m_ps:m_p_2) %>%
  group_by(XPScor, X.m, M.m, mtype) %>%
  summarise(cor = cor(x, m)) %>%
  # filter(mtype %in% c("m_p_2", "m_ps_2")) %>%
  ggplot(aes(x = XPScor, y = cor, color = mtype, shape = as.factor(X.m), linetype = as.factor(M.m))) +
  geom_line() +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) #+
  # facet_wrap(M.m~X.m)

results %>%
  filter(X.m == 0) %>%
  gather(key = par, value = value, p:ps) %>%
  ggplot(aes(x = value, fill = par)) +
  geom_density(alpha = .2) +
  geom_vline(aes(xintercept = M.m)) +
  facet_grid(M.m ~ XPScor)

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


