library(MASS)

rm(list = ls())

source("SimSource.R")

get_b1 <- function(X.mean, X.sd, X.M.cor, sds = 5) {
  if(X.M.cor == 0) return(0)
  x <- rnorm(10^5, X.mean, X.sd)
  b1 <- uniroot(function(b1) cor(x, flip(expit(x * b1))) - X.M.cor, interval = X.mean + c(-1,1) * X.sd * sds)
  return(round(b1$root, 3))
}

get_b0 <- function(X.mean, X.sd, mobRate, b1, sds = 5) {
  x <- rnorm(10^5, X.mean, X.sd)
  b0 <- uniroot(function(b0) mobRate - mean(flip(expit(b0 + x * b1))), interval = X.mean + c(-1,1) * X.sd * sds)
  return(round(b0$root, 3))
}
#----------------------
# Conditions - Real
#----------------------
cond <- list(n.schools = 1000,
             n.students = 30000,
             mobRate = .25,
             tripleRate = .375,
             X.mean = 0,
             X.sd = 1,
             X.M.cor = (0:6)/10,
             e.mean = 0,
             e.sd = sqrt(1),
             ICC = .5,
             u0.mean = 0) %>%
  expand.grid() %>%
  mutate(u0.sd = sqrt((e.sd^2 * ICC) / (1 - ICC))) %>%
  group_by(drop = 1:n()) %>%
  mutate(b1 = get_b1(X.mean, X.sd, X.M.cor),
         b0 = get_b0(X.mean, X.sd, mobRate, b1)) %>%
  ungroup() %>%
  select(-drop)

cond


gen_temp <- function(conds) {
  
  # Base Data
  df <- data.frame(ID = 1:conds$n.students) %>%
    mutate(X = rnorm(conds$n.students, conds$X.mean, conds$X.sd),
           X.stand = stand(X),
           PS.M = expit(conds$b0 + conds$b1 * X.stand),
           M = flip(PS.M))
  return(cbind(conds, df))
}


test <- cond %>%
  select(n.schools, n.students, X.mean, X.sd, b1, b0) %>%
  rowwise %>% 
  do( X = as_data_frame(.) ) %>% 
  ungroup

results <- test %>%
  mutate(result = map(X, gen_temp)) %>%
  select(-X) %>%
  unnest

results %>%
  group_by(b1,b0) %>%
  summarise(cor(X, M))

