library(dplyr)
library(tidyr)
library(ggplot2)

rm(list = ls())

logit <- function(p) log(p/(1 - p))
expit <- function(x) exp(x) / (exp(x) + 1)
flip <- function(ps) rbinom(length(ps), 1, ps)

genWeights <- function(nS, max_schs = max(nS), ws = rep(.5, max_schs)) {

  w <- data.frame(matrix(0, length(nS), max_schs))
  names(w) <- paste("w", 1:max_schs, sep = "")
  
  w[nS==1, 1] <- 1

  w[nS == 2,] <- w[nS == 2,] %>%
    mutate(w1 = expit(rnorm(n = n(), m = logit(ws[1]))),
           w2 = 1 - w1)
  
  w[nS == 3, ] <- w[nS == 3, ] %>%
    mutate(w1 = expit(rnorm(n = n(), m = logit(ws[2]))),
           w2 = expit(rnorm(n = n(), m = logit(ws[2]))),
           w3 = expit(rnorm(n = n(), m = logit(ws[3]))),
           sum = w1 + w2 + w3) %>%
    transmute(w1 = w1/sum,
              w2 = w2/sum,
              w3 = w3/sum)
  
  return(w)
}

moveSch <- function(prior.school, M, isMoving) {
  sProbs <- data.frame(prior.school, M) %>%
    group_by(prior.school) %>%
    summarise(mPerc = mean(M)) %>%
    mutate(prob = mPerc/sum(mPerc))
  
  sampSch <- function(curSch) {
    sample(x = sProbs$prior.school[!(sProbs$prior.school %in% curSch)], 
           size = 1, 
           prob = sProbs$prob[!(sProbs$prior.school %in% curSch)], 
           replace = T)
  }
  
  prior.school[isMoving] <- sapply(prior.school[isMoving], sampSch)

  
  return(prior.school)
  
}

