library(dplyr)
library(tidyr)
library(ggplot2)

rm(list = ls())

logit <- function(p) log(p/(1 - p))
expit <- function(x) exp(x) / (exp(x) + 1)
#----------------
# GALINDO 2015
#----------------
n = 10000

#----------------
# Random Equal - p66
#----------------

# Two schools - Original

EQ2 <- data.frame(ID = 1:n, Equal = "Equal", method = "Orig") %>%
  mutate(w1 = runif(n = n(), min = 0, max = 1),
         w2 = 1 - w1)

# Two schools - Gleb

EQ2 <- data.frame(ID = 1:n, Equal = "Equal", method = "Gleb") %>%
  mutate(w1 = expit(rnorm(n = n())),
         w2 = 1 - w1) %>%
  rbind(EQ2)

EQ2 %>%
  gather(key = weight, value = value, -ID, -Equal , -method) %>%
  ggplot(aes(x = value, fill = weight)) +
  geom_density(alpha = .5) +
  facet_wrap(~method)

EQ2 %>%
  gather(key = weight, value = value, -ID, -Equal , -method) %>%
  group_by(method, weight) %>%
  summarise(m = mean(value),
            sd = sd(value))

# Three Schools

EQ3 <- data.frame(ID = 1:n, Equal = "Equal", method = "Orig") %>%
  mutate(w1 = runif(n = n(), min = 0, max = 1),
         w2 = runif(n = n(), min = 0, max = 1),
         w3 = runif(n = n(), min = 0, max = 1),
         sum = w1 + w2 + w3) %>%
  transmute(ID = ID,
            w1 = w1/sum,
            w2 = w2/sum,
            w3 = w3/sum,
            method = method,
            Equal = Equal)


# Three Schools - Gleb

EQ3 <- data.frame(ID = 1:n, Equal = "Equal", method = "Gleb") %>%
  mutate(w1 = expit(rnorm(n = n())),
         w2 = expit(rnorm(n = n())),
         w3 = expit(rnorm(n = n())),
         sum = w1 + w2 + w3) %>%
  transmute(ID = ID,
            w1 = w1/sum,
            w2 = w2/sum,
            w3 = w3/sum,
            method = method,
            Equal = Equal) %>%
  rbind(EQ3)

EQ3 %>%
  gather(key = weight, value = value, -ID, -Equal , -method) %>%
  ggplot(aes(x = value, fill = weight)) +
  geom_density(alpha = .5) +
  facet_wrap(~method)

EQ3 %>%
  gather(key = weight, value = value, -ID, -Equal , -method) %>%
  group_by(method, weight) %>%
  summarise(m = mean(value),
            sd = sd(value))
#----------------
# Random unequal - p66
#----------------

# Two schools

UN2 <- data.frame(ID = 1:n, Equal = "Unequal", method = "Orig") %>%
  mutate(w1 = runif(n = n(), min = 0, max = 1/3),
         w2 = 1 - w1)


# Two schools - Gleb

UN2 <- data.frame(ID = 1:n, Equal = "Unequal", method = "Gleb") %>%
  mutate(w1 = expit(rnorm(n = n(), m = logit(.25))),
         w2 = 1 - w1) %>%
  rbind(UN2)


UN2 %>%
  gather(key = weight, value = value, -ID, -Equal , -method) %>%
  ggplot(aes(x = value, fill = weight)) +
  geom_density(alpha = .5) +
  facet_wrap(~method)

UN2 %>%
  gather(key = weight, value = value, -ID, -Equal , -method) %>%
  group_by(method, weight) %>%
  summarise(m = mean(value),
            sd = sd(value))

# Three Schools

UN3 <- data.frame(ID = 1:n, Equal = "Unequal", method = "Orig") %>%
  mutate(w1 = runif(n = n(), min = 0, max = 1),
         w2 = runif(n = n(), min = 0, max = 1),
         w3 = runif(n = n(), min = 0, max = 4),
         sum = w1 + w2 + w3) %>%
  transmute(ID = ID,
            w1 = w1/sum,
            w2 = w2/sum,
            w3 = w3/sum,
            method = method,
            Equal = Equal)

# Three Schools - Gleb

UN3 <- data.frame(ID = 1:n, Equal = "Unequal", method = "Gleb") %>%
  mutate(w1 = expit(rnorm(n = n(), m = logit(1/6))),
         w2 = expit(rnorm(n = n(), m = logit(1/6))),
         w3 = expit(rnorm(n = n(), m = logit(4/6))),
         sum = w1 + w2 + w3) %>%
  transmute(ID = ID,
            w1 = w1/sum,
            w2 = w2/sum,
            w3 = w3/sum,
            method = method,
            Equal = Equal) %>%
  rbind(UN3)

UN3 %>%
  gather(key = weight, value = value, -ID, -Equal , -method) %>%
  ggplot(aes(x = value, fill = weight)) +
  geom_density(alpha = .5) +
  facet_wrap(~method)

UN3 %>%
  gather(key = weight, value = value, -ID, -Equal , -method) %>%
  group_by(method, weight) %>%
  summarise(m = mean(value),
            sd = sd(value))

EQ2$w3 <- 0
EQ2$s <- 2

UN2$w3 <- 0
UN2$s <- 2

EQ3$s <- 3

UN3$s <- 3

df <- rbind(EQ2, EQ3, UN2, UN3) %>%
  as.tibble

df %>%
  gather(key = time, value = weight, w1:w3) %>%
  ggplot(aes(x = weight, color = Equal)) +
  geom_density() +
  facet_grid(s + method ~ time)
