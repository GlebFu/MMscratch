library(dplyr)
library(tidyr)
library(ggplot2)
library(plot3D)

rm(list = ls())

logit <- function(p) log(p/(1 - p))
expit <- function(x) exp(x) / (exp(x) + 1)
#----------------
# GALINDO 2015
#----------------
n = 100000

#----------------
# Random Equal - p66
#----------------

# Two schools - Original

df <- data.frame(ID = 1:n, method = "Orig") %>%
  mutate(w1 = runif(n = n(), min = 0, max = 1),
         w2 = 1 - w1)

# Two schools - Gleb

df <- data.frame(ID = 1:n, method = "Gleb") %>%
  mutate(w1 = expit(rnorm(n = n())),
         w2 = 1 - w1) %>%
  rbind(df)

df %>%
  gather(key = weight, value = value, -ID, - method) %>%
  ggplot(aes(x = value, fill = weight)) +
  geom_density(alpha = .5) +
  facet_wrap(~method)

df %>%
  gather(key = weight, value = value, -ID, -method) %>%
  group_by(method, weight) %>%
  summarise(m = mean(value),
            sd = sd(value))

# Three Schools

df <- data.frame(ID = 1:n, method = "Orig") %>%
  mutate(w1 = runif(n = n(), min = 0, max = 1),
         w2 = runif(n = n(), min = 0, max = 1),
         w3 = runif(n = n(), min = 0, max = 1),
         sum = w1 + w2 + w3) %>%
  transmute(ID = ID,
            w1 = w1/sum,
            w2 = w2/sum,
            w3 = w3/sum,
            method = method)


# Three Schools - Gleb

df <- data.frame(ID = 1:n, method = "Gleb") %>%
  mutate(w1 = expit(rnorm(n = n())),
         w2 = expit(rnorm(n = n())),
         w3 = expit(rnorm(n = n())),
         sum = w1 + w2 + w3) %>%
  transmute(ID = ID,
            w1 = w1/sum,
            w2 = w2/sum,
            w3 = w3/sum,
            method = method) %>%
  rbind(df)

df %>%
  gather(key = weight, value = value, -ID, -method) %>%
  ggplot(aes(x = value, fill = weight)) +
  geom_density(alpha = .5) +
  facet_wrap(~method)

df %>%
  gather(key = weight, value = value, -ID, -method) %>%
  group_by(method, weight) %>%
  summarise(m = mean(value),
            sd = sd(value))
#----------------
# Random unequal - p66
#----------------

# Two schools

df <- data.frame(ID = 1:n, method = "Orig") %>%
  mutate(w1 = runif(n = n(), min = 0, max = 1/3),
         w2 = 1 - w1)


# Two schools - Gleb

df <- data.frame(ID = 1:n, method = "Gleb") %>%
  mutate(w1 = expit(rnorm(n = n(), m = logit(.25))),
         w2 = 1 - w1) %>%
  rbind(df)


df %>%
  gather(key = weight, value = value, -ID, -method) %>%
  ggplot(aes(x = value, fill = weight)) +
  geom_density(alpha = .5) +
  facet_wrap(~method)

df %>%
  gather(key = weight, value = value, -ID, -method) %>%
  group_by(method, weight) %>%
  summarise(m = mean(value),
            sd = sd(value))

# Three Schools

df <- data.frame(ID = 1:n, method = "Orig") %>%
  mutate(w1 = runif(n = n(), min = 0, max = 1),
         w2 = runif(n = n(), min = 0, max = 1),
         w3 = runif(n = n(), min = 0, max = 4),
         sum = w1 + w2 + w3) %>%
  transmute(ID = ID,
            w1 = w1/sum,
            w2 = w2/sum,
            w3 = w3/sum,
            method = method)

# Three Schools - Gleb

df <- data.frame(ID = 1:n, method = "Gleb") %>%
  mutate(w1 = expit(rnorm(n = n(), m = logit(1/6))),
         w2 = expit(rnorm(n = n(), m = logit(1/6))),
         w3 = expit(rnorm(n = n(), m = logit(4/6))),
         sum = w1 + w2 + w3) %>%
  transmute(ID = ID,
            w1 = w1/sum,
            w2 = w2/sum,
            w3 = w3/sum,
            method = method) %>%
  rbind(df)

df %>%
  gather(key = weight, value = value, -ID, -method) %>%
  ggplot(aes(x = value, fill = weight)) +
  geom_density(alpha = .5) +
  facet_wrap(~method)

df %>%
  gather(key = weight, value = value, -ID, -method) %>%
  group_by(method, weight) %>%
  summarise(m = mean(value),
            sd = sd(value))
