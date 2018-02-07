library(dplyr)
library(tidyr)
library(ggplot2)

rm(list = ls())

orig <- read.csv("data.csv")
orig[orig == " "] <- NA

df <- orig %>%
  transmute(post = C5R4MSCL,
            pre = C3R4MTHT,
            ID = CHILDID,
            S3 = S3_ID,
            S4 = S4_ID,
            S5 = S5_ID,
            T4 = T4_ID,
            T5 = T5_ID) %>%
  mutate(M = as.numeric(ifelse((S3 == S4) & (S3 == S5), F, T)),
         moves = M + ((S3 != S4) & (S3 != S5) & (S4 != S5)))

df_r <- na.omit(df)
# head(df_r)
# summary(df_r)
# 
# length(unique(df_r$S4))
# length(unique(df_r$T4))



runSamp <- function(data) {
  data %>%
    group_by(S4, T4) %>%
    sample_n(1) %>%
    ungroup() %>%
    summarise(n = n(),
              pMobile = mean(moves != 0),
              pTwice = mean(moves == 1),
              pThrice = mean(moves == 2),
              nMobile = sum(moves != 0),
              nTwice = sum(moves == 1),
              nThrice = sum(moves == 2)) %>%
    as.vector
    # return()
}

test <- runSamp(df_r)

replicate(2, runSamp(df_r), simplify = T)

df_r2 <- df_r %>%
  group_by(S4, T4) %>%
  sample_n(1)

df_r2 %>%
  ungroup() %>%
  summarise(n = n(),
            pMobile = mean(moves != 0),
            pTwice = mean(moves == 1),
            pThrice = mean(moves == 2),
            nMobile = sum(moves != 0),
            nTwice = sum(moves == 1),
            nThrice = sum(moves == 2))

characts <- df_r2 %>%
  group_by(S4) %>%
  summarise(n = n())

characts %>%
  ggplot(aes(x = n)) +
  geom_histogram()

table(characts$n)

test <- df %>%
  group_by(S4) %>%
  summarise(n = n()) %>%
  arrange(-n)

table(test$n)
