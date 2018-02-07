library(R2MLwiN)
library(dplyr)

rm(list = ls())

options(MLwiN_path="C:/Program Files/MLwiN v2.36/")

df <- read.csv("eqData SCL.csv")

df <- df %>% 
  mutate(Mob_gm = Mob - mean(Mob),
         PRE_gm = PRE - mean(PRE)) %>%
  arrange(mm1)


# POST - Continuous outcome measure
# PRE_gm - Continous pretest measure (grand mean centered)
# Mob - Binary mobility indicator (grand mean centered)
# wPRE - Weighted sum of school level pretest
# WM - weighted sum of school level mobility
# mm1:3 - multiple membership ID year 1:3
# w1 - multiple membership weight year 1:3

mm <- list(list(mmvar = list("mm1", "mm2", "mm3"),
                weights = list("w1", "w2", "w3")),
           NA)

RISM_Equal_formula <- POST ~ 1 + PRE_gm + Mob_gm + wPRE + wM + (1 + PRE_gm  + Mob_gm | mm1) + (1 | CHILDID)

RISM_Equal <- runMLwiN(Formula = RISM_Equal_formula,
                       data = df,
                       estoptions = list(EstM = 1, 
                                         drop.data = F, 
                                         mm = mm, 
                                         debugmode = F))


