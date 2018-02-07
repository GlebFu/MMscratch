library(R2MLwiN)
library(dplyr)

rm(list = ls())

options(MLwiN_path="C:/Program Files/MLwiN v2.36/")

# data("wage1")
# F8 = logearn ~ 1 + age_40 + numjobs + (1 | company) + (1 | id)
# OurMultiMemb <- list(list(mmvar = list("company", "company2", "company3","company4"),
#                           weights = list("weight1", "weight2", "weight3", "weight4")), 
#                      NA)
# 
# MMembModel <- runMLwiN(Formula = F8, 
#                         data = wage1, 
#                         estoptions = list(EstM = 1, drop.data = FALSE, mm = OurMultiMemb))

df <- read.csv("eqData SCL.csv")

df <- df %>% 
  mutate(Mob_gm = Mob - mean(Mob),
         PRE_gm = PRE - mean(PRE)) %>%
  arrange(mm1, mm2, mm3)


mm <- list(list(mmvar = list("mm1", "mm2", "mm3"),
                weights = list("w1", "w2", "w3")),
           NA)

RIM_Equal_formula <- POST ~ 1 + PRE_gm + Mob_gm + wPRE + wM + (1 | mm1) + (1 | CHILDID)
RIM_Equal <- runMLwiN(Formula = RIM_Equal_formula,
                      data = df,
                      estoptions = list(EstM = 1, drop.data = F, mm = mm))


RISM_Equal_formula <- POST ~ 1 + PRE_gm + Mob_gm + wPRE + wM + (1 + PRE_gm + Mob_gm| mm1) + (1 | CHILDID)

# RISM_Equal_IGLS <- runMLwiN(Formula = RISM_Equal_formula,
#                        data = df,
#                        estoptions = list(EstM = 0, drop.data = F, mm = mm, debugmode = F, Meth = 1))


RISM_Equal <- runMLwiN(Formula = RISM_Equal_formula,
                       data = df,
                       estoptions = list(EstM = 1, 
                                         drop.data = F, 
                                         mm = mm, 
                                         debugmode = F))


