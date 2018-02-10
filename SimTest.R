library(stringr)
library(R2MLwiN)
options(MLwiN_path="C:/Program Files/MLwiN v2.36/")

rm(list = ls())

source("SimSource.R")

ecls <- read.csv("eqData SCL.csv")
cor(ecls$Mob, ecls$PRE)

# Conditions
n.sch <- 100
n.std <- 3000
X.mean <- 60
X.sd <- 10
mobRate <- .25
tripRate <- .375
X.M.cor <- -.05

e.mean <- 0
e.sd <- sqrt(240)

# ICC <- .15

u0.mean <- 0
# u0.sd <- sqrt((e.sd^2 * ICC) / (1 - ICC))
u0.sd <- sqrt(46)
ICC <- u0.sd^2 / (e.sd^2 + u0.sd^2)

# X.M.Bs <- round(gen_Mob_Coef(stand(rnorm(100000, X.mean, X.sd)), X.M.cor, mobRate), 2)
X.M.Bs <- round(gen_Mob_Coef(stand(rnorm(100000, X.mean, X.sd)), X.M.cor, mobRate, range = c(-1, 1) * 200), 2)

# stand <- function(x) (x - mean(x)) / (sum((x - mean(x))^2) / length(x))
# 
# data.frame(x = rnorm(100000, X.mean, X.sd)) %>%
#   mutate(ps = expit(stand(x)))
# 
# data.frame(x = rnorm(100000, X.mean, X.sd),
#            e = rnorm(100000)) %>%
#   mutate(y1 = stand(x + 1 + e),
#          y2 = x + 1 + e) %>%
#   cor


RIM.Bs <- c(B0 = 125, X = 1, M = .1, M.S = -2, X.S = .2, u0 = 1, e = 1)

#----------------------
# Generate Data
#----------------------

df <- data.frame(ID = 1:n.std) %>%
  mutate(X = rnorm(n.std, X.mean, X.sd),
         X.stand = stand(X),
         PS.M = expit(X.M.Bs[1] + X.M.Bs[2] * X.stand),
         M = flip(PS.M),
         e = rnorm(n.std, e.mean, e.sd),
         nSchools = (M + flip(M*tripRate)) + 1,
         S1 = sample(1:n.sch, n.std, replace = T),
         S2 = moveSch(S1, M, nSchools >= 2),
         S3 = moveSch(S2, M, nSchools >= 3))

# cor(cbind(df$X, df$X.stand, df$M))
#----------------------
# Generate Weights
#----------------------

df_r_eq <- cbind(df, isRandom = T, isEqual = T, genWeights(df$nSchools))
df_r_uq <- cbind(df, isRandom = T, isEqual = F, genWeights(df$nSchools, ws = c(1/6, 1/6, 4/6)))
df_f_eq <- cbind(df, isRandom = F, isEqual = T, genWeights(df$nSchools, sd = 0))
df_f_uq <- cbind(df, isRandom = F, isEqual = F, genWeights(df$nSchools, ws = c(1/6, 1/6, 4/6), sd = 0))

df <- rbind(df_r_eq, df_r_uq, df_f_eq, df_f_uq) %>%
  mutate(estimation = paste(ifelse(isRandom, "Random", "Fixed"), ifelse(isEqual, "Equal", "Unequal"), sep = "_"))

#----------------------
# Generate L2 random effects
#----------------------

df_sch <- data.frame(SID = 1:n.sch) %>%
  mutate(u0 = rnorm(n.sch, u0.mean, u0.sd))

#----------------------
# Generate weighted L2 predictors
#----------------------

df_sch <- df %>% 
  select(S1:S3, X, M) %>%
  gather(key = Time, value = SID, S1:S3) %>%
  group_by(Time, SID) %>%
  summarise(M = mean(M),
            X = mean(X)) %>%
  ungroup() %>%
  left_join(df_sch)

df <- df %>%
  left_join(filter(df_sch, Time == "S1") %>% select(-Time),
            suffix = c("", ".S1"),
            by = c("S1" = "SID")) %>%
  left_join(filter(df_sch, Time == "S1") %>% select(-Time),
            suffix = c("", ".S2"),
            by = c("S2" = "SID")) %>%
  left_join(filter(df_sch, Time == "S1") %>% select(-Time),
            suffix = c("", ".S3"),
            by = c("S3" = "SID"))

df <- df %>%
  mutate(wMj = (w1 * M.S1 + w2 * M.S2 + w3 * M.S3),
         wXj = (w1 * X.S1 + w2 * X.S2 + w3 * X.S3),
         w.u0 = (w1 * u0 + w2 * u0.S2 + w3 * u0.S3))

# df %>% 
#   filter(M == 1) %>%
#   group_by(w_type, nSchools) %>%
#   sample(10) %>% 
#   select(ID, w_type, M, S1:S3, w1:w3, M.S1, M.S2, M.S3, wMj)

#----------------------
# Generate Y
#----------------------
RIM_form <- ~ 1 + X + M + wMj + wXj + w.u0 + e

X_EQ <- t(model.matrix(RIM_form, filter(df, isEqual, isRandom) %>% arrange(ID)))

Y_EQ <- as.numeric(t(RIM.Bs %*% X_EQ))

X_UQ <- t(model.matrix(RIM_form, filter(df, !isEqual, isRandom) %>% arrange(ID)))

Y_UQ <- as.numeric(t(RIM.Bs %*% X_UQ))

df <- data.frame(Y_EQ, Y_UQ) %>%
  mutate(ID = 1:n()) %>%
  left_join(df)

# df %>%
#   select(Y_EQ, Y_UQ, ID, X, M, estimation, wMj, wXj) %>%
#   gather(key = y_type, value = Y, Y_EQ, Y_UQ) %>%
#   ggplot(aes(x = X, y = Y, color = M)) +
#   geom_point(alpha = .5, size = .5) +
#   facet_wrap(~ estimation)

#----------------------
# Test Analysis
#----------------------

head(df)

toMM <- function(s_w) {
  s_w <- as.numeric(s_w)
  l <- length(s_w)
  s <- s_w[1:(l*.5)]
  w <- s_w[(l*.5 + 1):l]
  
  n_s <- length(unique(s))
  
  mm <- c(unique(s), rep(0, length(s) - length(unique(s))))
  ww <- rep(0, length(s))
  i <- 1
  
  for(i in 1:length(s)) ww[i] <- sum(w[s %in% mm[i]])
  
  mms <- data.frame(t(c(mm, ww)))
  names(mms) <- c(paste("mm", 1:(l*.5), sep = ""), paste("ww", 1:(l*.5), sep = ""))
  
  
  
  
  return(mms)
}

df <- df %>%
  select(S1:S3, w1:w3) %>%
  rowwise() %>%
  do(toMM(.)) %>%
  cbind(df)

head(df)


df_analysis <- df %>% 
  select(ID, isEqual, isRandom, mm1:ww3, Y_EQ, Y_UQ, X, M, wMj, wXj) %>%
  arrange(mm1, mm2, mm3)


mm <- list(list(mmvar = list("mm1", "mm2", "mm3"),
                weights = list("ww1", "ww2", "ww3")),
           NA)

RIM_EQ_FRM <- Y_EQ ~ 1 + X + M + wMj + wXj + (1 | mm1) + (1 | ID)

RIM_EQ_R_EQ <- runMLwiN(Formula = RIM_EQ_FRM,
                        data = filter(df_analysis, isEqual, isRandom),
                        estoptions = list(EstM = 1, drop.data = F, mm = mm))

RIM_EQ_F_EQ <- runMLwiN(Formula = RIM_EQ_FRM,
                        data = filter(df_analysis, isEqual, !isRandom),
                        estoptions = list(EstM = 1, drop.data = F, mm = mm))

RIM_EQ_F_UQ <- runMLwiN(Formula = RIM_EQ_FRM,
                        data = filter(df_analysis, !isEqual, !isRandom),
                        estoptions = list(EstM = 1, drop.data = F, mm = mm))

RIM_UQ_FRM <- Y_UQ ~ 1 + X + M + wMj + wXj + (1 | mm1) + (1 | ID)

RIM_UQ_R_UQ <- runMLwiN(Formula = RIM_UQ_FRM,
                        data = filter(df_analysis, !isEqual, isRandom),
                        estoptions = list(EstM = 1, drop.data = F, mm = mm))

RIM_UQ_F_EQ <- runMLwiN(Formula = RIM_UQ_FRM,
                        data = filter(df_analysis, isEqual, !isRandom),
                        estoptions = list(EstM = 1, drop.data = F, mm = mm))

RIM_UQ_F_UQ <- runMLwiN(Formula = RIM_UQ_FRM,
                        data = filter(df_analysis, !isEqual, !isRandom),
                        estoptions = list(EstM = 1, drop.data = F, mm = mm))



getCoefs <- function(mdl) {
  estimates <- coef(mdl)
  estimates <- c(coef(mdl), ICC = estimates["RP2_var_Intercept"]/(estimates["RP1_var_Intercept"] + estimates["RP2_var_Intercept"]))
  pars <- c(RIM.Bs[1:5], u0 = u0.sd^2, e = e.sd^2, ICC = ICC)
  data.frame(vars = names(pars), 
             true = pars, 
             estimates, 
             EstEqual = ifelse(mdl@data$isEqual[1], "EQ", "UQ"), 
             EstRandom = ifelse(mdl@data$isRandom[1], "R", "F"),
             Gen = ifelse(str_detect(as.character(mdl@call)[2], "EQ"), "EQ", "UQ"),
             stringsAsFactors = F,
             row.names = NULL)
}


mods <- list(RIM_EQ_R_EQ, RIM_EQ_F_EQ, RIM_EQ_F_UQ, RIM_UQ_R_UQ, RIM_UQ_F_EQ, RIM_UQ_F_UQ)

res <- bind_rows(lapply(mods, getCoefs)) %>%
  mutate(Est = paste(EstEqual, EstRandom, sep = "_"))

res %>%
  ggplot(aes(x = vars, y = estimates, color = Est)) +
  geom_point() +
  geom_point(aes(y = true), shape = "x") + 
  facet_wrap(~ Gen)

res %>%
  ggplot(aes(x = Gen, y = estimates, color = Est)) +
  geom_point() +
  geom_hline(aes(yintercept = true)) + 
  geom_hline(aes(yintercept = true + true * c(-.1, .1)), linetype = "dashed") +
  facet_wrap(~ vars, scales = "free_y")

#----------------------
# Explore Data
#----------------------

df %>%
  select(ID, estimation, S1:S3, w1:w3) %>%
  gather(key = "Var.Time", value = "value", S1:S3, w1:w3) %>%
  mutate(var = str_sub(Var.Time, end = 1),
       time = paste("t", str_sub(Var.Time, start = 2), sep = "")) %>%
  select(-Var.Time) %>%
  spread(key = var, value = value) %>%
  group_by(ID, estimation, S) %>%
  mutate(w2 = sum(w))

df %>%
  select(nSchools, w1:w3, estimation) %>%
  gather(key = time, value = weight, -nSchools, -estimation) %>%
  filter(weight != 0) %>%
  ggplot(aes(x = weight, color = time)) +
  geom_density() +
  facet_grid(estimation~nSchools)

df %>%
  select(estimation, nSchools, w1:w3) %>%
  gather(key = time, value = weight, -nSchools, -estimation) %>%
  filter(weight != 0) %>%
  group_by(estimation, nSchools, time) %>%
  summarise(m = mean(weight),
            s = sd(weight),
            n = n())


df %>%
  select(M, S1, S2, S3, estimation) %>%
  filter(estimation == "Fixed_Equal") %>%
  gather(key = Time, value = ID, -M, -estimation) %>%
  ggplot(aes(x = ID, fill = as.factor(M))) +
  geom_bar() +
  facet_wrap(estimation~Time)

set.seed(0115)

df %>%
  filter(M == 1, isRandom) %>%
  group_by(estimation, nSchools) %>%
  sample_n(20) %>%
  select(ID, w1:w3, estimation, nSchools) %>%
  gather(key = time, value = weight, w1:w3) %>%
  ungroup() %>%
  mutate(ID = as.factor(ID),
         time = forcats::fct_rev(time)) %>%
  ggplot(aes(x = ID, y = weight, fill = time)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = c(1/3, 2/3, 1)) +
  coord_flip() +
  scale_fill_brewer(type = "qual", palette = 4, direction = -1) +
  facet_wrap(nSchools ~ estimation, ncol = 2, scale = "free_y")




