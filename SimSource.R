library(R2MLwiN)
library(tidyverse)

options(MLwiN_path="C:/Program Files/MLwiN v2.36/")

# rm(list = ls())

logit <- function(p) log(p/(1 - p))
expit <- function(x) exp(x) / (exp(x) + 1)
flip <- function(ps) rbinom(length(ps), 1, ps)
stand <- function(x) (x - mean(x)) / (sum((x - mean(x))^2) / length(x))
GMC <- function(x) x - mean(x)

genWeights <- function(nS, max_schs = max(nS), ws = rep(.5, max_schs), sd = 1) {

  w <- data.frame(matrix(0, length(nS), max_schs))
  names(w) <- paste("w", 1:max_schs, sep = "")
  
  w[nS==1, 1] <- 1

  w[nS == 2,] <- w[nS == 2,] %>%
    mutate(w1 = expit(rnorm(n = n(), m = logit(ws[1]), sd = sd)),
           w2 = 1 - w1)
  
  w[nS == 3, ] <- w[nS == 3, ] %>%
    mutate(w1 = expit(rnorm(n = n(), m = logit(ws[2]), sd = sd)),
           w2 = expit(rnorm(n = n(), m = logit(ws[2]), sd = sd)),
           w3 = expit(rnorm(n = n(), m = logit(ws[3]), sd = sd)),
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

gen_Mob_Coef <- function(X, X.M.cor, M.mean, range = c(-5, 5)) {
  b1 <- uniroot(function(b1) mean(X.M.cor - cor(flip(expit(b1 * X)), X)), range)
  b0 <- uniroot(function(b0) mean(expit(b0 + b1$root * X)) - M.mean,  range)
  
  Bs <- c(b0 = b0$root, b1 = b1$root)
  
  return(Bs)
}

toMM <- function(data) {
  ww <- data %>% 
    select(ID, w1:w3, estimation) %>%
    gather(key = ww, value = weight, w1:w3) %>%
    mutate(time = str_sub(ww, start = 2))
           
  ww <- mm <- data %>% 
    select(ID, S1:S3, estimation) %>%
    gather(key = mm, value = sid, S1:S3) %>%
    mutate(time = str_sub(mm, start = 2)) %>%
    left_join(ww) %>%
    group_by(ID, estimation, sid) %>%
    summarise(weight = sum(weight))
  
  ww <- ww %>%
    mutate(sid = paste("ww",1:n(), sep = "")) %>%
    spread(key = sid, value = weight)
  
  mm <- mm %>%
    mutate(weight = paste("mm",1:n(), sep = "")) %>%
    spread(key = weight, value = sid) %>%
    left_join(ww)
  
  mm[is.na(mm)] <- 0
  
  df <- left_join(mm, data) %>% ungroup
  
  return(df)
}

gen_data <- function(n.students, X.mean, X.sd, X.M.Bs, e.mean, e.sd, tripleRate, n.schools, RIM.Bs, u0.mean, u0.sd, ICC) {

  # Base Data
  df <- data.frame(ID = 1:n.students) %>%
    mutate(X = rnorm(n.students, X.mean, X.sd),
           X.stand = stand(X),
           PS.M = expit(X.M.Bs[1] + X.M.Bs[2] * X.stand),
           M = flip(PS.M),
           e = rnorm(n.students, e.mean, e.sd),
           nSchools = (M + flip(M*tripleRate)) + 1,
           S1 = sample(1:n.schools, n.students, replace = T),
           S2 = moveSch(S1, M, nSchools >= 2),
           S3 = moveSch(S2, M, nSchools >= 3))
  
  
  # Generate Weights
  df_r_eq <- cbind(df, isRandom = T, isEqual = T, genWeights(df$nSchools))
  df_r_uq <- cbind(df, isRandom = T, isEqual = F, genWeights(df$nSchools, ws = c(1/6, 1/6, 4/6)))
  df_f_eq <- cbind(df, isRandom = F, isEqual = T, genWeights(df$nSchools, sd = 0))
  df_f_uq <- cbind(df, isRandom = F, isEqual = F, genWeights(df$nSchools, ws = c(1/6, 1/6, 4/6), sd = 0))
  
  df <- rbind(df_r_eq, df_r_uq, df_f_eq, df_f_uq) %>%
    mutate(estimation = paste(ifelse(isRandom, "Random", "Fixed"), ifelse(isEqual, "Equal", "Unequal"), sep = "_"))
  

  # Generate L2 random effects
  df_sch <- data.frame(SID = 1:n.schools) %>%
    mutate(u0 = rnorm(n.schools, u0.mean, u0.sd))
  
  
  # Generate weighted L2 predictors 
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
  

  # Generate Y RIM
  RIM_form <- ~ 1 + X + M + wMj + wXj + w.u0 + e
  
  X_EQ <- t(model.matrix(RIM_form, filter(df, isEqual, isRandom) %>% arrange(ID)))
  
  Y_EQ <- as.numeric(t(RIM.Bs %*% X_EQ))
  
  X_UQ <- t(model.matrix(RIM_form, filter(df, !isEqual, isRandom) %>% arrange(ID)))
  
  Y_UQ <- as.numeric(t(RIM.Bs %*% X_UQ))
  
  df <- data.frame(Y_EQ, Y_UQ) %>%
    mutate(ID = 1:n()) %>%
    left_join(df)
  
  df <- toMM(df)
  
  return(df)
}

run_RIM <- function(data) {
  
  df_analysis <- data %>% 
    select(ID, isEqual, isRandom, mm1:ww3, Y_EQ, Y_UQ, X, M, wMj, wXj) %>%
    mutate(X = GMC(X),
           M = GMC(M),
           wMj = GMC(wMj),
           wXj = GMC(wXj)) %>%
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
  
  RIM_EQ_R_UQ <- runMLwiN(Formula = RIM_EQ_FRM,
                          data = filter(df_analysis, !isEqual, isRandom),
                          estoptions = list(EstM = 1, drop.data = F, mm = mm))
  
  RIM_EQ_F_UQ <- runMLwiN(Formula = RIM_EQ_FRM,
                          data = filter(df_analysis, !isEqual, !isRandom),
                          estoptions = list(EstM = 1, drop.data = F, mm = mm))
  
  RIM_UQ_FRM <- Y_UQ ~ 1 + X + M + wMj + wXj + (1 | mm1) + (1 | ID)
  
  RIM_UQ_R_EQ <- runMLwiN(Formula = RIM_UQ_FRM,
                          data = filter(df_analysis, isEqual, isRandom),
                          estoptions = list(EstM = 1, drop.data = F, mm = mm))
  
  RIM_UQ_R_UQ <- runMLwiN(Formula = RIM_UQ_FRM,
                          data = filter(df_analysis, !isEqual, isRandom),
                          estoptions = list(EstM = 1, drop.data = F, mm = mm))
  
  RIM_UQ_F_EQ <- runMLwiN(Formula = RIM_UQ_FRM,
                          data = filter(df_analysis, isEqual, !isRandom),
                          estoptions = list(EstM = 1, drop.data = F, mm = mm))
  
  RIM_UQ_F_UQ <- runMLwiN(Formula = RIM_UQ_FRM,
                          data = filter(df_analysis, !isEqual, !isRandom),
                          estoptions = list(EstM = 1, drop.data = F, mm = mm))
  
  mods <- list(RIM_EQ_R_EQ, 
               RIM_EQ_F_EQ, 
               RIM_EQ_R_UQ, 
               RIM_EQ_F_UQ, 
               RIM_UQ_R_EQ, 
               RIM_UQ_R_UQ, 
               RIM_UQ_F_EQ, 
               RIM_UQ_F_UQ)
  
  return(mods)
}



getCoefs <- function(mdl, RIM.Bs, u0.sd, e.sd, ICC) {
  estimates <- coef(mdl)
  estimates <- c(coef(mdl), ICC = estimates["RP2_var_Intercept"]/(estimates["RP1_var_Intercept"] + estimates["RP2_var_Intercept"]))
  error <- c(sqrt(diag(mdl@FP.cov)), sqrt(diag(mdl@RP.cov)), NA)
  pars <- c(RIM.Bs[1:5], u0 = u0.sd^2, e = e.sd^2, ICC = ICC)
  data.frame(params = names(pars), 
             true = pars, 
             estimates, 
             error,
             EstEqual = ifelse(mdl@data$isEqual[1], "EQ", "UQ"), 
             EstRandom = ifelse(mdl@data$isRandom[1], "R", "F"),
             Gen = ifelse(str_detect(as.character(mdl@call)[2], "EQ"), "EQ", "UQ"),
             stringsAsFactors = F,
             row.names = NULL)
}

run_sim <- function(n.students, X.mean, X.sd, X.M.Bs, e.mean, e.sd, tripleRate, n.schools, RIM.Bs, u0.mean, u0.sd, ICC) {
  df <- gen_data(n.students, X.mean, X.sd, X.M.Bs, e.mean, e.sd, tripleRate, n.schools, RIM.Bs, u0.mean, u0.sd, ICC)
  
  mods <- run_RIM(df)
  
  res <- bind_rows(lapply(mods, getCoefs, RIM.Bs, u0.sd, e.sd, ICC)) %>%
    mutate(Est = paste(EstEqual, EstRandom, sep = "_")) %>%
    gather(key = stat, value = value, estimates, error)
  
  return(res)
  
}

