library(R2MLwiN)
library(tidyverse)

options(MLwiN_path="C:/Program Files/MLwiN v2.36/")

# rm(list = ls())

#----------------------
# UTILITY FUNCTIONS
#----------------------

logit <- function(p) log(p/(1 - p))
expit <- function(x) exp(x) / (exp(x) + 1)
flip <- function(ps) rbinom(length(ps), 1, ps)
stand <- function(x) (x - mean(x)) / (sum((x - mean(x))^2) / length(x))
GMC <- function(x) x - mean(x)

#----------------------
# DATA GENERATION FUNCTIONS
#----------------------

# GENERATE WEIGHTS
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

# ASSIGN NEW SCHOOLS TO MOBILE STUDENTS
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

# REFORMAT DATA FOR MULTIPLE MEMBERSHIP ANALYSIS
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

# GENERATE LEVEL 1 DATA 
gen_L1 <- function(n = 1000, X_PS.cor = .5, X.m = 0, X.sd = 1, M.m = .25, e.m = 0, e.sd = 1, tripleRate = .375) {
  vars <- MASS::mvrnorm(n = n, mu = c(X.m, 0), Sigma = matrix(c(X.sd, X_PS.cor, X_PS.cor, 1), 2, 2), empirical = T)
  
  data.frame(X = vars[,1],
             p_M = pnorm(vars[,2])) %>%
    arrange(p_M) %>%
    mutate(rank = n():1,
           M = as.numeric(rank <= n() * M.m)) %>%
    select(X, p_M, M) %>%
    mutate(ID = 1:n(),
           e = rnorm(n(), e.m, e.sd),
           nSchools = (M + flip(M*tripleRate)) + 1)
}

gen_L2 <- function(n.schools, u0.m, u0.sd, df) {
  # Generate L2 random effects
  df_sch <- data.frame(SID = 1:n.schools) %>%
    mutate(u0 = rnorm(n.schools, u0.m, u0.sd))
  
  
  # Generate weighted L2 predictors 
  df_sch <- df %>% 
    select(S1:S3, X, M) %>%
    gather(key = Time, value = SID, S1:S3) %>%
    group_by(Time, SID) %>%
    summarise(M = mean(M),
              X = mean(X)) %>%
    ungroup() %>%
    left_join(df_sch)
  
  # Pull L2 variables from time 1 for each student's current school
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
  
  
  # Generate combined and weighted L2 variables
  df %>%
    mutate(wMj = (w1 * M.S1 + w2 * M.S2 + w3 * M.S3),
           wXj = (w1 * X.S1 + w2 * X.S2 + w3 * X.S3),
           w.u0 = (w1 * u0 + w2 * u0.S2 + w3 * u0.S3))
  
}


# GENERATE RESPONSES
gen_Y <- function(genMod, df) {

  # Generate responses given true weights are equal
  X_EQ <- t(model.matrix(genMod$frm, filter(df, isEqual, isRandom) %>% arrange(ID)))
  
  Y_EQ <- as.numeric(t(genMod$Bs %*% X_EQ))
  
  # Generate respoesns given true weights are unequal
  X_UQ <- t(model.matrix(genMod$frm, filter(df, !isEqual, isRandom) %>% arrange(ID)))
  
  Y_UQ <- as.numeric(t(genMod$Bs %*% X_UQ))
  
  data.frame(Y_EQ, Y_UQ) %>%
    mutate(ID = 1:n()) %>%
    left_join(df)
}

# DATA GENERATION DRIVER
gen_data <- function(n.students, X_PS.cor, X.m, X.sd, M.m, e.m, e.sd, tripleRate, n.schools, u0.m, u0.sd, genMod, gmc = T) {
  
  # Generate Level 1 Data
  df <- gen_L1(n.students, X_PS.cor, X.m, X.sd, M.m, e.m, e.sd, tripleRate)
  
  # Generate School Assignments
  df <- df %>%
    mutate(S1 = sample(1:n.schools, n.students, replace = T),
           S2 = moveSch(S1, M, nSchools >= 2),
           S3 = moveSch(S2, M, nSchools >= 3))
           
  # Generate Random Weights
  df_r_eq <- cbind(df, isRandom = T, isEqual = T, genWeights(df$nSchools))
  df_r_uq <- cbind(df, isRandom = T, isEqual = F, genWeights(df$nSchools, ws = c(1/6, 1/6, 4/6)))
  
  # Generate Fixed Weights (sd = 0)
  df_f_eq <- cbind(df, isRandom = F, isEqual = T, genWeights(df$nSchools, sd = 0))
  df_f_uq <- cbind(df, isRandom = F, isEqual = F, genWeights(df$nSchools, ws = c(1/6, 1/6, 4/6), sd = 0))
  
  df <- rbind(df_r_eq, df_r_uq, df_f_eq, df_f_uq) %>%
    mutate(estimation = paste(ifelse(isRandom, "Random", "Fixed"), ifelse(isEqual, "Equal", "Unequal"), sep = "_"))
  
  # Generate L2 Variables
  df <- gen_L2(n.schools, u0.m, u0.sd, df)
  
  # Grand Mean Centering
  if(gmc) {
    df <- df %>%
      mutate(M = GMC(M),
             X = GMC(X),
             wMj = GMC(wMj),
             wXj = GMC(wXj))
  }
  # Generate Responses
  df <- gen_Y(genMod, df)

  df <- toMM(df)
  
  return(df)
}

# gm <- list(frm = ~ 1 + X + M + wMj + wXj + w.u0 + e,
#      Bs = c(1, 1, 1, 1, 1, 1, 1))


# gen_data(n.students = 100,
#          X_PS.cor = .5,
#          X.m = 0,
#          X.sd = 1,
#          M.m = .25,
#          e.m = 0,
#          e.sd = 1,
#          tripleRate = .375,
#          n.schools = 10,
#          u0.m = 0,
#          u0.sd = 1,
#          genMod = gm) -> test


run_estimation <- function(df, frm) {
  
  runMM <- function(df) {
    # frm <- Y ~ 1 + X + M + wMj + wXj + (1 | mm1) + (1 | ID)
    
    mm <- list(list(mmvar = list("mm1", "mm2", "mm3"),
                    weights = list("ww1", "ww2", "ww3")),
               NA)
    
    runMLwiN(Formula = frm,
             data = df,
             estoptions = list(EstM = 1, drop.data = F, mm = mm))
  }
  
  df %>% 
    select(ID, isEqual, isRandom, mm1:ww3, Y_EQ, Y_UQ, X, M, wMj, wXj) %>%
    arrange(mm1, mm2, mm3) %>%
    gather(key = Gen, value = Y, Y_EQ:Y_UQ) %>%
    group_by(isEqual, isRandom, Gen) %>%
    nest() %>%
    mutate(model = map(data, runMM))

}

# test <- test %>%
#   run_estimation( Y ~ 1 + X + M + wMj + wXj + (1 | mm1) + (1 | ID))

getCoefs <- function(mdl) {
  estimates <- coef(mdl)
  # estimates <- c(coef(mdl), ICC = estimates["RP2_var_Intercept"]/(estimates["RP1_var_Intercept"] + estimates["RP2_var_Intercept"]))
  error <- c(sqrt(diag(mdl@FP.cov)), sqrt(diag(mdl@RP.cov)))
  df <- data.frame(stat = c("Estimate", "Error"), rbind(estimates, error))
  names(df) <- str_replace(names(df), "FP_", "FE_")
  names(df) <- str_replace(names(df), "RP2_var_Intercept", "RE_u0j")
  names(df) <- str_replace(names(df), "RP1_var_Intercept", "RE_ei")
  names(df) <- str_replace(names(df), "Intercept", "Int")
  
  return(df)
}

# test2 <- test %>%
#   mutate(coefs = map(model, getCoefs)) %>%
#   select(-data, -model) %>%
#   unnest(coefs)


run_sim <- function(n.students, X_PS.cor, X.m, X.sd, M.m, e.m, e.sd, tripleRate, n.schools, u0.m, u0.sd, gen_frm, Bs, est_frm) {
  
  genMod <- list(frm = gen_frm,
             Bs = Bs)
  
  df <- gen_data(n.students, X_PS.cor, X.m, X.sd, M.m, e.m, e.sd, tripleRate, n.schools, u0.m, u0.sd, genMod)
  
  mods <- df %>%
    run_estimation(est_frm)
  
  res <- mods %>%
    mutate(coefs = map(model, getCoefs)) %>%
    select(-data, -model) %>%
    unnest(coefs)
  
  
  return(res)
  
}

# run_sim(n.students = 100,
#          X_PS.cor = .5,
#          X.m = 0,
#          X.sd = 1,
#          M.m = .25,
#          e.m = 0,
#          e.sd = 1,
#          tripleRate = .375,
#          n.schools = 10,
#          u0.m = 0,
#          u0.sd = 1) -> test
