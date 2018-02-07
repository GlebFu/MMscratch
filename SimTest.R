library(stringr)

rm(list = ls())

source("SimSource.R")

ecls <- read.csv("eqData SCL.csv")
cor(ecls$Mob, ecls$PRE)

# Conditions
tripRate <- .375
n.sch <- 100
n.std <- 3000
X.mean <- 0
X.sd <- 1
X <- rnorm(100000, X.mean, X.sd)
mobRate <- .25
# X.b1 <- -1.83


gen_Mob_Coef <- funciton()
X.b1 <- uniroot(function(b1) mean(-.05 - cor(flip(expit(b1 * X)), X)), c(-5, 5))

cor(flip(expit(X.b1$root * X)), X)

Bs <- c(uniroot(function(X.b0) mean(expit(X.b0 + X.b1$root * X)) - mobRate, 
                c(-200,200))[[1]], 
        X.b1$root)


# Generate Data

df <- data.frame(ID = 1:n.std) %>%
  mutate(X = rnorm(n.std, X.mean, X.sd),
         PS.M = expit(Bs[1] + Bs[2] * X),
         M = rbinom(n.std, 1, PS.M),
         nSchools = (M + rbinom(n.std, 1, M*tripRate)) + 1,
         S1 = sample(1:n.sch, n.std, replace = T),
         S2 = moveSch(S1, M, nSchools >= 2),
         S3 = moveSch(S2, M, nSchools >= 3))

# undebug(moveSch)

# Generate Weights

df_eq <- cbind(df, w_type = "Equal", genWeights(df$nSchools))
df_uq <- cbind(df, w_type = "Unequal", genWeights(df$nSchools, ws = c(1/4, 1/6, 4/6)))

df <- rbind(df_eq, df_uq)

head(df)


df_sch <- df %>% 
  select(S1:S3, X, M) %>%
  gather(key = Time, value = SID, S1:S3) %>%
  group_by(Time, SID) %>%
  summarise(M = mean(M),
            X = mean(X)) %>%
  ungroup()

df <- df %>%
  left_join(filter(df_sch, Time == "S1") %>% select(-Time),
            suffix = c("", ".S1"),
            by = c("S1" = "SID")) %>%
  left_join(filter(df_sch, Time == "S2") %>% select(-Time),
            suffix = c("", ".S2"),
            by = c("S2" = "SID")) %>%
  left_join(filter(df_sch, Time == "S3") %>% select(-Time),
            suffix = c("", ".S3"),
            by = c("S3" = "SID"))

df %>%
  mutate(M.S = (w1 * M.S1 + w2 * M.S2 + w3 * M.S3),
         X.S = (w1 * X.S1 + w2 * X.S2 + w3 * X.S3))

head(df, 10)

df %>%
  select(ID, w_type, S1:S3, w1:w3) %>%
  gather(key = "Var.Time", value = "value", S1:S3, w1:w3) %>%
  mutate(var = str_sub(Var.Time, end = 1),
       time = paste("t", str_sub(Var.Time, start = 2), sep = "")) %>%
  select(-Var.Time) %>%
  spread(key = var, value = value) %>%
  group_by(ID, w_type, S) %>%
  mutate(w2 = sum(w))

df %>%
  select(nSchools, w1:w3, w_type) %>%
  gather(key = time, value = weight, -nSchools, -w_type) %>%
  filter(weight != 0) %>%
  ggplot(aes(x = weight, color = time)) +
  geom_density() +
  facet_grid(w_type~nSchools)

df %>%
  select(w_type, nSchools, w1:w3) %>%
  gather(key = time, value = weight, -nSchools, -w_type) %>%
  filter(weight != 0) %>%
  group_by(w_type, nSchools, time) %>%
  summarise(m = mean(weight),
            s = sd(weight),
            n = n())


df %>%
  select(M, S1, S2, S3, w_type) %>%
  gather(key = Time, value = ID, -M, -w_type) %>%
  ggplot(aes(x = ID, fill = as.factor(M))) +
  geom_bar() +
  facet_wrap(w_type~Time)

set.seed(0115)

df %>%
  filter(M == 1) %>%
  group_by(w_type) %>%
  sample_n(20) %>% 
  select(ID, w1:w3, w_type) %>%
  gather(key = time, value = weight, w1:w3) %>%
  ungroup() %>%
  mutate(ID = as.factor(ID),
         time = forcats::fct_rev(time)) %>%
  ggplot(aes(x = ID, y = weight, fill = time)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = c(1/3, 2/3, 1)) +
  coord_flip() +
  scale_fill_brewer(type = "qual", palette = 4, direction = -1) +
  facet_wrap(~ w_type, ncol = 1, scale = "free_y")

filter(df, M == 1) %>% head(30)



