library(tidyverse)

rm(list = ls())

get_RPB <- function(X, T) {
  if(mean(T) == 0) return(mean(X))
  
  (mean(X) - mean(T)) / mean(T)
  
}

get_RMSE <- function(X, T) {
  sqrt(mean((X - T)^2))
}

pull_results <- function(cond, res.df) {
  out.i <- list()
  
  for(i in 1:max(cond$CID)) {
    # Read results
    res.cond.i <- bind_rows(lapply(res.df$dirs[res.df$cond == i], read_csv))
    
    # Get Estimates
    res.est.i <- res.cond.i %>%
      filter(stat == "Estimate") %>%
      select(X_PS.cor, M.m, u0.sd, Bs, ICC, isEqual:RE_ei) %>%
      gather(key = Parameter, value = value, FE_Int:RE_ei)
    
    res.err.i <- res.cond.i %>%
      filter(stat == "Error") %>%
      select(X_PS.cor, M.m, u0.sd, Bs, ICC, isEqual:RE_ei) %>%
      gather(key = Parameter, value = value, FE_Int:RE_ei)
    
    # Get true values
    # Formula %>% cond[i,"gen_frm"]
    true.est.i <- eval(parse(text = cond[i,"Bs"]))
    true.est.i[length(true.est.i) - 1] <- mean(res.cond.i$u0.sd)^2
    true.est.i <- tibble(stat = "Estimate", Parameter = res.cond.i %>% select(FE_Int:RE_ei) %>% names(), Truth = true.est.i)
    
    true.err.i <- res.est.i %>%
      filter(stat == "Estimate") %>%
      group_by(isRandom, isEqual, Gen, Parameter) %>%
      summarise(Truth = sd(value),
                stat = "Error")
    
    # Combine
    res.i <- rbind(full_join(res.err.i, true.err.i), full_join(res.est.i, true.est.i))
    
    # Calculate outcomes
    out.i[[i]] <- res.i %>%
      group_by(X_PS.cor, M.m, u0.sd, Bs, ICC, isEqual, isRandom, Gen, stat, Parameter) %>%
      summarise(RPB = get_RPB(value, Truth),
                RMSE = get_RMSE(value, Truth))
    
    
    # res.i %>%
    #   group_by(X_PS.cor, M.m, u0.sd, Bs, ICC, isEqual, isRandom, Gen, stat, Parameter) %>% 
    #   filter(stat == "Estimate",
    #          Parameter == "RE_u0j") %>%
    #   summarise(rpb = (mean(value) - mean(Truth))/mean(Truth),
    #             rpb2 = get_RPB(value, Truth))
    
  }
  
  return(out.i)
}

cond <- read_csv("Results/2018-07-18/conditions.csv")[,-1]

res.files <- list.files(path = "Results/2018-07-18")[str_detect(list.files(path = "Results/2018-07-18"), "Results")]
res.dirs <- paste("Results/2018-07-18/", res.files, sep = "")
res.df <- tibble(files = res.files, dirs = res.dirs) %>%
  mutate(cond = str_split(files, "-| ", simplify = T)[,1],
         part = str_split(files, "-| ", simplify = T)[,2])



cond.manipulated <- names((cond %>% mutate_all(as.factor) %>% sapply(function(x) length(levels(x))))[(cond %>% mutate_all(as.factor) %>% sapply(function(x) length(levels(x)))) > 1])

rm(list = "cond.manipulated", "res.dirs", "res.files")

out.i <- pull_results(cond, res.df)


save(out.i, file = "Results/Brief/out.rdata")
load("Results/Brief/out.rdata")

results <- bind_rows(out.i) %>%
  ungroup() %>%
  mutate(Est_Equal = ifelse(isEqual, "Equal", "Unequal"),
         Est_Random = ifelse(isRandom, "Random", "Fixed"),
         Gen = ifelse(Gen == "Y_EQ", "Equal", "Unequal"),
         Correct = (Est_Equal == Gen & Est_Random == "Random"),
         Correct = ifelse(Correct, T, NA),
         Parameter = factor(Parameter),
         Bs = factor(Bs))

levels(results$Parameter) <- c("FE_Int", "FE_M", "FE_X", "RE_e", "RE_u")
# levels(results$Bs) <- c("Weak L2", "Strong L2")


df_sum <- results %>% filter(stat == "Estimate")
# df_sum <- results %>% filter(stat == "Error")

df_sum %>%
  ggplot(aes(x = Gen, y = RPB, color = Est_Equal, linetype = Est_Random, fill = Correct)) +
  geom_boxplot() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(.05, -.05), linetype = "dashed") +
  labs(title = "RPB - All Conditions") +
  theme_bw() +
  scale_fill_brewer(type = "seq", palette = 1) +
  scale_color_brewer(type = "qual", palette = 3)


df_sum %>%
  ggplot(aes(x = Gen, y = RPB, color = Est_Equal, linetype = Est_Random, fill = Correct)) +
  geom_boxplot() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(.05, -.05), linetype = "dashed") +
  labs(title = "RPB - All Conditions by Parameter") +
  facet_wrap(~ Parameter, scales = "free_y") +
  theme_bw() +
  scale_fill_brewer(type = "seq", palette = 1) +
  scale_color_brewer(type = "qual", palette = 3)

df_sum2 <- df_sum %>% filter(Parameter %in% c("FE_M", "FE_X", "RE_e", "RE_u"))

df_sum2 %>%
  ggplot(aes(x = Gen, y = RPB, color = Est_Equal, linetype = Est_Random, fill = Correct)) +
  geom_boxplot() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(.05, -.05), linetype = "dashed") +
  labs(title = "RPB - X_M Cor by Parameter") +
  facet_grid(Parameter ~ X_PS.cor) +
  # facet_grid(Parameter ~ X_PS.cor) +
  theme_bw() +
  scale_fill_brewer(type = "seq", palette = 1) +
  scale_color_brewer(type = "qual", palette = 3)

df_sum2 %>%
  ggplot(aes(x = Gen, y = RPB, color = Est_Equal, linetype = Est_Random, fill = Correct)) +
  geom_boxplot() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(.05, -.05), linetype = "dashed") +
  labs(title = "RPB - %M by Parameter") +
  facet_wrap(Parameter ~ M.m , scales = "free_y", ncol = 4) +
  theme_bw() +
  scale_fill_brewer(type = "seq", palette = 1) +
  scale_color_brewer(type = "qual", palette = 3)

df_sum2 %>%
  ggplot(aes(x = Gen, y = RPB, color = Est_Equal, linetype = Est_Random, fill = Correct)) +
  geom_boxplot() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(.05, -.05), linetype = "dashed") +
  labs(title = "RPB - ICC by Parameter") +
  facet_wrap(Parameter ~ ICC, scales = "free_y", ncol = 4) +
  theme_bw() +
  scale_fill_brewer(type = "seq", palette = 1) +
  scale_color_brewer(type = "qual", palette = 3)



df_sum2 %>%
  ggplot(aes(x = Gen, y = RPB, color = Est_Equal, linetype = Est_Random, fill = Correct)) +
  geom_boxplot() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(.05, -.05), linetype = "dashed") +
  labs(title = "RPB - L2 effects by Parameter") +
  facet_wrap(Parameter ~ Bs, scales = "free_y", ncol = 4) +
  theme_bw() +
  scale_fill_brewer(type = "seq", palette = 1) +
  scale_color_brewer(type = "qual", palette = 3)

# Parameter-wise

df_sum %>%
  filter(Parameter == "FE_Mj") %>%
  ggplot(aes(x = Gen, y = RPB, color = Est_Equal, linetype = Est_Random, fill = Correct)) +
  geom_boxplot() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(.05, -.05), linetype = "dashed") +
  labs(title = "FE_Mj RPB") +
  facet_grid(M.m ~ X_PS.cor) +
  theme_bw() +
  scale_fill_brewer(type = "seq", palette = 1) +
  scale_color_brewer(type = "qual", palette = 3)

df_sum %>%
  filter(Parameter == "FE_Mj") %>%
  ggplot(aes(x = Gen, y = RPB, color = Est_Equal, linetype = Est_Random, fill = Correct)) +
  geom_boxplot() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(.05, -.05), linetype = "dashed") +
  labs(title = "FE_Mj RPB") +
  facet_grid(M.m ~ Bs) +
  theme_bw() +
  scale_fill_brewer(type = "seq", palette = 1) +
  scale_color_brewer(type = "qual", palette = 3)
