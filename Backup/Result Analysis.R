library(tidyverse)

rm(list = ls())

# load("Results/results_real_rim.rdata")
# load("Results/results_clean_rim.rdata")
# load("Results/clean_RIM_100.rdata")
# load("Results/clean_RIM_100_GMC.rdata")
load("Results/clean_RIM_100_24Cond.rdata")

cond$ID <- as.character(1:nrow(cond))

results <- bind_rows(results, .id = "ID") %>%
  as.tibble() %>%
  left_join(cond) %>%
  select(-ID, -reps)


names(results) <- c("Est_Equal", "Est_Random", "Gen", "Stat", 
                    "Int", "FE_X", "FE_M", "FE_Mj", "FE_Xj", "RE_u", "RE_e", 
                    names(results)[12:26])

names(select(results, Int:RE_e))

trueBs <- as.tibble(do.call(rbind, results$Bs))

names(trueBs) <- paste("true_", names(select(results, Int:RE_e)), sep = "")

results <- cbind(results, trueBs)

fixBs <- function(x) as.factor(apply(do.call(rbind, x), 1, sum))

results$Bs <- fixBs(results$Bs)

# results <- results %>%
#   group_by(Est_Equal, Est_Random, Gen, Stat, X_PS.cor, M.m, ICC, Bs) %>%
#   sample_n(size = 10) %>%
#   ungroup()

# Get Estimates
df <- results %>%
  filter(Stat == "Estimate") %>%
  select(Est_Equal:Gen, X_PS.cor, M.m, ICC, Bs, Int:RE_e) %>%
  mutate(Est_Equal = ifelse(Est_Equal, "Equal", "Unequal"),
         Est_Random = ifelse(Est_Random, "Random", "Fixed"),
         Gen = ifelse(Gen == "Y_EQ", "Equal", "Unequal")) %>%
  gather(key = "Parameter", value = "Estimate", Int:RE_e)


# Get True Values
df <- results %>%
  filter(Stat == "Estimate") %>%
  select(Est_Equal:Gen, X_PS.cor, M.m, ICC, Bs, e.sd, u0.sd, true_Int:true_FE_Xj) %>%
  mutate(true_RE_e = e.sd^2,
         true_RE_u = u0.sd^2, 
         Est_Equal = ifelse(Est_Equal, "Equal", "Unequal"),
         Est_Random = ifelse(Est_Random, "Random", "Fixed"),
         Gen = ifelse(Gen == "Y_EQ", "Equal", "Unequal")) %>%
  gather(key = "Parameter", value = "True", true_Int:true_RE_u) %>%
  mutate(Parameter = str_replace(Parameter, "true_", "")) %>%
  full_join(df)
  

levels(df$Bs) <- c("Weak L2", "Strong L2")

df <- df %>% mutate(bias = Estimate - True,
                    RPB = bias,
                    ICC = round(ICC, 2))

df$RPB[df$True != 0] <- (df$bias[df$True != 0]  / df$True[df$True != 0] )


df_sum <- df %>%
  group_by(Est_Equal, Est_Random, Gen, X_PS.cor, M.m, ICC, Bs, Parameter) %>%
  summarise(RPB = mean(RPB)) %>%
  mutate(Correct = (Est_Equal == Gen & Est_Random == "Random"),
         Correct = ifelse(Correct, T, NA))
  
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

df_sum2 <- df_sum %>% filter(Parameter %in% c("FE_Mj", "FE_Xj", "RE_e", "RE_u"))

df_sum2 %>%
  ggplot(aes(x = Gen, y = RPB, color = Est_Equal, linetype = Est_Random, fill = Correct)) +
  geom_boxplot() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(.05, -.05), linetype = "dashed") +
  labs(title = "RPB - X_M Cor by Parameter") +
  facet_wrap(Parameter ~ X_PS.cor, scales = "free_y", ncol = 3) +
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
  facet_grid(ICC + X_PS.cor ~ M.m + Bs) +
  theme_bw() +
  scale_fill_brewer(type = "seq", palette = 1) +
  scale_color_brewer(type = "qual", palette = 3)
