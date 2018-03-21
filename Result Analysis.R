library(tidyverse)

rm(list = ls())

# load("Results/results_real_rim.rdata")
# load("Results/results_clean_rim.rdata")
# load("Results/clean_RIM_100.rdata")
load("Results/clean_RIM_200.rdata")

results <- bind_rows(results, .id = "X_PS_cor") %>%
  as.tibble() %>%
  mutate(X_PS_cor = (as.numeric(X_PS_cor) - 1) / 10)


names(results) <- c("X_PS_cor", "Est_Equal", "Est_Random", "Gen", "Stat", 
                    "Int", "FE_X", "FE_M", "FE_Mj", "FE_Xj", "RE_e", "RE_u")

results <- results %>%
  select(Est_Equal:RE_u, X_PS_cor) %>%
  mutate(Est_Equal = ifelse(Est_Equal, "Equal", "Unequal"),
         Est_Random = ifelse(Est_Random, "Random", "Fixed"),
         Gen = ifelse(Gen == "Y_EQ", "Equal", "Unequal")) %>%
  gather(key = "Parameter", value = "Value", Int:RE_u) %>%
  mutate(Truth = ifelse(Parameter %in% c("RE_e", "RE_u"), 0, 1))
  

results %>%
  group_by(Est_Equal,
           Est_Random,
           Gen,
           Stat,
           X_PS_cor,
           Parameter) %>%
  filter(Stat == "Error") %>%
  arrange_all()


#Galindo and Smith analysis
# GSres <- 
  results %>%
  group_by(Est_Equal,
           Est_Random,
           Gen,
           Stat,
           X_PS_cor,
           Parameter) %>%
  summarise(m.par = mean(Value),
            Truth = sd(Value))
  
  summarise(m.par = mean(Value),
            Truth = ifelse(Stat == "Estimate", mean(Truth), sd(Value))) %>%
  mutate(RPB = (m.par - Truth)) %>%
  mutate(sig = abs(RPB) < .05)

GSres %>%
  filter(Stat == "Estimate") %>%
  ggplot(aes(x = X_PS_cor, y = RPB, color = Est_Equal, shape = Est_Random)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(.05, -.05), linetype = "dashed") +
  facet_grid(Gen ~ Parameter) +
  labs(title = "RPB")

GSres %>%
  filter(Stat == "Error") %>%
  ggplot(aes(x = X_PS_cor, y = RPB, color = Est_Equal, shape = Est_Random)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(.05, -.05), linetype = "dashed") +
  facet_grid(Gen ~ Parameter) +
  labs(title = "RPB")

#RMSE
RMSE <- results %>%
  filter(stat == "estimates") %>%
  group_by(params,
           Est,
           Gen,
           true) %>%
  summarise(RMSE = sqrt(sum((value - true)^2) / n())) %>%
  mutate(RRMSE = RMSE / true,
         sig = abs(RRMSE) < .2)

RMSE %>%
  ggplot(aes(x = Est, y = RRMSE, color = Gen)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = c(.2, -.2), linetype = "dashed") +
  facet_wrap(~ params, scales = "free_y") +
  labs(title = "RMSE")

