library(tidyverse)

rm(list = ls())

# load("Results/results_real_rim.rdata")
# load("Results/results_clean_rim.rdata")
load("Results/clean_RIM_100.rdata")
# load("Results/clean_RIM_200.rdata")

results <- bind_rows(results) %>%
  as.tibble()


names(results) <- c("Est_Equal", "Est_Random", "Gen", "Stat", 
                    "Int", "FE_X", "FE_M", "FE_Mj", "FE_Xj", "RE_e", "RE_u",
                    names(results)[12:22])

results <- results %>%
  select(Est_Equal:RE_u, X_PS.cor) %>%
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
           X_PS.cor,
           Parameter) %>%
  filter(Stat == "Error") %>%
  arrange_all()


#Galindo and Smith analysis
GSres <- results %>%
  group_by(Est_Equal,
           Est_Random,
           Gen,
           Stat,
           X_PS.cor,
           Parameter) %>%
    summarise(m.par = mean(Value),
              Truth = ifelse(mean(Stat == "Estimate") == 1, mean(Truth), sd(Value))) %>%
  mutate(RPB = (m.par - Truth)) %>%
  mutate(sig = abs(RPB) < .05)

GSres %>%
  filter(Stat == "Estimate") %>%
  ggplot(aes(x = X_PS.cor, y = RPB, color = Est_Random, shape = Est_Equal)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(.05, -.05), linetype = "dashed") +
  facet_grid(Gen ~ Parameter) +
  labs(title = "RPB - Estimates")

GSres %>%
  filter(Stat == "Error") %>%
  ggplot(aes(x = X_PS.cor, y = RPB, color = Est_Random, shape = Est_Equal)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(.05, -.05), linetype = "dashed") +
  facet_grid(Gen ~ Parameter) +
  labs(title = "RPB - Standard Error")

#RMSE
RMSE <- results %>%
  group_by(Est_Equal,
           Est_Random,
           Gen,
           Stat,
           X_PS.cor,
           Parameter) %>%
  mutate(Truth = ifelse(mean(Stat == "Estimate") == 1, mean(Truth), sd(Value))) %>%
  summarise(RMSE = sqrt(sum(Value - Truth)^2 / n()))
  
  
RMSE %>%
  filter(Stat == "Estimate") %>%
  ggplot(aes(x = X_PS.cor, y = RMSE, color = Est_Random, shape = Est_Equal)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = c(.2, -.2), linetype = "dashed") +
  facet_grid(Gen ~ Parameter) +
  labs(title = "RMSE - Estimate")

RMSE %>%
  filter(Stat == "Error") %>%
  ggplot(aes(x = X_PS.cor, y = RMSE, color = Est_Random, shape = Est_Equal)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = c(.2, -.2), linetype = "dashed") +
  facet_grid(Gen ~ Parameter) +
  labs(title = "RMSE - Error")

####

test <- results %>%
  group_by(Est_Equal,
           Est_Random,
           Gen,
           Stat,
           X_PS.cor,
           Parameter) %>%
  mutate(Truth = ifelse(mean(Stat == "Estimate") == 1, mean(Truth), sd(Value)),
         Value = Value - Truth)

test %>%
  ggplot(aes(x = Gen, y = Value, color = paste(Est_Random, Est_Equal))) +
  geom_boxplot() +
  facet_grid(Stat ~ Parameter) +
  labs(title = "RMSE - Error")

test %>%
  filter(Stat == "Estimate") %>%
  ggplot(aes(x = Gen, y = abs(Value), color = paste(Est_Random, Est_Equal))) +
  geom_boxplot() +
  facet_grid(~Parameter) +
  labs(title = "RMSE - Error")
  