library(tidyverse)

rm(list = ls())

# load("Results/results_real_rim.rdata")
# load("Results/results_clean_rim.rdata")
load("Results/clean_RIM_100.rdata")

results <- bind_rows(results)

sd <- results %>%
  filter(stat == "error") %>%
  group_by(params,
           Est,
           Gen,
           true) %>%
  summarise(true.sd = mean(value))

#Galindo and Smith analysis
GSres <- results %>%
  filter(stat == "estimates") %>%
  group_by(params,
           Est,
           Gen,
           true) %>%
  summarise(m.par = mean(value),
            sd.par = sd(value)) %>%
  left_join(sd) %>%
  mutate(RPB = (m.par - true) / true,
         RSEB = (sd.par - true.sd) / true.sd) %>%
  select(params:Gen, RPB, RSEB) %>%
  gather(key = measure, value = value, RPB, RSEB) %>%
  mutate(sig = abs(value) < .05)

GSres %>%
  filter(measure == "RPB") %>%
  ggplot(aes(x = Est, y = value, color = Gen)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(.05, -.05), linetype = "dashed") +
  facet_wrap( ~ params, scales = "free_y") +
  # scale_alpha_manual(values=c(1, .25), guide = F) + 
  labs(title = "RPB")

# GSres %>%
#   filter(measure == "RPB") %>%
#   ggplot(aes(x = Gen, y = value, color = Est, shape = Est)) +
#   geom_point(size = 2) +
#   geom_hline(yintercept = 0) +
#   geom_hline(yintercept = c(.05, -.05), linetype = "dashed") +
#   facet_wrap( ~ params, scales = "free_y") +
#   scale_alpha_manual(values=c(1, .25), guide = F) + 
#   labs(title = "RPB")

GSres %>%
  filter(measure == "RSEB") %>%
  ggplot(aes(x = Est, y = value, color = Gen)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(.05, -.05), linetype = "dashed") +
  facet_wrap( ~ params, scales = "free_y") +
  # scale_alpha_manual(values=c(1, .25), guide = F) + 
  labs(title = "RSEB")

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

