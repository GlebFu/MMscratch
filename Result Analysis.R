library(tidyverse)

rm(list = ls())

load("Data/results.rdata")

head(results)

sd <- results %>%
  filter(stat == "error") %>%
  group_by(vars,
           Est,
           Gen,
           true) %>%
  summarise(true.sd = mean(value))

#Galindo and Smith analysis
GSres <- results %>%
  filter(stat == "estimates") %>%
  group_by(vars,
           Est,
           Gen,
           true) %>%
  summarise(m.par = mean(value),
            sd.par = sd(value)) %>%
  left_join(sd) %>%
  mutate(RRPB = (m.par - true) / true,
         RPB = (m.par - true) / true,
         RSEB = (sd.par - true.sd) / true.sd) %>%
  select(vars:Gen, RRPB, RPB, RSEB) %>%
  gather(key = measure, value = value, RRPB, RPB, RSEB) %>%
  mutate(sig = abs(value) < .05)

GSres %>%
  filter(measure == "RPB") %>%
  ggplot(aes(x = Est, y = value, color = Est, alpha = sig)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(.05, -.05), linetype = "dashed") +
  facet_grid(Gen ~ vars) +
  scale_alpha_manual(values=c(1, .25), guide = F) + 
  labs(title = "RPB")

# GSres %>%
#   filter(measure == "RRPB") %>%
#   ggplot(aes(x = Est, y = value, color = Est, alpha = sig)) +
#   geom_point() +
#   geom_hline(yintercept = 0) +
#   geom_hline(yintercept = c(.05, -.05), linetype = "dashed") +
#   facet_grid(Gen ~ vars) +
#   scale_alpha_manual(values=c(1, .25), guide = F) + 
#   labs(title = "RRPB")

GSres %>%
  filter(measure == "RSEB") %>%
  na.omit() %>%
  ggplot(aes(x = Est, y = value, color = Est, alpha = sig)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(.05, -.05), linetype = "dashed") +
  facet_grid(Gen ~ vars) +
  scale_alpha_manual(values=c(1, .25), guide = F) + 
  labs(title = "RSEB")

results %>% filter(vars == "M", stat == "estimates") %>% group_by(Est, Gen, true) %>% summarise(m = mean(value)) %>% mutate(m = (m - true) / true)
results %>% filter(vars == "M.S", stat == "estimates") %>% group_by(Est, Gen, true) %>% summarise(m = mean(value)) %>% mutate(m = (m - true) / true)

results %>% filter(vars == "M", stat == "estimates") %>%
  ggplot(aes(x = Est, y = value)) +
  geom_boxplot() +
  geom_hline(yintercept = .1) +
  facet_wrap(~ Gen)

results %>% filter(vars == "M", stat == "estimates") %>%
  ggplot(aes(x = value, color = Est, xintercept = mean(value))) +
  geom_density() +
  geom_vline(xintercept = .1) +
  facet_wrap(~ Gen)


#RMSE
RMSE <- results %>%
  filter(stat == "estimates") %>%
  group_by(vars,
           Est,
           Gen,
           true) %>%
  summarise(RMSE = sqrt(sum((value - true)^2) / n())) %>%
  mutate(RRMSE = RMSE / true,
         sig = abs(RRMSE) < .2)

RMSE %>%
  ggplot(aes(x = Est, y = RRMSE, color = Est, alpha = sig)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = c(.2, -.2), linetype = "dashed") +
  facet_grid(Gen ~ vars) +
  scale_alpha_manual(values=c(1, .25), guide = F) + 
  labs(title = "RMSE")

