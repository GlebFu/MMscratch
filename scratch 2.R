library(tidyverse)

rm(list = ls())

n <- 1000

df <- data.frame(x = rnorm(n))



cond <- list(b1 = c(.25, .5, 1, 2, 4),
             e.sd = c(.25, .5, 1, 2, 4)) %>%
  expand.grid() %>%
  mutate(ID = 1:n())

df <- replicate(nrow(cond), df, simplify = FALSE) %>%
  bind_rows(.id = "ID") %>%
  mutate(ID = as.numeric(ID)) %>%
  left_join(cond) %>%
  as.tibble %>%
  mutate(y = b1 * x + rnorm(n(), sd = e.sd))

cors <- df %>%
  group_by(b1, e.sd) %>%
  summarise(X_Y = round(cor(x, y),2),
            x = mean(x),
            y = 15)

df %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(alpha = .25, size = .25) +
  geom_text(data = cors, aes(label = X_Y), color = "black") +
  facet_grid(e.sd ~ b1)

ggsave(file = "Generating Correlations.jpg")
