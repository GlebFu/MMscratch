# toMM <- function(s_w) {
#   s_w <- as.numeric(s_w)
#   l <- length(s_w)
#   s <- s_w[1:(l*.5)]
#   w <- s_w[(l*.5 + 1):l]
#   
#   n_s <- length(unique(s))
#   
#   mm <- c(unique(s), rep(0, length(s) - length(unique(s))))
#   ww <- rep(0, length(s))
#   i <- 1
#   
#   for(i in 1:length(s)) ww[i] <- sum(w[s %in% mm[i]])
#   
#   mms <- data.frame(t(c(mm, ww)))
#   names(mms) <- c(paste("mm", 1:(l*.5), sep = ""), paste("ww", 1:(l*.5), sep = ""))
#   
#   return(mms)
# }
# 
# dfa <- test %>%
#   select(S1:S3, w1:w3) %>%
#   rowwise() %>%
#   do(toMM(.)) %>%
#   cbind(test)
# 
# dfb <- test %>% left_join(mm)
# 
# head(select(dfa, ID, mm1:ww3))
# head(select(dfb, ID, mm1:ww3))
# 
# 
# dfa_analysis <- dfa %>% 
#   select(ID, isEqual, isRandom, mm1:ww3, Y_EQ, Y_UQ, X, M, wMj, wXj) %>%
#   arrange(mm1, mm2, mm3)
# 
# dfb_analysis <- dfb %>% 
#   select(ID, isEqual, isRandom, mm1:ww3, Y_EQ, Y_UQ, X, M, wMj, wXj) %>%
#   arrange(mm1, mm2, mm3)
# 
# mm <- list(list(mmvar = list("mm1", "mm2", "mm3"),
#                 weights = list("ww1", "ww2", "ww3")),
#            NA)
# 
# RIM_EQ_FRM <- Y_EQ ~ 1 + X + M + wMj + wXj + (1 | mm1) + (1 | ID)
# 
# t1 <- runMLwiN(Formula = RIM_EQ_FRM,
#                         data = filter(dfa_analysis, isEqual, isRandom),
#                         estoptions = list(EstM = 1, drop.data = F, mm = mm))
# 
# t2 <- runMLwiN(Formula = RIM_EQ_FRM,
#                         data = filter(dfb_analysis, isEqual, isRandom),
#                         estoptions = list(EstM = 1, drop.data = F, mm = mm))
# 
# t1 == t2