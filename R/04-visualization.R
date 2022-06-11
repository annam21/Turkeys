# Data visualization 
# Anna Moeller 
# 11/10/2021

library(tidyverse)

# turdat <- readRDS("data/Eastern_tier1.rds") 
turdat <- readRDS("data/Eastern_tier1_withsymposium.rds") 

# Plot estimates by year
tst <- turdat %>% 
  mutate(vitalrate = replace(vitalrate, vitalrate == "survival", "annual survival")) %>% 
  count(vitalrate, plotyr) %>% 
  mutate(vitalrate = recode(
    vitalrate, 
    "annual survival" = "Annual survival",
    "clutch size" = "Clutch size",
    "hatching rate" = "Hatching rate",
    "nest success" = "Nest success",
    "nesting rate" = "Incubation initiation",
    "poult survival" = "Poult survival"
    ))
# tst$lab <- c(12,  2 , NA,  4,  NA,  2,  NA,  2 ,NA,   4,  2,  2,  2,  2,  3,  2,  2,  2,  2,  2,  2,  1 , 2,  4,  2 , 6,
#               1,  1 , 2,  2,  2,  3,  3,  1,  4,  4,  4,  2,  2,  2,  2,  3,  2,  3,  4,  3,  2,  5,  6,  6,  4,  1,
#                2  ,9 , 4  ,3,  4,  2,  2,  2)

xaxis <- seq(1970, 2020, by = 10)
ggplot(tst, aes(plotyr, n)) + 
  geom_col(aes(fill = vitalrate),  position = "dodge") + 
  # geom_text(aes(label = lab, y = n+0.7), size = 3) + 
  facet_wrap(~vitalrate) + 
  theme_classic() + 
  scale_x_continuous(breaks = xaxis, 
                     limits = c(1968, 2022)) + # extra room on both sides
  scale_y_continuous(breaks = seq(0,15, by = 2)) +
  theme(legend.position = "none") + 
  # xlab("Publication year") + 
  xlab("Study year") +
  ylab("Estimates used") 
# ggsave("results/vitalrate_year_withsymposium.jpg",
#        width = 8)




#####################################################################
# Table 1. vital rate distributions 
# res <- readRDS("results/Eastern_parameterestimates11122021.rds")
# Rmu <- res$BUGSoutput$median$R
# Rsd <- res$BUGSoutput$sd$R
# 
# bind_rows(
#   purrr::map_df(res$BUGSoutput$median, 1),
#   purrr::map_df(res$BUGSoutput$sd, 1),
# 
#   purrr::map_df(res$BUGSoutput$median, 2),
#   purrr::map_df(res$BUGSoutput$sd, 2),
#   
#   purrr::map_df(res$BUGSoutput$median, 3),
#   purrr::map_df(res$BUGSoutput$sd, 3),
#   
#   purrr::map_df(res$BUGSoutput$median, 4),
#   purrr::map_df(res$BUGSoutput$sd, 4),
# ) %>%
#   mutate(metric = rep(c('median', 'sd'), 4),
#          age = rep(c('subadult', 'subadult', 'adult', 'adult'), 2),
#          nestattempt = rep(1:2, each = 4) ) %>%
#   select(-PSD, -deviance) %>%
#   pivot_longer(cols = C:S, names_to = "vitalrate", values_to = "estimate") %>%
#   pivot_wider(names_from = metric, values_from = estimate) %>%
#   arrange(vitalrate, age, nestattempt) %>%
#   as.data.frame()%>%
#   #organization
#   select(vitalrate, age, nestattempt, median, sd) %>%
#   filter(!is.na(median))

