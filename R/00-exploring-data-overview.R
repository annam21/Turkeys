# Connecting the two excel sheets 
library(tidyverse)
tur_raw <- read_csv("data/ranking data.csv") %>% 
  select(Strata:`Usefulness tier`)
tur_over <- read_csv("data/turkey demographics paper overview.csv") %>%
  select(ID, Strata, "Publication Year", Years, Subspecies, Season)

tmp <- left_join(tur_raw, tur_over, by = "Strata")
tmp %>%
  arrange(Year) %>%
  select(Strata, Year, Years) %>%
  filter(!is.na(Years), !is.na(Year))
# # 39 don't have years in either. 
# # 30 others don't have year in overview but do in the data 
# # 422 others have year in overview but not in data
# # 414 have years in both

# # Which papers are missing? 
tur_raw <- read_csv("data/ranking data.csv") %>%
  rename(IDdemog= ID) %>%
  select(Strata, IDdemog)
tur_over <- read_csv("data/turkey demographics paper overview.csv") %>%
  rename(IDover = ID) %>%
  select(Strata, IDover)
anti_join(tur_raw, tur_over, by = "Strata")
anti_join(tur_over, tur_raw, by = "Strata") # 4 papers
tur_raw %>%
  filter(Strata == 28)

# Eyeball to make sure they're really the same
left_join(tur_raw, tur_over, by = c("Strata")) %>%
  mutate(chk = IDdemog == IDover) %>%
  filter(chk == FALSE) %>%
  arrange(Strata) %>%
  View
left_join(tur_over, tur_raw, by = "Strata") %>%
  mutate(chk = IDdemog == IDover) %>%
  filter(chk == FALSE) %>%
  arrange(Strata) %>%
  View

# # Break up the year ranges #### This is in 01 now
# # Single range
# oner <- tur_over %>%
#   filter(grepl("-", .$Years), !grepl(";", .$Years)) %>%
#   separate(Years, into = c("begin_yr", "end_yr"), sep = "-", remove = F) 
# # Where there's multiple ranges, pick 1st and last year
# mr <- tur_over %>%
#   filter(grepl(";", .$Years), grepl("-", .$Years)) %>%
#   separate(Years, into = c("firstint", "secint"), sep = ";", remove = F) %>% 
#   separate(firstint, into = c("begin_yr", "tmp"), sep = "-") %>% 
#   separate(secint, into = c("tmp", "end_yr"), sep = "-") %>% 
#   select(-tmp) 
# # Ignore the ranges with years separated by ;
# # Put them back together 
# tur_yrs <- tur_over %>% 
#   filter(!grepl("-", .$Years)) %>% 
#   bind_rows(., oner, mr) 
# 
# # Find the column that only has a single year, not a range, and keep that one.
# # No single years in overview
# # tmp %>% 
# #   mutate(gyover = !grepl("-", .$Years) & !grepl(";", .$Years)& !is.na(Years)) %>%
# #   filter(gyover) 
# left_join(tur_raw, tur_yrs, by = "Strata") %>% 
#   # Where there's a good single year in vital rates or overview
#   mutate(
#     gyvr = !grepl("-", .$Year) & !is.na(Year),
#     gyover = !is.na(begin_yr),
#     plotyr = case_when(
#       gyvr == T ~ Year,
#       gyvr == F & gyover == T ~ begin_yr # otherwise NA
#     )
#   ) %>%
#   select(-gyvr, -gyover, -Years)




