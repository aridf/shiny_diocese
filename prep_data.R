
library(tidyverse)
library(readxl)
library(stringi)

data <- read_excel("data/USCCB.xlsx")
grp <- read_csv("data/groups.csv")
vars <- read_csv("data/vars.csv")

names(data) <- gsub("_child", "child", names(data))
names(data) <- stri_replace_last_fixed(names(data), "_", "-")
names(data) <- gsub("child", "_child", names(data))

ldata <- data %>% 
  pivot_longer(
    cols = -`diocese-num`,
    names_to = c(".value", "group_id"),
    names_sep = ("-")
  ) %>% 
  mutate(across(everything(), ~replace_na(.x, -1))) %>%
  rename("diocese" = "diocese-num")

saveRDS(ldata, "data/long_data.RDS")


