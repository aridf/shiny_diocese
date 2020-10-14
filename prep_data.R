
library(tidyverse)
library(readxl)
library(visdat)
library(stringi)

data <- read_excel("data/USCCB.xlsx")
grp <- read_csv("data/groups.csv")
vars <- read_csv("data/vars.csv")

#vis_miss(ldata)

names(data) <- gsub("_child", "child", names(data))
names(data) <- stri_replace_last_fixed(names(data), "_", "-")
names(data) <- gsub("child", "_child", names(data))

ldata <- data %>% 
  pivot_longer(
    cols = -`diocese-num`,
    names_to = c(".value", "group_id"),
    names_sep = ("-")
  ) %>%
  rename("diocese" = "diocese-num") %>%
  mutate(yrsusa_tot = ifelse(str_detect(group_id, "child"), NA, yrsusa_tot),
         yrsusa_catp1 = ifelse(str_detect(group_id, "child"), NA, yrsusa_catp1),
         yrsusa_catp2 = ifelse(str_detect(group_id, "child"), NA, yrsusa_catp2),
         yrsusa_catp3 = ifelse(str_detect(group_id, "child"), NA, yrsusa_catp3),
         yrsusa_catp4 = ifelse(str_detect(group_id, "child"), NA, yrsusa_catp4),
         yrsusa_catp5 = ifelse(str_detect(group_id, "child"), NA, yrsusa_catp5))

ldata <- ldata %>%
  mutate(cit_catp4 = ifelse(str_detect(group_id, "child"), NA, cit_catp1),
         cit_catp1 = ifelse(str_detect(group_id, "child"), cit_catp1, NA))

saveRDS(ldata, "data/long_data.RDS")


