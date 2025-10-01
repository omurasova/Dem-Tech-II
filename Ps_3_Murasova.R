# Load required libraries
library(dplyr)
library(readr)
library(ggplot2)
library(here)
library(writexl)
library(HMDHFDplus)
setwd("~/University/Fall 2025/756 Dem Tech II")


datafile <- read_table("bltper_1x1.txt",
                 skip = 1)|>
  filter(Year == 2005)|>
  filter(Age %in% 16:31)|>
  select(year = Year,
         age = Age,
         nqx_die = qx)|>
  mutate(
    age = as.numeric(age),
    age = case_when(is.na(age) ~ 110,
                    TRUE ~ age),
    q_acc = pmax(0,0.062 - (0.000053*age^2)),
    q_all = nqx_die + q_acc,
    p_all = 1-q_all,
    lx =  c(85000, 85000 * cumprod(p_all))[1:length(p_all)],
    ndx_die = lx*nqx_die,
    ndx_acc = lx*q_acc
    )

write_xlsx(datafile,
           path = here::here ("data", "output_ps3.xlsx"))
