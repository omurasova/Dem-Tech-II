# Load required libraries
library(dplyr)
library(readr)
library(ggplot2)
library(here)
library(writexl)
setwd("~/University/Fall 2025/756 Dem Tech II")

# Load data
ps1_data_F2023 <- read_csv("University/Fall 2025/756 Dem Tech II/ps1_data_F2023.csv")

here:: i_am("University/Fall 2025/756 Dem Tech II/Ps_1_Murasova.R")
ps1_data_F2023 <- ps1_data_F2023 %>%
    mutate(n = lead(x) - x,
            nMx = nDx / nNx,
            qx = n* nMx / (1 + (n-nax) * nMx),
            qx  = if_else(is.na(n), 1, qx),
            px = 1 - qx,
            lx = c(100000, 100000 * cumprod(px))[1:length(px)],
            dx = lx * qx,
            Lx = if_else(
              is.na(n),
              lx / last(nMx), 
              n*lead(lx, default = 0) + nax * dx
              ),
           Tx  = rev(cumsum(rev(Lx))),
           ex  = Tx / lx
           )

write_xlsx(ps1_data_F2023, path = here::here ("University/Fall 2025/756 Dem Tech II/data", "output_ps1.xlsx"))

# lx

a<- ggplot(ps1_data_F2023, aes(x = x, y = lx)) +
  +   geom_line(color = "blue", linewidth = 1.2) +
  +   labs(title = "Life table function lx", x = "Age (x)", y = "lx") +
  +   theme_minimal()

ggsave(plot = a, 
      filename = here::here("University/Fall 2025/756 Dem Tech II/graphs", "graph_ps1_a.jpeg"),
      device = "jpeg")


# ndx
b <- ggplot(ps1_data_F2023, aes(x = x, y = dx)) +
  geom_line(color = "red", linewidth = 1.2) +
  labs(title = "Number of deaths (ndx)", x = "Age (x)", y = "ndx") +
  theme_minimal()

ggsave(plot = b, 
       filename = here::here("University/Fall 2025/756 Dem Tech II/graphs", "graph_ps1_b.jpeg"),
       device = "jpeg")

# nMx
c <- ggplot(ps1_data_F2023, aes(x = x, y = nMx)) +
  geom_line(color = "darkgreen", linewidth = 1.2) +
  geom_point(color = "darkgreen") +
  labs(title = "Age-specific death rate (nMx)", x = "Age (x)", y = "nMx") +
  theme_minimal()

ggsave(plot = c, 
       filename = here::here("University/Fall 2025/756 Dem Tech II/graphs", "graph_ps1_c.jpeg"),
       device = "jpeg")
