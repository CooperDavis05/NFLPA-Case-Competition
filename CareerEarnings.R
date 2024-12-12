library(purrr)
library(future)


contracts2 <- load_contracts(file_type = getOption("nflreadr.prefer", default = "rds"))

for(x in 1:nrow(contracts2)) {
  contracts2$career_earnings[x] = ifelse(is_empty(contracts2$cols[[x]]), 1, 0)
}   #### GETS RID OF ALL THE NULLS (WHICH IS ROOKIES TOO)

contracts2_new <- contracts2 |> filter(career_earnings == 0) |> select(-career_earnings) |>  filter(year_signed != 0)

#contracts2[[25]] <- future_map(contracts2[[25]], ~filter(.x, year == "Total"))

#contracts2 <- contracts2 |> 
#  mutate()

contracts2_new[[25]] <- purrr::map(contracts2_new[[25]], ~filter(.x, year < 2024))

for(x in 1:nrow(contracts2_new)) {
  contracts2_new$career_earnings[x] = sum(contracts2_new[[25]][[x]]$cash_paid)
}

contracts_new_active <- contracts2_new |> filter(is_active == TRUE) |> filter(value >= 0.795)

c(quantile(contracts_new_active$career_earnings, 0.20, na.rm = T), quantile(contracts_new_active$career_earnings, 0.80, na.rm = T))
won <- c(320, 326, 325, 318, 322, 320, 329, 317, 316, 331,
         320, 320, 317, 329, 316, 308, 321, 319, 322, 335,
         318, 313, 327, 314, 329, 323, 327, 323, 324, 314,
         308, 305, 328, 330, 322, 310, 324, 314, 312, 318,
         313, 320, 324, 311, 317, 325, 328, 319, 310, 324)

# Define bins (306-310, 311-315, ...)
bins <- seq(306, 335, by = 5)

# Cut the data into bins
won_bins <- cut(won, breaks = bins, right = FALSE, include.lowest = TRUE)

# Count occurrences in each bin
bin_counts <- table(won_bins)


## Career Earnings plot

contracts_new_active |>
  ggplot(aes(x = career_earnings)) +
  geom_histogram(aes(y = ..count.. / sum(..count..)), binwidth = 1, fill = "mediumseagreen", color = "mediumseagreen", alpha = 0.7) +
  labs(x = "Career Earnings (Millions)", y = "Density", 
       title = "NFL Career Earnings",
       subtitle = "Data via OTC | Cooper Davis | 2023 Season") +
  geom_vline(xintercept = c(1.567759, 21.464150), color = "black", linetype = "dashed", size = 0.3) +
  theme_minimal() +
  annotate("text", x = Inf, y = Inf, label = "Middle Class: $1.568M - $21.46M", 
                            hjust = 1.1, vjust = 2, color = "black", size = 4) +
  theme(plot.title = element_text(hjust=0.5, size=20),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.y = element_text(colour="black"),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank(),  
        legend.position = "top",
        legend.title = element_blank())
  
ggsave("/Users/Cooper/Documents/Folder for Contracts Project/Graphs/CareerEarnings2024.png", width = 10, height = 7, dpi = "retina")

## Doesn't include rookies



