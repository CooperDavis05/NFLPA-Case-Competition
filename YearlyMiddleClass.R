## 2011
contracts <- load_contracts(file_type = getOption("nflreadr.prefer", default = "rds"))

for(x in 1:nrow(contracts)) {
  contracts$empty[x] = ifelse(is_empty(contracts$cols[[x]]), 1, 0)
}  
contracts <- contracts |> filter(empty == 0) |> select(-empty) |>  filter(year_signed != 0)

#contracts[[25]] <-  purrr::map(contracts[[25]], ~filter(.x, year == 2011))

#for(x in 1:nrow(contracts)) {
#  contracts$empty[x] = ifelse(nrow(contracts$cols[[x]] == 0), 1, 0)
#}  

#contracts_2011 <- contracts |> filter(empty == 1) |> select(-empty) |> 
#  filter(year_signed <= 2011) |>
#  group_by(player, position) |> 
#  filter(year_signed == max(year_signed)) |> 
#  ungroup() |> 
#  select(player, position, cols, apy_cap_pct)

contracts[[25]] <-  purrr::map(contracts[[25]], ~filter(.x, year == 2011))

for(x in 1:nrow(contracts)) {
  contracts$empty[x] = ifelse(nrow(contracts$cols[[x]] == 0), 1, 0)
}  

contracts_2011 <- contracts |> filter(empty == 1) |> select(-empty) |> 
  filter(year_signed <= 2011) |>
  group_by(player, position, draft_year) |> 
  filter(year_signed == max(year_signed)) |> 
  ungroup() |> 
  select(player, position, cols, apy_cap_pct, draft_year) |> 
  group_by(player, draft_year) |> 
  filter(apy_cap_pct == max(apy_cap_pct)) |> 
  ungroup() |> 
  distinct(player, draft_year, .keep_all = TRUE)




contracts_2011 |> 
  ggplot(aes(x = apy_cap_pct)) +
  geom_histogram(aes(y = ..count.. / sum(..count..)), binwidth = 0.005, fill = "skyblue", color = "skyblue", alpha = 0.7) +
  theme_minimal() +
  geom_vline(xintercept = c((2/3)*median(contracts_new_active$apy_cap_pct), 
                            (2)*median(contracts_new_active$apy_cap_pct)), 
             color = "black", linetype = "dashed", size = 0.2) +
  labs(x = "APY Cap Percentage", y = "Density", 
       title = "APY Cap Percentage Distribution 2011",
       subtitle = "Data via OTC | Cooper Davis") +
  theme(panel.grid.major.y = element_line(size = 0.5),
        plot.title = element_text(hjust=0.5, size=20),
        axis.text.y = element_text(colour="black"),
        plot.subtitle = element_text(hjust=0.5))

ggsave("/Users/Cooper/Documents/Folder for Contracts Project/Graphs/APYCap2011.png", width = 10, height = 7, dpi = "retina")


### 2023 teams 
contracts <- load_contracts(file_type = getOption("nflreadr.prefer", default = "rds"))

for(x in 1:nrow(contracts)) {
  contracts$empty[x] = ifelse(is_empty(contracts$cols[[x]]), 1, 0)
}  
contracts <- contracts |> filter(empty == 0) |> select(-empty) |>  filter(year_signed != 0)

contracts[[25]] <- purrr::map(contracts[[25]], ~filter(.x, year == 2023))

contracts <- load_contracts(file_type = getOption("nflreadr.prefer", default = "rds"))

for(x in 1:nrow(contracts)) {
  contracts$empty[x] = ifelse(is_empty(contracts$cols[[x]]), 1, 0)
}  
contracts <- contracts |> filter(empty == 0) |> select(-empty) |>  filter(year_signed != 0)

contracts[[25]] <- purrr::map(contracts[[25]], ~filter(.x, year == 2023))

for(x in 1:nrow(contracts)) {
  contracts$empty[x] = ifelse(nrow(contracts$cols[[x]] == 0), 1, 0)
}  

contracts_2023 <- contracts |> filter(empty == 1) |> select(-empty) |> 
  filter(year_signed <= 2023) |>
  group_by(player, position, draft_year) |> 
  filter(year_signed == max(year_signed)) |> 
  ungroup() |> 
  select(player, position, cols, apy_cap_pct, draft_year) |> 
  group_by(player, draft_year) |> 
  filter(apy_cap_pct == max(apy_cap_pct)) |> 
  ungroup() |> 
  distinct(player, draft_year, .keep_all = TRUE)



contracts_2023 |> 
  ggplot(aes(x = apy_cap_pct)) +
  geom_histogram(aes(y = ..count.. / sum(..count..)), binwidth = 0.005, fill = "skyblue", color = "skyblue", alpha = 0.7) +
  theme_minimal() +
  geom_vline(xintercept = c((2/3)*median(contracts_new_active$apy_cap_pct), 
                            (2)*median(contracts_new_active$apy_cap_pct)), 
             color = "black", linetype = "dashed", size = 0.2) +
  labs(x = "APY Cap Percentage 2023", y = "Density", 
       title = "APY Cap Percentage Distribution 2023",
       subtitle = "Data via OTC | Cooper Davis") +
  theme(panel.grid.major.y = element_line(size = 0.5),
        plot.title = element_text(hjust=0.5, size=20),
        axis.text.y = element_text(colour="black"),
        plot.subtitle = element_text(hjust=0.5))

ggsave("/Users/Cooper/Documents/Folder for Contracts Project/Graphs/APYCap2023.png", width = 10, height = 7, dpi = "retina")



### 2020 teams 
contracts <- load_contracts(file_type = getOption("nflreadr.prefer", default = "rds"))

for(x in 1:nrow(contracts)) {
  contracts$empty[x] = ifelse(is_empty(contracts$cols[[x]]), 1, 0)
}  
contracts <- contracts |> filter(empty == 0) |> select(-empty) |>  filter(year_signed != 0)

contracts[[25]] <- purrr::map(contracts[[25]], ~filter(.x, year == 2020))

contracts <- load_contracts(file_type = getOption("nflreadr.prefer", default = "rds"))

for(x in 1:nrow(contracts)) {
  contracts$empty[x] = ifelse(is_empty(contracts$cols[[x]]), 1, 0)
}  
contracts <- contracts |> filter(empty == 0) |> select(-empty) |>  filter(year_signed != 0)

contracts[[25]] <- purrr::map(contracts[[25]], ~filter(.x, year == 2020))

for(x in 1:nrow(contracts)) {
  contracts$empty[x] = ifelse(nrow(contracts$cols[[x]] == 0), 1, 0)
}  

contracts_2020 <- contracts |> filter(empty == 1) |> select(-empty) |> 
  filter(year_signed <= 2020) |>
  group_by(player, position, draft_year) |> 
  filter(year_signed == max(year_signed)) |> 
  ungroup() |> 
  select(player, position, cols, apy_cap_pct, draft_year) |> 
  group_by(player, draft_year) |> 
  filter(apy_cap_pct == max(apy_cap_pct)) |> 
  ungroup() |> 
  distinct(player, draft_year, .keep_all = TRUE)



contracts_2020 |> 
  ggplot(aes(x = apy_cap_pct)) +
  geom_histogram(aes(y = ..count.. / sum(..count..)), binwidth = 0.005, fill = "skyblue", color = "skyblue", alpha = 0.7) +
  theme_minimal() +
  geom_vline(xintercept = c((2/3)*median(contracts_new_active$apy_cap_pct), 
                            (2)*median(contracts_new_active$apy_cap_pct)), 
             color = "black", linetype = "dashed", size = 0.2) +
  labs(x = "APY Cap Percentage 2020", y = "Density", 
       title = "APY Cap Percentage Distribution 2020",
       subtitle = "Data via OTC | Cooper Davis") +
  theme(panel.grid.major.y = element_line(size = 0.5),
        plot.title = element_text(hjust=0.5, size=20),
        axis.text.y = element_text(colour="black"),
        plot.subtitle = element_text(hjust=0.5))

ggsave("/Users/Cooper/Documents/Folder for Contracts Project/Graphs/APYCap2020.png", width = 10, height = 7, dpi = "retina")


