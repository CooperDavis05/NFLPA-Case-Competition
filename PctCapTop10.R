future::plan("multisession")

contracts <- load_contracts(file_type = getOption("nflreadr.prefer", default = "rds"))

for(x in 1:nrow(contracts)) {
  contracts$empty[x] = ifelse(is_empty(contracts$cols[[x]]), 1, 0)
}  
contracts_filtered_2024 <- contracts |> filter(empty == 0) |> select(-empty) |>  filter(year_signed != 0)


contracts_filtered_2024[[25]] <-  purrr::map(contracts_filtered_2024[[25]], ~filter(.x, year == 2024))

for(x in 1:nrow(contracts_filtered_2024)) {
  contracts_filtered_2024$empty[x] = ifelse(nrow(contracts_filtered_2024$cols[[x]] == 0), 1, 0)
}  
contracts_filtered_2024 <- contracts_filtered_2024 |> filter(empty == 1) |> select(-empty) |>  filter(year_signed != 0)


for(x in 1:nrow(contracts_filtered_2024)) {
  contracts_filtered_2024$cap_hit[x] = contracts_filtered_2024[[25]][[x]]$cap_number
  contracts_filtered_2024$cap_pct[x] = contracts_filtered_2024[[25]][[x]]$cap_percent
}


rosters_2024 <- load_rosters(2024) |> select(full_name, team, gsis_id)
  

rosters_2024$full_name <- clean_player_names(rosters_2024$full_name) 

contracts_filtered_2024$player <- clean_player_names(contracts_filtered_2024$player)

top_cap_players <- contracts_filtered_2024 |> filter(is_active == T) |> 
  right_join(rosters_2024, by = c("player" = "full_name", "gsis_id")) |>
  mutate(team.y = case_when(
    team.y == "BAL" ~ "Baltimore Ravens",
    team.y == "IND" ~ "Indianapolis Colts",
    team.y == "NE" ~ "New England Patriots",
    team.y == "PIT" ~ "Pittsburgh Steelers",
    team.y == "KC" ~ "Kansas City Chiefs",
    team.y == "LV" ~ "Las Vegas Raiders",
    team.y == "GB" ~ "Green Bay Packers",
    team.y == "CHI" ~ "Chicago Bears",
    team.y == "NYG" ~ "New York Giants",
    team.y == "DAL" ~ "Dallas Cowboys",
    team.y == "MIA" ~ "Miami Dolphins",
    team.y == "PHI" ~ "Philadelphia Eagles",
    team.y == "LA" ~ "Los Angeles Rams",
    team.y == "SF" ~ "San Francisco 49ers",
    team.y == "TEN" ~ "Tennessee Titans",
    team.y == "MIN" ~ "Minnesota Vikings",
    team.y == "ATL" ~ "Atlanta Falcons",
    team.y == "SEA" ~ "Seattle Seahawks",
    team.y == "BUF" ~ "Buffalo Bills",
    team.y == "JAX" ~ "Jacksonville Jaguars",
    team.y == "LAC" ~ "Los Angeles Chargers",
    team.y == "CAR" ~ "Carolina Panthers",
    team.y == "DET" ~ "Detroit Lions",
    team.y == "NYJ" ~ "New York Jets",
    team.y == "WAS" ~ "Washington Commanders",
    team.y == "CIN" ~ "Cincinnati Bengals",
    team.y == "ARI" ~ "Arizona Cardinals",
    team.y == "NO" ~ "New Orleans Saints",
    team.y == "CLE" ~ "Cleveland Browns",
    team.y == "TB" ~ "Tampa Bay Buccaneers",
    team.y == "DEN" ~ "Denver Broncos",
    team.y == "HOU" ~ "Houston Texans",
    TRUE ~ team.y  # Default case, keep original if no match
  )) |> 
  mutate(team.x = team.y) |> 
  select(-team.y) |> 
  rename(team = team.x) |> 
  summarize(player, position, team, cap_hit, cap_total = 255.4, cap_pct = round(cap_hit/cap_total, 3), gsis_id) |> 
  filter(!is.na(cap_hit)) |> 
  group_by(team) |> 
  mutate(cap_rank = rank(-cap_hit, ties.method = "first")) |> # Top 51 contracts
  filter(cap_rank <= 51) |> 
  ungroup()

top_ten_mutation <- top_cap_players |> 
  group_by(team) |> 
  mutate(top_ten_cap_sum = ifelse(cap_rank < 11, sum(cap_hit[cap_rank < 11], na.rm = TRUE), NA)) |>
  select(team, top_ten_cap_sum, cap_total) |> 
  filter(!is.na(top_ten_cap_sum)) |> 
  unique() |> 
  ungroup() |> 
  mutate(avg_cap_sum = mean(top_ten_cap_sum),
         avg_cap_sum_pct = avg_cap_sum/cap_total,
         year = 2024)

simple_top_ten <- top_ten_mutation |> 
  select(year, avg_cap_sum, cap_total, avg_cap_sum_pct) |> 
  unique()
  
  



# Salary Cap Ref
cap_ref <- tibble(cap = c(52388000, 57288000, 62172000, 67405000, 71101000, 75007000, 80582000, 85500000, 102000000, 109000000, 116000000, 123000000, 128872565, 120375000, 120600000, 123600000, 133000000, 143280000, 155270000, 167000000, 177200000, 188200000, 198200000, 182500000, 208200000, 224800000)/1000000,
                  year = c(1998:2023))

#### YOU IDIOT USE THE TEAM OF THE CAP YEAR YOU SLIMY GUY
####### 2023
contracts_filtered_2023 <- contracts |> filter(empty == 0) |> select(-empty) |>  filter(year_signed != 0)

contracts_filtered_2023[[25]] <-  purrr::map(contracts_filtered_2023[[25]], ~filter(.x, year == 2023))

for(x in 1:nrow(contracts_filtered_2023)) {
  contracts_filtered_2023$empty[x] = ifelse(nrow(contracts_filtered_2023$cols[[x]] == 0), 1, 0)
}  
contracts_filtered_2023 <- contracts_filtered_2023 |> filter(empty == 1) |> select(-empty) |>  filter(year_signed != 0)


for(x in 1:nrow(contracts_filtered_2023)) {
  contracts_filtered_2023$cap_hit[x] = contracts_filtered_2023[[25]][[x]]$cap_number
  contracts_filtered_2023$cap_pct[x] = contracts_filtered_2023[[25]][[x]]$cap_percent
  contracts_filtered_2023$team_real[x] = contracts_filtered_2023[[25]][[x]]$team
}



contracts_filtered_2023 <- contracts_filtered_2023 |> mutate(team = team_real) |> select(-team_real)

contracts_filtered_2023$player <- clean_player_names(contracts_filtered_2023$player)

top_cap_players_2023 <- contracts_filtered_2023 |>
  summarize(player, team, cap_hit, cap_total = cap_ref$cap[which(cap_ref$year == 2023)], cap_pct = round(cap_hit/cap_total, 3), gsis_id) |> 
  unique() |> 
  filter(!is.na(cap_hit)) |> 
  group_by(team) |> 
  mutate(cap_rank = rank(-cap_hit, ties.method = "first")) |> # Top 51 contracts
  filter(cap_rank <= 51) |> 
  ungroup()

# How to get rid of duplicate

top_ten_mutation_2023 <- top_cap_players_2023 |> 
  group_by(team) |> 
  mutate(top_ten_cap_sum = ifelse(cap_rank < 11, sum(cap_hit[cap_rank < 11], na.rm = TRUE), NA)) |>
  select(team, top_ten_cap_sum, cap_total) |> 
  filter(!is.na(top_ten_cap_sum)) |> 
  unique() |> 
  ungroup() |> 
  mutate(avg_cap_sum = mean(top_ten_cap_sum),
         avg_cap_sum_pct = avg_cap_sum/cap_total,
         year = 2023)

simple_top_ten_2023 <- top_ten_mutation_2023 |> 
  select(year, avg_cap_sum, cap_total, avg_cap_sum_pct) |> 
  unique()


####### 2022
contracts_filtered_2022 <- contracts |> filter(empty == 0) |> select(-empty) |>  filter(year_signed != 0)

contracts_filtered_2022[[25]] <-  purrr::map(contracts_filtered_2022[[25]], ~filter(.x, year == 2022))

for(x in 1:nrow(contracts_filtered_2022)) {
  contracts_filtered_2022$empty[x] = ifelse(nrow(contracts_filtered_2022$cols[[x]] == 0), 1, 0)
}  
contracts_filtered_2022 <- contracts_filtered_2022 |> filter(empty == 1) |> select(-empty) |>  filter(year_signed != 0)

for(x in 1:nrow(contracts_filtered_2022)) {
  contracts_filtered_2022$cap_hit[x] = contracts_filtered_2022[[25]][[x]]$cap_number
  contracts_filtered_2022$cap_pct[x] = contracts_filtered_2022[[25]][[x]]$cap_percent
  contracts_filtered_2022$team_real[x] = contracts_filtered_2022[[25]][[x]]$team
}

contracts_filtered_2022 <- contracts_filtered_2022 |> mutate(team = team_real) |> select(-team_real)

contracts_filtered_2022$player <- clean_player_names(contracts_filtered_2022$player)

top_cap_players_2022 <- contracts_filtered_2022 |>
  summarize(player, team, cap_hit, cap_total = cap_ref$cap[which(cap_ref$year == 2022)], cap_pct = round(cap_hit/cap_total, 3), gsis_id) |> 
  unique() |> 
  filter(!is.na(cap_hit)) |> 
  group_by(team) |> 
  mutate(cap_rank = rank(-cap_hit, ties.method = "first")) |> # Top 51 contracts
  filter(cap_rank <= 51) |> 
  ungroup()

top_ten_mutation_2022 <- top_cap_players_2022 |> 
  group_by(team) |> 
  mutate(top_ten_cap_sum = ifelse(cap_rank < 11, sum(cap_hit[cap_rank < 11], na.rm = TRUE), NA)) |>
  select(team, top_ten_cap_sum, cap_total) |> 
  filter(!is.na(top_ten_cap_sum)) |> 
  unique() |> 
  ungroup() |> 
  mutate(avg_cap_sum = mean(top_ten_cap_sum),
         avg_cap_sum_pct = avg_cap_sum/cap_total,
         year = 2023)

simple_top_ten_2022 <- top_ten_mutation_2022 |> 
  select(year, avg_cap_sum, cap_total, avg_cap_sum_pct) |> 
  unique()


total_top_tens_2014_2023 <- rbind(simple_top_ten, simple_top_ten_2023, simple_top_ten_2022)

## The rest

for(y in 2021:2014) {
  contracts_filtered_loop <- contracts |> filter(empty == 0) |> select(-empty) |>  filter(year_signed != 0)
  
  contracts_filtered_loop[[25]] <-  purrr::map(contracts_filtered_loop[[25]], ~filter(.x, year == y))
  
  for(x in 1:nrow(contracts_filtered_loop)) {
    contracts_filtered_loop$empty[x] = ifelse(nrow(contracts_filtered_loop$cols[[x]] == 0), 1, 0)
  }  
  contracts_filtered_loop <- contracts_filtered_loop |> filter(empty == 1) |> select(-empty) |>  filter(year_signed != 0)
  
  for(x in 1:nrow(contracts_filtered_loop)) {
    contracts_filtered_loop$cap_hit[x] = contracts_filtered_loop[[25]][[x]]$cap_number
    contracts_filtered_loop$cap_pct[x] = contracts_filtered_loop[[25]][[x]]$cap_percent
    contracts_filtered_loop$team_real[x] = contracts_filtered_loop[[25]][[x]]$team
  }
  
  contracts_filtered_loop <- contracts_filtered_loop |> mutate(team = team_real) |> select(-team_real)
  
  contracts_filtered_loop$player <- clean_player_names(contracts_filtered_loop$player)
  
  top_cap_players_loop <- contracts_filtered_loop |>
    summarize(player, team, cap_hit, cap_total = cap_ref$cap[which(cap_ref$year == y)], cap_pct = round(cap_hit/cap_total, 3), gsis_id) |> 
    unique() |> 
    filter(!is.na(cap_hit)) |> 
    group_by(team) |> 
    mutate(cap_rank = rank(-cap_hit, ties.method = "first")) |> # Top 51 contracts
    filter(cap_rank <= 51) |> 
    ungroup()
  
  top_ten_mutation_loop <- top_cap_players_loop |> 
    group_by(team) |> 
    mutate(top_ten_cap_sum = ifelse(cap_rank < 11, sum(cap_hit[cap_rank < 11], na.rm = TRUE), NA)) |>
    select(team, top_ten_cap_sum, cap_total) |> 
    filter(!is.na(top_ten_cap_sum)) |> 
    unique() |> 
    ungroup() |> 
    mutate(avg_cap_sum = mean(top_ten_cap_sum),
           avg_cap_sum_pct = avg_cap_sum/cap_total,
           year = y)
  
  simple_top_ten_loop <- top_ten_mutation_loop |> 
    select(year, avg_cap_sum, cap_total, avg_cap_sum_pct) |> 
    unique()
  
  total_top_tens_2014_2023 <- rbind(total_top_tens_2014_2023, simple_top_ten_loop)
}


# Make a graph
total_top_tens_2014_2023 |> 
  ggplot(aes(x = year, y = avg_cap_sum_pct)) +
  geom_line() +
  labs(x = "Year", 
       y = "Top 10 Cap Hit %", 
       title = "How Has the Trend in Top Cap Spending Changed?",
                   subtitle = "Data via OTC | Cooper Davis") +
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.5, size=20),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.y = element_text(color="black"),
        axis.text.x = element_text(color="black"),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor.y = element_line(color = "gray90"),
        panel.grid.minor.x = element_blank()) +
  scale_x_continuous(breaks = seq(2014, 
                                  2024, 
                                  by = 1))

ggsave("/Users/Cooper/Documents/Folder for Contracts Project/Graphs/Top10's%.png", width = 10, height = 7, dpi = "retina")

### DO TOP 5 FOR 2024

contracts_filtered_loop <- contracts |> filter(empty == 0) |> select(-empty) |>  filter(year_signed != 0)

contracts_filtered_loop[[25]] <-  purrr::map(contracts_filtered_loop[[25]], ~filter(.x, year == 2024))

for(x in 1:nrow(contracts_filtered_loop)) {
  contracts_filtered_loop$empty[x] = ifelse(nrow(contracts_filtered_loop$cols[[x]] == 0), 1, 0)
}  
contracts_filtered_loop <- contracts_filtered_loop |> filter(empty == 1) |> select(-empty) |>  filter(year_signed != 0)

for(x in 1:nrow(contracts_filtered_loop)) {
  contracts_filtered_loop$cap_hit[x] = contracts_filtered_loop[[25]][[x]]$cap_number
  contracts_filtered_loop$cap_pct[x] = contracts_filtered_loop[[25]][[x]]$cap_percent
  contracts_filtered_loop$team_real[x] = contracts_filtered_loop[[25]][[x]]$team
}

contracts_filtered_loop <- contracts_filtered_loop |> mutate(team = team_real) |> select(-team_real)

contracts_filtered_loop$player <- clean_player_names(contracts_filtered_loop$player)

top_cap_players_loop <- contracts_filtered_loop |>
  summarize(player, team, cap_hit, cap_total = 255.4, cap_pct = round(cap_hit/cap_total, 3), gsis_id) |> 
  unique() |> 
  filter(!is.na(cap_hit)) |> 
  group_by(team) |> 
  mutate(cap_rank = rank(-cap_hit, ties.method = "first")) |> # Top 51 contracts
  filter(cap_rank <= 51) |> 
  ungroup()

middle30_mutation_loop <- top_cap_players_loop |> 
  group_by(team) |> 
  mutate(top_five_cap_sum = ifelse(cap_rank < 6, sum(cap_hit[cap_rank < 6], na.rm = TRUE), NA)) |>
  select(team, top_five_cap_sum, cap_total) |> 
  filter(!is.na(top_five_cap_sum)) |> 
  unique() |> 
  ungroup() |> 
  mutate(avg_cap_sum = mean(top_five_cap_sum),
         avg_cap_sum_pct = avg_cap_sum/cap_total,
         year = 2024)

simple_top_five_loop <- top_five_mutation_loop |> 
  select(year, avg_cap_sum, cap_total, avg_cap_sum_pct) |> 
  unique()


total_top_fives_2014_2024 <- simple_top_five_loop

### Do 5 now!!!!!!
for(y in 2023:2014) {
  contracts_filtered_loop <- contracts |> filter(empty == 0) |> select(-empty) |>  filter(year_signed != 0)
  
  contracts_filtered_loop[[25]] <-  purrr::map(contracts_filtered_loop[[25]], ~filter(.x, year == y))
  
  for(x in 1:nrow(contracts_filtered_loop)) {
    contracts_filtered_loop$empty[x] = ifelse(nrow(contracts_filtered_loop$cols[[x]] == 0), 1, 0)
  }  
  contracts_filtered_loop <- contracts_filtered_loop |> filter(empty == 1) |> select(-empty) |>  filter(year_signed != 0)
  
  for(x in 1:nrow(contracts_filtered_loop)) {
    contracts_filtered_loop$cap_hit[x] = contracts_filtered_loop[[25]][[x]]$cap_number
    contracts_filtered_loop$cap_pct[x] = contracts_filtered_loop[[25]][[x]]$cap_percent
    contracts_filtered_loop$team_real[x] = contracts_filtered_loop[[25]][[x]]$team
  }
  
  contracts_filtered_loop <- contracts_filtered_loop |> mutate(team = team_real) |> select(-team_real)
  
  contracts_filtered_loop$player <- clean_player_names(contracts_filtered_loop$player)
  
  top_cap_players_loop <- contracts_filtered_loop |>
    summarize(player, team, cap_hit, cap_total = cap_ref$cap[which(cap_ref$year == y)], cap_pct = round(cap_hit/cap_total, 3), gsis_id) |> 
    unique() |> 
    filter(!is.na(cap_hit)) |> 
    group_by(team) |> 
    mutate(cap_rank = rank(-cap_hit, ties.method = "first")) |> # Top 51 contracts
    filter(cap_rank <= 51) |> 
    ungroup()
  
  top_five_mutation_loop <- top_cap_players_loop |> 
    group_by(team) |> 
    mutate(top_five_cap_sum = ifelse(cap_rank < 6, sum(cap_hit[cap_rank < 6], na.rm = TRUE), NA)) |>
    select(team, top_five_cap_sum, cap_total) |> 
    filter(!is.na(top_five_cap_sum)) |> 
    unique() |> 
    ungroup() |> 
    mutate(avg_cap_sum = mean(top_five_cap_sum),
           avg_cap_sum_pct = avg_cap_sum/cap_total,
           year = y)
  
  simple_top_five_loop <- top_five_mutation_loop |> 
    select(year, avg_cap_sum, cap_total, avg_cap_sum_pct) |> 
    unique()
  
  total_top_fives_2014_2024 <- rbind(total_top_fives_2014_2024, simple_top_five_loop)
}

total_top_fives_2014_2024 |> 
  ggplot(aes(x = year, y = avg_cap_sum_pct)) +
  geom_line() +
  labs(x = "Year", 
       y = "Top 5 Cap Hit %", 
       title = "How Has the Trend in Top Cap Spending Changed?",
       subtitle = "Data via OTC | Cooper Davis") +
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.5, size=20),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.y = element_text(color="black"),
        axis.text.x = element_text(color="black"),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor.y = element_line(color = "gray90"),
        panel.grid.minor.x = element_blank()) +
  scale_x_continuous(breaks = seq(2014, 
                                  2024, 
                                  by = 1))

ggsave("/Users/Cooper/Documents/Folder for Contracts Project/Graphs/Top5's%.png", width = 10, height = 7, dpi = "retina")










####### Do Middle 30!

contracts_filtered_loop <- contracts |> filter(empty == 0) |> select(-empty) |>  filter(year_signed != 0)

contracts_filtered_loop[[25]] <-  purrr::map(contracts_filtered_loop[[25]], ~filter(.x, year == 2024))

for(x in 1:nrow(contracts_filtered_loop)) {
  contracts_filtered_loop$empty[x] = ifelse(nrow(contracts_filtered_loop$cols[[x]] == 0), 1, 0)
}  
contracts_filtered_loop <- contracts_filtered_loop |> filter(empty == 1) |> select(-empty) |>  filter(year_signed != 0)

for(x in 1:nrow(contracts_filtered_loop)) {
  contracts_filtered_loop$cap_hit[x] = contracts_filtered_loop[[25]][[x]]$cap_number
  contracts_filtered_loop$cap_pct[x] = contracts_filtered_loop[[25]][[x]]$cap_percent
  contracts_filtered_loop$team_real[x] = contracts_filtered_loop[[25]][[x]]$team
}

contracts_filtered_loop <- contracts_filtered_loop |> mutate(team = team_real) |> select(-team_real)

contracts_filtered_loop$player <- clean_player_names(contracts_filtered_loop$player)

top_cap_players_loop <- contracts_filtered_loop |>
  summarize(player, team, cap_hit, cap_total = 255.4, cap_pct = round(cap_hit/cap_total, 3), gsis_id) |> 
  unique() |> 
  filter(!is.na(cap_hit)) |> 
  group_by(team) |> 
  mutate(cap_rank = rank(-cap_hit, ties.method = "first")) |> # Top 51 contracts
  filter(cap_rank <= 51) |> 
  ungroup()

all_contracts_mutation_loop <- top_cap_players_loop |> 
  group_by(team) |> 
  mutate(middle30_cap_sum = ifelse(cap_rank < 41 & cap_rank > 10, sum(cap_hit[cap_rank < 41 & cap_rank > 10], na.rm = TRUE), NA)) |>
  select(team, middle30_cap_sum, cap_total) |> 
  filter(!is.na(middle30_cap_sum)) |> 
  unique() |> 
  ungroup() |> 
  mutate(avg_cap_sum = mean(middle30_cap_sum),
         avg_cap_sum_pct = avg_cap_sum/cap_total,
         year = 2024)

simple_all_contracts_loop <- all_contracts_mutation_loop |> 
  select(year, avg_cap_sum, cap_total, avg_cap_sum_pct) |> 
  unique()


total_middle30s_2014_2024 <- simple_all_contracts_loop

for(y in 2023:2014) {
  contracts_filtered_loop <- contracts |> filter(empty == 0) |> select(-empty) |>  filter(year_signed != 0)
  
  contracts_filtered_loop[[25]] <-  purrr::map(contracts_filtered_loop[[25]], ~filter(.x, year == y))
  
  for(x in 1:nrow(contracts_filtered_loop)) {
    contracts_filtered_loop$empty[x] = ifelse(nrow(contracts_filtered_loop$cols[[x]] == 0), 1, 0)
  }  
  contracts_filtered_loop <- contracts_filtered_loop |> filter(empty == 1) |> select(-empty) |>  filter(year_signed != 0)
  
  for(x in 1:nrow(contracts_filtered_loop)) {
    contracts_filtered_loop$cap_hit[x] = contracts_filtered_loop[[25]][[x]]$cap_number
    contracts_filtered_loop$cap_pct[x] = contracts_filtered_loop[[25]][[x]]$cap_percent
    contracts_filtered_loop$team_real[x] = contracts_filtered_loop[[25]][[x]]$team
  }
  
  contracts_filtered_loop <- contracts_filtered_loop |> mutate(team = team_real) |> select(-team_real)
  
  contracts_filtered_loop$player <- clean_player_names(contracts_filtered_loop$player)
  
  top_cap_players_loop <- contracts_filtered_loop |>
    summarize(player, team, cap_hit, cap_total = cap_ref$cap[which(cap_ref$year == y)], cap_pct = round(cap_hit/cap_total, 3), gsis_id) |> 
    unique() |> 
    filter(!is.na(cap_hit)) |> 
    group_by(team) |> 
    mutate(cap_rank = rank(-cap_hit, ties.method = "first")) |> # Top 51 contracts
    filter(cap_rank <= 51) |> 
    ungroup()
  
  middle30_mutation_loop <- top_cap_players_loop |> 
    group_by(team) |> 
    mutate(middle30_cap_sum = ifelse(cap_rank < 41 & cap_rank > 10, sum(cap_hit[cap_rank < 41 & cap_rank > 10], na.rm = TRUE), NA)) |>
    select(team, middle30_cap_sum, cap_total) |> 
    filter(!is.na(middle30_cap_sum)) |> 
    unique() |> 
    ungroup() |> 
    mutate(avg_cap_sum = mean(middle30_cap_sum),
           avg_cap_sum_pct = avg_cap_sum/cap_total,
           year = y)
  
  simple_middle_30_loop <- middle30_mutation_loop |> 
    select(year, avg_cap_sum, cap_total, avg_cap_sum_pct) |> 
    unique()
  
  total_middle30s_2014_2024 <- rbind(total_middle30s_2014_2024, simple_middle_30_loop)
}

total_middle30s_2014_2024 |> 
  ggplot(aes(x = year, y = avg_cap_sum_pct)) +
  geom_line() +
  labs(x = "Year", 
       y = "Top 5 Cap Hit %", 
       title = "How Has the Trend in Top Cap Spending Changed?",
       subtitle = "Data via OTC | Cooper Davis") +
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.5, size=20),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.y = element_text(color="black"),
        axis.text.x = element_text(color="black"),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor.y = element_line(color = "gray90"),
        panel.grid.minor.x = element_blank()) +
  scale_x_continuous(breaks = seq(2014, 
                                  2024, 
                                  by = 1))

ggsave("/Users/Cooper/Documents/Folder for Contracts Project/Graphs/Middle30's%.png", width = 10, height = 7, dpi = "retina")



#Make a dataviz
# Do middle class cap hits



## Top 51 spending of total cap (can analyze trends based off of that)
#### Maybe want to analyze total cap spending now

contracts_filtered_loop <- contracts |> filter(empty == 0) |> select(-empty) |>  filter(year_signed != 0)

contracts_filtered_loop[[25]] <-  purrr::map(contracts_filtered_loop[[25]], ~filter(.x, year == 2024))

for(x in 1:nrow(contracts_filtered_loop)) {
  contracts_filtered_loop$empty[x] = ifelse(nrow(contracts_filtered_loop$cols[[x]] == 0), 1, 0)
}  
contracts_filtered_loop <- contracts_filtered_loop |> filter(empty == 1) |> select(-empty) |>  filter(year_signed != 0)

for(x in 1:nrow(contracts_filtered_loop)) {
  contracts_filtered_loop$cap_hit[x] = contracts_filtered_loop[[25]][[x]]$cap_number
  contracts_filtered_loop$cap_pct[x] = contracts_filtered_loop[[25]][[x]]$cap_percent
  contracts_filtered_loop$team_real[x] = contracts_filtered_loop[[25]][[x]]$team
}

contracts_filtered_loop <- contracts_filtered_loop |> mutate(team = team_real) |> select(-team_real)

contracts_filtered_loop$player <- clean_player_names(contracts_filtered_loop$player)

top_cap_players_loop <- contracts_filtered_loop |>
  summarize(player, team, cap_hit, cap_total = 255.4, cap_pct = round(cap_hit/cap_total, 3), gsis_id) |> 
  unique() |> 
  filter(!is.na(cap_hit)) |> 
  group_by(team) |> 
  mutate(cap_rank = rank(-cap_hit, ties.method = "first")) |> # Top 51 contracts
  filter(cap_rank <= 51) |> 
  ungroup()

all_contracts_mutation_loop <- top_cap_players_loop |> 
  group_by(team) |> 
  mutate(all_contracts_cap_sum = sum(cap_hit, na.rm = TRUE)) |>
  select(team, all_contracts_cap_sum, cap_total) |> 
  filter(!is.na(all_contracts_cap_sum)) |> 
  unique() |> 
  ungroup() |> 
  mutate(avg_cap_sum = mean(all_contracts_cap_sum),
         avg_cap_sum_pct = avg_cap_sum/cap_total,
         year = 2024)

simple_all_contracts_loop <- all_contracts_mutation_loop |> 
  select(year, avg_cap_sum, cap_total, avg_cap_sum_pct) |> 
  unique()


total_all_contractss_2014_2024 <- simple_all_contracts_loop

for(y in 2023:2014) {
  contracts_filtered_loop <- contracts |> filter(empty == 0) |> select(-empty) |>  filter(year_signed != 0)
  
  contracts_filtered_loop[[25]] <-  purrr::map(contracts_filtered_loop[[25]], ~filter(.x, year == y))
  
  for(x in 1:nrow(contracts_filtered_loop)) {
    contracts_filtered_loop$empty[x] = ifelse(nrow(contracts_filtered_loop$cols[[x]] == 0), 1, 0)
  }  
  contracts_filtered_loop <- contracts_filtered_loop |> filter(empty == 1) |> select(-empty) |>  filter(year_signed != 0)
  
  for(x in 1:nrow(contracts_filtered_loop)) {
    contracts_filtered_loop$cap_hit[x] = contracts_filtered_loop[[25]][[x]]$cap_number
    contracts_filtered_loop$cap_pct[x] = contracts_filtered_loop[[25]][[x]]$cap_percent
    contracts_filtered_loop$team_real[x] = contracts_filtered_loop[[25]][[x]]$team
  }
  
  contracts_filtered_loop <- contracts_filtered_loop |> mutate(team = team_real) |> select(-team_real)
  
  contracts_filtered_loop$player <- clean_player_names(contracts_filtered_loop$player)
  
  top_cap_players_loop <- contracts_filtered_loop |>
    summarize(player, team, cap_hit, cap_total = cap_ref$cap[which(cap_ref$year == y)], cap_pct = round(cap_hit/cap_total, 3), gsis_id) |> 
    unique() |> 
    filter(!is.na(cap_hit)) |> 
    group_by(team) |> 
    mutate(cap_rank = rank(-cap_hit, ties.method = "first")) |> # Top 51 contracts
    filter(cap_rank <= 51) |> 
    ungroup()
  
  all_contracts_mutation_loop <- top_cap_players_loop |> 
    group_by(team) |> 
    mutate(all_contracts_cap_sum = sum(cap_hit, na.rm = TRUE)) |>
    select(team, all_contracts_cap_sum, cap_total) |> 
    filter(!is.na(all_contracts_cap_sum)) |> 
    unique() |> 
    ungroup() |> 
    mutate(avg_cap_sum = mean(all_contracts_cap_sum),
           avg_cap_sum_pct = avg_cap_sum/cap_total,
           year = y)
  
  simple_all_contracts_loop <- all_contracts_mutation_loop |> 
    select(year, avg_cap_sum, cap_total, avg_cap_sum_pct) |> 
    unique()
  
  total_all_contractss_2014_2024 <- rbind(total_all_contractss_2014_2024, simple_all_contracts_loop)
}

total_all_contractss_2014_2024 |> 
  ggplot(aes(x = year, y = avg_cap_sum_pct)) +
  geom_line() +
  labs(x = "Year", 
       y = "All Player Cap Hit %", 
       title = "How Has the Trend in Mid Range Spending Changed?",
       subtitle = "Data via OTC | Cooper Davis | It is important to note the role dead cap plays in this") +
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.5, size=20),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.y = element_text(color="black"),
        axis.text.x = element_text(color="black"),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor.y = element_line(color = "gray90"),
        panel.grid.minor.x = element_blank()) +
  scale_x_continuous(breaks = seq(2014, 
                                  2024, 
                                  by = 1))

ggsave("/Users/Cooper/Documents/Folder for Contracts Project/Graphs/all_contracts's%.png", width = 10, height = 7, dpi = "retina")






### Bottom 10 contracts


contracts_filtered_loop <- contracts |> filter(empty == 0) |> select(-empty) |>  filter(year_signed != 0)

contracts_filtered_loop[[25]] <-  purrr::map(contracts_filtered_loop[[25]], ~filter(.x, year == 2024))

for(x in 1:nrow(contracts_filtered_loop)) {
  contracts_filtered_loop$empty[x] = ifelse(nrow(contracts_filtered_loop$cols[[x]] == 0), 1, 0)
}  
contracts_filtered_loop <- contracts_filtered_loop |> filter(empty == 1) |> select(-empty) |>  filter(year_signed != 0)

for(x in 1:nrow(contracts_filtered_loop)) {
  contracts_filtered_loop$cap_hit[x] = contracts_filtered_loop[[25]][[x]]$cap_number
  contracts_filtered_loop$cap_pct[x] = contracts_filtered_loop[[25]][[x]]$cap_percent
  contracts_filtered_loop$team_real[x] = contracts_filtered_loop[[25]][[x]]$team
}

contracts_filtered_loop <- contracts_filtered_loop |> mutate(team = team_real) |> select(-team_real)

contracts_filtered_loop$player <- clean_player_names(contracts_filtered_loop$player)

top_cap_players_loop <- contracts_filtered_loop |>
  summarize(player, team, cap_hit, cap_total = 255.4, cap_pct = round(cap_hit/cap_total, 3), gsis_id) |> 
  unique() |> 
  filter(!is.na(cap_hit)) |> 
  group_by(team) |> 
  mutate(cap_rank = rank(-cap_hit, ties.method = "first")) |> # Top 51 contracts
  filter(cap_rank <= 51) |> 
  ungroup()

bottom_10_mutation_loop <- top_cap_players_loop |> 
  group_by(team) |> 
  mutate(bottom10_cap_sum = ifelse(cap_rank > 40, sum(cap_hit[cap_rank > 40], na.rm = TRUE), NA)) |>
  select(team, bottom10_cap_sum, cap_total) |> 
  filter(!is.na(bottom10_cap_sum)) |> 
  unique() |> 
  ungroup() |> 
  mutate(avg_cap_sum = mean(bottom10_cap_sum),
         avg_cap_sum_pct = avg_cap_sum/cap_total,
         year = 2024)

simple_bottom_10_loop <- bottom_10_mutation_loop |> 
  select(year, avg_cap_sum, cap_total, avg_cap_sum_pct) |> 
  unique()


total_bottom10s_2014_2024 <- simple_bottom_10_loop


for(y in 2023:2014) {
  contracts_filtered_loop <- contracts |> filter(empty == 0) |> select(-empty) |>  filter(year_signed != 0)
  
  contracts_filtered_loop[[25]] <-  purrr::map(contracts_filtered_loop[[25]], ~filter(.x, year == y))
  
  for(x in 1:nrow(contracts_filtered_loop)) {
    contracts_filtered_loop$empty[x] = ifelse(nrow(contracts_filtered_loop$cols[[x]] == 0), 1, 0)
  }  
  contracts_filtered_loop <- contracts_filtered_loop |> filter(empty == 1) |> select(-empty) |>  filter(year_signed != 0)
  
  for(x in 1:nrow(contracts_filtered_loop)) {
    contracts_filtered_loop$cap_hit[x] = contracts_filtered_loop[[25]][[x]]$cap_number
    contracts_filtered_loop$cap_pct[x] = contracts_filtered_loop[[25]][[x]]$cap_percent
    contracts_filtered_loop$team_real[x] = contracts_filtered_loop[[25]][[x]]$team
  }
  
  contracts_filtered_loop <- contracts_filtered_loop |> mutate(team = team_real) |> select(-team_real)
  
  contracts_filtered_loop$player <- clean_player_names(contracts_filtered_loop$player)
  
  top_cap_players_loop <- contracts_filtered_loop |>
    summarize(player, team, cap_hit, cap_total = cap_ref$cap[which(cap_ref$year == y)], cap_pct = round(cap_hit/cap_total, 3), gsis_id) |> 
    unique() |> 
    filter(!is.na(cap_hit)) |> 
    group_by(team) |> 
    mutate(cap_rank = rank(-cap_hit, ties.method = "first")) |> # Top 51 contracts
    filter(cap_rank <= 51) |> 
    ungroup()
  
  bottom_10_mutation_loop <- top_cap_players_loop |> 
    group_by(team) |> 
    mutate(bottom_10_cap_sum = ifelse(cap_rank > 40, sum(cap_hit[cap_rank > 40], na.rm = TRUE), NA)) |>
    select(team, bottom_10_cap_sum, cap_total) |> 
    filter(!is.na(bottom_10_cap_sum)) |> 
    unique() |> 
    ungroup() |> 
    mutate(avg_cap_sum = mean(bottom_10_cap_sum),
           avg_cap_sum_pct = avg_cap_sum/cap_total,
           year = y)
  
  simple_bottom_10_loop <- bottom_10_mutation_loop |> 
    select(year, avg_cap_sum, cap_total, avg_cap_sum_pct) |> 
    unique()
  
  total_bottom10s_2014_2024 <- rbind(total_bottom10s_2014_2024, simple_bottom_10_loop)
}









total_all_contractss_2014_2024 <- total_all_contractss_2014_2024 |> 
  mutate(definition = "All Players")

total_middle30s_2014_2024 <- total_middle30s_2014_2024 |> 
  mutate(definition = "Middle30")

total_top_tens_2014_2023 <- total_top_tens_2014_2023 |> 
  mutate(definition = "Top10")

total_bottom10s_2014_2024 <- total_bottom10s_2014_2024 |> 
  mutate(definition = "Bottom11")

total_totals <- rbind(total_all_contractss_2014_2024, total_middle30s_2014_2024, total_top_tens_2014_2023, total_bottom10s_2014_2024)

total_totals |> 
  ggplot(aes(x = year, y = avg_cap_sum_pct, group = definition, color = definition)) +
  geom_line() +
  labs(x = "Year", 
       y = "All Player Cap Hit %", 
       title = "How Has the Trend in Mid Range Spending Changed?",
       subtitle = "Data via OTC | Cooper Davis | It is important to note the role dead cap plays in this") +
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.5, size=20),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.y = element_text(color="black"),
        axis.text.x = element_text(color="black"),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor.y = element_line(color = "gray90"),
        panel.grid.minor.x = element_blank()) +
  scale_x_continuous(breaks = seq(2014, 
                                  2024, 
                                  by = 1))

ggsave("/Users/Cooper/Documents/Folder for Contracts Project/Graphs/all_contracts's%.png", width = 10, height = 7, dpi = "retina")

write.csv(total_totals, "/Users/Cooper/Documents/Folder for Contracts Project/total_totals.csv")


