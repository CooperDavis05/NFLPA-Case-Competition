library(dplyr)
library(nflreadr)
library(ggplot2)

future::plan("multisession")

contracts <- load_contracts(file_type = getOption("nflreadr.prefer", default = "rds")) #Load in contracts

active_contracts_analysis_Cap <- contracts |> filter(is_active == T) |> 
  mutate(season = 2024)  

rosters_2024 <- load_rosters(2024)
rosters_2024$team <- clean_team_abbrs(rosters_2024$team)

rosters_2024 <- rosters_2024 |> 
  mutate(team = case_when(
    team == "BAL" ~ "Ravens",
    team == "IND" ~ "Colts",
    team == "NE" ~ "Patriots",
    team == "PIT" ~ "Steelers",
    team == "KC" ~ "Chiefs",
    team == "LV" ~ "Raiders",
    team == "GB" ~ "Packers",
    team == "CHI" ~ "Bears",
    team == "NYG" ~ "Giants",
    team == "DAL" ~ "Cowboys",
    team == "MIA" ~ "Dolphins",
    team == "PHI" ~ "Eagles",
    team == "LA" ~ "Rams",
    team == "SF" ~ "49ers",
    team == "TEN" ~ "Titans",
    team == "MIN" ~ "Vikings",
    team == "ATL" ~ "Falcons",
    team == "SEA" ~ "Seahawks",
    team == "BUF" ~ "Bills",
    team == "JAX" ~ "Jaguars",
    team == "LAC" ~ "Chargers",
    team == "CAR" ~ "Panthers",
    team == "DET" ~ "Lions",
    team == "NYJ" ~ "Jets",
    team == "WAS" ~ "Commanders",
    team == "CIN" ~ "Bengals",
    team == "ARI" ~ "Cardinals",
    team == "NO" ~ "Saints",
    team == "CLE" ~ "Browns",
    team == "TB" ~ "Buccaneers",
    team == "DEN" ~ "Broncos",
    team == "HOU" ~ "Texans",
    TRUE ~ team  # Default case, keep original if no match
  )) |> select(full_name, team, gsis_id)



active_contracts_analysis_Cap <- active_contracts_analysis_Cap |> left_join(rosters_2024, by = c("gsis_id" = "gsis_id")) |> 
  rename(team = team.x, team_yes = team.y)



active_contracts_analysis_Cap <- active_contracts_analysis_Cap |> 
  mutate(cap_total = 255.4, cap_pct = round(apy/cap_total, 3)) %>%
  separate(team, into = c("Team_1", "Team_2", "Team_3"), sep = "/", fill = "right") |> 
  mutate(Team_1 = case_when(
    Team_1 == "BAL" ~ "Ravens",
    Team_1 == "IND" ~ "Colts",
    Team_1 == "NE" ~ "Patriots",
    Team_1 == "PIT" ~ "Steelers",
    Team_1 == "KC" ~ "Chiefs",
    Team_1 == "LV" ~ "Raiders",
    Team_1 == "GB" ~ "Packers",
    Team_1 == "CHI" ~ "Bears",
    Team_1 == "NYG" ~ "Giants",
    Team_1 == "DAL" ~ "Cowboys",
    Team_1 == "MIA" ~ "Dolphins",
    Team_1 == "PHI" ~ "Eagles",
    Team_1 == "LA" ~ "Rams",
    Team_1 == "SF" ~ "49ers",
    Team_1 == "TEN" ~ "Titans",
    Team_1 == "MIN" ~ "Vikings",
    Team_1 == "ATL" ~ "Falcons",
    Team_1 == "SEA" ~ "Seahawks",
    Team_1 == "BUF" ~ "Bills",
    Team_1 == "JAX" ~ "Jaguars",
    Team_1 == "LAC" ~ "Chargers",
    Team_1 == "CAR" ~ "Panthers",
    Team_1 == "DET" ~ "Lions",
    Team_1 == "NYJ" ~ "Jets",
    Team_1 == "WAS" ~ "Commanders",
    Team_1 == "CIN" ~ "Bengals",
    Team_1 == "ARI" ~ "Cardinals",
    Team_1 == "NO" ~ "Saints",
    Team_1 == "CLE" ~ "Browns",
    Team_1 == "TB" ~ "Buccaneers",
    Team_1 == "DEN" ~ "Broncos",
    Team_1 == "HOU" ~ "Texans",
    TRUE ~ Team_1  # Default case, keep original if no match
  ))  |> 
  mutate(Team_2 = case_when(
    Team_2 == "BAL" ~ "Ravens",
    Team_2 == "IND" ~ "Colts",
    Team_2 == "NE" ~ "Patriots",
    Team_2 == "PIT" ~ "Steelers",
    Team_2 == "KC" ~ "Chiefs",
    Team_2 == "LV" ~ "Raiders",
    Team_2 == "GB" ~ "Packers",
    Team_2 == "CHI" ~ "Bears",
    Team_2 == "NYG" ~ "Giants",
    Team_2 == "DAL" ~ "Cowboys",
    Team_2 == "MIA" ~ "Dolphins",
    Team_2 == "PHI" ~ "Eagles",
    Team_2 == "LA" ~ "Rams",
    Team_2 == "SF" ~ "49ers",
    Team_2 == "TEN" ~ "Titans",
    Team_2 == "MIN" ~ "Vikings",
    Team_2 == "ATL" ~ "Falcons",
    Team_2 == "SEA" ~ "Seahawks",
    Team_2 == "BUF" ~ "Bills",
    Team_2 == "JAX" ~ "Jaguars",
    Team_2 == "LAC" ~ "Chargers",
    Team_2 == "CAR" ~ "Panthers",
    Team_2 == "DET" ~ "Lions",
    Team_2 == "NYJ" ~ "Jets",
    Team_2 == "WAS" ~ "Commanders",
    Team_2 == "CIN" ~ "Bengals",
    Team_2 == "ARI" ~ "Cardinals",
    Team_2 == "NO" ~ "Saints",
    Team_2 == "CLE" ~ "Browns",
    Team_2 == "TB" ~ "Buccaneers",
    Team_2 == "DEN" ~ "Broncos",
    Team_2 == "HOU" ~ "Texans",
    TRUE ~ Team_2  # Default case, keep original if no match
  )) |> 
  mutate(Team_3 = case_when(
    Team_3 == "BAL" ~ "Ravens",
    Team_3 == "IND" ~ "Colts",
    Team_3 == "NE" ~ "Patriots",
    Team_3 == "PIT" ~ "Steelers",
    Team_3 == "KC" ~ "Chiefs",
    Team_3 == "LV" ~ "Raiders",
    Team_3 == "GB" ~ "Packers",
    Team_3 == "CHI" ~ "Bears",
    Team_3 == "NYG" ~ "Giants",
    Team_3 == "DAL" ~ "Cowboys",
    Team_3 == "MIA" ~ "Dolphins",
    Team_3 == "PHI" ~ "Eagles",
    Team_3 == "LA" ~ "Rams",
    Team_3 == "SF" ~ "49ers",
    Team_3 == "TEN" ~ "Titans",
    Team_3 == "MIN" ~ "Vikings",
    Team_3 == "ATL" ~ "Falcons",
    Team_3 == "SEA" ~ "Seahawks",
    Team_3 == "BUF" ~ "Bills",
    Team_3 == "JAX" ~ "Jaguars",
    Team_3 == "LAC" ~ "Chargers",
    Team_3 == "CAR" ~ "Panthers",
    Team_3 == "DET" ~ "Lions",
    Team_3 == "NYJ" ~ "Jets",
    Team_3 == "WAS" ~ "Commanders",
    Team_3 == "CIN" ~ "Bengals",
    Team_3 == "ARI" ~ "Cardinals",
    Team_3 == "NO" ~ "Saints",
    Team_3 == "CLE" ~ "Browns",
    Team_3 == "TB" ~ "Buccaneers",
    Team_3 == "DEN" ~ "Broncos",
    Team_3 == "HOU" ~ "Texans",
    TRUE ~ Team_3  # Default case, keep original if no match
  )) %>%
  mutate(
    Team_1 = ifelse(Team_1 == team_yes, Team_1, 
                    ifelse(Team_2 == team_yes, Team_2, 
                           ifelse(Team_3 == team_yes, Team_3, NA))),
    Team_2 = ifelse(team_yes == Team_2, NA, Team_2),
    Team_3 = ifelse(team_yes == Team_3, NA, Team_3)
  ) %>%
  filter(!is.na(Team_1)) |> 
  rename(team = Team_1) |> 
  select(!c(Team_2, Team_3, team_yes))  |> 
  select(-full_name) |> group_by(team) |> 
  mutate(apy_rank = rank(-apy, ties.method = "first")) |> # Top 51 contracts
  filter(apy_rank <= 51) |> 
  mutate(top_5 = ifelse(apy_rank <= 5, 1, 0),
         top_10 = ifelse(apy_rank <= 10, 1, 0),
         middle_35 = ifelse(apy_rank <= 40 & apy_rank > 5, 1, 0),
         middle_30 = ifelse(apy_rank <= 40 & apy_rank > 10, 1, 0),
         rest = ifelse(apy_rank > 40, 1, 0)
  ) |> 
  ungroup() 






cap_ref <- tibble(cap = c(52388000, 57288000, 62172000, 67405000, 71101000, 75007000, 80582000, 85500000, 102000000, 109000000, 116000000, 123000000, 128872565, 120375000, 120600000, 123600000, 133000000, 143280000, 155270000, 167000000, 177200000, 188200000, 198200000, 182500000, 208200000, 224800000)/1000000,
                  year = c(1998:2023))


for(y in 2023:2014) {
  rosters_2014 <- load_rosters(y)
  rosters_2014$team <- clean_team_abbrs(rosters_2014$team)
  
  rosters_2014 <- rosters_2014 |> 
    mutate(team = case_when(
      team == "BAL" ~ "Ravens",
      team == "IND" ~ "Colts",
      team == "NE" ~ "Patriots",
      team == "PIT" ~ "Steelers",
      team == "KC" ~ "Chiefs",
      team == "LV" ~ "Raiders",
      team == "GB" ~ "Packers",
      team == "CHI" ~ "Bears",
      team == "NYG" ~ "Giants",
      team == "DAL" ~ "Cowboys",
      team == "MIA" ~ "Dolphins",
      team == "PHI" ~ "Eagles",
      team == "LA" ~ "Rams",
      team == "SF" ~ "49ers",
      team == "TEN" ~ "Titans",
      team == "MIN" ~ "Vikings",
      team == "ATL" ~ "Falcons",
      team == "SEA" ~ "Seahawks",
      team == "BUF" ~ "Bills",
      team == "JAX" ~ "Jaguars",
      team == "LAC" ~ "Chargers",
      team == "CAR" ~ "Panthers",
      team == "DET" ~ "Lions",
      team == "NYJ" ~ "Jets",
      team == "WAS" ~ "Commanders",
      team == "CIN" ~ "Bengals",
      team == "ARI" ~ "Cardinals",
      team == "NO" ~ "Saints",
      team == "CLE" ~ "Browns",
      team == "TB" ~ "Buccaneers",
      team == "DEN" ~ "Broncos",
      team == "HOU" ~ "Texans",
      TRUE ~ team  # Default case, keep original if no match
    )) |> select(full_name, team, season, gsis_id)
  
  
  
  Analysis_vs_prior_20112 <- contracts |> left_join(rosters_2014, by = c("gsis_id" = "gsis_id")) |> 
    filter(season == y) |> filter(years + year_signed >= y & year_signed <= y) |> 
    group_by(player, position) |> 
    filter(year_signed == max(year_signed)) |> 
    ungroup()
  
  for(x in 1:nrow(Analysis_vs_prior_20112)) {
    Analysis_vs_prior_20112$empty[x] = ifelse(is_empty(Analysis_vs_prior_20112$cols[[x]]), 1, 0)
  }   
  Analysis_vs_prior_20112 <- Analysis_vs_prior_20112 |> filter(empty == 0) |> select(-empty) |>  filter(year_signed != 0)
  
  Analysis_vs_prior_20112$team_yes <- Analysis_vs_prior_20112[[25]] %>%
    purrr::map(~ .x %>%
                 filter(year == y) %>%
                 pull(team))
  
  Analysis_vs_prior_20112 <- Analysis_vs_prior_20112 |> 
    filter(team.y == team_yes)
  
  
  Analysis_vs_prior_20112 <- Analysis_vs_prior_20112 %>%
    separate(team.x, into = c("Team_1", "Team_2", "Team_3"), sep = "/", fill = "right") |> 
    mutate(Team_1 = case_when(
      Team_1 == "BAL" ~ "Ravens",
      Team_1 == "IND" ~ "Colts",
      Team_1 == "NE" ~ "Patriots",
      Team_1 == "PIT" ~ "Steelers",
      Team_1 == "KC" ~ "Chiefs",
      Team_1 == "LV" ~ "Raiders",
      Team_1 == "GB" ~ "Packers",
      Team_1 == "CHI" ~ "Bears",
      Team_1 == "NYG" ~ "Giants",
      Team_1 == "DAL" ~ "Cowboys",
      Team_1 == "MIA" ~ "Dolphins",
      Team_1 == "PHI" ~ "Eagles",
      Team_1 == "LA" ~ "Rams",
      Team_1 == "SF" ~ "49ers",
      Team_1 == "TEN" ~ "Titans",
      Team_1 == "MIN" ~ "Vikings",
      Team_1 == "ATL" ~ "Falcons",
      Team_1 == "SEA" ~ "Seahawks",
      Team_1 == "BUF" ~ "Bills",
      Team_1 == "JAX" ~ "Jaguars",
      Team_1 == "LAC" ~ "Chargers",
      Team_1 == "CAR" ~ "Panthers",
      Team_1 == "DET" ~ "Lions",
      Team_1 == "NYJ" ~ "Jets",
      Team_1 == "WAS" ~ "Commanders",
      Team_1 == "CIN" ~ "Bengals",
      Team_1 == "ARI" ~ "Cardinals",
      Team_1 == "NO" ~ "Saints",
      Team_1 == "CLE" ~ "Browns",
      Team_1 == "TB" ~ "Buccaneers",
      Team_1 == "DEN" ~ "Broncos",
      Team_1 == "HOU" ~ "Texans",
      TRUE ~ Team_1  # Default case, keep original if no match
    ))  |> 
    mutate(Team_2 = case_when(
      Team_2 == "BAL" ~ "Ravens",
      Team_2 == "IND" ~ "Colts",
      Team_2 == "NE" ~ "Patriots",
      Team_2 == "PIT" ~ "Steelers",
      Team_2 == "KC" ~ "Chiefs",
      Team_2 == "LV" ~ "Raiders",
      Team_2 == "GB" ~ "Packers",
      Team_2 == "CHI" ~ "Bears",
      Team_2 == "NYG" ~ "Giants",
      Team_2 == "DAL" ~ "Cowboys",
      Team_2 == "MIA" ~ "Dolphins",
      Team_2 == "PHI" ~ "Eagles",
      Team_2 == "LA" ~ "Rams",
      Team_2 == "SF" ~ "49ers",
      Team_2 == "TEN" ~ "Titans",
      Team_2 == "MIN" ~ "Vikings",
      Team_2 == "ATL" ~ "Falcons",
      Team_2 == "SEA" ~ "Seahawks",
      Team_2 == "BUF" ~ "Bills",
      Team_2 == "JAX" ~ "Jaguars",
      Team_2 == "LAC" ~ "Chargers",
      Team_2 == "CAR" ~ "Panthers",
      Team_2 == "DET" ~ "Lions",
      Team_2 == "NYJ" ~ "Jets",
      Team_2 == "WAS" ~ "Commanders",
      Team_2 == "CIN" ~ "Bengals",
      Team_2 == "ARI" ~ "Cardinals",
      Team_2 == "NO" ~ "Saints",
      Team_2 == "CLE" ~ "Browns",
      Team_2 == "TB" ~ "Buccaneers",
      Team_2 == "DEN" ~ "Broncos",
      Team_2 == "HOU" ~ "Texans",
      TRUE ~ Team_2  # Default case, keep original if no match
    )) |> 
    mutate(Team_3 = case_when(
      Team_3 == "BAL" ~ "Ravens",
      Team_3 == "IND" ~ "Colts",
      Team_3 == "NE" ~ "Patriots",
      Team_3 == "PIT" ~ "Steelers",
      Team_3 == "KC" ~ "Chiefs",
      Team_3 == "LV" ~ "Raiders",
      Team_3 == "GB" ~ "Packers",
      Team_3 == "CHI" ~ "Bears",
      Team_3 == "NYG" ~ "Giants",
      Team_3 == "DAL" ~ "Cowboys",
      Team_3 == "MIA" ~ "Dolphins",
      Team_3 == "PHI" ~ "Eagles",
      Team_3 == "LA" ~ "Rams",
      Team_3 == "SF" ~ "49ers",
      Team_3 == "TEN" ~ "Titans",
      Team_3 == "MIN" ~ "Vikings",
      Team_3 == "ATL" ~ "Falcons",
      Team_3 == "SEA" ~ "Seahawks",
      Team_3 == "BUF" ~ "Bills",
      Team_3 == "JAX" ~ "Jaguars",
      Team_3 == "LAC" ~ "Chargers",
      Team_3 == "CAR" ~ "Panthers",
      Team_3 == "DET" ~ "Lions",
      Team_3 == "NYJ" ~ "Jets",
      Team_3 == "WAS" ~ "Commanders",
      Team_3 == "CIN" ~ "Bengals",
      Team_3 == "ARI" ~ "Cardinals",
      Team_3 == "NO" ~ "Saints",
      Team_3 == "CLE" ~ "Browns",
      Team_3 == "TB" ~ "Buccaneers",
      Team_3 == "DEN" ~ "Broncos",
      Team_3 == "HOU" ~ "Texans",
      TRUE ~ Team_3  # Default case, keep original if no match
    ))
  
  
  Analysis_vs_prior_201123 <- Analysis_vs_prior_20112 %>%
    mutate(
      Team_1 = ifelse(Team_1 == team_yes, Team_1, 
                      ifelse(Team_2 == team_yes, Team_2, 
                             ifelse(Team_3 == team_yes, Team_3, NA))),
      Team_2 = ifelse(team_yes == Team_2, NA, Team_2),
      Team_3 = ifelse(team_yes == Team_3, NA, Team_3)
    ) %>%
    filter(!is.na(Team_1)) |> 
    rename(team = Team_1) |> 
    select(!c(Team_2, Team_3, team_yes, team.y))  |> 
    group_by(team) |> 
    mutate(apy_rank = rank(-apy, ties.method = "first")) |> # Top 51 contracts
    filter(apy_rank <= 51) |> 
    mutate(top_5 = ifelse(apy_rank <= 5, 1, 0),
           top_10 = ifelse(apy_rank <= 10, 1, 0),
           middle_35 = ifelse(apy_rank <= 40 & apy_rank > 5, 1, 0),
           middle_30 = ifelse(apy_rank <= 40 & apy_rank > 10, 1, 0),
           rest = ifelse(apy_rank > 40, 1, 0)
           ) |> 
    ungroup() |> 
    select(-full_name) |> 
    mutate(cap_total = cap_ref$cap[which(cap_ref$year == y)], cap_pct = round(apy/cap_total, 3))
  
  active_contracts_analysis_Cap <- rbind(active_contracts_analysis_Cap, Analysis_vs_prior_201123) 

}

active_contracts_analysis_Cap_ready_For_save <- active_contracts_analysis_Cap |> select(-cols)

write.csv(active_contracts_analysis_Cap_ready_For_save, "contract_apy_by_year.csv")


analysis_totals <- active_contracts_analysis_Cap |> 
  group_by(season) |> 
  mutate(mean_cap_pct_top_5 = mean(if_else(top_5 == 1, cap_pct, NA_real_), na.rm = TRUE),
         mean_cap_pct_top_10 = mean(if_else(top_10 == 1, cap_pct, NA_real_), na.rm = TRUE),
         mean_cap_pct_middle_35 = mean(if_else(middle_35 == 1, cap_pct, NA_real_), na.rm = TRUE),
         mean_cap_pct_middle_30 = mean(if_else(middle_30 == 1, cap_pct, NA_real_), na.rm = TRUE),
         mean_cap_pct_rest = mean(if_else(rest == 1, cap_pct, NA_real_), na.rm = TRUE)) |> 
  ungroup() |> 
  select(season, mean_cap_pct_top_5, mean_cap_pct_top_10, mean_cap_pct_middle_35, 
         mean_cap_pct_middle_30, mean_cap_pct_rest) |> 
  pivot_longer(
    cols = starts_with("mean_cap"),
    names_to = "category",
    values_to = "mean_cap_pct"
  ) |> unique()


analysis_totals |> filter(category != "mean_cap_pct_top_10" & category != "mean_cap_pct_middle_30") |> 
  ggplot(aes(x = season, y = mean_cap_pct, group = category, color = category)) +
  geom_line() +
  labs(x = "Year", 
       y = "All Player APY Cap %", 
       title = "How Has the Trend in Mid Range Spending Changed?",
       subtitle = "Data via OTC | Cooper Davis") +
  theme_minimal() +  # Change the legend title here
  scale_color_manual(
    values = c("mean_cap_pct_top_5" = "darkred", 
               "mean_cap_pct_middle_35" = "darkblue", 
               "mean_cap_pct_rest" = "forestgreen"), 
    labels = c("mean_cap_pct_top_5" = "Top 5 Players", 
               "mean_cap_pct_middle_35" = "Middle 35 Players", 
               "mean_cap_pct_rest" = "Rest of Players")  # Assign new legend names
  ) +
  theme(plot.title = element_text(hjust=0.5, size=20),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.y = element_text(color="black"),
        axis.text.x = element_text(color="black"),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor.y = element_line(color = "gray90"),
        panel.grid.minor.x = element_blank(),
        legend.title = element_blank(),  # Customize legend title
        legend.text = element_text(size = 10)) +
  scale_x_continuous(breaks = seq(2014, 
                                  2024, 
                                  by = 1))
ggsave('/Users/Cooper/Documents/Folder for Contracts Project/MidRangeSpending.png', width = 10, height = 7, dpi = "retina")

