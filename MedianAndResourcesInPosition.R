library(dplyr)
library(nflreadr)
library(ggplot2)

future::plan("multisession")

contracts <- load_contracts(file_type = getOption("nflreadr.prefer", default = "rds")) #Load in contracts


#write.csv(contracts, "NFLContracts.csv", row.names = T) 

# Edit the contract data to see which players are within defined middle class
active_contracts_analysis <- contracts |> filter(is_active == T) |> group_by(position) |> 
  filter(apy > 0.794) 
active_contracts_analysis <- active_contracts_analysis |> 
  mutate(median_apy_at_signing = median(active_contracts_analysis$apy),
         median_lower = quantile(active_contracts_analysis$apy, 0.20, na.rm = T),
         median_higher = quantile(active_contracts_analysis$apy, 0.80, na.rm = T),
         within_median = ifelse(apy <= median_higher & apy >= median_lower, 1, 0),
         apy_cap_pct = apy/255.4) |> 
  ungroup()

middle_class_players_w_draft <- active_contracts_analysis |>  filter(within_median == 1)

# The 'middle class' is comprised of 1,237 of 2,497 active contracts

# Do the same thing without rookie contracts
active_contracts_rookies_analysis <- contracts |> filter(draft_year != year_signed) |> 
  filter(is_active == T) |> filter(apy > 0.794) 
active_contracts_rookies_analysis <- active_contracts_rookies_analysis |> 
  mutate(median_apy_at_signing = median(active_contracts_rookies_analysis$apy),
         median_lower = quantile(active_contracts_rookies_analysis$apy, 0.20, na.rm = T),
         median_higher = quantile(active_contracts_rookies_analysis$apy, 0.80, na.rm = T),
         within_median = ifelse(apy <= median_higher & apy >= median_lower, 1, 0),
         apy_cap_pct = apy/255.4)

middle_class_players_without_rookie <- active_contracts_rookies_analysis |>  filter(within_median == 1)

# The 'middle class' without rookies is comprised of 501 of 1450 active contracts
### Not really the middle 50%, might want to change this...



rosters_2024_rookies <- load_rosters(2024)

active_contracts_analysis <- active_contracts_analysis |> 
  mutate(in_bin_range = ifelse(apy_cap_pct > 0.004 & apy_cap_pct < 0.02, "Inside Range", "Outside Range"))

active_contracts_analysis |> 
  ggplot(aes(x = apy_cap_pct)) +
  geom_histogram(aes(y = ..count.. / sum(..count..),  fill = active_contracts_analysis$in_bin_range, color = active_contracts_analysis$in_bin_range), binwidth = 0.004, alpha = 0.7, boundary = 0) +
  theme_minimal() +
  geom_vline(xintercept = c(quantile(active_contracts_analysis$apy_cap_pct, 0.20, na.rm = T), 
                            quantile(active_contracts_analysis$apy_cap_pct, 0.80, na.rm = T)),
             color = "black", linetype = "dashed", size = 0.2) +
  scale_x_continuous(limits = c(0, NA)) +
  scale_fill_manual(values = c("Inside Range" = "#B19CD9", "Outside Range" = "skyblue"), guide = "none") +
  scale_color_manual(values = c("Inside Range" = "#B19CD9", "Outside Range" = "skyblue"), guide = "none") +
  labs(x = "APY Cap Percentage 2024", y = "Frequency", 
       title = "APY Cap Percentage Distribution 2024",
       subtitle = "Data via OTC | Cooper Davis | Middle Class Approximated by Purple Coloration (Exact by Dashed Lines)") +
  theme(panel.grid.major.y = element_line(size = 0.5),
        plot.title = element_text(hjust=0.5, size=20),
        axis.text.y = element_text(colour="black"),
        plot.subtitle = element_text(hjust=0.5))

ggsave("/Users/Cooper/Documents/Folder for Contracts Project/Graphs/APYCap2024.png", width = 10, height = 7, dpi = "retina")


### Do the same thing for no rookies 

active_contracts_rookies_analysis <- active_contracts_rookies_analysis |> 
  mutate(in_bin_range = ifelse(apy_cap_pct > 0.004 & apy_cap_pct < 0.04, "Inside Range", "Outside Range"))

active_contracts_rookies_analysis |> 
  ggplot(aes(x = apy_cap_pct)) +
  geom_histogram(aes(y = ..count.. / sum(..count..),  fill = active_contracts_rookies_analysis$in_bin_range, color = active_contracts_analysis$in_bin_range), binwidth = 0.004, alpha = 0.7, boundary = 0) +
  theme_minimal() +
  geom_vline(xintercept = c(quantile(active_contracts_rookies_analysis$apy_cap_pct, 0.20, na.rm = T), 
                            quantile(active_contracts_rookies_analysis$apy_cap_pct, 0.80, na.rm = T)),
             color = "black", linetype = "dashed", size = 0.2) +
  scale_x_continuous(limits = c(0, NA)) +
  scale_fill_manual(values = c("Inside Range" = "#B19CD9", "Outside Range" = "skyblue"), guide = "none") +
  scale_color_manual(values = c("Inside Range" = "#B19CD9", "Outside Range" = "skyblue"), guide = "none") +
  labs(x = "APY Cap Percentage 2024", y = "Frequency", 
       title = "APY Cap Percentage Distribution 2024",
       subtitle = "Data via OTC | Cooper Davis | Middle Class Approximated by Purple Coloration (Exact by Dashed Lines)") +
  theme(panel.grid.major.y = element_line(size = 0.5),
        plot.title = element_text(hjust=0.5, size=20),
        axis.text.y = element_text(colour="black"),
        plot.subtitle = element_text(hjust=0.5))

ggsave("/Users/Cooper/Documents/Folder for Contracts Project/Graphs/APYCap2024NoRookie.png", width = 10, height = 7, dpi = "retina")




#### Middle class analysis in 2010
rosters_2010_rookies <- load_rosters(2010)
contracts_2010 <- rosters_2010_rookies |> left_join(contracts, by = "gsis_id") |> 
  filter(year_signed < 2011 & year_signed > 0) |> 
  filter(year_signed + years > 2009) |> 
  summarize(season, full_name, position = position.x, gsis_id, entry_year, rookie_year, years, year_signed, value,
            apy, guaranteed, apy_cap_pct) |> 
  group_by(gsis_id) %>%
  filter(year_signed == max(year_signed)) %>%
  ungroup()

## Middle class with rookies (only got  contracts)
active_contracts_rookies_analysis_2010 <- contracts_2010 |> group_by(position) |> 
  mutate(median_apy_at_signing = median(apy),
         median_lower = median_apy_at_signing*2/3,
         median_higher = median_apy_at_signing*2,
         within_median = ifelse(apy <= median_higher & apy >= median_lower, 1, 0)) 

middle_class_players_w_draft_2010 <- active_contracts_rookies_analysis_2010 |>  filter(within_median == 1)

# The 'middle class' is comprised of 277 of 850 active contracts

# Middle class without rookies (only got  contracts)
active_contracts_no_rookies_analysis_2010 <- contracts_2010 |> filter(entry_year != year_signed) |> group_by(position) |> 
  mutate(median_apy_at_signing = median(apy),
         median_lower = median_apy_at_signing*2/3,
         median_higher = median_apy_at_signing*2,
         within_median = ifelse(apy <= median_higher & apy >= median_lower, 1, 0)) 

middle_class_players_without_draft <- active_contracts_no_rookies_analysis_2010 |>  filter(within_median == 1)

# The 'middle class' is comprised of 159 of 277 active contracts
# Includes almost all QBs, VERY BAD


middle_class_players_w_draft_2010 |> group_by(position) |> 
  summarize(position, mean_apy = mean(apy_cap_pct)) |> ungroup() |> unique() |> 
  ggplot(aes(fct_reorder(position, mean_apy, .desc = TRUE), mean_apy)) +
    geom_col()


#### EDITING 2014 ROSTERS TO HAVE THE RIGHT STUFF

rosters_2014 <- load_rosters(2014)
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
  filter(season == 2014) |> filter(years + year_signed >= 2014 & year_signed < 2015) |> 
  group_by(player, position) |> 
  filter(year_signed == max(year_signed)) |> 
    ungroup()

for(x in 1:nrow(Analysis_vs_prior_20112)) {
  Analysis_vs_prior_20112$empty[x] = ifelse(is_empty(Analysis_vs_prior_20112$cols[[x]]), 1, 0)
}   
Analysis_vs_prior_20112 <- Analysis_vs_prior_20112 |> filter(empty == 0) |> select(-empty) |>  filter(year_signed != 0)

Analysis_vs_prior_20112$team_yes <- Analysis_vs_prior_20112[[25]] %>%
  purrr::map(~ .x %>%
               filter(year == 2014) %>%
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
  rename(Team = Team_1) |> 
  select(!c(Team_2, Team_3, team_yes, team.y)) 


Analysis_vs_prior_201123_max <- Analysis_vs_prior_201123 |>
  group_by(position) |> 
  mutate(max_apy_cap_pct = max(apy_cap_pct)) |> 
  summarize(position, max_apy_cap_pct, year = season) |> unique() |> 
  ungroup()


#### Same thing for 2024 is a lot easier
contracts_max_2024 <- active_contracts_analysis |> 
  group_by(position) |> 
  mutate(max_apy_cap_pct = max(apy_cap_pct)) |> 
  summarize(position, max_apy_cap_pct, year = 2024) |> unique() |> 
  ungroup()


### Combine them
Combination_max <- contracts_max_2024 |> rbind(Analysis_vs_prior_201123_max)

Combination_max_final <- Combination_max |> 
  pivot_wider(names_from = year, values_from = max_apy_cap_pct, names_prefix = "pct_") |> 
  mutate(difference_mAPYcp = pct_2024 - pct_2014,
         mAPYcp_2024 = pct_2024,
         mAPYcp_b2011 = pct_2014) |> 
  select(position, difference_mAPYcp)

Combination_max <- Combination_max |> left_join(Combination_max_final, by = c("position"))


## Top 5-10 positional rankings

## Top 3rd, middle 3rd, etc. WITH THE MIDDLE CLASS



#desired_order <- c("QB", "RB", "WR", "FB", "TE", "LT", "LG", "C", "RG", "RT", "ED", "IDL", "LB", "CB", "S", "K", "P", "LS")

#Analysis_vs_prior_2011 <- Analysis_vs_prior_2011 |> 
#  mutate(position = factor(position, levels = desired_order))

Combination_max |> 
  ggplot(aes(fct_reorder(position, difference_mAPYcp, .desc = TRUE), max_apy_cap_pct, fill = factor(ifelse(year == 2014, "2014", "Current Day")))) +
  geom_col(position="dodge", color='gray50', width=0.7) + 
  scale_fill_manual(values = c("Current Day" = "purple", "2014" = "darkblue"), 
                    name = "Time Period") +
  theme(plot.title = element_text(hjust=0.5, size=20),
        axis.text.y = element_text(colour="black"),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank(),
        plot.subtitle = element_text(hjust=0.5),
        legend.position = "top",
        legend.title = element_blank()) +
  labs(x = "Position", y = "Max APY Cap%", 
       title = "How Have Resources put into Positions changed?", 
       subtitle = "Data via OverTheCap | Cooper Davis") +
  geom_text(aes(label = round(max_apy_cap_pct, 3)), 
            position = position_dodge2(width = 0.7), 
            vjust = 1.5, size = 1.75, color = "white") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray70")


ggsave('/Users/Cooper/Documents/Folder for Contracts Project/PositionResources.png', width = 10, height = 7, dpi = "retina")
  


### Percentage by year

contracts_draft_data <- active_contracts_analysis_Cap_ready_For_save |> filter(draft_year > 2013) |> 
  mutate(rookie_contract = ifelse(year_signed + years > season & year_signed == draft_year, 1, 0))
  mutate(rookie = ifelse(draft_year == season & season == year_signed, 1, 0),
         rookie_contract = ifelse(draft_year == season & season == year_signed, 1, 0),) |> 
  
  group_by(season) |> 
  mutate(num_rookies_by_year = sum(rookie)) |> 
  ungroup()

  



