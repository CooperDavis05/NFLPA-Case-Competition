## For this, the plan is to take the cash spending of teams and then use it 
### 2021 to 2023
#### Can do this, just have to check with OTC

future::plan("multisession")

contracts <- load_contracts(file_type = getOption("nflreadr.prefer", default = "rds"))

## Regular contracts

active_contracts <- load_contracts(file_type = getOption("nflreadr.prefer", default = "rds")) |> 
  filter(value >= 0.795) |> 
  filter(year_signed >= 2010)

for(x in 1:nrow(active_contracts)) {
  active_contracts$empty[x] = ifelse(is_empty(active_contracts$cols[[x]]), 1, 0)
}   
active_contracts2 <- active_contracts |> filter(empty == 0) |> select(-empty) |>  filter(year_signed != 0)


for(y in 2021:2023) { # Loop entirely through and figure out base salaries
  active_contracts2[[paste0("cash_paid_", y)]] <- active_contracts2[[25]] %>%
    purrr::map(~ .x %>%
                 filter(year == y) %>%
                 pull(as.numeric(cash_paid)))
}

for(y in 2021:2023) { # Loop entirely through and figure out base salaries
  active_contracts2[[paste0("team_", y)]] <- active_contracts2[[25]] %>%
    purrr::map(~ .x %>%
                 filter(year == y) %>%
                 pull(team))
}

active_contracts2 <- active_contracts2 %>%
  mutate(across(everything(), ~ ifelse(lengths(.x) == 0, NA, .x)))

active_contracts2[[26]] <- unlist(active_contracts2[[26]])
active_contracts2[[27]] <- unlist(active_contracts2[[27]])
active_contracts2[[28]] <- unlist(active_contracts2[[28]])
active_contracts2[[29]] <- unlist(active_contracts2[[29]])
active_contracts2[[30]] <- unlist(active_contracts2[[30]])
active_contracts2[[31]] <- unlist(active_contracts2[[31]])


cash_paid_good <- active_contracts2 |>
  select(cash_paid_2021, cash_paid_2022, cash_paid_2023, team_2021, team_2022, team_2023) |> 
  unique() |> 
  group_by(team_2021) |> 
  mutate(team_cash_spent_2021 = sum(cash_paid_2021)) |> 
  ungroup() |> 
  group_by(team_2022) |> 
  mutate(team_cash_spent_2022 = sum(cash_paid_2022)) |> 
  ungroup() |> 
  group_by(team_2023) |> 
  mutate(team_cash_spent_2023 = sum(cash_paid_2023)) |> 
  ungroup() 





########################################################################################################################################
########################################################################################################################################
########################################################################################################################################
########################################################################################################################################
########################################################################################################################################

# Create a function that cleans team names

team_id_clean <- function(team_id) {
  case_when(
    team_id == 1 ~ "Buffalo Bills",
    team_id == 2 ~ "New England Patriots",
    team_id == 3 ~ "Miami Dolphins",
    team_id == 4 ~ "New York Jets",
    team_id == 5 ~ "Baltimore Ravens",
    team_id == 6 ~ "Cleveland Browns",
    team_id == 7 ~ "Cincinnati Bengals",
    team_id == 8 ~ "Pittsburgh Steelers",
    team_id == 9 ~ "Houston Texans",
    team_id == 10 ~ "Indianapolis Colts",
    team_id == 11 ~ "Jacksonville Jaguars",
    team_id == 12 ~ "Tennessee Titans",
    team_id == 13 ~ "Denver Broncos",
    team_id == 14 ~ "Kansas City Chiefs",
    team_id == 15 ~ "Los Angeles Chargers",
    team_id == 16 ~ "Las Vegas Raiders",
    team_id == 17 ~ "New York Giants",
    team_id == 18 ~ "Dallas Cowboys",
    team_id == 19 ~ "Philadelphia Eagles",
    team_id == 20 ~ "Washington Commanders",
    team_id == 21 ~ "Chicago Bears",
    team_id == 22 ~ "Detroit Lions",
    team_id == 23 ~ "Green Bay Packers",
    team_id == 24 ~ "Minnesota Vikings",
    team_id == 25 ~ "Atlanta Falcons",
    team_id == 26 ~ "Carolina Panthers",
    team_id == 27 ~ "New Orleans Saints",
    team_id == 28 ~ "Tampa Bay Buccaneers",
    team_id == 29 ~ "Arizona Cardinals",
    team_id == 30 ~ "Los Angeles Rams",
    team_id == 31 ~ "San Francisco 49ers",
    team_id == 32 ~ "Seattle Seahawks",
    TRUE ~ "Unknown Team" 
  )
  
}


#### REDO HOW THINK ABT THIS

tcl <- teams_colors_logos

contracts_Jason <- read.csv("Cap2013_Present.csv")

contracts_join <- contracts |> select(player, otc_id)

date_of_contracts <- read.csv("OTC_Contracts.csv") |> 
  select(ID, date_signed, signing_bonus)



######## IMPORTANT BECAUSE IT IS MARCH 17th OF 21 TO MARCH 12th OF 24 (13th at 4:00pm)

contracts_jason_scraped <- contracts_join |> right_join(contracts_Jason, by = c("otc_id" = "player_id")) |> 
  unique() |> mutate(team = team_id_clean(team_id)) |>
  right_join(date_of_contracts, by = c("contract_id" = "ID")) |> 
  group_by(contract_id) |> 
  mutate(Signing_bonus_real = sum(PB)) |> 
  ungroup() |> 
  summarize(player, team, otc_id, contract_id, Position = Position, date_signed, Year_defined = Year, cash_paid = Cshpay, signing_bonus, Signing_bonus_real) |> 
  separate(date_signed, into = c("Month", "Day", "Year"), sep = "/", fill = "right") |> 
  mutate(Month = as.numeric(Month), Day = as.numeric(Day),  Year = as.numeric(Year)) |>
  filter(Year_defined %in% c(2021, 2022, 2023, 2024)) |> 
  mutate(date_sum = Year*365 + Day + Month*30,
         Year = Year+2000) |> 
  mutate(sign_date_before = ifelse(date_sum < 8863, 1, 0)) |> 
  filter(sign_date_before == 1) |> 
  filter(!(Year_defined == 2024 & Year != 2024)) |> 
  mutate(signed_in_2024 = ifelse(Year_defined == 2024 & Year == 2024, 1, 0)) |> 
  mutate(signing_bonus = ifelse(signing_bonus == 0, Signing_bonus_real, signing_bonus),
         cash_paid = ifelse(signed_in_2024 == 1, signing_bonus, cash_paid)) |> 
  select(-Signing_bonus_real) |> 
  mutate(signed_in_2021_before_it = ifelse(Year_defined == 2021 & Year == 2021 & date_sum <= 7772, 1, 0),
         cash_paid = ifelse(signed_in_2021_before_it == 1, cash_paid - signing_bonus, cash_paid)) |> 
  select(-signed_in_2021_before_it, -signed_in_2024, -sign_date_before)
  
  #mutate(Year_Count = case_when(
  #  date_sum > 7771 & date_sum < 8136  ~ "Year 1",
  #  date_sum >= 8136 & date_sum < 8500  ~ "Year 2",
  #  date_sum >= 8500  ~ "Year 3")) |> 
  


teams_compliance <- contracts_jason_scraped |> 
  group_by(team) |> 
  mutate(sum_team_cash = sum(cash_paid)) |> 
  ungroup() |> 
  select(team, sum_team_cash) |> unique() |> 
  mutate(total_cap_3_year = 615500000,
         cash_pct = sum_team_cash/total_cap_3_year,
         mean_pct = mean(cash_pct),
         over_100 = cash_pct >= 1,
         percentage_over_100 = mean(over_100))


teams_colors_logos_real <- teams_colors_logos %>% filter(team_abbr != "LA") %>% 
  filter(team_abbr != "OAK") %>% filter(team_abbr != "STL") %>% filter(team_abbr != "SD") %>% select(-team_abbr) |> 
  select(team_name, team_color, team_color2, team_logo_espn)

teams_compliance <- teams_compliance |> 
  left_join(teams_colors_logos_real, by = c('team' = 'team_name'))

compliance_graph <- teams_compliance |> mutate(sum_team_cash = sum_team_cash/1000000, total_cap_3_year = total_cap_3_year/1000000) |> 
  ggplot() + 
  geom_bar(aes(x = sum_team_cash, y = fct_reorder(team, sum_team_cash), fill = team_color, color = team_color2), alpha = 0.8, stat = "identity") +
  scale_color_identity(aesthetics = c("fill", "color")) +
  geom_image(aes(x = sum_team_cash, y = fct_reorder(team, sum_team_cash), image = team_logo_espn), asp = 16/9, size = 0.025) +
  theme_minimal() +
  scale_x_continuous(expand = c(0, 0),  # Remove all padding from the x-axis
                     limits = c(0, 1000),  # Set the range from 0 to 1000
                     labels = scales::label_comma()) +
  theme(
    strip.text = element_text(size = 14, face = "bold"),      # Make facet labels bold to resemble team logos
    strip.background = element_blank(),                       # Remove facet box background
    axis.text.x = element_text(size = 9),                    # Smaller x-axis text
    axis.ticks.y = element_blank(),                    # Minor x-axis ticks
    axis.ticks.x = element_line(size = 0.5),                   # Minor x-axis ticks
    axis.text.y = element_blank(),                    # Smaller y-axis text
    panel.grid.minor = element_blank(),                       # Remove minor grid lines
    plot.title = element_text(hjust=0.5, size=20),
    plot.subtitle = element_text(hjust=0.5)) +
    labs(x = "Cash Spending Sum (In Millions)", y = "Team", 
         title = "Cash Spending Over the Last 3 Year Spending Window",
         subtitle = "Data via NFLReadR + OTC | Cooper Davis | 2021-2023 League Years") +
  geom_vline(xintercept = 615.5*0.9, linetype = "dashed", color = "gray", size = 0.75, alpha = 0.5) +
  geom_vline(xintercept = 615.5*0.95, linetype = "dashed", color = "lightgreen", size = 0.75, alpha = 0.5) +
  geom_vline(xintercept = 615.5, linetype = "dashed", color = "black", size = 0.75, alpha = 0.5)
  
compliance_graph

ggsave("/Users/Cooper/Documents/Folder for Contracts Project/Graphs/TeamSpending.png", width = 10, height = 7, dpi = "retina", plot = compliance_graph)



void_graph |>  
  ggplot(aes(x = sum_voids_cap, y = fct_reorder(team, sum_voids_cap))) + 
  geom_bar(aes(fill = team_color, color = team_color2), stat = "identity") +
  geom_text(aes(label = odds, color = if_else(sum_voids_cap <= 13, 'black', 'white'), hjust = sum_voids_cap > 13), nudge_x = 5*sign((-void_graph$sum_voids_cap+13)*34), size = 2.5, fontface = "bold") +
  scale_color_identity(aesthetics = c("fill", "color")) +
  geom_image(aes(x = sum_voids_cap, image = team_logo_espn), asp = 16/9, size = 0.025) +
  theme(panel.grid.major.y = element_line(size = 0.5)) +
  theme(plot.title = element_text(hjust=0.5)) +
  theme(plot.title = element_text(size=20)) +
  theme(axis.text.y = element_text(colour="black")) +
  theme(plot.subtitle = element_text(hjust=0.5), ) +
  labs(x = "Sum of Void Year Cap Hits (in Millions)", y = "Team (Super Bowl Odds in Bar) ", title = "Amount of Cap Space Each Team Has in Void Years", subtitle = "Data via OverTheCap, Fanduel | @CDFBAnalysis")




theme_minimal() +
  theme(
    strip.text = element_text(size = 14, face = "bold"),      # Make facet labels bold to resemble team logos
    strip.background = element_blank(),                       # Remove facet box background
    axis.text.x = element_text(size = 9),                    # Smaller x-axis text
    axis.ticks.y = element_line(size = 0.5),                  # Minor x-axis ticks
    axis.ticks.x = element_line(size = 0.5),                  # Minor x-axis ticks
    axis.text.y = element_text(size = 10),                    # Smaller y-axis text
    panel.grid.minor = element_blank(),                       # Remove minor grid lines
    plot.title = element_text(hjust=0.5, size=20),
    plot.subtitle = element_text(hjust=0.5),
    legend.position = "NONE") +
  geom_text(aes(label = paste0(round(pct_cap*100, 2), "%")), 
            position = position_dodge2(width = 0.7), 
            vjust = 1.5, size = 2.25, color = "white") +
  labs(x = "Year", y = "% of Cap", 
       title = "How Did Minimum Salaries Change Throughout the Last CBA?", 
       subtitle = "Data via NFLReadR + OTC | Cooper Davis") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray")
min_salary_inc_before_all_graph
### The percentage over 100 is 0.8125

### The mean percentage is 1.064785


#total_contracts from RookieContractAnalysis

total_contracts_CB <- total_contracts |> 
  filter(Position == "CB") |> 
  filter(year_signed %in% 2020:2024) |> 
  summarize(player, otc_id, Years, Total, APY, year_signed, signing_bonus, signing_bonus_pct = signing_bonus/Total) |> 
  group_by(player) |> 
  filter(year_signed == max(year_signed)) |>
  ungroup() |> 
  unique() |> 
  filter(Years > 2) |> 
  filter(APY >= 19000000)
  



