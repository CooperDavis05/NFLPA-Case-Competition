future::plan("multisession")

contracts <- load_contracts(file_type = getOption("nflreadr.prefer", default = "rds"))

active_contracts <- contracts |> filter(is_active == T) |> 
  filter(value >= 0.795)

active_contracts_rookies_analysis <- contracts |> filter(draft_year != year_signed) |> 
  filter(is_active == T) |> filter(apy > 0.794) 
active_contracts_rookies_analysis <- active_contracts_rookies_analysis |> 
  mutate(median_apy_at_signing = median(active_contracts_rookies_analysis$apy),
         median_lower = quantile(active_contracts_rookies_analysis$apy, 0.20, na.rm = T),
         median_higher = quantile(active_contracts_rookies_analysis$apy, 0.80, na.rm = T),
         within_median = ifelse(apy <= median_higher & apy >= median_lower, 1, 0),
         apy_cap_pct = apy/255.4)


## Load in 2014 Contracts
contracts_Jason_2014 <- read.csv("Cap2013_Present.csv") |> 
  filter(Year == 2014)

contracts_join <- contracts |> select(player, otc_id, draft_year)

contracts_Jason_2014_joined <- contracts_join |>  # Filtered contracts from 2014
  left_join(contracts_Jason_2014, by = c("otc_id" = "player_id")) |> 
  filter(!is.na(ID)) |> unique()

apy_of_contracts_2014 <- read.csv("OTC_Contracts.csv") |> select(player_id, APY, year_signed)

contracts_Jason_2014_joined2 <- contracts_Jason_2014_joined |>  # Add that year's APY
  left_join(apy_of_contracts_2014, by = c("otc_id" = "player_id")) |> 
  filter(year_signed <= Year) |> 
  group_by(player, APY) |> 
  mutate(contract_on = Year-year_signed) |> 
  ungroup() |> 
  group_by(player) |> 
  mutate(min_year = min(contract_on)) |> 
  ungroup() |> 
  filter(contract_type != "Practice") |> 
  filter(min_year == contract_on) |> unique() |> 
  group_by(player) |> 
  mutate(max_APY = max(APY)) |> 
  slice(1) |> 
  ungroup() |> 
  filter(max_APY == APY) |> unique() |> 
  filter(draft_year != year_signed) |>  ###### FILTERING OUT ROOKIE CONTRACTS
  summarize(player, Year, otc_id, APY, total_cap = 133000000, 
            apy_cap_pct = round(APY/total_cap, 5)) |> 
  filter(APY >= 420000)

quantile(contracts_Jason_2014_joined2$apy_cap_pct, 0.20)
quantile(contracts_Jason_2014_joined2$apy_cap_pct, 0.80)

contracts_Jason_2014_joined2 |> 
  filter(APY >= 0.5577*1000000 & APY <= 5.3755*1000000) |> 
  count()

## This is 1013 players compared to 1104
## Median is 0.5577 (0.429%) to 5.3755 (4.135%)
## The difference in the middle classes is 0.136% of the cap
#### Which is around 0.347344
#### The new bounds are 1.095666 to 10.56079

# Before
active_contracts_rookies_analysis |> 
  filter(apy >= 1.095666 & apy <= 10.56079) |> 
  count()
#685

# After
active_contracts_rookies_analysis |> 
  filter(apy >= 1.125 & apy <= 10.25) |> 
  count()
#680


## The loss is 5 players (0.0730%), but that's purely due to our bounds
## Lets check another way

# Graph

mutated_Jason2 <- contracts_Jason_2014_joined2 |> 
  summarize(year = 2014, apy_cap_pct)

mutated_active2 <- active_contracts_rookies_analysis |> 
  summarize(year = 2024, apy_cap_pct)

combined2 <- bind_rows(mutated_Jason2, mutated_active2)

ggplot() +
  geom_histogram(data = combined2[combined2$year == 2024, ], aes(x = apy_cap_pct, y = ..count.. / sum(..count..),  fill = "2024"), color = "skyblue", binwidth = 0.004, boundary = 0) +
  geom_histogram(data = combined2[combined2$year == 2014, ], aes(x = apy_cap_pct, y = ..count.. / sum(..count..),  fill = "2014"), color = "gray", binwidth = 0.004, alpha = 0.7, boundary = 0) +
  theme_minimal() +
  geom_vline(xintercept = c(quantile(combined2$apy_cap_pct[combined2$year == 2024], 0.20, na.rm = T), 
                            quantile(combined2$apy_cap_pct[combined2$year == 2024], 0.80, na.rm = T)),
             color = "darkred", linetype = "dashed", size = 0.2) +
  geom_vline(xintercept = c(quantile(combined2$apy_cap_pct[combined2$year == 2014], 0.20, na.rm = T), 
                            quantile(combined2$apy_cap_pct[combined2$year == 2014], 0.80, na.rm = T)),
             color = "forestgreen", linetype = "dashed", size = 0.2) +
  scale_x_continuous(limits = c(0, NA)) +
  scale_fill_manual(values = c("2024" = "skyblue", "2014" = "gray"), breaks = c("2014", "2024") )+ 
  labs(x = "APY Cap Percentage", y = "Frequency", 
       title = "Rookie-Excluded APY Cap Percentage Distribution 2014 vs. 2024",
       caption = "Data via OTC | Cooper Davis | Green Dashed Lines are 2014 Middle Class | Red Dashed Lines are 2024 Middle Class") +
  theme(panel.grid.major.y = element_line(size = 0.5),
        plot.title = element_text(hjust=0.5, size=20),
        axis.text.y = element_text(colour="black"),
        plot.subtitle = element_text(hjust=0.5),
        legend.title = element_blank(),
        legend.position = "top")


ggsave('/Users/Cooper/Documents/Folder for Contracts Project/Graphs/MiddleClassChangeNoRookies.png', width = 10, height = 7, dpi = "retina")


##### LOOK AT IT WITH ROOKIES


contracts_Jason_2014_joined2_wr <- contracts_Jason_2014_joined |>  # Add that year's APY
  left_join(apy_of_contracts_2014, by = c("otc_id" = "player_id")) |> 
  filter(year_signed <= Year) |> 
  group_by(player, APY) |> 
  mutate(contract_on = Year-year_signed) |> 
  ungroup() |> 
  group_by(player) |> 
  mutate(min_year = min(contract_on)) |> 
  ungroup() |> 
  filter(contract_type != "Practice") |> 
  filter(min_year == contract_on) |> unique() |> 
  group_by(player) |> 
  mutate(max_APY = max(APY)) |> 
  slice(1) |> 
  ungroup() |> 
  filter(max_APY == APY) |> unique() |> 
  #filter(draft_year != year_signed) |>  ###### FILTERING OUT ROOKIE CONTRACTS
  summarize(player, Year, otc_id, APY, total_cap = 133000000, 
            apy_cap_pct = round(APY/total_cap, 5)) |> 
  filter(APY >= 420000)


quantile(active_contracts_analysis$apy_cap_pct, 0.20, na.rm = T)*255.4
# 0.3858991%
quantile(active_contracts_analysis$apy_cap_pct, 0.80, na.rm = T)*255.4
# 2.151862%

contracts_Jason_2014_joined2_wr |> 
  filter(APY >= 0.528*1000000 & APY <= 3.21*1000000) |> 
  count()

quantile(contracts_Jason_2014_joined2_wr$apy_cap_pct, 0.20, na.rm = T)
quantile(contracts_Jason_2014_joined2_wr$apy_cap_pct, 0.80, na.rm = T)
quantile(contracts_Jason_2014_joined2_wr$apy_cap_pct, 0.20, na.rm = T)*130
quantile(contracts_Jason_2014_joined2_wr$apy_cap_pct, 0.80, na.rm = T)*130

## This is 1903 players compared to 1993
## Median is 0.5278 (0.406%) to 3.20996 (2.4692%)
## The difference in the middle classes is 0.0719% of the cap
#### Which is around 1.837549

quantile(contracts_Jason_2014_joined2_wr$apy_cap_pct, 0.20, na.rm = T)*255.4
quantile(contracts_Jason_2014_joined2_wr$apy_cap_pct, 0.80, na.rm = T)*255.4
#### The new bounds are 1.036924 to 6.306337

# After
active_contracts_analysis |> 
  filter(apy >= 1.036924 & apy <= 6.306337) |> 
  count()
## 1130

# Before
active_contracts_analysis |> 
  filter(apy >= 0.9855864 & apy <= 5.495854) |> 
  count()
## 1195

### Middle Class Shrinks by 65 players, or 5.44%

mutated_Jason <- contracts_Jason_2014_joined2_wr |> 
  summarize(year = 2014, apy_cap_pct)

mutated_active <- active_contracts_analysis |> 
  summarize(year = 2024, apy_cap_pct)

combined <- bind_rows(mutated_Jason, mutated_active)


ggplot() +
  geom_histogram(data = combined[combined$year == 2024, ], aes(x = apy_cap_pct, y = ..count.. / sum(..count..),  fill = "2024"), binwidth = 0.004, boundary = 0) +
  geom_histogram(data = combined[combined$year == 2014, ], aes(x = apy_cap_pct, y = ..count.. / sum(..count..),  fill = "2014"), alpha = 0.7, binwidth = 0.004, boundary = 0) +
  theme_minimal() +
  geom_vline(xintercept = c(quantile(combined$apy_cap_pct[combined$year == 2024], 0.20, na.rm = T), 
                            quantile(combined$apy_cap_pct[combined$year == 2024], 0.80, na.rm = T)),
             color = "darkred", linetype = "dashed", size = 0.2) +
  geom_vline(xintercept = c(quantile(combined$apy_cap_pct[combined$year == 2014], 0.20, na.rm = T), 
                            quantile(combined$apy_cap_pct[combined$year == 2014], 0.80, na.rm = T)),
             color = "forestgreen", linetype = "dashed", size = 0.2) +
  scale_x_continuous(limits = c(0, NA)) +
  scale_fill_manual(values = c("2024" = "skyblue", "2014" = "gray"), breaks = c("2014", "2024") )+
  labs(x = "APY Cap Percentage", y = "Frequency", 
       title = "Rookie-Included APY Cap Percentage Distribution 2014 vs. 2024",
       caption = "Data via OTC | Cooper Davis | Green Dashed Lines are 2014 Middle Class | Red Dashed Lines are 2024 Middle Class") + 
  theme(panel.grid.major.y = element_line(size = 0.5),
        plot.title = element_text(hjust=0.5, size=20),
        axis.text.y = element_text(colour="black"),
        plot.subtitle = element_text(hjust=0.5),
        legend.title = element_blank(),
        legend.position = "top")

ggsave('/Users/Cooper/Documents/Folder for Contracts Project/Graphs/MiddleClassChangeRookies.png', width = 10, height = 7, dpi = "retina")



### Sum of APY Cap under the Curve
contracts_Jason_2014_joined2_wr |> 
  filter(APY >= 0.528*1000000 & APY <= 3.21*1000000) |> 
  summarize(total = sum(apy_cap_pct), num = sum(n()))

#9.50

active_contracts_analysis |> 
  filter(apy >= 0.985 & apy <= 5.49500) |> 
  summarize(total = sum(apy_cap_pct), num = sum(n()))

# 8.51
