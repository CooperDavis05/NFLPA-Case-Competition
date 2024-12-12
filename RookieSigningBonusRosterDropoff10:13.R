library(nflreadr)



contracts <- load_contracts(file_type = getOption("nflreadr.prefer", default = "rds"))


## Since 2016, only one without missing data
contracts_rookies <- contracts |> filter(year_signed == draft_year) |> 
  summarize(player, years, guaranteed, draft_team, 
            year_signed, draft_year, draft_round, draft_overall) |> 
  group_by(draft_year) |> filter(years > 3 & !is.na(draft_round) & draft_year > 2016 &
                                   guaranteed > 0) |> 
  mutate(guaranteed_mean = mean(guaranteed)) |> 
  ungroup() |> unique()

cut_down_cr <- contracts_rookies |> select(draft_year, guaranteed_mean) |> unique() |> 
  arrange(draft_year) |>  # Ensure the data is sorted by draft_year
  mutate(guaranteed_change = (guaranteed_mean - lag(guaranteed_mean))/lag(guaranteed_mean)) |> 
  filter(draft_year > 2017)

cut_down_cr |> arrange(draft_year) |> 
  ggplot(aes(draft_year, guaranteed_change*100)) +
  geom_col(position="dodge", color='#0081c6', fill = "#003069", width=0.7) +
  theme(plot.title = element_text(hjust=0.5, size=20),
        axis.text.y = element_text(colour="black"),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank(),
        plot.subtitle = element_text(hjust=0.5),
        legend.position = "top",
        axis.text.x = ggtext::element_markdown(),
        legend.title = element_blank()
  ) + 
  labs(x = "Year", y = "Percent Change in Mean Guaranteed Draft Pick Money", 
       title = "How has Mean Guaranteed Draft Money Changed?", 
       subtitle = "Data via NFLReadR + OTC | Cooper Davis") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray")


ggsave('/Users/Cooper/Documents/Folder for Contracts Project/PercentChangeGuaranteed.png', width = 10, height = 7, dpi = "retina")


############# ROSTER %

rosters_2024 <- load_rosters(2024) 




## Week 8/9 Players Who Were active
rosters_2024_prep_bin <- rosters_2024 |> 
  mutate(years_exp_bin = cut(years_exp,
    breaks = c(0, 1, 2, 3, 4, 7, Inf),
    labels = c("0", "1", "2", "3", "4-7", "7+"),
    right = F)) |> filter((status == "ACT" |  status == "INA") & (week == 9 | week == 8))


######## For each team

bins_2024_by_team <- rosters_2024_prep_bin |> 
  group_by(team) |>
  mutate(num_players = n()) |> 
  group_by(years_exp_bin, team) |>
  mutate(bin_pct = n()/num_players) |> 
  select(team, years_exp_bin, bin_pct) |> unique()
  
bins_2024_by_team |> 
  ggplot(aes(years_exp_bin, bin_pct*100)) +
  geom_col(position="dodge", color='#0081c6', fill = "#003069", width=0.7) +
  facet_wrap(~team, ncol=8) +
  theme(plot.title = element_text(hjust=0.5, size=20),
        axis.text.y = element_text(colour="black"),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank(),
        plot.subtitle = element_text(hjust=0.5),
        legend.position = "top",
        axis.text.x = ggtext::element_markdown(),
        legend.title = element_blank()
  ) + 
  labs(x = "Accrued Seasons", y = "Roster Percentage", 
       title = "Roster Percentage by Accrued Season", 
       subtitle = "Accured Seasons are Binned According to Minimum Salary Bands", 
       caption = "Data via NFLReadR + OTC | Cooper Davis") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray")
  
ggsave('/Users/Cooper/Documents/Folder for Contracts Project/RosterPctTeam.png', width = 10, height = 7, dpi = "retina")

######## Overall

bins_2024_overall <- rosters_2024_prep_bin |> 
  mutate(num_players = n()) |> 
  group_by(years_exp_bin) |>
  mutate(bin_pct = n()/num_players) |> 
  select(years_exp_bin, bin_pct) |> unique()

bins_2024_overall |> 
  ggplot(aes(years_exp_bin, bin_pct*100)) +
  geom_col(position="dodge", color='#0081c6', fill = "#003069", width=0.7) +
  theme(plot.title = element_text(hjust=0.5, size=20),
        axis.text.y = element_text(colour="black"),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank(),
        plot.subtitle = element_text(hjust=0.5),
        legend.position = "top",
        axis.text.x = ggtext::element_markdown(),
        legend.title = element_blank()
  ) + 
  labs(x = "Accrued Seasons", y = "Roster Percentage", 
       title = "Roster Percentage by Accrued Season", 
       subtitle = "Accured Seasons are Binned According to Minimum Salary Bands", 
       caption = "Data via NFLReadR + OTC | Cooper Davis") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray")

ggsave('/Users/Cooper/Documents/Folder for Contracts Project/RosterPctOverall.png', width = 10, height = 7, dpi = "retina")



min_salary_inc <- tibble(
  `Accrued_Seasons` = c("0", "1", "2", "3", "4", "5", "6", "7"),
  `2019` = c(495,	570, 645, 720, 805, 805, 805, 930, 930, 930, 1030)
  `2020` = c(610, 675, 750, 825, 910, 910, 910, 1050),
  `2021` = c(660, 780, 850, 920, 990, 990, 990, 1075),
  `2022` = c(705, 825, 895, 965, 1035, 1035, 1035, 1120),
  `2023` = c(750, 870, 940, 1010, 1080, 1080, 1080, 1165),
  `2024` = c(795, 915, 985, 1055, 1125, 1125, 1125, 1210),
  `2025` = c(840, 960, 1030, 1100, 1170, 1170, 1170, 1255),
  `2026` = c(885, 1005, 1075, 1145, 1215, 1215, 1215, 1300),
  `2027` = c(930, 1050, 1120, 1190, 1260, 1260, 1260, 1345),
  `2028` = c(975, 1095, 1165, 1235, 1305, 1305, 1305, 1390),
  `2029` = c(1020, 1140, 1210, 1280, 1350, 1350, 1350, 1435),
  `2030` = c(1065, 1185, 1255, 1325, 1395, 1395, 1395, 1480)
) |>
  pivot_longer(cols = starts_with("20"),  # Specify the columns to pivot
               names_to = "Year", 
               values_to = "salary") |> 
  #mutate("4" = "4-6", "5" = "4-6", "6" = "4-6", "7" = "7+", "8" = "7+", "9" = "7+", "10" = "7+") |> 
  #mutate("Accrued Seasons" = case_when(
  #  "Accrued Seasons" == "4-6" ~ c("4", "5", "6"),  # Replace "4-6" with 4, 5, and 6
  #  TRUE ~ "Accrued Seasons"
  #)) |> 
  mutate(Year = as.numeric(Year),
         salary_diff = (salary) - lag(salary, n = 12),
         salary_diff = ifelse(salary_diff < 0, 0, salary_diff),
         salary_diff_pct = salary_diff/lag(salary, n = 12))

min_salary_inc |> filter(Accrued_Seasons != 0) |> 
  ggplot(aes(x = Year, y = salary_diff_pct)) +
  geom_col() +
  facet_wrap(~Accrued_Seasons, ncol=4) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 14, face = "bold"),      # Make facet labels bold to resemble team logos
    strip.background = element_blank(),                       # Remove facet box background
    axis.text.x = element_text(size = 10),                    # Smaller x-axis text
    axis.ticks.y = element_line(size = 0.5),                  # Minor x-axis ticks
    axis.ticks.x = element_line(size = 0.5),                  # Minor x-axis ticks
    axis.text.y = element_text(size = 10),                    # Smaller y-axis text
    panel.grid.minor = element_blank(),                       # Remove minor grid lines
    plot.title = element_text(hjust=0.5, size=20),
    plot.subtitle = element_text(hjust=0.5)
  )

## Make pretty after





### Middle class definition as apy percentage of cap (transform back after the 2/3* and 2* rule)
### Average Percentage spent on the top 10 players per year (overall from league)
### APY percentage of cap on a histogram (2014/2024)
###### Cap by year for top 10% (top graph), middle 50% (middle line), below (lower line)



# Make new columns for all the years to get rate of change (4, 5, 6, 7, 8)
# A cool for loop where it takes the next one if the year is next
#### Where does rate of change stop
