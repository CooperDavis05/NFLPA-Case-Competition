## Visualize how it was before

cap_ref <- tibble(cap = c(52388000, 57288000, 62172000, 67405000, 71101000, 75007000, 80582000, 85500000, 102000000, 
                          109000000, 116000000, 123000000, 128872565, 120375000, 120600000, 123600000, 133000000, 
                          143280000, 155270000, 167000000, 177200000, 188200000, 198200000, 182500000, 208200000, 
                          224800000, 255400000, 272500000, 290000000, 314000000)/1000000,
                  year = c(1998:2027)) |> 
  mutate(pct_cap_change = ((cap) - lag(cap))/lag(cap)) #|> 
   # mutate(pct_cap_change = ifelse(year == 2022, 0.06, pct_cap_change),
   #       pct_cap_change = ifelse(year == 2021, 0.06, pct_cap_change))

#for (yr in 2021:2027) {
#  cap_ref <- cap_ref |> 
#    mutate(
#      cap = ifelse(
#        year == yr,
#        cap[year == yr - 1] * (1 + pct_cap_change[year == yr]),
#        cap
#      )
#    )
#}


  
  
min_salary_inc_before <- tibble(
  `Accrued_Seasons` = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
  `2011` = c(375000, 450000, 525000, 600000, 685000, 685000, 685000, 810000, 810000, 810000, 910000),
  `2012` = c(390000, 465000, 540000, 615000, 700000, 700000, 700000, 825000, 825000, 825000, 925000),
  `2013` = c(405000, 480000, 555000, 630000, 715000, 715000, 715000, 840000, 840000, 840000, 940000),
  `2014` = c(420000, 495000, 570000, 645000, 730000, 730000, 730000, 855000, 855000, 855000, 955000),
  `2015` = c(435000, 510000, 585000, 660000, 745000, 745000, 745000, 870000, 870000, 870000, 970000),
  `2016` = c(450000, 525000, 600000, 675000, 760000, 760000, 760000, 885000, 885000, 885000, 985000),
  `2017` = c(465000, 540000, 615000, 690000, 775000, 775000, 775000, 900000, 900000, 900000, 1000000),
  `2018` = c(480000, 555000, 630000, 705000, 790000, 790000, 790000, 915000, 915000, 915000, 1015000),
  `2019` = c(495000, 570000, 645000, 720000, 805000, 805000, 805000, 930000, 930000, 930000, 1030000)
) |>
  pivot_longer(cols = starts_with("20"),  # Specify the columns to pivot
               names_to = "Year", 
               values_to = "salary") |> 
  mutate(Year = as.numeric(Year))  |> 
  filter(Accrued_Seasons != "8" & Accrued_Seasons != "9") |> 
  mutate(Accrued_Seasons = ifelse(Accrued_Seasons == "7", "7-9", Accrued_Seasons),
         Accrued_Seasons = ifelse(Accrued_Seasons == "10", "10+", Accrued_Seasons))


min_salary_inc_before_all <- min_salary_inc_before |> right_join(cap_ref, by = c("Year" = "year")) |> 
  filter(Year < 2020 & Year > 2010) |> 
  mutate(salary = salary/1000000,
         pct_cap = salary/cap,
         Year = as.character(Year),
         Year = gsub("^20", "'", Year))


min_salary_inc_after <- tibble(
  `Accrued_Seasons` = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
  `2020` = c(610, 675, 750, 825, 910, 910, 910, 1050, 1050, 1050, 1050),
  `2021` = c(660, 780, 850, 920, 990, 990, 990, 1075, 1075, 1075, 1075),
  `2022` = c(705, 825, 895, 965, 1035, 1035, 1035, 1120, 1120, 1120, 1120),
  `2023` = c(750, 870, 940, 1010, 1080, 1080, 1080, 1165, 1165, 1165, 1165),
  `2024` = c(795, 915, 985, 1055, 1125, 1125, 1125, 1210, 1210, 1210, 1210),
  `2025` = c(840, 960, 1030, 1100, 1170, 1170, 1170, 1255, 1255, 1255, 1255),
  `2026` = c(885, 1005, 1075, 1145, 1215, 1215, 1215, 1300, 1300, 1300, 1300),
  `2027` = c(930, 1050, 1120, 1190, 1260, 1260, 1260, 1345, 1345, 1345, 1345),
  `2028` = c(975, 1095, 1165, 1235, 1305, 1305, 1305, 1390, 1390, 1390, 1390),
  `2029` = c(1020, 1140, 1210, 1280, 1350, 1350, 1350, 1435, 1435, 1435, 1435),
  `2030` = c(1065, 1185, 1255, 1325, 1395, 1395, 1395, 1480, 1480, 1480, 1480)
) |>
  pivot_longer(cols = starts_with("20"),  # Specify the columns to pivot
               names_to = "Year", 
               values_to = "salary") |> 
  mutate(Year = as.numeric(Year)) |> 
  filter(Accrued_Seasons != "8" & Accrued_Seasons != "9" & Accrued_Seasons != "10") |> 
  mutate(Accrued_Seasons = ifelse(Accrued_Seasons == "7", "7+", Accrued_Seasons))
         #,
         #salary_diff = (salary) - lag(salary, n = 12),
         #salary_diff = ifelse(salary_diff < 0, 0, salary_diff),
         #salary_diff_pct = salary_diff/lag(salary, n = 12))


min_salary_inc_after_all <- min_salary_inc_after |> right_join(cap_ref, by = c("Year" = "year")) |> 
  filter(Year > 2019) |> 
  mutate(salary = salary/1000,
         pct_cap = salary/cap,
         Year = as.character(Year),
         Year = gsub("^20", "'", Year))






min_salary_inc_before_all_graph <- min_salary_inc_before_all|> 
  mutate(Accrued_Seasons = ifelse(Accrued_Seasons == "4", "4-6", Accrued_Seasons)) |> 
  filter(!(Accrued_Seasons %in% c("5", "6"))) |> 
  ggplot(aes(x = factor(Year), y = pct_cap)) +
  geom_col(fill = "maroon", alpha = 0.8) +
  facet_wrap(~factor(Accrued_Seasons, levels = c("0", "1", "2", "3", "4-6", "7-9", "10+")), , 
                      ncol = 4) +
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

ggsave('/Users/Cooper/Documents/Folder for Contracts Project/Graphs/MinSalariesBeforeCBA.png', min_salary_inc_before_all_graph,  width = 10, height = 7, dpi = "retina")


min_salary_inc_after_all_graph <- min_salary_inc_after_all |> 
  mutate(Accrued_Seasons = ifelse(Accrued_Seasons == "4", "4-6", Accrued_Seasons)) |> 
  filter(!(Accrued_Seasons %in% c("5", "6"))) |> 
  ggplot(aes(x = factor(Year), y = pct_cap)) +
  geom_col(fill = "darkorange", alpha = 0.8) +
  facet_wrap(~Accrued_Seasons, ncol=3) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 14, face = "bold"),      # Make facet labels bold to resemble team logos
    strip.background = element_blank(),                       # Remove facet box background
    axis.text.x = element_text(size = 9),                    # Smaller x-axis text
    axis.ticks.y = element_line(size = 0.5),                  # Minor x-axis ticks
    axis.ticks.x = element_line(size = 0.5),                  # Minor x-axis ticks
    axis.text.y = element_text(size = 10),                    # Smaller y-axis text
    panel.grid.minor = element_blank(),                       # Remove minor grid lines
    panel.grid.major.x = element_blank(),
    plot.title = element_text(hjust=0.5, size=20),
    plot.subtitle = element_text(hjust=0.5),
    legend.position = "NONE")  + 
  geom_text(aes(label = paste0(round(pct_cap*100, 2), "%")), 
            position = position_dodge2(width = 0.7), 
            vjust = 1.5, size = 2.25, color = "black") +
  labs(x = "Year", y = "% of Cap", 
       title = "How Have Minimum Salaries Changed Throughout this CBA?", 
       subtitle = "Data via NFLReadR + OTC | Cooper Davis | 2025-2027 Cap Spaces Projected by OTC") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray")
min_salary_inc_after_all_graph

ggsave('/Users/Cooper/Documents/Folder for Contracts Project/Graphs/MinSalariesAfterCBA.png', min_salary_inc_after_all_graph,  width = 12, height = 6, dpi = "retina")


## Use MedianAndResourcesInPosition

cap_ref_change <- cap_ref |> 
  mutate(pct_cap_change = ((cap) - lag(cap))/lag(cap))


### Take the average of last CBA's cap increase PCT 
#### Extrapolate this to the minimum salary

cap_avg_2012_2020 <- mean(cap_ref_change$pct_cap_change[match(2015:2020, cap_ref_change$year)])

##Graph of the cap change percentage


cap_ref_change_graph <- cap_ref_change |> 
  filter(year >= 2020 & year <= 2027) |> 
  ggplot(aes(x = year, y = cap)) +
  geom_line(color = "pink", size = 1.5) +
  geom_point(color = "pink", size = 3) +
  scale_x_continuous(breaks = seq(2021, 2027, 1)) +
  geom_text(aes(label = scales::percent(pct_cap_change, accuracy = 0.1)), 
            nudge_y = 5, size = 3, color = "black") +
  labs(
    title = "Percentage Cap Change (2020-2027)",
    subtitle = "Data via OTC | Cooper Davis | 2025-2027 Projected By OTC",
    x = "Year",
    y = "Percentage Change"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12),
    panel.grid.minor.y = element_blank(),
  ) 
  #geom_hline(yintercept = cap_avg_2012_2020, linetype = "dashed", color = "black")
  #annotate(
  #  "text",
  #  x = 4,  
  #  y = cap_avg_2012_2020 + 0.003,
  #  label = paste0("Avg Change: ", round(cap_avg_2012_2020 * 100, 2), "%"),
  #  size = 3.5,
  #  color = "black",
  #  hjust = 0
  #)
cap_ref_change_graph


ggsave('/Users/Cooper/Documents/Folder for Contracts Project/Graphs/PctCapChangeForAnalysis.png', cap_ref_change_graph,
       width = 5,    
       height = 4,   
       dpi = 300     
)




# ~ 0.06880182 = 6.88%

## Second half of Cap Increase average from last contract

min_salary_inc_after_all2 <- min_salary_inc_after_all |> 
  mutate(salary_new = case_when(
    Accrued_Seasons == "0" & Year == "'20" ~ 0.610,
    Accrued_Seasons == "1" & Year == "'20" ~ 0.675,
    Accrued_Seasons == "2" & Year == "'20" ~ 0.750,
    Accrued_Seasons == "3" & Year == "'20" ~ 0.825,
    Accrued_Seasons == "4" & Year == "'20" ~ 0.910,
    Accrued_Seasons == "5" & Year == "'20" ~ 0.910,
    Accrued_Seasons == "6" & Year == "'20" ~ 0.910,
    Accrued_Seasons == "7+" & Year == "'20" ~ 1.05,########################
    Accrued_Seasons == "0" & Year != "'20" ~ 0.660,
    Accrued_Seasons == "1" & Year != "'20" ~ 0.780,
    Accrued_Seasons == "2" & Year != "'20" ~ 0.850,
    Accrued_Seasons == "3" & Year != "'20" ~ 0.920,
    Accrued_Seasons == "4" & Year != "'20" ~ 0.990,
    Accrued_Seasons == "5" & Year != "'20" ~ 0.990,
    Accrued_Seasons == "6" & Year != "'20" ~ 0.990,
    Accrued_Seasons == "7+" & Year != "'20" ~ 1.075))


#for (year in 2022:2027) {
#  min_salary_inc_after_all2 <- min_salary_inc_after_all2 |> 
#    mutate(Year = as.integer(sub("'", "20", Year))) |> 
#    group_by(Accrued_Seasons) |> 
#    mutate(
#      salary_new = ifelse(
#        Year == year,
#        ceiling(((salary_new[Year == year - 1] * (1 + cap_avg_2012_2020))/0.005))*0.005,
#        salary_new
#      ),
#    ) |>
#    ungroup()
#}

for (year in 2022:2027) {
  min_salary_inc_after_all2 <- min_salary_inc_after_all2 |> 
    mutate(Year = as.integer(sub("'", "20", Year))) |> 
    group_by(Accrued_Seasons) |> 
    mutate(
      salary_new = ifelse(
        Year == year,
        ceiling(((salary_new[Year == year - 1] * (1 + pct_cap_change[Year == year]))/0.005))*0.005,
        salary_new
      ),
    ) |>
    ungroup()
}


## Find the salary difference and how much/positive or negative that difference is
min_salary_inc_diff <- min_salary_inc_after_all2 |> 
  mutate(salary_diff = round(salary_new - salary, 4),
         pos_or_neg = salary_diff >= 0) 



########## Make a plot of this with the new salaries

min_salary_inc_diff_tbbl_ready <- min_salary_inc_diff |> 
  summarize(Accrued_Seasons, Year, salary_new = salary_new*1000) |> 
  pivot_wider(
    names_from = Year,
    values_from = salary_new
  ) |> 
  #mutate(`2028` = ceiling(`2027`*(1 + cap_avg_2012_2020)/1000/0.005)*5,
  #       `2029` = ceiling(`2028`*(1 + cap_avg_2012_2020)/1000/0.005)*5,
  #       `2030` = ceiling(`2029`*(1 + cap_avg_2012_2020)/1000/0.005)*5) |> 
  mutate(Accrued_Seasons = ifelse(Accrued_Seasons == "4", "4-6", Accrued_Seasons)) |> 
  filter(Accrued_Seasons != "5") |> 
  filter(Accrued_Seasons != "6")

min_salary_inc_diff_tbbl <- min_salary_inc_diff_tbbl_ready |> 
  gt() |> 
  tab_header(
    title = "NEW Minimum Salary Table"
  ) |> 
  cols_label('Accrued_Seasons' = "#CS") |> 
  fmt_currency(
    columns = 2:9, #12
    currency = "USD",
    decimals = 0,
    use_seps = T
  ) |> 
  opt_align_table_header(align = "center") |> 
  tab_options(
    table.font.size = "small",
    table.font.names = "Times New Roman",
    table.align = "center"
  ) |> 
  cols_align(
    align = "center",
    columns = everything()
  ) |> 
  tab_style(
    style = cell_borders(
      sides = c("left", "right", "top", "bottom"),
      color = "black",
      weight = px(1)
    ),
    locations = list(cells_body(columns = everything()),
                     cells_column_labels(columns = everything()))
  ) |> 
  tab_options(
    table.border.top.style = "none",  
    table.border.bottom.style = "none",
    data_row.padding = px(5), # Adjust padding between rows
    data_row.padding.horizontal = px(10)
  ) |> 
  tab_footnote(
    footnote = md("(all amounts in thousands of dollars)<br>Cap Space 2025-2027 Projected By OTC")
  ) 


gtsave(min_salary_inc_diff_tbbl, "/Users/Cooper/Documents/Folder for Contracts Project/Graphs/MinSalariesChanged.png", vwidth = 820)





## Now plot this with salaries
min_salary_inc_diff <- min_salary_inc_diff |> 
  mutate(new_cap_pct = salary_new/cap,
         Year = as.character(Year),
         Year = gsub("^20", "'", Year))    # First mutate cap pct
  
  
min_salary_inc_diff_graph <- min_salary_inc_diff |> 
  mutate(Accrued_Seasons = ifelse(Accrued_Seasons == "4", "4-6", Accrued_Seasons)) |> 
  filter(!(Accrued_Seasons %in% c("5", "6"))) |> 
  ggplot() +
  geom_col(aes(x = factor(Year), y = new_cap_pct), alpha = 1, fill = "#7851A9") +
  geom_col(aes(x = factor(Year), y = pct_cap), alpha = 0.4, fill = "darkblue") +
  facet_wrap(~Accrued_Seasons, ncol=3) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 14, face = "bold"),      # Make facet labels bold to resemble team logos
    strip.background = element_blank(),                       # Remove facet box background
    axis.text.x = element_text(size = 9),                    # Smaller x-axis text
    axis.ticks.y = element_line(size = 0.5),                  # Minor x-axis ticks
    axis.ticks.x = element_line(size = 0.5),                  # Minor x-axis ticks
    axis.text.y = element_text(size = 10),                    # Smaller y-axis text
    panel.grid.minor = element_blank(),                       # Remove minor grid lines
    panel.grid.major.x = element_blank(),
    plot.title = element_text(hjust=0.5, size=20),
    plot.subtitle = element_text(hjust=0.5),
    legend.position = "NONE") +
  labs(x = "Year", y = "% of Cap", 
       title = "How Would Minimum Salaries Change Under This New Guide?", 
       subtitle = "Light Purple Indicates the Minimum Salary Increases Under Our Proposal",
       caption = "Data via NFLReadR + OTC | Cooper Davis | 2025-2027 Cap Spaces Projected by OTC") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray")
min_salary_inc_diff_graph

ggsave('/Users/Cooper/Documents/Folder for Contracts Project/Graphs/MinSalariesNew.png', min_salary_inc_diff_graph,  width = 12, height = 6, dpi = "retina")





#min_salary_inc_before_all_bveu <- min_salary_inc_before_all |> group_by(Accrued_Seasons) |> 
#  mutate(pct_cap_change = (pct_cap) - lag(pct_cap))




# note didn't control for players like rashod bateman who have 1 fewer than should


### Get Rookie Contract





## Include the Rookie Contracts
contracts_rookies <- read.csv("dataframe_2024.csv")

contracts_rookies <- contracts_rookies |>
  mutate(Player = str_replace(Player, "^.*?\\s", "")) |> 
  mutate(Player = ifelse(str_starts(Player, "Jr.") | 
                           str_starts(Player, "III") |
                           str_starts(Player, "Pran-Granger"), 
                         str_replace(Player, "^.*?\\s", ""), Player)) |> 
  mutate(Player = ifelse(Player == "Olumuyiwa Fashanu", "Olu Fashanu",Player))

contracts_rookies$Player <- clean_player_names(contracts_rookies$Player) 

rookies_rosters_2024 <- load_rosters(2024) |> filter(entry_year == 2024)

rookies_rosters_2024$full_name <- clean_player_names(rookies_rosters_2024$full_name) 

new_contracts_rookies <- contracts_rookies |> 
  right_join(rookies_rosters_2024, by = c("Player" = "full_name")) |> 
  filter(!(Player == "Byron Murphy" & Pos == "CB"))



###### Other

active_contracts <- load_contracts(file_type = getOption("nflreadr.prefer", default = "rds")) |> 
  filter(is_active == T) |> 
  filter(value >= 0.795)
  
for(x in 1:nrow(active_contracts)) {
  active_contracts$empty[x] = ifelse(is_empty(active_contracts$cols[[x]]), 1, 0)
}   
active_contracts2 <- active_contracts |> filter(empty == 0) |> select(-empty) |>  filter(year_signed != 0)


active_contracts2$seasons <- active_contracts2[[25]] %>%
  purrr::map(~ .x %>%
               filter(year <= 2024) %>%
               nrow())

for(y in 2024) { # Loop entirely through and figure out base salaries
  active_contracts2[[paste0("base_", y)]] <- active_contracts2[[25]] %>%
    purrr::map(~ .x %>%
               filter(year == y) %>%
               pull(as.numeric(base_salary)))
  active_contracts2[[paste0("cap_hit_", y)]] <- active_contracts2[[25]] %>%
    purrr::map(~ .x %>%
                 filter(year == y) %>%
                 pull(as.numeric(cap_number)))
}

active_contracts2 <- active_contracts2 %>%
  mutate(across(everything(), ~ ifelse(lengths(.x) == 0, NA, .x)))

active_contracts2[[27]] <- unlist(active_contracts2[[27]])
active_contracts2[[28]] <- unlist(active_contracts2[[28]])





### Do just the before and after
min_salary_inc_diff_ba <- min_salary_inc_diff |> 
  select(Accrued_Seasons, Year, salary, salary_new) |> 
  filter(Year == "'24") |> 
  pivot_longer(
    cols = c(salary, salary_new),
    names_to = "salary_name",
    values_to = "salary"
  )

min_salary_inc_diff_check <- min_salary_inc_diff_ba |> 
  filter(salary_name == "salary")


created_filter <- active_contracts2 |> 
  filter(base_2024 >= 0.795 & cap_hit_2024 >= 0.795) |> 
  filter(base_2024 <= cap_hit_2024) |> 
  mutate(under_base_selection = ifelse(base_2024 < 1.375, 1, 0),
         cap_hit_diff = cap_hit_2024-base_2024)

created_filter <- created_filter %>%
  mutate(
    salary_bin = case_when(
      base_2024 >= min_salary_inc_diff_check$salary[min_salary_inc_diff_check$Accrued_Seasons == "0"] & base_2024 < min_salary_inc_diff_check$salary[min_salary_inc_diff_check$Accrued_Seasons == "1"] ~ "Bin_1",
      base_2024 >= min_salary_inc_diff_check$salary[min_salary_inc_diff_check$Accrued_Seasons == "1"] & base_2024 < min_salary_inc_diff_check$salary[min_salary_inc_diff_check$Accrued_Seasons == "2"] ~ "Bin_2",
      base_2024 >= min_salary_inc_diff_check$salary[min_salary_inc_diff_check$Accrued_Seasons == "2"] & base_2024 < min_salary_inc_diff_check$salary[min_salary_inc_diff_check$Accrued_Seasons == "3"] ~ "Bin_3",
      base_2024 >= min_salary_inc_diff_check$salary[min_salary_inc_diff_check$Accrued_Seasons == "3"] & base_2024 < min_salary_inc_diff_check$salary[min_salary_inc_diff_check$Accrued_Seasons == "4"] ~ "Bin_4",
      base_2024 >= min_salary_inc_diff_check$salary[min_salary_inc_diff_check$Accrued_Seasons == "4"] & base_2024 < min_salary_inc_diff_check$salary[min_salary_inc_diff_check$Accrued_Seasons == "7+"] ~ "Bin_5",
      base_2024 >= min_salary_inc_diff_check$salary[min_salary_inc_diff_check$Accrued_Seasons == "7+"] & base_2024 <= 1.375 ~ "Bin_6",
      TRUE ~ "Outside_Range"  # Optional: Handle values not in any bin
    )
  )

min_salary_inc_diff_implement <- min_salary_inc_diff_ba |> 
  filter(salary_name == "salary_new") %>%
  mutate(
    salary_bin = case_when(
      salary == min_salary_inc_diff_implement$salary[min_salary_inc_diff_implement$Accrued_Seasons == "0"] ~ "Bin_1",
      salary == min_salary_inc_diff_implement$salary[min_salary_inc_diff_implement$Accrued_Seasons == "1"] ~ "Bin_2",
      salary == min_salary_inc_diff_implement$salary[min_salary_inc_diff_implement$Accrued_Seasons == "2"] ~ "Bin_3",
      salary == min_salary_inc_diff_implement$salary[min_salary_inc_diff_implement$Accrued_Seasons == "3"] ~ "Bin_4",
      salary == min_salary_inc_diff_implement$salary[min_salary_inc_diff_implement$Accrued_Seasons == "4"] ~ "Bin_5",
      salary == min_salary_inc_diff_implement$salary[min_salary_inc_diff_implement$Accrued_Seasons == "7+"] ~ "Bin_6",
      TRUE ~ "Outside_Range"  # Optional: Handle values not in any bin
    )
  )


created_filter2 <- created_filter |> 
  right_join(min_salary_inc_diff_implement, by = "salary_bin") |> 
  mutate(Accrued_Seasons = ifelse(Accrued_Seasons == "4" |
                                    Accrued_Seasons == "5" |
                                    Accrued_Seasons == "6", "4-6", Accrued_Seasons)) |> 
  select(player, position, team, year_signed, years, base_2024, cap_hit_2024,
         salary_bin, Accrued_Seasons, salary) |> 
  unique() |> 
  mutate(salary_inc = round(salary - base_2024, 10),
         new_cap_hit = ifelse((salary_inc > 0) | (salary_inc == -0.03) | (salary_inc == -0.005), cap_hit_2024 + salary_inc, cap_hit_2024))


active_contracts_final <- active_contracts2 |> 
  select(player, position, team, year_signed, years, base_2024, cap_hit_2024) |> 
  full_join(created_filter2, by = c("player", "position", "team", "year_signed", "years", "base_2024", "cap_hit_2024")) |> 
  mutate(cap_hit_2024 = coalesce(new_cap_hit, cap_hit_2024))







# Cap Hit APY analysis without influence
active_contracts2 <- active_contracts2 |>mutate(caphit_pct = cap_hit_2024/255.4)
active_contracts2 |> 
  mutate(caphit_pct = cap_hit_2024/255.4) |> 
  ggplot(aes(x = caphit_pct)) +
  geom_histogram(aes(y = ..count.. / sum(..count..)), binwidth = 0.005, fill = "skyblue", color = "skyblue", alpha = 0.7) +
  theme_minimal() +
  geom_vline(xintercept = c(quantile(active_contracts2$caphit_pct, 0.20, na.rm = T), 
                            quantile(active_contracts2$caphit_pct, 0.80, na.rm = T)), 
             color = "black", linetype = "dashed", size = 0.2) +
  labs(x = "Cap Hit Percentage 2024", y = "Density", 
       title = "Cap Hit Percentage Distribution 2024",
       subtitle = "Data via OTC | Cooper Davis") +
  theme(panel.grid.major.y = element_line(size = 0.5),
        plot.title = element_text(hjust=0.5, size=20),
        axis.text.y = element_text(colour="black"),
        plot.subtitle = element_text(hjust=0.5))

## Do it for the mutated data
active_contracts_final <- active_contracts_final |> mutate(caphit_pct = cap_hit_2024/255.4)
active_contracts_final |> 
  mutate(caphit_pct = cap_hit_2024/255.4) |> 
  ggplot(aes(x = caphit_pct)) +
  geom_histogram(aes(y = ..count.. / sum(..count..)), binwidth = 0.005, fill = "skyblue", color = "skyblue", alpha = 0.7) +
  theme_minimal() +
  geom_vline(xintercept = c(quantile(active_contracts_final$caphit_pct, 0.20, na.rm = T), 
                            quantile(active_contracts_final$caphit_pct, 0.80, na.rm = T)), 
             color = "black", linetype = "dashed", size = 0.2) +
  labs(x = "Cap Hit Percentage 2024 After Change", y = "Density", 
       title = "Cap Hit Percentage Distribution 2024",
       subtitle = "Data via OTC | Cooper Davis") +
  theme(panel.grid.major.y = element_line(size = 0.5),
        plot.title = element_text(hjust=0.5, size=20),
        axis.text.y = element_text(colour="black"),
        plot.subtitle = element_text(hjust=0.5))

# Layered Plot

#Create the joins first
active_contracts2_join <- active_contracts2 |> 
  summarize(player, position, team, cap_hit_2024_before = cap_hit_2024,
            caphit_pct_before = caphit_pct)

active_contracts_final_join <- active_contracts_final |> 
  summarize(player, position, team, cap_hit_2024_after = cap_hit_2024,
            caphit_pct_after = caphit_pct)

active_contracts_both_join <- active_contracts2_join |> 
  full_join(active_contracts_final_join, by = c("player", "position", "team"))



### Goes back to the whole 2/3 and 2* thing
# Graph of the joined
active_contracts_both_join_graph <- active_contracts_both_join |>
  ggplot() +
  geom_histogram(aes(x = caphit_pct_before, y = ..count.. / sum(..count..), fill = "Before"), binwidth = 0.0025, boundary = 0, alpha = 1) +
  geom_histogram(aes(x = caphit_pct_after, y = ..count.. / sum(..count..), fill = "After"), binwidth = 0.0025, boundary = 0, alpha = 0.4) +
  theme_minimal() +
  scale_x_continuous(limits = c(0, 0.125)) +
  geom_vline(xintercept = c((2/3)*quantile(active_contracts_both_join$caphit_pct_before, 0.50, na.rm = T), 
                            (2)*quantile(active_contracts_both_join$caphit_pct_before, 0.50, na.rm = T)), 
             color = "darkred", linetype = "dashed", size = 0.2) +
  geom_vline(xintercept = c((2/3)*quantile(active_contracts_both_join$caphit_pct_after, 0.50, na.rm = T), 
                            2*quantile(active_contracts_both_join$caphit_pct_after, 0.50, na.rm = T)), 
             color = "blue", linetype = "dashed", size = 0.2) +
  labs(x = "Cap Hit Percentage 2024", y = "Frequency", 
       title = "Cap Hit Percentage Distribution 2024",
       subtitle = "Data via OTC | Cooper Davis") +
  scale_fill_manual(values = c("Before" = "darkred", "After" = "blue"), breaks = c("Before", "After")) +
  theme(panel.grid.major.y = element_line(size = 0.5),
        plot.title = element_text(hjust=0.5, size=20),
        axis.text.y = element_text(colour="black"),
        plot.subtitle = element_text(hjust=0.5),
        legend.title = element_blank(),
        legend.position = "top")

ggsave('/Users/Cooper/Documents/Folder for Contracts Project/Graphs/ChangeInMedianStuff.png', active_contracts_both_join_graph,  width = 10, height = 7, dpi = "retina")

# Count how many before and after
active_contracts_both_join <- active_contracts_both_join |> 
  mutate(median_lower_before = (2/3)*quantile(active_contracts_both_join$caphit_pct_before, 0.50, na.rm = T),
         median_higher_before = 2*quantile(active_contracts_both_join$caphit_pct_before, 0.50, na.rm = T),
         within_median_before = ifelse(caphit_pct_before <= median_higher_before & caphit_pct_before >= median_lower_before, 1, 0),
         median_lower_after = (2/3)*quantile(active_contracts_both_join$caphit_pct_after, 0.50, na.rm = T),
         median_higher_after = 2*quantile(active_contracts_both_join$caphit_pct_after, 0.50, na.rm = T),
         within_median_after = ifelse(caphit_pct_after <= median_higher_after & caphit_pct_after >= median_lower_after, 1, 0),)

#### 616 players within the median to 767


(2/3)*quantile(active_contracts_both_join$caphit_pct_before, 0.50, na.rm = T)*255.4
2*quantile(active_contracts_both_join$caphit_pct_before, 0.50, na.rm = T)*255.4
(2/3)*quantile(active_contracts_both_join$caphit_pct_after, 0.50, na.rm = T)*255.4
(2)*quantile(active_contracts_both_join$caphit_pct_after, 0.50, na.rm = T)*255.4

(2.353333-2.066667)/2.066667


#Lower bound from 1.033333 to 1.176667
#Higher bound to 3.1 to 3.53

# The middle class increased by about 28.6k in definition (expanding it, all shifted up)
# The middle class also shifted towards making more money as a percentage of the cap (as a result of the minimum salary shift)
### From 1.55 to 1.765 
##### The middle class is on average making a salary of $215k more
####### Which may not seem like all that much, but it is an increase of 13.87%
## This will also increase a lot when the new minimums ALL eclipse the current minimums


# 616 within before
# 606 with after

Previous_Salary_Table <- tibble(
  Accrued_Seasons = c("0", "1", "2", "3", "4-6", "7-9", "10+"),
  `2011` = c(375000, 450000, 525000, 600000, 685000, 810000, 910000),
  `2012` = c(390000, 465000, 540000, 615000, 700000, 825000, 925000),
  `2013` = c(405000, 480000, 555000, 630000, 715000, 840000, 940000),
  `2014` = c(420000, 495000, 570000, 645000, 730000, 855000, 955000),
  `2015` = c(435000, 510000, 585000, 660000, 745000, 870000, 970000),
  `2016` = c(450000, 525000, 600000, 675000, 760000, 885000, 985000),
  `2017` = c(465000, 540000, 615000, 690000, 775000, 900000, 1000000),
  `2018` = c(480000, 555000, 630000, 705000, 790000, 915000, 1015000),
  `2019` = c(495000, 570000, 645000, 720000, 805000, 930000, 1030000)
) |> 
  mutate(`2011` = `2011`/1000,
         `2012` = `2012`/1000,
         `2013` = `2013`/1000,
         `2014` = `2014`/1000,
         `2015` = `2015`/1000,
         `2016` = `2016`/1000,
         `2017` = `2017`/1000,
         `2018` = `2018`/1000,
         `2019` = `2019`/1000)


#### Just a table for Previous CBA like OTC
Previous_Salary_Table_Table <- Previous_Salary_Table |> 
  gt() |> 
  tab_header(
    title = "Previous CBA Salary Table"
  ) |> 
  cols_label('Accrued_Seasons' = "#CS") |> 
  fmt_currency(
    columns = 2:10, #12
    currency = "USD",
    decimals = 0,
    use_seps = T
  ) |> 
  opt_align_table_header(align = "center") |> 
  tab_options(
    table.font.size = "small",
    table.font.names = "Times New Roman",
    table.align = "center"
  ) |> 
  cols_align(
    align = "center",
    columns = everything()
  ) |> 
  tab_style(
    style = cell_borders(
      sides = c("left", "right", "top", "bottom"),
      color = "black",
      weight = px(1)
    ),
    locations = list(cells_body(columns = everything()),
                     cells_column_labels(columns = everything()))
  ) |> 
  tab_style(
    style = cell_text(align = "center"), # Adjust padding for width
    locations = cells_body(columns = everything())
  ) |> 
  tab_options(
    table.border.top.style = "none",  
    table.border.bottom.style = "none",
    data_row.padding = px(5), # Adjust padding between rows
    data_row.padding.horizontal = px(10)
  ) |> 
  tab_footnote(
    footnote = md("(all amounts in thousands of dollars)")
  ) 

gtsave(Previous_Salary_Table_Table, "/Users/Cooper/Documents/Folder for Contracts Project/Graphs/MinSalariesPrevious.png", vwidth = 820)

