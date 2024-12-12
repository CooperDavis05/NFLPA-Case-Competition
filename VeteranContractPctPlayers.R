

contracts_Jason<- read.csv("Cap2013_Present.csv")

contracts_join <- contracts |> select(player, otc_id, draft_year)

contracts_Jason_joined <- contracts_join |>  # Filtered contracts from 2014
  left_join(contracts_Jason_2014, by = c("otc_id" = "player_id")) |> 
  filter(!is.na(ID)) |> unique() |> 
  filter(Year <= 2024) |> 
  select(player, otc_id, Year, BS, CP, contract_type)


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
  pivot_wider(
    names_from = Accrued_Seasons,
    values_from = salary
  )

min_salary_inc_after <- tibble(
  `Accrued_Seasons` = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
  `2020` = c(610, 675, 750, 825, 910, 910, 910, 1050, 1050, 1050, 1050)*1000,
  `2021` = c(660, 780, 850, 920, 990, 990, 990, 1075, 1075, 1075, 1075)*1000,
  `2022` = c(705, 825, 895, 965, 1035, 1035, 1035, 1120, 1120, 1120, 1120)*1000,
  `2023` = c(750, 870, 940, 1010, 1080, 1080, 1080, 1165, 1165, 1165, 1165)*1000,
  `2024` = c(795, 915, 985, 1055, 1125, 1125, 1125, 1210, 1210, 1210, 1210)*1000
) |>
  pivot_longer(cols = starts_with("20"),  # Specify the columns to pivot
               names_to = "Year", 
               values_to = "salary") |> 
  pivot_wider(
    names_from = Accrued_Seasons,
    values_from = salary
  )

total_minimum_salaries <- rbind(min_salary_inc_before, min_salary_inc_after) |> 
  mutate(Year = as.numeric(Year))

contracts_Jason_joined_minimum <- contracts_Jason_joined |> 
  left_join(total_minimum_salaries, by = "Year") |> 
  mutate(Cap_minus_min = CP - BS,
    minimum_player = ifelse(!(contract_type %in% c("Drafted", "Rookie")) & Cap_minus_min < 75000 & (BS == `0` | 
                                 BS == `1` | 
                                 BS == `2` | 
                                 BS == `3` | 
                                 BS == `4` | 
                                 BS == `7` | 
                                 BS == `10`), 1, 0)) |> 
  filter(CP >= `0`) |> 
  group_by(Year) |> 
  mutate(total = n()) |> 
  ungroup() |> 
  group_by(Year) |> 
  mutate(total_min = sum(minimum_player)) |> 
  ungroup() |> 
  mutate(total_min_pct = total_min/total) |> 
  filter(Year > 2014) |> 
  select(Year, total_min_pct) |> unique()


  

