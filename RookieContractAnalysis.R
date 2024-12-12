

contracts_Jason_RFA <- read.csv("Cap2013_Present.csv")

contracts_join <- contracts |> select(player, otc_id, draft_year, draft_round, draft_overall)

contracts_Jason_RFA_joined <- contracts_join |> 
  left_join(contracts_Jason_2014, by = c("otc_id" = "player_id")) |> 
  filter(!is.na(ID)) |> unique()

contracts_Jason_RFA <- read.csv("Cap2013_Present.csv")

contracts_join <- contracts |> select(player, otc_id, draft_year)

# Do total contracts
total_contracts <-  contracts_join |> 
  left_join(read.csv("OTC_Contracts.csv"), by = c("otc_id" = "player_id")) |> 
  filter(!is.na(ID)) |> unique()

### Take the last pick of each draft
total_contracts_rookies_2022 <- total_contracts |> 
  filter(draft_year == 2022) |> 
  filter(draft_year == year_signed) |> 
  summarize(player, otc_id, draft_year, draft_round, draft_overall, Total) |> 
  unique()

total_contracts_rookies_2021 <- total_contracts |> 
  filter(draft_year == 2021) |> 
  filter(draft_year == year_signed) |> 
  summarize(player, otc_id, draft_year, draft_round, draft_overall, Total) |> 
  unique()


