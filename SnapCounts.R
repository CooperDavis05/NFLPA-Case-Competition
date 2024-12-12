library(nflreadr)

snap_counts_2024 <- load_snap_counts(2024)

rosters_2024 <- load_rosters(2024) |> 
  select(full_name, position, team, pfr_id, gsis_id, years_exp)

total_rosters <- snap_counts_2024 |> right_join(rosters_2024, by = c("player" = "full_name", 
                                                                     "team")) |> 
  group_by(gsis_id) |> 
  ungroup() |> 
  mutate(
      total_snaps = case_when(
        position.x %in% c("QB", "RB", "FB", "WR", "TE", "T", "G", "C") ~ offense_snaps,
        position.x %in% c("SS", "FS", "CB", "LB", "DT", "DE", "NT", "DB") ~ defense_snaps,
        position.x %in% c("LS", "K", "P") ~ st_snaps, 
        TRUE ~ NA_real_
      ),
      total_snaps_pct = case_when(
        position.x %in% c("QB", "RB", "FB", "WR", "TE", "T", "G", "C") ~ offense_pct,
        position.x %in% c("SS", "FS", "CB", "LB", "DT", "DE", "NT", "DB") ~ defense_pct,
        position.x %in% c("LS", "K", "P") ~ st_pct, 
        TRUE ~ NA_real_
      )
    ) |> 
  group_by(gsis_id) |> 
  mutate(avg_snaps_pct = mean(total_snaps_pct),
         total_snaps = sum(total_snaps)) |> 
  ungroup() |> 
  summarize(player, position = position.x, position.y, team, years_exp, gsis_id, avg_snaps_pct, total_snaps) |> 
  mutate(veteran = ifelse(years_exp < 4, 0, 1))
  
contracts_new <- contracts |> filter(is_active == T) |> 
  select(years, value, apy, otc_id, gsis_id)

total_roster_snaps_contracts <- total_rosters |> 
  right_join(contracts_new, by = c("gsis_id")) |>
  mutate(position = case_when(
    player == "Oli Udoh" ~ "T",
    player == "Hunter Luepke" ~ "FB",
    player == "Darrell Taylor" ~ "DE",
    player == "David Quessenberry" ~ "T",
    player == "Jihad Ward" ~ "DE",
    player == "Jerry Tillery" ~ "DT",
    player == "Tyler Davis" ~ "DT",
    TRUE ~ position
  ),
  position = ifelse(position %in% c("FS", "SS"), "S", position),
  position = ifelse(position %in% c("DT", "NT"), "DT", position),
  position = ifelse(position %in% c("C", "G"), "IOL", position),
  value = ifelse(value == 0.225 & player == "Mike Edwards", 2.8, value)) |> 
  filter(!is.na(avg_snaps_pct)) |> unique() |> 
  mutate(avg_snaps_pct_bin = cut(
    avg_snaps_pct,
    breaks = seq(0, 1, by = 0.05),
    include.lowest = T,
    labels = paste0(seq(0, 0.95, by = 0.05), "-", seq(0.05, 1, by = 0.05))
  ))

####### DO it with jason's data too

contracts_Jason_2024 <- read.csv("Cap2013_Present.csv") |> 
  filter(Year == 2024) |> summarize(otc_id = player_id, Year, caphit = CP) 

total_roster_snaps_contracts <- total_roster_snaps_contracts |> 
  left_join(contracts_Jason_2024, by = "otc_id")

clustered_data <- total_roster_snaps_contracts |> 
  mutate(position_numeric = as.numeric(as.factor(position.y)))  |> 
  filter(avg_snaps_pct <= 0.50 & avg_snaps_pct >= 0.15) |> 
  filter(!is.na(caphit)) |> 
  mutate(caphit = caphit/1000000)




#clustering_data_scaled <- clustered_data |> 
#  mutate(across(c(avg_snaps_pct, total_snaps, position_numeric), scale))

###### Try it using clustering
match_result <- matchit(
  veteran ~ avg_snaps_pct + total_snaps,
  data = clustered_data,
  method = "nearest" ,
  distance = "mahalanobis",
  exact = ~ position
)

total_roster_snaps_contracts_matching <- match.data(match_result)


### This is kmeans

set.seed(1)

kmeans_result <- kmeans(clustered_data, centers = 500)

total_roster_snaps_contracts_kmeans <- total_roster_snaps_contracts |> 
  mutate(cluster = kmeans_result$cluster) |> unique() |> 
  filter(apy > 0.225000)

look_at_snaps_contracts_kmeans <- total_roster_snaps_contracts_kmeans |> 
  filter(avg_snaps_pct <= 0.60 & avg_snaps_pct >= 0.25) |>  
  group_by(cluster, position.y) |> 
  mutate(position_count = n()) |> # Count occurrences of each position in each cluster
  filter(position_count >= 2) |> 
  ungroup() |> 
  summarize(cluster, position.y, player, years_exp, years, apy, avg_snaps_pct, total_snaps) |> 
  unique() 


ray_vs_gus <- total_roster_snaps_contracts_kmeans |> 
  filter(player %in% c("Ray Davis", "Gus Edwards", "William Gholston", "Zach Harrison",
                       "Poona Ford", "LaBryan Ray", "Evan Anderson", "Michael Pierce", 
                       "Jerry Tillery", "Jaquelin Roy", "Karl Brooks", "Greg Gaines", 
                       "Eddie Goldman", "Chris Williams", "Mike Pennel", "Jordan Jackson"))
  
  



######## GO WITH THE MATCHING?

matching_results <- total_roster_snaps_contracts_matching |> 
  group_by(subclass) |> 
  mutate(subclass_count = n()) |> 
  filter(subclass_count == 2) |> 
  mutate(difference = caphit[veteran == 1] - caphit[veteran == 0]) |> 
  ungroup()
  
matching_results <- matching_results |> mutate(avg_diff = mean(difference))

matching_results <- matching_results |> group_by(position) |> 
  mutate(avg_diff_pos = mean(difference)) |> 
  ungroup()

matching_results_graph_ready <- matching_results |> 
  select(position, avg_diff, avg_diff_pos)

###### FOR ROOKIES VS VETERANS
# For a similar snap percentage and snaps played (to control for injury) 
##### Making sure position was equal
#for players between 25% and 60% of snaps 
# The mean difference in salary was 2.19
#### Veterans are making on average 2.19 million more for the same snap counts as rookies
###### This does not include level of play, but it can be averaged out
####### Teams will want to focus on rookie contract players


matching_visual_w_k_fb_p_ls <- matching_results_graph_ready |> 
  filter(!(position %in% c("K", "P", "LS", "FB"))) |> 
  ggplot(aes(fct_reorder(position, avg_diff_pos, .desc = TRUE), avg_diff_pos)) +
  geom_col(position = "dodge", fill = "#FFB6C1", color = '#FFB6C1', width = 0.7, alpha = 0.5) + 
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.5, size=20),
        axis.text.y = element_text(colour="black"),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank(),
        plot.subtitle = element_text(hjust=0.5),
        legend.position = "none") +
  labs(x = "Position", y = "Veteran - Rookie Cap Difference", 
       title = "How are Veteran Backups Paid In Comparison to Rookie Backups", 
       subtitle = "15-50% of Snaps | FB, LS, K, P excluded | Data via OverTheCap | Cooper Davis") +
  #geom_text(aes(label = round(max_apy_cap_pct, 3)), 
  #          position = position_dodge2(width = 0.7), 
  #          vjust = 1.5, size = 1.75, color = "white") +
  geom_hline(yintercept = matching_results_graph_ready$avg_diff, linetype = "dashed", color = "black") +
  annotate("text", x = Inf, y = matching_results_graph_ready$avg_diff, 
           label = paste("Average Difference:", round(matching_results_graph_ready$avg_diff, 2)), 
           hjust = 1.1, vjust = -0.5, size = 4, color = "black")


ggsave('/Users/Cooper/Documents/Folder for Contracts Project/matching_visual_w_k_fb_p_ls.png', plot = matching_visual_w_k_fb_p_ls, width = 10, height = 7, dpi = "retina")



