library(dplyr)
library(nflreadr)
library(ggplot2)
library(nflplotR)

rosters_2024_rookies <- load_rosters(2024) |> mutate(rookie = ifelse(entry_year == season, 1, 0)) |> 
  filter(status == "ACT" | status == "INA") |> 
  mutate(rookie_pct = mean(rookie))
sum(rosters_2024_rookies$rookie)
#280

rosters_2010_rookies <- load_rosters(2010) |> mutate(rookie = ifelse(entry_year == season, 1, 0)) |> 
  filter(status == "ACT") |> 
  mutate(rookie_pct = mean(rookie))
sum(rosters_2010_rookies$rookie)
#306

###### More rookies on rosters in 2010 

##### What do we want to look at?

rosters_2024_rookies <- rosters_2024_rookies |> group_by(team) |> #Percent rookies by team
  mutate(team_rookie_pct = mean(rookie))
rosters_2010_rookies <- rosters_2010_rookies |> group_by(team) |> #Percent rookies by team
  mutate(team_rookie_pct = mean(rookie))

# Team rookie rates for both 2024 and 2021 and combine
team_rookies_2024 <- rosters_2024_rookies |> group_by(team) |> summarize(season, team, team_rookie_pct) |> 
  ungroup() |> unique()
team_rookies2010 <- rosters_2010_rookies |> group_by(team) |> summarize(season, team, team_rookie_pct) |> 
  ungroup() |> unique()|> mutate(team = clean_team_abbrs(team))
team_rookies <- team_rookies_2024 |> right_join(team_rookies2010, by = "team") |> 
  summarize(team, season.x, team_rookie_pct_2024 = team_rookie_pct.x, season.y, team_rookie_pct_2010 = team_rookie_pct.y) |> 
  mutate(team_rookie_diff = team_rookie_pct_2024 - team_rookie_pct_2010)

# Reshape the data
team_rookies2 <- pivot_longer(team_rookies, 
                          cols = c(team_rookie_pct_2024, team_rookie_pct_2010), 
                          names_to = "season", 
                          values_to = "rookie_pct") |> 
  summarize(team, season = ifelse(season == "team_rookie_pct_2024", "2024", "2010"), rookie_pct, team_rookie_diff,
            team_rookie_pct_2024 = ifelse(season == "2024", rookie_pct, 0)) |> 
  ungroup() |> 
  group_by(team) |> 
  mutate(team_rookie_pct_2024 = first(team_rookie_pct_2024))

# Edit this graph to show rookie percentage for team over time.
teams_colors_logos <- load_teams()
teams_colors_logos_real <- teams_colors_logos |> select(team_abbr, team_logo_espn) 
team_rookies2 <- team_rookies2 |> 
  left_join(teams_colors_logos_real, by = c('team' = 'team_abbr'))

# Load logos from URLs in a list
logo_list <- lapply(team_rookies2$team_logo_espn, image_read)

# Convert the images to raster objects
logo_grobs <- lapply(logo_list, rasterGrob, interpolate = TRUE)


p <- team_rookies2 |> 
  ggplot(aes(fct_reorder(team, team_rookie_pct_2024, .desc = TRUE), rookie_pct, fill = factor(ifelse(season == 2024, "2024", "2010")))) +
  geom_col(position="dodge", color='gray50', width=0.7) + 
  scale_fill_manual(values = c("2024" = "darkred", "2010" = "purple"), 
                    name = "Year") +
  scale_x_discrete(name = NULL,
                   labels = rep("", nrow(team_rookies2))) +
                     #team_rookies2$team_logo_espn[(1:nrow(team_rookies2) %% 2) != 0]) +
  theme(plot.title = element_text(hjust=0.5, size=20),
        axis.text.y = element_text(colour="black"),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank(),
        plot.subtitle = element_text(hjust=0.5),
        legend.position = "top",
        axis.text.x = ggtext::element_markdown(),
        legend.title = element_blank()
        ) + 
  labs(x = "Team", y = "Percentage of Rookies Rostered", 
       title = "How has Rookie Percentage Changed?", 
       subtitle = "Data via NFLReadR | Cooper Davis") +
  #geom_text(aes(label = round(max_apy_cap_pct, 3)), 
  #          position = position_dodge2(width = 0.7), 
  #          vjust = 1.5, size = 1.75, color = "white") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray70")

for (i in seq_along(logo_grobs)) {
  p <- p + annotation_custom(logo_grobs[[i]], 
                             xmin = i, 
                             xmax = i, 
                             ymin = 0, 
                             ymax = max(team_rookies2$rookie_pct) * 0.1)
}
print(p)
