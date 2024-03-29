------------------------------------------------------------------------

**Data: MLB.com via {baseballr}** \| Last Updated: October 5, 2023 at
08:07:46

------------------------------------------------------------------------

# Contents

- [Team Rankings](#team-rankings)
- [Runs Scored v Runs Allowed](#runs-scored-v-runs-allowed)
- [Composite Performance Rating (CPR)
  Rankings](#composite-performance-rating-cpr-rankings)
- [Scorigami (2023 Only)](#scorigami-2023-only)
- [Historic MLB Scorigami (Since
  1901)](#historic-mlb-scorigami-since-1901)
- [Top Team Analysis](#top-team-analysis)
- [Team Margins Plot](#team-margins-plot)
- [Scatterplot of Margins of Victory and
  Defeat](#scatterplot-of-margins-of-victory-and-defeat)
- [Margins of Victory and Defeat](#margins-of-victory-and-defeat)
- [One-Run Games](#one-run-games)
- [Best Records This Month](#best-records-this-month)
- [Runs Scored v Runs Allowed This
  Month](#runs-scored-v-runs-allowed-this-month)
- [Eras Records](#eras-records)
- [Home and Away Performance](#home-and-away-performance)
- [Monthly v Season Win Percentages](#monthly-v-season-win-percentages)
- [Win Percentage v Run Differential as Percent of Runs
  Scored](#win-percentage-v-run-differential-as-percent-of-runs-scored)
- [Pythagorean Wins](#pythagorean-wins)
- [Close Games](#close-games)

### Team Rankings

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

### Runs Scored v Runs Allowed

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

### Composite Performance Rating (CPR) Rankings

![](README_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

### Scorigami (2023 Only)

![](README_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

### Historic MLB Scorigami (Since 1901)

![](README_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

### Top Team Analysis

![](README_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

### Team Margins Plot

![](README_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

### Scatterplot of Margins of Victory and Defeat

![](README_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

### Margins of Victory and Defeat

![](README_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

### One-Run Games

![](README_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

### Best Records This Month

![](README_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

### Runs Scored v Runs Allowed This Month

![](README_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

### Eras Records

![](README_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

### Home and Away Performance

![](README_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

### Monthly v Season Win Percentages

![](README_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

### Win Percentage v Run Differential as Percent of Runs Scored

![](README_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

### Which teams play the closest games?

![](README_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

### Pythagorean Wins

![](README_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->

    ## [1] "Run-adjusted margin is more correlated than pythagorean wins (0.942 vs. 0.941)"

![](README_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->

### Close Games

![](README_files/figure-gfm/unnamed-chunk-29-1.png)<!-- -->

### Monthly Win Trends

![](README_files/figure-gfm/unnamed-chunk-30-1.png)<!-- -->

### Monthly Scoring Trends

![](README_files/figure-gfm/unnamed-chunk-31-1.png)<!-- -->

### Best Team of Past Month?

``` r
# getting vector of all team names
all_teams = mlb_teams(season = 2023) |>
  filter(sport_name == "Major League Baseball") |>
  pull(team_full_name) |>
  sort()

# getting end game data and adding info
end_games = mlb_schedule(season = 2023, level_ids = "1") |>
  filter(teams_away_team_name %in% all_teams &
           teams_home_team_name %in% all_teams &
           series_description == "Regular Season" &
           status_detailed_state == "Final") |>
  select(date, game_pk,
         away_team = teams_away_team_name, away_score = teams_away_score,
         home_score = teams_home_score, home_team = teams_home_team_name) |>
  mutate(date = as_date(date),
         win_team = ifelse(home_score > away_score, home_team, away_team),
         win_score = ifelse(home_score > away_score, home_score, away_score),
         lose_score = ifelse(home_score > away_score, away_score, home_score),
         lose_team = ifelse(home_score > away_score, away_team, home_team),
         final_score = paste0(win_score, "-", lose_score),
         description = paste0(win_team, " def. ", lose_team, " ", final_score))

# best records of the past month
# end_games |>
#   filter(date >= Sys.Date() - 31) |>
#   count(win_team) |>
#   rename(team = win_team, wins = n) |>
#   inner_join(end_games |>
#   filter(date >= Sys.Date() - 31) |>
#   count(lose_team) |>
#   rename(team = lose_team, losses = n), by = "team") |>
#   mutate(win_pct = round(wins / (wins + losses), 3)) |>
#   arrange(desc(win_pct))

# function to get runs scored in past month
get_past_month_runs_scored = function(team) {
  home_runs = end_games |> filter(date >= Sys.Date() - 31 & home_team == team) |> pull(home_score) |> sum()
  away_runs = end_games |> filter(date >= Sys.Date() - 31 & away_team == team) |> pull(away_score) |> sum()
  return(home_runs + away_runs)
}

# function to get runs allowed in past month
get_past_month_runs_allowed = function(team) {
  home_runs = end_games |> filter(date >= Sys.Date() - 31 & home_team == team) |> pull(away_score) |> sum()
  away_runs = end_games |> filter(date >= Sys.Date() - 31 & away_team == team) |> pull(home_score) |> sum()
  return(home_runs + away_runs)
}

# team color codes
team_color_codes = c("#A71930", "#CE1141", "#DF4601", "#BD3039", "#0E3386",
                     "#27251F", "#C6011F", "#00385D", "#333366", "#0C2340",
                     "#EB6E1F", "#004687", "#BA0021", "#005A9C", "#00A3E0",
                     "#FFC52F", "#D31145", "#FF5910", "#003087", "#003831",
                     "#E81828", "#FDB827", "#FFC425", "#FD5A1E", "#005C5C",
                     "#C41E3A", "#8FBCE6", "#C0111F", "#134A8E", "#FFB7C5")

# getting run-adjusted margins for each team in past month
past_month_ram = data.frame(team = all_teams) |>
  mutate(scored = sapply(team, get_past_month_runs_scored),
         allowed = sapply(team, get_past_month_runs_allowed),
         diff = scored - allowed,
         ram = round(diff / scored, 3),
         pos_lab = ifelse(ram >= 0, ram, ""),
         neg_lab = ifelse(ram < 0, ram, ""))

past_month_ram |>
  ggplot(aes(reorder(team, ram), ram)) +
  geom_col(aes(fill = team), show.legend = F) +
  geom_text(aes(label = pos_lab), size = 3, hjust = -0.25) +
  geom_text(aes(label = neg_lab), size = 3, hjust = 1.25) +
  annotate("text", x = 1.5, y = 0.25, label = "Data: MLB.com via {baseballr}", size = 3, fontface = "italic") +
  scale_fill_manual(values = team_color_codes) +
  scale_y_continuous(breaks = seq(-3, 3, by = 0.1)) +
  coord_flip(ylim = c(min(past_month_ram$ram) * 1.1, max(past_month_ram$ram) * 1.1)) +
  labs(x = NULL, y = "RAM",
       title = "Past Month Run-Adjusted Margin (RAM)",
       subtitle = "RAM = (Runs Scored - Runs Allowed) / Runs Scored")
```

![](README_files/figure-gfm/unnamed-chunk-32-1.png)<!-- -->

``` r
margin_results = end_games |>
  mutate(win_margin = win_score - lose_score,
         margin_description = paste0(win_team, " win by ", win_margin),
         loser_desc = paste0(lose_team, " lose by ", win_margin)) |>
  count(margin_description) |>
  mutate(desc = paste0(margin_description, " (", n, ")")) |>
  arrange(desc(n), margin_description) |>
  pull(desc)

margin_results_L = end_games |>
  mutate(win_margin = win_score - lose_score,
         margin_description = paste0(win_team, " win by ", win_margin),
         loser_desc = paste0(lose_team, " lose by ", win_margin)) |>
  count(loser_desc) |>
  mutate(desc = paste0(loser_desc, " (", n, ")")) |>
  arrange(desc(n), loser_desc) |>
  pull(desc)

upper_limit = 10

for (i in 1:upper_limit) {
  print(margin_results[i])
}
```

    ## [1] "Cincinnati Reds win by 1 (34)"
    ## [1] "Miami Marlins win by 1 (33)"
    ## [1] "Baltimore Orioles win by 1 (30)"
    ## [1] "Milwaukee Brewers win by 1 (29)"
    ## [1] "Philadelphia Phillies win by 1 (29)"
    ## [1] "Washington Nationals win by 1 (29)"
    ## [1] "Cleveland Guardians win by 1 (27)"
    ## [1] "New York Mets win by 1 (25)"
    ## [1] "San Francisco Giants win by 1 (25)"
    ## [1] "Seattle Mariners win by 1 (25)"

``` r
print("========================================")
```

    ## [1] "========================================"

``` r
for (i in 1:upper_limit) {
  print(margin_results_L[i])
}
```

    ## [1] "Cleveland Guardians lose by 1 (31)"
    ## [1] "Chicago White Sox lose by 1 (30)"
    ## [1] "Cincinnati Reds lose by 1 (29)"
    ## [1] "New York Mets lose by 1 (29)"
    ## [1] "Minnesota Twins lose by 1 (27)"
    ## [1] "Oakland Athletics lose by 1 (27)"
    ## [1] "Seattle Mariners lose by 1 (26)"
    ## [1] "St. Louis Cardinals lose by 1 (26)"
    ## [1] "Boston Red Sox lose by 1 (25)"
    ## [1] "Tampa Bay Rays lose by 1 (25)"

``` r
end_games |>
  mutate(diff = win_score - lose_score) |>
  filter(diff >= 3) |>
  count(win_team) |>
  arrange(desc(n)) |>
  rename(`Team` = win_team,
         `Wins by 3+` = n)
```

    ## # A tibble: 30 × 2
    ##    Team                `Wins by 3+`
    ##    <chr>                      <int>
    ##  1 Los Angeles Dodgers           74
    ##  2 Tampa Bay Rays                64
    ##  3 Texas Rangers                 62
    ##  4 Atlanta Braves                61
    ##  5 San Diego Padres              57
    ##  6 Toronto Blue Jays             55
    ##  7 Baltimore Orioles             54
    ##  8 Chicago Cubs                  53
    ##  9 Houston Astros                52
    ## 10 New York Yankees              48
    ## # ℹ 20 more rows

``` r
reg23 = end_games |>
  mutate(total_score = home_score + away_score)

post22 = mlb_schedule(season = 2022, level_ids = "1") |>
  filter(teams_away_team_name %in% all_teams &
           teams_home_team_name %in% all_teams &
           series_description %in% c("Division Series", "League Championship Series", "Wild Card Game", "World Series") &
           status_detailed_state == "Final") |>
  select(date, game_pk,
         away_team = teams_away_team_name, away_score = teams_away_score,
         home_score = teams_home_score, home_team = teams_home_team_name) |>
  mutate(date = as_date(date),
         win_team = ifelse(home_score > away_score, home_team, away_team),
         win_score = ifelse(home_score > away_score, home_score, away_score),
         lose_score = ifelse(home_score > away_score, away_score, home_score),
         lose_team = ifelse(home_score > away_score, away_team, home_team),
         final_score = paste0(win_score, "-", lose_score),
         description = paste0(win_team, " def. ", lose_team, " ", final_score),
         total_score = home_score + away_score)
```

``` r
# most postseason games had six or less runs scored
# post22 |>
#   count(total_score <= 6)

reg23 |>
  filter(total_score <= 6) |>
  count(win_team) |>
  rename(team = win_team, wins = n) |>
  inner_join(reg23 |>
  filter(total_score <= 6) |>
  count(lose_team) |>
  rename(team = lose_team, losses = n), by = "team") |>
  mutate(win_pct = round(wins / (wins + losses), 3)) |>
  arrange(desc(win_pct))
```

    ## # A tibble: 30 × 4
    ##    team                  wins losses win_pct
    ##    <chr>                <int>  <int>   <dbl>
    ##  1 Atlanta Braves          24     10   0.706
    ##  2 Los Angeles Dodgers     26     12   0.684
    ##  3 Baltimore Orioles       30     17   0.638
    ##  4 Toronto Blue Jays       31     20   0.608
    ##  5 Seattle Mariners        31     21   0.596
    ##  6 Arizona Diamondbacks    25     17   0.595
    ##  7 Pittsburgh Pirates      29     22   0.569
    ##  8 Los Angeles Angels      26     20   0.565
    ##  9 Tampa Bay Rays          24     19   0.558
    ## 10 Chicago Cubs            25     20   0.556
    ## # ℹ 20 more rows

``` r
end_games |>
  mutate(runs_scored = home_score + away_score) |>
  count(runs_scored) |>
  ggplot(aes(runs_scored, n)) +
  geom_col(fill = "springgreen4") +
  scale_x_continuous(breaks = seq(0, 30, by = 1)) +
  scale_y_continuous(breaks = seq(0, 250, by = 25)) +
  labs(x = "Runs Scored", y = "Number of Games")
```

![](README_files/figure-gfm/unnamed-chunk-37-1.png)<!-- -->
