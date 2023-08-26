------------------------------------------------------------------------

**Data: MLB.com via {baseballr}**

------------------------------------------------------------------------

# Contents

- [Team Rankings](#team-rankings)
- [Runs Scored v Runs Allowed](#runs-scored-v-runs-allowed)
- [Composite Performance Rating (CPR)
  Rankings](#composite-performance-rating-cpr-rankings)
- [Records x CPR Ranks](#records-x-cpr-ranks)
- [Scorigami (2023 Only)](#scorigami-2023-only)
- [Historic MLB Scorigami (Since
  1901)](#historic-mlb-scorigami-since-1901)
- [Top Team Analysis](#top-team-analysis)
- [Team Margins Plot](#team-margins-plot)
- [Scatterplot of Margins of Victory and
  Defeat](#scatterplot-of-margins-of-victory-and-defeat)
- [Margins of Victory and Defeat](#margins-of-victory-and-defeat)
- [One-Run Games](#one-run-games)
- [Yesterday’s Highest-Scoring Game](#yesterdays-highest-scoring-game)
- [Best Records This Month](#best-records-this-month)
- [Runs Scored v Runs Allowed This
  Month](#runs-scored-v-runs-allowed-this-month)
- [Sudden Death Records](#sudden-death-records)
- [Eras Records](#eras-records)
- [First Inning Runs Scored v
  Allowed](#first-inning-runs-scored-v-allowed)
- [First Inning Runs Scored v Allowed
  Rates](#first-inning-runs-scored-v-allowed-rates)
- [First Inning Scoring](#first-inning-scoring)
- [Home and Away Performance](#home-and-away-performance)
- [Monthly v Season Win Percentages](#monthly-v-season-win-percentages)
- [Win Percentage v Run Differential as Percent of Runs
  Scored](#win-percentage-v-run-differential-as-percent-of-runs-scored)
- [Runs Scored in Wins and Losses](#runs-scored-in-wins-and-losses)
- [Win Percentage by Home Runs](#win-percentage-by-home-runs)
- [Win Percentage by Strikeouts](#win-percentage-by-strikeouts)
- [Home Runs by Strikeouts](#home-runs-by-strikeouts)
- [Home Runs in Wins and Losses](#home-runs-in-wins-and-losses)
- [When are teams scoring?](#when-are-teams-scoring)
- [Pythagorean Wins](#pythagorean-wins)
- [Close Games](#close-games)

### Team Rankings

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

### Runs Scored v Runs Allowed

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

### Composite Performance Rating (CPR) Rankings

![](README_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

### Records x CPR Ranks

![](README_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

### Scorigami (2023 Only)

![](README_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

### Historic MLB Scorigami (Since 1901)

![](README_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

### Top Team Analysis

![](README_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

### Team Margins Plot

![](README_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

### Scatterplot of Margins of Victory and Defeat

![](README_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

### Margins of Victory and Defeat

![](README_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

### One-Run Games

![](README_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

### Yesterday’s Highest-Scoring Game

![](README_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

### Best Records This Month

![](README_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

### Runs Scored v Runs Allowed This Month

![](README_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

### Sudden Death Records

![](README_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

### Eras Records

![](README_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

### First Inning Runs Scored v Allowed

![](README_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

### First Inning Runs Scored v Allowed Rates

![](README_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->

### First Inning Scoring

![](README_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->

### Home and Away Performance

![](README_files/figure-gfm/unnamed-chunk-28-1.png)<!-- -->

### Monthly v Season Win Percentages

![](README_files/figure-gfm/unnamed-chunk-29-1.png)<!-- -->

### Win Percentage v Run Differential as Percent of Runs Scored

![](README_files/figure-gfm/unnamed-chunk-30-1.png)<!-- -->

### Runs Scored in Wins and Losses

![](README_files/figure-gfm/unnamed-chunk-31-1.png)<!-- -->

### Win Percentage by Home Runs

![](README_files/figure-gfm/unnamed-chunk-32-1.png)<!-- -->

### Win Percentage by Strikeouts

![](README_files/figure-gfm/unnamed-chunk-33-1.png)<!-- -->

### Home Runs by Strikeouts

![](README_files/figure-gfm/unnamed-chunk-34-1.png)<!-- -->

### Home Runs in Wins and Losses

![](README_files/figure-gfm/unnamed-chunk-35-1.png)<!-- -->

### When are teams scoring?

![](README_files/figure-gfm/unnamed-chunk-36-1.png)<!-- -->

### Which teams play the closest games?

![](README_files/figure-gfm/unnamed-chunk-37-1.png)<!-- -->

### Pythagorean Wins

![](README_files/figure-gfm/unnamed-chunk-38-1.png)<!-- -->

    ## [1] "Run-adjusted margin is more correlated than pythagorean wins (0.93 vs. 0.928)"

![](README_files/figure-gfm/unnamed-chunk-39-1.png)<!-- -->

``` r
get_run_adj_margin = function(team) {
  scored = get_runs_scored(team)
  allowed = get_runs_allowed(team)
  diff = scored - allowed
  adj = round(diff / scored, 3)
  return(adj)
}

team_ram = data.frame(team = all_teams) |>
  mutate(RAM = sapply(team, get_run_adj_margin)) |>
  arrange(desc(RAM))

ram_res = end_games |>
  mutate(win_team = ifelse(home_score > away_score, home_team, away_team)) |>
  inner_join(team_ram, by = c("home_team" = "team")) |>
  rename(home_RAM = RAM) |>
  inner_join(team_ram, by = c("away_team" = "team")) |>
  rename(away_RAM = RAM) |>
  mutate(RAM_win = ifelse(home_RAM > away_RAM, home_team, away_team)) |>
  count(win_team == RAM_win) |>
  pull(n)

paste0("RAM-only game prediction accuracy: ", round(ram_res[2] / sum(ram_res), 4) * 100, "%")
```

    ## [1] "RAM-only game prediction accuracy: 56.95%"

### Close Games

![](README_files/figure-gfm/unnamed-chunk-41-1.png)<!-- -->
