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

``` r
get_pythag_wins = function(team) {
  home_scored = end_games |> filter(home_team == team) |> pull(home_score)
  away_scored = end_games |> filter(away_team == team) |> pull(away_score)
  home_allow = end_games |> filter(home_team == team) |> pull(away_score)
  away_allow = end_games |> filter(away_team == team) |> pull(home_score)
  scored = sum(home_scored) + sum(away_scored)
  allowed = sum(home_allow) + sum(away_allow)
  x = scored ^ 2 / (scored ^ 2 + allowed ^ 2)
  return(round(x, 3))
}

team_records |>
  select(team, win_pct) |>
  mutate(py_wins = sapply(team, get_pythag_wins)) |>
  inner_join(team_abbrevs, by = "team") |>
  ggplot(aes(py_wins, win_pct)) +
  geom_point(aes(col = team), size = 4, show.legend = F) +
  scale_color_manual(values = team_color_codes) +
  geom_abline(linetype = "dashed", alpha = 0.5) +
  ggrepel::geom_text_repel(aes(label = abb), size = 3.5) +
  labs(x = "Pythagorean Win Percentage", y = "Actual Win Percentage",
       title = "Win Percentage by Pythagorean Wins",
       subtitle = "Teams above the dashed line have better records than they 'should'") +
  scale_x_continuous(breaks = seq(0, 1, by = 0.05), labels = scales::percent) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.05), labels = scales::percent)
```

![](README_files/figure-gfm/unnamed-chunk-38-1.png)<!-- -->

``` r
# team_records |>
#   select(team, win_pct) |>
#   mutate(py_wins = sapply(team, get_pythag_wins),
#          diff = py_wins - win_pct) |>
#   arrange(desc(diff))
```
