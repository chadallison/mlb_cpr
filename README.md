
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
- [Runs Scored This Month](#runs-scored-this-month)
- [Runs Allowed This Month](#runs-allowed-this-month)
- [Best Home Records](#best-home-records)
- [Best Away Records](#best-away-records)
- [Runs Scored v Runs Allowed This
  Month](#runs-scored-v-runs-allowed-this-month)

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

### Runs Scored This Month

![](README_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

### Runs Allowed This Month

![](README_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

### Best Home Records

![](README_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

### Best Away Records

![](README_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

### Runs Scored v Runs Allowed This Month

![](README_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->

<!-- ### what if hits won games? -->

``` r
end_with_pks = end_games |> left_join(all_game_pks, by = c("date", "home_team", "away_team"))
hit_criteria = c("Single", "Double", "Triple", "Home Run")

get_pk_hits = function(fpk) {
  f_data = mlb_pbp(fpk) |> filter(details.isInPlay == T & result.event %in% hit_criteria)
  home_team = unique(f_data$home_team)
  return(f_data |> mutate(is_hit = 1) |> group_by(batting_team) |> summarise(n_hits = sum(is_hit)) |>
    mutate(batting_team = ifelse(batting_team == home_team, "home_hits", "away_hits")) |>
    pivot_wider(names_from = "batting_team", values_from = "n_hits") |>
    transmute(game_pk = fpk, home_hits, away_hits))
}

bind_pk_set = data.frame(game_pk = NULL, home_hits = NULL, away_hits = NULL)
bad_pks = c(718700, 718558, 718371)

for (i in 1:length(all_game_pks$game_pk)) {
  if (all_game_pks$game_pk[i] %in% bad_pks) {
    print("bad pk")
    f_data = data.frame(game_pk = all_game_pks$game_pk[i], home_hits = NA, away_hits = NA)
  } else {
  f_data = get_pk_hits(all_game_pks$game_pk[i])
  }
  bind_pk_set = rbind(bind_pk_set, f_data)
  if (i %% 5 == 0) print(i)
}

bind_pk_set
```
