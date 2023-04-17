mlb cpr
================

# *this is very much a work in progress*

### setup

``` r
library(tidyverse)
library(tidymodels)
library(tvthemes)
library(janitor)
library(patchwork)
library(baseballr)

theme_custom = theme_avatar() +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_line(linewidth = 0.5, colour = "#D6D0C4"),
        panel.grid.minor = element_line(linewidth = 0.5, colour = "#D6D0C4"))

theme_set(theme_custom)
```

### data import

``` r
loop_dates = seq.Date(from = as_date("2023-03-30"), to = Sys.Date() - 1, by = 1)
end_games = data.frame(date = NULL, away_team = NULL, away_score = NULL, home_score = NULL, home_team = NULL)

for (i in 1:length(loop_dates)) {
  loop_df = mlb_game_pks(date = loop_dates[i]) |>
    mutate(date = loop_dates[i]) |>
    select(date, away_team = teams.away.team.name, away_score = teams.away.score,
           home_score = teams.home.score, home_team = teams.home.team.name)
  
  end_games = rbind(end_games, loop_df)
}

end_games = na.omit(end_games)
```

``` r
all_teams = sort(unique(end_games$home_team))

get_team_rspg = function(team) {
  home_scores = end_games |> filter(home_team == team) |> pull(home_score)
  away_scores = end_games |> filter(away_team == team) |> pull(away_score)
  return(round(mean(c(home_scores, away_scores)), 3))
}

get_team_rapg = function(team) {
  home_scores = end_games |> filter(home_team == team) |> pull(away_score)
  away_scores = end_games |> filter(away_team == team) |> pull(home_score)
  return(round(mean(c(home_scores, away_scores)), 3))
}

rpg_df = data.frame(team = all_teams) |>
  mutate(rspg = sapply(team, get_team_rspg),
         rapg = sapply(team, get_team_rapg))
```

``` r
rpg_df |>
  mutate(diff = rspg - rapg) |>
  ggplot(aes(rspg, rapg)) +
  geom_point() +
  geom_abline() +
  annotate("text", x = 6.5, y = 4, label = "these teams are\npretty good", col = "springgreen4") +
  annotate("text", x = 5, y = 7, label = "these teams \nkinda suck", col = "indianred3") +
  ggrepel::geom_text_repel(aes(label = team), size = 3, max.overlaps = 30)
```

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
end_extended = end_games |>
  left_join(rpg_df, by = c("home_team" = "team")) |>
  rename(home_rspg = rspg, home_rapg = rapg) |>
  left_join(rpg_df, by = c("away_team" = "team")) |>
  rename(away_rspg = rspg, away_rapg = rapg) |>
  mutate(home_exp = round((home_rspg + away_rapg) / 2, 3),
         away_exp = round((away_rspg + home_rapg) / 2, 3),
         home_off_cpr = home_score - home_exp,
         home_def_cpr = away_exp - away_score,
         away_off_cpr = away_score - away_exp,
         away_def_cpr = home_exp - home_score)
```

``` r
get_off_cpr = function(team) {
  home_cpr = end_extended |> filter(home_team == team) |> pull(home_off_cpr)
  away_cpr = end_extended |> filter(away_team == team) |> pull(away_off_cpr)
  return(round(mean(c(home_cpr, away_cpr)), 3))
}

get_def_cpr = function(team) {
  home_cpr = end_extended |> filter(home_team == team) |> pull(home_def_cpr)
  away_cpr = end_extended |> filter(away_team == team) |> pull(away_def_cpr)
  return(round(mean(c(home_cpr, away_cpr)), 3))
}

team_cpr = data.frame(team = all_teams) |>
  mutate(off_cpr = sapply(team, get_off_cpr),
         def_cpr = sapply(team, get_def_cpr),
         total_cpr = off_cpr + def_cpr)
```

``` r
team_cpr |>
  mutate(pos_lab = ifelse(total_cpr > 0, total_cpr, ""),
         neg_lab = ifelse(total_cpr < 0, total_cpr, "")) |>
  ggplot(aes(reorder(team, total_cpr), total_cpr)) +
  geom_col(aes(fill = total_cpr), show.legend = F) +
  geom_text(aes(label = pos_lab), size = 2.5, hjust = -0.25) +
  geom_text(aes(label = neg_lab), size = 2.5, hjust = 1.25) +
  scale_fill_gradient(low = "indianred3", high = "springgreen4") +
  coord_flip(ylim = c(-1.75, 1.75)) +
  labs(x = NULL, y = "Composite Performance Rating", title = "MLB CPR Rankings as of 17 April 2023")
```

![](README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

### MLB SCORIGAMI?
