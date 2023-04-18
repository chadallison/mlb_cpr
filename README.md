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
        plot.subtitle = element_text(hjust = 0.5, size = 9, vjust = 2.5, face = "italic"),
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
  labs(x = NULL, y = "Composite Performance Rating", title = "MLB CPR Rankings as of 18 April 2023")
```

![](README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
end_wl = end_games |>
  mutate(win_team = ifelse(home_score > away_score, home_team, away_team),
         lose_team = ifelse(home_score > away_score, away_team, home_team))

team_wins = end_wl |>
  count(win_team) |>
  rename(team = win_team, wins = n)

team_losses = end_wl |>
  count(lose_team) |>
  rename(team = lose_team, losses = n)

team_records = team_wins |>
  left_join(team_losses, by = "team") |>
  mutate(win_pct = round(wins / (wins + losses), 3),
         record = paste0(wins, "-", losses))
```

``` r
team_records |>
  select(team, win_pct) |>
  left_join(team_cpr, by = "team") |>
  select(team, win_pct, total_cpr) |>
  mutate(record_rank = rank(-win_pct, ties.method = "average"),
         cpr_rank = rank(-total_cpr, ties.method = "average"),
         xxx = case_when(cpr_rank > record_rank ~ "Not as Good as Record",
                         cpr_rank < record_rank ~ "Better Than Record",
                         cpr_rank == record_rank ~ "Accurate Record")) |>
  ggplot(aes(cpr_rank, record_rank)) +
  geom_point(aes(col = xxx), size = 3) +
  geom_abline(linetype = "dashed", alpha = 0.5) +
  ggrepel::geom_text_repel(aes(label = team), size = 3, alpha = 0.25) +
  scale_color_manual(values = c("black", "springgreen4", "indianred3")) +
  labs(x = "CPR Rank", y = "Record Rank", col = NULL,
       title = "2023 MLB Records x CPR Ranks")
```

![](README_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

### MLB SCORIGAMI?

``` r
gami_df = end_games |>
  transmute(date, win_team = ifelse(home_score > away_score, home_team, away_team),
            lose_team = ifelse(home_score > away_score, away_team, home_team),
            win_score = ifelse(home_score > away_score, home_score, away_score),
            lose_score = ifelse(home_score > away_score, away_score, home_score))

score_counts = gami_df |>
  mutate(win_score = factor(win_score),
         lose_score = factor(lose_score)) |>
  count(win_score, lose_score)

most_recent = gami_df |>
  mutate(win_score = factor(win_score),
         lose_score = factor(lose_score)) |>
  left_join(score_counts, by = c("win_score", "lose_score")) |>
  filter(n == 1) |>
  slice_max(date, n = 1)

last_gami = paste0("Last Scorigami: ", most_recent$win_team, " def. ", most_recent$lose_team, " ", most_recent$win_score, "-", most_recent$lose_score, " on ", most_recent$date)

yesterday_gami = gami_df |>
  mutate(win_score = factor(win_score),
         lose_score = factor(lose_score)) |>
  left_join(score_counts, by = c("win_score", "lose_score")) |>
  filter(n == 1 & date == Sys.Date() - 1) |>
  nrow()

yesterday_text = case_when(yesterday_gami == 0 ~ "No Scorigami Yesterday :(",
                           yesterday_gami == 1 ~ "One Scorigami Yesterday :)",
                           yesterday_gami > 1 ~ paste0(yesterday_gami, " Scorigamis Yesterday :)"))

gami_df |>
  mutate(win_score = factor(win_score),
         lose_score = factor(lose_score)) |>
  left_join(score_counts, by = c("win_score", "lose_score")) |>
  ggplot(aes(win_score, lose_score)) +
  geom_point(shape = "square", size = 8, aes(col = n), show.legend = F) +
  geom_text(aes(label = n), size = 3) +
  annotate("text", x = 4, y = 9.5, label = yesterday_text, size = 4) +
  geom_rect(aes(xmin = 1, xmax = 7, ymin = 8.75, ymax = 10.25), col = "black", fill = "transparent") +
  scale_color_gradient(high = "#5B7E54", low = "#97BA90") +
  labs(x = "Winning Score", y = "Losing Score",
       title = "2023 MLB Scorigami", subtitle = last_gami)
```

![](README_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->
