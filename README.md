
# Contents

- [Runs Scored v. Runs Allowed](#runs-scored-v.-runs-allowed)
- [Composite Performance Rating (CPR)
  Rankings](#composite-performance-rating-cpr-rankings)
- [Records x CPR Ranks](#records-x-cpr-ranks)
- [Scorigami](#scorigami)
- [Top Team Analysis](#top-team-analysis)
- [Team Margins Plot](#team-margins-plot)
- [Scatterplot of Margins of Victory and
  Defeat](#scatterplot-of-margins-of-victory-and-defeat)
- [Margins of Victory and Defeat](#margins-of-victory-and-defeat)
- [One-Run Games](#one-run-games)
- [Yesterday’s Highest-Scoring Game](#yesterdays-highest-scoring-game)

### Runs Scored v. Runs Allowed

<details>
<summary>
View Code
</summary>

``` r
rpg_df = rpg_df |>
  mutate(diff = rspg - rapg)

top_teams = rpg_df |>
  slice_max(diff, n = 3) |>
  pull(team)

bottom_teams = rpg_df |>
  slice_min(diff, n = 3) |>
  pull(team)

rpg_df |>
  left_join(team_abbrevs, by = "team") |>
  mutate(top_lab = ifelse(team %in% top_teams, abb, ""),
         bot_lab = ifelse(team %in% bottom_teams, abb, ""),
         other_lab = ifelse(!team %in% top_teams & !team %in% bottom_teams, abb, "")) |>
  ggplot(aes(rspg, rapg)) +
  geom_point(aes(col = team), size = 5, show.legend = F) +
  geom_vline(xintercept = mean(rpg_df$rspg), linetype = "dashed", alpha = 0.5) +
  geom_hline(yintercept = mean(rpg_df$rapg), linetype = "dashed", alpha = 0.5) +
  scale_color_manual(values = team_color_codes) +
  ggrepel::geom_text_repel(aes(label = top_lab), size = 3.5) +
  ggrepel::geom_text_repel(aes(label = bot_lab), size = 3.5) +
  ggrepel::geom_text_repel(aes(label = other_lab), size = 3.5, alpha = 0.25, max.overlaps = 30) +
  labs(x = "Runs Scored Per Game", y = "Runs Allowed Per Game",
       title = "Runs Scored v. Runs Allowed in 2023",
       subtitle = "Labeled Teams are Top or Bottom Three")
```

</details>

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

### Composite Performance Rating (CPR) Rankings

<details>
<summary>
View Code
</summary>

``` r
day_label = paste(day(Sys.Date()), month(Sys.Date(), label = T, abbr = F)[1], year(Sys.Date()))

team_cpr |>
  mutate(pos_lab = ifelse(total_cpr > 0, round(total_cpr, 3), ""),
         neg_lab = ifelse(total_cpr < 0, round(total_cpr, 3), "")) |>
  ggplot(aes(reorder(team, total_cpr), total_cpr)) +
  geom_col(aes(fill = team), show.legend = F) +
  geom_text(aes(label = pos_lab), size = 2.5, hjust = -0.25) +
  geom_text(aes(label = neg_lab), size = 2.5, hjust = 1.25) +
  scale_fill_manual(values = team_color_codes) +
  coord_flip(ylim = c(-1.75, 1.75)) +
  labs(x = NULL, y = "Composite Performance Rating", title = paste0("MLB CPR Rankings as of ", day_label))
```

</details>

![](README_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

### Records x CPR Ranks

<details>
<summary>
View Code
</summary>

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

</details>

![](README_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

### Scorigami

<details>
<summary>
View Code
</summary>

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

# THE MOST RECENT DATA ABOVE HAS THE GAME BY GAME DATA

last_gami = paste0("Last Scorigami: ", most_recent$win_team, " def. ", most_recent$lose_team, " ",
                   most_recent$win_score, "-", most_recent$lose_score, " on ", most_recent$date)

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
  mutate(win_score = as.numeric(win_score),
         lose_score = as.numeric(lose_score)) |>
  ggplot(aes(win_score, lose_score)) +
  geom_point(shape = "square", size = 8, aes(col = n), show.legend = F) +
  geom_text(aes(label = n), size = 3) +
  annotate("text", x = 4, y = 9.5, label = yesterday_text, size = 4) +
  geom_rect(aes(xmin = 1, xmax = 7, ymin = 8.75, ymax = 10.25), col = "black", fill = "transparent") +
  scale_color_gradient(high = "#5B7E54", low = "#97BA90") +
  scale_x_continuous(breaks = 1:25) +
  scale_y_continuous(breaks = 1:25) +
  labs(x = "Winning Score", y = "Losing Score",
       title = "2023 MLB Scorigami", subtitle = last_gami)
```

</details>

![](README_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

### Top Team Analysis

<details>
<summary>
View Code
</summary>

``` r
wbt_df = end_games |>
  mutate(home_win = ifelse(home_score > away_score, 1, 0),
         home_wb2 = ifelse(home_score > away_score + 1, 1, 0),
         home_wb3 = ifelse(home_score > away_score + 2, 1, 0),
         home_wb4 = ifelse(home_score > away_score + 3, 1, 0),
         home_wb5 = ifelse(home_score > away_score + 4, 1, 0),
         away_win = ifelse(away_score > home_score, 1, 0),
         away_wb2 = ifelse(away_score > home_score + 1, 1, 0),
         away_wb3 = ifelse(away_score > home_score + 2, 1, 0),
         away_wb4 = ifelse(away_score > home_score + 3, 1, 0),
         away_wb5 = ifelse(away_score > home_score + 4, 1, 0))

get_wb2_rate = function(team) {
  home_wb2 = wbt_df |> filter(home_team == team) |> pull(home_wb2)
  away_wb2 = wbt_df |> filter(away_team == team) |> pull(away_wb2)
  return(round(mean(c(home_wb2, away_wb2)), 3))
}

get_wb3_rate = function(team) {
  home_wb3 = wbt_df |> filter(home_team == team) |> pull(home_wb3)
  away_wb3 = wbt_df |> filter(away_team == team) |> pull(away_wb3)
  return(round(mean(c(home_wb3, away_wb3)), 3))
}

get_wb4_rate = function(team) {
  home_wb4 = wbt_df |> filter(home_team == team) |> pull(home_wb4)
  away_wb4 = wbt_df |> filter(away_team == team) |> pull(away_wb4)
  return(round(mean(c(home_wb4, away_wb4)), 3))
}

get_wb5_rate = function(team) {
  home_wb5 = wbt_df |> filter(home_team == team) |> pull(home_wb5)
  away_wb5 = wbt_df |> filter(away_team == team) |> pull(away_wb5)
  return(round(mean(c(home_wb5, away_wb5)), 3))
}

best_record_teams = team_records |>
  slice_max(win_pct, n = 3, with_ties = F) |>
  pull(team)

team_wbt = data.frame(team = best_record_teams) |>
  left_join(select(team_records, team, win_pct), by = "team") |>
  mutate(wb2 = sapply(team, get_wb2_rate),
         wb3 = sapply(team, get_wb3_rate),
         wb4 = sapply(team, get_wb4_rate),
         wb5 = sapply(team, get_wb5_rate))

vis_df = team_wbt |>
  pivot_longer(cols = !team, names_to = "metric", values_to = "value") |>
  mutate(metric = case_when(metric == "win_pct" ~ "Win Percentage",
                            metric == "wb2" ~ "Win by 2+ Rate",
                            metric == "wb3" ~ "Win by 3+ Rate",
                            metric == "wb4" ~ "Win by 4+ Rate",
                            metric == "wb5" ~ "Win by 5+ Rate"),
         metric = factor(metric, levels = c("Win Percentage", "Win by 2+ Rate", "Win by 3+ Rate",
                                            "Win by 4+ Rate", "Win by 5+ Rate")),
         val_lab = paste0(value * 100, "%"))

rec_teams = sort(unique(vis_df$team))
max_colors = c(team_color_codes[which(team_colors$team == rec_teams[1])],
               team_color_codes[which(team_colors$team == rec_teams[2])],
               team_color_codes[which(team_colors$team == rec_teams[3])])

vis_df |>
  ggplot(aes(metric, value)) +
  geom_col(aes(fill = team), position = "dodge") +
  geom_text(aes(label = val_lab, group = team), size = 3, vjust = -0.5, position = position_dodge2(width = 0.9)) +
  scale_fill_manual(values = max_colors) +
  labs(x = NULL, y = "Rate", fill = NULL, title = "Top Team Win Rates",
       subtitle = "Top Three Teams by Win Percentage Included") +
  theme(axis.text.y = element_blank())
```

</details>

![](README_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

### Team Margins Plot

<details>
<summary>
View Code
</summary>

``` r
end_margins = end_games |>
  mutate(home_margin = home_score - away_score,
         away_margin = away_score - home_score)

get_margin = function(team) {
  home_margins = pull(filter(end_margins, home_team == team), home_margin)
  away_margins = pull(filter(end_margins, away_team == team), away_margin)
  return(round(mean(c(home_margins, away_margins)), 3))
}

team_margins = data.frame(team = all_teams) |>
  mutate(margin = sapply(team, get_margin))

team_margins |>
  mutate(pos_lab = ifelse(margin >= 0, margin, ""),
         neg_lab = ifelse(margin < 0, margin, "")) |>
  ggplot(aes(reorder(team, margin), margin)) +
  geom_col(aes(fill = team), show.legend = F) +
  geom_text(aes(label = pos_lab), size = 2.5, hjust = -0.25) +
  geom_text(aes(label = neg_lab), size = 2.5, hjust = 1.25) +
  coord_flip(ylim = c(-3.25, 3.25)) +
  scale_fill_manual(values = team_color_codes) +
  labs(y = "Average Margin", x = NULL, title = "Average Margins by Team")
```

</details>

![](README_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

### Scatterplot of Margins of Victory and Defeat

<details>
<summary>
View Code
</summary>

``` r
vmarg_mean = mean(team_margins$win_margin)
dmarg_mean = mean(team_margins$def_margin)

team_margins |>
  mutate(wl_diff = win_margin - def_margin) |>
  left_join(team_abbrevs, by = "team") |>
  ggplot(aes(win_margin, def_margin)) +
  geom_point(size = 4, aes(col = team), show.legend = F) +
  ggrepel::geom_text_repel(aes(label = abb), size = 4) +
  geom_hline(yintercept = dmarg_mean, linetype = "dashed", alpha = 0.4) +
  geom_vline(xintercept = vmarg_mean, linetype = "dashed", alpha = 0.4) +
  scale_color_manual(values = team_color_codes) +
  labs(x = "Avg. Margin of Victory", y = "Avg. Margin of Defeat",
       title = "Scatterplot of Margins of Victory/Defeat",
       subtitle = "Dashed lines represent league averages") +
  annotate("text", x = 2.5, y = -5.1, label = "Win Small, Lose Big", fontface = "italic") +
  annotate("text", x = 5, y = -5.1, label = "Win Big, Lose Big", fontface = "italic") +
  annotate("text", x = 2.5, y = -2.1, label = "Win Small, Lose Small", fontface = "italic") +
  annotate("text", x = 5, y = -2.1, label = "Win Big, Lose Small", fontface = "italic") +
  geom_rect(aes(xmin = 2, xmax = 3, ymin = -2.25, ymax = -2), col = "black", fill = "transparent") +
  geom_rect(aes(xmin = 2, xmax = 3, ymin = -5.25, ymax = -5), col = "black", fill = "transparent") +
  geom_rect(aes(xmin = 4.5, xmax = 5.5, ymin = -2.25, ymax = -2), col = "black", fill = "transparent") +
  geom_rect(aes(xmin = 4.5, xmax = 5.5, ymin = -5.25, ymax = -5), col = "black", fill = "transparent") +
  coord_cartesian(xlim = c(min(team_margins$win_margin) - 0.25, max(team_margins$win_margin) + 0.25),
                  ylim = c(min(team_margins$def_margin) - 0.75, max(team_margins$def_margin) + 0.75))
```

</details>

![](README_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

### Margins of Victory and Defeat

<details>
<summary>
View Code
</summary>

``` r
team_margins |>
  mutate(marg_diff = abs(win_margin - def_margin)) |>
  ggplot() +
  geom_col(aes(reorder(team, marg_diff), win_margin, fill = team), show.legend = F) +
  geom_col(aes(reorder(team, marg_diff), def_margin, fill = team), show.legend = F, alpha = 0.75) +
  geom_text(aes(reorder(team, marg_diff), win_margin, label = round(win_margin, 2)), size = 2.5, hjust = -0.25) +
  geom_text(aes(reorder(team, marg_diff), def_margin, label = round(def_margin, 2)), size = 2.5, hjust = 1.25) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = team_color_codes) +
  coord_flip(ylim = c(min(team_margins$def_margin) - 0.25, max(team_margins$win_margin) + 0.25)) +
  labs(x = NULL, y = "← Avg. Margin of Defeat | Avg. Margin of Victory →",
       title = "Team Margins of Victory and Defeat", subtitle = "Ordered by Largest to Smallest Range") +
  theme(axis.text.x = element_blank())
```

</details>

![](README_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

### One-Run Games

<details>
<summary>
View Code
</summary>

``` r
one_runs = end_games |>
  filter(abs(home_score - away_score) == 1) |>
  mutate(win_team = ifelse(home_score > away_score, home_team, away_team),
         lose_team = ifelse(home_score > away_score, away_team, home_team),
         home_win = ifelse(win_team == home_team, 1, 0))

or_wins = rename(count(one_runs, win_team), wins = n)
or_losses = rename(count(one_runs, lose_team), losses = n)

left_join(or_wins, or_losses, by = c("win_team" = "lose_team")) |>
  mutate(gp = wins + losses,
         win_per = round(wins / gp, 3)) |>
  left_join(team_abbrevs, by = c("win_team" = "team")) |>
  ggplot(aes(gp, win_per)) +
  geom_point(aes(col = win_team), size = 4, show.legend = F) +
  scale_color_manual(values = team_color_codes) +
  ggrepel::geom_text_repel(aes(label = abb)) +
  labs(x = "One-Run Games Played", y = "Win Percentage in One-Run Games",
       title = "How are MLB teams performing in one-run games?")
```

</details>

![](README_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

### Yesterday’s Highest-Scoring Game

<details>
<summary>
View Code
</summary>

``` r
get_date_pks = function(fdate) {
  df = mlb_game_pks(date = fdate) |>
    select(game_pk, date = officialDate, away_team = teams.away.team.name, home_team = teams.home.team.name) |>
    mutate(date = as_date(date))
  
  return(df)
}

all_game_pks = data.frame(game_pk = NULL, date = NULL, away_team = NULL, home_team = NULL)

for (i in 1:length(loop_dates)) {
  df = get_date_pks(loop_dates[i])
  all_game_pks = rbind(all_game_pks, df)
}

max_home_team = end_games |>
  filter(date == Sys.Date() - 1) |>
  mutate(total_score = home_score + away_score) |>
  slice_max(total_score, n = 1) |>
  pull(home_team)

max_pk = all_game_pks |>
  filter(date == Sys.Date() - 1) |>
  filter(home_team == max_home_team) |>
  pull(game_pk)

vis_df = mlb_pbp(game_pk = max_pk) |>
  filter(count.outs.end == 3) |>
  select(inning = about.inning, away_team, away_score = result.awayScore, home_score = result.homeScore, home_team) |>
  pivot_longer(c(home_score, away_score), names_to = "team", values_to = "score") |>
  mutate(team = ifelse(team == "home_score", home_team, away_team)) |>
  group_by(inning, team) |>
  slice_max(score, n = 1)

max_teams = sort(unique(vis_df$team))
max_colors = c(team_color_codes[which(team_colors$team == max_teams[1])],
               team_color_codes[which(team_colors$team == max_teams[2])])

high_df = end_games |>
  filter(date == Sys.Date() - 1) |>
  mutate(total_score = home_score + away_score,
         win_team = ifelse(home_score > away_score, home_team, away_team),
         lose_team = ifelse(home_score > away_score, away_team, home_team),
         win_score = ifelse(home_score > away_score, home_score, away_score),
         lose_score = ifelse(home_score > away_score, away_score, home_score)) |>
  slice_max(total_score, n = 1)

high_lab = paste0(high_df$win_team, " def. ", high_df$lose_team, " ", high_df$win_score, "-", high_df$lose_score)

vis_df |>
  ggplot(aes(inning, score)) +
  geom_line(aes(col = team), linewidth = 2) +
  scale_x_continuous(breaks = 1:25) +
  scale_y_continuous(breaks = 1:25) +
  scale_color_manual(values = max_colors) +
  labs(x = "Inning", y = "Score", col = NULL, subtitle = high_lab,
       title = "Scoring Trends for Yesterday's Highest-Scoring Game")
```

</details>

![](README_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->
