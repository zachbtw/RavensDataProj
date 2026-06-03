library(tidyverse)
library(zoo)
library(ggplot2)
library(gganimate)
library(sportyR)
library(ggrepel)

plot_cushion_curve <- function(distances, OLID, show_plays = TRUE) {
  
  player_data <- distances |> 
    filter(OLID == !!OLID) |> 
    mutate(
      time_since_snap = (frameId - snap_frame) / 10
    )
  
  play_lengths <- player_data |> 
    group_by(gameId, playId) |> 
    summarize(
      play_length = max(time_since_snap),
      .groups = "drop"
    )
  
  cutoff_time <- quantile(
    play_lengths$play_length,
    probs = 0.95,
    na.rm = TRUE
  )
  
  mean_curve <- player_data |> 
    filter(time_since_snap <= cutoff_time) |> 
    group_by(time_since_snap) |> 
    summarize(
      mean_qb_dist = mean(qb_dist, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      smooth_dist = rollmean(
        mean_qb_dist,
        k = 5,
        fill = "extend"
      )
    )
  
  p <- ggplot()
  
  if (show_plays) {
    p <- p +
      geom_line(
        data = player_data,
        aes(
          x = time_since_snap,
          y = qb_dist,
          group = interaction(gameId, playId)
        ),
        alpha = 0.05
      )
  }
  
  p +
    geom_line(
      data = mean_curve,
      aes(
        x = time_since_snap,
        y = smooth_dist
      ),
      linewidth = 1.5
    ) +
    geom_point(
      data = mean_curve,
      aes(
        x = time_since_snap,
        y = smooth_dist
      )
    ) +
    labs(
      title = paste("OLID", OLID),
      x = "Seconds Since Snap",
      y = "Mean Defender Distance to QB"
    ) +
    theme_minimal()
  
}

plot_cushion_curves <- function(
    distances,
    OLIDs,
    show_plays = TRUE
) {
  
  player_data <- distances |>
    filter(OLID %in% OLIDs) |>
    mutate(
      time_since_snap = (frameId - snap_frame) / 10
    )
  
  cutoff_times <- player_data |>
    group_by(OLID, gameId, playId) |>
    summarize(
      play_length = max(time_since_snap),
      .groups = "drop"
    ) |>
    group_by(OLID) |>
    summarize(
      cutoff_time = quantile(
        play_length,
        probs = 0.95,
        na.rm = TRUE
      ),
      .groups = "drop"
    )
  
  mean_curve <- player_data |>
    left_join(
      cutoff_times,
      by = "OLID"
    ) |>
    filter(time_since_snap <= cutoff_time) |>
    group_by(OLID, time_since_snap) |>
    summarize(
      mean_qb_dist = mean(qb_dist, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(OLID, time_since_snap) |>
    group_by(OLID) |>
    mutate(
      smooth_dist = rollmean(
        mean_qb_dist,
        k = 5,
        fill = "extend"
      )
    ) |>
    ungroup()
  
  p <- ggplot()
  
  if (show_plays) {
    p <- p +
      geom_line(
        data = player_data,
        aes(
          x = time_since_snap,
          y = qb_dist,
          group = interaction(gameId, playId)
        ),
        alpha = 0.03
      )
  }
  
  p +
    geom_line(
      data = mean_curve,
      aes(
        x = time_since_snap,
        y = smooth_dist
      ),
      linewidth = 1.5
    ) +
    facet_wrap(~ OLID, ncol = 2) +
    labs(
      x = "Seconds Since Snap",
      y = "Defender Distance to QB"
    ) +
    theme_minimal()
  
}

bot4C <- OL_season_summary |> 
  filter(position == "C", n_plays >= 125) |> 
  slice_min(mean_time_to_pressure, n = 4)

top4C <- OL_season_summary |> 
  filter(position == "C", n_plays >= 125) |> 
  slice_max(mean_time_to_pressure, n = 4)

plot_cushion_curves(distances, bot4C$OLID)
plot_cushion_curves(distances, top4C$OLID)


plot_cushion_curve(distances, OLID = 48159) #good
plot_cushion_curve(distances, OLID = 46224) #bad

plot_cushion_curves(distances, bot4C$OLID)
plot_cushion_curves(distances, top4C$OLID)

# colors index ------------------------------------------------------------
team_colors <- c(
  football = "#654321",
  ARI = "#97233F",
  ATL = "#A71930",
  BAL = "#241773",
  BUF = "#00338D",
  CAR = "#0085CA",
  CHI = "#0B162A",
  CIN = "#FB4F14",
  CLE = "#311D00",
  DAL = "#003594",
  DEN = "#FB4F14",
  DET = "#0076B6",
  GB  = "#203731",
  HOU = "#03202F",
  IND = "#002C5F",
  JAX = "#006778",
  KC  = "#E31837",
  LA  = "#003594",
  LAC = "#0080C6",
  LV  = "#000000",
  MIA = "#008E97",
  MIN = "#4F2683",
  NE  = "#002244",
  NO  = "#D3BC8D",
  NYG = "#0B2265",
  NYJ = "#125740",
  PHI = "#004C54",
  PIT = "#FFB612",
  SEA = "#002244",
  SF  = "#AA0000",
  TB  = "#D50A0A",
  TEN = "#0C2340",
  WAS = "#5A1414"
)

get_random_play <- function(tracking) {
  
  game_id <- tracking |> 
    distinct(gameId) |> 
    pull(gameId) |> 
    sample(1)
  
  play_id <- tracking |> 
    filter(gameId == game_id) |> 
    distinct(playId) |> 
    pull(playId) |> 
    sample(1)
  
  list(gameId = game_id,
       playId = play_id)
}

animate_play <- function(tracking, game_id, play_id) {
  
  play_data <- tracking |> 
    filter(
      gameId == game_id,
      playId == play_id
    ) 
  
  p <- 
    geom_football(
      league = "NFL",
      x_trans = 60,
      y_trans = 26.6667
    ) +
    
    geom_point(
      data = play_data,
      aes(
        x = x,
        y = y,
        color = club
      ),
      size = 5
    ) +
    
    scale_color_manual(values = team_colors) +
    
    geom_text(
      data = play_data,
      aes(
        x = x,
        y = y,
        label = jerseyNumber
      ),
      color = "white",
      fontface = "bold",
      size = 3
    ) +
    
    labs(
      title = paste("Game", game_id, "- Play", play_id),
      subtitle = "Frame {closest_state}"
    ) +
    
    transition_states(
      frameId,
      transition_length = 1,
      state_length = 1
    )
  
  animate(
    p,
    fps = 10,
    width = 1200,
    height = 600,
    renderer = gifski_renderer()
  )
}

animate_play(tracking_full, rand1$gameId, rand1$playId)
animate_play(tracking_full, 2022091807,	2727)


# Clustering Visualization ------------------------------------------------

ggplot(
  OL_season_summary,
  aes(x = pct_pressure_plays,
    y = mean_time_to_pressure,
    color = cluster
  )
) +
  geom_point(size = 3) +
  labs(
    x = "Pressure Rate",
    y = "Mean Time to Pressure",
    color = "Cluster"
  ) +
  theme_minimal()


# Potential Talent Visualization ------------------------------------------

# First, Rookies
ggplot(filter(OL_ranked, is.na(birthDate)),
  aes(x = n_plays, y = pass_protection_score)) +
  geom_point() +
  geom_text_repel(
    aes(label = displayName),
    nudge_y = 0,
    box.padding = 0.4,
    point.padding = 0.3,
    max.overlaps = Inf) +
  labs(title = "Rookie Linemen Pass Protection Scores by Number of Plays",
       x = "Number of Plays", y = "Pass Protection Score") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  
# Just Centers
ggplot(filter(OL_ranked, position == "C"),
       aes(x = n_plays, y = pass_protection_score)) +
  geom_point() +
  geom_text_repel(
    aes(label = displayName),
    nudge_y = 0,
    box.padding = 0.4,
    point.padding = 0.3,
    max.overlaps = Inf) +
  labs(title = "Center Pass Protection Scores by Number of Plays",
       x = "Number of Plays", y = "Pass Protection Score") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Just Guards
ggplot(filter(OL_ranked, position == "G"),
       aes(x = n_plays, y = pass_protection_score)) +
  geom_point() +
  geom_text_repel(
    aes(label = displayName),
    nudge_y = 0,
    box.padding = 0.4,
    point.padding = 0.3,
    max.overlaps = Inf) +
  labs(title = "OG Pass Protection Scores by Number of Plays",
       x = "Number of Plays", y = "Pass Protection Score") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Just Tackles
ggplot(filter(OL_ranked, position == "T"),
       aes(x = n_plays, y = pass_protection_score)) +
  geom_point() +
  geom_text_repel(
    aes(label = displayName),
    nudge_y = 0,
    box.padding = 0.4,
    point.padding = 0.3,
    max.overlaps = Inf) +
  labs(title = "OT Pass Protection Scores by Number of Plays",
       x = "Number of Plays", y = "Pass Protection Score") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


# Best/Worst Graphs -------------------------------------------------------

plot_cushion_curves <- function(distances, OLIDs, show_plays = TRUE) {
  
  player_data <- distances |>
    filter(OLID %in% OLIDs) |>
    mutate(
      time_since_snap = (frameId - snap_frame) / 10
    )
  
  cutoff_times <- player_data |>
    group_by(OLID, gameId, playId) |>
    summarize(
      play_length = max(time_since_snap),
      .groups = "drop"
    ) |>
    group_by(OLID) |>
    summarize(
      cutoff_time = quantile(
        play_length,
        probs = 0.95,
        na.rm = TRUE
      ),
      .groups = "drop"
    )
  
  mean_curve <- player_data |>
    left_join(
      cutoff_times,
      by = "OLID"
    ) |>
    filter(time_since_snap <= cutoff_time) |>
    group_by(OLID, time_since_snap) |>
    summarize(
      mean_qb_dist = mean(qb_dist, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(OLID, time_since_snap) |>
    group_by(OLID) |>
    mutate(
      smooth_dist = rollmean(
        mean_qb_dist,
        k = 5,
        fill = "extend"
      )
    ) |>
    ungroup()
  
  p <- ggplot()
  
  if (show_plays) {
    p <- p +
      geom_line(
        data = player_data,
        aes(
          x = time_since_snap,
          y = qb_dist,
          group = interaction(gameId, playId)
        ),
        alpha = 0.03
      )
  }
  
  p +
    geom_line(
      data = mean_curve,
      aes(
        x = time_since_snap,
        y = smooth_dist
      ),
      linewidth = 1.5
    ) +
    facet_wrap(~ OLID, ncol = 2) +
    labs(
      x = "Seconds Since Snap",
      y = "Defender Distance to QB"
    ) +
    theme_minimal()
  
}

plot_cushion_curves <- function(distances, OLIDs, label_df = NULL, show_plays = TRUE) {
  
  player_data <- distances |>
    filter(OLID %in% OLIDs) |>
    mutate(
      time_since_snap = (frameId - snap_frame) / 10
    )
  
  cutoff_times <- player_data |>
    group_by(OLID, gameId, playId) |>
    summarize(
      play_length = max(time_since_snap),
      .groups = "drop"
    ) |>
    group_by(OLID) |>
    summarize(
      cutoff_time = quantile(
        play_length,
        probs = 0.95,
        na.rm = TRUE
      ),
      .groups = "drop"
    )
  
  mean_curve <- player_data |>
    left_join(cutoff_times, by = "OLID") |>
    filter(time_since_snap <= cutoff_time) |>
    group_by(OLID, time_since_snap) |>
    summarize(
      mean_qb_dist = mean(qb_dist, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(OLID, time_since_snap) |>
    group_by(OLID) |>
    mutate(
      smooth_dist = zoo::rollmean(
        mean_qb_dist,
        k = 5,
        fill = "extend"
      )
    ) |>
    ungroup()
  
  p <- ggplot()
  
  # raw plays
  if (show_plays) {
    p <- p +
      geom_line(
        data = player_data,
        aes(
          x = time_since_snap,
          y = qb_dist,
          group = interaction(gameId, playId)
        ),
        alpha = 0.1
      )
  }
  
  # mean curve
  p <- p +
  geom_line(
    data = mean_curve,
    aes(x = time_since_snap, y = smooth_dist),
    linewidth = 1.5,
    color = "black"
  )
  
  # labels for Best/Worst if provided
  if (!is.null(label_df)) {
    
    label_positions <- mean_curve |>
      group_by(OLID) |>
      summarize(
        x = max(time_since_snap),
        y = last(smooth_dist),
        .groups = "drop"
      ) |>
      left_join(label_df, by = "OLID")
    
    p <- p +
      geom_text(
        data = label_positions,
        aes(x = x, y = y, label = label),
        vjust = -0.5,
        fontface = "bold",
        size = 4
      )
  }
  
  p +
    facet_wrap(~ OLID, ncol = 2) +
    labs(title = "Saahdiq Charles (Worst PPS) vs Tyler Linderbaum (Best PPS)",
         subtitle = "By-Play (light) and Average (bold) Distances From Blocking Assignment Player to QB Over Time",
      x = "Seconds Since Snap",
      y = "Defender Distance to QB"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
          plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 10)) 
}

top <- OL_ranked |> 
  slice_max(pass_protection_score, n = 1)

bottom <- OL_ranked |> 
  slice_min(pass_protection_score, n = 1)

selected_OLs <- bind_rows(top, bottom)
selected_OLs <- selected_OLs |> 
  mutate(label = case_when(pass_protection_score == max(pass_protection_score) ~ paste0(displayName, " (Best)"),
                           pass_protection_score == min(pass_protection_score) ~ paste0(displayName, " (Worst)"),
                           TRUE ~ displayName))

selected_OLs <- bind_rows(
  top |>  mutate(label = "Best"),
  bottom |>  mutate(label = "Worst")
)

plot_cushion_curves(distances, selected_OLs$OLID, label_df = selected_OLs)
