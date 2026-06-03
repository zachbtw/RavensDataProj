library(tidyverse)
library(zoo)
  
tracking_1 <- read_csv("~/Documents/GitHub/RavensDataProj/rawdata/tracking_week_1.csv")
tracking_2 <- read_csv("~/Documents/GitHub/RavensDataProj/rawdata/tracking_week_2.csv")
tracking_3 <- read_csv("~/Documents/GitHub/RavensDataProj/rawdata/tracking_week_3.csv")
tracking_4 <- read_csv("~/Documents/GitHub/RavensDataProj/rawdata/tracking_week_4.csv")
tracking_5 <- read_csv("~/Documents/GitHub/RavensDataProj/rawdata/tracking_week_5.csv")
tracking_6 <- read_csv("~/Documents/GitHub/RavensDataProj/rawdata/tracking_week_6.csv")
tracking_7 <- read_csv("~/Documents/GitHub/RavensDataProj/rawdata/tracking_week_7.csv")
tracking_8 <- read_csv("~/Documents/GitHub/RavensDataProj/rawdata/tracking_week_8.csv")
tracking_9 <- read_csv("~/Documents/GitHub/RavensDataProj/rawdata/tracking_week_9.csv")
tracking_full <- bind_rows(tracking_1, tracking_2, tracking_3, tracking_4, tracking_5, tracking_6, tracking_7, tracking_8, tracking_9)

pass_plays <- read_csv("~/Documents/GitHub/RavensDataProj/rawdata/plays.csv") |> 
  filter(gameId %in% tracking_full$gameId, !is.na(passResult), passResult != "R", qbSpike != TRUE)
pass_plays <- pass_plays[,1:2]

tracking_full <- tracking_full |> 
  semi_join(pass_plays, by = c("gameId", "playId")) |> 
  standardize_play_direction()

write_csv(tracking_full, "fulltracking.csv")


standardize_play_direction <- function(tracking) {
  tracking |>
    mutate(
      x = ifelse(playDirection == "left", 120 - x, x),
      y = ifelse(playDirection == "left", 160 / 3 - y, y),
      
      # Adjust player direction
      dir = ifelse(playDirection == "left", dir + 180, dir),
      dir = ifelse(dir > 360, dir - 360, dir),
      
      # Adjust player orientation
      o = ifelse(playDirection == "left", o + 180, o),
      o = ifelse(o > 360, o - 360, o)
    )
}

games <- read_csv("~/Documents/GitHub/RavensDataProj/rawdata/games.csv")


players <- read_csv("~/Documents/GitHub/RavensDataProj/rawdata/players.csv")

OLs <- players |> 
  filter(position %in% c("C", "G", "T")) |> 
  select(OLID = nflId, everything())

pass_plays_OL <- read_csv("~/Documents/GitHub/RavensDataProj/rawdata/player_play.csv") |> 
  semi_join(pass_plays, by = c("gameId", "playId")) |> 
  left_join(players |> 
              select(nflId, displayName, position),
            by = "nflId") |> 
  select(gameId, playId, OLID = nflId, position, displayName, everything()) |> 
  filter(position %in% c("C", "G", "T"), !is.na(blockedPlayerNFLId1))


# Matchups and Processing -------------------------------------------------
tracking_full <- tracking_full |> 
  filter(!(gameId == 2022102000 & playId == 2095)) # taysom hill at qb, 2 qbs

blk_assign <- pass_plays_OL |> 
  select(gameId, playId, OLID = nflId, defId = blockedPlayerNFLId1)

qbs <- players |> 
  filter(position == "QB") |> 
  select(nflId)
qb_tracking <- tracking_full |> 
  semi_join(qbs, by = "nflId") |> 
  select(gameId, playId, frameId, qbId = nflId, qb_x = x, qb_y = y)

multi_qb_plays <- qb_tracking |> 
  distinct(gameId, playId, qbId) |> 
  count(gameId, playId) |> 
  filter(n > 1) |> 
  select(gameId, playId)



qb_tracking <- qb_tracking |> 
  anti_join(
    multi_qb_plays,
    by = c("gameId", "playId")
  ) |> 
  bind_rows(
    qb_tracking  |> 
      semi_join(
        multi_qb_plays,
        by = c("gameId", "playId")
      ) |> 
      filter(qbId != 45244)
  )

defender_tracking <- tracking_full |>
  select(
    gameId,
    playId,
    frameId,
    defId = nflId,
    def_x = x,
    def_y = y
  )


snap_end <- tracking_full |> 
  group_by(gameId, playId) |> 
  summarize(snap_frame = min(frameId[frameType == "SNAP"]),
            end_frame = min(frameId[event %in% c("pass_forward",
                                                 "pass_shovel",
                                                 "qb_sack",
                                                 "qb_strip_sack",
                                                 "fumble",
                                                 "pass_tipped")]),
            .groups = "drop")


get_distances <- function(blk_assign, defender_tracking, qb_tracking, snap_end) {
  blk_assign |>
    left_join(
      defender_tracking,
      by = c("gameId", "playId","defId")) |>
    left_join(qb_tracking,
              by = c("gameId", "playId", "frameId")) |>
    mutate(qb_dist = sqrt((def_x - qb_x)^2 +(def_y - qb_y)^2)) |>
    left_join(snap_end,
              by = c("gameId", "playId")) |>
    filter(frameId >= snap_frame, frameId <= end_frame)
}

get_play_summary <- function(distances, pass_plays_OL) {
  
  cushion_summary <- distances |> 
    group_by(gameId, playId, OLID) |> 
    summarize(cushion_area = sum(qb_dist, na.rm = TRUE) / 10,
              play_length = (first(end_frame) - first(snap_frame))/10,
              .groups = "drop")
  
  play_summary <- cushion_summary |>
    left_join(
      pass_plays_OL |>
        select(gameId, playId, OLID, pressure_allowed = pressureAllowedAsBlocker, time_to_pressure = timeToPressureAllowedAsBlocker),
      by = c("gameId", "playId", "OLID")
    )
  play_summary
}

OL_play_summary <- get_play_summary(distances, pass_plays_OL)

get_season_summary <- function(OL_play_summary, OLs) {
  
  OL_play_summary |>
    group_by(OLID) |>
    summarize(
      n_plays = n(),
      mean_cushion_area = mean(cushion_area, na.rm = TRUE),
      sd_cushion_area = sd(cushion_area, na.rm = TRUE),
      mean_time_to_pressure = mean(time_to_pressure, na.rm = TRUE),
      adj_time_to_pressure = mean(coalesce(time_to_pressure, play_length)),
      adj_time_to_pressure = mean(coalesce(time_to_pressure,
                                           na_if(play_length, Inf)),
                                  na.rm = TRUE),
      n_pressure_plays = replace_na(sum(pressure_allowed), sum(!is.na(time_to_pressure))),
      pct_pressure_plays = replace_na(mean(pressure_allowed), n_pressure_plays/n_plays),
      .groups = "drop"
    ) |>
    left_join(OLs, by = "OLID")
}

OL_season_summary <- get_season_summary(OL_play_summary, OLs) |> 
  filter(n_plays >= 120) |> 
  select(OLID, position, displayName, height, weight, birthDate, collegeName,
         n_plays, mean_cushion_area, sd_cushion_area, 
         n_pressure_plays, pct_pressure_plays, mean_time_to_pressure, adj_time_to_pressure)

write_csv(OL_season_summary, "OL_season_summary.csv")

# Clustering for Archetypes -----------------------------------------------

cluster_vars <- OL_season_summary %>%
  select(
    mean_cushion_area,
    pct_pressure_plays,
    mean_time_to_pressure,
    sd_cushion_area
  )

cluster_data <- scale(cluster_vars)

set.seed(123)

wss <- sapply(1:10, function(k) {
  kmeans(
    cluster_data,
    centers = k,
    nstart = 25
  )$tot.withinss
})

elbow_df <- data.frame(
  k = 1:10,
  wss = wss
)

ggplot(elbow_df,
       aes(k, wss)) +
  geom_line() +
  geom_point(size = 3) +
  scale_x_continuous(breaks = 1:10) +
  labs(title = "elbow plot for k-means clustering") +
  theme_minimal()

k <- 4

set.seed(12345)

km <- kmeans(
  cluster_data,
  centers = k,
  nstart = 50
)

OL_season_summary <- OL_season_summary %>%
  mutate(
    cluster = factor(km$cluster)
  )

OL_season_summary |> 
  group_by(cluster) |> 
  summarize(n_players = n(),
            mean_cushion_area = mean(mean_cushion_area),
            pct_pressure_plays = mean(pct_pressure_plays),
            mean_time_to_pressure = mean(mean_time_to_pressure),
            sd_cushion_area = mean(sd_cushion_area),
            .groups = "drop"
  ) |> 
  view()


# Pass Protection Score ---------------------------------------------------

OL_PPS <- OL_season_summary |> 
  mutate(
    z_cushion = as.numeric(scale(mean_cushion_area)),
    z_ttp = as.numeric(scale(mean_time_to_pressure)),
    z_pressure = as.numeric(scale(pct_pressure_plays))
  )

OL_PPS <- OL_scores |> 
  mutate(
    pass_protection_score =
      z_cushion +
      z_ttp -
      z_pressure
  )

OL_ranked <- OL_PPS |> 
  arrange(desc(pass_protection_score)) |> 
  select(displayName,
         position,
         pass_protection_score,
         mean_cushion_area,
         mean_time_to_pressure,
         pct_pressure_plays,
         n_plays,
         birthDate,
         OLID)



