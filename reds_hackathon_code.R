library(tidyverse)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(baseballr)
library(caret)
library(xgboost)
library(pdp)
library(data.table)
library(grid)
library(gridExtra)
library(jpeg)
library(patchwork)

# set working directory
# setwd()

# read data
read_csv("savant_pitch_level.csv") -> pitches
read_csv("fangraphs_season_level.csv") -> stats
read_csv("woba_weights.csv") -> woba

woba %>%
  pivot_longer(cols = c(wOBA:cFIP)) %>%
  mutate(name = recode(name, "wBB" = "walk",
                       "wHBP" = "hit_by_pitch")) %>%
  filter(Season >= 2021) -> woba

# events to filter out
patterns <- c("^stolen", "^caught", "^pickoff", "^wild_pitch",
              "^catcher_", "^passed_", "game_advisory")

# using pre-calculated times faced
pitches %>%
  arrange(game_date, game_year, game_pk, desc(inning_topbot), pitcher, at_bat_number, pitch_number) %>%
  dplyr::select(game_date, game_year, game_pk, pitcher, role_key, batter, stand, 
                at_bat_number, times_faced, events, estimated_woba_using_speedangle) %>%
  rename(xwoba = estimated_woba_using_speedangle) %>%
  left_join(woba, by = c("game_year" = "Season", "events" = "name")) %>%
  mutate(xwoba = if_else(events == "walk" | events == "hit_by_pitch", 
                         value, xwoba))  %>%
  mutate(xwoba = case_when(!is.na(xwoba) ~ xwoba,
                           grepl("strikeout", events) ~ 0)) %>%
  group_by(game_date, game_year, game_pk, pitcher, role_key, batter, at_bat_number, times_faced) %>%
  dplyr::slice(n()) -> order

# calculate each pitcher's xwoba agianst each batter handiness
order %>%
  filter(!is.na(xwoba)) %>%
  group_by(game_year, pitcher, role_key, stand) %>%
  summarise(xwoba = mean(xwoba),
            obs = n()) %>% 
  ungroup() %>%
  group_by(game_year, pitcher, role_key) %>%
  pivot_wider(names_from = stand, values_from = c(xwoba, obs)) %>%
  mutate(across(xwoba_L:obs_R, ~ replace(., is.na(.), 0)),
         hand_xwoba_diff = abs(xwoba_L - xwoba_R)) %>%
  mutate(obs = obs_L + obs_R) -> pit_splits

w_splits <- mean(pit_splits$obs)/2
splits_avg <- weighted.mean(pit_splits$hand_xwoba_diff, pit_splits$obs)

# regress pitching splits to mean
pit_splits %>%
  mutate(hand_xwoba_diff_adj = ((hand_xwoba_diff * obs) + (splits_avg * w_splits))/
           (obs + w_splits)) %>%
  dplyr::select(-c(obs, obs_L, obs_R)) -> pit_splits

# calculate difference between first and second time through batting order
order %>%
  ungroup() %>%
  dplyr::select(-c(at_bat_number, events, value)) %>%
  pivot_wider(names_from = times_faced, values_from = xwoba,
              names_prefix = "pa_") %>%
  dplyr::select(-pa_3, -pa_4, -pa_5) %>%
  mutate(diff = pa_1 - pa_2) %>%
  filter(!is.na(diff)) -> order

#table(order$role_key)
options(scipen = 999999)

mean(order$diff) -> tto2_xwoba_diff
mean(order$pa_1) -> tto1_xwoba

# calculate average xwoba difference 2nd vs 1st TTO per pitcher season 
order %>%
  group_by(game_year, pitcher, role_key) %>%
  summarise(tto1 = mean(pa_1),
            tto2_diff = mean(diff),
            tto_obs = n()) -> tto2

w <- mean(tto2$tto_obs)/2

# calculate tto with regression to mean
tto2 %>%
  mutate(tto2_adj_diff = ((tto2_diff * tto_obs) + (tto2_xwoba_diff * w))/
           (tto_obs + w),
         tto1_adj = ((tto1 * tto_obs) + (tto1_xwoba * w))/
           (tto_obs + w)) -> tto2

# read csv on player position data (used to remove position players)
read_csv("mlb_positions_21-23.csv") -> positions

# join fg stats with positions df to remove position players
stats %>%
  left_join(positions, by = c("Season" = "season", "MLBAMID" = "player_id")) %>%
  filter(primary_position_name == "Pitcher" | primary_position_name == "Two-Way Player") -> stats

# calculate pitches and batters faced (BF) per outing/game
stats %>%
  dplyr::select(Season, MLBAMID, Role, WAR, G, Pitches, TBF, Stuff_plus, Location_plus) %>%
  filter(!is.na(Location_plus)) %>%
  mutate(p_outing = Pitches/G,
         BF_outing = TBF/G,
         p_batter = Pitches/TBF) %>%
  rename(game_year = Season,
         pitcher = MLBAMID,
         role_key = Role) -> role_metrics

w_pb <- mean(role_metrics$Pitches)/4
pb_avg <- weighted.mean(role_metrics$p_batter, role_metrics$Pitches)

# regress pitches per batter to mean
role_metrics %>%
  mutate(p_batter_adj = ((p_batter * Pitches) + (pb_avg * w_pb))/
           (Pitches + w_pb)) -> role_metrics

# calculate pitcher pitch count totals
pitches %>%
  filter(!is.na(pitch_type)) %>%
  group_by(game_year, pitcher, role_key, pitch_type) %>%
  summarise(pitches = n()) -> pitch_type_totals

# calculate pct of time pitchers throw pitch types
pitch_type_totals %>%
  left_join(pitches %>%
              filter(!is.na(pitch_type)) %>%
              group_by(game_year, pitcher, role_key) %>%
              summarise(season_pitches = n()),
            by = c("game_year", "pitcher", "role_key")) %>%
  mutate(pct = pitches/season_pitches) -> pct_pitch_type

# calculate pitcher pitch counts/totals per game
pitches %>%
  arrange(game_pk, pitcher, at_bat_number, pitch_number) %>%
  group_by(game_pk, pitcher, role_key) %>%
  summarise(gm_pitches = n()) -> pitch_counts

# create vector of fastballs
fbs <- c("FF", "SI", "FC", "FS", "FO", "FA")

# calculating velocity slopes (velocity differences/pitches)
pitches %>%
  arrange(game_year, game_date, game_pk, pitcher, role_key, at_bat_number, pitch_number) %>%
  group_by(game_year, game_date, game_pk, pitcher, role_key) %>%
  dplyr::select(release_speed, pitch_type) %>%
  mutate(pitch_count = row_number()) %>%
  dplyr::slice(-(1:3)) %>%
  filter(!(is.na(pitch_type) | is.na(release_speed))) %>%
  ungroup() %>%
  filter(str_detect(pitch_type, paste(fbs, collapse = "|"))) %>%
  group_by(game_year, game_date, game_pk, pitcher, role_key, pitch_type) %>%
  #dplyr::select(at_bat_number, pitch_number, release_speed) %>%
  #head(1000) -> temp
  summarise( # -at_bat_number, -pitch_number
    avg_beg_velo = mean(release_speed[order(pitch_count)][1:3], na.rm = T),
    avg_end_velo = mean(release_speed[order(-pitch_count)][1:3], na.rm = T),
    pit_nums_beg = mean(pitch_count[order(pitch_count)][1:3], na.rm = T),
    pit_nums_end = mean(pitch_count[order(-pitch_count)][1:3], na.rm = T),
    velo_diff =  avg_end_velo - avg_beg_velo,
    type_pitches = n()
  ) -> maintain_velo

maintain_velo %>%
  left_join(pct_pitch_type, by = c("game_year", "pitcher", "role_key", "pitch_type")) %>%
  left_join(pitch_counts, by = c("game_pk", "pitcher", "role_key")) %>% 
  filter(type_pitches >= 6 & gm_pitches >= 17 & pct >= 0.1) %>%
  mutate(velo_slope = (avg_end_velo - avg_beg_velo)/(pit_nums_end - pit_nums_beg)) %>%
  # filter(pitcher == 663158 & game_year == 2023) %>%
  # view()
  group_by(game_year, pitcher, role_key, pitch_type) %>%
  summarise(velo_slope = weighted.mean(velo_slope, w = type_pitches),
            initial_velo = weighted.mean(avg_beg_velo, w = type_pitches),
            type_pitches = sum(type_pitches),
            obs = n()
  ) %>%
  ungroup() %>% group_by(across(c(game_year:role_key))) %>%
  arrange(game_year, pitcher, role_key, initial_velo, type_pitches) %>%
  slice_max(order_by = initial_velo, n = 1) %>%
  slice_max(order_by = type_pitches, n = 1) -> maintain_velo

w_velo <- mean(maintain_velo$type_pitches)/2
v_slope_avg <- weighted.mean(maintain_velo$velo_slope, maintain_velo$type_pitches)

# calculate velo slopes with regression to mean
maintain_velo %>%
  mutate(velo_slope_adj = ((velo_slope * type_pitches) + (v_slope_avg * w_velo))/
           (type_pitches + w_velo)) -> maintain_velo

# determine number of pitch types per pitcher
pitches %>%
  group_by(game_year, pitcher, role_key, pitch_type) %>%
  summarise(pitches = n()) %>%
  ungroup() %>%
  group_by(game_year, pitcher, role_key) %>%
  summarise(num_pitches = n()) -> num_pitch_types

# extract player names for each pitcher id
pitches %>%
  distinct(pitcher, player_name) -> players

# extract stuff, location, and pct of each pitch type
stats %>%
  dplyr::select(1:9, starts_with("Loc"), starts_with("Stf"), FA_pct_sc:UN_pct_sc) %>%
  dplyr::select(-SC_pct_sc, -KN_pct_sc, -EP_pct_sc, -UN_pct_sc) %>%
  mutate(across(Loc_plus_CH:CH_pct_sc, ~replace(., is.na(.), 0))) %>%
  dplyr::select(-Location_plus) %>% 
  rename(pitcher = MLBAMID, game_year = Season, role_key = Role) -> pitch_grades

# begin with role metrics
# join all data together
role_metrics %>%
  left_join(players, by = c("pitcher")) %>%
  relocate(player_name, .after = pitcher) %>%
  left_join(tto2, by = c("game_year", "pitcher", "role_key")) %>%
  left_join(pit_splits, by = c("game_year", "pitcher", "role_key")) %>%
  left_join(maintain_velo, by = c("game_year", "pitcher", "role_key")) %>%
  left_join(pitch_grades, by = c("game_year", "pitcher", "role_key")) %>%
  left_join(num_pitch_types, by = c("game_year", "pitcher", "role_key"))-> data


data %>%
  filter(Pitches >= 200) %>%
  dplyr::select(game_year:role_key, Pitches, tto_obs:tto1_adj, hand_xwoba_diff_adj, 
                velo_slope_adj, p_batter_adj, Loc_plus_CH:CH_pct_sc,
                Stuff_plus, Location_plus, num_pitches) %>%
  filter(tto_obs >= 20) -> data_model

# na check
colSums(is.na(data_model))

# ---- modeling ----
set.seed(1)

# split dataset into test and train
sample <- sample(c(T, F), nrow(data_model), replace = T, prob = c(0.8, 0.2))
train <- data_model[sample, ]
test <- data_model[!sample, ]

train_control = trainControl(
  method = "cv", number = 5, search = "grid", #repeats = 2,
  savePredictions = "all") 

searchGrid <- expand.grid(
  max_depth = seq(1, 5), # tree depth
  nrounds = c((3:8)*50), # number of trees
  eta = c(0.025, 0.05, .1), # learning rate
  gamma = 0,
  subsample = 0.5,
  min_child_weight = 1,
  colsample_bytree = 0.6)


set.seed(1)

# 2 TTO (diff between 2 and 1)
model2 = train( # train[, c(8, 10:15)]
  tto2_adj_diff ~ ., data = train[, c(7, 9:38)], 
  method = "xgbTree", trControl = train_control, tuneGrid = searchGrid,
  metric = "RMSE", verbosity = 0, verbose = T)

set.seed(1)
# 1 TTO
model1 = train( # train[, c(9:15)]
  tto1_adj  ~ ., data = train[, c(8:38)], 
  method = "xgbTree", trControl = train_control, tuneGrid = searchGrid,
  metric = "RMSE", verbosity = 0, verbose = T)


# identify pitchers who started and relieved in a given year (exclude these pitchers)
data %>%
  group_by(game_year, pitcher) %>%
  summarise(n = n()) -> swing_pits

# output model 2 results - diff between 1st and 2nd TTO
print(model2)

#
model2$pred %>%
  left_join(model2$bestTune %>%
              dplyr::select(nrounds, max_depth, eta) %>% 
              mutate(bestTune = 1), 
            by = c("nrounds", "max_depth", "eta")) %>%
  filter(bestTune == 1) %>%
  dplyr::select(rowIndex, pred) %>%
  rename(pred_tto2_diff = pred) -> fitted2

# output model 1 results - xwoba first time through order
print(model1)

#
model1$pred %>%
  left_join(model1$bestTune %>%
              dplyr::select(nrounds, max_depth, eta) %>% 
              mutate(bestTune = 1), 
            by = c("nrounds", "max_depth", "eta")) %>%
  filter(bestTune == 1) %>%
  dplyr::select(rowIndex, pred) %>%
  rename(pred_tto1 = pred) %>%
  left_join(fitted2, by = c("rowIndex")) -> fitted

#predict(model1, data[, c(8, 10, 11, 14, 19, 20, 23)])

# create df of fitted value based on training data
fitted %>%
  left_join(train %>%
              ungroup() %>%
              mutate(rowIndex = row_number()), 
            by = c("rowIndex")) %>%
  relocate(c(game_year:role_key), .before = rowIndex) %>%
  dplyr::select(-rowIndex) %>%
  mutate(pred_tto2 = pred_tto1 - pred_tto2_diff, .after = pred_tto2_diff) %>%
  mutate(pred_agg = (pred_tto1 + pred_tto2)/2, .after = pred_tto2) %>%
  arrange(pred_agg) %>%
  mutate(test = 0) -> pred_train

# create df of fitted value based on excluded test data
test %>%
  mutate(pred_tto1 = predict(model1, test[, c(8:38)]),
         pred_tto2_diff = predict(model2, test[, c(7, 9:38)])) %>%
  mutate(pred_tto2 = pred_tto1 - pred_tto2_diff, .after = pred_tto2_diff) %>%
  mutate(pred_agg = (pred_tto1 + pred_tto2)/2, .after = pred_tto2) %>%
  arrange(pred_agg) %>%
  mutate(test = 1) -> pred_test

# aggregate test and training sets together and remove pitchers 
# who were both relievers and starters in a given season
pred_train %>%
  left_join(swing_pits, by = c("game_year", "pitcher")) %>%
  mutate(across(c(hand_xwoba_diff_adj:p_batter_adj,
                  Stuff_plus:num_pitches), 
                ~ percent_rank(.)*100, .names = "sp_perc_{.col}"),
         .after = p_batter_adj) %>%
  filter(n == 1) %>%
  bind_rows(pred_test) %>%
  dplyr::select(game_year:role_key, pred_tto1:pred_agg,
                hand_xwoba_diff_adj:CH_pct_sc, num_pitches,
                Stuff_plus, Location_plus) %>%
  filter(role_key == "SP") -> pred_sp

# save starting pitcher predictions to csv
# write_csv(pred_sp, "SP_tto_predictions.csv")

data %>%
  filter(Pitches >= 200 & role_key == "RP" & !is.na(velo_slope)) %>%
  left_join(swing_pits, by = c("game_year", "pitcher")) %>%
  mutate(across(starts_with("Stf_"), ~ . - 5.5)) -> rp_test

rp_test %>%
  mutate(pred_tto1 = predict(model1, rp_test),
         pred_tto2_diff = predict(model2, rp_test),
         pred_tto2 = pred_tto1 - pred_tto2_diff, 
         pred_agg = (pred_tto1 + pred_tto2)/2) %>%
  filter(game_year == 2023 & n == 1) %>%
  arrange(pred_agg) -> pred_rp

# calculate relief pitcher percentile IF they were starters
pred_rp %>%
  mutate(Stuff_plus = Stuff_plus - 5.5) %>%
  mutate(sp_perc_hand_xwoba_diff_adj = ecdf(pred_sp$hand_xwoba_diff_adj)(hand_xwoba_diff_adj) * 100,
         sp_perc_velo_slope_adj = ecdf(pred_sp$velo_slope_adj)(velo_slope_adj) * 100,
         sp_perc_p_batter_adj = ecdf(pred_sp$p_batter_adj)(p_batter_adj) * 100,
         sp_perc_Stuff_plus = ecdf(pred_sp$Stuff_plus)(Stuff_plus) * 100,
         sp_perc_Location_plus = ecdf(pred_sp$Location_plus)(Location_plus) * 100,
         sp_perc_num_pitches = ecdf(pred_sp$num_pitches)(num_pitches) * 100) %>%
  dplyr::select(
    game_year:role_key, pred_tto1:pred_agg, hand_xwoba_diff_adj, velo_slope_adj, p_batter_adj, 
    sp_perc_hand_xwoba_diff_adj:sp_perc_num_pitches,
    Loc_plus_CH:CH_pct_sc, Stuff_plus, Location_plus, num_pitches) -> pred_rp

# save relief pitcher predictions to csv
# write_csv(pred_rp, "RP_tto_predictions.csv")

# concenate predictions 
pred_sp %>%
  bind_rows(pred_rp) -> all_pred

# Extract partial dependence plot for model1
partial(model1, pred.var = c("velo_slope_adj"),
        train = train[, c(8:38)], grid.resolution = 100) %>%
  rename(value = velo_slope_adj) %>%
  mutate(var = "Velocity Slope") %>%
  bind_rows(partial(model1, pred.var = c("hand_xwoba_diff_adj"),
                    train = train[, c(8:38)], grid.resolution = 100) %>%
              rename(value = hand_xwoba_diff_adj) %>%
              mutate(var = "xwOBA Hand Diff.")) %>%
  bind_rows(partial(model1, pred.var = c("p_batter_adj"),
                    train = train[, c(8:38)], grid.resolution = 100) %>%
              rename(value = p_batter_adj) %>%
              mutate(var = "Pitches / Batter")) -> pdp_model1

# Extract partial dependence plot for model1
partial(model2, pred.var = c("velo_slope_adj"),
        train = train[, c(8:38)], grid.resolution = 100) %>%
  rename(value = velo_slope_adj) %>%
  mutate(var = "Velocity Slope") %>%
  bind_rows(partial(model2, pred.var = c("hand_xwoba_diff_adj"),
                    train = train[, c(8:38)], grid.resolution = 100) %>%
              rename(value = hand_xwoba_diff_adj) %>%
              mutate(var = "xwOBA Hand Diff.")) %>%
  bind_rows(partial(model2, pred.var = c("p_batter_adj"),
                    train = train[, c(8:38)], grid.resolution = 100) %>%
              rename(value = p_batter_adj) %>%
              mutate(var = "Pitches / Batter")) -> pdp_model2

pdp1 <- pdp_model1 %>%
  ggplot(aes(value, yhat)) +
  geom_line(lwd = 2) +
  facet_wrap(~var, scales = "free_x") +
  theme_minimal() +
  labs(title = "Predicted xwOBA 1st TTO",
       y = "Predicted Value", x = "")

pdp2 <- pdp_model2 %>%
  ggplot(aes(value, yhat)) +
  geom_line(lwd = 2) +
  facet_wrap(~var, scales = "free_x") +
  theme_minimal() +
  labs(title = "Predicted xwOBA Diff. (TTO 1 - TTO 2)",
       y = "Predicted Value", x = "")

grid.arrange(pdp1, pdp2, nrow = 2, 
             top = textGrob(expression("\n" * italic("Partial Dependence Plots (Figure 2)"))),
             bottom = textGrob("Independent Variable Values"))

#custom_colors <- c("#041e42", "#bf0d3e")

importance_matrix1 <- xgb.importance(model = model1$finalModel)

importance_matrix1 %>%
  mutate(Feature = case_when(str_starts(Feature, "Loc") ~ "Location+",
                             str_starts(Feature, "Stf") ~ "Stuff+",
                             str_ends(Feature, "pct_sc") ~ "Usage%",
                             Feature == "hand_xwoba_diff_adj" ~ "xwOBA Hand Diff.",
                             Feature == "velo_slope_adj" ~ "Velocity Slope",
                             Feature == "p_batter_adj" ~ "Pitches / Batter",
                             T ~ Feature)) %>%
  group_by(Feature) %>%
  summarise(Gain = sum(Gain)) -> importance_matrix1

p1 <- xgb.ggplot.importance(as.data.table(importance_matrix1), n_clusters = 1) +theme_minimal() +
  theme(legend.position = "none",
        axis.title.x = element_blank()) +
  scale_fill_manual(values = "black") +
  labs(subtitle = "1st TTO", title = "")

importance_matrix2 <- xgb.importance(model = model2$finalModel)

# xgb.ggplot.importance(importance_matrix2, n_clusters = 1) +
#   theme_minimal() +
#   theme(legend.position = "none")

importance_matrix2 %>%
  mutate(Feature = case_when(str_starts(Feature, "Loc") ~ "Location+",
                             str_starts(Feature, "Stf") ~ "Stuff+",
                             str_ends(Feature, "pct_sc") ~ "Usage%",
                             Feature == "hand_xwoba_diff_adj" ~ "xwOBA Hand Diff.",
                             Feature == "velo_slope_adj" ~ "Velocity Slope",
                             Feature == "p_batter_adj" ~ "Pitches / Batter",
                             T ~ Feature)) %>%
  group_by(Feature) %>%
  summarise(Gain = sum(Gain)) -> importance_matrix2

p2 <- xgb.ggplot.importance(as.data.table(importance_matrix2), n_clusters = 1) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank()) +
  scale_fill_manual(values = "black") +
  labs(subtitle = "TTO Diff. (2nd - 1st)", title = "")

grid.arrange(p1, p2, ncol = 2, 
             top = textGrob(expression("\n" * italic("Feature Importance (Figure 1)"))),
             bottom = textGrob("Importance"))

cor(all_pred$num_pitches, all_pred$pred_tto2_diff)

# flip percentiles for handedness xwoba and pitches/batter
all_pred %>%
  mutate(sp_perc_hand_xwoba_diff_adj = 100 - sp_perc_hand_xwoba_diff_adj,
         sp_perc_p_batter_adj = 100 - sp_perc_p_batter_adj) -> all_pred
         

# filter to suggested sp (from rp)
all_pred %>%
  filter(pitcher == 663158) %>%
  dplyr::select(sp_perc_hand_xwoba_diff_adj:sp_perc_num_pitches) %>%
  pivot_longer(cols = c(sp_perc_hand_xwoba_diff_adj:sp_perc_num_pitches), 
               names_to = "var", values_to = "Percentile") -> plot_df

img <- readJPEG("C:/Users/alexo/OneDrive - Syracuse University/Documents/Sabermetrics Club/Case Competitions/2024 Hackathon/robert_suarez.jpg",
                native = T)


plot_df$var <- c("xwOBA Hand Diff.", "Velocity Slope", "Pitches / Batter",
                 "Stuff+", "Location+", "Pitch Types")
plot_df %>%
  ggplot() +
  geom_hline(
    aes(yintercept = y),
    data.frame(y = c(0, 25, 50, 75, 100)),
    color = "#2F241D", alpha = 0.5
  ) +
  geom_col(
    aes(x = str_wrap(var, 5), y = Percentile),
    position = "dodge2",
    show.legend = T, fill = "#FFC425", alpha = .9
  ) +
  scale_y_continuous(
    limits = c(-75, 100),
    expand = c(0, 0),
    breaks = c(0, 25, 50, 75, 100)
  ) +
  theme_minimal() +
  annotate(
    x = 0.5, y = -5,
    geom = "text",label = "0 (Percentile)", size = 4, 
    color = "#2F241D", fontface = "bold") +
  annotate(
    x = 0.5, y = 20, 
    geom = "text",label = "25", size = 4, 
    color = "#2F241D", fontface = "bold") +
  annotate(
    x = 0.5, y = 45,
    geom = "text",label = "50", size = 4, 
    color = "#2F241D", fontface = "bold") +
  annotate(
    x = 0.5, y = 70,
    geom = "text",label = "75", size = 4, 
    color = "#2F241D", fontface = "bold") +
  annotate(
    x = 0.5, y = 95,
    geom = "text",label = "100", size = 4, 
    color = "#2F241D", fontface = "bold") +
  labs(title = expression("\n" * italic("Figure 3: Robert Suarez SP Percentiles")),
       subtitle = "Percentiles are amongst 2021-23 Starting Pitchers") +
  coord_polar(clip = "off") +
  theme(
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(color = "#2F241D", size = 12),
    legend.position = "bottom",
    plot.margin = margin(.1, .1, .1, .1)
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = .5, color = "#2F241D"),
    plot.subtitle = element_text(hjust = .5),
    panel.grid = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  inset_element(p = img,on_top = F, 
                left = 0.35, right = 0.65, 
                bottom = 0.36, top = 0.66)

# filter to suggested sp (from rp)
all_pred %>%
  filter(pitcher == 668933 & game_year == 2023) %>%
  dplyr::select(sp_perc_hand_xwoba_diff_adj:sp_perc_num_pitches) %>%
  pivot_longer(cols = c(sp_perc_hand_xwoba_diff_adj:sp_perc_num_pitches), 
               names_to = "var", values_to = "Percentile") -> plot_df


img <- readJPEG("C:/Users/alexo/OneDrive - Syracuse University/Documents/Sabermetrics Club/Case Competitions/2024 Hackathon/graham_ashcraft.jpg",
                native = T)


plot_df$var <- c("xwOBA Hand Diff.", "Velocity Slope", "Pitches / Batter",
                 "Stuff+", "Location+", "Pitch Types")
plot_df %>%
  ggplot() +
  geom_hline(
    aes(yintercept = y),
    data.frame(y = c(0, 25, 50, 75, 100)),
    color = "black", alpha = 0.5
  ) +
  geom_col(
    aes(x = str_wrap(var, 5), y = Percentile),
    position = "dodge2",
    show.legend = T, fill = "#C6011F", alpha = .9
  ) +
  scale_y_continuous(
    limits = c(-75, 100),
    expand = c(0, 0),
    breaks = c(0, 25, 50, 75, 100)
  ) +
  theme_minimal() +
  annotate(
    x = 0.5, y = -5,
    geom = "text",label = "0 (Percentile)", size = 4, 
    color = "black", fontface = "bold") +
  annotate(
    x = 0.5, y = 20, 
    geom = "text",label = "25", size = 4, 
    color = "black", fontface = "bold") +
  annotate(
    x = 0.5, y = 45,
    geom = "text",label = "50", size = 4, 
    color = "black", fontface = "bold") +
  annotate(
    x = 0.5, y = 70,
    geom = "text",label = "75", size = 4, 
    color = "black", fontface = "bold") +
  annotate(
    x = 0.5, y = 95,
    geom = "text",label = "100", size = 4, 
    color = "black", fontface = "bold") +
  labs(title = expression("\n" * italic("Figure 4: Graham Ashcraft SP Percentiles")),
       subtitle = "Percentiles are amongst 2021-23 Starting Pitchers") +
  coord_polar(clip = "off") +
  theme(
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(color = "black", size = 12),
    legend.position = "bottom",
    plot.margin = margin(.1, .1, .1, .1)
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = .5, color = "black"),
    plot.subtitle = element_text(hjust = .5),
    panel.grid = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  inset_element(p = img,on_top = F, 
                left = 0.35, right = 0.65, 
                bottom = 0.36, top = 0.66)


Sims <- pitches %>%
  filter(pitcher == "608371" & game_date == "2023-04-30") %>%
  select(player_name, game_date, pitch_type, release_speed)

theme_set(theme_bw())
ggplot(all_pred, aes(x = hand_xwoba_diff_adj)) +
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white", bins = 25) +
  geom_density(adjust = 2, lwd = 1, colour = 4,
               fill = 4, alpha = 0.25) +
  labs(x = "Difference in Handedness Splits (Best - Worse)", y = "Density", title = "Predicted Handedness Splits Distribution for All Pitchers 2023") -> hand
hand
ggsave("density_plot.png", hand, width = 6, height = 4, dpi = 300)
