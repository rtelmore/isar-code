## ----pkgs-ch-4, include = F---------------------------------------------------------
library(tidyverse)
library(rvest)
library(ggplot2)
library(broom)
library(Lahman)
library(ggcorrplot)
library(baseballr)
library(lubridate)
library(mgcv)
library(splines)


## ----knitr-ggplot2-opts-ch-4, include = F-------------------------------------------
knitr::opts_chunk$set(fig.align="center",
                      warning = FALSE,
                      message = FALSE)
ggplot2::theme_set(ggplot2::theme_bw())


## -----------------------------------------------------------------------------------
team_df <- dplyr::tibble(Teams) %>%
  dplyr::filter(., yearID >= 1970)


## -----------------------------------------------------------------------------------
rc_df <- team_df %>%
  mutate(., HBP = if_else(is.na(HBP), 0, as.numeric(HBP)),
        X1B = H - X2B - X3B - HR, #singles
        TB = X1B + 2*X2B + 3*X3B + 4*HR, #total bases
        RC = (H + BB + HBP)*TB/(AB + BB + HBP))


## -----------------------------------------------------------------------------------
p <- ggplot(data = rc_df,
            aes(x = RC, y = R))
p + geom_point(col = "#6e0000", alpha = .15) +
  geom_smooth(method = "lm", col = "black") + ## equiv to add trendline 
  labs(x = "runs created", y = "actual runs") +
  theme_bw()


## -----------------------------------------------------------------------------------
rc_df %>%
  dplyr::filter(., yearID == "2000")


## -----------------------------------------------------------------------------------
rc_df %>%
  dplyr::filter(., yearID == "2000", teamID == "ANA") %>%
  dplyr::mutate(., percent_change = (RC - R)/R) %>%
  dplyr::select(., R, RC, percent_change)


## -----------------------------------------------------------------------------------
cor(select(rc_df, R, RC))


## -----------------------------------------------------------------------------------
rc_r_lm <- lm(R ~ RC, data = rc_df) 
summary(rc_r_lm)


## -----------------------------------------------------------------------------------
cor_mat <- cor(select(rc_df, R, RC, AB, H, HR, SO, CS))
cor_mat
ggcorrplot(cor_mat, method = "circle", 
           colors = c( "#2c7bb6", "#ffffbf", "#d7191c")) 


## -----------------------------------------------------------------------------------
Teams %>%
  dplyr::filter(., yearID >= 2001, yearID < 2007) %>%
  dplyr::group_by(., yearID) %>%
  dplyr::summarize(., out = sum(IPouts),
                   n = sum(G),
                   avg_outs = out/n)


## ---- include = FALSE---------------------------------------------------------------
df_batting <- tibble(Batting) %>%
  filter(yearID >= 2000, yearID <= 2006, AB >= 100)


## -----------------------------------------------------------------------------------
p <- ggplot(data = df_batting,
            aes(x = HR, y = R))
p + geom_point(col = "#6e0000") +
  geom_smooth(method = "lm", col = "black") +
  labs(x = "home runs", y = "runs")


## -----------------------------------------------------------------------------------
df_bat_simple <- df_batting %>%
  select(.,R, HR)
lm_batting_1 <- lm(R ~ HR,
                   data = df_bat_simple)
summary(lm_batting_1)


## -----------------------------------------------------------------------------------
confint(lm_batting_1, level = .99)


## -----------------------------------------------------------------------------------
df_batting_new <- df_bat_simple %>%
  mutate(pred_R = predict(lm_batting_1))
head(df_batting_new)


## -----------------------------------------------------------------------------------
new_data <- data.frame(R = c(NA, NA),
                       HR = c(30, 15))
predict(lm_batting_1, newdata = new_data, interval = "confidence")


## -----------------------------------------------------------------------------------
df_teams <- tibble(Teams) %>%
  dplyr::filter(., yearID >= 2010) %>%
  dplyr::mutate(., X1B = H - X2B - X3B - HR,
                BB_HBP = BB + HBP) %>%
  dplyr::select(., yearID, R, X1B, X2B, X3B, HR, BB_HBP) 
df_teams


## -----------------------------------------------------------------------------------
lm_runs <- lm(R ~ . - yearID, data = df_teams) ## Multiple regression model
lm_runs <- lm(R ~ X1B + X2B + X3B + HR + BB_HBP, data = df_teams)
summary(lm_runs)


## -----------------------------------------------------------------------------------
broom::tidy(lm_runs)


## -----------------------------------------------------------------------------------
head(broom::augment(lm_runs))


## -----------------------------------------------------------------------------------
plot(lm_runs) ## Model checking diagnostics


## -----------------------------------------------------------------------------------
Lahman::playerInfo(nameLast = "Trout")


## -----------------------------------------------------------------------------------
trout <- tibble(Batting) %>%
  dplyr::filter(., playerID == "troutmi01",
                yearID == 2018) %>%
  dplyr::mutate(., 
                X1B = H - X2B - X3B - HR,
                BB_HBP = BB + IBB + HBP,
                Outs = .982*AB - H + GIDP + SF + SH + CS) %>%
  dplyr::select(., yearID, R, X1B, X2B, X3B, HR, BB_HBP, Outs)
trout


## -----------------------------------------------------------------------------------
avg_team <- df_teams %>%
  dplyr::filter(., yearID == 2018) %>%
  dplyr::summarize_all(., .funs = mean) ## take each column in df_teams and compute mean
avg_team


## -----------------------------------------------------------------------------------
predict(lm_runs, newdata = avg_team) ## predict(lm_object, newdata)


## -----------------------------------------------------------------------------------
avg_outs <- Teams %>%
  dplyr::filter(., yearID == 2018) %>% 
  dplyr::summarize(., outs = mean(IPouts))
avg_outs


## -----------------------------------------------------------------------------------
trout_scale <- (avg_outs$outs - trout$Outs)/avg_outs$outs
trout_scale


## -----------------------------------------------------------------------------------
w_trout <- avg_team*trout_scale + 
  select(trout, yearID, R, X1B, X2B, X3B, HR, BB_HBP)
w_trout


## -----------------------------------------------------------------------------------
avg_team


## -----------------------------------------------------------------------------------
predict(lm_runs, newdata = w_trout) - predict(lm_runs, newdata = avg_team)


## -----------------------------------------------------------------------------------
Lahman::playerInfo(nameLast = "Desmond")
ian <- tibble(Batting) %>%
  dplyr::filter(., playerID == "desmoia01",
                yearID == 2018) %>%
  dplyr::mutate(., X1B = H - X2B - X3B - HR,
                BB_HBP = BB + IBB + HBP,
                Outs = .982*AB - H + GIDP + SF + SH + CS) %>%
  dplyr::select(., yearID, R, X1B, X2B, X3B, HR, BB_HBP, Outs)
ian_scale <- (avg_outs$outs - ian$Outs)/avg_outs$outs
w_ian <- avg_team*ian_scale + 
  select(ian, yearID, R, X1B, X2B, X3B, HR, BB_HBP)
predict(lm_runs, newdata = w_ian) - predict(lm_runs, newdata = avg_team)


## -----------------------------------------------------------------------------------
df_team <- Teams %>%
  dplyr::filter(., yearID >= 1970) %>%
  dplyr::mutate(., FIP = (13*HRA + 3*BBA - 2*SOA)/IPouts + 3.10)


## -----------------------------------------------------------------------------------
p <- ggplot(data = df_team,
            aes(x = FIP, y = ERA))
p + geom_point(col = "#6e0000") +
  theme_bw()


## ---- eval = F----------------------------------------------------------------------
## remotes::install_github("BillPetti/baseballr")


## ---- cache = T, warning = F, message = F-------------------------------------------
charlie_id <- baseballr::playerid_lookup(last_name = "Blackmon", first_name = "Charlie")


## ---- echo = T, cache = TRUE, warning = F, message = F------------------------------
df_charlie_bat <- scrape_statcast_savant(start_date = ymd("2019-03-28"), 
                                         end_date = ymd("2019-09-29"), 
                                         playerid = 453568)


## ---- include = F, cache = TRUE, warning = F, message = F---------------------------
justin_id <- playerid_lookup(last_name = "Verlander") # 434378


## ---- echo = T, cache = TRUE, warning = F, message = F------------------------------
df_justin_pit <- scrape_statcast_savant_pitcher(start_date = ymd("2019-03-28"), 
                                                end_date = ymd("2019-09-29"), 
                                                pitcherid = 434378)
## saveRDS(df_justin_pit, "../../data/verlander_dat.rds")
df_justin_pit_2 <- scrape_statcast_savant(start_date = ymd("2019-03-28"), 
                                          end_date = ymd("2019-09-29"), 
                                          playerid = 434378,
                                          player_type = "pitcher")


## -----------------------------------------------------------------------------------
p <- ggplot(df_charlie_bat, 
            aes(x = pitch_type, y = ..prop.., group = 1))
p + geom_bar() +
  labs(title = "Types of Pitches Thrown Against Charlie Blackmon in 2019",
       x = "Pitch Type",
       y = "Proportion of Pitches") + 
  theme_bw()


## -----------------------------------------------------------------------------------
df_charlie_grouped <- df_charlie_bat %>%
  dplyr::group_by(., pitch_type) %>%
  dplyr::summarize(., perc = n()/nrow(df_charlie_bat)) %>%
  dplyr::arrange(., desc(perc))
df_charlie_grouped$pitch_type[1:6]

p <- ggplot(df_charlie_grouped, 
            aes(x = pitch_type, y = perc))
p + geom_bar(stat = "identity", fill = "#6e0000") +
  labs(title = "Types of Pitches Thrown Against Charlie Blackmon in 2019",
       x = "Pitch Type",
       y = "Proportion of Pitches") + 
  theme_bw()


## -----------------------------------------------------------------------------------
df_charlie_grouped <- df_charlie_grouped %>%
  dplyr::arrange(., desc(-perc)) %>%
  dplyr::mutate(pitch_type = factor(pitch_type, pitch_type))

p <- ggplot(df_charlie_grouped, 
            aes(x = pitch_type, y = perc))
p + geom_bar(stat = "identity", fill = "#6e0000") +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  labs(x = "pitch type",
       y = "percentage") +
  theme_bw()


## -----------------------------------------------------------------------------------
p <- df_charlie_bat %>%
  filter(., pitch_type %in% c("CH", "CU", "FC", "FF", "FT", "SI", "SL"),
         type == "X") %>%
  ggplot(., aes(x = pitch_type, y = launch_speed, fill = pitch_type))

p + geom_violin(alpha = 0.8, color = "black") + 
  scale_fill_brewer("type", palette = "Dark2") +
  labs(title = "Charlie Blackmon's Exit Velocity by Pitch Type in 2018",
       x = "pitch type",
       y = "exit velocity (mph)") +
  coord_flip() +
  theme_bw()


## -----------------------------------------------------------------------------------
p <-  df_charlie_bat %>%
  filter(pitch_type %in% c("CH", "CU", "FC", "FF", "FT", "SI", "SL"),
         type == "X") %>%
  ggplot(aes(x = launch_speed, fill = pitch_type))

p + geom_density(alpha = 0.5, color = "black") + 
  scale_fill_brewer("type", palette = "Dark2") +
  labs(title = "Charlie Blackmon's Exit Velocity by Pitch Type in 2018",
       x = "exit velocity") +
  theme_bw()


## -----------------------------------------------------------------------------------
p <-  df_charlie_bat %>%
  filter(pitch_type %in% c("CH", "CU", "FC", "FF", "FT", "SI", "SL"),
         type == "X") %>%
  ggplot(aes(x = launch_speed, fill = pitch_type))

p + geom_density(color = "black") + 
  facet_wrap(. ~ pitch_type, ncol = 3) + 
  scale_fill_brewer("type", palette = "Dark2") +
  labs(title = "Charlie Blackmon's Exit Velocity by Pitch Type in 2018",
       x = "exit velocity (mph)") +
  guides(fill = FALSE) +
  theme_bw()


## -----------------------------------------------------------------------------------
p <- ggplot(df_justin_pit, 
            aes(x = pitch_type, y = ..prop.., group = 1))
p + geom_bar() +
  labs(title = "Types of Pitches Thrown by Justin Verlander in 2019",
       x = "pitch type",
       y = "proportion of pitches") + 
  theme_bw()


## -----------------------------------------------------------------------------------
p <-  df_justin_pit %>%
  filter(pitch_type %in% c("CH", "CU", "FF", "SL"),
         type == "X") %>%
  ggplot(aes(x = pitch_type, y = release_spin_rate, fill = pitch_type))

p + geom_violin(alpha = 0.8, color = "black") + 
  scale_fill_brewer("type", palette = "Dark2") +
  labs(title = "Justin Verlander's Spin Rate by Pitch Type in 2019",
       x = "pitch type",
       y = "spin rate") +
  coord_flip() +
  theme_bw()


## -----------------------------------------------------------------------------------
p <-  df_justin_pit %>%
  filter(pitch_type %in% c("CH", "CU", "FF", "SL"),
         type == "X") %>%
  group_by(month = month(game_date), pitch_type) %>%
  summarize(avg_sp = mean(release_speed)) %>%
  ungroup() %>%
  ggplot(aes(x = month, y = avg_sp, col = pitch_type))

p + geom_line() +
  geom_point() +
  scale_color_brewer("type", palette = "Dark2") +
  scale_x_continuous(labels = c("Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep"),
                     breaks = 3:9) +
  labs(title = "Justin Verlander's Average Release Speed by Pitch Type",
       subtitle = "(2019 Season)",
       x = "month",
       y = "release speed (mph)") +
  theme_bw()


## ---- include = T-------------------------------------------------------------------
top_zone <- 3.5
bot_zone <- 1.6
left_zone <- -0.75
right_zone <- 0.75
strike_zone_df <- data.frame(
  x = c(left_zone, left_zone, right_zone, right_zone, left_zone),
  y = c(bot_zone, top_zone, top_zone, bot_zone, bot_zone)
)


## ---- message = F-------------------------------------------------------------------
p <- ggplot(df_charlie_bat,
            aes(x = plate_x,
                y = plate_z))
p + geom_point(alpha = 0.2) +
  geom_path(data = strike_zone_df, aes(x, y), lwd = 1.5, color = "red") +
  labs(x = "horizontal location (ft)",
       y = "vertical location (ft)") +
  xlim(-3.5, 3.5) +
  coord_fixed() +
  theme_bw()


## ---- include = F-------------------------------------------------------------------
distinct(df_charlie_bat %>% select(description))


## ---- echo = T----------------------------------------------------------------------
df_charlie_bat <- df_charlie_bat %>%
  mutate(swing = if_else(description %in% c("ball", "blocked_ball",
                                            "automatic_ball", "called_strike",
                                            "hit_by_pitch"), 0 , 1))
p <- ggplot(df_charlie_bat,
            aes(x = plate_x, y = plate_z, col = as.factor(swing)))
p + geom_point(alpha = 0.15) +
  scale_color_brewer("swing", palette = "Set1") +
  geom_path(data = strike_zone_df,aes(x, y), lwd = 1.5, color = "red") +
  labs(x = "horizontal location (ft)",
       y = "vertical location (ft)") +
  xlim(-3.5, 3.5) +
  coord_fixed() +
  theme_bw()


## -----------------------------------------------------------------------------------
p <- df_charlie_bat %>%
  filter(swing == 0, type != "X") %>%
  ggplot(., aes(x = plate_x, y = plate_z, col = type))

p + geom_point(alpha = 0.15) +
  scale_color_brewer("call", palette = "Set1") +
  geom_path(data = strike_zone_df,aes(x, y), lwd = 1, color = "black") +
  labs(x = "horizontal location (ft)",
       y = "vertical location (ft)") +
  xlim(-3.5, 3.5) +
  coord_fixed()


## ---- include = F, cache = T, warning = F, message = F------------------------------
nolan_id <- playerid_lookup(last_name = "Arenado", first_name = "Nolan") %>%
  dplyr::pull(., mlbam_id)
df_nolan_bat <- scrape_statcast_savant(start_date = ymd("2019-03-28"), 
                                       end_date = ymd("2019-09-29"), 
                                       playerid = nolan_id)


## -----------------------------------------------------------------------------------
head(df_nolan_bat %>%
  dplyr::filter(., type == 'X') %>%
  dplyr::select(., type, events, launch_angle, launch_speed))


## -----------------------------------------------------------------------------------
p <- ggplot(data = df_nolan_bat %>%
              filter(type == 'X') %>%
              select(type, events, launch_angle, pitch_type),
            aes(x = launch_angle))
p + geom_density() 


## -----------------------------------------------------------------------------------
df_nolan_bat %>%
  dplyr::filter(., type == "X") %>%
  dplyr::distinct(., events)


## -----------------------------------------------------------------------------------
df_nolan_hits <- df_nolan_bat %>%
  filter(., type == "X", !is.na(launch_angle)) %>%
  mutate(., hit = if_else(events %in% c("single", "double", "triple", "home_run"),
                          1, 0))


## -----------------------------------------------------------------------------------
p <- ggplot(data = df_nolan_hits,
            aes(x = launch_angle, y = hit))
p + geom_point(col = "#6e0000", alpha = .15) + 
  labs(x = "launch angle") +
  geom_smooth()


## -----------------------------------------------------------------------------------
p <- ggplot(data = df_nolan_hits,
            aes(x = launch_speed, y = hit))
p + geom_point(col = "#6e0000", alpha = .15) + 
  labs(x = "launch speed") +
  geom_smooth()


## -----------------------------------------------------------------------------------
glm_nolan <- glm(hit ~ launch_angle, data = df_nolan_hits, 
                   family = "binomial") # Estimate the Prob of Hit = 1 given LA
summary(glm_nolan)

## -----------------------------------------------------------------------------------
df_nolan_hits <- df_nolan_hits %>% 
  dplyr::mutate(., preds = predict(glm_nolan, type = "response"))


## -----------------------------------------------------------------------------------
p <- ggplot(data = df_nolan_hits,
            aes(x = launch_angle, y = hit))
p + geom_point(col = "#6e0000", alpha = .15) + 
  geom_smooth(method = "glm", formula = y ~ x, 
              method.args = list(family = "binomial")) +
  labs(y = "P(hit)")


## -----------------------------------------------------------------------------------
glm_nolan_2 <- glm(hit ~ poly(launch_angle, 2), data = df_nolan_hits, 
                   family = "binomial") ## Quadratic model
summary(glm_nolan_2)


## -----------------------------------------------------------------------------------
p <- ggplot(data = df_nolan_hits,
            aes(x = launch_angle, y = hit))
p + geom_point(col = "#6e0000", alpha = .15) + 
  geom_smooth(method = "glm", formula = y ~ poly(x, 2), 
              method.args = list(family = "binomial")) +
  labs(y = "P(Hit = 1)")


## -----------------------------------------------------------------------------------
glm_nolan_pitch <- glm(hit ~ pitch_type,
                       data = df_nolan_hits, 
                       family = "binomial")
summary(glm_nolan_pitch)


## ---- include = F-------------------------------------------------------------------
df_nolan_bat <-df_nolan_bat %>%
  dplyr::mutate(., swing = if_else(description %in%
                                     c("ball", "blocked_ball",
                                       "automatic_ball", "called_strike",
                                       "hit_by_pitch"), 0 , 1))


## -----------------------------------------------------------------------------------
p <- df_nolan_bat %>%
  filter(., description == "hit_by_pitch") %>%
  ggplot(., aes(x = plate_x, y = plate_z))

p + geom_point() +
  geom_path(data = strike_zone_df, aes(x, y), lwd = 1, color = "black") +
  labs(x = "horizontal location (ft)",
       y = "vertical location (ft)") +
  xlim(-3.5, 3.5) +
  ylim(0, 5) +
  coord_fixed() +
  theme_bw()


## -----------------------------------------------------------------------------------
swing_gam <- mgcv::gam(swing ~ s(plate_x) + s(plate_z),
                 family = binomial, #logistic regression GAM
                 data = df_nolan_bat)

# Find predicted probabilities over a 50 x 50 grid
swing_predict_data <- tibble(expand.grid(plate_x = seq(-1.5, 1.5, length = 50), 
                                         plate_z = seq(0.5, 5, length = 50)))

# Get the predicted values from the model and convert to probability values:
swing_preds <- predict(swing_gam, newdata = swing_predict_data, 
                       type = "response")
swing_predict_data <- swing_predict_data %>%
  mutate(., swing_prob = swing_preds)


## -----------------------------------------------------------------------------------
p <- ggplot(data = swing_predict_data,
            aes(x = plate_x, y = plate_z))

p + geom_tile(aes(fill = swing_prob)) +
  scale_fill_distiller("", palette = "Spectral",
                       direction = -1,
                       limit = c(0,1)) +
  geom_path(data = strike_zone_df, aes(x, y),
            linetype = 2, color = "navy") +
  coord_fixed() +
  labs(x = "horizontal location (ft)",
       y = "vertical location (ft)",
       title = "When Does Nolan Arenado Swing?")


## -----------------------------------------------------------------------------------
team_df <- tibble(Teams) %>%
  dplyr::filter(., yearID >= 1970)
rc_df <- team_df %>%
  dplyr::mutate(., HBP = if_else(is.na(HBP), 0, as.numeric(HBP)),
                X1B = H - X2B - X3B - HR,
                TB = X1B + 2*X2B + 3*X3B + 4*HR,
                RC = (H + BB + HBP)*TB/(AB + BB + HBP))


## -----------------------------------------------------------------------------------
rc_df <- rc_df %>%
  mutate(., OBP = (H + BB + HBP)/(AB + BB + HBP),
         SLG = (X1B + 2*X2B + 3*X3B + 4*HR)/AB,
         OPS = OBP + SLG)


## -----------------------------------------------------------------------------------
cor_mat <- cor(select(rc_df, R, RC, AB, H, HR, OBP, SLG, OPS))
ggcorrplot(cor_mat, method = "circle", 
           colors = c( "#2c7bb6", "#ffffbf", "#d7191c")) 


## -----------------------------------------------------------------------------------
rc_df <- rc_df %>%
  mutate(A = H + BB - HR,
         B = (1.4*TB - .6*H - 3*HR + .1*BB)*1.02,
         C = AB - H,
         BsR = (A*B)/(B + C) + HR)


## -----------------------------------------------------------------------------------
cor_mat <- cor(select(rc_df, R, RC, AB, H, HR, OBP, SLG, OPS, BsR))
ggcorrplot(cor_mat, method = "circle", 
           colors = c( "#2c7bb6", "#ffffbf", "#d7191c")) 
cor_mat


## -----------------------------------------------------------------------------------
rc_r_lm <- lm(R ~ RC, data = rc_df)
summary(rc_r_lm)


## -----------------------------------------------------------------------------------
bsr_r_lm <- lm(R ~ BsR, data = rc_df)
summary(bsr_r_lm)


## -----------------------------------------------------------------------------------
df_teams <- Teams %>%
  filter(yearID >= 1970) %>%
  group_by(lgID, yearID) %>%
  summarize(avg_runs = mean(R))
head(df_teams)


## -----------------------------------------------------------------------------------
p <- ggplot(data = df_teams,
            aes(x = yearID, y = avg_runs, col = lgID))
p + geom_point() +
  geom_line() +
  scale_color_brewer("league", palette = "Set1") +
  labs(x = "year", y = "average runs per team")


## -----------------------------------------------------------------------------------
df_teams <- Teams %>%
  filter(yearID >= 2010) %>%
  mutate(X1B = H - X2B - X3B - HR,
         BB_HBP = BB + HBP) %>%
  select(R, X1B, X2B, X3B, HR, BB_HBP) ## This isn't necessary, but it's good practice
lm_runs <- lm(R ~ ., data = df_teams)
summary(lm_runs)


## -----------------------------------------------------------------------------------
#betts_id
# playerInfo(nameLast = "Betts")
betts <- Batting %>%
  filter(., playerID == "bettsmo01",
         yearID == 2018) %>%
  mutate(., X1B = H - X2B - X3B - HR,
         BB_HBP = BB + IBB + HBP,
         Outs = .982*AB - H + GIDP + SF + SH + CS)
select(betts, R, X1B, X2B, X3B, HR, BB_HBP)

#yelich
# playerInfo(nameLast = "Yelich")

yelich <- Batting %>%
  filter(playerID == "yelicch01",
         yearID == 2018) %>%
  mutate(X1B = H - X2B - X3B - HR,
         BB_HBP = BB + IBB + HBP,
         Outs = .982*AB - H + GIDP + SF + SH + CS)
select(yelich, R, X1B, X2B, X3B, HR, BB_HBP)


## -----------------------------------------------------------------------------------
avg_player <- df_teams %>%
  summarize_all(.funs = mean)
predict(lm_runs, newdata = avg_player)


## -----------------------------------------------------------------------------------
avg_outs <- Teams %>%
  filter(yearID >= 2010) %>% 
  summarize(outs = mean(IPouts))
avg_outs


## -----------------------------------------------------------------------------------
betts_scale <- (avg_outs$outs - betts$Outs)/avg_outs$outs
w_betts <- avg_player*betts_scale + select(betts, R, X1B, X2B, X3B, HR, BB_HBP)
yelich_scale <- (avg_outs$outs - yelich$Outs)/avg_outs$outs
w_yelich <- avg_player*yelich_scale + select(yelich, R, X1B, X2B, X3B, HR, BB_HBP)
predict(lm_runs, newdata = w_betts) - predict(lm_runs, newdata = avg_player)
predict(lm_runs, newdata = w_yelich) - predict(lm_runs, newdata = avg_player)


## -----------------------------------------------------------------------------------
df_team_ra <- Teams %>%
  filter(yearID >= 1970)
lm_ra <- lm(RA ~ HRA + BBA + SOA, 
            data = df_team_ra)
tidy(lm_ra)


## -----------------------------------------------------------------------------------
df_team_ra <- df_team_ra %>%
  mutate(FIP = (13*HRA + 3*BBA - 2*SOA)/IPouts + 3.10,
         FIP_new = predict(lm_ra))
cor(select(df_team_ra, RA, FIP, FIP_new, HRA, BBA, SOA))


## -----------------------------------------------------------------------------------
ggcorrplot(cor(select(df_team_ra, RA, FIP, FIP_new, HRA, BBA, SOA)))


## -----------------------------------------------------------------------------------
df_batting <- Batting %>%
  filter(yearID >= 2000) %>%
  mutate(X1B = H - X2B - X3B - HR,
         BB_HBP = BB + HBP)


## -----------------------------------------------------------------------------------
lm_a <- lm(R ~ X1B + X2B + X3B + HR + BB_HBP,
           data = df_batting)
tidy(lm_a)


## -----------------------------------------------------------------------------------
lm_b <- update(lm_a, . ~ . - BB_HBP, data = df_batting)
tidy(lm_b)


## -----------------------------------------------------------------------------------
lm_c <- update(lm_a, . ~ . + SB + CS, data = df_batting)
tidy(lm_c)


## -----------------------------------------------------------------------------------
arenado_pred <- filter(df_batting, 
                       playerID == playerInfo(nameLast = "Arenado")$playerID)
arenado_pred <- arenado_pred %>%
  mutate(preds = predict(lm_c, newdata = arenado_pred)) %>%
  select(playerID, yearID, R, preds)
arenado_pred

## Spin Rates in 2021
tyler_id <- baseballr::playerid_lookup(last_name = "Anderson", 
                                       first_name = "Tyler")

df_tyler <- scrape_statcast_savant_pitcher(start_date = ymd("2021-03-28"), 
                                           end_date = ymd("2021-07-20"), 
                                           pitcherid = 542881)

p <-  df_tyler %>%
  ggplot(aes(x = pitch_type, y = release_spin_rate, fill = pitch_type))

p + geom_violin(alpha = 0.8, color = "black") + 
  scale_fill_brewer("type", palette = "Dark2") +
  labs(title = "Tyler Anderson's Spin Rate in 2021",
       x = "pitch type",
       y = "spin rate") +
  coord_flip() +
  theme_bw()

df_tyler %>% 
  group_by(pitch_type) %>% 
  summarize(n = n())
## Look at before/after change
p <-  df_tyler %>%
  dplyr::filter(pitch_type %in% c("CH", "FC", "FF", "SI")) %>% 
  dplyr::mutate(before = ifelse(lubridate::ymd(game_date) <= 
                                  lubridate::ymd("2021-06-14"),
                                TRUE, FALSE)) %>% 
  ggplot(aes(x = release_spin_rate, fill = before))

p + geom_density(alpha = 0.8, color = "black") + 
  scale_fill_brewer("sticky?", palette = "Set1") +
  scale_x_continuous(breaks = seq(1500, 3000, by = 250)) +
  facet_wrap(~ pitch_type, ncol = 2) +
  labs(title = "Tyler Anderson's Spin Rate in 2021",
       x = "spin rate") +
  theme_bw()

ggsave("fig/chap-05/ta-spin-rate.png")

