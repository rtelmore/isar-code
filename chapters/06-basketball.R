## ----knitr-options, include = FALSE-------------------------------------------------------------------
knitr::opts_chunk$set(fig.align="center",
                      warning = FALSE,
                      message = FALSE,
                      comment = NA)


## -----------------------------------------------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(lubridate)
library(gganimate)
library(nbastatR)
library(jpeg)
library(grid)
library(hexbin)
library(gifski)
library(png)
library(mgcv)


## ----data---------------------------------------------------------------------------------------------
games <- readRDS("../data/nba-games-2021.rds")
ff <- readRDS("../data/four-factors-team-2021.rds")
adv <- readRDS("../data/advanced-team-2021.rds")


## -----------------------------------------------------------------------------------------------------
df_ff_games <- merge(games, ff) %>% 
  dplyr::mutate(., ftr = ftaTeam/fgaTeam)
p <- ggplot(data = df_ff_games,
            aes(x = ftr, y = rateFTA))
p + geom_line() +
  theme_bw()


## -----------------------------------------------------------------------------------------------------
game_summary <- games %>% 
  dplyr::select(., dateGame, idGame, slugTeam, slugMatchup, locationGame,
                outcomeGame, plusminusTeam) 


## -----------------------------------------------------------------------------------------------------
tmp <- inner_join(game_summary, adv) %>%
  group_by(slugTeam, locationGame) %>% 
  summarize(avg_pace = mean(pace)) %>% 
  arrange(desc(avg_pace))


## -----------------------------------------------------------------------------------------------------
df_ff <- inner_join(adv, ff) %>%
  dplyr::inner_join(game_summary) %>% 
  dplyr::select(dateGame, idGame, slugTeam, pctOREB, pctTOVTeam, pctEFG,
                rateFTA, ortg, drtg, outcomeGame, locationGame,
                plusminusTeam) %>%
  dplyr::mutate(is_win = ifelse(outcomeGame == "W", T, F))
  


## -----------------------------------------------------------------------------------------------------
cor(select(df_ff, pctEFG, pctOREB, pctTOVTeam, rateFTA))


## -----------------------------------------------------------------------------------------------------
lal_dal_adv <- merge(game_summary, adv) %>% 
  dplyr::filter(dateGame == ymd("2021-04-24"),
                slugTeam %in% c("LAL", "DAL"))
lal_dal_adv$slugTeam
pnorm((lal_dal_adv$ortg - mean(adv$ortg))/sd(adv$ortg))
pnorm((lal_dal_adv$drtg - mean(adv$drtg))/sd(adv$drtg))


## -----------------------------------------------------------------------------------------------------
ff_lm <- lm(plusminusTeam ~ pctOREB + pctTOVTeam + pctEFG + rateFTA + ortg,
            data = df_ff)


## -----------------------------------------------------------------------------------------------------
ff_lm <- lm(plusminusTeam ~ pctOREB + pctTOVTeam + pctEFG + rateFTA + ortg,
            data = df_ff)
summary(ff_lm)


## ---- eval = T----------------------------------------------------------------------------------------
df <- readRDS("../data/nuggets-2015-21.rds")


## -----------------------------------------------------------------------------------------------------
p <- ggplot(data = df,
            aes(x = slugSeason, fill = zoneBasic))
p + geom_bar() +
  scale_fill_brewer("", palette = "Dark2")


## -----------------------------------------------------------------------------------------------------
group_by(df, slugSeason, namePlayer) %>%
  summarize(n = n()) %>%
  filter(n == max(n))


## -----------------------------------------------------------------------------------------------------
court <- rasterGrob(readJPEG("../fig/nba_court.jpg"), 
                    width = unit(1, "npc"),
                    height = unit(1, "npc"))

p <- ggplot(data = df %>% filter(slugSeason == "2020-21"),
            aes(x = locationX, y = locationY, col = zoneBasic))
p + annotation_custom(court, -250, 250, -50, 420) +
  geom_point(alpha = .15) +
  scale_color_brewer("Zone", palette = "Set1") +
  coord_fixed() +
  xlim(250, -250) +
  ylim(-50, 420)


## -----------------------------------------------------------------------------------------------------
p <- ggplot(data = df %>% filter(slugSeason == "2020-21"),
            aes(x = locationX, y = locationY, col = zoneBasic,
                shape = typeEvent))
p + annotation_custom(court, -250, 250, -50, 420) +
  geom_point(alpha = .15) +
  scale_color_brewer("Zone", palette = "Set1") +
  coord_fixed() +
  xlim(250, -250) +
  ylim(-50, 420)


## -----------------------------------------------------------------------------------------------------
p <- ggplot(data = df %>% filter(yearSeason %in% c(2020, 2021),
                                 namePlayer == "Michael Porter Jr."),
            aes(x = locationX, y = locationY, col = zoneBasic,
                shape = typeEvent))
p + annotation_custom(court, -250, 250, -50, 420) +
  geom_point(alpha = .15) +
  facet_grid(yearSeason ~ typeEvent) +
  scale_color_brewer("Zone", palette = "Set1") +
  coord_fixed() +
  xlim(250, -250) +
  ylim(-50, 420)



## -----------------------------------------------------------------------------------------------------
ggplot(data = df,
            aes(x = locationX, y = locationY, col = zoneBasic)) +
  annotation_custom(court, -250, 250, -50, 420) +
  geom_point(alpha = .15) +
  scale_color_brewer("Zone", palette = "Set1") +
  coord_fixed() +
  xlim(250, -250) +
  ylim(-50, 420) +
  transition_manual(slugSeason) +
  labs(title = 'Season: {current_frame}')


## -----------------------------------------------------------------------------------------------------
ggplot(data = df %>% filter(zoneBasic != "Backcourt"),
            aes(x = zoneBasic, fill = typeEvent)) +
  geom_bar() +
  coord_flip() +
  scale_fill_brewer("", palette = "Set1") +
  transition_states(slugSeason) +
  labs(title = 'Season: {closest_state}')


## -----------------------------------------------------------------------------------------------------
df <- teams_shots(teams = "Golden State Warriors", seasons = 2021)
player <- "Stephen Curry"
steph_df <- filter(df, namePlayer == player)
make_gam <- gam(isShotMade ~ s(locationX, locationY), #Model for P(make; x & y loca)
                family = binomial, #logistic regression GAM
                data = steph_df)

# Find predicted probabilities over a 50 x 50 grid
make_predict_df <- expand.grid(locationX = seq(-250, 250, length = 50), 
                               locationY = seq(-50, 420, length = 50))

# Get the predicted values from the model and convert to probability values:
make_preds <- predict(make_gam, 
                      newdata = make_predict_df,
                      type = "response")
make_predict_df <- make_predict_df %>%
  mutate(., make_prob = make_preds)

p <- ggplot(data = make_predict_df,
            aes(x = -locationX, y = locationY, fill = make_prob))
p + annotation_custom(court, -250, 250, -50, 420) +
  geom_tile(alpha = .75) +
  scale_fill_distiller("P(Make)", palette = "Spectral") +
  coord_fixed() +
  scale_x_continuous("", breaks = NULL, limits = c(-250, 250)) +
  scale_y_continuous("", breaks = NULL, limits = c(-50, 420))



## -----------------------------------------------------------------------------------------------------
df <- readRDS("../data/nba_stats.rds")


## -----------------------------------------------------------------------------------------------------
dplyr::arrange(df, desc(ortg))


## -----------------------------------------------------------------------------------------------------
df %>% dplyr::group_by(., slugTeam) %>% 
  dplyr::summarize(., avg = mean(ortg)) %>% 
  dplyr::arrange(desc(-avg))


## -----------------------------------------------------------------------------------------------------
df %>% 
  dplyr::group_by(., slugTeam, locationGame) %>% 
  dplyr::summarise(., avg = mean(ortg)) %>% 
  dplyr::ungroup() %>% 
  tidyr::pivot_wider(., names_from = locationGame, values_from = avg) %>% 
  dplyr::mutate(., diff = abs(H - A)) %>% 
  dplyr::arrange(., desc(diff))


## -----------------------------------------------------------------------------------------------------
four_glm <- glm(as.factor(outcomeGame) ~ ortg + locationGame,
                family = "binomial",
                data = df)
predict(four_glm, 
        newdata = data.frame(outcomeGame = NA, 
                             ortg = 115, 
                             locationGame = "A"),
        type = "response")


## -----------------------------------------------------------------------------------------------------
summary(four_glm)

