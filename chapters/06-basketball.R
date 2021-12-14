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
library(teamcolors)


## ----data---------------------------------------------------------------------------------------------
nbagames_2021 <- readRDS("data/nba-games-2021.rds")
nba_ff_team_2021  <- readRDS("data/four-factors-team-2021.rds")
nba_adv_team_2021 <- readRDS("data/advanced-team-2021.rds")


## -----------------------------------------------------------------------------------------------------
df_ff_games <- merge(nbagames_2021, nba_ff_team_2021 ) %>% 
  dplyr::mutate(., ftr = ftaTeam/fgaTeam)
p <- ggplot(data = df_ff_games,
            aes(x = ftr, y = rateFTA))
p + geom_line() +
  theme_bw()


## -----------------------------------------------------------------------------------------------------
game_summary <- nbagames_2021 %>% 
  dplyr::select(., dateGame, idGame, slugTeam, slugMatchup, locationGame,
                outcomeGame, plusminusTeam) 


## -----------------------------------------------------------------------------------------------------
tmp <- inner_join(game_summary, nba_adv_team_2021) %>%
  group_by(slugTeam, locationGame) %>% 
  summarize(avg_pace = mean(pace)) %>% 
  arrange(desc(avg_pace))


## -----------------------------------------------------------------------------------------------------
df_ff <- inner_join(nba_adv_team_2021, nba_ff_team_2021 ) %>%
  dplyr::inner_join(game_summary) %>% 
  dplyr::select(dateGame, idGame, slugTeam, pctOREB, pctTOVTeam, pctEFG,
                rateFTA, ortg, drtg, outcomeGame, locationGame,
                plusminusTeam) %>%
  dplyr::mutate(is_win = ifelse(outcomeGame == "W", T, F))
  


## -----------------------------------------------------------------------------------------------------
cor(select(df_ff, pctEFG, pctOREB, pctTOVTeam, rateFTA))


## -----------------------------------------------------------------------------------------------------
lal_dal_adv <- merge(game_summary, nba_adv_team_2021) %>% 
  dplyr::filter(dateGame == ymd("2021-04-24"),
                slugTeam %in% c("LAL", "DAL"))
lal_dal_adv$slugTeam

pnorm((lal_dal_adv$ortg - mean(nba_adv_team_2021$ortg))/sd(nba_adv_team_2021$ortg))
pnorm((lal_dal_adv$drtg - mean(nba_adv_team_2021$drtg))/sd(nba_adv_team_2021$drtg))

den_lac_id <- nbagames_2021 %>%
  dplyr::filter(., dateGame == ymd("2021-04-01"),
                slugTeam %in% c("LAC", "DEN")) %>% dplyr::pull(idGame)

den_lac_adv <- nba_adv_team_2021 %>%
  dplyr::filter(., idGame == den_lac_id[1])

p <- ggplot(data = nba_adv_team_2021, aes(x = pctEFG))
p + geom_density() + geom_vline(data = den_lac_adv,
                                aes(xintercept = pctEFG, col = slugTeam)) + 
  scale_color_brewer("team", palette = "Set1") + 
  theme_bw()
ggsave("fig/chap-06/lac-den-efg.png", hei = 5, wid = 6)

p <- ggplot(data = nba_adv_team_2021,
            aes(x = pctOREB))
p + geom_density() + geom_vline(data = den_lac_adv,
                                aes(xintercept = pctOREB, col = slugTeam, 
                                    fill = slugTeam)) + 
  scale_fill_brewer("team", palette = "Set1") +
  scale_color_brewer("team", palette = "Set1") +
  guides(col = "none") +
  theme_bw()
ggsave("fig/chap-06/lac-den-oreb.png", hei = 5, wid = 6)


tc <- teamcolors::teamcolors %>%
  dplyr::filter(., name == c("Denver Nuggets"))
cutoff <- den_lac_adv$pctEFG[2]
tmp <- density(nba_adv_team_2021$pctEFG)
tmp <- data.frame(x = tmp$x, y = tmp$y) %>%
  dplyr::mutate(shade = (x >= cutoff))
p <- ggplot(data = tmp,
            aes(x = x, ymin = 0, ymax = y, fill = shade))
p + geom_ribbon() +
  geom_line(aes(y = y)) +
  geom_vline(xintercept = cutoff, color = 'navy', linetype = "dashed") + 
  scale_fill_manual(values = c(tc$primary, tc$secondary)) + 
  annotate(geom = 'text', x = cutoff, y = 6, color = 'black',
           label = 'DEN EFG', hjust = -0.1) + 
  guides(fill = "none") +
  labs(x = "effective field goal percentage", y = "density") +
  theme_bw()
ggsave("fig/chap-06/lac-den-better-efgp.png", hei = 5, wid = 6)


ecdf(nba_adv_team_2021$pctEFG)(den_lac_adv$pctEFG)

rr <- pnorm((den_lac_adv$pctOREB - mean(nba_adv_team_2021$pctOREB)) / 
              sd(nba_adv_team_2021$pctOREB)) 
names(rr) <- den_lac_adv$slugTeam
rr

games <- nbagames_2021 %>%
  dplyr::mutate(., efg = (fg2mTeam + 1.5*fg3mTeam)/(fg2aTeam + fg3aTeam))
df_merge <- merge(games, nba_adv_team_2021)

df_reg <- df_merge %>% 
  merge(., nba_ff_team_2021) %>%
  dplyr::select(., pctEFG, pctOREB, pctTOVTeam, rateFTA, plusminusTeam) %>%
  dplyr::mutate(., win = ifelse(plusminusTeam > 0, TRUE, FALSE),
                pctTOVTeam = pctTOVTeam/100)
adv_glm <- glm(win ~ . - plusminusTeam,
               data = df_reg,
               family = "binomial")
summary(adv_glm)

adv_glm <- glm(win ~ pctEFG + pctOREB + pctTOVTeam + rateFTA,
               data = df_reg,
               family = "binomial")

predict(adv_glm, type = "response")[1:10]


## -----------------------------------------------------------------------------------------------------
ff_lm <- lm(plusminusTeam ~ pctOREB + pctTOVTeam + pctEFG + rateFTA + ortg,
            data = df_ff)


## -----------------------------------------------------------------------------------------------------
ff_lm <- lm(plusminusTeam ~ pctOREB + pctTOVTeam + pctEFG + rateFTA + ortg,
            data = df_ff)
summary(ff_lm)


## ---- eval = T----------------------------------------------------------------------------------------
## Shot Charts
nba_nuggets_shots_2021 <- nbastatR::teams_shots(teams = "Denver Nuggets", 
                                                seasons = 2021)

nba_nuggets_shots <- readRDS("data/nuggets-2015-21.rds")

court <- grid::rasterGrob(jpeg::readJPEG(system.file("images/nba_court.jpg", 
                                                     package = "ISAR")), 
                          width = unit(1, "npc"),
                          height = unit(1, "npc"))
court <- grid::rasterGrob(jpeg::readJPEG("fig/nba_court.jpg"))
player_name <- "Nikola Jokic"
p <- ggplot(data = dplyr::filter(nba_nuggets_shots, 
                                 namePlayer == player_name,
                                 yearSeason == 2021),
            aes(x = locationX, y = locationY))
p + annotation_custom(court, -250, 250, -50, 420) +
  geom_point(alpha = .15) +
  geom_rug(alpha = 0.1) +
  scale_x_continuous(breaks = NULL, labels = NULL, limits = c(-250, 250)) +
  scale_y_continuous(breaks = NULL, labels = NULL, limits = c(-50, 420)) +
  coord_fixed() +
  labs(x = "", y = "")
ggsave("fig/chap-06/jokic-shots-2021.png", height = 6, width = 6)

p <- ggplot(data = dplyr::filter(nba_nuggets_shots, 
                                 namePlayer == player_name,
                                 yearSeason == 2021),
            aes(x = locationX, y = locationY, col = zoneBasic))
p + annotation_custom(court, -250, 250, -50, 420) +
  geom_point(alpha = .5) +
  scale_color_brewer("Zone", palette = "Set1") +
  coord_fixed() +
  xlim(250, -250) +
  ylim(-50, 420) +
  labs(x = "", y = "")
ggsave("fig/chap-06/jokic-shots-w-zones-2021.png", height = 6, width = 8)

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
court <- rasterGrob(readJPEG("fig/nba_court.jpg"), 
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
df <- nbastatR::teams_shots(teams = "Golden State Warriors", seasons = 2021)
player <- "Stephen Curry"
steph_df <- filter(df, namePlayer == player)
make_gam <- mgcv::gam(isShotMade ~ s(locationX, locationY), #Model for P(make; x & y loca)
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

