## ----knitr-ggplot2-opts-ch-1, include = F-------------------------------------------
knitr::opts_chunk$set(fig.align="center",
                      warning = FALSE,
                      message = FALSE)
ggplot2::theme_set(ggplot2::theme_bw())


## ----rproj, echo = F, fig.cap = "The R Project's website."--------------------------
knitr::include_graphics('Screenshots/RHomepage_screenshot.png')


## ----cran, echo = F, fig.cap = 'The Comprehensive R Archive Network, or CRAN, website.'----
knitr::include_graphics('Screenshots/CRANSite_screenshot.png')


## ----rstudiohome, echo = F, fig.cap = 'The RStudio homepage.'-----------------------
knitr::include_graphics('Screenshots/RStudioDesktopPage_screenshot.png')


## ----rstudiodownload, echo = F, fig.cap = "Downloading RStudio."--------------------
knitr::include_graphics('Screenshots/RStudioDownloadPage_screenshot.png')


## ----rstudioopen, echo = F, fig.cap = 'Opening RStudio for the First Time.'---------
knitr::include_graphics('Screenshots/FirstTimeOpenRStudio_screenshot.png')


## -----------------------------------------------------------------------------------
2+2


## ----rstudiopkg, echo = F, fig.cap = "RStudio Pane Showing Installed Packages."-----
knitr::include_graphics('Screenshots/RStudioPackagesTab.png')


## ----rstudioinstallpkg, echo = F, fig.cap = "RStudio Pane Modal Window to Install Packages."----
knitr::include_graphics('Screenshots/RStudioInstallPackages.png')


## ---- eval = F----------------------------------------------------------------------
## # Install the remotes package
## install.packages("remotes")
## # Load remotes to my library for use in this R session
## library(remotes)
## # Install the ballr package from github
## install_github("rtelmore/ballr")
## # Load ballr to my library for use in this R session
## library(ballr)
## # Alternatively, install without loading remotes to the
## #  library, i.e. using the install_github function without
## #  loading remotes.
## remotes::install_github("rtelmore/ballr")


## ---- eval = F----------------------------------------------------------------------
## install.packages("ISAR")
## ## or
## remotes::install_github("rtelmore/ISAR")


## -----------------------------------------------------------------------------------
field_goals <- 2
4*field_goals
pts_by_field_goals <- field_goals*3
pts_by_field_goals


## -----------------------------------------------------------------------------------
pitch <- "ball" 
pitch_count <- 115


## -----------------------------------------------------------------------------------
## vector of character Team Names for the NL West division 
##  in baseball
nl_west_teams <- c("Colorado Rockies", "Los Angeles Dodgers", 
                   "San Diego Padres", "San Francisco Giants") 

## vector of numbers, specifically Nikola Jokic's FG PCT, 
##  3PT FG PCT, and FT PCT during the 2019 NBA playoffs
jokic_shooting_slash <- c(.506, .393, .846) 
## vector of logicals, whether the Denver Broncos made the 
##  playoffs or not from 2015-2019.
broncos_playoffs <- c(TRUE, FALSE, FALSE, FALSE, FALSE)


## -----------------------------------------------------------------------------------
broncos_playoffs[1:2]
jokic_shooting_slash[c(1, 3)]


## -----------------------------------------------------------------------------------
## Assign Par by hole at Cherry Hills Country Club to be an integer
cherry_hills_par <- as.integer(c(4, 4, 4, 4, 5, 3, 4, 3, 4,
                                 4, 5, 3, 4, 4, 3, 4, 5, 5)) 
str(cherry_hills_par)


## -----------------------------------------------------------------------------------
## 3x4 matrix and populate it with the scores for the three players 
## for each of the four rounds. 
pga_2020_top_three <- matrix(c(69, 69, 65, 64,
                               68, 67, 68, 66,
                               69, 67, 65, 68), 
                             nrow = 3, ncol = 4, byrow = TRUE) 
pga_2020_top_three #print the matrix to the console


## -----------------------------------------------------------------------------------
colnames(pga_2020_top_three) <- c("round_1", "round_2", 
                                  "round_3", "round_4")
rownames(pga_2020_top_three) <- c("Morikawa", "Casey", "Johnson")
pga_2020_top_three #print the matrix to the console


## -----------------------------------------------------------------------------------
pga_2020_top_three[2, ]
pga_2020_top_three[2, c(3, 4)]


## -----------------------------------------------------------------------------------
wild_bill <- list("William", "Karlsson", "1/8/1993", "Forward", 
                  "Left", "Vegas Golden Knights", "Sweden", 15, 
                  31, 46)
wild_bill


## -----------------------------------------------------------------------------------
races <- factor(c("downhill", "superg", "downhill", "gs", "slalom", 
                  "slalom", "dcombined"))
races
table(races)


## ----df_hyp-------------------------------------------------------------------------
df <- data.frame(balls = c(4, 2, 0, 1, 2, 1, 0, 3, 2),
                 strikes = c(0, 2, 2, 1, 1, 2, 1, 2, 2),
                 pitches = c(4, 5, 3, 2, 4, 3, 1, 6, 4))
df


## -----------------------------------------------------------------------------------
df$pitches
df[1:3, "pitches"]


## -----------------------------------------------------------------------------------
for (i in 1:5){
  print(i)
}


## ---- eval = FALSE------------------------------------------------------------------
## 
## for (i in something) {            # decide how many times your want the loop to execute
##   some code                       # the code that we want to repeat i times
## }


## -----------------------------------------------------------------------------------
races <- factor(c("downhill", "superg", "downhill", "gs", "slalom", 
                  "slalom", "dcombined"))
for (race in races) {
  statement <- paste("Andrew competed in the", race)
  print (statement)
}
     


## ---- eval = F----------------------------------------------------------------------
## if( logical_condition ) {
##  # execute code if the condition is true
## }
##  # Continue with the rest of the script


## ---- eval = F----------------------------------------------------------------------
## if(condition) {
##   # execute code that runs when the condition is true
## } else {
##   # execute code that runs when the condition is false
## }
##   # Continue with the rest of the script


## ---- eval = F----------------------------------------------------------------------
## 
## if ( condition_1 ) {
##   # execute code that runs when condition1 is true
## } else if ( condition_2 ) {
##   # execute code that runs when condition1 is false but condition2 is true
## } else {
##   # execute code that runs when both condition1 and condition2 are false
## }
##   # Continue with the rest of the script


## ---- eval = FALSE------------------------------------------------------------------
## while (logical_condition) {
##   # Only evaluate this code while the logical_condition is
##   #  TRUE.
## }


## -----------------------------------------------------------------------------------
library(Lahman)
str(Batting)
rox_batting <- subset(Batting, teamID == "COL" & yearID == 2019)


## -----------------------------------------------------------------------------------
dim(rox_batting)
length(rox_batting$R)


## -----------------------------------------------------------------------------------
mean(rox_batting$HR)
median(rox_batting$HR)
sd(rox_batting$HR)


## ---- fig.cap = "Histogram of home runs for the 2019 Colorado Rockies."-------------
hist(rox_batting$HR, main = "Home Runs: 2019 Rockies",
     xlab = "home runs", ylab = "frequency")


## ----hr-rbi, fig.cap = "Scatterplot of home runs (x) and runs batted in (y) for the 2019 Colorado Rockies."----
plot(rox_batting$HR, rox_batting$RBI,
     xlab = "home runs (HR)", ylab = "runs batted in (RBI)",
     col = "#333366", pch = 16)
rox_regression <- lm(RBI ~ HR, data = rox_batting)
abline(rox_regression, col = "black") 
grid()


## ---- eval = TRUE, fig.cap = "Home runs by year for the Colorado Rockies."----------
library(Lahman)
## The Lahman package includes several data sets including one 
##  named Batting. We will subset the data to include only data
##  from the Colorado Rockies.
df <- subset(Batting, teamID == "COL") 
## Compute the total number of home runs per year
df_by_year <- aggregate(HR ~ yearID, data = df, FUN = sum)
plot(x = df_by_year$yearID, y = df_by_year$HR, type = "b",
     pch = 16, xlab = "year", ylab = "home runs", col = "#333366")
grid()


## ---- eval = FALSE------------------------------------------------------------------
## ## New Code


## ---- eval = F----------------------------------------------------------------------
## help(mean)
## ?(mean)
## help.search("plotting")


## -----------------------------------------------------------------------------------
library(Lahman)


## -----------------------------------------------------------------------------------
nrow(Pitching)
df <- subset(Pitching, teamID %in% c("COL", "NYA"))
nrow(df)


## -----------------------------------------------------------------------------------
agg_df <- aggregate(df$W, by = list(df$teamID, df$yearID), sum)


## ---- fig.cap = "Total number of wins by year for the New York Yankees (red) and the Colorado Rockies (purple) from 1990 - 2019."----
df_nya <- subset(agg_df, Group.1 == "NYA" & Group.2 >= 1990)
df_col <- subset(agg_df, Group.1 == "COL" & Group.2 >= 1990)
plot(df_nya$Group.2, df_nya$x, type = "b", col = "#E4002B", 
     ylim = c(50, 120), xlab = "year", ylab = "wins")
points(df_col$Group.2, df_col$x, type = "b", pch = 16, col = "#333366")
grid()


## -----------------------------------------------------------------------------------
df_bat <- subset(Batting, 
                 yearID >= 1990 & teamID %in% c("NYA", "COL"))
singles <- df_bat$H - df_bat$X2B - df_bat$X3B - df_bat$HR
total_bases <- singles + 2*df_bat$X2B + 3*df_bat$X3B + 4*df_bat$HR
slg_pct <- total_bases/df_bat$AB


## ---- warning = FALSE, message = FALSE----------------------------------------------
df_new <- data.frame(yearID = df_bat$yearID,
                     teamID = df_bat$teamID,
                     slg_pct = slg_pct)
df_new <- subset(df_new, teamID == "COL")
agg_df_new <- aggregate(df_new, 
                        by = list(df_new$teamID, df_new$yearID), 
                        mean, na.rm = T)


## ---- fig.cap = "Average slugging percentage for the Colorado Rockies from 1993 - 2019."----
plot(agg_df_new$Group.2, agg_df_new$slg_pct, 
     type = "b", col = "#333366", xlab = "year",
     ylab = "average slugging percentage")
grid()


## -----------------------------------------------------------------------------------
df_col <- subset(Batting, teamID == "COL")
df_col <- df_col[, c("yearID", "H", "X2B", "X3B", "HR", "AB")]
df_col_agg <- aggregate(df_col, 
                        by = list(df_col$yearID),
                        sum)
df_col_agg$singles = df_col_agg$H - df_col_agg$X2B - 
  df_col_agg$X3B - df_col_agg$HR
df_col_agg$total_bases <- df_col_agg$singles + 2*df_col_agg$X2B +
  3*df_col_agg$X3B + 4*df_col_agg$HR
df_col_agg$slg_pct <- df_col_agg$total_bases/df_col_agg$AB


## ----fig.cap = "TEam slugging percentage for the Colorado Rockies from 1993 - 2019."----
plot(df_col_agg$Group.1, df_col_agg$slg_pct, 
     type = "b", col = "#333366", xlab = "year",
     ylab = "slugging percentage")
grid()


## ---- eval = F----------------------------------------------------------------------
## hist()


## ---- eval = F----------------------------------------------------------------------
## remotes::install_github("rtelmore/ISAR")


## -----------------------------------------------------------------------------------
library(ISAR)


## -----------------------------------------------------------------------------------
dim(masters)


## -----------------------------------------------------------------------------------
hist(masters$total)


## -----------------------------------------------------------------------------------
hist(masters$total, border = "navy", xlab = "total", 
     main = "Histogram of total strokes in the 2019 Masters.")
abline(v = mean(masters$total, na.rm = T), lty = 2)


## -----------------------------------------------------------------------------------
df <- masters[order(masters$round_1), c("player_name", "round_1")]
knitr::kable(head(df))

