

############################################# MLB Playoff Predicting Model ###########################################

# Load relevant packages
library(Lahman)
library(dplyr)

# Accesses the data
data(SeriesPost)

############################################# Gathering/Cleaning the data ###########################################

# Creates a dataframe of all recent losing teams
losers <- SeriesPost %>% 
  filter(yearID >= 2000) %>%
  select(yearID, teamIDloser, round)

# Dataframe of the last 19 WS champs
winners <- SeriesPost %>%
  filter(yearID >= 2000 & round == "WS") %>%
  select(yearID, teamIDwinner, round)

# Adjusting the values in the round column
losers$round[losers$round == "ALDS1" | losers$round == "ALDS2" | losers$round == "NLDS1" | losers$round == "NLDS2" |
               losers$round == "ALWC" | losers$round == "NLWC"] <- "DS"
losers$round[losers$round == "ALCS" | losers$round == "NLCS"] <- "CS"
losers$round[losers$round == "WS"] <- "LWS"

# Renames columns and merges
colnames(losers)[colnames(losers) == "teamIDloser"] <- "teamID"
colnames(winners)[colnames(winners) == "teamIDwinner"] <- "teamID"
teams <- rbind(losers, winners)

# loads the predictors
data("Teams")

# Filters the stats we want, merges with teams dataframe
team_stats <- Teams %>%
  filter(yearID >= 2000) %>%
  filter(DivWin == 'Y' | WCWin == 'Y') %>%
  select(yearID, teamID, W, AB, H, HR, ERA, FP)

teams <- merge(teams, team_stats, by.x = c("yearID", 'teamID'), by.y = c("yearID", 'teamID'))

# Makes a variable for batting average
teams$BA <- teams$H/teams$AB
teams <- teams[-c(5, 6)]

############################################## Exploratory Data Analysis #######################################

library(ggplot2)

table(teams$round)
numerics <- c("W", "HR", "ERA", "FP", "BA")
lapply(teams[, numerics], summary)

# Histograms of each numeric feature
ggplot(teams, aes(x = W, fill = round)) +
  geom_histogram() # relatively normal, one potential outlier, wins don't seem to matter much
ggplot(teams, aes(x = HR, fill = round)) +
  geom_histogram() # bimodal, maybe a few outliers, don't seem to matter much either
ggplot(teams, aes(x = ERA, fill = round)) +
  geom_histogram() # relatively normal, 
ggplot(teams, aes(x = FP, fill = round)) +
  geom_histogram() # fine
ggplot(teams, aes(x = BA, fill = round)) +
  geom_histogram() # pretty noisy, one outlier, seems to matter the most

# Plotting time series for each numeric feature
teams %>%
  group_by(yearID) %>%
  summarise(W = mean(W)) %>%
  ggplot(aes(x = yearID, y = W)) + # pretty random
  geom_line() 
teams %>%
  group_by(yearID) %>%
  summarise(HR = mean(HR)) %>%
  ggplot(aes(x = yearID, y = HR)) + # up and down but trends downward, and then back up post-2014
  geom_line()
teams %>%
  group_by(yearID) %>%
  summarise(ERA = mean(ERA)) %>%
  ggplot(aes(x = yearID, y = ERA)) + # steady decline over time (better bullpen pitching?)
  geom_line()
teams %>%
  group_by(yearID) %>%
  summarise(FP = mean(FP)) %>%
  ggplot(aes(x = yearID, y = FP)) + # pretty stable as expected
  geom_line()
teams %>%
  group_by(yearID) %>%
  summarise(BA = mean(BA)) %>%
  ggplot(aes(x = yearID, y = BA)) + # steady decline over time
  geom_line()

########################################## Model Building ########################################

library(MASS)
library(AICcmodavg)

# makes our target variable an ordered factor
teams$round <- factor(teams$round, order = TRUE, levels = c("DS", "CS", "LWS", "WS"))

# Builds the model and prints the summary
mod <- polr(round ~ W + HR + ERA + FP + BA, data = teams, Hess = TRUE)
summary(mod) # AIC = 392.4725
AICc(mod) # AICc = 392.6924

# 95% confidence interval for our parameter estimates (Exponentiated to undo the log scale)
interval <- exp(confint.default(mod))
print(interval)
exp(coef(mod)) # wins and HR seem to be insignificant estimators

mod2 <- polr(round ~ HR + ERA + FP + BA, data = teams, Hess = TRUE) # Removes wins
summary(mod2) # AIC = 390.4935
AICc(mod2) # AICc = 390.6642

mod3 <- polr(round ~ W + ERA + FP + BA, data = teams, Hess = TRUE) # Removes HRs
summary(mod3) # AIC = 391.1524
AICc(mod3) # AICc = 391.3231

mod4 <- polr(round ~ ERA + FP + BA, data = teams, Hess = TRUE) # Removes wins and HRs
summary(mod4) # AIC = 389.1799
AICc(mod4) # AICc = 389.3077

########################################## Testing Assumptions ##################################

library(Hmisc)

# Tests the proportional odds assumption
sf <- function(y) {
  c('Y>=1' = qlogis(mean(y >= 1)),
    'Y>=2' = qlogis(mean(y >= 2)),
    'Y>=3' = qlogis(mean(y >= 3)),
    'Y>=4' = qlogis(mean(y >= 4)))
}

(s <- with(teams, summary(as.numeric(round) ~ W + HR + ERA + FP + BA, fun=sf)))
# proportional odds assumption might not hold for batting average

# Scatterplot matrix to check for multicollinearity
cor(teams[4:8])
pairs(~ W + HR + ERA + FP + BA, data = teams) # no serious issues here

########################################### Predictions ############################################

library(tidyr)

# Dataframe for 2019 playoff teams
teams_19 <- c("WAS", "LAN", "ATL", "STL", "MIL", "NYA", "HOU", "TB", "OAK", "MIN")
new_teams <- data.frame(W = c(93, 106, 97, 91, 89, 103, 107, 96, 97, 101),
                        HR = c(231, 279, 249, 210, 250, 306, 288, 217, 257, 307),
                        ERA = c(4.27, 3.37, 4.19, 3.80, 4.40, 4.31, 3.66, 3.65, 3.97, 4.18),
                        FP = c(0.985, 0.982, 0.987, 0.989, 0.983, 0.983, 0.988, 0.985, 0.986, 0.981),
                        BA = c(0.265, 0.257, 0.258, 0.245, 0.246, 0.267, 0.274, 0.254, 0.249, 0.270))

# Makes predictions with the full model
predictions <- predict(mod, new_teams, type = "probs")
team_predictions <- cbind(teams_19, predictions)

colnames(team_predictions)[1] <- "playoff_teams"

team_predictions <- as.data.frame(team_predictions)
team_predictions_gathered <- gather(team_predictions, key = "round", value = "probability", -playoff_teams)
team_predictions_gathered$probability <- as.numeric(team_predictions_gathered$probability)
team_predictions_gathered$probability <- round(team_predictions_gathered$probability, 3)

# Plots the probabilities for each team
ggplot(team_predictions_gathered, aes(x = playoff_teams, y = probability, fill = round)) +
  geom_bar(stat = 'identity', position = position_dodge())

# We repeat the process with our model without wins and HRs
predictions2 <- predict(mod4, new_teams, type = "probs")
team_predictions2 <- cbind(teams_19, predictions2)

colnames(team_predictions2)[1] <- "playoff_teams"

team_predictions2 <- as.data.frame(team_predictions2)
team_predictions_gathered2 <- gather(team_predictions2, key = "round", value = "probability", -playoff_teams)
team_predictions_gathered2$probability <- as.numeric(team_predictions_gathered2$probability)
team_predictions_gathered2$probability <- round(team_predictions_gathered2$probability, 3)

ggplot(team_predictions_gathered2, aes(x = playoff_teams, y = probability, fill = round)) +
  geom_bar(stat = 'identity', position = position_dodge())


