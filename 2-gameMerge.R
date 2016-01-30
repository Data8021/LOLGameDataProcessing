library(dplyr)

## 2nd script to merge data
load("data/allGameData.Rda")
load("data/gamesPlayed.Rda")
load("data/playerDF.Rda")
load("data/teamDF.Rda")
load("data/esportapediaMHLinks.Rda")

## Downselect duplicative variables
minimizedGamesPlayed <- select(gamesPlayed, -gameRealm, -gameCode,
                               -(team1ID:team2Acro))
## Merge with detailed Game Data
fullGameDetails <- left_join(minimizedGamesPlayed, allGameData,
                       by = c("gameHash" = "gameHash"))


fullGameDetails <- left_join(fullGameDetails, playerDF,
                             by = c("t1player1Name" = "playerComboName"))

## Create variables for team acroynm
fullGameDetails <- mutate(fullGameDetails,
                            t1Acro=sapply(fullGameDetails$t1player1Name,
                                            function(x) {
                                              gsub(" .*$", "", x)    
                                            }))

fullGameDetails <- mutate(fullGameDetails,
                          t2Acro=sapply(fullGameDetails$t2player1Name,
                                        function(x) {
                                          gsub(" .*$", "", x)    
                                        }))

## Merge in team1 information
fullGameDetails <- left_join(fullGameDetails, teamDF,
                             by = c("t1Acro" = "teamAcro"))

## Rename fields
fullGameDetails <- rename(fullGameDetails,
                          t1TeamID = teamID,
                          t1TeamSlug = teamSlug,
                          t1TeamName = teamName)

## Merge in team2 information
fullGameDetails <- left_join(fullGameDetails, teamDF,
                             by = c("t2Acro" = "teamAcro"))

## Rename fields
fullGameDetails <- rename(fullGameDetails,
                          t2TeamID = teamID,
                          t2TeamSlug = teamSlug,
                          t2TeamName = teamName)

test <- select(test, -(t1player1Masteries:t2player5Runes))
write.csv(test, "data/test.csv")
write.csv(playerDF, "data/playerDF.csv")
