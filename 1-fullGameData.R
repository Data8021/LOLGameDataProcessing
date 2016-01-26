## 1st Script -- Begin processing raw game files
library(dplyr)

## Load functions

extractPlayerData <- function(num = i, name, firstLink, secondLink, thirdLink = "skip"){

  if (thirdLink == "skip") {
    allGameData[num, paste0("team1player1", name)] <<- fullGameList[[num]][[firstLink]][[secondLink]][[1]]
    allGameData[num, paste0("team1player2", name)] <<- fullGameList[[num]][[firstLink]][[secondLink]][[2]]
    allGameData[num, paste0("team1player3", name)] <<- fullGameList[[num]][[firstLink]][[secondLink]][[3]]
    allGameData[num, paste0("team1player4", name)] <<- fullGameList[[num]][[firstLink]][[secondLink]][[4]]
    allGameData[num, paste0("team1player5", name)] <<- fullGameList[[num]][[firstLink]][[secondLink]][[5]]
    allGameData[num, paste0("team2player1", name)] <<- fullGameList[[num]][[firstLink]][[secondLink]][[6]]
    allGameData[num, paste0("team2player2", name)] <<- fullGameList[[num]][[firstLink]][[secondLink]][[7]]
    allGameData[num, paste0("team2player3", name)] <<- fullGameList[[num]][[firstLink]][[secondLink]][[8]]
    allGameData[num, paste0("team2player4", name)] <<- fullGameList[[num]][[firstLink]][[secondLink]][[9]]
    allGameData[num, paste0("team2player5", name)] <<- fullGameList[[num]][[firstLink]][[secondLink]][[10]]
  } else {
    allGameData[num, paste0("team1player1", name)] <<- fullGameList[[num]][[firstLink]][[secondLink]][[thirdLink]][[1]]
    allGameData[num, paste0("team1player2", name)] <<- fullGameList[[num]][[firstLink]][[secondLink]][[thirdLink]][[2]]
    allGameData[num, paste0("team1player3", name)] <<- fullGameList[[num]][[firstLink]][[secondLink]][[thirdLink]][[3]]
    allGameData[num, paste0("team1player4", name)] <<- fullGameList[[num]][[firstLink]][[secondLink]][[thirdLink]][[4]]
    allGameData[num, paste0("team1player5", name)] <<- fullGameList[[num]][[firstLink]][[secondLink]][[thirdLink]][[5]]
    allGameData[num, paste0("team2player1", name)] <<- fullGameList[[num]][[firstLink]][[secondLink]][[thirdLink]][[6]]
    allGameData[num, paste0("team2player2", name)] <<- fullGameList[[num]][[firstLink]][[secondLink]][[thirdLink]][[7]]
    allGameData[num, paste0("team2player3", name)] <<- fullGameList[[num]][[firstLink]][[secondLink]][[thirdLink]][[8]]
    allGameData[num, paste0("team2player4", name)] <<- fullGameList[[num]][[firstLink]][[secondLink]][[thirdLink]][[9]]
    allGameData[num, paste0("team2player5", name)] <<- fullGameList[[num]][[firstLink]][[secondLink]][[thirdLink]][[10]]
  }
    
}

## Load raw game details
load("data/fullGameList.Rda")

## Initialize data frame
allGameData <- data.frame(gameHash = as.character(),
                          gameId = as.numeric(),
                          platformId = as.character(),
                          gameCreation = as.numeric(),
                          gameDuration = as.numeric(),
                          queueId = as.numeric(),
                          mapId = as.numeric(),
                          seasonId = as.numeric(),
                          gameVersion = as.numeric(),
                          gameMode = as.character(),
                          gameType = as.character(),
                          t1Win = as.character(),
                          t1FirstBlood = as.logical(),
                          t1FirstTower = as.logical(), 
                          t1FirstInhibitor = as.logical(),
                          t1FirstBaron = as.logical(),
                          t1FirstDragon = as.logical(),
                          t1FirstRiftHerald = as.logical(),
                          t1TowerKills = as.numeric(),
                          t1InhibitorKills = as.numeric(),
                          t1BaronKills = as.numeric(),
                          t1DragonKills = as.numeric(),
                          t1VilemawKills = as.numeric(),
                          t1RiftHeraldKills = as.numeric(),
                          t1DominionVictoryScore = as.numeric(),
                          t2Win = as.character(),
                          t2FirstBlood = as.logical(),
                          t2FirstTower = as.logical(), 
                          t2FirstInhibitor = as.logical(),
                          t2FirstBaron = as.logical(),
                          t2FirstDragon = as.logical(),
                          t2FirstRiftHerald = as.logical(),
                          t2TowerKills = as.numeric(),
                          t2InhibitorKills = as.numeric(),
                          t2BaronKills = as.numeric(),
                          t2DragonKills = as.numeric(),
                          t2VilemawKills = as.numeric(),
                          t2RiftHeraldKills = as.numeric(),
                          t2DominionVictoryScore = as.numeric(),
                          team1player1Name = as.character(),
                          team1player2Name = as.character(),
                          team1player3Name = as.character(),
                          team1player4Name = as.character(),
                          team1player5Name = as.character(),
                          team2player1Name = as.character(),
                          team2player2Name = as.character(),
                          team2player3Name = as.character(),
                          team2player4Name = as.character(),
                          team2player5Name = as.character(),
                          team1player1Champion = as.numeric(),
                          team1player2Champion = as.numeric(),
                          team1player3Champion = as.numeric(),
                          team1player4Champion = as.numeric(),
                          team1player5Champion = as.numeric(),
                          team2player1Champion = as.numeric(),
                          team2player2Champion = as.numeric(),
                          team2player3Champion = as.numeric(),
                          team2player4Champion = as.numeric(),
                          team2player5Champion = as.numeric(),
                          team1player1Spell1 = as.numeric(),
                          team1player2Spell1 = as.numeric(),
                          team1player3Spell1 = as.numeric(),
                          team1player4Spell1 = as.numeric(),
                          team1player5Spell1 = as.numeric(),
                          team2player1Spell1 = as.numeric(),
                          team2player2Spell1 = as.numeric(),
                          team2player3Spell1 = as.numeric(),
                          team2player4Spell1 = as.numeric(),
                          team2player5Spell1 = as.numeric(),
                          team1player1Spell2 = as.numeric(),
                          team1player2Spell2 = as.numeric(),
                          team1player3Spell2 = as.numeric(),
                          team1player4Spell2 = as.numeric(),
                          team1player5Spell2 = as.numeric(),
                          team2player1Spell2 = as.numeric(),
                          team2player2Spell2 = as.numeric(),
                          team2player3Spell2 = as.numeric(),
                          team2player4Spell2 = as.numeric(),
                          team2player5Spell2 = as.numeric(),
                          team1player1Masteries = I(list()),
                          team1player2Masteries = I(list()),
                          team1player3Masteries = I(list()),
                          team1player4Masteries = I(list()),
                          team1player5Masteries = I(list()),
                          team2player1Masteries = I(list()),
                          team2player2Masteries = I(list()),
                          team2player3Masteries = I(list()),
                          team2player4Masteries = I(list()),
                          team2player5Masteries = I(list()),
                          team1player1Runes = I(list()),
                          team1player2Runes = I(list()),
                          team1player3Runes = I(list()),
                          team1player4Runes = I(list()),
                          team1player5Runes = I(list()),
                          team2player1Runes = I(list()),
                          team2player2Runes = I(list()),
                          team2player3Runes = I(list()),
                          team2player4Runes = I(list()),
                          team2player5Runes = I(list()),
                          team1player1Item0 = as.numeric(),
                          team1player2Item0 = as.numeric(),
                          team1player3Item0 = as.numeric(),
                          team1player4Item0 = as.numeric(),
                          team1player5Item0 = as.numeric(),
                          team2player1Item0 = as.numeric(),
                          team2player2Item0 = as.numeric(),
                          team2player3Item0 = as.numeric(),
                          team2player4Item0 = as.numeric(),
                          team2player5Item0 = as.numeric(),
                          team1player1Item1 = as.numeric(),
                          team1player2Item1 = as.numeric(),
                          team1player3Item1 = as.numeric(),
                          team1player4Item1 = as.numeric(),
                          team1player5Item1 = as.numeric(),
                          team2player1Item1 = as.numeric(),
                          team2player2Item1 = as.numeric(),
                          team2player3Item1 = as.numeric(),
                          team2player4Item1 = as.numeric(),
                          team2player5Item1 = as.numeric(),
                          team1player1Item2 = as.numeric(),
                          team1player2Item2 = as.numeric(),
                          team1player3Item2 = as.numeric(),
                          team1player4Item2 = as.numeric(),
                          team1player5Item2 = as.numeric(),
                          team2player1Item2 = as.numeric(),
                          team2player2Item2 = as.numeric(),
                          team2player3Item2 = as.numeric(),
                          team2player4Item2 = as.numeric(),
                          team2player5Item2 = as.numeric(),
                          team1player1Item3 = as.numeric(),
                          team1player2Item3 = as.numeric(),
                          team1player3Item3 = as.numeric(),
                          team1player4Item3 = as.numeric(),
                          team1player5Item3 = as.numeric(),
                          team2player1Item3 = as.numeric(),
                          team2player2Item3 = as.numeric(),
                          team2player3Item3 = as.numeric(),
                          team2player4Item3 = as.numeric(),
                          team2player5Item3 = as.numeric(),
                          team1player1Item4 = as.numeric(),
                          team1player2Item4 = as.numeric(),
                          team1player3Item4 = as.numeric(),
                          team1player4Item4 = as.numeric(),
                          team1player5Item4 = as.numeric(),
                          team2player1Item4 = as.numeric(),
                          team2player2Item4 = as.numeric(),
                          team2player3Item4 = as.numeric(),
                          team2player4Item4 = as.numeric(),
                          team2player5Item4 = as.numeric(),
                          team1player1Item5 = as.numeric(),
                          team1player2Item5 = as.numeric(),
                          team1player3Item5 = as.numeric(),
                          team1player4Item5 = as.numeric(),
                          team1player5Item5 = as.numeric(),
                          team2player1Item5 = as.numeric(),
                          team2player2Item5 = as.numeric(),
                          team2player3Item5 = as.numeric(),
                          team2player4Item5 = as.numeric(),
                          team2player5Item5 = as.numeric(),
                          team1player1Item6 = as.numeric(),
                          team1player2Item6 = as.numeric(),
                          team1player3Item6 = as.numeric(),
                          team1player4Item6 = as.numeric(),
                          team1player5Item6 = as.numeric(),
                          team2player1Item6 = as.numeric(),
                          team2player2Item6 = as.numeric(),
                          team2player3Item6 = as.numeric(),
                          team2player4Item6 = as.numeric(),
                          team2player5Item6 = as.numeric(),
                          team1player1Kills = as.numeric(),
                          team1player2Kills = as.numeric(),
                          team1player3Kills = as.numeric(),
                          team1player4Kills = as.numeric(),
                          team1player5Kills = as.numeric(),
                          team2player1Kills = as.numeric(),
                          team2player2Kills = as.numeric(),
                          team2player3Kills = as.numeric(),
                          team2player4Kills = as.numeric(),
                          team2player5Kills = as.numeric(),
                          team1player1Deaths = as.numeric(),
                          team1player2Deaths = as.numeric(),
                          team1player3Deaths = as.numeric(),
                          team1player4Deaths = as.numeric(),
                          team1player5Deaths = as.numeric(),
                          team2player1Deaths = as.numeric(),
                          team2player2Deaths = as.numeric(),
                          team2player3Deaths = as.numeric(),
                          team2player4Deaths = as.numeric(),
                          team2player5Deaths = as.numeric(),
                          team1player1Assists = as.numeric(),
                          team1player2Assists = as.numeric(),
                          team1player3Assists = as.numeric(),
                          team1player4Assists = as.numeric(),
                          team1player5Assists = as.numeric(),
                          team2player1Assists = as.numeric(),
                          team2player2Assists = as.numeric(),
                          team2player3Assists = as.numeric(),
                          team2player4Assists = as.numeric(),
                          team2player5Assists = as.numeric(),
                          team1player1LargestKillingSpree = as.numeric(),
                          team1player2LargestKillingSpree = as.numeric(),
                          team1player3LargestKillingSpree = as.numeric(),
                          team1player4LargestKillingSpree = as.numeric(),
                          team1player5LargestKillingSpree = as.numeric(),
                          team2player1LargestKillingSpree = as.numeric(),
                          team2player2LargestKillingSpree = as.numeric(),
                          team2player3LargestKillingSpree = as.numeric(),
                          team2player4LargestKillingSpree = as.numeric(),
                          team2player5LargestKillingSpree = as.numeric(),
                          team1player1LargestMultiKill = as.numeric(),
                          team1player2LargestMultiKill = as.numeric(),
                          team1player3LargestMultiKill = as.numeric(),
                          team1player4LargestMultiKill = as.numeric(),
                          team1player5LargestMultiKill = as.numeric(),
                          team2player1LargestMultiKill = as.numeric(),
                          team2player2LargestMultiKill = as.numeric(),
                          team2player3LargestMultiKill = as.numeric(),
                          team2player4LargestMultiKill = as.numeric(),
                          team2player5LargestMultiKill = as.numeric(),
                          team1player1KillingSprees = as.numeric(),
                          team1player2KillingSprees = as.numeric(),
                          team1player3KillingSprees = as.numeric(),
                          team1player4KillingSprees = as.numeric(),
                          team1player5KillingSprees = as.numeric(),
                          team2player1KillingSprees = as.numeric(),
                          team2player2KillingSprees = as.numeric(),
                          team2player3KillingSprees = as.numeric(),
                          team2player4KillingSprees = as.numeric(),
                          team2player5KillingSprees = as.numeric(),
                          team1player1LongestTimeSpentLiving = as.numeric(),
                          team1player2LongestTimeSpentLiving = as.numeric(),
                          team1player3LongestTimeSpentLiving = as.numeric(),
                          team1player4LongestTimeSpentLiving = as.numeric(),
                          team1player5LongestTimeSpentLiving = as.numeric(),
                          team2player1LongestTimeSpentLiving = as.numeric(),
                          team2player2LongestTimeSpentLiving = as.numeric(),
                          team2player3LongestTimeSpentLiving = as.numeric(),
                          team2player4LongestTimeSpentLiving = as.numeric(),
                          team2player5LongestTimeSpentLiving = as.numeric(),
                          team1player1DoubleKills = as.numeric(),
                          team1player2DoubleKills = as.numeric(),
                          team1player3DoubleKills = as.numeric(),
                          team1player4DoubleKills = as.numeric(),
                          team1player5DoubleKills = as.numeric(),
                          team2player1DoubleKills = as.numeric(),
                          team2player2DoubleKills = as.numeric(),
                          team2player3DoubleKills = as.numeric(),
                          team2player4DoubleKills = as.numeric(),
                          team2player5DoubleKills = as.numeric(),
                          team1player1TripleKills = as.numeric(),
                          team1player2TripleKills = as.numeric(),
                          team1player3TripleKills = as.numeric(),
                          team1player4TripleKills = as.numeric(),
                          team1player5TripleKills = as.numeric(),
                          team2player1TripleKills = as.numeric(),
                          team2player2TripleKills = as.numeric(),
                          team2player3TripleKills = as.numeric(),
                          team2player4TripleKills = as.numeric(),
                          team2player5TripleKills = as.numeric(),
                          team1player1QuadraKills = as.numeric(),
                          team1player2QuadraKills = as.numeric(),
                          team1player3QuadraKills = as.numeric(),
                          team1player4QuadraKills = as.numeric(),
                          team1player5QuadraKills = as.numeric(),
                          team2player1QuadraKills = as.numeric(),
                          team2player2QuadraKills = as.numeric(),
                          team2player3QuadraKills = as.numeric(),
                          team2player4QuadraKills = as.numeric(),
                          team2player5QuadraKills = as.numeric(),
                          team1player1PentaKills = as.numeric(),
                          team1player2PentaKills = as.numeric(),
                          team1player3PentaKills = as.numeric(),
                          team1player4PentaKills = as.numeric(),
                          team1player5PentaKills = as.numeric(),
                          team2player1PentaKills = as.numeric(),
                          team2player2PentaKills = as.numeric(),
                          team2player3PentaKills = as.numeric(),
                          team2player4PentaKills = as.numeric(),
                          team2player5PentaKills = as.numeric(),
                          team1player1UnrealKills = as.numeric(),
                          team1player2UnrealKills = as.numeric(),
                          team1player3UnrealKills = as.numeric(),
                          team1player4UnrealKills = as.numeric(),
                          team1player5UnrealKills = as.numeric(),
                          team2player1UnrealKills = as.numeric(),
                          team2player2UnrealKills = as.numeric(),
                          team2player3UnrealKills = as.numeric(),
                          team2player4UnrealKills = as.numeric(),
                          team2player5UnrealKills = as.numeric(),
                          team1player1TotalDamageDealt = as.numeric(),
                          team1player2TotalDamageDealt = as.numeric(),
                          team1player3TotalDamageDealt = as.numeric(),
                          team1player4TotalDamageDealt = as.numeric(),
                          team1player5TotalDamageDealt = as.numeric(),
                          team2player1TotalDamageDealt = as.numeric(),
                          team2player2TotalDamageDealt = as.numeric(),
                          team2player3TotalDamageDealt = as.numeric(),
                          team2player4TotalDamageDealt = as.numeric(),
                          team2player5TotalDamageDealt = as.numeric(),
                          team1player1MagicDamageDealt = as.numeric(),
                          team1player2MagicDamageDealt = as.numeric(),
                          team1player3MagicDamageDealt = as.numeric(),
                          team1player4MagicDamageDealt = as.numeric(),
                          team1player5MagicDamageDealt = as.numeric(),
                          team2player1MagicDamageDealt = as.numeric(),
                          team2player2MagicDamageDealt = as.numeric(),
                          team2player3MagicDamageDealt = as.numeric(),
                          team2player4MagicDamageDealt = as.numeric(),
                          team2player5MagicDamageDealt = as.numeric(),
                          team1player1PhysicalDamageDealt = as.numeric(),
                          team1player2PhysicalDamageDealt = as.numeric(),
                          team1player3PhysicalDamageDealt = as.numeric(),
                          team1player4PhysicalDamageDealt = as.numeric(),
                          team1player5PhysicalDamageDealt = as.numeric(),
                          team2player1PhysicalDamageDealt = as.numeric(),
                          team2player2PhysicalDamageDealt = as.numeric(),
                          team2player3PhysicalDamageDealt = as.numeric(),
                          team2player4PhysicalDamageDealt = as.numeric(),
                          team2player5PhysicalDamageDealt = as.numeric(),
                          team1player1TrueDamageDealt = as.numeric(),
                          team1player2TrueDamageDealt = as.numeric(),
                          team1player3TrueDamageDealt = as.numeric(),
                          team1player4TrueDamageDealt = as.numeric(),
                          team1player5TrueDamageDealt = as.numeric(),
                          team2player1TrueDamageDealt = as.numeric(),
                          team2player2TrueDamageDealt = as.numeric(),
                          team2player3TrueDamageDealt = as.numeric(),
                          team2player4TrueDamageDealt = as.numeric(),
                          team2player5TrueDamageDealt = as.numeric(),
                          team1player1LargestCriticalStrike = as.numeric(),
                          team1player2LargestCriticalStrike = as.numeric(),
                          team1player3LargestCriticalStrike = as.numeric(),
                          team1player4LargestCriticalStrike = as.numeric(),
                          team1player5LargestCriticalStrike = as.numeric(),
                          team2player1LargestCriticalStrike = as.numeric(),
                          team2player2LargestCriticalStrike = as.numeric(),
                          team2player3LargestCriticalStrike = as.numeric(),
                          team2player4LargestCriticalStrike = as.numeric(),
                          team2player5LargestCriticalStrike = as.numeric(),
                          team1player1TotalDamageDealtToChampions = as.numeric(),
                          team1player2TotalDamageDealtToChampions = as.numeric(),
                          team1player3TotalDamageDealtToChampions = as.numeric(),
                          team1player4TotalDamageDealtToChampions = as.numeric(),
                          team1player5TotalDamageDealtToChampions = as.numeric(),
                          team2player1TotalDamageDealtToChampions = as.numeric(),
                          team2player2TotalDamageDealtToChampions = as.numeric(),
                          team2player3TotalDamageDealtToChampions = as.numeric(),
                          team2player4TotalDamageDealtToChampions = as.numeric(),
                          team2player5TotalDamageDealtToChampions = as.numeric(),
                          team1player1MagicDamageDealtToChampions = as.numeric(),
                          team1player2MagicDamageDealtToChampions = as.numeric(),
                          team1player3MagicDamageDealtToChampions = as.numeric(),
                          team1player4MagicDamageDealtToChampions = as.numeric(),
                          team1player5MagicDamageDealtToChampions = as.numeric(),
                          team2player1MagicDamageDealtToChampions = as.numeric(),
                          team2player2MagicDamageDealtToChampions = as.numeric(),
                          team2player3MagicDamageDealtToChampions = as.numeric(),
                          team2player4MagicDamageDealtToChampions = as.numeric(),
                          team2player5MagicDamageDealtToChampions = as.numeric(),
                          team1player1PhysicalDamageDealtToChampions = as.numeric(),
                          team1player2PhysicalDamageDealtToChampions = as.numeric(),
                          team1player3PhysicalDamageDealtToChampions = as.numeric(),
                          team1player4PhysicalDamageDealtToChampions = as.numeric(),
                          team1player5PhysicalDamageDealtToChampions = as.numeric(),
                          team2player1PhysicalDamageDealtToChampions = as.numeric(),
                          team2player2PhysicalDamageDealtToChampions = as.numeric(),
                          team2player3PhysicalDamageDealtToChampions = as.numeric(),
                          team2player4PhysicalDamageDealtToChampions = as.numeric(),
                          team2player5PhysicalDamageDealtToChampions = as.numeric(),
                          team1player1TrueDamageDealtToChampions = as.numeric(),
                          team1player2TrueDamageDealtToChampions = as.numeric(),
                          team1player3TrueDamageDealtToChampions = as.numeric(),
                          team1player4TrueDamageDealtToChampions = as.numeric(),
                          team1player5TrueDamageDealtToChampions = as.numeric(),
                          team2player1TrueDamageDealtToChampions = as.numeric(),
                          team2player2TrueDamageDealtToChampions = as.numeric(),
                          team2player3TrueDamageDealtToChampions = as.numeric(),
                          team2player4TrueDamageDealtToChampions = as.numeric(),
                          team2player5TrueDamageDealtToChampions = as.numeric(),
                          team1player1TotalHeal = as.numeric(),
                          team1player2TotalHeal = as.numeric(),
                          team1player3TotalHeal = as.numeric(),
                          team1player4TotalHeal = as.numeric(),
                          team1player5TotalHeal = as.numeric(),
                          team2player1TotalHeal = as.numeric(),
                          team2player2TotalHeal = as.numeric(),
                          team2player3TotalHeal = as.numeric(),
                          team2player4TotalHeal = as.numeric(),
                          team2player5TotalHeal = as.numeric(),
                          team1player1TotalUnitsHealed = as.numeric(),
                          team1player2TotalUnitsHealed = as.numeric(),
                          team1player3TotalUnitsHealed = as.numeric(),
                          team1player4TotalUnitsHealed = as.numeric(),
                          team1player5TotalUnitsHealed = as.numeric(),
                          team2player1TotalUnitsHealed = as.numeric(),
                          team2player2TotalUnitsHealed = as.numeric(),
                          team2player3TotalUnitsHealed = as.numeric(),
                          team2player4TotalUnitsHealed = as.numeric(),
                          team2player5TotalUnitsHealed = as.numeric(),
                          team1player1TotalDamageTaken = as.numeric(),
                          team1player2TotalDamageTaken = as.numeric(),
                          team1player3TotalDamageTaken = as.numeric(),
                          team1player4TotalDamageTaken = as.numeric(),
                          team1player5TotalDamageTaken = as.numeric(),
                          team2player1TotalDamageTaken = as.numeric(),
                          team2player2TotalDamageTaken = as.numeric(),
                          team2player3TotalDamageTaken = as.numeric(),
                          team2player4TotalDamageTaken = as.numeric(),
                          team2player5TotalDamageTaken = as.numeric(),
                          team1player1MagicalDamageTaken = as.numeric(),
                          team1player2MagicalDamageTaken = as.numeric(),
                          team1player3MagicalDamageTaken = as.numeric(),
                          team1player4MagicalDamageTaken = as.numeric(),
                          team1player5MagicalDamageTaken = as.numeric(),
                          team2player1MagicalDamageTaken = as.numeric(),
                          team2player2MagicalDamageTaken = as.numeric(),
                          team2player3MagicalDamageTaken = as.numeric(),
                          team2player4MagicalDamageTaken = as.numeric(),
                          team2player5MagicalDamageTaken = as.numeric(),
                          team1player1PhysicalDamageTaken = as.numeric(),
                          team1player2PhysicalDamageTaken = as.numeric(),
                          team1player3PhysicalDamageTaken = as.numeric(),
                          team1player4PhysicalDamageTaken = as.numeric(),
                          team1player5PhysicalDamageTaken = as.numeric(),
                          team2player1PhysicalDamageTaken = as.numeric(),
                          team2player2PhysicalDamageTaken = as.numeric(),
                          team2player3PhysicalDamageTaken = as.numeric(),
                          team2player4PhysicalDamageTaken = as.numeric(),
                          team2player5PhysicalDamageTaken = as.numeric(), 
                          team1player1TrueDamageTaken = as.numeric(),
                          team1player2TrueDamageTaken = as.numeric(),
                          team1player3TrueDamageTaken = as.numeric(),
                          team1player4TrueDamageTaken = as.numeric(),
                          team1player5TrueDamageTaken = as.numeric(),
                          team2player1TrueDamageTaken = as.numeric(),
                          team2player2TrueDamageTaken = as.numeric(),
                          team2player3TrueDamageTaken = as.numeric(),
                          team2player4TrueDamageTaken = as.numeric(),
                          team2player5TrueDamageTaken = as.numeric(), 
                          team1player1GoldEarned = as.numeric(),
                          team1player2GoldEarned = as.numeric(),
                          team1player3GoldEarned = as.numeric(),
                          team1player4GoldEarned = as.numeric(),
                          team1player5GoldEarned = as.numeric(),
                          team2player1GoldEarned = as.numeric(),
                          team2player2GoldEarned = as.numeric(),
                          team2player3GoldEarned = as.numeric(),
                          team2player4GoldEarned = as.numeric(),
                          team2player5GoldEarned = as.numeric(), 
                          team1player1GoldSpent = as.numeric(),
                          team1player2GoldSpent = as.numeric(),
                          team1player3GoldSpent = as.numeric(),
                          team1player4GoldSpent = as.numeric(),
                          team1player5GoldSpent = as.numeric(),
                          team2player1GoldSpent = as.numeric(),
                          team2player2GoldSpent = as.numeric(),
                          team2player3GoldSpent = as.numeric(),
                          team2player4GoldSpent = as.numeric(),
                          team2player5GoldSpent = as.numeric(), 
                          team1player1TurretKills = as.numeric(),
                          team1player2TurretKills = as.numeric(),
                          team1player3TurretKills = as.numeric(),
                          team1player4TurretKills = as.numeric(),
                          team1player5TurretKills = as.numeric(),
                          team2player1TurretKills = as.numeric(),
                          team2player2TurretKills = as.numeric(),
                          team2player3TurretKills = as.numeric(),
                          team2player4TurretKills = as.numeric(),
                          team2player5TurretKills = as.numeric(),
                          team1player1InhibitorKills = as.numeric(),
                          team1player2InhibitorKills = as.numeric(),
                          team1player3InhibitorKills = as.numeric(),
                          team1player4InhibitorKills = as.numeric(),
                          team1player5InhibitorKills = as.numeric(),
                          team2player1InhibitorKills = as.numeric(),
                          team2player2InhibitorKills = as.numeric(),
                          team2player3InhibitorKills = as.numeric(),
                          team2player4InhibitorKills = as.numeric(),
                          team2player5InhibitorKills = as.numeric(),
                          team1player1TotalMinionsKilled = as.numeric(),
                          team1player2TotalMinionsKilled = as.numeric(),
                          team1player3TotalMinionsKilled = as.numeric(),
                          team1player4TotalMinionsKilled = as.numeric(),
                          team1player5TotalMinionsKilled = as.numeric(),
                          team2player1TotalMinionsKilled = as.numeric(),
                          team2player2TotalMinionsKilled = as.numeric(),
                          team2player3TotalMinionsKilled = as.numeric(),
                          team2player4TotalMinionsKilled = as.numeric(),
                          team2player5TotalMinionsKilled = as.numeric(),
                          team1player1NeutralMinionsKilled = as.numeric(),
                          team1player2NeutralMinionsKilled = as.numeric(),
                          team1player3NeutralMinionsKilled = as.numeric(),
                          team1player4NeutralMinionsKilled = as.numeric(),
                          team1player5NeutralMinionsKilled = as.numeric(),
                          team2player1NeutralMinionsKilled = as.numeric(),
                          team2player2NeutralMinionsKilled = as.numeric(),
                          team2player3NeutralMinionsKilled = as.numeric(),
                          team2player4NeutralMinionsKilled = as.numeric(),
                          team2player5NeutralMinionsKilled = as.numeric(),
                          team1player1NeutralMinionsKilledTeamJungle = as.numeric(),
                          team1player2NeutralMinionsKilledTeamJungle = as.numeric(),
                          team1player3NeutralMinionsKilledTeamJungle = as.numeric(),
                          team1player4NeutralMinionsKilledTeamJungle = as.numeric(),
                          team1player5NeutralMinionsKilledTeamJungle = as.numeric(),
                          team2player1NeutralMinionsKilledTeamJungle = as.numeric(),
                          team2player2NeutralMinionsKilledTeamJungle = as.numeric(),
                          team2player3NeutralMinionsKilledTeamJungle = as.numeric(),
                          team2player4NeutralMinionsKilledTeamJungle = as.numeric(),
                          team2player5NeutralMinionsKilledTeamJungle = as.numeric(),
                          team1player1NeutralMinionsKilledEnemyJungle = as.numeric(),
                          team1player2NeutralMinionsKilledEnemyJungle = as.numeric(),
                          team1player3NeutralMinionsKilledEnemyJungle = as.numeric(),
                          team1player4NeutralMinionsKilledEnemyJungle = as.numeric(),
                          team1player5NeutralMinionsKilledEnemyJungle = as.numeric(),
                          team2player1NeutralMinionsKilledEnemyJungle = as.numeric(),
                          team2player2NeutralMinionsKilledEnemyJungle = as.numeric(),
                          team2player3NeutralMinionsKilledEnemyJungle = as.numeric(),
                          team2player4NeutralMinionsKilledEnemyJungle = as.numeric(),
                          team2player5NeutralMinionsKilledEnemyJungle = as.numeric(),
                          team1player1TotalTimeCrowdControlDealt = as.numeric(),
                          team1player2TotalTimeCrowdControlDealt = as.numeric(),
                          team1player3TotalTimeCrowdControlDealt = as.numeric(),
                          team1player4TotalTimeCrowdControlDealt = as.numeric(),
                          team1player5TotalTimeCrowdControlDealt = as.numeric(),
                          team2player1TotalTimeCrowdControlDealt = as.numeric(),
                          team2player2TotalTimeCrowdControlDealt = as.numeric(),
                          team2player3TotalTimeCrowdControlDealt = as.numeric(),
                          team2player4TotalTimeCrowdControlDealt = as.numeric(),
                          team2player5TotalTimeCrowdControlDealt = as.numeric(),
                          team1player1ChampLevel = as.numeric(),
                          team1player2ChampLevel = as.numeric(),
                          team1player3ChampLevel = as.numeric(),
                          team1player4ChampLevel = as.numeric(),
                          team1player5ChampLevel = as.numeric(),
                          team2player1ChampLevel = as.numeric(),
                          team2player2ChampLevel = as.numeric(),
                          team2player3ChampLevel = as.numeric(),
                          team2player4ChampLevel = as.numeric(),
                          team2player5ChampLevel = as.numeric(),
                          team1player1VisionWardsBoughtInGame = as.numeric(),
                          team1player2VisionWardsBoughtInGame = as.numeric(),
                          team1player3VisionWardsBoughtInGame = as.numeric(),
                          team1player4VisionWardsBoughtInGame = as.numeric(),
                          team1player5VisionWardsBoughtInGame = as.numeric(),
                          team2player1VisionWardsBoughtInGame = as.numeric(),
                          team2player2VisionWardsBoughtInGame = as.numeric(),
                          team2player3VisionWardsBoughtInGame = as.numeric(),
                          team2player4VisionWardsBoughtInGame = as.numeric(),
                          team2player5VisionWardsBoughtInGame = as.numeric(),
                          team1player1SightWardsBoughtInGame = as.numeric(),
                          team1player2SightWardsBoughtInGame = as.numeric(),
                          team1player3SightWardsBoughtInGame = as.numeric(),
                          team1player4SightWardsBoughtInGame = as.numeric(),
                          team1player5SightWardsBoughtInGame = as.numeric(),
                          team2player1SightWardsBoughtInGame = as.numeric(),
                          team2player2SightWardsBoughtInGame = as.numeric(),
                          team2player3SightWardsBoughtInGame = as.numeric(),
                          team2player4SightWardsBoughtInGame = as.numeric(),
                          team2player5SightWardsBoughtInGame = as.numeric(),
                          team1player1WardsPlaced = as.numeric(),
                          team1player2WardsPlaced = as.numeric(),
                          team1player3WardsPlaced = as.numeric(),
                          team1player4WardsPlaced = as.numeric(),
                          team1player5WardsPlaced = as.numeric(),
                          team2player1WardsPlaced = as.numeric(),
                          team2player2WardsPlaced = as.numeric(),
                          team2player3WardsPlaced = as.numeric(),
                          team2player4WardsPlaced = as.numeric(),
                          team2player5WardsPlaced = as.numeric(),
                          team1player1WardsKilled = as.numeric(),
                          team1player2WardsKilled = as.numeric(),
                          team1player3WardsKilled = as.numeric(),
                          team1player4WardsKilled = as.numeric(),
                          team1player5WardsKilled = as.numeric(),
                          team2player1WardsKilled = as.numeric(),
                          team2player2WardsKilled = as.numeric(),
                          team2player3WardsKilled = as.numeric(),
                          team2player4WardsKilled = as.numeric(),
                          team2player5WardsKilled = as.numeric(),
                          team1player1FirstBloodKill = as.logical(),
                          team1player2FirstBloodKill = as.logical(),
                          team1player3FirstBloodKill = as.logical(),
                          team1player4FirstBloodKill = as.logical(),
                          team1player5FirstBloodKill = as.logical(),
                          team2player1FirstBloodKill = as.logical(),
                          team2player2FirstBloodKill = as.logical(),
                          team2player3FirstBloodKill = as.logical(),
                          team2player4FirstBloodKill = as.logical(),
                          team2player5FirstBloodKill = as.logical(),
                          team1player1FirstBloodAssist = as.logical(),
                          team1player2FirstBloodAssist = as.logical(),
                          team1player3FirstBloodAssist = as.logical(),
                          team1player4FirstBloodAssist = as.logical(),
                          team1player5FirstBloodAssist = as.logical(),
                          team2player1FirstBloodAssist = as.logical(),
                          team2player2FirstBloodAssist = as.logical(),
                          team2player3FirstBloodAssist = as.logical(),
                          team2player4FirstBloodAssist = as.logical(),
                          team2player5FirstBloodAssist = as.logical(),
                          team1player1FirstTowerKill = as.logical(),
                          team1player2FirstTowerKill = as.logical(),
                          team1player3FirstTowerKill = as.logical(),
                          team1player4FirstTowerKill = as.logical(),
                          team1player5FirstTowerKill = as.logical(),
                          team2player1FirstTowerKill = as.logical(),
                          team2player2FirstTowerKill = as.logical(),
                          team2player3FirstTowerKill = as.logical(),
                          team2player4FirstTowerKill = as.logical(),
                          team2player5FirstTowerKill = as.logical(),
                          team1player1FirstTowerAssist = as.logical(),
                          team1player2FirstTowerAssist = as.logical(),
                          team1player3FirstTowerAssist = as.logical(),
                          team1player4FirstTowerAssist = as.logical(),
                          team1player5FirstTowerAssist = as.logical(),
                          team2player1FirstTowerAssist = as.logical(),
                          team2player2FirstTowerAssist = as.logical(),
                          team2player3FirstTowerAssist = as.logical(),
                          team2player4FirstTowerAssist = as.logical(),
                          team2player5FirstTowerAssist = as.logical(),
                          team1player1FirstInhibitorKill = as.logical(),
                          team1player2FirstInhibitorKill = as.logical(),
                          team1player3FirstInhibitorKill = as.logical(),
                          team1player4FirstInhibitorKill = as.logical(),
                          team1player5FirstInhibitorKill = as.logical(),
                          team2player1FirstInhibitorKill = as.logical(),
                          team2player2FirstInhibitorKill = as.logical(),
                          team2player3FirstInhibitorKill = as.logical(),
                          team2player4FirstInhibitorKill = as.logical(),
                          team2player5FirstInhibitorKill = as.logical(),
                          team1player1FirstInhibitorAssist = as.logical(),
                          team1player2FirstInhibitorAssist = as.logical(),
                          team1player3FirstInhibitorAssist = as.logical(),
                          team1player4FirstInhibitorAssist = as.logical(),
                          team1player5FirstInhibitorAssist = as.logical(),
                          team2player1FirstInhibitorAssist = as.logical(),
                          team2player2FirstInhibitorAssist = as.logical(),
                          team2player3FirstInhibitorAssist = as.logical(),
                          team2player4FirstInhibitorAssist = as.logical(),
                          team2player5FirstInhibitorAssist = as.logical(),
                          team1player1CombatPlayerScore = as.numeric(),
                          team1player2CombatPlayerScore = as.numeric(),
                          team1player3CombatPlayerScore = as.numeric(),
                          team1player4CombatPlayerScore = as.numeric(),
                          team1player5CombatPlayerScore = as.numeric(),
                          team2player1CombatPlayerScore = as.numeric(),
                          team2player2CombatPlayerScore = as.numeric(),
                          team2player3CombatPlayerScore = as.numeric(),
                          team2player4CombatPlayerScore = as.numeric(),
                          team2player5CombatPlayerScore = as.numeric(),
                          team1player1ObjectivePlayerScore = as.numeric(),
                          team1player2ObjectivePlayerScore = as.numeric(),
                          team1player3ObjectivePlayerScore = as.numeric(),
                          team1player4ObjectivePlayerScore = as.numeric(),
                          team1player5ObjectivePlayerScore = as.numeric(),
                          team2player1ObjectivePlayerScore = as.numeric(),
                          team2player2ObjectivePlayerScore = as.numeric(),
                          team2player3ObjectivePlayerScore = as.numeric(),
                          team2player4ObjectivePlayerScore = as.numeric(),
                          team2player5ObjectivePlayerScore = as.numeric(),
                          team1player1TotalPlayerScore = as.numeric(),
                          team1player2TotalPlayerScore = as.numeric(),
                          team1player3TotalPlayerScore = as.numeric(),
                          team1player4TotalPlayerScore = as.numeric(),
                          team1player5TotalPlayerScore = as.numeric(),
                          team2player1TotalPlayerScore = as.numeric(),
                          team2player2TotalPlayerScore = as.numeric(),
                          team2player3TotalPlayerScore = as.numeric(),
                          team2player4TotalPlayerScore = as.numeric(),
                          team2player5TotalPlayerScore = as.numeric(),
                          team1player1TotalScoreRank = as.numeric(),
                          team1player2TotalScoreRank = as.numeric(),
                          team1player3TotalScoreRank = as.numeric(),
                          team1player4TotalScoreRank = as.numeric(),
                          team1player5TotalScoreRank = as.numeric(),
                          team2player1TotalScoreRank = as.numeric(),
                          team2player2TotalScoreRank = as.numeric(),
                          team2player3TotalScoreRank = as.numeric(),
                          team2player4TotalScoreRank = as.numeric(),
                          team2player5TotalScoreRank = as.numeric(),
                          stringsAsFactors = FALSE)

## Load game data from each game
for (i in 1:1386) {
  
  ## Generic game data
  allGameData[i, "gameHash"] <- names(fullGameList)[i]
  allGameData[i, "gameId"] <- fullGameList[[i]][["gameId"]]
  allGameData[i, "platformId"] <- fullGameList[[i]][["platformId"]]
  allGameData[i, "gameCreation"] <- fullGameList[[i]][["gameCreation"]]
  allGameData[i, "gameDuration"] <- fullGameList[[i]][["gameDuration"]]
  allGameData[i, "queueId"] <- fullGameList[[i]][["queueId"]] ## Unnecesary
  allGameData[i, "mapId"] <- fullGameList[[i]][["mapId"]] ## likely not necesary
  allGameData[i, "seasonId"] <- fullGameList[[i]][["seasonId"]] ## likely not Unnecesary
  allGameData[i, "gameVersion"] <- fullGameList[[i]][["gameVersion"]] ## Not sure how to handle yet
  allGameData[i, "gameMode"] <- fullGameList[[i]][["gameMode"]] ## Unnecesary
  allGameData[i, "gameType"] <- fullGameList[[i]][["gameType"]] ## Unnecesary
  
  ## Team 1 data
  allGameData[i, "t1Win"] <- fullGameList[[i]][["teams"]][["win"]][[1]]
  allGameData[i, "t1FirstBlood"] <- fullGameList[[i]][["teams"]][["firstBlood"]][[1]]
  allGameData[i, "t1FirstTower"] <- fullGameList[[i]][["teams"]][["firstTower"]][[1]]
  allGameData[i, "t1FirstInhibitor"] <- fullGameList[[i]][["teams"]][["firstInhibitor"]][[1]]
  allGameData[i, "t1FirstBaron"] <- fullGameList[[i]][["teams"]][["firstBaron"]][[1]]
  allGameData[i, "t1FirstDragon"] <- fullGameList[[i]][["teams"]][["firstDragon"]][[1]]
  allGameData[i, "t1TowerKills"] <- fullGameList[[i]][["teams"]][["towerKills"]][[1]]
  allGameData[i, "t1InhibitorKills"] <- fullGameList[[i]][["teams"]][["inhibitorKills"]][[1]]
  allGameData[i, "t1BaronKills"] <- fullGameList[[i]][["teams"]][["baronKills"]][[1]]
  allGameData[i, "t1DragonKills"] <- fullGameList[[i]][["teams"]][["dragonKills"]][[1]]
  allGameData[i, "t1VilemawKills"] <- fullGameList[[i]][["teams"]][["vilemawKills"]][[1]]
  allGameData[i, "t1DominionVictoryScore"] <- fullGameList[[i]][["teams"]][["dominionVictoryScore"]][[1]]
  
  ## Test for Rift Herald
  if ("firstRiftHerald" %in% names(fullGameList[[i]][["teams"]])) {
    allGameData[i, "t1FirstRiftHerald"] <- fullGameList[[i]][["teams"]][["firstRiftHerald"]][[1]]
  } else {
    allGameData[i, "t1FirstRiftHerald"] <- NA
  }
  
  if ("riftHeraldKills" %in% names(fullGameList[[i]][["teams"]])) {
    allGameData[i, "t1RiftHeraldKills"] <- fullGameList[[i]][["teams"]][["riftHeraldKills"]][[1]]
  } else {
    allGameData[i, "t1RiftHeraldKills"] <- NA
  }
  
  ## Team 2 data
  allGameData[i, "t2Win"] <- fullGameList[[i]][["teams"]][["win"]][[2]]
  allGameData[i, "t2FirstBlood"] <- fullGameList[[i]][["teams"]][["firstBlood"]][[2]]
  allGameData[i, "t2FirstTower"] <- fullGameList[[i]][["teams"]][["firstTower"]][[2]]
  allGameData[i, "t2FirstInhibitor"] <- fullGameList[[i]][["teams"]][["firstInhibitor"]][[2]]
  allGameData[i, "t2FirstBaron"] <- fullGameList[[i]][["teams"]][["firstBaron"]][[2]]
  allGameData[i, "t2FirstDragon"] <- fullGameList[[i]][["teams"]][["firstDragon"]][[2]]
  allGameData[i, "t2TowerKills"] <- fullGameList[[i]][["teams"]][["towerKills"]][[2]]
  allGameData[i, "t2InhibitorKills"] <- fullGameList[[i]][["teams"]][["inhibitorKills"]][[2]]
  allGameData[i, "t2BaronKills"] <- fullGameList[[i]][["teams"]][["baronKills"]][[2]]
  allGameData[i, "t2DragonKills"] <- fullGameList[[i]][["teams"]][["dragonKills"]][[2]]
  allGameData[i, "t2VilemawKills"] <- fullGameList[[i]][["teams"]][["vilemawKills"]][[2]]
  allGameData[i, "t2DominionVictoryScore"] <- fullGameList[[i]][["teams"]][["dominionVictoryScore"]][[2]]
  
  ## Test for Rift Herald
  if ("firstRiftHerald" %in% names(fullGameList[[i]][["teams"]])) {
    allGameData[i, "t2FirstRiftHerald"] <- fullGameList[[i]][["teams"]][["firstRiftHerald"]][[2]]
  } else {
    allGameData[i, "t2FirstRiftHerald"] <- NA
  }
  
  if ("riftHeraldKills" %in% names(fullGameList[[i]][["teams"]])) {
    allGameData[i, "t2RiftHeraldKills"] <- fullGameList[[i]][["teams"]][["riftHeraldKills"]][[2]]
  } else {
    allGameData[i, "t2RiftHeraldKills"] <- NA
  }

  ## Load player names
  allGameData[i, "team1player1Name"] <- fullGameList[[i]][["participantIdentities"]][["player"]][[1]][[1]]
  allGameData[i, "team1player2Name"] <- fullGameList[[i]][["participantIdentities"]][["player"]][[1]][[2]]
  allGameData[i, "team1player3Name"] <- fullGameList[[i]][["participantIdentities"]][["player"]][[1]][[3]]
  allGameData[i, "team1player4Name"] <- fullGameList[[i]][["participantIdentities"]][["player"]][[1]][[4]]
  allGameData[i, "team1player5Name"] <- fullGameList[[i]][["participantIdentities"]][["player"]][[1]][[5]]
  allGameData[i, "team2player1Name"] <- fullGameList[[i]][["participantIdentities"]][["player"]][[1]][[6]]
  allGameData[i, "team2player2Name"] <- fullGameList[[i]][["participantIdentities"]][["player"]][[1]][[7]]
  allGameData[i, "team2player3Name"] <- fullGameList[[i]][["participantIdentities"]][["player"]][[1]][[8]]
  allGameData[i, "team2player4Name"] <- fullGameList[[i]][["participantIdentities"]][["player"]][[1]][[9]]
  allGameData[i, "team2player5Name"] <- fullGameList[[i]][["participantIdentities"]][["player"]][[1]][[10]]
  
  ## Load player champion
  extractPlayerData(num = i,
                    name = "Champion",
                    firstLink = "participants",
                    secondLink = "championId")
  
  ## Load player spells
  extractPlayerData(num = i,
                    name = "Spell1",
                    firstLink = "participants",
                    secondLink = "spell1Id")
  
  extractPlayerData(num = i,
                    name = "Spell2",
                    firstLink = "participants",
                    secondLink = "spell2Id")

  ## Load masteries and runes
  allGameData[[i, "team1player1Masteries"]] <- fullGameList[[i]][["participants"]][["masteries"]][[1]]
  allGameData[[i, "team1player2Masteries"]] <- fullGameList[[i]][["participants"]][["masteries"]][[2]]
  allGameData[[i, "team1player3Masteries"]] <- fullGameList[[i]][["participants"]][["masteries"]][[3]]
  allGameData[[i, "team1player4Masteries"]] <- fullGameList[[i]][["participants"]][["masteries"]][[4]]
  allGameData[[i, "team1player5Masteries"]] <- fullGameList[[i]][["participants"]][["masteries"]][[5]]
  allGameData[[i, "team2player1Masteries"]] <- fullGameList[[i]][["participants"]][["masteries"]][[6]]
  allGameData[[i, "team2player2Masteries"]] <- fullGameList[[i]][["participants"]][["masteries"]][[7]]
  allGameData[[i, "team2player3Masteries"]] <- fullGameList[[i]][["participants"]][["masteries"]][[8]]
  allGameData[[i, "team2player4Masteries"]] <- fullGameList[[i]][["participants"]][["masteries"]][[9]]
  allGameData[[i, "team2player5Masteries"]] <- fullGameList[[i]][["participants"]][["masteries"]][[10]]
  
  allGameData[[i, "team1player1Runes"]] <- fullGameList[[i]][["participants"]][["runes"]][[1]]
  allGameData[[i, "team1player2Runes"]] <- fullGameList[[i]][["participants"]][["runes"]][[2]]
  allGameData[[i, "team1player3Runes"]] <- fullGameList[[i]][["participants"]][["runes"]][[3]]
  allGameData[[i, "team1player4Runes"]] <- fullGameList[[i]][["participants"]][["runes"]][[4]]
  allGameData[[i, "team1player5Runes"]] <- fullGameList[[i]][["participants"]][["runes"]][[5]]
  allGameData[[i, "team2player1Runes"]] <- fullGameList[[i]][["participants"]][["runes"]][[6]]
  allGameData[[i, "team2player2Runes"]] <- fullGameList[[i]][["participants"]][["runes"]][[7]]
  allGameData[[i, "team2player3Runes"]] <- fullGameList[[i]][["participants"]][["runes"]][[8]]
  allGameData[[i, "team2player4Runes"]] <- fullGameList[[i]][["participants"]][["runes"]][[9]]
  allGameData[[i, "team2player5Runes"]] <- fullGameList[[i]][["participants"]][["runes"]][[10]]
  
  ## Load player items
  extractPlayerData(num = i,
                    name = "Item0",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "item0")
  
  extractPlayerData(num = i,
                    name = "Item1",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "item1")
  
  extractPlayerData(num = i,
                    name = "Item2",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "item2")
  
  extractPlayerData(num = i,
                    name = "Item3",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "item3")
  
  extractPlayerData(num = i,
                    name = "Item4",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "item4")
  
  extractPlayerData(num = i,
                    name = "Item5",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "item5")
  
  extractPlayerData(num = i,
                    name = "Item6",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "item6")
  
  ## Load KDAs
  extractPlayerData(num = i,
                    name = "Kills",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "kills")
  
  extractPlayerData(num = i,
                    name = "Deaths",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "deaths")
  
  extractPlayerData(num = i,
                    name = "Assists",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "assists")
  
  ## Load sprees
  extractPlayerData(num = i,
                    name = "LargestKillingSpree",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "largestKillingSpree")
  
  extractPlayerData(num = i,
                    name = "LargestMultiKill",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "largestMultiKill")
  
  extractPlayerData(num = i,
                    name = "KillingSprees",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "killingSprees")
  
  extractPlayerData(num = i,
                    name = "LongestTimeSpentLiving",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "longestTimeSpentLiving")
  
  ## Load multi kills
  extractPlayerData(num = i,
                    name = "DoubleKills",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "doubleKills")
  
  extractPlayerData(num = i,
                    name = "TripleKills",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "tripleKills")
  
  extractPlayerData(num = i,
                    name = "QuadraKills",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "quadraKills")
  
  extractPlayerData(num = i,
                    name = "PentaKills",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "pentaKills")
  
  extractPlayerData(num = i,
                    name = "UnrealKills",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "unrealKills")
  
  ## Load damage data
  extractPlayerData(num = i,
                    name = "TotalDamageDealt",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "totalDamageDealt")
  
  extractPlayerData(num = i,
                    name = "MagicDamageDealt",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "magicDamageDealt")
  
  extractPlayerData(num = i,
                    name = "PhysicalDamageDealt",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "physicalDamageDealt")
  
  extractPlayerData(num = i,
                    name = "TrueDamageDealt",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "trueDamageDealt")
  
  extractPlayerData(num = i,
                    name = "LargestCriticalStrike",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "largestCriticalStrike")
  
  extractPlayerData(num = i,
                    name = "TotalDamageDealtToChampions",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "totalDamageDealtToChampions")
  
  extractPlayerData(num = i,
                    name = "MagicDamageDealtToChampions",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "magicDamageDealtToChampions")
  
  extractPlayerData(num = i,
                    name = "PhysicalDamageDealtToChampions",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "physicalDamageDealtToChampions")
  
  extractPlayerData(num = i,
                    name = "TrueDamageDealtToChampions",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "trueDamageDealtToChampions")
  
  ## Load Heal data
  extractPlayerData(num = i,
                    name = "TotalHeal",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "totalHeal")
  
  extractPlayerData(num = i,
                    name = "TotalUnitsHealed",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "totalUnitsHealed")
  
  ## Load damage taken data
  extractPlayerData(num = i,
                    name = "TotalDamageTaken",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "totalDamageTaken")
  
  extractPlayerData(num = i,
                    name = "MagicalDamageTaken",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "magicalDamageTaken")
  
  extractPlayerData(num = i,
                    name = "PhysicalDamageTaken",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "physicalDamageTaken")
  
  extractPlayerData(num = i,
                    name = "TrueDamageTaken",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "trueDamageTaken")
  
  ## Load gold data
  extractPlayerData(num = i,
                    name = "GoldEarned",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "goldEarned")
  
  extractPlayerData(num = i,
                    name = "GoldSpent",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "goldSpent")
  
  ## Load turret/inhibitor kills
  extractPlayerData(num = i,
                    name = "TurretKills",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "turretKills")
  
  extractPlayerData(num = i,
                    name = "InhibitorKills",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "inhibitorKills")
  
  ## Load minions killed data
  extractPlayerData(num = i,
                    name = "TotalMinionsKilled",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "totalMinionsKilled")
  
  extractPlayerData(num = i,
                    name = "NeutralMinionsKilled",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "neutralMinionsKilled")
  
  extractPlayerData(num = i,
                    name = "NeutralMinionsKilledTeamJungle",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "neutralMinionsKilledTeamJungle")
  
  extractPlayerData(num = i,
                    name = "NeutralMinionsKilledEnemyJungle",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "neutralMinionsKilledEnemyJungle")
  
  extractPlayerData(num = i,
                    name = "TotalTimeCrowdControlDealt",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "totalTimeCrowdControlDealt")
  
  ## Load champ level and ward data
  extractPlayerData(num = i,
                    name = "ChampLevel",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "champLevel")
  
  extractPlayerData(num = i,
                    name = "VisionWardsBoughtInGame",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "visionWardsBoughtInGame")
  
  extractPlayerData(num = i,
                    name = "SightWardsBoughtInGame",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "sightWardsBoughtInGame")
  
  extractPlayerData(num = i,
                    name = "WardsPlaced",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "wardsPlaced")
  
  extractPlayerData(num = i,
                    name = "WardsKilled",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "wardsKilled")
  
  ## Load first kill/assists
  extractPlayerData(num = i,
                    name = "FirstBloodKill",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "firstBloodKill")
  
  extractPlayerData(num = i,
                    name = "FirstBloodAssist",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "firstBloodAssist")
 
  extractPlayerData(num = i,
                    name = "FirstTowerKill",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "firstTowerKill")
  
  extractPlayerData(num = i,
                    name = "FirstTowerAssist",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "firstTowerAssist")
  
  ## test for first inhibitor stats
  if ("firstInhibitorKill" %in% names(fullGameList[[i]][["participants"]][["stats"]])) {
    extractPlayerData(num = i,
                      name = "FirstInhibitorKill",
                      firstLink = "participants",
                      secondLink = "stats",
                      thirdLink = "firstInhibitorKill")
    
    extractPlayerData(num = i,
                      name = "FirstInhibitorAssist",
                      firstLink = "participants",
                      secondLink = "stats",
                      thirdLink = "firstInhibitorAssist")
    
  } else {
    allGameData[i, "team1player1FirstInhibitorKill"] <- NA
    allGameData[i, "team1player2FirstInhibitorKill"] <- NA
    allGameData[i, "team1player3FirstInhibitorKill"] <- NA
    allGameData[i, "team1player4FirstInhibitorKill"] <- NA
    allGameData[i, "team1player5FirstInhibitorKill"] <- NA
    allGameData[i, "team2player1FirstInhibitorKill"] <- NA
    allGameData[i, "team2player2FirstInhibitorKill"] <- NA
    allGameData[i, "team2player3FirstInhibitorKill"] <- NA
    allGameData[i, "team2player4FirstInhibitorKill"] <- NA
    allGameData[i, "team2player5FirstInhibitorKill"] <- NA
    allGameData[i, "team1player1FirstInhibitorAssist"] <- NA
    allGameData[i, "team1player2FirstInhibitorAssist"] <- NA
    allGameData[i, "team1player3FirstInhibitorAssist"] <- NA
    allGameData[i, "team1player4FirstInhibitorAssist"] <- NA
    allGameData[i, "team1player5FirstInhibitorAssist"] <- NA
    allGameData[i, "team2player1FirstInhibitorAssist"] <- NA
    allGameData[i, "team2player2FirstInhibitorAssist"] <- NA
    allGameData[i, "team2player3FirstInhibitorAssist"] <- NA
    allGameData[i, "team2player4FirstInhibitorAssist"] <- NA
    allGameData[i, "team2player5FirstInhibitorAssist"] <- NA
  }
  
  ## Load player scores
  extractPlayerData(num = i,
                    name = "CombatPlayerScore",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "combatPlayerScore")
  
  extractPlayerData(num = i,
                    name = "ObjectivePlayerScore",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "objectivePlayerScore")
  
  extractPlayerData(num = i,
                    name = "TotalPlayerScore",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "totalPlayerScore")

  extractPlayerData(num = i,
                    name = "TotalScoreRank",
                    firstLink = "participants",
                    secondLink = "stats",
                    thirdLink = "totalScoreRank")
  
  print(round((i/length(fullGameList))*100,1))  
}

## Save partially processed game data
save(allGameData, file="data/allGameData.Rda")
