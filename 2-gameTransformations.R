## 2nd Script - Run after fullGameData.R
library(dplyr)

## Convert select variables to factors
allGameData$t1Win = as.factor(allGameData$t1Win)
allGameData$t2Win = as.factor(allGameData$t2Win)
allGameData$queueId = as.factor(allGameData$queueId)
allGameData$mapId = as.factor(allGameData$mapId)
allGameData$seasonId = as.factor(allGameData$seasonId)
allGameData$gameVersion = as.factor(allGameData$gameVersion)
allGameData$gameMode = as.factor(allGameData$gameMode)
allGameData$gameType = as.factor(allGameData$gameType)

## Calculate difference between team 1 and team 2
allGameData = mutate(allGameData,
                     diffTowerKills = t1TowerKills - t2TowerKills,
                     diffInhibitorKills = t1InhibitorKills - t2InhibitorKills,
                     diffBaronKills = t1BaronKills - t2BaronKills,
                     diffDragonKills = t1DragonKills - t2DragonKills,
                     diffVilemawKills = t1VilemawKills - t2VilemawKills,
                     diffRiftHeraldKills = t1RiftHeraldKills - t2RiftHeraldKills,
                     diffDominionVictoryScore = t1DominionVictoryScore - t2DominionVictoryScore)

