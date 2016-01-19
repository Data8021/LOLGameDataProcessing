glm.fit = glm(t1Win ~ t1FirstBlood+t1FirstTower+t1FirstInhibitor+t1FirstDragon+
                t1FirstBaron+diffDragonKills+diffBaronKills,
              data=allGameData, family=binomial)
summary(glm.fit)

glm.fit2 = glm(t1Win ~ t1FirstBlood+t1FirstTower+t1FirstDragon+
                t1FirstBaron+diffDragonKills+diffBaronKills,
              data=allGameData, family=binomial)
summary(glm.fit2)

glm.fit3 = glm(t1Win ~ t1FirstTower+t1FirstDragon,
               data=allGameData, family=binomial)
summary(glm.fit3)