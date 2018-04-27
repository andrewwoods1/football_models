
# Predict coming games

library(sqldf)


mH = glm(training$`Home xG` ~ HomexGR + 
           AwayxGR +
           AvHomeShots + 
           AvAwayShotsConc +  
           HomexGPerShotHome,
         family = Gamma(link = "log"),
         data = training)
summary(mH)



mA = glm(training$`Away xG` ~ HomexGR + 
           AwayxGR +
           AvAwayShots + 
           AvHomeShotsConc +  
           AwayxGPerShotAway,
         family = Gamma(link = "log"),
         data = training)
summary(mA)







QH = "SELECT
DISTINCT
[HomexGR],
[AvHomeShots],
[HomexGperShotHome],
[AvHomeShotsConc]
from [training]
where
[HomeTeam] = 'Everton'"

QA = "SELECT
DISTINCT
[AwayxGR],
[AvAwayShots],
[AwayxGPerShotAway],
[AvAwayShotsConc]
from [training]
where
[AwayTeam] = 'Man City'"


newHomeData = sqldf(QH)
newAwayData = sqldf(QA)
newData = cbind(newHomeData,newAwayData)


#Make predictiona
hxg = exp(predict(mH,newdata = newData))
axg = exp(predict(mA,newdata = newData))

res = bivpois.table(4,4,c(hxg,axg,0))

m = (diag(5)*.18)+1
#Adjust draw probs
resadj = m*res
res = resadj/sum(resadj)

prHomeWin = res[2,1]+res[3,1]+res[4,1]+res[5,1]+
  res[3,2]+res[4,2]+res[5,2]+
  res[4,3]+res[5,3]+
  res[5,4]

prDraw = sum(diag(res))

prAwayWin = 1 - prHomeWin - prDraw
odds = c(
prHomeWin^-1,
prAwayWin^-1,
prDraw^-1)
odds





