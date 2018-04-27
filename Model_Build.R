
library(sqldf)

summary(df)
#Build model on first half of season



datasub = df
  #df[1:190,]

query = "
SELECT
A.HomeTeam,
[Home xG],
[Away xG],
A.AwayTeam,
AvHomeShots,
AvAwayShots,
[AvHomeShotsConc],
[AvAwayShotsConc],
HomexGR,
AwayxGR,
HomexGPerShotHome,
AwayxGPerShotAway
FROM
datasub A

LEFT JOIN --HOME SHOTS
(
SELECT 
HomeTeam,
AVG(HS) as [AvHomeShots]
FROM
datasub
GROUP BY HomeTeam
) B
ON
A.[HomeTeam] = B.[HomeTeam]


LEFT JOIN --AWAY SHOTS
(
SELECT 
AwayTeam,
AVG([AS]) as [AvAwayShots]
FROM
datasub
GROUP BY AwayTeam
) C
ON
A.[AwayTeam] = C.[AwayTeam]

LEFT JOIN -- HOME SHOTS CONC
(
SELECT 
HomeTeam,
AVG([AS]) as [AvHomeShotsConc]
FROM
datasub
GROUP BY HomeTeam
) D
ON
A.[HomeTeam] = D.[HomeTeam]

LEFT JOIN -- AWAY SHOTS CONC
(
SELECT 
AwayTeam,
AVG([HS]) as [AvAwayShotsConc]
FROM
datasub
GROUP BY AwayTeam
) E
ON
A.[AwayTeam] = E.[AwayTeam]

LEFT JOIN -- HOME TEAM HOME XGR
(
SELECT
HomeTeam,
SUM([Home xG])/(SUM([Home xG])+SUM([Away xG])) as HomexGR
FROM
datasub
GROUP BY HomeTeam
) F
ON
A.[HomeTeam] = F.[HomeTeam]

LEFT JOIN -- AWAY TEAM AWAY XGR
(
SELECT
AwayTeam,
SUM([Away xG])/(SUM([Away xG])+SUM([Home xG])) as AwayxGR
FROM
datasub
GROUP BY AwayTeam
) G
ON
A.[AwayTeam] = G.[AwayTeam]

LEFT JOIN -- HOME TEAM xGPerShotHome
(
SELECT
HomeTeam,
SUM([Home xG])/SUM([HS]) as HomexGPerShotHome
FROM
datasub
GROUP BY HomeTeam
) H
ON
A.[HomeTeam] = H.[HomeTeam]

LEFT JOIN -- AWAY TEAM xGPerShotAway
(
SELECT
AwayTeam,
SUM([Away xG])/SUM([AS]) as AwayxGPerShotAway
FROM
datasub
GROUP BY AwayTeam
) I
ON 
A.[AwayTeam] = I.[AwayTeam]




"

training = sqldf(query)



# Build Model -------------------------------------------------------------

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












