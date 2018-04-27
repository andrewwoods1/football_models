# Data engineering

library(googlesheets)
library(sqldf)
library(dplyr)

gs_ls()
xG = gs_title("EPL 1718 XG")
xG <- xG %>%
  gs_read(ws = 1)

#View(xG)

results = gs_title("EPL 1718 DATA")
results <- results %>%
  gs_read(ws = 1)

#View(results)

#unique(xG$`Home Team`)
#unique(results$HomeTeam)

#Standardize team names
xG$`Home Team`[xG$`Home Team` == "West Bromwich Albion"] = "West Brom"
xG$`Away Team`[xG$`Away Team` == "West Bromwich Albion"] = "West Brom"

xG$`Home Team`[xG$`Home Team` == "Manchester City"] = "Man City"
xG$`Away Team`[xG$`Away Team` == "Manchester City"] = "Man City"

xG$`Home Team`[xG$`Home Team` == "Manchester United"] = "Man United"
xG$`Away Team`[xG$`Away Team` == "Manchester United"] = "Man United"

xG$`Home Team`[xG$`Home Team` == "Newcastle United"] = "Newcastle"
xG$`Away Team`[xG$`Away Team` == "Newcastle United"] = "Newcastle"

query = "

SELECT 
A.*,
B.*
FROM
xG A
LEFT JOIN results B
ON
A.[HOME TEAM] = B.[HomeTeam] AND
A.[AWAY TEAM] = B.[AwayTeam]

"

df = sqldf(query)



##################################









