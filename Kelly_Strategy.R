#Kelly strategy
library(sqldf)
library(reshape2)

#View(predicted.values)


query = "
SELECT
A.HomeTeam,
A.AwayTeam,
B.B365H,
B.B365D,
B.B365A,
A.Hodds,
A.Dodds,
A.Aodds,
B.FTR
FROM
[predicted.values] A
LEFT JOIN
datasub B
ON
A.HomeTeam = B.HomeTeam and
A.AwayTeam = B.AwayTeam

"
df = sqldf(query)


#Input from CSV
df = read.csv(file,choose(),header = T)


#Identify value bets
#True/False
df$ValueHome = (df$B365H - df$Hodds)>0
df$ValueDraw = (df$B365D - df$Dodds)>0
df$ValueAway = (df$B365A - df$Aodds)>0

#Take games where I have found some value
bets = df[df$ValueHome!=FALSE|df$ValueDraw!=FALSE|df$ValueAway!=FALSE,]

#Calculate % of bank to stake
bets$KellyHome = 
  round(((bets$B365H-1)*(bets$Hodds^-1) - (1 - bets$Hodds^-1))/(bets$B365H-1),2)
bets$KellyDraw = 
  round(((bets$B365D-1)*(bets$Dodds^-1) - (1 - bets$Dodds^-1))/(bets$B365D-1),2)
bets$KellyAway = 
  round(((bets$B365A-1)*(bets$Aodds^-1) - (1 - bets$Aodds^-1))/(bets$B365A-1),2)


#Experiment... limit max stake to 15%
bets$KellyHome[bets$KellyHome>0.15] = 0.15
bets$KellyAway[bets$KellyAway>0.15] = 0.15
bets$KellyDraw[bets$KellyDraw>0.15] = 0.15



#View(bets)





bals = 1000
bank = 1000

for (i in 1:nrow(bets)){
  
  
  print(i)
  #cat(bets$HomeTeam[i],bets$AwayTeam[i])
  
  #Home Bets
  if (bets$KellyHome[i]>0){
    if (bets$FTR[i] == "H"){
      cat("Bet placed was on Home in game between ",bets$HomeTeam[i]," and ",
          bets$AwayTeam[i],". ",bank*bets$KellyHome[i]," at odds of ",bets$B365H[i],". New balance is ",bank + (bets$B365H[i] - 1)*bank*bets$KellyHome[i],"\n\n")
      bank = bank + (bets$B365H[i] - 1)*bank*bets$KellyHome[i]
      
    print(bank)
      }
    if (bets$FTR[i] != "H"){
      cat("Bet on Home,",bank*bets$KellyHome[i]," lost. New balance ",bank - bank*bets$KellyHome[i],"\n\n")
      bank = bank - bank*bets$KellyHome[i]
      
    print(bank)
      }
  }
  
  #Away Bets
  if (bets$KellyAway[i]>0){
    if (bets$FTR[i] == "A"){
      cat("Bet placed was on Away in game between",bets$HomeTeam[i]," and ",
          bets$AwayTeam[i],". ",bank*bets$KellyAway[i]," at odds of ",bets$B365A[i],". New balance is ",bank + (bets$B365A[i] - 1)*bank*bets$KellyAway[i],"\n\n")
      bank = bank + (bets$B365A[i] - 1)*bank*bets$KellyAway[i]
      
    print(bank)
      }
    if (bets$FTR[i] != "A"){
      cat("Bet on Away,",(bank)*(bets$KellyAway[i])," lost. New balance ",bank - bank*bets$KellyAway[i],"\n\n") 
      bank = bank - bank*bets$KellyAway[i]
       
    print(bank)
       }
  }
  
  #Draw Bets
  if (bets$KellyDraw[i]>0){
    if (bets$FTR[i] == "D"){
      cat("Bet placed was Draw in game between",bets$HomeTeam[i]," and ",
          bets$AwayTeam[i],". ",bank*bets$KellyDraw[i]," at odds of ",bets$B365D[i],". New balance is ",bank + (bets$B365D[i] - 1)*bank*bets$KellyDraw[i],"\n\n")
      bank = bank + (bets$B365D[i] - 1)*bank*bets$KellyDraw[i]
      print(bank)
      }
    if (bets$FTR[i] != "D"){
      cat("Bet on Draw,",bank*bets$KellyDraw[i]," lost. New balance ",bank - bank*bets$KellyDraw[i],"\n\n")
      bank = bank - bank*bets$KellyDraw[i]
      
    print(bank)
      }
  }
  
  
  bals = c(bals,bank)
}


#write.csv(df,"C:\\Users\\ANDREW\\Desktop\\input.CSV")
#bals = cbind(1:length(bals),bals)



#bets$Balance = bals


#View(bets[,c(1,2,9)])













