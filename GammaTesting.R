# Test for data following the Gammma distribution
library(ggplot2)

# Data used can be found in FootballData repository

df1 = read.csv(file.choose()) # 1617
df2 = read.csv(file.choose()) # 1718

df = rbind(df1,df2)

head(df)

xG = c(df$Home.xG,df$Away.xG)

hist(xG,col = 'grey',main = 'Hist. of xG',xlab = 'xG',ylab = 'Count')


r = mean(xG)/var(xG)
s = r*mean(xG)
ks.test(xG,"pgamma",shape = s,rate = r)
# Test statistic of 0.026 - the greatest verticle distance between the empirical distribution and the theoretical gamma distribution



cdfgamma = ecdf(rgamma(1000,shape = s,rate = r))

x = cbind(cdfgamma,rep(1,length(cdfgamma)))
x = data.frame(rbind(x,cbind(xG,rep(2,length(xG)))))

plot(ecdf(xG),col = 'blue',lwd = 0.5,main = "Empirical CDF vs CDF of 1000 draws from Gamma distribution",xlab = 'Value',ylab = 'Percentile')
plot(cdfgamma,add = T,col = 'maroon',lwd = 5)


# Test H and A xG on their own

ks.test(df$Home.xG,"pgamma",shape = mean(df$Home.xG)^2/var(df$Home.xG), rate = mean(df$Home.xG)/var(df$Home.xG))
# p value 0.43

ks.test(df$Away.xG,"pgamma",shape = mean(df$Away.xG)^2/var(df$Away.xG), rate = mean(df$Away.xG)/var(df$Away.xG))

# p value 0.95

