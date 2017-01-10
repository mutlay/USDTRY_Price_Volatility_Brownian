# Mehaz: https://stoltzmaniac.com/price-volatility-basic-brownian-motion/

# We simulate USDTRY exchange to find probability distribution at the end of the month by the assumptions: 
# - Current exchange is 3.7 TL
# - Roughly 95% of the time, the price will be +/- $0.1 compared to the day before
# - the daily changes are normally distributed and volatility motion is Brownian.

set.seed(5)

initialPrice = 3.7
dailyPlusMinus = 0.1
dailyDeviation = dailyPlusMinus/2
# assumes 2*sigma roughly approximates 95% range on normal dist.

#Note: This isn't the proper way to calculate a standard deviation.
# We really have no information on how the +/- was calculated,
# so it's the best you can do.

n = 30 #number of days
N = 1000 #number of simulations

prices = matrix(ncol=N,nrow=n)
for(i in 1:N){
prices[,i] = initialPrice +
cumsum(rnorm(n = n,mean = 0,sd = dailyDeviation))
}

steps = 1:nrow(prices)
yLimits = c(initialPrice-dailyDeviation*n/1.5,
initialPrice+dailyDeviation*n/1.5)

png()

plot(steps,prices[,1],type='l',
ylim=yLimits,xlab='Days',
ylab='Daily Price (TL)',
main='Simulation of Daily Prices of USDTRY for 30 days')
for(i in 2:ncol(prices)){
lines(prices[,i])
}

# After plotting the data, you quickly notice that you still don't quite have the answer you're looking for, but you do have a great visualization. You quickly plot a histogram of the simulated end of month prices.

endOfMonthPrices = prices[n,]
hist(endOfMonthPrices,main='Histogram End of Month Price USDTRY',
     xlab='Price (TL)',30)

# You also visualize your data by looking at a CDF plot. 

plot(ecdf(endOfMonthPrices),
     main='CDF of End of Month Price USDTRY',
     xlab='Price (TL)')

dev.off()