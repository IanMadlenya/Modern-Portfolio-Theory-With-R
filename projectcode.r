#######project1 is for Exercise A#######
#######test is for Exercise B#######

####Load up data
library(stockPortfolio)
ticker <- c("ATVI", "TTWO", "WMT", "KNM", "NTDOY", "EXPO", "JCP", "TW", "HURN", "KSS", "JNJ", "TJX", "NVS", "RMTI", "GSK", "AEL", "PRU", "SLF", "MFC", "MET", "F", "HMC", "ZAAP", "DDS", "TTM", "^GSPC")

project1 <- getReturns(ticker, start='2005-12-31', end = '2010-12-31')
####3 month Risk-free rate & 6 month Risk-free rate
Rf1 = 0.0004 
Rf2 = 0.0008
####

#Exercise A 
#1
cmarko <- stockModel(project1, Rf = Rf1, model = "none", drop=26)
opcmarko <- optimalPort(cmarko)
opcmarko
plot(opcmarko, xlim=c(0,0.5), ylim=c(-0.01,0.16), main="Efficiency Frontier using Markowitz Model")
slope <- (opcmarko$risk-Rf1)/opcmarko$risk
segments(0,Rf1,2*opcmarko$risk,Rf1+slope*2*opcmarko$R)
points(opcmarko$risk, opcmarko$R, pch=19, col="green") #Point of tangency
text(opcmarko$risk+0.002, opcmarko$R-0.0005, "Point of Tangency", cex = 0.5)
opcmarko$R #Expected return at point
opcmarko$risk #risk at point

#2
portPossCurve(cmarko,add=TRUE, riskRange = 9)

#3 equal allocation
x3 = rep(1/25, times = 25)
rbar3 =  cmarko$R %*% x3
sd3 = (t(x3) %*% cmarko$COV %*% x3)^0.5
points(sd3, rbar3)
text(sd3+0.002, rbar3-0.0005, "Equal Allocation", cex = 0.5)

#4 assume SIM holds, Rf borrowing & lending exists, use excess return to beta ratio
csim <- stockModel(project1, model = "SIM", Rf= Rf1, index=26, shortSelling = FALSE)
opcsim <- optimalPort(csim, Rf = Rf1, shortSell = FALSE)
opcsim$X
opcsim$R
opcsim$risk
opcsimA <- csim$alpha %*% opcsim$X
opcsimB <- csim$beta %*% opcsim$X
points(opcsim$risk, opcsim$R)
text(opcsim$risk+0.002, opcsim$R-0.0005, "SIM without short sales", cex = 0.5)

csimss <- stockModel(project1, Rf = Rf1, model = "SIM", index=26, shortSelling = TRUE)
opcsimss <- optimalPort(csimss, Rf=Rf1, shortSell = TRUE)
opcsimss$X
opcsimss$R
opcsimss$risk
opcsimssA <- csimss$alpha %*% opcsimss$X
opcsimssB <- csimss$beta %*% opcsimss$X
points(opcsimss$risk, opcsimss$R)
text(opcsimss$risk+0.002, opcsimss$R-0.0005, "SIM with short sales", cex = 0.5)

#5 constant correlation model
ccm <- stockModel(project1, model = "CCM", Rf= Rf1, shortSelling = FALSE)
opccm <- optimalPort(ccm, Rf = Rf1, shortSell = FALSE)
opccm$X
opccm$R
opccm$risk
points(opccm$risk, opccm$R)
text(opccm$risk+0.002, opccm$R-0.0005, "CCM without short sales", cex = 0.5)

ccmss <- stockModel(project1, model = "CCM", Rf= Rf1, shortSelling = TRUE)
opccmss <- optimalPort(ccmss, Rf = Rf1, shortSell = TRUE)
opccmss$X
opccmss$R
opccmss$risk
points(opccmss$risk, opccmss$R)
text(opccmss$risk+0.002, opccmss$R-0.0005, "CCM with short sales", cex = 0.5)

#6 multigroup model
ind <- c('Multimedia & Graphics Software','Multimedia & Graphics Software','Department Stores','Multimedia & Graphics Software','Multimedia & Graphics Software', 'Management Services','Department Stores','Management Services','Management Services','Department Stores','Drug Manufacturers','Department Stores','Drug Manufacturers','Drug Manufacturers','Drug Manufacturers','Life Insurance','Life Insurance','Life Insurance','Life Insurance','Life Insurance','Auto Manufacturers','Auto Manufacturers','Auto Manufacturers','Department Stores','Auto Manufacturers','Index')
data <- as.data.frame(cbind(ticker, ind))
ticker <- data$ticker
ind <- data$ind

cmgmss <- stockModel(project1, model = "MGM", Rf= Rf1, industry = ind, drop=26)
opcmgmss <- optimalPort(cmgmss, Rf = Rf1)
opcmgmss$X
opcmgmss$R
opcmgmss$risk
points(opcmgmss$risk, opcmgmss$R)
text(opcmgmss$risk+0.002, opcmgmss$R-0.0005, "MGM with short sales", cex = 0.5)

#7 Graph
plot(opcmarko, xlim=c(0,0.5), ylim=c(-0.01,0.16), main="Risk and Returns of Portfolios")
slope <- (opcmarko$risk-Rf1)/opcmarko$risk
segments(0,Rf1,2*opcmarko$risk,Rf1+slope*2*opcmarko$R)
points(opcmarko$risk, opcmarko$R, pch=19, col="green") #Point of tangency
text(opcmarko$risk+0.002, opcmarko$R-0.0005, "Point of Tangency", cex = 0.5)
points(sd3, rbar3)
text(sd3+0.002, rbar3-0.0005, "Equal Allocation", cex = 0.5)
points(opcsim$risk, opcsim$R)
text(opcsim$risk+0.002, opcsim$R-0.0005, "SIM without short sales", cex = 0.5)
points(opcsimss$risk, opcsimss$R)
text(opcsimss$risk+0.002, opcsimss$R-0.0005, "SIM with short sales", cex = 0.5)
points(opccmss$risk, opccmss$R)
text(opccmss$risk+0.002, opccmss$R-0.0005, "CCM with short sales", cex = 0.5)
points(opcmgmss$risk, opcmgmss$R)
text(opcmgmss$risk+0.002, opcmgmss$R-0.0005, "MGM with short sales", cex = 0.5)

#Exercise B
#Test Period
#504a50riskfree
opcsim50 <- opcsim
opcsim50$X <- opcsim50$X/2
opcsim50$X[26] <- as.numeric(0.5)
opcsim50$R <- (opcsim50$R)/2+(0.5*Rf1)
opcsim50$risk <- (opcsim50$risk)/2

test <- getReturns(ticker, start='2010-12-31', end = '2013-03-31')
test1 <- test
test1$R <- cbind(test1$R, rep(Rf1, times=27))

tpEqu <- testPort(test$R[,-26], X=rep(1/25,25))
tpopsim1 <- testPort(test, opcsim)
tpopsim50<- testPort(test1, opcsim50)
tpopccm1 <- testPort(test, opccm)
tpopmgm1 <- testPort(test, opcmgmss)

#Fixing tpopsim50
tpopsim50$sumRet[26] <- 1

#Generate the time plots:
plot(cumprod(1+rev(test$R[,26])),ylim=c(0,2.5),ylab = "Returns", lty=1, col="pink", type="l", main="Portfolio Performance from 2010-12-31 to 2013-3-31")
lines(tpEqu, lty=2, col="black")
lines(tpopsim1, lty=3, col="green")
lines(tpopsim50, lty=4, col="red")
lines(tpopccm1, lty=5, col="yellow")
lines(tpopmgm1, lty=6, col="grey")

#Add a legend:
legend('topleft', lty=1:6, c('Market', 'Equal Allocation', 'SIM NO SS', '50 SIM 50 RF', 'CCM NO SS', 'MGM_SS'), col=c("pink", "black", "green", "red", "yellow", "grey"))


#RESULTS AND DISCUSSION

opcmarko$X
opcmarko$R
opcmarko$risk

x3
rbar3
sd3

opcsim$X
opcsim$R
opcsim$risk
opcsimA
opcsimB

opcsimss$X
opcsimss$R
opcsimss$risk
opcsimssA
opcsimssB

opccm$X
opccm$R
opccm$risk

opccmss$X
opccmss$R
opccmss$risk

opcmgmss$X
opcmgmss$R
opcmgmss$risk

