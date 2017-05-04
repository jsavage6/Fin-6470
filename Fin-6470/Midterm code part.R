install.packages("urca")
library(urca)

ln.spotd1 = log(dairy$DL.Nearest.Settlement)
ln.spotd2 = log(dairy$BJ.Nearest.Settlement)
ln.spotd3 = diff(ln.spotd1)
ln.spotd4 = diff(ln.spotd2)
spotd1 = dairy$DL.Nearest.Settlement
spot2 = dairy$BJ.Nearest.Settlement
spot3 = diff(spotd1)
spot4 = diff(spot2)

dickf.sdl <- ur.df(y=ln.spotd1, type = "drift", selectlags = "BIC")
summary(dickf.sdl)

dickf.sd2 <- ur.df(y=ln.spotd2, type = "drift", selectlags = "BIC")
summary(dickf.sd2)

dickf.sd3 <- ur.df(y=ln.spotd3, type = "drift", selectlags = "BIC")
summary(dickf.sd3)

dickf.sd4 <- ur.df(y=ln.spotd4, type = "drift", selectlags = "BIC")
summary(dickf.sd4)

dickf.sd5 <- ur.df(y=spotd1, type = "drift", selectlags = "BIC")
summary(dickf.sd5)

dickf.sd6 <- ur.df(y=spot2, type = "drift", selectlags = "BIC")
summary(dickf.sd6)

dickf.sd7 <- ur.df(y=spot3, type = "drift", selectlags = "BIC")
summary(dickf.sd7)

dickf.sd8 <- ur.df(y=spot4, type = "drift", selectlags = "BIC")
summary(dickf.sd8)

##plots

dlspotgraph = plot(spotd1, ylab = "DL Spot Price", xlab = "Time")
bjspotgraph = plot(spot2, ylab = "BJ Spot Price", xlab = "Time")
lnd1spotgraph = plot(ln.spotd1, ylab = "Log(DL) Spot Price", xlab = "Time")
lnbjspotgraph = plot(ln.spotd2, ylab = "Log(DL) Spot Price", xlab = "Time")
dldiffgraph = plot(spot3, ylab = "DL Spot Price Difference", xlab = "Time")
bjdiffgraph = plot(spot4, ylab = "BJ Spot Price Difference", xlab = "Time")
lndldiffgraph = plot(ln.spotd3, ylab = "Log(DL) Spot Price Difference", xlab = "Time")
lnbjdiffgraph = plot(ln.spotd3, ylab = "Log(DL) Spot Price Difference", xlab = "Time")

#problem D

vol.s = sd(ln.spotd1)
vol.f = sd(ln.spotd2)

rho = cor(ln.spotd1,ln.spotd2)

h.star = rho * (vol.s/vol.f)

fit <- lm(ln.spotd1 ~ ln.spotd2)
summary(fit)

##number 3

alpha.oil = 0.342
beta.oil = 0.539
sigma.oil = .11
S.oil.0 = 0.69
nreps = 45
basis.oil = -0.02
basis.gas = -0.01

ln.spot.oil <- rep(0, nreps)
ln.spot.oil[1] <- log(S.oil.0)
ln.basis.oil <- rep(0, nreps)
ln.basis.oil[1] <- log(basis.oil)

z1 <- rnorm(nreps)

for(t in 2:nreps)
{
  ln.spot.oil[t] <- ln.spot.oil[t-1] + alpha.oil * (beta.oil - exp(ln.spot.oil[t-1])) + z1[t] * sigma.oil
}

plot(ln.spot.oil, lwd=2 , type="l")

basis.oil[t] <- alpha.oil * basis.oil[t-1] + beta.oil * S.oil.0[t] + rnorm(1,0)
for(t in 2:nreps)
{
  basis.oil[t] <-  alpha.oil * basis.oil[t-1] + beta.oil * S.oil.0[t] + rnorm(1,0)
}

