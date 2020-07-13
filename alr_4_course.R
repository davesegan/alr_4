# ----------------
# Chapter 1: Scatterplots and Regression
# ----------------

library(alr4)
# All of these work the same
plot(dheight ~ mheight, data = Heights)
plot(Heights$mheight, Heights$dheight)
with(Heights, plot(mheight, dheight))

# Select data
sel <- with(Heights,
            (57.5 < mheight) & (mheight <= 58.5) |
              (62.5 < mheight) & (mheight <= 63.5) |
              (67.5 < mheight) & (mheight <= 68.5))

plot(dheight ~ mheight, data = Heights, subset = sel)

# Opposite
plot(dheight ~ mheight, data = Heights, subset = !sel)

# Graphical parameters with par
oldpar <- par(mfrow = c(1,2))
# this mfrow keeps 1 row and two columns

# Lets now start out with our lm command to fit a linar model
# Practice 1
Forbes
summary(Forbes)

m0 <- lm(pres ~ bp, data = Forbes)
plot(pres ~ bp, data = Forbes, xlab = "Boiling Point (deg. F)",
     ylab = "Pressure (in Hg)")
abline(m0)
plot(residuals(m0) ~ bp, Forbes, xlab = "Boiling Point (deg. F)",
     ylab = "Residuals")
abline(a=0, b=0, lty=2) # lty=2 indicates dashed line
par(oldpar)

# Practice 2
wblake
summary(wblake)

meanLength <- with(wblake, tapply(Length, Age, mean))
plot(Age ~ Length, data = wblake)
abline(lm(Length ~ Age, data = wblake))
lines(1:8, meanLength, lty=2)

# Practice 3
turkey
summary(turkey)
?turkey

plot(Gain ~ A, turkey, xlab = "Amount (percent of diet)",
     ylab = "Weight gain (g)", pch=S, col=S)
legend("bottomright", inset=0.02, legend=c("1 Control", "2 New source A", "3 New source B"), cex=0.75, lty=1:3, pch=1:3, lwd=c(1, 1.5, 2))

# pch is the figure of the point, col is the color
# plot(1:20, pch=1:20, col=1:20)

# Summary graph
oldpar <- par(mfrow=c(2, 2))
xs <- names(anscombe)[1:4]
ys <- names(anscombe)[5:8]
for (i in 1:4){
  plot(anscombe[, xs[i]], anscombe[, ys[i]], xlab=xs[i], ylab=ys[i],
       xlim=c(4,20), ylim=c(2, 16))
  abline(lm( anscombe[, ys[i]] ~ anscombe[, xs[i]]))
}

# Smooth line
plot(dheight ~ mheight, Heights, cex=.1, pch=20)
abline(lm(dheight ~ mheight, Heights), lty=1)
with(Heights, lines(lowess(dheight ~ mheight, f=6/10, iter=1), lty=2))

# Scatterplot matrices

names(fuel2001)
summary(fuel2001)
?fuel2001

# add transformed variables
fuel2001 <- transform(fuel2001,
                      Dlic = 1000 * Drivers/Pop,
                      Fuel = 1000 * FuelC/Pop,
                      Income = Income/1000)
names(fuel2001)

# alternatively
fuel2001$FuelPerDriver <- fuel2001$FuelC / fuel2001$Drivers

names(fuel2001)

# pairs to draw scatterplot matrices
pairs(~ Tax + Dlic + Income + log(Miles) + Fuel, data = fuel2001)

# 1.7 Problems

#1.1
plot(log(fertility) ~ log(ppgdp), UN11)

#1.2
?wblake
meanLength <- with(wblake, tapply(Length, Age, mean))
sdLength <- with(wblake, tapply(Length, Age, sd))
samplesize <- with(wblake, tapply(Length, Age, length))