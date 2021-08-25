cor(cars$speed, cars$dist)
cor(cars$dist, cars$speed)
plot(cars, xlab = "Speed (mph)", ylab = "Stopping distance (ft)",
    las = 1, xlim = c(0, 25))
d <- seq(0, 25, length.out = 200)
fm1 <- lm(dist ~ speed, data = cars)
lines(d, predict(fm1, data.frame(speed = d)), col = "red")
plot(predict(fm1, data.frame(speed = d), interval = 'confidence'))
summary(fm1)
anova(fm1)


x <- rnorm(15)
y <- x + rnorm(15)

plot(x, y)
predict(lm(y ~ x))
new <- data.frame(x = seq(-3, 3, 0.5))
predict(lm(y ~ x), new, se.fit = TRUE)
pred.w.plim <- predict(lm(y ~ x), new, interval = "prediction")
pred.w.clim <- predict(lm(y ~ x), new, interval = "confidence")
matplot(new$x, cbind(pred.w.clim, pred.w.plim[,-1]),
        lty = c(1,2,2,3,3), type = "l", ylab = "predicted y", lwd = 5)


head(iris,20)
str(iris)
fm1 <- lm(Sepal.Length ~ Species, data = iris) #Sepal.Width + Petal.Length^2 + log(Petal.Width) +
summary(fm1)
anova(fm1)

#?t.test
