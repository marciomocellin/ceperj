head(cars)
plot(cars, xlab = "Speed (mph)", ylab = "Stopping distance (ft)",
    las = 1, xlim = c(0, 25))

(media <- mean(cars$dist, na.rm=FALSE))
(fm1 <- lm(dist ~ speed, data = cars))

# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -17.5791     6.7584  -2.601   0.0123 *  
# speed         3.9324     0.4155   9.464 1.49e-12 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 15.38 on 48 degrees of freedom
# Multiple R-squared:  0.6511,    Adjusted R-squared:  0.6438 
# F-statistic: 89.57 on 1 and 48 DF,  p-value: 1.49e-12
lines(cars$speed, predict(fm1, data.frame(speed = cars$speed)), col = "blue", lwd = 5)
abline(h = media[[1]], col = "red", lwd = 5)
predict<-predict(fm1, data.frame(speed = cars$speed))
carro <- cbind(cars, predict)
tail(carro)
?option

# speed dist  predict
#    23   54 72.86631
#    24   70 76.79872
#    24   92 76.79872
#    24   93 76.79872
#    24  120 76.79872
#    25   85 80.73112

Rsquared <- sum((carro$predict-media)^2)/sum((carro$dist-media)^2) # 0.6510794

𝑅𝑀𝑆𝐸 <-sum((carro$dist-carro$predict)^2)/length(carro$predict)
𝑅𝑆𝐸 <-sqrt(sum((carro$dist-carro$predict)^2)/(length(carro$predict)-1))
𝑀𝐴𝑃𝐸 <- (sum(abs((carro$dist-carro$predict)/carro$dist)))/length(carro$predict)*100
Raju <- 1-(sum((carro$dist-carro$predict)^2)/(50-1-1))/(sum((carro$dist-media)^2)/(50+1))


x1 <- rnorm(1000, 40, 15)
x2 <- rnorm(1000, 50, 20)
x3 <- rnorm(1000, 60, 20)
exemplo<- data.frame(y = 4.2*x1 + 2.1*x2 + 10 + rnorm(1000), x1 = x1, x2 = x2, x3 = x3)
head(exemplo)

#         y = 4.2*x1 + 2.1*x2 + 10
#        y       x1        x2       x3
# 274.9930 30.11609  65.70255 43.79735
# 266.2660 45.01851  31.29477 42.63868
# 368.0841 46.76239  77.59796 72.55738
# 316.1412 48.43889  48.35543 85.91233
# 347.5132 27.64759 105.62870 75.39855
# 248.1023 31.88220  50.58691 39.26399

(fm1 <- lm(y ~ x1, data = exemplo))
summary(fm1) # Multiple R-squared:  0.6965,    Adjusted R-squared:  0.6962
(fm2 <- lm(y ~ x2, data = exemplo))
summary(fm2) # Multiple R-squared:  0.3144, Adjusted R-squared:  0.3138 
(fm3 <- lm(y ~ x3, data = exemplo))
summary(fm3) # Multiple R-squared:  0.001464, Adjusted R-squared:  0.0004633 
(fm4 <- lm(y ~ x1 + x2, data = exemplo))
summary(fm4) # Multiple R-squared:  0.9998, Adjusted R-squared:  0.9998
AIC(fm4) #2882.295
(fm5 <- lm(y ~ x1 +x2 + x3, data = exemplo)) # R-squared:  0.9998,    Adjusted R-squared:  0.9998 
summary(fm5)
AIC(fm5) #2884.268

treino <- exemplo[1:900,]
teste <- exemplo[901:1000,]
(fmtrer <- lm(y ~ x1 + x2, data = treino))
# (Intercept)           x1           x2  
#      10.036        4.200        2.099 
summary(fmtrer) #R-squared:  0.9998,    Adjusted R-squared:  0.9998 
predict<-predict(fm1, teste[,2:4] )

df_test<-data.frame(predict = predict, treino = treino$y)
head(df_test)
#  predict   treino
# 198.1642 275.3292
# 255.3745 263.4887
# 341.1765 368.8236
# 349.5312 315.0987
# 283.8067 347.5476
# 318.0341 251.4957
Rsquared <- sum((df_test$predict-mean(df_test$treino))^2)/sum((df_test$treino-mean(df_test$treino))^2)
# 0.6736748