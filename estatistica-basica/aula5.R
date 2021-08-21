algodao<-data.frame(percent =
c(
15, 20, 25, 30, 35, 
15, 20, 25, 30, 35, 
15, 20, 25, 30, 35, 
15, 20, 25, 30, 35, 
15, 20, 25, 30, 35 ),
resist =
c(
7, 12, 14, 19, 7,
7, 17, 18, 25, 10,
15, 12, 18, 22, 11,
11, 18, 19, 19, 15,
9, 18, 19, 23, 11
) )
boxplot(algodao$resist~algodao$percent)

(glm.algodao<-glm(resist~percent, family = gaussian(), algodao))
summary(glm.algodao)
plot(algodao)
lines(algodao$percent, predict(glm.algodao, data.frame(percent = algodao$percent)))
algodao$percent<-as.factor(algodao$percent)

(glm.algodao<-glm(resist~ percent + poly(percent, 2), family = gaussian(), algodao))
summary(glm.algodao)

cypemethrin <- data.frame(Dose =
c(rep(1, 20), rep(2, 20), rep(4, 20), rep(8, 20), rep(16, 20), rep(32, 20),
  rep(1, 20), rep(2, 20), rep(4, 20), rep(8, 20), rep(16, 20), rep(32, 20)),
mortos =
c(rep(1, 1) , rep(0, 19),
  rep(1, 4) , rep(0, 16),
  rep(1, 9) , rep(0, 20-9),
  rep(1, 13), rep(0, 20-13),
  rep(1, 18), rep(0, 20-18),
  rep(1, 20), rep(0, 20-20),
  rep(1, 0), rep(0, 20-0),
  rep(1, 2), rep(0, 20-2),
  rep(1, 6), rep(0, 20-6),
  rep(1, 10), rep(0, 20-10),
  rep(1, 12), rep(0, 20-12),
  rep(1, 16), rep(0, 20-16)),
sexo = c(rep('F', 20*6), rep('M', 20*6)))

cypemethrin$sexo<-as.factor(cypemethrin$sexo)


(glm.cypemethrin<-glm(mortos~Dose + sexo, family = binomial(link = "logit"), cypemethrin))
summary(glm.cypemethrin)
(glm.cypemethrin<-glm(mortos~log2(Dose) + sexo, family = binomial(link = "logit"), cypemethrin))
summary(glm.cypemethrin)

## data.frame incompleto
Quedas <- data.frame(
Individuo = c(1, 2, 3, 4, 5),
Quedas = c(1, 1, 2, 0, 2),
Intervencao = c(1, 1, 1, 1, 1),
Sexo = c(0, 0, 1, 1, 0),
Balanco = c(45, 62, 43, 76, 51),
Forca = c(70, 66, 64, 48, 72))

(glm.Quedas<-glm( Quedas ~ Individuo + Intervencao + Sexo + Balanco + Forca, family = poisson(link = "log"), Quedas))
summary(glm.cypemethrin)


## Dobson (1990) Page 93: Teste de controle aleatorio :
counts <- c(18,17,15,20,10,20,25,13,12)
outcome <- gl(3,1,9)
treatment <- gl(3,3)
data.frame(treatment, outcome, counts) # showing data
glm.D93 <- glm(counts ~ outcome + treatment, family = poisson(link = "log"))
anova(glm.D93)



# um exemplo de Gamma, de McCullagh & Nelder (1989, pp. 300-2)
clotting <- data.frame(
    u = c(5,10,15,20,30,40,60,80,100),
    lot1 = c(118,58,42,35,27,25,21,19,18),
    lot2 = c(69,35,26,21,18,16,13,12,12))
summary(glm(lot1 ~ log(u), data = clotting, family = Gamma))
summary(glm(lot2 ~ log(u), data = clotting, family = Gamma))


hist(rgamma(1000, 3.5))
