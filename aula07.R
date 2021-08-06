library(qcc)
#install.packages("qcc", repos = "https://cran.fiocruz.br/")

par(mfrow=c(2,3))
matriz<-data.frame(tempo = 1:100, dados = rnorm(100))
plot(matriz, type = 'l', ylim = c(-6, 6))

matriz<-data.frame(tempo = 1:100, dados = rnorm(100)+(-50:49)/10)
plot(matriz, type = 'l', ylim = c(-6, 6))

matriz<-data.frame(tempo = 1:100, dados = rnorm(100)*(seq(1, 25.75, 0.25))/10)
plot(matriz, type = 'l', ylim = c(-6, 6))

matriz<-data.frame(tempo = 1:100, dados = rnorm(100)+c(rep(-3, 49), 3,rep(-3, 50)))
plot(matriz, type = 'l', ylim = c(-6, 6))

matriz<-data.frame(tempo = 1:100, dados = rnorm(100)+c(rep(-3, 50),rep(3, 50)))
plot(matriz, type = 'l', ylim = c(-6, 6))

matriz<-data.frame(tempo = 1:100, dados = rnorm(100)+sin(1:100)*2)
plot(matriz, type = 'l')



data <- data.frame(
    V1 = c(12.4, 8.8, 10.8, 7.1, 6.7, 7.0, 6.7, 10.1, 6.0, 7.0, 7.3, 7.3, 7.5, 7.0, 6.8),
    V2 = c(7.6 ,7.0 ,7.4 ,7.6 ,6.6 ,7.9 ,7.9 ,7.4 ,7.7 ,6.5 ,7.2 ,6.7 ,7.7 ,6.8 ,7.8),
    V3 = c(7.2, 9.3, 7.7, 6.1, 7.3, 10.8, 8.1, 7.9, 8.1, 7.5, 13.7, 7.7, 9.4, 6.9, 7.5),
    V4 = c(7.9, 7.8, 7.0, 7.9, 7.9, 7.6, 7.1, 10.8, 8.1, 8.1, 7.3, 6.5, 6.9, 7.9, 7.3),
    V5 = c(7.9, 7.8, 8.0, 7.1, 7.9, 7.1, 7.0, 9.4, 8.4, 6.8, 7.5, 7.8, 7.2, 7.2, 11.6),
    V6 = c(8.0, 6.7, 7.0, 7.7, 7.5, 6.6, 7.4, 6.5, 11.2, 11.3, 7.5, 7.7, 7.9, 8.0, 7.1),
    stringsAsFactors = default.stringsAsFactors())

plot.xbar = qcc(data, type="xbar")
summary(plot.xbar)
# O gráfico de amplitude (R) é gerado apenas com a substituição do
# argumento type para “R”, conforme o código abaixo.

plot.R = qcc(data, type="R")

# Gráficos de individuais e amplitude móvel

data2 = c(0.8, 0.9, 2.9, 1.3, 1.3, 1.3, 3.8, 1.2, 2.4, 5.0, 1.7, 0.6, 2.6, 2.1, 5.8, 1.2, 1.2, 2.3, 4.0, 3.0, 1.6, 9.0, 1.1, 0.7, 4.8, 2.5, 8.3, 0.9, 1.0, 4.5)
plot.ind = qcc(data2, type="xbar.one")

novo.data2 = matrix(cbind(data2[1:length(data2)-1], 
                          data2[2:length(data2)]),
                    ncol=2)

plot.amp.movel = qcc(novo.data2, type="R", plot = TRUE)

# Atualizando as cartas de controle

plot.atual.1 = qcc(data[1:10,], type = "xbar", newdata = data[11:15,])

plot.atual.2 = qcc(data2[1:20], type="xbar.one", newdata = data2[21:30])

plot(plot.atual.2, chart.all=FALSE)


newdata = matrix(c(0.8, 0.9, 0.7,
                   1.2, 1.3, 1.1,
                   1.5, 1.6, 1.4,
                   1.8, 1.8, 1.8,
                   2.5, 2.5, 2.5,
                   2.8, 2.8, 2.8,
                   3.5, 3.5, 3.5,
                   4.0, 4.0, 4.0,
                   4.8, 2.5, 8.3,
                   0.9, 1.0, 4.5), ncol = 6, byrow = TRUE)

plot.shewhart = qcc(data, newdata = newdata, type="xbar")
qcc.options(run.length = 3)

plot.shewhart.3 = qcc(data, newdata = newdata, type="xbar")


