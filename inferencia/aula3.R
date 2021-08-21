#Exercio 01
mi0<-700
omego<-10
n<-40
z<-(698-700)/(omego/sqrt(40))
pnorm(-1.264911)
qnorm(0.025)
qnorm(1-(0.025))

#Exercio 02


pnorm(1070, mean = 1120, sd = (125/sqrt(8)))



mi0<-206
omego<-12
n<-30
z<-(210-mi0)/(omego/sqrt(n))

qnorm(1-(0.1))# regiao critica
1-pnorm(z)# p-valor

#Exercio 03

mi0 <-40
s<-0.25
n<-25
t<-(38-mi0)/(sqrt(s/n))

pt(t, n-1)# p-valor
qt(0.05, n-1)

#Exercio 20

p<-668/726

z<-(p-0.5)/sqrt(0.5*0.5/726)
1-pnorm(z)# p-valor

qnorm(1-(0.05))
