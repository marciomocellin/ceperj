require(BSDA)
library(BSDA)

ifelse(!require(BSDA),install.packages('BSDA', dependencies=T),1)
library(BSDA)

decolagem<-c(13,20,12,17,35,19,22,43,49,45,13,23)
pouso<-c(13,4,6,21,29,5,27,9,12,7,36,12)
decolagem-pouso # 0  16   6  -4   6  14  -5  34  37  38 -23  11
length(decolagem-pouso)

z<-((3+.05)-(12/2))/(sqrt(12)/2)

SIGN.test(decolagem, pouso, md=0, alternative="two.sided")

n = 100
x = 46
z<-((x +.05)-(n/2))/(sqrt(n)/2)
#posto



# 13,20,12,17,35,19,22,43,49,45,13,23
# 12      13      13      17      19  20 22 23 35 43 45 49
# 1       2.5     2.5     4       5   6

#1 2 3 4 5 # mediana = 3
#
#1 2 3 4 5 6 # mediana = 3.5

######

normal=c(1903, 1935, 1910,2496, 2108, 1961,2060, 1444, 1612, 1316, 1511)
secada=c(2009, 1915, 2011, 2463, 2180, 1925, 2122, 1482, 1542, 1443, 1535)
D<-normal-secada 
#-106   20 -101   33  -72   36  -62  -38   70 -127  -24
#10     1  9      3   8     4   6    5      7   11  2
ps<-c(-10, 1, -9, 3, -8, 4, -6, -5, 7, -11, -2)
n<-length(secada) #11

wilcox.test(normal,secada, paired=F, alternative = c("two.sided"))

####

baixo<-c(85,90,107,85,100,97,101,64)
# rank =  6.5  8.5 18.5  6.5 15.5 12.5 17.0  1.0
medio<-c(78,97,107,80,90,83)
alto<-c(93,100,97,79,97)
QI<-c(baixo,medio,alto)
N<-length(QI)
grupo<-
c(rep("b",length(baixo)),rep("m",length(medio)),rep("a",length(alto)))
rank<-rank(QI)
R1<-(sum(rank(QI)[grupo == 'b']))^2
n1<-length(baixo)
R2<-(sum(rank(QI)[grupo == 'm']))^2
n2<-sum(grupo == 'm')
R3<-(sum(rank(QI)[grupo == 'a']))^2
n3<-sum(grupo == 'a')

H<-12/(N*(N+1))*(R1/n1+R2/n2+R3/n3)-(3*(N+1))

1-pchisq(H,3-1)
kruskal.test(QI~factor(grupo))

# 𝐻0:𝑂𝑠 𝑔𝑟𝑢𝑝𝑜𝑠 𝑠ã𝑜 𝑠𝑒𝑚𝑒𝑙ℎ𝑎𝑛𝑡𝑒𝑠
# 𝐻1: 𝑃𝑒𝑙𝑜 𝑚𝑒𝑛𝑜𝑠 𝑢𝑚 𝑔𝑟𝑢𝑝𝑜 𝑑𝑖𝑓𝑒𝑟𝑒 𝑑𝑜𝑠 𝑑𝑒𝑚𝑎𝑖𝑠

###############

anos= c(0, 1, 3, 5, 9, 12, 13, 15, 21, 25)
gafanhotos=c(0.00, 0.19, 0.15, 1.49, 1.10, 1.12, 1.61, 1.42, 1.48, 1.92)
cor.test(anos,gafanhotos, method="spearman",alternative="two.sided")
plot(anos, gafanhotos)
