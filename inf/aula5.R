qchisq

a<-c(132, 98, 95, 98, 105, 133, 158)
sum(a) #819

b<-c(2, 1, 1, 1, 1, 2, 2)
819*(b /sum(b))
0.2 0.1 0.1 0.1 0.1 0.2 0.2
163.8  81.9  81.9  81.9  81.9 163.8 163.8

b<-c(3, 2, 2, 2, 2, 3, 3)
819*(b /sum(b))
144.52941  96.35294  96.35294  96.35294  96.35294 144.52941 144.52941



obs <- c(132, 98, 95, 98, 105, 133, 158)
esp <- rep(sum(a)/7,7) # 117 117 117 117 117 117 117

((obs - esp)^2)/esp #1.923077  3.085470  4.136752  3.085470  1.230769  2.188034 14.367521
sum(((obs - esp)^2)/esp) #30.01709

1-pchisq(sum(((obs - esp)^2)/esp), df = 6)
a<-rnorm(50)
qqnorm(a)
qqline(a)
(899/1232)*(39/1232)

M <- as.table(rbind(c(491, 377, 31), c(213, 112, 8)))
dimnames(M) <- list(gender = c("C", "M"),
                    party = c("preto","branco", "laranja"))

# gender preto branco laranja
#      C   491    377      31
#      M   213    112       8

(Xsq <- chisq.test(M))  # Prints test summary
Xsq$observed   # observed counts (same as M)
Xsq$expected   # expected counts under the null

(39*333)/1232 #10.5414

# gender    preto   branco laranja
#      C 513.7143 356.8271 28.4586
#      M 190.2857 132.1729 10.5414
# 
Xsq$residuals  # Pearson residuals
Xsq$stdres

observed<-Xsq$observed
#gender preto branco laranja
#     C   491    377      31
#     M   213    112       8
expected<-Xsq$expected
# gender    preto   branco laranja
#      C 513.7143 356.8271 28.4586
#      M 190.2857 132.1729 10.5414

observed-expected
# gender      preto     branco    laranja
#      C -22.714286  20.172890   2.541396
#      M  22.714286 -20.172890  -2.541396

((observed-expected)^2)/expected
# gender     preto    branco   laranja
#      C 1.0043302 1.1404556 0.2269505
#      M 2.7113900 3.0788876 0.6126982

sum(((observed-expected)^2)/expected) #8.774712

# (Xsq <- chisq.test(M))  # Prints test summary
# 
#         Pearson's Chi-squared test
# 
# data:  M
# X-squared = 8.7747, df = 2, p-value = 0.01243

1-pchisq(sum(((observed-expected)^2)/expected), 2) #p-valor = 0.01243356

((abs(6-8)-1)^2)/(6+8) # 0.07142857
1-pchisq(((abs(6-8)-1)^2)/(6+8), 1)
# 0.789268
# p-valor = 78,93%