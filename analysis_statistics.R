portfolio <- read.table("dionice.txt.",header=TRUE)
portfolio <-as.matrix(portfolio)
portfolio

monp<-as.matrix(portfolio[,1])
span<-as.matrix(portfolio[,2])
koei<-as.matrix(portfolio[,3])
n=nrow(portfolio)
n

#računanje jednostavnih prinosa
monp_p= (monp[2:n]/monp[1:(n-1)]-1)*100
span_p= (span[2:n]/span[1:(n-1)]-1)*100
koei_p= (koei[2:n]/koei[1:(n-1)]-1)*100
mat=cbind(monp_p, span_p, koei_p)
mat

#računanje očekivanih prinosa
monp_e=mean(monp_p)
span_e=mean(span_p)
koei_e=mean(koei_p)

#računanje standardnih devijacija
monp_s<-sd(monp_p, na.rm=TRUE) 
span_s<-sd(span_p, na.rm=TRUE)
koei_s<-sd(koei_p, na.rm=TRUE)

stdev<-c(monp_s,span_s,koei_s)
stdev
average<-c(monp_e,span_e,koei_e)
average
#interpretacija: standardna devijacija za dionicu span: prosječno odstupanje od očekivanog prinosa dionice span je 1.55%
#očekivani prinos za dionicu span: očekivani prinos dionice span iznosi 0.32%


#očekivani prinos portfelja i varijanca(dionice monp i span)
matrica_prinosa=cbind(monp_p,span_p,koei_p)
matrica_prinosa
cor(matrica_prinosa)
cor(monp_p,span_p)
w1=0.16
w2=0.84
ER_p=w1*monp_e+w2*span_e
ER_p
#Očekivani prinos portfelja je 0.22%
var_p=w1^2*monp_s^2+w2^2*span_s^2+2*w1*w2*monp_s*span_s*cor(monp_p,span_p)
var_p
#standardna devijacija portfelja interpretacija(pisano u consoli): prosječno odstupanje od očekivanog prinosa portfelja je 1.82%

#c)najrizičnija dionica u promatranom razdoblju je MONP dionica čija standardna devijacija iznosi 2.81%
#d)ako se smanji vrijednosni udio najrizičnije dionice, rizik portfelja će se smanjiti
#e) tri dionice:
w1=0.33
w2=0.33
w3=0.33
ER_p=w1*monp_e+w2*span_e+w3*koei_e
ER_p
var_p=w1^2*monp_s^2+w2^2*span_s^2+w3*koei_s^2+2*w1*w2*cov(monp_p,span_p)+2*w1*w3*cov(monp_p,koei_p)+2*w2*w3*cov(span_p,koei_p)
var_p

stdev_p=sqrt(var_p)
stdev_p

#f)
w1=0.25
w2=0.35
w3=0.40
ER_p=w1*monp_e+w2*span_e+w3*koei_e
ER_p
var_p=w1^2*monp_s^2+w2^2*span_s^2+w3*koei_s^2+2*w1*w2*cov(monp_p,span_p)+2*w1*w3*cov(monp_p,koei_p)+2*w2*w3*cov(span_p,koei_p)
var_p

stdev_p=sqrt(var_p)
stdev_p
#budući da sada dionica s najvećim rizikom(monp) sada ima najmanji udio u portfelju, očekivani
#prinos se povećao te se  smanjila standardna devijacija, odnosno rizik portfelja


# Pretvranja u data frame
portfolio_df <- as.data.frame(portfolio)

library(ggplot2)

#ggplot
ggplot(data = portfolio_df, aes(x = 1:nrow(portfolio_df))) +
  geom_line(aes(y = MONP, color = "MONP")) +
  geom_line(aes(y = SPAN, color = "SPAN")) +
  geom_line(aes(y = KOEI, color = "KOEI")) +
  labs(title = "Stock Prices Over Time", x = "Observation", y = "Price") +
  scale_color_manual(values = c("MONP" = "red", "SPAN" = "blue", "KOEI" = "green"))




p <- ts(scan("dionica.txt"),start=1, end=20)

p

Y<-as.matrix(p)
Y
n=nrow(Y)
n
h=2

#Naivni model I
F_I<-matrix(0,n+h,1)
F_I

for (i in 2:n) {
  F_I[i,1]=Y[(i-1),1]}
F_I

for (i in 1:h) {
  F_I[(n+i),1]=Y[n,1]}
F_I

install.packages("forecast")
library(forecast)

accuracy(object=F_I[2:n,1],x=Y[2:n,1])

#Naivni model II
F_II<-matrix(0,n+h,1)
F_II
for (i in 3:n) {
  F_II[i,1]=Y[(i-1),1]+(Y[(i-1),1]-Y[(i-2),1])}
F_II
for (i in 1:h) {
  F_II[(n+i),1]=Y[n,1]+i*(Y[n,1]-Y[(n-1),1])}
F_II

accuracy(object=F_II[3:n,1],x=Y[3:n,1])

#Naivni model IIa
F_IIa<-matrix(0,n+h,1)
F_IIa
for (i in 3:n) {
  F_IIa[i,1]=Y[(i-1),1]*(Y[(i-1),1]/Y[(i-2),1])}
F_IIa
for (i in 1:h) {
  F_IIa[(n+i),1]=Y[n,1]*(Y[n,1]/Y[(n-1),1])^i}
F_IIa

accuracy(object=F_IIa[3:n,1],x=Y[3:n,1])

#ažurirana aritmetička sredina
F<-matrix(0,n+h,1)
F
for (i in 2:n) {
  F[i,1]=mean(Y[(1:(i-1)),1])}
F
for (i in 1:h) {
  F[(n+i),1]=mean(Y[1:n,1])}
F

accuracy(f=F[2:n,1],x=Y[2:n,1])

#četveročlani pomični prosjeci
k=4

install.packages("TTR")
library(TTR) 

F_II<-SMA(Y,n=k)
F_II<-as.matrix(F_II)
F_II

n=nrow(Y)

F<-matrix(0,n+h,1)
F
for (i in (k+1):n) {
  F[i,1]=F_II[(i-1),1]}
F
for (i in 1:h) {
  F[(n+i),1]=F_II[n,1]}
F

accuracy(f=F[(k+1):n,1],x=Y[(k+1):n,1])

#dvočlani ponderirani pomični prosjeci
library(TTR) #prethodno instalirati TTR paket# 

k=2 
F0<-WMA(Y, n = k, wts = c(0.3,0.7),wilder=FALSE)

F0<-as.matrix(F0)
F0
n=nrow(Y)
n

F<-matrix(0,n+h,1)
F

for (i in (k+1):n) {
  F[i,1]=F0[(i-1),1]}
F

for (i in 1:h) {
  F[(n+i),1]=F0[n,1]}
F

accuracy(f=F[(k+1):n,1],x=Y[(k+1):n,1])

#jednostavno eksponencijalno izglađivanje
install.packages("forecast")
library(forecast)

F1 <- ses(Y, alpha=0.5, initial="simple", h=2)
F1

fitted<-F1$fitted 
fitted<-as.matrix(fitted)
fitted

forecast<-F1$mean 
forecast<-as.matrix(forecast)
forecast

F<-matrix(0,n+h,1)
F
for (i in 2:n) {
  F[i,1]=fitted[i,1]}
F
for (i in 1:h) {
  F[(n+i),1]=forecast[i,1]}
F

accuracy(f=F[2:n,1],x=Y[2:n,1])

#linerarno eksponencijalno izglađivanje
F1 <- holt(Y, h=2, damped=FALSE,initial="simple", alpha=0.3, beta=0.5)
F1

fitted<-F1$fitted
fitted<-as.matrix(fitted)
fitted
forecast<-F1$mean 
forecast<-as.matrix(forecast)
forecast

F<-matrix(0,n+h,1)
F

for (i in 2:n) {
  F[i,1]=fitted[i,1]}
F
for (i in 1:h) {
  F[(n+i),1]=forecast[i,1]}
F

accuracy(f=F[2:n,1],x=Y[2:n,1])
