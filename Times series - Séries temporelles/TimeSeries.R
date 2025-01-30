### Chargement des librairies pour l'étude de séries ###

library(readxl)
library(tseries)
library(urca)
library(forcats)
library(ggplot2)
data <- read_excel("base_des_données.xlsx")
View(data)
data1 = ts(data, start=1991, end=2020, class = "ts", frequency = 1)
View(data1)
# le type des données
class(data1) 
### Represntation graphique des variables ###
croissance_pop = ts(data1[,2],start = 1991, end = 2020,frequency = 1 )
autoplot(croissance_pop,
        col=3, main="Croissance de la population",
        xlab="Année",ylab="Taux")
Chomage<-ts(data1[,3],start = 1991, end = 2020,frequency = 1)
ts.plot(Chomage, main="Chômage",xlab="Année",ylab="Taux" )
FBCF= ts(data1[,4],start = 1991, end = 2020,frequency = 1)
plot(FBCF, main="FBCF",col=92,col.main ="darkgreen")
grid(col = "lightgray")
#Répresentation globale des variables #
ts.plot(croissance_pop, Chomage, FBCF, main="Population,chômage, FBCF",
        col=c(4,2,8),xlab="Année",ylab="Taux")
grid(col = "lightgray")
legend("bottomleft", c("Population", "Chômage", "FBCF"))

### Tests paramètriques avant différenciation###
test_df = ur.df(croissance_pop)
summary(test_df)
test_PP<- ur.pp(croissance_pop)
summary(test_PP)
test_KPSS<- ur.kpss(croissance_pop)
summary(test_KPSS)
test1_df = ur.df(Chomage)
summary(test1_df)
test1_PP<- ur.pp(Chomage)
summary(test1_PP)
test1_KPSS<- ur.kpss(Chomage)
summary(test1_KPSS)
test2_df = ur.df(FBCF)
summary(test2_df)
test2_PP<- ur.pp(FBCF)
summary(test2_PP)
test2_KPSS<- ur.kpss(FBCF)
summary(test2_KPSS)

### MODELISATION ET PREDICTION UNIVARIEE #####
auto_arima.pop= forecast::auto.arima(croissance_pop)
summary(auto_arima.pop)
AIC(auto_arima.pop)

predict(arima(croissance_pop, order = c(2,2,0) ))

forecast::checkresiduals(auto_arima.pop)
forecast::Acf(croissance_pop)
forecast::checkresiduals(auto_arima.pop)
predict<- forecast::forecast(auto_arima.pop, h=10, level=0.95)
autoplot(predict, n.head=1, xlab="Temps", ylab="Taux de croisance de la population")
summary(predict)
print(predict)

dif_pop= diff(croissance_pop, differences = 5)
ar_pop<-ar(dif_pop, order = 2)
summary(ar_pop)
plot(forecast::forecast(ar_pop)) #prédiction du modèle AR
#BIC(dif_pop)

## Différenciation des variables ###
dif_pop= diff(croissance_pop, differences = 5)
autoplot(dif_pop)+ggtitle("Population")+xlab("Année")+ylab("%")
pacf(dif_pop, main="Dif_population")
dif_chomage = diff(Chomage, differences =5)
ts.plot(dif_chomage)
autoplot(dif_chomage)+ggtitle("Chomage")+xlab("Année")+ylab("%")
dev.copy(device = png, file="chomage.png") #copie de l'image dans un fichier png
dif_fbcf= diff(FBCF, differences = 5)
ts.plot(dif_fbcf, gpars=list(xlab="Année", ylab="%",main="FBCF", lty=c(3:9)))
grid()
autoplot(dif_fbcf)+ggtitle("Investissement")+xlab("Année")+ylab("%")
ts.plot(dif_pop,dif_chomage,dif_fbcf)
grid()

## TESTS DE RACINE UNITAIRE après différenciation ###
test1 <- ur.df(dif_chomage)
summary(test1)
test2= ur.kpss(dif_chomage)
summary(test2)
acf(dif_chomage)
pacf(dif_chomage)
test.fbcf <- ur.df(dif_fbcf)
summary(test.fbcf)
test.pop = ur.df(dif_pop)
summary(test.pop)
acf(dif_pop, col=2)
pacf(dif_pop, col=2)
summary(ur.pp(dif_pop))
summary(ur.kpss(dif_pop))
test.chomage= ur.df(dif_chomage)
summary(test.chomage)

### Constitution d'une nouvelle base VAR ###
library(vars)
datavar<- cbind(dif_pop,dif_chomage,dif_fbcf)
View(datavar)
plot(datavar, col=4)
grid()
autoplot(datavar, main="Séries stationnaires", xlab = "Année",ylab = "%")
autoplot(datavar)+ggtitle("Data-VAR")
VAR(datavar, type = "const", ic="AIC")

## ESTIMATION DU MODELE VAR ##
est_var<- VARselect(datavar, lag.max = 10) #renvoie les critères d'information et l'erreur de prédiction finale pour l'augmentation séquentielle de l'ordre de décalage jusqu'à un processus VAR(p)
est_var$selection
summary(est_var)
ts.plot(est_var$criteria[1,])
#lagselect
estimation<- VAR(datavar, p=5, type = "trend")
summary(estimation)
## DECOMPOSITION DU MODELE VAR
decomp= fevd(estimation)
plot(decomp, col=c(5,10,12))

##TEST DE VALIDATION ##
stabilité<- stability(estimation, type = "OLS-CUSUM")
plot(stabilité)

## SUMILATION ET PREVISION ##
estimation_irf <- irf(estimation)
plot(estimation_irf)
prevision = predict(estimation, n.ahead = 5, ci=0.95)
plot(prevision)
grid()
lines(prevision$predict,col="red")
plot(prevision,plot.type ="single") # representation de prevision 
View(prevision)
########### TESTS DE DIAGNOSTIC ########
## Test de normalité 

normalitytest <- normality.test(estimation)
summary(normalitytest)
plot(normalitytest)

## Test ARCH
archtest <- arch.test(estimation)
summary(archtest)
plot(archtest)

## test de correlation en série
serialtest <- serial.test(estimation)
plot(serialtest)
test= Box.test(estimation, lag = 10, type = "Box-Pierce")
Box.test(estimation, lag = 20, type = "Ljung-Box")
