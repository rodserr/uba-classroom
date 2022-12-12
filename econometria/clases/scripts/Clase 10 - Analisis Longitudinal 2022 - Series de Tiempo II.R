# install.packages("urca")
# install.packages("forecast")
# install.packages("lmtest")
library(urca)
library(forecast)
#############################################################
#                    PRONOSTICOS
#############################################################
# Carga datos macro de EEUU
data(npext)
# Los convierte en un objeto "time series"
y<-ts(na.omit(npext$unemploy),start=1909,end=1988,
      frequency=1)
# Recorto la última parte de la muestra para hacer un pronóstico ex-post
# Dejo para la regresión hasta 1970 inclusive
yreg<-ts(na.omit(npext$unemploy),start=1909,end=1970,
         frequency=1)
# Estimo con esa muestra recortada (los mismos modelos de antes)
arma11f<-arima(yreg,order=c(1,0,1))
arma22bf<-arima(yreg,order=c(2,0,2), fixed = c(0,NA,NA,NA,NA),transform.pars = FALSE)
# Aquí debería verificar que los residuos siguen siendo ruido blanco

# Realizo la predicción para los años 1971 a 1988 (18 años)
arma11ff=predict(arma11f,n.ahead=18)
arma22bff=predict(arma22bf,n.ahead=18)
# Y tomo los valores verdaderos de esos años para comparar
y_real=ts(tail(y,10),start=1971,end=1988)

# Calculo para ambos pronósticos los estadísticos de bondad 
# del pronóstico
accuracy(arma11ff$pred, y_real)
accuracy(arma22bff$pred, y_real)
# También puede hacerse con el comando Eval_Pron presentado
# en la Clase 1
Eval_Pron(arma11ff$pred, y_real,Nombre="Pron_1")
Eval_Pron(arma22bff$pred, y_real,Nombre="Pron_2")


# Relleno los años no pronosticados con NA para que los gráficos se
# superpongan adecuadamente
f_arma11<-ts(c(rep(NA,length(y)-18),arma11ff$pred),
             start=1909,frequency=1)
f_arma22b<-ts(c(rep(NA,length(y)-18),arma22bff$pred),
              start=1909,frequency=1)

# grafico todo junto
windows()
plot(y)
lines(f_arma11,col="blue",lty=1)
lines(f_arma22b,col="red",lty=1)
dev.off()

# *****************************************
#     SIMULACIONES
# *****************************************
# Semilla de los números aleatorios
set.seed(123456)
# Genero un AR(1) con phi_1 = 0,8
ar1=arima.sim(n=500,list(ar=0.8),innov=rnorm(500))
plot.ts(ar1)
# Genero una caminata aleatoria (Random Walk)
# Es un AR(1) con phi_1 = 1
rw=arima.sim(n=499,list(order=c(0,1,0)),innov=rnorm(500))
plot.ts(rw)
# trend
trd<-1:500
# Agrego una tendencia (Random Walk with drift)
rw.wd<-0.2*trd + rw
plot.ts(rw.wd)
# Le agrego al AR(1) una tendencia determinística
dt<-ar1+0.2*trd
plot.ts(dt)


# *****************************************
#      REGRESIÓN ESPURIA DE DOS RW_WD
# *****************************************
set.seed(123456)
# Genero 2 RW con distintas perturbaciones
# Genero una caminata aleatoria (Random Walk)
rw1=arima.sim(n=499,list(order=c(0,1,0)),innov=rnorm(500))
rw2=arima.sim(n=499,list(order=c(0,1,0)),innov=rnorm(500))
RW=matrix(c(rw1,rw2),ncol=2)
plot.ts(RW, plot.type="single",col=3:1)
reg=lm(rw1~rw2)
summary(reg)

# *****************************************
#      TEST DE RAIZ UNITARIA ADF
#    AUGMENTED DICKEY-FULLER TEST
# *****************************************
library(urca)
data(Raotbl3) # Base de datos 
# lc	Real consumption expenditure.
# li	Real income.
# lw	Real wealth.
# Se adjunta la base de datos para no invocarla en los comandos
attach(Raotbl3)
lc<-ts(lc,start=c(1966,4),end=c(1991,2),frequency=4)
# Se grafica la serie para elegir el modelo
plot(lc)    # se elige type="trend"
# Se calcula el máximo lag para el cálculo recursivo
max.lag=trunc(min(length(lc)/3,12)*(length(lc)/100)^0.25)
# Test con criterio "AIC"
lc.ctA=ur.df(lc, lags = max.lag, selectlags = "AIC", type="trend")
# El estadístico de interés es el Tau
summary(lc.ctA)
# Idem anterior con criterio "BIC"
lc.ctB=ur.df(lc, lags = max.lag, selectlags = "BIC", type="trend")
summary(lc.ctB)
# Idem anterior con criterio "StepWise"
lc.ctSW=ur.df(lc, lags = max.lag, selectlags = "Fixed", type="trend")
summary(lc.ctSW)
# repetir desde max.lag hasta 0 lags o hasta que sea significativo 
# el último rezago
# En este caso se valida lag=5 y NO se rechaza la raiz unitaria

# En los 3 casos se verifica la existencia de raiz unitaria
# Para ver si con una diferenciación se estacionariza, realizo
# el test sobre la diferencia
# Genero la diferencia de la serie
dlc=diff(lc)
# Grafico para elegir el modelo
plot(dlc)
# Si tengo dudas puedo regresar la serie de interés
# contra tendencia e intercepto
trend=seq(1:length(dlc))
summary(lm(dlc~trend))

# El modelo adecuado resulta ser con intercepto pero sin tendencia
dlc.cA=ur.df(dlc, lags = max.lag, selectlags = "AIC", type="drift")
summary(dlc.cA)

# Idem con criterio "BIC"
dlc.cS=ur.df(dlc, lags = max.lag, selectlags = "BIC", type="drift")
summary(dlc.cS)

# Idem con criterio "StepWise
dlc.cSW=ur.df(dlc, lags = max.lag, selectlags = "Fixed", type="drift")
summary(dlc.cSW)
# repetir desde max.lag hasta 0 lags o hasta que sea significativo 
# el último rezago
# En este caso se valida lag=7 y SI se rechaza la raiz unitaria

# *****************************************
# Determinación del orden de integración del PBI de EEUU
library(urca)
data(nporg)
# Carga el logaritmo del PBI deflactado (sin NA)
gnp<-ts(log(na.omit(nporg[,"gnp.r"])), start=1909, frequency=1)
ts.plot(gnp, type="l")

max.lag=trunc(min(length(gnp)/3,12)*(length(gnp)/100)^0.25)
gnp.adf=ur.df(gnp, lags = max.lag, selectlags = "AIC", type="trend")
summary(gnp.adf)

# Genera la diferencia de PBI deflactado
dgnp<-diff(gnp)
plot(dgnp)
max.lag=trunc(min(length(dgnp)/3,12)*(length(dgnp)/100)^0.25)
dgnp.adf<-ur.df(dgnp,lags = max.lag, selectlags = "AIC", type="drift")
summary(dgnp.adf)


# *****************************************
