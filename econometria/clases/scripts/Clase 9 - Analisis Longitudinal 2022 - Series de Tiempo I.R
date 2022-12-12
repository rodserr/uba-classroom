# ******************************************
#    METODOLOGÍA BOX - JENKINS
# ******************************************
# install.packages("urca")
# install.packages("forecast")
# install.packages("lmtest")

# ******************************************
#      Simulación de Series de Tiempo I
# ******************************************
# Seteo de números aleatorios
set.seed(123456)

# Genero un proceso simulado con el comando arima.sim(stats)
# Genero un AR(1)
y<-arima.sim(n=100,list(ar=0.9),innov=rnorm(100))
y[1:5]
plot.ts(y)
acf(y,main='ACF', ylim=c(-1,1),ci.col="blue")
pacf(y,main='PACF', ylim=c(-1,1),ci.col="blue")

# ******************************************
# Si se desea todos los gráficos en una ventana
# Abro la ventana
windows()
# Organiza los gráficos en una matriz
layout(matrix(c(1,1,2,3),2,2,byrow=TRUE))
# Grafica la serie, la ACF y la PACF
plot.ts(y,ylab='y')
acf(y,main='Autocorrelations',ylab='y',
    ylim=c(-1,1),ci.col="blue")
pacf(y,main='Partial Autocorrelations',ylab='y',
     ylim=c(-1,1),ci.col="blue")
# Cierra la ventana
dev.off()
# ******************************************
# Para tener los valores numéricos agrego "plot=F"
acf(y)
acf(y, plot=FALSE)
pacf(y, plot=FALSE)

#*******************************************************
# Estimación del modelo
#*******************************************************
# transform.pars estacionariza la serie, de ser
# necesario
arima(y,c(1,0,0),transform.pars=FALSE)
est.ar1<-arima(y,c(1,0,0),transform.pars=FALSE)
# Ver la lista est.ar1 que se genera

#*******************************************************
#    Verificación del modelo
#*******************************************************
# Ahora miro los residuos para ver si son ruido blanco
res<-residuals(est.ar1)
acf(res,main='ACF', ylim=c(-1,1),ci.col="blue")
# Para ver el test de Ljung - Box para un lag determinado
Box.test(res,lag=8,type="Ljung-Box", fitdf=1)
# Para una verificación más eficiente cargo el script corr_res
corr_res(est.ar1,12,1)
# Es ruido blanco

# ******************************************
#      Simulación de Series de Tiempo II
# ******************************************
# Simulación de un modelo MA(1) 
x<-arima.sim(n=100,list(ma=c(0.7)),innov=rnorm(100))
x[1:5]
plot.ts(x)
acf(x,main='ACF', ylim=c(-1,1),ci.col="blue")
pacf(x,main='PACF', ylim=c(-1,1),ci.col="blue")

#*******************************************************
# Estimación del modelo
#*******************************************************
arima(x,c(0,0,1),transform.pars=FALSE)
est.ma1<-arima(x,c(0,0,1),transform.pars=FALSE)

#*******************************************************
#    Verificación del modelo
#*******************************************************
# Ahora miro los residuos para ver si son ruido blanco
# Para una verificación eficiente utilizo script corr_res
corr_res(est.ma1,12,1)
# Es ruido blanco

# ******************************************
#      Simulación de Series de Tiempo III
# ******************************************
# Simulación de un modelo ARMA(1.1) 
w<-arima.sim(n=100,list(ar=c(0.6),ma=c(0.8)),innov=rnorm(100))
w[1:5]
plot.ts(w)
acf(w,main='ACF', ylim=c(-1,1),ci.col="blue")
pacf(w,main='PACF', ylim=c(-1,1),ci.col="blue")

#*******************************************************
# Estimación del modelo
#*******************************************************
arima(x,c(1,0,1),transform.pars=FALSE)
est.arma11<-arima(x,c(1,0,1),transform.pars=FALSE)

#*******************************************************
#    Verificación del modelo
#*******************************************************
# Ahora miro los residuos para ver si son ruido blanco
corr_res(est.arma11,12,2)
# Es ruido blanco

#*******************************************************
#    Tareas prácticas
#*******************************************************
#
#  Generar un proceso ARMA(2,1), plotear su correlograma
#  estimar el modelo y verificar ruido blanco

# ******************************************
#  Serie de tiempo con atributos temporales
# ******************************************
# arima.sim genera objetos ts. Si quiero transformar
# una lista cualquiera de numeros en una serie de tiempo
s=c(1:100)*0.03+rnorm(100)
s[1:5]

# Convierto la serie de números en una serie de tiempo
# mensual que comienza en enero de 2009
s1=ts(s,start=c(2009,1), frequency=12)
plot.ts(s1)

# Serie trimestral que comienza en 1er. Trim. 1980
s2=ts(s,start=c(1980,1), frequency=4)
plot.ts(s2)

# Serie anual que comienza en 1922
s3=ts(s,start=c(1922))
plot.ts(s3)

#############################################################
#           Trabajo con series empíricas
#############################################################

library(urca)
# Carga datos macro de EEUU
data(npext)   #Nelson & Plosser data set

# Convierto la serie de desempleo en un objeto "time series"
# Es la única serie estacionaria del data set
y<-ts(na.omit(npext$unemploy),start=1890,end=1988,
      frequency=1)
plot.ts(y)

# Miro los correlogramas
acf(y,main='ACF', ylim=c(-1,1),ci.col="blue")
pacf(y,main='PACF', ylim=c(-1,1),ci.col="blue")
#######################################################
# Modelizo  con un AR(1)
arima(y,order=c(1,0,0))
arma10<-arima(y,order=c(1,0,0))
library(lmtest)
# El comando coeftest() me brinda mejor salida
coeftest(arma10)

# Verificación ruido blanco
corr_res(arma10,12,1)
# No es ruido blanco

########################################################
# Otra tentativa con ar(2)
arma20<-arima(y,order=c(2,0,0))

# Ahora miro los residuos para ver si son ruido blanco
corr_res(arma20,12,2)
# No es ruido blanco

##################################################################
# Tercera tentativa con arma(1,1)
# arima(y,order=c(1,0,1))
arma11<-arima(y,order=c(1,0,1))

# Ahora miro los residuos para ver si son ruido blanco
corr_res(arma11,12,2)
# Los p-value disponibles señalan ruido blanco
# queremos testear los lags faltantes (1 y 2)

# Usamos el comando bgtest de "lmtest"
bgtest(arma11)
# No funciona porque necesita extraer el residuo de una 
# salida de regresión lineal realizada con el comando "lm()". 

# Genero una regresión lineal "fake"
arma11_lm=lm(arma11$residuals ~ 1)
bgtest(arma11_lm, order=1)
bgtest(arma11_lm, order=2)
# Se verifica que no hay autocorrelación

#################################################
# ¿ Encontrar otro modelo con ruido blanco ?
# pruebo con MA(2) y un coeficiente AR en t-2
# Lo indico con order=c(2,0,2) indicando los máximos rezagos
# del AR y el MA y con y fijo como 0 el phi_1
# En fixed aparecen 5 parámetros para incluir el intercepto
# arima(y,order=c(2,0,2), fixed = c(0,NA,NA,NA,NA))
arma22b<-arima(y,order=c(2,0,2), fixed = c(0,NA,NA,NA,NA))
coeftest(arma22b)
# Fijarse que el theta_2 es no significativo
# pero si lo retiro, no tengo ruido blanco
# Ahora miro los residuos para ver si son ruido blanco
corr_res(arma22b,12,3)
# Fijarse que el theta_2 es no significativo
# pero si lo retiro, no tengo ruido blanco

# Los p-value disponibles señalan ruido blanco
# queremos testear los lags faltantes (1 2 y 3)

arma22b_lm=lm(arma22b$residuals ~ 1)
bgtest(arma22b_lm, order=1)
bgtest(arma22b_lm, order=2)
bgtest(arma22b_lm, order=3)

# Tengo 2 modelos ruido blanco arma11 y arma22b
# Elijo con Akaike o Schwarz (el valor menor o el más negativo)
AIC(arma11)    ;  AIC(arma22b)

BIC(arma11)    ;   BIC(arma22b)
# Ambos señalan que el mejor modelo es el ARMA(1,1)

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Utilización del comando auto.arima()
# para identificación automática del mejor modelo
library(forecast)
auto=auto.arima(y,max.p=3,max.q=3,start.p=1,
           start.q=1,ic="aic")
coeftest(auto)
# Elige el mismo modelo (no siempre)
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Cargar las series macro de Argentina (ODA_Indec)
# Modelizar Exportaciones y Variación porcentual de las Importaciones
Var_porc_y=diff(y)/lag(y,-1)


