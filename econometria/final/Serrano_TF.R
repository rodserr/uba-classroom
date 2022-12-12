# Library----
library(tidyverse)
library(lmtest)
library(sandwich)
library(stargazer)
library(pder)
library(plm)
library(dynlm)
library(urca)
library(vars)

# helpers ----

vcorr_res=function(var_reg,lags,tipo="PT.adjusted") {
  
  library(vars)  
  T=var_reg$obs
  if(lags>T/3) {
    print("Elija una cantidad menor de lags")}
  else {
    lag=c()
    q_values=c()
    p_values=c()
    for(i in 1:lags)   {
      lag=c(lag,i)  
      warn<-options(warn=-1)
      if(tipo=="BG" | tipo=="ES") {
        p=serial.test(var_reg,lags.bg=i,type=tipo)}
      else {
        p=serial.test(var_reg,lags.pt=i,type=tipo)  }
      options(warn)
      q_values=c(q_values,round(p$serial$statistic, digits=4))
      #  if(tipo=="PT.adjusted" | tipo=="PT.asymptotic") {
      #  if(i<=var_reg$p){
      if(i<=var_reg$p & (tipo=="PT.adjusted" | tipo=="PT.asymptotic") ){
        p_values=c(p_values,"NA")
      } else {
        p_values=c(p_values,round(p$serial$p.value, digits=4))
      }
      #  }
    }
  }
  
  correlograma=cbind(lag,q_values,p_values)
  rownames(correlograma)=NULL
  colnames(correlograma)=c("lag","Estad?stico Q","p-value")
  return(correlograma)
  options(digits=4)
  print(correlograma)
}

Test.ADF=function(y,mod,sel.lags="AIC",alfa=0.10) {
  
  # Convierto la serie en serie de tiempo
  x=ts(y,start=1)
  # Determino la máxima cantidad de lags
  maxlag=trunc(min(length(x)/3,12)*(length(x)/100)^0.25)
  print(paste("Cantidad máxima de lags: ", maxlag))
  
  # Preparo las fórmulas (no valen para lag=0)
  if(mod==1){form=d(x) ~ L(x)+L(d(x),1:i) -1}
  if(mod==2){form=d(x) ~ L(x)+L(d(x),1:i)}
  if(mod==3){form=d(x) ~ L(x)+trend(x)+L(d(x),1:i)}
  #**********************************************************
  if(sel.lags=="SW"){
    tabla=c()  # No habrá tabla
    # Inicializo con lag=0
    lag=0
    for(i in maxlag:1) {
      reg=dynlm(form)
      A=summary(reg)
      if(A$coefficients[(i+mod),4]< alfa){
        lag=i ; break }
    }
    print(paste("Metodo SW , lag = ",lag))
  }
  #**********************************************************  
  if(sel.lags=="BIC" | sel.lags=="AIC"){
    tabla=matrix(rep(0,((maxlag+1)*3)),ncol=3)
    colnames(tabla)=c("Lag","AIC","BIC")
    for(i in 1:maxlag) {
      reg=dynlm(form,start=maxlag+2)
      tabla[i+1,1]=i
      tabla[i+1,2]=AIC(reg)
      tabla[i+1,3]=BIC(reg)
    }
    if(mod==1){form0=d(x) ~ L(x) -1}
    if(mod==2){form0=d(x) ~ L(x)}
    if(mod==3){form0=d(x) ~ L(x)+trend(x)}
    reg0=dynlm(form0, start=maxlag+2)
    tabla[1,1]=0
    tabla[1,2]=AIC(reg0)
    tabla[1,3]=BIC(reg0)
  }
  #**********************************************************  
  if(sel.lags=="AIC"){
    tabla.ord=tabla[order(tabla[,2],decreasing=F),]
    lag=tabla.ord[1,1]
    print(paste("Método AIC , lag = ",lag))
  }
  #**********************************************************
  if(sel.lags=="BIC"){
    tabla.ord=tabla[order(tabla[,3],decreasing=F),]
    lag=tabla.ord[1,1]
    print(paste("Método BIC , lag = ",lag))
  }
  #**********************************************************   
  if(mod==1){salida<-ur.df(x, lags = lag, selectlags = "Fixed", type="none")}
  if(mod==2){salida<-ur.df(x, lags = lag, selectlags = "Fixed", type="drift")}
  if(mod==3){salida<-ur.df(x, lags = lag, selectlags = "Fixed", type="trend")}
  print(summary(salida))
  Test.ADF=list(mod,sel.lags,lag,tabla)
  names(Test.ADF)=c("Modelo","Selector de Lags","Lags sel","Tabla")
  return(Test.ADF)
}

Eval_Pron=function(Y_P,Y_A,Nombre="Pron_1"){
  # Calculo de los estadísticos
  pron=cbind(Y_A,Y_P)
  RMSE=sqrt(sum((Y_P-Y_A)^2)/length(Y_P)) ; RMSE
  MAE=sum(abs(Y_P-Y_A)/length(Y_P))  ;  MAE
  MAPE=100*sum(abs((Y_P-Y_A)/Y_A)/length(Y_P))  ; MAPE
  D1=sqrt(sum(Y_P^2)/length(Y_P))   ;   D1
  D2=sqrt(sum(Y_A^2)/length(Y_P))   ;  D2
  Theil=RMSE/(D1+D2)  ;   Theil
  
  # Descomposición del Theil
  MSE=RMSE^2
  U1=(1/MSE)*(mean(Y_P)- mean(Y_A))^2
  sd_Y_P=sqrt(var(Y_P)*(length(Y_P)-1)/length(Y_P))
  sd_Y_A=sqrt(var(Y_A)*(length(Y_A)-1)/length(Y_A))
  U2=(1/MSE)*((sd_Y_P)-sd_Y_A)^2
  U3=1-U1-U2
  
  # Resumen
  Pron_1=matrix(c(RMSE,MAE, MAPE,Theil,U1,U2,U3),ncol=1)
  rownames(Pron_1)=c("RMSE","MAE", "MAPE","U_Theil","U_sesgo","U_varianza","U_covarianza")
  colnames(Pron_1)=c(Nombre)
  return(Pron_1)
}

corr_res=function(xreg,lags,p_q) {
  res=xreg$residuals
  T=length(res)
  if(lags>T/3) {
    print("Elija una cantidad menor de lags")}
  else {
    a=p_q
    lag=c()
    q_values=c()
    p_values=c()
    for(i in 1:lags)   {
      lag=c(lag,i)  
      warn<-options(warn=-1)
      p=Box.test(xreg$residuals,lag=i,type="Ljung", fitdf=a)
      options(warn)
      q_values=c(q_values,round(p$statistic, digits=4))
      if(i<=a){
        p_values=c(p_values,"NA")
      } else {
        p_values=c(p_values,round(p$p.value, digits=4))
      }
    }
  }
  
  correlograma=cbind(lag,q_values,p_values)
  rownames(correlograma)=NULL
  colnames(correlograma)=c("lag","Estadistico Q","p-value")
  return(correlograma)
  options(digits=4)
  print(correlograma)
}

######## PARTE I - DATOS DE PANEL-----------
# Cargar datos 
bancos <- haven::read_stata('econometria/final/Base bancos depurada.dta')
bancosp <- pdata.frame(bancos, index = c('entidad', 'year'))

# a) Regrese un modelo Pooling (MCO)
bancos_formula <- 'roa~capital+creditrisk+ProdPersonal+operating+size+hh'
summary(bancos_pool <- plm(bancos_formula, bancosp, model = 'pooling'))
coeftest(bancos_pool, vcov = vcovHC)

# b) Regrese modelos de efectos fijos (Within) con efectos individuales, efectos temporales y ambos efectos, respectivamente.

bancos_w <- plm(bancos_formula, bancosp, model = 'within')
bancos_w_two <- plm(bancos_formula, bancosp, model = 'within', effect = "twoways")
bancos_rand <- plm(bancos_formula, bancosp, model = 'random')
bancos_rand_two <- plm(bancos_formula, bancosp, model = 'random', effect = "twoways")

# c) Implemente tests de hipótesis para verificar la presencia y significatividad de estos efectos y en función del resultado de los mismos decida entre
# los modelos estimados cuál resultaría el más adecuado. Recuerde que en este contexto (efectos fijos) los tests a utilizar son los tests F.

pFtest(bancos_w, bancos_pool) # Rechaza, existe efectos infividuales
pFtest(bancos_w_two, bancos_w) # Rechaza, existe efectos temporales
pFtest(bancos_w_two, bancos_pool) # Rechaza, existe efectos infividuales

# d) Incorpore la posibilidad de modelar el/los efecto/s encontrado/s con modelos de efectos aleatorios (GLS). Regrese dichos modelos utilizando las
# alternativas provistas por el software (random.method). Para no explorar tantos modelos limítese a la especificación de Swamy y Arora

rand_model_swar=plm(bancos_formula, bancosp, model = "random", effect="individual", random.method="swar")
summary(rand_model_swar, vcov=vcovHC)

# e) Implemente tests de hipótesis para verificar la presencia y significatividad de estos efectos y en función del resultado de los mismos decida entre 
# los modelos estimados cuál resultaría el más adecuado. Recuerde que en este contexto (efectos aleatorios) los tests a utilizar son los tests LM.

plmtest(bancos_pool, type = 'honda')
plmtest(bancos_pool, effect="twoways", type = 'honda')

# f) Una vez elegidos los mejores modelos Within y GLS elija entre ellos utilizando un test adecuado. Fíjese para poder hacer el test ambos modelos
# deben contemplar los mismos efectos.

phtest(bancos_w, rand_model_swar)

# g) Confeccione una tabla resumen con los parámetros obtenidos y sus errores estándar que incluya los modelos Pooling, Within y GLS
# (en estos dos últimos sólo los elegidos por los tests). Elija errores estándar robustos

stargazer(bancos_pool, bancos_w, rand_model_swar,
          column.labels = c("MCO", "Within", "GLS"),
          title= "Comparacion de estimadores", type="text",
          digits=3)



######## PARTE II - MODELOS DE VECTORES AUTORREGRESIVOS------------

# a) Cargar el archivo y construir dos conjuntos de datos. Uno de ellos (muestra), con los datos de starts y comps para el período 1968:01 – 1994:12
# que se utilizará como muestra para definir el modelo, estimarlo y realizar pronósticos y otro (total) con los datos del período completo 1968:01-1996:06 
# que permitirá evaluar el pronóstico.
housing <- haven::read_stata('econometria/final/Housing.dta')
housing_ts=ts(housing[,-1], start=c(1968, 1), frequency=12)
housing_ts_train=ts(housing[,-1], start=c(1968,1), end=c(1994, 12), frequency=12)

# b) Verificar que ambas series son estacionarias I(0). Utilice el script test.ADF.R , que reportará el resultado del test una vez que se elija el
# modelo y el método para seleccionar la cantidad de lags a incluir en el test. Realice los tests con el modelo seleccionado y con los 3 métodos de
# selección de lags (utilice un nivel de significatividad alfa=0.10 en el método SW). Haga un cuadro con los resultados, pero mantenga en lo que sigue 
# la hipótesis de que ambas series son I(0)

plot(housing_ts)
acf(housing_ts_train, main='ACF', ylim=c(-1,1),ci.col="blue")
pacf(housing_ts_train, main='PACF', ylim=c(-1,1),ci.col="blue")

Test.ADF(housing_ts_train[,1], 1, sel.lags="AIC")
Test.ADF(housing_ts_train[,1], 1, sel.lags="BIC")
Test.ADF(housing_ts_train[,1], 1, sel.lags="SW", alfa=0.10)

Test.ADF(housing_ts_train[,2], 1, sel.lags="AIC")
Test.ADF(housing_ts_train[,2], 1, sel.lags="BIC")
Test.ADF(housing_ts_train[,2], 1, sel.lags="SW", alfa=0.10)

# c) Con los datos del conjunto “muestra” identificar y estimar modelos ARMA para cada una de las dos variables.
arma_starts<-arima(housing_ts_train[,1], order=c(1,0,1))
arma_comps<-arima(housing_ts_train[,2], order=c(3,0,3))

# d) Dado que se trata de series I(0) se estimarán directamente las series sin transformar. Verifique la ausencia de autocorrelación remanente en los residuos 
# con el programa corr_res.R ya utilizado en el curso. Dado que se trata de series mensuales puede limitarse a verificar la ausencia de autocorrelación en 
# los primeros 6 rezagos y los rezagos 12, 24 y 36
corr_res(arma_starts, 36, 0)
corr_res(arma_comps, 36, 0)

# e) Realice el pronóstico de cada una de las series para el período 1995:01-1996:06 con la metodología indicada en la clase correspondiente
arma_starts_predict = predict(arma_starts, n.ahead=18)
arma_comps_predict = predict(arma_comps, n.ahead=18)

preds <- ts(
  data.frame(
    starts = c(rep(NA,length(housing_ts_train[,1])), arma_starts_predict$pred),
    comps = c(rep(NA,length(housing_ts_train[,1])), arma_comps_predict$pred)
  ),
  start=c(1968, 1), frequency=12)
   

plot(housing_ts_train[,1])
lines(preds[,'starts'], col="red",lty=1)

plot(housing_ts_train[,2])
lines(preds[,'comps'], col="red",lty=1)

# f) Con los datos del conjunto “muestra” estimar un modelo VAR que incluya las 2 variables eligiendo provisoriamente los lags adecuados por medio del
# criterio de Akaike (utilice max.lag=24). Fíjese en el gráfico de las series si corresponde agregar constante y / o tendencia.
VARselect(housing_ts_train,lag.max=24, type="const")
var1=VAR(housing_ts_train, type="const", ic="AIC", p = 4)
summary(var1)
plot(var1)

# g) Verifique la ausencia de autocorrelación de los residuos hasta el rezago 12. Utilice el programa vcorr_res.R . Privilegie el método Portmanteau Asintótico
# y verifique los rezagos faltantes con el test de Breusch y Godfrey (sin ajuste). Si aparece algún caso de rechazo considere aumentar la cantidad de
# lags y repita los diagnósticos anteriores.
vcorr_res(var1, 12, "PT.asymptotic")

# h) Verifique la estabilidad del VAR estimado.
roots(var1)

# i) Realice pruebas de Causalidad en el sentido de Granger y de acuerdo a los resultados de los tests reorganice el VAR colocando primero la serie 
# más “exógena”. Esta reorganización es necesaria para la prueba de normalidad.
causal1<-causality(var1,cause="starts")
causality(var1,cause="starts")
causal2<-causality(var1,cause="comps")
causality(var1,cause="comps")

# j) Verifique la normalidad de las perturbaciones y reporte el resultado del test. Si el test conjunto rechaza, verifique cada una de las series de residuos
# para ver cuál es el que causa el rechazo y repórtelo.
t.norm<-normality.test(var1, multivariate.only=FALSE)
t.norm
hist(t.norm$resid[,1])
hist(t.norm$resid[,2])

# k) Con el modelo estimado realice una predicción de las series para el período 1995:01-1996:06. Puede utilizar el comando “predict()” presentado en clase.
pred <- predict(var1, n.ahead=18)

# l) Una vez realizados los 2 pronósticos (ARMA, y VAR) para las series para el período 1995:01-1996:06, se realizará una evaluación de los mismos comparando 
# cada uno de ellos con los verdaderos valores según figuran en el conjunto “total”. Utilice para la evaluación el script Eval_Pron.R visto en la Clase 1. 
# Organice las evaluaciones en un cuadro sucinto para cada serie (Pronóstico vs. Estadístico) como se muestra más abajo.

housing_ts_test=ts(housing[,-1], start=c(1995,1), end=c(1996, 06), frequency=12)

t(
  cbind(
    Eval_Pron(pred$fcst$starts[,'fcst'], housing_ts_test[, 'starts'], 'VAR'),
    Eval_Pron(arma_starts_predict$pred, housing_ts_test[, 'starts'], 'ARMA')
  )
)

t(
  cbind(
    Eval_Pron(pred$fcst$comps[,'fcst'], housing_ts_test[, 'comps'], 'VAR'),
    Eval_Pron(arma_comps_predict$pred, housing_ts_test[, 'comps'], 'ARMA')
  )
)

# m) Comente, con base en los resultados obtenidos, si la intuición de que la estimación de un modelo multivariado mejoraría los pronósticos se cumplió 
# en este caso o no.

