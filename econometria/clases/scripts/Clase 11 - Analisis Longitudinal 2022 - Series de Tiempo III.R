#**************************************************
# Cointegración - Método de Engle-Granger
#**************************************************
# Relación de largo plazo de las series de Consumo, Ingreso y 
# Riqueza de UK
library(urca)

data(Raotbl3)
attach(Raotbl3)
# Carga las series macro de UK
# Consumo
lc<-ts(lc,start=c(1966,4),end=c(1991,2),
       frequency=4)
# Ingreso
li<-ts(li,start=c(1966,4),end=c(1991,2),
       frequency=4)
# Riqueza
lw<-ts(lw,start=c(1966,4),end=c(1991,2),
       frequency=4)
# Extrae un subconjunto o "ventana" para replicar
# las regresiones del paper
ukcons<-window(cbind(lc,li,lw),start=c(1967,2),
               end=c(1991,2))
# Gráfico de las series y sus diferencias
plot.ts(ukcons)
plot.ts(diff(ukcons))

# Hace las regresiones Engle Granger 
lc.eq<-summary(lm(lc~li+lw,data=ukcons))
li.eq<-summary(lm(li~lc+lw,data=ukcons))
lw.eq<-summary(lm(lw~li+lc,data=ukcons))

# Guarda los residuos en sendas variables
error.lc<-ts(resid(lc.eq),start=c(1967,2),
             end=c(1991,2),frequency=4)
error.li<-ts(resid(li.eq),start=c(1967,2),
             end=c(1991,2),frequency=4)
error.lw<-ts(resid(lw.eq),start=c(1967,2),
             end=c(1991,2),frequency=4)

#*********************************************************

# Verifica si son estacionarios. Ojo, verificar con los
# Valores Críticos de Engle y Granger
x=error.li
max.lag=trunc(min(length(x)/3,12)*(length(x)/100)^0.25)
ci.li<-ur.df(x, lags = max.lag, selectlags = "BIC", type="none")
summary(ci.li)
# Tau = -3.53   No es correcto porque se suprimieron 11 observaciones

# Hay que rehacer el test con los lags indicados por BIC, en
# este caso 1 lag
# Entonces mejor, para evitar confusiones, hacer esta rutina
x=error.li
max.lag=trunc(min(length(x)/3,12)*(length(x)/100)^0.25)
ci.li<-ur.df(x, lags = max.lag, selectlags = "BIC", type="none")
ci.li@testreg$call
# Reporta 1 lag
ci.li<-ur.df(x, lags = 1, selectlags = "Fixed", type="none")
summary(ci.li)
# Tau = -4.059   Rechaza

x=error.lw
max.lag=trunc(min(length(x)/3,12)*(length(x)/100)^0.25)
ci.lw<-ur.df(x, lags = max.lag, selectlags = "BIC", type="none")
ci.lw@testreg$call
# Reporta 1 lag
ci.lw<-ur.df(x, lags = 1, selectlags = "Fixed", type="none")
summary(ci.lw)
# Tau = -2.71    No rechaza

x=error.lc
max.lag=trunc(min(length(x)/3,12)*(length(x)/100)^0.25)
ci.lc<-ur.df(x, lags = max.lag, selectlags = "BIC", type="none")
ci.lc@testreg$call
# Reporta 1 lag
ci.lc<-ur.df(x, lags = 1, selectlags = "Fixed", type="none")
summary(ci.lc)
# Tau = -4.14         Rechaza

#**************************************************
# Metodología de Engle-Granger
# Modelo con término de corrección del error (ECM) 
# para consumo, ingreso y riqueza en UK
#**************************************************
# Utilizando las series y regresiones anteriores
library(dynlm)
ecm1=dynlm(d(lc)~d(li)+d(lw)+L(error.lc))
summary(ecm1)

# Verifico que los residuos de la regresión sean ruido blanco
corr_res(ecm1,12,0)

# Agrego rezagos de la diferencia de alguna/s variables 
# para absorber la autocorrelación remanente
ecm1=dynlm(d(lc)~d(li)+d(lw)+L(error.lc)+L(d(lc),2:3))
summary(ecm1)

# Verifico que los residuos de la regresión sean ruido blanco
corr_res(ecm1,12,0)

#**************************************************
# Metodología de Engle-Granger
# Series artificiales de Enders
#**************************************************
# Cargar archivos de Excel Enders.xls
# Realizar la prueba de cointgración con todos los pares de series
# Realizar la prueba de cointegración con las 3 series juntas


