library(vars)
library(urca)
library(forecast)

######## Ejercicio 1----

# 1. Cargar los datos
data(Raotbl3)
z1=data.frame(cbind(diff(Raotbl3$lc), diff(Raotbl3$li)))
names(z1)=c("dlc","dli")

# Convertimos en un objeto "series de tiempo"
y=ts(z1, start=c(1967,1), frequency=4)

# 2. Verificar el orden de integración de las series lc y li mediante el test ADF, eligiendo el modelo y los lags
# a utilizar en el test. En principio se postula que las series son I(1) . El análisis debe incluir el test sobre
# sus diferencias dlc y dli para descartar la existencia de doble raíz unitaria.
plot.ts(y)
acf(y[,'dlc'], main='ACF', ylim=c(-1,1),ci.col="blue")
pacf(y[,'dlc'], main='ACF', ylim=c(-1,1),ci.col="blue")
acf(y[,'dli'], main='ACF', ylim=c(-1,1),ci.col="blue")
pacf(y[,'dli'], main='ACF', ylim=c(-1,1),ci.col="blue")

# 3. Considerando la hipótesis anterior construya un VAR con ambas variables en diferencias (dlc y dli) y determine 
# el modelo (con o sin tendencia y/o intercepto) y la cantidad de lags a utilizar. Una vez estimado el modelo realice
# los diagnósticos que se indican

VARselect(y[,1:2],lag.max=8, type="const")
var1=VAR(y[,1:2],type="const", ic="AIC", p = 3)
summary(var1)

# a. Estacionariedad de VAR (Es mandatorio que las raíces (inversas) sean en módulo menores que 1
roots(var1)

# b. No existencia de autocorrelación en los residuos del VAR. Para esto se verificará con el programa vcorr_res.R. 
# Si la hubiera debe corregirse agregando más rezagos o, en caso de ser una autocorrelación estacional
# (en el rezago 4 en losniveles o en el rezago 3 en las diferencias) también pueden agregarse variables dummies
# estacionales.
vcorr_res(var1, 12, "PT.adjusted")

# c. Normalidad de las perturbaciones. No es mandatoria
t.norm<-normality.test(var1, multivariate.only=FALSE)
t.norm
hist(t.norm$resid[,1])
hist(t.norm$resid[,2])

# d. Causalidad en el sentido de Granger. Este test le indicará si el orden elegido para ubicar las series
# en el modelo es el correcto, en el sentido de que la serie más exógena debe ubicarse primero para luego dar 
# sentido a la transformación de Cholesky.
causality(var1,cause="dli")
causality(var1,cause="dlc")

######## Ejercicio 2----

# 1) Con los datos de Raotbl3 construir dos conjuntos de datos que incluyan a las variables dlc y dli. 
# Uno de ellos (muestra), con los datos hasta 1989q02 que se utilizará como muestra para definir el modelo, 
# estimarlo y realizar pronósticos sobre el período 1989q03 – 1991q02 y otro (total) con los datos del período
# completo 1967q12-1991q02 (fijese que al tomar diferencias se pierde una observación) que permitirá evaluar el pronóstico
y_train=ts(y, start=c(1967,1), end=c(1989,2) , frequency=4)

# 2) Con los datos del conjunto “muestra” estimar un VAR, eligiendo los lags adecuados (puede repetir la selección 
# realizada en la primera parte ya que la muestra es casi la misma.
var_train=VAR(y_train[,1:2],type="const", ic="AIC", p = 3)

# 3) Responda la siguiente pregunta conceptual. ¿Tiene influencia en el pronóstico la ubicación
# de las series según indicó el test de causalidad?.


# 4) Con el modelo estimado realice una predicción para el período 1989q03 a 1991q02 y grafique las series pronosticadas
# junto con las realizaciones utilizando para ello el conjunto “total”
pred <- predict(var_train, n.ahead=8)

# Ingreso
plot(y[,1])
plot(pred,names="dli")
lines(y[,1])      
dev.off()

#Consumo
plot(y[,2])  
plot(pred,names="dlc")
lines(y[,2])      
dev.off()

# 5) Ahora considere cada serie independientemente. Estime modelos ARMA(p,q) para las mismas con los datos hasta 1989q02
# y realice un pronóstico de cada una de ellas para el período 1989q03 a 1991q02.
arma_dli = auto.arima(y_train[,1],max.p=3,max.q=3,start.p=1, start.q=1,ic="aic")
arma_dlc = auto.arima(y_train[,2],max.p=3,max.q=3,start.p=1, start.q=1,ic="aic")

dli_arma_pred = predict(arma_dli, n.ahead=8)
dlc_arma_pred = predict(arma_dlc, n.ahead=8)

# 6) Compare los pronósticos realizados para cada serie mediante ambos métodos mediante el programa Eval_Pron.R. 
# En principio podría esperarse un pronóstico más preciso con el modelo VAR que con las series en forma independiente
# ¿Se cumple esta hipótesis?
y_test=ts(y, start=c(1989,3), end=c(1991,2), frequency=4)
Eval_Pron(y_test[,'dlc'], dli_arma_pred$pred)
Eval_Pron(y_test[,'dlc'], pred$fcst$dlc[,1])

Eval_Pron(y_test[,'dli'], dlc_arma_pred$pred)
Eval_Pron(y_test[,'dli'], pred$fcst$dli[,1])

######## Ejercicio 3----
# 1) Construya un modelo VAR (ya lo debe tener de la 1era parte) y verifique y fundamente el ordenamiento elegido para
# las series si no lo hizo ya en la 1era parte, teniendo en cuenta que la FIR requiere la identificación del VAR mediante
# la transformación de Cholesky

z1=data.frame(cbind(diff(Raotbl3$lc), diff(Raotbl3$li), diff(Raotbl3$lw)))
names(z1)=c("dlc","dli","dlw")
y=ts(z1, start=c(1967,1), frequency=4)
var1=VAR(y[,1:3],type="const", p=3)

causality(var1,cause="dlc") # Rechazo
causality(var1,cause="dli") # No Rechazo
causality(var1,cause="dlw") # No Rechazo

var2=VAR(y[,2:3],type="const", p=3)
causality(var2,cause="dli") # No Rechazo
causality(var2,cause="dlw") # No Rechazo

z1=data.frame(cbind(diff(Raotbl3$lc), diff(Raotbl3$lw), diff(Raotbl3$li)))
names(z1)=c("dlc","dlw","dli")
y=ts(z1, start=c(1967,1), frequency=4)
var1=VAR(y[,1:3],type="const", p=3)


# 2) Con la totalidad de la muestra, grafique las funciones de impulso respuesta para 10 períodos. A los efectos de tener
# un resultado más contundente (aunque un poco engañoso) tomará un nivel de confianza de 68 % , correspondiente a 1 desvío
# estándar de los residuos, desde la media. Grafique cada FIR en forma individual (son 4 gráficos)

irf.1=irf(var1, ci = .68)
plot(irf.1)
print(irf.1)

# 3) Intente comentar sobre lo que muestran las FIR, por ejemplo:

# a. ¿Cuánto vale el shock inicial sobre cada variable? Recuerde que esto lo puede calcular como la raíz cuadrada de 
# los valores de la diagonal en la matriz de varianzasy covarianzas en el summary(nombre.var). También aparece dicho
# valor cuando se shockea a la propia variable en el caso de que esta sea la más exógena (ver script)

# b. Cuantos trimestres perdura el shock en forma significativa a partir del momento en que se produce el mismo, 
# en la propia serie y en la otra. Recuerde que si la FIR queda dentro de las bandas de confianza, estadísticamente vale 0.

# c. En que porcentaje crece o decrece cada variable en el largo plazo después de cada shock. Aquí utilice 100 períodos 
# y la FIR acumulada. Para hacer más comprensible este valor recuerde que puede normalizar el shock al 1 % 
# (por ejemplo si con un shock de 0.0032 (0.32 %)la variable subió en el largo plazo hasta 0.0121 (1.21 %), ante un shock 
#  del 1 % crecerá 1.21 % *1 / 0.32 = 3.78 % , regla de tres simple). Los valores exactos de la FIR los puede encontrar
# con print(nombre.var$irf) Nota: Recuerde que las unidades de las series dlc y dli son tasas o porcentajes de cambios, 
# para comentar apropiadamente y que el shock tiene como magnitud un desvío estándar de la perturbación promedio 
# (estimada, o sea de los residuos) de la ecuación de la variable shockeada.

# 4) Muestre que un orden inverso de las series en el modelo genera FIR distintas, ya que dependen en forma crucial del 
# orden por la transformación de Cholesky (Puede mostrarlo en forma gráfica o numérica).
