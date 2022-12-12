#***********************************************
#     Vectores Autorregresivos VAR
#***********************************************
# install.packages("vars")
library(vars)

# Cargar lutkepohl2 archivo de STATA
lu=data.frame(lutkepohl2)
# eliminamos los NAs guiandonos con la serie de dln_inc 
# diferencia del logaritmo del PBI, o sea tasa de crecimiento
lu2=subset(lu, !is.na(dln_inc))

# Generamos un data.frame reducido con las series que vamos a usar
z1=data.frame(matrix(c(lu2$dln_inc,lu2$dln_consump,lu2$dln_inv), ncol=3))
names(z1)=c("dln_inc","dln_consump","dln_inv")

# Convertimos en un objeto "series de tiempo"
y=ts(z1,start=c(1960,2),frequency=4)


windows()
plot.ts(y)
dev.off()

# ploteo dl_inc en azul y dl_consump en rojo 
windows()
plot.ts(y[,1], col="blue")
lines(y[,2], col="red")
dev.off()

#***********************************************
#*      DISE?O DEL MODELO
#***********************************************     
# Queremos encontrar las relaciones entre el Consumo
# y el Ingreso. Al ser ambas series I(1), trabajaremos
# con sus diferencias logar?tmicas que ser?n I(0)
# DEBER?AMOS COMPROBAR LO ANTERIOR

#***********************************************
#     Estimaci?n del  VAR
#***********************************************
# Estimamos un VAR con nro. de lags elegidos con Akaike
# Utilizamos el comando VAR() del paquete "vars"
args(VAR)
var1=VAR(y[,1:2],type="const", lag.max=8, ic="AIC")
summary(var1)

# Tambi?n se puede correr el argumento VARselect que 
# selecciona los lags seg?n un criterio 
args(VARselect)
ic=VARselect(y[,1:2],lag.max=8, type="const")
ic$selection

# C?mo es la secuencia de c?lculos de VARselect
VARselect(y[,1:2],lag.max=8, type="const")

# Y luego fijamos la cantidad de lags con p=3
# que fue lo indicado por VARselect
var1=VAR(y[,1:2],type="const", p=3)
summary(var1)

# Se puede agregar una ex?gena
var2=VAR(y[,1:2],type="const", p=3,exogen=y[,3])
summary(var2)
# Como no encontró el nombre la llamó exo1

#***********************************************
#              Diagn?sticos
#***********************************************

# Estabilidad. 
# Reporta las ra?ces inversas
# Si en m?dulo son menores que 1 el VAR es estable
roots(var1)

# Correlaci?n serial
# El comando que propone el paquete es "serial.test"
args(serial.test)
st=serial.test(var1,type = "PT.asymptotic",lags.pt=8)
serial.test(var1,type = "PT.asymptotic",lags.pt=8)
windows()
plot(st, names="dln_inc")
plot(st, names="dln_consump")
dev.off()

# Mejor cargar vcorr_res que calcula los estad?sticos
# para varios lags
# Está programado con serial.test, pero tiene los 
# resultados para varios lags.
# Se utiliza el test Q (Portmanteau) en su versión
# original o ajustada por grados de libertad
vcorr_res(var1,12,"PT.adjusted")
# Para testear los rezagos faltantes
# se usa el test de Breusch y Godfrey en su 
# versión original o ajustada
vcorr_res(var1,12,"BG")
vcorr_res(var1,12,"ES")

# Testeo de heterocedasticidad ARCH
# No hay en el paquete un testeo de heterocedasticidad
# del tipo White
args(arch.test)
t.arch<-arch.test(var1,lags.multi=5, multivariate.only=FALSE)
t.arch

# Test de normalidad
# Es una versión multivariada del test de 
# Jarque y Bera
args(normality.test)
t.norm<-normality.test(var1, multivariate.only=FALSE)
t.norm
windows()
hist(t.norm$resid[,1])
hist(t.norm$resid[,2])
dev.off()

#***********************************************
# Causalidad en el sentido de Granger
#***********************************************
args(causality)
# Testea Causalidad en el sentido de Granger
# y "causalidad instantanea" que ser?a correlaci?n 
# contempor?nea de las variables
causal1<-causality(var1,cause="dln_inc")
causality(var1,cause="dln_inc")
causal2<-causality(var1,cause="dln_consump")
causality(var1,cause="dln_consump")


#***********************************************
#              Predicci?n
#***********************************************
# Predicci?n ex-ante, es decir hacia el futuro
# no conocido de las series
predictions<-predict(var1,n.ahead=25, ci=0.95)
windows()
plot(predictions,names="dln_inc")
plot(predictions,names="dln_consump")
dev.off()

# Predicci?n ex post. Es decir corto la muestra
# para poder comparar el pronóstico con los 
# valores disponibles para el períoso pronosticado
# Extraigo un data set parcial hasta 1980q4
y_reg=ts(y, start=c(1960,2), end=c(1980,4) , frequency=4)

# Estimo el VAR con los datos hasta 1980
var3=VAR(y_reg[,1:2],type="const", lag.max=8, ic="AIC")
summary(var3)

# Realizo una predicci?n para los 8 trimestres faltantes
pred3<-predict(var3,n.ahead=8)

# Grafico la serie y su pron?stico

# Ingreso
plot(y[,1])
plot(pred3,names="dln_inc")
lines(y[,1])      
dev.off()

#Consumo
plot(y[,2])  
plot(pred3,names="dln_consump")
lines(y[,2])      
dev.off()

#***********************************************
#  Tarea pr?ctica
#  Estimar un VAR en diferencias de los datos de
#  Raotbl3
#  Correr los diagn?sticos
#  * Estacionariedad
#  * No autocorrelaci?n
#  * Causalidad 
#  * Normalidad
#  Predecir el per?odo 
#***********************************************
library(urca)
data(Raotbl3)
attach(Raotbl3)
# Carga las series macro de UK
# Consumo
dlc=diff(lc)
dli=diff(li)
dlw=diff(lw)

# Generamos un data.frame con las series que vamos a usar
z1=data.frame(cbind(dlc, dli,dlw))
names(z1)=c("dlc","dli","dlw")

# Convertimos en un objeto "series de tiempo"
y=ts(z1,start=c(1967,1),frequency=4)
