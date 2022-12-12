library(vars)
# Cargar lutkepohl2.dta , archivo de STATA 
lu=data.frame(lutkepohl2)
# eliminamos los NAs
lu2=subset(lu, !is.na(dln_inc))

# Generamos un data.frame reducido con las series que vamos a usar
z1=data.frame(matrix(c(lu2$dln_inc,lu2$dln_consump,lu2$dln_inv), ncol=3))
names(z1)=c("dln_inc","dln_consump","dln_inv")

# Convertimos en un objeto "series de tiempo"
y=ts(z1,start=c(1960,2),frequency=4)

# Graficamos las series
plot.ts(y)

# Estimamos un VAR con 3 lags con ingreso y consumo
# Lo hacemos con 3 lags segun lo visto en clase anterior
var1=VAR(y[,1:2],type="const", p=3)
summary(var1)

# Causalidad en el sentido de Granger
causality(var1,cause="dln_inc")
causality(var1,cause="dln_consump")

#############################################
#       Funciones de impulso respuesta
############################################
# Normalizaci?n de Cholesky. Impulso, un 
# error est?ndar de la variable impulsora
args(irf)

# Calcula la IRF (todos los impulsos
# y todas las respuestas) con Cholesky (ortho=TRUE)
# Bandas de confianza al 95 % para 10 per?odos (default)
irf.1=irf(var1)
windows()
plot(irf.1)
dev.off()
print(irf.1)
# Interpretaci?n: Respuesta de las series a un 
# impulso de la serie del t?tulo $serie 
# Valor del impulso: 1 desv?o est?ndar de los residuos
# de la ecuaci?n de la serie impulso

# Para que no reporte los valores de las bandas
# de confianza
print(irf.1$irf)
# Fijarse que los desv?os est?ndar de los residuos
# de la ecuaci?n son (ver en el summary)

# Covariance matrix of residuals:
# dln_inc dln_consump
# dln_inc     1.312e-04   6.352e-05
# dln_consump 6.352e-05   9.455e-05

# sd(dln_inc) = 0.01145426
# sd(dln_consump) = 0.009723682

# Solo se aprecia en verdadero valor en la ex?gena
# Puedo cambiar los valores del nivel de confianza
irf.1=irf(var1, ci=0.68, n.ahead=100)
windows()
plot(irf.1)
dev.off()
print(irf.1)
##########################################
# Cambio el orden de las variables
yb=data.frame(cbind(y[ ,2],y[ ,1]))
names(yb)=c("dln_consump","dln_inc")
yb=ts(yb,start=c(1960,2),frequency=4) 

# Fijarse que la estimaci?n no cambia
var1b=VAR(yb,type="const", p=3)
summary(var1b)   # no cambia

# La IRF si cambia
irf.1b <- irf(var1b,nahead=10)  
windows()
plot(irf.1b)        # si cambia
dev.off()
print(irf.1b$irf)

##########################################
# Se puede graficar cada irf individualmente
irf.11=irf(var1, impulse="dln_inc", 
           response="dln_consump")
windows()
plot(irf.11)
dev.off()
# Se puede elegir otras bandas de confianza
# ci=0.95 son dos errores est?ndar
# ci=0.68 es un error est?ndar
irf.12=irf(var1, impulse="dln_inc", 
           response="dln_consump",ci=0.68)
windows()
plot(irf.12)
dev.off()
# Donde las bandas de confianza incluyen al cero
# la IRF es no significativa

##########################################

# Se puede ignorar la covarianza entre los
# errores del modelo reducido. En este caso
# el impulso es de valor = 1 
# No tiene en cuenta el orden de magnitud de
# las variables
irf.1c=irf(var1, ortho=FALSE) 
windows()
plot(irf.1c)
dev.off()
print(irf.1c$irf)
#####################################################
# Funciones de impulso respuesta acumulativa
irf.1d <- irf(var1,impulse="dln_inc", cumulative=TRUE)
windows()
plot(irf.1d)
dev.off()
print(irf.1d)

# Estacionariedad de la respuesta
irf.1e <- irf(var1,impulse="dln_inc", n.ahead=50)
windows()
plot(irf.1e)
dev.off()
print(irf.1e)

irf.1f <- irf(var1,impulse="dln_inc",n.ahead=50,
              cumulative=TRUE)
windows()
plot(irf.1f)
dev.off()
print(irf.1f)

#***********************************************
#    DESCOMPOSICION DE LA VARIANZA 
#***********************************************
# Descomposici?n de la varianza
des.1<-fevd(var1,n.ahead=5)
plot(des.1,addbars=2)
print(des.1)




################################################
#       VAR ESTRUCTURAL
###############################################
# Realizo las restricciones para reproducir Cholesky
# La matriz Amat es nuestra matriz B
amat <- diag(2)
diag(amat) <- NA
amat[2,1]=NA
amat
# La matriz Bmat es la que plantea la no covarianza
# de los shocks entre ecuaciones estructuales
bmat=diag(2)
bmat
diag(bmat)=NA
bmat
args(SVAR)
svar1 <- SVAR(var1, estmethod = "direct", Amat = amat, Bmat=bmat)
summary(svar1)
irf(svar1)   # igual que Cholesky
# plot(irf(svar1))

##########################################
# Ahora realizo las restricciones para suponer que
# ambos(b12 y b21) son cero
amat2 <- diag(2)
diag(amat2) <- NA
# amat[2,1]=NA
amat2
# La matriz Bmat es la que plantea la no covarianza
# de los shocks entre ecuaciones estructuales
bmat2=diag(2)
diag(bmat2)=NA
bmat2
svar2 <- SVAR(var1, estmethod = "direct", Amat = amat2, Bmat=bmat2)
irf(svar2)
# plot(irf(svar1))
summary(svar2)
# Compara el VAR exactamente identificado con el 
# sobreidentificado (Test LR)
svar2$LR$p.value       # rechaza
##############################################
# En vez de realizar la inversi?n del orden de las series
# puedo plantear un Cholesky al rev?s
amat3 <- diag(2)
diag(amat3) <- NA
amat3[1,2]=NA
amat3
# La matriz Bmat es la que plantea la no covarianza
# de los shocks entre ecuaciones estructuales
bmat3=diag(2)
diag(bmat3)=NA
bmat3
svar3 <- SVAR(var1, estmethod = "direct", Amat = amat3, Bmat=bmat3)
summary(svar3)
irf(svar3)   # 
# plot(irf(svar1))

#**********************************
#*        Trabajo en clase
#**********************************
#*Cargar el archivo Housing.dta
#*Y realizar las IRF del VAR identificado
#*con Cholesky
library(vars)
# Cargar Housing.dta
# Borro month
z=subset(Housing, select=-c(month))

# Genero un objeto ts
y_tot=ts(z,start=c(1968,1),frequency=12)
# estimo un VAR con los lags indicados por AKAIKE
var1=VAR(y_tot,type="const", lag.max=12, ic="AIC")
summary(var1)

# Paso por alto los diagn?sticos, salvo 
# Causalidad en el sentido de Granger
causality(var1,cause="starts")
# starts Granger-causa a comps
causality(var1,cause="comps")
# comps no Granger-causa a starts
# Por tanto inc es mas "ex?gena"

# .......
# Recuperar las IRF



