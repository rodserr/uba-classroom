library(pder)
library(plm)
library(lmtest)
library(sandwich)
library(stargazer)   # Para el reporte

# Comparaci�n entre los estimadores MCO, Within y GLS

#**************************************************
# Modelo de Comercio Exterior Kinal y Lahiri (1993)
#**************************************************
data("ForeignTrade", package = "pder")
FT <- pdata.frame(ForeignTrade)
ercomp(imports ~ gnp, FT)
models <- c("within", "random", "pooling")
sapply(models, function(x) coef(plm(imports ~ gnp, FT, model = x))["gnp"])


#**************************************************
#    Modelo bancos    El-Gamal y Inanoglu (2005)  
#**************************************************
data("TurkishBanks", package = "pder")
TurkishBanks <- na.omit(TurkishBanks)
TB <- pdata.frame(TurkishBanks)
ercomp(log(cost)~ log(output), TB)     # Panel no balanceado
sapply(models, function(x)
  coef(plm(log(cost)~ log(output), TB, model = x))["log(output)"])

#**************************************************
#    Modelo Electricidad costo-producci�n
#    Kumbhakar (1996) y Horrace y Schmidt (1996)
#**************************************************
data("TexasElectr", package = "pder")
TexasElectr$cost <- with(TexasElectr, explab + expfuel + expcap)
TE <- pdata.frame(TexasElectr)
ercomp(log(cost)~log(output), TE)
sapply(models, function(x)
  coef(plm(log(cost)~log(output), TE, model = x))["log(output)"])

#**************************************************
#    Modelo Democracia   
#    Acemoglu, Johnson, Robinson y Yared (2008)
#**************************************************
data("DemocracyIncome25", package = "pder")
DI <- pdata.frame(DemocracyIncome25)
ercomp(democracy~lag(income), DI)
sapply(models, function(x)
  coef(plm(democracy~lag(income), DI, model = x))["lag(income)"])


#*******************************************************
#   EJERCICIO DESARROLLADO EN EL CAP. 16 DE 
#         Gujarati y Porter 5ta Edici�n
#*******************************************************
# Cargar archivo de Excel Table 16_1.xls

library(readxl)
Table_16_1 <- read_excel("D:/Acad�mico 1erC 2022/Maestr�a en Datos - An�lisis Longitudinal de Datos/Clases/Clase 5 - 2022 Datos de Panel II/Table 16_1.xls")
View(Table_16_1)

aviones=pdata.frame(Table_16_1,index=c("I","T"))
pdim(aviones)

# I: Identificaci�n de la aerol�nea
# T: Identificaci�n del a�o
# Q: Producci�n, como ingresos por milla por pasajero (�ndice)
# C: Costo total, en miles de d�lares
# PF: Precio del combustible
# LF: Factor de carga, la utilizaci�n promedio de la capacidad de la flotilla.

# Se estima la funci�n de costos de las aerolineas en funci�n
# de las variables mencionadas

#*******************************************************
# (1) Regresi�n MCO (Pooling) 
#*******************************************************
Av_p=plm(C~Q+PF+LF,aviones,model="pooling")
coeftest(Av_p, vcov=vcovHC)

#*******************************************************
# (2a) Regresi�n LSDV con intercepto y 5 dummies
# (La regresi�n LSDV s�lo tiene objetivo did�ctico)
#*******************************************************
Av_d1=lm(C~Q+PF+LF+factor(I),aviones)
coeftest(Av_d1, vcov=vcovHC)

#*******************************************************
# (2b) Regresi�n LSDV con 6 dummies
#*******************************************************
Av_d2=lm(C~Q+PF+LF+factor(I)-1,aviones)
coeftest(Av_d2, vcov=vcovHC)

#*******************************************************
# (3) Regresi�n Within con efectos individuales
#*******************************************************
Av_Wi=plm(C~Q+PF+LF,aviones, model = "within",effect = "individual")
coeftest(Av_Wi, vcov=vcovHC)

# Si quiero ver los coef. dummies versi�n 6 dummies
summary(fixef(Av_Wi))
# Otra forma
summary(fixef(Av_Wi, type="level"))
# Si quiero ver los coef. dummies versi�n 5 dummies
# Pero no reporta el intercepto alfa. Es el primer coeficiente
# del reporte anterior
summary(fixef(Av_Wi,type="dfirst"))
# Si quiero que los interceptos sumen cero
summary(fixef(Av_Wi, type="dmean"))

#*******************************************************
#  Test F para verificar si corresponden efectos individuales
#*******************************************************
#*# Test de variables dummies individuales omitidas
pFtest(Av_Wi,Av_p)

# De acuerdo con los resultados corresponde considerar
# efectos individuales

#*******************************************************
# (4) Modelo de efectos aleatorios - Efectos individuales
#*******************************************************
# El m�todo de estimaci�n para los componentes de la varianza
# en el modelo de efectos aleatorios tiene 4 opciones.
# "swar" (default), "amemiya", "walhus", y "nerlove",

#*******************************************************
# (4a) Modelo de efectos aleatorios - Efectos individuales 
#     Varianza con Swamy y Arora      
#*******************************************************
Av_Ri_a=plm(C~Q+PF+LF,aviones, model = "random", effect="individual")
summary(Av_Ri_a, vcov=vcovHC)

# Tambi�n puedo generar la f�rmula y guardarla en una variable
Fav=C~Q+PF+LF
Av_Ri_a=plm(Fav,aviones, model = "random", effect="individual", 
          random.method="swar")
summary(Av_Ri_a, vcov=vcovHC)

#*******************************************************
# (4b) Modelo de efectos aleatorios - Efectos individuales 
#     Varianza con Amemiya      
#*******************************************************
Av_Ri_b=plm(C~Q+PF+LF,aviones, model = "random", effect="individual", 
          random.method="amemiya")
summary(Av_Ri_b, vcov=vcovHC)

#*******************************************************
# (4c) Modelo de efectos aleatorios - Efectos individuales 
#     Varianza con Walhus      
#*******************************************************
Av_Ri_c=plm(C~Q+PF+LF,aviones, model = "random", effect="individual", 
            random.method="walhus")
summary(Av_Ri_c, vcov=vcovHC)

#*******************************************************
# (4d) Modelo de efectos aleatorios - Efectos individuales 
#     Varianza con Nerlove     
#*******************************************************
Av_Ri_d=plm(C~Q+PF+LF,aviones, model = "random", effect="individual", 
            random.method="nerlove")
summary(Av_Ri_d, vcov=vcovHC)

# Para probar si existen efectos individuales en el contexto
# de Efectos Aleatorios, no se usa el pFtest sino el test
# de Breusch y Pagan
plmtest(Av_p)
# Otra forma
plmtest(Av_p,effect="individual")
# Otra forma
plmtest(C~Q+PF+LF, data=aviones)
# Por tanto, si se eligiera el modelo de Efectos Aleatorios
# corresponder�a poolingi_a

#*******************************************************
# Reporte de lo actuado hasta aqu� con Stagazer
#*******************************************************
rob_p=sqrt(diag(vcovHC(Av_p, type = "HC0")))
rob_Wi=sqrt(diag(vcovHC(Av_Wi, type = "HC0")))
rob_Ri=sqrt(diag(vcovHC(Av_Ri_a, type = "HC0")))
stargazer(Av_p,Av_Wi,Av_Ri_a,
          column.labels=c("MCO","Within","GLS"),
          se=list(rob_p,rob_Wi,rob_Ri),
          title= "Comparaci�n de estimadores",type="text",
          digits=3)

#*******************************************************
#  Test de Hausman para decidir entre modelo de efectos
#  fijos y modelo de efectos aleatorios
#*******************************************************
phtest(Av_Wi, Av_Ri_a)
# Otra forma
phtest(C~Q+PF+LF, data = aviones)

# Hausman test (regression-based)
phtest(C~Q+PF+LF, data = aviones, method = "aux")

# robust Hausman test (regression-based con ajuste por heterocedasticidad)
phtest(C~Q+PF+LF, data = aviones, method = "aux", vco = vcovHC)


#*******************************************************
#            Modelo de Consumo de Gasolina
#         desarrollado en el libro de Baltagi 
#*******************************************************
# Gasoline data
data(Gasoline)
pGas <- pdata.frame(Gasoline, index = c('country', 'year'))
pdim(pGas)
Geq=lgaspcar~lincomep+lrpmg+lcarpcap

# Realizar una regresi�n Pooling
# sumarizar con errores robustos

# Realizar una regresi�n Within con efectos individuales
# sumarizar con errores robustos

# Decidir entre Pooling y Within con el test pFtest

# Realizar una regresi�n GLS (Random) con efectos individuales
# sumarizar con errores robustos

# Decidir entre Pooling y GLS con el test plmtest

# Decidir entre Random y Within con el test de Hausman

# Realizar un reporte con Stargazer


