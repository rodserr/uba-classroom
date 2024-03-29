#**************************************************
#*   Clase4   -  DATOS DE PANEL I  
#**************************************************     

# install.packages("pder")
# install.packages("plm")

library(pder)
library(plm)
# options(scipen = 99) 

#**************************************************
#*        EJERCICIO   1
#**************************************************     
# Cargamos el archivo de datos
# Schaller (1990) para testear la teor�a de las inversiones Tobin (1969)
data("TobinQ", package = "pder")
# se declara como panel y se indican las variables indizadoras
pTobinQ <- pdata.frame(TobinQ, index = c('cusip', 'year'))
pdim(pTobinQ)

# cargo el modelo
# ikn = Inversi�n/Capital
# qn = Q de Tobin (Valor de mercado de la firma/Valor del capital f�sico)
# O sea es la Tasa de inversi�n explicada por la Q de Tobin de la empresa

Qeq <- ikn ~ qn
# Regresi�n MCO agrupada
Q.pooling <- plm(Qeq, pTobinQ, model = "pooling")
print(Q.pooling)
summary(Q.pooling)
SRC_MCO=deviance(Q.pooling)

Q.MCO <- lm(Qeq, data=TobinQ)
summary(Q.MCO)

# Regresi�n Within
Q.within <- plm(Qeq,pTobinQ, model = "within")
Q.within <- update(Q.pooling, model = "within")
print(Q.within)
summary(Q.within)
SRC_W=deviance(Q.within)

# Tambi�n puede realizarse la regresi�n con LSDV

Q.LSDV <- lm( ikn ~ qn+factor(cusip), data=TobinQ)
summary(Q.LSDV)

# Test de variables omitidas
# gl1=gl(MCO)-gl(Within)
gl1=Q.pooling$df.residual - Q.within$df.residual
# gl2=gl(Within)
gl2=Q.within$df.residual
FE=((SRC_MCO-SRC_W)/gl1)/(SRC_W/gl2)
FE

# Hay un comando pre programado para hacer el test
pFtest(Q.within,Q.pooling)

#**************************************************
#*        EJERCICIO   2
#**************************************************        
# abrir archivo de stata mus08_stata.dta
pStata<- pdata.frame(mus08_stata, index = c("idnumber", "survey"))
pdim(pStata)
Seq=lwage~ ed + exp+ exp2+ wks

# Pooling
S.pooling <- plm( Seq, data=pStata,model = "pooling")
summary(S.pooling)
SRC_MCO=deviance(S.pooling)

S.within <- plm( Seq, data=pStata,model = "within")
summary(S.within)

# Fijarse que como, al momento de ser encuestados,los individuos
# ya completaron su educaci�n, la variable "ed" es constante
# para cada individuo, por lo que no pueden estimarse los retornos
# a la educaci�n

SRC_W=deviance(S.within)

# Test de variables omitidas
# gl1=gl(MCO)-gl(Within)
gl1=S.pooling$df.residual - S.within$df.residual
# gl2=gl(Within)
gl2=S.within$df.residual
FE=((SRC_MCO-SRC_W)/gl1)/(SRC_W/gl2)
FE

# p-value
alfa=0.05
FC=qf((1-alfa),gl1,gl2)
FC
pv_F=1-pf(FE,gl1,gl2)
pv_F

# Hay un comando pre programado para hacer el test
pFtest(S.within,S.pooling)

#**************************************************
#*        EJERCICIO   3
#**************************************************        
# Gasoline data
data(Gasoline)
pGas <- pdata.frame(Gasoline, index = c('country', 'year'))
pdim(pGas)
Geq=lgaspcar~lincomep+lrpmg+lcarpcap

# Realizar las regresiones de los modelos Pooling (Within)
# y Efectos Fijos (MCO) y realizar el test de existencia 
# de efectos fijos. Hacer un informe

#*******************************************************
#              EJERCICIO   4   
#*******************************************************
# EJERCICIO DESARROLLADO EN EL CAP. 16 DE 
# Gujarati y Porter 5ta Edici�n

# Cargar archivo de Excel Table 16_1.xls
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

#*******************************************************
# (2a) Regresi�n LSDV con intercepto y 5 dummies
# (La regresi�n LSDV s�lo tiene objetivo did�ctico)
#*******************************************************
Av_d1=lm(C~Q+PF+LF+factor(I),aviones)

#*******************************************************
# (2b) Regresi�n LSDV con 6 dummies
#*******************************************************
Av_d2=lm(C~Q+PF+LF+factor(I)-1,aviones)
coeftest(Av_d2, vcov=vcovHC)

#*******************************************************
# (3) Regresi�n Within con efectos individuales
#*******************************************************
Av_Wi=plm(Fav,aviones, model = "within",effect = "individual")

#*******************************************************
#  Test F para verificar si corresponden efectos individuales
#*******************************************************
#*# Test de variables dummies individuales omitidas
pFtest(Av_Wi,Av_p)
