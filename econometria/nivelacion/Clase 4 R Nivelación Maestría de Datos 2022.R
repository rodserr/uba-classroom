# Importar Table_7_9_Modificada.xls utilizada en la clase 1
pollos=Table_7_9_Modificada
names(pollos)=c("anio", "q_pollo", "ingreso", "p_pollo" , "p_cerdo", "p_vac", "p_subs") 
reg_p=lm(q_pollo~ingreso+p_pollo+ p_cerdo+p_vac, data=pollos)
summary(reg_p)

# install.packages("lmtest")
# Usaremos de este paquete los comandos
# waldtest() y lrtest()
library(lmtest)

#*****************************************
# Test F de significatividad global
#*****************************************
#*#*El uso generalizado es Test F para muestra chica
#*con normalidad y el Test de Wald para muestra grande
#*
# Con el estad�stico F
waldtest(reg_p, test="F")  

# Idem pero con M�xima Verosimilitud Wald Test
waldtest(reg_p, test="Chisq")

# Idem pero con M�xima Verosimilitud LR Test
# Test de Raz�n de Verosimilitud
lrtest(reg_p)

#*****************************************
#       Test de Variables Redundantes o 
#       Test de Variables Omitidas
#*****************************************
#*El uso generalizado es Test F para muestra chica
#*con normalidad y el Test LR para muestra grande
#*
# Variables redundantes con estad�stico F
waldtest(reg_p,c(3,4),test="F")

# Variables redundantes con estad�stico Wald
waldtest(reg_p,c(3,4),test="Chisq")

# Variables redundantes con estad�stico LR de
# Raz�n de Verosimilitud
lrtest(reg_p,c(3,4))

# install.packages("car")
library(car)
# Usaremos de este paquete el comando
# linearHypothesis()
#*****************************************
#*# Test de restricciones lineales
#*****************************************
#*#*El uso generalizado es Test F para muestra chica
#*con normalidad y el Test de Wald para muestra grande
#*
#*
# Test de restricciones lineales (default F)
linearHypothesis(reg_p, "p_cerdo-p_vac=0")

# Test de restricciones lineales (Wald Chi cuadrado)
linearHypothesis(reg_p, "p_cerdo-p_vac=0", test="Chisq")

# install.packages("strucchange")
library(strucchange)
# Usaremos de este paquete el comando
# sctest()

#*****************************************
#*Test de Cambio Estructural (Chow Test)
#*#***************************************
#*El uso generalizado es Test F para muestra chica
#*con normalidad y el Test de Wald para muestra grande
#*
# Test de Chow con estad�stico F
# "point" indica la �ltima observaci�n del primer tramo 
sctest(q_pollo~ingreso+p_pollo+ p_cerdo+p_vac, 
       data=pollos, type="Chow", point=10)

# Test de Chow con estad�stico Wald
sctest(q_pollo~ingreso+p_pollo+ p_cerdo+p_vac, 
       data=pollos, type="Chow", asymptotic=T, point=10)

# Tarea pr�ctica
# Con el archivo Tabla_11_7_Gujarati utilizado en la clase 3 y TD1
# 1) Hacer una regresi�n que explique el rendimiento con base
# en las caracter�sticas de los veh�culos
# 2) Verificar la significatividad global de la regresi�n
# con todos los tests disponibles
# 3) Verificar la significatividad conjunta de las variables 
# f�sicas (Peso y Volumen). Usar todos los tests disponibles
# 4) Verificar si el coeficiente correspondiente al Peso es igual
# al coeficiente correspondiente a Velocidad M�xima
# 5) Dado que la tabla est� ordenada por Peso de los veh�culos, 
# Verificar si existe un cambio estructural al pasar de 35 a 40 
# cientos de libras
