# Importar Table_7_9_Modificada.xls
pollos=Table_7_9_Modificada
names(pollos)=c("anio", "q_pollo", "ingreso", "p_pollo" , "p_cerdo", "p_vac", "p_subs") 
reg_p=lm(q_pollo~ingreso+p_pollo+ p_cerdo+p_vac, data=pollos)
summary(reg_p)

# Verificar si ingreso, p_vac y p_subs son conjuntamente no significativas
# Con test F
# install.packages("lmtest")
library(lmtest)

waldtest(reg_p, c(1,4),test="F")
# Con test lr
lrtest(reg_p, c(1,4))
# El resultado (rechazo) me indica que  no puedo eliminar
# ambas variables de una vez

# Pruebo de eliminar "ingreso"
reg_p2=lm(q_pollo~p_pollo+ p_cerdo+p_vac, data=pollos)
summary(reg_p2)
# Queda todo significativo

# �Y si hubiera eliminado p_vac?
reg_p3=lm(q_pollo~ingreso+p_pollo+ p_cerdo, data=pollos)
summary(reg_p3)
# Queda todo significativo

# Esto indica la posibilidad de que ambas variables
# est�n aportando explicaci�n a q_pollo pero  haya 
# un problema de multicolinealidad

# Verificar las correlaciones entre las variables 
cor(pollos)
# Evidentemente la alta correlaci�n entre ambas genera
# el fen�meno de multicolinealidad.

# Si se desea pronosticar, me quedo con reg_p
# Si deseo depurar la regresi�n de cualquier variable
# no significativa, me quedo con reg_p2 (mayor R^2)

# Verificar la forma funcional con el test RESET de Ramsey
resettest(reg_p2, 2:3, type=c("fitted"))
resettest(reg_p2, 2, type=c("fitted"))
resettest(reg_p2, 2, type=c("regressor"))

#*************************************************
#*         Prueba de especificaci�n
#*************************************************
# Verificar la forma funcional con el test RESET de Ramsey
# Agregando las potencias 2 y 3 de Y_hat
resettest(reg_p2, 2:3, type=c("fitted"))
# rechazo (la especificaci�n no se valida)

# Agregando s�lo la potencia 2 de Y_hat
resettest(reg_p2, 2, type=c("fitted"))   
# rechazo (la especificaci�n no se valida)

# Agregando la potencia 2 de cada explicativa
resettest(reg_p2, 2, type=c("regressor"))
# rechazo (la especificaci�n no se valida)

# Pruebo con otra especificaci�n, por ejemplo logaritmos
reg_lp=lm(log(q_pollo)~log(ingreso)+log(p_pollo)+
            log( p_cerdo)+log(p_vac), data=pollos)
summary(reg_lp)

# Verificar si los precios de cerdo y vaca son conjuntamente
# no significativos
# Con test F
waldtest(reg_lp, c(3,4),test="F")
# No rechazo (las variables son redundantes)

# Con test lr
lrtest(reg_lp, c(3,4))
# No rechazo (las variables son redundantes)
# Conclusi�n: Puedo eliminar ambas de una sola vez
# Probar de todas maneras eliminar una cada vez y 
# verificar que ambas se deben eliminar

reg_lp2=lm(log(q_pollo)~log(ingreso)+log(p_pollo), data=pollos)
summary(reg_lp2)

# Verifico la especificaci�n con el test RESET
# Con las potencias 2 y 3 de Y_hat
resettest(reg_lp2, 2:3, type=c("fitted"))
# No rechaza (la especificaci�n se valida)

# Con la potencia 2 de Y_hat
resettest(reg_lp2, 2, type=c("fitted"))
# No rechaza (la especificaci�n se valida)

#**********************************************
# Verificar normalidad en ambas especificaciones
#**********************************************
# install.packages("moments")
library("moments")
# Test de Jarque Bera
jarque.test(reg_lp2$residuals)
# No rechazo normalidad

# Indicativamente puedo testear normalidad
# en la especificaci�n en niveles
jarque.test(reg_p2$residuals)
# No rechazo normalidad

#################################################################
#  Variables dummies
# Importar Datos_EPH_CABA_3Tri2021.xls
mincer=Datos_EPH_CABA_3Tri2021

# Regresi�n agrupada
rminc1=lm(log(salario) ~ educa+edad+I(edad^2), data = mincer)
summary(rminc1)

# Regresi�n con distinci�n de g�nero en el intercepto
rminc2=lm(log(salario) ~ hombre+educa+edad+I(edad^2), data = mincer)
summary(rminc2)

# Regresi�n con distinci�n de g�nero en el la pendiente (de educa)
rminc3=lm(log(salario) ~ educa+I(educa*hombre)+edad+I(edad^2), data = mincer)
summary(rminc3)

# Regresi�n con distinci�n de g�nero en el intercepto y
# en la pendiente de educa
rminc4=lm(log(salario) ~ hombre+educa+I(educa*hombre)+edad+
            I(edad^2), data = mincer)
summary(rminc4)

# En realidad habr�a que haber empezado por esta �ltima y luego
# depurar la regresi�n de las variables no relevantes

rminc5=lm(log(salario) ~ hombre+educa+I(educa*hombre)+edad+
            I(edad^2), data = mincer)
summary(rminc5)

# Predecir para mujeres y varones el salario, considerar
# 14 a�os de educacion y 40 a�os de edad.
# Usamos predict()
head(mincer,5)
head(predict(rminc5,mincer),5)
# Fijarse que coincide con los valores de y_hat
# que los encuentro en rminc4$fittedvalues
head(rminc5$fitted.values,5)

# Pero para tener los salarios y no log(salario) debo
# calcular exp(log(salario))
head(exp(predict(rminc4,mincer)),5)

# Para resolver con las caracter�sticas deseadas
# Se puede generar un nuevo data frame 
mincer_pred=mincer[1:2,]
mincer_pred=edit(mincer_pred)
predict(rminc5,mincer_pred)
exp(predict(rminc5,mincer_pred))

# Calcular
# A qu� edad, esta deja de aumentar el salario y 
# comienza a disminuirlo

# Practica
# Cargar el archivo "Tablas Libro Karlaftis et al.xls"
# y realizar una regresi�n que explique el Tr�nsito Vehicular Diario
# por la poblaci�n del condado y el tipo de carretera (4 dummies)
