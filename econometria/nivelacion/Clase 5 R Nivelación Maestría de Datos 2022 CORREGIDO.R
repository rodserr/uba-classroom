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

# ¿Y si hubiera eliminado p_vac?
reg_p3=lm(q_pollo~ingreso+p_pollo+ p_cerdo, data=pollos)
summary(reg_p3)
# Queda todo significativo

# Esto indica la posibilidad de que ambas variables
# estén aportando explicación a q_pollo pero  haya 
# un problema de multicolinealidad

# Verificar las correlaciones entre las variables 
cor(pollos)
# Evidentemente la alta correlación entre ambas genera
# el fenómeno de multicolinealidad.

# Si se desea pronosticar, me quedo con reg_p
# Si deseo depurar la regresión de cualquier variable
# no significativa, me quedo con reg_p2 (mayor R^2)

# Verificar la forma funcional con el test RESET de Ramsey
resettest(reg_p2, 2:3, type=c("fitted"))
resettest(reg_p2, 2, type=c("fitted"))
resettest(reg_p2, 2, type=c("regressor"))

#*************************************************
#*         Prueba de especificación
#*************************************************
# Verificar la forma funcional con el test RESET de Ramsey
# Agregando las potencias 2 y 3 de Y_hat
resettest(reg_p2, 2:3, type=c("fitted"))
# rechazo (la especificación no se valida)

# Agregando sólo la potencia 2 de Y_hat
resettest(reg_p2, 2, type=c("fitted"))   
# rechazo (la especificación no se valida)

# Agregando la potencia 2 de cada explicativa
resettest(reg_p2, 2, type=c("regressor"))
# rechazo (la especificación no se valida)

# Pruebo con otra especificación, por ejemplo logaritmos
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
# Conclusión: Puedo eliminar ambas de una sola vez
# Probar de todas maneras eliminar una cada vez y 
# verificar que ambas se deben eliminar

reg_lp2=lm(log(q_pollo)~log(ingreso)+log(p_pollo), data=pollos)
summary(reg_lp2)

# Verifico la especificación con el test RESET
# Con las potencias 2 y 3 de Y_hat
resettest(reg_lp2, 2:3, type=c("fitted"))
# No rechaza (la especificación se valida)

# Con la potencia 2 de Y_hat
resettest(reg_lp2, 2, type=c("fitted"))
# No rechaza (la especificación se valida)

#**********************************************
# Verificar normalidad en ambas especificaciones
#**********************************************
# install.packages("moments")
library("moments")
# Test de Jarque Bera
jarque.test(reg_lp2$residuals)
# No rechazo normalidad

# Indicativamente puedo testear normalidad
# en la especificación en niveles
jarque.test(reg_p2$residuals)
# No rechazo normalidad

#################################################################
#  Variables dummies
# Importar Datos_EPH_CABA_3Tri2021.xls
mincer=Datos_EPH_CABA_3Tri2021

# Regresión agrupada
rminc1=lm(log(salario) ~ educa+edad+I(edad^2), data = mincer)
summary(rminc1)

# Regresión con distinción de género en el intercepto
rminc2=lm(log(salario) ~ hombre+educa+edad+I(edad^2), data = mincer)
summary(rminc2)

# Regresión con distinción de género en el la pendiente (de educa)
rminc3=lm(log(salario) ~ educa+I(educa*hombre)+edad+I(edad^2), data = mincer)
summary(rminc3)

# Regresión con distinción de género en el intercepto y
# en la pendiente de educa
rminc4=lm(log(salario) ~ hombre+educa+I(educa*hombre)+edad+
            I(edad^2), data = mincer)
summary(rminc4)

# En realidad habría que haber empezado por esta última y luego
# depurar la regresión de las variables no relevantes

rminc5=lm(log(salario) ~ hombre+educa+I(educa*hombre)+edad+
            I(edad^2), data = mincer)
summary(rminc5)

# Predecir para mujeres y varones el salario, considerar
# 14 años de educacion y 40 años de edad.
# Usamos predict()
head(mincer,5)
head(predict(rminc5,mincer),5)
# Fijarse que coincide con los valores de y_hat
# que los encuentro en rminc4$fittedvalues
head(rminc5$fitted.values,5)

# Pero para tener los salarios y no log(salario) debo
# calcular exp(log(salario))
head(exp(predict(rminc4,mincer)),5)

# Para resolver con las características deseadas
# Se puede generar un nuevo data frame 
mincer_pred=mincer[1:2,]
mincer_pred=edit(mincer_pred)
predict(rminc5,mincer_pred)
exp(predict(rminc5,mincer_pred))

# Calcular
# A qué edad, esta deja de aumentar el salario y 
# comienza a disminuirlo

# Practica
# Cargar el archivo "Tablas Libro Karlaftis et al.xls"
# y realizar una regresión que explique el Tránsito Vehicular Diario
# por la población del condado y el tipo de carretera (4 dummies)
