# Library----
library(tidyverse)
library(lmtest)
library(sandwich)
library(stargazer)
library(AER)

# a) Cargar datos -----
data <- haven::read_stata('econometria/datos/mus06data.dta')

# b) Limpiar NAs-----
data <- subset(data, !is.na(linc))

# c) ¿Qué signos se esperan para el resto de los coeficientes?-----
# Para totchr se esperaria coeficiente positivo (mientras mas enfermedades padece el individuo mayor la el gasto en medicamentos)
# Para la edad se esperaria coeficiente positivo ya que es menos probable que la persona este asegurada ya que no entra en la edad aceptada o ya no tiene trabajo 
# sin embargo tambien podria darse el caso de que la persona mayor cuente con el seguro social
# Para female esperaria coeficiente positivo ya que por tema de sesgo de genero las mujeres tienen peores condiciones laborales
# blhisp se espera que sea coeficiente positivo ya que las condiciones laborales son mas precarias por lo que menos personas cuentan con seguros
# linc se esperaria negativo, ya que mientras mayor el ingreso mayor la probabilidad de adquirir un seguro y no tener que costear medicamentos

# d) Realizar la regresión de ldrugexp sobre hi_empunion, totchr, age, female, blhisp y linc----
summary(mco_p <- lm(ldrugexp~hi_empunion+totchr+age+female+blhisp+linc, data=data))
bptest(mco_p)

# e) Idem anterior pero ahora con errores estándar robustos-----
coeftest(mco_p, vcov = vcovHC(mco_p, type="HC1"))
# Los coeficientes de significatividad practicamente no se vieron afectados

# f) En un par de renglones intente una interpretación, con sus unidades, de los betas estimados----
# El coeficiente obtenido para hi_empunion indica las personas que cuentan con seguro medico pagan 7% mas que las que no cuentan con seguro medico
# El coeficiente obtenido para totchr indica que por cada enfermedad cronica que padezca la persona paga 44% de mas 
# El coeficiente obtenido para age indica que por cada anio de edad la persona paga 0,35% menos
# el coeficiente female indica que las mujeres pagan 5,7% mas que los hombres
# el coeficiente blhisp indica que las personas de raza negra o hispana, pagan 15% unidades menos que las personas que no son de esa raza
# La variable linc no es estadisticamente significativa

# g) Instrumentar la variable hi_empunion con la variable ssiratio----
summary(iv_1 <- ivreg(ldrugexp~hi_empunion+totchr+age+female+blhisp+linc|ssiratio+totchr+age+female+blhisp+linc, data=data))

# h) Repetir la regresión anterior pero obtener errores estándar robustos utilizando: summary(reg, vcov=sandwich)----
summary(iv_1, vcov=sandwich)

# i) Realizar el test de Hausman cuya H 0 es que la variable instrumentada es en realidad exógena----
summary(iv_1, vcov=sandwich, diagnostics = T)
# El test de Hausman rechaza lo que indica que la variable hi_empunion si es endógena, por lo que si es correcto instrumentarla

# j) Instrumentar la variable hi_empunion con las 4 variables disponibles (ssiratio, lowincome, firmsz y multlc)----
summary(iv_2 <- ivreg(ldrugexp~hi_empunion+totchr+age+female+blhisp+linc|ssiratio+lowincome+firmsz+multlc+totchr+age+female+blhisp+linc, data=data))

# k) Idem anterior pero obtener errores estándar robustos.----
summary(iv_2, vcov=sandwich)
# No hay mucha diferencia en los coeficientes, ni en los errores estándar comparados con la regresion realizada en h)

# l) Agregar los diagnósticos que informan, además del resultado del test de Wu - Hausman, el test de Sargan y el test de instrumentos débiles.----
summary(iv_2, vcov=sandwich, diagnostics=T)
# El test de Weak Instrument y el test de Hausman rechazan, lo que confirma la caracteristica endógena de la variable hi_empunion y validan los intrumentos
# Sin embargo el test de Sargan rechaza, lo que indica que al menos un instrumento es endógeno, cuando se remueve el instrumento lowincome el test de Sargan no rechaza, 
# lo cual valida la regresión. Esto indica ademas que la variable lowincome es  endógena

summary(iv_3 <- ivreg(ldrugexp~hi_empunion+totchr+age+female+blhisp+linc|ssiratio+firmsz+multlc+totchr+age+female+blhisp+linc, data=data))
summary(iv_3, vcov=sandwich, diagnostics=T)

# la variable female no es estadisticamente significativa, por lo cual para la regresion final no se toma en cuenta
summary(iv_4 <- ivreg(ldrugexp~hi_empunion+totchr+age+blhisp+linc|ssiratio+firmsz+multlc+totchr+age+blhisp+linc, data=data))
summary(iv_4, vcov=sandwich, diagnostics=T)

# m) Con la metodología de regresión en 2 etapas explicada en la clase teórica realice la regresión pedida en el item 2-g----

# 1) Regrese la variable hi_empunion sobre la variable ssiratio
data_g_1 <- data
reg_g_1 <- lm(hi_empunion~ssiratio, data=data_g_1)
data_g_1$hi_empunion_hat=reg_g_1$fitted.values
data_g_1$hi_empunion_res=reg_g_1$residuals

summary(reg_g_1_2 <- lm(ldrugexp~hi_empunion_hat+totchr+age+female+blhisp+linc, data=data_g_1))

# 2) Regrese la variable hi_empunion sobre el resto de las variables de la ecuación más ssiratio
data_g_2 <- data
reg_g_2 <- lm(hi_empunion~ssiratio+totchr+age+female+blhisp+linc, data=data_g_2)
data_g_2$hi_empunion_hat=reg_g_2$fitted.values
data_g_2$hi_empunion_res=reg_g_2$residuals

summary(reg_g_2_2 <- lm(ldrugexp~hi_empunion_hat+totchr+age+female+blhisp+linc, data=data_g_2))
coeftest(reg_g_2_2, vcov = vcovHC(reg_g_2_2, type="HC1"))

# Es de notar que la 2da regresion de este apartado es la que coincide con los resultados de la regresion realizasa con el comando ivreg
# ya que utiliza no solo el instrumento sino las demas variables regresoras


stargazer(mco_p, iv_4, reg_g_2_2, type="text")
