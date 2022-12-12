# Librerias------------
library(tidyverse)
library(janitor)
library(readxl)
library(lmtest)
library(moments)
library(sandwich)
library(nlme)

############################### Ejercicio 1----
######### a) Importar Datos
datos_1 <- read_excel("econometria/datos/Tabla 11_7 Gujarati.xls", skip = 4) %>% clean_names()

######### b) Regresion
summary(reg_1 <- lm(rendimiento~potencia+peso+vel_max+volumen, data = datos_1))

######### c) Determinar si la especificación del modelo es correcta utilizando el test RESET, con la potencia 2
resettest(reg_1, 2, type=c("fitted"))
# rechazo (la especificaci?n no se valida)

######### d) Probar otras especificaciones
### log-log
summary(reg_1_loglog <- lm(log(rendimiento)~log(potencia)+log(peso)+log(vel_max)+log(volumen), data = datos_1))

### log-lin 
summary(reg_1_loglin <- lm(log(rendimiento)~potencia+peso+vel_max+volumen, data = datos_1))

### lin-log
summary(reg_1_linlog <- lm(rendimiento~log(potencia)+log(peso)+log(vel_max)+log(volumen), data = datos_1))

### Validacion
resettest(reg_1_loglog, 2, type=c("fitted"))
resettest(reg_1_loglin, 2, type=c("fitted"))
resettest(reg_1_linlog, 2, type=c("fitted"))
##### Solo valida el modelo loglin

######### e) Con el modelo mejor especificado, verificar los signos de los coeficientes estimados y su significatividad individual.
summary(reg_1_loglin)$coefficients

######### f) Verificar la bondad del ajuste y la significatividad global de la regresión
significatividad_global <- function(reg_fit){
  
  fstat <- summary(reg_fit)$fstatistic
  
  pf(fstat['value'], fstat['numdf'], fstat['dendf'], lower.tail=FALSE)
  
}
summary(reg_1_loglin)$r.squared
significatividad_global(reg_1_loglin)

######### g) Verificar la normalidad de las perturbaciones con el test Jarque-Bera
jarque.test(reg_1_loglin$residuals)
# Rechazo Normalidad

######### h) Verificar la existencia de Heterocedasticidad con el test de Breusch-Godfrey, White
### Breusch-Godfrey
bptest(reg_1_loglin)
# Rechazo, es decir Existe Heterocedasticidad 

### White
bptest(log(rendimiento)~potencia+I(potencia^2)+peso+I(peso^2)+vel_max+I(vel_max^2)+volumen+I(volumen^2), data=datos_1) 
# Rechazo, es decir Existe Heterocedasticidad 

### Corrigo Heterocedasticidad (Plan B)
summary(reg_1_loglin)
coeftest(reg_1_loglin, vcov = vcovHC(reg_1_loglin, type="HC1"))

######### i) Verificar la existencia de Autocorrelación en las perturbaciones
### Durbin y Watson
dwtest(reg_1_loglin, alternative="greater")
# Rechazo, es decir hay autocorrelacion positiva

### Breusch y Godfrey
bgtest(reg_1_loglin, order=1)
# Rechazo, es decir hay autocorrelacion positiva

### Reordenamiento de los datos
datos_1_reordenados <- datos_1 %>% slice_sample(prop = 1)
summary(reg_1_loglin_reorder <- lm(log(rendimiento)~potencia+peso+vel_max+volumen, data = datos_1_reordenados))

bptest(reg_1_loglin_reorder)
dwtest(reg_1_loglin_reorder, alternative="greater")

############################### Ejercicio 2----
######### a) Importar Datos
datos_2 <- read_excel("econometria/datos/Table 12_4 Gujarati.xls", sheet = "Table 12_4", skip = 3)

######### b) Regresion
summary(reg_2 <- lm(Y~X, data = datos_2))

######### c) Determinar si la especificación del modelo es correcta utilizando el test RESET, con la potencia 2
resettest(reg_2, 2, type=c("fitted"))
# rechazo (la especificaci?n no se valida)

######### d) Probar otras especificaciones
### log-log
summary(reg_2_loglog <- lm(log(Y)~log(X), data = datos_2))

### log-lin 
summary(reg_2_loglin <- lm(log(Y)~X, data = datos_2))

### lin-log
summary(reg_2_linlog <- lm(Y~log(X), data = datos_2))

### Validacion
resettest(reg_2_loglog, 2, type=c("fitted"))
resettest(reg_2_loglin, 2, type=c("fitted"))
resettest(reg_2_linlog, 2, type=c("fitted"))
##### Solo valida el modelo lin-log

######### e) Con el modelo mejor especificado, verificar los signos de los coeficientes estimados y su significatividad individual.
summary(reg_2_linlog)$coefficients

######### f) Verificar la bondad del ajuste y la significatividad global de la regresión
summary(reg_2_linlog)$r.squared
significatividad_global(reg_2_linlog)

######### g) Verificar la normalidad de las perturbaciones con el test Jarque-Bera
jarque.test(reg_2_linlog$residuals)
# No Rechazo Normalidad

######### h) Verificar la existencia de Heterocedasticidad con el test de Breusch-Godfrey, White
### Breusch-Godfrey
bptest(reg_2_linlog)
# Rechazo, es decir Existe Heterocedasticidad 

### White
bptest(Y~log(X)+I(log(X)^2), data=datos_2) 
# No Rechazo, es decir No Existe Heterocedasticidad 

### Corrigo Heterocedasticidad (Plan B)
summary(reg_2_linlog)
coeftest(reg_2_linlog, vcov = vcovHAC(reg_2_linlog, type="HC1"))

######### i) Verificar la existencia de Autocorrelación en las perturbaciones
### Durbin y Watson
dwtest(reg_2_linlog, alternative="greater")
# Rechazo, es decir hay autocorrelacion positiva

### Breusch y Godfrey
bgtest(reg_2_linlog, order=1)
# Rechazo, es decir hay autocorrelacion positiva

### GLS
summary(reg_2_GLS <- gls(Y~log(X), data=datos_2, correlation=corAR1(), method="ML"))

summary(reg_2_GLS)$coefficients
summary(reg_2_GLS)$r.squared
jarque.test(as.vector(reg_2_GLS$residuals))
