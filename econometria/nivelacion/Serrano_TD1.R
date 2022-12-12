# Librerias
library(tidyverse)
library(janitor)
library(readxl)

###### Ejercicio 1----
## a) Importar Datos
datos_1 <- read_excel("nivelacion_econometria/datos/Tabla 11_7 Gujarati.xls", skip = 4) %>% clean_names()

## b) Regresion
reg_1 <- lm(rendimiento~potencia+peso+vel_max+volumen, data = datos_1)
summary(reg_1)

# c) Transformacion de datos
datos_1_1 <- datos_1 %>% 
  mutate(
    rendimiento = rendimiento*.425144,
    volumen = volumen*.0283168,
    vel_max = vel_max*1.60934,
    peso = peso*45.3592
  )

# d) Regresion con datos transformados
reg_1_1 <- lm(rendimiento~potencia+peso+vel_max+volumen, data = datos_1_1)
summary(reg_1_1)

# e)
# I. ¿Cambian los coeficientes beta estimados? ¿por qué?
reg_1$coefficients
reg_1_1$coefficients

# Los coeficientes cambian porque se expresan en unidades distintas, sin embargo los signos de los coeficientes se mantienen ya que la relacion
# entre las variables explicativas y la variable respuesta no se afecta. Sino las magnitudes de los coeficientes por estar expresados 
# en unidades diferentes

# II. ¿Cambia la significatividad individual de los parámetros beta? (test t)
summary(reg_1)$coefficients[,4]
summary(reg_1_1)$coefficients[,4]

# La significatividad de los coeficientes no cambia

# III. ¿Cambia los indicadores de bondad del ajuste? (Indicadores R 2 y R 2 ajustado)
summary(reg_1)$r.squared 
summary(reg_1_1)$r.squared

summary(reg_1)$adj.r.squared
summary(reg_1_1)$adj.r.squared

# Los indicadores de bondad del ajuste no cambia

# IV. ¿Cambia la significatividad global de la regresión? (test F)
sig_global <- function(reg_fit){
  
  fstat <- summary(reg_fit)$fstatistic
  
  pf(fstat['value'], fstat['numdf'], fstat['dendf'], lower.tail=FALSE)
  
}

sig_global(reg_1)
sig_global(reg_1_1)

# La significatividad global no cambia


###### Ejercicio 2----
# a) Regresion (misma del apartado c) del Ejercicio 1) 
reg_2 <- reg_1_1

#b) Signos de los coeficientes
reg_2$coefficients

# c) Significatividad individual
summary(reg_2)$coefficients[,4]

# d) Bondad del Ajuste
summary(reg_2)$r.squared
# Como podemos observar el R-squared es alto por lo que el modelo se ajusta a los datos 

# e) Significatividad Global
sig_global(reg_2)

# f) Regresion Depurada
reg_2_1 <- lm(rendimiento~potencia+peso+vel_max, data = datos_1_1)
summary(reg_2_1)


###### Ejercicio 3-----
# a) Parametros Beta
# Vector de var. explicada
Y=matrix(datos_1_1$rendimiento, ncol=1)

# Matriz de los datos
X= datos_1_1 %>% 
  transmute(intercept = 1, potencia, peso, vel_max, volumen) %>% 
  as.matrix()

# Coeficientes Beta
b_hat=solve(t(X) %*% X) %*% (t(X) %*% Y)

# b) Estadisticos R-squared
# Valores ajustados, Residuos y SRC
Y_hat=X %*% b_hat   ; head(Y_hat,5)
u_hat=Y-Y_hat       ; head(u_hat,5)

src=sum(u_hat^2)   ;  src  

# C?lculo de la matriz de varianzas y covarianzas
N=length(Y)
k=length(b_hat)
gl=N-k            ;  gl
var_u=src/gl    ; var_u 

vcov=var_u * solve(t(X) %*% X) ; vcov

# R Cuadrado
R2=1-src/sum((Y-mean(Y))^2)   ;   R2
R2a=1-(1-R2)*(N-1)/(N-k)      ;   R2a

# c) Obtener el estadístico F de significatividad global de la regresión.
alfa=0.05

FC=qf((1-alfa),1,N-1)     ;   FC
FE=(R2/1)/((1-R2)/(N-k))  ;   FE
pv_F=1-pf(FE,1,N-1)       ;   pv_F
Tabla_f=matrix(c(FC,FE,pv_F),nrow=1)
colnames(Tabla_f)=c("Cr?tico","Empirico", "p-value")
print(Tabla_f)

# d) Obtener la matriz de varianzas y covarianzas
vcov

# e) estadísticos t de significatividad individual    y    f) p-value de los estadísticos t
sig_indv_matricial <- function(beta_coef, variable){
  tC_a=qt(1-alfa/2,gl) 
  tE_a=beta_coef[variable,]/sqrt(vcov[variable,variable]) 
  pv_ta=2*(1-pt(tE_a,gl))    
  
  Tabla=matrix(c(variable, tC_a,tE_a,pv_ta),nrow=1)
  colnames(Tabla)=c('variable', "Critico","Empirico", "p_value")
  as_tibble(Tabla)
  
}

sig.indiv <- bind_rows(
  sig_indv_matricial(b_hat, 'intercept'),
  sig_indv_matricial(b_hat, 'potencia'),
  sig_indv_matricial(b_hat, 'peso'),
  sig_indv_matricial(b_hat, 'vel_max'),
  sig_indv_matricial(b_hat, 'volumen')
)

sig.indiv

# g) Verificacion

# Coeficientes Betas
b_hat 
reg_1_1$coefficients

# R-square
R2 ; summary(reg_1_1)$r.squared
R2a ; summary(reg_1_1)$adj.r.squared

# Sifnificatividad individual
summary(reg_1_1)$coefficients[,4]
sig.indiv %>% select(variable, p_value)

###### Ejercicio 4----

# a) Importar datos
datos_4 <- read_excel("nivelacion_econometria/datos/Datos_EPH_CABA_3Tri2021.xls") %>% clean_names()

# b) Regresion
reg_4=lm(log(salario)~educa+edad+I(edad^2)+hombre, data=datos_4)
summary(reg_4)

# c) Signos de los coeficientes
reg_4$coefficients

# d) Significatividad individual
summary(reg_4)$coefficients[,4]

# e) Bondad del Ajuste
summary(reg_4)$r.squared

# f) Significatividad Global
sig_global(reg_4)

# g) Regresion Depurada
# no se remueve ninguna variable ya que todas son significativas

# h) Determinar a qué edad la experiencia deja de incrementar el salario y empieza a disminuirlo.
reg_4$coefficients['edad']/(2*reg_4$coefficients['I(edad^2)'])

# j) Determinar si en esta población existe o no brecha salarial de género y calcularla.
base_hombre <- reg_4$coefficients['(Intercept)']
base_mujer <- base_hombre+reg_4$coefficients['hombre']
(exp(base_mujer)-exp(base_hombre))/exp(base_hombre)

