# Importar el archivo de Excel "Datos Gasto e ingreso.xlsx"

# Regresión Gasto - Ingreso - Resolución matricial
data_GI=Datos_Gasto_ingreso
names(data_GI)
# Abre ventana de visualización
windows()
plot(data_GI$ingreso_p,data_GI$gasto_p)      # Gráfico dispersión X-Y

# Regresión Gasto - Ingreso - Resolución con R
reg_GI=lm(gasto_p ~ ingreso_p, data=data_GI) # realiza la regresión
# Agrega  linea de regresión al gráfico de dispersión
abline(reg_GI) 
# Cierra la ventana
dev.off()


options(scipen = 9)   # Para que no reporte en notación científica
summary(reg_GI)       # "salida" de la regresión
deviance(reg_GI)      # Suma de los residuos al cuadrado (no en la salida)

# Cálculo matricial - Verificación de los resultados
# Vector de var. explicada
Y=matrix(data_GI$gasto_p,ncol=1)   ;  head(Y,5)
# Matriz de los datos
X=matrix(c(rep(1,length(Y)),data_GI$ingreso_p),ncol=2) ;  head(X,5)
# Estimación
b_hat=solve(t(X) %*% X) %*% (t(X) %*% Y)  ;  b_hat

# Valores ajustados, Residuos y SRC
Y_hat=X %*% b_hat   ; head(Y_hat,5)
u_hat=Y-Y_hat       ; head(u_hat,5)

Tabla=cbind(Y,Y_hat,u_hat)
colnames(Tabla)=c("Y","Y_hat","u_hat")
head(Tabla,5)

src=sum(u_hat^2)   ;  src  
# comparar con: deviance(reg_GI) 

# Cálculo de la matriz de varianzas y covarianzas
N=length(Y)
k=length(b_hat)
gl=N-k            ;  gl
var_u=src/gl    ; var_u  
# S.E. de la regresión o S.E. de los residuos
sqrt(var_u) 

vcov=var_u * solve(t(X) %*% X) ; vcov
sqrt(diag(vcov))   

##########################
# Verificación de los tests
alfa=0.05

# Test t del intercepto
tC_a=qt(1-alfa/2,gl)           ; tC_a
tE_a=b_hat[1]/sqrt(vcov[1,1])  ; tE_a
pv_ta=2*(1-pt(tE_a,gl))        ; pv_ta

Tabla=matrix(c(tC_a,tE_a,pv_ta),nrow=1)
colnames(Tabla)=c("Crítico","Empirico", "p-value")
print(Tabla)


# Test t de la pendiente
tC_b=qt(1-alfa/2,gl)           ; tC_b
tE_b=b_hat[2]/sqrt(vcov[2,2])  ; tE_b
pv_tb=2*(1-pt(tE_b,gl))        ; pv_tb

Tabla=matrix(c(tC_b,tE_b,pv_tb),nrow=1)
colnames(Tabla)=c("Crítico","Empirico", "p-value")
print(Tabla)

# R Cuadrado
R2=1-src/sum((Y-mean(Y))^2)   ;   R2
R2a=1-(1-R2)*(N-1)/(N-k)      ;   R2a
R2a_B=1-(src/gl)/(sum((Y-mean(Y))^2)/(N-1))  ;  R2a_B

# Test F
FC=qf((1-alfa),1,N-1)     ;   FC
FE=(R2/1)/((1-R2)/(N-k))  ;   FE
pv_F=1-pf(FE,1,N-1)       ;   pv_F

Tabla=matrix(c(FC,FE,pv_F),nrow=1)
colnames(Tabla)=c("Crítico","Empirico", "p-value")
print(Tabla)

########################################

# Verificación de las consecuencias de las escalas en los datos
reg_GIa=lm(gasto_p ~ ingreso_p, data=data_GI) 
summary(reg_GIa)           
deviance(reg_GIa)          

reg_GIb=lm(gasto_p ~ ingreso_mp, data=data_GI) 
summary(reg_GIb)           
deviance(reg_GIb)       

reg_GIc=lm(gasto_mp ~ ingreso_p, data=data_GI) 
summary(reg_GIc)           
deviance(reg_GIc)       

reg_GId=lm(gasto_mp ~ ingreso_mp, data=data_GI) 
summary(reg_GId)           
deviance(reg_GId)       

# Trabajos propuestos
# Realizar la regresión matricial de la tabla 7.9

# Generar variables con otras unidades en el archivo Mincer.xls y 
# verificar el cambio de los valores estimados y la constancia
# de la inferencia estadística

###################
# Con el archivo Tabla_11_7_Gujarati
# Calcular betas , Y_hat, u_hat, SRC, 
#  vcov, errores estándar de los betas

# Luego hacer la regresión con R y verificar los resultados


