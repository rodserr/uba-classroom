#********************************************************
#*#   CORRELOGRAMA DE LOS RESIDUOS DE UNA REGRESIÓN
#********************************************************
# Versión 26/07/2022     Encoding: UTF-8

# Es una función para calcular el test Q y sus p-value
# para los residuos de una regresión. Sus argumentos son
# xreg: una salida de una regresión
# lags: cuántos lags desea en el analizar (debe ser<=T/3)
# p_q:cantidad de parámetros incluídos en la regresión

corr_res=function(xreg,lags,p_q) {
res=xreg$residuals
T=length(res)
if(lags>T/3) {
  print("Elija una cantidad menor de lags")}
else {
a=p_q
lag=c()
q_values=c()
p_values=c()
for(i in 1:lags)   {
  lag=c(lag,i)  
  warn<-options(warn=-1)
  p=Box.test(xreg$residuals,lag=i,type="Ljung", fitdf=a)
  options(warn)
  q_values=c(q_values,round(p$statistic, digits=4))
  if(i<=a){
    p_values=c(p_values,"NA")
  } else {
  p_values=c(p_values,round(p$p.value, digits=4))
  }
}
}

correlograma=cbind(lag,q_values,p_values)
rownames(correlograma)=NULL
colnames(correlograma)=c("lag","Estadistico Q","p-value")
return(correlograma)
options(digits=4)
print(correlograma)
}
##########################################################
# ejemplo:
# xreg=arima(y,c(2,0,2))
# corr_res(xreg,12,4)
