# Es una función para calcular el test Q y sus p-value
# para los residuos de una estimación VAR. Sus argumentos son
# var_reg: una salida de una estimación VAR
# lags: cuÃ¡ntos lags desea analizar (debe ser<=T/3)
# tipo: tipo de estadístico
# c("PT.asymptotic", "PT.adjusted", "BG", "ES")) 
vcorr_res=function(var_reg,lags,tipo="PT.adjusted") {

T=var_reg$obs
if(lags>T/3) {
  print("Elija una cantidad menor de lags")}
else {
lag=c()
q_values=c()
p_values=c()
for(i in 1:lags)   {
  lag=c(lag,i)  
  warn<-options(warn=-1)
  if(tipo=="BG" | tipo=="ES") {
    p=serial.test(var_reg,lags.bg=i,type=tipo)}
  else {
  p=serial.test(var_reg,lags.pt=i,type=tipo)  }
  options(warn)
  q_values=c(q_values,round(p$serial$statistic, digits=4))
#  if(tipo=="PT.adjusted" | tipo=="PT.asymptotic") {
#  if(i<=var_reg$p){
  if(i<=var_reg$p & (tipo=="PT.adjusted" | tipo=="PT.asymptotic") ){
    p_values=c(p_values,"NA")
  } else {
  p_values=c(p_values,round(p$serial$p.value, digits=4))
  }
#  }
}
}

correlograma=cbind(lag,q_values,p_values)
rownames(correlograma)=NULL
colnames(correlograma)=c("lag","Estadístico Q","p-value")
return(correlograma)
options(digits=4)
print(correlograma)
}
##########################################################
# ejemplo:
# # vcorr_res(var1,12,"BG")
