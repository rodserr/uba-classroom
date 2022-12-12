#-------   FUNCION EVAL_PRON()   -------------------------------
# Evalúa pronósticos. Los argumentos son 
# Y_A Vector de valores verdaderos 
# Y_P Vector de valores pronosticados
# Nombre para la salida del pronóstico
Eval_Pron=function(Y_A,Y_P,Nombre="Pron_1"){
  # Calculo de los estadísticos
  pron=cbind(Y_A,Y_P)
  RMSE=sqrt(sum((Y_P-Y_A)^2)/length(Y_P)) ; RMSE
  MAE=sum(abs(Y_P-Y_A)/length(Y_P))  ;  MAE
  MAPE=100*sum(abs((Y_P-Y_A)/Y_A)/length(Y_P))  ; MAPE
  D1=sqrt(sum(Y_P^2)/length(Y_P))   ;   D1
  D2=sqrt(sum(Y_A^2)/length(Y_P))   ;  D2
  Theil=RMSE/(D1+D2)  ;   Theil
  
  # Descomposición del Theil
  MSE=RMSE^2
  U1=(1/MSE)*(mean(Y_P)- mean(Y_A))^2
  sd_Y_P=sqrt(var(Y_P)*(length(Y_P)-1)/length(Y_P))
  sd_Y_A=sqrt(var(Y_A)*(length(Y_A)-1)/length(Y_A))
  U2=(1/MSE)*((sd_Y_P)-sd_Y_A)^2
  U3=1-U1-U2
  
  # Resumen
  Pron_1=matrix(c(RMSE,MAE, MAPE,Theil,U1,U2,U3),ncol=1)
  rownames(Pron_1)=c("RMSE","MAE", "MAPE","Theil","U1","U2","U3")
  colnames(Pron_1)=c(Nombre)
  return(Pron_1)
}
#--------------------------------------------------------------
