# Importar Table 7_6 Modificada.xls
rosas=Table_7_6_Modificada
names(rosas)=c("trimestre","q_rosas", "p_rosas", "p_claveles", "ingreso", "tiempo")
reg_r=lm(q_rosas ~ p_rosas+p_claveles+ingreso+tiempo, data=rosas)
summary(reg_r)

# Verificar los signos de los coeficientes
# Interpretar los coeficientes
# Verificar la significatividad individual de los mismos
# Valorar la bondad del ajuste
# Verificar la significatividad global de la regresi�n
# Depurar la regresi�n
# Realizar un informe ejecutivo

# Importar Table_7_9_Modificada.xls
pollos=Table_7_9_Modificada
names(pollos)=c("anio", "q_pollo", "ingreso", "p_pollo" , "p_cerdo", "p_vac", "p_subs") 
reg_p=lm(q_pollo~ingreso+p_pollo+ p_cerdo+p_vac+p_subs, data=pollos)
summary(reg_p)

# Idem anterior

# Importar Mincer.xls
reg_m=lm(log(SALARIO)~EDUCA+EDAD+I(EDAD^2)+HOMBRE, data=Mincer)
summary(reg_m)

# Idem anterior
# � A qu� edad la experiencia deja de incrementar el sueldo?
# � En qu� porcentaje se incrementar� el salario debido a la maestr�a? 


# options(scipen=9)
# options(digit=6)


