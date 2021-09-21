# library(RMySQL)
library(sqldf)

options(sqldf.driver = "SQLite") 

#Leo el csv y lo guardo en un dataframe
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
bankData <- read.csv("data/bank/bank-full.csv", sep=";")
# names(bankData)
# head(bankData)

#tabla yes/no
bank_yn <- sqldf("SELECT 1 id_yn, 'yes' yn union SELECT 0 id_yn, 'no' yn")

#tabla job
#admin.   blue-collar  entrepreneur     housemaid    management       retired self-employed      services       student    technician    unemployed       unknown 
bank_job <- sqldf("SELECT 1 id_job, 'admin.' job union SELECT 2 id_job, 'blue-collar' job
                  union SELECT 3 id_job, 'entrepreneur' job union SELECT 4 id_job, 'housemaid' job 
                  union SELECT 5 id_job, 'management' job union SELECT 6 id_job, 'retired' job 
                  union SELECT 7 id_job, 'self-employed' job union SELECT 8 id_job, 'services' job
                  union SELECT 9 id_job, 'student' job union SELECT 10 id_job, 'technician' job
                  union SELECT 11 id_job, 'unemployed' job union SELECT 12 id_job, 'unknown' job        
                  ")

#tabla marial
#divorced  married   single
bank_marital <- sqldf("SELECT 1 id_marital, 'divorced/widowed' marital union SELECT 2 id_marital, 'married' marital
                      union SELECT 3 id_marital, 'single' marital")

#tabla education
#primary secondary  tertiary   unknown 
bank_education <- sqldf("SELECT 1 id_education, 'primary' education union SELECT 2 id_education, 'secondary' education
                      union SELECT 3 id_education, 'tertiary' education union SELECT 4 id_education, 'unknown' education")

#tabla contact 
#cellular telephone   unknown
bank_contact <- sqldf("SELECT 1 id_contact, 'cellular' contact union SELECT 2 id_contact, 'telephone' contact
                      union SELECT 3 id_contact, 'unknown' contact")

# tabla poutcome
#failure   other success unknown 
bank_poutcome <- sqldf("SELECT 1 id_poutcome, 'failure' poutcome union SELECT 2 id_poutcome, 'success' poutcome
                      union SELECT 3 id_poutcome, 'other' poutcome union SELECT 4 id_poutcome, 'unknown' poutcome")

bank_datos <- bankData 
bank_datos$ID <- seq.int(nrow(bank_datos))

bank_datos$default  <- replace(bank_datos$default, bank_datos$default == "yes", 1)
bank_datos$default  <- replace(bank_datos$default, bank_datos$default == "no", 0)

bank_datos$housing  <- replace(bank_datos$housing, bank_datos$housing == "yes", 1)
bank_datos$housing  <- replace(bank_datos$housing, bank_datos$housing == "no", 0)

bank_datos$loan  <- replace(bank_datos$loan, bank_datos$loan == "yes", 1)
bank_datos$loan  <- replace(bank_datos$loan, bank_datos$loan == "no", 0)

bank_datos$y  <- replace(bank_datos$y, bank_datos$y == "yes", 1)
bank_datos$y  <- replace(bank_datos$y, bank_datos$y == "no", 0)

bank_datos$month  <- replace(bank_datos$month, bank_datos$month =="jan", 1)
bank_datos$month  <- replace(bank_datos$month, bank_datos$month == "feb", 2)
bank_datos$month  <- replace(bank_datos$month, bank_datos$month == "mar", 3)
bank_datos$month  <- replace(bank_datos$month, bank_datos$month == "apr", 4)
bank_datos$month  <- replace(bank_datos$month, bank_datos$month == "may", 5)
bank_datos$month  <- replace(bank_datos$month, bank_datos$month == "jun", 6)
bank_datos$month  <- replace(bank_datos$month, bank_datos$month == "jul", 7)
bank_datos$month  <- replace(bank_datos$month, bank_datos$month == "aug", 8)
bank_datos$month  <- replace(bank_datos$month, bank_datos$month == "sep", 9)
bank_datos$month  <- replace(bank_datos$month, bank_datos$month == "oct", 10)
bank_datos$month  <- replace(bank_datos$month, bank_datos$month == "nov", 11)
bank_datos$month  <- replace(bank_datos$month, bank_datos$month == "dec", 12)

bank_datos$education  <- replace(bank_datos$education, bank_datos$education == "primary", 1)
bank_datos$education  <- replace(bank_datos$education, bank_datos$education == "secondary", 2)
bank_datos$education  <- replace(bank_datos$education, bank_datos$education == "tertiary", 3)
bank_datos$education  <- replace(bank_datos$education, bank_datos$education == "unknown", 4)

bank_datos$marital  <- replace(bank_datos$marital, bank_datos$marital == "divorced", 1)
bank_datos$marital  <- replace(bank_datos$marital, bank_datos$marital == "married", 2)
bank_datos$marital  <- replace(bank_datos$marital, bank_datos$marital == "single", 3)

bank_datos$contact  <- replace(bank_datos$contact, bank_datos$contact == "cellular", 1)
bank_datos$contact  <- replace(bank_datos$contact, bank_datos$contact == "telephone", 2)
bank_datos$contact  <- replace(bank_datos$contact, bank_datos$contact == "unknown", 3)

bank_datos$poutcome  <- replace(bank_datos$poutcome, bank_datos$poutcome == "failure", 1)
bank_datos$poutcome  <- replace(bank_datos$poutcome, bank_datos$poutcome == "success", 2)
bank_datos$poutcome  <- replace(bank_datos$poutcome, bank_datos$poutcome == "other", 3)
bank_datos$poutcome  <- replace(bank_datos$poutcome, bank_datos$poutcome == "unknown", 4)

bank_datos$job  <- replace(bank_datos$job, bank_datos$job == "admin.", 1)
bank_datos$job  <- replace(bank_datos$job, bank_datos$job == "blue-collar", 2)
bank_datos$job  <- replace(bank_datos$job, bank_datos$job == "entrepreneur", 3)
bank_datos$job  <- replace(bank_datos$job, bank_datos$job == "housemaid", 4)
bank_datos$job  <- replace(bank_datos$job, bank_datos$job == "management", 5)
bank_datos$job  <- replace(bank_datos$job, bank_datos$job == "retired", 6)
bank_datos$job  <- replace(bank_datos$job, bank_datos$job == "self-employed", 7)
bank_datos$job  <- replace(bank_datos$job, bank_datos$job == "services", 8)
bank_datos$job  <- replace(bank_datos$job, bank_datos$job == "student", 9)
bank_datos$job  <- replace(bank_datos$job, bank_datos$job == "technician", 10)
bank_datos$job  <- replace(bank_datos$job, bank_datos$job == "unemployed", 11)
bank_datos$job  <- replace(bank_datos$job, bank_datos$job == "unknown", 12)

#renombro campos
names(bank_datos)[names(bank_datos) == "y"] <- "term_deposit"
names(bank_datos)[names(bank_datos) == "education"] <- "id_education"
names(bank_datos)[names(bank_datos) == "marital"] <- "id_marital"
names(bank_datos)[names(bank_datos) == "contact"] <- "id_contact"
names(bank_datos)[names(bank_datos) == "poutcome"] <- "id_previos_outcome"
names(bank_datos)[names(bank_datos) == "job"] <- "id_job"
names(bank_datos)[names(bank_datos) == "default"] <- "id_default"



# #Para trabajar los datos en la bd mysql
# #conexion a la bd mysql
# con = dbConnect(MySQL(), user='usuario_tp_bd', password='123456.', dbname='clases_bd', host='localhost')
# #creo las tablas e inserto los datos
# dbSendQuery(con, "SET GLOBAL local_infile = true;")
# dbWriteTable(con, "bank_datos", bank_datos, overwrite = TRUE,row.names=FALSE)
# dbWriteTable(con, "bank_yn", bank_yn, overwrite = TRUE,row.names=FALSE)
# dbWriteTable(con, "bank_job", bank_job, overwrite = TRUE,row.names=FALSE)
# dbWriteTable(con, "bank_education", bank_education, overwrite = TRUE,row.names=FALSE)
# dbWriteTable(con, "bank_marital", bank_marital, overwrite = TRUE,row.names=FALSE)
# dbWriteTable(con, "bank_poutcome", bank_poutcome, overwrite = TRUE,row.names=FALSE)
# dbWriteTable(con, "bank_contact", bank_contact, overwrite = TRUE,row.names=FALSE)

