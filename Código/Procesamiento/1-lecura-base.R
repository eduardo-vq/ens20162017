# Cargar base de datos desde SPSS
ens_2017 <- read_spss("Datos/Datos Originales/ENS_F1_2_Comuna.sav")

# Guardar en formato CSV
write.csv2(ens_2017, file = "Datos/Datos Intermedios/ens_dic2017.csv")

# Limpiar entorno de trabajo
rm(list=ls())
