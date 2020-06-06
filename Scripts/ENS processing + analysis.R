# ELECTIVO: CIENCIA ABIERTA Y SOFTWARE LIBRE.
# MAGÍSTER EN CCSS UNIVERSIDAD DE CHILE - 2020 
# Eduardo Peña Ortiz
# Encuesta Nacional de Salud 2016-2017

#agregar otros paquetes para entrega
install.packages("arsenal")

# ---- 1. CARGAR BASE DE DATOS A RSTUDIO Y PREPARAR VARIABLES ----

# Cargar Paquete
library(haven)

# Cargar Bases de datos

#Base de datos Formulario 1 y 2
ensf1_2dta <- read_dta("Data/InputData/ENS_F1_2_.dta")
ensf1_2sav <- read_spss("Data/InputData/ENS_F1_2_.sav")

#Base de datos con variables complejas de Formulario 1 y 2
ens_complejo <- read_spss("Data/InputData/ENS_F1_2_Comuna.sav")

#Explorar bases de datos
names(ensf1_2dta)
names(ensf1_2sav)
names(ens_complejo)

#Comparar variables y Unir DataFrames 
library(arsenal)#Cargar Paquete
comparedf(ensf1_2sav,ens_complejo)
ens <- merge(ensf1_2sav,ens_complejo, all=TRUE) 

#Seleccionar variables de interés
library(dplyr)
ENS_analisis <- select(ens, factor1 = Fexp_F1p_Corr, factor2 = Fexp_F2p_Corr, factor1x2 = Fexp_F1F2p_Corr, sexo = Sexo, comuna = Comuna, edad = Edad_Codificada, plmnr_cronc=m9p17A, asma=m9p18A, hta=h2, tbq= ta3)
Fexp_F1p_Corr

# RECODIFICACIÓN

# Sexo (nominal)

table(ENS_analisis$sexo) #1 = hombre, 2 = mujer
class(ENS_analisis$sexo)
ENS_analisis$sexo <- as.numeric(ENS_analisis$sexo) 

ENS_analisis <- mutate(ENS_analisis, sexo_factor = factor(ENS_analisis$sexo,labels = c("Hombre", "Mujer"))) #queda como objeto factor
ENS_analisis$sexo_factor #visualizar datos 
table(ENS_analisis$sexo_factor)

# Comuna ()

#No se realizan modificaciones
table(ENS_analisis$comuna)
class(ENS_analisis$comuna)

#Edad: Menor a 65 o 65+ (nominal)

#1. 15-24 años / 2. 25-44 años / 3. 45-64años / 4. 65 o más.
#Recodificar en menor de 65, 65+
summary(ENS_analisis$edad)
class(ENS_analisis$edad)
ENS_analisis$edad <- as.numeric(ENS_analisis$edad)

#Especificar paquete desde el cual queremos ejecutar la función 'recode' 
#para poder recodificar según tramos

ENS_analisis <- mutate(ENS_analisis, edad_rec65 = car::recode(ENS_analisis$edad, "1:3 = 1;4 = 2"))
                                        
table(ENS_analisis$edad_rec65)

#Convertir a factor para poner etiquetas
ENS_analisis$edad_rec65factor <- factor(ENS_analisis$edad_rec65, labels= c("menor de 65", "65+"))
table(ENS_analisis$edad_rec65factor)


#Enfermedad pulmonar crónica

#¿Alguna vez un doctor o médico le ha dicho que tiene o que padece de Bronquitis crónica, enfisema pulmonar, enfermedad pulmonar obstructiva crónica o EPOC? -8888 NO SABE /  1 SÍ  / 2 NO
summary(ENS_analisis$plmnr_cronc)
table(ENS_analisis$plmnr_cronc)
class(ENS_analisis$plmnr_cronc)
ENS_analisis$plmnr_cronc <- as.numeric(ENS_analisis$plmnr_cronc)

ENS_analisis$plmnr_cronc <- car::recode(ENS_analisis$plmnr_cronc, "1=1;2=0; -8888=NA")

#Convertir a factor para poner etiquetas
ENS_analisis$plmnr_cronc_factor<- factor(ENS_analisis$plmnr_cronc, labels= c("No", "Sí"))
table(ENS_analisis$plmnr_cronc_factor)

#Filtrar casos con Enfermdad pulmonar crónica 
EPC <- ENS_analisis %>%
  filter(plmnr_cronc == 1)
  dim(EPC)

#Asma

#¿Alguna vez un doctor o médico le ha dicho que tiene o que padece de Asma? -8888 NO SABE /  1 SÍ  / 2 NO
summary(ENS_analisis$asma)
table(ENS_analisis$asma)
class(ENS_analisis$asma)
ENS_analisis$asma <- as.numeric(ENS_analisis$asma)

ENS_analisis$asma <- car::recode(ENS_analisis$asma, "1=1;2=0; -8888=NA")

#Convertir a factor para poner etiquetas
ENS_analisis$asma_factor<- factor(ENS_analisis$asma, labels= c("No", "Sí"))
table(ENS_analisis$asma_factor)

#Filtrar casos con Asma 
ASMA <- ENS_analisis %>%
  filter(asma == 1)
dim(ASMA)

#Hipertensión arterial 
#ENS: "¿Alguna vez un profesional de la salud le ha diagnosticado presión alta?": 1 SÍ, UNA SOLA VEZ 2 SÍ, MÁS DE UNA VEZ  3 NO, NUNCA ME LO HAN DICHO 4 NO RECUERDO, NO ESTOY SEGURO(A)
#Recordificar 1 y 2 a Sí, 3 y 4 a No.
summary(ENS_analisis$hta)
table(ENS_analisis$hta)
class(ENS_analisis$hta)
ENS_analisis$hta <- as.numeric(ENS_analisis$hta)

ENS_analisis <- mutate(ENS_analisis, hta_rec = car::recode(ENS_analisis$hta, "1:2=1;3:4=0; -8888=NA"))
table(ENS_analisis$hta_rec)

#Convertir a factor para poner etiquetas
ENS_analisis$hta_factor<- factor(ENS_analisis$hta_rec, labels= c("No", "Sí"))
table(ENS_analisis$hta_factor)

#Filtrar casos con Asma 
HTA <- ENS_analisis %>%
  filter(hta_rec == 1)
dim(HTA)


saveRDS(ENS_analisis, file = "Data/AnalysisData/ENS20162017_seleccion.RDS")
D
# ---- 2. RESULTADOS  ----

library(summarytools)

#poner listas de ponderadores

# RESULTADO 1-2: frecuencias ponderadas con "summarytools" 
# Ditribución de comunas encuestados
freq(ENS_analisis$comuna, weights = ENS_analisis$factor1)

# RESULTADO 2-2:  tabla de doble entrada con "summarytools", ponderada
# Posición sobre manifestaciones O-19 según sexo, perfil columna
ctable(ENS_analisis$comuna, ENS_analisis$plmnr_cronc_factor, prop = "c", weights = ENS_analisis$factor1x2)





