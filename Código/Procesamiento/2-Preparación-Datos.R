# Leer desde archivo CSV intermedio
ENS <- read.csv2("Datos/Datos Intermedios/ens_dic2017.csv")

# ---- 1. RECODIFICAR VARIABLES Y CONSTRUCCIÓN DE UN ÍNDICE ----

#Seleccionar variables de interés

ENS_analisis <- select(ENS, factor1 = Fexp_F1p_Corr, factor2 = Fexp_F2p_Corr, 
                       factor1x2 = Fexp_F1F2p_Corr, sexo = Sexo, ingresos = as27, 
                       tramoingresos = as28, npersonas_hogar = n_per, 
                       region = Region, pueblosind = c6, educ = as7_corr_1,
                       comuna = Comuna, edad = Edad_Codificada, 
                       iam = d1_F1, acv= d4, evp = d7, dm=di3,
                       plmnr_cronc=m9p17A, asma=m9p18A, hta=h2, tbq= ta3) 



# RECODIFICACIÓN

# Sexo (nominal)
#1 = hombre, 2 = mujer
table(ENS_analisis$sexo) 
class(ENS_analisis$sexo)
ENS_analisis$sexo <- as.numeric(ENS_analisis$sexo) 

ENS_analisis <- mutate(ENS_analisis, sexo_factor = factor(ENS_analisis$sexo,labels = c("Hombre", "Mujer"))) #queda como objeto factor
ENS_analisis$sexo_factor #visualizar datos 
table(ENS_analisis$sexo_factor)

# Pertenencia a pueblo indígena (Nominal)
table(ENS_analisis$pueblosind) 
class(ENS_analisis$pueblosind)
ENS_analisis$pueblosind <- as.numeric(ENS_analisis$pueblosind)

ENS_analisis <- mutate(ENS_analisis, pind_dic = car::recode(ENS_analisis$pueblosind, "1:9 = 1; 10 = 2"))
ENS_analisis$pind_dic <- factor(ENS_analisis$pind_dic, levels = c(1,2),
                                labels = c("Si", "No"))

ENS_analisis$pind_dic  #visualizar datos
table(ENS_analisis$pind_dic)

# Construcción de variable: Ingresos per cápita 

# ingresos totales
table(ENS_analisis$ingresos) 
class(ENS_analisis$ingresos)
ENS_analisis$ingresos <- as.numeric(ENS_analisis$ingresos) 

#npersonas
table(ENS_analisis$npersonas_hogar) 
class(ENS_analisis$npersonas_hogar)
ENS_analisis$nph <- as.numeric(ENS_analisis$npersonas_hogar)

ENS_analisis <- mutate(ENS_analisis, ingpercap = ingresos/ npersonas_hogar) 
ENS_analisis <- mutate(ENS_analisis, tramos_ingpc = car::recode(ENS_analisis$ingpercap, "0:99985 = 1; 99986:165000  = 2 ; 165001:253300 = 3; 253301:433332 = 4 ; 433332:999999999 = 5"))
ENS_analisis$tramos_ingpc <- factor(ENS_analisis$tramos_ingpc, levels = c(1,2,3,4,5),
                                    labels = c("0-99.985", "99.986-165.000", "165.001-253.300", "253.301-433.332","433332-máx"))
ENS_analisis$tramos_ingpc  #visualizar datos
table(ENS_analisis$tramos_ingpc)

# Nivel Educacional Alcanzado o Actual
# Se recodifica en: "Ed Básica o sin estudios", "Ed Media", "Ed Técnica", "Ed Profesional"
table(ENS_analisis$educ) 
class(ENS_analisis$educ)
ENS_analisis$educ <- as.numeric(ENS_analisis$educ)

ENS_analisis <- mutate(ENS_analisis, niveleduc = car::recode(ENS_analisis$educ, "1:7 = 1; 8:11  = 2 ; 12 = 3; 13:14 = 4 ; -8888=NA; -9999 = NA"))
ENS_analisis$niveleduc <- factor(ENS_analisis$niveleduc, levels = c(1,2,3,4),
                                 labels = c("Ed Básica o sin estudios", "Ed Media", "Ed Técnica", "Ed Profesional"))

ENS_analisis$niveleduc  #visualizar datos
table(ENS_analisis$niveleduc)

# Construcción de variable: Enfermedad Respiratoria
# Se construye a partir de variable Enfermedad Pulmonar Crónica y Asma

# Enfermedad pulmonar crónica 

#¿Alguna vez un doctor o médico le ha dicho que tiene o que padece de Bronquitis crónica, enfisema pulmonar, enfermedad pulmonar obstructiva crónica o EPOC? -8888 NO SABE /  1 SÍ  / 2 NO
#recodificar en 1.Sí 0.No 
summary(ENS_analisis$plmnr_cronc)
table(ENS_analisis$plmnr_cronc)
class(ENS_analisis$plmnr_cronc)
ENS_analisis$plmnr_cronc <- as.numeric(ENS_analisis$plmnr_cronc)

ENS_analisis$plmnr_cronc <- car::recode(ENS_analisis$plmnr_cronc, "1=1;2=0; -8888=NA")

# Asma 

#¿Alguna vez un doctor o médico le ha dicho que tiene o que padece de Asma? -8888 NO SABE /  1 SÍ  / 2 NO
#recodificar en 1.Sí 0.No 
summary(ENS_analisis$asma)
table(ENS_analisis$asma)
class(ENS_analisis$asma)
ENS_analisis$asma <- as.numeric(ENS_analisis$asma)

ENS_analisis$asma <- car::recode(ENS_analisis$asma, "1=1;2=0; -8888=NA")

# Enfermedad Respiratoria

ENS_analisis <- ENS_analisis %>% mutate(enfresp = case_when(asma == 1 ~ 1,
                                                            plmnr_cronc == 1 ~ 1,
                                                            TRUE~0))
table(ENS_analisis$enfresp)

# Enfermedad Cardiovascular

# Se contruye a partir de Autoreporte de Infarto, Accidente Cerebro Vascular
# y Enfermedad Vascular Periférica

#¿Alguna vez un médico o doctor le ha dicho que tuvo o que sufrió un infarto al corazón?

#-8888 NO SABE /  1 SÍ  / 2 NO
#recodificar en 1.Sí 0.No 
summary(ENS_analisis$iam)
table(ENS_analisis$iam)
class(ENS_analisis$iam)
ENS_analisis$iam <- as.numeric(ENS_analisis$iam)

ENS_analisis$iam <- car::recode(ENS_analisis$iam, "1=1;2=0; -8888=NA ; -9999=NA")

#¿Alguna vez un médico o doctor le ha dicho que tuvo o que sufrió un 
#accidente vascular o trombosis cerebral (o derrame)?
#-8888 NO SABE /  1 SÍ  / 2 NO
#recodificar en 1.Sí 0.No 
summary(ENS_analisis$acv)
table(ENS_analisis$acv)
class(ENS_analisis$acv)
ENS_analisis$acv <- as.numeric(ENS_analisis$acv)

ENS_analisis$acv <- car::recode(ENS_analisis$acv, "1=1;2=0; -8888=NA ; -9999=NA")

#¿Alguna vez un médico o doctor le ha dicho que tuvo o que sufrió una enfermedad vascular
#periférica o a las arterias de sus piernas?
#-8888 NO SABE /  1 SÍ  / 2 NO
#recodificar en 1.Sí 0.No 
summary(ENS_analisis$evp)
table(ENS_analisis$evp)
class(ENS_analisis$evp)
ENS_analisis$evp <- as.numeric(ENS_analisis$evp)

ENS_analisis$evp <- car::recode(ENS_analisis$evp, "1=1;2=0; -8888=NA ; -9999=NA")

#Enfermedad Cardiovascular

ENS_analisis <- ENS_analisis %>% mutate(enfcv = case_when(iam == 1 ~ 1,
                                                          acv == 1 ~ 1, 
                                                          evp == 1 ~ 1,
                                                          TRUE~0))
table(ENS_analisis$enfcv)

#Hipertensión Arterial 
#ENS: "¿Alguna vez un profesional de la salud le ha diagnosticado presión alta?": 1 SÍ, UNA SOLA VEZ 2 SÍ, MÁS DE UNA VEZ  3 NO, NUNCA ME LO HAN DICHO 4 NO RECUERDO, NO ESTOY SEGURO(A)
#Recordificar en números asociados a subgrupos de respuesta 1:2 en 1, 3:4 en 0.
summary(ENS_analisis$hta)
table(ENS_analisis$hta)
class(ENS_analisis$hta)
ENS_analisis$hta <- as.numeric(ENS_analisis$hta)

ENS_analisis <- mutate(ENS_analisis, hta_rec = car::recode(ENS_analisis$hta, "1:2=1;3:4=0; -8888=NA"))
table(ENS_analisis$hta_rec)

#Diabetes (nominal)
#ENS: "¿Alguna vez un doctor, una enfermera u otro profesional de la salud 
#le ha dicho a Ud. que ha tenido o que tiene o que padece de Diabetes (azúcar alta en la sangre)?": 1 SÍ / 2 NO / 3 NO RECUERDO
#Recordificar en números asociados a subgrupos de respuesta 1 en 1, 2:3 en 0.
summary(ENS_analisis$dm)
table(ENS_analisis$dm)
class(ENS_analisis$dm)
ENS_analisis$dm <- as.numeric(ENS_analisis$dm)

ENS_analisis <- mutate(ENS_analisis, dm_rec = car::recode(ENS_analisis$dm, "1=1;2:3=0"))
table(ENS_analisis$dm_rec)

#Construir indicador: Cantidad de Factores de Riesgo
ENS_analisis <- mutate(ENS_analisis, Risk = (enfresp + hta_rec + dm_rec + enfcv))
table(ENS_analisis$Risk)

ENS_analisis <- mutate(ENS_analisis, Risk = (enfresp + hta_rec + dm_rec + enfcv))
table(ENS_analisis$Risk)

# ---- 2. GUARDAR BASE EDITADA EN FORMATO R ----

# Guardar base en formato RDS, con bases listas para análisis
saveRDS(ENS_analisis, file = "Datos/Datos de Análisis/ENS2017_analisis.RDS")

# Limpiar entorno de trabajo
rm(list=ls())
