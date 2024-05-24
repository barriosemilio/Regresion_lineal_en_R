#                               ==================
# ============================== TRABAJO PRACTICO ==============================
#                               ==================

# Carrera: Licenciatura en Estadística y Ciencia de Datos
# Materia: Algoritmos y Programación I
# Año: 2023 - II Cuatrimestre

# Docente Titular: Peluso, Pablo | Perez, David

# Alumnos: Macarena Molinedo | Nicolás Bores | Emilio Barrios
# ==============================================================================

# Objetivo:
# ========

# Aplicar los conceptos y conocimientos adquiridos durante el curso
# sobre el lenguaje de programación R en el desarrollo de un trabajo
# práctico grupal, teniendo en cuenta los siguientes lineamientos:

# - El trabajo debe ser realizado en grupo de 3 o 4 integrantes

# - Seleccionar una base de datos a elección del grupo. 
#   Puede ser generada a partir de ne encuesta, obtenida de Internet,
#   de otras materias, etc.

# - Utilizando R, generar un proyecto y los chunks necesarios para obtener:
#   - Un modelo lineal a partir de 2 campos de la base 
#     que estén más relacionados.
#   - Generar al menos 2 planillas para analizar y explicar el comportamiento
#     de la muestra.
#   - Generar al menos 2 gráficos que permitan interpretar los resultados.
#   - A partir del comportamiento lineal, generar el código necesario para poder
#     predecir el comportamiento del modelo fuera del intervalo abarcado por la 
#     la base de datos.

# - Explicar oralmente el proceso y mostrar los resultados el Jueves 16
#   de Noviembre. 
#   La exposición oral debe durar alrededor de 15 minutos y deben exponer todos
#   integrantes.
#   

# ==============================================================================

# ================
# == Resolución ==
# ================
#
#                       ===========================
# ====================== CONFIGURACIONES INICIALES =============================
#                       ===========================

# ========================
# == Carga de librerías ==
# ========================

libs_para_instalar=c("ggplot2","readxl","plotly","dplyr","tidyr","matlab","readr","scales","googlesheets4",
                     "stringr","modeest","stats","utils","caret",
                     "tidyverse","devtools","corrplot","xboost","randomForest")

install.packages(libs_para_instalar)

library(ggplot2)
library(readxl)
library(plotly)
library(dplyr)
library(tidyr)
library(matlab)
library(readr)
library(scales)
library(googlesheets4)
library(stringr)

# Verificamos que se encuentren cargadas en memoria
search()

# ===================
# == Opcional ( ! )==
# ===================

# install.packages("funModeling")

# Instalamos una librería funModeling no disponible en los repositorios oficiales
# Usamos la librería devtools previamente instalada
# library(devtools)
# require(devtools)

# Instalamos del repositorio github
# install_github("pablo14/funModeling")

# Llamamos a la librería
# library(funModeling)


# =====================
# == SELECCIÓN DE BD == TP_Base_Zodiaco_2022.xls
# =====================

# getwd()
# setwd("C://Users//emilio.barrios//Documents//R Scripts//PR1)

# 

file.choose() # Abre una ventana en el Explorer  para seleccionar archivo si es que preferimos abrirlo desde otra carpeta que no sea la de proyecto
db=read_xls("G://Mi unidad//0000 UNTREF//2023 2C - Algoritmos y Programacion I//R_proy_y_data_20200827//data//TP_Base_Zodiaco_2022.xls")

# db=read_xls("C://Users//emilio.barrios//Documents//R Scripts//PR1//TP_Base_Zodiaco_2022.xls")


#                 ============================================
# ================ Análisis Exploratorio de los Datos (Básico)  ==================
#                 ============================================

# Vemos las variables con las que estamos trabajando
names(db)
# > names(db)
# [1] "N de Orden" "TURNOS"     "PESO"       "ALTURA"     "CALZADO"    "SEXO"       "TRABAJA"    "FUMA"       "Nhermanos" 

str(db)
# > str(db)
#tibble [258 × 10] (S3: tbl_df/tbl/data.frame)
#$ N de Orden: num [1:258] 252 89 12 150 55 157 134 156 123 194 ...
#$ TURNOS    : chr [1:258] "D" "B" "A" "C" ...
#$ PESO      : num [1:258] 35 40 42 42 42 43 43 44 44 44 ...
#$ ALTURA    : num [1:258] 1.5 1.53 1.53 1.53 1.6 1.48 1.56 1.54 1.57 1.6 ...
#$ CALZADO   : num [1:258] 36 36 35 36 36 34 35 36 35 36 ...
#$ SEXO      : chr [1:258] "F" "F" "F" "F" ...
#$ TRABAJA   : chr [1:258] "NO" "NO" "SI" "SI" ...
#$ FUMA      : chr [1:258] "SI" "NO" "NO" "SI" ...
#$ SIGNO     : chr [1:258] "Libra" "GEMINIS" "CAPRICORNIO" "GEMINIS" ...
#$ Nhermanos : num [1:258] 2 1 5 2 1 1 2 1 0 0 ...

# Validamos
View(db)

# Encoding variables categóricas que consideramos relevantes

db$TURNOS=factor(db$TURNOS, level=c("A","B","C","D"),labels=c(1,2,3,4)) # creamos un vector numérico para la variable TURNOS
db$SEXO=factor(db$SEXO, level=c("F","M"),labels=c(0,1)) # creamos un vector numérico para la variable SEXO
db$TRABAJA=factor(db$TRABAJA, level=c("NO","SI"),labels=c(0,1)) # creamos un vector numérico para la variable TRABAJA
db$FUMA=factor(db$FUMA, level=c("NO","SI"),labels=c(0,1)) # creamos un vector numérico para la variable FUMA
# db$SIGNO=factor(db$SIGNO, level=c("ACUARIO","PISCIS","ARIES","TAURO","GEMINIS","CANCER","LEO","VIRGO","LIBRA","ESCORPIO","SAGITARIO","CAPRICORNIO"),labels=c(1,2,3,4,5,6,7,8,9,10,11,12)) # creamos un vector numérico para la variable FUMA

# Validamos
View(db)

# Verificamos las modificaciones
str(db)
# tibble [258 × 10] (S3: tbl_df/tbl/data.frame)
# $ N de Orden: num [1:258] 252 89 12 150 55 157 134 156 123 194 ...
# $ TURNOS    : Factor w/ 4 levels "1","2","3","4": 4 2 1 3 1 3 3 3 2 3 ...
# $ PESO      : num [1:258] 35 40 42 42 42 43 43 44 44 44 ...
# $ ALTURA    : num [1:258] 1.5 1.53 1.53 1.53 1.6 1.48 1.56 1.54 1.57 1.6 ...
# $ CALZADO   : num [1:258] 36 36 35 36 36 34 35 36 35 36 ...
# $ SEXO      : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...
# $ TRABAJA   : Factor w/ 2 levels "0","1": 1 1 2 2 1 2 1 1 1 1 ...
# $ FUMA      : Factor w/ 2 levels "0","1": 2 1 1 2 1 1 1 1 1 1 ...
# $ SIGNO     : chr [1:258] "Libra" "GEMINIS" "CAPRICORNIO" "GEMINIS" ...
# $ Nhermanos : num [1:258] 2 1 5 2 1 1 2 1 0 0 ...

# Vemos que la estructura de la bd tiene variables categóricas nominales y que 
# en nuestro caso optamos por no usarla: Eliminamos la columna SIGNO
db=select(db,-c(SIGNO))

# Filtramos y estandarizamos de la siguiente manera: Filtro HERMANOS > 10
db=db %>% filter(`Nhermanos`<10)

# Verificamos en db
View(db)


# =======================================================
# == Necesitamos verificar la relación entre variables ==
# =======================================================

cor(db) # no puedo inicialmente ejecutar la función de correlación porque hay variables del tipo factor

# Para ello verificamos las clases de las variables y las convertimos
sapply(db,class)
# N de Orden      TURNOS        PESO      ALTURA     CALZADO        SEXO     TRABAJA        FUMA       SIGNO   Nhermanos 
# "numeric"    "factor"   "numeric"   "numeric"   "numeric"    "factor"    "factor"    "factor" "character"   "numeric" 

# Procedemos a convertir las variables a numeric
db$TURNOS=as.numeric(as.character(db$TURNOS))
db$SEXO=as.numeric(as.character(db$SEXO))
db$TRABAJA=as.numeric(as.character(db$TRABAJA))
db$FUMA=as.numeric(as.character(db$FUMA))

# Verificamos nuevamente
sapply(db,class)
# N de Orden     TURNOS       PESO     ALTURA    CALZADO       SEXO    TRABAJA       FUMA  Nhermanos 
# "numeric"  "numeric"  "numeric"  "numeric"  "numeric"  "numeric"  "numeric"  "numeric"  "numeric" 

# Validamos
View(db)

# Ejecuto la función de correlación para ver la fuerza de relación este variables
cor(db)
#              N de Orden       TURNOS         PESO       ALTURA     CALZADO        SEXO     TRABAJA        FUMA    Nhermanos
# N de Orden  1.000000000  0.967591974  0.007587813 -0.008861627 -0.06321742 -0.01529916  0.17161483  0.11681770 -0.009786790
# TURNOS      0.967591974  1.000000000  0.019910307  0.004391586 -0.05829958 -0.01612191  0.20604290  0.09522214  0.005671206
# PESO        0.007587813  0.019910307  1.000000000  0.703546210  0.75064864  0.63617804 -0.04893743 -0.10874971  0.043679449
# ALTURA     -0.008861627  0.004391586  0.703546210  1.000000000  0.81856845  0.64901095 -0.07747831 -0.07809589  0.105468053
# CALZADO    -0.063217419 -0.058299585  0.750648643  0.818568451  1.00000000  0.81374743 -0.06812854 -0.10238354  0.050641338
# SEXO       -0.015299155 -0.016121907  0.636178042  0.649010950  0.81374743  1.00000000 -0.03806148 -0.15103882  0.010726112
# TRABAJA     0.171614831  0.206042898 -0.048937431 -0.077478311 -0.06812854 -0.03806148  1.00000000 -0.02934528  0.165445313
# FUMA        0.116817703  0.095222137 -0.108749707 -0.078095893 -0.10238354 -0.15103882 -0.02934528  1.00000000  0.028153586
# Nhermanos  -0.009786790  0.005671206  0.043679449  0.105468053  0.05064134  0.01072611  0.16544531  0.02815359  1.000000000

# Graficamos
pairs(db)

# Vemos que hay una fuerte relación entre las variables ALTURA - CALZADO

# Realizamos una nueva descripción estadística con la función summary() y verificamos el desvío estándar para las dos variables seleccionadas
summary(db$CALZADO)
sd(db$CALZADO)

summary(db$ALTURA)
sd(db$ALTURA)

# Vemos ambas distribuciones
hist(db$CALZADO)
hist(db$ALTURA)

# Volvemos a verificar la correlación pero solo entre las variables seleccionadas
correlacion=cor(db$CALZADO, db$ALTURA)
correlacion
#> correlación
#[1] 0.8185685

# ================
# == Conclusión ==
# ================
# Como el valor de la correlación de ambas variables es + 0.5 lo que significa que es una relación buena 


#                     ===============================
# ====================  Generación del Modelo Lineal  ============================
#                     ===============================

# En base al análisis previo, entonces se considera que las 2 variables a utilizar para modelizar son:

# Variable explicativa (x) >> CALZADO
# Variable dependiente (y) >> ALTURA 

# ==============
# == Objetivo == Aplicar un modelo de regresión lineal para predecir la ALTURA (y) en relación al CALZADO (x)
# ==============

# Generamos el modelo
modelo=lm(ALTURA ~ CALZADO,data=db)

# Ejecutamos la funcion summary() para analizar R cuadrado
summary(modelo)
# Call:
#   lm(formula = ALTURA ~ CALZADO, data = db)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.133826 -0.030048  0.000275  0.027326  0.305851 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 0.571663   0.048465   11.79   <2e-16 ***
#   CALZADO     0.028525   0.001254   22.76   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.05451 on 255 degrees of freedom
# Multiple R-squared:  0.6701,	Adjusted R-squared:  0.6688 
# F-statistic: 517.9 on 1 and 255 DF,  p-value: < 2.2e-16


#                     ===============================
# ====================   Generación de Planillas     ==========================
#                     ===============================

# Generamos 2 planillas

planilla_1<-select (db, -c('N de Orden', TURNOS, PESO, SEXO, TRABAJA, FUMA, Nhermanos))
View(planilla_1)

planilla_2<-select (db, -c('N de Orden', TURNOS, CALZADO, SEXO, TRABAJA, FUMA, Nhermanos))
View(planilla_2)


#                     ===============================
# ====================    Generación de Gráficas     ============================
#                     ===============================


# Realizamos la gráfica de dispersión entre las variables
plot (ALTURA ~ CALZADO,data=db, 
      xlab = "Calzado (numerarión Americana) ", ylab = "Altura (metros)",
      main = "Altura vs Calzado")

# Graficamos con ggplot() la dispersión puntos en color café y recta en color verde
ggplot(data = db, aes(x = CALZADO, y = ALTURA)) +
  geom_point(col="brown") +
  geom_line(aes(x = CALZADO, y = modelo$fitted.values), color = "darkgreen") +
  xlab("Calzado") + 
  ylab("Estatura") + 
  ggtitle("Linea de tendencia sobre datos")


# Generamos el grafico combinado con líneas de regresión
ggplot(db) +
  # 1
  geom_line(aes(x = PESO, y = ALTURA), linetype = 1, size = 2, color = "green", alpha = 0.3) +
  geom_smooth(aes(x = PESO, y = ALTURA), method = "lm", color = "blue", se = FALSE) +
  # 2
  geom_line(aes(x = CALZADO, y = ALTURA), linetype = 1, size = 2, color = "red", alpha = 0.3) +
  geom_smooth(aes(x = CALZADO, y = ALTURA), method = "lm", color = "black", se = FALSE) +
  # Eje secundario superior para Calzado
  scale_x_continuous(sec.axis = sec_axis(~ ., name = "Calzado")) +
  # Etiquetas y título
  xlab("Peso") +
  ylab("Altura") +
  ggtitle("Relación entre Peso/Calzado y Altura/Calzado con Regresión Lineal")


# Generamos otro gráfico combinado
ggplot(data = db, aes(x = CALZADO, y = ALTURA)) +
  geom_point(col="black") +
  geom_smooth(method = "lm", color = "blue", se = TRUE) +
  geom_line(aes(x = CALZADO, y = ALTURA), color = "red") +
  xlab("Talle") + 
  ylab("Estatura") + 
  ggtitle("Linea de tendencia sobre datos")



#                     ===============================
# ====================  Predicción del Modelo Lineal  ============================
#                     ===============================

# Hacemos la predicción con valores dummies fuera del intervalo/input original

# Generamos nuevos registros
nuevos.calzados <- data.frame(CALZADO = seq(30, 40)) 

# Realizamos la predicción ejecutando la funcion predict()
predict (modelo, nuevos.calzados)
# > predict (modelo, nuevos.calzados)
# 1        2        3        4        5        6        7        8        9       10       11 
# 1.427421 1.455947 1.484472 1.512997 1.541523 1.570048 1.598573 1.627098 1.655624 1.684149 1.712674 




