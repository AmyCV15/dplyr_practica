###practica de Tidyverse ####
#clase del 260225 #

#Liberías necesarias para trabajar 
# Cargamos lo que usaremos en todo el tutorial
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
url_tinto  <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv"
url_blanco <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv"

vino_tinto  <- read_delim(url_tinto,  delim = ";", show_col_types = FALSE)
vino_blanco <- read_delim(url_blanco, delim = ";", show_col_types = FALSE)

# Añadimos columna "tipo" para identificarlos al unirlos
vino_tinto  <- mutate(vino_tinto,  tipo = "tinto")
vino_blanco <- mutate(vino_blanco, tipo = "blanco")

# bind_rows() apila las dos tablas (como pegar una encima de la otra)
vinos <- bind_rows(vino_tinto, vino_blanco)

glimpse(vinos)

#Ahora con otra base de datos diferente, desde un archivo local.

salud <- read_csv("01_RawData/dataset_categorical_NA.csv", show_col_types = FALSE)
glimpse(salud)

#probando las ventajas de pipe
vinos |>
  filter(tipo == "tinto") |>
  filter(quality >= 7) |>
  select(tipo, quality, alcohol) |>
  arrange(desc(alcohol)) |>
  head(8)

salud |>
  filter(SmokingStatus == "Fuma") |>
  select(ID, Age, BMI,
         Cholesterol) |>
  head(8)
#varias formas de hacerlo 

# DPLYR: mismo resultado, más claro
filter(vinos, quality == 9)
vinos[vinos$quality == 9, ]
#tambien funciona así 
vinos %>% filter(quality == 9)
# Participantes con BMI mayor a 30 (obesidad)
filter(salud, BMI > 30)

salud %>% 
  filter(BMI <= 99.0) %>% 
  select(ID, Age, BMI,
         Cholesterol) #esto no sale revisalo 
#Hay que poner select para que pueda regresar un resultado
salud %>% 
  filter(BMI <= 99.0 & BMI >= 20) #ahora si sale. Algo pasa con números enteros

#con vinos sí funciona
vinos %>% 
  filter(tipo == "tinto" & quality >= 8)


# otro 
filter(vinos, tipo == "tinto" & quality == 5)
#
# Participantes que fuman Y tienen BMI mayor a 25
filter(salud, SmokingStatus == "Fuma", BMI > 25)

#Datos condiciones con OR
# Vinos que tengan quality == 9 O alcohol > 14
filter(vinos, quality == 9 | alcohol > 14) |>
  select(tipo, quality, alcohol) |>
  head(8)

#Filtrar por varios valores
# Vinos de calidad 8 o 9
filter(vinos, quality %in% c(8, 9)) |>
  select(tipo, quality, alcohol) |>
  head(8)
#filtrar por rangos 
# Vinos con pH entre 3.0 y 3.1 (incluyendo los extremos)
filter(vinos, between(pH, 3.0, 3.1)) |>
  select(tipo, pH, quality) |>
  head(6)
#Manejar NA en los filtros 
# Participantes sin dato de edad O menores de 30
filter(salud, is.na(Age) | Age < 30) |>
  select(ID, Age, SmokingStatus)
filter(salud, is.na(Age) ) |>
  select(ID, Age, SmokingStatus)
#para ver cuantos NA son por columnas seleccionadas 
salud %>%  
  filter(is.na(Age)) %>% 
  count()


#### Ejercicios con filter ###
#1. ¿Cuántos vinos blancos tienen una calidad de 5 o menos?
vinos %>% 
  filter(tipo == "blanco") %>% 
  filter(quality <= 5) %>% 
  count()
#otra forma más rápida
vinos %>% 
  filter(tipo == "blanco", quality <= 5) %>% 
  count()

#Filtra los vinos tintos que tengan alcohol mayor al promedio de todos los vinos.
#Pista: puedes usar mean(vinos$alcohol) como valor de referencia dentro del filter.

vinos %>%  
  filter(tipo == "tinto", alcohol > mean(alcohol))
#también funciona así 
vinos %>%  
  filter(alcohol > mean(vinos$alcohol))

#3. ¿Cuántos participantes del dataset salud son ex-fumadores y residen en zona urbana?
head(salud$SmokingStatus)
head(salud$ResidenceType)
salud %>% 
  filter(SmokingStatus == "Ex-fumadora", ResidenceType == "Urbano") %>% 
  count()

#4. Filtra los participantes con nivel de estrés mayor a 80. ¿Qué estatus de 
#tabaquismo predomina entre ellos?
head(salud$StressLevel)
salud %>% 
  filter(StressLevel > 80) %>% 
  count(SmokingStatus, sort = TRUE)
##### Verbo Selected!
# R BASE: seleccionar columnas por nombre (con comillas y c())
vinos[, c("tipo", "quality", "alcohol")]
# DPLYR: más limpio, sin comillas, sin c()
select(vinos, tipo, quality, alcohol)
# Vinos: tres columnas clave
vinos |>
  select(tipo, quality, alcohol) |>
  head(5)
# Salud: perfil básico de cada participante
salud |>
  select(ID, Age, BMI, SmokingStatus, ResidenceType) |>
  head(5)
# Quitamos las columnas de dióxido de azufre
vinos |>
  select(-`free sulfur dioxide`, -`total sulfur dioxide`) |>
  names()
# Quitamos las medidas corporales directas del dataset de salud
salud |>
  select(-Weight, -Height, -WaistCircumference, -HipCircumference) |>
  names()
vinos |>
  select(
    tipo,
    calidad        = quality,
    alcohol,
    acidez_volatil = `volatile acidity`
  ) |>
  head(4)
salud |>
  select(
    participante = ID,
    edad         = Age,
    imc          = BMI,
    tabaquismo   = SmokingStatus,
    residencia   = ResidenceType
  ) |>
  head(4)
#esto no cambia la base de datos a menos que yo lo indique renombrando 
#en otro objeto la base de datos 
# Columnas cuyo nombre empieza con "total" (vinos)
vinos |>
  select(tipo, quality, starts_with("total")) |>
  head(4)
# Columnas que contienen "Circumference" (salud)
salud |>
  select(ID, contains("Circumference")) |>
  head(4)
# Solo columnas numéricas (salud)
salud |>
  select(where(is.numeric)) |>
  head(3)
## Ejercicios 
#1. Del dataset vinos, selecciona solo las columnas de texto (tipo character).
vinos %>% 
  select(where(is.character)) 
#2. Del dataset vinos, selecciona todas las columnas cuyo nombre contenga 
#la palabra "acid".
vinos %>% 
  select(contains("acid"))
#3. Del dataset salud, selecciona ID, Age y todas las columnas que contienen 
#medidas de circunferencia. Renombra WaistCircumference como cintura.

salud %>% 
  select(
    ID, 
    Age, 
    contains("Circumference"), 
    Cintura = WaistCircumference
  )
# 4. Filtra solo los participantes del dataset salud que residan en zona 
#rural y luego selecciona únicamente ID, Age, BMI y SmokingStatus.
head(salud$ResidenceType)
salud %>%  
  filter(ResidenceType == "Rural") %>% 
  select(ID, Age, BMI, SmokingStatus)

##Verbo 3. Arrange
#Ejercicios 
#1. ¿Cuáles son los 10 vinos con mayor pH? Muestra también su tipo y calidad.
vinos %>% 
  select(tipo, quality, pH) %>% 
  arrange(pH) %>% 
  head(10)
#2.¿Qué participante del dataset salud tiene el nivel de glucosa más alto? 
#Muestra el top 5 con su ID, edad y estatus de tabaquismo.
salud %>% 
  select(ID, Age, SmokingStatus, Glucose) %>% 
  arrange(desc(Glucose)) %>% 
  head(5)
#3. Ordena los vinos blancos primero por calidad descendente y luego por 
#acidez volátil (volatile acidity) ascendente. Muestra las primeras 10 filas.
head(vinos)
vinos %>% 
  filter(tipo == "blanco") %>% 
  arrange(desc(quality)) %>% 
  arrange(`volatile acidity`) %>% ###el error era por las comillas que no estaba 
  head(10)                        #agregando
View(vinos)

## Verbo 4. Mutate
#Ejercicios 
#1. Crea una nueva columna en vinos que se llame nivel_alcohol que clasifique 
#así: - Menos de 10%: "Bajo" · Entre 10% y 12%: "Medio" · Más de 12%: "Alto"

vinos %>% 
  mutate(
    nivel_alcohol = case_when(
      alcohol < 10 ~ "Bajo",        ###modificaciones por clase:
      alcohol <= 12 ~ "Medio",      #recuerda poner is.na(alcohol) ~ "sin dato"
      alcohol > 12 ~ "Alto"         #poner entre parentesis para poder usar el &
    )
  ) %>% 
  select(tipo, nivel_alcohol, alcohol)

#2. En salud, crea una columna grupo_edad que clasifique a los participantes en:
 # - Menos de 30: "Joven" · Entre 30–50: "Adulto" · Más de 50: "Mayor"
#Recuerda manejar los NA como primera condición.

salud %>% 
  mutate(
    grupo_edad = case_when(
      is.na(Age) ~ "Sin dato",
      Age <= 30 ~ "Joven",
      Age <= 50 ~ "Adulto",
      Age > 50 ~ "Mayor",
    )
  ) %>% 
  arrange(grupo_edad) %>% 
  select(ID, Age, grupo_edad)

dim(salud)  

#3. Crea una columna fumador_activo en salud que sea TRUE si el participante 
#fuma actualmente (SmokingStatus == "Fuma"), FALSE en caso contrario.
salud$SmokingStatus
salud %>% 
  filter(!is.na(SmokingStatus)) %>% 
  mutate(
    fumador_activo = ifelse(SmokingStatus == "Fuma", "TRUE", "FALSE")
  ) %>% 
  select(SmokingStatus, ID, Age, fumador_activo, BloodPressure, HeartRate)


## Verbo 5. Summarise
#Ejercicios 
#1. Calcula el promedio de alcohol, pH y calidad para cada combinación de 
#tipo y categoria de vino.

#primero agregar la columna categoría 
vinos <- vinos %>% 
  mutate(
    categoria = case_when(
      quality <= 4 ~ "Baja",
      quality <= 6 ~ "Media",
      quality <= 8 ~ "Alta",
      TRUE         ~ "Excepcional"   # TRUE actúa como "else"
    )
  )
vinos %>% 
  group_by(tipo, categoria) %>%                     
    summarise(
      n = n(),
      media_alcohol = round(mean(alcohol), 3),
      media_pH = round(mean(pH), 3),
      media_calidad = round(mean(quality), 3)
      
    )

#2. ¿Qué estado civil del dataset salud tiene el nivel de estrés más alto en 
#promedio? Muestra el top 3.

salud %>% 
  filter(!is.na(MaritalStatus), !is.na(StressLevel)) %>% 
  group_by(MaritalStatus) %>% 
  summarise(
    n = n(),
    promedio_estres = round(mean(StressLevel)),
    .groups       = "drop"
  ) %>% 
  arrange(desc(promedio_estres)) %>% 
  head(3)
  

#3. Calcula el promedio de glucosa y colesterol por estatus de tabaquismo en salud.

salud %>% 
  filter(!is.na(Glucose), !is.na(Cholesterol), !is.na(SmokingStatus)) %>% 
  group_by(SmokingStatus) %>% 
  summarise(
    promedio_glucosa = round(mean(Glucose), 3),
    promedio_colesterol = round(mean(Cholesterol), 3),
    .groups       = "drop"
  )

#4. Desafío: ¿En qué combinación de EducationLevel y ResidenceType se observa 
#el mayor BMI promedio en salud? Muestra el top 5.

salud %>% 
  filter(!is.na(EducationLevel), !is.na(ResidenceType)) %>% 
  group_by(EducationLevel, ResidenceType) %>% 
  summarise(
    n             = n(),
    media_imc     = round(mean(BMI, na.rm = TRUE), 3),
    .groups       = "drop"                            ##recuerda usar na.rm 
  ) %>%                                               #para evitarte estar filtrando a cada rato...!
  arrange(desc(media_imc)) %>% 
  head(5)


### Verbo 6. ###
#Ejercicios 
#1. Construye una tabla resumen_educacion con el BMI promedio y nivel de estrés promedio 
#por EducationLevel. Luego usa left_join() para agregarla al dataset salud. 
#¿Cuántas filas tiene el resultado?

resumen_educacion <- salud %>% 
  filter(!is.na(EducationLevel)) %>% 
  group_by(EducationLevel) %>% 
  summarise(
    promedio_IMC = round(mean(BMI, na.rm = TRUE), 3),
    promedio_estres = round(mean(StressLevel, na.rm = TRUE), 3),
    .groups = "drop"
  )
resumen_educacion 
View(left_join(salud, resumen_educacion, by = "EducationLevel")) #se agregan sin haberse agrupado
union_salud1 <- left_join(salud, resumen_educacion, by = "EducationLevel")
#mismas filas 
nrow(union_salud1)
union_salud1$EducationLevel ##=???? por qué me deja un NA?
#####################################################################################################


#2. ¿Existen niveles educativos en resumen_educacion que no aparezcan en salud? 
#Usa anti_join() para verificarlo.
View(anti_join(salud, resumen_educacion, by = "EducationLevel"))
#Claaaro porque tiene NA en nivel de educación

#3. Usando left_join(), combina salud con resumen_zona y luego muestra el BMI 
#promedio por ResidenceType, comparándolo con el imc_promedio_zona calculado en el resumen

#copiando script para obtener objeto resumen zona 
resumen_zona <- salud |>
  group_by(ResidenceType) |>
  summarise(
    imc_promedio_zona        = round(mean(BMI, na.rm = TRUE), 1),
    colesterol_promedio_zona = round(mean(Cholesterol, na.rm = TRUE), 1),
    n_en_zona                = n(),
    .groups                  = "drop"
  )

resumen_zona
salud %>% 
  left_join(resumen_zona, by = "ResidenceType")  %>% 
  group_by(ResidenceType) %>% 
  summarise(
  promedio_imc_total = round(mean(BMI), 3),
  imc_promedio_zona_2 = imc_promedio_zona,
  .groups = "drop"
)
 #no entendí este ejercicio. ############## REVISAR #####################################################


### preguntas para clase:
#por qué en 11.3 se muestran 5 en lugar de 3? 
# Los 3 vinos con mayor calidad de cada tipo
vinos |>
  group_by(tipo) |>
  slice_max(quality, n = 3) |>
  select(tipo, quality, alcohol) |>
  arrange(tipo, desc(quality))

##### te muestra el ranking de todos los empates. hay 4 calidades 9, 4 calidades 8 y así...

### 11. 4 across() <-  este está buenísimo 
vinos |>
  group_by(tipo) |>
  summarise(
    across(c(quality, alcohol, pH), ~ round(mean(.x), 2)),
    .groups = "drop"
  )
# Media de TODAS las columnas numéricas de salud por estatus de tabaquismo
salud |>
  filter(!is.na(SmokingStatus)) |>
  group_by(SmokingStatus) |>
  summarise(
    across(where(is.numeric), ~ round(mean(.x, na.rm = TRUE), 1)),
    .groups = "drop"
  ) |>
  select(SmokingStatus, Age, BMI, Cholesterol, BloodPressure, Glucose)



### Ejercicio Integrador ####

#A) ¿Cuál es el promedio de calidad de los vinos blancos de categoría “Alta” o 
#“Excepcional”, agrupado por nivel de alcohol (nivel_alcohol)?

#agregamos columnas de nivel de alcohol y categoria de calidad
vinos_integrador <-  vinos %>% 
  mutate(
    nivel_alcohol = case_when(
      alcohol < 10 ~ "Bajo",
      alcohol <= 12 ~ "Medio",
      alcohol > 12 ~ "Alto"
    )
  ) %>% 
  mutate(
    categoria = case_when(
      quality <= 4 ~ "Baja",
      quality <= 6 ~ "Media",
      quality <= 8 ~ "Alta",
      TRUE         ~ "Excepcional"   
    )
  ) %>% 
  select(tipo, alcohol, quality, nivel_alcohol, categoria)
#respondemos pregunta
vinos_integrador %>% 
  filter(tipo == "blanco" & categoria %in% c("Alta", "Excepcional")) %>% 
  group_by(nivel_alcohol) %>% 
  summarise(
    n = n(),
    promedio_calidad = round(mean(quality, na.rm = TRUE), 3)
  )
#########################################################################################################  
#B) En el dataset salud, ¿qué 5 participantes tienen la mayor diferencia entre 
#su BMI y el BMI promedio de su grupo educativo? Muestra su ID, nivel educativo, 
#BMI individual y la diferencia.
salud$EducationLevel
salud %>% 
  filter(!is.na(EducationLevel), !is.na(BMI)) %>% 
  group_by(EducationLevel) %>% 
  summarise(
    n = n(),
    promedio_BMI = round(mean(BMI, na.rm = TRUE), 3),
    diferencia = BMI - promedio_BMI,
    .groups = "drop"
  )
#pues esto no funciona en realiad
salud %>% 
  filter(!is.na(EducationLevel), !is.na(BMI)) %>% 
  group_by(EducationLevel) %>% 
  mutate(
    promedio_BMI = round(mean(BMI, na.rm = TRUE), 3),
    diferencia = BMI - promedio_BMI,
  ) %>% 
  ungroup() %>% #por que tengo que desagrupar para que ahora sí me los agrupen?? 
  slice_max(diferencia, n = 5) %>% 
  select(ID, EducationLevel, BMI, diferencia)
#############################################################################################################33
#esta es la verdadera solucion a lo que el ejercicio esrá solicitando :)
salud %>% 
  filter(!is.na(EducationLevel), !is.na(BMI)) %>% 
  group_by(EducationLevel) %>% 
  mutate(
    promedio_BMI = round(mean(BMI, na.rm = TRUE), 3),
    diferencia = BMI - promedio_BMI,
  ) %>% 
  slice_max(diferencia, n = 5) %>% 
  select(ID, EducationLevel, BMI, diferencia)




###### para poder subirlo a mi repositorio "clonado" de Github ###########
library(usethis)
use_git_remote(name = "origin", url = "https://github.com/AmyCV15/dplyr_practica.git", overwrite = TRUE)
##########################################################################


