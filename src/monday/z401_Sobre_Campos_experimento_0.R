##
## Sobre Campos
##
## ---------------------------
## Step 1: Cargando los datos y las librerías
## ---------------------------
##
## Genius is one percent inspiration and 99 percent perspiration
## --- ~~Thomas Edison~~ Kate Sanborn

# Limpiamos el entorno
rm(list = ls())
gc(verbose = FALSE)

# Librerías necesarias
require("data.table")
require("rpart")
require("ggplot2")
require("dplyr")

# Poner la carpeta de la materia de SU computadora local
setwd(gsub(" ", "", paste(gsub('/', '\\\\', gsub("/m_d_m/dmef", "", getwd())), "\\m_d_m\\dmef")))

# Poner sus semillas
semillas <- c(309367, 149521, 690467, 699191, 795931)

# Cargamos el dataset
dataset <- fread("./datasets/competencia1_2022.csv")

# Nos quedamos solo con el 202101
dataset <- dataset[foto_mes == 202101]

# Creamos una clase binaria
dataset[, clase_binaria := ifelse(
                            clase_ternaria == "BAJA+2",
                                "evento",
                                "noevento"
                            )]
# Borramos el target viejo
dataset[, clase_ternaria := NULL]

set.seed(semillas[1])

# Particionamos de forma estratificada
in_training <- caret::createDataPartition(dataset$clase_binaria,
                     p = 0.70, list = FALSE)
dtrain  <-  dataset[in_training, ]
dtest   <-  dataset[-in_training, ]

calcular_ganancia <- function(modelo, test) {
    pred_testing <- predict(modelo, test, type = "prob")
    sum(
        (pred_testing[, "evento"] >= 0.025) * ifelse(test$clase_binaria == "evento",
                                         78000, -2000) / 0.3
    )
}


## Actividad para medir bien la influencia de la media en esa variable, 
## escriba una función de experimento que refleje la transformación  

experimento <- function() {
    gan <- c()
    for (s in semillas) {
        set.seed(s)
        in_training <- caret::createDataPartition(dataset$clase_binaria, p = 0.70,
            list = FALSE)
        train  <-  dataset[in_training, ]
        test   <-  dataset[-in_training, ]

        r <- rpart(clase_binaria ~ .,
                    data = train,
                    xval = 0,
                    cp = -1,
                    minsplit = 20,
                    minbucket = 10,
                    maxdepth = 5)

        gan <- c(gan, calcular_ganancia(r, test))
    }
    mean(gan)
}

experimento()

## ---------------------------
## Step 5: Outliers
## ---------------------------

# Veamos el boxplot de una variable muy importante según nuestro árbol
ggplot(dtrain, aes(x=ctrx_quarter)) + geom_boxplot()
ggplot(dtrain, aes(x=mprestamos_personales)) + geom_boxplot()
ggplot(dtrain, aes(x=mcuentas_saldo)) + geom_boxplot()
ggplot(dtrain, aes(x=mactivos_margen)) + geom_boxplot()
ggplot(dtrain, aes(x=mcaja_ahorro)) + geom_boxplot()
ggplot(dtrain, aes(x=mcuenta_corriente)) + geom_boxplot()

summary(dtrain$ctrx_quarter)

# Vemos la distribución de los deciles
quantile(dtrain$ctrx_quarter, probs = c(0,0.5, 0.75, 0.9, 0.95, 0.99, 1))

# Supongamos que tenemos una lista de variables a las que queremos transformar
mis_variables <- c("ctrx_quarter",
                    "mprestamos_personales",
                    "mcuentas_saldo",
                    "mactivos_margen",
                    "mcaja_ahorro",
                    "mcuenta_corriente")

campos <- paste(mis_variables, collapse = " - ")
formula <- paste0( "clase_binaria ~ . -", campos )

experimento2 <- function() {
  gan <- c()
  for (s in semillas) {
    set.seed(s)
    in_training <- caret::createDataPartition(dataset$clase_binaria, p = 0.70,
                                              list = FALSE)
    train  <-  dataset[in_training, ]
    test   <-  dataset[-in_training, ]
    
    prefix <- "r_"
    for (var in mis_variables) {
      train[, (paste(prefix, var, sep = "")) := ntile(get(var), 10)]
      test[, (paste(prefix, var, sep = "")) := ntile(get(var), 10)]
    }
    
    r <- rpart(formula,
               data = train,
               xval = 0,
               cp = -1,
               minsplit = 20,
               minbucket = 10,
               maxdepth = 5)
    
    gan <- c(gan, calcular_ganancia(r, test))
  }
  mean(gan)
}

experimento2()

## ---------------------------
## Step 9: Un + poco de R, seleccionar las variables para modelar 
## ---------------------------

# Las mejores más la variables rankeadas
mis_variables_2 <- c("r_ctrx_quarter",
                    "active_quarter",
                    "r_mprestamos_personales",
                    "cprestamos_personales",
                    "r_mactivos_margen",
                    "r_mcuentas_saldo",
                    "ccomisiones_otras",
                    "r_mcuenta_corriente",
                    "cdescubierto_preacordado") 

campos <- paste(mis_variables_2, collapse = " + ")
formula <- paste0( "clase_binaria ~ ", campos )

modelo5 <- rpart(formula,
                    data = dtrain,
                    xval = 0,
                    cp = -1,
                    minsplit = 20,
                    minbucket = 10,
                    maxdepth = 5)

print(modelo5$variable.importance)
calcular_ganancia(modelo5, dtest)

## ---------------------------
## Step 10: Embeddings (Caseros)
## ---------------------------

# Hagamos interactuar algunas variables para ver si conseguimos alguna mejor
nuevas <- c()
for (var1 in mis_variables_2) {
    for (var2 in mis_variables_2) {
        if (var1 != var2) {
            nueva <- paste(var1, var2, sep = "___")
            dtrain[, (nueva) := get(var1) * get(var2)]
            dtest[, (nueva) := get(var1) * get(var2)]
            nuevas <- c(nuevas, nueva)
        }
    }
}

mis_variables_3 <- c(nuevas, mis_variables_2) 

campos2 <- paste(mis_variables_3, collapse = " + ")
formula2 <- paste0( "clase_binaria ~ ", campos2 )

modelo6 <- rpart(formula2,
                    data = dtrain,
                    xval = 0,
                    cp = -1,
                    minsplit = 20,
                    minbucket = 10,
                    maxdepth = 5)

print(modelo6$variable.importance)

# Importante: Que una modelo tenga otras variables importantes no implicar que
# sea mejor, ni peor. Eso se debe evaluar con los experimentos

##
## TAREA: Multiples experimentos. Un script por cada uno que debe incluir:
## - Feature engineering correctamente aplicado
## - Opt Bayesiana para el dataset que se incluya nuevas variables
## - Scorear en los datos de marzo y subir a kaggle el score.

#saldo en tarjetas, cantida de tarjetas, fecha cierre - vencimiento
#los limites de tarjeta no se actualiza muchas veces cuando debe
#las 2 variables, cuando las proyecto, esa variable queda más ordenada
#limite total de la tarjeta y lo consumido


