##
## Sobre más features
##
## ---------------------------
## Step 1: Setup
## ---------------------------
##
## <Insert a smart quote here about more is better>.
## --- Ale

rm(list = ls())
gc(verbose = FALSE)

# Librerías necesarias
require("data.table")
require("rpart")
require("ggplot2")
require("lightgbm")
require("xgboost")

# Poner la carpeta de la materia de SU computadora local
setwd(gsub(" ", "", paste(gsub('/', '\\\\', gsub("/m_d_m/dmef", "", getwd())), "\\m_d_m\\dmef")))

# Poner sus semillas
semillas <- c(309367, 149521, 690467, 699191, 795931)

# Cargamos los datasets y nos quedamos solo con 202101 y 202103
dataset <- fread("./datasets/competencia2_2022.csv.gz")
marzo <- dataset[foto_mes == 202103]
mayo <- dataset[foto_mes == 202105]
rm(dataset)

# Clase BAJA+1 y BAJA+2 juntas
clase_binaria <- ifelse(marzo$clase_ternaria == "CONTINUA", 0, 1)
clase_real <- marzo$clase_ternaria
marzo$clase_ternaria <- NULL
mayo$clase_ternaria <- NULL

## ---------------------------
## Step 2: XGBoost, un modelo simple ...
## ---------------------------

dtrain <- xgb.DMatrix(
        data = data.matrix(marzo),
        label = clase_binaria, missing = NA)

# Empecemos con algo muy básico
param_fe <- list(
            max_depth = 2,
            eta = 0.1,
            objective = "binary:logistic")
nrounds <- 5

xgb_model <- xgb.train(params = param_fe, data = dtrain, nrounds = nrounds)

## ---------------------------
## Step 3: XGBoost, ... para generar nuevas variables
## ---------------------------

# https://research.facebook.com/publications/practical-lessons-from-predicting-clicks-on-ads-at-facebook/

new_features <- xgb.create.features(model = xgb_model, data.matrix(marzo))
colnames(new_features)[150:173]

## ---------------------------
## Step 4: Entendiendo como se construyen.
## ---------------------------

xgb.plot.tree(colnames(new_features), xgb_model, trees = 1)


## ---------------------------
## Step 5: Viendo cuán importantes son las nuevas variables, pero con un LGBM!!!
## ---------------------------

dtrain_lgb  <- lgb.Dataset(
            data = data.matrix(new_features),
            label = clase_binaria)

mlgb <- lgb.train(
            dtrain_lgb,
            params = list(
                objective = "binary",
                max_bin = 15,
                min_data_in_leaf = 4000,
                learning_rate = 0.05),
            verbose = -1)

lgb.importance(mlgb)

## ---------------------------
## Step 6: Jugando un poco más con los parámetros del XGBoost
## ---------------------------

set.seed(semillas[1])
#parametros para random forest, para obtener nuevas features
param_fe2 <- list(
                colsample_bynode = 0.8,
                learning_rate = 1,
                max_depth = 3, # <--- IMPORTANTE CAMBIAR
                num_parallel_tree = 10, # <--- IMPORTANTE CAMBIAR
                subsample = 0.8,
                objective = "binary:logistic"
            )

xgb_model2 <- xgb.train(params = param_fe2, data = dtrain, nrounds = 1)

# Veamos un paso a paso
new_features2 <- xgb.create.features(model = xgb_model2, data.matrix(marzo))

colnames(new_features2)[150:230]

dtrain_lgb2  <- lgb.Dataset(
            data = data.matrix(new_features2),
            label = clase_binaria)

mlgb2 <- lgb.train(
            dtrain_lgb2,
            params = list(
                objective = "binary",
                max_bin = 15,
                min_data_in_leaf = 4000,
                learning_rate = 0.05),
            verbose = -1)

lgb.importance(mlgb2)$Feature

# Filtrando las features que entraron
## Preguntas
## - ¿Entraron todas las variables?

## ---------------------------
## Step 7: Sumando canaritos
## ---------------------------

marzo[  , ctrx_quarter_normalizado := ctrx_quarter ]
marzo[ cliente_antiguedad==1 , ctrx_quarter_normalizado := ctrx_quarter * 5 ]
marzo[ cliente_antiguedad==2 , ctrx_quarter_normalizado := ctrx_quarter * 2 ]
marzo[ cliente_antiguedad==3 , ctrx_quarter_normalizado := ctrx_quarter * 1.2 ]

#variable extraida de una tesis de maestria de Irlanda
marzo[  , mpayroll_sobre_edad  := mpayroll / cliente_edad ]

#se crean los nuevos campos para MasterCard  y Visa, teniendo en cuenta los NA's
#varias formas de combinar Visa_status y Master_status
marzo[ , mv_status01       := pmax( Master_status,  Visa_status, na.rm = TRUE) ]
marzo[ , mv_status02       := Master_status +  Visa_status ]
marzo[ , mv_status03       := pmax( ifelse( is.na(Master_status), 10, Master_status) , ifelse( is.na(Visa_status), 10, Visa_status) ) ]
marzo[ , mv_status04       := ifelse( is.na(Master_status), 10, Master_status)  +  ifelse( is.na(Visa_status), 10, Visa_status)  ]
marzo[ , mv_status05       := ifelse( is.na(Master_status), 10, Master_status)  +  100*ifelse( is.na(Visa_status), 10, Visa_status)  ]

marzo[ , mv_status06       := ifelse( is.na(Visa_status), 
                                        ifelse( is.na(Master_status), 10, Master_status), 
                                        Visa_status)  ]

marzo[ , mv_status07       := ifelse( is.na(Master_status), 
                                        ifelse( is.na(Visa_status), 10, Visa_status), 
                                        Master_status)  ]


#combino MasterCard y Visa
marzo[ , mv_mfinanciacion_limite := rowSums( cbind( Master_mfinanciacion_limite,  Visa_mfinanciacion_limite) , na.rm=TRUE ) ]

marzo[ , mv_Fvencimiento         := pmin( Master_Fvencimiento, Visa_Fvencimiento, na.rm = TRUE) ]
marzo[ , mv_Finiciomora          := pmin( Master_Finiciomora, Visa_Finiciomora, na.rm = TRUE) ]
marzo[ , mv_msaldototal          := rowSums( cbind( Master_msaldototal,  Visa_msaldototal) , na.rm=TRUE ) ]
marzo[ , mv_msaldopesos          := rowSums( cbind( Master_msaldopesos,  Visa_msaldopesos) , na.rm=TRUE ) ]
marzo[ , mv_msaldodolares        := rowSums( cbind( Master_msaldodolares,  Visa_msaldodolares) , na.rm=TRUE ) ]
marzo[ , mv_mconsumospesos       := rowSums( cbind( Master_mconsumospesos,  Visa_mconsumospesos) , na.rm=TRUE ) ]
marzo[ , mv_mconsumosdolares     := rowSums( cbind( Master_mconsumosdolares,  Visa_mconsumosdolares) , na.rm=TRUE ) ]
marzo[ , mv_mlimitecompra        := rowSums( cbind( Master_mlimitecompra,  Visa_mlimitecompra) , na.rm=TRUE ) ]
marzo[ , mv_madelantopesos       := rowSums( cbind( Master_madelantopesos,  Visa_madelantopesos) , na.rm=TRUE ) ]
marzo[ , mv_madelantodolares     := rowSums( cbind( Master_madelantodolares,  Visa_madelantodolares) , na.rm=TRUE ) ]
marzo[ , mv_fultimo_cierre       := pmax( Master_fultimo_cierre, Visa_fultimo_cierre, na.rm = TRUE) ]
marzo[ , mv_mpagado              := rowSums( cbind( Master_mpagado,  Visa_mpagado) , na.rm=TRUE ) ]
marzo[ , mv_mpagospesos          := rowSums( cbind( Master_mpagospesos,  Visa_mpagospesos) , na.rm=TRUE ) ]
marzo[ , mv_mpagosdolares        := rowSums( cbind( Master_mpagosdolares,  Visa_mpagosdolares) , na.rm=TRUE ) ]
marzo[ , mv_fechaalta            := pmax( Master_fechaalta, Visa_fechaalta, na.rm = TRUE) ]
marzo[ , mv_mconsumototal        := rowSums( cbind( Master_mconsumototal,  Visa_mconsumototal) , na.rm=TRUE ) ]
marzo[ , mv_cconsumos            := rowSums( cbind( Master_cconsumos,  Visa_cconsumos) , na.rm=TRUE ) ]
marzo[ , mv_cadelantosefectivo   := rowSums( cbind( Master_cadelantosefectivo,  Visa_cadelantosefectivo) , na.rm=TRUE ) ]
marzo[ , mv_mpagominimo          := rowSums( cbind( Master_mpagominimo,  Visa_mpagominimo) , na.rm=TRUE ) ]

#a partir de aqui juego con la suma de Mastercard y Visa
marzo[ , mvr_Master_mlimitecompra:= Master_mlimitecompra / mv_mlimitecompra ]
marzo[ , mvr_Visa_mlimitecompra  := Visa_mlimitecompra / mv_mlimitecompra ]
marzo[ , mvr_msaldototal         := mv_msaldototal / mv_mlimitecompra ]
marzo[ , mvr_msaldopesos         := mv_msaldopesos / mv_mlimitecompra ]
marzo[ , mvr_msaldopesos2        := mv_msaldopesos / mv_msaldototal ]
marzo[ , mvr_msaldodolares       := mv_msaldodolares / mv_mlimitecompra ]
marzo[ , mvr_msaldodolares2      := mv_msaldodolares / mv_msaldototal ]
marzo[ , mvr_mconsumospesos      := mv_mconsumospesos / mv_mlimitecompra ]
marzo[ , mvr_mconsumosdolares    := mv_mconsumosdolares / mv_mlimitecompra ]
marzo[ , mvr_madelantopesos      := mv_madelantopesos / mv_mlimitecompra ]
marzo[ , mvr_madelantodolares    := mv_madelantodolares / mv_mlimitecompra ]
marzo[ , mvr_mpagado             := mv_mpagado / mv_mlimitecompra ]
marzo[ , mvr_mpagospesos         := mv_mpagospesos / mv_mlimitecompra ]
marzo[ , mvr_mpagosdolares       := mv_mpagosdolares / mv_mlimitecompra ]
marzo[ , mvr_mconsumototal       := mv_mconsumototal  / mv_mlimitecompra ]
marzo[ , mvr_mpagominimo         := mv_mpagominimo  / mv_mlimitecompra ]

set.seed(semillas[1])
for (i in 1:20)  {
    marzo[, paste0("canarito", i) := runif(nrow(marzo))]
}

new_features3 <- xgb.create.features(model = xgb_model2, data.matrix(marzo))

# Veamos que están las variables que generamos
colnames(new_features3)[150:230]

dtrain_lgb3  <- lgb.Dataset(
            data = data.matrix(new_features3),
            label = clase_binaria)

mlgb3 <- lgb.train(
            dtrain_lgb3,
            params = list(
                objective = "binary",
                max_bin = 15,
                min_data_in_leaf = 4000,
                learning_rate = 0.05,
                num_iterations = 500 ## <-- aumento las iteraciones
            ),
            verbose = -1)

var_importance <- lgb.importance(mlgb3)$Feature

# Veamos cuantas canaritos aparecieron
list_canaritos <- grepl("canarito", var_importance)

# Cuantos canaritos aparecieron?
length(var_importance[list_canaritos])

#desde la 20 a la 51 aparecen canaritos, muchas despues de la 51 se pierden o las toma como malas

# En que posiciones
idx <- seq(length(list_canaritos))
idx[list_canaritos]

# En que posiciones aprecieron el resto de las variables generadas
list_new_features <- grepl("V\\d+", var_importance)
idx[list_new_features]

#puede que haya variables importantes después de los canaritos

#nohup Rscript archivo.R &> archivo_log.log &
#top
#less archivo_salida.log
#shift g

#son útiles las regularizaciones? usar la opt bay para ver cual usar, una, dos, tres
#probar la optimización de hiperparámetros entrenando en enero y validando o midiendo en marzo