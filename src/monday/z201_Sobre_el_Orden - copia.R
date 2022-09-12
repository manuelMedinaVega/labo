##
## Sobre el Orden
##
## ---------------------------
## Step 1: Ejecutando un árbol
## ---------------------------
##
## A tree with strong roots laughs at storms.
## --- Malay proverb
##

rm( list=ls() )  #remove all objects
gc()    

# Librerías necesarias
require("data.table")
require("rpart")
require("ggplot2")


# Poner la carpeta de la materia de SU computadora local
setwd(gsub(" ", "", paste(gsub('/', '\\\\', gsub("/m_d_m/dmef", "", getwd())), "\\m_d_m\\dmef")))

# Poner sus semillas
semillas <- c(309367, 149521, 690467, 699191, 795931)

# Cargamos el dataset
dataset <- fread("./datasets/competencia1_2022.csv")
dtrain <- dataset[foto_mes == 202101]

# Generamos el primer modelo
arbol <- rpart(formula =    "clase_ternaria ~ .",
                 data =      dtrain,
                 xval =      0,
                 cp =       -0.3,
                 minsplit =  0,
                 minbucket = 1,
                 maxdepth =  4)

#xval: number of cross-validations. (default: 10)

#cp: complexity parameter. Any split that does not decrease the overall lack of fit by a factor of cp 
  #is not attempted. The main role of this parameter is to save computing time by pruning off splits 
  #that are obviously not worthwhile. Essentially, the user informs the program that any split which 
  #does not improve the fit by cp will likely be pruned off by cross-validation, and that hence the 
  #program need not pursue it. (default: 0.01)

#minsplit: the minimum number of observations that must exist in a node in order for a split to be 
  #attempted. (default: 20)

#minbucket: the minimum number of observations in any terminal node. If only one of minbucket or minsplit is 
  #specified, the code either sets minsplit to minbucket*3 or minbucket to minsplit/3, as appropriate.
  #o sea que pone siempre el minsplit como el triple del minbucket. (default: round(minsplit/3))

#maxdepth: Set the maximum depth of any node of the final tree, with the root node counted as depth 0. 
  #(default: 30)

#control: optional parameters for controlling tree growth. For example: 
  #control=rpart.control(minsplit=30, cp=0.001)
  #requires that the minimun number of observations in a node be 30 before attempting a split and that
  #a split must decrease the overall lack of fit by a factor of 0.001 (cost complexity factor) before
  #being attempted

print(arbol)

## Preguntas
## Usualmente se suele cortar las variables en 2 intervalos
## - ¿Se podría cortar en más intervalos?, sí
## - ¿Cuál sería el costo?, es más complejo, se requiere mayor procesamiento
## - ¿Se obtendrían mejores resultados?, sí, pero igual que al tener más profundidad
##
## Una de las muchas ventajas que tienen los árboles es la simpleza que tienen
## para ser implementados en fácilmente en sistemas productivos, dado que la
## reescritura de las reglas de salida es muy simple.


## ---------------------------
## Step 2: De árbol a tabla
## ---------------------------

# La siguiente función devuelve todas las hojas (nodos terminales) en una tabla
# para poder analizar mejor nuestro árbol.
tablahojas <- function(arbol, datos, target = "clase_ternaria") {
  # Tomamos la columna con el target
  target_vector <- datos[, get(target)]
  # Tomamos las clases de nuestro target
  classes <- unique(target_vector)
  # Tomamos las posicion de las hojas que aplican a los registro de nuestro ds
  row_leaf <- unique(arbol$where)
  leaves <- data.table(row_frame = row_leaf)
  setkey(leaves,row_frame)
  # Relacion target ~ hojas
  leaves_target <- dcast(
    data.table(
      target = target_vector,
      leaf = arbol$where),
    leaf ~ target, length,
    value.var = "target")
  setkey(leaves_target, leaf)
  # Juntamos todo
  leaves_target <- leaves_target[leaves, nomatch = 0]
  # Sumamos algunas columnas calculadas
  colnames(leaves_target[, classes, with = FALSE])[apply(
    leaves_target[, classes, with = FALSE], 1, which.max)]
  # Clase mayoritaria
  leaves_target[, y := colnames(
                    leaves_target[, classes, with = FALSE]
                  )[apply(leaves_target[, classes, with = FALSE],
                   1, which.max)]]
  # Cantidad de elementos de la hoja
  leaves_target[, TOTAL := unlist(Reduce(function(a, b) Map(`+`, a, b), .SD)),
                 .SDcols = classes]
  leaves_target
}

# Ejecutamos la función sobre nuestro modelo, con nuestros datos
hojas <- tablahojas(arbol, dtrain)
print(hojas)
sum(hojas[,TOTAL])

## Preguntas
## - ¿Con qué criterio eligió la clase de cada hoja que determino la
##   clasificación de los registros? la clase con mayor número de observaciones
## - ¿Cuántas hojas con BAJAS+2 hay? 0

## ---------------------------
## Step 3: Calculando la ganancia de cada hoja
## ---------------------------

# Agregamos un nuevo campo de nombre ganancia
hojas[, ganancia := `BAJA+2` * 78000 - 2000 * (CONTINUA + `BAJA+1`)]

## Pregunta
## - ¿Cuantás hojas que no son BAJA+2 tienen aún así ganancia positiva?

## ---------------------------
## Step 4: Sumarizando el envío
## ---------------------------

print(hojas[ganancia > 0, .(
    ganancia = sum(ganancia),
    enviados = sum(TOTAL),
    sevan = sum(`BAJA+2`))])

## Preguntas
## Si enviaramos todos los casos de las hojas con ganancia positiva
## - ¿Cuánta ganancia tendríamos? 18912000
## - ¿Cuánta personas estimularíamos? 9904
## - ¿A cuántas personas acertaríamos? 484


## ---------------------------
## Step 5: Binarizando la salida (en tu cara RAE)
## ---------------------------

# Creamos un nuevo target binario
dtrain[, clase_binaria := ifelse(
                            clase_ternaria == "CONTINUA",
                                "NO",
                                "SI"
                            )]
# Borramos el target viejo
dtrain[, clase_ternaria := NULL]

arbolbinario <- rpart("clase_binaria ~ .",
                 data =      dtrain,
                 xval =      0,
                 cp =       -0.3,
                 minsplit =  0,
                 minbucket = 5,
                 maxdepth =  4)
# Transformamos las hojas a una tabla
hojasbinario <- tablahojas(arbolbinario, dtrain, "clase_binaria")

# Y agregamos la ganancia de cada hoja
hojasbinario[, ganancia := SI * 78000 - 2000 * NO]
print(hojasbinario)
# Por último sumarizamos
print(hojasbinario[ganancia > 0,
 .(ganancia = sum(ganancia), enviados = sum(TOTAL), sevan = sum(SI))])

## Pregunta
## - ¿Considera que la agrupación de clases fue positiva para la  ganancia? sí, aumentó

## ---------------------------
## Step 6: Salida probabilísticas
## ---------------------------

# Calculamos la probabilidad de evento en cada hoja
hojasbinario[, p_evento := SI / (SI + NO)]

# Ordenamos de forma descendiente las probabilidades, ya que nos interesan
# ante todo las probabilidades más altas
hojasordenadas <- hojasbinario[order(-p_evento),]

# Calculamos la ganancia acumulada, desde con la probabilidad desde la primera
# fila con probabilidad más alta hasta la fila N, para cada fila.
hojasordenadas[, gan_acum := cumsum(ganancia)]

print(hojasordenadas)

# TAREAS:
# - Calculé la probabilidad de NO evento
hojasbinario[, p_no_evento := noevento / (evento + noevento)]
# - Puede pasar que dos hojas tengan la misma probabilidad, escriba una query 
#   que las agrupe.

## Preguntas
## - ¿Cómo ve la relación entre la probabilidad ordenada y la hojas con
##   ganancia?, a partir de p-evento = 0.028, se obtienen ganancias positivas
## - ¿Cuál es la máxima ganancia posible es nuestro árbol?, 20656000
## - ¿Cuál es el `punto de corte` que sugiere?, p-evento = 0.028
## - ¿Por qué es distinto al teórico?
## - ¿Es nuestro `punto de corte` es igual de útil?

## ---------------------------
## Step 7: Graficando la ganancia
## ---------------------------

ggplot(hojasordenadas, aes(x = p_evento ,y = gan_acum)) +
     scale_x_reverse() +
     geom_line(size = 1)

## Pregunta
## ¿Cómo interpretamos este gráfico?

## ---------------------------
## Step 8: No todo es plata en la vida
## ---------------------------

## NOTA:
## Existen más formas de medir la calidad del modelo a través de las
## probabilidades que nos entrega. A nivel global podemos usar `AUC`: área bajo
## la curva ROC:https://en.wikipedia.org/wiki/Receiver_operating_characteristic
## que nos muestra el comportamiento global de la performance del modelo.
##
## Para la **curva ROC** vamos a necesitar construir una Matriz de confusión
## https://en.wikipedia.org/wiki/Confusion_matrix#Table_of_confusion por cada
## punto de corte posible.

# Vamos a sumar las variables `tp`, `tn`, `fp` y `fn`
hojasordenadas[, c("evento_acum","noevento_acum") :=
                  list(cumsum(evento),cumsum(noevento))]
total_evento <- hojasordenadas[, sum(evento)]
total_noevento <- hojasordenadas[, sum(noevento)]
hojasordenadas[, c("evento_restantes", "noevento_restantes") :=
            list(total_evento - evento_acum, total_noevento - noevento_acum)]

hojasordenadas[, tp := evento_acum]
hojasordenadas[, tn := noevento_restantes]
hojasordenadas[, fp := noevento_acum]
hojasordenadas[, fn := evento_restantes]

# Para validar los cálculos anteriores vamos a visualizar solo los campos
# importantes
print(hojasordenadas[, .(p_evento, evento, noevento, tp, tn, fp, fn)])

## ---------------------------
## Step 9: Armando nuestra curva ROC
## ---------------------------

# Calculamos las variables necesarios para la curva ROC
hojasordenadas[, tpr := (tp / (tp + fn))]
hojasordenadas[, fpr := (fp / (fp + tn))]

# La graficamos
ggplot(hojasordenadas, aes(x = fpr, y = tpr)) +
  # Agregamos la función identidad
  geom_abline(intercept = 0, slope = 1) +
  geom_line(lwd = 1)

## Pregunta
## ¿Qué representa la curva ROC?, cuanto tolero los errores, cuanto puede llegar a fallar un test

## ---------------------------
## Step 10: Calculando el área bajo la curva
## ---------------------------

## NOTA: Como es muy complejo reflejar en palabras una curva, se suele calcular
## el área bajo su curva (auc) y reflejar ese valor como métrica de la 
## calidad del modelo.

# Calculamos su área, necesita instalar el siguiente paquete
# install.packages("geometry")
require("geometry")

x <- c(hojasordenadas$fpr,1)
y <- c(hojasordenadas$tpr, 0)
# El valor de la auc
print(polyarea(x, y))


## Preguntas
## -¿AUC es una métrica global o local?, global, roc es local
## -¿Pueden dos curvas distintas tener un mismo valor de AUC? sí

## ---------------------------
## Step 11: No limitarnos a la ROC
## ---------------------------

# Podemos construir una curva para el accuraccy
hojasordenadas[, acc := ((tp + tn) / (tp + tn + fp + fn))]

# Y graficarla
ggplot(hojasordenadas, aes(x = p_evento, y = acc)) +
  geom_line(lwd = 1)

## Preguntas
## - ¿Se ajusta esta curva a nuestra necesidad de negocio?
## -¿Cuál es el threshold optimo según la curva de accuracy?
## - Si hubiéramos elegido nuestro modelo usando el accuracy, ¿Cuanta plata
##   hubiera ganado o perdido la empresa?
## - ¿Es necesario que la salida del modelo sea un probabilidad para aplicar
##   estos conceptos?

## TAREA:
## - Construya la curva correspondiente al F1 Score.
## - La métrica F1, es criticado por dar un mismo peso a recall y al precision.
##   Por esto mismo, a alguien se le ocurrió el F-Beta. Construya esta última
##   para varios Betas.
## - ¿Hay algún Beta que tenga un **punto de corte** similar al nuestro?
