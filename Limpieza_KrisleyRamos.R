#archivos CSV
library(readxl)

datos <- read.csv("C:/Users/krisl/Downloads/heart_disease_para_limpiar.csv", head = TRUE, sep = ";")
datos

#Muestra una proporsión de Datos
head(datos)

#Dimensión de  los Datos
dim(datos)

# Reemplaza "Na" o "na" con NA en todas las columnas
datos <- as.data.frame(lapply(datos, function(col) gsub("(^|[^a-zA-Z])Na([^a-zA-Z]|$)", "\\1NA\\2", col)))
datos[datos == "NA"] <- NA
sum(is.na(datos))
colSums(is.na(datos))


# Calcula el número de valores faltantes en cada columna
datos_faltantes <- colSums(is.na(datos))
library(Rcpp)
library(Amelia)
missmap(datos)


# Lista de columnas cualitativas en las que deseas aplicar la moda
columnas_cualitativas <- c("sex","tipo_dolor","azucar_ayunas","res_electrocardio","angina","pendiente","thal","enferm_cardiaca")

# Calcular la moda para las columnas cualitativas
moda_cualitativa <- sapply(columnas_cualitativas, function(col) {
  moda <- names(sort(table(datos[[col]], useNA = "ifany"), decreasing = TRUE))[1]
  return(moda)
})

for (col in columnas_cualitativas) {
  datos[is.na(datos[, col]), col] <- moda_cualitativa[col]
}

sum(is.na(datos))
colSums(is.na(datos))

columnas_seleccionadas <- c("edad", "pres_art_reposo", "colesterol", "frec_cardica", "depresion", "num_vasos")

# Convierte las columnas numéricas a su tipo correspondiente
datos[columnas_seleccionadas] <- lapply(datos[columnas_seleccionadas], as.numeric)
str(datos)

# Imputación por mediana
library(caret)
library(mice)

# Calcula la mediana de cada columna seleccionada, ignorando los NA
medianas <- sapply(datos[, columnas_seleccionadas], function(x) median(x, na.rm = TRUE))
print(medianas)

# Crea un objeto de preprocesamiento utilizando el método medianImpute
preproc_obj <- preProcess(datos[, columnas_seleccionadas], method = "medianImpute")

# Aplica el objeto de preprocesamiento a las columnas seleccionadas en los datos
datos_imputado <- predict(preproc_obj, newdata = datos[, columnas_seleccionadas])

# Reemplaza las columnas originales con las columnas imputadas
datos[, columnas_seleccionadas] <- datos_imputado
colSums(is.na(datos))
library(Rcpp)
library(Amelia)
missmap(datos)

library(datasets) 
library(VIM) 
aggr(datos,numbers=T,sortVar=T)
# Guardar los datos limpios en un archivo CSV
write.csv(datos, file = "limpio1.csv", row.names = FALSE)

