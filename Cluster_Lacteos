# Instalar y cargar librerías necesarias
library(factoextra)
library(cluster)
library(ggplot2)
library(NbClust)
library(ggpubr)
library(corrplot)
library(Hmisc)
library(lattice)
library(survival)
library(Formula)
library(dplyr)
library(fpc)
library(pvclust)
library(viridis)
library(gplots)
library(pheatmap)
library(clValid)
library(dendextend)

# Paso 1: Cargar los datos
data <- read.csv("C:/Users/krisl/Downloads/mercadosleche.csv")
head(data) 
# Ver estructura de los datos
str(data)

# Paso 2: Análisis estadístico descriptivo
summary(data)

# Histograma de cada variable
numeric_vars <- names(data)[sapply(data, is.numeric)]

for (var in numeric_vars) {
  p <- ggplot(data, aes_string(var)) +
    geom_histogram(bins = 30, fill = "skyblue", color = "black") +
    labs(title = paste("Histograma de", var), x = var, y = "Frecuencia") +
    theme_minimal()
  print(p)
}
# library(funModeling)
# df_status(data)
# plot_num(data)

library(ggplot2)
# correlaciones
par(mfrow = c(1, 1))
plot(data, col = alpha("steelblue", 0.4), pch = 19, las = 1)

# Paso 3: Análisis de correlación
cor_matrix <- cor(data[, numeric_vars], use = "complete.obs")
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.8, tl.col = "black", addCoef.col = "black", number.cex = 0.7)

# Paso 4: Determinación del número óptimo de clusters
# Método del codo (elbow method)
set.seed(123)
wss <- numeric(15)
for (i in 1:15) {
  kmeans_result <- kmeans(data[, numeric_vars], centers = i, nstart = 25)
  wss[i] <- kmeans_result$tot.withinss
}
plot(1:15, wss, type = "b", xlab = "Número de clústeres", ylab = "Suma de cuadrados dentro del clúster (WSS)", main = "Método del codo")

# Método de la silueta
sil_width <- numeric(15)
for (i in 2:15) {
  pam_fit <- pam(data[, numeric_vars], diss = FALSE, k = i)
  sil_width[i] <- pam_fit$silinfo$avg.width
}
plot(1:15, sil_width, type = "b", xlab = "Número de clústeres", ylab = "Anchura media de la silueta", main = "Método de la silueta")

# Método de la brecha (gap statistic)
fviz_nbclust(data[, numeric_vars], kmeans, method = "gap_stat")

# Método de NbClust
resnumclas <- NbClust(data[, numeric_vars], distance = "euclidean", min.nc = 2, max.nc = 5, method = "kmeans", index = "alllong")

# Paso 5: Aplicación de K-means clustering
set.seed(123)
num_clusters <- 2  # Usando el número óptimo de clusters determinado
kmeans_result <- kmeans(data[, numeric_vars], centers = num_clusters, nstart = 25)

# Visualización de los clústeres K-means
fviz_cluster(kmeans_result, data = data[, numeric_vars], geom = "point", stand = FALSE, ellipse.type = "norm") + 
  labs(title = "Clústeres de vehículos (K-means)", x = "Componente Principal 1", y = "Componente Principal 2")

# Paso 6: Interpretación y visualización de los resultados
data$Cluster_KMeans <- kmeans_result$cluster
cluster_summary <- aggregate(data[, numeric_vars], by = list(Cluster = data$Cluster_KMeans), mean)
print(cluster_summary)

# Guardar el resultado con clústeres asignados
write.csv(data, "C:/Users/krisl/Downloads/leche_con_clusters1.csv", row.names = FALSE)

# Paso 7: Conclusiones
# Basándonos en los métodos aplicados, observamos que cada técnica de clúster proporciona una agrupación ligeramente diferente.
# K-means es útil por su simplicidad y velocidad. La elección del número óptimo de clústeres fue respaldada por métodos gráficos como el codo y la silueta.

# PCA y K-means clustering
pca_data <- prcomp(data[, numeric_vars])
p1 <- fviz_pca_ind(X = pca_data, geom = "point", title = "PCA - Productos Lacteos", palette = "jco") +
  theme_bw() + 
  theme(legend.position = "bottom")

# K-means clustering
km_data <- kmeans(x = data[, numeric_vars], centers = 2)
p2 <- fviz_cluster(object = km_data, data = data[, numeric_vars],
                   ellipse.type = "norm", geom = "point", main = "Productos Lacteos", 
                   stand = FALSE, palette = "jco") + 
  theme_bw() + 
  theme(legend.position = "none")

# Hierarchical clustering 
p3 <- fviz_dend(x = hclust(dist(data[, numeric_vars])), k = 2, k_colors = "jco",
                show_labels = FALSE, main = "Productos Lacteos") 
ggarrange(p1, p2)

ggarrange(p3)

# Hopkins statistics
library(clustertend) 
set.seed(321) 

# Visual Assessment of cluster Tendency (VAT)
dist_data <- dist(data[, numeric_vars], method = "euclidean") 
p1 <- fviz_dist(dist.obj = dist_data, show_labels = FALSE) +
  labs(title = "Productos Lacteos") +
  theme(legend.position = "bottom") 
ggarrange(p1)

# Numero optimo de clusters
# Elbow method
datos <- scale(data[, numeric_vars]) 
fviz_nbclust(x = datos, FUNcluster = kmeans, method = "wss") + 
  labs(title = "Numero optimo de clusters")
# Average silhouette method
fviz_nbclust(x = datos, FUNcluster = kmeans, method = "silhouette") +
  labs(title = "Numero optimo de clusters")

# Gap statistic method
fviz_nbclust(x = datos, FUNcluster = kmeans, method = "gap_stat", nboot = 500,
             verbose = FALSE, nstart = 25) +
  labs(title = "Numero optimo de clusters")
# NbClust
#numero_clusters <- NbClust(data = datos, distance = "euclidean", min.nc = 2,
    #                       max.nc = 10, method = "kmeans", index = "alllong")
#fviz_nbclust(numero_clusters)

# Validacion interna de los clusters: estabilidad, silhouette y Dunn
km_clusters <- eclust(x = datos, FUNcluster = "kmeans", k = 3, seed = 123, 
                      hc_metric = "euclidean", nstart = 50, graph = FALSE)
fviz_silhouette(sil.obj = km_clusters, print.summary = TRUE, palette = "jco", 
                ggtheme = theme_classic())

# Media silhouette por cluster 
km_clusters$silinfo$clus.avg.widths

# Coeficiente silhouette para cada observacion 
head(km_clusters$silinfo$widths)

km_clusters$silinfo$widths %>% filter(sil_width <= 0)

p <- fviz_cluster(object = km_clusters, geom = "point", ellipse.type = "norm", 
                  palette = "jco")
p + 
  geom_point(data = p$data[c(112, 128),], colour = "firebrick", size = 2.5) +
  theme_bw() + theme(legend.position = "bottom")

km_clusters <- eclust(x = datos, FUNcluster = "kmeans", k = 5, seed = 123, 
                      hc_metric = "euclidean", nstart = 50, graph = FALSE) 
p1 <- fviz_cluster(object = km_clusters, geom = "point", ellipse.type = "norm",
                   palette = "jco") + 
  theme_classic() + 
  theme(legend.position = "none")
p2 <- fviz_silhouette(sil.obj = km_clusters, print.summary = FALSE, 
                      palette = "jco", ggtheme = theme_classic()) +
  theme(legend.position = "none")
ggarrange(p1, p2)





