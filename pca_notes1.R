
data.matrix = matrix(nrow = 100, ncol = 10)

colnames(data.matrix) = c(
  paste0("wt", 1:5), # "wild type"
  paste0("ko", 1:5)  # knocked out samples
)

rownames(data.matrix) = paste0("gene", 1:100)

# Fake read counts

set.seed(42)

for (i in 1:100) {
  wt.values = rpois(5, lambda = sample(x = 10:1000, size = 1))
  ko.values = rpois(5, lambda = sample(x = 10:1000, size = 1))
  
  data.matrix[i, ] = c(wt.values, ko.values)
}

head(data.matrix)

# t() transpose matrix

pca = prcomp(t(data.matrix), scale = TRUE)

# graphs

plot(pca$x[, 1], pca$x[, 2])

# See variation

# We use the square of standards deviation to calculate how much variation in the original data each principal component accounts for.
pca.var = pca$sdev^2

pca.var.per = round(pca.var / sum(pca.var)*100, 1)

barplot(pca.var.per, main = "Scree plot", xlab = "Principal component", ylab = "Percent variation")

# Making it for ggplot2

library(ggplot2)

pca.data = data.frame(Sample = rownames(pca$x),
                      X = pca$x[, 1],
                      Y = pca$x[, 2])

ggplot(data = pca.data, aes(x = X, y = Y, label = Sample)) +
  geom_text() +
  xlab(paste0("PC1 - ", pca.var.per[1], "%")) +
  ylab(paste0("PC2 - ", pca.var.per[2], "%")) +
  theme_bw() +
  ggtitle("My PCA Graph")

loading_scores = pca$rotation[, 1]
gene_scores = abs(loading_scores)

gene_score_ranked = sort(gene_scores, decreasing = T)
top_10_genes = names(gene_score_ranked[1:10])

top_10_genes

pca$rotation[top_10_genes, 1]


data.matrix = matrix(nrow = 2, ncol = 6)
colnames(data.matrix) = paste0("Mouse", 1:6)
rownames(data.matrix) = paste0("Gene", 1:2)

data.matrix[1,] = c(10, 11, 8, 3, 2, 1)

plot(x = data.matrix[1,], y = rep(0, 6))

datos = data.frame(Sample = colnames(data.matrix), Gene1 = data.matrix[1,])

ggplot(datos, aes(Gene1, rep(0, 6), label = Sample)) +
  geom_text()

datos$Gene2 = c(6, 4, 5, 3, 2.8, 1)

ggplot(datos, aes(Gene1, Gene2, label = Sample)) +
  geom_text()

datos$Gene3 = c(12, 9, 10, 2.5, 1.3, 2)

library(plotly)

p = plot_ly(datos, x = ~Gene1, y = ~Gene2, z = ~Gene3)
p = add_markers(p)
p = layout(p, scene = list(
  xaxis = list(title = "Gene 1"),
  yaxis = list(title = "Gene 2"),
  zaxis = list(title = "Gene 3")
))

p

datos$Gene4 = c(5, 7, 6, 2, 4, 7)

# We can't no longer make 4 dimensional plots

We make our pca

# lets go back to two genes

ggplot(datos, aes(Gene1, Gene2, label = Sample)) +
  geom_text(vjust = 0, hjust = 0) +
  geom_point() +
  geom_segment(aes(xend = Gene1, yend = 0), linetype = 2, color = "blue") +
  annotate(geom = "text", x = 6, y = 1, label = "Average measurement for Gene 1", color = "blue") +
  geom_segment(aes(xend = 0, yend = Gene2), linetype = 2, color = "red") +
  annotate(geom = "text", x = 6, y = 3, label = "Average measurement for Gene 2", color = "red") +
  theme_minimal()

# Average values, we can calculate the center of the area

avg_gene1 = mean(datos$Gene1)
avg_gene2 = mean(datos$Gene2)

ggplot(datos, aes(Gene1, Gene2, label = Sample)) +
  geom_text(vjust = 0, hjust = 0) +
  geom_point() +
  geom_segment(aes(xend = Gene1, yend = 0), linetype = 2, color = "blue") +
  annotate(geom = "point", x = avg_gene1, y = 0, color = "blue", size = 4, shape = 17) +
  annotate(geom = "text", x = avg_gene1, y = 0.5, label = "Average measurement for Gene 1", color = "blue") +
  annotate(geom = "point", x = 0, y = avg_gene2, color = "red", size = 4, shape = 17) +
  geom_segment(aes(xend = 0, yend = Gene2), linetype = 2, color = "red") +
  annotate(geom = "text", x = 1.3, y = avg_gene2-0.2, label = "Average measurement for Gene 2", color = "red") +
  annotate(geom = "point", x = avg_gene1, y = avg_gene2, color = "green", size = 4, shape = 17) +
  annotate(geom = "text", x = avg_gene1+0.2, y = avg_gene2+0.2, label = "Center", color = "red") +
  geom_vline(aes(xintercept = avg_gene1)) +
  geom_hline(aes(yintercept = avg_gene2)) +
  theme_minimal()

datos[, 1:3]

datos2 = datos[, 1:3]

acp = prcomp(~Gene1 + Gene2, data = datos2)


acp_datos = as.data.frame(acp$x)

ggplot(acp_datos, aes(x = PC1, y = PC2)) +
  geom_point()

acp2 = psych::principal(datos2[, 2:3], rotate = "none")

library(gclus)


