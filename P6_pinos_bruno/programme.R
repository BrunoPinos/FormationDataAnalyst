library(dplyr)
library(FactoMineR)
library(factoextra)
library(gridExtra)
library(corrplot)
library(caret)



load("model")

example<-read.csv("example.csv", encoding = "UTF-8")
test_billet<-read.csv("test-billet.csv", encoding = "UTF-8")

df<-rbind(example, test_billet)


res.pca <- PCA(select(df, margin_low, length),graph = FALSE,
               scale.unit = TRUE)

colnames(res.pca$ind$coord)<-c("dim1","dim2")


df<-df%>%cbind(res.pca$ind$coord)

probabilities <- model %>% predict(df, type = "response")

predicted.classes <- ifelse(probabilities > 0.5, "1", "0")

probabilities

predicted.classes
