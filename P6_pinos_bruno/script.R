library(dplyr)
library(FactoMineR)
library(factoextra)
library(gridExtra)
library(corrplot)
library(caret)

load("model")

df<-read.csv("test-billet.csv", encoding = "UTF-8")

df.transformed <- preproc.param %>% predict(df)

predictions <- model %>% predict(df.transformed)

df$id

c<-predictions$class%>%as.data.frame()%>%cbind(df$id,.)%>%
  select("id" = "df$id", "is_genuine" =".")

DT::datatable(c)
``
