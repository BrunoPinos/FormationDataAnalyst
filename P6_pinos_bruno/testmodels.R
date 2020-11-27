df<-read.csv("test-billet.csv", encoding = "UTF-8")

load("models")

df.transformed <- preproc.param %>% predict(df)

predictions <- model_lda %>% predict(df.transformed)

a<-predictions$class




probabilities <- model %>%
  predict(df.transformed, type = "response")

predicted.classes <- ifelse(probabilities > 0.5, "True", "False")

b<-predicted.classes

a
b