# install.packages(c('readr', 'magrittr'))
# install.packages('caret')

# importar pacotes
library(tibble)
library(readr)
library(magrittr)
library(dplyr)
library(caret)
library(plotly)
library(rpart)
library(rattle)

# ler os dados
setwd('C:/Users/Wesllei/Downloads/Work/Data Science/')
dados <- readr::read_csv('Churn_Modelling.csv', col_names = TRUE, col_types = NULL, locale = locale(encoding = "ISO-8859-2"))

# análise sobre os dados
View(dados)
head(dados) # 6 primeiros registros
tail(dados) # 6 últimos registros
summary(dados)

# pré-processamento
dados <- dados %>% mutate(Gender = as.factor(Gender),
                          Exited = ifelse(Exited == 1, 'SIM', 'NAO'),
                          Exited = as.factor(Exited))

summary(dados)

# quantidade de clientes retidos e perdidos
exited = dados %>% count(Exited)

plot_ly(exited, labels = c('Retidos','Perdidos'), values = ~n, type = "pie", textposition = "inside", textinfo = "percent", sort = FALSE)

# quantidade de clientes retidos e perdidos por país
geography = dados %>% count(Exited,Geography)

plot_ly(geography, x = ~Geography, y = ~n, type = "bar", color = ~Exited) %>% 
  layout(legend = list(title = list(text = 'Cliente Perdido')),
         xaxis = list(title = "País"),
         yaxis = list(title = "Número de Clientes"))

# cliente perdido por score de crédito
fig <- plot_ly(dados, x = ~Age, y = ~ CreditScore, color = ~Exited, type = "scatter", mode = "markers") %>% 
  layout(legend = list(title = list(text = 'Cliente Perdido')),
         xaxis = list(title = "Idade"),
         yaxis = list(title = "Score"))
fig <- fig %>% layout(title = 'Distribuição de Cliente por Idade, Score e Cancelamento')
fig

# cliente perdido por idade
plot_ly(dados, y = ~Age, color = ~Exited, type = "box") %>% 
  layout(showlegend = FALSE,
         xaxis = list(title = "Cliente Perdido"),
         yaxis = list(title = "Idade"))

# dividir os dados de forma balanceada
set.seed(123) # gerar sempre os mesmos índices 

indices <- createDataPartition(dados$Exited, p = .7, list = FALSE, times = 1)
treinamento <- dados[indices,]
teste <- dados[-indices,]

dadosTreinamento = treinamento %>% count(Exited)
plot_ly(dadosTreinamento, labels = c('Retidos','Perdidos'), values = ~n, type = "pie", textposition = "inside", 
        text = ~n, textinfo = "percent+text", sort = FALSE)

dadosTeste = teste %>% count(Exited)
plot_ly(dadosTeste, labels = c('Retidos','Perdidos'), values = ~n, type = "pie", textposition = "inside",  
        text = ~n, textinfo = "percent+text", sort = FALSE)

# induzir o modelo

# minsplit (quantidade mínima de exemplares em um nó para que ele tenha subárvores)
# split (ganho de informação: entropia e Gini)
modeloDT <- rpart(Exited ~ CreditScore + Gender + Age + Balance, data = treinamento, 
                  method = "class", control = rpart.control(minsplit = 20), parms = list(split = 'Information'))

# deduzir o modelo
classeEstimada <- predict(modeloDT, teste, "class")
matrizConfusao <- confusionMatrix(classeEstimada, teste$Exited, positive = 'SIM')

matrizConfusao

# inserindo mais dados
modeloDT <- rpart(Exited ~ CreditScore + Gender + Age + Balance + NumOfProducts + HasCrCard + EstimatedSalary, data = treinamento, 
                  method = "class", control = rpart.control(minsplit = 20), parms = list(split = 'Information'))

classeEstimada <- predict(modeloDT, teste, "class")
matrizConfusao <- confusionMatrix(classeEstimada, teste$Exited, positive = 'SIM')

matrizConfusao

# é só inserir mais dados? 
modeloDT <- rpart(Exited ~ CreditScore + Gender + Age + Balance + NumOfProducts + HasCrCard + EstimatedSalary + CustomerId, 
                  data = treinamento, method = "class", control = rpart.control(minsplit = 20), parms = list(split = 'Information'))

classeEstimada <- predict(modeloDT, teste, "class")
matrizConfusao <- confusionMatrix(classeEstimada, teste$Exited, positive = 'SIM')

matrizConfusao

# inserindo atributos relevantes
modeloDT <- rpart(Exited ~ CreditScore + Gender + Age + Tenure + Balance + NumOfProducts + HasCrCard + EstimatedSalary + IsActiveMember + Geography, 
                  data = treinamento, method = "class", control = rpart.control(minsplit = 20), parms = list(split = 'Information'))

classeEstimada <- predict(modeloDT, teste, "class")
matrizConfusao <- confusionMatrix(classeEstimada, teste$Exited, positive = 'SIM')

matrizConfusao

# árvore de decisão gerada
fancyRpartPlot(modeloDT, sub = "")

