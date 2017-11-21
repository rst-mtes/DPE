if("dplyr" %in% (.packages())){
    detach("package:dplyr", unload=TRUE) 
    detach("package:plyr", unload=TRUE) 
} 
library(plyr)
library(dplyr)

#training <- read.csv("dpe-montreuil.csv", na.strings = c("NA", "NULL",""))
training <- read.csv2("extractBDDDPE_Montreuil_Nov2017.csv", na.strings = c("NA", "NULL",""))

#On ne garde que qq paramètres
training <- rename(training, conso = consommation_energie, class_conso = classe_consommation_energie,
                   type_bat_id=tr002_type_batiment_id, type_energie_id=tr004_type_energie_id,
                   surface = surface_habitable, type_usage_id = tr006_type_usage_id,
                   construct_year = annee_construction)

keep <- c(1,12,13,14,15,16,20,21,53,54)
names(training)[keep]
training <- select(training, keep)


#On élimine les lignes non complètes
anyNA(training)
training <- na.omit(training)
anyNA(training)

#Filtrage : on supprime les lignes avec conso = 0, les dates < 1800 et les classification en N
query <- (training$conso != 0)
training2 <- training[query,]
query <- (training2$class_conso != "N")
training2 <- training2[query,]
query <- (training2$construct_year > 1800)
training2 <- training2[query,]

training2$class_conso <- factor(training2$class_conso)
training2$type_bat_id <- factor(training2$type_bat_id)
training2$type_energie_id <- factor(training2$type_energie_id)
training2$type_usage_id <- factor(training2$type_usage_id)


set.seed(1805)

library(caret)


inTrain <- createDataPartition(training2$class_conso, p = 0.6, list = FALSE)
train_df <- training2[inTrain, ]
test_df <- training2[-inTrain, ]

summary(train_df)




### Simple arbre de décision
modFit_rpart <- train(class_conso ~ type_bat_id + construct_year + surface + type_usage_id + type_energie_id, method="rpart",data =train_df)
print(modFit_rpart$finalModel)
confusionMatrix(test_df$class_conso,predict(modFit_rpart, newdata=test_df))$overall
confusionMatrix(test_df$class_conso,predict(modFit_rpart, newdata=test_df))


plot(modFit_rpart$finalModel, uniform = TRUE,
     main="Classification Tree")
text(modFit_rpart$finalModel, use.n=TRUE, all=TRUE, cex=0.5)


### Algo Gradient Boosting
modFit_gbm <- train(class_conso ~ type_bat_id + construct_year + surface + type_usage_id + type_energie_id, method="gbm", data =train_df, verbose = T)
print(modFit_gbm$finalModel)
confusionMatrix(test_df$class_conso,predict(modFit_gbm, newdata=test_df))$overall


### Algo random forest 
modFit_rf <- train(class_conso ~ type_bat_id + construct_year + surface + type_usage_id + type_energie_id, method="rf",data =train_df, prox = TRUE, verbose = T)
print(modFit_rf$finalModel)
confusionMatrix(test_df$class_conso,predict(modFit_rf, newdata=test_df))$overall
confusionMatrix(test_df$class_conso,predict(modFit_rf, newdata=test_df))


