#install.packages("tidyverse")
library(tidyverse) 

#install.packages("mlbench")
# zoo <- read.csv("C:/Users/HP/Desktop/zoo.data", header=FALSE)
data(Zoo, package="mlbench")#_____________________________________________________________________________________________________________________________________
view(Zoo) #l'ouvrir dans Tab

names(Zoo)  # dans consol
Zoo$type #les donnes de attribut type
head(Zoo) # Voir les 6 premiers lignes

as_tibble(Zoo) # convert Zoo to tibble

 Zoo <- Zoo %>%
     modify_if(is.logical, factor, levels = c(TRUE, FALSE)) %>%
     modify_if(is.character, factor)

#_____________________________________________________________________________________________________________________________________
#Classification
#_____________________________________________________________________________________________________________________________________


library(rpart) # The rpart package in R is used for fitting and visualizing decision tree models. 

as_tibble(Zoo, rownames = "animal")

tree_default <- Zoo %>% rpart(type ~ ., data = .)
tree_default

#install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(tree_default, extra = 2) # function to visualize the decision tree stored in the tree_default object // this code should generate a visual representation of the decision tree // The extra = 2 argument is specified to show additional information in the plot, such as the number of observations in each leaf node.

tree_full <- Zoo %>% rpart(type ~ ., data = ., control =  rpart.control(minsplit = 2, cp=0))
rpart.plot(tree_full, extra = 2, roundint=FALSE, box.palette = list("Gy", "Gn", "Bu", "Bn", "Or", "Rd", "Pu"))

pred <- predict(tree_default, Zoo, type="class")
pred
confusion_table <- with(Zoo, table(type, pred)) #statistics

#Somme de diagonal
correct <- confusion_table |> diag() |> sum()
correct
error <- confusion_table |> sum() - correct
error
accuracy <- correct / (correct + error)
accuracy

#on peut utiliser un function
accuracy <- function(truth, prediction) {
 tbl <- table(truth, prediction)
 sum(diag(tbl))/sum(tbl)
}
accuracy(Zoo |> pull(type), pred)

#Get a confusion table with more statistics (using caret)
library(caret)
confusionMatrix(data = pred, 
 reference = Zoo |> pull(type))

#Test
inTrain <- createDataPartition(y = Zoo$type, p = .8, list = FALSE)
Zoo_train <- Zoo |> slice(inTrain)
Zoo_test <- Zoo |> slice(-inTrain) # on supprime le reste

#Trimming
fit <- Zoo_train |>
 train(type ~ .,
 data = _ ,
 method = "rpart",
 control = rpart.control(minsplit = 2),
 trControl = trainControl(method = "cv", number = 10),
 tuneLength = 5)
fit

#Autre dendogramme ( grand cp meilleur accuracy)
rpart.plot(fit$finalModel, extra = 2,
 box.palette = list("Gy", "Gn", "Bu", "Bn", "Or", "Rd", "Pu"))

#Testing: Confusion Matrix and Confidence Interval for Accuracy
pred <- predict(fit, newdata = Zoo_test)
pred
confusionMatrix(data = pred, ref = Zoo_test |> pull(type))

#_____________________________________________________________________________________________________________________________________
#Clustering
#_____________________________________________________________________________________________________________________________________
#2-Utilisez kmeans avec k=7 afin de répartir l’ensemble des individus du dataset en 7 clusters
#_______________________________________________
#pkgs <- sort(c('ggpubr', 'factoextra'))
#pkgs_install <- pkgs[!(pkgs %in% installed.packages()[,"Package"])]
#if(length(pkgs_install)) install.packages(pkgs_install)

#install.packages(factoextra)
#install.packages(ggpubr)
library(ggpubr)
library(factoextra)

#Resultat de k-means
df <- Zoo
names(df)
#[1] "hair"     "feathers" "eggs"     "milk"     "airborne" "aquatic"  "predator" "toothed" 
#[9] "backbone" "breathes" "venomous" "fins"     "legs"     "tail"     "domestic" "catsize" 
#[17] "type"    
names(df[, -17])
#[1] "hair"     "feathers" "eggs"     "milk"     "airborne" "aquatic"  "predator" "toothed" 
#[9] "backbone" "breathes" "venomous" "fins"     "legs"     "tail"     "domestic" "catsize" 
df <- Zoo[, -17]
df <- as.data.frame(lapply(df, function(x) {
     if (is.factor(x)) {
         x <- as.character(x)
         x <- ifelse(x == "TRUE", 1, ifelse(x == "FALSE", 0, x))
         x <- as.numeric(x)
     }
     return(x)
 })) #il faut convertir TRUE/FALSE to 1/0 car kmeans n'accepte que les types numerique, il faut ne pas toucher legs, et voilà il faut affecter Zoo sans type
 df #df <- as.data.frame(lapply(df, function(x) ifelse(as.character(x) == "TRUE", 1, 0)))
 res.km <- kmeans(scale(df), 7, nstart = 25) # res.km <- kmeans(scale(df[, -17]), 7, nstart = 25) RESULTAT MINUTE 53
 res.km$cluster
 #[1] 2 2 6 2 2 2 2 6 6 2 2 5 6 4 4 4 5 2 6 1 5 5 2 5 3 7 7 2 2 2 3 2 2 5 6 2 2 5 6 3 3 5 3 5 2 2 4 2
 #[49] 2 2 2 3 7 4 2 2 5 5 5 5 6 6 7 2 2 2 1 2 2 2 2 5 3 6 1 1 7 4 5 5 7 4 6 5 2 4 6 5 3 7 5 7 6 2 2 5
 #[97] 2 3 2 4 5
res.km$totss
#[1] 1600

fviz_cluster(res.km, data = df,
                           palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#800000", "#aaB800", "#29a200", "#999990"), 
                           geom = "point",
                           ellipse.type = "convex", 
                           ggtheme = theme_bw()
              ) #ou df[, -17],

#install.packages("cluster")
library(cluster)

silhouette_score <- function(k){
  km <- kmeans(df, centers = k, nstart=25)
  ss <- silhouette(km$cluster, dist(df))
  mean(ss[, 3])
}
k <- 2:10
avg_sil <- sapply(k, silhouette_score)
plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE)
#_____________________________________________
#1-Dans cette partie appliquer une Classification Ascendante Hiérarchique puis 
#coupez l’arbre obtenu pour obtenir 7 clusters 
#______________________________________________
 d <- dist(df)
 hc <- hclust(d, method = "complete")
 plot(hc)
 fviz_dend(hc, k = 7)

 #install.packages("fpc")
library(fpc)

fpc::cluster.stats(d, res.km$cluster)






