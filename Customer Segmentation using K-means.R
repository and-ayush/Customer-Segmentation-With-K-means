# loading libraries
library(mice)
library(dplyr)
library(fastDummies)

# loading the dataset
data <- read.csv(file.choose(), header = T)
str(data)
summary(data)

# handling blanks
data[data == ''] <- NA

# chamging all charactes variable to factors
k_data <- data %>% mutate_if(is.character, as.factor) %>% 
  select(2:9)
str(k_data)

# pct of missing values 
na_value <- function(x){
  sum(is.na(x))/length(x)*100
}
apply(k_data, 2 , na_value)

md.pattern(k_data, rotate.names = T)

# imputation for missing values 
impute <- mice(k_data, m = 3 , seed = 123)
summary(impute)

# Completing dataset
impute$imp$Work_Experience
stripplot(impute)
data_class <- complete(impute, 2)
summary(data_class)
summary(k_data)

# Converting non numeric data into dummies 
data_class <- dummy_cols(data_class, remove_most_frequent_dummy = T , remove_selected_columns = T)

# scale the dataset 
data_class[, 1:16] <- scale(data_class[,1:16])

# determining the number of clusters 
library(factoextra)
fviz_nbclust(data_class, kmeans, method = 'wss')
clusters <- kmeans(data_class[,1:16], centers = 7, nstart = 100)
print(clusters)
data_class$cluster <- clusters$cluster
table(data_class$cluster)
fviz_cluster(list(data = data_class, cluster = clusters$cluster))
