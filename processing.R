#############################################################################################################################################
# PKGS (needs to be installed before)
#############################################################################################################################################

# pkgs
require(dplyr)
require(data.table)
require(reshape2)
require(recommenderlab)
require(methods)

#############################################################################################################################################
# DATA PROCESSING
#############################################################################################################################################

# load data
dataset = read.csv("product_views.csv", header = T, sep = ";")

dataset = dataset[!duplicated(dataset),]
dataset$product_id[dataset$product_id == ""] = NA
dataset = dataset[complete.cases(dataset), ]

# select users with min 2 productviews
test = dataset %>% group_by(user.id) %>% summarise(count = n())
test = filter(test, count >= 2)
dataset = subset(dataset, dataset$user.id %in% unique(test$user.id))
rm(test)

# select products with min 2 views
test <- dataset %>% group_by(product_id) %>% summarise(count = n())
test <- filter(test, count >= 2)
test <- unique(test$product_id)
dataset <- subset(dataset, dataset$product_id %in% test)

# create binary matrix for modeling
dataset = as.data.table(dataset)
affinity.data = dcast.data.table(dataset, user.id~product_id, length)
affinity.data = as.data.frame(affinity.data)
rownames(affinity.data) = affinity.data$user.id
affinity.data$user.id = NULL
affinity.matrix = as(as.matrix(affinity.data), "binaryRatingMatrix")

#############################################################################################################################################
# PRODUCT BASED RECOMMENDATIONS
#############################################################################################################################################

# run IBCF model
mod_ibcf = Recommender(data = affinity.matrix, method = "IBCF", parameter = list(method = "Jaccard"))

# get similarity matrix of products
result_ibcf = as.matrix(mod_ibcf@model$sim)

# get five product recommendations per product
result_ibcf = reshape2::melt(result_ibcf)
result_ibcf = result_ibcf[with(result_ibcf, order(Var1, -value)), ]
result_ibcf = result_ibcf %>% group_by(Var1) %>% slice(c(1:5))
result_ibcf$Var2 = as.factor(result_ibcf$Var2)

# replace items with no correlation with top products
test = as.data.frame(sort(table(dataset$product_id), decreasing = T)[1:6])
result_ibcf$Var2[result_ibcf$value == 0] = NA
result_ibcf$Var2[is.na(result_ibcf$Var2)] = sample(test$Var1, size=sum(is.na(result_ibcf$Var2)), replace = T)

# write recommendations into list and remove brackets
result_ibcf = result_ibcf %>% group_by(Var1) %>% summarise(recommendations = list(as.character(Var2)))
result_ibcf$recommendations = gsub("c\\(|)|\"|([\n])","", result_ibcf$recommendations)

# rename columns
colnames(result_ibcf) = c("product_id","recommendations")
