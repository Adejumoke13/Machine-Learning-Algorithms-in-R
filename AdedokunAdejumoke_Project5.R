###Predicting Heart Attack Level in Hospital Patients 
###Classification Tree analysis

### Read the dataset into R Environment.

heart_data <- read.csv("C:\\Users\\Jeddah\\Downloads\\heartattack1.csv")
head(heart_data)
str(heart_data)
attach(heart_data)
library(wooldridge)
library(stargazer)
library(caret)
library(cvms)
library(tibble) 
library(fastDummies)
library(ggplot2)
library(InformationValue)
library(neuralnet)
library(pastecs)
library(rpart)
library(rpart.plot)
library(randomForest)
library(xgboost)
library(Matrix)
library(adabag)

library(gains)

## View the summary of the dataset
summary(heart_data)
heart_data2 <- subset(heart_data, select = -c(restecg, slp,thall,cp) )
head(heart_data2)

## target variable Plot
barplot(table(output),col = "light blue", main = "Target variable Plot")



# use first 303 rows of data
heart_data2 <- heart_data2[1:303, ]
# select variables for regression 
selected.var <- c(1:14)

## target variable Plot

barplot(table(output),col = "light blue", main = "Target variable Plot")

# partition data
set.seed(2) 

train.index <- sample(c(1:303), 182) 
train.df <- heart_data2[train.index, selected.var]
valid.df <- heart_data2[-train.index, selected.var]



##Model 1: default and all variables
#Fit a classification tree model Model 1
heart_tree <- rpart(output ~ .,  data = train.df,method = "class")

tree_plot <- prp(heart_tree, main = "Heart Attack Classification Tree Model 1",
    type = 2,extra = 1,  
    box.palette = "BuOr", 
    faclen = 0,tweak = 1.6, split.font = 1,uniform = T )
heart_tree$variable.importance
###Predict training data 
tree_train <- predict(heart_tree,train.df, type = "class")
con.mat <- table(tree_train, train.df$output)
con.mat
cfm <- as_tibble(con.mat,.name_repair = ~ c("A", "B", "N"))
cfm
plot_confusion_matrix(cfm, 
                      target_col = "A", 
                      prediction_col = "B",
                      counts_col = "N", add_sums = TRUE,add_normalized = F
                      ,font_row_percentages = font(size = 3.2),
                      font_col_percentages = font(size = 3.2))
ACCURACY<- sum(diag(con.mat))/sum(con.mat)
ACCURACY


###Predict Validation data 
tree_valid <- predict(heart_tree,valid.df,type = "prob")
con.mat1 <- table(tree_valid, valid.df$output)
con.mat1
cfm1 <- as_tibble(con.mat1,.name_repair = ~ c("A", "B", "N"))
cfm1
plot_confusion_matrix(cfm1, palette =  "Blues",
                      target_col = "A", 
                      prediction_col = "B",
                      counts_col = "N", add_sums = TRUE,add_normalized = F
                      ,font_row_percentages = font(size = 3.2),
                      font_col_percentages = font(size = 3.2))
ACCURACY1<- sum(diag(con.mat1))/sum(con.mat1)

library(pROC)
auc(valid.df$output, tree_valid[,1])



#### Model1 results : ROC curve
as.data.frame(train.df)
library(pROC)
library("ROCR")
gh <- as.data.frame(Pred.cart)
Pred.cart <-predict(heart_tree, newdata = valid.df, type = "prob")
Pred2 <- prediction(gh, valid.df["output"]) 
plot(performance(Pred2, "tpr", "fpr"))



## Model 2
### significant variables from logistic regression ran earlier 
heart_tree1 <- rpart(output ~ sex + trtbps + exng + caa + `chest.pain.type`+thalachh + oldpeak+
                       slope + thalassemia , data = train.df, method = "class")

tree_plot1 <- prp(heart_tree1, main = "Heart Attack Classification Tree Model 2",
                 type = 2,extra = 1,  
                 box.palette = "BuGn", 
                 faclen = 0,tweak = 1.6, split.font = 1,uniform = T )
summary(heart_tree1)
heart_tree1$variable.importance



###Predict training data 1
tree_train1 <- predict(heart_tree1,train.df)
con.mat2 <- table(tree_train1, train.df$output)
con.mat2
cfm2 <- as_tibble(con.mat2,.name_repair = ~ c("A", "B", "N"))
cfm2
plot_confusion_matrix(cfm2, 
                      target_col = "A", 
                      prediction_col = "B",
                      counts_col = "N", add_sums = TRUE,add_normalized = F
                      ,font_row_percentages = font(size = 3.2),
                      font_col_percentages = font(size = 3.2))
ACCURACY2<- sum(diag(con.mat2))/sum(con.mat2)
ACCURACY2

###Predict Validation data 1
tree_valid2 <- predict(heart_tree1,  valid.df, type="prob")
con.mat3 <- table(tree_valid2, valid.df$output)
con.mat3
cfm3 <- as_tibble(con.mat3,.name_repair = ~ c("A", "B", "N"))
cfm3
plot_confusion_matrix(cfm3, palette =  "Blues",
                      target_col = "A", 
                      prediction_col = "B",
                      counts_col = "N", add_sums = TRUE,add_normalized = F
                      ,font_row_percentages = font(size = 3.2),
                      font_col_percentages = font(size = 3.2))
ACCURACY3<- sum(diag(con.mat3))/sum(con.mat3)


##AUROC score for validation data 
library(pROC)
auc(valid.df$output, tree_valid2[,1])

###Model 3 
##Best Pruned Tree with all X's
prune1 <- rpart(output ~ ., data = train.df, method = "class", 
                cp = 0.00001, minsplit = 5, xval = 5)
pruned.ct2 <- prune(prune1, cp = 0.0154639)
pruned.ct <- prune(prune1, 
                   cp = prune1$cptable[which.min(prune1$cptable[,"xerror"]),"CP"])
printcp(prune1)
prp(pruned.ct, main = "Heart Attack best pruned Tree",
    type = 1,extra = 1,  
    box.palette = "BuOr", 
    faclen = 0,tweak = 1.4, split.font = 1, varlen = -10)
prune1$variable.importance
###Predict training data 2
tree_train3 <- predict(pruned.ct,train.df,type = "class")
con.mat4 <- table(tree_train3, train.df$output)
con.mat4
cfm4 <- as_tibble(con.mat4,.name_repair = ~ c("A", "B", "N"))
cfm4
plot_confusion_matrix(cfm4, 
                      target_col = "A", 
                      prediction_col = "B",
                      counts_col = "N", add_sums = TRUE,add_normalized = F
                      ,font_row_percentages = font(size = 3.2),
                      font_col_percentages = font(size = 3.2))
ACCURACY4<- sum(diag(con.mat4))/sum(con.mat4)

###Predict Validation data 2
tree_valid3 <- predict(pruned.ct2,valid.df,type = "prob")
con.mat5 <- table(tree_valid3, valid.df$output)
con.mat5
cfm5 <- as_tibble(con.mat5,.name_repair = ~ c("A", "B", "N"))
cfm5
plot_confusion_matrix(cfm5, palette =  "Blues",
                      target_col = "A", 
                      prediction_col = "B",
                      counts_col = "N", add_sums = TRUE,add_normalized = F
                      ,font_row_percentages = font(size = 3.2),
                      font_col_percentages = font(size = 3.2))
ACCURACY5<- sum(diag(con.mat5))/sum(con.mat5)
ACCURACY5
##AUROC score for validation data 

library(pROC)
auc(valid.df$output, tree_valid3[,1])



### Full Depth Tree Model 
deeper_tree <- rpart(output ~ ., data = train.df, method = "class", cp = 0,
                     minsplit = 1)
prp(deeper_tree, main = "Heart Attack best pruned Tree",
    type = 1,extra = 1,  
    box.palette = "BuOr", 
    faclen = 0,tweak = 1.7, split.font = 1, varlen = -10)

###Predict training data 2
tree_traint <- predict(deeper_tree,train.df,type = "prob")
conf.mat<- table(tree_traint[,2], train.df$output)
conf.mat


###Predict Validation data 2
tree_validv <- predict(deeper_tree,valid.df,type = "class")
conf.mat <- table(tree_validv, valid.df$output)
conf.mat

### Random Forest Model
heart_rand <- randomForest(as.factor(output) ~ ., data = train.df, ntree = 500, 
                   mtry = 4, nodesize = 5, importance = TRUE)  
prp(heart_rand, type = 1,extra = 1,  
    box.palette = "BuOr", 
    faclen = 0, tweak = 1.7, split.font = 1, varlen = -10)
getTree(heart_rand, 1, labelVar=TRUE)
plot(heart_rand)
## variable importance plot
varImpPlot(heart_rand, type = 1, color ="blue", main = "Variable Importance Plot" )
importance(heart_rand,scale=TRUE)


## confusion matrix: Training 
tree_train4 <- predict(heart_rand,train.df,type = "class")
con.mat6 <- table(tree_train4, train.df$output)
con.mat6
cfm6 <- as_tibble(con.mat6,.name_repair = ~ c("A", "B", "N"))
cfm6
plot_confusion_matrix(cfm6, rm_zero_text = F,
                      target_col = "A", 
                      prediction_col = "B",
                      counts_col = "N", add_sums = TRUE,add_normalized = F
                      ,font_row_percentages = font(size = 3.2),
                      font_col_percentages = font(size = 3.2))
ACCURACY6<- sum(diag(con.mat6))/sum(con.mat6)

###Predict Validation data 
tree_valid4 <- predict(heart_rand,valid.df,type = "prob")
con.mat7 <- table(tree_valid4, valid.df$output)
con.mat7
cfm7 <- as_tibble(con.mat7,.name_repair = ~ c("A", "B", "N"))
cfm7
plot_confusion_matrix(cfm7, palette =  "Blues",
                      target_col = "A", 
                      prediction_col = "B",
                      counts_col = "N", add_sums = TRUE,add_normalized = F
                      ,font_row_percentages = font(size = 3.2),
                      font_col_percentages = font(size = 3.2))
ACCURACY7<- sum(diag(con.mat7))/sum(con.mat7)
ACCURACY7

###AUCROC SCORE
library(pROC)
auc(valid.df$output, tree_valid4[,1])


###Gains chart
library(e1071)
library(Rcpp)
library(gains)


data.frame(actual = valid.df$output[1:5], predicted = logit.reg.pred[1:5])
gain <- gains(valid.df$output, as.numeric(tree_valid4[,2]), groups = 10)
class(gain)
names(gain)

data.frame(c(0,gain$cume.pct.of.total*sum(valid.df$output)) ,
           c(0,gain$cume.obs) )

data.frame( c(0,sum(valid.df$output)) , c(0, dim(valid.df)[1]) )


plot(c(0,gain$cume.pct.of.total*sum(valid.df$output))~c(0,gain$cume.obs),col="red", 
     xlab="#patients", ylab="Cumulative", main="Gains/Lift Chart", type="l")
lines(c(0,sum(valid.df$output))~c(0, dim(valid.df)[1]), lty=2, col="orange")



###Decile-Wise Charts

heights <- gain$mean.resp/mean(valid.df$output)

gain$mean.resp
gain$mean.resp*200
mean(valid.df$output)
heights

midpoints <- barplot(heights, names.arg = gain$depth, ylim = c(0,9), col = "light green",
                     xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise chart")
text(midpoints, heights+0.5, labels=round(heights, 1), cex = 0.6, col = "red")


# bagging and boosting
set.seed(1)
train.df$output <- as.factor(train.df$output)
boost <-  boosting.cv(output ~ age + sex + trtbps + chol + fbs + thalachh + exng + oldpeak + caa + `chest.pain.type_atypical.angina` + 
                     `chest.pain.type_non.anginal` + `chest.pain.type_typical.angina` + `restecg.results_.normal` + 
                     `restecg.results_ST.T.wave.abnormality` + slope_flat + slope_upsloping + `thalassemia_normal.blood.flow` + 
                     thalassemia_nothing + `thalassemia_reversible.defect`, data = train.df2)

boost$confusion
tree_train5 <- predict(boost,train.df2,type = "class")
con.mat.2 <- table(tree_train5, train.df$output)
con.mat.2
bag <- bagging(output ~ sex + trtbps + chol + fbs + thalachh + exng + oldpeak + caa + `chest.pain.type_atypical.angina` + 
                 `chest.pain.type_non.anginal` + `chest.pain.type_typical.angina` + `restecg.results_.normal` + 
                 `restecg.results_ST.T.wave.abnormality` + slope_flat + slope_upsloping + `thalassemia_normal.blood.flow` + 
                 thalassemia_nothing + `thalassemia_reversible.defect`, data = train.df2)

tree_train5 <- predict(bag,train.df2,type = "class")



 