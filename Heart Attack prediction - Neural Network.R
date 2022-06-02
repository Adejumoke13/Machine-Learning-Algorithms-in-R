###Predicting Heart Attack Level in Hospital Patients 
###Neural network model

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



## View the summary of the dataset
summary(heart_data)
heart_data2 <- subset(heart_data, select = -c(restecg, slp,thall,cp) )
head(heart_data2)

## target variable Plot
barplot(table(output),col = "light blue", main = "Target variable Plot")

### Dummy encode the categorical variables
heart.df <- dummy_cols(heart_data2, select_columns = c('chest.pain.type','restecg.results',
                                                       'slope', 'thalassemia'), remove_first_dummy = T, remove_selected_columns = TRUE)


head(heart.df)
attach(heart.df)
str( heart.df)

### Eliminate the spaces in the column names 

names(heart.df) <- make.names(names(heart.df), unique=TRUE)

## Create the classes for the target variable
heart.df$Yes <- heart.df$output=="1"
heart.df$No  <- heart.df$output=="0"

## Convert the True and False arguments into numerical
heart.df <- heart.df*1
head(heart.df)

# set seed for reproducing the partition
# partition data
set.seed(2)

heart.df <- heart.df[1:303, ]
# select variables for regression 
selected.var <- c(1:22)

# set seed for reproducing the partition

train.index <- sample(c(1:303), 182) 
train.df <- heart.df[train.index, selected.var]
valid.df <- heart.df[-train.index, selected.var]

names(train.df)

## model 1
### Run the neural network model
nn <- neuralnet(output ~ age + sex + trtbps + chol + fbs + thalachh + exng + oldpeak + caa + `chest.pain.type_atypical.angina` + 
                        `chest.pain.type_non.anginal` + `chest.pain.type_typical.angina` + `restecg.results_.normal` + 
                       `restecg.results_ST.T.wave.abnormality` + slope_flat + slope_upsloping + `thalassemia_normal.blood.flow` + 
                       thalassemia_nothing + `thalassemia_reversible.defect`, linear.output = F, data = train.df, hidden = 3)
neoplot <- plot(nn, col.hidden.synapse= T,information = T)
nn$result.matrix
names(nn)

### Predict the train model: confusion matrix

#Test the resulting output
### Predict the training data 

pred.nn <- predict(nn, train.df, type = "response")
### Training data confusion matrix
con.mat <- table(ifelse(pred.nn > 0.5, 1, 0), train.df[,10])    
con.mat
colnames(con.mat) <- c(0, 1)
cfm <- as_tibble(con.mat,.name_repair = ~ c("Yes", "No", "N"))
cfm
plot_confusion_matrix(cfm, 
                      target_col = "Yes", 
                      prediction_col = "No",
                      counts_col = "N", add_sums = TRUE, add_normalized = FALSE)

sum(diag(con.mat))/sum(con.mat)



### Predict the Validation model : confusion matrix
#### Model results : ROC CURVE
pred.nn1<- predict(nn, valid.df, type = "response")
con.mat1 <- table(ifelse(pred.nn1 >0.18,1 , 0), valid.df[,10])    
con.mat1
colnames(con.mat1) <- c(0, 1)
cfm1 <- as_tibble(con.mat1,.name_repair = ~ c("Yes", "No", "N"))
cfm1
plot_confusion_matrix(cfm1, 
                      target_col = "Yes", 
                      prediction_col = "No",
                      counts_col = "N",  add_sums = TRUE, add_normalized = FALSE)
sum(diag(con.mat1))/sum(con.mat1)
round(sort(pred.reg2, decreasing = T), 3)


plotROC(valid.df[,10], pred.nn1)
confusionMatrix(valid.df[,10], pred.nn1, threshold = 0.18)

misClassError(valid.df[,10], pred.nn1, threshold = 0.18)


optimalCutoff(valid.df[,10], pred.nn1, optimiseFor = "misclasserror",
              returnDiagnostics = TRUE)



##MODEL 2
### Using the Best model in Project 3 and dropping some insignificant models 5 layers
nn2 <- neuralnet(output ~ sex + trtbps + chol + caa + `chest.pain.type_atypical.angina` + 
                  `chest.pain.type_non.anginal` + `chest.pain.type_typical.angina`+
                   slope_flat + slope_upsloping, data = train.df, hidden = 5)
neoplot2 <- plot(nn2, col.hidden.synapse = T,information = T)
nn2$net.result
names(nn2)
#Test the resulting output
### Predict the training data 

pred.nn2 <- predict(nn2, train.df, type = "response")
stat.desc(pred.nn2)
### Training data confusion matrix
con.mat2 <- table(ifelse(pred.nn2 > 0.55,1 , 0), train.df[,10])    
con.mat2
colnames(con.mat2) <- c(0, 1)
cfm2 <- as_tibble(con.mat2,.name_repair = ~ c("Yes", "No", "N"))
cfm2
plot_confusion_matrix(cfm2, 
                      target_col = "Yes", 
                      prediction_col = "No",
                      counts_col = "N",  add_sums = TRUE, add_normalized = FALSE)
sum(diag(con.mat2))/sum(con.mat2)


### Predict the Validation model : confusion matrix
#Test the resulting output

pred.nn3<- predict(nn2, valid.df, type = "response")

#### Model results : ROC CURVE
pred.nn3<- predict(nn2, valid.df, type = "response")
con.mat3 <- table(ifelse(pred.nn3 > 0.38,1 , 0), valid.df[,10])    
con.mat3
colnames(con.mat3) <- c(0, 1)
cfm3 <- as_tibble(con.mat3,.name_repair = ~ c("Yes", "No", "N"))
cfm3
plot_confusion_matrix(cfm3, 
                      target_col = "Yes", 
                      prediction_col = "No",
                      counts_col = "N",  add_sums = TRUE, add_normalized = FALSE)
sum(diag(con.mat3))/sum(con.mat3)
round(sort(pred.reg2, decreasing = T), 3)

#### Model2 results : ROC curve
plotROC(valid.df[,10], pred.nn3)
confusionMatrix(valid.df[,10], pred.nn3, threshold = 0.38)
misClassError(valid.df[,10], pred.nn3, threshold = 0.38)
optimalCutoff(valid.df[,10], pred.nn3, optimiseFor = "misclasserror",
              returnDiagnostics = TRUE)

###Model 3
## normalizing some variables : training data  

train.df[c(3, 4, 6)] <- scale(train.df[c(3, 4, 6)])
train.df2 <- as.data.frame(train.df)
glimpse(train.df2)
nn3 <- neuralnet(output ~ sex + trtbps + chol + caa + `chest.pain.type_atypical.angina` + 
                   `chest.pain.type_non.anginal` + `chest.pain.type_typical.angina`+
                   slope_flat + slope_upsloping, data = train.df2, hidden = 5)
neoplot2 <- plot(nn3, col.hidden.synapse = T,information = T)
nn3$result.matrix
### Predict the training data 

pred.nn4 <- predict(nn3, train.df2, type = "response")
### Training data confusion matrix
con.mat4 <- table(ifelse(pred.nn4 > 0.55,1 , 0), train.df2[,10])    
con.mat4
stat.desc(pred.nn4)
colnames(con.mat4) <- c(0, 1)
cfm4 <- as_tibble(con.mat4,.name_repair = ~ c("Yes", "No", "N"))
cfm4
plot_confusion_matrix(cfm4, 
                      target_col = "Yes", 
                      prediction_col = "No",
                      counts_col = "N",  add_sums = TRUE, add_normalized = FALSE)
sum(diag(con.mat4))/sum(con.mat4)


#Test the resulting output on the Third nn model: Validation Model
###Normalize the variables
valid.df[c(3, 4, 6)] <- scale(valid.df[c(3, 4, 6)])
valid.df2 <- as.data.frame(valid.df)

### Predict the Validation model : confusion matrix
pred.nn5<- predict(nn3, valid.df2, type = "response")
con.mat5 <- table(ifelse(pred.nn5 > 0.39,1 , 0), valid.df2[,10])    
con.mat5
colnames(con.mat5) <- c(0, 1)
cfm5 <- as_tibble(con.mat5,.name_repair = ~ c("Yes", "No", "N"))
cfm5
plot_confusion_matrix(cfm5, 
                      target_col = "Yes", 
                      prediction_col = "No",
                      counts_col = "N",  add_sums = TRUE, add_normalized = FALSE)
sum(diag(con.mat5))/sum(con.mat5)
round(sort(pred.nn5, decreasing = T), 3)

#### Model2 results : ROC cURVE
plotROC(valid.df2[,10], pred.nn5)
confusionMatrix(valid.df2[,10], pred.nn5, threshold = 0.385)
misClassError(valid.df2[,10], pred.nn5, threshold = 0.385)
optimalCutoff(valid.df2[,10], pred.nn5, optimiseFor = "misclasserror",
              returnDiagnostics = TRUE)


### Other Charts
###Gains chart
library(e1071)
library(Rcpp)
library(gains)
class(gain)
names(gain)

logit.reg.pred <- predict(heart.reg, valid.df2[,-14], type = "response")
data.frame(actual = valid.df$output[1:5], predicted = logit.reg.pred[1:5])
gain <- gains(valid.df2$output, pred.nn5, groups = 10)

data.frame(c(0,gain$cume.pct.of.total*sum(valid.df2$output)) ,
           c(0,gain$cume.obs) )

data.frame( c(0,sum(valid.df2$output)) , c(0, dim(valid.df2)[1]) )


plot(c(0,gain$cume.pct.of.total*sum(valid.df2$output))~c(0,gain$cume.obs),col="red", 
     xlab="#patients", ylab="Cumulative", main="Gains/Lift Chart", type="l")
lines(c(0,sum(valid.df2$output))~c(0, dim(valid.df2)[1]), lty=2, col="orange")


###Decile-Wise Charts

heights <- gain$mean.resp/mean(valid.df2$output)

gain$mean.resp
gain$mean.resp*200
mean(valid.d2f$output)

midpoints <- barplot(heights, names.arg = gain$depth, ylim = c(0,9), col = "light blue",
                     xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise chart")
text(midpoints, heights+0.5, labels=round(heights, 1), cex = 0.8, col = "red")

install.packages("dplR")
