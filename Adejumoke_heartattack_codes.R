### Read the dataset into R Environment.

heart_data <- read.csv("C:\\Users\\Jeddah\\Downloads\\heartattack_dataset.csv")
head(heart_data)
str(heart_data)
attach(heart_data)

### Descriptive Statistics 
## Measures of Central tendency
summary(heart_data)

### There are 13 features. Even though 12 of them are integer type, only 6 numerical, the rest are categorical

### Age Distribution of the patients by sex of the patients
library(ggplot2)
library(tidyverse)
attach(my_data)
## Add the name of the gender to the dataset
my_data <- heart_data %>%
  mutate(gender = case_when(
    sex == 0 ~ "Female",
    TRUE  ~ "Male"
    ))
table(gender)
ggplot(my_data, aes(x=age, color=gender)) +
  geom_histogram(fill = "white", size = 1.3)+
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
ggtitle("Age distribution by Gender") 


##Pie chart of Gender Distribution
par(mfrow= c(1,1))
mytable <- table(sex)
lbs <- paste0(c("female","male"), " ", round(100 * mytable/sum(mytable), 2), "%")
pie(count_2, labels = pie_labels)
pie(mytable, labels = lbs, col = c("orange", "grey"),
    main="Gender show")


##Correlation matrix to determine the relationship between the variables.
install.packages("reshape")
library(reshape) # to generate input for the plot 
cor.mat <- round(cor(heart_data),2) # rounded correlation matrix 
melted.cor.mat <- melt(cor.mat) 
ggplot(melted.cor.mat, aes(x = X1, y = X2, fill = value)) +  
  geom_tile() +  
  geom_text(aes(x = X1, y = X2, label = value)) +
ggtitle("Correlation Matrix of Heart Attack Dataset") 


### Chest Pain type by gender
my_data2 <- my_data %>%
  mutate(Chest_pain = case_when(
    cp == 0 ~ "asymptomatic", cp == 1 ~ "typical angina", 
    cp == 2 ~ "atypical angina",
    TRUE  ~ " non-anginal pain"
  ))

ggplot(my_data2, aes(x=factor(Chest_pain),fill=gender))+
  geom_bar(stat="count", width=0.7)+
  theme_minimal()+
  scale_fill_brewer(palette="Dark2")
ggtitle("Chest Pain Distribution by Gender") 


## Scatterplot cholesterol level and blood pressure 
ggplot(my_data2, aes(x=chol, y=trtbps)) + 
  geom_point(color="blue3")+
  geom_smooth(method=lm)+
  ggtitle("Scatterplot of Cholesterol levels and blood pressure levels")

## Histogram Distribution of the datasets
library(ggplot2)
library(gridExtra)
library(grid)
plot1 <- ggplot(my_data2, aes(x=trtbps, color = factor(output))) + geom_histogram(color="purple", fill="grey")
plot2 <- ggplot(my_data2, aes(x=chol, color = factor(output))) + geom_histogram(color="darkblue", fill="grey")
plot3<- ggplot(my_data2, aes(x=thalachh, color = factor(output))) + geom_histogram(color="red", fill="grey")
plot4<- ggplot(my_data2, aes(x=oldpeak, color = factor(output))) + geom_histogram(color="black", fill="grey")
grid.arrange(plot1, plot2, plot3, plot4, ncol=2, top = textGrob("Distribution of Blood Pressure, Cholesterol, Heart rate and ST Distribution"))

## Output distribution by Gender
ggplot(my_data2, aes(x=factor(output),fill=gender))+
  geom_bar(stat="count", width=0.7)+
  theme_minimal()+
  scale_fill_brewer(palette= "Paired" )+
ggtitle("Output by Gender") 

## Boxplots distribution 
par(mfcol = c(2, 2)) 
boxplot(oldpeak ~ output, xlab = "Output", ylab = "oldpeak", col= "azure" ) 
boxplot(thall ~ output, xlab = "Output", ylab = "thall", col= "orange" ) 
boxplot(thalachh ~ output, xlab = "Output", ylab = 
          "thalachh", col= "lightblue" ) 
boxplot(age ~ output, xlab = "Output", ylab = "Age", col= "lightblue" )
mtext("Boxplot of the Output and Other Variables", side = 3, line = - 2, outer = TRUE)

## Scatterplot matrix
par(mfcol = c(1, 1)) 
my_cols <- c("#00AFBB", "#E7B800", "#FC4E07")
pairs(heart_data[, c(1,3:5,8, 10)], pch = 19,  cex =1,
      col = my_cols[restecg])

## Bar plots 
par(mfcol = c(2, 2)) 
barplot(table(cp), names.arg = c("Aymptomatic", "typical angina",
                          "atypical angina", "non-anginal pain"), main = "Chest pain type", col = "azure2" )
barplot(table(fbs), names.arg = c("blood sugar < 120", "blood sugar > 120"),
        main = "Blood Sugar Levels", col = "azure3" )
barplot(table(caa), main = "Number of blood Vessels", col = "azure1" )
barplot(table(slp), names.arg = c("Downsloping-0", "Flat-1", "Upsloping-2"),
        main = " The slope of the peak exercise ST segment ", col = "azure")
