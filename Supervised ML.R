#GRIP - The Sparks Foundation
#Data Science and Business Analytics Internship
#Task 1 - Prediction Student score based on study hours
#By:- Sunishtha Yadav

# Reading data from remote link
data <- read.csv(url("http://bit.ly/w-data"))
View(data)
print("Data imported successfully")
#Plotting the distribution of scores
plot(data$Hours, data$Scores, main= "Scores vs Hours",
     xlab = "Hours", ylab = "Scores", pch=19)
#Preparing data
set.seed(2)
id <- sample(2,nrow(data),prob=c(0.7,0.3),replace = TRUE)
print(id)
train <- data[id==1,]
test <- data[id==2,]
View(train)
View(test)
#Training data
mdl <- lm(data = train, formula = Scores~Hours)
summary(mdl)
print("Training complete.")

# Plotting the regression line
plot(data$Hours,data$Scores, col = "blue", main = "Scores vs Hours", abline(mdl),
     cex=1.2, pch=16, ylab = "Scores", xlab = "Hours")

# Testing Model: Comparing Actual vs Predicted
y <- predict(mdl, test)
print(data.frame(y, test$Scores))

#predicting the value at 9.25
pred <- predict(mdl, data.frame(Hours=c(9.25)))
print(pred)
install.packages("ie2misc")
# Mean Absolute Error
library("ie2misc")
print(mae(test$Scores, y))