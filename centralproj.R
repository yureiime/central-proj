set.seed(9876) #Setting seed
cen <- read.csv("Central2024P.csv", stringsAsFactors = TRUE)
train <- sample(1:nrow(cen), 2000) #train set of 0.8
test <- (-train)

#Method 1: Multiple Linear and Polynomial Regression
#Best Model according to Adjusted R^2
L3 <- lm(Price~Area*Tenure+Region+Age+Purchaser, cen[train,])
summary(L3)
pred3 <- predict(L3, newdata=cen[test,])
mean((pred3-cen[test, "Price"])^2)
coef(L3)

#Examples of other lm models
summary(lm(Price~.+I(Area^2)+I(Area^3), cen[train,]))
summary(lm(Price~Tenure*Purchaser+Region+Area+Age+I(Area^2), cen[train,]))
summary(lm(Price~Tenure*Purchaser+Region+Area+Age, cen[train,]))

#Method 2: Best Subset Selection
library(leaps)
regfit2 <- regsubsets(Price~., data=central, nvmax=18)
regfit2.summary <- summary(regfit2)
plot(regfit2.summary$adjr2, main="Adjusted r^2 plot", xlab="Number of variables", ylab="Adjusted r^2", type="b")
plot(regfit2.summary$cp, main="Cp plot", xlab="Number of variables", ylab="cp", type="b")
plot(regfit2.summary$bic, main="BIC plot", xlab="Number of variables", ylab="BIC", type="b")
b <- which.max(regfit2.summary$adjr2)
c <- which.min(regfit2.summary$cp)
d <- which.min(regfit2.summary$bic)

# Model based on adjusted R square criteria
rsq <- coef(regfit2, b)
rsq

# Model based on Cp criteria
cp <- coef(regfit2, c)
cp

# Model based on BIC criteria
bic <- coef(regfit2, d)
bic

# 10-fold cross validation on best subset
predict.regsubsets <- function(object, newdata, id){
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id=id)
  xvars <- names(coefi)
  mat[, xvars]%*%coefi
}

k <- 10  
set.seed(9876)
folds <- sample(1:k, nrow(central), replace=TRUE)
cv.errors <- matrix(NA, k, 18, dimnames=list(NULL, paste(1:18)))
for (j in 1:k) {
  best.fit <- regsubsets(Price~., data=central[folds!=j,], nvmax=18)
  for (i in 1:18){
    pred <- predict.regsubsets(best.fit, central[folds==j,], id=i)
    cv.errors[j,i] <- mean((central$Price[folds==j]-pred)^2)
  }
}

#Test error
mean.cv <- apply(cv.errors, 2, mean)
min(mean.cv)

#Model with lowest cross validation error
bb <- which.min(mean.cv)
bssmod <- coef(regfit2, bb)
bssmod

#Method 3: Ridge Regression
library(glmnet)
x <- model.matrix(Price~., central)[, -1]
y <- central$Price
centraltrain <- central[train,]
centraltest <- central[test,]
trainx <- model.matrix(Price~., centraltrain)[, -1]
trainy <- centraltrain$Price
testx <- model.matrix(Price~., centraltest)[, -1]
testy <- centraltest$Price
ridgemod <- glmnet(trainx, trainy, alpha = 0)
cvout <- cv.glmnet(trainx, trainy, alpha = 0)
lambdarr <- cvout$lambda.min
lambdarr

#Calculating test error
ridgepred <- predict(ridgemod, s = lambdarr, newx = x[test,])
mean((ridgepred-testy)^2)

#Ridge regression model
outrr <- glmnet(x, y, alpha = 0)
rrmodel <- predict(outrr, type = "coefficients", s = lambdarr)[1:19,]
rrmodel[rrmodel!=0]

#Method 4: The LASSO
lassomod <- glmnet(trainx, trainy, alpha = 1)
cvout1 <- cv.glmnet(trainx, trainy, alpha = 1)
lambdalasso <- cvout1$lambda.min
lambdalasso

#Test error
lassopred <- predict(lassomod, s = lambdalasso, newx = x[test,])
mean((lassopred-testy)^2)

#The lasso model
outlr <- glmnet(x, y, alpha = 1)
lrmodel <- predict(outlr, type = "coefficients", s = lambdalasso)[1:19,]
lrmodel[lrmodel!=0]

#Method 5: Elastic Net Regression
#set alpha = 0.5 for Elastic Net (0 for Ridge, 1 for Lasso)
elasticnet <- cv.glmnet(x.train, y.train, alpha = 0.25) 

#Cross validation results
plot(elasticnet)
bestlambda <- elasticnet$lambda.min
final_model <- glmnet(x.train, y.train, alpha = 0.5, lambda = bestlambda)

#Use model for testing
y.pred <- predict(final_model, newx = testx)
MSE <- mean((test_data$Price - y.pred)^2)
MSE
bestlambda
model_coefficients <- coef(final_model)
model_coefficients

#Method 6: K-Nearest Neighbors
x <- read.csv("Central2024P.csv", stringsAsFactors = TRUE)
library(class)

x$Tenure <- as.numeric(x$Tenure)
x$Purchaser <- as.numeric(x$Purchaser)
x$Region <- as.numeric(x$Region)
#x[,2:6] <- scale(x[,2:6])
set.seed(9876)
num_folds <- 5

# Vector to store the mean squared errors for each fold
mse_cv <- numeric(num_folds)

# Perform k-fold cross-validation
for (i in 1:num_folds) {
  # Define the indices for the current fold
  fold_indices <- sample(1:nrow(x), size = nrow(x) / num_folds)
  
  # Split the data into training and validation sets
  train_fold <- x[-fold_indices, ]
  validation_fold <- x[fold_indices, ]
  
  # KNN model
  knn_model <- knn(train = train_fold[, -which(names(train_fold) == "Price")],
                   test = validation_fold[, -which(names(validation_fold) == "Price")],
                   cl = train_fold$Price,
                   k = 4)  # You can adjust the value of k as needed
  
  predictions <- as.numeric(knn_model)
  
  mse_cv[i] <- mean((predictions - validation_fold$Price)^2)
}

# Calculate the average Mean Squared Error across all folds
average_mse_cv <- mean(mse_cv)

print(paste("Average Mean Squared Error (Cross-Validation):", average_mse_cv))

#Method 7: Decision Tree
library(tree) 

#Initial Tree construction
tree.central <- tree(Price~., data = x, subset = train)
summary(tree.central)
tree.central
plot(tree.central)
title("Regression Tree for Central2024P data")
text(tree.central, pretty = 0, cex = 0.6, srt = 5)

#Finding minimum nodes through cv
cv.central <- cv.tree(tree.central)
cv.central
plot(cv.central$size, cv.central$dev, type="b", main="Cross validation: Deviance versus Size",
     xlab="Number of terminal nodes", ylab="deviance")
minimum_nodes <- cv.central$size[which.min(cv.central$dev)]  
minimum_nodes

#Pruning given minimum nodes
prune.central <- prune.tree(tree.central, best=minimum_nodes)
plot(prune.central)
title ("Pruned Regression Tree for central data")
text(prune.central, pretty=0, cex = 0.6, srt = 5)

predict <- predict(prune.central, newdata = x[test,])
central.test <- x[test, 'Price']

plot(predict, central.test, main="Pruned Tree prediction versus observed prices for test data",
     xlab="predict Price", ylab="Observed Price")
mean((predict-central.test)^2)

#Method 8: Random Forest
library(randomForest)
predictor_variables <- names(x)[-which(names(x) == "Price")]
target_variable <- "Price"

# Train the Random Forest model using only the training data
rf_model <- randomForest(
  formula = as.formula(paste(target_variable, "~ .")),
  data = x[train,],  # Use only the training data
  ntree = 550,  # Number of trees in the forest
  mtry = 4,  # Number of variables randomly sampled as candidates at each split
  importance = TRUE  # Calculate variable importance
)

# Print the model summary
print(rf_model)

# Predict on the test data
predictions <- predict(rf_model, newdata = x[test,])

# Calculate Mean Squared Error
mse <- mean((predictions - x[test,]$Price)^2)
print(paste("Mean Squared Error:", mse))

# Get variable importance measures
importance <- importance(rf_model)
print(importance)

# Plot variable importance
varImpPlot(rf_model)