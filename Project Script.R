options(scipen = 999)
setwd("C:/Users/jacka/Desktop/College Final Year/Semester 2/STAT 30270/Project")
data = read.csv('data_project_deepsolar.csv', header = T)
head(data)
library(dplyr)
summary(data)
names(data)[names(data) == "ï..solar_system_count"] <- 'solar_system_count'

## Visualisation
library(ggthemes)
library(ggplot2)

my_post_theme=theme_minimal() +
  theme(axis.text.x = element_text(face="bold", color="#666666",size=10),
        axis.title = element_text(color="black", face="bold",size=12),
        plot.title = element_text(color="black", face="bold", size=14),
        axis.text.y = element_text(face="bold", color="#666666",size=10),
        legend.text=element_text(size=12),
        legend.title=element_text(size=13),
        legend.key=element_blank(),
        axis.ticks.length=unit(0, "cm"))

# plot states 
plot.new()
plot <- ggplot(data[,1:2], aes(x=state, fill=solar_system_count, order=solar_system_count)) + 
  geom_bar() + my_post_theme + labs(fill = "Type") + xlab("Count") +
  theme(axis.title.x = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5))
plot

par(mfrow=c(2,2))
plot.new()

# Wealth and Occupation
cdplot(data$solar_system_count ~ data$average_household_income, 
       xlim=c(min(data$average_household_income),250000), ylab='', 
       xlab="Average Household Income", col = c("#F8766D", "#619CFF"))
cdplot(data$solar_system_count ~ data$housing_unit_median_value, 
       xlim=c(min(data$housing_unit_median_value),1800000), ylab='', 
       xlab="Median Housing Unit Value", col = c("#F8766D", "#619CFF"))
cdplot(data$solar_system_count ~ data$occupation_information_rate, 
       xlim=c(min(data$occupation_information_rate),.2), ylab='', 
       xlab="Proportion Employed in Information Sector", col = c("#F8766D", "#619CFF"))
cdplot(data$solar_system_count ~ data$occupation_construction_rate, 
       xlim=c(min(data$occupation_construction_rate),.28), ylab='', 
       xlab="Proportion Employed in Construction Sector", col = c("#F8766D", "#619CFF"))

# Politics
par(mfrow=c(1,2))
cdplot(data$solar_system_count ~ data$voting_2016_dem_percentage, 
       ylab='', xlab="Proportion voted Democratic 2016", col = c("#F8766D", "#619CFF"))
cdplot(data$solar_system_count ~ data$voting_2012_dem_percentage, 
       ylab='',xlab="Proportion voted Democratic 2012", col = c("#F8766D", "#619CFF"))

## Preprocessing
# Missing values
head(data)
sum(is.na(data)) # this is zero

Y <- data[,1]
X <- data[,3:84]

# Convert political win by label encoding
X$voting_2016_dem_win <-  as.numeric(X$voting_2016_dem_win)
X$voting_2012_dem_win <-  as.numeric(X$voting_2012_dem_win)

# One hot encode the state variable
library(mltools)
library(data.table)

onehot_state <- one_hot(as.data.table(data[,2]))
onehot_state

# Combine all variables 
X <- cbind.data.frame(onehot_state, X)

# Fitting a principal component analysis
library(caret)
PCA <- preProcess(X, method = c("scale", "pca"), thresh = .99)
X_pca <- data.frame(predict(PCA, X))

# Train-Test-Val with K Fold cross validation
library(caTools)
clean_data <- cbind.data.frame(Y, X_pca)
clean_data$Y = as.factor(clean_data$Y)
split_data_ind <- sample.split(clean_data$PC2, SplitRatio = 0.8)
train_df <- subset(clean_data, split_data_ind == TRUE)
holdout_df <- subset(clean_data, split_data_ind == FALSE)

## Train and Predict Validation Set to Choose Initial Model
# Model Training
library(rpart)
library(adabag)
library(randomForest)
library(e1071)
library(adabag)

#N = nrow(train_df)
#K <- 10       

#folds <- rep(1:K, ceiling(N/K)) 
#folds <- sample(folds) # random permute 
#folds <- folds[1:N] # ensure we got N data points

#out <- matrix(NA, K, 1)
#prog_bar <- txtProgressBar(min = 0, max = K, style = 3)

#for ( k in 1:K ) {
  #train_ind <- which(folds != k)
  #test_ind <- setdiff(1:N, train_ind)
  
  # fit the classifiers on the training data
  #basic_tree <- rpart(Y ~ ., data = train_df, subset = train_ind)
  #log_reg <- glm(Y ~ ., data = train_df, subset = train_ind, family = "binomial")
  #bag_tree <- bagging(Y ~ ., data = train_df, subset = train_ind) 
  #rand_for <- randomForest(Y ~ ., data = train_df, subset = train_ind, importance = TRUE)
  #svm_linear = svm(formula = Y ~ ., data = train_df, subset = train_ind, type = 'C-classification', kernel = 'linear')
  #svm_rbf = svm(formula = Y ~ ., data = train_df, subset = train_ind, type = 'C-classification', kernel = 'radial')
  #fitboost <- boosting(Y ~ ., data = train_df, subset = train_ind, coeflearn = "Freund", boos = FALSE)
  
  # predict on the validation set
  # boosting tree
  #pred1 <- predict(fitboost, type = "class", newdata = train_df[test_ind,]) 
  #tab1 <- table(train_df$Y[test_ind], pred1$class) 
  #out[k,1] <- sum(diag(tab1))/sum(tab1) 
  
  # logistic regression
  #pred2 <- predict(log_reg, type = "response", newdata = train_df[test_ind,])  
  #pred2 <- ifelse(pred2 > 0.5, 1, 0) 
  #tab2 <- table(train_df$Y[test_ind], pred2) 
  #out[k,2] <- sum(diag(tab2))/sum(tab2)
  
  # bag tree
  #pred3 <- predict(bag_tree, type = "class", newdata = train_df[test_ind,]) 
  #tab3 <- table(train_df$Y[test_ind], pred3$class) 
  #out[k,3] <- sum(diag(tab3))/sum(tab3)
  
  # random forest
  #pred4 <- predict(bag_tree, type = "class", newdata = train_df[test_ind,]) 
  #tab4 <- table(train_df$Y[test_ind], pred4$class) 
  #out[k,4] <- sum(diag(tab4))/sum(tab4)
    
  # svm linear
  #pred5 <- predict(svm_linear, type = "class", newdata = train_df[test_ind,]) 
  #tab5 <- table(train_df$Y[test_ind], pred5) 
  #out[k,5] <- sum(diag(tab5))/sum(tab5)
    
  # svm rbf
  #pred6 <- predict(svm_rbf, type = "class", newdata = train_df[test_ind,]) 
  #tab6 <- table(train_df$Y[test_ind], pred6) 
  #out[k,6] <- sum(diag(tab6))/sum(tab6)
  
  #setTxtProgressBar(prog_bar, k)
#}

# Get table of mean accuracy
#library(xtable)
#out # our accuracy at each fold for each classifier

#avg <- data.frame(t(round(colMeans(out), 4))) 
#avg
#print.xtable(xtable(avg), file = "./class_acc.txt")

## Find optimal Models
# Kernelised Logistic Regression
#design_matrix = data.matrix(clean_data[2:50])
#y = clean_data$Y
#library(calibrateBinary)
#cv.out <- cv.KLR(design_matrix, y, K=8)
#print(cv.out)

#holdout_mat <- data.matrix(holdout_df[2:50])
#etahat.cv <- KLR(design_matrix, y, holdout_mat, kernel = "matern", 
#                lambda=cv.out$lambda,rho=cv.out$rho)



# Optimise the SVM using training data 
N = nrow(train_df)
K <- 10       
folds <- rep(1:K, ceiling(N/K)) 
folds <- sample(folds) # random permute 
folds <- folds[1:N] # ensure we got N data points

Cost = 2^(-1:4) # default is 1
Gamma = 2^(-8:-3) # default is 1/(data dimension)
options(max.print=1000000)
acc <- array(dim=c(length(Cost), length(Gamma), K), 
             dimnames = list(Cost,Gamma, c(1:10)))

for (k in 1:K) {
  
  train_ind <- which(folds != k)
  test_ind <- setdiff(1:N, train_ind)
  
  for (c in 1:length(Cost)){
    
    for (g in 1:length(Gamma)){
      
      svm = svm(formula = Y ~ ., data = train_df, subset = train_ind, cost = Cost[c], 
            gamma = Gamma[g], type = 'C-classification', kernel = 'radial')
      
      pred <- predict(svm, type = "class", newdata = train_df[test_ind,])
      tab <- table(train_df$Y[test_ind], pred)
    
      acc[c,g,k] = sum(diag(tab))/sum(tab) # store accuracy
    }
  }
  cat("Just finished fold number: ",k)
}

fold_avg_svm = apply(acc, c(1,2), mean)

which(fold_avg_svm == max(fold_avg_svm), arr.ind = TRUE) # Optimal is: Cost = 2, Gamma = 0.015625

# Optimise AdaBoost using training data and see Lab 9 examples
boost_split <- sample.split(train_df$PC2, SplitRatio = 0.7)
boost_train <- subset(train_df, boost_split == TRUE)
boost_test <- subset(train_df, boost_split == FALSE)

fitboost_Fre <- boosting(Y ~ ., data = boost_train, coeflearn = "Freund", mfinal=1000, boos = FALSE)
Error_Fre <- errorevol(fitboost_Fre, boost_test)$error

fitboost_Bre <- boosting(Y ~ ., data = boost_train, coeflearn = "Breiman", mfinal=1000, boos = FALSE)
Error_Bre <- errorevol(fitboost_Bre, boost_test)$error

fitboost_Zhu <- boosting(Y ~ ., data = boost_train, coeflearn = "Zhu", mfinal=1000, boos = FALSE)
Error_Zhu <- errorevol(fitboost_Zhu, boost_test)$error

# Plot error paths, only first 200 iterations 
mat <- cbind(Error_Fre[1:200], Error_Bre[1:200], Error_Zhu[1:200]) 
cols <- c("deepskyblue4", "darkorange3", "darkred") 

matplot(mat, type = "l", col = cols, 
        lwd = 2, xlab = "Number of trees", 
        ylab = "Classification Error") 
legend(x = 140, y = 0.17, cex = 0.75, legend = c("Freund", "Breiman", "Zhu"), 
       col = cols, lwd = 2, bty = "n", title="Algorithm") 
legend(x = 175, y = 0.17, cex = 0.75, col = cols, lwd = 2, bty = "n", title="Minimum", 
       legend = c(round(min(Error_Fre), 4), round(min(Error_Bre), 4), round(min(Error_Zhu), 4))) 
points(apply(mat, 2, which.min), apply(mat, 2, min), 
       col = cols, pch = c(15,16,17), cex = 1.2)
# Best is Breiman with 37 trees

################################################################################
## Compare both optimised models ##
N = nrow(train_df)
K <- 5     
R <- 100 

acc_vals <- vector("list", R)
winner <- matrix(NA, R, K) 

for ( r in 1:R ) {
  acc <- matrix(NA, K, 2) 
  folds <- rep( 1:K, ceiling(N/K) ) 
  folds <- sample(folds)
  folds <- folds[1:N] 
  
  for ( k in 1:K ) { 
    train_ind <- which(folds != k)
    test_ind <- setdiff(1:N, train_ind)
    
    # fitting 
    svm_rbf = svm(formula = Y ~ ., data = train_df, subset = train_ind,
                  type = 'C-classification', kernel = 'radial', cost = 2, gamma = 0.015625) 
    
    fitboost <- boosting(Y ~ ., data = train_df, subset = train_ind, 
                         coeflearn = "Breiman", mfinal = 37, boos = FALSE)
    
    # predict 
    pred <- predict(svm_rbf, type = "class", newdata = train_df[test_ind,])
    tab <- table(train_df$Y[test_ind], pred) 
    acc[k,1] <- sum(diag(tab))/sum(tab) 

    pred1 <- predict(fitboost, type = "class", newdata = train_df[test_ind,]) 
    tab1 <- table(train_df$Y[test_ind], pred1$class) 
    acc[k,2] <- sum(diag(tab1))/sum(tab1)
    
    # select the best classifier 
    winner[r,k] <- ifelse( acc[k,1] > acc[k,2], "SVM(RBF)", "Adaboost")
  }
  acc_vals[[r]] <- acc  
  print(r) 
}

acc_vals
avg <- t(sapply(acc_vals, colMeans))

# find the mean accuracy across all iterations
meanAcc <- colMeans(avg) 
meanAcc

# find stand.dev of these means
# applies sd function to the columns of avg
sdAcc <- apply(avg, 2, sd)/sqrt(R) 
sdAcc

# beautiful plot of results
matplot(avg, type = "l", lty = c(2,3), 
        col = c("darkorange2", "deepskyblue3"), 
        xlab = "Replications", ylab = "Accuracy") 

# add confidence intervals 
bounds1 <- rep( c(meanAcc[1] - 2*sdAcc[1], meanAcc[1] + 2*sdAcc[1]), each = R ) 
bounds2 <- rep( c(meanAcc[2] - 2*sdAcc[2], meanAcc[2] + 2*sdAcc[2]), each = R ) 
polygon(c(1:R, R:1), bounds1, 
        col = adjustcolor("darkorange2", 0.2), border = FALSE) 
polygon(c(1:R, R:1), bounds2, 
        col = adjustcolor("deepskyblue3", 0.2), border = FALSE)

# add estimated mean line 
abline(h = meanAcc, col = c("darkorange2", "deepskyblue3")) 

# add legend 
legend("topright", fill = c("darkorange2", "deepskyblue3"), 
       legend = c("SVM", "AdaBoost"), bty = "n")
################################################################################
# RESULTS AND DISCUSSION SECTION #
# NOW USE BEST MODEL TO PREDICT HOLDOUT SET AND GIVE CONFUSION MATRIX # 
library(xtable)

fitboost <- boosting(Y ~ ., data = train_df, coeflearn = "Breiman", 
                     mfinal = 37, boos = FALSE)

pred <- predict(fitboost, type = "class", newdata = holdout_df) 
pred1 <- predict(fitboost, type = "class", newdata = train_df)
tab <- table(holdout_df$Y, pred)
test_acc = sum(diag(tab))/sum(tab)
tab1 <- table(holdout_df$Y, pred1)
train_acc = sum(diag(tab1))/sum(tab1)

acc_hold <- cbind.data.frame("Accuracy", train_acc, test_acc)
colnames(acc_hold) <- c("Metric","Training","Testing")
acc_hold
print.xtable(xtable(acc_hold), file = "./hold_acc.txt")



