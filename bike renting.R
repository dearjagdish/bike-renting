setwd("D:/edwisor/Projects/bike rental")
data <- read.csv("day.csv", header = T)

#Loading required libraries
library(corrgram)
library(MLmetrics)
library(Metrics)
library(rsq)
library(caret)
library(rpart)
library(usdm)
library(randomForest)
library(xgboost)


#checking for missing values
missing_val = data.frame(apply(data,2,function(x){sum(is.na(x))}))
missing_val

# ## BoxPlots - Distribution and Outlier Check
numeric_index = sapply(data,is.numeric) #selecting only numeric
numeric_data = data[,numeric_index]
cnames = colnames(numeric_data)

for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "cnt"), data = subset(data))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i],x="cnt")+
           ggtitle(paste("Box plot of cnt for",cnames[i])))
}

# ## Plotting plots together
gridExtra::grid.arrange(gn1, gn2, gn3, gn4, nrow=2, ncol=2)
gridExtra::grid.arrange(gn5, gn6, gn7, gn8, nrow=2, ncol=2)
gridExtra::grid.arrange(gn9, gn10, gn11, gn12, nrow=2, ncol=2)
gridExtra::grid.arrange(gn13, gn14, gn15, nrow =2, ncol=2)


#correlation plot
corrgram(data[,numeric_index], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")


#check multicolinearity
vif(numeric_data[, -15])
vifcor(numeric_data[, -15], th = 0.9)

## Dimension Reduction
data = subset(data, select = -c(dteday, instant, atemp, casual, registered))

#Normality check
hist(data$hum)
hist(data$windspeed)
hist(data$temp)

#Divide the data into train and test
#set.seed(123)
train_index = sample(1:nrow(data), 0.8 * nrow(data))
train = data[train_index,]
test = data[-train_index,]

###model building
model_name = vector()
rsq_value = vector()
rmse_value = vector()
mae_value = vector()

register_model_score = function(name, rsq, rmse, mae){
  model_name <<- append(model_name, name)
  rsq_value <<- append(rsq_value, rsq)
  rmse_value <<- append(rmse_value, rmse)
  mae_value <<- append(mae_value, mae)

}

print_score <- function(postResample_score, model_name, register_model = 1){
  rsq = postResample_score[[2]]
  mae = postResample_score[[3]]
  rmse_val = postResample_score[[1]]
  
  cat(c("R-sq=", rsq, "\n"))
  cat(c("RMSE =", rmse_val, "\n"))
  cat(c("MAE =", mae, "\n"))
 
  if(register_model ==1){
    register_model_score(model_name, rsq, rmse_val, mae)
  }
}

#calculate MAPE
mape_value = vector()
Mape = function(y, yhat){
  mean(abs((y - yhat)/y))
  mape_value <<- append(mape_value, mape)
  cat(c("MAPE =", mape, "\n"))
}
  

# ##rpart for regression
name1 = "Decision Tree"
fit = rpart(cnt ~ ., data = train, method = "anova")
predictions_DT = predict(fit, test[,-11])
score1 = postResample(predictions_DT, test$cnt)
print_score(score1, name1)
mape_DT <- mape(test$cnt, predictions_DT)
mape_value <<- append(mape_value, mape_DT)

#linear regression
name2 = "linear regression"
LR_model = lm(cnt ~., data = train)
summary(LR_model)
predictions_LR = predict(LR_model, test[, 1:10])
score2 = postResample(predictions_LR, test$cnt)
print_score(score2, name2)
mape_LR <- mape(test$cnt, predictions_LR)
mape_value <<- append(mape_value, mape_LR)

#KNN regressor
#name3 = "KNN Regression"
#y = train$cnt
#Knn_pred = FNN::knn.reg(train, test, y, k=1, algorithm=c("kd_tree", "cover_tree", "brute"))
#score3 = postResample(Knn_pred, test$cnt)
#print_score(score3, name3)
#mape_knn <- mape(test$cnt, knn_pred)

#random forest
name4 = "Random Forest"
RF = randomForest(cnt ~ ., data = train, mtry = 4, importance = TRUE, ntress = 500)
predictions_RF = predict(RF, test[,-11])
score4 = postResample(predictions_RF, test$cnt)
print_score(score4, name4)
mape_RF <- mape(test$cnt, predictions_RF)
mape_value <<- append(mape_value, mape_RF)

#xgboost
name5 = "xgboost regressor"
bike_model_xgb <- xgboost(data = as.matrix(train), # training data as matrix
                          label = train$cnt,  # column of outcomes
                          nrounds = 84,       # number of trees to build
                          objective = "reg:linear", # objective
                          eta = 0.3,
                          depth = 6,
                          verbose = 0  # silent
)

# Make predictions
xgb_pred <- predict(bike_model_xgb, as.matrix(test))
score5 = postResample(xgb_pred, test$cnt)
print_score(score5, name5)
mape_xgb <- mape(test$cnt, xgb_pred)
mape_value <<- append(mape_value, mape_xgb)


###Model summary
models_df = data.frame(model_name, rsq_value, rmse_value, mae_value, mape_value)
models_df

##Higher the value of r square and lower the value of MAE, RMSE and MAPE better the model.  Hence decided to fix XGB regressor for this dataset.

#output
test_data = test[, 1:10]
df = data.frame(xgb_pred)
output_R = cbind(test_data, df)
write.csv(output_R, "output_R.csv", row.names = F)