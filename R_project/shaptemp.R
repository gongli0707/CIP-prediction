varImp(glmnet_m,scale = TRUE)[["importance"]]
v$Overall <- v$Overall / sum(v$Overall)
v

predictor.glm <- Predictor$new(
  model = glmnet_m, 
  data = testSet1 
)

pred <- function(model, newdata)  {
  results <- as.data.frame(predict(model, (newdata)))
  return(results[[3L]])
}


response <- as.numeric(c(1,0)[testSet1$outcome]) 
predictor.glm <- Predictor$new(
  model = glmnet_m, 
  data = testSet1, 
  y = response, 
  predict.fun = pred,
  class = "classification"
)


(high <- predict(glmnet_m, testSet1,type = 'prob') %>% .[, 1] %>% as.vector() %>% which.max()) 
## [1] 154
(low <- predict(glmnet_m, testSet1,type = 'prob') %>% .[, 1] %>% as.vector() %>% which.min())   
## [1] 28

# get these observations
high_prob_ob <- testSet1[high, ]
low_prob_ob  <- testSet1[low, ]


lime.glm <- LocalModel$new(predictor.glm, k = 5,
                           x.interest = high_prob_ob) %>% 
  plot() + ggtitle("GLM")



#
# A projection pursuit regression (PPR) example
#

# Load the sample data; see ?datasets::mtcars for details
data(mtcars)

# Fit a projection pursuit regression model
fit <- lm(mpg ~ ., data = mtcars)

# Compute approximate Shapley values using 10 Monte Carlo simulations
set.seed(101)  # for reproducibility

temp_x = subset(trainingSet1, select = -outcome)

x_num <- apply(temp_x, 2,as.numeric )


shap <- explain(glmnet_m, X = subset(trainingSet1, select = -outcome), nsim = 10, 
                pred_wrapper = predict)
shap

# Compute exact Shapley (i.e., LinearSHAP) values
shap <- explain(glmnet_m, exact = TRUE)
shap

# Shapley-based plots
library(ggplot2)
autoplot(shap)  # Shapley-based importance plot
autoplot(shap, type = "dependence", feature = "wt", X = mtcars)
autoplot(shap, type = "contribution", row_num = 1)  # explain first row of X



#
# A projection pursuit regression (PPR) example
#

# Load the sample data; see ?datasets::mtcars for details
data(mtcars)

# Fit a projection pursuit regression model
mtcars.ppr <- ppr(mpg ~ ., data = mtcars, nterms = 1)

# Compute approximate Shapley values using 10 Monte Carlo simulations
set.seed(101)  # for reproducibility
shap <- explain(mtcars.ppr, X = subset(mtcars, select = -mpg), nsim = 10, 
                pred_wrapper = predict)
shap

# Shapley-based plots
library(ggplot2)
autoplot(shap)  # Shapley-based importance plot
autoplot(shap, type = "dependence", feature = "wt", X = mtcars)
autoplot(shap, type = "contribution", row_num = 1)  # explain first row of X
