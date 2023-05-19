## read dataset------
source('R/00helper.R')
load("E:/2022/IRP/R_project/data/glmnet_m0517111.rds")

## read dataset and data preparation--

dat <- read_excel("E:/2022/IRP/R_project/data/model_dat0519_en.xlsx")


set.seed(20230517)
trainIndex <- createDataPartition(dat$outcome, p = 0.8, list = FALSE)

trainingSet <- dat[trainIndex,]
testSet <- dat[-trainIndex,]


dat$outcome %>% table()
trainingSet$outcome %>% table()
testSet$outcome %>% table()

zero_one <- c('nonIRP','IRP')

trainingSet$outcome <- zero_one[trainingSet$outcome]
testSet$outcome <- zero_one[testSet$outcome]

trainingSet <- trainingSet %>%
  mutate(outcome = as.factor(outcome)) %>%
  arrange(outcome) %>%
  select(outcome,everything())

testSet <- testSet %>%
  mutate(outcome = as.factor(outcome))%>%
  arrange(outcome) %>%
  select(outcome,everything())

# colnames(trainingSet) <- c('outcome',paste0('var',c(1:(ncol(trainingSet)-1 ) )))
# colnames(testSet) <- c('outcome',paste0('var',c(1:(ncol(trainingSet)-1 ) )))

##modelling----

### set up-----
rfFuncs$summary <- BigSummary

objectives = 'auc_ap_sbr'  #'auc_ap_sbr',"SBrier"

twoClassCtrl <- trainControl(
  method = "repeatedcv",
  number = 3,
  repeats = 10,
  savePredictions = "final",
  classProbs = T,
  summaryFunction = BigSummary
  )
### repeated cv training---
set.seed(20230517)
glmnet_m <- train(outcome ~., data = trainingSet,
                           method = "glmnet",   #glmnet
                           metric = objectives,
                           trControl = twoClassCtrl,
                            tuneLength = 10
)


get_best_result(glmnet_m)
glmnet_m$bestTune


tune_values <- table(glmnet_m$results$alpha,glmnet_m$results$lambda) %>% data.frame()

## coef for non-IRP
coef_glmnet <- coef(glmnet_m$finalModel, glmnet_m$finalModel$lambdaOpt) %>%
  matrix() %>% data.frame()
outcome_n <- dat %>% colnames() %>% length()
rownames(coef_glmnet) <- c("intercept",colnames(dat)[-outcome_n])
coef_glmnet <-  coef_glmnet %>% filter(.!=0) %>%
  mutate(.=.*-1)
# equivalent to coef_glmnet <- coef(glmnet_m$finalModel, glmnet_m0517111$finalModel$lambdaOpt)
# test <- glmnet(x = trainingSet[,-1] ,y=trainingSet[,1],
#                family = 'binomial',
#                alpha =  1, # glmnet_m$bestTune
#                lambda =0.0260704)

# coef_table <- coef(test) %>% matrix() %>% data.frame()
# rownames(coef_table) <- c("intercept",colnames(model_dat_82X_1)[-outcome_n])
# coef_table <- coef_table %>% filter(.!=0)

### evaluate model performance------
set.seed(20230517)
roc_f(model_m = glmnet_m,testdat= testSet)

glm_perf <- sum_perf_f(glmnet_m, testdat=testSet,model_name='ElasticNet') %>% data.frame()

par(mfrow=c(1,2))
cali_plot_f(model_m=glmnet_m,model_name='',testdat=testSet)

ggsave("cali0518.pdf")
dev.off()

ks_glm <- kernelshap(glmnet_m, trainingSet[,-1],
                     bg_X = trainingSet,
                     type="prob")


ks_glm1 <- ks_glm$S[[1]]

ks_glm2 <- ks_glm$S[[2]]


ks_glm1_abs <- apply(ks_glm11,2,abs) %>% data.frame()


ks_feature_importance <- apply(ks_glm1_abs,2,mean) %>% data.frame()

ks_feature_importance$. %>% sum()


ks_feature_importance1 <- ks_feature_importance %>%
  mutate(fi = ./sum(.),
         variable_name = rownames(ks_feature_importance))

sv_importance(shapviz(ks_glm1,trainingSet[,-1]), "bar",
              fill = "#8f8f8f", show_other = FALSE,
              max_display = nrow(coef_glmnet)-1)

sv_importance(shapviz(ks_glm1,trainingSet[,-1]), "bee",
              color_bar_title = 'Feature Value',
              viridis_args = list(begin = 0.13, end = 1, option = "turbo"),
              max_display = nrow(coef_glmnet)-1)

sv_importance(shapviz(ks_glm1,trainingSet[,-1]), "bee")


sv_importance(shapviz(ks_glm2,trainingSet[,-1]), "bee")


glm_cat_val <- pred_cat_f(model_m = glmnet_m,model_name='ElasticNet',testdat=testSet,data_type = 'Validation')%>% data.frame()

glm_cat_test <- pred_cat_f(model_m = glmnet_m,model_name='ElasticNet',
                           testdat=testSet,data_type = 'Test')%>% data.frame()

cat_sum <- rbind(glm_cat_test,
                 glm_cat_val
)

DT::datatable(cat_sum,
              filter = "top", editable = "cell", extensions = "Buttons",
              options = list(
                dom = "Blfrtip",
                scrollX = TRUE,
                buttons = c("copy", "csv", "excel", "pdf", "print"),
                lengthMenu = list(
                  c(5, 25, 50, 100, -1),
                  c(5, 25, 50, 100, "All")
                )
              )
)




















