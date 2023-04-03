
model_dat1_nodummy1111_en <- read_excel("E:/IRP/R_project/data/model_dat1_nodummy1111_en.xlsx")

model_dat_1220 <- read_excel("E:/IRP/R_project/data/model_dat_1220.xlsx")

model_dat <- model_dat1_nodummy1111_en

model_dat_1220[,4] = underlying_disease_original$ICIs

model_dat <- model_dat_1220



miss <- miss_var_summary(model_dat) %>%
  mutate(pct_miss = round(pct_miss, 2)) %>%
  filter(pct_miss > 0)

# DT::datatable(miss,
#               filter = "top", editable = "cell", extensions = "Buttons",
#               options = list(
#                 dom = "Blfrtip",
#                 scrollX = TRUE,
#                 buttons = c("copy", "csv", "excel", "pdf", "print"),
#                 lengthMenu = list(
#                   c(5, 25, 50, 100, -1),
#                   c(5, 25, 50, 100, "All")
#                 )
#               )
# )

model_dat_82 <- model_dat %>% 
  mutate(`Number of non-antitumor drugs`=
           ifelse(is.na(`Number of non-antitumor drugs`),
                  median(na.omit(`Number of non-antitumor drugs`)),
                  `Number of non-antitumor drugs`),
         
         `CD4 lymphocyte count`=
           ifelse(is.na(`CD4 lymphocyte count`),
                  median(na.omit(`CD4 lymphocyte count`)),
                  `CD4 lymphocyte count`),
         
         `T lymphocyte count`=
           ifelse(is.na(`T lymphocyte count`),
                  median(na.omit(`T lymphocyte count`)),
                  `T lymphocyte count`),
         
         `Number of previous anti-tumor drugs`=
           ifelse(is.na(`Number of previous anti-tumor drugs`),
                  median(na.omit(`Number of previous anti-tumor drugs`)),
                  `Number of previous anti-tumor drugs`),
         
         
         `Percentage of basophils`=
           ifelse(is.na(`Percentage of basophils`),
                  median(na.omit(`Percentage of basophils`)),
                  `Percentage of basophils`),
         
         
         `ICIs drugs`=
           ifelse(is.na(`ICIs drugs`),
                  median(na.omit(`ICIs drugs`)),
                  `ICIs drugs`) %>% as.factor()
  ) 
  
model_dat_82$`Number of underlying diseases` =model_dat_82$`Number of underlying diseases` %>% as.factor()


model_dat_82$`Number of underlying diseases` =  fct_collapse(model_dat_82$`Number of underlying diseases`,
                                                             "2" = c("2","3", "4", "5"))

model_dat_82$`Number of non-antitumor drugs` =model_dat_82$`Number of non-antitumor drugs` %>% as.factor()
model_dat_82$`Number of non-antitumor drugs` =
  fct_collapse(model_dat_82$`Number of non-antitumor drugs`,
               "1" = c("1","2","3","4","5","6") 
  )

model_dat_82_new <- model_dat_82 %>% 
  select(-`Number of previous anti-tumor drugs`) %>% 
  mutate(`History of lung diseases` = as.factor(`History of lung diseases`),
         `History of radiation therapy` = `History of radiation therapy` %>% as.factor(),
         outcome = as.factor(outcome)) 

model_dat_82_new <- model_dat_82_new %>% select(outcome,c(1:9))





dummyModel <- dummyVars(as.factor(outcome) ~ ., data = model_dat_82)

model_dat_82X <- as.data.frame(predict(dummyModel, newdata = model_dat_82))


model_dat_82_new1 <- cbind(model_dat_82_new,model_dat_82X[,c(6:12)])

model_dat_82X <- as.data.frame(predict(dummyModel, newdata = model_dat_82_new))
colnames(model_dat_82_new) <- c( "outcome","Temperature", "Number of underlying diseases", "History of lung diseases", 
                                 "ICIs drugs1", "ICIs drugs2","ICIs drugs3","ICIs drugs4","ICIs drugs5",
                                 "ICIs drugs6","ICIs drugs7","ICIs drugs8","Number of non-antitumor drugs", "History of radiation therapy", 
                                 "Number of previous anti-tumor drugs", "CD4 lymphocyte count", 
                                 "T lymphocyte count", "Percentage of basophils")
  
       
 
#--------------------------------------------------        


model_dat_82 <- model_dat %>% 
  mutate(`Number of combined drugs`=ifelse(is.na(`Number of combined drugs`),median(na.omit(`Number of combined drugs`)),`Number of combined drugs`),
         
         `Number of previous treatment drugs`=ifelse(is.na(`Number of previous treatment drugs`),median(na.omit(`Number of previous treatment drugs`) %>% as.numeric()),`Number of previous treatment drugs`),
         
         `Number of combined antitumor drugs`=ifelse(is.na(`Number of combined antitumor drugs`),median(na.omit(`Number of combined antitumor drugs`)),`Number of combined antitumor drugs`),
         
         
         `ICIs drugs`=ifelse(is.na(`ICIs drugs`),median(na.omit(`ICIs drugs`) %>% as.numeric()),`ICIs drugs`)
         
  ) %>% mutate(
    
    `Percentage of CD4 lymphocytes`=
      `Percentage of CD4 lymphocytes 2`
  ) %>% 
  
  select(-`Lung cancer`,-`Superior lobe`,-`method of administration`,-`Percentage of CD4 lymphocytes 2`) %>% 
  mutate(`ICIs dosage`=ifelse(`ICIs dosage`<=200,'1','2') %>% as.factor() ) %>% 
  mutate(`ICIs dosage` = `ICIs dosage` %>% as.factor(),
         `Previous lung disease` =`Previous lung disease` %>% as.factor(),
         Smoking = as.factor(Smoking),
         `Previous treatment_yn` =`Previous treatment_yn` %>% as.factor(),
         `ICIs drugs` = as.factor(`ICIs drugs`)) 


#%>% 
# mutate(`Number of underlying diseases` = as.factor(`Number of underlying diseases`)) 

##Number of underlying diseases;0为一类，1为一类，2，3，4，5及以上为一类






#1. number of privious treatment drugs 是不是对应原始表格中的“既往治疗药物种类”，如果是的话，麻烦修改为“既往抗肿瘤治疗药物种类”。合并方案：0 为一类，1-5位一类，然后是否可以再分亚组1+2、3-5.


model_dat_82$`Number of previous treatment drugs` =
  fct_collapse(model_dat_82$`Number of previous treatment drugs`,
               
               "1" = c("1","2","3", "4", "5") )


#2.number of combined drugs 是不是对应原始表格中的“合并普通药物种类”呢，如果是的话麻烦修改为“合并非抗肿瘤药物种类”。合并方案：0为一类，1-6为一类，然后是否可以再分亚组1+2、3-6.

model_dat_82$`Number of combined drugs` =
  fct_collapse(model_dat_82$`Number of combined drugs`,
               "1" = c("1", "2","3","4","5","6") 
  )

## 0为一类；1-4为一类，然后是否可以再分亚组1-2、3-4.

model_dat_82$`Number of combined antitumor drugs` =
  fct_collapse(model_dat_82$`Number of combined antitumor drugs`,
               "1" = c("1","2","3","4") 
  )

## KPS 合并 70 及以下为一类； 80为一类，90及以上为一类
model_dat_82$`KPS score` =
  fct_collapse(model_dat_82$`KPS score` %>% as.character,
               "70" = c("30","40","50","60","70") ,
               "90" = c("90","100")          
  )


#miss_var_summary(model_dat_82)

model_dat_82$outcome <- as.factor(model_dat_82$outcome)



# Create dummy variable

dummyModel <- dummyVars(as.factor(outcome) ~ ., data = model_dat_82)

model_dat_82X <- as.data.frame(predict(dummyModel, newdata = model_dat_82))


rangeModel <- preProcess(model_dat_82X, method = "range")
model_dat_82X <- predict(rangeModel, newdata = model_dat_82X)


model_dat_82_new <- cbind(model_dat_82$outcome, model_dat_82X)
names(model_dat_82_new)[1] <- "outcome"

colnames(model_dat_82_new) <- c( "outcome","Temperature", "Number of underlying diseases", "History of lung diseases", 
                                "ICIs drugs1", "ICIs drugs2","ICIs drugs3","ICIs drugs4","ICIs drugs5",
                                "ICIs drugs6","ICIs drugs7","ICIs drugs8","Number of non-antitumor drugs", "History of radiation therapy", 
                                "Number of previous anti-tumor drugs", "CD4 lymphocyte count", 
                                "T lymphocyte count", "Percentage of basophils")

cor_mat <- cor(model_dat_82_new[,-1]) %>% data.frame()


# model_dat_82_new_tmp <- model_dat_82_new[,c(-4,-12,-22,-25)] 
# 
# model_dat_82_new_tmp1 <- model_dat_82_new_tmp[,c(-23,-25,-27)]
# 
# model_dat_82_new <- model_dat_82_new_tmp1

## Split training and test dataset
model_dat_82_new1$outcome <- as.factor(model_dat_82_new1$outcome)
set.seed(355)
trainIndex <- createDataPartition(model_dat_82_new1$outcome, p = 0.8, list = FALSE)
trainingSet <- model_dat_82_new1[trainIndex,]
testSet <- model_dat_82_new1[-trainIndex,]

model_dat_82_new1$outcome %>% table() ## 48/190=0.2526
trainingSet$outcome %>% table() ## 34/134 =0.2537
testSet$outcome %>% table() ##14/56 =0.25



zero_one <- c('zero','one')
trainingSet$outcome
trainingSet$outcome <- zero_one[trainingSet$outcome]

testSet$outcome <- zero_one[testSet$outcome]

trainingSet =trainingSet %>% mutate(outcome = as.factor(outcome)) %>% arrange(outcome)

testSet =testSet %>% mutate(outcome = as.factor(outcome))%>% arrange(outcome)


set.seed(355)

rfFuncs$summary <- BigSummary

objectives =  'auc_ap_sbr' #'SBrier'# "SBrier"





x_tmp = trainingSet[,c(2:ncol(trainingSet))]

y_tmp = trainingSet[,1]

rfProfile <- rfe(x_tmp,y_tmp, sizes=c(1:ncol(trainingSet)), rfeControl=rfeControl(rfFuncs,
                                                                                  method = "repeatedcv",                                                                          repeats = 10,
                                                                                  number = 3
),

metric=objectives)

trellis.par.set(caretTheme())
plot(rfProfile, type = c("g", "o"))
predictors(rfProfile) 


#########new dataset

trainingSet1 <- trainingSet %>% select(outcome,predictors(rfProfile) %>% dput()) 


testSet1 <- testSet %>% select(outcome,predictors(rfProfile) %>% dput())

trainingSet1 <- trainingSet

testSet1 <- testSet

set.seed(355)
twoClassCtrl <- trainControl(
  method = "repeatedcv",
  number = 3,
  repeats = 10, #for repeatedcv
  savePredictions = "final",
  classProbs = T,
  summaryFunction = BigSummary,
  sampling = "up"
)#
 # 
#
set.seed(355)

only_underlying <- trainingSet1[,c(1,3)]
glmnet_m <- train(outcome ~., data = trainingSet1, 
                  method = "glmnet",  
                  metric = objectives, 
                  trControl = twoClassCtrl
                  )#, ,tuneLength = 20

coef(glmnet_m$finalModel,glmnet_m$bestTune$lambda)

roc_glmnet <- roc_f(model_m=glmnet_m,testdat=testSet1)
roc_glmnet 

rf_m <- train(outcome ~., data = trainingSet1, method = "rf",  metric = objectives,  trControl = twoClassCtrl, tuneLength = 10)

#xgb_m <- train(outcome ~., data = trainingSet1, method = "xgbTree",  metric = objectives,  trControl = twoClassCtrl)

saveRDS(glmnet_m, "glmnet_m1220.rds")
saveRDS(rf_m, "rf_m.rds")

glmnet_m <- read_rds( "glmnet_m.rds")
rf_m <- read_rds("rf_m.rds")

roc_glmnet <- roc_f(model_m=glmnet_m,testdat=testSet1)
roc_glmnet 
#train: Area under the curve: 0.8199
# 95% CI: 0.7969-0.8437 (2000 stratified bootstrap replicates)

##  test: Area under the curve: 0.9019
##  95% CI: 0.7169-0.9643 (2000 stratified bootstrap replicates)

roc_rf <- roc_f(model_m=rf_m,testdat=testSet1)

##  training: Area under the curve: 0.8128
# 95% CI: 0.7891-0.8362 (2000 stratified bootstrap replicates)

## test Area under the curve: 0.8636
## 95% CI: 0.6703-0.9598 (2000 stratified bootstrap replicates)


glm_perf <- sum_perf_f(glmnet_m, testdat=testSet1,model_name='ElasticNet') %>% data.frame()
rf_perf <- sum_perf_f(rf_m, testdat=testSet1,model_name='RF') %>% data.frame()

metrics_res <- rbind(glm_perf,rf_perf)


ks_glm <- kernelshap(glmnet_m, trainingSet1[,-1],
                     bg_X = trainingSet1,
                     type="prob")

ks_glm_tmp <- ks_glm$S[[1]] %>% data.frame()

ks_glm1 <- ks_glm$S[[1]]

ks_glm2 <- ks_glm$S[[2]] %>% as.data.frame() %>% as.matrix()

sv_importance(shapviz(ks_glm1[,c(1:17)],trainingSet1[,-1]), "bee")

sv_importance(shapviz(ks_glm1[,c(11:17)],trainingSet1[,-1]), "bee")

sv_importance(shapviz(ks_glm2,trainingSet1[,-1]), "bee")


#colnames(ks_glm2) <-  c("Sindillizumab", "Number of underlying diseases_2", "ohter ICIs drugs", 
 # "Percentage of NK cell", "Number of underlying diseases_0", 
 # "Previous lung disease", "`Perbolizumab", "Temperature", "Tirelizumab", 
 # "Carrilizumab", "Percentage of neutrophilic granulocyte")

glm_cat_test <- pred_cat_f(model_m = glmnet_m,model_name='ElasticNet',
                           testdat=testSet1,data_type = 'Test')%>% data.frame()

rf_cat_test <- pred_cat_f(model_m = rf_m,model_name='RF',testdat=testSet1,data_type = 'Test')%>% data.frame()

xgb_cat_test <- pred_cat_f(model_m = xgb_m,model_name='XGBoost',testdat=testSet1,data_type = 'Test')%>% data.frame()


glm_cat_val <- pred_cat_f(model_m = glmnet_m,model_name='ElasticNet',testdat=testSet1,data_type = 'Validation')%>% data.frame()

rf_cat_val <- pred_cat_f(model_m = rf_m,model_name='RF',testdat=testSet1,data_type = 'Validation')%>% data.frame()

xgb_cat_val <- pred_cat_f(model_m = xgb_m,model_name='XGBoost',testdat=testSet1,data_type = 'Validation')%>% data.frame()


cat_sum <- rbind(glm_cat_test,rf_cat_test,xgb_cat_test,
                 glm_cat_val,rf_cat_val,xgb_cat_val
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




cali_plot_f(model_m=glmnet_m,model_name='ElasticNet',testdat=testSet1)

cali_plot_f(model_m=rf_m,model_name='RF',testdat=testSet1)


pred_train_dat <- glmnet_m$pred %>% 
  select(obs,pred,one,zero)



library(CalibrationCurves)
cal_train <- val.prob.ci.2(pred_train_dat$one, c(1,0)[pred_train_dat$obs], CL.smooth = TRUE, logistic.cal = FALSE, lty.log = 2,
                           col.log = "red", lwd.log = 1.5)


val.prob.ci.2(pred_test_dat$one, c(1,0)[pred_test_dat$obs], CL.smooth = TRUE, logistic.cal = FALSE, lty.log = 2,
              col.log = "red", lwd.log = 1.5)

predictions = pred_train_dat$one
labels = pred_train_dat$obs

z = log(predictions/(1-predictions))
logit_newprob = -1.11+z
new_nprob = 1/(1+exp(-logit_newprob ))

cali_dat = data.frame(id = c(1:nrow(testSet)),
                      labels,new_nprob )#predictions

cali_dat = data.frame(id = c(1:nrow(testSet)),
                      labels,predictions)#predictions


cali_dat = data.frame(id = c(1:nrow(pred_train_dat)),
                      c(1,0)[pred_train_dat$obs],pred_train_dat$one)


####### calibration on the train before calibration
calibration.plot(cali_dat, which.model = 1, na.rm = FALSE, alpha = 0.05, N.bins = 5, 
                 xlab = "Predicted Probability (before recalibration)", 
                 ylab = "Observed Proportion", 
                 main = NULL, color= NULL, model.names= NULL)

########calibration on the train after recalibration
cali_dat = data.frame(id = c(1:nrow(pred_train_dat)),
                      c(1,0)[pred_train_dat$obs],new_nprob)

calibration.plot(cali_dat, which.model = 1, na.rm = FALSE, alpha = 0.05, N.bins = 5, 
                 xlab = "Predicted Probability (Validation)", 
                 ylab = "Observed Proportion", 
                 main = NULL, color= NULL, model.names= NULL)

##########calibration on the test 

pred_test_dat <- pred_dat_f(dat = testSet1,
                            outcome=testSet1$outcome,
                            model=glmnet_m)

predictions = pred_test_dat$one

z = log(predictions/(1-predictions))
logit_newprob = -1.11+z
new_nprob1 = 1/(1+exp(-logit_newprob ))

cali_dat = data.frame(id = c(1:nrow(pred_test_dat)),
                      c(1,0)[pred_test_dat$obs],new_nprob1)#pred_test_dat$one

calibration.plot(cali_dat, which.model = 1, na.rm = FALSE, alpha = 0.05, N.bins = 5, 
                 xlab = "Predicted Probability (Validation)", 
                 ylab = "Observed Proportion", 
                 main = NULL, color= NULL, model.names= NULL)

######calculate PPV based on the recalibrated probability

pred_train_dat_recali <- pred_train_dat %>% mutate(one = new_nprob)
pred_test_dat_recali <- pred_test_dat %>% mutate(one = new_nprob1)


glm_cat_train <- pred_cat_f2(pred_train_dat_recali)%>% data.frame()

glm_cat_test <- pred_cat_f2(pred_test_dat_recali)%>% data.frame()


