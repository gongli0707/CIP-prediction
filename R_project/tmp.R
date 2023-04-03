model_dat1_0302

model_dat1_0302_new <- cbind(model_dat1_0302,input_template)



for(i in which(colnames(model_dat1_0302_new) %in% c("Number of underlying diseases", 
                                                    "History of lung diseases", "ICIs drugs", "Number of non-antitumor drugs", 
                                                    "History of radiation therapy", "Number of previous anti-tumor drugs")   )){
  model_dat1_0302_new[,i] <-  Mode(model_dat1_0302_new[,i])
}



for(i in which(colnames(model_dat1_0302_new) %in% c( "CD4 lymphocyte count", "T lymphocyte count", "Percentage of basophils")   )){
  model_dat1_0302_new[,i] <-  fillmedian_f(model_dat1_0302_new[,i])
}


model_dat1_0302_new %>% miss_var_summary()

model_dat_1220 <- read_excel("E:/2022/IRP/R_project/data/model_dat_1220.xlsx")
model_dat <- model_dat_1220

NSCLC = model_dat_82_new2$`NSCLC(yes/no)`

input_template <-   cbind(NSCLC,model_dat)

saveRDS(input_template,'input_template.rds')



miss <- miss_var_summary(model_dat) %>%
  mutate(pct_miss = round(pct_miss, 2)) %>%
  filter(pct_miss > 0)


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

model_dat_82_new1 <- model_dat_82_new %>% select(outcome,c(1:9)) %>% 
  select(-`ICIs drugs`,-`Number of underlying diseases`)


kept_vars <- c("性别",                                  
               "年龄" ,                                 
               "身高cm"  ,                              
               "体重kg" ,                               
               "BM1"   ,                                   
               "收缩压"  ,                              
               "舒张压" ,                               
               "是否吸烟"    ,                          
               "是否饮酒"  ,                            
               "KPS评分"  ,                             
               "是否肺癌" ,                             
               "上叶"   ,                               
               "下叶"  ,                                
               "cancer stage（1，2，3，4）"  ,             
               "既往或现在是否有自身免疫性疾病" ,          
               "免疫药物用法用量mg"    ,                
               "是否首次使用免疫治疗" ,                 
               "疗程（次数）" ,                            
               "是否手术(Y/N)"  ,                       
               "既往是否治疗"  ,                        
               "既往治疗药物种类" ,                     
               "CD4淋巴细胞百分比（27-51）",            
               "CD8淋巴细胞绝对计数(320-1250×是否^6/L)",
               "CD8淋巴细胞百分比（15-44）"  ,          
               "总T淋巴细胞百分比（50-84）"  ,          
               "B淋巴细胞绝对计数( 90-560×是否^6/L)" ,  
               "B淋巴细胞百分比（5-18）"  ,             
               "NK细胞绝对计数( 150-1是否0×是否^6/L)" , 
               "NK细胞百分比（7-4）"    ,               
               "红细胞(HR)（4.3-5.8*是否^12/L）...40" , 
               "血红蛋白(HR) 130-175g/L...41" ,         
               "白细胞(HR)3.5-9.5*是否^9/L...42"    ,   
               "淋巴细胞百分比20-50%...43" ,            
               "单核细胞百分比3-是否...44" ,            
               "中性粒细胞百分比40-75...45"   ,         
               "嗜酸细胞百分比0.4-8.0...46"  ,             
               "血小板(HR)125-350*是否^9/L...48",       
               "红细胞(HR)（4.3-5.8*是否^12/L）...49" , 
               "血红蛋白(HR) 130-175g/L...50"    ,      
               "白细胞(HR)3.5-9.5*是否^9/L...51",       
               "淋巴细胞百分比20-50%...52"     ,        
               "单核细胞百分比3-是否...53" ,            
               "中性粒细胞百分比40-75...54" ,           
               "嗜酸细胞百分比0.4-8.0...55"  ,          
               "嗜碱细胞百分比 0-1...56"    ,           
               "血小板(HR)125-350*是否^9/L...57",       
               "label"     ,                            
               "NSCLC"  ,                               
               "Temperature"   ,                        
               "Number of underlying diseases" ,        
               "History of lung diseases" ,             
               "ICIs drugs"     ,                       
               "Number of non-antitumor drugs"   ,      
               "History of radiation therapy" ,         
               "Number of previous anti-tumor drugs",   
               "CD4 lymphocyte count",                  
               "T lymphocyte count"  ,                  
               "Percentage of basophils"    ,           
               "outcome" )


model_dat1_0302_new1 <- model_dat1_0302_new[,-c(6,16,17,19,22,24,25,27,30,34,47,58)]


factor_var <- c("性别","是否吸烟", "是否饮酒", "KPS评分", "是否肺癌", "上叶", "下叶", 
  "cancer stage（1，2，3，4）", "既往或现在是否有自身免疫性疾病","是否首次使用免疫治疗", "疗程（次数）", 
  "是否手术(Y/N)", "既往是否治疗", "既往治疗药物种类", "Number of underlying diseases", "History of lung diseases", 
  "ICIs drugs", "Number of non-antitumor drugs", "History of radiation therapy", 
  "Number of previous anti-tumor drugs")


for(i in which(colnames(model_dat) %in% kept_num_cols)){
  model_dat1[,i] <-  as.numeric(model_dat1[,i])
}
model_dat1$label <- as.factor(table1_dat$label)

for(i in which(colnames(model_dat1_0302_new1) %in% factor_var)){
  model_dat1_0302_new1[,i] <-  as.factor(model_dat1_0302_new1[,i])
}




model_dat_82 <- model_dat1_0302_new1 
dummyModel <- dummyVars(as.factor(outcome) ~ ., data = model_dat_82)

model_dat_82X <- as.data.frame(predict(dummyModel, newdata = model_dat_82))


model_dat_82_new2 <- cbind(model_dat_82_new1,model_dat_82X[,c(3,4,6:12)],underlying_disease_original_v1$NSCLC)


saveRDS(model_dat_82_new2, "model_dat_82_new2.rds")

model_dat_82X <- as.data.frame(predict(dummyModel, newdata = model_dat_82))
colnames(model_dat_82_new2) <- c( "outcome", "Temperature", "History of lung diseases(yes/no)" , "Number of non-antitumor drugs", 
                                  "History of radiation therapy", "CD4 lymphocyte count", "T lymphocyte count", 
                                  "Percentage of basophils",'Number of underlying diseases=1',
                                  'Number of underlying diseases>=2',
                                 "Attilizumab(yes/no)", "Carrilizumab(yes/no)","Tirelizumab(yes/no)","Nevirumab(yes/no)","Pembrolizumab(yes/no)",
                                 "Toripalimab(yes/no)","Sindillizumab(yes/no)",'NSCLC(yes/no)')


colnames(model_dat_82_new2) <- c( "outcome", "Temperature", "History of lung diseases", "Number of non-antitumor drugs", 
                                  "History of radiation therapy", "CD4 lymphocyte count", "T lymphocyte count", 
                                  "Percentage of basophils",'Number of underlying diseases=1',
                                  'Number of underlying diseases>=2',
                                  "ICI1", "ICI2","ICI3","ICI4","ICI5",
                                  "ICI6","ICI7",'NSCLC')


model_dat_82$outcome <- as.factor(model_dat_82$outcome)
set.seed(355)

model_dat_82_new2 <- cbind(model_dat_82$outcome, model_dat_82X)
colnames(model_dat_82_new2) <- c('outcome',colnames(model_dat_82X))

trainIndex <- createDataPartition(model_dat_82_new2$outcome, p = 0.8, list = FALSE)
trainingSet <- model_dat_82_new2[trainIndex,]
testSet <- model_dat_82_new2[-trainIndex,]

model_dat_82_new2$outcome %>% table() ## 48/190=0.2526
trainingSet$outcome %>% table() ## 34/134 =0.2537
testSet$outcome %>% table() ##14/56 =0.25



zero_one <- c('zero','one')
trainingSet$outcome
trainingSet$outcome <- zero_one[trainingSet$outcome]

testSet$outcome <- zero_one[testSet$outcome]

trainingSet =trainingSet %>% mutate(outcome = as.factor(outcome)) %>% arrange(outcome)

testSet =testSet %>% mutate(outcome = as.factor(outcome))%>% arrange(outcome)


colnames(trainingSet) <- c('outcome',paste0('var',c(1:108)))
colnames(testSet) <- c('outcome',paste0('var',c(1:108)))

set.seed(355)

rfFuncs$summary <- BigSummary

objectives = 'SBrier'  #'auc_ap_sbr' # # ## "SBrier"





x_tmp = trainingSet[,c(2:ncol(trainingSet))]



y_tmp = trainingSet[,1]

rfProfile <- rfe(x_tmp,y_tmp, sizes=c(1:ncol(trainingSet)), 
                 rfeControl=rfeControl(rfFuncs,
                                       method = "repeatedcv", 
                                       repeats = 10, number = 3
),metric=objectives)

trellis.par.set(caretTheme())
plot(rfProfile, type = c("g", "o"))
predictors(rfProfile) %>% dput()

saveRDS(rfProfile,file = 'output/rfProfile.rds')


#########new dataset

trainingSet1 <- trainingSet %>% select(outcome,c("Sindillizumab(yes/no)", "Number of underlying diseases>=2", "History of lung diseases(yes/no)", 
                                                 "Tirelizumab(yes/no)", "NSCLC(yes/no)", "Pembrolizumab(yes/no)", "CD4 lymphocyte count", 
                                                 "Temperature")) 


testSet1 <- testSet %>% select(outcome,c("Sindillizumab(yes/no)", "Number of underlying diseases>=2", "History of lung diseases(yes/no)", 
                                         "Tirelizumab(yes/no)", "NSCLC(yes/no)", "Pembrolizumab(yes/no)", "CD4 lymphocyte count", 
                                         "Temperature")) 

# trainingSet1 <- trainingSet
# 
# testSet1 <- testSet

set.seed(355)
twoClassCtrl <- trainControl(
  method = "repeatedcv",
  number = 3,
  repeats = 10, #for repeatedcv
  savePredictions = "final",
  classProbs = T,
  summaryFunction = BigSummary
  
)#,sampling = "up"
# 
#
set.seed(355)

glmnet_m <- train(outcome ~., data = trainingSet, 
                  method = "glmnet",  
                  metric = objectives, 
                  trControl = twoClassCtrl,
        
)#, ,          tuneLength = 20


set.seed(355)
roc_f(model_m = glmnet_m,testdat= testSet)
glm_perf <- sum_perf_f(glmnet_m, testdat=testSet,model_name='ElasticNet') %>% data.frame()

coef(glmnet_m$finalModel,glmnet_m$bestTune$lambda)

ks_glm <- kernelshap(glmnet_m, trainingSet1[,-1],
                     bg_X = trainingSet1,
                     type="prob")



ks_glm_tmp <- ks_glm$S[[1]] %>% data.frame()

ks_glm1 <- ks_glm$S[[1]] 

ks_glm2 <- ks_glm$S[[2]]

ks_glm11 <- ks_glm$S[[1]] %>% data.frame()

ks_glm1_abs <- apply(ks_glm11,2,abs) %>% data.frame()


ks_feature_importance <- apply(ks_glm1_abs,2,mean) %>% data.frame()

ks_feature_importance$. %>% sum()


ks_feature_importance1 <- ks_feature_importance %>% 
  mutate(fi = ./sum(.),
         variable_name = rownames(ks_feature_importance))

sv_importance(shapviz(ks_glm1,trainingSet1[,-1]), "bar",fill = "#8f8f8f")
  
sv_importance(shapviz(ks_glm1,trainingSet1[,-1]), "bee",color_bar_title = 'Feature Value', viridis_args = list(begin = 0.13, end = 1, option = "turbo"))

sv_importance(shapviz(ks_glm1,trainingSet1[,-1]), "bee")


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



ggplot(data=model_dat)+ 
  geom_bar(aes(as.factor(model_dat$`Number of underlying diseases`)))+
  xlab('Number of underlyining diseases')+
  ylab('Frequency')
