##########https://gweissman.github.io/post/evaluating-the-equivalence-of-different-formulations-of-the-scaled-brier-score/


BigSummary <- function (data, lev = NULL, model = NULL) {
  pr_auc <- try(MLmetrics::PRAUC(data[, lev[2]],
                                 ifelse(data$obs == lev[2], 1, 0)),
                silent = TRUE)
  brscore <- try(mean((data[, lev[2]] - ifelse(data$obs == lev[2], 1, 0)) ^ 2),
                 silent = TRUE)
  sbrscore <- try(1-(brscore/mean((ifelse(data$obs == lev[2], 1, 0) - mean(ifelse(data$obs == lev[2], 1, 0)))^2)),
                  silent=TRUE)
  
 
  rocObject <- try(pROC::roc(ifelse(data$obs == lev[2], 1, 0), data[, lev[2]],
                             direction = "<", quiet = TRUE), silent = TRUE)
  if (inherits(pr_auc, "try-error")) pr_auc <- NA
  if (inherits(brscore, "try-error")) brscore <- NA
  rocAUC <- if (inherits(rocObject, "try-error")) {
    NA
  } else {
    rocObject$auc
  }
  tmp <- unlist(e1071::classAgreement(table(data$obs,
                                            data$pred)))[c("diag", "kappa")]
  # scaled_prauc <-  (pr_auc-min(pr_auc))*(1/(max(pr_auc)-min(pr_auc)))
  # scaled_rocAUC <-  (rocAUC-min(rocAUC))*(1/(max(rocAUC)-min(rocAUC)))
  # scaled_sbrscore <-  (sbrscore-min(sbrscore))*(1/(max(sbrscore)-min(sbrscore)))
  # weighted_perf <- (1/3)*(scaled_prauc+scaled_rocAUC+scaled_sbrscore)
  out <- c(Acc = tmp[[1]],
           Kappa = tmp[[2]],
           AUCROC = rocAUC,
           AUCPR = pr_auc,
           SBrier = sbrscore,
           Brier = brscore,
          # Spiegelhalter_z = s_z,
          # Spiegelhalter_p = s_p,
           Precision = caret:::precision.default(data = data$pred,
                                                 reference = data$obs,
                                                 relevant = lev[2]),
           Recall = caret:::recall.default(data = data$pred,
                                           reference = data$obs,
                                           relevant = lev[2]),
           F = caret:::F_meas.default(data = data$pred, reference = data$obs,
                                      relevant = lev[2]),
           auc_ap_sbr= mean(c(rocAUC,pr_auc,sbrscore))
           #s_auc_ap_sbr = weighted_perf
           )
  
  out
}


pred_dat_f <- function(dat,outcome,model){
  
  pred_dat = data.frame(obs=outcome,
                        pred = predict(model,dat)
  )
  
  out <- cbind(pred_dat,predict(model, dat,type='prob'))
  return(out)
  
}

#BigSummary(pred_dat_f(testSet1,testSet1$outcome,glmnet_m), lev = classes, model = glmnet_m)

sum_perf_f <- function(model_m,
                       model_name,
                       testdat,
                       classes= c("zero","one"),
                       ...){
  
  pred_train_dat <- model_m$pred %>% 
    select(obs,pred,one,zero)
  
  pred_test_dat <- pred_dat_f(dat = testdat,
                              outcome=testdat$outcome,
                              model=model_m)
  
  
  train_perf <- c(data_type="Validation",
                  model_name = model_name,
                  BigSummary(pred_train_dat,lev = classes),s_f(pred_train_dat,
                                                               lev = classes))
  test_perf <- c(data_type="Test",
                 model_name = model_name,
                 BigSummary(pred_test_dat,lev = classes),s_f(pred_test_dat,
                                                             lev = classes))
  
  out <- rbind(train_perf,test_perf)
  out
  
}

roc_f <- function(model_m,testdat,
                  ...){
  
  pred_train_dat <- model_m$pred %>% 
    select(obs,pred,one,zero)
  
  pred_test_dat <- pred_dat_f(dat = testdat,
                              outcome=testdat$outcome,
                              model=model_m)
  
  roc_train <- roc(c(1,0)[pred_train_dat$obs], pred_train_dat$one,smooth =TRUE,ci = TRUE)
  
  roc_test <- roc(c(1,0)[pred_test_dat$obs], pred_test_dat$one,smooth =TRUE,ci = TRUE)
  
  return(list(roc_train,roc_test))
  
}

viz_perf_f <- function(model_m,
                       testdat,
                       bin_num,
                       ...){
  
  pred_train_dat <- model_m$pred %>% 
    select(obs,pred,one,zero)
  
  pred_test_dat <- pred_dat_f(dat = testdat,
                              outcome=testdat$outcome,
                              model=model_m)
  
  res_train <- evalm(pred_train_dat %>% 
                       select(one,zero,obs)
                     
                     ,positive = 'one',bin=bin_num)
  
  
  res_test <- evalm(pred_test_dat %>% 
                      select(one,zero,obs)
                    
                    ,positive = 'one',bin=bin_num)
  
  return(list(res_train,res_test))
}

vip_f <- function(model_m,model_name,...){
  importance <- varImp(model_m, scale=TRUE)[["importance"]] %>% data.frame()
  
  importance$Overall <- importance$Overall / sum(importance$Overall)
  importance$var_name <- rownames(importance)
  importance$model_name <- model_name
  importance <- importance %>% arrange(desc(Overall))
  return(importance)
}



s_f <- function (data, lev = NULL, model = NULL){
  
  s_z <-  try(rms::val.prob(data[, lev[2]],ifelse(data$obs == lev[2], 1, 0))[17])
  s_p <-  try(rms::val.prob(data[, lev[2]],ifelse(data$obs == lev[2], 1, 0))[18])
  
 out <- c(Spiegelhalter_z = s_z,
          Spiegelhalter_p = s_p)
 return(out)
}

cali_dat_f <- function(model_m,testdat){
  
  pred_train_dat <- model_m$pred %>% 
    select(obs,pred,one,zero)
  
  pred_test_dat <- pred_dat_f(dat = testdat,
                              outcome=testdat$outcome,
                              model=model_m)
  
  cali_train_dat = data.frame(id = c(1:nrow(pred_train_dat)),
                              c(1,0)[pred_train_dat$obs], pred_train_dat$one)
  
  cali_test_dat = data.frame(id = c(1:nrow(pred_test_dat)),
                             c(1,0)[pred_test_dat$obs], pred_test_dat$one)
  
  out <- list(cali_train_dat,cali_test_dat)
  return(out)
  
}


cali_plot_f <- function(model_m,model_name,testdat){
  
  calidat <- cali_dat_f(model_m=model_m,testdat=testdat)
  
  plot_val_out <- calibration.plot(calidat[[1]], which.model = 1, na.rm = FALSE, alpha = 0.05, N.bins = 5, 
                                   xlab = paste0("Predicted Probability",model_name), 
                                   ylab = "Observed Proportion", 
                                   main = 'Validation', color= NULL, model.names= NULL)
  
  plot_test_out <- calibration.plot(calidat[[2]], which.model = 1, na.rm = FALSE, alpha = 0.05, N.bins = 5, 
                                    xlab = paste0("Predicted Probability",model_name), 
                                    ylab = "Observed Proportion", 
                                    main = 'Test', color= NULL, model.names= NULL)
  
  out <- list(plot_val_out,plot_test_out)
  
}



## pred_cat

pred_cat_f <- function(model_m,model_name,testdat=NULL,data_type='Test',...){
  
  
  
  pred_train_dat <- model_m$pred %>% 
    select(obs,pred,one,zero)
  
 pred_test_dat <- pred_dat_f(dat = testdat,
                                          outcome=testdat$outcome,
                                          model=model_m)
 
if(data_type=='Test'){
  pred_dat <- pred_test_dat
}else{
  pred_dat <- pred_train_dat
 }
   

  risksub =pred_dat[pred_dat$one<0.05,]
  
  rate=paste0(0.05)
  
  
  casenum=sum(c(1,0)[risksub$obs])
  
  n_total <- nrow(risksub)
  
  ppv=casenum/n_total
  
  tempout1 <- c(rate,casenum,n_total, ppv)
  
  ###########
  
  
  risksub =pred_dat[pred_dat$one>=0.05 & pred_dat$one<0.2,]
  
  
  rate= paste0(0.05,"-",0.2)
  
 
  casenum=sum(c(1,0)[risksub$obs])
  
  n_total <- nrow(risksub)
  
  ppv=casenum/n_total
  
  tempout2 <- c(rate,casenum,n_total, ppv)
  
  ###########3
  
  risksub =pred_dat[pred_dat$one>=0.2 & pred_dat$one<0.5,]
  
  
  rate= paste0(0.2,"-",0.5)
  
  
  casenum=sum(c(1,0)[risksub$obs])
  
  n_total <- nrow(risksub)
  
  ppv=casenum/n_total
  
  tempout3 <- c(rate,casenum,n_total, ppv)
  
  ##########4
  risksub =pred_dat[pred_dat$one>=0.5 & pred_dat$one<0.8,]
  
  
  rate= paste0(0.5,"-",0.8)
  
  
  casenum=sum(c(1,0)[risksub$obs])
  
  n_total <- nrow(risksub)
  
  ppv=casenum/n_total
  
  tempout4 <- c(rate,casenum,n_total, ppv)
  
  ###########3
  
  risksub =pred_dat[pred_dat$one>=0.8,]
  
  
  rate= paste0(0.8)
  
  
  casenum=sum(c(1,0)[risksub$obs])
  
  n_total <- nrow(risksub)
  
  ppv=casenum/n_total
  
  tempout5 <- c(rate,casenum,n_total, ppv)
  ##

  pred_cat =rbind(tempout1,tempout2,tempout3,tempout4, tempout5)
  pred_cat = cbind(model_name,data_type,pred_cat)

  #colnames(pred_cat) <- c('model_name','data_type',"predicted risk","#POI","#pts","POI rate")
  
  pred_cat
}

pred_cat_f2 <- function(pred_dat,...){
  
  
  
  
  
  
  risksub =pred_dat[pred_dat$one<0.05,]
  
  rate=paste0(0.05)
  
  
  casenum=sum(c(1,0)[risksub$obs])
  
  n_total <- nrow(risksub)
  
  ppv=casenum/n_total
  
  tempout1 <- c(rate,casenum,n_total, ppv)
  
  ###########
  
  
  risksub =pred_dat[pred_dat$one>=0.05 & pred_dat$one<0.2,]
  
  
  rate= paste0(0.05,"-",0.2)
  
  
  casenum=sum(c(1,0)[risksub$obs])
  
  n_total <- nrow(risksub)
  
  ppv=casenum/n_total
  
  tempout2 <- c(rate,casenum,n_total, ppv)
  
  ###########3
  
  risksub =pred_dat[pred_dat$one>=0.2 & pred_dat$one<0.5,]
  
  
  rate= paste0(0.2,"-",0.5)
  
  
  casenum=sum(c(1,0)[risksub$obs])
  
  n_total <- nrow(risksub)
  
  ppv=casenum/n_total
  
  tempout3 <- c(rate,casenum,n_total, ppv)
  
  ##########4
  risksub =pred_dat[pred_dat$one>=0.5 & pred_dat$one<0.8,]
  
  
  rate= paste0(0.5,"-",0.8)
  
  
  casenum=sum(c(1,0)[risksub$obs])
  
  n_total <- nrow(risksub)
  
  ppv=casenum/n_total
  
  tempout4 <- c(rate,casenum,n_total, ppv)
  
  ###########3
  
  risksub =pred_dat[pred_dat$one>=0.8,]
  
  
  rate= paste0(0.8)
  
  
  casenum=sum(c(1,0)[risksub$obs])
  
  n_total <- nrow(risksub)
  
  ppv=casenum/n_total
  
  tempout5 <- c(rate,casenum,n_total, ppv)
  ##
  
  pred_cat =rbind(tempout1,tempout2,tempout3,tempout4, tempout5)
  #pred_cat = cbind(model_name,data_type,pred_cat)
  
  #colnames(pred_cat) <- c('model_name','data_type',"predicted risk","#POI","#pts","POI rate")
  
  pred_cat
}




