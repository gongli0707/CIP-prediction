model_dat0302 <- table1_dat[table1_dat %>% colnames() %in% miss_20_vars]
model_dat0302 <- model_dat0302 %>% data.frame()

Mode <- function(x) {
  ux <- unique(na.omit(x))
  index <- ux[which.max(tabulate(match(x, ux)))]
  
 x <-  ifelse(is.na(x),index,x)
 x
  
}


fillmedian_f  <- function(x){
  x<-as.numeric(x) #first convert each column into numeric if it is from factor
  x = ifelse(is.na(x),median(x, na.rm=TRUE),x)
  
  #convert the item with NA to median value from the column
  x = as.numeric(x)#display the column
  x
}


kept_num_cols <-c( "年龄", "身高cm", "体重kg", "BMI", "体温.", "收缩压", 
                   "舒张压",  "免疫药物用法用量mg", "疗程.次数.",
                   "CD4淋巴细胞绝对计数.550.1440.10.6.L....61", 
                   "CD4淋巴细胞百分比.27.51....62", "CD8淋巴细胞绝对计数.320.1250.10.6.L....63", 
                   "CD8淋巴细胞百分比.15.44....64", "总T淋巴细胞绝对计数.955.2860.10.6.L....65", 
                   "总T淋巴细胞百分比.50.84....66", "B淋巴细胞绝对计数..90.560.10.6.L....67", 
                   "B淋巴细胞百分比.5.18....68", "NK细胞绝对计数..150.1100.10.6.L....69", 
                   "NK细胞百分比.7.4....70", "红细胞.HR..4.3.5.8.10.12.L....81", 
                   "血红蛋白.HR..130.175g.L...82", "白细胞.HR.3.5.9.5.10.9.L...83", 
                   "淋巴细胞百分比20.50....84", "单核细胞百分比3.10...85", "中性粒细胞百分比40.75...86", 
                   "嗜酸细胞百分比0.4.8.0...87", "嗜碱细胞百分比.0.1...88", "血小板.HR.125.350.10.9.L...89", 
                   "红细胞.HR..4.3.5.8.10.12.L....90", "血红蛋白.HR..130.175g.L...91", 
                   "白细胞.HR.3.5.9.5.10.9.L...92", "淋巴细胞百分比20.50....93", 
                   "单核细胞百分比3.10...94", "中性粒细胞百分比40.75...95", "嗜酸细胞百分比0.4.8.0...96", 
                   "嗜碱细胞百分比.0.1...97", "血小板.HR.125.350.10.9.L...98"
)


kept_factor_cols <- c( "性别","是否吸烟", "是否饮酒", "KPS评分", "是否肺癌", "上叶", 
                       "下叶", "cancer.stage.I.II.III.IV.", "合并基础疾病数量", "既往是否有肺部疾病", 
                       "既往或现在是否有自身免疫性疾病", "免疫药物名称","是否首次使用免疫治疗", "免疫单用或联用",  "合并其他抗肿瘤药物种类", 
                       "合并普通药物种类", "是否手术.Y.N.", "Prior.cancer.treatment.radiation.Y.N.", 
                       "既往是否治疗", "既往治疗药物种类","label")


c("NSCLC", "Temperature", "Number of underlying diseases", "History of lung diseases", 
  "ICIs drugs", "Number of non-antitumor drugs", "History of radiation therapy", 
  "Number of previous anti-tumor drugs", "CD4 lymphocyte count", 
  "T lymphocyte count", "Percentage of basophils", "outcome")

for(i in which(colnames(model_dat0302) %in% kept_num_cols)){
  model_dat0302[,i] <-  as.numeric(model_dat0302[,i])
}
model_dat0302$label <- as.factor(table1_dat$label)

for(i in which(colnames(model_dat0302) %in% kept_num_cols)){
  model_dat0302[,i] <-  fillmedian_f(model_dat0302[,i])
}


for(i in which(colnames(model_dat0302) %in% kept_factor_cols)){
  model_dat0302[,i] <-  Mode(model_dat0302[,i])
}


for(i in which(colnames(model_dat0302) %in% kept_factor_cols)){
  model_dat0302[,i] <-  as.factor(model_dat0302[,i])
}




tmp <- miss_var_summary(model_dat0302) %>% data.frame()


model_dat1_0302_new <- cbind(model_dat0302,input_template)



nums <- c("Temperature", "CD4 lymphocyte count", 
          "T lymphocyte count", "Percentage of basophils")


factors <- c("NSCLC",  "Number of underlying diseases", "History of lung diseases", 
             "ICIs drugs", "Number of non-antitumor drugs", "History of radiation therapy", 
             "Number of previous anti-tumor drugs", "outcome")



c("NSCLC",  "Number of underlying diseases", "History of lung diseases", 
  "ICIs drugs", "Number of non-antitumor drugs", "History of radiation therapy", 
  "Number of previous anti-tumor drugs")

for(i in which(colnames(model_dat1_0302_new) %in% nums)){
  model_dat1_0302_new[,i] <-  as.numeric(model_dat1_0302_new[,i])
}

for(i in which(colnames(model_dat1_0302_new) %in% factors)){
  model_dat1_0302_new[,i] <-  as.factor(model_dat1_0302_new[,i])
}


for(i in which(colnames(model_dat1_0302_new) %in% nums)){
  model_dat1_0302_new[,i] <-  fillmedian_f(model_dat1_0302_new[,i])
}


for(i in which(colnames(model_dat1_0302_new) %in% factors)){
  model_dat1_0302_new[,i] <-  Mode(model_dat1_0302_new[,i])
}

tmp <- miss_var_summary(model_dat1_0302_new)


model_dat1_0302_new1 <- model_dat1_0302_new[,-c(6,16,17,19,22,24,25,27,30,34,47,58)]

