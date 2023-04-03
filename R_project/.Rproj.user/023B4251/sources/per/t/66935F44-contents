underlying_disease_original <- read_excel("data/underlying disease_original.xlsx")

##糖尿病、高血压、冠心病、慢阻肺、乙肝
underlying_disease_original_v1 <- underlying_disease_original %>% 
  mutate(hypertension = grepl(pattern = '高血压', `合并基础疾病`) %>% as.factor(),
         diabetes = grepl(pattern = '糖尿',`合并基础疾病`)%>% as.factor(),
         CHD = grepl(pattern = '冠状',`合并基础疾病`)%>% as.factor(),
         hepatitis = (grepl('肝炎',`合并基础疾病`) & grepl('乙',`合并基础疾病`))%>% as.factor(),
         lung = grepl('肺气肿|结核|阻塞性肺|肺积|右肺腺癌|肺不张|矽肺',`合并基础疾病`)%>% as.factor(),

         
         underlying_yn = ifelse((hypertension==TRUE | diabetes==TRUE | CHD==TRUE | hepatitis==TRUE |lung==TRUE), TRUE,FALSE)%>% 
         as.factor(),
          ici = tmp_dat$`ICIs drugs`,
         adaption = adaption %>% as.factor(),
         NSCLC= ifelse(adaption=='非小细胞肺癌',1,0) %>% as.factor())


         



         










tmp <- t1_f(dat = underlying_disease_original_v1,
                  vars = c( "hypertension", "diabetes", 
                           "CHD", "hepatitis","lung","underlying_yn",'ici','adaption','NSCLC'),
                  num_vars = num_vars,
                  factor_vars= c( "hypertension", "diabetes", 
                                 "CHD", "hepatitis","lung","underlying_yn",'ici','adaption','NSCLC'),
                  showlevel_p=T,
                  strata='label')



tmp <- t1_f(dat = tmp_dat$`ICIs drugs`,
            vars = c( 'ici','outcome'),
            factor_vars= c('ici'),
            showlevel_p=T,
            strata='outcome')


DT::datatable(tmp,
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

