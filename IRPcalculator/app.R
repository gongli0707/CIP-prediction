#source('code/lib_helper.R')
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(rintrojs)
library(forcats)
library(glmnet)
library(caret) ##dummyVars

body <- dashboardBody(
  
  useShinyjs(),
  introjsUI(),
  
  fluidRow(
    column(width = 10,
           introBox(data.step = 1, data.intro = 'Input your information for the IRP risk calculation in this section',
                    box(
                      
                      
                      title = "Answer the following questions", width = NULL, status = "primary",solidHeader = TRUE,
                      
                      box(
                         width = NULL, solidHeader = FALSE, status = "primary",
                        materialSwitch(
                          inputId = "NSCLC",
                          label = "Did you develop NSCLC?",
                          value = FALSE,
                          status = "primary"
                        ),
                        
                        materialSwitch(
                          inputId = "lung",
                          label = "Have you ever had lung diseases before?", 
                          value = FALSE,
                          status = "primary"
                        ),
                        
                        materialSwitch(
                          inputId = "antitumor_therapy",
                          label = "Have you ever received antitumor therapy before?", 
                          value = FALSE,
                          status = "primary"
                        ),
                     
                        selectInput(
                          inputId = "cancerstage",
                          label = "cancer stage",
                          choices = c(
                            "I",
                            
                            "II",
                            
                            "III",
                            "IV"
                          )
                          # ,
                          # options = list(
                          #   title = "This is a placeholder")
                        ),
                        
                        numericInput("temperature", "Your body temperature (℃) at your last visit ",
                                     value = 36, 
                                     min = 0, max = 100, step = 1),
                        numericInput("underlying_dis", "How many underlying disease(s) do you have?",
                                     value = 0, 
                                     min = 0, max = 100, step = 1),
                        selectInput(
                          inputId = "ICIS",
                          label = "ICIS drug used",
                          choices = c(
                            "Tirelizumab",
                            
                            "Sindillizumab",
                            "other"
                          )
                          
                        ),
                        numericInput("kps", "what is your KPS score",
                                     value = 0, 
                                     min = 0, max = 100),
                        numericInput("cd4", "Percentage of CD4+ lymphocytes at your last visit",
                                     value = 0, 
                                     min = 0, max = 100),
                        numericInput("hb", "Hemoglobin at your last visit",
                                     value = 0, 
                                     min = 0, max = 100)
                        
                      ),
                  
                      
                      box(
                        title = "Prediction", width = NULL,status = "primary",
                        uiOutput('text1'),
                        htmlOutput('graph1'),
                        
                       
                        progressBar(
                          id = "pb2",
                          value = 0,
                          total = 100,
                          title = "",
                          status = 'danger',
                          #striped = TRUE,
                          display_pct = TRUE
                        ),
                        actionButton(
                          inputId = "go",
                          label = "Launch calculation"
                        )
                      )
                      
                    )
           )
    ),
    
 
    
    
    column(width = 2,
           introBox(data.step = 3, data.intro = 'This section is a summary of our work',
                    box(
                      title = "Our work", width = NULL, solidHeader = TRUE,status = "primary",
                      uiOutput('ourwork')
                    )
           )
    )
  )
)

# We'll save it in a variable `ui` so that we can preview it in the console
ui <- dashboardPage(
  dashboardHeader(title ='ICIs-related IRP Risk Calculator'),
  dashboardSidebar(disable = TRUE),
  body
)


server <- function(input,output,session){
  
  #
  # UI - GENERAL --------------------------------------------------------------
  
  
  #show intro modal
  observeEvent("", {
    showModal(modalDialog(
      includeHTML("intro_page.html"),
      easyClose = TRUE,
      footer = tagList(
        actionButton(inputId = "intro", label = "INTRODUCTION TOUR",
                     icon = icon("info-circle"),class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$intro,{
    removeModal()
  })
  
  # show intro tour
  observeEvent(input$intro,
               introjs(session, options = list("nextLabel" = "Continue",
                                               "prevLabel" = "Previous",
                                               "doneLabel" = "OK"))
  )
  
  
  
  
  
  
  
  
  
  calculate_f <- function(
    temperature=0,
    underlying_dis=0,
    lung=0,
    ICIS="Attilizumab", 
    NSCLC=0,
    cd4 = 0,
    kps = 0,
    antitumor_therapy = 0,
    cancerstage = 0,
    hb = 0
  
    
  ){

    library(dplyr)
    library(readxl)
    
    my_model <- readRDS("glmnet_m.rds")
    testSet <- readRDS("testSet.rds")
   
    

    library(stats)

    
    
    ##input new obs: the first row
    new_obs = testSet[1,] %>% data.frame()
    
    colnames(new_obs) <- colnames(testSet)
    
 
    if(ICIS == "Tirelizumab"){
      new_obs$`Tirelizumab(yes/no)`="1"
    }else{new_obs$`Tirelizumab(yes/no)`="0"}
    
    if(ICIS == "Sindillizumab"){
      new_obs$`Sindillizumab(yes/no)`="1"
    } else{new_obs$`Sindillizumab(yes/no)`="0"}
    
    if(underlying_dis >= 2){
      new_obs$`Number of underlying disease >=2` ="1"
    }else{
      new_obs$`Number of underlying disease >=2` ="0"
    }
    
    if(lung == TRUE){
      new_obs$`History of lung diseases(yes/no)`= "1"
    }else{new_obs$`History of lung diseases(yes/no)`= "0"}
    
    
    if(NSCLC == TRUE){
      new_obs$`NSCLC(yes/no)`= "1"
    }else{new_obs$`NSCLC(yes/no)`= "0"}
    
    if(kps <= 70){
      new_obs$`KPS score <=70` ="1"
    }else{new_obs$`KPS score <=70` ="0"}
    
    if(antitumor_therapy==TRUE){
      new_obs$`History of antitumor therapy(yes/no)`="1"
    }else{new_obs$`History of antitumor therapy(yes/no)` ="0"}
    
    if(cancerstage=="IV"){
      new_obs$`Cancer stage =IV`="1"
    }else{new_obs$`Cancer stage =IV`="0"}
    
    if(hb >=130 & hb <=175){
      new_obs$Hemoglobin = "1"
    }else{new_obs$Hemoglobin = "0"}

 
    
    newdat <- rbind(testSet,new_obs)
    
    newdat$`History of lung diseases(yes/no)` = as.double(newdat$`History of lung diseases(yes/no)`)
    newdat$`NSCLC(yes/no)` = as.double(newdat$`NSCLC(yes/no)`)
    newdat$`Number of underlying disease >=2` = as.double(newdat$`Number of underlying disease >=2`)
    newdat$`Tirelizumab(yes/no)` = as.double(newdat$`Tirelizumab(yes/no)`)
    newdat$`Sindillizumab(yes/no)` = as.double(newdat$`Sindillizumab(yes/no)`)
    newdat$`KPS score <=70` = as.double(newdat$`KPS score <=70`)
    newdat$`History of antitumor therapy(yes/no)` = as.double(newdat$`History of antitumor therapy(yes/no)`)
    newdat$`Cancer stage =IV` = as.double(newdat$`Cancer stage =IV`)

    

    
    fittedTL <- predict(my_model,newdat)
    fittedProb <- predict(my_model, newdat, type = "prob")
    predictions <- fittedProb$IRP[nrow(newdat)]
    
    return(predictions)
    
  }
  
  graph_f <- function(x){
    
    danger = c(  "\U2764\UFE0F")
    initial = c("&#x1F90D;")
    total_n =27*6
    initial_graph = paste0(rep(initial,total_n),'')
    
    if(is.na(x)){
      output_graph = initial_graph
    }else{
      danger_n = rep(danger,round(x*total_n))
      tmp_n = total_n- round(x*total_n)
      nodanger_n = rep(initial,tmp_n)
      output_graph =  c(danger_n,nodanger_n)
      
    }
    
    return(output_graph)
    
  }
  

  
  observeEvent(input$go, {
    a = calculate_f(temperature=input$temperature
                    ,underlying_dis=input$underlying_dis,
                    lung=input$lung,ICIS=input$ICIS, NSCLC=input$NSCLC,
                    cd4=input$cd4,
                    kps = input$kps,
                    antitumor_therapy = input$antitumor_therapy,
                    cancerstage = input$cancerstage,
                    hb = input$hb)
    
    for (i in 1:a*100) {
      updateProgressBar(
        session = session,
        id = "pb2",
        value = a*100, total = 100, 
        title = paste("Your estimated risk is:")
      )
      # Sys.sleep(0.1)
    }
  })
  
  
  # output$text1 =  renderUI({ 
  #   pred_risk =  calculate_f(temperature=input$temperature
  #                            ,underlying_dis=input$underlying_dis,
  #                            lung=input$lung,ICIS=input$ICIS, NSCLC=input$NSCLC,
  #                            cd4=input$cd4)
  #   
  #   if(is.na(pred_risk)){
  #     
  #     pred_out =  paste0("<h4>",
  #                        "You estimated IRP risk is: ",'?',
  #                        "</h3>","<br>"
  #                        
  #                        
  #     )
  #   }else{
  #     pred_out = paste0("<h4>",
  #                       "You estimated irAE risk is: ",round(pred_risk*100,2),'%',
  #                       "</h3>","<br>"
  #     )
  #   }
  #   
  #   HTML(
  #     pred_out
  #   )
  #   
  #   
  #   
  # })
  
  
  
  
  output$mytext <- renderUI({
    calculate_f(temperature=input$temperature
                ,underlying_dis=input$underlying_dis,
                lung=input$lung,ICIS=input$ICIS, NSCLC=input$NSCLC,
                cd4=input$cd4 )
    HTML(paste0(
      "<h4>",
      "Temperature: ",input$temperature,
      "</h3>",
      
      "<h4>",
      "NSCLC: ",input$NSCLC,
      "</h3>",
      
      "<h4>",
      "Number of underlying diseases: ",input$underlying_dis,
      "</h3>",
      
      "<h4>",
      "Previous lung disease: " ,input$lung,
      "</h3>",
      
      "<h4>",
      "ICIs drugs: " ,input$ICIS,
      "</h3>",
      
      "<h4>",
      "CD4 lymphocyte count: " ,input$cd4,
      "</h3>",
      
      # "<h4>",
      # "Number of previous treatment drugs: " ,input$treat_drug,
      # "</h3>",
      # 
      # "<h4>",
      # "Percentage of NK cell: ",                 input$NK,
      # "</h3>",
      # 
      # "<h4>",
      # "Percentage of eosinophilus cells: "   ,   input$eosinophilus,
      # "</h3>",
      # 
      # "<h4>",
      # "Percentage of neutrophilic granulocyte: ", input$neutrophilic,
      # "</h3>"
      
      
      # "Annual interest rate: ", input$height, "%",
      # "<br>",
      # "Term: ", input$height, " years (", input$length * 12, " months)",
      # "<br>"
      # "<b>", "Monthly payment: ", format(round(monthPay, digits = 2), big.mark = ","), "</b>",
      # "<br>"
      # "<b>", "Total cost: ", "</b>", format(round(input$height, 2), big.mark = ","), " (principal) + ", format(round(monthPay * 12 * input$length - input$principal, 2), big.mark = ","), " (interest) = ", "<b>", format(round(monthPay * 12 * input$length, digits = 2), big.mark = ","), "</b>"
    ))
  })
  
  
  output$ourwork <- renderUI({
    HTML(paste0(
      "<h4>", "Immune checkpoint inhibitors (ICIs) are a new class of anticancer drugs that activated T-cell-mediated immune responses against tumor cell. In the past decades, therapeutically blocking inhibitory molecular included cytotoxic T-lymphocyte antigen 4 (CTLA4) inhibitors，programmed cell death 1 (PD1) inhibitors and programmed cell death 1 ligand (PD-L1) inhibitors. ",
      "</h3>",
      "<br>",
      "<h4>",
      "Trials have certified that ICIs can significantly improve the survival of some patients with advanced malignancies, and ICIs have become the first-line treatment for an increasing number of indications. The ICIs treatment can also cause immune-related adverse events (irAEs), which may be serious or fatal.",
      "</h3>",
      "<br>",
      "<h4>",
      "A systematic review and meta-analysis demonstrated that the ICIs-associated fatal irAEs were most common in pneumonitis, colitis, hepatitis, and neurologic effects. Immune-related pneumonitis is a clinically serious and potentially lethal adverse event, which accounts for 35% of anti-PD-1/PD-L1-related deaths. Establishing a safety prediction model for immune-related pneumonitis is the key to improving the outcome of tumor patients.",
      "</h3>",
      "<br>",
      "<h4>",
      "Machine Learning is a new artificial intelligence method, which has been widely used to establish prediction model[8, 9]. In this study, we will explore the risk factors of immune-related pneumonitis caused by ICIs, and establish a machine learning model to quantitatively predict the probability of immune-related pneumonitis, so as to provide a risk prediction tool for clinical practice.",
      "</h3>"
      # "Annual interest rate: ", input$height, "%",
      # "<br>",
      # "Term: ", input$height, " years (", input$length * 12, " months)",
      # "<br>"
      # "<b>", "Monthly payment: ", format(round(monthPay, digits = 2), big.mark = ","), "</b>",
      # "<br>"
      # "<b>", "Total cost: ", "</b>", format(round(input$height, 2), big.mark = ","), " (principal) + ", format(round(monthPay * 12 * input$length - input$principal, 2), big.mark = ","), " (interest) = ", "<b>", format(round(monthPay * 12 * input$length, digits = 2), big.mark = ","), "</b>"
    ))
  })
  
}



# Preview the UI in the console
shinyApp(ui = ui, server = server)