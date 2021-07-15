#Install these packages to run this Shiny App
#install.packages("shiny")
#install.packages("jsonlite")
#install.packages("rms")
#install.packages("nlme")
#install.packages("lm.beta")
#install.packages("sensitivity")
#install.packages("meta")
#install.packages("rpart")
#install.packages("coxme")

library(shiny)
library(jsonlite)
library(rms)
library(nlme)
library(lm.beta)
library(sensitivity)
library(meta)
library(rpart)
library(coxme)
#Surival package data: cancer colon diabetic flchain heart mgus nafld1 pbc transplant   
data(lungcancer)
options(shiny.maxRequestSize=1000*1024^2)    #This will increase the shiny file upload limit from current 5MB max
options(scipen=10)                                                               #General option to see low decimals

####################################

shinyServer(
  
  
  function(input, output) {
    
    #Reactives values  
    values <- reactiveValues()
    
    df <- reactive({                  #This indicates the data frame I will use.
            if ( input$UseText == "No") {
              get(input$dataframe)  
            }  else {
              dataInput_txt()
            }
    })

    #DELETE THIS        
#    default_df_name <- reactive({  
    #      if ( input$UseText == "No") {
        #        "mtcars"  
    #      }  else {
    #        "text_file"
    #      }
    #    })
    
    atch <- reactive({                  #Used to attach the data frame. 
      attach(df())      #I can only attach within a function...I think
    })

################################################################################    
#                       Download/Upload/Save data                              #
################################################################################    

    #This is the new data from this question: 4. Do you want to create a new data frame?
    #Use this as a summary to confirm new data
    new_smry_df <- reactive({     
      if(input$create_new_df == "Yes") {
      str(newdf())  
      }
    })
    
    #Don't need this for "Data" tab
    output$new_smry_df <- renderPrint({
      if(input$create_new_df == "Yes") {
        new_smry_df()
      }
    })
    
    #Use
    output$use_txt <- renderUI({
      radioButtons("UseText", "3. Analyze/Save the text file?", 
                   choices= c("No", "Yes"), selected="No")     
    })

    #The new data frame name for the text file
    output$new_df_name <- renderUI({ 
      textInput("NewDfName", "4. Enter the new data frame name", 
                value= "new")     
    })
    
    #The new data frame name for the transformed/imputed and some original
    output$new_df_name_all <- renderUI({ 
      textInput("NewDfNameAll", "1. Enter the new name for transformed/imputed data", 
                value= "transformed")     
    })

    #The new data frame name for the transformed/imputed/factor scores/some original
    output$new_df_name_all_fs <- renderUI({ 
      textInput("NewDfNameAllFs", "1. Enter the new name for transformed/imputed/factor data", 
                value= "factor")     
    })

#Modify the dataset
    #Change the data type
    #Character
    output$modify_character <- renderUI({                                 #Same idea as output$vy
      selectInput("ModifyCharacter", "1. Select variables to convert to a 'character'.", 
                  choices = var(), multiple=TRUE)
    })
    #Factor
    output$modify_factor <- renderUI({                                 #Same idea as output$vy
      selectInput("ModifyFactor", "2. Select variables to convert to a 'factor'.", 
                  choices = var(), multiple=TRUE)
    })
    #Numeric
    output$modify_numeric <- renderUI({                                 
      selectInput("ModifyNumeric", "3. Select variables to convert to a 'numeric'.", 
                  choices = var(), multiple=TRUE)
    })

    ## Modify the dataset ##
    #Subset the dataset
    output$subset_df_yes_no <- renderUI({                                 
      selectInput("SubsetYesNo", "4. Want to subset the data?", 
                  choices = c("No", "Yes"), multiple=FALSE, selected="No")
    })
    #Formula for the subset
    output$subset_args <- renderUI({ 
      textInput("SubsetArgs", "5. Enter the formula to subset data.", 
                value= "subset= , select=")     
    })
    #Modify the dataset
    output$modify_df_yes_no <- renderUI({                                 
      selectInput("ModifyDfYesNo", "6. Want to create the modified dataset?", 
                  choices = c("No", "Yes"), multiple=FALSE, selected="No")
    })

    #####################
    # Save modified data #
    #####################
    output$modified_df_save <- renderUI({  
      selectInput("ModifiedDfSave", "7. Save the modified data?", 
                  choices = c("No", "Yes"), multiple=FALSE, selected="No")     #Will make choices based on my reactive function.
    })
    
    ## Subset the dataset ##
    modifySubsetFnc <- function(df, ModifyDfYesNo, SubsetArgs, SubsetYesNo) {
        if (SubsetYesNo == "Yes") {
          df_mod <- eval(parse(text=paste0("subset(df,", SubsetArgs, ")" )))
        } else {
          df_mod <- df
      }
      return(df_mod)
    }
    #Runs the function above
    modifySubsetDf <- reactive({
      if(input$ModifyDfYesNo == "Yes") {
        modifySubsetFnc(df=df(), ModifyDfYesNo=input$ModifyDfYesNo, 
                      SubsetArgs=input$SubsetArgs, SubsetYesNo=input$SubsetYesNo)
      }
    })
## Modify the variable type of data   
    #Character
    modifiedCharFnc <- function(df, ModifyCharacter) {
      df_mod <- df
        if (!is.null(ModifyCharacter)) {
          df_mod[, which(colnames(df_mod) %in% ModifyCharacter)] <- sapply(df_mod[which(colnames(df_mod) %in% ModifyCharacter)], as.character)
      }
      return(df_mod[, which(colnames(df_mod) %in% ModifyCharacter), drop = FALSE])
    }
    #Runs the function above
    modifiedCharDf <- reactive({
      if(input$ModifyDfYesNo == "Yes") {
        modifiedCharFnc(df=modifySubsetDf(), ModifyCharacter=input$ModifyCharacter)
      }
    })
    #Factor
    modifiedFacFnc <- function(df, ModifyFactor) {
      df_mod <- df
        if (!is.null(ModifyFactor)) {
        df_mod[, which(colnames(df_mod) %in% ModifyFactor)] <- sapply(df_mod[which(colnames(df_mod) %in% ModifyFactor)], as.character)
      }
      if (!is.null(ModifyFactor)) {
        df_mod[, which(colnames(df_mod) %in% ModifyFactor)] <- lapply(df_mod[which(colnames(df_mod) %in% ModifyFactor)], as.factor)
      }
      return(df_mod[, which(colnames(df_mod) %in% ModifyFactor), drop = FALSE])
    }
    #Runs the function above
    modifiedFacDf <- reactive({
      if(input$ModifyDfYesNo == "Yes") {
        modifiedFacFnc(df=modifySubsetDf(), ModifyFactor=input$ModifyFactor)
      }
    })
    #Numeric
    modifiedNumFnc <- function(df, ModifyNumeric) {
      df_mod <- df
        if (!is.null(ModifyNumeric)) {
        df_mod[, which(colnames(df_mod) %in% ModifyNumeric),drop = FALSE] <- sapply(df_mod[which(colnames(df_mod) %in% ModifyNumeric)], as.numeric)
      }
      return(df_mod[, which(colnames(df_mod) %in% ModifyNumeric) ,drop = FALSE])
    }
    #Runs the function above
    modifiedNumDf <- reactive({
      if(input$ModifyDfYesNo == "Yes") {
      modifiedNumFnc(df=modifySubsetDf(), ModifyNumeric=input$ModifyNumeric)
      }
    })
    
    #Variables that are not modified
    non_modified_vars <- reactive({
      if(input$ModifyDfYesNo == "Yes") {
        setdiff(var(),  c(input$ModifyCharacter,input$ModifyFactor, input$ModifyNumeric))
      }
    })
    
    #Create modified dataset
    modifiedDf <- reactive({
      if(input$ModifiedDfSave == "Yes") {
        cbind(modifySubsetDf()[, which(colnames(modifySubsetDf()) %in% non_modified_vars()), drop = FALSE], 
              modifiedCharDf(), modifiedFacDf(), modifiedNumDf()) 
      }
    })

    output$modified_df_name <- renderUI({ 
      textInput("ModifiedDfName", "8. Enter the data frame name.", 
                value= "mod_df")     
    })
    output$download_modified_df <- downloadHandler(
      filename = "modified_df.RData",
      content = function(con) {
        assign(input$ModifiedDfName, modifiedDf())
        save(list=input$ModifiedDfName, file=con)
      }
    )
    
    #################################################
    #New data from the text file
    newdf <- reactive({
      if(input$create_new_df == "Yes") {
        df()
      }
    })
    
    #This reactive function identifies which predictors not used in the transformation/imputation will be saved
    keep_some <- reactive({
      setdiff(var(), ptrns_trnfd_vls())
    })
    
    #NEW DF function for transformed and imputed data
    newDFfnc <- function(questn, keeps, df1, df2, df3) {
      if(questn == "Yes") {
        df <- cbind(
          df1[, keeps], df2, df3) 
      }
      colnames(df) <- c(colnames(df1[, keeps]), paste0(colnames(df2), "_i"), paste0(colnames(df3), "_t"))
      return(df)
    }

        
    #Runs the above function, creates transformed and imputed data 
    newdf_all <- reactive({
      if(input$create_new_df_all == "Yes") {
        newDFfnc(questn=input$create_new_df_all, keeps=keep_some(), df1=df(), df2=imputed(), df3=as.data.frame(ptrans()[["transformed"]]))
      }
    })
    

    #NEW DF function for transformed/imputed and factor scores data
    newDFfnc_fs <- function(questn, keeps, df1, df2, df3, df4) {
      if(questn == "Yes") {
        df <- cbind(
          df1[, keeps], df2, df3, df4) 
      }
      colnames(df) <- c(colnames(df1[, keeps]), paste0(colnames(df2), "_i"), paste0(colnames(df3), "_t"), colnames(df4))
      return(df)
    }

    #Select variables, transformed, imputed 
    newdf_all_fs <- reactive({
      if(input$create_new_df_all_fs == "Yes") {
        newDFfnc_fs(questn=input$create_new_df_all_fs, keeps=keep_some(), df1=df(), df2=imputed(), df3=as.data.frame(ptrans()[["transformed"]]), df4=pca_fac_df())
      }
    })
    

    ######  Upload  #####
    #1. View the data frame name
    output$View_main_df <- renderUI({ 
      selectInput("viewMainDF", "Want to view the top of the model data?", 
                  choices = c("No", "Yes"), multiple=FALSE, selected="No")
    })
    #1A. Object with transition names
    view_main_data_yesno <- reactive({
      input$viewMainDF
    })
    #2. This prints the main data
    output$view_main_data_out <- renderTable({
      if(view_main_data_yesno() == "Yes") {
        head(df(), 10)
      }
    }, rownames = TRUE)
    
    #Text files    
    output$text_file_type <- renderUI({
      radioButtons(inputId="TextFile", label="1. What is the text file type?", choices = c("Comma", "Tab"))
    })
    
    #Type of text file, identifies the type of seperator I will use
    sep_type <- reactive({
      if(input$TextFile == "Comma") {
        ","
      } else {
        "\t"
      }
    })
    
    output$upload_df <- renderUI({
      fileInput("file1", "2. Choose a text File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      )
    })
    
    #Text file data
    dataInput_txt <- reactive({
      sessionEnvir <- sys.frame()
      file1 <- input$file1
      if (!is.null(input$file1)) read.csv(file=file1$datapath, header = TRUE, sep= sep_type())
    })
    
    #This gives the data structure of the text file
    output$datastr_txt <- renderPrint({
      if (is.null(dataInput_txt()))  return()  else str(dataInput_txt())
    })

    #R data file upload
    output$upload_r_df <- renderUI({
      fileInput('f1', 'Choose an RData File', accept=c('.RData'))
    })

    #Loads R file
    dataInput <- reactive({
      sessionEnvir <- sys.frame()
      if (!is.null(input$f1)) load(input$f1$datapath, sessionEnvir)
    })
    
    #Data structure of the R file
    output$datastr <- renderPrint({
      if (is.null(dataInput()))  return()  else str(dataInput())
    })
    
    ######  Downaload  #####
    output$downloadSave <- downloadHandler(
      filename = "Rdata.RData",
      content = function(con) {
        assign(input$NewDfName, newdf())
        save(list=input$NewDfName, file=con)
      }
    )

    #R data file upload of imputed, transformed and some original data
    output$downloadSaveAll <- downloadHandler(
      filename = "Rdata.RData",
      content = function(con) {
        assign(input$NewDfNameAll, newdf_all())
        save(list=input$NewDfNameAll, file=con)
      }
    )

    #R data file upload of imputed, transformed, factor scores, and some original data
    output$downloadSaveAllFs <- downloadHandler(
      filename = "Rdata.RData",
      content = function(con) {
        assign(input$NewDfNameAllFs, newdf_all_fs())
        save(list=input$NewDfNameAllFs, file=con)
      }
    )
    
    #Indicate the data object name to download as an R workspace Rdata file.
#    output$df_save_nm <- renderUI({                                 
#      textInput("name", label="6. Dataset object name to save in Rdata file:", value =input$NewDfName)     
#    })
    
    #Indicate the data object name to download as an R workspace Rdata with transformed/imputed and original data.
 #   output$df_save_nm_all <- renderUI({                                 
#      textInput("name_all", label="2. Dataset object name to save in Rdata file:", value =input$NewDfNameAll)     
#    })

    #Indicate the data object name to download as an R workspace Rdata with transformed/imputed/factor and original data.
#    output$df_save_nm_all_fs <- renderUI({                                 
#      textInput("name_all_fs", label="2. Dataset object name to save in Rdata file:", value =input$NewDfNameAllFs)     
#    })
    
    #################################################
    #From text file
    output$new_df <- renderUI({                                 #Same idea as output$vy
      selectInput("create_new_df", "5. Do you want to create a new data frame?", 
                  choices = c("No", "Yes"), multiple=FALSE, selected="No")
    })
    
    #From transformed/imputed/factor and original data
#    output$new_df_all <- renderUI({                                 #Same idea as output$vy
#      selectInput("create_new_df_all", "5. Do you want to create a new data frame?", 
#                  choices = c("No", "Yes"), 
#                  multiple=FALSE, selected="No")
#    })
    
    #Render function that lists whether I want to download the transformed/Imputed data
    output$new_df_all <- renderUI({
      radioButtons("create_new_df_all", label="2. Do you want to create a new data frame of Transformed/Imputed data?", 
                   choices = c("No", "Yes"), selected="No")
    })
    #Render function that lists whether I want to download the factor scores with the transformed/Imputed data
    output$new_df_all_fs <- renderUI({
      radioButtons("create_new_df_all_fs", label="2. Do you want to create a new data frame of Factor Scores with Trans/Imputed?", 
                   choices = c("No", "Yes"), selected="No")
    })

    output$factor_score_output <- renderPrint({ 
      summary(pca_fac_df())
    })
    
    
################################################################################    
################################################################################    
    
    outcome <- reactive({                  #Outcome is my reactive function name I will use below. 
      input$variableY                      #variableY comes from the UI file drop down box.
    })

    predictor <- reactive({             #Same idea as "outcome" 
      input$variableX  
    })
    
    other_cov <- reactive({             #Same idea as "outcome" 
      setdiff(var(),c(outcome(), predictor()))
    })
    
    gls_lev1_2 <- reactive({             #Same idea as "outcome" 
      setdiff(var(),outcome())
    })

    splines <- reactive({             #Spline terms 
      if (input$rcsy == "Yes") {
        input$rcs_X  
      }  
    })
    
    non_spline <- reactive({             #Linear or non-spline terms
      if (input$rcsy == "Yes") {
        setdiff(input$variableX,input$rcs_X)  
      }  else {
        input$variableX
      }
    })
    
    spline_rcs <- reactive({             #Spline terms 
      if (input$rcsy == "Yes") {
      paste0("rcs(", substr(splines(), -1, nchar(splines()) + 1), ", 5)")
      }
    })
    
    mdl_vnms <- reactive({             #Model variables that has RCS included.
      c(spline_rcs(), non_spline())
      })
    
    mdl_fmla <<- reactive({             #Spline terms 
      as.formula(paste(paste0(input$variableY , "~"),   
                       paste(mdl_vnms(), collapse= "+")))
    })

#Cox models    
    cox_mdl_fmla1 <- reactive({             #Spline terms 
      as.formula(paste(paste0("Surv(",input$variableY,")" , "~"),   
                       paste(mdl_vnms(), collapse= "+")))
    })

    cox_mdl_fmla1u <- reactive({             #Spline terms 
      as.formula(paste(paste0("Surv(",input$variableY,")" , "~"),   
                       paste(strsplit(input$up_fmla, "~")[[1]][2], collapse= "+")))
    })

    cox_mdl_fmla2 <- reactive({             #Spline terms 
      as.formula(paste(paste0("Surv(",input$variableY, ",", censor1(), ")", "~"),   
                       paste(mdl_vnms(), collapse= "+")))
    })

    cox_mdl_fmla2u <- reactive({             #Spline terms 
      if (length(input$variableX) ==1) {
        as.formula(paste(paste0("Surv(",input$variableY, ",", censor1(), ")", "~"),   
                         paste(strsplit(input$up_fmla, "~")[[1]][2])))
      } else {
        as.formula(paste(paste0("Surv(",input$variableY, ",", censor1(), ")", "~"),   
                         paste(strsplit(input$up_fmla, "~")[[1]][2], collapse= "+")))
      }
    })
    
    #AFT models
    aft_mdl_fmla1 <- reactive({             #Spline terms 
      as.formula(paste(paste0("Surv(",input$variableY,")" , "~"),   
                       paste(mdl_vnms(), collapse= "+")))
    })
    
    aft_mdl_fmla1u <- reactive({             #Spline terms 
      as.formula(paste(paste0("Surv(",input$variableY,")" , "~"),   
                       paste(strsplit(input$up_fmla, "~")[[1]][2], collapse= "+")))
    })
    
    aft_mdl_fmla2 <- reactive({             #Spline terms 
      as.formula(paste(paste0("Surv(",input$variableY, ",", censor1(), ")", "~"),   
                       paste(mdl_vnms(), collapse= "+")))
    })
    
    aft_mdl_fmla2u <- reactive({             #Spline terms 
      if (length(input$variableX) ==1) {
        as.formula(paste(paste0("Surv(",input$variableY, ",", censor1(), ")", "~"),   
                         paste(strsplit(input$up_fmla, "~")[[1]][2])))
      } else {
        as.formula(paste(paste0("Surv(",input$variableY, ",", censor1(), ")", "~"),   
                         paste(strsplit(input$up_fmla, "~")[[1]][2], collapse= "+")))
      }
    })
    
    gls_cor <- reactive({
      as.formula(paste(paste0("~"),   
                       paste(gls_clst1(), collapse= "|")))
    })

    var <- reactive({                  #I use this to get the variable names from the data frame. 
      names(df())  
    })  
    

    output$outcome_hist <- renderPlot({
      if (class(df()[,outcome()]) %in%  c("numeric","integer","labelled")) {
        hist(df()[,outcome()], main="Histogram of the outcome variable",
             xlab=paste0(input$variableY))
      }
    })

##
    #Function to convert predicted scores to scores on raw scale or as proportions or leave as the same
    yhat_hist_plot_fnc <- function(yhat, reg_yhat) {
      switch(reg_yhat,                
             "Linear"   = plot_yhat <- yhat, 
             "Logistic" = plot_yhat <- yhat,
             "Ordinal Logistic"  = plot_yhat <- yhat,
             "Poisson"  = plot_yhat <- (yhat),
             "Quantile" = plot_yhat <- yhat,
             "Cox PH"   = plot_yhat <- yhat,
             "Cox PH with censoring"     = plot_yhat <- yhat,
             "AFT"   = plot_yhat <- yhat,
             "AFT with censoring"     = plot_yhat <- yhat,
             "Generalized Least Squares" = plot_yhat <- yhat )
      return(plot_yhat)
    }
    
    #This reactive function runs the yhat_plot_fnc function above  
    yhat_hist_rslt <- reactive ({
      #if (input$begin_mdl == "Yes") {
        try( yhat_hist_plot_fnc(yhat=predict(fit1()), reg_yhat=input$regress_type) )
      #}
    })
    
    
    output$y_hat_hist <- renderPlot({
#      if (input$begin_mdl == "Yes") {
      try( hist(yhat_hist_rslt(), main="Histogram of the predicted Y values",
                        xlab=paste0(input$variableY, " (range: ", round(min(yhat_hist_rslt(), na.rm=T),3), " to ", round(max(yhat_hist_rslt(), na.rm=T), 3), ". Unique values= ", length(unique(yhat_hist_rslt())) ,".)" )) )
#      }
    })
    
    ########### Model approximation section ###########        
    output$MIForAprx <- renderUI({  
      selectInput("MI_for_aprx", "1. Did you do Multiple Imputation?", 
                  choices = c("No", "Yes"), multiple=FALSE, selected="No")     
    })
    
    ## Reactive functions for model approximation ##
    #1. Get yhat values
    #Function that creates data frame for approximation
    apprx_df_fnc <- function(adf, x, y, fit) {
      adf <- adf[complete.cases(adf[, c(y, x)]),] #Added y in case there are NAs for the outcome
      df <- data.frame(predict(fit)[!is.na(predict(fit))], adf[, x, drop=FALSE]) #drop=FALSE allows me to have 1 variable and keep the column name
      colnames(df)[1] <-  y
      return(df)
    }
    
    #Runs the above function
    apprx_df_og <- reactive({
      if (input$MI_for_aprx == "No") {
        apprx_df_fnc(adf=df(), x=predictor(), y=input$variableY, fit=fit1())
      }
    })
    
    apprx_df_si <- reactive({
      if (input$MI_for_aprx == "Yes") {
        apprx_df_fnc(adf=new_imputed.si(), x=predictor(), y=input$variableY, fit=fit.si())
      }
    })
    
    #Creates a formula for the model approximation
    aprx_mdl_fmla <<- reactive({             #Spline terms 
      as.formula(paste(paste0(input$variableY , "~"),   
                       paste(mdl_vnms(), collapse= "+")))
    })
    
    #Runs the approximated model using the linear predictor as the outcome
    apprx_fit1 <- reactive({
      
      if (input$MI_for_aprx == "No") {
        
        if(input$updy == "Yes") {
          ols(as.formula(input$up_fmla), sigma=1, data=apprx_df_og()) 
        } else {
          ols(mdl_fmla(), sigma=1, data=apprx_df_og()) 
        }
        
      }
      
#      if (input$MI_for_aprx == "Yes") {
        
#        if(input$updy == "Yes") {
#          ols(as.formula(input$up_fmla), sigma=1, data=apprx_df_si()) 
#        } else {
#          ols(mdl_fmla(), sigma=1, data=apprx_df_si())
#        }
        
#      }
      
    })
    
    #Single imputed data
    apprx_fit1_si <- reactive({
      
            if (input$MI_for_aprx == "Yes") {
      
              if(input$updy == "Yes") {
                ols(as.formula(input$up_fmla), sigma=1, data=apprx_df_si()) 
              } else {
                ols(mdl_fmla(), sigma=1, data=apprx_df_si())
              }
      
            }
      
    })
    
    
        #Function that approximates the models between the actual and approximated predicted scores  
        ############        
        aprx_mdl_fnc <- function(a, f, lp) {
          lp <- lp[!is.na(lp)]
          s <- fastbw(a, aics=10000000)  #MAKE THIS CUTOFF HIGHER IF NOT ALL x'S SHOW UP
          betas <- s$Coefficients   # matrix, rows=iterations
          X     <- cbind(1, f$x)    # design matrix
          # Compute the series of approximations to lp
          ap <- X %*% t(betas)
          # For each approx. compute approximation R^2 and ratio of likelihood 
          #ratio chi-square for approximate model to that of original model
          m <- ncol(ap) - 1   # all but intercept-only model
          #          r2 <- frac <- numeric(m)
          r2 <- numeric(m)
          for(i in 1:m) {
            lpa <- ap[,i]
            r2[i] <- cor(lpa, lp)^2
            #Summary table of results
          }
          apx_rslt <- data.frame(Index=1:length(rownames(s[[1]])), X=rownames(s[[1]]), c(r2, 0))
          colnames(apx_rslt) <- c("Index", "Deleted X", "Remaining R2")
          return(list(r2=r2, apx_rslt=apx_rslt))
        }
        
        #Runs the aprx_mdl_fnc function above        
        apx_mdl <- reactive({
          if (input$MI_for_aprx == "No") {
            aprx_mdl_fnc(a=apprx_fit1(), f=fit1(), lp=predict(fit1()))
          } else {
            aprx_mdl_fnc(a=apprx_fit1_si(), f=fit.si(), lp=predict(fit.si()))
          }
          
        })
        
        #Function that plots the model approximation
        aprx_plot_fnc <- function(r2) {
          plot(r2, r2, type='p', col= "red", cex=3, pch=20,
               xlim= c(min(r2)*0.98, max(r2)*1.02), ylim= c(min(r2)*0.98, max(r2)*1.02), 
               main="Approximate model predictability after deletion of N predictors",
               xlab=expression(paste('Approximation ', R^2)),
               ylab=expression(paste('Approximation ', R^2)))
          text(r2, r2*.98, 1:length(r2), cex=2)
          abline(h=.95, col=gray(.83)); abline(v=.95, col=gray(.83))
          abline(h=.90, col=gray(.83)); abline(v=.90, col=gray(.83))
          abline(h=.85, col=gray(.83)); abline(v=.85, col=gray(.83))
          abline(h=.80, col=gray(.83)); abline(v=.80, col=gray(.83))
          abline(a=0, b=1, col=gray(.83))
        }

        #Runs the aprx_plot_fnc function above        
        aprx_plot <- reactive({
          aprx_plot_fnc(r2=apx_mdl()[["r2"]])
        })

        #Creates plot output that can be accessed in the UI 
        output$approximate_plot <- renderPlot({
          aprx_plot()[["r2"]]  
        })
        
        #Creates text output that can be accessed in the UI 
        output$approximate_print <- renderDataTable({
          apx_mdl()[["apx_rslt"]]  
        })
        
        #################  Get model approximation and download ###########
        #Default model formula.
        aprx_mdl_fmla2 <- reactive({ 
          as.formula(input$up_fmla)
        })
        
        #Update the model formula.
        output$aprx_uf <- renderUI({                                 #Same idea as output$vy
          textInput("aprx_up_fmla", "1. Define the approximated model.", 
                    value= deparse(aprx_mdl_fmla2(), width.cutoff=500 ))     #Will make choices based on my reactive function.
        })
        
        #Run an approximated model.
        output$aprx_mdl_yes <- renderUI({                                 #Same idea as output$vy
          selectInput("AprxMdlYes", "2. Do you want to appromximate the model?", 
                      choices = c("No", "Yes"), multiple=FALSE, selected="No")     
        })
        
        #The new data frame name for the model fit
        output$aprx_mdl_fit_name <- renderUI({ 
          textInput("AprxMdlFitName", "3. Enter the approximated model fit name.", 
                    value= "apr_mdl_fit")     
        })
        
        #Runs the approximated model using the linear predictor as the outcome
        apprx_fit2 <- reactive({
          if (input$MI_for_aprx == "Yes") {
          
          if(input$AprxMdlYes == "Yes") {
            ols(as.formula(input$aprx_up_fmla), sigma=1, data=apprx_df_si()) 
          } else {
            ols(as.formula(input$up_fmla), sigma=1, data=apprx_df_si())
          }  
            
          }  
          else {
            if(input$AprxMdlYes == "Yes") {
              ols(as.formula(input$aprx_up_fmla), sigma=1, data=apprx_df_og()) 
            } else {
              ols(as.formula(input$up_fmla), sigma=1, data=apprx_df_og())
            }
            
          }
          
        })
        
        #Output of the approximated model
        output$apprx_fit_print <- renderPrint({                                                 
          print(apprx_fit2())  #Summary of model fit.
          #          str(apprx_df())
        })
        
        
        output$aprox_model <- downloadHandler(
          filename = "aprox_model_fit.RData",
          content = function(con) {
            assign(input$AprxMdlFitName, apprx_fit2())
            save(list=input$AprxMdlFitName, file=con)
          }
        )
#################
###########        
    
        fit1 <<- reactive({                  #This stores my regression fit for use below.
          
          dd_df <<- datadist(df()); options(datadist='dd_df');
          
          if (input$begin_mdl == "Yes") {
            
            switch(input$regress_type,                #"var" and can be used anywhere in server.r.
                   "Linear"   = if(input$updy == "Yes") {
                     ols(as.formula(input$up_fmla), x=TRUE, y=TRUE, data=df())
                   } else {
                     ols(mdl_fmla(), x=TRUE, y=TRUE, data=df())},
                   "Logistic" = if(input$updy == "Yes") {
                     lrm(as.formula(input$up_fmla), x=TRUE, y=TRUE, data=df(), tol=1e-100, maxit=20) #I added tol value so it can handle time predictor (YYMM)
                   } else { #Added maxit to handle small samples. See https://stat.ethz.ch/pipermail/r-help/2017-September/449115.html  
                     lrm(mdl_fmla(), x=TRUE, y=TRUE, data=df(), tol=1e-100, maxit=20)},  #I added tol value so it can handle time predictor that causes "singularity"
                   "Ordinal Logistic" = if(input$updy == "Yes") {
                     orm(as.formula(input$up_fmla), x=TRUE, y=TRUE, data=df())
                   } else {
                     orm(mdl_fmla(), x=TRUE, y=TRUE, data=df())},
                   "Poisson" = if(input$updy == "Yes") {
                     Glm(as.formula(input$up_fmla), x=TRUE, y=TRUE, data=df(), family=poisson())
                   } else {
                     Glm(mdl_fmla(), x=TRUE, y=TRUE, data=df(), family=poisson())},
                   "Quantile" = if(input$updy == "Yes") {
                     Rq(as.formula(input$up_fmla), x=TRUE, y=TRUE, data=df(), tau=as.numeric(rq_tau1()))
                   } else {
                     Rq(mdl_fmla(), x=TRUE, y=TRUE, data=df(), tau=as.numeric(rq_tau1()))},
                   "Cox PH"   = if(input$updy == "Yes") {
                     cph(cox_mdl_fmla1u(), x=TRUE, y=TRUE, data=df(), surv=TRUE)
                   } else {
                     cph(cox_mdl_fmla1(), x=TRUE, y=TRUE, data=df(), surv=TRUE)},
                   "Cox PH with censoring" = if(input$updy == "Yes") {
                     cph(cox_mdl_fmla2u(), x=TRUE, y=TRUE, data=df(), surv=TRUE)
                   } else {
                     cph(cox_mdl_fmla2(), x=TRUE, y=TRUE, data=df(), surv=TRUE)},
                   #AFT models
                   "AFT"   = if(input$updy == "Yes") {
                     psm(aft_mdl_fmla1u(), data=df(), x=TRUE, y=TRUE, dist=AFT_PSM_Dist())
                   } else {
                     psm(aft_mdl_fmla1(), data=df(), x=TRUE, y=TRUE, dist=AFT_PSM_Dist())},
                   "AFT with censoring" = if(input$updy == "Yes") {
                     psm(aft_mdl_fmla2u(), data=df(),  x=TRUE, y=TRUE, dist=AFT_PSM_Dist())
                   } else {
                     psm(aft_mdl_fmla2(), x=TRUE, y=TRUE, data=df(), dist=AFT_PSM_Dist())},
                   "Generalized Least Squares" = if(input$updy == "Yes") {
                     Gls(as.formula(input$up_fmla), x=TRUE, data=df(), correlation=corCAR1(form= gls_cor()))
                   } else {
                     Gls(mdl_fmla(), x=TRUE, data=df(),
                         correlation=corCAR1(form= gls_cor()))}
                   #correlation= do.call("corCAR1", list(form=~week|uid)) )})
                   #as.formula(paste(paste0("~"), paste(gls_clst1(), collapse= "|")))
            )
          } else {
            
            #        if (input$MIbegin == "Yes") {
            
            switch(input$regress_type,
                   "Linear"   = if(input$updy == "Yes") {
                     fit.mult.impute(as.formula(input$up_fmla), ols, mi(), data=df(), pr=FALSE)
                   } else {
                     fit.mult.impute(mdl_fmla(), ols, mi(), data=df(), pr=FALSE)},
                   "Logistic" = if(input$updy == "Yes") {
                     fit.mult.impute(as.formula(input$up_fmla), lrm, mi(), data=df(), pr=FALSE, tol=1e-100)
                   } else {
                     fit.mult.impute(mdl_fmla(), lrm, mi(), data=df(), pr=FALSE)},
                   "Ordinal Logistic" = if(input$updy == "Yes") {
                     fit.mult.impute(as.formula(input$up_fmla), orm, mi(), data=df(), pr=FALSE)
                   } else {
                     fit.mult.impute(mdl_fmla(), orm, mi(), data=df(), pr=FALSE)},
                   "Poisson" = if(input$updy == "Yes") {
                     fit.mult.impute(as.formula(input$up_fmla), Glm, mi(), data=df(), pr=FALSE, family=poisson())
                   } else {
                     fit.mult.impute(mdl_fmla(), Glm, mi(), data=df(), pr=FALSE)},
                   "Quantile" = if(input$updy == "Yes") {
                     fit.mult.impute(as.formula(input$up_fmla), Rq, mi(), data=df(), pr=FALSE, tau=as.numeric(rq_tau1()))
                   } else {
                     fit.mult.impute(mdl_fmla(), Rq, mi(), data=df(), pr=FALSE, tau=as.numeric(rq_tau1()))},
                   "Cox PH"   = if(input$updy == "Yes") {
                     fit.mult.impute(cox_mdl_fmla1u(), cph, mi(), data=df(), pr=FALSE, surv=TRUE)
                   } else {
                     fit.mult.impute(cox_mdl_fmla1(), cph, mi(), data=df(), pr=FALSE, surv=TRUE)},
                   "Cox PH with censoring" = if(input$updy == "Yes") {
                     fit.mult.impute(cox_mdl_fmla2u(), cph, mi(), data=df(), pr=FALSE, surv=TRUE)
                   } else {
                     fit.mult.impute(cox_mdl_fmla2(), cph, mi(), data=df(), pr=FALSE, surv=TRUE)},
                   #AFT models
                   "AFT"   = if(input$updy == "Yes") {
                     fit.mult.impute(aft_mdl_fmla1u(), psm, mi(), data=df(), pr=FALSE)
                   } else {
                     fit.mult.impute(aft_mdl_fmla1(), psm, mi(), data=df(), pr=FALSE)},
                   "AFT with censoring" = if(input$updy == "Yes") {
                     fit.mult.impute(aft_mdl_fmla2u(), psm, mi(), data=df(), pr=FALSE)
                   } else {
                     fit.mult.impute(aft_mdl_fmla2(), psm, mi(), data=df(), pr=FALSE)},
                   "Generalized Least Squares" = if(input$updy == "Yes") {
                     fit.mult.impute(cox_mdl_fmla2u(), Gls, mi(), data=df(), pr=FALSE, correlation=corCAR1(form= gls_cor()))
                   } else {
                     fit.mult.impute(cox_mdl_fmla2(), Gls, mi(), data=df(), pr=FALSE, correlation=corCAR1(form= gls_cor()))}
            )
          }
          
        })

    rq_tau1 <- reactive({                  #This stores the quantile regression value. 
      input$tau1
    })
    
    gls_clst1 <- reactive({                  #This stores the GLS cluster variable. 
      input$gls_clst
    })
    
    censor1 <- reactive({                  #This stores the censoring variable. 
      input$censor
    })
    
    censor2 <- reactive({                  #This abbreviates the censoring variable if it uses a formula. 
      #Create censor variable using function below
    if(input$regress_type %in% c("Cox PH with censoring", "AFT with censoring")) {
      abbrv_censor(censor= censor1() )
    } else {
      NA
    }
    })
    
    #Function that gives the censor variable even using a formula for the value
    abbrv_censor <- function(censor) {
      #Symbols used for censor variable
      symb <- c(" ", "!", "\\+", ">", "<" )
      #String split, grab first element as
      rslt <- list()
      for (i in 1:length(symb)) {
        rslt[i] <- strsplit(censor, symb[i] )
      }
      #Select correct censor variable
      if(all(sapply(rslt,length) == 1) ==TRUE) {
        newtc <- censor
      } else {
        newtc <- rslt[which(sapply(rslt,length) > 1)][[1]][1]
      }
      return("Abbreviated.Censor"=newtc)
    }


    #  Model  builder tab  #
    ## Outputs that will show up in UI.R file and in GUI ##

    #Determine if we should begin modeling.
    output$BeginModel <- renderUI({  
      selectInput("begin_mdl", "2. Begin modeling?", 
                  choices = c("No", "Yes"), multiple=FALSE, selected="No")     #Will make choices based on my reactive function.
    })
    
    #Select the outcome
    output$vy <- renderUI({                                #Creates a UI function here but it will
      selectInput("variableY", "3. Select the outcome",       #get called to the UI file.
                  choices = var(), multiple=FALSE, selected=var()[1] )   #Will make choices based on my reactive function.
    })

    #Select the predictors.
    output$vx <- renderUI({                                 #Same idea as output$vy
      selectInput("variableX", "4. Select the predictors", 
                  choices = setdiff(var(), outcome()), multiple=TRUE, selected=var()[2])     #Will make choices based on my reactive function.
    })

    #Selects the regression type.
    output$reg_typ <- renderUI({                                #Creates a UI function here but it will
      selectInput("regress_type", "5. Select the regression method",
                  choices = c("Linear", 
                              "Logistic", 
                              "Ordinal Logistic",
                              "Poisson",
                              "Cox PH",
                              "Cox PH with censoring",
                              "AFT",
                              "AFT with censoring",
                              "Generalized Least Squares",
                              "Quantile"),
                  selected="Linear", multiple=FALSE)
    })
    
    #6. Indicate the censoring variable.
    output$censoring <- renderUI({                                 #Same idea as output$vy
      textInput("censor", "6. Censoring variable for survival model")     #Will make choices based on my reactive function.
    })

    #7. Indicate if there should be splines.
    output$aft_dist <- renderUI({  
      selectInput("aftDist", "7. Select the Y distribution for AFT models", choices = c("weibull", "exponential", "gaussian","logistic","lognormal", "loglogistic"), 
                  multiple=FALSE, selected="weibull")     
    })
    
    #7A. Object for AFT Y distribution for psm()
    AFT_PSM_Dist <- reactive({         
      input$aftDist
    })
    
    #Indicate the clustering structure.
    output$clustering <- renderUI({                                 #Same idea as output$vy
      selectInput("gls_clst", "8. GLS clustering: List level 1 and 2 variables (e.g., time | id)",     #Will make choices based on my reactive function.
      choices = gls_lev1_2(), multiple=TRUE, selected=gls_lev1_2()[1:2])
    })

    #Indicate the quantile/percentile value to use in quantile regression
    output$quant_tau <- renderUI({                                 #Same idea as output$vy
      numericInput("tau1", "9. Quantile level for quantile regression",
                   value=0.50, min=0, max=1, step=.05)     #Will make choices based on my reactive function.
    })
    
    #Indicate if there should be splines.
    output$rcs_yes <- renderUI({                                 #Same idea as output$vy
      selectInput("rcsy", "10. Do you want splines?", 
                  choices = c("No", "Yes"), multiple=FALSE, selected="No")     #Will make choices based on my reactive function.
    })
    
    #Select the variables that will have splines.
    output$rx <- renderUI({                                 #Same idea as output$vy
      selectInput("rcs_X", "11. Select the spline variables (continuous only)", 
                  choices = predictor(), multiple=TRUE, selected=predictor()[1])     #Will make choices based on my reactive function.
    })

    #Indicate if you should update the model.
    output$update_yes <- renderUI({                                 #Same idea as output$vy
      selectInput("updy", "12. Do you want to update the model formula?", 
                  choices = c("No", "Yes"), multiple=FALSE, selected="No")     #Will make choices based on my reactive function.
    })
    
    #Update the model formula.
    output$uf <- renderUI({                                 #Same idea as output$vy
      textInput("up_fmla", "13. Update formula (Interactions: Change \"+\" to \'*\', strat(X) to stratify CPH).", 
                value= deparse(mdl_fmla(), width.cutoff=500 ))     #Will make choices based on my reactive function.
    })
    
    #Indicates if the SINGLE predictor is stratified, needed to print regression results output$regress
    MsStrat0 <- reactive({
      if( length(input$variableX) < 2) {
       # if(input$updy == "Yes") {
          length(grep("strat", strsplit(input$up_fmla, "~")[[1]][2]))
        } else {
        0
        } 
    }) 
    
    #Regression results.
    output$regress <- renderPrint({                                                
      atch()
      if(MsStrat0() ==1) {
        print(list( "Model"=fit1(), "AIC"=AIC(fit1()) ))
      } else {   
        if( input$regress_type %in% c("Logistic", "Ordinal Logistic", "Poisson", "Cox PH", "Cox PH with censoring")) {
          print( list("Model"=fit1(), "Exponentiated Coefficients"= exp(fit1()[["coefficients"]]), "AIC"=AIC(fit1())) )
        } else {
          print(list( "Model"=fit1(), "AIC"=AIC(fit1()) ))
        }
      }
    })
    #Regression results.
    #Model predicted Y regression equation
    output$regress_equation <- renderPrint({                                                 
      options(scipen=20)
      if (input$begin_mdl == "Yes") {
      print(Function(fit1()))
      }
    })
    
    describeY <- reactive({                  #This stores the censoring variable. 
      describe(as.numeric(df()[, input$variableY], na.rm=TRUE), #Summary of Y variable.
                     descript=paste0("Summary of ",input$variableY) )  
    })
    
    #The single partial effect variable to plot
    pe_x_var <- reactive({         
      input$pe_X
    })

    #The single or multiple variables to for the summary plot
    sm_x_var <- reactive({         
      input$sm_X
    })

#The single or multiple variables for the summary plot
#Uses "omit" argument so I need to specify everything except the predictors I want.
nm_x_var <- reactive({         
  setdiff(predictor(), input$nm_X)   
})

#This gives the cross-validated sample size for the "B" argument which is the size of the single ommitted group
#crossvalidate_n <- reactive({
#  if(nrow(fit1()[["x"]]) < 80) {   #use 82 so it will round
#    floor(nrow(fit1()[["x"]])/2)          #This will give a training group 1/2 of the model sample size
#  }  else {
#    40
#  }
#})


    output$specifications <- renderPrint({   
      if (input$begin_mdl == "Yes") {
      print( specs(fit1(), long=TRUE))  #Summary of model fit.
      }
    })
    
    output$desc_Y <- renderPrint({                                                 
      print(describeY())
    })
    
    ## Summary of predicted variable values ##
    describeYhatHistRslt <- reactive({ 
      describe(yhat_hist_rslt(), descript=paste0("Predicted values of ",input$variableY) ) #Summary of predicted Y variable.)
#            descript="Predicted values" )  
           #descript=paste0("Summary of ",input$variableY) )  
    })
    
    ## Summary of transformed Yhat values ##
    describeYhatHistRsltTrnsf <- reactive({ 
      fncTrnsfYhatSmry(YhatRslt=describeYhatHistRslt(), RegType= input$regress_type)
    })
    
output$desc_YhatHistRslt <- renderPrint({ 
#  if (input$begin_mdl == "Yes") {
  list( "Predicted.Values"=try( describeYhatHistRslt() ), 
        "Transformed.Yhat"=try(describeYhatHistRsltTrnsf()[["Transformed.Yhat"]] ),
        "Transformed.Full.Range"=try(describeYhatHistRsltTrnsf()[["Transformed.Full.Range"]] )
  )
#  }
})  

################################
## Function to transform Yhat ##
################################
fncTrnsfYhatSmry <- function(YhatRslt, RegType) {
  #Excluded values
  excld_describe <- c("n", "missing", "distinct", "Info")
  #Transform scores
  Transformed.Yhat <- switch(RegType,                
                             "Linear" = NA, 
                             "Logistic" = plogis(as.numeric(YhatRslt$counts[setdiff(names(YhatRslt$counts), excld_describe) ])),
                             "Ordinal Logistic" = plogis(as.numeric(YhatRslt$counts[setdiff(names(YhatRslt$counts), excld_describe) ])),
                             "Poisson" = exp(as.numeric(YhatRslt$counts[setdiff(names(YhatRslt$counts), excld_describe) ])),
                             "Quantile" = NA,
                             "Cox PH" = plogis(as.numeric(YhatRslt$counts[setdiff(names(YhatRslt$counts), excld_describe) ])),
                             "Cox PH with censoring" = plogis(as.numeric(YhatRslt$counts[setdiff(names(YhatRslt$counts), excld_describe) ])),
                             "AFT"  = NA,
                             "AFT with censoring"     = NA,
                             "Generalized Least Squares" = NA )
  #Add names
  names(Transformed.Yhat) <- switch(RegType,                
                                    "Linear" = NULL, 
                                    "Logistic" = setdiff(names(YhatRslt$counts), excld_describe),
                                    "Ordinal Logistic" = setdiff(names(YhatRslt$counts), excld_describe),
                                    "Poisson" = setdiff(names(YhatRslt$counts), excld_describe),
                                    "Quantile" = NA,
                                    "Cox PH" = setdiff(names(YhatRslt$counts), excld_describe),
                                    "Cox PH with censoring" = setdiff(names(YhatRslt$counts), excld_describe),
                                    "AFT"  = NA,
                                    "AFT with censoring"     = NA,
                                    "Generalized Least Squares" = NA )
  #Transform range of lowest and highest values
  Transformed.Full.Range <- switch(RegType,                
                             "Linear" = NA, 
                             "Logistic" = range(plogis(as.numeric(YhatRslt$extremes)) ),
                             "Ordinal Logistic" = range(plogis(as.numeric(YhatRslt$extremes)) ),
                             "Poisson" = exp(as.numeric(YhatRslt$extremes)),
                             "Quantile" = NA,
                             "Cox PH" = range(plogis(as.numeric(YhatRslt$extremes)) ),
                             "Cox PH with censoring" = range(plogis(as.numeric(YhatRslt$extremes)) ),
                             "AFT"  = NA,
                             "AFT with censoring"     = NA,
                             "Generalized Least Squares" = NA )
  
  return(list("Transformed.Yhat"=Transformed.Yhat, "Transformed.Full.Range"= Transformed.Full.Range))
}  


#########################################
## Binary classification of predictors ##
#########################################

#1. Select a cutoff level.
output$pred_class_thresh <- renderUI({                                 
  numericInput("PredClassThresh", "1. Select a cutoff level.", 
               value = 0, step = .1)     
})
#1A. Object for cutoff level 
prediction_class_threshold <- reactive({
#  if (input$begin_mdl == "Yes") {
    input$PredClassThresh
#  }
})

#2. Select a survival model time (e.g., 7 days)
output$pred_class_time <- renderUI({                                 
  numericInput("PredClassTime", "2. Select a survival model time (e.g., 7 days).", 
               value = 1, step = 1, min=0)     
})
#2A. Object for survival model time 
prediction_classification_time <- reactive({
#  if (input$begin_mdl == "Yes") {
    input$PredClassTime
#  }
  })

#3. Name of the prior model.
output$class_pri_mdl_nm <- renderUI({                           
  textInput("classPrMdlFtNm", "3. Prior model name")     
})
#3A. Prior model fit reactive function.
class_prior_model_fit_name <- reactive({                 
  if ( use_class_prior_model_YN() == "Yes") {
    get(input$classPrMdlFtNm) 
  } 
})

#4. Indicate if you want to use a prior model
output$use_pred_cls_pri_mdl_yesno <- renderUI({                                 
  selectInput("useClsPrModelYN", "4. Do you want to use the prior model fit?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")  
  })

  #4A. Object for classification plot 
  use_class_prior_model_YN <- reactive({
    input$useClsPrModelYN
  })
  
#5. Select the approximate number of histogram bars
output$pred_class_hist_bars <- renderUI({                                 
  numericInput("PredClassHistBars", "5. Select the approximate number of histogram bars.", 
               value = 15, step = 1, min=2)     
})
#5A. Object for histogram bars 
prediction_class_histogram_bars <- reactive({
#  if (input$begin_mdl == "Yes") {
    input$PredClassHistBars
#  }
})

#6. Select a survival model time (e.g., 7 days)
output$class_hist_asp_ratio <- renderUI({                                 
  selectInput("clsHistAspRtio", "6. Do you want both y-axes on the same scale?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     
})
#6A. Object for survival model time 
class_histogram_aspect_ratio <- reactive({
#  if (input$begin_mdl == "Yes") {
    input$clsHistAspRtio
#  }
})

#7. Indicate if you want the classification plot
output$pred_class_hist_yesno <- renderUI({                                 
  selectInput("PredClassHistYN", "7. Do you want to run the classification plot?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     
})
#7A. Object for classification plot 
prediction_class_histogram_yes_no <- reactive({
 # if (input$begin_mdl == "Yes") {
    input$PredClassHistYN
#  }
})
#Run functions below
#8. Get data for functions
get_binary_class_df <- reactive({
  if (prediction_class_histogram_yes_no() =="Yes") {
    if (use_class_prior_model_YN() =="Yes") {
      fncYhatClassDf(Fit=class_prior_model_fit_name(), Y=outcome(), Threshold=prediction_class_threshold(), Censor=censor1(),
                     PredTime=prediction_classification_time(), RegType=input$regress_type, DF=df() )
    } else {
      fncYhatClassDf(Fit= fit1(), Y=outcome(),
                     Threshold=prediction_class_threshold(), Censor=censor1(),
                     PredTime=prediction_classification_time(), RegType=input$regress_type, DF=df() )
    }
  }
})
#8A. Run the data function
binary_classification_data_output <- reactive({
  if (prediction_class_histogram_yes_no() =="Yes") {
    get_binary_class_df()
  }
})

#9. Get AUC
get_binary_class_AUC <- reactive({
  if (prediction_class_histogram_yes_no() =="Yes") {
    fncThreshAUC(ClassDF=binary_classification_data_output() )
#    fncThreshAUC(Fit=fit1(), Y=outcome(), Threshold=describeYhatHistRslt(), Censor=censor2()[length(censor2())], 
#                   PredTime=prediction_classification_time(), RegType=input$regress_type, DF=df(), ClassDF=binary_classification_data_output() )
  }
})
#9A. Run the AUC function
binary_classification_AUC_output <- reactive({
  if (prediction_class_histogram_yes_no() =="Yes") {
    get_binary_class_AUC()
}
  })

#10. Plot binary classification
plot_binary_class_function <- reactive({
  if (prediction_class_histogram_yes_no() =="Yes") {
    fncYhatClassPlt(ClassDF=binary_classification_data_output(), AUC=binary_classification_AUC_output(), 
                    Brks=prediction_class_histogram_bars(), RegType= input$regress_type, aspectRatio=class_histogram_aspect_ratio() #Dropped, Yhat=describeYhatHistRslt() 
                    )
  }
})
#10A. Run the plot function 
output$plot_binary_class_run <- renderPlot({
  if (prediction_class_histogram_yes_no() =="Yes") {
    plot_binary_class_function()
  }
  })

#11. Get sensitivity and specificity from data
get_binary_class_sensitivity_specificity <- reactive({
  if (prediction_class_histogram_yes_no() =="Yes") {
    fncClassDfSmry(ClassDF=binary_classification_data_output(), RegType= input$regress_type)
  }
})
#11A. Run the print function 
output$get_bin_class_sens_spc_out <- renderPrint({
  if (prediction_class_histogram_yes_no() =="Yes") {
    get_binary_class_sensitivity_specificity()
  }
})
#############################
## Decision curve analysis ##
#############################
#12. Indicate if you want the classification plot
output$decison_curve_anly_yesno <- renderUI({                                 
  selectInput("decisCrvAnlYN", "2. Do you want to run the decision curve analysis?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     
})
#12A. Object for classification plot 
#prediction_class_histogram_yes_no
decison_curve_analysis_yes_no <- reactive({
  input$decisCrvAnlYN
})
#13. Indicate if you want the classification plot
output$net_or_intervention <- renderUI({                                 
  selectInput("netOrIntrvntn", "1. Plot net benefit or interventions avoided?", 
              choices = c("Net Benefit", "Interventions Avoided"), multiple=FALSE, selected="Net Benefit")     
})
#13A. Object for classification plot 
#prediction_class_histogram_yes_no
net_benefit_or_interventions <- reactive({
  input$netOrIntrvntn
})
#Run functions below
#14. Get data for functions 
get_thresh_quant_df <- reactive({
  if (decison_curve_analysis_yes_no() =="Yes") {
    if (use_class_prior_model_YN() =="Yes") {
      fncThreshQntl(Fit=class_prior_model_fit_name(), Y=outcome(), Threshold=describeYhatHistRslt(), Censor=censor1(), 
                    PredTime=prediction_classification_time(), RegType=input$regress_type, DF=df())
    } else {
      fncThreshQntl(Fit=fit1(), Y=outcome(), Threshold=describeYhatHistRslt(), Censor=censor1(), 
                    PredTime=prediction_classification_time(), RegType=input$regress_type, DF=df())
    }
  }
})
#14A. Run the data function
thresh_quant_data_output <- reactive({
  if (decison_curve_analysis_yes_no() =="Yes") {
    get_thresh_quant_df()
  }
})
#15. Set up Decision Curve
plot_thresh_quant_function <- reactive({
  if (decison_curve_analysis_yes_no() =="Yes") {
    fncDcsnCrvPlt(ThreshQntl=thresh_quant_data_output(), CType=net_benefit_or_interventions(),
                  xlim1=descion_crv_plt_xlim1(), xlim2=descion_crv_plt_xlim2(), ylim1=descion_crv_plt_ylim1(), ylim2=descion_crv_plt_ylim2())
  }
})
#15A. Run the plot function 
output$plot_thresh_quant_run <- renderPlot({
  if (decison_curve_analysis_yes_no() =="Yes") {
    plot_thresh_quant_function()
  }
})

#16. Indicate lower limit of x-axis
output$descionCrvPltXlim1 <- renderUI({
  numericInput("dc_Xlim1", "3. Lower X-axis limit.",
               #value = cox_min_time(), step = 1)
               value = 0, step = .1)
})
#16A. Set up Decision Curve
descion_crv_plt_xlim1 <- reactive({
  if (decison_curve_analysis_yes_no() =="Yes") {
    input$dc_Xlim1
  }
})
#17. Indicate upper limit of x-axis
output$descionCrvPltXlim2 <- renderUI({
  numericInput("dc_Xlim2", "4. Upper X-axis limit.",
               value = 1, step = .1)
})
#17A. Set up Decision Curve
descion_crv_plt_xlim2 <- reactive({
  if (decison_curve_analysis_yes_no() =="Yes") {
    input$dc_Xlim2
  }
})
#18. Indicate lower limit of y-axis
output$descionCrvPltYlim1 <- renderUI({
  numericInput("dc_Ylim1", "5. Lower Y-axis limit.",
               value = 0, step = .1)
})
#18A. Set up Decision Curve
descion_crv_plt_ylim1 <- reactive({
  if (decison_curve_analysis_yes_no() =="Yes") {
    input$dc_Ylim1
  }
})
#19. Indicate upper limit of x-axis
output$descionCrvPltYlim2 <- renderUI({
  numericInput("dc_Ylim2", "6. Upper Y-axis limit.",
               value = 1, step = .1)
})
#19A. Set up Decision Curve
descion_crv_plt_ylim2 <- reactive({
  if (decison_curve_analysis_yes_no() =="Yes") {
    input$dc_Ylim2
  }
})

################
##  Functions ##
################

##############
## Get data ##
##############
fncYhatClassDf <- function(Fit, Y, Threshold, Censor=NULL, PredTime=NULL, RegType, DF)  {
  tm1 <- Fit
  atime <- PredTime
  tdf <- DF
  tY <- Y
  tcensor <- Censor
  PREDrange <- range(predict(Fit), na.rm=T)  
  
  # Need to make object for this so I can calculate the trapezoid AUC:
  threshLev <- Threshold
  #Get predictions for values at threshold
  #Make data for predict()
  newtdf1 <- switch(RegType,                
                    "Linear"   = tdf[ tdf[,  tY] >= threshLev, ], 
                    "Logistic" = tdf[ tdf[,  tY] == 1, ],
                    "Ordinal Logistic"  = tdf[ tdf[,  tY] > 1, ],
                    "Poisson"  = tdf[ tdf[,  tY] >= exp(threshLev), ],
                    "Quantile" = tdf[ tdf[,  tY] >= threshLev, ],
                    "Cox PH"   = tdf[tdf[,  tY] < atime, ],
#old                    "Cox PH with censoring"  = tdf[ tdf[, tcensor ] == max(tdf[, tcensor ], na.rm=T) & tdf[,  tY] >= atime, ],
                    "Cox PH with censoring"  = tdf[ eval(parse(text=paste0("tdf$",tcensor) )) == TRUE & tdf[,  tY] < atime, ],
                    "AFT"  = tdf[tdf[,  tY] < atime, ],
#old                    "AFT with censoring"     = tdf[ tdf[, tcensor ] ==max(tdf[, tcensor ], na.rm=T) & tdf[,  tY] >= atime, ],
                    "AFT with censoring"     = tdf[eval(parse(text=paste0("tdf$",tcensor) )) == TRUE & tdf[,  tY] < atime, ],
                    "Generalized Least Squares" = tdf[ tdf[,  tY] >= threshLev, ] )
  newtdf2 <- switch(RegType,                
                    "Linear"   = tdf[ tdf[,  tY] < threshLev, ], 
                    "Logistic" = tdf[ tdf[,  tY] == 0, ],
                    "Ordinal Logistic"  = tdf[ tdf[,  tY]  ==1, ],
                    "Poisson"  = tdf[ tdf[,  tY] < exp(threshLev), ],
                    "Quantile" = tdf[ tdf[,  tY] < threshLev, ],
                    "Cox PH"   = tdf[tdf[,  tY] >= atime, ],
#old                    "Cox PH with censoring"  = tdf[ tdf[, tcensor ] ==min(tdf[, tcensor ], na.rm=T) & tdf[,  tY] < atime, ],
                    "Cox PH with censoring"  = tdf[ eval(parse(text=paste0("tdf$",tcensor) )) == FALSE & tdf[,  tY] >= atime, ],
                    "AFT"  = tdf[tdf[,  tY] >= atime, ],
#old                    "AFT with censoring"     = tdf[ tdf[, tcensor ] ==min(tdf[, tcensor ], na.rm=T) & tdf[,  tY] >= atime, ],
                    "AFT with censoring"     = tdf[eval(parse(text=paste0("tdf$",tcensor) )) == FALSE & tdf[,  tY] < atime, ],
                    "Generalized Least Squares" = tdf[ tdf[,  tY] < threshLev, ] )
  #Get tranformed values of threshold when using logits 
  Transform.Threshold <- switch(RegType,                
                                "Linear"   = NA, 
                                "Logistic" = plogis(threshLev),
                                "Ordinal Logistic"  = plogis(threshLev),
                                "Poisson"  = exp(threshLev),
                                "Quantile" = NA,
                                "Cox PH"   = plogis(threshLev),
                                "Cox PH with censoring"  = plogis(threshLev),
                                "AFT"  = plogis(threshLev),
                                "AFT with censoring"     = plogis(threshLev),
                                "Generalized Least Squares" = NA )
  #Predictions
  #pm_all <- predict(tm1, newdata= tdf)
  pm1 <- predict(tm1, newdata= newtdf1)
  pm2 <- predict(tm1, newdata= newtdf2)
  #Get values for xlim of plot
    senspcXmin <- PREDrange[1]
    senspcXmax <- PREDrange[2]
  ## This determines the amount of predictions above a certain level
  #Sensitivity
  #Get probability for 4 types of response
#old  pr_table1 <- prop.table(table(factor(pm1 >= threshLev, levels=c("FALSE","TRUE") )))
#old  pr_table2 <- prop.table(table(factor(pm2 >= threshLev, levels=c("FALSE","TRUE") )))
  pr_table1 <- prop.table(table(factor(pm1 > threshLev, levels=c("FALSE","TRUE") )))
  pr_table2 <- prop.table(table(factor(pm2 > threshLev, levels=c("FALSE","TRUE") )))
  #Sensitivity and 1 - specificity
  propAbovMY1 <-  pr_table1["TRUE"]  #Sensitivity
  fls_Neg <-  pr_table1["FALSE"]  #FALSE negative
  propAbovMY0 <- pr_table2["TRUE"]  #1-specificity or false-positive
  specifity <-  pr_table2["FALSE"]  #Specificity
#Get frequencies
  f_table1 <- table(factor(pm1 > threshLev, levels=c("FALSE","TRUE") ))
  f_table2 <- table(factor(pm2 > threshLev, levels=c("FALSE","TRUE") ))
  #Sensitivity and 1 - specificity
  N.AbovMY1 <-  f_table1["TRUE"]  #Sensitivity
  N.fls_Neg <-  f_table1["FALSE"]  #FALSE negative
  N.AbovMY0 <- f_table2["TRUE"]  #1-specificity or false-positive
  N.specifity <-  f_table2["FALSE"]  #Specificity
    
  return(list("pm1"=pm1, "pm2"=pm2, "threshLev"=threshLev, "senspcXmin"=senspcXmin, "senspcXmax"=senspcXmax,
              "propAbovMY1"=propAbovMY1, "fls_Neg"=fls_Neg,  "propAbovMY0"=propAbovMY0, "specifity"=specifity,
              "Transform.Threshold"=Transform.Threshold,
              "N.AbovMY1"=N.AbovMY1, "N.fls_Neg"=N.fls_Neg,  "N.AbovMY0"=N.AbovMY0, "N.specifity"=N.specifity))
  
}

#####################################################
## Function to create AUC for each threshold I set ##
#####################################################
fncThreshAUC <- function(ClassDF) {
  ## This AUC is based only on the cutoff level. Use this to get the binary classification AUC.
  yClassSens <- c(ClassDF$propAbovMY1)
  yClassSpec <- c(ClassDF$specifity) 
  #Add in values of 0 and 1 for perfect sensitivity and specificity
  yClassSens <- c(1, yClassSens, 0)
  yClassSpec <- c(0, yClassSpec, 1)
  
  #Gets differences between specificity values
  spc_diff <- diff(yClassSpec)
  
  #Adds sensitivity portions of formula
  sns_diff <- vector(length= length(yClassSens) - 1)
  i <- 1
  while (i < length(yClassSens) ) {
    sns_diff[i] <- yClassSens[i] + yClassSens[i+1] 
    i = i+1
  }
  #Command to get AUC#
  Threshold.AUC <- sum(.5*(spc_diff * sns_diff))
  return("Threshold.AUC"=Threshold.AUC)
}

##############
## Graphing ##
##############
fncYhatClassPlt <- function(ClassDF, AUC, Brks, RegType, aspectRatio)  {
  par(mfrow=c(2,1))
#  xlimMin <- Yhat[["extremes"]][1]
#  xlimMax <- Yhat[["extremes"]][10]
  xlimMin <- ClassDF$senspcXmin
  xlimMax <- ClassDF$senspcXmax
  #Sensitivity and 1-specificity
  Sens.Value <- switch(RegType,                
                        "Linear"   = round(ClassDF$propAbovMY1, 3), 
                        "Logistic" = round(ClassDF$propAbovMY1, 3),
                        "Ordinal Logistic"  = round(ClassDF$propAbovMY1, 3),
                        "Poisson"  = round(ClassDF$propAbovMY1, 3),
                        "Quantile" = round(ClassDF$propAbovMY1, 3),
                        "Cox PH"   = round(ClassDF$propAbovMY1, 3),
                        "Cox PH with censoring"  = round(ClassDF$propAbovMY1, 3),
                        "AFT"  = round(ClassDF$fls_Neg, 3) ,
                        "AFT with censoring"     = round(ClassDF$fls_Neg, 3),
                        "Generalized Least Squares" = round(ClassDF$propAbovMY1, 3) )
  Spec.Value <- switch(RegType,                
                        "Linear"   = round(ClassDF$propAbovMY0, 3), 
                        "Logistic" = round(ClassDF$propAbovMY0, 3),
                        "Ordinal Logistic"  = round(ClassDF$propAbovMY0, 3),
                        "Poisson"  = round(ClassDF$propAbovMY0, 3),
                        "Quantile" = round(ClassDF$propAbovMY0, 3),
                        "Cox PH"   = round(ClassDF$propAbovMY0, 3),
                        "Cox PH with censoring"  = round(ClassDF$propAbovMY0, 3),
                        "AFT"  = round(ClassDF$specifity, 3), 
                        "AFT with censoring"     = round(ClassDF$specifity, 3),
                        "Generalized Least Squares" = round(ClassDF$propAbovMY0, 3) )
  #Bar colors
  Bar.Colors1 <- switch(RegType,                
                                "Linear"   = c("grey", "green"), 
                                "Logistic" = c("grey", "green"),
                                "Ordinal Logistic"  = c("grey", "green"),
                                "Poisson"  = c("grey", "green"),
                                "Quantile" = c("grey", "green"),
                                "Cox PH"   = c("grey", "green"),
                                "Cox PH with censoring"  = c("grey", "green"),
                                "AFT"  = c("green", "grey"),
                                "AFT with censoring"     = c("green", "grey"),
                                "Generalized Least Squares" = c("grey", "green") )
  Bar.Colors2 <- switch(RegType,                
                        "Linear"   = c("grey", "red"), 
                        "Logistic" = c("grey", "red"),
                        "Ordinal Logistic"  = c("grey", "red"),
                        "Poisson"  = c("grey", "red"),
                        "Quantile" = c("grey", "red"),
                        "Cox PH"   = c("grey", "red"),
                        "Cox PH with censoring"  = c("grey", "red"),
                        "AFT"  = c("red", "grey"),
                        "AFT with censoring"     = c("red", "grey"),
                        "Generalized Least Squares" = c("grey", "red") )
  #AUC value
  AUC_val <- switch(RegType,                
                    "Linear"   = round(AUC, 3), 
                    "Logistic" = round(AUC, 3),
                    "Ordinal Logistic"  = round(AUC, 3),
                    "Poisson"  = round(AUC, 3),
                    "Quantile" = round(AUC, 3),
                    "Cox PH"   = round(AUC, 3),
                    "Cox PH with censoring"  = round(AUC, 3),
                    "AFT"  = round(1-AUC, 3),
                    "AFT with censoring"     = round(1-AUC, 3),
                    "Generalized Least Squares" = round(AUC, 3) )

  #Histogram values  
  h1 <- hist(ClassDF$pm1,  plot=FALSE, breaks=Brks)
  cuts1 <- cut(h1$breaks, c(-Inf, ClassDF$threshLev, Inf))
  h2 <- hist(ClassDF$pm2, plot=FALSE, breaks=Brks)
  cuts2 <- cut(h2$breaks, c(-Inf, ClassDF$threshLev, Inf))
  #Get maximum part of histogram y-axis
  if (aspectRatio == "Yes") {
    hyMAX <- max(max(h1$counts), max(h2$counts))
  } else {
    hyMAX <- c(max(h1$counts), max(h2$counts))
  }
  
  #Histograms
  plot(h1, col=Bar.Colors1[cuts1], xlim=c(xlimMin, xlimMax), ylim=c(0, head(hyMAX, 1)),
       main=paste0("Outcome = Yes. (n = ", sum(ClassDF$N.AbovMY1, ClassDF$N.fls_Neg),"). Proportion of predictions at or above cutoff: ", Sens.Value, ".  AUC = ", AUC_val, "." ),
       xlab=paste0("Sensitivity: True-Positives in green using a cutoff of ", ClassDF$threshLev, " (Transformed = ", round(ClassDF$Transform.Threshold, 3), ")." ))
  abline(v=ClassDF$threshLev, lwd=3, col=4)
  plot(h2, col=Bar.Colors2[cuts2], xlim=c(xlimMin, xlimMax), ylim=c(0, tail(hyMAX, 1)),
       main=paste0("Outcome = No. (n = ", sum(ClassDF$N.specifity, ClassDF$N.AbovMY0),"). Proportion of predictions at or above cutoff: ", Spec.Value, ".  AUC = ", AUC_val, "."  ),
       xlab=paste0("1 - Specificity: False-Positives in red using a cutoff of ", ClassDF$threshLev, " (Transformed = ", round(ClassDF$Transform.Threshold, 3), ")." ))
  abline(v=ClassDF$threshLev, lwd=3, col=4)
  par(mfrow=c(1,1))
}

########################
## Get summary values ##
########################
fncClassDfSmry <- function(ClassDF, RegType) {
  #Sensitivity value
  propAbovMY1 <- switch(RegType,                
                    "Linear"   = unname(ClassDF$propAbovMY1), 
                    "Logistic" = unname(ClassDF$propAbovMY1),
                    "Ordinal Logistic"  = unname(ClassDF$propAbovMY1),
                    "Poisson"  = unname(ClassDF$propAbovMY1),
                    "Quantile" = unname(ClassDF$propAbovMY1),
                    "Cox PH"   = unname(ClassDF$propAbovMY1),
                    "Cox PH with censoring"  = unname(ClassDF$propAbovMY1),
                    "AFT"  = 1-unname(ClassDF$propAbovMY1),
                    "AFT with censoring"     = 1-unname(ClassDF$propAbovMY1),
                    "Generalized Least Squares" = unname(ClassDF$propAbovMY1) )
  #Specificity value
  specifity <- switch(RegType,                
                        "Linear"   = unname(ClassDF$specifity), 
                        "Logistic" = unname(ClassDF$specifity),
                        "Ordinal Logistic"  = unname(ClassDF$specifity),
                        "Poisson"  = unname(ClassDF$specifity),
                        "Quantile" = unname(ClassDF$specifity),
                        "Cox PH"   = unname(ClassDF$specifity),
                        "Cox PH with censoring"  = unname(ClassDF$specifity),
                        "AFT"  = 1-unname(ClassDF$specifity),
                        "AFT with censoring"     = 1-unname(ClassDF$specifity),
                        "Generalized Least Squares" = unname(ClassDF$specifity) )
  #False.Positives value
  propAbovMY0 <- switch(RegType,                
                      "Linear"   = unname(ClassDF$propAbovMY0), 
                      "Logistic" = unname(ClassDF$propAbovMY0),
                      "Ordinal Logistic"  = unname(ClassDF$propAbovMY0),
                      "Poisson"  = unname(ClassDF$propAbovMY0),
                      "Quantile" = unname(ClassDF$propAbovMY0),
                      "Cox PH"   = unname(ClassDF$propAbovMY0),
                      "Cox PH with censoring"  = unname(ClassDF$propAbovMY0),
                      "AFT"  = 1-unname(ClassDF$propAbovMY0),
                      "AFT with censoring"     = 1-unname(ClassDF$propAbovMY0),
                      "Generalized Least Squares" = unname(ClassDF$propAbovMY0) )
  #False.Negatives value
  fls_Neg <- switch(RegType,                
                        "Linear"   = unname(ClassDF$fls_Neg), 
                        "Logistic" = unname(ClassDF$fls_Neg),
                        "Ordinal Logistic"  = unname(ClassDF$fls_Neg),
                        "Poisson"  = unname(ClassDF$fls_Neg),
                        "Quantile" = unname(ClassDF$fls_Neg),
                        "Cox PH"   = unname(ClassDF$fls_Neg),
                        "Cox PH with censoring"  = unname(ClassDF$fls_Neg),
                        "AFT"  = 1-unname(ClassDF$fls_Neg),
                        "AFT with censoring"     = 1-unname(ClassDF$fls_Neg),
                        "Generalized Least Squares" = unname(ClassDF$fls_Neg) )

  ## Frequencies ##
  #Sensitivity value
  N.AbovMY1 <- switch(RegType,                
                        "Linear"   = unname(ClassDF$N.AbovMY1), 
                        "Logistic" = unname(ClassDF$N.AbovMY1),
                        "Ordinal Logistic"  = unname(ClassDF$N.AbovMY1),
                        "Poisson"  = unname(ClassDF$N.AbovMY1),
                        "Quantile" = unname(ClassDF$N.AbovMY1),
                        "Cox PH"   = unname(ClassDF$N.AbovMY1),
                        "Cox PH with censoring"  = unname(ClassDF$N.AbovMY1),
                        "AFT"  = unname(ClassDF$N.fls_Neg),
                        "AFT with censoring"     = unname(ClassDF$N.fls_Neg),
                        "Generalized Least Squares" = unname(ClassDF$N.AbovMY1) )
  #Specificity value
  N.specifity <- switch(RegType,                
                      "Linear"   = unname(ClassDF$N.specifity), 
                      "Logistic" = unname(ClassDF$N.specifity),
                      "Ordinal Logistic"  = unname(ClassDF$N.specifity),
                      "Poisson"  = unname(ClassDF$N.specifity),
                      "Quantile" = unname(ClassDF$N.specifity),
                      "Cox PH"   = unname(ClassDF$N.specifity),
                      "Cox PH with censoring"  = unname(ClassDF$N.specifity),
                      "AFT"  = unname(ClassDF$N.AbovMY0),
                      "AFT with censoring"     = unname(ClassDF$N.AbovMY0),
                      "Generalized Least Squares" = unname(ClassDF$N.specifity) )
  #False.Positives value
  N.AbovMY0 <- switch(RegType,                
                        "Linear"   = unname(ClassDF$N.AbovMY0), 
                        "Logistic" = unname(ClassDF$N.AbovMY0),
                        "Ordinal Logistic"  = unname(ClassDF$N.AbovMY0),
                        "Poisson"  = unname(ClassDF$N.AbovMY0),
                        "Quantile" = unname(ClassDF$N.AbovMY0),
                        "Cox PH"   = unname(ClassDF$N.AbovMY0),
                        "Cox PH with censoring"  = unname(ClassDF$N.AbovMY0),
                        "AFT"  = unname(ClassDF$N.specifity),
                        "AFT with censoring"     = unname(ClassDF$N.specifity),
                        "Generalized Least Squares" = unname(ClassDF$N.AbovMY0) )
  #False.Negatives value
  N.fls_Neg <- switch(RegType,                
                    "Linear"   = unname(ClassDF$N.fls_Neg), 
                    "Logistic" = unname(ClassDF$N.fls_Neg),
                    "Ordinal Logistic"  = unname(ClassDF$N.fls_Neg),
                    "Poisson"  = unname(ClassDF$N.fls_Neg),
                    "Quantile" = unname(ClassDF$N.fls_Neg),
                    "Cox PH"   = unname(ClassDF$N.fls_Neg),
                    "Cox PH with censoring"  = unname(ClassDF$N.fls_Neg),
                    "AFT"  = unname(ClassDF$N.AbovMY1),
                    "AFT with censoring"     = unname(ClassDF$N.AbovMY1),
                    "Generalized Least Squares" = unname(ClassDF$N.fls_Neg) )
  
  #Total N
  total_N <- sum(N.AbovMY1,N.AbovMY0, N.specifity, N.fls_Neg) 
  
  #Weighted threshold value
  wt_thresh <- switch(RegType,                
                      "Linear"   = pnorm( ClassDF$threshLev, mean = mean(df()[, outcome()], na.rm=T), sd = sd(df()[, outcome()], na.rm=T)),
                      "Logistic" = plogis(ClassDF$threshLev),
                      "Ordinal Logistic"  = plogis(ClassDF$threshLev),
                      "Poisson"  = pnorm( exp(ClassDF$threshLev), mean = mean(df()[, outcome()], na.rm=T), sd = sd(df()[, outcome()], na.rm=T)),                      
                      "Quantile" = pnorm( ClassDF$threshLev, mean = mean(df()[, outcome()], na.rm=T), sd = sd(df()[, outcome()], na.rm=T)),
                      "Cox PH"   = plogis(ClassDF$threshLev),
                      "Cox PH with censoring"  = plogis(ClassDF$threshLev),
                      "AFT"  = pnorm(ClassDF$threshLev , mean = mean(c(ClassDF$pm1, ClassDF$pm2), na.rm=T), sd = sd(c(ClassDF$pm1, ClassDF$pm2), na.rm=T)),
                      "AFT with censoring"     = pnorm(ClassDF$threshLev , mean = mean(c(ClassDF$pm1, ClassDF$pm2), na.rm=T), sd = sd(c(ClassDF$pm1, ClassDF$pm2), na.rm=T)),
                      "Generalized Least Squares" = pnorm( ClassDF$threshLev, mean = mean(df()[, outcome()], na.rm=T), sd = sd(df()[, outcome()], na.rm=T)) )
  
  #Accuracy 
  Accuracy.Rate <- (N.AbovMY1 + N.specifity)/ total_N
  #Error rate
  Error.Rate <- (N.AbovMY0 + N.fls_Neg)/ total_N
  #Positive Predictive Value
  PPV <- N.AbovMY1/ (N.AbovMY1 + N.AbovMY0)
  #Negative Predictive Value
  NPV <- N.specifity/ (N.specifity + N.fls_Neg)
  #Net benefit
  Net.Benefit <- N.AbovMY1/total_N - N.AbovMY0/total_N * (wt_thresh /(1 - wt_thresh ))   
  #All treated
  All.Treated <- (N.AbovMY1 + N.fls_Neg)/total_N - (N.AbovMY0 + N.specifity)/total_N * (wt_thresh /(1 - wt_thresh))
  #Interventions avoided
  Interventions.Saved <- N.specifity/total_N -  N.fls_Neg/total_N * (1 - wt_thresh)/ wt_thresh  
  return(list("Classification"= c("Sensitivity"=propAbovMY1, "Specifity"= specifity, "False.Positives"= propAbovMY0, "False.Negatives"= fls_Neg,
                                  "Accuracy.Rate"=Accuracy.Rate, "Error.Rate"=Error.Rate),
              "Predictive.Values"= c("Positive.Predictive.Value"=PPV, "Negative.Predictive.Value"=NPV),
              "Frequencies"= c("N.Sensitivity"=N.AbovMY1, "N.Specifity"=N.specifity, 
              "N.False.Positives"= N.AbovMY0, "N.False.Negatives"= N.fls_Neg, "N"=total_N),
              "Decision.Curve.Analysis"= c("Net.Benefit"=Net.Benefit, "All.Treated"=All.Treated,
              "Interventions.Avoided"=Interventions.Saved)))
}
 
###########################################
## Create a decision curve analysis plot ##
###########################################
fncThreshQntl <- function(Fit, Y, Threshold, Censor=NULL, PredTime=NULL, RegType, DF) {
  #Get sensitivity and specificity for IQR of predicted values
  yClass.05 <-  fncYhatClassDf(Fit=Fit, Y=Y, Threshold=as.numeric(Threshold[["counts"]][7]), 
                               Censor=Censor, PredTime=PredTime, RegType=RegType, DF=DF)
  yClass.10 <-  fncYhatClassDf(Fit=Fit, Y=Y, Threshold=as.numeric(Threshold[["counts"]][8]), 
                               Censor=Censor, PredTime=PredTime, RegType=RegType, DF=DF)
  yClass.25 <-  fncYhatClassDf(Fit=Fit, Y=Y, Threshold=as.numeric(Threshold[["counts"]][9]), 
                               Censor=Censor, PredTime=PredTime, RegType=RegType, DF=DF)
  yClass.50 <-  fncYhatClassDf(Fit=Fit, Y=Y, Threshold=as.numeric(Threshold[["counts"]][10]), 
                               Censor=Censor, PredTime=PredTime, RegType=RegType, DF=DF)
  yClass.75 <-  fncYhatClassDf(Fit=Fit, Y=Y, Threshold=as.numeric(Threshold[["counts"]][11]), 
                               Censor=Censor, PredTime=PredTime, RegType=RegType, DF=DF)
  yClass.90 <-  fncYhatClassDf(Fit=Fit, Y=Y, Threshold=as.numeric(Threshold[["counts"]][12]), 
                               Censor=Censor, PredTime=PredTime, RegType=RegType, DF=DF)
  yClass.95 <-  fncYhatClassDf(Fit=Fit, Y=Y, Threshold=as.numeric(Threshold[["counts"]][13]), 
                               Censor=Censor, PredTime=PredTime, RegType=RegType, DF=DF)
  #Group the output
  YClass <- list("yClass.05"=yClass.05, "yClass.10"=yClass.10, "yClass.25"=yClass.25,"yClass.50"=yClass.50,
                 "yClass.75"=yClass.75, "yClass.90"=yClass.90, "yClass.95"=yClass.95)
  #Quantile levels for YClass
  YClass_Quants <- as.numeric(names(Threshold$counts)[7:13])
  
  ## Total N ##
  total_N <- vector()
  for (i in 1:length(YClass)) {
    total_N[i] <- sum(YClass[[i]]$N.AbovMY1, YClass[[i]]$N.fls_Neg, YClass[[i]]$N.AbovMY0, YClass[[i]]$N.specifity)
  }
   
  #Weighted threshold value
  Threshold.Level <- vector()
  for (i in 1:length(YClass)) {
    Threshold.Level[i] <- switch(RegType,                
                      "Linear"   = YClass_Quants[i], 
                      "Logistic" = plogis(YClass[[i]]$threshLev),
                      "Ordinal Logistic"  = YClass_Quants[i],
                      "Poisson"  = YClass_Quants[i],
                      "Quantile" = YClass_Quants[i],
                      "Cox PH"   = plogis(YClass[[i]]$threshLev),
                      "Cox PH with censoring"  = plogis(YClass[[i]]$threshLev),
                      "AFT"  = 1 - YClass_Quants[i],
                      "AFT with censoring"     = 1 - YClass_Quants[i],
                      "Generalized Least Squares" = YClass_Quants[i] )
  }
  #################
  ## Net Benefit ##
  #################
  Net.Benefit <- vector()
  Interventions.Saved <- vector()
  All.Treated <- vector()
  if(RegType %in% c("AFT with censoring","AFT")) {
    for (i in 1:length(YClass)) {
      #Net benefit: True positives - False positives * weighting by the relative harm of a false-positive and a false-negative result
      Net.Benefit[i] <- YClass[[i]]$N.fls_Neg/total_N[i] - YClass[[i]]$N.specifity/total_N[i] * (Threshold.Level[i] /(1 - Threshold.Level[i]))   
      #All treated: All positives - all negatives * weighting by the relative harm of a false-positive and a false-negative result
      All.Treated[i] <- (YClass[[i]]$N.fls_Neg + YClass[[i]]$N.AbovMY1)/total_N[i] - (YClass[[i]]$N.specifity + YClass[[i]]$N.AbovMY0)/total_N[i] * (Threshold.Level[i] /(1 - Threshold.Level[i]))   
      #Net benefit for True Negatives...Interventions avoided
      Interventions.Saved[i] <- YClass[[i]]$N.AbovMY0/total_N[i] - YClass[[i]]$N.AbovMY1/total_N[i] *  (1 - Threshold.Level[i]) / Threshold.Level[i]      
    }
  } else (
    for (i in 1:length(YClass)) {
      #Net benefit: True positives - False positives * weighting by the relative harm of a false-positive and a false-negative result
      Net.Benefit[i] <- YClass[[i]]$N.AbovMY1/total_N[i] - YClass[[i]]$N.AbovMY0/total_N[i] * (Threshold.Level[i] /(1 - Threshold.Level[i]))   
      #All treated: All positives - all negatives * weighting by the relative harm of a false-positive and a false-negative result
      All.Treated[i] <- (YClass[[i]]$N.AbovMY1 + YClass[[i]]$N.fls_Neg)/total_N[i] - (YClass[[i]]$N.AbovMY0 + YClass[[i]]$N.specifity)/total_N[i] * (Threshold.Level[i] /(1 - Threshold.Level[i]))   
      #Net benefit for True Negatives...Interventions avoided
      Interventions.Saved[i] <- YClass[[i]]$N.specifity/total_N[i] - YClass[[i]]$N.fls_Neg/total_N[i] *  (1 - Threshold.Level[i]) / Threshold.Level[i]      
    }
  )
  return(list("total_N"=total_N, "Threshold.Level"=Threshold.Level, 
              "Net.Benefit"=Net.Benefit, "All.Treated"=All.Treated, "Interventions.Saved"=Interventions.Saved))
}

#####################################
## Function to plot decision curve ##
#####################################
fncDcsnCrvPlt <- function(ThreshQntl, CType, xlim1,xlim2,ylim1,ylim2) {
  if(CType == "Net Benefit") {
    plot(ThreshQntl$Threshold.Level, ThreshQntl$Net.Benefit,type="n", xlim=c(xlim1,xlim2), ylim=c(ylim1,ylim2), 
         main="Decision curve plot of threshold by benefit", xlab="Threshold", ylab="Net Benefit")
    lines(ThreshQntl$Threshold.Level, ThreshQntl$Net.Benefit,cex=2, lwd=3, lty=2)
    lines(ThreshQntl$Threshold.Level, ThreshQntl$All.Treated,cex=2, lwd=3, lty=1, col=2)
    abline(h=0, col=8, lwd=2)
    legend("bottomright", legend=c("Model", "All treated", "None treated"), 
           col=c(1,2,8), lty=c(2,1,1),
           lwd=2, cex=2)
  } else {
    plot(ThreshQntl$Threshold.Level, ThreshQntl$Interventions.Saved, 
         lwd=4, type="l", col=4, axes=F, xlim=c(xlim1,xlim2), ylim=c(ylim1,ylim2), 
         main="Decision curve plot of interventions avoided", xlab="Threshold", 
         ylab="Interventions avoided per 100 persons")
    axis(1)
    axis(2, at= seq(0,1, .1), labels= seq(0,1, .1)*100)
    box()
  }
}

################################################################################

    #Monte Carlo tab
    output$s_seed <- renderUI({                                 #Same idea as output$vy
      numericInput("set_seed", "Set the seed value", value = 1, min=1)
    })

    output$nsim <- renderUI({                                 #Same idea as output$vy
      numericInput("n_sim", "Enter the number of Simulations",
                   value = 10000, min=1)
    })
    
#    output$all_x <- renderUI({                                 #Same idea as output$vy
#      radioButtons("dist_x_var", "Select the predictors", 
#                   choices = predictor(), selected=predictor()[1])     #Will make choices based on my reactive function.
#    })
    
    output$m_x <- renderUI({                                 #
      values$mn_x <- mean(as.numeric(df()[, input$dist_x_var], na.rm=TRUE))
      numericInput("mean_x", "Select the mean", 
      value= values$mn_x)     #Will make choices based on my reactive function.
    })

    output$s_x <- renderUI({                                 #
      values$sd_x <- sd(df()[, input$dist_x_var], na.rm=TRUE)
      numericInput("std_x", "Select the Standard Deviation", 
                   value= values$sd_x)     #Will make choices based on my reactive function.
    })

    output$dis <- renderPlot({
      if(input$sum_yes == "Yes") {
        plot(  do.call("summary", list(fit1(), sm_x_var(), est.all=FALSE) )) 
        #Below works but requires the actual name, "sm_x_var()" doesn't work in 
        #both places below (e.g., state). If I can get it to read it as
        #the real name like "state", it should work
        #plot(  do.call("summary", list(fit1(), state=unique(df()[,"state"])[1], est.all=FALSE) )) 
      } else {
        plot(summary(fit1()) , pch=11, col.points=2)
      }
    }, height = 600)
#################################################
#    fncPltSmr <- function(sum_yes, fit1, sm_x_var, df) {
    #      lv <- unique(df[, sm_x_var])[1]
    #  if(sum_yes == "Yes") {
    #   #plot( summary(fit1, sm_x_var , est.all=T) ) 
    #   plot(  do.call("summary", list(fit1, sm_x_var=unique(df[,sm_x_var])[1], est.all=FALSE) )) 
    #   
    # } else {
    #   plot(summary(fit1))
    # }
      
    #}
    
    #    output$dis <- renderPlot({
    #fncPltSmr(sum_yes=input$sum_yes, fit1=fit1(), sm_x_var= sm_x_var(), df=df())
    #     })
    
#################################################
    
    #Create yes/no box to determine plot single partial effect
    output$sum_one_yes <- renderUI({                                 #Same idea as output$vy
      selectInput("sum_yes", "1. Do you want to plot a single predictor effect?", 
                  choices = c("No", "Yes"), multiple=FALSE, selected="No")     #Will make choices based on my reactive function.
    })
    #Select the variables that will get a single summary.
    output$sum_one_x <- renderUI({
      radioButtons("sm_X", "2. Select the predictor.", 
                   choices = predictor(), selected=predictor()[1])     #Will make choices based on my reactive function.
    })
    
        
#Revise the model.  
output$mod_ify <- renderPrint({
  print(fastbw(fit1()))
})

#This prints the summary of predictor coefficients.  
output$predictor_smry <- renderPrint({
  summary(fit1())
})

#This Plot ANOVA to show variable importance. 
#I set a higher tolerance based on Harrell's website
#https://stat.ethz.ch/pipermail/r-help/2007-September/141709.html  
output$p_anova <- renderPlot({
  plot(anova(fit1(), tol=1e-13), cex=1.25, cex.lab=1.25, pch=19)
}, height=700)

#This prints ANOVA to show variable importance.  
output$anova_smry <- renderPrint({
  anova(fit1(), digits=4, tol=1e-13)
})

#This plots the predicted values    
    output$prt_prd <- renderPlot({
      if(input$pe_yes == "Yes") {
        plot(  do.call("Predict", list(fit1(), pe_x_var())   )) 
      } else {
        plot(Predict(fit1()))
      }
    }, height = 600)
    #Create yes/no box to determine plot single partial effect
    output$prt_one_yes <- renderUI({                                 #Same idea as output$vy
      selectInput("pe_yes", "1. Do you want to plot a single partial effect?", 
                  choices = c("No", "Yes"), multiple=FALSE, selected="No")     #Will make choices based on my reactive function.
    })
    #Select the variables that will get 
    output$prt_one_x <- renderUI({
      selectInput("pe_X", "2. Select a single predictor.", 
                  choices = predictor(), multiple=FALSE, selected=predictor()[1])     #Will make choices based on my reactive function.
    })

###################################################################    
## Creates a plot for an interaction of continuous X by a factor ##
###################################################################    
    #1. Select the continuous predictor
    output$xyplot_x <- renderUI({
      selectInput("XyplotX", "1. Select a continuous predictor.", 
                   choices = predictor(), selected=predictor()[1], multiple=FALSE)     
    })
    #Continuous predictor
    XyplotX1 <- reactive({                  
      input$XyplotX
    })
    #2. Select the factor for grouping
    output$xyplot_z <- renderUI({
      selectInput("XyplotZ", "2. Select a factor.", 
                  choices = setdiff(predictor(), XyplotX1() ),  multiple=FALSE)     
    })
    #Factor
    XyplotZ1 <- reactive({                  
      input$XyplotZ
    })
    #3. Make confidence bands for the plot
    output$xyplot_bands <- renderUI({                                 
      selectInput("XyplotBands", "3. Do you want confidence bands?", 
                  choices = c("No", "Yes"), multiple=FALSE, selected="No")
    })
    #Reactive function for directly above
    xyplot_Bands_YesNo <- reactive({                 
      input$XyplotBands 
    })
    #4. Select line colors
    output$xyplot_line_clrs <- renderUI({                                 
      selectInput("XypltLnClr", "4. Select line colors.", 
                  choices = xyplot_Line_Color_Names(), multiple=TRUE)     
    })
    #Reactive function for directly above
    xyplot_Line_Colors <- reactive({                 
      input$XypltLnClr 
    })
    #XYplot line color names
    xyplot_Line_Color_Names <- reactive({                 
      colors()[c(552, 498,652, 254, 26,547, 24, 152, 32,66, 68,97, 120,142,   
                 175, 310, 367,372,399, 485, 589, 615)]    
    })
    #5. Create yes/no box to make the XY plot
    output$xyplot_yes_no <- renderUI({                                 
      selectInput("XyplotYesNo", "5. Do you want to create the plot?", 
                  choices = c("No", "Yes"), multiple=FALSE, selected="No")     
    })
    #Reactive function for directly above
    xyplot_create_YesNo <- reactive({                 
      input$XyplotYesNo 
    })
    #Reactive function to get group levels
    xyplot_groups <- reactive({                 
      if(xyplot_create_YesNo() == "Yes") {
        unique(xyplotData()[, XyplotZ1()]) 
      }
    })
    #6. Select specific groups
    output$xyplot_grp_levs <- renderUI({                                 
      selectInput("xyplotGrpLvs", "6. Highlight specific groups?", 
                  choices = xyplot_groups(), multiple=TRUE)     
    })
    #Reactive function to get group levels
    xyplot_Group_Levels <- reactive({                 
      input$xyplotGrpLvs 
    })
    #7. Indicate lower limit of x-axis
    output$xyplot_Xlim1 <- renderUI({
      numericInput("xyplotLmX1", "7. Lower X-axis limit.",
                   value = xylm()$XMin, step = .1)
    })
    #7A. Reactive function for the variable
    xyplot_X_limit_1 <- reactive({
      input$xyplotLmX1
    })
    #8. Indicate upper limit of x-axis
    output$xyplot_Xlim2 <- renderUI({
      numericInput("xyplotLmX2", "8. Upper X-axis limit.",
                   value = xylm()$XMax, step = .1)
    })
    #8A. Reactive function for the variable
    xyplot_X_limit_2 <- reactive({
      input$xyplotLmX2
    })
    #9. Indicate lower limit of y-axis
    output$xyplot_Ylim1 <- renderUI({
      numericInput("xyplotLmY1", "9. Lower Y-axis limit.",
                   value = xylm()$YMin, step = .1)
    })
    #9A. Reactive function for the variable
    xyplot_Y_limit_1 <- reactive({
      input$xyplotLmY1
    })
    #10. Indicate upper limit of Y-axis
    output$xyplot_Ylim2 <- renderUI({
      numericInput("xyplotLmY2", "10. Upper Y-axis limit.",
                   value = xylm()$YMax, step = .1)
    })
    #10A. Reactive function for the variable
    xyplot_Y_limit_2 <- reactive({
      input$xyplotLmY2
    })
    #Place all XY limits in a list reactive function
    xylm_all <- reactive({
      if(xyplot_create_YesNo() == "Yes") {
        list( "XMin"=xyplot_X_limit_1(), "XMax"=xyplot_X_limit_2(),
              "YMin"=xyplot_Y_limit_1(),"YMax"=xyplot_Y_limit_2() )
      }
    })
    #Get the Predicted values needed for the plot
    xyplotData <- reactive({                 
      do.call("Predict", list(fit1(), XyplotX1(), XyplotZ1() )   ) 
    })
    #Set up function to get XY limits from an XYplot
    xylm_run <- reactive({
      if(xyplot_create_YesNo() == "Yes") {
      fncXYpltLmts(xyplotDF=xyplotData(), ContX=XyplotX1(), GroupX=XyplotZ1())
      }
    })
    #Get XY limits from an XYplot
    xylm <- reactive({                  
      xylm_run()
    })
    
    #Set up function to create the XYplot
    Xyplt_setup <- reactive({
      if(xyplot_create_YesNo() == "Yes") {
      fncXYpltInt(DF=df(), xyplotDF=xyplotData(), ContX= XyplotX1(), GroupX=XyplotZ1(), 
                  GroupLevs=xyplot_Group_Levels(), XYlims=xylm_all(), Clrs=xyplot_Line_Colors(), CIbands=xyplot_Bands_YesNo()) 
    }
    })
    #This creates the plot
    output$xYplot_interaction <- renderPlot({
      Xyplt_setup()
    }, height = 800)
    
  
    ################################################################################
    #                   Function to get X and Y limits for XY plot                 #
    ################################################################################
    fncXYpltLmts <- function(xyplotDF, ContX, GroupX) {
      #Get xlim and ylim values from generic plot
      p1 <- xYplot(xyplotDF[[which(names(xyplotDF) =="yhat")]] ~ xyplotDF[[which(names(xyplotDF) == ContX )]] ,
                   groups=xyplotDF[[which(names(xyplotDF) == GroupX )]], main="Partial prediction plot of " )
      XMin <- p1$x.limits[1]
      XMax <- p1$x.limits[2]
      YMin <- p1$y.limits[1]
      YMax <- p1$y.limits[2]
      return(list("XMin"=XMin, "XMax"=XMax, "YMin"=YMin, "YMax"=YMax))
    }
    
    ################################################################################
    #                   Function to create contrast XYplot                         #
    ################################################################################
    fncXYpltInt <- function(DF, xyplotDF, ContX, GroupX, GroupLevs, XYlims, Clrs, CIbands, ...) {
      #Create the X and Y limits
      XLim <- c(XYlims[["XMin"]], XYlims[["XMax"]])
      YLim <- c(XYlims[["YMin"]], XYlims[["YMax"]])
      #Run plots fo different scenarios  
      if( is.null(GroupLevs) ) {
        if(CIbands == "Yes") {
          xYplot( Cbind(xyplotDF[[which(names(xyplotDF) == "yhat")]], 
                        xyplotDF[[which(names(xyplotDF) == "lower")]], 
                        xyplotDF[[which(names(xyplotDF) == "upper")]]) ~ xyplotDF[[which(names(xyplotDF) == ContX)]],  
                  groups=xyplotDF[[which(names(xyplotDF) == GroupX)]],
                  method= 'filled bands', type= 'l', 
                  col.fill= adjustcolor(Clrs, alpha.f = 0.2),
                  lty= 1:length(unique(DF[, GroupX ])), col= Clrs, 
                  lwd= 3, ylab= "Yhat", xlab= ContX, cex= 1.75,
                  xlim=XLim, ylim=YLim,
                  main= paste0("Partial prediction plot of ", ContX, " by levels of ", GroupX) )
        } else {
          xYplot(xyplotDF[[which(names(xyplotDF) == "yhat")]] ~ xyplotDF[[which(names(xyplotDF) == ContX)]] ,  
                 groups= xyplotDF[[which(names(xyplotDF) == GroupX)]],
                 type= 'l', lty= 1:length(unique(DF[, GroupX ])), col= Clrs, 
                 lwd= 3, ylab= "Yhat", xlab= ContX, cex= 1.75, 
                 xlim=XLim, ylim=YLim,
                 main= paste0("Partial prediction plot of ", ContX, " by levels of ", GroupX) )
        } 
      }  else {
        if(CIbands == "Yes") {
          xYplot( Cbind(xyplotDF[[which(names(xyplotDF) == "yhat")]][ xyplotDF[, GroupX ] %in% GroupLevs ] , 
                        xyplotDF[[which(names(xyplotDF) == "lower")]][ xyplotDF[, GroupX ] %in% GroupLevs ] , 
                        xyplotDF[[which(names(xyplotDF) == "upper")]][ xyplotDF[, GroupX ] %in% GroupLevs ] ) ~ xyplotDF[[which(names(xyplotDF) == ContX)]][ xyplotDF[, GroupX ] %in% GroupLevs ],  
                  groups=xyplotDF[[which(names(xyplotDF) == GroupX)]][ xyplotDF[, GroupX ] %in% GroupLevs ],
                  method= 'filled bands', type= 'l', 
                  col.fill= adjustcolor(Clrs, alpha.f = 0.2),
                  lty= 1:length(unique(DF[, GroupX ])), col= Clrs, 
                  lwd= 3, ylab= "Yhat", xlab= ContX, cex= 1.75, 
                  xlim=XLim, ylim=YLim,
                  main= paste0("Partial prediction plot of ", ContX, " by levels of ", GroupX) )
        } else {
          xYplot(xyplotDF[xyplotDF[, GroupX ] %in% GroupLevs, which(names(xyplotDF) == "yhat")] ~ xyplotDF[xyplotDF[, GroupX ] %in% GroupLevs, which(names(xyplotDF) == ContX)] ,  
                 groups= xyplotDF[[which(names(xyplotDF) == GroupX)]][ xyplotDF[, GroupX ] %in% GroupLevs ],
                 type= 'l', lty= 1:length(unique(DF[, GroupX ])), col= Clrs, 
                 lwd= 3, ylab= "Yhat", xlab= ContX, cex= 1.75, 
                 xlim=XLim, ylim=YLim,
                 main= paste0("Partial prediction plot of ", ContX, " by levels of ", GroupX) )
        } 
      }
    }
     
    ##########################################
    # Contrast plots for partial predictions #
    ##########################################
    ## UI functions
    #Create the levels for the XY plot group argument
    #Level 1
    output$xyplot_con_lev1 <- renderUI({
      selectInput("xyplotConLev1", "1. Select the primary level",
                  choices = xy_contrast_levs() , multiple=FALSE, selected= xy_contrast_levs()[1])
    })
    #Level 2
    output$xyplot_con_lev2 <- renderUI({
      selectInput("xyplotConLev2", "2. Select the reference level",
                  choices = xy_contrast_levs() , multiple=FALSE,
                  selected= setdiff(xy_contrast_levs(), input$xyplotConLev1)[1] )
    })
    #Yes/No on running the plots
    output$xyp_yes_no <- renderUI({ #Same idea as output$vy
      selectInput("XypYesNo", "3. Do you want to run the contrast plot?",
                  choices = c("No", "Yes"), multiple=FALSE,
                  selected="No")     #Will make choices based on my reactive function.
    })
    #Create the x-axis limits for the XY plot group argument
    #Lower X
    output$xyplot_con_xlim0 <- renderUI({
      if (input$XyplotYesNo=="Yes") {
        numericInput("xyplotConXlim0", "4. Select the lower x-axis limit",
                     value=con_xy_data()[["xlim0"]], step=1)
      }
    })
    #Upper X
    output$xyplot_con_xlim1 <- renderUI({
      if (input$XyplotYesNo=="Yes") {
        numericInput("xyplotConXlim1", "5. Select the upper x-axis limit",
                     value=con_xy_data()[["xlim1"]], step=1)
      }
    })
    #Create the y-axis limits for the XY plot group argument
    #Lower
    output$xyplot_con_ylim0 <- renderUI({
      if (input$XyplotYesNo=="Yes") {
        numericInput("xyplotConYlim0", "6. Select the lower y-axis limit",
                     #value=0, step=1)
                     value=con_xy_data()[["ylim0"]], step=1)
      }
    })
    #Upper
    output$xyplot_con_ylim1 <- renderUI({
      if (input$XyplotYesNo=="Yes") {
        numericInput("xyplotConYlim1", "7. Select the upper y-axis limit",
                     #value=1, step=1)
                     value=con_xy_data()[["ylim1"]], step=1)
      }
    })
    ## Create the contrast xyplot
    output$xyplot_contrast_plot <- renderPlot({
      if (input$XypYesNo=="Yes") {
        xyp_contrast()
      }
    }, height = 700)
    
    
    #Reactive functions
    #Creates unique levels for the grouping factor in the xyplot
    xy_contrast_levs <- reactive({
      unique(df()[, XyplotZ1()])
    })
    
    ######################################
    ##  Function to make contrast data  ##
    ######################################
    contrastXyDataFnc <- function(model, X, group, lev1, lev2, reg) {
      #Specs to get min and max limit for X
      spcs <- specs(model, long=TRUE)
      x_min <- spcs[[ "limits"]][rownames(spcs[[ "limits"]]) == "Low", which(colnames(spcs[[ "limits"]]) == X )]
      x_max <- spcs[[ "limits"]][rownames(spcs[[ "limits"]]) == "High", which(colnames(spcs[[ "limits"]]) == X )]
      #y label
      if(reg %in% c("Cox PH", "Cox PH with censoring")) {
        xyplot_ylab <- paste0("Hazard Ratio (",lev1, ":", lev2,")")
      }
      if(reg %in% c("AFT","AFT with censoring")) {
        xyplot_ylab <- paste0("Survival Time Ratio (",lev1, ":", lev2,")")
      }
      if(reg %in% c("Logistic", "Ordinal Logistic") ) {
        xyplot_ylab <- paste0("Odds Ratio (",lev1, ":", lev2,")")
      }
      if(reg %in% c("Linear","Poisson","Quantile","Generalized Least
                    Squares")) {
        xyplot_ylab <- paste0("Contrast (",lev1, ":", lev2,")")
    }
      ## Determine if it is an abline at 0 or 1 ##
      if(reg %in% c("Cox PH", "Cox PH with censoring","Logistic", "Ordinal Logistic","AFT","AFT with censoring") ) {
        abline_01 <- 1
      }
      if(reg %in% c("Linear","Poisson","Quantile","Generalized Least
                    Squares")) {
        abline_01 <- 0
    }
      #Put elements int a list
      a <- list(lev1, seq(x_min, x_max, length.out=250))
      names(a) <- c(group, X)
      b <- list(lev2, seq(x_min, x_max, length.out=250))
      names(b) <- c(group, X)
      #w <- do.call("contrast", list( fit=model, a=a, b=b) )
      w <- contrast( fit=model, a=a, b=b)
      
      ## Exponentiate the data if needed ##
      #Contrast
      if(reg %in% c("Cox PH", "Cox PH with censoring","Logistic", "Ordinal Logistic","Poisson","AFT","AFT with censoring")) {
        w[["Contrast"]] <- exp(w[["Contrast"]])
      }
      #Lower
      if(reg %in% c("Cox PH", "Cox PH with censoring","Logistic", "Ordinal Logistic","Poisson","AFT","AFT with censoring")) {
        w[["Lower"]] <- exp(w[["Lower"]])
      }
      #Upper
      if(reg %in% c("Cox PH", "Cox PH with censoring","Logistic", "Ordinal Logistic","Poisson","AFT","AFT with censoring")) {
        w[["Upper"]] <- exp(w[["Upper"]])
      }
      
      #Get the upper and lower limits of the Y-axis
      ylim0 <- min(w[["Lower"]])
      ylim1 <- max(w[["Upper"]])
      return(list(w=w, xyplot_ylab=xyplot_ylab, abline_01=abline_01,
                  xlim0=x_min, xlim1=x_max, ylim0=ylim0, ylim1=ylim1))
      }
    ## Get the contrast data
    con_xy_data <- reactive({
      if (input$XyplotYesNo=="Yes") {
        contrastXyDataFnc(model=fit1(), X= XyplotX1(), group=XyplotZ1(),
                          lev1=input$xyplotConLev1, lev2=input$xyplotConLev2, reg=input$regress_type )
      }
    })
    ## Function to run contrast xyplot
    xypContrastFnc <- function(data, xlim0, xlim1, ylim0, ylim1, X, Z, Lev1, Lev2) {
      xYplot(Cbind(data[["w"]][["Contrast"]], data[["w"]][["Lower"]],
                   data[["w"]][["Upper"]]) ~ data[["w"]][[X]] ,
             ylab=data[["xyplot_ylab"]], type='l', method='filled bands', abline=list(h=data[["abline_01"]], col=2,lwd=2),
             col.fill=gray(.95), xlim=c(xlim0, xlim1) , ylim=c(ylim0, ylim1), xlab=X, lwd=2,
             main=paste0("Partial prediction plot of ", X, " contrasting ", Z,  
                         " levels of ", Lev1, " to ", Lev2 ),
             sub=paste0(Z," effect: ",Lev1))
    }
    
    #Run the plot
    xyp_contrast <- reactive({
      if (input$XypYesNo=="Yes") {
        xypContrastFnc(data=con_xy_data(), xlim0=input$xyplotConXlim0, xlim1=input$xyplotConXlim1,
                       ylim0=input$xyplotConYlim0, ylim1=input$xyplotConYlim1, 
                       X=XyplotX1(), Z=XyplotZ1() , Lev1=input$xyplotConLev1, Lev2=input$xyplotConLev2)
      }
    })
    
    ## Table of contrast at percentiles to show where the interaction occurs ##
    #Function that gets contrasts quantiles
    contrastQuantFnc <- function(w, sp1, X) {
      p10 <- which(abs(w[[X]] - sp1[["limits"]][[X]][rownames(sp1$limits) == "Low:prediction"]) == min(abs(w[[X]] - sp1[["limits"]][[X]][rownames(sp1$limits) == "Low:prediction"])))[1]
      p25 <- which(abs(w[[X]] - sp1[["limits"]][[X]][rownames(sp1$limits) == "Low:effect"]) == min(abs(w[[X]] - sp1[["limits"]][[X]][rownames(sp1$limits) == "Low:effect"])))[1]
      p50 <- which(abs(w[[X]] - sp1[["limits"]][[X]][rownames(sp1$limits) == "Adjust to"]) == min(abs(w[[X]] - sp1[["limits"]][[X]][rownames(sp1$limits) == "Adjust to"])))[1]
      p75 <- which(abs(w[[X]] - sp1[["limits"]][[X]][rownames(sp1$limits) == "High:effect"]) == min(abs(w[[X]] - sp1[["limits"]][[X]][rownames(sp1$limits) == "High:effect"])))[1]
      p90 <- which(abs(w[[X]] - sp1[["limits"]][[X]][rownames(sp1$limits) == "High:prediction"]) == min(abs(w[[X]] - sp1[["limits"]][[X]][rownames(sp1$limits) == "High:prediction"])))[1]
      #Put the values in a vector
      contrast_quant <- c(p10,p25,p50,p75,p90)
      #Bind the rows of data I need "Contrast"	 "SE"	 "Lower"	 "Upper"	 "Z"	 "Pvalue"
      con_quan_df <- cbind( round(w[[X]][contrast_quant], 2), round(w[["Contrast"]][contrast_quant], 2),
                            #round(w[["SE"]][contrast_quant], 2), #Dropping this inappropriate exp(SE)
                            round(w[["Lower"]][contrast_quant], 2),
                            round(w[["Upper"]][contrast_quant], 2), round(w[["Z"]][contrast_quant], 2),
                            round(w[["Pvalue"]][contrast_quant], 4))
      #Column names
      colnames(con_quan_df) <- c(X,"Contrast","Lower","Upper","Z","Pvalue")
      #Row names
      rownames(con_quan_df) <- c("10th","25th","50th","75th","90th")
      return(con_quan_df)
    }
    
    #Function that runs the function above
    contrastQuantTbl <- reactive({
      if (input$XypYesNo=="Yes") {
        contrastQuantFnc(w=con_xy_data()[["w"]], sp1=specs(fit1(), long=TRUE), X=XyplotX1())
      }
    })
    #Run function for the Quantile table for contrasts
    output$contrast_quant_table <- renderTable({
      if (input$XypYesNo=="Yes") {
        contrastQuantTbl()
      }
    }, rownames = TRUE)
    
    ##############    
    ## Nomogram ##
    ##############    
#This plots the predicted values as a nomogram    

##Select the times for probability of survival
#1. Revise plot layout
output$nomo_prob_surv_time <- renderUI({
  textInput("nomoPrbSrvTm", "1. Probability of survival to:", 
            value= "c(1,2)" )     
})
#1A. Object with new layout
nomogram_prob_survival_time <- reactive({
  eval(parse(text=input$nomoPrbSrvTm ))
})

#2. Divider value to transform the survival time for "med" and "mmeant" (e.g., time/12=Years)
output$nomo_trans_time_denom <- renderUI({  
#  if(nomogram_yes() == "Yes") {
    numericInput("nomoTrnsTmDnm", "2. Transformation time (e.g., 36/12= 3 years)",
                 min=1, step=1, value=1)
#  }
})
#2A. Object with multiplier value
nomo_transformation_time_denom <- reactive({
  if(nomogram_yes() == "Yes") {
    input$nomoTrnsTmDnm
  }
})

#Select the function at times (matches with "med" and "mmeant") or use .05 and .95 quantiles
#3. Revise plot layout
output$nomo_pred_surv_time_xaxis <- renderUI({
  textInput("nomoPrdSrvTmXaxis", "3. Median and mean survival X-axis times:", 
            value= "c(1,2)" )     
})
#3A. Object with new layout
nomo_pred_surv_time_xaxis <- reactive({
  if(nomogram_yes() == "Yes") {
  eval(parse(text=input$nomoPrdSrvTmXaxis ))
  }
})

#For labels
#4. Select numerical amount for labels (e.g., 3 and 5 years)
output$nomo_surv_time_prob_vals <- renderUI({
  textInput("nomoSrvTimePrVls", "4. Select label values for survival times.", 
            value= "c(1,2)" )     
})
#4A. Object with new layout
nomo_survival_time_prob_values <- reactive({
  if(nomogram_yes() == "Yes") {
  eval(parse(text=input$nomoSrvTimePrVls ))
  }
})

#Select the time frame (select once when both time frames are identical e.g., 3 year and 5 year)
#For labels
#5. Select numerical amount for labels (e.g., 3 and 5 years)
output$nomo_surv_time_prob_pers <- renderUI({
  selectInput("nomoSrvTimePrbPrs", "5. Select label time period",
              choices = c("hour", "day", "week", "month", "year"), multiple=FALSE )
})
#5A. Object with new layout
nomo_survival_time_prob_periods <- reactive({
#  eval(parse(text=input$nomoSrvTimePrVls ))
  if(nomogram_yes() == "Yes") {
  input$nomoSrvTimePrbPrs
  }
})

#6. Transition specific formulas
output$nomo_up_Fmla <- renderUI({
  textInput("nomoUpFmla", "6. Update nomogram formula", 
  #value= deparse(nomo_fmla_output(), width.cutoff=500 )  )     
  value= nomo_fmla_output()  )     
})
#6B. Make full regression call
#nomo_first_formula <- reactive({
#  paste0("coxph(list(",  deparse(ms_cph_mdl_fmla(), width.cutoff=500 ),  ",), data=", input$msDfInputName,", ", "id=", 
#         cph_time_fmla_id(), ", model=TRUE" , ")")
#})
#6A. Object with formula
nomo_update_formula <- reactive({
  parse(text=sub("expression", "", input$nomoUpFmla))
})

#7. Create yes/no box to determine plot single partial effect
output$nomo_yes <- renderUI({                                 #Same idea as output$vy
  selectInput("nomoYes", "7. Do you want to create a nomogram?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     #Will make choices based on my reactive function.
})
#7A. Object with new layout
nomogram_yes <- reactive({
  input$nomoYes
})

#8. Run function to get nomogram formula
get_nomo_fmla <- reactive({
#  if (nomogram_yes() =="Yes") {
    fncNomo(RegType=input$regress_type)
#  }
})
#8A. Run the data function
nomo_fmla_output <- reactive({
#  if (nomogram_yes() =="Yes") {
    get_nomo_fmla()
#  }
})

#9. Get nomogram output
get_nomo_output <- reactive({
  if (nomogram_yes() =="Yes") {
    fncNomoOutput(Nom=nomo_update_formula(), Fit=fit1(), PrbSrvTm=nomogram_prob_survival_time(), TrnSrvTm=nomo_transformation_time_denom(), 
                MdMnSrvTm=nomo_pred_surv_time_xaxis(), Time_label=nomo_survival_time_prob_values(), 
                Time_label_period=nomo_survival_time_prob_periods(), RegType=input$regress_type)
  }
})


#10. Run plot
output$nomo_gram <- renderPlot({
  if(nomogram_yes() == "Yes") {
    plot(get_nomo_output())
  } else {
    plot(nomogram(fit1()))
  }
}, height = 800)

#11. Print nomogram results
output$nomogram_smry <- renderPrint({ 
  if(nomogram_yes() == "Yes") {
    get_nomo_output()
  } else {
    nomogram(fit1())
  }
})


## Functions ##
#Function that creates nomogram expression
#fncNomo <- function(Fit, PrbSrvTm, TrnSrvTm, MdMnSrvTm, Time_label,RegType)  {
fncNomo <- function(RegType)  {
  nom <- switch(RegType,                
                "Linear"   = expression(nomogram(Fit)), 
                "Logistic" = expression(nomogram(Fit, fun=plogis)),
                "Ordinal Logistic"  = expression(nomogram(Fit, fun=plogis)),
                "Poisson"  = expression(nomogram(Fit, fun=exp)),
                "Quantile" = expression(nomogram(Fit)),
                "Cox PH"   = expression(nomogram(Fit, 
                                                 fun=list(surv1, surv2, mnt, med),
                                                 funlabel=c(paste0(Time_label[1], "-", Time_label_period, " Survival"),
                                                            paste0(Time_label[2], "-", Time_label_period, " Survival"),
                                                            paste0('Mean Survival Time (', Time_label_period, 's)'),
                                                            paste0('Median Survival Time (', Time_label_period, 's)')),
                                                 fun.at=list(ss, ss, MdMnSrvTm, MdMnSrvTm)   )),
                "Cox PH with censoring"  =   expression(nomogram(Fit, 
                                                                 fun=list(surv1, surv2, mnt, med),
                                                                 funlabel=c(paste0(Time_label[1], "-", Time_label_period, " Survival"),
                                                                            paste0(Time_label[2], "-", Time_label_period, " Survival"),
                                                                            paste0('Mean Survival Time (', Time_label_period, 's)'),
                                                                            paste0('Median Survival Time (', Time_label_period, 's)')),
                                                                 fun.at=list(ss, ss, MdMnSrvTm, MdMnSrvTm)   )),
                "AFT"  = expression(nomogram(Fit, 
                                             fun=list(surv1, surv2, mnt, med),
                                             funlabel=c(paste0(Time_label[1], "-", Time_label_period, " Survival"),
                                                        paste0(Time_label[2], "-", Time_label_period, " Survival"),
                                                        paste0('Mean Survival Time (', Time_label_period, 's)'),
                                                        paste0('Median Survival Time (', Time_label_period, 's)')),
                                             fun.at=list(ss, ss, MdMnSrvTm, MdMnSrvTm)   )),
                "AFT with censoring" = expression(nomogram(Fit, 
                                                           fun=list(surv1, surv2, mnt, med),
                                                           funlabel=c(paste0(Time_label[1], "-", Time_label_period, " Survival"),
                                                                      paste0(Time_label[2], "-", Time_label_period, " Survival"),
                                                                      paste0('Mean Survival Time (', Time_label_period, 's)'),
                                                                      paste0('Median Survival Time (', Time_label_period, 's)')),
                                                           fun.at=list(ss, ss, MdMnSrvTm, MdMnSrvTm)   )),
                "Generalized Least Squares" = expression(nomogram(Fit)) )
  return(list("Nomogram"=nom  ))
  
}

#Function that creates nomogram
fncNomoOutput <- function(Nom, Fit, PrbSrvTm, TrnSrvTm, MdMnSrvTm, Time_label,Time_label_period,RegType)  {
  #Survival probability labeling 
  ss    <- c(0,.05,.1,.2,.3,.4,.5,.6,.7,.8,.9,.95, 1)
  #Survival function  
  surv  <- switch(RegType,                
                  "Linear"   = NA , 
                  "Logistic" = NA ,
                  "Ordinal Logistic"  = NA ,
                  "Poisson"  = NA ,
                  "Quantile" = NA ,
                  "Cox PH"   =  Survival(Fit),
                  "Cox PH with censoring"  =  Survival(Fit),
                  "AFT"  =  Survival(Fit),
                  "AFT with censoring" =  Survival(Fit),
                  "Generalized Least Squares" =  NA )
  #Survival at time 1 
  surv1  <- switch(RegType,                
                   "Linear"   = NA , 
                   "Logistic" = NA ,
                   "Ordinal Logistic"  = NA ,
                   "Poisson"  = NA ,
                   "Quantile" = NA ,
                   "Cox PH"   =  function(x) surv(PrbSrvTm[1], lp=x),
                   "Cox PH with censoring"  =  function(x) surv(PrbSrvTm[1], lp=x),
                   "AFT"  =  function(x) surv(PrbSrvTm[1], lp=x),
                   "AFT with censoring" =  function(x) surv(PrbSrvTm[1], lp=x),
                   "Generalized Least Squares" =  NA )
  #Survival at time 2 
  surv2  <- switch(RegType,                
                   "Linear"   = NA , 
                   "Logistic" = NA ,
                   "Ordinal Logistic"  = NA ,
                   "Poisson"  = NA ,
                   "Quantile" = NA ,
                   "Cox PH"   =  function(x) surv(PrbSrvTm[2], lp=x),
                   "Cox PH with censoring"  =  function(x) surv(PrbSrvTm[2], lp=x),
                   "AFT"  =  function(x) surv(PrbSrvTm[2], lp=x),
                   "AFT with censoring" =  function(x) surv(PrbSrvTm[2], lp=x),
                   "Generalized Least Squares" =  NA )
  #Quantile function 
  quan  <- switch(RegType,                
                  "Linear"   = NA , 
                  "Logistic" = NA ,
                  "Ordinal Logistic"  = NA ,
                  "Poisson"  = NA ,
                  "Quantile" = NA ,
                  "Cox PH"   = Quantile(Fit),
                  "Cox PH with censoring"  = Quantile(Fit),
                  "AFT"  =  Quantile(Fit),
                  "AFT with censoring" = Quantile(Fit),
                  "Generalized Least Squares" =  NA )
  #Median function 
  med  <- switch(RegType,                
                 "Linear"   = NA , 
                 "Logistic" = NA ,
                 "Ordinal Logistic"  = NA ,
                 "Poisson"  = NA ,
                 "Quantile" = NA ,
                 "Cox PH"   = function(x) quan(lp=x)/TrnSrvTm,
                 "Cox PH with censoring"  = function(x) quan(lp=x)/TrnSrvTm,
                 "AFT"  =  function(x) quan(lp=x)/TrnSrvTm,
                 "AFT with censoring" = function(x) quan(lp=x)/TrnSrvTm,
                 "Generalized Least Squares" =  NA )
  #Mean function 
  meant  <- switch(RegType,                
                   "Linear"   = NA , 
                   "Logistic" = NA ,
                   "Ordinal Logistic"  = NA ,
                   "Poisson"  = NA ,
                   "Quantile" = NA ,
                   "Cox PH"   = Mean(Fit),
                   "Cox PH with censoring"  = Mean(Fit),
                   "AFT"  =  Mean(Fit),
                   "AFT with censoring" = Mean(Fit),
                   "Generalized Least Squares" =  NA )
  #Mean time function 
  mnt  <- switch(RegType,                
                 "Linear"   = NA , 
                 "Logistic" = NA ,
                 "Ordinal Logistic"  = NA ,
                 "Poisson"  = NA ,
                 "Quantile" = NA ,
                 "Cox PH"   = function(x) meant(lp=x)/TrnSrvTm,
                 "Cox PH with censoring"  = function(x) meant(lp=x)/TrnSrvTm,
                 "AFT"  =  function(x) meant(lp=x)/TrnSrvTm,
                 "AFT with censoring" = function(x) meant(lp=x)/TrnSrvTm,
                 "Generalized Least Squares" =  NA )
  #Plot nomogram
  Nomogram.Output  <- switch(RegType,                
         "Linear"   = eval(Nom) , 
         "Logistic" = eval(Nom) ,
         "Ordinal Logistic"  = eval(Nom) ,
         "Poisson"  = eval(Nom) ,
         "Quantile" = eval(Nom) ,
         "Cox PH"   = eval(Nom) ,
         "Cox PH with censoring"  = eval(Nom) ,
         "AFT"  = eval(Nom) ,
         "AFT with censoring" = eval(Nom) ,
         "Generalized Least Squares" =  eval(Nom) )
  return("Nomogram.Output"=Nomogram.Output)
}


## Calibration ##
output$calibrate_type <- renderUI({                                #Creates a UI function here but it will
  selectInput("caliType", "1. Select the calibration method",
              choices = c("crossvalidation", "boot", ".632", "randomization"),
              selected= "crossvalidation", multiple=FALSE)
})

#This allows us to select the number of k-fold crossvalidation groups or number of bootstraps for the calibration tab
output$calibrate_B_arg_n <- renderUI({
  numericInput("cali_B_n", "2. Crossvalidation k-folds or Bootstrap #", value = 10, min=2)
})

#3. Pick a time for survival models
output$calibrate_surv_time <- renderUI({
  numericInput("calSrvTM", "3. Select a time for survival models", value= as.numeric(describeY()[["counts"]][10]), 
               min=as.numeric(describeY()[["extremes"]][1]), max=as.numeric(describeY()[["extremes"]][10]), step=1)
})

#4. Pick a time for survival models
output$calibrate_surv_quan_n <- renderUI({
  numericInput("calSrvQnN", "4. Select quantile N for survival models", value= 5, min=1, max=100, step=1) 
})
#4A. Quantile N object
calibrate_survival_quantile_n <- reactive({
  input$calSrvQnN
})

#Asks if you did multiple imputation.
output$MIForCali <- renderUI({  
  selectInput("MI_for_cali", "5. Did you use Multiple Imputation?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     #Will make choices based on my reactive function.
})

#Determine if we should begin the calibration.
output$BeginCalibrate <- renderUI({  
  selectInput("begin_cali", "6. Begin calibration?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     #Will make choices based on my reactive function.
})

#This creates a calibration curve    
output$cali_brate <- renderPlot({
  if (input$MI_for_cali == "No") {
    
  if (input$begin_cali == "Yes") {
  set.seed(1)
  if (input$caliType == "boot") {
    plot(calibrate(fit1(), B=input$cali_B_n, u=input$calSrvTM, method="boot"), subtitles=TRUE)
  }
  if (input$caliType == "crossvalidation") {
    plot(calibrate(fit1(), B=input$cali_B_n, u=input$calSrvTM, method="crossvalidation"), subtitles=TRUE)
  }
  if (input$caliType == ".632") {
    plot(calibrate(fit1(), u=input$calSrvTM, method=".632", B=input$cali_B_n), subtitles=TRUE)
  }
  if (input$caliType == "randomization") {
    plot(calibrate(fit1(),  u=input$calSrvTM, method="randomization", B=input$cali_B_n), subtitles=TRUE)
  }
#Add validation of right censored data
      set.seed(1)
      if (input$regress_type %in%  c("Cox PH","Cox PH with censoring","AFT","AFT with censoring")) {
        plot(calibrate(fit1(), B=input$cali_B_n, u=input$calSrvTM, m=floor(length(fit1()$linear.predictors) /calibrate_survival_quantile_n()), 
                       cmethod='KM'), add=TRUE)
      }
  }
  } 
  ######
  if (input$MI_for_cali == "Yes") {
    
    if (input$begin_cali == "Yes") {
      set.seed(1)
      if (input$caliType == "boot") {
        plot(calibrate(fit.si(), B=input$cali_B_n, u=input$calSrvTM, method="boot"), subtitles=TRUE)
      }
      if (input$caliType == "crossvalidation") {
        plot(calibrate(fit.si(), B=input$cali_B_n, u=input$calSrvTM, method="crossvalidation"), subtitles=TRUE)
      }
      if (input$caliType == ".632") {
        plot(calibrate(fit.si(), u=input$calSrvTM, method=".632", B=input$cali_B_n), subtitles=TRUE)
      }
      if (input$caliType == "randomization") {
        plot(calibrate(fit.si(),  u=input$calSrvTM, method="randomization", B=input$cali_B_n), subtitles=TRUE)
      }
      #Add validation of right censored data
      set.seed(1)
      if (input$regress_type %in%  c("Cox PH","Cox PH with censoring","AFT","AFT with censoring")) {
        plot(calibrate(fit.si(), B=input$cali_B_n, u=input$calSrvTM, m=floor( length(fit.si()$linear.predictors) /calibrate_survival_quantile_n()), 
                       cmethod='KM'), add=TRUE)
      }
    }
  }
  
}, height = 800)


#Select the validation method, "boot" is slow
output$validate_type <- renderUI({                                #Creates a UI function here but it will
  selectInput("valiType", "1. Select the validation method",
              choices = c("crossvalidation", "boot", ".632", "randomization"),
              selected= "crossvalidation", multiple=FALSE)
})

#This allows us to select the number of k-fold crossvalidation groups or number of bootstraps for the validation tab
output$validate_B_arg_n <- renderUI({
  numericInput("vali_B_n", "2. Crossvalidation k-folds or Bootstrap #", value = 10, min=2)
})

#Asks if you did multiple imputation.
output$MIForVali <- renderUI({  
  selectInput("MI_for_vali", "3. Did you do Multiple Imputation?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     #Will make choices based on my reactive function.
})

#Determine if we should begin the calibration.
output$BeginValidate <- renderUI({  
  selectInput("begin_vali", "4. Begin validation?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     #Will make choices based on my reactive function.
})

output$vali_date <- renderPrint({
  if (input$MI_for_vali == "No") {
    
    if (input$begin_vali == "Yes") {
      set.seed(1)
      if (input$valiType == "boot") {
        print(rms:::validate(fit1(), B=input$vali_B_n, method="boot", bw=TRUE), digits=3, B=50)
      }
      if (input$valiType == "crossvalidation") {
        print(rms:::validate(fit1(), B=input$vali_B_n, method="crossvalidation"), digits=3)
      }
      if (input$valiType == ".632") {
        print(rms:::validate(fit1(),  method=".632", B=input$vali_B_n), digits=3)
      }
      if (input$valiType == "randomization") {
        print(rms:::validate(fit1(),  method="randomization", B=input$vali_B_n), digits=3)
      }
    }
  }
  ##########
  if (input$MI_for_vali == "Yes") {
    
    if (input$begin_vali == "Yes") {
      set.seed(1)
      if (input$valiType == "boot") {
        print(rms:::validate(fit.si(), B=input$vali_B_n, method="boot", bw=TRUE), digits=3, B=50)
      }
      if (input$valiType == "crossvalidation") {
        print(rms:::validate(fit.si(), B=input$vali_B_n, method="crossvalidation"), digits=3)
      }
      if (input$valiType == ".632") {
        print(rms:::validate(fit.si(),  method=".632", B=input$vali_B_n), digits=3)
      }
      if (input$valiType == "randomization") {
        print(rms:::validate(fit.si(),  method="randomization", B=input$vali_B_n), digits=3)
      }
    }
  }
})


################
# Test section #
################
## Reactive functions ## 
#Function that creates predicted values based on model fit
## ORIGINAL CODE--CHANGE IT BACK TO THIS IF THERE IS AN ISSUE ##
#rms_func <- reactive({
#  Function(fit1())
#})
rms_func <- reactive({
  if (input$uprf =="Yes") {
    edit(Function(fit1()))      #This line allows me to  update my Function
  } else
  Function(fit1())
})


#Creates a small data frame of just my inputs, will use it to create yhat scores
xdf <- reactive({
  xdf <- data.frame(df()[, predictor(), drop=FALSE])  #The drop argument allows me to show 1 predictor
})

#This creates the function to gather data element  characteristics for the model (e.g., names, mean) 
vlfnc <- function(xdf) {
  v_ls <- list()     #List of different elements
  #For loops that goes through all of the columns, should use predictors()
  for(i in 1:ncol(xdf)) {
    v_ls$levs[[i]] <- length(unique(xdf[,i]))
    v_ls$rng[[i]]  <- c(sort(xdf[,i])[1], tail(sort(xdf[,i]), 1)) 
    v_ls$cls[[i]]  <- class(xdf[,i])
    v_ls$mn[[i]]   <- mean(as.numeric(as.character(xdf[, i])), na.rm=T)
    v_ls$sd[[i]]   <- sd(as.numeric(as.character(xdf[, i])), na.rm=T)
    #v_ls$pdf[[i]]  <- table(xdf[, i])/sum(table(xdf[, i]))
    #v_ls$pdf[[i]]  <- as.vector(table(xdf[, i])/sum(table(xdf[, i])))
    v_ls$pdf[[i]]  <- as.vector(prop.table(table(xdf[, i])))
    names(v_ls$pdf[[i]]) <- names(prop.table(table(xdf[, i])))
    v_ls$cnm[[i]] <- colnames(xdf)[i]  #Seems to have an issue with a 1 column data frame. Tried
                                       #to change it to names(), didn't help. I get MC data ok, but 
                                       #it gives it the name as the command
    v_ls$typ[[i]]  <- typeof(xdf[,i])  #This is used to work with 'labelled' class in RMS data
#    v_ls$pdf_lbl[[i]]  <- sort(as.character(unique(xdf[,i]))[!is.na(as.character(unique(xdf[,i])))])  #Gives me the correct PDF value labels, removes "NA"    names(v_ls$mn)[[i]] <- colnames(xdf)[i]
    v_ls$pdf_lbl[[i]]  <- names(v_ls$pdf[[i]])
    names(v_ls$mn)[[i]] <- colnames(xdf)[i]
    names(v_ls$sd)[[i]] <- colnames(xdf)[i]
    names(v_ls$pdf)[i] <- colnames(xdf)[i]
    v_ls$cdf[[i]]  <- as.vector(cumsum(table(xdf[, i])/sum(table(xdf[, i]))))
  }
  return(v_ls)  
}

#This is a reactive function that uses my homemade fucntion above off of the xdf data
vls1 <- reactive({
  vlfnc(xdf()) 
})


#1
## Function to produce the simulated data sets in various steps
mc_arg_fnc <- function(LEVS, CLS, MN, SD, TYP, NMS) {
  #Input distribution
  input_dist <- list()
  input_dist <- ifelse(LEVS <= 2 & CLS %in% c("numeric", "integer", "logical"), "rbinom", NA) 
  input_dist <- ifelse(LEVS  > 2 & CLS %in% c("numeric", "integer"), "rnorm", input_dist) 
  input_dist <- ifelse(LEVS >= 2 & CLS %in% c("character", "factor"), "runif", input_dist)
  #The next 2 lines allows me to use Harrell's data which has a class of "labelled" for some variables
  input_dist <- ifelse(is.na(input_dist) & CLS == 'labelled' & TYP %in% c("double", "integer"), "rnorm", input_dist) 
  input_dist <- ifelse(is.na(input_dist) & CLS == 'labelled' & TYP == "character", "runif", input_dist) 
  names(input_dist) <- NMS
  
  #Use this to create vectors to identify arguments
  input_arg2 <- list()
  input_arg2 <- ifelse(LEVS <= 2 & CLS %in% c("numeric", "integer", "logical"), 1, NA)       #for the rbinom distribution
  input_arg2 <- ifelse(LEVS  > 2 & CLS %in% c("numeric", "integer"), MN, input_arg2)         #for the rnorm distribution
  input_arg2 <- ifelse(LEVS >= 2 & CLS %in% c("character", "factor"), 0, input_arg2)         #for the runif distribution
  #The next 2 lines allows me to use Harrell's data which has a class of "labelled" for some variables
  input_arg2 <- ifelse(is.na(input_arg2) & CLS == 'labelled' & TYP %in% c("double", "integer"), MN, input_arg2) 
  input_arg2 <- ifelse(is.na(input_arg2) & CLS == 'labelled' & TYP == "character", 0, input_arg2) 
  
  #3rd argument of random distribution (e.g., prob, sd, max)
  input_arg3 <- list()
  input_arg3 <- ifelse(LEVS <= 2 & CLS %in% c("numeric", "integer", "logical"), MN, NA)       #for the rbinom distribution 
  input_arg3 <- ifelse(LEVS  > 2 & CLS %in% c("numeric", "integer"), SD, input_arg3)          #for the rnorm distribution 
  input_arg3 <- ifelse(LEVS >= 2 & CLS %in% c("character", "factor"), 1, input_arg3)          #for the runif distribution
  #The next 2 lines allows me to use Harrell's data which has a class of "labelled" for some variables
  input_arg3 <- ifelse(is.na(input_arg3) & CLS == 'labelled' & TYP %in% c("double", "integer"), SD, input_arg3) 
  input_arg3 <- ifelse(is.na(input_arg3) & CLS == 'labelled' & TYP == "character", 1, input_arg3) 
  return(list(input_dist=input_dist, input_arg2=input_arg2, input_arg3=input_arg3))
}

#1A
#Reactive function that runs mc_arg_fnc()
mc_arg_fnc1 <- reactive({
  mc_arg_fnc(LEVS=vls2()[["levs"]], CLS=vls2()[["cls"]], MN=vls2()[["mn"]], SD=vls2()[["sd"]], TYP=vls2()[["typ"]],
             NMS=vls2()[["cnm"]]) 
})

#2
#Function that creates the object for the arguments and converts it to a data frame
mc_sim_fnc <- function(input_dist, NSIM, input_arg2, input_arg3) {
  mc_sim <- data.frame(input_dist, NSIM, input_arg2, input_arg3)
  mc_sim[ ,1] <- as.character(mc_sim[ ,1])   #Changes this to a character instead of the problematic factor
  mc_sim <- as.data.frame(mc_sim)
  return(mc_sim)
}

#2A
#Reactive function that runs mc_sim_fnc() above
mc_sim_fnc1 <- reactive({
  mc_sim_fnc(input_dist=mc_arg_fnc1()[[1]], NSIM=input$n_sim, 
             input_arg2=mc_arg_fnc1()[[2]], input_arg3=mc_arg_fnc1()[[3]])
})

#2B 
#Removes unnecessary values to make sure there are only 4 columns in the data
mc_sim_fix_fnc1 <- function(McSimDf) {
  mc_sim_fnc1_arg2 <- vector(mode="list", length=nrow(McSimDf))
  mc_sim_fnc1_arg3 <- vector(mode="list", length=nrow(McSimDf))
  
  for (i in 1:nrow(McSimDf)) {
    mc_sim_fnc1_arg2[[i]] <- McSimDf[i, 2 + i]
    mc_sim_fnc1_arg3[[i]] <- McSimDf[i, 2 + i + nrow(McSimDf)]
  }  
  #I added the if()/else() commands to to fix the problem with sole character/factor X crashing Monte Carlo 
  ALLrunif <- all(McSimDf$input_dist == "runif") #Check to see if all distributions are "runif"
  if ( ALLrunif == FALSE ) {
    mcSimFixLs <- cbind("arg2"=unlist(mc_sim_fnc1_arg2), "arg3"=unlist(mc_sim_fnc1_arg3))
  } else {
    mcSimFixLs <- cbind("arg2"= McSimDf$input_arg2, "arg3"=McSimDf$input_arg3)
  } 
#  mcSimFixLs <- cbind("arg2"=unlist(mc_sim_fnc1_arg2), "arg3"=unlist(mc_sim_fnc1_arg3))
  return(mcSimFixLs)
}

#Reactive function to gets just the argument values I should use 
McSimFix <- reactive({
  mc_sim_fix_fnc1(McSimDf=mc_sim_fnc1())
})

mc_sim_fnc2 <-  reactive({
  cbind(mc_sim_fnc1()[, 1:2], McSimFix())
})

#3
#Function that creates data frame (as a list) of different MC simulated inputs
input_mc_fnc1 <- function(mc_sim) {
  input_mc_dft1 <- list()
  for(i in 1:nrow(mc_sim)) {
    set.seed(input$set_seed + i)            #Sets the seed within the for loop so it is used on each predictor
    input_mc_dft1[[i]] <- do.call(mc_sim[i,1], list(mc_sim[i,2], mc_sim[i,3], mc_sim[i,4]))
    #input_mc_dft1[[i]] <- sapply(mc_sim[i,2], eval(parse(text = mc_sim[i,1])), mc_sim[i,3], mc_sim[i,4]) #Backup code
  }
  return(input_mc_dft1)
}
    
#3A
  #Reactive function that runs input_mc_fnc1() above
input_mc_df1 <- reactive({
#  input_mc_fnc1(mc_sim=mc_sim_fnc1())
  input_mc_fnc1(mc_sim=mc_sim_fnc2()) #Uses the "fixed" data
})

## Function to identify single or multiple groups with a value ##
fncMCUnifLev <- function(Up.Lev.Prop, Lbls, X) {
  X.Var.Num <- which(predictor() == X) 
  Up.Lev.Prop <- Up.Lev.Prop[[X.Var.Num]]
  Lbls <- Lbls[[X.Var.Num]]
  None.Levs <- length(Up.Lev.Prop [Up.Lev.Prop == 0])
  Lev.Not.0 <- which(Up.Lev.Prop != 0)
  #Create new labels and values for just those with data
  New.Lbls <- Lbls[Lev.Not.0]
  New.PDF <- Up.Lev.Prop[Lev.Not.0]
  return(list(Lev.Not.0=Lev.Not.0,  None.Levs=None.Levs,
              New.Lbls=New.Lbls, New.PDF=New.PDF))
}

#Reactive plot that runs fncMCUnifLev()
mc_unif_df <- reactive({
  fncMCUnifLev(Up.Lev.Prop= vls2()[["pdf"]], Lbls= vls2()[["pdf_lbl"]],
               X= input$updf) 
}) 

#4
#Function that loops through the simulated data and coverts the 0-1 values to factors
#based on the original values so that it will work with the formula function
input_mc_fnc2 <- function(input_dist, input_mc_df, PDF, LBL, Unif.Update, Unif.Df) {
  
  for(i in 1:length(input_dist)) {
    if(input_dist[i] == "runif") {
      if(Unif.Update == "Yes") {
        input_mc_df[[i]] <- cut(input_mc_df[[i]], breaks=c(-.01, cumsum(Unif.Df[["New.PDF"]] )) , 
                                labels=Unif.Df[["New.Lbls"]]
        )
      } else {
        input_mc_df[[i]] <- cut(input_mc_df[[i]],breaks=c(-.01, cumsum(PDF[[i]])), 
                                labels=LBL[[i]])
      }
    }
  }
  return(input_mc_df)
}

#4A
#Reactive function that runs input_mc_fnc2() above
input_mc_df2 <- reactive({
  #  input_mc_fnc1(input_dist=mc_sim_fnc()[["input_dist"]], input_mc_df=input_mc_df1(), PDF=vls1()[["pdf"]])
  input_mc_fnc2(input_dist=mc_sim_fnc1()[[1]], input_mc_df=input_mc_df1(),
                PDF=vls2()[[6]], LBL=vls2()[["pdf_lbl"]], Unif.Update= input$updpdf, Unif.Df=mc_unif_df())
})


#5
  #Function that converts the list object to a data frame
mc_df_fnc <- function(input_mc_df, CNM) {
  input_mc_df <- as.data.frame(input_mc_df)
  #Give column names to the data frame
  colnames(input_mc_df) <- CNM
  #names(input_mc_df) <- CNM
  return(input_mc_df)
}

#Reactive function that runs the function above to complete the simulated data
mc_df1 <- reactive({
  mc_df_fnc(input_mc_df=input_mc_df2(), CNM=vls2()[["cnm"]]) 
})

#######################
#Predicted values
yhat <- reactive ({
   do.call(rms_func(), mc_df1())   
  #sapply(mc_df1(),rms_func() )   
})

#Function to convert predicted scores to scores on raw scale or as proportions or leave as the same
yhat_plot_fnc <- function(yhat, reg_yhat) {
  switch(reg_yhat,                
         "Linear"   = plot_yhat <- yhat, 
#         "Logistic" = plot_yhat <- 1/(1+exp(-yhat)),
         "Logistic" = plot_yhat <- plogis(yhat),
#"Ordinal Logistic"          = plot_yhat <- 1/(1+exp(-yhat)),
"Ordinal Logistic"          = plot_yhat <- plogis(yhat),
         "Poisson"  = plot_yhat <- exp(yhat),
         "Quantile" = plot_yhat <- yhat,
#"Cox PH"   = plot_yhat <- 1/(1+exp(-yhat)),
"Cox PH"   = plot_yhat <- plogis(yhat),
#"Cox PH with censoring"     = plot_yhat <- 1/(1+exp(-yhat)),
"Cox PH with censoring"     = plot_yhat <- plogis(yhat),
#"AFT"   = plot_yhat <- 1/(1+exp(-yhat)),
"AFT"   = plot_yhat <- yhat,
#"AFT with censoring"     = plot_yhat <- 1/(1+exp(-yhat)),
"AFT with censoring"     = plot_yhat <- yhat,
         "Generalized Least Squares" = plot_yhat <- yhat)
  return(plot_yhat)
}

#This reactive function runs the yhat_plot_fnc function above  
yhat_plot_rslt <- reactive ({
  yhat_plot_fnc(yhat=yhat(), reg_yhat=input$regress_type)
})

############################
## Monte Carlo simulation ##
############################

#This gives the yhats for each indicator, the Morris OAT method
y_inp_fnc <- function(dfls, fnc) {
  yhat_input <- list()
  for(i in 1:ncol(dfls)) {
    #yhat_input[[i]] <-  do.call(fnc, dfls[i])
    yhat_input[[i]] <-  do.call(fnc, dfls[i])
  }
  names(yhat_input) <- colnames(dfls)        #Adds names to list
  return(yhat_input)
}

#This runs y_inp_fnc() on the data and produces yhat for each unique indicator (Morris OAT)
yhat_input <- reactive({
  do.call(y_inp_fnc, list(dfls=mc_df1(), fnc=rms_func())) 
})

yhat_mn <- reactive({
mean(yhat())
})

yhat_sd <- reactive({
  sd(yhat())
})

yhat_input_mn <- reactive({
  sapply(yhat_input(), mean)
})

yhat_input_sd <- reactive({
  sapply(yhat_input(), sd)
})

###############################
## Means, SDs, PDFs ##
vls_up_fnc <- function(x_ls,  UPDY,  PRED,  UMN,  VAL, 
                              SUPDY,        SUMN, SVAL, 
                              PUPDY,        PUMN, PVAL) {
  if(UPDY == "Yes") { 
    #x_ls$mn[which(PRED %in% UMN)]   <- unique(na.omit(as.numeric(unlist(strsplit(unlist((as.character(VAL))), "[^0.0-9.9]+")))))
    x_ls$mn[which(PRED %in% UMN)]   <- unique(na.omit(as.numeric(unlist(strsplit(unlist((as.character(VAL))), "[^-0.0-9.9]+")))))
  }
  if(SUPDY == "Yes") { 
    x_ls$sd[which(PRED %in% SUMN)]  <- unique(na.omit(as.numeric(unlist(strsplit(unlist((as.character(SVAL))), "[^0.0-9.9]+")))))
  }
  if(PUPDY == "Yes") { 
    x_ls[["pdf"]][[which(PRED == PUMN)]] <- as.numeric(unlist(strsplit(unlist((as.character(PVAL))), "[^0.0-9.9]+")))[-1]
    #This needs to be fixed so that multiple list values can be used  
    #    x_ls[["pdf"]][which(PRED  %in% PUMN)] <- as.numeric(unlist(strsplit(unlist((as.character(PVAL))), "[^0.0-9.9]+")))[-1]  
  } 
  return(x_ls)
}

##Runs the vls_sup_fnc function
vls2 <- reactive({
  vls_up_fnc(x_ls=vls1(), UPDY=input$updmn, PRED=predictor(), UMN=input$umn, VAL=input$umnval,
             SUPDY=input$updsd,  SUMN=input$usd,  SVAL=input$usdval,
             PUPDY=input$updpdf, PUMN=input$updf, PVAL=input$updfval)
})

#Get the MC simulated data with the Yhat value--note that the seed is different for each predictor
mc_df_y_fnc <-  function(yhat2, df2) {
  new_df <- data.frame(cbind(y=yhat2, df2) )
  return(new_df)
}
#This produces the data frame
mc_df_y <- reactive({
  mc_df_y_fnc(yhat2=yhat(), df2=mc_df1())
})

## This function returns the partial correlation coefficients
pcc_fnc <-  function(yhat, df) {
new_df <- cbind(y=yhat, df)
  pcc_gsa <- pcc(new_df, new_df[, "y"])
  return(pcc_gsa)
}
#This produces PCC results
pcc_rslt <- reactive({
  pcc_fnc(yhat=yhat(), df=mc_df1())
})

#SRC attempt
#This is the partial correlation coefficients
src_fnc <-  function(df3) {
    #src_gsa <- sort(coef(lm.beta(lm(y ~ . , data= df3))))  #Turned this off so it's not redundant with src_gsa2
    src_gsa <- coef(lm.beta(lm(y ~ . , data= df3)))     #Use for the tornado plot
    src_gsa_R2 <- summary(lm(y~ . - 1, data=df3))$"r.squared"
    src_anova <- anova(lm(y ~ ., data= df3))
    #src_gsa <- src(df3, y)
    #  src_gsa <- lm(y ~ ., data= df3)
    #src_gsa <- coxph(Surv(y)~., data=df3)
  return(list("Standardized Regression Coefficients (numerical order)"= sort(src_gsa[-1]),
              "src_coef"=src_gsa[-1],
              "SRC R^2"= src_gsa_R2,
              "SRC ANOVA Summary Table"= src_anova))
}

#This produces PCC results
src_rslt <- reactive({
  src_fnc(df3= mc_df_y())
})

#######################
#Select the predictors that will have modified means.
output$up_mn_var <- renderUI({  
  selectInput("umn", "1. Select the predictor(s) that will have their means modified", 
                    choices = predictor(), multiple=TRUE, selected=predictor()[1])     #Will make choices based on my reactive function.
})
#Update the actual means values.
output$up_mn_val <- renderUI({
  textInput("umnval", "2. Update the means (see \".Names\" below for predictor order)" , 
              value= deparse(vls2()[["mn"]][which(predictor() %in% input$umn)], width.cutoff=500 ))
              #value= deparse((vls2()[["mn"]][input$umn]), width.cutoff=500 ))
})
#Indicate if you should update the mean used in determining the Monte Carlo distribution.
output$update_mc_mn <- renderUI({
  selectInput("updmn", "3. Shall we update the mean(s) now?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     
})

#Select the predictors that will have modified standard deviations.
output$up_sd_var <- renderUI({  
  selectInput("usd", "4. Select the predictor(s) that will have their SDs modified", 
              choices = predictor(), multiple=TRUE, selected=predictor()[1])     #Will make choices based on my reactive function.
})
output$up_sd_val <- renderUI({
  textInput("usdval", "5. Update the SDs (see \".Names\" below for predictor order)", 
            value= deparse(vls2()[["sd"]][which(predictor() %in% input$usd)], width.cutoff=500 ))
  #            value= deparse(vls2()[["sd"]][input$usd], width.cutoff=500 ))
})
#Indicate if you should update the standard deviation used in determining the Monte Carlo distribution.
output$update_mc_sd <- renderUI({
  selectInput("updsd", "6. Shall we update the standard deviation(s) now?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     
})

#Select the predictors that will have modified standard deviations.
output$up_pdf_var <- renderUI({  
  selectInput("updf", "7. Select a FACTOR predictor to modify its probability density function (no '0/1')", 
              #This needs to be fixed so that multiple list values can be used  
              #choices = predictor(), multiple=TRUE, selected=predictor()[1])
             choices = predictor(), multiple=FALSE, selected=predictor()[1])     #NEED TO FIX THIS 
})
output$up_pdf_val <- renderUI({
  textInput("updfval", "8. Update the probability density function", 
            #This needs to be fixed so that multiple list values can be used  
            #value= deparse(vls2()[["pdf"]][input$updf], width.cutoff=500 ))     
  value= deparse(vls2()[["pdf"]][[which(predictor() == input$updf)]], width.cutoff=500 ))     
})
#Indicate if you should update the standard deviation used in determining the Monte Carlo distribution.
output$update_mc_pdf <- renderUI({
  selectInput("updpdf", "9. Shall we update one probability density function now?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     
})
#Download Monte Carlo simulated data
observe({
  if(input$dmcdf == "Yes")
  {
    sink("mc_data.txt", append=F)
    write.table(mc_df_y(), row.names=F, sep="\t")
    sink()
    }
})
## This will allow me to update the Function that creates predicted values so I can create a whole new MC simulation
output$update_rms_fnc <- renderUI({
  selectInput("uprf", "10. Shall we update the formula function? See \"Edit box\".", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     
})

###############################
#         MC Plots             #
###############################
## Tornado plot ##
src_plot_fnc <- function(lm_src) {
  src_coef_obj <- sort(abs(lm_src))
  #Range of values to use for the xlim
  src_rng <- c(0 - max(src_coef_obj)*1.1/2,
               0 + max(src_coef_obj)*1.1/2)
  #Creates a vector of values to use for the tornado plot, centered on 0
  tp_val <- c(0 - src_coef_obj/2,
              0 + src_coef_obj/2)
  #Creates a matrix from tp_val to use in for loop
  tp_m <- matrix(tp_val, nrow=length(src_coef_obj), byrow=F)
  #This is a vector of 3 values so it will fit with 3 variables
#  line_val <- c(min(tp_val), median(tp_val), max(tp_val))
  line_val <- c(min(tp_val), max(tp_val))
  #Plot
#  plot(line_val, 1:length(src_coef_obj), xlim=src_rng, type="n",
  par(mar=c(5, 4, 4, 6))
  plot(seq(line_val[1], line_val[2], length.out=length(src_coef_obj)), 1:length(src_coef_obj), xlim=src_rng, type="n",
       main="Tornado plot",
       xlab="Absolute value of SRC, 1/2 above and below 0",
       ylab="", axes=F)
  for( i in 1:nrow(tp_m)) {
    lines(c(tp_m[i, 1], tp_m[i, 2]), c(i, i), col=2, lwd=8)
  }
  axis(1, labels=TRUE)
  axis(4, at=1:length(src_coef_obj), labels=names(src_coef_obj), las=2)
  box()
}

#Runs the src_plot_fnc function above to get the Tornado plot
src_plot_rslt <- reactive({
  src_plot_fnc(abs(src_rslt()[["src_coef"]]))
})

## Cutoff plot ##
#Input box to ask for a cutoff
output$cutoff_yes <- renderUI({
  selectInput("cutplot", "Would you like to include a cutoff value?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     
})

#This specifies a level
output$cutoff_val <- renderUI({                                 #Same idea as output$vy
  numericInput("cutval", "Select the cutoff level.",
               value="")     #Will make choices based on my reactive function.
})

#This creates a function to plot a histogram with an optional cutoff value
cutoff_fnc <- function(yhat, cut_yes, cut_off) {
  hist(yhat, xlab= "Predicted values", main="Histogram of predicted values with an optional cutoff level")
  if (cut_yes=="Yes") {
    abline(v=cut_off, col="blue", lwd=3)
  } else {
    abline(v="")
  }
}
#This runs the cutoff_fnc function plot above
cutoff_plot_rslt <- reactive({
  cutoff_fnc(yhat_plot_rslt(), input$cutplot, input$cutval)
})


output$cutoff_smry <- renderPrint({ 
  #How many are at or above this cutoff and those that are not
  #How many are at or below this cutoff and those that are not
  if (input$cutplot =="Yes") {
  list(
    "At or above the cutoff" =length(yhat_plot_rslt()[yhat_plot_rslt() >= input$cutval ])/ length(yhat_plot_rslt()), 
    "Below the cutoff" =(1 - (length(yhat_plot_rslt()[yhat_plot_rslt() >= input$cutval ])/ length(yhat_plot_rslt())))
    #"At or below cutoff when lower is better"=length(yhat_plot_rslt()[yhat_plot_rslt() <= input$cutval ])/ length(yhat_plot_rslt()), 
    #"Above cutoff when lower is better"=(1 - (length(yhat_plot_rslt()[yhat_plot_rslt() <= input$cutval ])/ length(yhat_plot_rslt())))
  )
  }
})

## Summary of Simulated values from Cut-off plot tab ##
describeYhatPlotRslt <- reactive({ 
  print(describe(yhat_plot_rslt(), 
        descript=paste0("Simulated values of ",input$variableY) )) #Summary of predicted Y variable.)
})
#Describe Yhat from simulation
output$desc_YhatPlotRslt <- renderPrint({                                                 
  print(describeYhatPlotRslt())
})

## Cobweb plot ##
#Function that converts non-numeric variables to numeric variables
recode_mc_fnc1 <- function(df, dists, cvls, By.Seq) {
  df[, 1] <- as.numeric(cut(df[, 1], breaks=quantile(df[, 1], probs=seq(0,1, By.Seq)), include.lowest=T, right=FALSE))
    for (i in 1:ncol(df[,-1])) {
    if (dists[i] %in% c("runif", "rbinom")) {
      df[, i+1] <- as.numeric(as.factor(df[, i+1]))
      df[, i+1] <- round((df[, i+1] / cvls[[i]])*100, 0)
    } else {
      df[, i+1] <- as.numeric(cut(df[, i+1], breaks=quantile(df[, i+1], probs=seq(0,1, By.Seq)), include.lowest=T, right=FALSE))
    }
      #if (dists[i] == "rnorm") {
      #if (!dists[i] %in% c("runif", "rbinom")) {
      #  df[, i+1] <- as.numeric(cut(df[, i+1], breaks=quantile(df[, i+1], probs=seq(0,1, .01)), include.lowest=T, right=FALSE))
      #}
  }
  return(df)
}
#This runs the recode_mc_fnc1 function above.
mc_df_y2 <- reactive({
  recode_mc_fnc1(mc_df_y(), mc_arg_fnc1()[["input_dist"]], cvls=vls2()[["levs"]], By.Seq=cobweb_by_sequence())
})

#This function creates the cobweb plot
cobweb_fnc <- function(df) {
  plot(1:ncol(df), seq(1, 100, length.out = ncol(df)), type="n",
       axes=F, xlab="Predictors", ylab="Percentiles")
  apply(df[df[, 1] >= 1,], 1, lines, col="gray")
  #Generic color for top and bottom 10-20%
  if ("Top 20%" %in% input$topbottom)  {
    apply(df[df[, 1] >= 81,], 1, lines, col="yellow")
  } 
  if ("Bottom 20%" %in% input$topbottom )  {
    apply(df[df[, 1] < 21,], 1, lines, col="yellow")
  }
  if ("Top 10%" %in% input$topbottom)  {
    apply(df[df[, 1] >= 91,], 1, lines, col="orange") 
  } 
  if ("Bottom 10%" %in% input$topbottom )  {
    apply(df[df[, 1] < 11,], 1, lines, col="orange")
  }
  #Lines are coded from highest to lowest so lower percentages will show on top of higher ones
  if ("Bottom 5%" %in% input$topbottom )  {
    apply(df[df[, 1] < 6,], 1, lines, col="blue")
  }
  if ("Top 5%" %in% input$topbottom)  {
    apply(df[df[, 1] >= 96,], 1, lines, col="red") #colors()[505]
  } 
  if ("Top 1%" %in% input$topbottom)  {
    apply(df[df[, 1] >= 99,], 1, lines, col=colors()[69]) #525
  } 
  if ("Bottom 1%" %in% input$topbottom )  {
    apply(df[df[, 1] <= 1,], 1, lines, col=colors()[456]) #69
  }
  axis(1, at=1:ncol(df), labels=c("Outcome", vls2()[["cnm"]]))
  axis(2, labels=TRUE)
}

cobweb_plot <- reactive({
  cobweb_fnc(df=mc_df_y2())
  })

#Input box to ask for top or bottom 5%
output$top_bottom_5 <- renderUI({
  selectInput("topbottom", "1, Highlight top or bottom outcome scores.", 
              choices = c("Top 1%","Top 5%","Top 10%", "Top 20%", "Bottom 1%", "Bottom 5%", "Bottom 10%", "Bottom 20%"), 
              multiple=TRUE, selected="Top 5%")     
})
#Indicate the R2 level value to use as a stopping rule for redundancy analysis
output$cobweb_seq <- renderUI({ 
  numericInput("cwebSeq", "2. Select the percentile level for all lines.",
               value=0.01, min=0, max=1, step=.01) 
})
#Reactive function for cobweb_seq 
cobweb_by_sequence <- reactive({ 
    input$cwebSeq
})
#Add in labels to binomial or categorical variables
cobweb_fctr_fnc <- function(df, dists) {
  facdf <- list()
  for (i in 1:length(df[["cnm"]])) {
    if (dists[i] %in% c("runif", "rbinom")) {
      facdf[[i]] <- round(df[["cdf"]][[i]]*100, 0)
            names(facdf[[i]]) <-  df[["pdf_lbl"]][[i]]
#      names(facdf)[[i]] <-  df[["cnm"]][[i]]  #This line doesn't let me keep level names
      
    } else {
      facdf[[i]] <- NULL
      names(facdf)[[i]] <-  NULL    #This line wrongly keeps continuous level names
    }
  }
  return(facdf)
}

cobweb_fctr_rslt <- reactive({
  cobweb_fctr_fnc(df=vls2(), dists=mc_arg_fnc1()[["input_dist"]])
})

################################################################################
#                           Data Reduction                                     #
################################################################################

############################
### Redundancy analysis  ###
############################

#This creates the function to gather data element  characteristics for the model (e.g., names, levels) 
reduce_vlfnc <- function(xdf) {
  v_ls <- list()     #List of different elements
  #For loops that goes through all of the columns
  for(i in 1:ncol(xdf)) {
    v_ls$levs[[i]] <- length(unique(xdf[,i]))
    v_ls$cls[[i]]  <- class(xdf[,i])
    v_ls$cnm[[i]]  <- colnames(xdf)[i]  #Seems to have an issue with a 1 column data frame. Tried
    v_ls$typ[[i]]  <- typeof(xdf[,i])  #This is used to work with 'labelled' class in RMS data
  }
  return(v_ls)  
}

#reduce_vlfnc(titanic3)

#This is a reactive function that uses my homemade fucntion above off of the xdf data
reduce_vls1 <- reactive({
  reduce_vlfnc(xdf()) 
})

#This indicates if a predictor is a continuous variable with < 6 levels 
reduce_arg_fnc <- function(LEVS, CLS,  TYP, NMS) {
  #Input distribution
  input_dist <- list()
  input_dist <- ifelse(LEVS <= 2 & CLS %in% c("numeric", "integer", "logical"), "rbinom", NA) 
  input_dist <- ifelse(LEVS  > 2 & CLS %in% c("numeric", "integer"), "rnorm", input_dist) 
  input_dist <- ifelse(LEVS >= 2 & CLS %in% c("character", "factor"), "runif", input_dist)
  #The next 2 lines allows me to use Harrell's data which has a class of "labelled" for some variables
  input_dist <- ifelse(is.na(input_dist) & CLS == 'labelled' & TYP %in% c("double", "integer"), "rnorm", input_dist) 
  input_dist <- ifelse(is.na(input_dist) & CLS == 'labelled' & TYP == "character", "runif", input_dist) 
  input_dist <- ifelse(input_dist == "rnorm" & LEVS %in% 3:10, "low_lev", input_dist)  #low_lev indicates vars to use I() in redundancy analysis
  names(input_dist) <- NMS
  return(input_dist=input_dist)
}

#Reactive function that runs reduce_arg_fnc()
reduce_arg1 <- reactive({
  reduce_arg_fnc(LEVS=reduce_vls1()[["levs"]], CLS=reduce_vls1()[["cls"]], TYP=reduce_vls1()[["typ"]], NMS=reduce_vls1()[["cnm"]]) 
})

#Indicates continuous variables with too few levels for the redundancy analysis
low_lev_cont <- reactive({
  names(reduce_arg1())[which(reduce_arg1() == "low_lev")]
})

#These are the predictors that are non-continuous or the continuous that will get spline terms 
non_low_lev <- reactive({             
  setdiff(predictor(), low_lev_cont())  
})

#This creates the I() terms.
I_low_lev <- reactive({
  if (is.null(low_lev_cont())) {
    NULL
  } else {
    paste0("I(", substr(low_lev_cont(), -1, nchar(low_lev_cont()) + 1), ")")
  }
})

#These are the final variables in the redundancy variables
redun_vnms <- reactive({
  if (I_low_lev() == "I()") {
  non_low_lev() 
  } else {
    c(I_low_lev(), non_low_lev())
  }
})

#This is the formula to use in the redundancy analysis
redun_fmla <- reactive({             #Spline terms 
  as.formula(paste(paste0(" ", "~"),   
                   paste(redun_vnms(), collapse= "+")))
})

### Redundancy analysis, using I() for <= 10 unique levels 
redun_anlys <- reactive({
  if (input$RedChoice == "Yes") {
  redun(redun_fmla(), r2= input$RedR2Lev, type='adjusted', data= df())
  }
})

#Indicate the R2 level value to use as a stopping rule for redundancy analysis
output$redun_r2_lev <- renderUI({ 
  numericInput("RedR2Lev", "1. Select the R2 level indicating redundancy.",
               value=0.80, min=0, max=1, step=.05) 
})

#Indicate if you want a redundancy analysis
output$redun_choice <- renderUI({ 
  radioButtons("RedChoice", "2. Would you like to run the redundancy analysis?",
               choices = c("No", "Yes"),
               selected="No") 
})

#This produces the redundancy analysis results
output$redun_smry <- renderPrint({
  if (input$RedChoice == "Yes") {
  print(redun_anlys())
    #  print(list(low_lev_cont(),non_low_lev(), I_low_lev(), redun_vnms()))
  }
})

############################
###   Cluster analysis   ###
############################
#This is the formula to use in the redundancy analysis
cluster_anl_fmla <- reactive({             #Spline terms 
  as.formula(paste(paste0(" ", "~"),   
                   paste(predictor(), collapse= "+")))
})

#Select the Similarity matrix type
output$clust_sim_matrix <- renderUI({
  selectInput("ClustSim", "1. Select the similarity matrix.",
              choices = c("spearman","pearson","hoeffding","bothpos","ccbothpos"), multiple=FALSE, selected="pearson" )
})

### Cluster analysis run 
cluster_anlys <- reactive({ 
  if (input$ClustChoice == "Yes") {
  varclus(cluster_anl_fmla(), sim=input$ClustSim, data= df())
  }
})

#Indicate if you want a redundancy analysis
output$clust_choice <- renderUI({ 
  radioButtons("ClustChoice", "1. Would you like to run the cluster analysis?",
               choices = c("No", "Yes"),
               selected="No") 
})


#This produces the cluster analysis results
output$cluster_plot <- renderPlot({
  if (input$ClustChoice == "Yes") {
    plot(cluster_anlys())
  }
})

############################
###   Transform/Impute   ###
############################
ptrans <-   reactive({
  set.seed(1)
  transcan(cluster_anl_fmla(),
           imputed=TRUE, transformed=TRUE, trantab=TRUE, pl=FALSE,
           show.na=TRUE, data=df(), nk= input$SIknots, asis=input$asisx, 
           #frac=.1,  #This might be fracmiss...max amount of NAs to determine if I should keep. Might add this later if the default to keep all is not useful  
           pr=FALSE)
})

#This produces the transformation/imputation summary
output$trans_smry <- renderPrint({
  if (input$TransChoice == "Yes") {
    summary(ptrans(), digits=4)             #Shows R2 for each variable, give idea about which predictors had 
  }
})

### Figure 8.3, no changes needing here, use plotoutput()
output$ptrans_plot <- renderPlot({
  if (input$TransChoice == "Yes") {
    ggplot(ptrans(), scale=TRUE) + theme(axis.text.x=element_text(size=6))   #Not all variables having missing data.
  }
})

###New data frame for "Imputed". Has original data and imputed values in the raw scale
imputed <- reactive({
  as.data.frame(impute(ptrans(), data=df(), list.out=TRUE))
})

#Indicate if you want the transformation/imputed values
output$SIks <- renderUI({                                 #Same idea as output$vy
  numericInput("SIknots", "1. Select the number of knots.", value = 4, min=3, step=1)     #Will make choices based on my reactive function.
})

#Selects variables that won't get transformed or imputed
output$AsIs_x <- renderUI({                                 #Same idea as output$vy
  selectInput("asisx", "2. Variables neither transformed nor splined.", 
              choices = predictor(), multiple=TRUE)     #Will make choices based on my reactive function.
})

#Indicate if you want the transformation/imputed values
output$Transcan_Choice <- renderUI({ 
  radioButtons("TransChoice", "3. Would you like to run the simultaneous transformation and imputation?",
               choices = c("No", "Yes"),
               selected="No") 
})

############################
###        PCA           ###
############################
#Select the categorical variables that need a model matrix
output$mm_var_ls <- renderUI({
selectInput("MmVarLs", "1. Select the categorical variables that get a model matrix",
            choices = predictor(), multiple=TRUE )
  })

#Indicate if you want a redundancy analysis
output$pca_choice <- renderUI({ 
  radioButtons("PCAChoice", "1. Would you like to run the PCA analysis?",
               choices = c("No", "Yes"),
               selected="No") 
})

# Create a design matrix from ekg categories...expands 1 factor to multiple set 
#This function creates model matrices for the various categorical variables
mod_mat_fnc <- function(var_ls, df) {
  mm_ls <- list()
  for (i in 1:length(var_ls)) {
    mm_ls[[i]] <- model.matrix(~ df[, which(colnames(df) == var_ls[i])])[, -1]
  }
  return(as.data.frame(mm_ls))
}

#Reactive function that runs mod_mat_fnc above
mod_mat <- reactive({ 
  if (input$PCAChoice == "Yes") {
  mod_mat_fnc(var_ls=input$MmVarLs, df=imputed())
  }

})

#Reactive function that combines imputed data and expanded model matrix
new_imputed <- reactive({ 
    if (!is.null(input$MmVarLs)) {
      cbind(imputed()[,setdiff(colnames(imputed()), input$MmVarLs)], mod_mat())
    } else {
      imputed()[,setdiff(colnames(imputed()), input$MmVarLs)]
    }
})


# Imputed/raw principal component scores 
prin.raw <- reactive({
  princomp(~ ., cor=TRUE, data=new_imputed())
})

# Transformed principal component scores 
prin.trans <- reactive({
  princomp(~ . , cor=TRUE, data=as.data.frame(ptrans()[["transformed"]]))
})

#Scree plot
# Function to create a scree plot, adds the cumulative fraction of variance explained
#addscree <- function(x, npcs=min(20, length(x$sdev)),
addscree <- function(x, npcs=length(x$sdev),
                                          plotv=FALSE,
                     col=1, offset=.8, adj=0, pr=FALSE) {
  vars <- x$sdev^2
  cumv <- cumsum(vars)/sum(vars)
  if(pr) print(cumv)
  text(1:npcs, vars[1:npcs] + offset*par('cxy')[2],
       as.character(round(cumv[1:npcs], 2)),
       srt=45, adj=adj, cex=.9, xpd=NA, col=col)
  if(plotv) lines(1:npcs, vars[1:npcs], type='b', col=col)
}

### Figure 8.4 Scree plot ###
#Line for raw/imputed values
output$screeplot <- renderPlot({ 
  if (input$PCAChoice == "Yes") {
    plot(prin.raw(), type='lines', 
         ylim=c(min(min(prin.trans()[[1]]^2), min(prin.raw()[[1]]^2))*.9, max(max(prin.trans()[[1]]^2), max(prin.raw()[[1]]^2))*1.1) , 
         main='')
    addscree(prin.raw())
    addscree(prin.trans(),  plotv=TRUE, col='red',
             offset=-.8, adj=1)
    abline(h=1, lty=3, col="grey")
  } 
})

######################################
# Indicate desired number of factors #
######################################
output$factor_nmbr <- renderUI({                                 #Same idea as output$vy
  numericInput("FactorNmbr", "Select the number of factors to build.", value = 1, min=1, max=50)
})


### Function that creates PCA components###
#USE ONLY Transformed data...won't work on categorical raw data
# Compute PC1 on a subset of transcan-transformed predictors, modified so I just need the DF
pco <- function(df) {
  f <- princomp(~ ., data=df, cor=TRUE)
  vars <- f$sdev^2
  cat('Fraction of variance explained by PC1:',
      round(vars[1]/sum(vars),2), '\n')
  f$scores[,1]
}

#Reactive function that gathers variables in the ptrans$transformed data
pca_fac_ls <- reactive({
  if (input$PCAChoice == "Yes") {
    pca_fac_ls_fnc(df=as.data.frame(ptrans()[["transformed"]]), numfac=input$FactorNmbr)
  }
  })

#Render UI to show which variables are in the model---This is the original attempt...needs work
##output$sm_fac_ls <- renderUI({
##  textInput("SmFacLs", "2. Seclect the factor predictors" , 
##            value= pca_fac_ls())
#})

#Reactive function of variable names from ptrans transformed values.
ptrns_trnfd_vls <- reactive({
  colnames(ptrans()[["transformed"]])
})

#####################
# Create 10 factors #
#####################

# Factor 1 variables  #
output$sm_fac_ls1 <- renderUI({
  selectInput("SmFacLs1", "1. Seclect the variables for Factor 1" , 
              choices= ptrns_trnfd_vls(), multiple=TRUE)
})
# Factor 2 variables  #
output$sm_fac_ls2 <- renderUI({
  selectInput("SmFacLs2", "2. Seclect the variables for Factor 2" , 
              choices= ptrns_trnfd_vls(), multiple=TRUE)
})
# Factor 3 variables  #
output$sm_fac_ls3 <- renderUI({
  selectInput("SmFacLs3", "3. Seclect the variables for Factor 3" , 
              choices= ptrns_trnfd_vls(), multiple=TRUE)
})
# Factor 4 variables  #
output$sm_fac_ls4 <- renderUI({
  selectInput("SmFacLs4", "4. Seclect the variables for Factor 4" , 
              choices= ptrns_trnfd_vls(), multiple=TRUE)
})
# Factor 5 variables  #
output$sm_fac_ls5 <- renderUI({
  selectInput("SmFacLs5", "5. Seclect the variables for Factor 5" , 
              choices= ptrns_trnfd_vls(), multiple=TRUE)
})
# Factor 6 variables  #
output$sm_fac_ls6 <- renderUI({
  selectInput("SmFacLs6", "6. Seclect the variables for Factor 6" , 
              choices= ptrns_trnfd_vls(), multiple=TRUE)
})
# Factor 7 variables  #
output$sm_fac_ls7 <- renderUI({
  selectInput("SmFacLs7", "7. Seclect the variables for Factor 7" , 
              choices= ptrns_trnfd_vls(), multiple=TRUE)
})
# Factor 8 variables  #
output$sm_fac_ls8 <- renderUI({
  selectInput("SmFacLs8", "8. Seclect the variables for Factor 8" , 
              choices= ptrns_trnfd_vls(), multiple=TRUE)
})
# Factor 9 variables  #
output$sm_fac_ls9 <- renderUI({
  selectInput("SmFacLs9", "9. Seclect the variables for Factor 9" , 
              choices= ptrns_trnfd_vls(), multiple=TRUE)
})
# Factor 10 variables #
output$sm_fac_ls10 <- renderUI({
  selectInput("SmFacLs10", "10. Seclect the variables for Factor 10" , 
              choices= ptrns_trnfd_vls(), multiple=TRUE)
})

#This function makes N number of copies of the column names, retain the ones I want
#in each factor with a later renderUI
pca_fac_ls_fnc <- function(df, numfac) {
  ls_df <- vector(mode="list", length=numfac)
  for (i in 1:numfac) {
    ls_df[[i]]  <- colnames(df)
  }
  return(ls_df)
}

####
#Reactive function that collects all of the variable in each of the 10 factors
tot_fac_vls <- reactive({
  list(input$SmFacLs1, input$SmFacLs2, input$SmFacLs3, input$SmFacLs4, input$SmFacLs5,
       input$SmFacLs6, input$SmFacLs7, input$SmFacLs8, input$SmFacLs9, input$SmFacLs10)
})

#Function that removes out the factors I don't need from the 10 possible factors
mod_fac_vls <- function(vars, numfac) {
  vls <- vector(mode="list", length=numfac)
  for (i in 1:numfac) {
    vls[[i]] <- vars[[i]]
  }
  return(vls)
}

#Reactive function that runs mod_fac_vls() above
mod_fac <- reactive({
  mod_fac_vls(vars=tot_fac_vls(), numfac=input$FactorNmbr)
})

#This functions calculates factor scores for whatever elements are in the list
pca_fac_df_fnc <- function(df, vls) {
  ls_df <- vector(mode="list", length=length(vls))
  for (i in 1:length(vls)) {
    ls_df[[i]]  <- pco(as.data.frame(df[,vls[[i]]]))
    names(ls_df) <- paste0("factor", 1:length(vls))
  }
  return(as.data.frame(ls_df))
}

#Reactive function that runs pca_fac_df_fnc() above
pca_fac_df <- reactive({
  pca_fac_df_fnc(df=as.data.frame(ptrans()[["transformed"]]), vls=mod_fac())
})
####

#This functions calculates factor scores for whatever elements are in the list
##pca_fac_df_fnc <- function(df, vls) {
##  ls_df <- list(mode="list", length=length(vls))
##  for (i in 1:length(vls)) {
##    ls_df[[i]]  <- pco(as.data.frame(df[,vls[[i]]]))
##    names(ls_df) <- paste0("factor", 1:length(vls))
##  }
##  return(as.data.frame(ls_df))
##}


#1. Select predicitors that go into a factor
#3. Way to identify factors that I create
#4. Way to Cbind the factors that I create
#5, Way to cbind my factors with the main data

################################################################################




################################################################################
#                          META ANALYSIS                                       #
################################################################################

meta_df <- reactive({                  #This indicates the data frame I will use.
  input$meta_dataframe  
})

meta_var <- reactive({                  #I use this to get the variable names from the data frame. 
  names(get(meta_df()))  
})

#Indicate if you are assessing a binary or continuous outcome.
output$bin_or_con <- renderUI({
  selectInput("bincon", "B. Are you analyzing a binary or continuous outcome", 
              choices = c("Binary", "Continuous"), multiple=FALSE, selected="Binary")     
})

############
## Binary ##
############
#Treatment group
output$event.e_bin <- renderUI({                                #

    selectInput("eventEbin", "1. Select the number of events in the treatment group.",       #
              choices = meta_var(), multiple=FALSE, selected=meta_var()[3] )   #
})
output$n.e_bin <- renderUI({                                #
  selectInput("nEbin", "2. Select the number of observations in the treatment group.",       #
              choices = setdiff(meta_var(), input$eventEbin), multiple=FALSE, selected=meta_var()[4] )   #
})
#Control group
output$event.c_bin <- renderUI({                                #
  selectInput("eventCbin", "3. Select the number of events in the control group.",       #
              choices = setdiff(meta_var(), input$eventEbin), multiple=FALSE, selected=meta_var()[5] )   #
})
output$n.c_bin <- renderUI({                                #
  selectInput("nCbin", "4. Select the number of observations in the control group.",       #
              choices = setdiff(meta_var(), input$eventEbin), multiple=FALSE, selected=meta_var()[6] )   #
})

################
## Continuous ##
################
#Treatment group
output$n.e_con <- renderUI({                                #
  selectInput("nEcon", "1. Select the number of observations in the treatment group.",       #
              choices = meta_var(), multiple=FALSE, selected=meta_var()[1] )   #
})
output$mean.e_con <- renderUI({                                #
  selectInput("meanEcon", "2. Select the Mean variable of the treatment group.",       #
              choices = setdiff(meta_var(), input$nEcon), multiple=FALSE, selected=meta_var()[2] )   #
})
output$sd.e_con <- renderUI({                                #
  selectInput("sdEcon", "3. Select the SD variable of the treatment group.",       #
              choices = setdiff(meta_var(), input$nEcon), multiple=FALSE, selected=meta_var()[2] )   #
})

#Control group
output$n.c_con <- renderUI({                                #
  selectInput("nCcon", "4. Select the number of observations in the control group.",       #
              choices = setdiff(meta_var(), input$nEcon), multiple=FALSE, selected=meta_var()[4] )   #
})
output$mean.c_con <- renderUI({                                #
  selectInput("meanCcon", "5. Select the Mean variable of the control group.",       #
              choices = setdiff(meta_var(), input$nEcon), multiple=FALSE, selected=meta_var()[5] )   #
})
output$sd.c_con <- renderUI({                                #
  selectInput("sdCcon", "6. Select the SD variable of the control group.",       #
              choices = setdiff(meta_var(), input$nEcon), multiple=FALSE, selected=meta_var()[6] )   #
})

##############
## Analysis ##
##############
mbin1 <- reactive({                  #I use this to get the variable names from the data frame. 
  metabin(event.e= get(meta_df())[[input$eventEbin]],   #Number of events in experimental group 
          n.e= get(meta_df())[[input$nEbin]],       #Number of observations in experimental group
          event.c=get(meta_df())[[input$eventCbin]], 
          n.c=get(meta_df())[[input$nCbin]], #Control group
          data = get(meta_df()),
          sm = "OR")    #Summary measure
})

mcon1 <- reactive({                  #I use this to get the variable names from the data frame. 
  metacont(n.e=get(meta_df())[[input$nEcon]],      #Sample size in experimental/treatment group
           mean.e=get(meta_df())[[input$meanEcon]],   #Mean of experimental group
           sd.e= get(meta_df())[[input$sdEcon]],   #Number of subjects in experimental
           n.c= get(meta_df())[[input$nCcon]],    #Sample size in control group
           mean.c= get(meta_df())[[input$meanCcon]], 
           sd.c= get(meta_df())[[input$sdCcon]],   
           data=get(meta_df()), 
           sm="SMD")  #Standardized Mean Difference
})

## Summary ##
output$meta_summary <- renderPrint({ 
  if (input$bincon == "Binary") {
    summary(mbin1())   
  } else {
    summary(mcon1())
  }
})

## Forest plot ##
output$forestplot <- renderPlot({ 
  if (input$bincon == "Binary") {
    forest(mbin1())   
  }
  if (input$bincon == "Continuous") {
    forest(mcon1())   
  } 
})


################################################################################
#                          POWER ANALYSIS                                      #
################################################################################

############
## Binary ##
############
#Treatment group
output$power_bin <- renderUI({                                #
  numericInput("powerBin", "1. Select the power level.",       #
               value = 0.80, min=0, max=1, step=.01 )   #
})
output$n_bin <- renderUI({                                #
  numericInput("nBin", "2. Select the sample size.",       #
               value = 100, min=0, step=1)   #
})
output$p1_bin <- renderUI({                                #
  numericInput("p1Bin", "3. Select the proportion of the treatment group.",       #
               value = 0.15, min=0, max=1, step=.01 )   #
})
output$p2_bin <- renderUI({                                #
  numericInput("p2Bin", "4. Select the proportion of the control group.",       #
               value = 0.30, min=0, max=1, step=.01 )   #
})
output$sig_bin <- renderUI({                                #
  numericInput("sigBin", "5. Select the significance level (alpha).",       #
               value = 0.05, min=0, max=1, step=.01 )   #
})
output$one_two_side_bin <- renderUI({                                #
  selectInput("oneTwoSideBin", "6. Select a one- or two-sided test.",       #
              choices = c("two.sided", "one.sided"), multiple=FALSE, 
              selected="two.sided" )   #
})
output$pwr_smp_bin <- renderUI({                                #
  selectInput("pwrsmpBin", "7. Do you want to determine power or sample size.",       #
              choices = c("Power", "Sample Size"), multiple=FALSE, selected="Sample Size" )   #
})

################
## Continuous ##
################
#Treatment group
output$power_con <- renderUI({                                #
  numericInput("powerCon", "1. Select the power level.",       #
               value = 0.80, min=0, max=1, step=.01 )   #
})
output$n_con <- renderUI({                                #
  numericInput("nCon", "2. Select the sample size.",       #
               value = 100, min=0, step=1)   #
})
output$delta_con <- renderUI({                                #
  numericInput("deltaCon", "3. Select the delta level.",       #
               value = 0.5, min=0, step=.01 )   #
})
output$sd_Con <- renderUI({                                #
  numericInput("sdCon", "4. Select the standard deviation.",       #
               value = 2, min=0, step=.01 )   #
})
output$sig_con <- renderUI({                                #
  numericInput("sigCon", "5. Select the significance level (alpha).",       #
               value = 0.05, min=0, max=1, step=.01 )   #
})
output$type_con <- renderUI({                                #
  selectInput("typeCon", "6. Select the type of t-test.",       #
              choices = c("two.sample", "one.sample", "paired"), multiple=FALSE, 
              selected="two.sample" )   #
})
output$one_two_side_con <- renderUI({                                #
  selectInput("oneTwoSideCon", "7. Select a one- or two-sided test.",       #
              choices = c("two.sided", "one.sided"), multiple=FALSE, 
              selected="two.sided" )   #
})
output$pwr_smp_con <- renderUI({                                #
  selectInput("pwrsmpCon", "8. Do you want to determine power or sample size.",       #
              choices = c("Power", "Sample Size"), multiple=FALSE, selected="Sample Size" )   #
})

###################
## Harmonic mean ##
###################
output$grp1_n <- renderUI({                                #
  numericInput("grp1N", "1. N for group 1.",       #
               value = 100, min=0,  step=1 )   #
})
output$grp2_n <- renderUI({                                #
  numericInput("grp2N", "2. N for group 2.",       #
               value = 200, min=0,  step=1 )   #
})
output$harmonic_n <- renderUI({                                #
  textInput("harmonicN", "3. Use this N for power analysis.",       #
            value= deparse(harmonicMn()))     
})
#Harmonic mean N formula
harmonicMn <- reactive({
  round((2*(input$grp1N * input$grp2N))/(input$grp1N + input$grp2N), 0)
})

##############
## Analysis ##
##############
## Summary ##  typeBin
output$power_summary <- renderPrint({ 
  list("Binary Outcome"= if (input$pwrsmpBin == "Sample Size") {
    power.prop.test(power=input$powerBin, p1=input$p1Bin, p2=input$p2Bin, 
                    sig.level=input$sigBin, alternative=input$oneTwoSideBin )
  }  else {
    power.prop.test(n=input$nBin, p1=input$p1Bin, p2=input$p2Bin, 
                    sig.level=input$sigBin, alternative=input$oneTwoSideBin)
  },
  "Continuous Outcome"= if (input$pwrsmpCon == "Sample Size") {
    power.t.test(power=input$powerCon, delta=input$deltaCon, sd=input$sdCon, 
                    sig.level=input$sigCon, type=input$typeCon, alternative=input$oneTwoSideCon)
  }  else {
    power.t.test(n=input$nCon, delta=input$deltaCon, sd=input$sdCon, 
                    sig.level=input$sigCon, type=input$typeCon, alternative=input$oneTwoSideCon)
  }
  
  )
  
  })

###################
## Effect size   ##
###################
## Effect sizes for proportions ##
as1 <- reactive({                  
  (asin(sign(input$p1Bin) * sqrt(abs(input$p1Bin))))*2  #Arcsine tranformation for treatment group
})
as2 <- reactive({                  
  (asin(sign(input$p2Bin) * sqrt(abs(input$p2Bin))))*2  #Arcsine tranformation for control group
})
#Effect size
ES_prop <- reactive({ 
  abs(as1() - as2())
})

## Effect size for means ##
ES_mean <- reactive({ 
  input$deltaCon/input$sdCon
})

## Summary ##
output$effect_size_summary <- renderPrint({ 
  list("Binary Outcome"= if (!is.null(c(input$p1Bin, input$p2Bin))) {
    ES_prop()
  }  else {
    NULL
  },
  "Continuous Outcome"= if (!is.null(c(input$deltaCon, input$sdCon))) {
    ES_mean()
  }  else {
    NULL
  }
  
  )
  
})


###############################
## Output FOR TABS ##
output$name_dist_type <- renderPrint({ 
  mc_arg_fnc1()[["input_dist"]]
})

output$morris_oat <- renderPrint({ 
  list("Morris OAT Input: Mean --Elementary Effects-- (numerical order)"   = sort(yhat_input_mn()), 
       "Morris OAT Input: Mean --Elementary Effects-- (alphabetical order)"= yhat_input_mn()[sort(names(yhat_input_mn()))], 
       "Morris OAT Input: Standard Deviation (numerical order)"   = sort(yhat_input_sd()), 
       "Morris OAT Input: Standard Deviation (alphabetical order)"= yhat_input_sd()[sort(names(yhat_input_sd()))]
  )   
  })

output$mc_gsa <- renderPrint({ 
  list("Monte Carlo Uncertainty Mean"= mean(yhat()), 
       "Monte Carlo Uncertainty SD"  = sd(yhat()),
       "Monte Carlo Coefficient of Variation"  = sd(yhat())/mean(yhat()) ,
       "Monte Carlo Partial Correlation Coefficients: Sensitivity Analysis"= pcc_rslt(),
       "Monte Carlo GSA"=src_rslt()[-2]
  )   
})

output$oat_mn_sd <- renderPlot({ 
  plot(yhat_input_mn(), yhat_input_sd(), type="n", xlab="Yhat mean", ylab="Yhat SD")
  abline(v=mean(as.numeric(df()[, input$variableY], na.rm=TRUE)), col="red")
  abline(v=median(as.numeric(df()[, input$variableY], na.rm=TRUE)), col="blue")
  text(yhat_input_mn(), yhat_input_sd(), labels=vls2()[["cnm"]])
}, height = 600)

output$tornadoplot <- renderPlot({ 
  src_plot_rslt()
}, height = 800 )

output$cutoffplot <- renderPlot({ 
  #  hist(yhat_plot_rslt())
  cutoff_plot_rslt()
}, height = 600)

output$cobwebplot <- renderPlot({ 
  cobweb_plot()
}, height = 600 )

output$cobweb_lev_nm <- renderPrint({ 
  cobweb_fctr_rslt()
})

################################################################################
#                    Confidence interval plots                                 #
################################################################################
#Continuous outcomes
tconf <- function(x, y, dataf, conf_lev) {
  #Aggregates outcome by factor 
  #agr_m <- aggregate(dataf[, y] ~ dataf[, x], FUN="mean", data= dataf)
  #agr_sd <- aggregate(dataf[, y] ~ dataf[, x], FUN="sd", data= dataf)
  #agr_n <- aggregate(dataf[, y] ~ dataf[, x], FUN="length", data= dataf)
  agr_m <- aggregate(dataf[, y], list(dataf[, x]), FUN="mean", na.rm=T)
  agr_sd <- aggregate(dataf[, y], list(dataf[, x]), FUN="sd", na.rm=T)
  agr_n <- aggregate(dataf[, y], list(dataf[, x]), FUN="length")
  agr_df <- data.frame(x_lev=agr_m[, 1], agr_m=agr_m[, 2], agr_sd=agr_sd[, 2], agr_n=agr_n[, 2])
  
  #Calculates confidence intervals
  MOE <- qt((conf_lev/2)+.5, df=agr_df$agr_n - 1) * agr_df$agr_sd/sqrt(agr_df$agr_n)
  Lower <- agr_df$agr_m - MOE
  Upper <- agr_df$agr_m + MOE
  adf_alpha <- data.frame(cbind(PointEst=agr_df$agr_m, Lower=Lower, Upper=Upper))
  rownames(adf_alpha) <- agr_df$x_lev
  alpha_o <- order(rownames(adf_alpha), decreasing = T) 
  adf_alpha <- adf_alpha[alpha_o, ] 
  adf_o <- order(adf_alpha[, "PointEst"], decreasing = T) 
  adf_numeric <- adf_alpha[adf_o, ] 
  return(list(adf_alpha=adf_alpha, adf_numeric=adf_numeric) ) 
}

##############

#Binary outcomes
bconf <- function(x, y, dataf, conf_lev) {
  #Aggregates outcome by factor 
#  agr_sum <- aggregate(dataf[, y] ~ dataf[, x], FUN="sum", data= dataf)
#  agr_n <- aggregate(dataf[, y] ~ dataf[, x], FUN="length", data= dataf)
  agr_sum <- aggregate(dataf[, y], list(dataf[, x]),  FUN="sum", na.rm=T)
  agr_n <- aggregate(dataf[, y], list(dataf[, x]), FUN="length")
  agr_df <- data.frame(x_lev=agr_sum[, 1], agr_sum=agr_sum[, 2], agr_n=agr_n[, 2])
  #Calculates confidence intervals
  adf_alpha <- binconf(x=agr_df[,2], n=agr_df[,3], alpha=1 - conf_lev)
  adf_alpha <- data.frame(adf_alpha)
  rownames(adf_alpha) <- agr_df$x_lev
  alpha_o <- order(rownames(adf_alpha), decreasing = T) 
  adf_alpha <- adf_alpha[alpha_o, ] 
  adf_o <- order(adf_alpha[, "PointEst"], decreasing = T) 
  adf_numeric <- adf_alpha[adf_o, ] 
  return(list(adf_alpha=adf_alpha, adf_numeric=adf_numeric) ) 
}

##############

#Exact Poisson
pconf <- function(x, y, dataf, conf_lev) {
  #Aggregates outcome by factor 
#  agr_sum <- aggregate(dataf[, y] ~ dataf[, x], FUN="sum", data= dataf)
#  agr_n <- aggregate(dataf[, y] ~ dataf[, x], FUN="length", data= dataf)
  agr_sum <- aggregate(dataf[, y], list(dataf[, x]), FUN="sum", na.rm=T)
  agr_n <- aggregate(dataf[, y], list(dataf[, x]), FUN="length")
  agr_df <- data.frame(x_lev=agr_sum[, 1], agr_sum=agr_sum[, 2], agr_n=agr_n[, 2])
  #Calculates confidence intervals
  adf_alpha <- matrix(ncol= 3, nrow= nrow(agr_df), byrow = TRUE)
  for (i in 1:nrow(agr_df)) {
    adf_alpha[i, ] <- unlist(poisson.test(x=agr_df[i,2], T=agr_df[i,3], conf.level= .95)[c("estimate","conf.int")])
  }
  adf_alpha <- data.frame(adf_alpha)
  colnames(adf_alpha) <- c("PointEst", "Lower", "Upper")
  rownames(adf_alpha) <- agr_df$x_lev
  alpha_o <- order(rownames(adf_alpha), decreasing = T) 
  adf_alpha <- adf_alpha[alpha_o, ] 
  adf_o <- order(adf_alpha[, "PointEst"], decreasing = T) 
  adf_numeric <- adf_alpha[adf_o, ] 
  return(list(adf_alpha=adf_alpha, adf_numeric=adf_numeric) ) 
}

conf <- function(x=xcivar, y=ycivar, dataf=df(), conf_lev=ciconf_lev) {
  switch(input$ci_type,                #"var" and can be used anywhere in server.r.
         "Mean (t)" =  tconf(x, y, dataf, conf_lev), 
         "Proportion (binomial)" =  bconf(x, y, dataf, conf_lev), 
         "Poisson (exact)" =  pconf(x, y, dataf, conf_lev) 
  )
}


#Point estimates and confidence intervals reactive function
cidf <- reactive({                  #This indicates the data frame I will use.
  if(input$CiCreate == "Yes") {
   conf(x=input$xcivar, y=input$ycivar, dataf=df(), conf_lev=input$ciconf_lev)
  }
})
#Use this for putting the output in the correct order
#cidf2 <- reactive({                  #This indicates the data frame I will use.
#  list("Alphabetical"=cidf()[["adf_alpha"]][order(rownames(cidf()[["adf_alpha"]]), decreasing = F), ], 
#       "Numerical"=cidf()[["adf_numeric"]][order(cidf()[["adf_numeric"]][["PointEst"]], decreasing = F), ])
#})

##########################################
# Plot function for confidence intervals #
##########################################
plot_ci_fnc <- function(xcivar, ycivar, ydf, cidf, ciconf_lev, alpha_num) {
  if (alpha_num=="Alphabetical") {
    adf <- cidf$adf_alpha
  }
  if (alpha_num=="Numerical") {
    adf <- cidf$adf_numeric
  }  
  mainYmn <- mean(ydf[, ycivar], na.rm=T)
  main_ttl <- paste0(ciconf_lev * 100, "% ", "Confidence Intervals of ", ycivar, " by ", xcivar)
  rng <- seq(min(adf), max(adf),length.out=nrow(adf))
  par(mar=c(5,7,4,4))
  plot(rng, 1:nrow(adf), type="n", ylab="", 
       xlab= paste0("Value (the grey vertical line is the overall mean of ", round(mainYmn, 3), ")"),
       main=main_ttl, axes=F ) 
    for (i in 1:nrow(adf)) {
    lines(c(adf[,'Lower'][i], adf[,'Upper'][i]), c(i,i), lwd=4, col=4) 
    points(adf[,'PointEst'][i],i, pch=24, col=2, lwd=1, bg=7, cex=1.75) 
  }
  abline(v=mainYmn, lwd=3, col="grey", lty=3)
  axis(1) 
  axis(2,at=1:nrow(adf),labels=substr(rownames(adf), 1, 10), las=1, cex.axis=1)
#  axis(2,at=1:nrow(adf),labels=rownames(adf), las=1, cex.axis=1)
  axis(4,at=1:nrow(adf),labels=round(adf[, "PointEst"],2), las=1, cex.axis=1)
  box()
}

#Confidence interval plot reactive function
plot_ci <- reactive({                  #This indicates the data frame I will use.
  if(input$CiCreate == "Yes") {
  plot_ci_fnc(xcivar=input$xcivar, ycivar=input$ycivar, ydf=df(), cidf=cidf(), 
              ciconf_lev=input$ciconf_lev, alpha_num=input$alpha_num)
  }
})

############################
## UI selection functions ##
############################

#Select the outcome
output$CIy <- renderUI({                                #Creates a UI function here but it will
  selectInput("ycivar", "1. Select the outcome.",       #get called to the UI file.
              choices = var(), multiple=FALSE, selected=var()[1] )   #Will make choices based on my reactive function.
})

#Select the predictors.
output$CIx <- renderUI({                                 #Same idea as output$vy
  selectInput("xcivar", "2. Select the factor.", 
              choices = setdiff(var(), input$ycivar), multiple=FALSE, selected=var()[2])     #Will make choices based on my reactive function.
})

#Select the CI type
output$Ci_Choice_Type <- renderUI({                                
  selectInput("ci_type", "3. Select the type of confidence interval.",
              choices = c("Proportion (binomial)", "Mean (t)", "Poisson (exact)"),
              selected= "Mean (t)", multiple=FALSE)
})

#Select the confidence interval level
output$Ci_Conf_Lev <- renderUI({                                 
  numericInput("ciconf_lev", "4. Enter the confidence level.",
               value = .95, min=.01, max = .99, step = .01)
})

#Select the sorting order.
output$Ci_Alpha_Num <- renderUI({                                #Creates a UI function here but it will
  radioButtons("alpha_num", "5. Sort alphabetically or numerically?",
               choices = c("Alphabetical", "Numerical"),
               selected="Alphabetical")
})

#Confidence interval plot
output$Plot_Ci_output <- renderPlot({ 
  if(input$CiCreate == "Yes") {
  plot_ci()
  }
  }, height = 700)

#Confidence interval values
output$Cidf_output <- renderPrint({ 
  #cidf2()
  if(input$CiCreate == "Yes") {
    list("Alphabetical"=cidf()[["adf_alpha"]][order(rownames(cidf()[["adf_alpha"]]), decreasing = F), ], 
       "Numerical"=cidf()[["adf_numeric"]][order(cidf()[["adf_numeric"]][["PointEst"]], decreasing = F), ])
    }
})

#Select whether to run the 95% confidence interval or not
output$Ci_create <- renderUI({                                #Creates a UI function here but it will
  radioButtons("CiCreate", "6. Create confidence intervals?",
               choices = c("No", "Yes"),
               selected="No")
})


#######################################
# Graphs for trajectories by time     #
#######################################

#Binomial
fbconf <- function(x, xlev, y, z, dataf, conf_lev) {
  #Aggregates outcome by factor 
  if( is.null(xlev)) {
    dataf <- dataf
  } else {
    dataf <- dataf[ dataf[, x] %in% xlev,  ]
  }
  agr_sum <- aggregate(dataf[, y], list(dataf[, x] , dataf[, z]), FUN="sum")
  agr_n <- aggregate(dataf[, y], list(dataf[, x] , dataf[, z]), FUN="length")
  agr_df <- data.frame(x_lev=agr_sum[, 1], z_lev=agr_sum[, 2], agr_sum=agr_sum[, 3], agr_n=agr_n[, 3])
  #Calculates confidence intervals
  agr_df <- cbind(agr_df, binconf(x=agr_df[,3], n=agr_df[,4], alpha=1 - conf_lev))
  return(agr_df) 
}

#Continuous outcomes
ftconf <- function(x, xlev, y, z, dataf, conf_lev) {
  #Aggregates outcome by factor 
  if( is.null(xlev)) {
    dataf <- dataf
  } else {
    dataf <- dataf[ dataf[, x] %in% xlev,  ]
  }
  agr_m <- aggregate(dataf[, y], list(dataf[, x] , dataf[, z]), FUN="mean")
  agr_sd <- aggregate(dataf[, y], list(dataf[, x] , dataf[, z]), FUN="sd")
  agr_n <- aggregate(dataf[, y], list(dataf[, x] , dataf[, z]), FUN="length")
  agr_df <- data.frame(x_lev=agr_m[, 1], z_lev=agr_m[, 2], agr_m=agr_m[, 3], agr_sd=agr_sd[, 3], agr_n=agr_n[, 3])
  #Calculates confidence intervals
  MOE <- qt((conf_lev/2)+.5, df=agr_df$agr_n - 1) * agr_df$agr_sd/sqrt(agr_df$agr_n)
  Lower <- agr_df$agr_m - MOE
  Upper <- agr_df$agr_m + MOE
  adf_alpha <- data.frame(cbind(PointEst=agr_df$agr_m, Lower=Lower, Upper=Upper))
  agr_df <- cbind(agr_df, adf_alpha)
  return(agr_df ) 
}

#Exact Poisson
fpconf <- function(x, xlev, y, z, dataf, conf_lev) {
  #Aggregates outcome by factor 
  if( is.null(XLeV)) {
    dataf <- dataf
  } else {
    dataf <- dataf[ dataf[, x] %in% xlev,  ]
  }
  agr_sum <- aggregate(dataf[, y] ~ dataf[, x]+ dataf[, z], FUN="sum")
  agr_n <- aggregate(dataf[, y] ~ dataf[, x]+ dataf[, z], FUN="length")
  agr_df <- data.frame(x_lev=agr_sum[, 1], z_lev=agr_sum[, 2], agr_sum=agr_sum[, 3], agr_n=agr_n[, 3])
  #Calculates confidence intervals
  adf_alpha <- matrix(ncol= 3, nrow= nrow(agr_df), byrow = TRUE)
  for (i in 1:nrow(agr_df)) {
    adf_alpha[i, ] <- unlist(poisson.test(x=agr_df[i,3], T=agr_df[i,4], conf.level= conf_lev)[c("estimate","conf.int")])
  }
  adf_alpha <- data.frame(adf_alpha)
  colnames(adf_alpha) <- c("PointEst", "Lower", "Upper")
  agr_df <- cbind(agr_df, adf_alpha)
  return(agr_df=agr_df ) 
}

fconf <- function(x=xcivar, xlev=xlev, y=ycivar, z=zcivar, dataf, conf_lev=ciconf_lev, Fci_type) {
  switch(Fci_type,                #"var" and can be used anywhere in server.r.
         "Mean (t)" =  ftconf(x, xlev, y, z, dataf, conf_lev), 
         "Proportion (binomial)" =  fbconf(x, xlev, y, z, dataf, conf_lev), 
         "Poisson (exact)" =  fpconf(x, xlev, y, z, dataf, conf_lev) 
  )
}

#Reactive function that runs fconf above
fcidf <- reactive({                  #This indicates the data frame I will use.
  if(input$FCiCreate == "Yes") {
    fconf(x=input$fxcivar, xlev=fci_plot_Group_Levels(), y=input$fycivar, z=input$fzcivar, 
          dataf=df(), conf_lev=input$fciconf_lev, Fci_type=input$fci_type)
  }
})

###############################################################
## Function here for the point estimate, lower, upper bounds ##
###############################################################
ci_fac_fnc <- function(x_lev, z_lev, agr_df, NK) {
  #ci_fac_fnc <- function(x_lev, z_lev, agr_df) {
  prmtrs <- c("PointEst", "Lower", "Upper")
  ctrs <- as.vector(unique(agr_df[, x_lev]))
  #Point est 
  ci_p <- list()
  for (i in 1:length(ctrs)) {
    x <- agr_df[agr_df[, x_lev] ==ctrs[i], z_lev]
    y <- agr_df[agr_df[, x_lev] ==ctrs[i], prmtrs[1]]
    xx <- rcspline.eval(x, inclx=TRUE, nk=NK)
    knots <- attr(xx, "knots")
    coef <- lsfit(xx, y)$coef
    w <- rcspline.restate(knots, coef[-1], x="{\\rm BP}")
    xtrans <- eval(attr(w, "function"))
    ci_p[[i]] <- cbind(x, y_p=coef[1] + xtrans(x), y)
  }
  #Lower CI
  ci_l <- list()
  for (i in 1:length(ctrs)) {
    x <- agr_df[agr_df[, x_lev]==ctrs[i], z_lev]
    y <- agr_df[agr_df[, x_lev]==ctrs[i], prmtrs[2]]
    xx <- rcspline.eval(x, inclx=TRUE, nk=NK)
    knots <- attr(xx, "knots")
    coef <- lsfit(xx, y)$coef
    w <- rcspline.restate(knots, coef[-1], x="{\\rm BP}")
    xtrans <- eval(attr(w, "function"))
    ci_l[[i]] <- cbind(x, y_l=coef[1] + xtrans(x), y)
  }
  #Upper CI
  ci_u <- list()
  for (i in 1:length(ctrs)) {
    x <- agr_df[agr_df[, x_lev]==ctrs[i], z_lev]
    y <- agr_df[agr_df[, x_lev]==ctrs[i], prmtrs[3]]
    xx <- rcspline.eval(x, inclx=TRUE, nk=NK)
    knots <- attr(xx, "knots")
    coef <- lsfit(xx, y)$coef
    w <- rcspline.restate(knots, coef[-1], x="{\\rm BP}")
    xtrans <- eval(attr(w, "function"))
    ci_u[[i]] <- cbind(x, y_u=coef[1] + xtrans(x), y)
  }
  #For point estimtaes only
  max_pest <- vector() 
  min_pest <- vector() 
  for(i in 1:length(ci_p)) {
    max_pest[i] <- max(ci_p[[i]][,2])
    min_pest[i] <- min(ci_p[[i]][,2])
  }
  #For CIs only
  max_ci <- vector() 
  min_ci <- vector() 
  for(i in 1:length(ci_p)) {
    max_ci[i] <- max(ci_u[[i]][,2])
    min_ci[i] <- min(ci_l[[i]][,2])
  }
  return(list(ci_p=ci_p, ci_l=ci_l, ci_u=ci_u, max_pest=max_pest, min_pest=min_pest,
              max_ci=max_ci, min_ci=min_ci, ctrs=ctrs))
}

#Reactive function that runs ci_fac_fnc() above
fci_fac <- reactive({                  #This indicates the data frame I will use.
  if(input$FCiCreate == "Yes") {
  ci_fac_fnc(x_lev="x_lev", z_lev="z_lev", agr_df=fcidf(), NK=input$FciNkKnots)
  }
})

######################################################
## Function to get the point overall point estimate ##
######################################################
fncFciTotMn <- function(y, z, dataf) {
  agr_mean <- aggregate(dataf[, y], list(dataf[, z]), FUN="mean")
  return(agr_mean) 
}

################################################
## Function to get the overall point estimate ##
################################################
fncAllTrndSpln <- function( agr_df, NK) {
  #Point est 
  ci_p <- list()
  x <- agr_df[ ,1]
  y <- agr_df[ ,2]
  xx <- rcspline.eval(x, inclx=TRUE, nk=NK)
  knots <- attr(xx, "knots")
  coef <- lsfit(xx, y)$coef
  w <- rcspline.restate(knots, coef[-1], x="{\\rm BP}")
  xtrans <- eval(attr(w, "function"))
  ci_p <- cbind(x, y_p=coef[1] + xtrans(x))
  return("ci_p"=ci_p )
}

############################################################
##            Function to create the time plot            ##
############################################################
plot_fci_fnc <- function(x, y, z, xcivar, ycivar, zcivar, dataf, LCol, Fci.Fac, 
                         ci_p, ci_l, ci_u,max_pest, min_pest, max_ci, min_ci, ctrs, 
                         cibands, fCiXLim1, fCiXLim2, fCiYLim1, fCiYLim2, Tot.Line, FCI.Tot,
                         FCI.Tot.Straight, Conf.Intrv, Tgt.Line, Straight.Line) {
  #Make text out of the confidence level
  ConINT <- paste0(as.character(Conf.Intrv*100), "%")
  #Main title
  if(cibands == "Yes") {
    Main.Title <- paste0( ycivar, " trajectories of ", xcivar,  " by ", zcivar, " with ", ConINT, " confidence bands")
  } else {
    Main.Title <- paste0( ycivar, " trajectories of ", xcivar,  " by ", zcivar)
  }
  
  #Set up colors
  my_clr <- LCol
  plot(unique(dataf[, z]), seq(min(min_ci, na.rm=T), max(max_ci, na.rm=T), length.out=length(unique(dataf[, z]))), type="n",  
       cex.lab=1.35,cex.main=1.35,cex.sub=1.35, 
       ylab=ycivar, xlab=zcivar, xlim=c(fCiXLim1, fCiXLim2), ylim=c(fCiYLim1, fCiYLim2),
       main= Main.Title )
  #Plot point estimate lines
  ci_time <- list() 
  l95 <- list() 
  u95 <- list() 
  xx_t <- list() 
  yy_t <- list() 
  if(Straight.Line == "Yes") {
    for (i in 1:length(ctrs)) {
      lines(ci_p[[i]][, "x"], ci_p[[i]][, "y"], lty=i, col= my_clr[i],lwd=2)
      text(ci_p[[i]][1, "x"], ci_p[[i]][1, "y"], ctrs[i])
      text(ci_p[[i]][nrow(ci_p[[i]]), "x"], ci_p[[i]][nrow(ci_p[[i]]), "y"], ctrs[i])
    }
  } else {
    for (i in 1:length(ctrs)) {
      lines(ci_p[[i]][, "x"], ci_p[[i]][, "y_p"], lty=i, col= my_clr[i],lwd=2)
      text(ci_p[[i]][1, "x"], ci_p[[i]][1, "y_p"], ctrs[i])
      text(ci_p[[i]][nrow(ci_p[[i]]), "x"], ci_p[[i]][nrow(ci_p[[i]]), "y_p"], ctrs[i])
    }
  }
  if(cibands == "Yes") {
    if(Straight.Line == "Yes") {
      for (i in 1:length(ctrs)) {
        ci_time[[i]] <- ci_l[[i]][,1]
        l95[[i]] <- ci_l[[i]][, "y"]
        u95[[i]] <- ci_u[[i]][, "y"]
        xx_t[[i]] <- c(ci_time[[i]], rev(ci_time[[i]]))
        yy_t[[i]] <- c(l95[[i]], rev(u95[[i]]))
        polygon(unlist(xx_t[[i]]), unlist(yy_t[[i]]), col = adjustcolor(my_clr[i], alpha.f = 0.25), 
                border=adjustcolor(my_clr[i], alpha.f = 0.25))
      }   
      } else {
        for (i in 1:length(ctrs)) {
          ci_time[[i]] <- ci_l[[i]][,1]
          l95[[i]] <- ci_l[[i]][, 2] #"y_p"
          u95[[i]] <- ci_u[[i]][, 2] #"y_p"
          xx_t[[i]] <- c(ci_time[[i]], rev(ci_time[[i]]))
          yy_t[[i]] <- c(l95[[i]], rev(u95[[i]]))
          polygon(unlist(xx_t[[i]]), unlist(yy_t[[i]]), col = adjustcolor(my_clr[i], alpha.f = 0.25), 
                  border=adjustcolor(my_clr[i], alpha.f = 0.25))
        }
    } 
  }
  #Add overall line
  if (Tot.Line == "Yes") {
    if(Straight.Line == "Yes") {
      lines(FCI.Tot.Straight, col="black", lty=1, lwd=7)  
    } else {
      lines(FCI.Tot, col="black", lty=1, lwd=7)  
    }
  }
  #Add target line
  abline(h=Tgt.Line, col="gray", lty=3, lwd=7)
}

#Confidence interval plot reactive function
plot_fci <- reactive({                  #This indicates the data frame I will use.
  if(input$FCiCreate == "Yes") {
    plot_fci_fnc(x="x_lev", y="PointEst", z="z_lev", xcivar=input$fxcivar, ycivar=input$fycivar, zcivar=input$fzcivar,
                 dataf=fcidf(), LCol= fci_plot_Line_Colors(), ci_p=fci_fac()$ci_p, ci_l=fci_fac()$ci_l, ci_u=fci_fac()$ci_u,
    max_pest=fci_fac()$max_pest, min_pest=fci_fac()$min_pest, max_ci=fci_fac()$max_ci, min_ci=fci_fac()$min_ci, 
    ctrs=fci_fac()$ctrs, cibands=input$fcibands, fCiXLim1=input$fCiXLim1, fCiXLim2=input$fCiXLim2, 
    fCiYLim1=input$fCiYLim1, fCiYLim2=input$fCiYLim2, Tot.Line=fci_overall_line(), FCI.Tot=fci_all_line(), 
    FCI.Tot.Straight=fci_tot_group_aggr(), Conf.Intrv=input$fciconf_lev, Tgt.Line=fCi_target_line(), Straight.Line= fCi_straight_line() )
    }
})

## Get the overall group trend rates ##
fci_tot_group_aggr <- reactive({
  fncFciTotMn(y=input$fycivar, z=input$fzcivar, dataf=df())
})
## Get the overall group trend line ##
fci_all_line <- reactive({
   fncAllTrndSpln(agr_df=fci_tot_group_aggr(), NK=input$FciNkKnots)
})

############
##   UI   ##
############
#Select the outcome
output$FCIy <- renderUI({                                #Creates a UI function here but it will
  selectInput("fycivar", "1. Select the outcome.",       #get called to the UI file.
              choices = var(), multiple=FALSE, selected=var()[1] )   #Will make choices based on my reactive function.
})

#Select the predictors.
output$FCIx <- renderUI({                                 #Same idea as output$vy
  selectInput("fxcivar", "2. Select the factor.", 
              choices = setdiff(var(), input$fycivar), multiple=FALSE, selected=var()[2])     #Will make choices based on my reactive function.
})

#Select the predictors.
output$FCIz <- renderUI({                                 #Same idea as output$vy
  selectInput("fzcivar", "3. Select a time variable.", 
              choices = setdiff(var(), c(input$fycivar, input$fxcivar)), multiple=FALSE, selected=var()[2])     #Will make choices based on my reactive function.
})
#Select specific groups
output$fciplot_grp_levs <- renderUI({                                 
  selectInput("fciPlotGrpLvs", "4. Highlight specific groups?", 
              choices = fci_plot_groups(), multiple=TRUE)     
})
#Reactive function to get group levels
fci_plot_Group_Levels <- reactive({                 
  input$fciPlotGrpLvs 
})
#Reactive function to get group levels
fci_plot_groups <- reactive({                 
    unique(df()[, input$fxcivar]) 
})
#Select the CI type
output$FCi_Choice_Type <- renderUI({                                
  selectInput("fci_type", "5. Select the type of confidence interval.",
              choices = c("Proportion (binomial)", "Mean (t)", "Poisson (exact)"),
              selected= "Mean (t)", multiple=FALSE)
})

#Select the confidence interval level
output$FCi_Conf_Lev <- renderUI({                                 
  numericInput("fciconf_lev", "6. Enter the confidence level.",
               value = .95, min=.01, max = .99, step = .01)
})
#Select the Confidence bands.
output$FCI_bands <- renderUI({                                 #Same idea as output$vy
  selectInput("fcibands", "7. Do you want confidence bands?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     #Will make choices based on my reactive function.
})
#Select line colors
output$fci_plot_ln_clrs <- renderUI({                                 
  selectInput("fciPltLnClr", "8. Select line colors.", 
              choices = xyplot_Line_Color_Names(), multiple=TRUE)     
})
#Reactive function for directly above
fci_plot_Line_Colors <- reactive({                 
  input$fciPltLnClr 
})
#Select how many knots I want
output$FCI_nk_knots <- renderUI({                                
      numericInput("FciNkKnots", "9. Select the number of spline knots.",
       value = 3, min=3, max = 10, step = 1)
})
#Select whether to run the 95% confidence interval or not
output$FCi_create <- renderUI({                                
  selectInput("FCiCreate", "10. Create the time plot?",
              choices = c("No", "Yes"),
              selected="No")
})
#Select whether to run the 95% confidence interval or not
output$FCi_ovral_line <- renderUI({                                
  selectInput("fciOvrLn", "11. Add the overall group line?",
              choices = c("No", "Yes"),
              selected="No")
})
#Reactive function for above
fci_overall_line <- reactive({ 
  input$fciOvrLn  
})
#Add a target line
output$FCi_Tgt_Line <- renderUI({                                 
  numericInput("fciTgtLn", "12. Add a target line.",
               value = NULL, step = .1)
})
#Reactive function for above
fCi_target_line <- reactive({ 
  input$fciTgtLn  
})

## Code for plot range
#Range of X value
range_fzcivar <- reactive({ 
  range(as.numeric(df()[, input$fzcivar]), na.rm=TRUE )  
})
#Range of Y value
range_fycivar <- reactive({ 
  range(as.numeric(df()[, input$fycivar]), na.rm=TRUE )  
})

#13. Indicate if you want a straight line
output$FCi_strght_ln <- renderUI({                                
  selectInput("fciStrtLn", "13. Use straight trend lines?",
              choices = c("No", "Yes"),
              selected="No")
})
#13A. Reactive function for above
fCi_straight_line <- reactive({ 
  input$fciStrtLn  
})
#14. Indicate lower limit of x-axis
output$FCI__Xlim1 <- renderUI({
  numericInput("fCiXLim1", "14. Lower X-axis limit.",
               value = range_fzcivar()[1], step = 1)
})
#15. Indicate upper limit of x-axis
output$FCI__Xlim2 <- renderUI({
  numericInput("fCiXLim2", "15. Upper X-axis limit.",
               value = range_fzcivar()[2], step = 1)
})
#16. Indicate lower limit of y-axis
output$FCI__Ylim1 <- renderUI({
  numericInput("fCiYLim1", "16. Lower Y-axis limit.",
               value = range_fycivar()[1], step = 1)
})
#17. Indicate upper limit of x-axis
output$FCI__Ylim2 <- renderUI({
  numericInput("fCiYLim2", "17. Upper Y-axis limit.",
               value = range_fycivar()[2], step = 1)
})

#Confidence interval plot for time
output$Plot_Fci_output <- renderPlot({ 
  if(input$FCiCreate == "Yes") {
    plot_fci()
  }
}, height = 800 )

#This prints the point estimates and confidence intervals
output$time_ci_out1 <- renderTable({
  if(input$FCiCreate == "Yes") {
    fcidf()
  }
}, rownames = TRUE)


############


################################################################################
## PREDs section: Begin  ##
################################################################################
output$SaveModelFit <- renderUI({  
  selectInput("save_mdl", "1. Save the model fit?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     #Will make choices based on my reactive function.
})

#Model fit object
mdlfit <- reactive({
  if(input$save_mdl == "Yes") {
    fit1()
  }
})

#The new data frame name for the model fit
output$mdl_fit_name <- renderUI({ 
  textInput("MdlFitName", "2. Enter the model fit name.", 
            value= "model_fit")     
})

output$model <- downloadHandler(
  filename = "model_fit.RData",
  content = function(con) {
    assign(input$MdlFitName, mdlfit())
    save(list=input$MdlFitName, file=con)
  }
)

#Upload model object
output$upload_model_fit <- renderUI({
  fileInput('mf1', '1. Choose a model.', accept=c('.RData'))
})

#Loads R file
fitInput <- reactive({
  sessionEnvir <- sys.frame()
  if (!is.null(input$mf1)) load(input$mf1$datapath, sessionEnvir)
})

#Data structure of the R file
output$mdl_print <- renderPrint({
  if (is.null(fitInput()))  return()  else print(fitInput())
})

#Upload data frame to save predictions into
output$upload_PRED_df <- renderUI({
  fileInput('pred_df', '1. Choose an RData File.', accept=c('.RData'))
})

#Loads R file
PredDataInput <- reactive({
  sessionEnvir <- sys.frame()
  if (!is.null(input$pred_df)) load(input$pred_df$datapath, sessionEnvir)
})

#Data structure of the R file
output$PREDdatastr <- renderPrint({
  if (is.null(PredDataInput()))  return()  else str(PredDataInput())
})


## This creates a function that applies model predictions to data 
fit_yhat_fnc <- function(fit1, fit_class, df) {
  switch(fit_class,
         "ols"   = new_pred_df <- data.frame(df, lpred=predict(fit1, type= "lp", newdata=df)),
         "lrm"   = new_pred_df <- data.frame(df, lpred=predict(fit1, type= "lp", newdata=df), ppred=predict(fit1, type= "fitted", newdata=df)),
         "orm"   = new_pred_df <- data.frame(df, lpred=predict(fit1, type= "lp", newdata=df), predict(fit1, type= "fitted.ind", newdata=df)),
         "Glm"   = new_pred_df <- data.frame(df, lpred=predict(fit1, type= "lp", newdata=df), epred=exp(predict(fit1, type= "lp", newdata=df))),
         "Rq"    = new_pred_df <- data.frame(df, lpred=predict(fit1, type= "lp", newdata=df)),
         "cph"   = new_pred_df <- data.frame(df, lpred=predict(fit1, type= "lp", newdata=df)),
         "Gls"   = new_pred_df <- data.frame(df, lpred=predict(fit1, type= "lp", newdata=df)))
  return(new_pred_df)
}

#Fit current model on data
#Indicate which data to use to fit the current model to the data
output$curr_fit_df <- renderUI({                                 
  textInput("CurrFit", label="1. Select the data for predictions.", value ="")     
})

output$fit_curr_mdl <- renderUI({ 
  selectInput("fitCurMdl", "2. Make predictions with current model?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")
})

#This runs the fit_yhat_fnc() function above for predictions using the current model in the shiny app
fit_yhat_curr_df <- reactive({
  if(input$fitCurMdl == "Yes") {
  fit_yhat_fnc(fit1= fit1(), fit_class= class(fit1())[1], df= get(input$CurrFit))
  }
})

#The new data frame name for the predictions based on the current model
output$curr_mdl_pred_df <- renderUI({ 
  textInput("CurrMdlPredDf", "3. Save the model fit name as:", 
            value= "PRED_df")     
})

#This saves my new data
output$pred_curr <- downloadHandler(
  filename = "PRED_df.RData",
  content = function(con) {
    assign(input$CurrMdlPredDf, fit_yhat_curr_df())
    save(list=input$CurrMdlPredDf, file=con)
  }
)

########
#Indicate which model fit to use
output$primary_fit <- renderUI({                                 
  textInput("PrimeFit", label="1. Select the model fit.", value ="")     
})

output$prime_mdl_smry <- renderPrint({
  print(get(input$PrimeFit))
})

####
#Fit a different model on data
#Indicate which data to use to fit a different model to the data
output$new_fit_df <- renderUI({                                 
  textInput("NewFit", label="2. Select the data for predictions.", value ="")     
})

output$fit_new_mdl <- renderUI({                                 
  selectInput("fitNewMdl", "3. Make predictions with an existing model?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")
})

#This runs the fit_yhat_fnc() function above for predictions using a different model in the shiny app
fit_yhat_new_df <- reactive({
  if(input$fitNewMdl == "Yes") {
    fit_yhat_fnc(fit1= get(input$PrimeFit), fit_class= class(get(input$PrimeFit))[1], df= get(input$NewFit))
  }
})

#The new data frame name for the predictions based on a different model
output$new_mdl_pred_df <- renderUI({ 
  textInput("NewMdlPredDf", "4. Save the model fit name as:", 
            value= "PRED_df")     
})

#This saves my new data
output$pred_new <- downloadHandler(
  filename = "PRED_df.RData",
  content = function(con) {
    assign(input$NewMdlPredDf, fit_yhat_new_df())
    save(list=input$NewMdlPredDf, file=con)
  }
)

################################################################################
##                      Multiple imputation                                   ##
################################################################################
#Select the predictors.
output$MIx <- renderUI({                                 #Same idea as output$vy
  selectInput("xMIvar", "1. Select the variables.", 
              choices = var(), multiple=TRUE, selected=var()[1])     #Will make choices based on my reactive function.
})
#Select the number of imputations.
output$MIn <- renderUI({                                 #Same idea as output$vy
  numericInput("MInumber", "2. Select the number of imputations.", value = 2, min=2, step=1)     #Will make choices based on my reactive function.
})
#Select the number of knots for the continuous variables.
output$MIks <- renderUI({                                 #Same idea as output$vy
  numericInput("MIknots", "3. Select the number of knots.", value = 4, min=0, step=1)     #Will make choices based on my reactive function.
})
#Determine if we should begin the multiple imputations.
output$MI_Begin <- renderUI({  
  selectInput("MIbegin", "4. Begin multiple imputation?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     
})

#This creates the formula to use for multiple imputation
mi_fmla <- reactive({             #Spline terms 
  as.formula(paste(paste0(" ", "~"),   
                   paste(input$xMIvar, collapse= "+")))
})
#Multiple imputation for data sets
mi <- reactive({
  if (input$MIbegin == "Yes") {
    set.seed(1)  
    aregImpute(mi_fmla(),
               data=df(), n.impute=input$MInumber,   #Proportion NA= .20 so we use 20 imputations.
               nk=input$MIknots,                   #Default 4 knots used when variables are used to impute other values.
               pr=FALSE)               #Suppress printing of iteration messages.
  }
})

#######################################################################
###   Single imputation for multiple imputation calibrate/validate  ###
#######################################################################
ptrans.si <-   reactive({
  if (input$MIbegin == "Yes") {
    set.seed(1)
    transcan(mi_fmla(), imputed=TRUE, transformed=FALSE, trantab=TRUE, pl=FALSE, show.na=TRUE, data=df(), pr=FALSE, 
             nk=input$SIknots, asis=input$asisx)
  }
})

###New data frame for "Imputed". Has original data and imputed values in the raw scale
imputed.si <- reactive({
  as.data.frame(impute(ptrans.si(), data=df(), list.out=TRUE))
})

#This merges imputed data with just the model variables that are not in the imputed data
new_imputed.si <- reactive({ 
  if (input$MIbegin == "Yes") {
    data.frame(imputed.si()[, colnames(imputed.si()) %in% c(outcome(), predictor())], 
               df()[, setdiff(c(outcome(), predictor()), colnames(imputed.si())[colnames(imputed.si()) %in% c(outcome(), predictor())]),  drop=FALSE])
  } 
})

############################
# Run single imputed model #
############################
atch.si <- reactive({                   
  if (input$MIbegin == "Yes") {
    attach(new_imputed.si())      
  } 
})
#This is the single imputation model fit for the calibration/validation/approximation tabs
fit.si <<- reactive({
  atch.si()
  dd_df.si <<- datadist(new_imputed.si()); options(datadist='dd_df.si');
  if (input$MIbegin == "Yes") {
    
    switch(input$regress_type,                #"var" and can be used anywhere in server.r.
           "Linear"   = if(input$updy == "Yes") {
             ols(as.formula(input$up_fmla), x=TRUE, y=TRUE, data=new_imputed.si())
           } else {
             ols(mdl_fmla(), x=TRUE, y=TRUE, data=new_imputed.si())},
           "Logistic" = if(input$updy == "Yes") {
             lrm(as.formula(input$up_fmla), x=TRUE, y=TRUE, data=new_imputed.si(), tol=1e-100, maxit=20) #I added tol value so it can handle time predictor (YYMM)
           } else { #Added maxit to handle small samples. See https://stat.ethz.ch/pipermail/r-help/2017-September/449115.html  
             lrm(mdl_fmla(), x=TRUE, y=TRUE, data=new_imputed.si(), tol=1e-100, maxit=20)},  #I added tol value so it can handle time predictor that causes "singularity"
           "Ordinal Logistic" = if(input$updy == "Yes") {
             orm(as.formula(input$up_fmla), x=TRUE, y=TRUE, data=new_imputed.si(), data=new_imputed.si())
           } else {
             orm(mdl_fmla(), x=TRUE, y=TRUE, data=new_imputed.si())},
           "Poisson" = if(input$updy == "Yes") {
             Glm(as.formula(input$up_fmla), x=TRUE, y=TRUE, data=new_imputed.si(), family=poisson())
           } else {
             Glm(mdl_fmla(), x=TRUE, y=TRUE, data=new_imputed.si(), family=poisson())},
           "Quantile" = if(input$updy == "Yes") {
             Rq(as.formula(input$up_fmla), x=TRUE, y=TRUE, data=new_imputed.si(), tau=as.numeric(rq_tau1()))
           } else {
             Rq(mdl_fmla(), x=TRUE, y=TRUE, data=new_imputed.si(), tau=as.numeric(rq_tau1()))},
           "Cox PH"   = if(input$updy == "Yes") {
             cph(cox_mdl_fmla1u(), x=TRUE, y=TRUE, data=new_imputed.si(), surv=TRUE)
           } else {
             cph(cox_mdl_fmla1(), x=TRUE, y=TRUE, data=new_imputed.si(), surv=TRUE)},
           "Cox PH with censoring" = if(input$updy == "Yes") {
             cph(cox_mdl_fmla2u(), x=TRUE, y=TRUE, data=new_imputed.si(), surv=TRUE)
           } else {
             cph(cox_mdl_fmla2(), x=TRUE, y=TRUE, data=new_imputed.si(), surv=TRUE)},
           #AFT models
           "AFT"   = if(input$updy == "Yes") {
             psm(aft_mdl_fmla1u(), data=new_imputed.si(), x=TRUE, y=TRUE, dist=AFT_PSM_Dist())
           } else {
             psm(aft_mdl_fmla1(), data=new_imputed.si(), x=TRUE, y=TRUE, dist=AFT_PSM_Dist())},
           "AFT with censoring" = if(input$updy == "Yes") {
             psm(aft_mdl_fmla2u(), data=new_imputed.si(), x=TRUE, y=TRUE, dist=AFT_PSM_Dist())
           } else {
             psm(aft_mdl_fmla2(), data=new_imputed.si(),  x=TRUE, y=TRUE, dist=AFT_PSM_Dist())},
           "Generalized Least Squares" = if(input$updy == "Yes") {
             Gls(as.formula(input$up_fmla), x=TRUE, data=new_imputed.si(), correlation=corCAR1(form= gls_cor()))
           } else {
             Gls(mdl_fmla(), x=TRUE, data=new_imputed.si(),
                 correlation=corCAR1(form= gls_cor()))}
    )
  }
})
#Multiple imputation results
output$MI_smry <- renderPrint({ 
  (mi())
})


############################
#       Describe           #
############################
#Render UIs to get variables
#Summary plot
#Select the outcome
output$desc_y <- renderUI({                                
  selectInput("describeY", "1. Select the target variable",        
              choices = var(), multiple=FALSE, selected=var()[1] ) 
})
#Select the predictors.
output$desc_x <- renderUI({                                 #Same idea as output$vy
  selectInput("describeX", "2. Select the predictors", 
              choices = setdiff(var(), desc_outcome()), multiple=TRUE, selected=var()[2]) 
})
output$desc_choice <- renderUI({  
  selectInput("DescChoice", "3. Run the summary now?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     
})
## Reactive functions ##
#Formula for describe section
desc_outcome <- reactive({                  #Outcome is my reactive function name I will use below. 
  input$describeY                      #variableY comes from the UI file drop down box.
})

desc_predictor <- reactive({             #Same idea as "outcome" 
  input$describeX  
})
desc_fmla <- reactive({             #Spline terms 
  as.formula(paste(paste0(desc_outcome() , "~"),   
                   paste(desc_predictor(), collapse= "+")))
})
#Summary plot for outcome by predictors
desc_smry <- reactive({             #Spline terms 
  if (input$DescChoice == "Yes") {
    summary(desc_fmla(), data=df())
  }
})
#This renders the summary plot
output$DescSmryPlt <- renderPlot({ 
  par(mar=c(2, 6, 1.5, 6))
  if (input$DescChoice == "Yes") {
    plot(desc_smry(), 
         cex.lab=.95, cex=1.25, col=2
         )
  }
#}, height = 700, width = 1000  )
}, height = 800, width = 1200  )

##########################################################################
##      Scatter plot with a correlation test and loess smoothing        ##
##########################################################################

#1. Y variable "Select the response variable"
output$sctr_crtst_y <- renderUI({                                
  selectInput("sctrCrtstY", "1. Select the Y-axis variable",        
              choices = var(), multiple=FALSE, selected=var()[1] ) 
})
#1A. Reactive function for the Y variable
scatter_cor_test_y <- reactive({
  input$sctrCrtstY
})
#2. X formula  "Select the explanatory variable"
#Select the predictors.
output$sctr_crtst_x <- renderUI({                                 #Same idea as output$vy
  selectInput("sctrCrtstX", "2. Select the X-axis variable", 
              choices = setdiff(var(), scatter_cor_test_y()), multiple=FALSE, selected=var()[2]) 
})
#2A. Reactive function for the X variable
scatter_cor_test_x <- reactive({
  input$sctrCrtstX
})
#3. correlation method
output$sctr_crtst_meth <- renderUI({                                
  selectInput("sctrCrtstMth", "3. Select the correlation method.",        
              choices = c("pearson", "kendall", "spearman"), multiple=FALSE, selected="pearson" ) 
})
#3A. Reactive function for the correlation method
scatter_cor_test_method <- reactive({
  input$sctrCrtstMth
})
#4. Alternative hypothesis test
output$sctr_crtst_alt <- renderUI({                                
  selectInput("sctrCrtstAlt", "4. Select a one- or two-sided test.",        
              choices = c("two.sided", "less", "greater"), multiple=FALSE, selected="two.sided" ) 
})
#4A. Reactive function for alternative hypothesis test
scatter_cor_test_alternative <- reactive({
  input$sctrCrtstAlt
})
#5. 
output$scatter_cor_test_CI <- renderUI({                                #
  numericInput("sctrCrtstCI", "5. Select the confidence interval level.",       #
               value = 0.95, min=0, max=1, step=.01 )   #
})
#5A. Reactive function for confidence interval
Scatter_Cor_Test_Conf_Int <- reactive({
  input$sctrCrtstCI
})
#6. Exact method
output$scatter_cor_test_exct <- renderUI({  
  textInput("sctrCrtstEM", "6. Use the exact method?", 
  #            choices = c("",TRUE, FALSE), multiple=FALSE, selected="")     
  value = NULL)
})
#6A. Reactive function for Exact method
Scatter_Cor_Test_Exact_Method <- reactive({
  input$sctrCrtstEM
})
#7. Exact method
output$scatter_cor_test_cnt <- renderUI({  
  selectInput("sctrCrtstCC", "7. Use the continuity correction?", 
              choices = c(TRUE, FALSE), multiple=FALSE, selected=FALSE)     
})
#7A. Reactive function for Exact method
Scatter_Cor_Test_Cont_Correct <- reactive({
  input$sctrCrtstCC
})
#8. Line color
output$sctr_crtst_clr <- renderUI({                                
  selectInput("sctrCrtstClr", "8. Select the plot's line color.",        
              choices = c("red","orange","yellow","green","blue","purple","gray","black"), 
              multiple=FALSE, selected="red" ) 
})
#8A. Reactive function for alternative hypothesis test
scatter_cor_line_color <- reactive({
  input$sctrCrtstClr
})
#9. Exact method
output$scatter_cor_test_run_YN <- renderUI({  
  selectInput("sctrCrtstYN", "9. Run the correlation and plot?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     
})
#9A. Reactive function for Exact method
Scatter_Cor_Test_Run_Yes_No <- reactive({
  input$sctrCrtstYN
})
#10. Run the function below
scatter_cor_test_cor_run <- reactive({
  if(Scatter_Cor_Test_Run_Yes_No() == "Yes") {    
    fncSctrPltCr(DF=df(), X=scatter_cor_test_x(), Y=scatter_cor_test_y(), 
                 sct_plt_alt=scatter_cor_test_alternative(), 
                 sct_plt_meth=scatter_cor_test_method() , 
                 sct_plt_exct= eval(parse(text=Scatter_Cor_Test_Exact_Method() )) , 
                 
                 #sct_plt_exct=Scatter_Cor_Test_Exact_Method(), 
                 sct_plt_ci_lv=Scatter_Cor_Test_Conf_Int(), 
                 sct_plt_cont=Scatter_Cor_Test_Cont_Correct() 
    )
  }  
})
#10A.Correlation test output  
output$scatter_cor_test_cor_test_out <- renderPrint({
  if(Scatter_Cor_Test_Run_Yes_No() == "Yes") {
    scatter_cor_test_cor_run()
  }
})
#11. Run the function below
scatter_cor_test_plt_run <- reactive({
  if(Scatter_Cor_Test_Run_Yes_No() == "Yes") {    
    fncSctrPlt(DF=df(), X=scatter_cor_test_x(), Y=scatter_cor_test_y(), 
                 sct_plt_clr=scatter_cor_line_color(), CT=scatter_cor_test_cor_run()
    )
  }  
})
#11A.Scatter plot  
output$scatter_cor_test_plt_out <- renderPlot({
  if(Scatter_Cor_Test_Run_Yes_No() == "Yes") {
    scatter_cor_test_plt_run()
  }
})

######################################
## Function to get correlation test ##
######################################
fncSctrPltCr <- function(DF, X, Y, 
                         sct_plt_alt, 
                         sct_plt_meth , 
                         sct_plt_exct, 
                         sct_plt_ci_lv, 
                         sct_plt_cont
                         ) {
  #Correlation test
  CT <- cor.test(x=DF[, X], y=DF[, Y], alternative = sct_plt_alt,
                 method = sct_plt_meth, 
                 exact = sct_plt_exct, 
                 conf.level = sct_plt_ci_lv, 
                 continuity = sct_plt_cont, 
                 na.rm=TRUE)
  return("Correlation.Test"= CT)
}
##################################
## Function to get scatter plot ##
##################################
fncSctrPlt <- function(DF, X, Y, sct_plt_clr, CT) {
  #Scatter plot
    plot(DF[, X], DF[, Y] , main= paste0("Correlation of ", Y, " on ", X, 
                                        " (correlation= ", round(as.numeric(CT["estimate"]), 3), 
                                        ", ", "p-value= ", try(round(as.numeric(CT["p.value"]), 4)), ")"),
         xlab=X, ylab=Y)
    lw1 <- loess(formula= as.formula(paste(Y, "~", X)), data=DF)
    j <- order(DF[, X])
    lines(DF[, X][j], lw1$fitted[j],col= sct_plt_clr,lwd=3)
}

##########################################################################
## summaryRc plot of continuous Y by continuous X with a stratification ##
##########################################################################

#1. Y variable "Select the response variable"
output$smryRc_y <- renderUI({                                
  selectInput("smryrcY", "1. Select the continuous outcome variable",        
              choices = var(), multiple=FALSE, selected=var()[1] ) 
})
#1A. Reactive function for the Y variable
smryRc_outcome <- reactive({
  input$smryrcY
})
#2. X formula  "Select the explanatory variable"
#Select the predictors.
output$smryRc_x <- renderUI({                                 #Same idea as output$vy
  selectInput("smryrcX", "2. Select the continuous predictor", 
              choices = setdiff(var(), smryRc_outcome()), multiple=FALSE, selected=var()[2]) 
})
#2A. Reactive function for the X variable
smryRc_X_var <- reactive({
  input$smryrcX
})
# stratify  
#3. Do you want to stratify by a factor
output$smryRc_strat_yes_no <- renderUI({  
  selectInput("smryRcStratYN", "3. Do you want to stratify?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     
})
#4. Select a stratification variable
output$smryRc_z <- renderUI({                                 #Same idea as output$vy
  selectInput("smryrcZ", "4. Select the stratification variable", 
              choices = setdiff(var(), c(smryRc_outcome(),smryRc_X_var )), multiple=FALSE, 
              selected=setdiff(var(), c(smryRc_outcome(),smryRc_X_var() ))[1]) 
})
#4A. Reactive function for the stratification variable
smryRc_Z_var <- reactive({
  input$smryrcZ
})
#5. Run the graph
output$smryrc_choice <- renderUI({  
  selectInput("smryRcChoice", "5. Run the summary plot now?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     
})
#6. Run the function below
summaryRC_plot_function_run <- reactive({
  if(input$smryRcChoice == "Yes") {
    if(input$smryRcStratYN == "Yes") {    
    fncSumRcPlot(X=smryRc_X_var(), Y=smryRc_outcome(), Z=smryRc_Z_var(), DF=df())
  }  else {
    fncSumRcPlot(X=smryRc_X_var(), Y=smryRc_outcome(), Z=NULL, DF=df())
  }
} 
})
#6A. Summary plot output 
output$summaryRC_plot_function_out <- renderPlot({
  if(input$smryRcChoice == "Yes") {
    summaryRC_plot_function_run()
  }
})
## Function that creates summaryRc plot ##
fncSumRcPlot <- function(X, Y, Z=NULL, DF) {
  Vnms <- X
  #Create formula
  if(is.null(Z)) {
    fmla <- as.formula(paste(paste0(Y , "~"),   
                             paste(Vnms, collapse= "+") ))
  } else {
    fmla <- as.formula(paste(paste0(Y , "~"),   
                             paste(Vnms, collapse= "+"), "+ stratify(", Z,")" ))
  }
  #Create summary plot
  if(is.null(Z)) {
    summaryRc(fmla, data=DF, col=c(1,2), cex.quant=1.5, lwd=2, datadensity=TRUE, trim=.01,
              nloc=FALSE)
  } else {
    summaryRc(fmla, data=DF, col=c(1:length(levels(as.factor(DF[, Z] )))), cex.quant=1.5, lwd=2,
              datadensity=TRUE, trim=.01, label.curves=list(keys='lines'), nloc=FALSE)
  }
}


## Missing variable plots
#Select the outcome
output$miss_y <- renderUI({                                
  selectInput("missY", "1. Select the target variable",        
              choices = var(), multiple=FALSE, selected=var()[1] ) 
})
#Select the predictors.
output$miss_x <- renderUI({                                 #Same idea as output$vy
  selectInput("missX", "2. Select the predictors", 
              choices = setdiff(var(), miss_outcome()), multiple=TRUE, selected=var()[2]) 
})
output$miss_choice <- renderUI({  
  selectInput("MissChoice", "3. Run the summary now?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     
})
## Reactive functions ##
#Formula for describe section
miss_outcome <- reactive({                  #Outcome is my reactive function name I will use below. 
  input$missY                      #variableY comes from the UI file drop down box.
})
#Predictors for missing section
miss_predictor <- reactive({ 
  input$missX  
})
#Formula for missing section
miss_fmla <- reactive({             #Spline terms 
  as.formula(paste(paste0("is.na(", miss_outcome(),")" , "~"),   
                   paste(miss_predictor(), collapse= "+")))
})
#Summary plot for outcome by predictors
miss_smry <- reactive({             #Spline terms 
  if (input$MissChoice == "Yes") {
    summary(miss_fmla(), data=df())
  }
})
#This renders the summary plot for missing values. Tried , pch=19 but didn't work
output$MissSmryPlt <- renderPlot({ 
  if (input$MissChoice == "Yes") {
    plot(miss_smry(), main="Proportion missing", col=2)
  }
}, height = 700, width = 1000  )
#
na.patterns <- reactive({             
  #naclus(df()[, c(miss_outcome(), miss_predictor())]) #Recusive and summary impacted, easier to use df() only
    naclus(df())
})
#This renders the wire plot for missing values. Tried , pch=19 but didn't work
output$naPlt <- renderPlot({ 
  if (input$MissChoice == "Yes") {
    naplot(na.patterns(), 'na per var', col=2)
  }
}, height = 700, width = 1000)
#This renders the dendogram for missing values
output$naDendo <- renderPlot({ 
  if (input$MissChoice == "Yes") {
    plot(na.patterns())
  }
}, height = 700, width = 1000 )
#This does the recursive partitioning of the 
who.na <- reactive({             #Spline terms 
  if (input$MissChoice == "Yes") {
    rpart(miss_fmla(), data=df(), minbucket=15)
  }
})
#This renders the tree diagram for missing values
output$naTree <- renderPlot({ 
  if (input$MissChoice == "Yes") {
    plot(who.na(), margin= .1)
    text(who.na())
  }
}, height = 700, width = 1000)
#Logistic regression to predict missingness .
lrm_miss <- reactive({             #Spline terms 
  if (input$MissChoice == "Yes") {
    lrm(miss_fmla(), data=df(), tol=1e-100, maxit=20)  #added "tol" to handle fitting issues
  }                                                    #Added maxit to handle small samples. 
})                                                     #See https://stat.ethz.ch/pipermail/r-help/2017-September/449115.html  
#Print regression output
output$smry_lrm_miss <- renderPrint({
  lrm_miss()
})
#Print the anova summary table of the regression above 
output$anova_lrm_miss <- renderPrint({
  anova(lrm_miss())
})

################################################################################
#                           CALCULATOR                                         #
################################################################################
#1. Textbox to enter a formula
output$calculator_box <- renderUI({  
  textInput("calcBox", "1. Enter a mathematical equation.")
})
#1A. Reactive function for textbox to enter a formula
Calculator_Box_Input <- reactive({
  input$calcBox
})                                                     
#2. Run the formula
output$calculator_yesno <- renderUI({                                 
  selectInput("calcYN", "2. Do you want to calculate the results?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")  
})
#2A. Reactive function for textbox to enter a formula
Calculator_YN <- reactive({
  input$calcYN
})                                                     
#Print the calculation 
output$prnt_calculation <- renderPrint({
  if (Calculator_YN() == "Yes") {
    eval(parse(text=Calculator_Box_Input() ))
  }
})

################################################################################
#                           Summarize the data                                 #
################################################################################
#1. Select the variabes.
output$desc_summ_vars <- renderUI({
  selectInput("dscSmmVrs", "1. Select the variables",
              choices = var(), multiple=TRUE, 
              selected=var()[1])
})
#1A. Reactive function for textbox to enter a formula
descriptive_summary_variables <- reactive({
  input$dscSmmVrs
})                                                     
#2. Run the formula
output$des_summ_yesno <- renderUI({                                 
  selectInput("dscSmYN", "2. Do you want to summarize the data?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")  
})
#2A. Reactive function for textbox to enter a formula
Desc_Summary_YN <- reactive({
  input$dscSmYN
})                                                     
#3. Reactive function to get summary values
Desc_Summary_Central_Tendency <- reactive({
  summary(df()[, descriptive_summary_variables()])
})                                                     
#4. Reactive function to get summary values
Desc_Summary_SD <- reactive({
  apply(df()[, descriptive_summary_variables(), drop=FALSE], 2, sd, na.rm=TRUE)
})                                                     
#5. Reactive function for the coefficient of variation
Desc_Summary_COV <- reactive({
  #Desc_Summary_SD()/ colMeans(df()[, descriptive_summary_variables(), drop=FALSE], na.rm=TRUE)
  suppressWarnings(fncSmryCOV(DF=df(), X=descriptive_summary_variables(), 
                              SD= Desc_Summary_SD()))
})                                                     
#Print the summary 
output$prnt_desc_summ <- renderPrint({
  if (Desc_Summary_YN() == "Yes") {
    list("Summary"= Desc_Summary_Central_Tendency(),
         "Standard.Deviation"= Desc_Summary_SD(),
         "Coefficient.of.Variation"= try(Desc_Summary_COV()) )
  }
})

###############################################
##  Function to get coefficient of variation ##
###############################################
fncSmryCOV <- function(DF, X, SD) {
  #Make the SD a matrix
  aSD <- matrix(SD, nrow=1)
  #Get the means
  aMEAN <- lapply(DF[,X],  mean, na.rm=T)
  #Make the mean a matrix
  aMEAN <- matrix(unlist(aMEAN), nrow=1)
  COV <- as.vector(sweep(aSD, 1, unlist(aMEAN), "/") )
  names(COV) <- X
  return(COV)
}

################################################################################
##                        Summary X Histogram                                 ##
################################################################################
#1. Select the histogram variabe.
output$smry_var_hist_var <- renderUI({
  selectInput("smryVrHstVr", "1. Select the variable",
              choices = var(), multiple=FALSE, 
              selected=var()[1])
})
#1A. Reactive function for the variabe
descriptive_summary_histogram_variable <- reactive({
  input$smryVrHstVr
})
#1B. Summary mean
smry_var_hist_mean <- reactive({
  summary(df()[, descriptive_summary_histogram_variable()])["Mean"]
})
#1C. Summary median
smry_var_hist_median <- reactive({
  summary(df()[, descriptive_summary_histogram_variable()])["Median"]
})
#2. Select the approximate number of histogram bars
output$smry_var_hist_bars <- renderUI({                                 
  numericInput("smryVrHstBrs", "2. Select the approximate number of histogram bars.", 
               value = 15, step = 1, min=2)     
})
#2A. Object for histogram bars 
summary_variable_histogram_bars <- reactive({
  input$smryVrHstBrs
})
#3. Bar color
output$smry_var_hist_bar_clr <- renderUI({                                
  selectInput("smryVrHstBrClr", "3. Select the bar color.",        
              choices = c("red","orange","yellow","green","blue","purple","gray","black"), 
              multiple=FALSE, selected="blue" ) 
})
#3A. Reactive function for Bar color
summary_var_histogram_bar_color <- reactive({
  input$smryVrHstBrClr
})
#4. Indicate if you want to show the mean and median
output$smry_hist_mn_med_yesno <- renderUI({                                 
  selectInput("smryHstMnMdYN", "4. Want to show the mean and median?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     
})
#4A. Object for the mean and median 
summary_hist_mean_median_yes_no <- reactive({
  input$smryHstMnMdYN
})
#5. Line colors
output$smry_var_hist_ln_clr <- renderUI({                                
  selectInput("smryVrHstLnClr", "5. Select the line colors.",        
              choices = c("red","orange","yellow","green","blue","purple","gray","black"), 
              multiple=TRUE, selected="black" ) 
})
#5A. Reactive function for the line color
summary_var_histogram_line_color <- reactive({
  input$smryVrHstLnClr
})
#6. Indicate if you want the histogram
output$smry_var_hist_yesno <- renderUI({                                 
  selectInput("smryVrHstYN", "6. Do you want to run the histogram?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     
})
#6A. Object for classification plot 
summary_var_hist_yes_no <- reactive({
  input$smryVrHstYN
})
#7. Run the histogram function below
summary_var_histogram_run <- reactive({
  if(summary_var_hist_yes_no() == "Yes") {    
    fncSmryHist(DF=df(), X=descriptive_summary_histogram_variable(), 
                BNS=summary_variable_histogram_bars(), CLR=summary_var_histogram_bar_color(), 
                LCLR=summary_var_histogram_line_color(), MN=smry_var_hist_mean(), 
                MED=smry_var_hist_median(), AddLine=summary_hist_mean_median_yes_no())
  }  
})
#7A.histogram  
output$summary_var_histogram_out <- renderPlot({
  if(summary_var_hist_yes_no() == "Yes") {
    summary_var_histogram_run()
  }
})

###############################
## Function to get histogram ##
###############################
fncSmryHist <- function(DF, X, BNS, CLR, LCLR, MN, MED, AddLine) {
  hist(x=DF[, X], breaks=BNS, col=CLR, xlab= X,
       main= paste0("Histogram of ", X, " (Mean= ", round(MN, 3),", Median= ", round(MED, 3), ")"))
  #Add mean and median lines
  if (AddLine== "Yes") {
    abline(v=MN,  col= head(LCLR, 1), lwd=3, lty=1)
    abline(v=MED, col= tail(LCLR, 1), lwd=3, lty=2)
  }
}

################################################################################
##           Density plot trend over time by groups                           ##
################################################################################
#1. Select the outcome variable.
output$dnsty_grp_trnd_Yvar <- renderUI({
  selectInput("dnsGrpTrnY", "1. Select the outcome.",
              choices = var(), multiple=FALSE, 
              selected=var()[1])
})
#1A. Reactive function for the outcome variable
density_group_trend_outcome <- reactive({
  input$dnsGrpTrnY
})
#1B. Reactive function for the density of the outcome variable
density_group_trend_Y_density <- reactive({
  density(df()[, density_group_trend_outcome()], na.rm=TRUE) 
})
#2. Select the grouping variable.
output$dnsty_grp_trnd_Xvar <- renderUI({
  selectInput("dnsGrpTrnX", "2. Select the group factor.",
              choices = setdiff(var(), density_group_trend_outcome()), multiple=FALSE, 
              selected=var()[2])
})
#2A. Reactive function for the variable
density_group_trend_group <- reactive({
  input$dnsGrpTrnX
})
#2B. Reactive function for the group levels
density_group_trend_grp_levels <- reactive({
  unique(df()[, density_group_trend_group()])
})
#3. Select the specific groups.
output$dnsty_grp_trnd_Xlevs <- renderUI({
  selectInput("dnsGrpTrXlev", "3. Select specific groups.",
              choices = density_group_trend_grp_levels(), multiple=TRUE)
})
#3A. Reactive function for the variable
density_group_trend_grp_X_levs <- reactive({
  input$dnsGrpTrXlev
})
#4. Select the time indicator.
output$dnsty_grp_trnd_Zvar <- renderUI({ 
  selectInput("dnsGrpTrnZ", "4. Select the time indicator.", 
              choices = setdiff(var(), c(density_group_trend_outcome(), density_group_trend_group() ) ), 
              multiple=FALSE, selected= setdiff(var(), c(density_group_trend_outcome(), density_group_trend_group() ) )[1])
})
#4A. Reactive function for the variable
density_group_trend_time <- reactive({
  input$dnsGrpTrnZ
})
#4B. Reactive function for the range of time
density_group_trend_range_time <- reactive({
  range(as.numeric(df()[, density_group_trend_time()]), na.rm=TRUE)
})
#5. Select the rolling time period 
output$dnsty_grp_trnd_Z_inc <- renderUI({                                 
  numericInput("dnsGrpTrnZInc", "5. Select time increments (3= 3 months).", 
               value = 1, step = 1)     
})
#5A. Reactive function for the variable
density_group_trend_Time_Increment <- reactive({
  input$dnsGrpTrnZInc
})
#6. Line color
output$dnsty_grp_trnd_ln_clr <- renderUI({                                
  selectInput("dnsGrpTrnLClr", "6. Select the line color.",        
              choices = c("red","orange","yellow","green","blue","purple","gray","black"), 
              multiple=FALSE, selected="red" ) 
})
#6A. Reactive function for line color
density_group_trend_line_color <- reactive({
  input$dnsGrpTrnLClr
})
#7. Select the rolling time period 
output$dnsty_grp_trnd_trgt <- renderUI({                                 
  numericInput("dnsGrpTrnTrgt", "7. Set a target.", 
               value = NULL, step = .01)     
})
#7A. Reactive function for the variable
density_group_trend_Target <- reactive({
  input$dnsGrpTrnTrgt
})
#8. Legend location
output$dnsty_grp_trnd_lgd_loc <- renderUI({                                
  selectInput("dnsGrpTrnLgdLoc", "8. Select the legend location.",        
              choices = c("bottomright","bottom","bottomleft","left","topleft","top","topright","right","center"), 
              multiple=FALSE, selected="red" ) 
})
#8A. Reactive function for legend location
density_group_trend_legend_location <- reactive({
  input$dnsGrpTrnLgdLoc
})
#9. Set the seed 
output$dnsty_grp_trnd_st_sed <- renderUI({                                 
  numericInput("dnsGrpTrnStSd", "9. Set (seed) group name randomness.", 
               value = 1, step = 1, min=1)     
})
#9A. Reactive function for the variable
density_group_trend_set_seed <- reactive({
  input$dnsGrpTrnStSd
})
#10. Indicate if you want to run the trend function
output$dnsty_grp_trnd_run_yesno <- renderUI({                                 
  selectInput("dnsGrpTrnRunYN", "10. Run the trend?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     
})
#10A. Object for yes/no running of the plot 
density_group_trend_run_yes_no <- reactive({
  input$dnsGrpTrnRunYN
})
#11. Speed of the animation play 
output$dnsty_grp_trnd_sec <- renderUI({                                 
  numericInput("dnsGrpTrnSec", "11. Play speed (1000= 1 second).", 
               value = 500, step = 100, min=0)     
})
#11A. Reactive function for the variable
density_group_trend_seconds <- reactive({
  input$dnsGrpTrnSec
})
#12. Play the trend graph
output$dnsty_grp_trnd_ply <- renderUI({
  sliderInput("dnsGrpTrnPly", "12. Play the time trend.",   
              min= density_group_trend_range_time()[1], 
              max= density_group_trend_range_time()[2] - (density_group_trend_Time_Increment() - 1), 
              value =1, step=1, animate=list(interval= density_group_trend_seconds() ))  
})
#12A. Reactive function for the variable
density_group_trend_play <- reactive({
  input$dnsGrpTrnPly
})
#13. Indicate lower limit of x-axis
output$dnsty_grp_trnd_Xlim1 <- renderUI({
  numericInput("dnsGrpTrnX1", "13. Lower X-axis limit.",
               value = min(density_group_trend_Y_density()[["x"]], na.rm=TRUE), step = .1)
})
#13A. Reactive function for the variable
density_group_trend_Xlim1 <- reactive({
  input$dnsGrpTrnX1
})
#14. Indicate upper limit of x-axis
output$dnsty_grp_trnd_Xlim2 <- renderUI({
  numericInput("dnsGrpTrnX2", "14. Upper X-axis limit.",
               value = max(density_group_trend_Y_density()[["x"]], na.rm=TRUE), step = .1)
})
#14A. Reactive function for the variable
density_group_trend_Xlim2 <- reactive({
  input$dnsGrpTrnX2
})
#15. Indicate lower limit of y-axis
output$dnsty_grp_trnd_Ylim1 <- renderUI({
  numericInput("dnsGrpTrnY1", "15. Lower Y-axis limit.",
               value = min(density_group_trend_Y_density()[["y"]], na.rm=TRUE), step = .1)
})
#15A. Reactive function for the variable
density_group_trend_Ylim1 <- reactive({
  input$dnsGrpTrnY1
})
#16. Indicate upper limit of Y-axis
output$dnsty_grp_trnd_Ylim2 <- renderUI({
  numericInput("dnsGrpTrnY2", "16. Upper Y-axis limit.",
               value = max(density_group_trend_Y_density()[["y"]], na.rm=TRUE), step = .1)
})
#16A. Reactive function for the variable
density_group_trend_Ylim2 <- reactive({
  input$dnsGrpTrnY2
})
#17. Run trend output function below  
density_group_trend_output <- reactive({
  if(density_group_trend_run_yes_no() == "Yes") {    
    fncTmDnsty(DF=df(), X= density_group_trend_group(), Y= density_group_trend_outcome(), 
               Z= density_group_trend_time(), Increment= density_group_trend_Time_Increment(), 
               Seed.Multiplier= density_group_trend_set_seed() )
  }  
})
#18. Plot function below  
density_group_trend_plot <- reactive({
  if(density_group_trend_run_yes_no() == "Yes") {    
    fncTmDnstPlot(TDList= density_group_trend_output(), X= density_group_trend_group(), 
                  Y= density_group_trend_outcome(), Z= density_group_trend_time(), 
                  Period= density_group_trend_play(), Lcol= density_group_trend_line_color(), 
                  Target= density_group_trend_Target(), Groups= density_group_trend_grp_X_levs(), 
                  Legend.Loc= density_group_trend_legend_location(), 
                  Xmin=density_group_trend_Xlim1(), Xmax=density_group_trend_Xlim2(), 
                  Ymin=density_group_trend_Ylim1(), Ymax= density_group_trend_Ylim2()) 
  }  
})
#18A. Trend plot  
output$dnsty_grp_trnd_plot <- renderPlot({
  if(density_group_trend_run_yes_no() == "Yes") {
    density_group_trend_plot()
  }
})
#19. Each period's output  
density_group_trend_by_time <- reactive({
  if(density_group_trend_run_yes_no() == "Yes") {    
    fncTmDnstOut(TDList= density_group_trend_output(), 
                 Period=density_group_trend_play(), Target= density_group_trend_Target())  
  }  
})
#19A. Each period's output  
output$dnsty_grp_trnd_out_by_tm <- renderPrint({
  if(density_group_trend_run_yes_no() == "Yes") {    
    density_group_trend_by_time()
  }
})

################################################################################
## Function to create aggregated values and density plot over time increments ##
################################################################################
fncTmDnsty <- function(DF, X, Y, Z, Groups, Increment, Period, Xmin, Xmax, Ymax, Seed.Multiplier) {
  #Get summary of values
  Group.Names <- unique(DF[, X]) 
  Group.Names.Length <-  length(Group.Names)
  Time.Period.Length <- length(unique(DF[, Z]))
  Increment.Length <- Time.Period.Length - (Increment - 1)
  Time.Period.Values <- unique(DF[, Z])
  oTPV <- order(Time.Period.Values)
  Time.Period.Values <- Time.Period.Values[oTPV]
  #Get time periods for aggregating rates  
  i <- 1
  Time.Periods <- list()
  while( i <= Increment.Length) {
    Time.Periods[[i]] <- Time.Period.Values[i:(i + (Increment -1))] 
    i = i + 1
  }
  #Aggregate values
  AggrY <- list()
  for (i in 1:length(Time.Periods )) {0
    AggrY[[i]] <- aggregate(DF[DF[, Z] %in% Time.Periods[[i]], Y] ~ DF[DF[, Z] %in% Time.Periods[[i]], X], FUN=mean)
    colnames(AggrY[[i]]) <- c(X, Y)
  }
  
  #Overall density values
  D1 <- density(x = DF[, Y], na.rm=TRUE )
  XLim <- range(D1[["x"]])
  YLim <- range(D1[["y"]])
  
  #Main title: Get first and last time points
  Time.Start.Label <- lapply(Time.Periods, `[[`, 1)
  Time.Stop.Label <- lapply(Time.Periods, `[[`, Increment) 
  
  #Values: Lists with each value by time point and group names
  DXname <- lapply(AggrY, function(x) x[-length(x)])
  DYvals <- lapply(AggrY, function(x) x[length(x)])
  
  #Density values by groups and time
  DYvals <- lapply(DYvals, unlist)
  D2 <- lapply(DYvals,density, na.rm=TRUE)
  
  #Means and medians by groups
  YTmean <- lapply(DYvals, mean)
  YTmedian <- lapply(DYvals, median)
  
  
  #Get X and Y density values for the line to set as boundaries for text names
  DxDY <- list()
  for (j in 1:Increment.Length ) {
    DxDY[j] <- (lapply(D2[j], function(x) x[1:2]))
  }
  
  #I need to create a vector with the exact number of elements I need. Gets matching values within line
  Dx.Cord <- vector(mode = "list", length = Increment.Length)
  Dy.Cord <- vector(mode = "list", length = Increment.Length)
  Dy.Cord.Random <- vector(mode = "list", length = Increment.Length)
  for (i in 1:nrow(DXname[[i]]) ) {
    for (j in 1:Increment.Length ) {
      Dx.Cord[[j]][i] <- DxDY[[j]][[ 1]][which( min(abs(DxDY[[j]][[1]] - DYvals[[j]][i])) == abs(DxDY[[j]][[1]] - DYvals[[j]][i] ))]
      Dy.Cord[[j]][i] <- DxDY[[j]][[ 2]][which( min(abs(DxDY[[j]][[1]] - DYvals[[j]][i])) == abs(DxDY[[j]][[1]] - DYvals[[j]][i] ))]
      set.seed(i * Seed.Multiplier)
      Dy.Cord.Random[[j]][i] <- runif(1, min = 0.015, max = Dy.Cord[[j]][i])
      
    }
  }
  
  return(list("AggrY"=AggrY, "Time.Periods"=Time.Periods,
              "D1"=D1, "D2"=D2, "XLim"=XLim, "YLim"=YLim, "YTmean"=YTmean, "YTmedian"=YTmedian,  
              "Time.Start.Label"=Time.Start.Label, "Time.Stop.Label"=Time.Stop.Label,
              "DXname"=DXname, "DYvals"=DYvals, "DxDY"=DxDY, "Dx.Cord"=Dx.Cord, "Dy.Cord"=Dy.Cord, "Dy.Cord.Random"=Dy.Cord.Random,  
              "Increment"=Increment, "Time.Period.Length"=Time.Period.Length, 
              "Increment.Length"=Increment.Length, "Seed.Multiplier"=Seed.Multiplier))
}

################################################################################
##    Function to Plot density of aggregated values  over time increments     ##
################################################################################
fncTmDnstPlot <- function(TDList, X, Y, Z, Period, Lcol, Target, Groups,
                          Legend.Loc,Xmin, Xmax, Ymin, Ymax) {
  Increment.Length <- TDList[["Increment.Length"]]
  #Title
  if(TDList[["Increment"]] == 1) {
    Main.Title <- paste(Y," rate by ", Z, ": ", 
                        TDList[["Time.Start.Label"]][[Period]],  sep= "")
  } else {
    Main.Title <- paste(Y," rate by ", Z, ": ", 
                        TDList[["Time.Start.Label"]][[Period]], " - ",                      
                        TDList[["Time.Stop.Label"]][[Period]],  sep= "")
  }
  #Density plot
  plot(TDList[["D2"]][[ Period]], xlim=c(Xmin,Xmax), ylim= c(Ymin, Ymax),  
       col=Lcol, lwd=8  ,
       main=Main.Title, xlab= "Rate" 
  )
  #Add vertical lines
  abline(v=Target, col="blue", lwd=2)           
  abline(v= mean(TDList[["AggrY"]][[1]][[2]]), col=colors()[102], lwd=2)
  abline(v= mean(TDList[["AggrY"]][[Increment.Length]][[2]]), col=colors()[102], lwd=2, lty=2)
  ###Rug and text to identify the med centers.###
  rug(TDList[["DYvals"]][[Period]], side=1, col=Lcol)
  #Text for group values
  if ( is.null(Groups) ) {  
    text(TDList[["DYvals"]][[Period]], unlist(TDList[["Dy.Cord.Random"]][Period]),   
         labels= unlist(TDList[["DXname"]][[Period]][X]),
         cex=2)
  } else {
    non_groups <- setdiff(unlist(TDList[["DXname"]][[Period]][[X]]) , Groups)   #Get excluded groups
    Groups.Temp <- as.character(unlist(TDList[["DXname"]][[Period]][[X]]))      #Get all groups
    Groups.Temp[which(Groups.Temp %in% non_groups )] <- ""                      #Change non-groups to blanks
    text(TDList[["DYvals"]][[Period]], unlist(TDList[["Dy.Cord.Random"]][Period]),   
         labels= Groups.Temp, cex=2)
  }
  ###Legend###
  #This creates a legend for the ablines with and without targets. TDList[["AggrY"]][[Period]] YTmean
  if ( !is.numeric(Target) ) {  
    legend(x=Legend.Loc, legend=c(paste0("Starting pooled mean: ", round(mean(TDList[["AggrY"]][[1]][[2]], na.rm=TRUE), 3)),
                                  paste0("Ending pooled mean: ", round(mean(TDList[["AggrY"]][[Increment.Length]][[2]], na.rm=TRUE), 3) )),
           col=c(colors()[102], colors()[102], "blue"),
           lty= c(1,2,1), lwd= 1.5, cex = 1.5, bty="n", inset=c(0, .05))
  } else {  
    #legend(Legend.Loc, legend=c("Starting mean","Ending mean", "Target"),
           legend(Legend.Loc, legend=c(paste0("Starting pooled mean: ", round(mean(TDList[["AggrY"]][[1]][[2]], na.rm=TRUE), 3)),
                                       paste0("Ending pooled mean: ", round(mean(TDList[["AggrY"]][[Increment.Length]][[2]], na.rm=TRUE), 3) ) , "Target"),
           col=c(colors()[102], colors()[102], "blue"),
           lty= c(1,2,1), lwd= 1.5, cex = 1.5, bty="n", inset=c(0, .05))
  }
}

################################################################################
##           Function to get aggregated values over time increments           ##
################################################################################
fncTmDnstOut <- function(TDList, Period, Target) {
  out.by.period <- TDList[["AggrY"]][[Period]]

  #High Target
  HT.table <- vector(length=2)
  if ( is.numeric(Target) ) {  
    HT.table <- prop.table(table(out.by.period[, 2] < Target))
    if ( length(HT.table) == 1 ) {  
      names(HT.table) <- "All"
      } else {
        names(HT.table) <- c("At or above target", "Below target")
      }
  } else {
    HT.table <- NA
  }    
  #Low Target
  LT.table <- vector(length=2)
  if ( is.numeric(Target) ) {  
    LT.table <- prop.table(table(out.by.period[, 2] >= Target))
    if ( length(LT.table) == 1 ) {  
      names(LT.table) <- "All"
    } else {
      names(LT.table) <- c("At or below target", "Above target")
    }
  } else {
    LT.table <- NA
  }
  return(list("Period Rate"= out.by.period, "High Target"= HT.table, "Low Target"= LT.table))
}


################################################################################
##                       Cost analysis with the Cox PH model                  ##
################################################################################

########################################################
#   Plot 1: Density plots on cost by intervention type #
########################################################
#Homemade functions
#This plots the outcome for all patients
cost_plot1 <- function(y, x, df) {
  #####Color codes#####
  c_red <- 2; c_blue <- 4
  red_trnspt  <- adjustcolor(c_red, alpha.f = 0.4)                                #Set red color transparency.
  blue_trnspt <- adjustcolor(c_blue, alpha.f = 0.4)                               #Set blue color transparency.

  d_ctl <- density(df[, y], na.rm=T, adjust=2)                       #Controls
  cst_tbl <- c(mean(df[, y], na.rm=T),
               median(df[, y], na.rm=T))
  names(cst_tbl) <- c("Mean", "Median" )
  
  plot(d_ctl, main=paste0("Density plot of ", y), 
       xlab=y, axes=F)
  polygon(d_ctl, col=blue_trnspt, border=blue_trnspt) 
  
  abline(v=median(df[, y], na.rm=T),
         col="blue")
  abline(v=mean(df[, y], na.rm=T),
         col="blue", lty=3)
  axis(1)
  legend("top", legend=c("Mean", "Median"), 
         col=c(4,4), lty=c(3,1),
         lwd=2, cex=1)
  legend("top", legend=c(paste0("Mean= ",round(cst_tbl["Mean"], 0)), paste0("Median= ", round(cst_tbl["Median"], 0))), 
         col=c(4,4), lty=c(3,1),
         lwd=2, cex=1.25)
  
  box()
}

##################
# UPDATE START HERE #
##################
#This plots the outcome by a binary grouping variable
cost_plot1_grp <- function(y, x, df) {
  #####Color codes#####
  c_red <- 2; c_blue <- 4
  red_trnspt  <- adjustcolor(c_red, alpha.f = 0.4)                                #Set red color transparency.
  blue_trnspt <- adjustcolor(c_blue, alpha.f = 0.4)                               #Set blue color transparency.
  
  d_ctl <- density(df[, y][as.numeric(df[, x]) == min(as.numeric(df[, x]))], na.rm=T, adjust=2) #Controls
  d_trt <- density(df[, y][as.numeric(df[, x]) == max(as.numeric(df[, x]))], na.rm=T, adjust=2) #x
  #Treatment/control group means/medians
  ctl_med <- median(df[, y][as.numeric(df[, x]) == min(as.numeric(df[, x]))], na.rm=T)
  trt_med <- median(df[, y][as.numeric(df[, x]) == max(as.numeric(df[, x]))], na.rm=T)
  ctl_mn <- mean(df[, y][as.numeric(df[, x]) == min(as.numeric(df[, x]))], na.rm=T)
  trt_mn <- mean(df[, y][as.numeric(df[, x]) == max(as.numeric(df[, x]))], na.rm=T)
  
  plot(d_trt, main=paste0("Density plot of ", y, " by ", x), 
       xlab=y, axes=F)
  lines(d_ctl)
  polygon(d_ctl, col=red_trnspt, border=red_trnspt) 
  polygon(d_trt, col=blue_trnspt, border=blue_trnspt) 
  
  ###  
  abline(v=ctl_med, col="red")
  abline(v=trt_med, col="blue")
  abline(v=ctl_mn, col="red", lty=3)
  abline(v=trt_mn, col="blue", lty=3)
  axis(1)
  legend("top", legend=c(paste0("Treatment: Mean= ",round(trt_mn, 0)), paste0("Treatment: Median= ", round(trt_med, 0)), 
                         paste0("Control: Mean= ", round(ctl_mn, 0)), paste0("Control: Median= ", round(ctl_med, 0))), 
         col=c(4,4,2,2), lty=c(3,1,3,1),
         lwd=2, cex=1.25)
  box()
}

#Function that gets cost value quantiles between the treatment and control groups 
cost_quant <- function(y, x, df) {
  
  #Treatment/control group means/medians
  ctl_med <- median(df[, y][as.numeric(df[, x]) == min(as.numeric(df[, x]), na.rm=T)], na.rm=T)
  trt_med <- median(df[, y][as.numeric(df[, x]) == max(as.numeric(df[, x]), na.rm=T)], na.rm=T)
  ctl_mn <- mean(df[, y][as.numeric(df[, x]) == min(as.numeric(df[, x]), na.rm=T)], na.rm=T)
  trt_mn <- mean(df[, y][as.numeric(df[, x]) == max(as.numeric(df[, x]), na.rm=T)], na.rm=T)
  #### Get quantiles and bind with means. Partially used in the legend and the observed table in a later section
  qt1 <- quantile(df[, y][as.numeric(df[, x]) == min(as.numeric(df[, x]), na.rm=T)], probs=c(.1, .25, .5, .75, .9), na.rm=T)
  qt2 <- quantile(df[, y][as.numeric(df[, x]) == max(as.numeric(df[, x]), na.rm=T)], probs=c(.1, .25, .5, .75, .9), na.rm=T)
  odf_mqt <- as.data.frame(rbind(cbind(round(qt2, 2), round(qt1 ,2)),
                                 cbind(round(trt_mn, 2), round(ctl_mn, 2)) ))
  rownames(odf_mqt) <- c("p10", "p25", "p50", "p75", "p90", "Mean")
  colnames(odf_mqt) <- c("Treatment", "Control")  #Observed data.frame of the means and quantiles
  ###  
  return(list("odf_mqt"=odf_mqt))
}

#Select the type of density plot to run (full sample or by a group)
output$dens_plt1_typ <- renderUI({
  selectInput("DensPlt1Typ", "1. Choose the type of analysis.", 
              choices = c("Unstratified", "Stratified"), multiple=FALSE, selected="Unstratified")     
})
#Select the grouping variable for the density plot with 2 plots--value of 1=treatment group
output$dens_plt1_x <- renderUI({
  selectInput("DensPlt1X", "2. Select a binary stratified variable (optional).", 
              choices = predictor())     
})

#Create a reactive function for the stratified variable in the Cost density plot
dens_stratified <- reactive({                  #Outcome is my reactive function name I will use below. 
  input$DensPlt1X                      #DensPlt1X comes from the UI file drop down box.
})

#Create yes/no box to plot the density plot
output$run_dens_plt1 <- renderUI({ 
  selectInput("RunDensPlt1", "3. Do you want to run the plots?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     
})

#This runs the cost_plot1() function above
dens_plt1 <- reactive({
  if(input$RunDensPlt1 == "Yes") {
    
    if(input$DensPlt1Typ == "Stratified") {
      cost_plot1_grp(y= outcome(), x= dens_stratified() , df= df())
    } else {
      cost_plot1(y= outcome(), df= df())
    }
  }  
})
#This plots the density plot    
output$plot_dens_plt1 <- renderPlot({
    dens_plt1() 
}, height = 700)

#This gets quantiles and means
cost_quant_run <- reactive({
  if(input$RunDensPlt1 == "Yes") {
    
    if(input$DensPlt1Typ == "Stratified") {
      cost_quant(y= outcome(), x= dens_stratified() , df= df())
    } 
  }  
})

#Creates mean function for the fit
mn <- reactive({
  if(input$RunDensPlt1 == "Yes") {
    Mean(fit1())       # Creates function to compute the mean
  }  
})
#Creates median function for the fit
med <- reactive({
  if(input$RunDensPlt1 == "Yes") {
    Quantile(fit1())       # Creates function to compute quantiles
  }  
})


############################################
#   Plot 2: Estimated cost over quantiles  #
############################################
quant_ests_fnc <- function(fit, Y, X, reg) {
  #Creates function that will make correct value 
  MED_cox <<- Quantile(fit)                                                       #Creates function to compute quantiles

  #5. Cost--get predicted values at different quantiles
  if (reg %in% c("Cox PH", "Cox PH with censoring")) {
  p1.50 <- Predict(fit, fun=function(x) MED_cox(lp=x))                           #MED_coxian
  p1.10 <- Predict(fit, fun=function(x) MED_cox(q=.90, lp=x))                    #10th percentile
  p1.25 <- Predict(fit, fun=function(x) MED_cox(q=.75, lp=x))                    #25th percentile
  p1.75 <- Predict(fit, fun=function(x) MED_cox(q=.25, lp=x))                    #75th percentile
  p1.90 <- Predict(fit, fun=function(x) MED_cox(q=.10, lp=x))                    #90th percentile
  }
  if (reg == "Ordinal Logistic") {
    p1.50 <- Predict(fit, fun=function(x) MED_cox(lp=x))                           #MED_coxian
    p1.10 <- Predict(fit, fun=function(x) MED_cox(q=.10, lp=x))                    #10th percentile
    p1.25 <- Predict(fit, fun=function(x) MED_cox(q=.25, lp=x))                    #25th percentile
    p1.75 <- Predict(fit, fun=function(x) MED_cox(q=.75, lp=x))                    #75th percentile
    p1.90 <- Predict(fit, fun=function(x) MED_cox(q=.90, lp=x))                    #90th percentile
  }
  
  #This gets predicted intervention scores for different percentiles
  int_pnm <- c(paste0(X,".1"), paste0(X,".2")) #Only place I use X argument
  est1.10 <- p1.10[which(row.names(p1.10) %in% int_pnm), c("yhat", "lower", "upper")]
  est1.25 <- p1.25[which(row.names(p1.25) %in% int_pnm), c("yhat", "lower", "upper")]
  est1.50 <- p1.50[which(row.names(p1.50) %in% int_pnm), c("yhat", "lower", "upper")]
  est1.75 <- p1.75[which(row.names(p1.75) %in% int_pnm), c("yhat", "lower", "upper")]
  est1.90 <- p1.90[which(row.names(p1.90) %in% int_pnm), c("yhat", "lower", "upper")]
  #This changes it to a data.frame class from an RMS class
  class(est1.10) <- "data.frame"
  class(est1.25) <- "data.frame"
  class(est1.50) <- "data.frame"
  class(est1.75) <- "data.frame"
  class(est1.90) <- "data.frame"
  est1 <- data.frame(rbind(unlist(est1.10), unlist(est1.25), unlist(est1.50),     #Merge the results
                           unlist(est1.75), unlist(est1.90)))
  
  #Re-arrange order to get values set up for display in the plot
  est2 <- est1[, c(2,6,4,1,5,3)]
  colnames(est2) <- c("TREATMENT", "L95", "U95","Control", "L95", "U95")
  rownames(est2) <- c("p10", "p25", "p50","p75", "p90")
  #Add cost difference between Treatment and controls 
  est2$Diff. <- abs(round(est2$Control) - round(est2$TREATMENT))
  ## Add in weighted average into the table
  #Make row of data
  wtot <- as.data.frame(matrix(nrow=1, ncol=7))
  colnames(wtot) <- c("TREATMENT", "L95", "U95","Control", "L95", "U95", "Diff.")
  rownames(wtot) <- "Weighted"
  #Calculate weighted average
  wtot[, "Diff."] <- ((.25 * est2[rownames(est2) =="p10", "Diff."]) + (.25 * est2[rownames(est2) =="p25", "Diff."]) + 
           (.25 * est2[rownames(est2) =="p50", "Diff."]) + (.15 * est2[rownames(est2) =="p75", "Diff."]) + 
           (.1 * est2[rownames(est2) =="p90", "Diff."]))
  #Combine everything
  est2 <- rbind(est2, wtot)
  return(list(est1=est1, est2=est2))
}  

#This runs the quant_plt1_fnc() function above
quant_ests <- reactive({
  if(input$RunDensPlt1 == "Yes") {
    
    if(input$DensPlt1Typ == "Stratified") {
    #  quant_ests_fnc(fit=fit1(), Y= outcome(), X= input$DensPlt1X, reg=input$regress_type)
      quant_ests_fnc(fit=fit1(), Y= outcome(), X= dens_stratified(), reg=input$regress_type)
    } 
  }  
})

  #Graph on differences between intervention groups on expected costs over percentiles 
quant_plt1_fnc <- function(ests, Y) {
  est1 <- quant_ests()[["est1"]]
  est2 <- quant_ests()[["est2"]]
  #Later shades for second plot
  c_red <- 2; c_blue <- 4
  red_trnspt2  <- adjustcolor(c_red, alpha.f = 0.2)                               #Set red color transparency.
  blue_trnspt2 <- adjustcolor(c_blue, alpha.f = 0.2)                              #Set blue color transparency.
  plot(1:5, est1[, 1], type="n",
       ylab=Y, xlab="Percentiles",
       ylim=c(0, max(est1, na.rm=T)*.9), xlim=c(1, 5),
       axes=F, main=paste0("Estimated ", Y, " over quantiles"))
  lines(1:5, est1[, 1], col="red", lwd=3)
  lines(1:5, est1[, 2], col="blue", lwd=3)
  #Set up coordinates for polygon function, NOTE: UPPER AND LOWER MEAN THE OPPOSITE 
  #BECAUSE A COX MODEL VIEWS COST IN THE OPPOSITE DIRECTION.
  axis(2)
  axis(1, at=1:5, tick=F,  labels= c("p10", "p25", "p50", "p75", "p90")) #pos=1,
  xxt <- c(1:5,5:1)
  yya <- c(est1$lower2, rev(est1$upper2))  #Treatment
  yyc <- c(est1$lower1, rev(est1$upper1))  #Control
  #plot(xx,yyd, type="n", ylim=c(0,1))
  polygon(xxt, yya, col = blue_trnspt2, border=blue_trnspt2)
  polygon(xxt, yyc, col = red_trnspt2, border=red_trnspt2)
  legend("topleft", legend=c("Treatment","Control"), 
         col=c(4,2), lty=1,
         lwd=2, cex=1.25)
  box()
}

#This runs the quant_plt1_fnc() function above
quant_plt1 <- reactive({
  if(input$RunDensPlt1 == "Yes") {
    
    if(input$DensPlt1Typ == "Stratified") {
      quant_plt1_fnc(ests=quant_ests(), Y= outcome())
    } 
  }  
})

#This plots the quantile plot    
output$plot_quant_plt1 <- renderPlot({
  if(input$regress_type == "Quantile") {
    qr_plt1()
  } else {
    quant_plt1()
  }
}, height = 700)

#This prints the point estimates and confidence intervals
output$quant_out1 <- renderTable({
  if(input$regress_type == "Quantile") {
    qr_ests()[["est2"]]
  } else {
    quant_ests()[["est2"]]
  }
}, rownames = TRUE)

##########################
### Get mean estimates ###
##########################
mean_ests_fnc <- function(fit, Y, X, reg) {
  #Creates function that will make correct value 
  MN_cox <<- Mean(fit)                                                       #Creates function to compute quantiles
  
  #5. Cost--get predicted values at different quantiles
  if (reg %in% c("Cox PH", "Cox PH with censoring")) {
    p1.mean <- Predict(fit, fun=function(x) MN_cox(lp=x))                           #MN_coxian
  }
  if (reg == "Ordinal Logistic") {
    p1.mean <- Predict(fit, fun=function(x) MN_cox(lp=x))                           #MN_coxian
  }
  
  #This gets predicted intervention scores for different percentiles
  int_pnm <- c(paste0(X,".1"), paste0(X,".2")) #Only place I use X argument
  est1.mean <- p1.mean[which(row.names(p1.mean) %in% int_pnm), c("yhat", "lower", "upper")]
  #This changes it to a data.frame class from an RMS class
  class(est1.mean) <- "data.frame"
  est1 <- data.frame(rbind(unlist(est1.mean)))
  
  #Re-arrange order to get values set up for display in the plot
  est2 <- est1[, c(2,6,4,1,5,3)]
  colnames(est2) <- c("TREATMENT", "L95", "U95","Control", "L95", "U95")
  rownames(est2) <- "Mean"
  #Add cost difference beteen Treatment and controls 
  est2$Diff. <- abs(round(est2$Control) - round(est2$TREATMENT))
  return(list(est1=est1, est2=est2))
}  

#This runs the quant_plt1_fnc() function above
mean_ests <- reactive({
  if(input$RunDensPlt1 == "Yes") {
    
    if(input$DensPlt1Typ == "Stratified") {
      #mean_ests_fnc(fit=fit1(), Y= outcome(), X= input$DensPlt1X, reg=input$regress_type)
      mean_ests_fnc(fit=fit1(), Y= outcome(), X= dens_stratified(), reg=input$regress_type)
    } 
  }  
})
#This prints the point estimates and confidence intervals
output$mean_out1 <- renderTable({
  mean_ests()[["est2"]]
}, rownames = TRUE)

## This extracts the observed data.frame means and quantiles from the reactive function below ##
obs_df_mqt <- reactive({
  if(input$RunDensPlt1 == "Yes") {
    
    if(input$DensPlt1Typ == "Stratified") {
      cost_quant_run()$odf_mqt
    } 
  }  
})
#This prints the means and quantiles
output$obsdfmqt_out1 <- renderTable({
  obs_df_mqt()
}, rownames = TRUE)

#################################### Begin here
##1. Creates a plot for an interaction of continuous X by a factor ##
#Select the continuous predictor
output$Cxyplot_x <- renderUI({
  selectInput("CXyplotX", "1. Select a continuous predictor.", 
              choices = predictor(), selected=predictor()[1], multiple=FALSE)     
})
#Continuous predictor
CXyplotX1 <- reactive({                  
  input$CXyplotX
})
#2. Select the factor for grouping
output$Cxyplot_z <- renderUI({
  selectInput("CXyplotZ", "2. Select a factor.", 
              choices = setdiff(predictor(), input$CXyplotX),  multiple=FALSE)     
})
#Factor
CXyplotZ1 <- reactive({                  
  input$CXyplotZ
})
#3. Make confidence bands for the plot
output$Cxyplot_bands <- renderUI({                                 
  selectInput("CXyplotBands", "3. Do you want confidence bands?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")
})
#Reactive function for directly above
Cxyplot_Bands_YesNo <- reactive({                 
  input$CXyplotBands 
})
#4. Select line colors
output$Cxyplot_line_clrs <- renderUI({                                 
  selectInput("CXypltLnClr", "4. Select line colors.", 
              choices = xyplot_Line_Color_Names(), multiple=TRUE)     
})
#Reactive function for directly above
Cxyplot_Line_Colors <- reactive({                 
  input$CXypltLnClr 
})
#5. Create yes/no box to make the XY plot
output$Cxyplot_yes_no <- renderUI({                                 
  selectInput("CXyplotYesNo", "5. Do you want to create the plot?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     
})
#Reactive function for directly above
Cxyplot_create_YesNo <- reactive({                 
  input$CXyplotYesNo 
})
#Reactive function to get group levels
Cxyplot_groups <- reactive({                 
  if(Cxyplot_create_YesNo() == "Yes") {
    unique(CxyplotData()[, CXyplotZ1()]) 
  }
})
#Select specific groups
output$Cxyplot_grp_levs <- renderUI({                                 
  selectInput("CxyplotGrpLvs", "6. Highlight specific groups?", 
              choices = Cxyplot_groups(), multiple=TRUE)     
})
#Reactive function to get group levels
Cxyplot_Group_Levels <- reactive({                 
  input$CxyplotGrpLvs 
})
#7. Indicate lower limit of x-axis
output$Cxyplot_Xlim1 <- renderUI({
  numericInput("CxyplotLmX1", "7. Lower X-axis limit.",
               value = Cxylm()$XMin, step = .1)
})
#7A. Reactive function for the variable
Cxyplot_X_limit_1 <- reactive({
  input$CxyplotLmX1
})
#8. Indicate upper limit of x-axis
output$Cxyplot_Xlim2 <- renderUI({
  numericInput("CxyplotLmX2", "8. Upper X-axis limit.",
               value = Cxylm()$XMax, step = .1)
})
#8A. Reactive function for the variable
Cxyplot_X_limit_2 <- reactive({
  input$CxyplotLmX2
})
#9. Indicate lower limit of y-axis
output$Cxyplot_Ylim1 <- renderUI({
  numericInput("CxyplotLmY1", "9. Lower Y-axis limit.",
               value = Cxylm()$YMin, step = .1)
})
#9A. Reactive function for the variable
Cxyplot_Y_limit_1 <- reactive({
  input$CxyplotLmY1
})
#10. Indicate upper limit of Y-axis
output$Cxyplot_Ylim2 <- renderUI({
  numericInput("CxyplotLmY2", "10. Upper Y-axis limit.",
               value = Cxylm()$YMax, step = .1)
})
#10A. Reactive function for the variable
Cxyplot_Y_limit_2 <- reactive({
  input$CxyplotLmY2
})
#Place all XY limits in a list reactive function
Cxylm_all <- reactive({
  if(Cxyplot_create_YesNo() == "Yes") {
    list( "XMin"=Cxyplot_X_limit_1(), "XMax"=Cxyplot_X_limit_2(),
          "YMin"=Cxyplot_Y_limit_1(),"YMax"=Cxyplot_Y_limit_2() )
  }
})

## Get the Predicted values needed for the plot ##
CxyplotData <- reactive({                 
  do.call("Predict", list(fit1(), CXyplotX1(), CXyplotZ1(), fun= function(x)x*-1) )    
})
#Set up function to get XY limits from an XYplot
Cxylm_run <- reactive({
  if(Cxyplot_create_YesNo() == "Yes") {
    fncXYpltLmts(xyplotDF=CxyplotData(), ContX=CXyplotX1(), GroupX=CXyplotZ1())
  }
})
#Get XY limits from an XYplot
Cxylm <- reactive({                  
  Cxylm_run()
})

#Set up function to create the XYplot
CXyplt_setup <- reactive({
  if(Cxyplot_create_YesNo() == "Yes") {
    fncXYpltInt(DF=df(), xyplotDF=CxyplotData(), ContX= CXyplotX1(), GroupX= CXyplotZ1(), 
                GroupLevs= Cxyplot_Group_Levels(), XYlims= Cxylm_all(), Clrs= Cxyplot_Line_Colors(), CIbands=Cxyplot_Bands_YesNo()) 
  }
})
#This creates the plot
output$CxYplot_interaction <- renderPlot({
  CXyplt_setup()
}, height = 800)

##########################################
# Contrast plots for partial predictions #
##########################################
## UI functions
#Create the levels for the XY plot group argument
#Level 1
output$Cxyplot_con_lev1 <- renderUI({
  selectInput("CxyplotConLev1", "1. Select the primary level",
              choices = Cxy_contrast_levs() , multiple=FALSE, selected= Cxy_contrast_levs()[1])
})
#Level 2
output$Cxyplot_con_lev2 <- renderUI({
  selectInput("CxyplotConLev2", "2. Select the reference level",
              choices = Cxy_contrast_levs() , multiple=FALSE,
              selected= setdiff(Cxy_contrast_levs(), input$CxyplotConLev1)[1] )
})
#Yes/No on running the plots
output$Cxyp_yes_no <- renderUI({ #Same idea as output$vy
  selectInput("CXypYesNo", "3. Do you want to run the contrast plot?",
              choices = c("No", "Yes"), multiple=FALSE,
              selected="No")     #Will make choices based on my reactive function.
})
#Create the x-axis limits for the XY plot group argument
#Lower X
output$Cxyplot_con_xlim0 <- renderUI({
  if (input$CXyplotYesNo=="Yes") {
    numericInput("CxyplotConXlim0", "4. Select the lower x-axis limit",
                 value=Ccon_xy_data()[["xlim0"]], step=1)
  }
})
#Upper X
output$Cxyplot_con_xlim1 <- renderUI({
  if (input$CXyplotYesNo=="Yes") {
    numericInput("CxyplotConXlim1", "5. Select the upper x-axis limit",
                 value=Ccon_xy_data()[["xlim1"]], step=1)
  }
})
#Create the y-axis limits for the XY plot group argument
#Lower
output$Cxyplot_con_ylim0 <- renderUI({
  if (input$CXyplotYesNo=="Yes") {
    numericInput("CxyplotConYlim0", "6. Select the lower y-axis limit",
                 #value=0, step=1)
                 value=Ccon_xy_data()[["ylim0"]], step=1)
  }
})
#Upper
output$Cxyplot_con_ylim1 <- renderUI({
  if (input$CXyplotYesNo=="Yes") {
    numericInput("CxyplotConYlim1", "7. Select the upper y-axis limit",
                 #value=1, step=1)
                 value=Ccon_xy_data()[["ylim1"]], step=1)
  }
})
## Create the contrast xyplot
output$Cxyplot_contrast_plot <- renderPlot({
  if (input$CXypYesNo=="Yes") {
    Cxyp_contrast()
  }
}, height = 700)


#Reactive functions
#Creates unique levels for the grouping factor in the xyplot
Cxy_contrast_levs <- reactive({
  unique(df()[, CXyplotZ1()])
})

#Function to make contrast data
CcontrastXyDataFnc <- function(model, X, group, lev1, lev2, reg) {
  #Specs to get min and max limit for X
  spcs <- specs(model, long=TRUE)
  x_min <- spcs[[ "limits"]][rownames(spcs[[ "limits"]]) == "Low", which(colnames(spcs[[ "limits"]]) == X )]
  x_max <- spcs[[ "limits"]][rownames(spcs[[ "limits"]]) == "High", which(colnames(spcs[[ "limits"]]) == X )]
  #y label
  if(reg %in% c("Cox PH", "Cox PH with censoring")) {
    xyplot_ylab <- paste0("Cost Ratio (",lev1, ":", lev2,")")
  }
  if(reg %in% c("Logistic", "Ordinal Logistic") ) {
    xyplot_ylab <- paste0("Odds Ratio (",lev1, ":", lev2,")")
  }
  if(reg %in% c("Linear","Poisson","Quantile","Generalized Least
                Squares")) {
    xyplot_ylab <- paste0("Contrast (",lev1, ":", lev2,")")
}
  ## Determine if it is an abline at 0 or 1 ##
  if(reg %in% c("Cox PH", "Cox PH with censoring","Logistic", "Ordinal Logistic") ) {
    abline_01 <- 1
  }
  if(reg %in% c("Linear","Poisson","Quantile","Generalized Least
                Squares")) {
    abline_01 <- 0
}
  #Put elements int a list
  a <- list(lev1, seq(x_min, x_max, length.out=250))
  names(a) <- c(group, X)
  b <- list(lev2, seq(x_min, x_max, length.out=250))
  names(b) <- c(group, X)
  #w <- do.call("contrast", list( fit=model, a=a, b=b) )
  w <- contrast( fit=model, a=a, b=b)
  Upper2 <- exp(-w[["Lower"]])
  Lower2 <- exp(-w[["Upper"]])
  
  ## Exponentiate the data if needed ##
  #Contrast
  if(reg %in% c("Cox PH", "Cox PH with censoring")) {
    w[["Contrast"]] <- exp(-w[["Contrast"]])
  }
  #Lower
  if(reg %in% c("Cox PH", "Cox PH with censoring")) {
    #w[["Lower"]] <- exp(-w[["Lower"]])
    w[["Upper"]] <- Upper2
  }
  #Upper
  if(reg %in% c("Cox PH", "Cox PH with censoring")) {
    #w[["Upper"]] <- exp(-w[["Upper"]])
    w[["Lower"]] <- Lower2
  }
  #S.E.
  if(reg %in% c("Cox PH", "Cox PH with censoring")) {
    w[["SE"]] <- exp(w[["SE"]])
  }
  #"Ordinal Logistic"  
  #Contrast
  if(reg %in% c("Ordinal Logistic")) {
    w[["Contrast"]] <- exp(w[["Contrast"]])
  }
  #Lower
  if(reg %in% c("Ordinal Logistic")) {
    w[["Upper"]] <- exp(w[["Lower"]])
  }
  #Upper
  if(reg %in% c("Ordinal Logistic")) {
    w[["Lower"]] <- exp(w[["Upper"]])
  }
  #S.E.
  if(reg %in% c("Ordinal Logistic")) {
    w[["S.E."]] <- exp(w[["S.E."]])
  }
  
  #Get the upper and lower limits of the Y-axis
  ylim0 <- min(w[["Lower"]])
  ylim1 <- max(w[["Upper"]])
  return(list(w=w, xyplot_ylab=xyplot_ylab, abline_01=abline_01, xlim0=x_min, xlim1=x_max,ylim0=ylim0, ylim1=ylim1))
  }
## Get the contrast data
Ccon_xy_data <- reactive({
  if (input$CXyplotYesNo=="Yes") {
    CcontrastXyDataFnc(model=fit1(), X= CXyplotX1(), group=CXyplotZ1(),
                       lev1=input$CxyplotConLev1, lev2=input$CxyplotConLev2, reg=input$regress_type )
  }
})
## Function to run contrast xyplot
CxypContrastFnc <- function(data, xlim0, xlim1, ylim0, ylim1, X, Z, Lev1, Lev2) {
  xYplot(Cbind(data[["w"]][["Contrast"]], data[["w"]][["Lower"]],
               data[["w"]][["Upper"]]) ~ data[["w"]][[X]] ,
         ylab=data[["xyplot_ylab"]], type='l', method='filled bands', abline=list(h=data[["abline_01"]], col=2,lwd=2),
         col.fill=gray(.95), xlim=c(xlim0, xlim1), ylim=c(ylim0, ylim1), xlab=X, lwd=2,
         main=paste0("Partial prediction plot of ", X, " contrasting ", Z,  
                     " levels of ", Lev1, " to ", Lev2 ),
         sub=paste0(Z," effect: ",Lev1))
}

#Run the plot
Cxyp_contrast <- reactive({
  if (input$CXypYesNo=="Yes") {
    CxypContrastFnc(data=Ccon_xy_data(), xlim0=input$CxyplotConXlim0, xlim1=input$CxyplotConXlim1,
                    ylim0=input$CxyplotConYlim0, ylim1=input$CxyplotConYlim1, X=CXyplotX1(),
                    Z=CXyplotZ1() , Lev1=input$CxyplotConLev1, Lev2=input$CxyplotConLev2)
  }
})
 
## Table of contrast at percentiles to show where the interaction occurs ##
#Function that gets contrasts quantiles
CcontrastQuantFnc <- function(w, sp1, X) {
  p10 <- which(abs(w[[X]] - sp1[["limits"]][[X]][rownames(sp1$limits) == "Low:prediction"]) == min(abs(w[[X]] - sp1[["limits"]][[X]][rownames(sp1$limits) == "Low:prediction"])))[1]
  p25 <- which(abs(w[[X]] - sp1[["limits"]][[X]][rownames(sp1$limits) == "Low:effect"]) == min(abs(w[[X]] - sp1[["limits"]][[X]][rownames(sp1$limits) == "Low:effect"])))[1]
  p50 <- which(abs(w[[X]] - sp1[["limits"]][[X]][rownames(sp1$limits) == "Adjust to"]) == min(abs(w[[X]] - sp1[["limits"]][[X]][rownames(sp1$limits) == "Adjust to"])))[1]
  p75 <- which(abs(w[[X]] - sp1[["limits"]][[X]][rownames(sp1$limits) == "High:effect"]) == min(abs(w[[X]] - sp1[["limits"]][[X]][rownames(sp1$limits) == "High:effect"])))[1]
  p90 <- which(abs(w[[X]] - sp1[["limits"]][[X]][rownames(sp1$limits) == "High:prediction"]) == min(abs(w[[X]] - sp1[["limits"]][[X]][rownames(sp1$limits) == "High:prediction"])))[1]
  #Put the values in a vector
  contrast_quant <- c(p10,p25,p50,p75,p90)
  #Bind the rows of data I need "Contrast"	 "SE"	 "Lower"	 "Upper"	 "Z"	 "Pvalue"
  con_quan_df <- cbind( round(w[[X]][contrast_quant], 2), round(w[["Contrast"]][contrast_quant], 2),
                        #round(w[["SE"]][contrast_quant], 2),  #Dropping this because it may be inapproriate to report a exp(SE)
                        round(w[["Lower"]][contrast_quant], 2),
                        round(w[["Upper"]][contrast_quant], 2), round(w[["Z"]][contrast_quant], 2),
                        round(w[["Pvalue"]][contrast_quant], 4))
  #Column names
  colnames(con_quan_df) <- c(X,"Contrast","Lower","Upper","Z","Pvalue")
  #Row names
  rownames(con_quan_df) <- c("10th","25th","50th","75th","90th")
  return(con_quan_df)
}

#Function that runs the function above
CcontrastQuantTbl <- reactive({
  if (input$CXypYesNo=="Yes") {
    CcontrastQuantFnc(w=Ccon_xy_data()[["w"]], sp1=specs(fit1(), long=TRUE), X=CXyplotX1())
  }
})
#Run function for the Quantile table for contrasts
output$Ccontrast_quant_table <- renderTable({
  if (input$CXypYesNo=="Yes") {
    CcontrastQuantTbl()
  }
}, rownames = TRUE)

#################################### End here

##################################################
#Create yes/no box to determine plot single partial effect
output$CoxMnMed <- renderUI({                                 #Same idea as output$vy
  selectInput("cox_mn_med", "1. Do you want to plot the mean or quantile effect?", 
              choices = c("Mean", "Quantile"), multiple=FALSE, selected="Mean")     #Will make choices based on my reactive function.
})
#Select the percentile level
output$cox_pct_lvl <- renderUI({                                 #Same idea as output$vy
  numericInput("CoxPctLvl", "2. Enter the percentile/quantile.",
               value = .50, min=.01, max = .99, step = .01)
})
#Reactive function that stores the chosen percentile level
cox_pctl <- reactive({
  if(input$RunDensPlt1 == "Yes") {
    if (input$regress_type %in% c("Cox PH", "Cox PH with censoring")) {
      1 - input$CoxPctLvl
          } else {
      input$CoxPctLvl
    }
  }  
})

#This plots the predicted values  for the partial effects plots  
output$cox_prt_prd <- renderPlot({
  if(input$RunDensPlt1 == "Yes") {
    
    if(input$cox_mn_med == "Quantile") {
      
      if(input$one_cox_x_yes == "Yes") {
        plot(  do.call("Predict", list(fit1(), pe_cox_x_var(), fun=function(x) med()(q=cox_pctl(), lp=x)) )) 
      } else {
        plot(  do.call("Predict", list(fit1(),  fun=function(x) med()(q=cox_pctl(), lp=x)) )) 
      }
    } else {
      if(input$one_cox_x_yes == "Yes") {
        plot(  do.call("Predict", list(fit1(), pe_cox_x_var(), fun=function(x) mn()(lp=x)) )) 
      } else {
        plot(  do.call("Predict", list(fit1(),  fun=function(x) mn()(lp=x)) )) 
      }
    }
  }
}, height = 700)

#Create yes/no box to determine plot single partial effect
output$OneCoxXYes <- renderUI({                                 
  selectInput("one_cox_x_yes", "4. Do you want to plot a single partial effect?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     
})
#Select the variables that will get only get plotted
output$prt_one_cox_x <- renderUI({
  selectInput("cox_X", "3. Select a single predictor.", 
               choices = predictor(), multiple=FALSE, selected=predictor()[1])
})
#The single partial effect variable to plot
pe_cox_x_var <- reactive({         
  input$cox_X
})
##################################################
#########################
## Quantile regression ##
#########################
######
#10th percentile
quant_reg.10 <- reactive({                  
  if (input$begin_mdl == "Yes") {
    switch(input$regress_type,                #"var" and can be used anywhere in server.r.
           "Quantile" = if(input$updy == "Yes") {
             Rq(as.formula(input$up_fmla), x=TRUE, y=TRUE, tau=.1) 
           } else {
             Rq(mdl_fmla(), x=TRUE, y=TRUE,  tau=.1)}
    )
  } else {
    switch(input$regress_type,    
           "Quantile" = if(input$updy == "Yes") {
             fit.mult.impute(as.formula(input$up_fmla), Rq, mi(), data=df(), pr=FALSE, tau=.1) 
           } else {
             fit.mult.impute(mdl_fmla(), Rq, mi(), data=df(), pr=FALSE, tau=.1)} 
    )
  }
})
#25th percentile
quant_reg.25 <- reactive({                  
  if (input$begin_mdl == "Yes") {
    switch(input$regress_type,                #"var" and can be used anywhere in server.r.
           "Quantile" = if(input$updy == "Yes") {
             Rq(as.formula(input$up_fmla), x=TRUE, y=TRUE, tau=.25) 
           } else {
             Rq(mdl_fmla(), x=TRUE, y=TRUE,  tau=.25)}
    )
  } else {
    switch(input$regress_type,    
           "Quantile" = if(input$updy == "Yes") {
             fit.mult.impute(as.formula(input$up_fmla), Rq, mi(), data=df(), pr=FALSE, tau=.25) 
           } else {
             fit.mult.impute(mdl_fmla(), Rq, mi(), data=df(), pr=FALSE, tau=.25)} 
    )
  }
})
#50th percentile
quant_reg.50 <- reactive({                  
  if (input$begin_mdl == "Yes") {
    switch(input$regress_type,                #"var" and can be used anywhere in server.r.
           "Quantile" = if(input$updy == "Yes") {
             Rq(as.formula(input$up_fmla), x=TRUE, y=TRUE, tau=.5) 
           } else {
             Rq(mdl_fmla(), x=TRUE, y=TRUE,  tau=.5)}
    )
  } else {
    switch(input$regress_type,    
           "Quantile" = if(input$updy == "Yes") {
             fit.mult.impute(as.formula(input$up_fmla), Rq, mi(), data=df(), pr=FALSE, tau=.5) 
           } else {
             fit.mult.impute(mdl_fmla(), Rq, mi(), data=df(), pr=FALSE, tau=.5)} 
    )
  }
})
#75th percentile
quant_reg.75 <- reactive({                  
  if (input$begin_mdl == "Yes") {
    switch(input$regress_type,                #"var" and can be used anywhere in server.r.
           "Quantile" = if(input$updy == "Yes") {
             Rq(as.formula(input$up_fmla), x=TRUE, y=TRUE, tau=.75) 
           } else {
             Rq(mdl_fmla(), x=TRUE, y=TRUE,  tau=.75)}
    )
  } else {
    switch(input$regress_type,    
           "Quantile" = if(input$updy == "Yes") {
             fit.mult.impute(as.formula(input$up_fmla), Rq, mi(), data=df(), pr=FALSE, tau=.75) 
           } else {
             fit.mult.impute(mdl_fmla(), Rq, mi(), data=df(), pr=FALSE, tau=.75)}
    )
  }
})
#90th percentile
quant_reg.90 <- reactive({                  
  if (input$begin_mdl == "Yes") {
    switch(input$regress_type,                #"var" and can be used anywhere in server.r.
           "Quantile" = if(input$updy == "Yes") {
             Rq(as.formula(input$up_fmla), x=TRUE, y=TRUE, tau=.9) 
           } else {
             Rq(mdl_fmla(), x=TRUE, y=TRUE,  tau=.9)}
    )
  } else {
    switch(input$regress_type,    
           "Quantile" = if(input$updy == "Yes") {
             fit.mult.impute(as.formula(input$up_fmla), Rq, mi(), data=df(), pr=FALSE, tau=.9) 
           } else {
             fit.mult.impute(mdl_fmla(), Rq, mi(), data=df(), pr=FALSE, tau=.9)}
    )
  }
})


#Function to get quantile estimates from quantile regression
pred_qnt_reg5_fnc <- function(qr.10, qr.25, qr.50, qr.75, qr.90, Dens1X) {
  #Get predicted scores
  pq10 <- do.call("Predict", list(qr.10, Dens1X))
  pq25 <- do.call("Predict", list(qr.25, Dens1X))
  pq50 <- do.call("Predict", list(qr.50, Dens1X))
  pq75 <- do.call("Predict", list(qr.75, Dens1X))
  pq90 <- do.call("Predict", list(qr.90, Dens1X))
  #Get estimated intervention values
  est1.10 <- unlist(pq10[ which(names(pq10) %in% c("yhat", "lower", "upper"))])
  est1.25 <- unlist(pq25[ which(names(pq25) %in% c("yhat", "lower", "upper"))])
  est1.50 <- unlist(pq50[ which(names(pq50) %in% c("yhat", "lower", "upper"))])
  est1.75 <- unlist(pq75[ which(names(pq75) %in% c("yhat", "lower", "upper"))])
  est1.90 <- unlist(pq90[ which(names(pq90) %in% c("yhat", "lower", "upper"))])
  #Rbind the point estimates and confidence intervals
  est1 <- data.frame(rbind(est1.10, est1.25, est1.50,     
                           est1.75, est1.90))
  #Re-arrange order to get values set up for display in the plot
  est2 <- est1[, c(2,4,6,1,3,5)]
  colnames(est2) <- c("TREATMENT", "L95", "U95","Control", "L95", "U95")
  rownames(est2) <- c("p10", "p25", "p50","p75", "p90")
  #Add cost difference beteen Treatment and controls 
  est2$Diff. <- abs(round(est2$Control) - round(est2$TREATMENT))
  return(list(est1=est1, est2=est2))
}
#pred_qnt_reg5_fnc(qr.10=qr1, qr.25=qr1, qr.50=qr1, qr.75=qr1, qr.90=qr1, Dens1X="intervention")

#This runs the pred_qnt_reg5_fnc() function above
qr_ests <- reactive({
  if(input$RunDensPlt1 == "Yes") {
    
    if(input$DensPlt1Typ == "Stratified") {
      pred_qnt_reg5_fnc(qr.10=quant_reg.10(), qr.25=quant_reg.25(), qr.50=quant_reg.50(), 
                        #qr.75=quant_reg.75(), qr.90=quant_reg.90(), Dens1X=input$DensPlt1X)
      qr.75=quant_reg.75(), qr.90=quant_reg.90(), Dens1X=dens_stratified())
    } 
  }  
})
      
      ##############
      #Graph on differences between intervention groups on expected costs over percentiles 
      qr_plt1_fnc <- function(ests, Y) {
        est1 <- ests[["est1"]]
        est2 <- ests[["est2"]]
        #Later shades for second plot
        c_red <- 2; c_blue <- 4
        red_trnspt2  <- adjustcolor(c_red, alpha.f = 0.2)                               #Set red color transparency.
        blue_trnspt2 <- adjustcolor(c_blue, alpha.f = 0.2)                              #Set blue color transparency.
        plot(1:5, est1[, 1], type="n",
             ylab=Y, xlab="Percentiles",
             ylim=c(0, max(est1, na.rm=T)*.9), xlim=c(1, 5),
             axes=F, main=paste0("Estimated ", Y, " over quantiles"))
        lines(1:5, est1[, 1], col="red", lwd=3)
        lines(1:5, est1[, 2], col="blue", lwd=3)
        #Set up coordinates for polygon function, NOTE: UPPER AND LOWER MEAN THE OPPOSITE 
        #BECAUSE A COX MODEL VIEWS COST IN THE OPPOSITE DIRECTION.
        axis(2)
        axis(1, at=1:5, tick=F, labels= c("p10", "p25", "p50", "p75", "p90"))  # pos=1,
        xxt <- c(1:5,5:1)
        yya <- c(est1$lower2, rev(est1$upper2))  #Treatment
        yyc <- c(est1$lower1, rev(est1$upper1))  #Control
        #plot(xx,yyd, type="n", ylim=c(0,1))
        polygon(xxt, yya, col = blue_trnspt2, border=blue_trnspt2)
        polygon(xxt, yyc, col = red_trnspt2, border=red_trnspt2)
        legend("topleft", legend=c("Treatment","Control"), 
               col=c(4,2), lty=1,
               lwd=2, cex=1.25)
        box()
      }
      
      #This runs the qr_plt1_fnc() function above
      qr_plt1 <- reactive({
        if(input$RunDensPlt1 == "Yes") {
          
          if(input$DensPlt1Typ == "Stratified") {
            qr_plt1_fnc(ests=qr_ests(), Y= outcome())
          } 
        }  
      })
      
################################################################################
##  Survival analysis stuff: Schoenfeld residuals  and survival plots   ##
################################################################################
## Delete the next 10 lines June 1, 2020 if it causes no issues in Schoenfeld residuals
#This gets the residual names
#      schoenfeld_X_names <- reactive({
#        rownames(schoenfeld_e()[[1]])
#      })

#Identifies the number of residuals that should be used in the drop down box below.
#Indicates if we drop the "Global" value when >1 or use the first if ==1.
#      schoenfeld_X_num <- reactive({
#        ifelse( length(schoenfeld_X_names()) > 1, length(schoenfeld_X_names()) * -1, 1)
#      })
      
#Select the variable for the Schoenfeld residuals
output$Schoenfeld_X <- renderUI({
  selectInput("schoenfeldx", "1. Select a single Schoenfeld residual.", 
  #DELETE 6/1/2020  #choices = schoenfeld_X_names()[schoenfeld_X_num()] , multiple=FALSE, selected=schoenfeld_X_names()[1])
  choices = schoenfeld_X_names_plot() , multiple=FALSE, selected=schoenfeld_X_names_plot()[1])
})

#Creates the Schoenfeld residuals
schoenfeld_e <- reactive({
  if (input$regress_type %in% c("Cox PH", "Cox PH with censoring")) {
    cox.zph(fit1(), transform='identity', terms=TRUE)
  }  
})

#This prints the point estimates and confidence intervals
output$schoenfeld_test <- renderTable({
  if (input$regress_type %in% c("Cox PH", "Cox PH with censoring")) {
    schoenfeld_e()[[1]]
  }
}, rownames = TRUE)

#This gets the residual names for plots
schoenfeld_X_names_plot <- reactive({
  rownames(schoenfeld_e()[[1]])[-length(rownames(schoenfeld_e()[[1]]))]
})
#Get means of Schoenfeld residuals to use as reference line in plot
schoenfeld_var_mn <- reactive({
  colMeans(as.data.frame(schoenfeld_e()[["y"]]))
})
#This plots the Schoenfeld residuals    
output$schoenfeld_plt <- renderPlot({
  if (input$regress_type %in% c("Cox PH", "Cox PH with censoring")) {
    #DELETE 6/1/2020  #plot(schoenfeld_e()[input$schoenfeldx], col=2, lwd=2)
    plot(schoenfeld_e()[which(schoenfeld_X_names_plot()== input$schoenfeldx)], col=2, lwd=2)
    abline(h= schoenfeld_var_mn()[which(names(schoenfeld_var_mn()) == input$schoenfeldx)], col=3, lwd=2, lty=3)
    #abline(h=0, lty=3, col=4, lwd=2)
  }
}, height = 800)

## Survival plots ##
#Select predictors
output$srv_plt_one_x <- renderUI({
  selectInput("SrvPltX", "1. Select a single predictor.", 
              choices = predictor(), multiple=FALSE, selected=predictor()[1])
})
#Select confidence interval level
output$srv_plt_lvl <- renderUI({                                 #Same idea as output$vy
  numericInput("SrvPltLvl", "2. Enter the confidence level.",
               value = .95, min=0, max = .99, step = .01)
})
#Do you want confidence bands or bars?
output$SurvPltBands <- renderUI({                                 
  selectInput("surv_plt_band", "3. Do you want confidence bands or bars?", 
              choices = c("bands","bars"), multiple=FALSE, selected="bands")     
})
#Create yes/no box to run survival plot
output$SurvPltRun <- renderUI({                                 
  selectInput("surv_plt_run", "4. Do you want to create the survival plot?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     
})
#Minimum value of Cox survival time
cox_min_time <- reactive({ 
  round(min(as.numeric(df()[, input$variableY]), na.rm=TRUE ))  
})
#Maximum value of Cox survival time
cox_max_time <- reactive({ 
  round(max(as.numeric(df()[, input$variableY]), na.rm=TRUE ))
})
#Indicate lower limit of x-axis
output$SurvPltXlim1 <- renderUI({
  numericInput("sp_Xlim1", "5. Lower X-axis limit.",
               #value = cox_min_time(), step = 1)
               value = 0, step = 1)
})
#Indicate upper limit of x-axis
output$SurvPltXlim2 <- renderUI({
  numericInput("sp_Xlim2", "6. Upper X-axis limit.",
               value = cox_max_time(), step = 1)
})
#Indicate lower limit of y-axis
output$SurvPltYlim1 <- renderUI({
  numericInput("sp_Ylim1", "7. Lower Y-axis limit.",
               value = 0, step = .01)
})
#Indicate upper limit of x-axis
output$SurvPltYlim2 <- renderUI({
  numericInput("sp_Ylim2", "8. Upper Y-axis limit.",
               value = 1, step = .01)
})

#Create yes/no box to run survival plot
output$SpTimeInc <- renderUI({                                 
  numericInput("sp_timeinc", "9. Indicate the X-axis time increment.", 
               value = round((input$sp_Xlim2/5), 0), step = 1, min=0)     
})
#Indicate if you want the hazard function
output$HazardPlot <- renderUI({                                 
  selectInput("hazard_plot", "10. Do you want the hazard or survival?", 
              choices = c("Cumulative Hazard", "Cumulative Incidence", "Survival"), multiple=FALSE, selected="Survival")     
})
#Indicate if you want the log-minus-log plot
output$SrvLogLog <- renderUI({                                 
  selectInput("srv_log_log", "11. Want a log-minus-log plot (assess PH)?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     
})
#"Survival" or "Hazard" to be used for labels
SrvHzrLbl <- reactive({ 
  input$hazard_plot
  #  if (input$hazard_plot == "No") {
  #    "Survival"
  #  } else {
  #    #"Hazard"
  #    "Cumulative Hazard"
  #  }
})
#This will go into the "fun" argument based on if I want a survival or Hazard function
SrvHzr <- reactive({ 
      switch(SrvHzrLbl(), 
             "Survival"                =  function(x) {x},
             "Cumulative Hazard"       =  "cumhaz",
             "Cumulative Incidence"    =  function(x) {1 - x})
  })
#"Survival" or "Hazard" to be used to indicate a log-minus-log plot
SrvLogLog <- reactive({ 
  if (input$srv_log_log == "No") {
    FALSE
  } else {
    TRUE
  }
})

#Survival fit object
srvft1 <- reactive({ 
  if (input$regress_type %in% c("Cox PH", "Cox PH with censoring","AFT","AFT with censoring")) {
    survfit(fit1(), conf.int=input$SrvPltLvl)
  }
})

#This plots the predicted values  for the partial effects plots  
output$surv_plot1 <- renderPlot({
  
  if(input$surv_plt_run == "No") {
    
    if ("strata" %in% names(srvft1()) ) {
      plot(srvft1(), 
           xlim=c(input$sp_Xlim1, input$sp_Xlim2), ylim=c(input$sp_Ylim1,input$sp_Ylim2),
           ylab= paste0(SrvHzrLbl()), fun=SrvHzr(),
           xlab=paste0(SrvHzrLbl()," functions of time stratified by ", strsplit(names(srvft1()$strata)[1], "=")[[1]][1], 
                       " with ", input$SrvPltLvl*100, "% confidence intervals"), 
           mark.time=T, pch=LETTERS[1:length(names(srvft1()$strata))])
    } 
          if ( !"strata" %in% names(srvft1()) ) {
      plot(srvft1(), fun=SrvHzr(),
           xlim=c(input$sp_Xlim1, input$sp_Xlim2), ylim=c(input$sp_Ylim1,input$sp_Ylim2),
           ylab=paste0(SrvHzrLbl()),
           xlab=paste0(SrvHzrLbl(), " function of time with ", input$SrvPltLvl*100, "% confidence intervals"))
    }
  } else {
    
    if(input$surv_plt_run == "Yes") {
      (do.call("survplot", list(fit1(), input$SrvPltX, conf.int=input$SrvPltLvl, conf=input$surv_plt_band, 
                                xlim=c(input$sp_Xlim1, input$sp_Xlim2), ylim=c(input$sp_Ylim1,input$sp_Ylim2),
                                xlab=paste0(SrvHzrLbl(), " time by ", input$SrvPltX), time.inc=input$sp_timeinc, 
                                fun=SrvHzr(), loglog= SrvLogLog() ))) 
    } 
    if(input$surv_plt_run == "Yes") {
      box()
    }
  }
  }, height = 800)

############################################ Begin here
## Survival plots ##
#Select predictors
output$km_srv_plt_one_x <- renderUI({
  selectInput("KMSrvPltX", "1. Select a single predictor.", 
              choices = predictor(), multiple=FALSE, selected=predictor()[1])
})
#Indicate if you want the hazard function
output$KMHazardPlot <- renderUI({                                 
  selectInput("km_hazard_plot", "2. Do you want hazard curves?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     
})
#Create yes/no box to run survival plot
output$KMSurvPltRun <- renderUI({                                 
  selectInput("km_surv_plt_run", "3. Do you want to create the KM plot?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     
})
#Indicate the restricted mean
output$KmRestrictMean <- renderUI({
  numericInput("km_restrict_mean", "4. Select the restricted mean.",
               value = km_cox_max_time(), step = 1)
})
#Indicate lower limit of x-axis
output$KMSurvPltXlim1 <- renderUI({
  numericInput("km_sp_Xlim1", "5. Lower X-axis limit.",
               value = 0, step = 1)
})
#Indicate upper limit of x-axis
output$KMSurvPltXlim2 <- renderUI({
  numericInput("km_sp_Xlim2", "6. Upper X-axis limit.",
               value = km_cox_max_time(), step = 1)
})
#Indicate lower limit of y-axis
output$KMSurvPltYlim1 <- renderUI({
  numericInput("km_sp_Ylim1", "7. Lower Y-axis limit.",
               value = 0, step = .01)
})
#Indicate upper limit of x-axis
output$KMSurvPltYlim2 <- renderUI({
  numericInput("km_sp_Ylim2", "8. Upper Y-axis limit.",
               value = 1, step = .01)
})

#Maximum value of Cox survival time
km_cox_max_time <- reactive({ 
  round(max(as.numeric(df()[, input$variableY]), na.rm=TRUE ))
})
#"Survival" or "Hazard" to be used for labels
KMSrvHzrLbl <- reactive({ 
  if (input$km_hazard_plot == "No") {
    "Survival"
  } else {
    "Hazard"
  }
})

#Function to get the right KM formula for groups
kmSrvftFmlaFnc <- function (regress_type,Y,cens,KMSrvPltX) {
  if (regress_type == "Cox PH") {
    fmla <- as.formula(paste(paste0("Surv(", Y, ")", "~", KMSrvPltX)))
  }
  if (regress_type == "Cox PH with censoring") {
    fmla <- as.formula(paste(paste0("Surv(", Y, ",", cens, ")", "~", KMSrvPltX)))
  }
  #AFT models
  if (regress_type == "AFT") {
    fmla <- as.formula(paste(paste0("Surv(", Y, ")", "~", KMSrvPltX)))
  }
  if (regress_type == "AFT with censoring") {
    fmla <- as.formula(paste(paste0("Surv(", Y, ",", cens, ")", "~", KMSrvPltX)))
  }
  return(fmla)
} 
#Function to get the right KM formula for the underline/unconditional survival function
kmSrvftFmlaUnconFnc <- function (regress_type,Y,cens,KMSrvPltX) {
  if (regress_type == "Cox PH") {
    fmla <- as.formula(paste(paste0("Surv(", Y, ")", "~", 1 )))
  }
  if (regress_type == "Cox PH with censoring") {
    fmla <- as.formula(paste(paste0("Surv(", Y, ",", cens, ")", "~", 1 )))
  }
  #AFT models
  if (regress_type == "AFT") {
    fmla <- as.formula(paste(paste0("Surv(", Y, ")", "~", 1 )))
  }
  if (regress_type == "AFT with censoring") {
    fmla <- as.formula(paste(paste0("Surv(", Y, ",", cens, ")", "~", 1 )))
  }
  return(fmla)
} 

#Survival fit object for the conditional model
KMsrvftFmla <- reactive({
  if (input$km_surv_plt_run == "Yes") {
    kmSrvftFmlaFnc(regress_type=input$regress_type, Y=outcome(), cens=censor1(), KMSrvPltX=input$KMSrvPltX)
 }
})
#Survival fit object for the UNconditional model
KMsrvftUnconFmla <- reactive({
  if (input$km_surv_plt_run == "Yes") {
    kmSrvftFmlaUnconFnc(regress_type=input$regress_type, Y=outcome(), cens=censor1() )
  }
})

#KM legend
KMLgnd <- reactive({ 
  if (input$km_hazard_plot == "Yes") {
   "bottom"
  } else {
    "top"
  }
})

#Kaplan-Meier survival or hazard plots
kmSurvPltFnc <- function(KMsrvftFmla, df, Y, km_hazard_plot, KMSrvHzrLbl, km_sp_Xlim1, km_sp_Xlim2,
                         km_sp_Ylim1, km_sp_Ylim2, KMSrvPltX, lgnd) {
  pltType <- ifelse(km_hazard_plot == "No", "S", "F")
  plot(survfit(KMsrvftFmla, data= df), 
       xlim=c(km_sp_Xlim1, km_sp_Xlim2), ylim=c(km_sp_Ylim1, km_sp_Ylim2),
       ylab= paste0(KMSrvHzrLbl, " Probability"), 
       xlab=Y, mark.time=T, fun=pltType , lwd=3, 
       main= paste0("Kaplan-Meier plot of ", tolower(KMSrvHzrLbl), " by ", KMSrvPltX),
       pch=LETTERS[1:length(unique(df[, KMSrvPltX]))],
       col=1:length(unique(df[, KMSrvPltX] )), lty= 1:length(unique( df[, KMSrvPltX])))
  legend(lgnd, legend=sort(unique( df[, KMSrvPltX])), col=1:length(unique(df[, KMSrvPltX] )), 
         lty=1:length(unique(df[, KMSrvPltX] )), bty="n", lwd=2, cex=1.5,
         title=KMSrvPltX)
} 

#Survival fit object
KMsrvftPlot <- reactive({ 
  if (input$km_surv_plt_run == "Yes") {
    kmSurvPltFnc(KMsrvftFmla=KMsrvftFmla(), df=df(), Y=outcome(), km_hazard_plot=input$km_hazard_plot, 
                 KMSrvHzrLbl=KMSrvHzrLbl(), km_sp_Xlim1=input$km_sp_Xlim1, 
                 km_sp_Xlim2=input$km_sp_Xlim2, km_sp_Ylim1=input$km_sp_Ylim1, 
                 km_sp_Ylim2=input$km_sp_Ylim2, KMSrvPltX=input$KMSrvPltX, lgnd=KMLgnd()) 
  }
})
output$km_plot <- renderPlot({
  if (input$km_surv_plt_run == "Yes") {
    KMsrvftPlot()
  }
}, height = 700)

## Output for"baseline" KM  survival function ##
#Survival function
blSrvFitFnc1 <- function (FMLA, df) {
  sf_rslt <-survfit(FMLA, data = df)
  return(sf_rslt)
} 
#Print restricted mean and median
blSrvFitFnc2 <- function (sf_rslt, KM_RESTRICT_MEAN) {
  rslt <- do.call("print", list(sf_rslt, rmean=KM_RESTRICT_MEAN))
#  rslt <- print( sf_rslt , print.rmean=TRUE, rmean=KM_RESTRICT_MEAN)
  return(rslt)
} 

#Reactive function for baseline function above
KM_SF_UC1 <- reactive({
  if (input$km_surv_plt_run == "Yes") {
    blSrvFitFnc1(FMLA=KMsrvftUnconFmla(), df=df() )
  }
})
#Restricted mean and median
KM_SF_UC2 <- reactive({
  if (input$km_surv_plt_run == "Yes") {
    blSrvFitFnc2(sf_rslt=KM_SF_UC1(), KM_RESTRICT_MEAN=input$km_restrict_mean)
  }
})

#Reactive function for baseline function above
KM_SF_C1 <- reactive({
  if (input$km_surv_plt_run == "Yes") {
    blSrvFitFnc1(FMLA=KMsrvftFmla(), df=df() )
  }
})
#Restricted mean and median
KM_SF_C2 <- reactive({
  if (input$km_surv_plt_run == "Yes") {
    blSrvFitFnc2(sf_rslt=KM_SF_C1(), KM_RESTRICT_MEAN=input$km_restrict_mean)
  }
})

## Output for survival functions
#Unconditional
output$KM_SF_Output_UC <- renderPrint({
  if (input$km_surv_plt_run == "Yes") {
    KM_SF_UC2() 
  }
})
#Conditional
output$KM_SF_Output_C <- renderPrint({
  if (input$km_surv_plt_run == "Yes") {
    KM_SF_C2() 
  }
})


############################################ End here

#########################
## UI input boxes ##
#Select the predictors.
output$time_dependent_predictors <- renderUI({
  selectInput("TDPredX", "1. Select the variables",
              choices = var(), multiple=TRUE, 
              selected=var()[2])
})

#Text input for time-dependent cutoff level
output$td_cut_lev <- renderUI({ 
  textInput("TdCutLev", "2. Type in time period cutoffs.", value ="Default") 
})
#Yes/no on if I want to calculate an interaction for a continuous variable
output$td_X_by_time_yesno <- renderUI({                                 
  selectInput("tdXByTimeYesNo", "3. Want a continuous X by Time interaction?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     
})
#A select box for the variable that interacts with time
output$time_dependent_var <- renderUI({                                
  selectInput("TimeDepVar", "4. Select the interaction variable",       
              choices = predictor(), multiple=FALSE, 
              selected= predictor()[1])   #Will make choices based on my reactive function.
})

output$td_log_increment <- renderUI({ 
  numericInput("TdLogIncrement", "5. Select a value to add to log(time).",
               value=1, min=0, step=1) 
})

#Yes/No if I want to create the time-dependent file
output$td_data_yes_no <- renderUI({ 
  selectInput("TdDataYesNo", "6. Want a time-dependent data file?",
              choices = c("No", "Yes"), multiple=FALSE, selected="No") 
})
#Download time dependent data file
output$td_data_download_name <- renderUI({ 
  textInput("TdDataDownloadName", "7. Enter the data name.", 
            value= "td_df")     
})
output$td_data_download <- downloadHandler(
  filename = "time_dependent.RData",
  content = function(con) {
    assign(input$TdDataDownloadName, TdDataFrame())
    save(list=input$TdDataDownloadName, file=con)
  }
)

#Function that gets the cutoff values for survplot with the default being each unique time point
tdcutFnc <- function(Y, TdCutLev, df) {
  if(TdCutLev == "Default") {
    #if(is.null(TdCutLev)) {
    tout <-  eval(parse(text = "1:max(df[, Y])"))
  } else {
    tout <- eval(parse(text = TdCutLev))
  }
  return(tout)
}

#Reactive function that runs tdcutFnc() 
tdcut1 <-  reactive({
  tdcutFnc(Y= outcome(), TdCutLev=input$TdCutLev, df=df() )
})

#Function that creates a time dependent data file
timeDepDfFnc <- function(X, Y, Z, censor, reg, df, tdcut1, AddLog, XbyT) {
  #No censoring
  if (reg == "Cox PH") {
    cph_fmla <- as.formula(paste(paste0("Surv(", Y,")" , "~"),
                                 paste(X, collapse= "+")))
    timeDepDF <- survSplit(formula= cph_fmla, data=df, cut=tdcut1,
                           start="tstart", id= "id", episode= 
                             "tgroup",
                           end="tstop", event="event")
  }
  #Censoring
  if (reg == "Cox PH with censoring") {
    ccph_fmla <- as.formula(paste(paste0("Surv(", Y, ",", censor, ")", "~"),
                                  paste(X, collapse= "+")))
    timeDepDF <- survSplit(formula=ccph_fmla , data=df, cut=tdcut1,
                           start="tstart", id= "id", episode= "tgroup",
                           end="tstop", event="event")
  }
  
  #INTERACTION  
  if (XbyT == "Yes") {
    timeDepDF[,paste0(Z, ".log.", Y)] <- 
    timeDepDF[, Z]*log(timeDepDF[,"tgroup"] + AddLog)      
  }    
    #timeDepDF[,paste0(Z, ".log.", Y)] <- 
    #timeDepDF[, Z]*log(timeDepDF[,"tgroup"] + AddLog)      
  return(timeDepDF)
}

#Reactive function that runs tdcutFnc() and creates the time-dependent file 
TdDataFrame <-  reactive({
  if (input$TdDataYesNo == "Yes") { 
    timeDepDfFnc(X=input$TDPredX, Y=outcome(), Z=input$TimeDepVar, censor=censor1(), 
                 reg=input$regress_type, df=df(), tdcut1=tdcut1(), 
                 AddLog= input$TdLogIncrement, XbyT=input$tdXByTimeYesNo)
  }
})

#This function corrects a problem with Harrell's which.influence() that doesn't work with 1 predictor.
which.influence2 <- function (fit, cutoff = 0.2) 
{
  cox <- inherits(fit, "cph")
  stats <- resid(fit, "dfbetas")
  stats <- matrix(stats, ncol=1)   #SZ: This corrects the problem with just 1 predictor
  rnam <- which(!is.na(stats[, 1]))
  stats <- stats[rnam, , drop = FALSE]
  d <- dimnames(stats)[[1]]
  if (length(d)) 
    rnam <- d
  at <- fit$Design
  w <- list()
  namw <- NULL
  k <- 0
  oldopt <- options("warn")
  options(warn = -1)
  on.exit(options(oldopt))
  if (!cox) {
    ww <- rnam[abs(stats[, 1]) >= cutoff]
    if (length(ww)) {
      k <- k + 1
      w[[k]] <- ww
      namw <- "Intercept"
    }
  }
  Assign <- fit$assign
  nm <- names(Assign)[1]
  if (nm == "Intercept" | nm == "(Intercept)") 
    Assign[[1]] <- NULL
  j <- 0
  for (i in (1:length(at$name))[at$assume.code != 8]) {
    j <- j + 1
    as <- Assign[[j]]
    if (length(as) == 1) 
      ww <- rnam[abs(stats[, as]) >= cutoff]
    else {
      z <- rep(FALSE, length(rnam))
      for (r in as) z <- z | abs(stats[, r]) >= cutoff
      ww <- rnam[z]
    }
    if (length(ww)) {
      k <- k + 1
      w[[k]] <- ww
      namw <- c(namw, at$name[i])
    }
  }
  if (length(w)) 
    names(w) <- namw
  w
}

## UI function for DFBETAS U level
#Select confidence interval level
output$U_Cutoff_Lvl <- renderUI({                                 #Same idea as output$vy
  numericInput("UCutoffLvl", "1. DFBETAS cutoff U level.",
               value = .2, min=0, step = .01)
})
#Create yes/no box to run the DFBETAS residuals
output$DFBETASYesNo <- renderUI({                                 
  selectInput("DFBETAS_Yes_No", "2. Want DFBETAS influential cases?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     
})

## Download residuals button
#####################
# Save DFBETAS output #
#####################
output$SaveDFBETAS <- renderUI({  
  selectInput("save_dfbetas", "1. Save the residuals?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     #Will make choices based on my reactive function.
})
dfbetasFit <- reactive({
  if(input$save_dfbetas == "Yes") {
    list("DFBETAS"= try(dfbetas_res()),
         "Deviance"= Deviance_res(),
         "Influential"= try(w_influ()),
         "Influential.Data"= try(w_influ_df()),
         "Schoenfeld"= try(Schoenfeld_res()),
         "Martingale"= try(Martingale_res()))
  }
})
output$dfbetas_fit_name <- renderUI({ 
  textInput("DFBETASFitName", "2. Enter the residuals' name.", 
            value= "DFBETASres")     
})
output$dfbetas_influential_residuals <- downloadHandler(
  filename = "dfbetas_residuals.RData",
  content = function(con) {
    assign(input$DFBETASFitName, dfbetasFit())
    save(list=input$DFBETASFitName, file=con)
  }
)
#####################
## Gets Martingale residuals
Schoenfeld_res <- reactive({ 
  if (input$regress_type %in% c("Cox PH", "Cox PH with censoring")) {
    residuals(fit1(), type= "schoenfeld")
  }
})
## Gets Martingale residuals
Martingale_res <- reactive({ 
  if (input$regress_type %in% c("Cox PH", "Cox PH with censoring")) {
    residuals(fit1(), type="martingale")
  }
})
## Gets dfbetas residuals
dfbetas_res <- reactive({ 
  if (input$regress_type %in% c("Cox PH", "Cox PH with censoring")) {
    residuals(fit1(), type="dfbetas")
  }
})
## Gets Deviance residuals
Deviance_res <- reactive({ 
  if (input$regress_type %in% c("AFT","AFT with censoring","Cox PH", "Cox PH with censoring")) {
    residuals(fit1(), type="deviance")
  }
})
#Gets influential observations by predictors
w_influ <- reactive({ 
  if (input$regress_type %in% c("Cox PH", "Cox PH with censoring")) {
    if (length(predictor()) ==1) {
      which.influence2(fit1(), cutoff=input$UCutoffLvl)
    } else {
      which.influence(fit1(), cutoff=input$UCutoffLvl)
    }
  }
})
#show.influence report argument
w_influ_report <- reactive({ 
  if (input$regress_type %in% c( "Cox PH with censoring")) {
    if (input$DFBETAS_Yes_No =="Yes") { 
      c(outcome(), censor2())
    } 
  } else {
    outcome()
  }
})

#Gets the data frame of the influential observations
w_influ_df <- reactive({ 
  if (input$regress_type %in% c("Cox PH", "Cox PH with censoring")) {
    if (input$DFBETAS_Yes_No =="Yes") { 
    show.influence(w_influ(), dframe=df(), report=w_influ_report() )
    }
  }
})

output$InfluenceDFBETAS <- renderPrint({
  if (input$regress_type %in% c("Cox PH", "Cox PH with censoring")) {
    w_influ_df()
    }
  })

#############################
## Deviance residual plots ##  
#############################

#Deviance residual outlier function plot
#1. Create yes/no box to run the DFBETAS residuals
output$dev_res_yesno <- renderUI({                                 
  selectInput("devResYN", "1. Do you want to plot the deviance residuals?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     
})
#1A. Reactive function for residuals
deviance_residuals_yesno <- reactive({ 
  input$devResYN
  })
#1B. Run function to get plot
plot_deviance_outlier_function <- reactive({
  if (deviance_residuals_yesno() =="Yes") {
    fncDevResOtlrPlot(Deviance=Deviance_res(), DF=df(),  Y=outcome(), Censor=censor1(), RegType=input$regress_type )
  }
})
#1C. Create the plot
output$prediction_deviance_outlier_run <- renderPlot({
  if (deviance_residuals_yesno() =="Yes") {
    plot_deviance_outlier_function()
  }
})

## Deviance by covariates
#1. Select the predictors.
output$deviance_cov_plot_x <- renderUI({
  selectInput("devCovPltX", "1. Select the covariate", 
              choices = var(), multiple=FALSE, selected=var()[1])
})
#1A. Reactive function for residuals
deviance_covariate_plot_X <- reactive({ 
  input$devCovPltX
})

#2. Create yes/no box to run the DFBETAS residuals
output$dev_cov_yesno <- renderUI({                                 
  selectInput("devCovYN", "2. Do you want to plot deviance by a covariate?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     
})
#2A. Reactive function for residuals
deviance_covariate_yesno <- reactive({ 
  input$devCovYN
})
#2B. Run function to get plot
plot_deviance_covariate_function <- reactive({
  if (deviance_covariate_yesno() =="Yes") {
    fncDevResCovPlot(Deviance=Deviance_res(), DF=df(), X=deviance_covariate_plot_X(), RegType=input$regress_type)
  }
})
#2C. Create the plot
output$prediction_deviance_covariate_run <- renderPlot({
  if (deviance_covariate_yesno() =="Yes") {
    plot_deviance_covariate_function()
  }
})


## Function to create plot of outliers
fncDevResOtlrPlot <- function(Deviance, DF,  Y, Censor=NULL, RegType ) {
  if(RegType %in% c("AFT with censoring","Cox PH with censoring") ) {
    Mdl_Status <- eval(parse(text=paste0(deparse(substitute(DF)), "$", Censor) ))
  }
  #Plot for censoring
  if(RegType %in% c("AFT with censoring","Cox PH with censoring") ) {
    plot(DF[, Y], Deviance , type='n', xlab= Y, ylab="Deviance Residual (SD)",
         main="Deviance residuals across time")
    points(DF[, Y][ Mdl_Status == FALSE ], Deviance[ Mdl_Status == FALSE ] , col=4, pch=1)
    points(DF[, Y][Mdl_Status == TRUE],    Deviance[ Mdl_Status == TRUE ] , col=2, pch=19)
  }
  #Regular plot
  if(RegType %in% c("AFT","Cox PH") ) {
    plot(DF[, Y], Deviance , type='p', xlab= Y, ylab="Deviance Residuals (SD)",
         main="Deviance residuals across time")
  }
  #Legend
  if(RegType %in% c("AFT with censoring","Cox PH with censoring") ) {
    legend('topright', legend=c("Event", "Survived/Censored"), col=c(2,4), pch=c(19,1), cex=2)
  }
}

## Function to create plot of residuals by covariates
fncDevResCovPlot <- function(Deviance, DF, X, RegType ) {
  #Regular plot
  if(RegType %in% c("AFT","AFT with censoring","Cox PH", "Cox PH with censoring") ) {
    scatter.smooth(DF[, X], Deviance, ylab="Deviance Residuals (SD)", xlab= X, 
                   main=paste0("Deviance residuals by ", X), 
                   lpars =list(col = "red", lwd = 3, lty = 1))
  }
}

#fncDevResCovPlot(Deviance=res, DF=pbc, X="bili", RegType= "Cox PH with censoring")

################################################################################

#Multilevel modeling 
#Selects a level 2 covariate
output$CoxLev2 <- renderUI({
  selectInput("cox_lev2", "1. Select a level 2 (between-group) cluster variable.", 
              choices = other_cov(), multiple=FALSE, selected=other_cov()[1])
})

cox_lev2 <- reactive({
  if (input$CoxmeYes == "Yes") {
    input$cox_lev2
  }
})

#Indicate if you should run the multilevel model.
output$coxme_yes <- renderUI({ 
  selectInput("CoxmeYes", "2. Run the mixed effects Cox PH model?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")
})
#This sets up the level 2 correlation structure
mlv_cor <- reactive({
  paste0("(1|", cox_lev2(),")")
})

#Create the Cox model (no censoring)
#cme_fmla1 <- reactive({
#  as.formula(paste(paste0("Surv(",input$variableY,")" , "~"),   
#                   paste(c(mdl_vnms(), mlv_cor()), collapse= "+")))
#})
#Create the Cox model with censoring
#cme_fmla2 <- reactive({
#  as.formula(paste(paste0("Surv(", input$variableY, ",", censor1(), ")", "~"),   
#                   paste(c(mdl_vnms(), mlv_cor()), collapse= "+")))
#})
cme_fmla1 <- reactive({
  if(input$updy =="Yes") {
    as.formula(paste(paste0("Surv(",input$variableY,")" , "~"),   
                     paste( c( strsplit(input$up_fmla, "~")[[1]][2], mlv_cor()), collapse= "+")))
  } else {
    as.formula(paste(paste0("Surv(",input$variableY,")" , "~"),   
                     paste(c(mdl_vnms(), mlv_cor()), collapse= "+")))
  }
})
#Create the Cox model with censoring
cme_fmla2 <- reactive({
  if(input$updy =="Yes") {
    as.formula(paste(paste0("Surv(",input$variableY,  ",", censor1(), ")" , "~"),   
                     paste(c(strsplit(input$up_fmla, "~")[[1]][2], mlv_cor()), collapse= "+")))
  } else {
    as.formula(paste(paste0("Surv(", input$variableY, ",", censor1(), ")", "~"),   
                     paste(c(mdl_vnms(), mlv_cor()), collapse= "+")))
  }
})

#Run the multilevel Cox model
efit1 <- reactive({                  
  if (input$CoxmeYes == "Yes") {
    switch(input$regress_type, 
           "Cox PH"                =  coxme(cme_fmla1(), x=TRUE, y=TRUE),
           "Cox PH with censoring" =  coxme(cme_fmla2(), x=TRUE, y=TRUE))
  }
})
#Print up the model output
output$efit1_out <- renderPrint({
  if (input$CoxmeYes == "Yes") {
    list("Model"=efit1(), "AIC"= AIC(efit1()) )
  }
})

#################################################
## Multilevel model tests and model assessment ##
#################################################
#Likelihood ratio test
reLRT <- reactive({
  if (input$CoxmeYes == "Yes") {
  paste0("X2= ", round(2*(logLik(efit1()) - logLik(fit1())), 2), ", P-value= ", 
         round(1-pchisq(as.vector(2*(logLik(efit1()) - logLik(fit1()))), 1), 5) )
  }
})

#########################
# Cox & Snell pseudo R2 #
#########################
#This function calculates the Cox & Snell pseudo R2
R2_coxme_fnc <- function(fit) {
  n <- fit$n[2]
  lr <- -2 * (fit$loglik[1] - fit$loglik[2])  #Likelihood ratio statistic
  ll0 <- -2 * fit$loglik[1]                      #Using log-likelihood from null model
  R2.max <- 1 - exp(-ll0/n)                         #R2 max formula
  R2 <- as.vector((1 - exp(-lr/n))/R2.max)                     #R2 formula
  return(R2)
}

R2_coxme <- reactive({
  if (input$CoxmeYes == "Yes") {
    R2_coxme_fnc(efit1())
  }
})

##################
## R2 reduction ##
##################
#Create the Cox model (no censoring)
cme_fmla_null1 <- reactive({
  if (input$CoxmeYes == "Yes") {
    as.formula(paste(paste0("Surv(",input$variableY,")" , "~"),   
                   paste(mlv_cor(), collapse= "+")))
  }
})
#Create the Cox model with censoring
cme_fmla_null2 <- reactive({
  if (input$CoxmeYes == "Yes") {
    as.formula(paste(paste0("Surv(", input$variableY, ",", censor1(), ")", "~"),   
                   paste(mlv_cor(), collapse= "+")))
  }
})
#Run the multilevel Cox model
efitnull <- reactive({                  
  if (input$CoxmeYes == "Yes") {
    switch(input$regress_type, 
           "Cox PH"                =  coxme(cme_fmla_null1(), x=TRUE, y=TRUE,  
                                            data=df()[complete.cases(df()[, c(predictor(), outcome(), input$SrvPltX)]), ]),
           "Cox PH with censoring" =  coxme(cme_fmla_null2(), x=TRUE, y=TRUE,
                                            data=df()[complete.cases(df()[, c(predictor(), outcome(), input$SrvPltX)]), ]))
  }
})

##########################
# Intraclass correlation #
##########################
#ICC
rho_1 <- reactive({
  if (input$CoxmeYes == "Yes") {
  bw1_var()/(bw1_var() + (pi^2/6))
  }
})

########################
# Median hazards ratio #
########################
mhr_1 <- reactive({
  if (input$CoxmeYes == "Yes") {
  exp(sqrt(2 * bw1_var()) * qnorm(3/4))
  }
})
########################

# Random effects variance of the full model
bw1_var <- reactive({
  if (input$CoxmeYes == "Yes") {
  as.vector(VarCorr(efit1())[[cox_lev2()]])
  }
})
#Random effects variance of the Null model
bw01_var <- reactive({
  if (input$CoxmeYes == "Yes") {
    as.vector(VarCorr(efitnull())[[cox_lev2()]])
  }
})
#Random effects reduction in variance of the Null model
bw01_reduc <- reactive({
  if (input$CoxmeYes == "Yes") {
  (bw01_var() - bw1_var())/bw01_var()
  }
})

#Model assessment results
output$efit1_tests <- renderPrint({ 
  if (input$CoxmeYes == "Yes") {
    list("Random effects between-group SD"=c("1. SD"= sqrt(bw1_var()), "2. Added risk at 1 SD"= exp(sqrt(bw1_var()))),
      "Random effects likelihood ratio test"=c("LRT"=noquote(reLRT())),
         "Cox & Snell pseudo R2"= c("R2"=R2_coxme()),
         "Intraclass correlation"=c("ICC"= rho_1()),
         "Median hazard ratio" = c("MHR"= mhr_1()), 
       "Reduction in between group variance"=c("1. Reduction"=bw01_reduc(), "2. Null model variance"=bw01_var(),
                                               "3. Null model SD"= sqrt(bw01_var()) ))
  }
})

##################
## Frailty plot ##
##################

#Function to get multilevel frailties 
frail_fnc <- function(fit, REvar) {
  fdf1 <- data.frame(fit$frail)
  f_o <- order(fdf1[,1])
  fdf2 <- fdf1[f_o, 1, drop=F] 
  return(list("Alphabetical"=fdf1, "Numerical"=fdf2))
}

#Function to switch direction of multilevel frailties for cost purposes
frail_cost_fnc <- function(fit, REvar) {
  fdf1 <- data.frame(fit$frail) 
  f_o <- order(fdf1[,1], decreasing =TRUE)
  fdf2 <- fdf1[f_o, 1, drop=F]
  #Cost inverted by -1 to give the correct order
  fdf1 <- fdf1 * -1
  fdf2 <- fdf2 * -1 
  return(list("Alphabetical"=fdf1, "Numerical"=fdf2))
}

#Reactive function for getting survival frailties 
frailfncResult <- reactive({ 
  frail_fnc(fit=efit1(), REvar=bw1_var()) 
})

#Reactive function for getting survival frailties for cost 
frailfncCostResult <- reactive({ 
  frail_cost_fnc(fit=efit1(), REvar=bw1_var()) 
})

#Function that creates plot of frailties and returns alphabetical/numerical sorted values
frail_plot_fnc <- function(df, x, REvar, abbrLength) {
  RESD <- sqrt(REvar)
  frail <- df[[2]]
  #Plot
  xx <- barplot(frail[,1], names.arg= abbreviate(rownames(frail), abbrLength, method = "left.kept"), 
                main = paste0("Random effects frailties by ", x), 
                col="blue", cex.names=1, ylim=c(min(frail[,1])*1.2, max(frail[,1]))*1.2)
  text(x=xx, y=frail[,1]*1.1, abbreviate(rownames(frail), abbrLength, method = "left.kept"), cex=1)
  abline(h=RESD, lty=2, lwd=2, col="grey")
  abline(h=RESD*-1, lty=2, lwd=3, col="grey")
  legend(x="bottomright", legend=paste0("Random effects SD = ", round(sqrt(REvar), 3)), 
         lty=2, lwd=2, col="grey", bty= "n", cex=1.5)
}

frail_run <- reactive({
    if (cox_me_yes() == "Yes") {
    
    if (cox_me_cost_yes() == "No") {
      frail_plot_fnc(df=frailfncResult(), x=cox_lev2(), REvar=bw1_var(), abbrLength=abbrLength())
    } #else {
  if (cox_me_cost_yes() == "Yes") {
    frail_plot_fnc(df=frailfncCostResult(), x=cox_lev2(), REvar=bw1_var(), abbrLength=abbrLength())
  }    
  }
})


#This plots the frailties by the cluster variable  
output$frail_plot1 <- renderPlot({
    if (cox_me_yes() == "Yes") {
        frail_run()
      } 
}, height = 700)

#Indicates how long the abbrviation should be in the frailty plot
output$abbr_length <- renderUI({  
  numericInput("abbrLen", "1. Enter the above plot's minimum abbreviation length for labels.",
               value=3, min=1, max=10, step=1)     #Abbreviation shouldn't be long because of high cluster N
})

#Abbreviation length
abbrLength <- reactive({
  input$abbrLen 
})

#Frailties sorted alphabetically by group and numerically by score
output$frail_output <- renderPrint({ 
  if (cox_me_yes() == "Yes") {
    if (cox_me_cost_yes() == "Yes") {
      list("Alphabetical"=frailfncCostResult()$Alphabetical, "Numerical"=frailfncCostResult()$Numerical)
    } else {
      list("Alphabetical"=frailfncResult()$Alphabetical, "Numerical"=frailfncResult()$Numerical)
    }
  }
})


#UI function for drop down questions#
#Indicate if I am doing a cost analysis.
output$coxme_cost_yes <- renderUI({  
  selectInput("CoxmeCostYes", "3. Is this a cost analysis?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     #Will make choices based on my reactive function.
})

#Reactive function for if I want a Cox mixed effects model 
cox_me_yes <- reactive({ 
  input$CoxmeYes
})

#Reactive function for if the Cox model is cost related 
cox_me_cost_yes <- reactive({ 
  input$CoxmeCostYes
})

#####################
# Save model output #
#####################
output$SaveModelFitCme <- renderUI({  
  selectInput("save_mdl_cme", "1. Save the model fit?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     #Will make choices based on my reactive function.
})

#Model fit object
cmemdlfit <- reactive({
  if(input$save_mdl_cme == "Yes") {
    efit1()
  }
})

#The new data frame name for the model fit
output$cme_mdl_fit_name <- renderUI({ 
  textInput("CmeMdlFitName", "2. Enter the model fit name.", 
            value= "model_fit")     
})

output$cme_model <- downloadHandler(
  filename = "cme_model_fit.RData",
  content = function(con) {
    assign(input$CmeMdlFitName, cmemdlfit())
    save(list=input$CmeMdlFitName, file=con)
  }
)

###########################################################
##       Quantile and mean estimates for CPH and PSM     ##
###########################################################
#1. Select the grouping variable for the density plot with 2 plots--value of 1=treatment group
output$surv_binary_X <- renderUI({
  selectInput("survBinX", "1. Select a binary X to compare survival estimates.", 
              choices = predictor())     
})
#1A. Create a reactive function for the stratified variable in the Cost density plot
survival_var_X <- reactive({
  input$survBinX
})

#2. Select the type of density plot to run (full sample or by a group)
output$surv_binary_compare <- renderUI({
  selectInput("survBinComp", "2. Do you want to compare survival estimates?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     
})

############################################
#   Plot 2: Estimated cost over quantiles  #
############################################
surv_quant_ests_fnc <- function(fit, Y, X, reg) {
  #Creates function that will make correct value 
  surv_MED_cox <<- Quantile(fit)                                                       #Creates function to compute quantiles
  
  #5. Cost--get predicted values at different quantiles
  if (reg %in% c("Cox PH", "Cox PH with censoring")) {
    p1.50 <- Predict(fit, fun=function(x) surv_MED_cox(lp=x))                           #MED_coxian
    p1.10 <- Predict(fit, fun=function(x) surv_MED_cox(q=.90, lp=x))                    #10th percentile
    p1.25 <- Predict(fit, fun=function(x) surv_MED_cox(q=.75, lp=x))                    #25th percentile
    p1.75 <- Predict(fit, fun=function(x) surv_MED_cox(q=.25, lp=x))                    #75th percentile
    p1.90 <- Predict(fit, fun=function(x) surv_MED_cox(q=.10, lp=x))                    #90th percentile
  }
  if (reg %in% c("AFT","AFT with censoring") ) {
    p1.50 <- Predict(fit, fun=function(x) surv_MED_cox(lp=x))                           #MED_coxian
    p1.10 <- Predict(fit, fun=function(x) surv_MED_cox(q=.10, lp=x))                    #10th percentile
    p1.25 <- Predict(fit, fun=function(x) surv_MED_cox(q=.25, lp=x))                    #25th percentile
    p1.75 <- Predict(fit, fun=function(x) surv_MED_cox(q=.75, lp=x))                    #75th percentile
    p1.90 <- Predict(fit, fun=function(x) surv_MED_cox(q=.90, lp=x))                    #90th percentile
  }
  
  #This gets predicted intervention scores for different percentiles
  int_pnm <- c(paste0(X,".1"), paste0(X,".2")) #Only place I use X argument
  est1.10 <- p1.10[which(row.names(p1.10) %in% int_pnm), c("yhat", "lower", "upper")]
  est1.25 <- p1.25[which(row.names(p1.25) %in% int_pnm), c("yhat", "lower", "upper")]
  est1.50 <- p1.50[which(row.names(p1.50) %in% int_pnm), c("yhat", "lower", "upper")]
  est1.75 <- p1.75[which(row.names(p1.75) %in% int_pnm), c("yhat", "lower", "upper")]
  est1.90 <- p1.90[which(row.names(p1.90) %in% int_pnm), c("yhat", "lower", "upper")]
  #This changes it to a data.frame class from an RMS class
  class(est1.10) <- "data.frame"
  class(est1.25) <- "data.frame"
  class(est1.50) <- "data.frame"
  class(est1.75) <- "data.frame"
  class(est1.90) <- "data.frame"
  est1 <- data.frame(rbind(unlist(est1.10), unlist(est1.25), unlist(est1.50),     #Merge the results
                           unlist(est1.75), unlist(est1.90)))
  
  #Re-arrange order to get values set up for display in the plot
  #Cox PH
  if (reg %in% c("Cox PH", "Cox PH with censoring")) {
    est2 <- est1[, c(2,6,4,1,5,3)]
  }
  #AFT
  if (reg %in% c("AFT","AFT with censoring") ) {
    est2 <- est1[, c(2,4,6,1,3,5)]
  }
  colnames(est2) <- c("TREATMENT", "L95", "U95","Control", "L95", "U95")
  rownames(est2) <- c("p10", "p25", "p50","p75", "p90")
  
  #Add cost difference between Treatment and controls 
  est2$Diff. <- round(est2$TREATMENT - est2$Control, 2 )
  ## Add in weighted average into the table
  #Combine everything
  return(list(est1=est1, est2=est2))
}  

#This runs the quant_plt1_fnc() function above
surv_quant_ests <- reactive({
  if(input$survBinComp == "Yes") {
    surv_quant_ests_fnc(fit=fit1(), Y= outcome(), X= survival_var_X(), reg=input$regress_type)
  }  
})

#This prints the point estimates and confidence intervals
output$surv_quant_out1 <- renderTable({
  if(input$survBinComp == "Yes") {
    surv_quant_ests()[["est2"]]
  }
}, rownames = TRUE)


##########################
### Get mean estimates ###
##########################
surv_mean_ests_fnc <- function(fit, Y, X, reg) {
  #Creates function that will make correct value 
  surv_MN_cox <<- Mean(fit)                                                       #Creates function to compute quantiles
  
  #5. Cost--get predicted values at different quantiles
  if (reg %in% c("Cox PH", "Cox PH with censoring")) {
    p1.mean <- Predict(fit, fun=function(x) surv_MN_cox(lp=x))                           #MN_coxian
  }
  if (reg %in% c("AFT","AFT with censoring")) {
    p1.mean <- Predict(fit, fun=function(x) surv_MN_cox(lp=x))                           #MN_coxian
  }
  
  #This gets predicted intervention scores for different percentiles
  int_pnm <- c(paste0(X,".1"), paste0(X,".2")) #Only place I use X argument
  est1.mean <- p1.mean[which(row.names(p1.mean) %in% int_pnm), c("yhat", "lower", "upper")]
  #This changes it to a data.frame class from an RMS class
  class(est1.mean) <- "data.frame"
  est1 <- data.frame(rbind(unlist(est1.mean)))
  
  #Re-arrange order to get values set up for display in the plot
  #Cox PH
  if (reg %in% c("Cox PH", "Cox PH with censoring")) {
    est2 <- est1[, c(2,6,4,1,5,3)]
  }
  #AFT
  if (reg %in% c("AFT","AFT with censoring") ) {
    est2 <- est1[, c(2,4,6,1,3,5)]
  }
  colnames(est2) <- c("TREATMENT", "L95", "U95","Control", "L95", "U95")
  rownames(est2) <- "Mean"
  #Add cost difference beteen Treatment and controls 
  est2$Diff. <- abs(round(est2$Control) - round(est2$TREATMENT))
  return(list(est1=est1, est2=est2))
}  

#This runs the quant_plt1_fnc() function above
surv_mean_ests <- reactive({
  if(input$survBinComp == "Yes") {
    surv_mean_ests_fnc(fit=fit1(), Y= outcome(), X= survival_var_X(), reg=input$regress_type)
  }  
})

#This prints the point estimates and confidence intervals
output$surv_mean_out1 <- renderTable({
  if(input$survBinComp == "Yes") {
    surv_mean_ests()[["est2"]]
  }
}, rownames = TRUE)

#Observed mean and quantiles
#This gets quantiles and means
#Function that gets cost value quantiles between the treatment and control groups 
surv_quant <- function(y, x, df) {
  
  #Treatment/control group means/medians
  ctl_med <- median(df[, y][as.numeric(df[, x]) == min(as.numeric(df[, x]), na.rm=T)], na.rm=T)
  trt_med <- median(df[, y][as.numeric(df[, x]) == max(as.numeric(df[, x]), na.rm=T)], na.rm=T)
  ctl_mn <- mean(df[, y][as.numeric(df[, x]) == min(as.numeric(df[, x]), na.rm=T)], na.rm=T)
  trt_mn <- mean(df[, y][as.numeric(df[, x]) == max(as.numeric(df[, x]), na.rm=T)], na.rm=T)
  #### Get quantiles and bind with means. Partially used in the legend and the observed table in a later section
  qt1 <- quantile(df[, y][as.numeric(df[, x]) == min(as.numeric(df[, x]), na.rm=T)], probs=c(.1, .25, .5, .75, .9), na.rm=T)
  qt2 <- quantile(df[, y][as.numeric(df[, x]) == max(as.numeric(df[, x]), na.rm=T)], probs=c(.1, .25, .5, .75, .9), na.rm=T)
  odf_mqt <- as.data.frame(rbind(cbind(round(qt2, 2), round(qt1 ,2)),
                                 cbind(round(trt_mn, 2), round(ctl_mn, 2)) ))
  rownames(odf_mqt) <- c("p10", "p25", "p50", "p75", "p90", "Mean")
  colnames(odf_mqt) <- c("Treatment", "Control")  #Observed data.frame of the means and quantiles
  ###  
  return(list("odf_mqt"=odf_mqt))
}
#Run the function above
surv_quant_run <- reactive({
  if(input$survBinComp == "Yes") {
    surv_quant(y= outcome(), x= survival_var_X() , df= df())
  }  
})

## This extracts the observed data.frame means and quantiles from the reactive function below ##
surv_obs_df_mqt <- reactive({
  if(input$survBinComp == "Yes") {
    surv_quant_run()$odf_mqt
  }  
})
#This prints the means and quantiles
output$surv_obsdfmqt_out1 <- renderTable({
  if(input$survBinComp == "Yes") {
    surv_obs_df_mqt()
  }
}, rownames = TRUE)

###############################################
## Probability of survival at specific time  ##
###############################################
#1. Indicate survival time
output$surv_probability_time <- renderUI({
  numericInput("survProbTime", "1. Select a survival time.",
               value = 1, step = 1, min=1)
})
#1A. Reactive function of survival time
surv_Prob_Time_Val <- reactive({
  if(input$survTimeProbYN == "Yes") {
    input$survProbTime
  }  
})

#2. Select whether it is a survival probability or hazard function 
output$surv_time_probability_hazard_survival <- renderUI({
  selectInput("survTimeProbHazSurv", "2. Select a hazard or survival.", 
              choices = c("Hazard", "Survival"), multiple=FALSE, selected="Survival")     
})
#2A. Reactive function of survival or hazard
surv_Time_Prob_Haz_Srv <- reactive({
  if(input$survTimeProbYN == "Yes") {
    input$survTimeProbHazSurv
  }  
})

#3. Run the survival probability calculation
output$surv_time_probability_yesno <- renderUI({
  selectInput("survTimeProbYN", "3. Calculate the probability of survival time?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     
})

#4. Get values that are adjusted to
surv_adjto_vals <- reactive({
  if(input$survTimeProbYN == "Yes") {
    predict(fit1(), type="adjto.data.frame")
  }  
})

#5. Run the function below
surv_hazard_survival_function_run <- reactive({
  if(input$survTimeProbYN == "Yes") {
    fncProbSrvTime(fit=fit1(), time=surv_Prob_Time_Val(), reg=input$regress_type, DataFrm=surv_adjto_vals(), what=surv_Time_Prob_Haz_Srv())
  }  
})

#6. Probability output 
output$surv_hazard_survival_function_out <- renderPrint({
  if(input$survTimeProbYN == "Yes") {
    surv_hazard_survival_function_run()
  }
})

## Function that creates survival probability at a certain time ##
fncProbSrvTime <- function(fit, time, reg, DataFrm, what) {
  #Gets 'adjusted to' values from the model
  adj_vals <- edit(DataFrm)
  #Get predicted values
  X.beta <- predict(fit, data.frame(adj_vals))
  #Calculate probabilities
  if (reg %in% c("AFT","AFT with censoring")) {
    if (what == "Survival") {
      surv  <- Survival(fit)
      Prob  <- surv(time, X.beta)
    } else {
      haz   <- Hazard(fit)
      Prob  <- haz(time, X.beta)
    } 
  } 
  if (reg %in% c("Cox PH","Cox PH with censoring")) {
    if (what == "Survival") {
      surv  <- Survival(fit)
      Prob  <- surv(time, X.beta)
    }  
    else {
      stop("Error: Cannot get hazard probablility for Cox PH model. Try AFT psm().")
    }
  }
  #Assign name of probability  
  names(Prob) <- what 
  #Final values  
  return(list( "Probability"=Prob, "Time"=time, "X.Values"=adj_vals))
}

#################################
## Download survival estimates ##
#################################
#1. Indicate survival time
output$survival_estimate_time <- renderUI({
  numericInput("survEstTime", "1. Select a survival time.",
               value = 1, step = 1, min=1)
})

#2. Select whether it is a survival probability or hazard function 
output$survival_estimate_hazard_survival <- renderUI({
  selectInput("survEstHazSurv", "2. Select a hazard or survival.", 
              choices = c("hazard", "survival"), multiple=FALSE, selected="survival")     
})

#3. Run the survival probability calculation
output$survival_estimate_yesno <- renderUI({
  selectInput("survEstYN", "3. Calculate the estimates?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     
})
#4. Download time dependent data file
output$surv_est_data_download_name <- renderUI({ 
  textInput("SEDataDownloadName", "4. Enter the data framename.", 
            value= "survival_est")     
})
#5. Setup to download
output$se_data_download <- downloadHandler(
  filename = "survival_estimate_df.RData",
  content = function(con) {
    assign(input$SEDataDownloadName, SEDataFrame())
    save(list=input$SEDataDownloadName, file=con)
  }
)

#Reactive function that runs the multi-state data function above
SEDataFrame <- reactive({
  if (input$survEstYN=="Yes") {
    fncDownSrvEst(fit=fit1(), Time=input$survEstTime, reg=input$regress_type, DataFrm=df(), What=input$survEstHazSurv)
  }
})

## Function that creates survival estimate data at a certain time ##
fncDownSrvEst <- function(fit, Time, reg, DataFrm, What) {
  #Median survival time
  med   <- Quantile(fit)
  #Mean survival time
  meant <- Mean(fit)
  
  #Calculate probabilities
  if (reg %in% c("AFT","AFT with censoring")) {
    if (What == "survival") {
      se1 <- survest(fit, newdata=DataFrm, times=Time, what=What) 
    } else {
      se1 <- survest(fit, newdata=DataFrm, times=Time, what=What)  #Does not work on CPH
    } 
  } 
  if (reg %in% c("Cox PH","Cox PH with censoring")) {
    if (What == "survival") {
      se1 <- survest(fit, newdata=DataFrm, times=Time)
    }  
    else {
      stop("Error: Cannot get hazard function for Cox PH model. Try AFT psm().")
    }
  }
  #Construct data frame for output  
  if (reg %in% c("AFT","AFT with censoring")) {
    if (What == "survival") {
      SurvEst <- data.frame(meant(lp=se1$linear.predictors),med(lp=se1$linear.predictors), se1$linear.predictors, se1$surv, se1$lower, se1$upper, se1$std.err)
      colnames(SurvEst) <- c("Mean.Surv.Time","Med.Surv.Time","LinearPredictor", "survival","Lower", "Upper","SE")
    } else {
      SurvEst <- se1
    } 
  } 
  if (reg %in% c("Cox PH","Cox PH with censoring")) {
    if (What == "survival") {
      SurvEst <- data.frame(meant(lp=predict(fit)),med(lp=predict(fit)), se1$surv, se1$lower, se1$upper, se1$std.err)
      colnames(SurvEst) <- c("Mean.Surv.Time","Med.Surv.Time", "survival","Lower", "Upper","SE")
    }  
    else {
      stop("Error: Cannot get hazard function for Cox PH model. Try AFT psm().")
    }
  }
  #Final data  
  return(SurvEst)
}


################################################################################


################################################################################
##                           Multi-state model                                ##
################################################################################

##################
# Create MS data #
##################
#1. Select the transient state time variables
output$st_Time_Trans <- renderUI({                                #Creates a UI function here but it will
  selectInput("stTimeTran", "1. Select transient-state time variables",       #get called to the UI file.
              choices = var(), multiple=TRUE )   #Will make choices based on my reactive function.
})
#1A. Object with just transient state time variables
state_time_trans <- reactive({
  input$stTimeTran
})

aprx_mdl_fmla2 <- reactive({ 
  as.formula(input$up_fmla)
})

#2. Select the terminal state  binary variable
output$st_Bin_Term <- renderUI({
  selectInput("stBinTerm", "2. Select a terminal-state binary indicator",
              choices = setdiff(var(), state_time_trans()), multiple=FALSE )
})
#2A. Object with just terminal state time indicator
state_bin_term <- reactive({
  input$stBinTerm
})

#3. Select the terminal state  time variables
output$st_Time_Term <- renderUI({
  selectInput("stTimeTerm", "3. Select terminal-state time variable",
              choices = setdiff(var(), c(state_bin_term(), state_time_trans()) ), multiple=FALSE )
})
#3A. Object with just terminal state time variables
state_time_term <- reactive({
  input$stTimeTerm
})

########
#4. Select the cumulative Prior and current events variables based on state time variables above 
output$st_Prior_Event <- renderUI({
  selectInput("stPrEvnt", "4. Select states to track events",
              choices = state_time_trans(), multiple=TRUE)
})
#4A. Object with just transient state time variables
state_prior_event <- reactive({
  input$stPrEvnt
})
#5. Select the ID variable
output$st_Time_ID <- renderUI({
  selectInput("stTimeID", "5. Select the ID variable",
              choices = setdiff(var(), c(state_time_trans(), state_bin_term(), state_time_term(),
                                         state_prior_event()) ), multiple=FALSE)
})
#5A. ID variable
state_time_id <- reactive({
  input$stTimeID
})
#6. Select the time-independent variables to merge into data 
output$st_Time_I_V <- renderUI({
  selectInput("stTimeIV", "6. Select the time-independent variables",
              choices = setdiff(var(), c(state_time_trans(), state_bin_term(), state_time_term(),
                                         state_prior_event(), state_time_id()) ), multiple=TRUE)
})
#6A. Object with just time-independent variables
state_time_independent_x <- reactive({
  input$stTimeIV
})

#7. Make data frame
output$make_multi_state_df <- renderUI({  
  selectInput("makeMultiStateDF", "7. Create the multi-state dataframe?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     
})
#8. Download time dependent data file
output$ms_data_download_name <- renderUI({ 
  textInput("MsDataDownloadName", "8. Enter the data framename.", 
            value= "mdata")     
})
#9. Setup to download
output$ms_data_download <- downloadHandler(
  filename = "multi_state_df.RData",
  content = function(con) {
    assign(input$MsDataDownloadName, MsDataFrame())
    save(list=input$MsDataDownloadName, file=con)
  }
)


#######################################
## Function to make multi-state data ##
#######################################
msDataFnc <- function(MSDF, input_dataframe, STATE_time_trans, STATE_bin_term, STATE_time_term,
                      STATE_prior_event, STATE_time_id, STATE_time_independent_x) {
  #Set up
  msdf <- input_dataframe
  #Data frames
  Data2 <- paste( "data2 =", msdf)
  D1 <- paste0( msdf, "[,", paste0("c(", paste0(which(colnames(MSDF) %in% c(STATE_time_independent_x, STATE_time_id)), collapse=","), ")", "]" ))
  Data1 <- paste("data1=", D1)
  #ID variable
  ID <- paste("id=", STATE_time_id)
  #Terminal variable
  Terminal_variable <- paste(STATE_bin_term, "=",  "event(", STATE_time_term, ",", STATE_bin_term,")"  )
  #Transient variables
  state_time_trans_ls <- list()
  #Loops through variable names, keeps the name since the wide format variables are removed in the call
  for (i in 1:length(STATE_time_trans)) {
    state_time_trans_ls[i] <- paste(STATE_time_trans[i], "=", "event(", STATE_time_trans[i],")")
  }
  state_time_trans_fmla <- paste(unlist(state_time_trans_ls), collapse=",")
  #Prior variables and cumulative events
  #Prior events
  state_prior_event_ls <- list()
  for (i in 1:length(STATE_prior_event)) {
    state_prior_event_ls[i] <- paste0("pri_", STATE_prior_event[i], "=", "tdc(", STATE_prior_event[i],")")
  }
  state_prior_event_fmla <- paste(unlist(state_prior_event_ls), collapse=",")
  #Cumulative events...uses same events as prior
  state_cumul_event_ls <- list()
  for (i in 1:length(STATE_prior_event)) {
    state_cumul_event_ls[i] <- paste0("cml_", STATE_prior_event[i], "=", "cumevent(", STATE_prior_event[i],")")
  }
  state_cumul_event_fmla <- paste(unlist(state_cumul_event_ls), collapse=",")
  #This gives me all of the arguments
  tmerge_args <- paste(unlist(list(Data1, Data2, ID, Terminal_variable, state_time_trans_fmla, state_prior_event_fmla, state_cumul_event_fmla)), collapse=",")
  #2nd tmerge call that calculates the cumulative number of events
  tmerge_args2 <- paste("; multi_state_df <- tmerge(multi_state_df, multi_state_df,", ID, ",", "enum = cumtdc(tstart))")
  #3rd tmerge call that makes a list of current events
  all_events <- c(STATE_time_trans, STATE_bin_term)
  level_by_state_ls <- list()
  for (i in 1:length(all_events)) {
    level_by_state_ls[i] <- paste0(i,"*",all_events[i])
  }
  tmerge_args3 <- paste("; temp <- with(multi_state_df,", paste(unlist(level_by_state_ls), collapse=" + "),")" )
  #4th tmerge call creates a variable for the current events above
  tmerge_args4 <- paste("; multi_state_df$event <- factor(temp, labels=", 
                        paste0("c('", paste(c("none", all_events),collapse="','"), "')"), ")")
  
  #The pastes in tmerge and the dataframe name to create the full code
  tmerge_formula <- paste("multi_state_df <- tmerge(", tmerge_args, ")", 
                          tmerge_args2, tmerge_args3, tmerge_args4 )
  return(tmerge_formula)
}

##################################
# Function that runs tmerge code # 
##################################
msDfsourceFnc <- function(tsetup) {
  #Create temporary file
  tempfile("tp", fileext=".txt")
  f <- tempfile()
  #Begin to load temp file with tmerge setup code
  sink(f)
  #Remove quotes from setup code
  cat(tsetup)
  sink(NULL)
  #Run code to create data frame
  source(f)
  #End connection
  unlink(f)
  rm(f)
}

############################################
## Function that modifies multistate data ##
############################################
#Modify multi-state data, add in current state and time in each state
modMSdfFnc <- function(MDATA, id_var) {
  ## Add current state into the data
  #Get list of events
  events <- list()
  for (i in 1:length(unique(MDATA[, id_var]))) {
    events[[i]] <- MDATA[MDATA[, id_var] == i, "event"]
  }
  #Make current state
  cstate <- lapply(events, function(x) x[-length(x)])
  cstate <- lapply(cstate, function(x) c("none",as.character(x)))
  #Make current state
  cstatedf <- data.frame("cstate"=unlist(cstate) )
  #Add cstate data in with main file
  MDATA$cstate <- cstatedf$cstate
  #Add state time
  MDATA$stime <- MDATA$tstop - MDATA$tstart 
  return(MDATA)
}

##############################################
## Function that runs all 3 functions above ##
##############################################
makeMsDF3fnc <- function(MSDF, input_dataframe, STATE_time_trans,
                         STATE_bin_term, STATE_time_term,
                         STATE_prior_event, STATE_time_id,
                         STATE_time_independent_x) {
#Sets up script that is saved as a temp file  
  tsetup <- msDataFnc(MSDF=MSDF, input_dataframe= input_dataframe, STATE_time_trans=STATE_time_trans,
                      STATE_bin_term=STATE_bin_term, STATE_time_term=STATE_time_term,
                      STATE_prior_event=STATE_prior_event, STATE_time_id=STATE_time_id,
                      STATE_time_independent_x=STATE_time_independent_x)
  #Sources the temp file to create the data
  msDfsourceFnc(tsetup)
  #Makes final modifications to data frame
  multi_state_df <- modMSdfFnc(MDATA=multi_state_df, id_var=STATE_time_id)
  return(multi_state_df)
}
#Reactive function that runs the multi-state data function above
MsDataFrame <- reactive({
  if (input$makeMultiStateDF=="Yes") {
    makeMsDF3fnc(MSDF=df(), input_dataframe= input$dataframe, STATE_time_trans=state_time_trans(),
                 STATE_bin_term=state_bin_term(), STATE_time_term=state_time_term(),
                 STATE_prior_event=state_prior_event(), STATE_time_id=state_time_id(),
                 STATE_time_independent_x=state_time_independent_x())
  }
})

###########################
## Load multi-state data ##
###########################
#1. Enter the multi-state data name
output$ms_df_input_name <- renderUI({ 
  textInput("msDfInputName", "1. Enter the multi-state data name", value="mgus1")     
})
#1A. Get the data
df_MULTI_STATE <- reactive({                  #This indicates the data frame I will use.
  get(input$msDfInputName)  
})
#1C. Get the variable names  
var_ms_df <- reactive({                  #I use this to get the variable names from the data frame. 
  names(df_MULTI_STATE())  
})  

#2. Select the time start variable
output$st_Time_Start <- renderUI({                                
  selectInput("stTimeStart", "2. Select 'time start' variable",
              choices = var_ms_df(), multiple=FALSE )
})
#2A. Object with just start time 
state_time_start <- reactive({
  input$stTimeStart
})
#3. Select the time stop variable
output$st_Time_Stop <- renderUI({                                
  selectInput("stTimeStop", "3. Select 'time stop' variable",
              choices = setdiff(var_ms_df(), state_time_start()), multiple=FALSE )
})
#3A. Object with just time stop indicator
state_time_stop <- reactive({
  input$stTimeStop
})
#4. Select the event variable
output$st_Time_Event <- renderUI({                                
  selectInput("stTimeEvent", "4. Select the event variable",
              choices = setdiff(var_ms_df(), c(state_time_start(), state_time_stop()) ), multiple=FALSE )
})
#4A. Object with just event indicator
state_time_event <- reactive({
  input$stTimeEvent
})
#5. Select the ID variable
output$st_Time_Srvchck_ID <- renderUI({                                
  selectInput("stTimeSrvchckID", "5. Select the ID variable",
              choices = setdiff(var_ms_df(), c(state_time_start(), state_time_stop(), state_time_event()) ), 
              multiple=FALSE )
}) 
#5A. Object with just ID indicator
state_time_Srvchck_ID <- reactive({
  input$stTimeSrvchckID
})
#6. Check the survival pattern and construction of the dataset
output$check_multi_state_df <- renderUI({  
  selectInput("checkMultiStateDF", "6. Do you want to check the data build?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     
})
#6A. Run the function that shows the survival check and data attributes
Run_survival_check <- reactive({
  if (input$checkMultiStateDF=="Yes") {
    srvChckFnc(Data=df_MULTI_STATE(), ID=state_time_Srvchck_ID(), tstart=state_time_start(), 
               tstop=state_time_stop(), event=state_time_event())
  }
})
#6B. Show the output of the check
output$survival_check_attr <- renderPrint({
  Run_survival_check()
})

###############################################
## Function to check data and transitions ##
###############################################
srvChckFnc <- function(Data, ID_variable, tstart, tstop, event) {
  #Create formula for survival check
  srvChckFmla <- as.formula(paste0("Surv(", tstart, ",", tstop, ",", event,  ")", "~ 1") )
  
  #Check the counts for various transitions and number of transitions per subject in each state
  Survival_Check <- survcheck(srvChckFmla, data=Data, id= Data[, which(colnames(Data) == ID_variable) ])
  
  #Examine construction of the time dependent dataset
  Attributes <-attr(Data, "tcount")
  return(list(Survival_Check=Survival_Check, Attributes=Attributes))
}  

##############################################
## Examine the current probability-in-state ##
##############################################
#Aalen-Johansen survival estimate
#7. Choose stratified probabilities
output$prob_in_state_strata <- renderUI({  
  selectInput("ProbInStateStrata", "7. Want stratified probabilities?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     
})
#7A. Object for stratified probabilities yes/no 
Pr_in_St_Strata <- reactive({
  input$ProbInStateStrata
})
#8. Select the time start variable
output$prob_in_state_strata_factor <- renderUI({
  selectInput("ProbInStateStrataFactor", "8. Select a strata factor",
              choices = var_ms_df(), multiple=FALSE )
})
#8A. Object with just start time 
Pr_in_St_Strata_Factor <- reactive({
  if (Pr_in_St_Strata() =="Yes") {
    input$ProbInStateStrataFactor
  } else {
    NULL
  }
})
#8B. Max survival time restricted mean
Pr_in_St_max_time <- reactive({
  max(df_MULTI_STATE()[, state_time_stop()], na.rm=TRUE)
})
#9. Restricted mean 
output$prob_in_state_rmean <- renderUI({  
  numericInput("probInStateRmean", "9. Select restricted mean time",
               value=Pr_in_St_max_time(), min=1, max=Pr_in_St_max_time(), step=1)
})
#9A. Object for Restricted mean 
Pr_in_St_Rmean <- reactive({
  input$probInStateRmean
})
#10. Run probability-in-state curves
output$run_probability_in_state <- renderUI({  
  selectInput("runProbInSt", "10. Run probability-in-state?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     
})
#10A. Object for running probability in state curves 
Run_Pr_in_St <- reactive({
  input$runProbInSt
})
#10B. Run the function that calculates the unconditional probability in state
run_prob_in_state_0 <- reactive({
  if (Run_Pr_in_St() =="Yes") {
    probStCrv0Fnc(Data=df_MULTI_STATE(), ID_var=state_time_Srvchck_ID(), tstart_var=state_time_start(), 
                  tstop_var=state_time_stop(), event_var=state_time_event())
  }
})
#10C. Run the unconditional survival fit
prob_in_state_0 <- reactive({
  run_prob_in_state_0()
})

#10D. Run the function that calculates the probability in state for a stratified analysis
run_prob_in_state_1 <- reactive({
  if (Run_Pr_in_St() =="Yes") {
    if (Pr_in_St_Strata() =="Yes") {
      probStCrv1Fnc(Data=df_MULTI_STATE(), ID_var=state_time_Srvchck_ID(), tstart_var=state_time_start(), 
                  tstop_var=state_time_stop(), event_var=state_time_event(), X= Pr_in_St_Strata_Factor() )
    }
  }
})
#10E. Run the conditional survival fit
prob_in_state_1 <- reactive({
  run_prob_in_state_1()
})

#10F. Run the function for probability in state 95% confidence intervals
summary_CI_prob_in_state <- reactive({
  #  if (Run_Pr_in_St() =="Yes") {
  fncPsSmryCI(RUN=Run_Pr_in_St(), IS.sTRATA=Pr_in_St_Strata(),
              PS0=run_prob_in_state_0(), PS1=run_prob_in_state_1(), RMean=Pr_in_St_Rmean())
  #  }
})
#10G. Run the function above
output$print_prob_in_state <- renderPrint({
  if (Run_Pr_in_St() =="Yes") {
    summary_CI_prob_in_state()
  }
})

###############################################
## Function for probability in state         ##
###############################################
#Null model survival function
probStCrv0Fnc <- function(Data, ID_var, tstart_var, tstop_var, event_var) {
  #Formula
  srvFuncFmla0 <- as.formula(paste0("Surv(", tstart_var, ",", tstop_var, ",", event_var,  ")", "~ 1") )
  #Survival function for a null model
  Survival_function_0 <- survfit(srvFuncFmla0, data=Data, id=Data[, which(colnames(Data) == ID_var) ],
                                 influence=TRUE) 
  return(Survival_function_0)
}
#Single covariate survival function
probStCrv1Fnc <- function(Data, ID_var, tstart_var, tstop_var, event_var, X) {
  #Formula
  srvFuncFmla1 <- as.formula(paste0("Surv(", tstart_var, ",", tstop_var, ",", event_var,  ")", "~ ", X) )
  #Survival function for a null model
  Survival_function_1 <- survfit(srvFuncFmla1, data=Data, id=Data[, which(colnames(Data) == ID_var) ], 
                                 influence=TRUE) 
  return(Survival_function_1)
}
###############################################################################
# Function to add 95% confidence intervals to the probability in state output # 
###############################################################################
fncPsSmryCI <- function(RUN="No", IS.sTRATA="No", PS0, PS1, RMean) {
  if (RUN =="Yes") {
    if (IS.sTRATA =="Yes") {
      s1 <- summary(PS1, rmean= RMean )$table
    } else {
      s1 <- summary(PS0, rmean= RMean  )$table
    }
  }
  s1 <- data.frame(s1)
  s1$Lower.95.CL <- s1$rmean - (1.96 * s1[,4])
  s1$Upper.95.CL <- s1$rmean + (1.96 * s1[,4])
  colnames(s1)[3] <- paste0("rmean(T=", RMean, ")")
  colnames(s1)[4] <- "std(rmean)"
  return(s1) 
}

## Restricted mean time-in-state ##
output$rmean_time_state_prime_level <- renderUI({
  selectInput("rmeanTmStPrLev", "11. Select the primary level",
              choices = levels(as.factor( df_MULTI_STATE()[, Pr_in_St_Strata_Factor()])), multiple=FALSE )
})
#11A. Primary level_name 
rmean_Tm_St_Prime_Level <- reactive({
  input$rmeanTmStPrLev
})
#11B. 
rmean_Tm_St_Prime_Level_number <- reactive({
  which(levels(as.factor( df_MULTI_STATE()[, Pr_in_St_Strata_Factor()])) == rmean_Tm_St_Prime_Level())
  })
#12. Restricted mean 
output$rmean_time_in_state_rmean <- renderUI({  
  numericInput("rmeanTmStRmean", "12. Select restricted mean time",
               value=Pr_in_St_max_time(), min=1, max=Pr_in_St_max_time(), step=1)
})
#12A. Object for Restricted mean 
rmean_Time_in_St_Rmean <- reactive({
  input$rmeanTmStRmean
})
#13. Run mean time-in-state curves
output$run_time_in_state_z_test <- renderUI({  
  selectInput("runTmInStZTst", "13. Run mean time-in-state test?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     
})
#13A. Object for mean time-in-state test 
Run_Tm_State_Z_Test <- reactive({
  input$runTmInStZTst
})

#13B. Run the function that calculates the mean difference test
run_mean_time_in_state <- reactive({
  if (Run_Tm_State_Z_Test() =="Yes") {
    rmean_z_test(survobj= prob_in_state_1(), rmean=rmean_Time_in_St_Rmean(), 
                 level= rmean_Tm_St_Prime_Level_number())  
    }
})

#13C. Run the conditional survival fit
mn_Tm_In_State_Z_Test <- reactive({
  run_mean_time_in_state()
})
#13D. Print the probability in state output
output$print_prob_in_state_Z_test <- renderPrint({
  if (Run_Tm_State_Z_Test() =="Yes") {
    mn_Tm_In_State_Z_Test()
#    run_mean_time_in_state()
  }
})


###############################################
## Function for Z test of mean time in state ##
###############################################
rmean_z_test <- function(survobj, rmean=NULL, level) {
  number_of_strata <- length(survobj[["strata"]])
  #This will stop the function from working if it doesn't have 2 groups in the strata
  if(number_of_strata != 2) 
    stop(paste0("Only runs for number of groups = 2. Your number of groups = ", number_of_strata)) 
  s1 <- summary((survobj), rmean=rmean)$table                                   #Get summary info
  r1 <- nrow(s1)                                                                #Get max row number
  tmp_ls <- strsplit(names(survobj$strata), "=")                                #Split strata levels and var name
  strata_levs <- c(tmp_ls[[1]][[2]], tmp_ls[[2]][[2]])                          #List of strata levels
  level_name <- strata_levs[level]                                              #Primary group name
  row_numbers <- grep(paste0("=",level_name ),                                  #Get primary group's row numbers 
                      rownames(summary((survobj))$table))
  #Put each group's output into a list that I will merge later
  df_list <- list()
  df_list[[1]] <- s1[row_numbers, ]
  df_list[[2]] <- s1[setdiff(1:r1, row_numbers), ]
  #Calculate summary statistics  
  Diff <- df_list[[1]][,3] - df_list[[2]][,3]
  SE  <- sqrt(df_list[[1]][,4]^2 + df_list[[2]][,4]^2)
  Z <- abs(Diff)/SE
  df2nms <- rownames(df_list[[2]])
  #2 tail p-value
  P.Value <- (1-pnorm(Z))*2
  dfz <- data.frame(df_list[[1]], "Group 2"=df2nms, df_list[[2]], Diff, SE, Z, P.Value)
  colnames(dfz)[c(1:4, 6:9)] <- c("n.1", "nevent.1", "rmean.1", "std.rmean.1",
                                  "n.2", "nevent.2", "rmean.2", "std.rmean.2")
  return(dfz)
}

################################
## Graph probability in state ##
################################
#14. Stratify probability-in-state curves
output$prob_state_strata_plot_yesno <- renderUI({  
  selectInput("prStStrplotYn", "14. Want to stratify curves?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     
})
#14A. Object for running probability in state curves 
Prob_State_Strat_Plot_YN <- reactive({
  input$prStStrplotYn
})

# Get the state names
Prob_St_Event_Names <- reactive({
  as.character(unlist(dimnames(prob_in_state_1()$transitions)[1]))
})
## Colors ##
#15. Select colors for probability-in-state curves
output$prob_state_curves_colors <- renderUI({  
  selectInput("prStcurveColor", "15. Select state colors for curves", 
              choices = colors()[c(24,552, 498, 652, 254,26, 152,8, 32,68,
                                   75,95,119,139,142,310 ,367 ,450 ,568 ,589)], multiple=TRUE)     
})
#15A. Object for curve colors 
Prob_St_Curve_Color <- reactive({
  input$prStcurveColor
})
#15B. Run function to assign colors to states
Prob_St_Curve_Color_Assign <- reactive({
  fncPrStCurveCol(States= Prob_St_Event_Names(), Choices=Prob_St_Curve_Color() ) 
})
#15C. Run the probability in state curve colors
Prob_State_Curve_Colors <- reactive({
  fncPrStCurveCol(States= unname(unlist(dimnames(prob_in_state_0()$transitions)[1])), 
                  Choices=Prob_St_Curve_Color_Assign())
})

#16. Select the time excluded states
output$prob_state_exclude_state <- renderUI({
  selectInput("prStExclSt", "16. Select the states to exclude",
              choices = Prob_St_Event_Names(), multiple=TRUE )
})
#16A. Object with just start time 
Prob_St_Exclude_State <- reactive({
  input$prStExclSt
})
#17. Legend location
output$prob_state_time_legend <- renderUI({  
  selectInput("prStTmLegend", "17. Select the legend location",
              choices = c("bottomright", "bottom", "bottomleft", "left", 
                          "topleft", "top", "topright", "right", "center"),  multiple=FALSE)
})
#17A. Object with Legend location
Prob_St_Tm_legend <- reactive({
  input$prStTmLegend
})

## X and Y limits ##
#18. Indicate lower limit of x-axis
output$prob_state_curve_Xlim1 <- renderUI({
  numericInput("psc_Xlim1", "18. Lower X-axis limit",
               value = 0, step = 1)
})
#18A. Object for lower limit of x-axis
Pr_St_curve_Xlim1 <- reactive({
  input$psc_Xlim1
})

#19. Indicate upper limit of x-axis
output$prob_state_curve_Xlim2 <- renderUI({
  numericInput("psc_Xlim2", "19. Upper X-axis limit",
               value = Pr_in_St_max_time(), step = 1)
})
#19A. Object for upper limit of x-axis
Pr_St_curve_Xlim2 <- reactive({
  input$psc_Xlim2
})

#20. Indicate lower limit of y-axis
output$prob_state_curve_Ylim1 <- renderUI({
  numericInput("psc_Ylim1", "20. Lower Y-axis limit",
               value = 0, step = .01)
})
#20A. Object for lower limit of Y-axis
Pr_St_curve_Ylim1 <- reactive({
  input$psc_Ylim1
})

#21. Indicate upper limit of y-axis
output$prob_state_curve_Ylim2 <- renderUI({
  numericInput("psc_Ylim2", "21. Upper Y-axis limit",
               value = 1, step = .01)
})
#21A. Object for lower limit of Y-axis
Pr_St_curve_Ylim2 <- reactive({
  input$psc_Ylim2
})

#22. X axis time values
output$prob_state_time_x_axis_vals <- renderUI({  
  selectInput("prStTmXAxisVls", "22. X-axis time value labels",
              choices = as.character(0:Pr_in_St_max_time()),  multiple=TRUE)
})
#22A, Object with X axis values
Prob_St_Tm_X_Axis_Val <- reactive({
  input$prStTmXAxisVls
})
#23. X axis location for text values
output$prob_state_time_x_axis_text <- renderUI({  
  numericInput("prStTmXAxisTxt", "23. X-axis state label location",
              min=0, max=Pr_in_St_max_time(), step=1, value=Pr_in_St_max_time() * 0.25)
})
#23A, Object with X axis text value
Prob_St_Tm_X_Axis_Txt <- reactive({
  input$prStTmXAxisTxt
})

#24. Run probability-in-state curves
output$run_prob_state_curves <- renderUI({  
  selectInput("runPrStCurve", "24. Run probability-in-state curves?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     
})
#24A. Object for mean time-in-state test 
Run_Pr_St_Curves <- reactive({
  input$runPrStCurve
})
#24B. Run the probability in state curve plot 
Run_Prob_State_Curve_Plot <- reactive({
  if(Run_Pr_St_Curves() == "Yes") {
    if(Prob_State_Strat_Plot_YN() == "Yes") {
    fncProbStateCurve(AJcurve=prob_in_state_1(), Strata_fac_Name=Pr_in_St_Strata_Factor(),
                      Color=Prob_St_Curve_Color_Assign(), 
                      XLim=c(Pr_St_curve_Xlim1(), Pr_St_curve_Xlim2() ), 
                      YLim=c(Pr_St_curve_Ylim1(), Pr_St_curve_Ylim2() ),  
                      XTime= Prob_St_Tm_X_Axis_Val(), Excluded=Prob_St_Exclude_State(), 
                      TextX=Prob_St_Tm_X_Axis_Txt(), 
                       Leg_Loc=Prob_St_Tm_legend() ) 
  } else {
    fncProbStateCurve(AJcurve=prob_in_state_0(), Strata_fac_Name=Pr_in_St_Strata_Factor(),
                      Color=Prob_St_Curve_Color_Assign(), 
                      XLim=c(Pr_St_curve_Xlim1(), Pr_St_curve_Xlim2() ), 
                      YLim=c(Pr_St_curve_Ylim1(), Pr_St_curve_Ylim2() ),  
                      XTime= Prob_St_Tm_X_Axis_Val(), Excluded=Prob_St_Exclude_State(), 
                      TextX=Prob_St_Tm_X_Axis_Txt(), 
                      Leg_Loc=Prob_St_Tm_legend() ) 
  }
  }
})
#24C. Object for the run of the probability in state curves
Prob_State_Curve_Plot <- reactive({
  if(Run_Pr_St_Curves() == "Yes") {
    Run_Prob_State_Curve_Plot()  
  }
  })
#24D. Render plot function for the Plot the probability in state curves
output$probability_in_state_plot <- renderPlot({
  if(Run_Pr_St_Curves() == "Yes") {
    Prob_State_Curve_Plot()  
    }
})

##################################################  
## Function to plot probability in state curves ##
##################################################  
fncProbStateCurve <- function(AJcurve, Strata_fac_Name=NULL, Color=NULL, XLim=NULL, YLim=NULL, XTime=NULL, 
                              Excluded=NULL, TextX=NULL, Leg_Loc=NULL) {
  #Get values for plot arguments 
  summary_state_nms <- unname(unlist(dimnames(AJcurve$transitions)[1]))               #Run first. State names
  number_state_nms <- length(summary_state_nms)                               #Number of states
  Line_Colors <- Color[ which(!summary_state_nms %in% Excluded ) ] 
#  Line_Colors <- Color 
  strata_number <- if(length(AJcurve[["strata"]])== 0) {                      #Number of strata
    1 
  } else {
    length(AJcurve[["strata"]])
  }                            
  Strata_fac <- if( strata_number== 1) {                                      #Strata names
    "All" 
  } else {
    Strata_fac_Name
  }                
  active_plot_states <- setdiff(summary_state_nms, Excluded)                  #Total strata - Excluded
  strata_levs <- if( strata_number== 1) {
    "All" 
  } else {
    sub(paste0(Strata_fac ,"="), "", names(AJcurve$strata) )                  #List of strata level names
  }
  #Get X and Y coordinates on the curves
  tpstate <- summary(AJcurve)                                                 #Survival fit summary
  tpstate_probs <- data.frame(cbind(tpstate$time, tpstate$pstate))            #Curve X,Y coordinates
  colnames(tpstate_probs) <- c("Time", summary_state_nms)                     #Column names
  #Calculate text label Y coordinates
  exlv <- round(TextX, 0)
#  rng_exlv <- (exlv-5):(exlv+5)
#  xy_cord_df <- tpstate_probs[tpstate_probs$Time %in% rng_exlv, ]
  rng_exlv <- (which.min(abs(tpstate_probs$Time - exlv)) - 5):(which.min(abs(tpstate_probs$Time - exlv)) + 5)
  xy_cord_df <- tpstate_probs[tpstate_probs$Time %in% range(tpstate_probs$Time[rng_exlv])[1]:range(tpstate_probs$Time[rng_exlv])[2], ]
  xy_cord_df_mean <- colMeans(xy_cord_df[, -1])
  #Legend name
  Leg_Name <- strata_levs
  #Plot title
  Plot_title <- if( strata_number== 1) {
    "Probability-in-State over time"
  } else {
    paste0("Probability-in-State over time by ",Strata_fac, " levels" )                  
  }
  #Set up plot
  plot(AJcurve, col=rep(Line_Colors, each=strata_number), main= Plot_title, 
       lwd=2, lty=1:strata_number, 
       xlim=XLim, ylim=YLim, xaxt='n', noplot=Excluded,
       xlab="Time", ylab="Current state") 
  axis(1, as.numeric(XTime), XTime)
  text(x=rep(TextX, times= strata_number), 
       y=xy_cord_df_mean[ which(!summary_state_nms %in% Excluded ) ],
       labels=summary_state_nms[ which(!summary_state_nms %in% Excluded ) ], col=Line_Colors, cex=3)
#No legend returned if there is no strata (i.e., unconditional model)
  if(strata_number > 1) {
#    legend(Leg_Loc, legend=Leg_Name, col=1, lty=1:(length(active_plot_states)), cex=2, lwd=2)
    legend(Leg_Loc, legend=Leg_Name, col=1, lty=1:(length(strata_levs)), cex=2, lwd=2)
  }
}


##########################################
## Function to select colors for curves ##
##########################################
fncPrStCurveCol <- function(States, Choices) {
  my_clr <- c(24,552, 498, 652, 254,26, 152,8,32 ,68,75,95,119,139,142,310 ,367 ,450 ,568 ,589)
  my_clr_names <- colors()[my_clr]
  Colors_for_Plots <- my_clr_names[match(Choices[1:length(States)], my_clr_names) ]
  names(Colors_for_Plots) <- States
    return(Colors_for_Plots)
}


##########################################
## Cox type specific multi-state models ##
##########################################

#1. Select the time start variable
output$cph_Time_Start <- renderUI({                                
  selectInput("cphTimeStart", "1. Select 'time start' variable",
              choices = var_ms_df(), multiple=FALSE )
})
#1A. Object with just start time 
cph_time_start <- reactive({
  input$cphTimeStart
})
#2. Select the time stop variable
output$cph_Time_Stop <- renderUI({                                
  selectInput("cphTimeStop", "2. Select 'time stop' variable",
              choices = setdiff(var_ms_df(), cph_time_start()), multiple=FALSE )
})
#2A. Object with just time stop indicator
cph_time_stop <- reactive({
  input$cphTimeStop
})
#3. Select the event variable
output$cph_Time_Event <- renderUI({                                
  selectInput("cphTimeEvent", "3. Select the event variable",
              choices = setdiff(var_ms_df(), c(cph_time_start(), cph_time_stop()) ), multiple=FALSE )
})
#3A. Object with just event indicator
cph_time_event <- reactive({
  input$cphTimeEvent
})
#4. Select the ID variable
output$Cph_Time_Fmla_ID <- renderUI({                                
  selectInput("cphTimeFmlaID", "4. Select the ID variable",
              choices = setdiff(var_ms_df(), c(cph_time_start(), cph_time_stop(), cph_time_event()) ), 
              multiple=FALSE )
}) 
#4A. Object with just ID indicator
cph_time_fmla_id <- reactive({
  input$cphTimeFmlaID
})
#5. Select the predictors.
output$M_S_v_x <- renderUI({
  selectInput("msVX", "5. Select the predictors", 
              choices = setdiff(var_ms_df(), c(cph_time_start(), cph_time_stop(), cph_time_event(), cph_time_fmla_id()) ), multiple=TRUE, 
              selected=setdiff(var_ms_df(), c(cph_time_start(), cph_time_stop(), cph_time_event(), cph_time_fmla_id()) )[1])
})
#5A. Object with just multi-state predictors
ms_v_x <- reactive({
  input$msVX
})
#5B. Make the formula
ms_cph_mdl_fmla <- reactive({
  as.formula(paste(paste0("Surv(",cph_time_start(), ",", cph_time_stop(), ",", cph_time_event(), ")", "~"),   
                   paste(ms_v_x(), collapse= "+")))
})
#6. Revise the formula text
output$MS_CPH_Uf <- renderUI({
  textInput("upMsCphFmla", "6. Update formula (e.g., strata(X) )", 
            value= deparse(ms_cph_mdl_fmla(), width.cutoff=500 ))
})
#6A. Object with updated formula
ms_cph_ufmla <- reactive({
  input$upMsCphFmla
})
#7. Indicate if you should update the model.
output$Update_MS_CPH_Yes <- renderUI({
  selectInput("upMsCphY", "7. Use the updated model formula?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")
})
#7A. Object with updated yes/no
update_ms_cph_y <- reactive({
  input$upMsCphY
})
#7B. Updated formula
ms_cph_mdl_fmla2u <- reactive({ 
  if (length(ms_v_x() ) ==1) {
    as.formula(paste(paste0("Surv(",cph_time_start(), ",", cph_time_stop(), ",", cph_time_event(), ")", "~"),   
                     paste(strsplit(ms_cph_ufmla(), "~")[[1]][2])))
  } else {
    as.formula(paste(paste0("Surv(",cph_time_start(), ",", cph_time_stop(), ",", cph_time_event(), ")", "~"),   
                     paste(strsplit(ms_cph_ufmla(), "~")[[1]][2], collapse= "+")))
  }
})
#8. Transition specific formulas
output$ms_Trn_Spc_Fmla <- renderUI({
  textInput("msTrnSpcFmla", "8. Update transition specific formula", 
            value= ms_trn_spc_formula() )     
})
#8B. Make full regression call
ms_trn_spc_formula <- reactive({
  paste0("coxph(list(",  deparse(ms_cph_mdl_fmla(), width.cutoff=500 ),  ",), data=", input$msDfInputName,", ", "id=", 
         cph_time_fmla_id(), ", model=TRUE" , ")")
})
#8A. Object with formula
ms_trn_spc_fmla <- reactive({
  input$msTrnSpcFmla
})
#9. Indicate if you should use the transition specific model.
output$MS_Trns_Use_Yes <- renderUI({
  selectInput("msTrnsUseYes", "9. Use the transition specific formula?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")
})
#9A. Object with yes/no on using transition specific formula
ms_trns_use_yesno <- reactive({
  input$msTrnsUseYes
})
#10. Determine if we should begin modeling.
output$Begin_MS_Mdl <- renderUI({  
  selectInput("BeginMSMdl", "10. Begin modeling?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")
})
#10A. Object with yes/no on running the model
begin_ms_model <- reactive({
  input$BeginMSMdl
})
#10B. Defines the type of model being used
multi_state_model_type <- reactive({
  if(begin_ms_model() == "Yes") {
    if(ms_trns_use_yesno() == "Yes") {
      "Transition"
    } else {
      "Baseline"
    }
  }
})
#10C. Reactive function that runs the multi-state transition model function 
MS_Trns_Fit <- reactive({
  if(begin_ms_model() == "Yes") {
    if(ms_trns_use_yesno() == "Yes") {
      makeMsTrnsFitfnc(ms_trn_spc_fmla() )
    }
  }
})
#10D. Create the multi state model 
multi_state_model <- reactive({
  if(begin_ms_model() == "Yes") {
    switch(multi_state_model_type(),
           "Baseline"   = if(update_ms_cph_y() == "Yes") {
             coxph(ms_cph_mdl_fmla2u(), data=df_MULTI_STATE(), 
                   id=df_MULTI_STATE()[, which(colnames(df_MULTI_STATE()) == cph_time_fmla_id()) ], model=TRUE)
           } else {
             coxph(ms_cph_mdl_fmla(), data=df_MULTI_STATE(), 
                   id=df_MULTI_STATE()[, which(colnames(df_MULTI_STATE()) == cph_time_fmla_id()) ], model=TRUE)
           },
           "Transition" = if(ms_trns_use_yesno() == "Yes") {
             MS_Trns_Fit()
           }  
    )}
})
#11. Download time dependent data file
output$ms_Cph_Download_Nm <- renderUI({ 
  textInput("msCphDownloadNm", "11. Enter the data frame name", 
            value= "ms_fit")     
})
#12. Setup to download
output$ms_model_download <- downloadHandler(
  filename = "multi_state_fit.RData",
  content = function(con) {
    assign(input$msCphDownloadNm, multi_state_model() )
    save(list=input$msCphDownloadNm, file=con)
  }
)
#13. Create reactive function of X level names 
multi_state_x_level_names <- reactive({
  attributes(multi_state_model()$cmap)$dimnames[[1]][-1]
})
#14. Create reactive function of transition names 
multi_state_transition_names <- reactive({
  attributes(multi_state_model()$cmap)$dimnames[[2]]
})
#15. Regression model output
output$ms_model_fit_out <- renderPrint({
  if(begin_ms_model() == "Yes") {
    list("Model"=multi_state_model(),
         "States"=multi_state_model()$states,
         "Current State Map"=multi_state_model()$cmap,
         "Transitions"=multi_state_model()$transitions,
         "Coefficient's X Levels"= multi_state_x_level_names(),
         "AIC"= AIC(efit1()) )
    #"Coefficient's X Levels"=attributes(multi_state_model()$cmap)$dimnames[[1]][-1])
  }
})


####################################
## Make transition specific model ##
####################################
makeMsTrnsFitfnc <- function(setup) {
  #Sets up script that is saved as a temp file  
  tfsetup <- paste0("ms_model_fit <- ", setup)
  #Sources the temp file to create the data
  msDfsourceFnc(tfsetup)
  return(ms_model_fit)
}

##################################################
##  Schoenfeld residuals for multi-state models ##
##################################################
#1. Decide if we should make Schoenfeld residuals.
output$Begin_MS_Schoenfeld_Res <- renderUI({  
  selectInput("BeginMSchRes", "1. Create Schoenfeld residuals?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")
})
#1A. Object for yes/no on residuals
begin_ms_schoenfeld_residuals <- reactive({
  input$BeginMSchRes
})
#2. Select the variable for the Schoenfeld residuals
output$MSSchoenfeld_X <- renderUI({
  if(begin_ms_model() == "Yes") {
    selectInput("MSschoenfeldx", "2. Select a single Schoenfeld residual", 
              choices = MSschoenfeld_X_names_plot() , multiple=FALSE, selected=MSschoenfeld_X_names_plot()[1])
  }
})
#2A. Object for individual X residual name
ms_schoenfeld_X <- reactive({
  input$MSschoenfeldx
})
#Creates Schoenfeld residuals
MSschoenfeld_e <- reactive({
  if(begin_ms_model() == "Yes") {
    if(begin_ms_schoenfeld_residuals() == "Yes") {
    cox.zph(multi_state_model(), transform='identity', terms=TRUE)
  }
}
})
#This prints the point estimates and confidence intervals
output$MSschoenfeld_test <- renderTable({
  if(begin_ms_model() == "Yes") {
    if(begin_ms_schoenfeld_residuals() == "Yes") {
      MSschoenfeld_e()[[1]]
    }
  }
}, rownames = TRUE)
#This gets the residual names for plots
MSschoenfeld_X_names_plot <- reactive({
  if(begin_ms_model() == "Yes") {
    if(begin_ms_schoenfeld_residuals() == "Yes") {
      rownames(MSschoenfeld_e()[[1]])[-length(rownames(MSschoenfeld_e()[[1]]))]
    }
  }
})
#This plots the Schoenfeld residuals    
output$MSschoenfeld_plt <- renderPlot({
  if(begin_ms_model() == "Yes") {
    if(begin_ms_schoenfeld_residuals() == "Yes") {
  plot(MSschoenfeld_e()[which(MSschoenfeld_X_names_plot()== ms_schoenfeld_X())], col=2, lwd=2)
  abline(h=0, lty=3, col=4, lwd=2)
    }
  }
}, height = 800)


################################################################
## Make a model summary table to use for the state space plot ##
################################################################
#1. Select the summary X levels I want
output$MS_Sum_X_Levs1 <- renderUI({
  if(begin_ms_model() == "Yes") {
    selectInput("msSumXLev1", "1. Select summary X levels", 
                choices = multi_state_x_level_names() , multiple=FALSE, selected=multi_state_x_level_names()[1])
  }
})
#1A. Object with X axis values
ms_summary_x_levels1 <- reactive({
  if(begin_ms_model() == "Yes") {
    input$msSumXLev1
  }
})
#2. Transition names
output$MS_Sum_Trns_Nms1 <- renderUI({  
  if(begin_ms_model() == "Yes") {
    selectInput("msSumTrnNm1", "2. Select specific transitions",
              #choices = multi_state_transition_names(),  multiple=TRUE, selected=multi_state_transition_names()[1])
    choices = multi_state_transition_names(),  multiple=TRUE)
  }
})
#2A. Object with transition names
ms_summary_transition_names1 <- reactive({
  if(begin_ms_model() == "Yes") {
    input$msSumTrnNm1
  }
})
#3. Multiplier value for continuous variables
output$MS_Sum_Multi_Val1 <- renderUI({  
  if(begin_ms_model() == "Yes") {
    numericInput("mSSumMultiVal1", "3. Select continuous X multiplier",
               min=1, step=1, value=1)
  }
})
#3A. Object with multiplier value
ms_summary_multiplier_value1 <- reactive({
  if(begin_ms_model() == "Yes") {
    input$mSSumMultiVal1
  }
})
#Optional 2nd summary
#4. Select the summary X levels I want
output$MS_Sum_X_Levs2 <- renderUI({
  if(begin_ms_model() == "Yes") {
    selectInput("msSumXLev2", "1. Select summary X levels", 
                choices = multi_state_x_level_names() , multiple=FALSE,  
                selected=setdiff(multi_state_x_level_names(), ms_summary_x_levels1())[1]
                )
  }
})
#4A. Object with X axis values
ms_summary_x_levels2 <- reactive({
  if(begin_ms_model() == "Yes") {
    input$msSumXLev2
  }
})
#5. Transition names
output$MS_Sum_Trns_Nms2 <- renderUI({  
  if(begin_ms_model() == "Yes") {
    selectInput("msSumTrnNm2", "2. Select specific transitions",
                #choices = multi_state_transition_names(),  multiple=TRUE, selected=multi_state_transition_names()[1])
    choices = multi_state_transition_names(),  multiple=TRUE)
  }
})
#5A. Object with transition names
ms_summary_transition_names2 <- reactive({
  if(begin_ms_model() == "Yes") {
    input$msSumTrnNm2
  }
})
#6. Multiplier value for continuous variables
output$MS_Sum_Multi_Val2 <- renderUI({  
  if(begin_ms_model() == "Yes") {
    numericInput("mSSumMultiVal2", "3. Select continuous X multiplier",
                 min=1, step=1, value=1)
  }
})
#6A. Object with multiplier value
ms_summary_multiplier_value2 <- reactive({
  if(begin_ms_model() == "Yes") {
    input$mSSumMultiVal2
  }
})
#7. Requesting two levels.
output$Sum_2_X_Lev_Yes <- renderUI({  
  if(begin_ms_model() == "Yes") {
    selectInput("Sum2XLevY", "7. Did you enter two X-levels?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")
  }
})
#7A. Object for requesting two levels.
summarize_2_x_levels_yes <- reactive({
  if(begin_ms_model() == "Yes") {
    input$Sum2XLevY 
  }
})
#8. Run the model summary
output$Begin_MS_Sum_Yes <- renderUI({  
  if(begin_ms_model() == "Yes") {
    selectInput("BeginMSSumY", "8. Run the model summary?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")
  }
})
#8A. Object for requesting two levels.
begin_ms_summary_yes <- reactive({
  if(begin_ms_model() == "Yes") {
    input$BeginMSSumY 
  }
})
#9. Reactive function that runs the model summary function.
run_ms_model_summary <- reactive({
    if(begin_ms_summary_yes() == "Yes") {
      if(summarize_2_x_levels_yes() == "Yes") {
        fncMdlXLevSmry(model=multi_state_model(), 
                   trans= list(ms_summary_transition_names1(), ms_summary_transition_names2()), 
                   main= ms_summary_x_levels1(), 
                   second= ms_summary_x_levels2(), 
                   multiplier=c(ms_summary_multiplier_value1(), ms_summary_multiplier_value2()), round_lev=2)  
      } else {
        mdlSmryFnc(model=multi_state_model(), 
                   trans=ms_summary_transition_names1(), 
                   main=ms_summary_x_levels1(), 
                   multiplier=ms_summary_multiplier_value1(), round_lev=2) 
    }
  }
})
#10. Print results
output$ms_model_summary_out <- renderPrint({ 
  run_ms_model_summary()
})

######################################
## Function for model summary table ##
######################################
mdlSmryFnc <- function(model, trans=NULL, main=NULL, multiplier=1, round_lev=2) {
  #Transition names
  cmap_tbl <- model$cmap
  coxph_row_nm <- rownames(cmap_tbl)
  coxph_trans <- colnames(cmap_tbl)
  #Creates the CMAP names that removes the strata part of the name
  cmap_nms <- list()
  for(i in 1:length(coxph_row_nm)) {
    cmap_nms[[i]] <- strsplit(strsplit(coxph_row_nm[[i]], c("\\("), fixed=F)[[1]][2], c("\\)"), fixed=F)[[1]][1]
  }
  #Replaces NAs found in non-strata variables with the proper name
  for(i in 1:length(coxph_row_nm)) {
    if(is.na(cmap_nms[[i]])) {
      cmap_nms[[i]] <- coxph_row_nm[[i]]
    }
  }
  #Change the names in the cmap table
  rownames(cmap_tbl) <- cmap_nms 
  # "main2" Identifies the target variable used for the "key" and "X" part of the function output 
  if (is.null(main)) {
    main2 <- which(rownames(cmap_tbl) != "Baseline")[1]
  } else {
    main2 <- which(rownames(cmap_tbl) == main)
  }
  #Key transitions from function input or all transitions by default
  if (is.null(trans)) {
    key_trans <- coxph_trans
  } else {
    key_trans <- trans
  }
  #Element names and numbers of key transitions
  trans_nms <- which(coxph_trans %in% key_trans)
  coef_trans <- cmap_tbl[main2, which(coxph_trans %in% key_trans)]
  
  #Identify the main variable and associated rows from the coefficient table
  if (is.null(main)) {
    main_var_coef <- unlist(cmap_tbl[rownames(cmap_tbl) != "Baseline", drop=F, ][1,])
  } else {
    main_var_coef <- unlist(cmap_tbl[rownames(cmap_tbl)== main, ])
  }
  #Model summary output
  mdl_out <- summary(model)$coefficients
  #Pull out the exponentiated coefficients
  #exp_coef <- mdl_out[main_var_coef, 2]
  exp_coef <- mdl_out[main_var_coef, which(colnames(mdl_out) =="exp(coef)")]
  #Pull out the coefficient p-values
  #pval_trans <-mdl_out[main_var_coef, 6]
  pval_trans <-mdl_out[main_var_coef, which(colnames(mdl_out) =="Pr(>|z|)")]
  #Convert p-values to *
  pval_symbol <- vector( length=length(pval_trans))
  pval_symbol[pval_trans >= .1]  <- ""
  pval_symbol[pval_trans < .1]   <- "+"
  pval_symbol[pval_trans < .05]  <- "*"
  pval_symbol[pval_trans < .01]  <- "**"
  pval_symbol[pval_trans < .001] <- "***"
  #Get the summary of the transitions for the plot text
  smry_trans <- paste0(round(exp_coef[trans_nms]^multiplier, round_lev), pval_symbol[trans_nms])
  #This will be used to paste the 2 states together for table headings
  smry_result <- vector(length=length(trans_nms))
  #Vector to store the transition names from:to
  sts <- colnames(model$cmap)[trans_nms]
  #For loop that creates the transition names
  for (i in 1:length(trans_nms)) {
    smry_result[i] <- paste0(model$states[as.numeric(unlist(strsplit(sts[i], ":"))[1])], 
                             " -> ",
                             model$states[as.numeric(unlist(strsplit(sts[i], ":"))[2])])
  }
  #Get the summary table
  trans_out <- as.data.frame(cbind("Transition"=smry_result, "HR"=exp_coef[trans_nms], 
                                   "p.value"= pval_trans[trans_nms]))
  #Name the summary coefficients and p-value symbols vector elements
  names(smry_trans) <- smry_result
  #Returns the output table and the transition summary needed for the plot text
  return(list(output=trans_out , smry=smry_trans, key=cmap_tbl[main2, which(coxph_trans %in% key_trans)], 
              X= rownames(cmap_tbl)[main2]))
}

####################################################
## Function to make MS output for multiple levels ##
####################################################
fncMdlXLevSmry <- function(model, trans=NULL, main=NULL, second=NULL, multiplier=1, round_lev=2) {
  XLevs=c(main, second)
  rslt <- list()
  for (i in 1:length(XLevs)) {
    rslt[[i]] <- mdlSmryFnc(model=model, trans=trans[[i]], main=XLevs[i],
                            multiplier=multiplier[i], round_lev=round_lev)
  }
  for (i in 1:length(XLevs)) {
    names(rslt)[i] <- XLevs[[i]]
  }
  return(rslt)
}

#########################
## State space diagram ##
#########################
#1. Select colors for probability-in-state curves
output$State_Spc_Color <- renderUI({  
  selectInput("stateSpcClrs", "1. Select colors for states and lines", 
              choices = colors()[c(24,552, 498, 652, 254,26, 152,8, 32,68,
                                   75,95,119,139,142,310 ,367 ,450 ,568 ,589)], multiple=TRUE)     
})
#1A. Object for curve colors 
state_space_colors <- reactive({
  input$stateSpcClrs
})
#2. Revise plot layout
output$ms_St_Spc_Layout <- renderUI({
  textInput("msStSpcLyt", "2. New plot layout", 
            value= state_build_txt_input() )     
})
#2C. Object with the standard diagram box setup
state_build_txt_input <- reactive({
  fncStateBuildTxtInput(multi_state_model()$states)
})
#2A. Object with new layout
ms_state_space_layout <- reactive({
  input$msStSpcLyt
})
#3. Modifies the plot layout.
output$MS_Mod_St_Spc_Layout <- renderUI({  
  selectInput("msModStSpcLyt", "3. Want to modify plot layout?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")
})
#3A. Object with yes/no on modifying the layout
ms_modify_state_space_layout <- reactive({
  input$msModStSpcLyt
})
#4. Legend location
output$St_Sp_Legend <- renderUI({  
  selectInput("StSpLgnd", "4. Select the legend location",
              choices = c("bottomright", "bottom", "bottomleft", "left", 
                          "topleft", "top", "topright", "right", "center"),  multiple=FALSE)
})
#4A. Object with Legend location
state_space_legend <- reactive({
  input$StSpLgnd
})
#5. Modifies the plot layout.
output$Make_St_Spc_Diag <- renderUI({  
  selectInput("makeStSpcDiag", "5. Create state space figure?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")
})
#5A. Object with yes/no on creating the state space diagram
ms_make_state_space_diagram <- reactive({
  input$makeStSpcDiag
})
#5B. Make state space diagram
#Make_State_Space_Plot <- reactive({
#  if(begin_ms_model() == "Yes") {
#    if( ms_make_state_space_diagram() == "Yes") {
#      fncStateSpacePlot(fit=multi_state_model(), fit_smry=run_ms_model_summary(), 
#   Choices=state_space_colors(), Build=ms_make_state_space_diagram(), 
#   NewBuild= eval(parse(text=ms_state_space_layout() )), lgn_loc=state_space_legend() )  
#    }
#   }
#})
#5C. MS state space plot    
output$ms_state_space_plot <- renderPlot({
  if(begin_ms_model() == "Yes") {
    if( ms_make_state_space_diagram() == "Yes") {
      #Make_State_Space_Plot()
      fncStateSpacePlot(fit=multi_state_model(), fit_smry=run_ms_model_summary(), 
                        Choices=state_space_colors(), Build=ms_make_state_space_diagram(), 
                        NewBuild= eval(parse(text=ms_state_space_layout() )), lgn_loc=state_space_legend() ) 
    }
  }

  ## Interactive shiny plot
#  points( source_coords$xy[1], source_coords$xy[2], cex=3, pch=NA)
  points( source_coords$xy[1], source_coords$xy[1], cex=3, pch=NA)
  ## Destination
  text(dest_coords$x, dest_coords$y, DistCost()$Lost , cex=2)
}, height = 800)
#5D. Creates the state space text values 
Make_State_Space_Txt_Vals <- reactive({
  if(begin_ms_model() == "Yes") {
    if( ms_make_state_space_diagram() == "Yes") {
      fncMSStSpcTxt(mdl_smr= run_ms_model_summary())
    }
  }
})
  
########################################################################
## Interactive state space values for plotting witht the click option ##
########################################################################
#Point click options outside of plot so it'll be clean if I just want the plot
  ms_State_Space_Initial_X <- -.1
  ms_State_Space_Initial_Y <- -.1
  
  ## Source Locations (Home Base)
  source_coords <- reactiveValues(xy=c(x=ms_State_Space_Initial_X, y=ms_State_Space_Initial_Y) )
  
  ## Dest Coords
  dest_coords <- reactiveValues(x=ms_State_Space_Initial_X, y=ms_State_Space_Initial_Y)
  observeEvent(plot_click_slow(), {
    dest_coords$x <- c(dest_coords$x, plot_click_slow()$x)
    dest_coords$y <- c(dest_coords$y, plot_click_slow()$y)
  })
  
  ## Don't fire off the plot click too often
  plot_click_slow <- debounce(reactive(input$plot_click), 300)
  
  #My values
  DistCost <- reactive({
    num_points <- length(dest_coords$x)
    #Coefficient values
    list( Lost= Make_State_Space_Txt_Vals() )
  })
  
################  

#########################################################################
## Function that creates the matrix for the arrows in the state figure ##
#########################################################################
fncStateCnct <- function(what, fit, fit_smry,  ...) { 
  nmbr_st <- length(fit$states)
  sname <- c("Entry", fit$states[-1])
  ls_length <- length(fit_smry)
  connect <- matrix(0, nmbr_st, nmbr_st, dimnames=list(sname, sname)) 
  if(ls_length==4) {
    mat_coords_txt <- strsplit(names(fit_smry[[3]]), ":")
  } else {
    mat_coords_txt <- strsplit(names(fit_smry[[1]][[3]]), ":")
  }
  mat_coords_X <- as.numeric(unlist(lapply(mat_coords_txt, `[[`, 1)))  #Grabs 1st element
  mat_coords_Y <- as.numeric(unlist(lapply(mat_coords_txt, `[[`, 2)))  #Grabs 2nd element
  list_lngth <- length(mat_coords_txt)
  for(i in 1:list_lngth) {
    connect[mat_coords_X[i], mat_coords_Y[i]] <- 1
  }
  return(connect)
}

########################################################
## Function that gets number of arrows for each state ##
########################################################
fncStateArrow <- function(connect_matrix) {
  #Gets the number of values greater than 0 
  grt_thn_0 <- function(x) sum(x > 0)
  #Runs the function above for each state
  apply(connect_matrix, 2, grt_thn_0) 
}

################################################
## Function that replicates the color numbers ##
################################################
fncStateColArrow <- function(arrow_count, box_col) {
  arw_col <- rep(box_col, arrow_count)
}

#######################################################
## Function that gets the standard diagram box setup ##
#######################################################
fncStateBuild <- function(connect_matrix) {
  st_bld <- c(1, length(rownames(connect_matrix))-2, 1)
  return(st_bld)
}
  #2B. Function to get quick state build
  fncStateBuildTxtInput <- function(states_in_fit) {
    st_bld <- c(1, length(states_in_fit)-2, 1)
    st_bld <- paste0("c(",toString(st_bld), ")" )
    return(st_bld)
  }
  
################################################
## Function to get state space diagram values ##
################################################
#Number of summary transitions
fncNmbSmrTrn <- function(mdl_smr) {
  nmbr_sum_trn <- length(mdl_smr[[1]][[3]])
}

#Number of summary coefficients
fncNmbSmrX <- function(mdl_smr) {
  nmbr_sum_X <- length(mdl_smr)
}

################################################
## Function to get state space diagram values ##
################################################
fncMSStSpcSmry <- function(mdl_smr) {
  nx <- fncNmbSmrX(mdl_smr)
  #Number of predictors in state space diagram 
  ms_nmbr_state_fig_x <- length(mdl_smr)
  state_space_smry1 <- mdl_smr[[1]][["smry"]]
  #For loop to get values for 1 or 2 coefficients
  if(nx==2) {
    for(i in 1:nx) {
      #Get primary summary coefficients 
      state_space_smry2 <- paste0("(",mdl_smr[[2]][["smry"]],")")
      names(state_space_smry2) <- names(mdl_smr[[2]][["smry"]])
      State_Space_Summary <- list(state_space_smry1=state_space_smry1, state_space_smry2=state_space_smry2)
    }
  } else {
    State_Space_Summary <- list(state_space_smry1)
  }
  return(State_Space_Summary)
}

################################################
## Function to get state space Text values ##
################################################
fncMSStSpcTxt <- function(mdl_smr) {
  nx <- fncNmbSmrX(mdl_smr)
  #Number of predictors in state space diagram 
  ms_nmbr_state_fig_x <- length(mdl_smr)
  #For loop to get values for 1 or 2 coefficients
  if(nx==2) {
    for(i in 1:nx) {
      #Get primary summary coefficients 
      state_space_smry1 <- mdl_smr[[1]][["smry"]]
      state_space_smry2 <- paste0("(",mdl_smr[[2]][["smry"]],")")
      names(state_space_smry2) <- names(mdl_smr[[2]][["smry"]])
      State_Space_Summary <- c(state_space_smry1, state_space_smry2)
    }
  } else {
    State_Space_Summary <- mdl_smr[["smry"]]
  }
  State_Space_Summary <- c(" ", State_Space_Summary)
  return(State_Space_Summary)
}

############################################
## Function to create state space diagram ##
############################################
fncStateSpacePlot <- function(fit, fit_smry, Choices, Build="No", NewBuild, lgn_loc
                              ) {
  States <-  c("Entry", fit$states[-1])
  #Get objects from various functions
  connect_mat <- fncStateCnct(fit=fit, fit_smry=fit_smry)
  connect_mat <- edit(connect_mat)
  cnt_arws <- fncStateArrow(connect_mat)
  ms_clr <- fncPrStCurveCol(States, Choices)
  arow_colr <- fncStateColArrow(cnt_arws, ms_clr)
  if(Build== "Yes") {
    state_build <- NewBuild
  } 
  #Therneau's state space figure
  statefig(matrix(state_build), connect_mat, cex=1.5,
           acol=arow_colr, #Arrows point in order of columns, see matrix 
           bcol=ms_clr, #In order of column names
           lwd=2.5) 
  ###########
    nx <- fncNmbSmrX(mdl_smr=fit_smry)
    if(nx==2) {
      #Text labels for secondary coefficient2
#      legend(lgn_loc, legend= c(paste0("State <- ", toupper(abbreviate(fit_smry[[1]][4], minlength=6))), 
#                                paste0("State <- ( ", toupper(abbreviate(fit_smry[[2]][4], minlength=6)), " )") ), 
      legend(lgn_loc, legend= c(paste0("State <- ", abbreviate(fncStSpcLegendFactoLev(fit, fit_smry[[1]][4]), minlength=10)), 
                                paste0("State <- ( ", abbreviate(fncStSpcLegendFactoLev(fit, fit_smry[[2]][4]), minlength=10), " )") ), 
             bty="n", cex=2)
    } else {
#      legend(lgn_loc, legend= paste0("State <- ", toupper(abbreviate(fit_smry[4], minlength=6))), 
      legend(lgn_loc, legend= paste0("State <- ", abbreviate(fncStSpcLegendFactoLev(fit, fit_smry[4]), minlength=10)), 
             bty="n", cex=2)
    }
  }

############################################
## Function to get text label coordinates ##   
############################################
#1. Primary coefficient  
fncStSpcTxtXY1 <- function(mdl_smr) {
  st_spc_smry_ls <- fncMSStSpcSmry(mdl_smr=mdl_smr)
  
  #Get primary X/Y coordinates 
  txt_lbl_crds1 <- locator(n= length(st_spc_smry_ls[[1]]))
  return(txt_lbl_crds1)
}

#2. Secondary coefficient  
fncStSpcTxtXY2 <- function(mdl_smr) {
  st_spc_smry_ls <- fncMSStSpcSmry(mdl_smr=mdl_smr)
  #Run locator
  if(length(state_smry_ls) ==2) {
    txt_lbl_crds2 <- locator(n= length(st_spc_smry_ls[[2]]))
  } else {
    txt_lbl_crds2 <- NULL
  }
  return(txt_lbl_crds2)
}

##########################################################################
## Function that removes factor names from factor levels for the legend ##
##########################################################################
#For example "Age_group65+" -> "65+"
fncStSpcLegendFactoLev <- function(Model_fit, X_Lev) {
  #Get the separate parts of the model call so I can get all X
  mdl_cl <- names(Model_fit[["model"]])
  #Get variable names from the model
  mdl_var <- mdl_cl[-(grep("\\(", mdl_cl))]
  #Determine whether each X is the level we want or not
  tvec <- vector()
  for(i in 1:length(mdl_var)) {
    if(grepl(mdl_var[i], X_Lev) == TRUE ) {
      tvec[i] <- TRUE
    } else {
      tvec[i] <- FALSE
    }
  }
  #Indicates the correct variable name to check for
  sub_nm <- mdl_var[tvec]
  #Creates the coefficient level to use in the legend
  output_val <- vector()
  if(nchar( sub(sub_nm, "", X_Lev) ) ==0 ) {
    output_val[1] <- sub_nm
  } else {
    output_val[1] <-  sub(sub_nm, "", X_Lev)
  }
  return(Level=output_val)
}

###########################################################
## Function to create state space text values and legend ##
###########################################################
#fncStSpcTextLgd <- function(mdls, Model_fit, tc1, tc2=NULL, lgn_loc) {
#  #State space summary
#  st_spc_smry_ls <- fncMSStSpcSmry(mdl_smr=mdls)
#  #Get basic values
#  nt <- fncNmbSmrTrn(mdl_smr=mdls)
#  nx <- fncNmbSmrX(mdl_smr=mdls)
#  #Text labels for primary coefficient2
#  if(nx==2) {
#    #Text labels for secondary coefficient2
#    for(i in 1:nt) {
#      text(tc1$x[i], tc1$y[i], st_spc_smry_ls[[1]][i], cex=1.5)
#      text(tc2$x[i], tc2$y[i], st_spc_smry_ls[[2]][i], cex=1.5)
#      legend(lgn_loc, legend= c(paste0("State <- ",   abbreviate(fncStSpcLegendFactoLev(Model_fit, mdls[[1]][4]), minlength=10)), 
#                                paste0("State <- ( ", abbreviate(fncStSpcLegendFactoLev(Model_fit, mdls[[2]][4]), minlength=10), " )") ), 
#             #paste0("State <- ( ", abbreviate(mdls[[2]][4], minlength=6), " )") ), 
#             bty="n", cex=1.33)
#      
#    }
#  } else {
#    for(i in 1:nt) {
#      text(tc1$x[i], tc1$y[i], st_spc_smry_ls[[1]][i], cex=1.5)  
#      legend(lgn_loc, legend= paste0("State <- ", abbreviate(fncStSpcLegendFactoLev(Model_fit, mdls[[1]][4]), minlength=10)), 
#      #legend(lgn_loc, legend= paste0("State <- ", abbreviate(mdls[[1]][4], minlength=10)), 
#                    bty="n", cex=1.33)
#    }
#  }
#}


################################################################################

################################################################################
## Testing section: Begin  ##
################################################################################
#output$testplot1 <- renderPlot({ 
##  plot(values$a, values$b)
##} )

#output$test1 <- renderPrint({
#list(
#  vls1=vls1(), 
#  vls2=vls2(), input_mc_df2=head(input_mc_df2()[[1]]),
#  mc_df_y2=mc_df_y2(),
#  str_input_mc_df2= str(input_mc_df2()),
#  name_input_mc_df2= names(input_mc_df2()), class_input_mc_df2= class(input_mc_df2()),
#  length_input_mc_df2= length(input_mc_df2()), mc_unif_df=mc_unif_df(),
#  mc_df_y=head(mc_df_y()), 
#    mc_df1= head(mc_df1()),  "mc_sim_fnc1"= mc_sim_fnc1() 
#)  

#  })


################################################################################
## Testing section: End ##
################################################################################

  })   #This is the last line of code that closes out the entire server file
  
