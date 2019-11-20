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
data(lungcancer)
options(shiny.maxRequestSize=1000*1024^2)    #This will increase the shiny file upload limit from current 5MB max

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
      str(newdf())  
    })
    
    #Don't need this for "Data" tab
    output$new_smry_df <- renderPrint({
      new_smry_df()
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
      selectInput("ModifyNumeric", "3. Select variables to convert to a 'character'.", 
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
    
    mdl_fmla <- reactive({             #Spline terms 
      as.formula(paste(paste0(input$variableY , "~"),   
                       paste(mdl_vnms(), collapse= "+")))
    })

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
      as.formula(paste(paste0("Surv(",input$variableY, ",", censor1(), ")", "~"),   
                       paste(strsplit(input$up_fmla, "~")[[1]][2], collapse= "+")))
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
             "Logistic" = plot_yhat <- 1/(1+exp(-yhat)),
#             "Ordinal Logistic"          = plot_yhat <- 1/(1+exp(-yhat)),
             "Ordinal Logistic"          = plot_yhat <- yhat,
             "Poisson"  = plot_yhat <- exp(yhat),
             "Quantile" = plot_yhat <- yhat,
#             "Cox PH"   = plot_yhat <- 1/(1+exp(-yhat)),
             "Cox PH"   = plot_yhat <- yhat,
#"Cox PH with censoring"     = plot_yhat <- 1/(1+exp(-yhat)),
             "Cox PH with censoring"     = plot_yhat <- yhat,
             "Generalized Least Squares" = plot_yhat <- yhat )
      return(plot_yhat)
    }
    
    #This reactive function runs the yhat_plot_fnc function above  
    yhat_hist_rslt <- reactive ({
      yhat_hist_plot_fnc(yhat=predict(fit1()), reg_yhat=input$regress_type)
    })
    
    
    output$y_hat_hist <- renderPlot({
#        hist(predict(fit1()), main="Histogram of the predicted Y values",
#             xlab=paste0(input$variableY))
      hist(yhat_hist_rslt(), main="Histogram of the predicted Y values",
                        xlab=paste0(input$variableY, " (range: ", round(min(yhat_hist_rslt(), na.rm=T),3), " to ", round(max(yhat_hist_rslt(), na.rm=T), 3), ". Unique values= ", length(unique(yhat_hist_rslt())) ,".)" ))
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
    aprx_mdl_fmla <- reactive({             #Spline terms 
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
          plot(r2, r2, type='p', col= "red", cex=1.5, xlim= c(min(r2)*0.98, max(r2)*1.02), ylim= c(min(r2)*0.98, max(r2)*1.02), 
               main="Approximate model predictability after deletion of N predictors",
               xlab=expression(paste('Approximation ', R^2)),
               ylab=expression(paste('Approximation ', R^2)))
          text(r2, r2*.98, 1:length(r2))
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
               ols(as.formula(input$up_fmla), x=TRUE, y=TRUE) 
             } else {
               ols(mdl_fmla(), x=TRUE, y=TRUE)}, 
             "Logistic" = if(input$updy == "Yes") {
               lrm(as.formula(input$up_fmla), x=TRUE, y=TRUE, tol=1e-100, maxit=20) #I added tol value so it can handle time predictor (YYMM)
             } else {                                                               #Added maxit to handle small samples. See https://stat.ethz.ch/pipermail/r-help/2017-September/449115.html
               lrm(mdl_fmla(), x=TRUE, y=TRUE, tol=1e-100, maxit=20)},  #I added tol value so it can handle time predictor that causes "singularity"
             "Ordinal Logistic" = if(input$updy == "Yes") {
               orm(as.formula(input$up_fmla), x=TRUE, y=TRUE) 
             } else {
               orm(mdl_fmla(), x=TRUE, y=TRUE)},
             "Poisson" = if(input$updy == "Yes") {
               Glm(as.formula(input$up_fmla), x=TRUE, y=TRUE, family=poisson()) 
             } else {
               Glm(mdl_fmla(), x=TRUE, y=TRUE, family=poisson())},
             "Quantile" = if(input$updy == "Yes") {
               Rq(as.formula(input$up_fmla), x=TRUE, y=TRUE, tau=as.numeric(rq_tau1())) 
             } else {
               Rq(mdl_fmla(), x=TRUE, y=TRUE,  tau=as.numeric(rq_tau1()))},
             "Cox PH"   = if(input$updy == "Yes") {
               cph(cox_mdl_fmla1u(), x=TRUE, y=TRUE, surv=TRUE) 
             } else {
               cph(cox_mdl_fmla1(), x=TRUE, y=TRUE, surv=TRUE)},
             "Cox PH with censoring" = if(input$updy == "Yes") {
               cph(cox_mdl_fmla2u(), x=TRUE, y=TRUE, surv=TRUE) 
             } else {
               cph(cox_mdl_fmla2(), x=TRUE, y=TRUE, surv=TRUE)},
             "Generalized Least Squares" = if(input$updy == "Yes") {
               Gls(as.formula(input$up_fmla), x=TRUE, correlation=corCAR1(form= gls_cor()))
             } else {
               Gls(mdl_fmla(), x=TRUE, 
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
                   fit.mult.impute(cox_mdl_fmla1u(), cph, mi(), data=df(), pr=FALSE) 
                 } else {
                   fit.mult.impute(cox_mdl_fmla1(), cph, mi(), data=df(), pr=FALSE)}, 
                 "Cox PH with censoring" = if(input$updy == "Yes") {
                   fit.mult.impute(cox_mdl_fmla2u(), cph, mi(), data=df(), pr=FALSE) 
                 } else {
                   fit.mult.impute(cox_mdl_fmla2(), cph, mi(), data=df(), pr=FALSE)}, 
                 "Generalized Least Squares" = if(input$updy == "Yes") {
                   fit.mult.impute(cox_mdl_fmla2u(), Gls, mi(), data=df(), pr=FALSE, correlation=corCAR1(form= gls_cor())) 
                 } else {
                   fit.mult.impute(cox_mdl_fmla2(), Gls, mi(), data=df(), pr=FALSE, correlation=corCAR1(form= gls_cor()))}
          )
        }
        
    })
        

    rq_tau1 <- reactive({                  #This stores the censoring variable. 
      input$tau1
    })
    
    censor1 <- reactive({                  #This stores the censoring variable. 
      input$censor
    })
    
    gls_clst1 <- reactive({                  #This stores the censoring variable. 
      input$gls_clst
    })
    
    dfx1 <- reactive({                  #This stores simulated data for one predictor.
      set.seed(input$set_seed)
      switch(input$dist_type,                #"var" and can be used anywhere in server.r.
             "Binomial"  = mean(do.call("rbinom", list(n=input$n_sim, size=input$trials_x, prob=input$prob_x))),
             "Normal"    = mean(do.call("rnorm", list(n=input$n_sim, mean=input$mean_x, sd=input$std_x))),
             "Poisson"   = mean(do.call("rpois", list(n=input$n_sim, mean=input$mean_x))),
#             "Triangle"  = mean(do.call("rpois", list(n=input$n_sim, mean=input$mean_x))),
             "Uniform"   = mean(do.call("runif", list(n=input$n_sim, min=round(input$std_x), max=round(input$mean_x)))))  
    })     

    #Function to download predicted scores (yhat) and row names from a model based on the type of regression
    #REMOVING THIS BECAUSE I NOW HAVE THE PRED TAB
#    dyhat_fnc <- function(fit1, reg_yhat) {
#      switch(reg_yhat,                
#             "Linear"   = row_yhat_df <- data.frame(rowName=as.numeric(as.character(names(fit1[["linear.predictors"]]))), yhat=fit1[["linear.predictors"]]), 
#             "Logistic" = row_yhat_df <- data.frame(rowName=as.numeric(as.character(names(fit1[["linear.predictors"]]))), yhat=fit1[["linear.predictors"]]),
#             "Ordinal Logistic"          = row_yhat_df <- data.frame(rowName=as.numeric(as.character(names(fit1[["linear.predictors"]]))), yhat=fit1[["linear.predictors"]]),
#             "Poisson"  = row_yhat_df <- data.frame(rowName=as.numeric(as.character(names(fit1[["linear.predictors"]]))), yhat=fit1[["linear.predictors"]]),
#             "Quantile" = row_yhat_df <- data.frame(rowName=as.numeric(as.character(rownames(fit1$fitted.values))), yhat=fit1$fitted.values),
#             "Cox PH"   = row_yhat_df <- data.frame(rowName=as.numeric(as.character(names(fit1[["linear.predictors"]]))), yhat=fit1[["linear.predictors"]]),
#             "Cox PH with censoring"     = row_yhat_df <- data.frame(rowName=as.numeric(as.character(names(fit1[["linear.predictors"]]))), yhat=fit1[["linear.predictors"]]),
#             "Generalized Least Squares" = row_yhat_df <- data.frame(rowName=as.numeric(as.character(names(fit1[["fitted"]]))), yhat=fit1[["fitted"]])) 
#      return(row_yhat_df)
#    }
    
    #Reactive function that runs the dyhat_fnc function above
    #REMOVING THIS BECAUSE I NOW HAVE THE PRED TAB
#    dyhat_df <- reactive({
#      dyhat_fnc(fit1(), input$regress_type)
#    })     
    

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
                              "Quantile",
                              "Cox PH",
                              "Cox PH with censoring",
                              "Generalized Least Squares"),
                  selected="Linear", multiple=FALSE)
    })
    
    #Indicate the quantile/percentile value to use in quantile regression
    output$quant_tau <- renderUI({                                 #Same idea as output$vy
      numericInput("tau1", "6. Quantile level for quantile regression",
                value=0.50, min=0, max=1, step=.05)     #Will make choices based on my reactive function.
    })
    
    #Indicate the censoring variable.
    output$censoring <- renderUI({                                 #Same idea as output$vy
      textInput("censor", "7. Censoring variable for Cox PH model")     #Will make choices based on my reactive function.
    })
    
    #Indicate the clustering structure.
    output$clustering <- renderUI({                                 #Same idea as output$vy
      selectInput("gls_clst", "8. GLS clustering: List level 1 and 2 variables (in order)",     #Will make choices based on my reactive function.
      choices = gls_lev1_2(), multiple=TRUE, selected=gls_lev1_2()[1:2])
    })

    #Indicate if there should be splines.
    output$rcs_yes <- renderUI({                                 #Same idea as output$vy
      selectInput("rcsy", "9. Do you want splines?", 
                  choices = c("No", "Yes"), multiple=FALSE, selected="No")     #Will make choices based on my reactive function.
    })
    
    #Select the variables that will have splines.
    output$rx <- renderUI({                                 #Same idea as output$vy
      selectInput("rcs_X", "10. Select the spline variables (continuous only)", 
                  choices = predictor(), multiple=TRUE, selected=predictor()[1])     #Will make choices based on my reactive function.
    })

    #Indicate if you should update the model.
    output$update_yes <- renderUI({                                 #Same idea as output$vy
      selectInput("updy", "11. Do you want to update the model formula?", 
                  choices = c("No", "Yes"), multiple=FALSE, selected="No")     #Will make choices based on my reactive function.
    })
    
    #Update the model formula.
    output$uf <- renderUI({                                 #Same idea as output$vy
      textInput("up_fmla", "12. Update formula (Interactions: Change \"+\" to \'*\', strat(X) to stratify CPH)", 
                value= deparse(mdl_fmla(), width.cutoff=500 ))     #Will make choices based on my reactive function.
    })
    
    #Download the model data and predictions--REMOVING BECAUSE I HAVE THE PRED TAB
#    observe({
#      if(input$dmdfyhat == "Yes")
#      {
#        sink("model_yhat.txt", append=F)
#        write.table(dyhat_df(), row.names=F, sep="\t")
#        sink()
#      }
#    })
    
    #Regression results.
    output$regress <- renderPrint({                                                 
      atch()
      if( input$regress_type %in% c("Logistic", "Ordinal Logistic", "Poisson", "Cox PH", "Cox PH with censoring")) {
        print( list("Model"=fit1(), "Exponentiated Coefficients"= exp(fit1()[["coefficients"]])) )
      } else {
        print(fit1())  #Summary of model fit.
      }
    })
    #Model predicted Y regression equation
    output$regress_equation <- renderPrint({                                                 
      options(scipen=20)
      print(Function(fit1()))
    })
    
    
    describeY <- reactive({                  #This stores the censoring variable. 
      print(describe(as.numeric(df()[, input$variableY], na.rm=TRUE), #Summary of Y variable.
                     descript=paste0("Summary of ",input$variableY) ))  
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
      print( specs(fit1(), long=TRUE))  #Summary of model fit.
    })
    
    output$desc_Y <- renderPrint({                                                 
      print(describeY())
    })
    
    ####  DO I NEED THIS CODE (DFX1)?????? ###
    #Test for simulated data
    output$test <- renderPrint({
      dfx1()      
    })
    
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
        plot(summary(fit1()))
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

## Creates a plot for an interaction of continuous X by a factor ##
#Select the continuous predictor
    output$xyplot_x <- renderUI({
      selectInput("XyplotX", "1. Select a continuous predictor.", 
                   choices = predictor(), selected=predictor()[1], multiple=FALSE)     
    })
    #Select the factor for grouping
    output$xyplot_z <- renderUI({
      selectInput("XyplotZ", "2. Select a factor.", 
                  choices = setdiff(predictor(), input$XyplotX),  multiple=FALSE)     
    })
    #Make confidence bands for the plot
    output$xyplot_bands <- renderUI({                                 
      selectInput("XyplotBands", "3. Do you want confidence bands?", 
                  choices = c("No", "Yes"), multiple=FALSE, selected="No")
    })
    #Create yes/no box to make the XY plot
    output$xyplot_yes_no <- renderUI({                                 
      selectInput("XyplotYesNo", "4. Do you want to create the plot?", 
                  choices = c("No", "Yes"), multiple=FALSE, selected="No")     
    })
    
    #Continuous predictor
    XyplotX1 <- reactive({                  
      input$XyplotX
    })
    #Factor
    XyplotZ1 <- reactive({                  
      input$XyplotZ
    })
    #Get the Predicted values needed for the plot
    xyplotData <- reactive({                 
      do.call("Predict", list(fit1(), XyplotX1(), XyplotZ1() )   ) 
    })

#This creates the plot
    output$xYplot_interaction <- renderPlot({
      if(input$XyplotYesNo == "Yes") {
        if(input$XyplotBands == "Yes") {
          xYplot( Cbind(xyplotData()[[which(names(xyplotData()) =="yhat")]], 
                        xyplotData()[[which(names(xyplotData()) =="lower")]], 
                        xyplotData()[[which(names(xyplotData()) =="upper")]]) ~ xyplotData()[[which(names(xyplotData()) ==XyplotX1())]],  
                  groups=xyplotData()[[which(names(xyplotData()) ==XyplotZ1())]],
               method='filled bands', type='l', 
               col.fill=adjustcolor(1:length(unique(df()[, XyplotZ1() ])), alpha.f = 0.2),
               lty=1:length(unique(df()[, XyplotZ1() ])),
               lcol=1:length(unique(df()[, XyplotZ1() ])), 
                lwd=2, ylab="Yhat", xlab=XyplotX1(), cex=1.75,
               main=paste0("Partial prediction plot of ", XyplotX1(), " by levels of ", XyplotZ1()) )
        } else {
          xYplot(xyplotData()[[which(names(xyplotData()) =="yhat")]] ~ xyplotData()[[which(names(xyplotData()) ==XyplotX1())]] ,  
                 groups=xyplotData()[[which(names(xyplotData()) ==XyplotZ1())]],
                 type='l',
                 lty=1:length(unique(df()[, XyplotZ1() ])),
                 lcol=1:length(unique(df()[, XyplotZ1() ])), 
                 lwd=2, ylab="Yhat", xlab=XyplotX1(), cex=1.75, 
                 main=paste0("Partial prediction plot of ", XyplotX1(), " by levels of ", XyplotZ1()) )
        }
      } 
    }, height = 600)
    
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
    #Create the y-asix limits for the XY plot group argument
    #Lower
    output$xyplot_con_ylim0 <- renderUI({
      if (input$XyplotYesNo=="Yes") {
        numericInput("xyplotConYlim0", "3. Select the lower y-axis limit",
                     #value=0, step=1)
                     value=con_xy_data()[["ylim0"]], step=1)
      }
    })
    #Upper
    output$xyplot_con_ylim1 <- renderUI({
      if (input$XyplotYesNo=="Yes") {
        numericInput("xyplotConYlim1", "4. Select the upper y-axis limit",
                     #value=1, step=1)
                     value=con_xy_data()[["ylim1"]], step=1)
      }
    })
    #Yes/No on running the plots
    output$xyp_yes_no <- renderUI({ #Same idea as output$vy
      selectInput("XypYesNo", "5. Do you want to run the contrast plot?",
                  choices = c("No", "Yes"), multiple=FALSE,
                  selected="No")     #Will make choices based on my reactive function.
    })
    ## Create the contrast xyplot
    output$xyplot_contrast_plot <- renderPlot({
      if (input$XypYesNo=="Yes") {
        xyp_contrast()
      }
    }, height = 600)
    
    
    #Reactive functions
    #Creates unique levels for the grouping factor in the xyplot
    xy_contrast_levs <- reactive({
      unique(df()[, XyplotZ1()])
    })
    
    #Function to make contrast data
    contrastXyDataFnc <- function(model, X, group, lev1, lev2, reg) {
      #Specs to get min and max limit for X
      spcs <- specs(model, long=TRUE)
      x_min <- spcs[[ "limits"]][rownames(spcs[[ "limits"]]) == "Low", which(colnames(spcs[[ "limits"]]) == X )]
      x_max <- spcs[[ "limits"]][rownames(spcs[[ "limits"]]) == "High", which(colnames(spcs[[ "limits"]]) == X )]
      #y label
      if(reg %in% c("Cox PH", "Cox PH with censoring")) {
        xyplot_ylab <- paste0("Hazard Ratio (",lev1, ":", lev2,")")
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
      
      ## Exponentiate the data if needed ##
      #Contrast
      if(reg %in% c("Cox PH", "Cox PH with censoring","Logistic", "Ordinal Logistic","Poisson")) {
        w[["Contrast"]] <- exp(w[["Contrast"]])
      }
      #Lower
      if(reg %in% c("Cox PH", "Cox PH with censoring","Logistic", "Ordinal Logistic","Poisson")) {
        w[["Lower"]] <- exp(w[["Lower"]])
      }
      #Upper
      if(reg %in% c("Cox PH", "Cox PH with censoring","Logistic", "Ordinal Logistic","Poisson")) {
        w[["Upper"]] <- exp(w[["Upper"]])
      }
      
      #Get the upper and lower limits of the Y-axis
      ylim0 <- min(w[["Lower"]])
      ylim1 <- max(w[["Upper"]])
      return(list(w=w, xyplot_ylab=xyplot_ylab, abline_01=abline_01,ylim0=ylim0, ylim1=ylim1))
      }
    ## Get the contrast data
    con_xy_data <- reactive({
      if (input$XyplotYesNo=="Yes") {
        contrastXyDataFnc(model=fit1(), X= XyplotX1(), group=XyplotZ1(),
                          lev1=input$xyplotConLev1, lev2=input$xyplotConLev2, reg=input$regress_type )
      }
    })
    ## Function to run contrast xyplot
    xypContrastFnc <- function(data, ylim0, ylim1, X, Z, Lev1, Lev2) {
      xYplot(Cbind(data[["w"]][["Contrast"]], data[["w"]][["Lower"]],
                   data[["w"]][["Upper"]]) ~ data[["w"]][[X]] ,
             ylab=data[["xyplot_ylab"]], type='l', method='filled bands', abline=list(h=data[["abline_01"]], col=2,lwd=2),
             col.fill=gray(.95), ylim=c(ylim0, ylim1), xlab=X, lwd=2,
             main=paste0("Partial prediction plot of ", X, " contrasting ", Z,  
                         " levels of ", Lev1, " to ", Lev2 ),
             sub=paste0(Z," effect: ",Lev1))
    }
    
    #Run the plot
    xyp_contrast <- reactive({
      if (input$XypYesNo=="Yes") {
        xypContrastFnc(data=con_xy_data(), ylim0=input$xyplotConYlim0, ylim1=input$xyplotConYlim1, X=XyplotX1(),
                       Z=XyplotZ1() , Lev1=input$xyplotConLev1, Lev2=input$xyplotConLev2)
      }
    })
    
    ## Table of contrast at percentiles to show where the interaction occurs ##
    #Function that gets contrasts quantiles
    contrastQuantFnc <- function(w, sp1, X) {
      p10 <- which(abs(w[[X]] - sp1[["limits"]][[X]][rownames(sp1$limits) == "Low:prediction"]) == min(abs(w[[X]] - sp1[["limits"]][[X]][rownames(sp1$limits) == "Low:prediction"])))
      p25 <- which(abs(w[[X]] - sp1[["limits"]][[X]][rownames(sp1$limits) == "Low:effect"]) == min(abs(w[[X]] - sp1[["limits"]][[X]][rownames(sp1$limits) == "Low:effect"])))
      p50 <- which(abs(w[[X]] - sp1[["limits"]][[X]][rownames(sp1$limits) == "Adjust to"]) == min(abs(w[[X]] - sp1[["limits"]][[X]][rownames(sp1$limits) == "Adjust to"])))
      p75 <- which(abs(w[[X]] - sp1[["limits"]][[X]][rownames(sp1$limits) == "High:effect"]) == min(abs(w[[X]] - sp1[["limits"]][[X]][rownames(sp1$limits) == "High:effect"])))
      p90 <- which(abs(w[[X]] - sp1[["limits"]][[X]][rownames(sp1$limits) == "High:prediction"]) == min(abs(w[[X]] - sp1[["limits"]][[X]][rownames(sp1$limits) == "High:prediction"])))
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
    
    
#This plots the predicted values as a nomogram    
output$nomo_gram <- renderPlot({
  if(input$nomo_yes == "Yes") {
    plot(  do.call("nomogram", list(fit1(), omit=nm_x_var()) )) 
  } else {
    plot(nomogram(fit1()))
  }
}, height = 600)
#Create yes/no box to determine plot single partial effect
output$nomo_one_yes <- renderUI({                                 #Same idea as output$vy
  selectInput("nomo_yes", "1. Do you want to plot a single or multiple predictor scores?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     #Will make choices based on my reactive function.
})
#Select the variables that will get 
output$nomo_one_x <- renderUI({
  selectInput("nm_X", "2. Select the predictor(s).", multiple=TRUE,
              choices = predictor(), selected=predictor()[1])     #Will make choices based on my reactive function.
})


output$calibrate_type <- renderUI({                                #Creates a UI function here but it will
  selectInput("caliType", "1. Select the calibration method",
              choices = c("crossvalidation", "boot", ".632", "randomization"),
              selected= "crossvalidation", multiple=FALSE)
})

#This allows us to select the number of k-fold crossvalidation groups or number of bootstraps for the calibration tab
output$calibrate_B_arg_n <- renderUI({
  numericInput("cali_B_n", "2. Crossvalidation k-folds or Bootstrap #", value = 10, min=2)
})

#Asks if you did multiple imputation.
output$MIForCali <- renderUI({  
  selectInput("MI_for_cali", "3. Did you do Multiple Imputation?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     #Will make choices based on my reactive function.
})

#Determine if we should begin the calibration.
output$BeginCalibrate <- renderUI({  
  selectInput("begin_cali", "4. Begin calibration?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     #Will make choices based on my reactive function.
})

#This creates a calibration curve    
output$cali_brate <- renderPlot({
  if (input$MI_for_cali == "No") {
    
  if (input$begin_cali == "Yes") {
  values$md_y <- median(as.numeric(df()[, input$variableY], na.rm=TRUE))
  values$mn_y <- mean(as.numeric(df()[, input$variableY], na.rm=TRUE))
  set.seed(1)
  if (input$caliType == "boot") {
    plot(calibrate(fit1(), B=input$cali_B_n, u=values$mn_y, method="boot"), subtitles=TRUE)
  }
  if (input$caliType == "crossvalidation") {
    plot(calibrate(fit1(), B=input$cali_B_n, u=values$mn_y, method="crossvalidation"), subtitles=TRUE)
  }
  if (input$caliType == ".632") {
    plot(calibrate(fit1(), u=values$mn_y, method=".632", B=input$cali_B_n), subtitles=TRUE)
  }
  if (input$caliType == "randomization") {
    plot(calibrate(fit1(),  u=values$mn_y, method="randomization", B=input$cali_B_n), subtitles=TRUE)
  }
  }
  } 
  ######
  if (input$MI_for_cali == "Yes") {
    
    if (input$begin_cali == "Yes") {
      values$md_y <- median(as.numeric(new_imputed.si()[, input$variableY], na.rm=TRUE))
      values$mn_y <- mean(as.numeric(new_imputed.si()[, input$variableY], na.rm=TRUE))
      set.seed(1)
      if (input$caliType == "boot") {
        plot(calibrate(fit.si(), B=input$cali_B_n, u=values$mn_y, method="boot"), subtitles=TRUE)
      }
      if (input$caliType == "crossvalidation") {
        plot(calibrate(fit.si(), B=input$cali_B_n, u=values$mn_y, method="crossvalidation"), subtitles=TRUE)
      }
      if (input$caliType == ".632") {
        plot(calibrate(fit.si(), u=values$mn_y, method=".632", B=input$cali_B_n), subtitles=TRUE)
      }
      if (input$caliType == "randomization") {
        plot(calibrate(fit.si(),  u=values$mn_y, method="randomization", B=input$cali_B_n), subtitles=TRUE)
      }
    }
  }
  
}, height = 600)


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
  selectInput("begin_vali", "3. Begin validation?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     #Will make choices based on my reactive function.
})

output$vali_date <- renderPrint({
  if (input$MI_for_vali == "No") {
    
    if (input$begin_vali == "Yes") {
      set.seed(1)
      if (input$valiType == "boot") {
        print(validate(fit1(), B=input$vali_B_n, method="boot", bw=TRUE), digits=3, B=50)
      }
      if (input$valiType == "crossvalidation") {
        print(validate(fit1(), B=input$vali_B_n, method="crossvalidation"), digits=3)
      }
      if (input$valiType == ".632") {
        print(validate(fit1(),  method=".632", B=input$vali_B_n), digits=3)
      }
      if (input$valiType == "randomization") {
        print(validate(fit1(),  method="randomization", B=input$vali_B_n), digits=3)
      }
    }
  }
  ##########
  if (input$MI_for_vali == "Yes") {
    
    if (input$begin_vali == "Yes") {
      set.seed(1)
      if (input$valiType == "boot") {
        print(validate(fit.si(), B=input$vali_B_n, method="boot", bw=TRUE), digits=3, B=50)
      }
      if (input$valiType == "crossvalidation") {
        print(validate(fit.si(), B=input$vali_B_n, method="crossvalidation"), digits=3)
      }
      if (input$valiType == ".632") {
        print(validate(fit.si(),  method=".632", B=input$vali_B_n), digits=3)
      }
      if (input$valiType == "randomization") {
        print(validate(fit.si(),  method="randomization", B=input$vali_B_n), digits=3)
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
    v_ls$pdf[[i]]  <- as.vector(table(xdf[, i])/sum(table(xdf[, i])))
    v_ls$cnm[[i]] <- colnames(xdf)[i]  #Seems to have an issue with a 1 column data frame. Tried
                                       #to change it to names(), didn't help. I get MC data ok, but 
                                       #it gives it the name as the command
    v_ls$typ[[i]]  <- typeof(xdf[,i])  #This is used to work with 'labelled' class in RMS data
    v_ls$pdf_lbl[[i]]  <- as.character(unique(xdf[,i]))  #This will give me the correct PDF value labels
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
  input_mc_fnc1(mc_sim=mc_sim_fnc1())
})


#4
  #Function that loops through the simulated data and coverts the 0-1 values to factors
  #based on the original values so that it will work with the formula function
input_mc_fnc2 <- function(input_dist, input_mc_df, PDF, LBL) {
  for(i in 1:length(input_dist))
    if(input_dist[i] == "runif") {
      input_mc_df[[i]] <- cut(input_mc_df[[i]],breaks=c(-.01, cumsum(PDF[[i]])),  
#                              labels=names(PDF[[i]]))
                           labels=LBL[[i]])
      
    }
  return(input_mc_df)
}

#4A
#Reactive function that runs input_mc_fnc2() above
input_mc_df2 <- reactive({
#  input_mc_fnc1(input_dist=mc_sim_fnc()[["input_dist"]], input_mc_df=input_mc_df1(), PDF=vls1()[["pdf"]])
  input_mc_fnc2(input_dist=mc_sim_fnc1()[[1]], input_mc_df=input_mc_df1(), 
                PDF=vls2()[[6]], LBL=vls2()[["pdf_lbl"]])
})

#5
  #Function that converst the list object to a data frame
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
         "Logistic" = plot_yhat <- 1/(1+exp(-yhat)),
         "Ordinal Logistic"          = plot_yhat <- 1/(1+exp(-yhat)),
         "Poisson"  = plot_yhat <- exp(yhat),
         "Quantile" = plot_yhat <- yhat,
         "Cox PH"   = plot_yhat <- 1/(1+exp(-yhat)),
         "Cox PH with censoring"     = plot_yhat <- 1/(1+exp(-yhat)),
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
    x_ls$mn[which(PRED %in% UMN)]   <- unique(na.omit(as.numeric(unlist(strsplit(unlist((as.character(VAL))), "[^0.0-9.9]+")))))
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
    src_gsa_R2 <- summary(lm(y~ ., data=df3))$"r.squared"
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
              value= deparse((vls2()[["mn"]][which(predictor() %in% input$umn)]), width.cutoff=500 ))
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
  selectInput("updf", "7. Select one predictor that will have its probability density function modified", 
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

## Cobweb plot ##
#Function that converts non-numeric variables to numeric variables
recode_mc_fnc1 <- function(df, dists, cvls) {
  df[, 1] <- as.numeric(cut(df[, 1], breaks=quantile(df[, 1], probs=seq(0,1, .01)), include.lowest=T, right=FALSE))
    for (i in 1:ncol(df[,-1])) {
    if (dists[i] %in% c("runif", "rbinom")) {
      df[, i+1] <- as.numeric(as.factor(df[, i+1]))
      df[, i+1] <- round((df[, i+1] / cvls[[i]])*100, 0)
    } else {
      df[, i+1] <- as.numeric(cut(df[, i+1], breaks=quantile(df[, i+1], probs=seq(0,1, .01)), include.lowest=T, right=FALSE))
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
  recode_mc_fnc1(mc_df_y(), mc_arg_fnc1()[["input_dist"]], cvls=vls2()[["levs"]])
})

#This function creates the cobweb plot
cobweb_fnc <- function(df) {
  plot(1:ncol(df), seq(1, 100, length.out = ncol(df)), type="n",
       axes=F, xlab="Predictors", ylab="Percentiles")
  apply(df[df[, 1] >= 1,], 1, lines, col="gray")
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
  selectInput("topbottom", "Do you want to look at the top or bottom 5%", 
              choices = c("Top 1%","Top 5%", "Bottom 1%", "Bottom 5%"), multiple=TRUE, selected="Top 5%")     
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
output$pwr_smp_bin <- renderUI({                                #
  selectInput("pwrsmpBin", "6. Do you want to determine power or sample size.",       #
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
output$pwr_smp_con <- renderUI({                                #
  selectInput("pwrsmpCon", "7. Do you want to determine power or sample size.",       #
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
## Summary ##
output$power_summary <- renderPrint({ 
  list("Binary Outcome"= if (input$pwrsmpBin == "Sample Size") {
    power.prop.test(power=input$powerBin, p1=input$p1Bin, p2=input$p2Bin, sig.level=input$sigBin)
  }  else {
    power.prop.test(n=input$nBin, p1=input$p1Bin, p2=input$p2Bin, sig.level=input$sigBin)
  },
  "Continuous Outcome"= if (input$pwrsmpCon == "Sample Size") {
    power.t.test(power=input$powerCon, delta=input$deltaCon, sd=input$sdCon, 
                    sig.level=input$sigCon, type=input$typeCon)
  }  else {
    power.t.test(n=input$nCon, delta=input$deltaCon, sd=input$sdCon, 
                    sig.level=input$sigCon, type=input$typeCon)
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
}, height = 700, width = 1000 )

output$cutoffplot <- renderPlot({ 
  #  hist(yhat_plot_rslt())
  cutoff_plot_rslt()
}, height = 400, width = 800 )

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
fbconf <- function(x, y, z, dataf, conf_lev) {
  #Aggregates outcome by factor 
#  agr_sum <- aggregate(dataf[, y] ~ dataf[, x] + dataf[, z], FUN="sum", data= dataf)
#  agr_n <- aggregate(dataf[, y] ~ dataf[, x] + dataf[, z], FUN="length", data= dataf)
  agr_sum <- aggregate(dataf[, y], list(dataf[, x] , dataf[, z]), FUN="sum")
  agr_n <- aggregate(dataf[, y], list(dataf[, x] , dataf[, z]), FUN="length")
  agr_df <- data.frame(x_lev=agr_sum[, 1], z_lev=agr_sum[, 2], agr_sum=agr_sum[, 3], agr_n=agr_n[, 3])
  #Calculates confidence intervals
  agr_df <- cbind(agr_df, binconf(x=agr_df[,3], n=agr_df[,4], alpha=1 - conf_lev))
  return(agr_df) 
}

#Continuous outcomes
ftconf <- function(x, y, z, dataf, conf_lev) {
  #Aggregates outcome by factor 
#  agr_m <- aggregate(dataf[, y] ~ dataf[, x]+ dataf[, z], FUN="mean", data= dataf)
#  agr_sd <- aggregate(dataf[, y] ~ dataf[, x]+ dataf[, z], FUN="sd", data= dataf)
#  agr_n <- aggregate(dataf[, y] ~ dataf[, x]+ dataf[, z], FUN="length", data= dataf)
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
fpconf <- function(x, y, z, dataf, conf_lev) {
  #Aggregates outcome by factor 
#  agr_sum <- aggregate(dataf[, y] ~ dataf[, x]+ dataf[, z], FUN="sum", data= dataf)
#  agr_n <- aggregate(dataf[, y] ~ dataf[, x]+ dataf[, z], FUN="length", data= dataf)
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

fconf <- function(x=xcivar, y=ycivar, z=zcivar, dataf, conf_lev=ciconf_lev) {
  #fconf <- function(x=xcivar, y=ycivar, z=zcivar, dataf=df(), conf_lev=ciconf_lev) {
  switch(input$fci_type,                #"var" and can be used anywhere in server.r.
         "Mean (t)" =  ftconf(x, y, z, dataf, conf_lev), 
         "Proportion (binomial)" =  fbconf(x, y, z, dataf, conf_lev), 
         "Poisson (exact)" =  fpconf(x, y, z, dataf, conf_lev) 
  )
}

#Reactive function that runs fconf above
fcidf <- reactive({                  #This indicates the data frame I will use.
  if(input$FCiCreate == "Yes") {
  fconf(x=input$fxcivar, y=input$fycivar, z=input$fzcivar, dataf=df(), conf_lev=input$fciconf_lev)
  }
})

########################################
#Put a function here for the point estimate, lower, upper bounds
ci_fac_fnc <- function(x_lev, z_lev, agr_df) {
  prmtrs <- c("PointEst", "Lower", "Upper")
  ctrs <- as.vector(unique(agr_df[, x_lev]))
  #Point est 
  ci_p <- list()
  for (i in 1:length(ctrs)) {
    x <- agr_df[agr_df[, x_lev] ==ctrs[i], z_lev]
    y <- agr_df[agr_df[, x_lev] ==ctrs[i], prmtrs[1]]
    xx <- rcspline.eval(x, inclx=TRUE, nk=4)
    knots <- attr(xx, "knots")
    coef <- lsfit(xx, y)$coef
    w <- rcspline.restate(knots, coef[-1], x="{\\rm BP}")
    xtrans <- eval(attr(w, "function"))
    ci_p[[i]] <- cbind(x, y_p=coef[1] + xtrans(x))
  }
  #Lower CI
  ci_l <- list()
  for (i in 1:length(ctrs)) {
    x <- agr_df[agr_df[, x_lev]==ctrs[i], z_lev]
    y <- agr_df[agr_df[, x_lev]==ctrs[i], prmtrs[2]]
    xx <- rcspline.eval(x, inclx=TRUE, nk=4)
    knots <- attr(xx, "knots")
    coef <- lsfit(xx, y)$coef
    w <- rcspline.restate(knots, coef[-1], x="{\\rm BP}")
    xtrans <- eval(attr(w, "function"))
    ci_l[[i]] <- cbind(x, y_l=coef[1] + xtrans(x))
  }
  #Upper CI
  ci_u <- list()
  for (i in 1:length(ctrs)) {
    x <- agr_df[agr_df[, x_lev]==ctrs[i], z_lev]
    y <- agr_df[agr_df[, x_lev]==ctrs[i], prmtrs[3]]
    xx <- rcspline.eval(x, inclx=TRUE, nk=4)
    knots <- attr(xx, "knots")
    coef <- lsfit(xx, y)$coef
    w <- rcspline.restate(knots, coef[-1], x="{\\rm BP}")
    xtrans <- eval(attr(w, "function"))
    ci_u[[i]] <- cbind(x, y_u=coef[1] + xtrans(x))
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
  ci_fac_fnc(x_lev="x_lev", z_lev="z_lev", agr_df=fcidf())
  }
})

########################################

#This creates the time plot
plot_fci_fnc <- function(x, y, z, xcivar, ycivar, zcivar, dataf, ci_p, ci_l, ci_u, max_pest, min_pest, max_ci, min_ci, ctrs, cibands) {
  #Set up colors
  my_clr <- c(175, 33, 26, 9,76,119, 310, 45, 368, 653, 69, 96,  145, 451, 500)
  plot(unique(dataf[, z]), seq(min(min_ci, na.rm=T), max(max_ci, na.rm=T), length.out=length(unique(dataf[, z]))), type="n",  
       cex.lab=1.35,cex.main=1.35,cex.sub=1.35, 
       ylab=ycivar, xlab=zcivar, ylim=c(min(min_ci, na.rm=T)*.95, max(max_ci, na.rm=T)*1.05),
       main= paste0( ycivar, " trajectories of ", xcivar,  " by ", zcivar))
  #Plot point estimate lines
  ci_time <- list() 
  l95 <- list() 
  u95 <- list() 
  xx_t <- list() 
  yy_t <- list() 
  for (i in 1:length(ctrs)) {
    lines(ci_p[[i]][, "x"], ci_p[[i]][, "y_p"], lty=i, col=colors()[my_clr[i]],lwd=2)
    text(ci_p[[i]][1, "x"], ci_p[[i]][1, "y_p"], ctrs[i])
    text(ci_p[[i]][nrow(ci_p[[i]]), "x"], ci_p[[i]][nrow(ci_p[[i]]), "y_p"], ctrs[i])
  }
  if(cibands == "Yes") {
    for (i in 1:length(ctrs)) {
    ci_time[[i]] <- ci_l[[i]][,1]
    l95[[i]] <- ci_l[[i]][,2]
    u95[[i]] <- ci_u[[i]][,2]
    
    xx_t[[i]] <- c(ci_time[[i]], rev(ci_time[[i]]))
    yy_t[[i]] <- c(l95[[i]], rev(u95[[i]]))
    polygon(unlist(xx_t[[i]]), unlist(yy_t[[i]]), col = adjustcolor(colors()[my_clr[i]], alpha.f = 0.1), 
            border=adjustcolor(colors()[my_clr[i]], alpha.f = 0.1))
  }
  #return(list(ci_p=ci_p, ci_l=ci_l, ci_u=ci_u, l95=l95, u95=u95, xx_t=xx_t, yy_t=yy_t))
  }
}

#Confidence interval plot reactive function
plot_fci <- reactive({                  #This indicates the data frame I will use.
  if(input$FCiCreate == "Yes") {
    plot_fci_fnc(x="x_lev", y="PointEst", z="z_lev", xcivar=input$fxcivar, ycivar=input$fycivar, zcivar=input$fzcivar,
                 dataf=fcidf(), ci_p=fci_fac()$ci_p, ci_l=fci_fac()$ci_l, ci_u=fci_fac()$ci_u,
    max_pest=fci_fac()$max_pest, min_pest=fci_fac()$min_pest, max_ci=fci_fac()$max_ci, min_ci=fci_fac()$min_ci, 
    ctrs=fci_fac()$ctrs, cibands=input$fcibands)
    }
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
              choices = setdiff(var(), input$fycivar), multiple=FALSE, selected=var()[2])     #Will make choices based on my reactive function.
})
#Select the Confidence bands.
output$FCI_bands <- renderUI({                                 #Same idea as output$vy
  selectInput("fcibands", "4. Do you want confidence bands?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     #Will make choices based on my reactive function.
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
#Select whether to run the 95% confidence interval or not
output$FCi_create <- renderUI({                                
  selectInput("FCiCreate", "7. Create the time plot?",
              choices = c("No", "Yes"),
              selected="No")
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
             ols(as.formula(input$up_fmla), x=TRUE, y=TRUE) 
           } else {
             ols(mdl_fmla(), x=TRUE, y=TRUE)}, 
           "Logistic" = if(input$updy == "Yes") {
             lrm(as.formula(input$up_fmla), x=TRUE, y=TRUE, tol=1e-100, maxit=20) #I added tol value so it can handle time predictor (YYMM)
           } else {                                                               #Added maxit to handle small samples. See https://stat.ethz.ch/pipermail/r-help/2017-September/449115.html
             lrm(mdl_fmla(), x=TRUE, y=TRUE, tol=1e-100, maxit=20)},  #I added tol value so it can handle time predictor that causes "singularity"
           "Ordinal Logistic" = if(input$updy == "Yes") {
             orm(as.formula(input$up_fmla), x=TRUE, y=TRUE) 
           } else {
             orm(mdl_fmla(), x=TRUE, y=TRUE)},
           "Poisson" = if(input$updy == "Yes") {
             Glm(as.formula(input$up_fmla), x=TRUE, y=TRUE, family=poisson()) 
           } else {
             Glm(mdl_fmla(), x=TRUE, y=TRUE, family=poisson())},
           "Quantile" = if(input$updy == "Yes") {
             Rq(as.formula(input$up_fmla), x=TRUE, y=TRUE, tau=as.numeric(rq_tau1())) 
           } else {
             Rq(mdl_fmla(), x=TRUE, y=TRUE,  tau=as.numeric(rq_tau1()))},
           "Cox PH"   = if(input$updy == "Yes") {
             cph(cox_mdl_fmla1u(), x=TRUE, y=TRUE, surv=TRUE) 
           } else {
             cph(cox_mdl_fmla1(), x=TRUE, y=TRUE, surv=TRUE)},
           "Cox PH with censoring" = if(input$updy == "Yes") {
             cph(cox_mdl_fmla2u(), x=TRUE, y=TRUE, surv=TRUE) 
           } else {
             cph(cox_mdl_fmla2(), x=TRUE, y=TRUE, surv=TRUE)},
           "Generalized Least Squares" = if(input$updy == "Yes") {
             Gls(as.formula(input$up_fmla), x=TRUE, correlation=corCAR1(form= gls_cor()))
           } else {
             Gls(mdl_fmla(), x=TRUE, 
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
  ctl_med <- median(df[, y][as.numeric(df[, x]) == min(as.numeric(df[, x]))], na.rm=T)
  trt_med <- median(df[, y][as.numeric(df[, x]) == max(as.numeric(df[, x]))], na.rm=T)
  ctl_mn <- mean(df[, y][as.numeric(df[, x]) == min(as.numeric(df[, x]))], na.rm=T)
  trt_mn <- mean(df[, y][as.numeric(df[, x]) == max(as.numeric(df[, x]))], na.rm=T)
  #### Get quantiles and bind with means. Partially used in the legend and the observed table in a later section
  qt1 <- quantile(df[, y][as.numeric(df[, x]) == min(as.numeric(df[, x]))], probs=c(.1, .25, .5, .75, .9), na.rm=T)
  qt2 <- quantile(df[, y][as.numeric(df[, x]) == max(as.numeric(df[, x]))], probs=c(.1, .25, .5, .75, .9), na.rm=T)
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
  #Add cost difference beteen Treatment and controls 
  est2$Diff. <- abs(round(est2$Control) - round(est2$TREATMENT))
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
## Creates a plot for an interaction of continuous X by a factor ##
#Select the continuous predictor
output$Cxyplot_x <- renderUI({
  selectInput("CXyplotX", "1. Select a continuous predictor.", 
              choices = predictor(), selected=predictor()[1], multiple=FALSE)     
})
#Select the factor for grouping
output$Cxyplot_z <- renderUI({
  selectInput("CXyplotZ", "2. Select a factor.", 
              choices = setdiff(predictor(), input$CXyplotX),  multiple=FALSE)     
})
#Make confidence bands for the plot
output$Cxyplot_bands <- renderUI({                                 
  selectInput("CXyplotBands", "3. Do you want confidence bands?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")
})
#Create yes/no box to make the XY plot
output$Cxyplot_yes_no <- renderUI({                                 
  selectInput("CXyplotYesNo", "4. Do you want to create the plot?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     
})

#Continuous predictor
CXyplotX1 <- reactive({                  
  input$CXyplotX
})
#Factor
CXyplotZ1 <- reactive({                  
  input$CXyplotZ
})
#Get the Predicted values needed for the plot
CxyplotData <- reactive({                 
  do.call("Predict", list(fit1(), CXyplotX1(), CXyplotZ1(), fun= function(x)x*-1) )    
})

#This creates the plot
output$CxYplot_interaction <- renderPlot({
  if(input$CXyplotYesNo == "Yes") {
    if(input$CXyplotBands == "Yes") {
      xYplot( Cbind(CxyplotData()[[which(names(CxyplotData()) =="yhat")]], 
                    CxyplotData()[[which(names(CxyplotData()) =="lower")]], 
                    CxyplotData()[[which(names(CxyplotData()) =="upper")]]) ~ CxyplotData()[[which(names(CxyplotData()) ==CXyplotX1())]],  
              groups=CxyplotData()[[which(names(CxyplotData()) == CXyplotZ1())]],
              method='filled bands', type='l', 
              col.fill=adjustcolor(1:length(unique(df()[, CXyplotZ1() ])), alpha.f = 0.2),
              lty=1:length(unique(df()[, CXyplotZ1() ])),
              lcol=1:length(unique(df()[, CXyplotZ1() ])), 
              lwd=2, ylab="Yhat", xlab=CXyplotX1(), cex=1.75,
              main=paste0("Partial prediction plot of ", CXyplotX1(), " by levels of ", CXyplotZ1()) )
    } else {
      xYplot(CxyplotData()[[which(names(CxyplotData()) =="yhat")]] ~ CxyplotData()[[which(names(CxyplotData()) == CXyplotX1())]] ,  
              groups=CxyplotData()[[which(names(CxyplotData()) == CXyplotZ1())]],
              type='l',
              lty=1:length(unique(df()[, CXyplotZ1() ])),
              lcol=1:length(unique(df()[, CXyplotZ1() ])), 
              lwd=2, ylab="Yhat", xlab=CXyplotX1(), cex=1.75, 
              main=paste0("Partial prediction plot of ", CXyplotX1(), " by levels of ", CXyplotZ1()) )
    }
  } 
}, height = 600)

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
#Create the y-asix limits for the XY plot group argument
#Lower
output$Cxyplot_con_ylim0 <- renderUI({
  if (input$CXyplotYesNo=="Yes") {
    numericInput("CxyplotConYlim0", "3. Select the lower y-axis limit",
                 #value=0, step=1)
                 value=Ccon_xy_data()[["ylim0"]], step=1)
  }
})
#Upper
output$Cxyplot_con_ylim1 <- renderUI({
  if (input$CXyplotYesNo=="Yes") {
    numericInput("CxyplotConYlim1", "4. Select the upper y-axis limit",
                 #value=1, step=1)
                 value=Ccon_xy_data()[["ylim1"]], step=1)
  }
})
#Yes/No on running the plots
output$Cxyp_yes_no <- renderUI({ #Same idea as output$vy
  selectInput("CXypYesNo", "5. Do you want to run the contrast plot?",
              choices = c("No", "Yes"), multiple=FALSE,
              selected="No")     #Will make choices based on my reactive function.
})
## Create the contrast xyplot
output$Cxyplot_contrast_plot <- renderPlot({
  if (input$CXypYesNo=="Yes") {
    Cxyp_contrast()
  }
}, height = 600)


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
  return(list(w=w, xyplot_ylab=xyplot_ylab, abline_01=abline_01,ylim0=ylim0, ylim1=ylim1))
  }
## Get the contrast data
Ccon_xy_data <- reactive({
  if (input$CXyplotYesNo=="Yes") {
    CcontrastXyDataFnc(model=fit1(), X= CXyplotX1(), group=CXyplotZ1(),
                       lev1=input$CxyplotConLev1, lev2=input$CxyplotConLev2, reg=input$regress_type )
  }
})
## Function to run contrast xyplot
CxypContrastFnc <- function(data, ylim0, ylim1, X, Z, Lev1, Lev2) {
  xYplot(Cbind(data[["w"]][["Contrast"]], data[["w"]][["Lower"]],
               data[["w"]][["Upper"]]) ~ data[["w"]][[X]] ,
         ylab=data[["xyplot_ylab"]], type='l', method='filled bands', abline=list(h=data[["abline_01"]], col=2,lwd=2),
         col.fill=gray(.95), ylim=c(ylim0, ylim1), xlab=X, lwd=2,
         main=paste0("Partial prediction plot of ", X, " contrasting ", Z,  
                     " levels of ", Lev1, " to ", Lev2 ),
         sub=paste0(Z," effect: ",Lev1))
}

#Run the plot
Cxyp_contrast <- reactive({
  if (input$CXypYesNo=="Yes") {
    CxypContrastFnc(data=Ccon_xy_data(), ylim0=input$CxyplotConYlim0, ylim1=input$CxyplotConYlim1, X=CXyplotX1(),
                    Z=CXyplotZ1() , Lev1=input$CxyplotConLev1, Lev2=input$CxyplotConLev2)
  }
})

## Table of contrast at percentiles to show where the interaction occurs ##
#Function that gets contrasts quantiles
CcontrastQuantFnc <- function(w, sp1, X) {
  p10 <- which(abs(w[[X]] - sp1[["limits"]][[X]][rownames(sp1$limits) == "Low:prediction"]) == min(abs(w[[X]] - sp1[["limits"]][[X]][rownames(sp1$limits) == "Low:prediction"])))
  p25 <- which(abs(w[[X]] - sp1[["limits"]][[X]][rownames(sp1$limits) == "Low:effect"]) == min(abs(w[[X]] - sp1[["limits"]][[X]][rownames(sp1$limits) == "Low:effect"])))
  p50 <- which(abs(w[[X]] - sp1[["limits"]][[X]][rownames(sp1$limits) == "Adjust to"]) == min(abs(w[[X]] - sp1[["limits"]][[X]][rownames(sp1$limits) == "Adjust to"])))
  p75 <- which(abs(w[[X]] - sp1[["limits"]][[X]][rownames(sp1$limits) == "High:effect"]) == min(abs(w[[X]] - sp1[["limits"]][[X]][rownames(sp1$limits) == "High:effect"])))
  p90 <- which(abs(w[[X]] - sp1[["limits"]][[X]][rownames(sp1$limits) == "High:prediction"]) == min(abs(w[[X]] - sp1[["limits"]][[X]][rownames(sp1$limits) == "High:prediction"])))
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
#This gets the residual names
      schoenfeld_X_names <- reactive({
        rownames(schoenfeld_e()[[1]])
      })

#Identifies the number of residuals that should be used in the drop down box below.
#Indicates if we drop the "Global" value when >1 or use the first if ==1.
      schoenfeld_X_num <- reactive({
        ifelse( length(schoenfeld_X_names()) > 1, length(schoenfeld_X_names()) * -1, 1)
      })
      
#Select the variable for the Schoenfeld residuals
output$Schoenfeld_X <- renderUI({
  selectInput("schoenfeldx", "1. Select a single Schoenfeld residual.", 
              choices = schoenfeld_X_names()[schoenfeld_X_num()] , multiple=FALSE, selected=schoenfeld_X_names()[1])
})

#This gives a number for the variable for the Schoenfeld residual
schoenfeld_e <- reactive({
  if (input$regress_type %in% c("Cox PH", "Cox PH with censoring")) {
    cox.zph(fit1())
  }  
})

#This prints the point estimates and confidence intervals
output$schoenfeld_test <- renderTable({
  if (input$regress_type %in% c("Cox PH", "Cox PH with censoring")) {
    schoenfeld_e()[[1]]
  }
}, rownames = TRUE)

#This plots the Schoenfeld residuals    
output$schoenfeld_plt <- renderPlot({
  if (input$regress_type %in% c("Cox PH", "Cox PH with censoring")) {
    plot(schoenfeld_e()[input$schoenfeldx], col=2, lwd=2)
    abline(h=0, lty=3, col=4, lwd=2)
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
  selectInput("hazard_plot", "10. Do you want the hazard curve?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     
})
#Indicate if you want the log-minus-log plot
output$SrvLogLog <- renderUI({                                 
  selectInput("srv_log_log", "11. Want a log-minus-log plot (assess PH)?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     
})
#This will go into the "fun" argument based on if I want a survival or Hazard function
SrvHzr <- reactive({ 
  if (input$hazard_plot == "No") {
    function(x) {x}
  } else {
    function(x) {1 - x}
  }
})
#"Survival" or "Hazard" to be used for labels
SrvHzrLbl <- reactive({ 
  if (input$hazard_plot == "No") {
    "Survival"
  } else {
    "Hazard"
  }
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
  if (input$regress_type %in% c("Cox PH", "Cox PH with censoring")) {
    survfit(fit1(), conf.int=input$SrvPltLvl)
  }
})

#This plots the predicted values  for the partial effects plots  
output$surv_plot1 <- renderPlot({
  
  if(input$surv_plt_run == "No") {
    
    if ("strata" %in% names(srvft1()) ) {
      plot(srvft1(), 
           xlim=c(input$sp_Xlim1, input$sp_Xlim2), ylim=c(input$sp_Ylim1,input$sp_Ylim2),
           ylab= paste0(SrvHzrLbl(), " Probability"), fun=SrvHzr(),
           xlab=paste0(SrvHzrLbl()," functions of time stratified by ", strsplit(names(srvft1()$strata)[1], "=")[[1]][1], 
                       " with ", input$SrvPltLvl*100, "% confidence intervals"), 
           mark.time=T, pch=LETTERS[1:length(names(srvft1()$strata))])
    } 
          if ( !"strata" %in% names(srvft1()) ) {
      plot(srvft1(), fun=SrvHzr(),
           xlim=c(input$sp_Xlim1, input$sp_Xlim2), ylim=c(input$sp_Ylim1,input$sp_Ylim2),
           ylab=paste0(SrvHzrLbl()," Probability"),
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
  selectInput("km_hazard_plot", "2. Do you want the hazard curve?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     
})
#Create yes/no box to run survival plot
output$KMSurvPltRun <- renderUI({                                 
  selectInput("km_surv_plt_run", "3. Do you want to create the KM plot?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     
})
#Indicate lower limit of x-axis
output$KMSurvPltXlim1 <- renderUI({
  numericInput("km_sp_Xlim1", "4. Lower X-axis limit.",
               value = 0, step = 1)
})
#Indicate upper limit of x-axis
output$KMSurvPltXlim2 <- renderUI({
  numericInput("km_sp_Xlim2", "5. Upper X-axis limit.",
               value = km_cox_max_time(), step = 1)
})
#Indicate lower limit of y-axis
output$KMSurvPltYlim1 <- renderUI({
  numericInput("km_sp_Ylim1", "6. Lower Y-axis limit.",
               value = 0, step = .01)
})
#Indicate upper limit of x-axis
output$KMSurvPltYlim2 <- renderUI({
  numericInput("km_sp_Ylim2", "7. Upper Y-axis limit.",
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

#Survival fit object
KMsrvftFmla <- reactive({ 
  if (input$regress_type %in% c("Cox PH", "Cox PH with censoring")) {
    as.formula(paste(paste0("Surv(", outcome(), ",", censor1(), ")", "~", input$KMSrvPltX)))
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
       xlab=paste0(KMSrvHzrLbl," function of ", Y), 
       mark.time=F, fun=pltType , lwd=2,
       pch=LETTERS[1:length(unique(df[, KMSrvPltX]))],
       col=1:length(unique(df[, KMSrvPltX] )), lty= 1:length(unique( df[, KMSrvPltX])))
  legend(lgnd, legend=unique( df[, KMSrvPltX]), col=1:length(unique(df[, KMSrvPltX] )), 
         lty=1:length(unique(df[, KMSrvPltX] )), bty="n", lwd=2, cex=1.5)
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
#A select box for the variable that interacts with time
output$time_dependent_var <- renderUI({                                
  selectInput("TimeDepVar", "3. Select the interaction variable",       
              choices = predictor(), multiple=FALSE, 
              selected= predictor()[1])   #Will make choices based on my reactive function.
})

output$td_log_increment <- renderUI({ 
  numericInput("TdLogIncrement", "4. Select a value to add to log(time).",
               value=0, min=0, step=1) 
})

#Yes/No if I want to create the time-dependent file
output$td_data_yes_no <- renderUI({ 
  selectInput("TdDataYesNo", "5. Want a time-dependent data file?",
              choices = c("No", "Yes"), multiple=FALSE, selected="No") 
})
#Download time dependent data file
output$td_data_download_name <- renderUI({ 
  textInput("TdDataDownloadName", "6. Enter the data name.", 
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
timeDepDfFnc <- function(X, Y, Z, censor, reg, df, tdcut1, AddLog) {
  #No censoring
  if (reg == "Cox PH") {
    cph_fmla <- as.formula(paste(paste0("Surv(", Y,")" , "~"),
                                 paste(X, collapse= "+")))
    timeDepDF <- survSplit(formula= cph_fmla, data=df, cut=tdcut1,
                           start="tstart", id= "id.obs", episode= 
                             "tgroup",
                           end="tstop", event="event")
  }
  #Censoring
  if (reg == "Cox PH with censoring") {
    ccph_fmla <- as.formula(paste(paste0("Surv(", Y, ",", censor, ")", "~"),
                                  paste(X, collapse= "+")))
    timeDepDF <- survSplit(formula=ccph_fmla , data=df, cut=tdcut1,
                           start="tstart", id= "id.obs", episode= "tgroup",
                           end="tstop", event="event")
  }
  #INTERACTION  
  timeDepDF[,paste0(Z, ".log.", Y)] <- 
    timeDepDF[, Z]*log(timeDepDF[,"tgroup"] + AddLog)      
  return(timeDepDF)
}

#Reactive function that runs tdcutFnc() and creates the time-dependent file 
TdDataFrame <-  reactive({
  if (input$TdDataYesNo == "Yes") { 
    timeDepDfFnc(X=input$TDPredX, Y=outcome(), Z=input$TimeDepVar, censor=censor1(), 
                 reg=input$regress_type, df=df(), tdcut1=tdcut1(), 
                 AddLog= input$TdLogIncrement)
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
  selectInput("save_dfbetas", "1. Save the DFBETAS?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     #Will make choices based on my reactive function.
})
dfbetasFit <- reactive({
  if(input$save_dfbetas == "Yes") {
    list("DFBETAS"=dfbetas_res(),
         "Influential"=w_influ(),
         "InfluentialData"=w_influ_df(),
         "Schoenfeld"= Schoenfeld_res(),
         "Martingale"=Martingale_res())
  }
})
output$dfbetas_fit_name <- renderUI({ 
  textInput("DFBETASFitName", "2. Enter the DFBETAS name.", 
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
#Gets the data frame of the influential observations
w_influ_df <- reactive({ 
  if (input$regress_type %in% c("Cox PH", "Cox PH with censoring")) {
    if (input$DFBETAS_Yes_No =="Yes") { 
    show.influence(w_influ(), dframe=df())
    }
  }
})

output$InfluenceDFBETAS <- renderPrint({
  if (input$regress_type %in% c("Cox PH", "Cox PH with censoring")) {
    w_influ_df()
    }
  })
  

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
cme_fmla1 <- reactive({
  as.formula(paste(paste0("Surv(",input$variableY,")" , "~"),   
                   paste(c(mdl_vnms(), mlv_cor()), collapse= "+")))
})
#Create the Cox model with censoring
cme_fmla2 <- reactive({
  as.formula(paste(paste0("Surv(", input$variableY, ",", censor1(), ")", "~"),   
                   paste(c(mdl_vnms(), mlv_cor()), collapse= "+")))
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
  efit1()
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

################################################################################
## Testing section: Begin  ##
################################################################################
#output$testplot1 <- renderPlot({ 
##  plot(values$a, values$b)
##} )
##output$test1 <- renderPrint({
## str(modifiedFacDf())
  #  non_modified_vars()
##  })


################################################################################
## Testing section: End ##
################################################################################

  })   #This is the last line of code that closes out the entire server file
  
