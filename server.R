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
library(coda)
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
      selectInput("ModifyCharacter", "1. Select variables to convert to 'character'.", 
                  choices = var(), multiple=TRUE)
    })
    #Factor
    output$modify_factor <- renderUI({                                 #Same idea as output$vy
      selectInput("ModifyFactor", "2. Select variables to convert to 'factor'.", 
                  choices = var(), multiple=TRUE)
    })
    #Numeric
    output$modify_numeric <- renderUI({                                 
      selectInput("ModifyNumeric", "3. Select variables to convert to 'numeric'.", 
                  choices = var(), multiple=TRUE)
    })
    #4. Time
    output$modify_time_X <- renderUI({                                 
      selectInput("ModifyTimeX", "4. Select variables to format as 'Date'.", 
                  choices = var(), multiple=TRUE)
    })
    #5. Select Time format. Not using pure strptime ("1/31/2021 21:15")
    output$modify_time_format <- renderUI({  
      selectInput("ModifyTimeFmt", "5. Choose the correct time format.", 
                  choices = c("31JAN2021", "31JAN21","31-JAN-2021","31-JAN-21","01/31/2021", "01/31/21", 
                              "01-31-2021", "01-31-21", "2021-01-31", "21-01-31", 
                              "1/31/2021 21:15 as 1/31/2021", "1/31/2021 21:15:30 as 1/31/2021", 
                              "1/31/2021 12:00:00 AM as 1/31/2021", 
                              "2021-01-31 21:15:30 as 2021-01-31",
                              "31JAN2021:12:00:00 as 31JAN2021",
                              "JAN2021", "JAN21","JAN-2021","JAN-21","01/2021", "01/21",
                              "01-2021", "01-21", "2021-01", "21-01", "44227 in Excel"), 
                  multiple=TRUE, selected="01-31-2021")
    })
    #6. Number of replications per format used
    output$modify_tm_fmt_rep <- renderUI({ 
      textInput("ModifyTimeFmtReps", "6. Number of variables per format.", 
                value= paste0('c(', length(input$ModifyTimeX), ')') )     
    })
    
    ## Modify the dataset ##
    #7. Subset the dataset
    output$subset_df_yes_no <- renderUI({                                 
      selectInput("SubsetYesNo", "7. Want to subset the data?", 
                  choices = c("No", "Yes"), multiple=FALSE, selected="No")
    })
    #8. Formula for the subset
    output$subset_args <- renderUI({ 
      textInput("SubsetArgs", "8. Enter the formula to subset data.", 
                value= "subset= , select=")     
    })
    #9. Modify the dataset
    output$modify_df_yes_no <- renderUI({                                 
      selectInput("ModifyDfYesNo", "9. Want to create the modified dataset?", 
                  choices = c("No", "Yes"), multiple=FALSE, selected="No")
    })
    #10. Time
    output$modify_2_var_Time <- renderUI({                                 
      selectInput("Modify2VrTm", "10. Create 'Time': Select 2 variables.", 
                  choices = var(), multiple=TRUE)
    })
    #11. Make month indicators
    output$modify_Time_add_month <- renderUI({                                 
      selectInput("ModifyTmAddMth", "11. Select X to create 'YYMM' and 'Month'.", 
                  choices = var(), multiple=FALSE)
    })
    #12. Add time variables to dataset
    output$modify_add_time_YN <- renderUI({                                 
      selectInput("ModAddTmYN", "12. Add time variables to modified data?", 
                  choices = c("No", "Yes"), multiple=FALSE, selected="No")
    })
    
    #####################
    # Save modified data #
    #####################
    output$modified_df_save <- renderUI({  
      selectInput("ModifiedDfSave", "13. Save the modified data?", 
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
          df_mod[, which(colnames(df_mod) %in% ModifyCharacter)] <- lapply(df_mod[which(colnames(df_mod) %in% ModifyCharacter)], as.character)
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
        df_mod[, which(colnames(df_mod) %in% ModifyNumeric)] <- lapply(df_mod[which(colnames(df_mod) %in% ModifyNumeric)], as.numeric)
      }
      return(df_mod[, which(colnames(df_mod) %in% ModifyNumeric) ,drop=FALSE])
    }
    #Runs the function above
    modifiedNumDf <- reactive({
      if(input$ModifyDfYesNo == "Yes") {
      modifiedNumFnc(df=modifySubsetDf(), ModifyNumeric=input$ModifyNumeric)
      }
    })
    ## Date formats ##
    #Create formats
    fncFmtVec <- function(Format, CntVec) {
      Full.Format <- rep(Format, CntVec )
      return(Full.Format)
    }
    #Runs the function above
    modifiedFullFormat <- reactive({
      if(input$ModifyDfYesNo == "Yes") {
        fncFmtVec(Format= input$ModifyTimeFmt, 
                  CntVec= eval(parse(text=input$ModifyTimeFmtReps  )) )
      }
    })
    
    ## FUnction to create formatted data ##
    fncDateFmt <- function(DF, X, Format) {
      if(is.null(X)) {
        df_mod <- DF[, which(colnames(DF) %in% X), drop = FALSE]
      } else {
        df_mod <- DF[, which(colnames(DF) %in% X), drop = FALSE]
        for(i in 1:length(X)) {
          switch(Format[i], 
                 "31JAN2021" = df_mod[, X[i]] <- as.Date(df_mod[, X[i]], format="%d%b%Y"), 
                 "31JAN21" = df_mod[, X[i]] <- as.Date(df_mod[, X[i]], format="%d%b%y"), 
                 "31-JAN-2021" =  df_mod[, X[i]] <- as.Date(df_mod[, X[i]], format="%d-%b-%Y"),
                 "31-JAN-21" = df_mod[, X[i]] <- as.Date(df_mod[, X[i]], format="%d-%b-%y"),
                 "01/31/2021" = df_mod[, X[i]] <- as.Date(df_mod[, X[i]], format="%m/%d/%Y"),
                 "01/31/21" = df_mod[, X[i]] <- as.Date(df_mod[, X[i]], format="%m/%d/%y"),
                 "01-31-2021" = df_mod[, X[i]] <- as.Date(df_mod[, X[i]], format="%m-%d-%Y"),
                 "01-31-21" = df_mod[, X[i]] <- as.Date(df_mod[, X[i]], format="%m-%d-%y"),
                 "2021-01-31" = df_mod[, X[i]] <- as.Date(df_mod[, X[i]], format="%Y-%m-%d"),
                 "21-01-31" = df_mod[, X[i]] <- as.Date(df_mod[, X[i]], format="%y-%m-%d"),
                 "1/31/2021 21:15" = df_mod[, X[i]] <- strptime(as.character(df_mod[, X[i]]), format="%m/%d/%Y %H:%M"),
                 "1/31/2021 21:15 as 1/31/2021" = df_mod[, X[i]] <- as.Date(strptime(as.character(df_mod[, X[i]]), format="%m/%d/%Y %H:%M")),
                 "1/31/2021 21:15:30" = df_mod[, X[i]] <- strptime(as.character(df_mod[, X[i]]), format="%m/%d/%Y %H:%M:%S"),
                 "1/31/2021 21:15:30 as 1/31/2021" = df_mod[, X[i]] <- as.Date(strptime(as.character(df_mod[, X[i]]), format="%m/%d/%Y %H:%M:%S")),
                 "1/31/2021 12:00:00 AM" = df_mod[, X[i]] <- strptime(as.character(df_mod[, X[i]]), format="%m/%d/%Y %H:%M:%S %p"),
                 "1/31/2021 12:00:00 AM as 1/31/2021" = df_mod[, X[i]] <- as.Date(strptime(as.character(df_mod[, X[i]]), format="%m/%d/%Y %H:%M:%S %p")),
                 "2021-01-31 21:15:30 as 2021-01-31" = df_mod[, X[i]] <- as.Date(strptime(as.character(df_mod[, X[i]]), format="%Y-%m-%d %H:%M:%S")),
                 "31JAN2021:12:00:00 as 31JAN2021" = df_mod[, X[i]] <- as.Date(strptime(as.character(df_mod[, X[i]]), format="%d%b%Y:%H:%M:%S")),
                 "JAN2021" = df_mod[, X[i]] <- as.Date(paste0("01", as.character(df_mod[, X[i]]) ),  format="%d%b%Y"), 
                 "JAN21" = df_mod[, X[i]] <- as.Date(paste0("01", as.character(df_mod[, X[i]]) ), format="%d%b%y"), 
                 "JAN-2021" =  df_mod[, X[i]] <- as.Date(paste0("01-", as.character(df_mod[, X[i]]) ), format="%d-%b-%Y"),
                 "JAN-21" = df_mod[, X[i]] <- as.Date(paste0("-01", as.character(df_mod[, X[i]]) ), format="%d-%b-%y"),
                 "01/2021" = df_mod[, X[i]] <- as.Date(paste0("01/", as.character(df_mod[, X[i]]) ), format="%m/%d/%Y"),
                 "01/21" = df_mod[, X[i]] <- as.Date(paste0("01/", as.character(df_mod[, X[i]]) ), format="m/%d/%y"),
                 "01-2021" = df_mod[, X[i]] <- as.Date(paste0("01-", as.character(df_mod[, X[i]]) ), format="%m-%d-%Y"),
                 "01-21" = df_mod[, X[i]] <- as.Date(paste0("01-", as.character(df_mod[, X[i]]) ), format="%m-%d-%y"),
                 "2021-01" = df_mod[, X[i]] <- as.Date(paste0(as.character(df_mod[, X[i]]), "-01"), format="%Y-%m-%d"),
                 "21-01" = df_mod[, X[i]] <- as.Date(paste0(as.character(df_mod[, X[i]]) , "-01"), format="%y-%m-%d"),
                 "44227 in Excel" = df_mod[, X[i]] <- as.Date(df_mod[, X[i]], origin="1899-12-30")) 
        }
      }
      return(df_mod)
    }
    #Runs the function above
    modifiedDateDf <- reactive({
      if(input$ModifyDfYesNo == "Yes") {
        fncDateFmt(DF=modifySubsetDf(),  X=input$ModifyTimeX, Format= modifiedFullFormat() )
      }
    })
    
    ## Variables that are not modified ##
    non_modified_vars <- reactive({
      if(input$ModifyDfYesNo == "Yes") {
        setdiff(var(),  c(input$ModifyCharacter,input$ModifyFactor, 
                          input$ModifyNumeric, input$ModifyTimeX ))
      }
    })
    #Create modified dataset, without time variables
    modifiedDf1 <- reactive({
      #if(input$ModifiedDfSave == "Yes") {
        if(input$ModifyDfYesNo == "Yes") {
        cbind(modifySubsetDf()[, which(colnames(modifySubsetDf()) %in% non_modified_vars()), drop=FALSE], 
              modifiedCharDf(), modifiedFacDf(), modifiedNumDf(), modifiedDateDf() ) 
      }
    })
    
    ## FUnction to create time variable ##
    fncTimeCrt <- function(DF, X) {
      df_mod <- DF[, which(colnames(DF) %in% X), drop = FALSE]
      df_mod[, "Time"] <- as.numeric(difftime(DF[, X[2]] , DF[, X[1]], units = "days"))
      #Make final data frame
      df_mod <- df_mod[, "Time", drop = FALSE] 
      return(df_mod)
    }
    #Runs the function above
    modifiedTimeVarCrt <- reactive({
      if(input$ModAddTmYN == "Yes") {
        if( !is.null(input$Modify2VrTm) ) {
          fncTimeCrt(DF=modifiedDf1(),  X=input$Modify2VrTm )
        } else {
          #"No.Time.Var"= c(NA)
          data.frame("No.Time.Var"= rep(NA, nrow(modifiedDf1())))
        } 
      }
        })
    ## FUnction to create YYMM and Month variables ##
    fncYYMMmthCrt <- function(DF, X) {
      df_mod <- DF[, which(colnames(DF) %in% X), drop = FALSE]
      df_mod[, "YYMM"] <- (as.numeric(format(df_mod[, X], "%Y")) * 100) + as.numeric(format(df_mod[, X], "%m"))
      #Create Month
      df_mod[, "Month"] <- as.numeric(ordered( df_mod[, "YYMM"] ))
      #Make final data frame
      df_mod <- df_mod[, which(colnames(df_mod) %in% c("YYMM", "Month") )] 
      return(df_mod)
    }
    #Runs the function above
    modifiedYMmonthCrt <- reactive({
      if(input$ModAddTmYN == "Yes") {
        fncYYMMmthCrt(DF=modifiedDf1(),  X=input$ModifyTmAddMth )
      }
    })
    
    output$modified_df_name <- renderUI({ 
      textInput("ModifiedDfName", "14. Enter the data frame name.", 
                value= "mod_df")     
    })
    
    #Create modified dataset
    modifiedDf <- reactive({
      if(input$ModifiedDfSave == "Yes") {
      if(input$ModAddTmYN == "Yes") {
#        cbind(modifiedDf1(), modifiedTimeVarCrt(), modifiedYMmonthCrt() ) 
        cbind(modifiedDf1(), modifiedTimeVarCrt(), modifiedYMmonthCrt() ) 
      } else {
        modifiedDf1()
      }
      }
    })
    #Save data
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
      if (is.null(dataInput_txt()))  return()  else names(dataInput_txt())
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
      if (class(df()[,outcome()])[1] %in%  c("numeric","integer","labelled")) {
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
             "Proportion Y Logistic" = plot_yhat <- yhat,
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
    
################################################################################
    ###############################################
    ## Plot of Predictions by specific variables ##
    ###############################################
    ############
    ##   UI   ##
    ############
    #Select the predictors.
    output$opyx <- renderUI({                                 #Same idea as output$vy
      selectInput("opyxvar", "1. Select a predictor.", 
                  choices = predictor(), multiple=FALSE, selected=predictor()[1])
    })
    #Reactive function to the predictor
    opy_x_var <- reactive({                 
      input$opyxvar 
    })
    
    #2. Select whether to run the 95% confidence interval or not
    output$opy_strat_fac <- renderUI({                                
      selectInput("opyStrFac", "2. Want to group by a factor?",
                  choices = c("No", "Yes"),
                  selected="No")
    })
    ## Code for plot range
    #2. Range of X value
    opy_stratify_factor <- reactive({ 
      input$opyStrFac  
    })
    #3. Select the predictors.
    output$opyz <- renderUI({                                 #Same idea as output$vy
      selectInput("opyzvar", "3. Select optional factor.", 
                  choices = setdiff(predictor(), opy_x_var() ), multiple=FALSE, 
                  selected=setdiff(predictor(), opy_x_var())[1] )     
    })
    #3. Reactive function to get the factor
    opy_z_var <- reactive({                 
      input$opyzvar 
    })
    #Reactive function to get group levels
    opy_plot_groups <- reactive({
      if(opy_stratify_factor() == "Yes") {
        unique(df()[, opy_z_var() ]) 
      }
    })
    #4. Select specific groups
    output$opyplot_grp_levs <- renderUI({                                 
      if(length(predictor() )== 1) {
        selectInput("opyPlotGrpLvs", "4. Highlight specific groups?", 
                    choices = "NA", multiple=TRUE, selected= "NA" )     
      } else {
        selectInput("opyPlotGrpLvs", "4. Highlight specific groups?", 
                    choices = sort(opy_plot_groups()), multiple=TRUE, selected= sort(opy_plot_groups()) )     
      }
    })
    #4. Reactive function to get group levels
    opy_plot_Group_Levels <- reactive({                 
      input$opyPlotGrpLvs 
    })
    #4. Get number of levels for colors
    opy_color_level_number <- reactive({ 
      if(opy_stratify_factor == "No") {
        1:2
      } else {
         1:length(opy_plot_Group_Levels())
      }
    })
    #5. Select line colors
    output$opy_plot_ln_clrs <- renderUI({                                 
      if(length(predictor() )== 1) {
        selectInput("opyPltLnClr", "5. Select symbol colors.", 
                    choices = xyplot_Line_Color_Names(), multiple=TRUE, 
                    selected= xyplot_Line_Color_Names()[1:2 ] )     
      } else {
        selectInput("opyPltLnClr", "5. Select symbol colors.", 
                    choices = xyplot_Line_Color_Names(), multiple=TRUE, 
                    selected= xyplot_Line_Color_Names()[1:length(opy_plot_Group_Levels()) ] )     
      }
    })
#5. Reactive function for directly above
opy_plot_Line_Colors <- reactive({                 
  input$opyPltLnClr 
})
#6. Select the line label size
output$opy_plot_symbl_sz <- renderUI({                                 
  numericInput("opyPlsymblSz", "6. Select the symbol size.", 
               value = 2, min=0, step = .1)     
})
#6 Reactive function for directly above
opy_plot_symbol_size <- reactive({                 
  input$opyPlsymblSz 
})
#7. Add a target line
output$opy_Hor_Line <- renderUI({                                 
  textInput("opyHorLn", "7. Add a horizontal line.",
            value = paste0('c( ', ')') )
})
#7. Reactive function for above
opy_horizontal_line <- reactive({ 
  input$opyHorLn  
})
#8. Add a time point line
output$opy_Vert_Line <- renderUI({                                 
  textInput("opyVtPtLn", "8. Add a vertical line.",
            value = paste0('c( ', ')'))
})
#8. Reactive function for above
opy_vertical_line <- reactive({ 
  input$opyVtPtLn  
})
#9. Select target line color
output$opy_plot_tgt_ln_clrs <- renderUI({                                 
  selectInput("opyPltTLnClr", "9. Select the line's color.", 
              choices = xyplot_Line_Color_Names(), multiple=FALSE, selected="black")     
})
#9. Reactive function for directly above
opy_plot_Target_Line_Colors <- reactive({                 
  input$opyPltTLnClr 
})
#10. Select label size multiplier
output$opy_plot_lab_multi <- renderUI({                                 
  numericInput("opyPltLabMlt", "10. Increase label sizes.",
               value = 1, min=.01, step = .1)
})
#10. Reactive function for directly above
opy_plot_label_multiplier <- reactive({                 
  input$opyPltLabMlt 
})
#11. Legend location
output$opy_lgd_loc <- renderUI({                                
  selectInput("opyLgdLoc", "11. Select the legend location.",        
              choices = c("bottomright","bottom","bottomleft","left","topleft","top","topright","right","center"), 
              multiple=FALSE, selected="topleft" ) 
})
#11. Reactive function for legend location
opy_legend_location <- reactive({
  input$opyLgdLoc
})
#12. Select whether to run the 95% confidence interval or not
output$opy_create <- renderUI({                                
  selectInput("opyCreate", "12. Create the plot?",
              choices = c("No", "Yes"),
              selected="No")
})
## Code for plot range
#13. Range of X value
range_opyx_var <- reactive({ 
  range(as.numeric(df()[, opy_x_var()]), na.rm=TRUE )  
})
#14. Range of Y value
range_opyz_var <- reactive({ 
  range(as.numeric(df()[, outcome() ]), na.rm=TRUE )  
})
#13. Indicate lower limit of x-axis
output$opy__Xlim1 <- renderUI({
  numericInput("opyXLim1", "13. Lower X-axis limit.",
               value = range_opyx_var()[1], step = 1)
})
#13. Indicate lower limit of x-axis
opy__Xlim_val1 <- reactive({
  input$opyXLim1
})
#14. Indicate upper limit of x-axis
output$opy__Xlim2 <- renderUI({
  numericInput("opyXLim2", "14. Upper X-axis limit.",
               value = range_opyx_var()[2] , step = 1)
})
#14. Indicate upper limit of x-axis
opy__Xlim_val2 <- reactive({
  input$opyXLim2
})
#15. Indicate lower limit of y-axis
output$opy__Ylim1 <- renderUI({
  numericInput("opyYLim1", "15. Lower Y-axis limit.",
               value = range_opyz_var()[1], step = .1)
})
#15. Indicate lower limit of y-axis
opy__ylim_val1 <- reactive({
  input$opyYLim1
})
#16. Indicate upper limit of x-axis
output$opy__Ylim2 <- renderUI({
  numericInput("opyYLim2", "16. Upper Y-axis limit.",
               value = range_opyz_var()[2], step = .1)
})
#16. Indicate upper limit of x-axis
opy__ylim_val2 <- reactive({
  input$opyYLim2
})

#Get transformed predictions
trans_y_hat <- reactive({                  #This indicates the data frame I will use.
#  if(input$opyCreate == "Yes") {
    fncPredTrans(FIT=fit1(), RegType=input$regress_type)
#  }
})

#Observed and predcited plot 
plot_opy <- reactive({                  #This indicates the data frame I will use.
  if(input$opyCreate == "Yes") {
    fncPredAndY(FIT=fit1(), TransYhat=trans_y_hat(), TransYSpec=describeYhatHistRsltTrnsf(), X=opy_x_var(), 
                Y=outcome(), Strat.YN=opy_stratify_factor(), Z=opy_z_var(), ZLevs=opy_plot_Group_Levels(), DF=df(), 
                XYZCOL=opy_plot_Line_Colors(), CEX=opy_plot_symbol_size(), Legend.Loc=opy_legend_location(), 
                XLine=opy_vertical_line() , YLine=opy_horizontal_line() , ABLine.Col=opy_plot_Target_Line_Colors() ,
                xlim1=opy__Xlim_val1(), xlim2=opy__Xlim_val2(), ylim1=opy__ylim_val1(), ylim2=opy__ylim_val2(), 
                labMulti=opy_plot_label_multiplier(), reg_yhat=input$regress_type )
  }
})
#Observed and predcited plot 
output$observed_pred_scatter <- renderPlot({ 
  if(input$opyCreate == "Yes") {
    plot_opy()
  }
}, height = 800 )

output$residuals_by_group <- renderPrint({                                                 
  options(scipen=20)
  if (input$opyCreate == "Yes") {
    print(plot_opy() )
  }
})

######################################################
## Reactive functions that runs the functions below ##    
######################################################
#Function to get transformed predictions #
    fncPredTrans <- function(FIT, RegType) {
        #Transform scores
        Transformed.Yhat <- switch(RegType,                
                                   "Linear" = predict(FIT), 
                                   "Logistic" = plogis(predict(FIT)),
                                   "Proportion Y Logistic" = plogis(predict(FIT)),
                                   "Ordinal Logistic" = plogis(predict(FIT)),
                                   "Poisson" = exp(predict(FIT)),
                                   "Quantile" = predict(FIT),
                                   "Cox PH" = plogis(predict(FIT)),
                                   "Cox PH with censoring" = plogis(predict(FIT)),
                                   "AFT"  = exp(predict(FIT)),
                                   "AFT with censoring"     = exp(predict(FIT)),
                                   "Generalized Least Squares" = predict(FIT) )

        return("Transformed.Yhat"=Transformed.Yhat)
      }  

##############################################################
## Function for a Plot of Predictions by specific variables ##
##############################################################
    fncPredAndY <- function(FIT, TransYhat, TransYSpec, X, Y, Strat.YN, Z=NULL, ZLevs, DF, 
                            XYZCOL, CEX, Legend.Loc, XLine=NULL, YLine=NULL, ABLine.Col=NULL,
                            xlim1, xlim2, ylim1, ylim2, labMulti, reg_yhat) {
      #Get Transformed Yhat mean and mediam
      Trans.Yhat.Mean <- unlist(TransYSpec$Transformed.Yhat["Mean"])
      Trans.Yhat.Median <- unlist(TransYSpec$Transformed.Yhat[".50"])
      
      #Title
      if( Strat.YN== "No") {
        Main.Title <- paste0("Observed and Predicted (MN= ", round(Trans.Yhat.Mean, 2), 
                             ", MED=", round(Trans.Yhat.Median, 2),") ", Y, " values by ", X) 
      } else {
        Main.Title <- paste0("Observed and Predicted (MN= ", round(Trans.Yhat.Mean, 2), 
                             ", MED= ", round(Trans.Yhat.Median, 2),") ", Y, " values by ", X, " on ", Z) 
      }    
      #Get values for plotting
      SPECS <- specs(FIT, long=TRUE)
      #  YLIM <- range(DF[, Y])
      YLIM <- c(ylim1, ylim2)
      #  XLIM <- range(SPECS[["limits"]][X])
      #  XLIM <- as.numeric(SPECS[["limits"]][[X]][6:7])
      XLIM <- c(xlim1, xlim2)
      #Get number of levels for the factor
      if( Strat.YN== "No") {
        unique_levs <- NULL
      } else {
        unique_levs <- intersect( sort(unique(DF[, Z][ !is.na(DF[, Z]) ] )), ZLevs)
      }
      #Get the x-axis labels
      if( class(DF[, X]) %in% c("factor", "character")) {
        XAxisLab <- sort(unique(DF[, X][ !is.na(DF[, X]) ] ))
      } else {
        #  XAxisLab <- round(sort(unique(DF[, X][ !is.na(DF[, X]) ] )))
        XAxisLab <- NULL
      }
      
      #Create plot  
      if( Strat.YN== "No") {
        par(mar = c(6, 6, 3, 1) + 0.1)
        plot(as.numeric(DF[, X]), DF[, Y], col=XYZCOL[1], pch=1, xlim=XLIM, ylim=YLIM, cex= CEX, 
             ylab="", xlab="", axes=F )
        title(Main.Title, cex.main = 1.1*labMulti) 
        mtext(X, side = 1, line = 4, cex=1.1*labMulti )
        mtext(Y, side = 2, line = 4, cex=1.1*labMulti )
        axis(1, at= XAxisLab, labels= XAxisLab, las=1, cex.axis=1*labMulti )
        axis(2, las=3, cex.axis=1*labMulti)
        box()
#        points(DF[, X], TransYhat, col=XYZCOL[2], pch=3, cex= CEX  )
        points(DF[, X], TransYhat, col=tail(XYZCOL, 1), pch=3, cex= CEX  )
        abline(v=as.numeric(eval(parse(text=XLine )) ), col=ABLine.Col, lwd=2*CEX)
        abline(h=as.numeric(eval(parse(text=YLine )) ), col=ABLine.Col, lwd=2*CEX)
      } else {
        par(mar = c(6, 6, 3, 1) + 0.1)
        plot(as.numeric(DF[, X]), DF[, Y],  
             type="n", xlim=XLIM, ylim=YLIM, cex= CEX, ylab="", xlab="", axes=F )
        for (i in 1:length(unique_levs )) {
          title(Main.Title, cex.main = 1.1*labMulti) 
          mtext(X, side = 1, line = 4, cex=1.1*labMulti )
          mtext(Y, side = 2, line = 4, cex=1.1*labMulti )
          axis(1, at= XAxisLab, labels= XAxisLab, las=1, cex.axis=1*labMulti )
          axis(2, las=3, cex.axis=1*labMulti)
          box()
          points(DF[, X][ DF[, Z]== unique_levs[i] ], DF[, Y][ DF[, Z]== unique_levs[i] ], 
                 col=XYZCOL[i], pch=1, cex= CEX )
          points(DF[, X][ DF[, Z]== unique_levs[i] ], TransYhat[ DF[, Z]== unique_levs[i] ], 
                 col=XYZCOL[i], pch=3, cex= CEX )
          abline(v=as.numeric(eval(parse(text=XLine )) ), col=ABLine.Col, lwd=2*CEX)
          abline(h=as.numeric(eval(parse(text=YLine )) ), col=ABLine.Col, lwd=2*CEX)
        }
        
      }
      ###Legend###
      #Get legend levels 
      if( Strat.YN== "No") {
        legend_levs <- NULL
      } else {
        legend_levs <- intersect(levels(as.factor(DF[, Z][ !is.na(DF[, Z]) ] )) , as.character(unique_levs))
      }
      #This creates a legend for the ablines with and without factors. 
      if ( Strat.YN== "No" ) {  
        #legend(x=Legend.Loc, legend=c("Observed", paste0("Predicted ", "(RES MN= ", round(resid_levs, 3), ")") ),
        legend(x=Legend.Loc, legend=c("Observed", "Predicted"),
               col=XYZCOL,
               pch=c(1, 3),
               lty= 0, lwd= 3, cex = 1.5, bty="n" #, inset=c(0, .05)
        )
      } else {  
        #legend(Legend.Loc, legend=c("Observed", "Predicted", paste(legend_levs, "RES MN=", round(resid_levs, 3))),
        legend(x=Legend.Loc, legend=c("Observed", "Predicted", intersect(legend_levs, ZLevs) ),
               col= c(1,1, XYZCOL),
               pch=c(1, 3, rep(3, length(ZLevs))),
               lty= 0, 
               lwd= 3, cex = 1.5, bty="n")
      }
      #Get Residuals by levels 
      if( Strat.YN== "No") {
        resid_levs <- c(mean(resid(FIT), na.rm=TRUE), median(resid(FIT), na.rm=TRUE))
      } else {   #naresid() allows me to have residuals and NAs for rows with missing variables/residuals
#        resid_levs <- cbind(by(naresid(FIT$na.action, resid(FIT))[!duplicated(names(naresid(FIT$na.action, resid(FIT))))] , DF[, Z], mean, na.rm=TRUE), 
#                            by(naresid(FIT$na.action, resid(FIT))[!duplicated(names(naresid(FIT$na.action, resid(FIT))))] , DF[, Z], median, na.rm=TRUE))
        if( reg_yhat %in% c("Ordinal Logistic", "Logistic") ) {
          resid_levs <- cbind(by(naresid(FIT$na.action, resid(FIT))[!duplicated(as.character(1:nrow(DF)) )] , DF[, Z], mean, na.rm=TRUE), 
                            by(naresid(FIT$na.action, resid(FIT))[!duplicated(as.character(1:nrow(DF)) )] , DF[, Z], median, na.rm=TRUE))
        } else {  
          resid_levs <- cbind(by(naresid(FIT$na.action, resid(FIT))[!duplicated(names(naresid(FIT$na.action, resid(FIT))))] , DF[, Z], mean, na.rm=TRUE),
                              by(naresid(FIT$na.action, resid(FIT))[!duplicated(names(naresid(FIT$na.action, resid(FIT))))] , DF[, Z], median, na.rm=TRUE))
        } 
      }
      #Get Residuals by levels 
      if( Strat.YN== "No") {
        names(resid_levs) <- c("Mean of residuals", "Median of residuals")
      } else {
        colnames(resid_levs) <- c(paste0("Mean residual by ", Z), 
                              paste0("Median residual by ", Z))
      }
      return(resid_levs)
    }
    
################################################################################
    
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
            
            switch(input$regress_type,                                          #replaced "as.formula(input$up_fmla)" with Upd_mdl_fmla()
                   "Linear"   = if(input$updy == "Yes") {
                     ols(Upd_mdl_fmla(), x=TRUE, y=TRUE, data=df(), weights= eval(parse(text= weighting_regression_var())) )
                   } else {
                     ols(mdl_fmla(), x=TRUE, y=TRUE, data=df(), weights= eval(parse(text= weighting_regression_var())))},
                   "Logistic" = if(input$updy == "Yes") {
                     lrm(Upd_mdl_fmla(), x=TRUE, y=TRUE, data=df(), tol=1e-100, maxit=20, 
                         weights= eval(parse(text= weighting_regression_var())) ) #I added tol value so it can handle time predictor (YYMM)
                   } else { #Added maxit to handle small samples. See https://stat.ethz.ch/pipermail/r-help/2017-September/449115.html  
                     lrm(mdl_fmla(), x=TRUE, y=TRUE, data=df(), tol=1e-100, maxit=20, 
                         weights= eval(parse(text= weighting_regression_var())))},  #I added tol value so it can handle time predictor that causes "singularity"
                   "Proportion Y Logistic" = if(input$updy == "Yes") {
                     Glm(Upd_mdl_fmla(), x=TRUE, y=TRUE, data=df(), family=binomial(), weights= eval(parse(text= weighting_regression_var())))
                   } else {
                     Glm(mdl_fmla(), x=TRUE, y=TRUE, data=df(), family=binomial(), weights= eval(parse(text= weighting_regression_var())) )},
                   "Ordinal Logistic" = if(input$updy == "Yes") {
                     orm(Upd_mdl_fmla(), x=TRUE, y=TRUE, data=df())
                   } else {
                     orm(mdl_fmla(), x=TRUE, y=TRUE, data=df())},
                   "Poisson" = if(input$updy == "Yes") {
                     Glm(Upd_mdl_fmla(), x=TRUE, y=TRUE, data=df(), family=poisson(), weights= eval(parse(text= weighting_regression_var())))
                   } else {
                     Glm(mdl_fmla(), x=TRUE, y=TRUE, data=df(), family=poisson(), weights= eval(parse(text= weighting_regression_var())) )},
                   "Quantile" = if(input$updy == "Yes") {
                     Rq(Upd_mdl_fmla(), x=TRUE, y=TRUE, data=df(), tau=as.numeric(rq_tau1()), weights= eval(parse(text= weighting_regression_var())) )
                   } else {
                     Rq(mdl_fmla(), x=TRUE, y=TRUE, data=df(), tau=as.numeric(rq_tau1()), weights= eval(parse(text= weighting_regression_var())) )},
                   "Cox PH"   = if(input$updy == "Yes") {
                     cph(cox_mdl_fmla1u(), x=TRUE, y=TRUE, data=df(), surv=TRUE, weights= eval(parse(text= weighting_regression_var())))
                   } else {
                     cph(cox_mdl_fmla1(), x=TRUE, y=TRUE, data=df(), surv=TRUE, weights= eval(parse(text= weighting_regression_var())))},
                   "Cox PH with censoring" = if(input$updy == "Yes") {
                     cph(cox_mdl_fmla2u(), x=TRUE, y=TRUE, data=df(), surv=TRUE, weights= eval(parse(text= weighting_regression_var())))
                   } else {
                     cph(cox_mdl_fmla2(), x=TRUE, y=TRUE, data=df(), surv=TRUE, weights= eval(parse(text= weighting_regression_var())))},
                   #AFT models
                   "AFT"   = if(input$updy == "Yes") {
                     psm(aft_mdl_fmla1u(), data=df(), x=TRUE, y=TRUE, dist=AFT_PSM_Dist(), weights= eval(parse(text= weighting_regression_var())) )
                   } else {
                     psm(aft_mdl_fmla1(), data=df(), x=TRUE, y=TRUE, dist=AFT_PSM_Dist(), weights= eval(parse(text= weighting_regression_var())) )},
                   "AFT with censoring" = if(input$updy == "Yes") {
                     psm(aft_mdl_fmla2u(), data=df(),  x=TRUE, y=TRUE, dist=AFT_PSM_Dist(), weights= eval(parse(text= weighting_regression_var())) )
                   } else {
                     psm(aft_mdl_fmla2(), x=TRUE, y=TRUE, data=df(), dist=AFT_PSM_Dist(), weights= eval(parse(text= weighting_regression_var())) )},
                   "Generalized Least Squares" = if(input$updy == "Yes") {
                     Gls(Upd_mdl_fmla(), x=TRUE, data=df(), correlation=corCompSymm(form= gls_cor()), weights= eval(parse(text= weighting_regression_var())) )
                     #Gls(as.formula(input$up_fmla), x=TRUE, data=df(), correlation=corCompSymm(form= gls_cor()))
                   } else {
                     Gls(mdl_fmla(), x=TRUE, data=df(), correlation=corCompSymm(form= gls_cor()), 
                         weights= eval(parse(text= weighting_regression_var())) )}
                   #correlation= do.call("corCAR1", list(form=~week|uid)) )})
                   #as.formula(paste(paste0("~"), paste(gls_clst1(), collapse= "|")))
            )
          } else {
            
            #        if (input$MIbegin == "Yes") {
            
            switch(input$regress_type,
                   "Linear"   = if(input$updy == "Yes") {
                     fit.mult.impute(Upd_mdl_fmla(), ols, mi(), data=df(), pr=FALSE)
                   } else {
                     fit.mult.impute(mdl_fmla(), ols, mi(), data=df(), pr=FALSE)},
                   "Logistic" = if(input$updy == "Yes") {
                     fit.mult.impute(Upd_mdl_fmla(), lrm, mi(), data=df(), pr=FALSE, tol=1e-100)
                   } else {
                     fit.mult.impute(mdl_fmla(), lrm, mi(), data=df(), pr=FALSE)},
                   "Proportion Y Logistic" = if(input$updy == "Yes") {
                     fit.mult.impute(Upd_mdl_fmla(), Glm, mi(), data=df(), pr=FALSE, family=binomial())
                   } else {
                     fit.mult.impute(mdl_fmla(), Glm, mi(), data=df(), pr=FALSE)},
                   "Ordinal Logistic" = if(input$updy == "Yes") {
                     fit.mult.impute(Upd_mdl_fmla(), orm, mi(), data=df(), pr=FALSE)
                   } else {
                     fit.mult.impute(mdl_fmla(), orm, mi(), data=df(), pr=FALSE)},
                   "Poisson" = if(input$updy == "Yes") {
                     fit.mult.impute(Upd_mdl_fmla(), Glm, mi(), data=df(), pr=FALSE, family=poisson())
                   } else {
                     fit.mult.impute(mdl_fmla(), Glm, mi(), data=df(), pr=FALSE, family=poisson())},
                   "Quantile" = if(input$updy == "Yes") {
                     fit.mult.impute(Upd_mdl_fmla(), Rq, mi(), data=df(), pr=FALSE, tau=as.numeric(rq_tau1()))
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
                     fit.mult.impute(Upd_mdl_fmla(), Gls, mi(), data=df(), pr=FALSE, correlation=corCAR1(form= gls_cor()))
                   } else {
                     fit.mult.impute(mdl_fmla(), Gls, mi(), data=df(), pr=FALSE, correlation=corCAR1(form= gls_cor()))}
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
                              "Proportion Y Logistic",
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

    #10. Enter a weight variable.
    output$weighting_reg <- renderUI({                                 
      textInput("weight_var", "10. Enter a weight variable.")     
    })
    
    #10A. Enter a weight variable.
    weighting_regression_var <- reactive({         
      input$weight_var
    })
    
    
    #Indicate if there should be splines.
    output$rcs_yes <- renderUI({                                 #Same idea as output$vy
      selectInput("rcsy", "11. Do you want splines?", 
                  choices = c("No", "Yes"), multiple=FALSE, selected="No")     #Will make choices based on my reactive function.
    })
    
    #Select the variables that will have splines.
    output$rx <- renderUI({                                 #Same idea as output$vy
      selectInput("rcs_X", "12. Select the spline variables (continuous only)", 
                  choices = predictor(), multiple=TRUE, selected=predictor()[1])     #Will make choices based on my reactive function.
    })

    #Indicate if you should update the model.
    output$update_yes <- renderUI({                                 #Same idea as output$vy
      selectInput("updy", "13. Do you want to update the model formula?", 
                  choices = c("No", "Yes"), multiple=FALSE, selected="No")     #Will make choices based on my reactive function.
    })
    
    #13. Update the model formula.
    output$uf <- renderUI({                                 #Same idea as output$vy
      textInput("up_fmla", "14. Update formula (Interactions: Change \"+\" to \'*\', strat(X) to stratify CPH).", 
                value= deparse(mdl_fmla(), width.cutoff=500 ))     #Will make choices based on my reactive function.
    })
    #13A. Quantile N object
    Upd_mdl_fmla <<- reactive({
      as.formula(input$up_fmla)
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
    
    #Model AIC, needed to revise for Poisson
    fit1_AIC <- reactive({
      if( input$regress_type == "Poisson") {
        print( fit1()$aic )
      } else {
        print( unname(AIC(fit1())) ) 
      } 
    }) 
    
    fncFt1Aic <- function(Model, RegTyp) {
      if( RegTyp %in% c("Poisson", "Proportion Y Logistic")) {
         Model$aic 
      } else {
         unname(AIC(Model ))  
      } 
      
    }

    #Regression results.
    output$regress <- renderPrint({                                                
      atch()
      if(MsStrat0() ==1) {
        list( "Model"=fit1(), "AIC"= fncFt1Aic(Model=fit1(), RegTyp=input$regress_type) )
      } else {   
        if( input$regress_type %in% c("Logistic", "Proportion Y Logistic","Ordinal Logistic", "Poisson", "Cox PH", "Cox PH with censoring")) {
          list("Model"=fit1(), "Exponentiated Coefficients"= exp(fit1()[["coefficients"]]), "AIC"= fncFt1Aic(Model=fit1(), RegTyp=input$regress_type) ) 
        } else {
#          list("Model"=fit1(), "AIC"= fit1_AIC() )
          list("Model"=fit1(), "AIC"= fncFt1Aic(Model=fit1(), RegTyp=input$regress_type) )
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

#Get model specifications
model_specs_long <- reactive({         
  if (input$begin_mdl == "Yes") {
    specs(fit1(), long=TRUE)  #Summary of model fit.
  }
})
#Print model specifications
output$specifications <- renderPrint({   
      if (input$begin_mdl == "Yes") {
        model_specs_long()  #Summary of model fit.
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
      list( "Predicted.Values"=try( describeYhatHistRslt() ),
            "Transformed.Yhat"=try(describeYhatHistRsltTrnsf()[["Transformed.Yhat"]] ),
            "Transformed.Full.Range"=try(describeYhatHistRsltTrnsf()[["Transformed.Full.Range"]]),
            "Variation"=c("Standard.Deviation.Yhat"= sd(yhat_hist_rslt(), na.rm=TRUE), 
              "Coefficient.of.Variation"= sd(yhat_hist_rslt(), na.rm=TRUE)/mean(yhat_hist_rslt(), na.rm=TRUE)), 
            "Mean Absolute Deviation" = fncMAD(DF= df(), Y= outcome(), Trans.Y=trans_y_hat(), Reg.Meth= input$regress_type)
      )
    })

################################
## Function to transform Yhat ##
################################
fncTrnsfYhatSmry <- function(YhatRslt, RegType) {
  #Excluded values
  excld_describe <- c("n", "missing", "distinct", "Info")
  #Transform scores
  Transformed.Yhat <- switch(RegType,                
                             "Linear" = as.numeric(YhatRslt$counts[setdiff(names(YhatRslt$counts), excld_describe) ]), 
                             "Logistic" = plogis(as.numeric(YhatRslt$counts[setdiff(names(YhatRslt$counts), excld_describe) ])),
                             "Proportion Y Logistic" = plogis(as.numeric(YhatRslt$counts[setdiff(names(YhatRslt$counts), excld_describe) ])),
                             "Ordinal Logistic" = plogis(as.numeric(YhatRslt$counts[setdiff(names(YhatRslt$counts), excld_describe) ])),
                             "Poisson" = exp(as.numeric(YhatRslt$counts[setdiff(names(YhatRslt$counts), excld_describe) ])),
                             "Quantile" = as.numeric(YhatRslt$counts[setdiff(names(YhatRslt$counts), excld_describe) ]),
                             "Cox PH" = plogis(as.numeric(YhatRslt$counts[setdiff(names(YhatRslt$counts), excld_describe) ])),
                             "Cox PH with censoring" = plogis(as.numeric(YhatRslt$counts[setdiff(names(YhatRslt$counts), excld_describe) ])),
                             "AFT"  = exp(as.numeric(YhatRslt$counts[setdiff(names(YhatRslt$counts), excld_describe) ])) ,
                             "AFT with censoring"     = exp(as.numeric(YhatRslt$counts[setdiff(names(YhatRslt$counts), excld_describe) ])) ,
                             "Generalized Least Squares" = as.numeric(YhatRslt$counts[setdiff(names(YhatRslt$counts), excld_describe) ]) )
  #Add names
  names(Transformed.Yhat) <- switch(RegType,                
                                    "Linear" = setdiff(names(YhatRslt$counts), excld_describe), 
                                    "Logistic" = setdiff(names(YhatRslt$counts), excld_describe),
                                    "Proportion Y Logistic" = setdiff(names(YhatRslt$counts), excld_describe),
                                    "Ordinal Logistic" = setdiff(names(YhatRslt$counts), excld_describe),
                                    "Poisson" = setdiff(names(YhatRslt$counts), excld_describe),
                                    "Quantile" = setdiff(names(YhatRslt$counts), excld_describe),
                                    "Cox PH" = setdiff(names(YhatRslt$counts), excld_describe),
                                    "Cox PH with censoring" = setdiff(names(YhatRslt$counts), excld_describe),
                                    "AFT"  = setdiff(names(YhatRslt$counts), excld_describe),
                                    "AFT with censoring"     = setdiff(names(YhatRslt$counts), excld_describe),
                                    "Generalized Least Squares" = setdiff(names(YhatRslt$counts), excld_describe) )
  #Transform range of lowest and highest values
  Transformed.Full.Range <- switch(RegType,                
                             "Linear" = range(as.numeric(YhatRslt$extremes) ), 
                             "Logistic" = range(plogis(as.numeric(YhatRslt$extremes)) ),
                             "Proportion Y Logistic" = range(plogis(as.numeric(YhatRslt$extremes)) ),
                             "Ordinal Logistic" = range(plogis(as.numeric(YhatRslt$extremes)) ),
                             "Poisson" = range(exp(as.numeric(YhatRslt$extremes)) ),
                             "Quantile" = range(as.numeric(YhatRslt$extremes) ),
                             "Cox PH" = range(plogis(as.numeric(YhatRslt$extremes)) ),
                             "Cox PH with censoring" = range(plogis(as.numeric(YhatRslt$extremes)) ),
                             "AFT"  = range(exp(as.numeric(YhatRslt$extremes)) ),
                             "AFT with censoring"     = range(exp(as.numeric(YhatRslt$extremes)) ),
                             "Generalized Least Squares" = range(as.numeric(YhatRslt$extremes) ) )
  
  return(list("Transformed.Yhat"=Transformed.Yhat, "Transformed.Full.Range"= Transformed.Full.Range))
}  

## Function to get Mean Absolute Deviation ##
    fncMAD <- function(DF, Y, Trans.Y, Reg.Meth) {
      MAD <- switch(Reg.Meth,                
                                 "Linear" =     mean(abs(DF[,Y] - Trans.Y ), na.rm=T), 
                                 "Logistic" = NA,
                                 "Proportion Y Logistic" = NA,
                                 "Ordinal Logistic" = NA,
                                 "Poisson" =     mean(abs(DF[,Y] - Trans.Y ), na.rm=T),
                                 "Quantile" =     mean(abs(DF[,Y] - Trans.Y ), na.rm=T),
                                 "Cox PH" = NA,
                                 "Cox PH with censoring" = NA,
                                 "AFT"  = NA ,
                                 "AFT with censoring"     = NA ,
                                 "Generalized Least Squares" =     mean(abs(DF[,Y] - Trans.Y ), na.rm=T) )

      return(MAD)
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
  numericInput("PredClassTime", "2. Select a survival model time.", 
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
  selectInput("useClsPrModelYN", "4. Use the prior model fit?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")  
  })

  #4A. Object for classification plot 
  use_class_prior_model_YN <- reactive({
    input$useClsPrModelYN
  })

  #5. Select TRUE bar colors
  output$pred_class_t_br_clrs <- renderUI({                                 
    selectInput("prClTBrClr", "5. Select 'True' bar colors.", 
                choices = xyplot_Line_Color_Names(), multiple=FALSE, selected= "green")     
  })
  #5a. Reactive function for directly above
  pred_class_T_Bar_Colors <- reactive({                 
    input$prClTBrClr 
  })
  #6. Select FALSE bar colors
  output$pred_class_f_br_clrs <- renderUI({                                 
    selectInput("prClFBrClr", "6. Select 'False' bar colors.", 
                choices = xyplot_Line_Color_Names(), multiple=FALSE, selected= "red")     
  })
  #6a. Reactive function for directly above
  pred_class_F_Bar_Colors <- reactive({                 
    input$prClFBrClr 
  })
  
  #7. Select threshold line colors
  output$pred_class_ln_clrs <- renderUI({                                 
    selectInput("prClLnClr", "7. Set threshold line color.", 
                choices = xyplot_Line_Color_Names(), multiple=FALSE, selected= "blue")     
  })
  #7a. Reactive function for directly above
  pred_class_Line_Colors <- reactive({                 
    input$prClLnClr 
  })
  
  #8. Select line width
  output$pred_class_ln_wdth <- renderUI({                                 
    numericInput("prClLnWd", "8. Set threshold line width.", 
                 value = 2, min=0, step = 1)     
  })
  #8a. Reactive function for directly above
  pred_class_Line_Width <- reactive({                 
    input$prClLnWd 
  })
  
#9. Select the approximate number of histogram bars
output$pred_class_hist_bars <- renderUI({                                 
  numericInput("PredClassHistBars", "9. Select the approximate number of histogram bars.", 
               value = 15, step = 1, min=2)     
})
#9A. Object for histogram bars 
prediction_class_histogram_bars <- reactive({
#  if (input$begin_mdl == "Yes") {
    input$PredClassHistBars
#  }
})

#10. Select a survival model time (e.g., 7 days)
output$class_hist_asp_ratio <- renderUI({                                 
  selectInput("clsHistAspRtio", "10. Do you want both y-axes on the same scale?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     
})
#10A. Object for survival model time 
class_histogram_aspect_ratio <- reactive({
#  if (input$begin_mdl == "Yes") {
    input$clsHistAspRtio
#  }
})
#11. Indicate if you want the classification plot
output$pred_class_hist_yesno <- renderUI({                                 
  selectInput("PredClassHistYN", "11. Do you want to run the classification plot?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     
})
#11A. Object for classification plot 
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
                     PredTime=prediction_classification_time(), RegType=input$regress_type, DF=df(), OffSetName= mdl_off_set_output()$offset_variable_name)
    } else {
      fncYhatClassDf(Fit= fit1(), Y=outcome(),
                     Threshold=prediction_class_threshold(), Censor=censor1(),
                     PredTime=prediction_classification_time(), RegType=input$regress_type, DF=df(), OffSetName= mdl_off_set_output()$offset_variable_name)
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
                    Brks=prediction_class_histogram_bars(), RegType= input$regress_type, 
                    aspectRatio=class_histogram_aspect_ratio(), TBar=pred_class_T_Bar_Colors(), 
                    FBar=pred_class_F_Bar_Colors(), ThreshCol=pred_class_Line_Colors(), ThreshSize=pred_class_Line_Width() 
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
#2. Legend location
output$dca_lgd_loc <- renderUI({                                
  selectInput("dcaLgdLoc", "2. Select the legend location.",        
              choices = c("bottomright","bottom","bottomleft","left","topleft","top","topright","right","center"), 
              multiple=FALSE, selected="topright" ) 
})
#2. Reactive function for legend location
dca_legend_location <- reactive({
  input$dcaLgdLoc
})
#3. Select line colors
output$dca_plot_ln_clrs <- renderUI({                                 
  selectInput("dcaPltLnClr", "3. Select line colors.", 
              choices = xyplot_Line_Color_Names(), multiple=TRUE,
              selected= xyplot_Line_Color_Names()[c(4,7,8)] )     
})
#3a. Reactive function for directly above
dca_plot_Line_Colors <- reactive({                 
  input$dcaPltLnClr 
})
#4. Select line width
output$dca_ln_wdth <- renderUI({                                 
  numericInput("dcaLnWd", "4. Select line width.", 
               value = 2, min=0, step = 1)     
})
#4a. Reactive function for directly above
dca_Line_Width <- reactive({                 
  input$dcaLnWd 
})

#12. Indicate if you want the classification plot
output$decison_curve_anly_yesno <- renderUI({                                 
  selectInput("decisCrvAnlYN", "9. Do you want to run the decision curve analysis?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     
})
#12A. Object for classification plot 
#prediction_class_histogram_yes_no
decison_curve_analysis_yes_no <- reactive({
  input$decisCrvAnlYN
})
#Run functions below
#14. Get data for functions 
get_thresh_quant_df <- reactive({
  if (decison_curve_analysis_yes_no() =="Yes") {
    if (use_class_prior_model_YN() =="Yes") {
      fncThreshQntl(Fit=class_prior_model_fit_name(), Y=outcome(), Threshold=describeYhatHistRslt(), Censor=censor1(), 
                    PredTime=prediction_classification_time(), RegType=input$regress_type, DF=df(), OffSetName=mdl_off_set_output()$offset_variable_name)
    } else {
      fncThreshQntl(Fit=fit1(), Y=outcome(), Threshold=describeYhatHistRslt(), Censor=censor1(), 
                    PredTime=prediction_classification_time(), RegType=input$regress_type, DF=df(), OffSetName=mdl_off_set_output()$offset_variable_name)
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
                  xlim1=descion_crv_plt_xlim1(), xlim2=descion_crv_plt_xlim2(), 
                  ylim1=descion_crv_plt_ylim1(), ylim2=descion_crv_plt_ylim2(), 
                  Legend.Loc=dca_legend_location(), LCol=dca_plot_Line_Colors(), LSize=dca_Line_Width() )
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
  numericInput("dc_Xlim1", "5. Lower X-axis limit.",
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
  numericInput("dc_Xlim2", "6. Upper X-axis limit.",
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
  numericInput("dc_Ylim1", "7. Lower Y-axis limit.",
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
  numericInput("dc_Ylim2", "8. Upper Y-axis limit.",
               value = 1, step = .1)
})
#19A. Set up Decision Curve
descion_crv_plt_ylim2 <- reactive({
  if (decison_curve_analysis_yes_no() =="Yes") {
    input$dc_Ylim2
  }
})
#20. Run the print function 
output$get_decision_curve_out <- renderPrint({
  if (decison_curve_analysis_yes_no() =="Yes") {
    thresh_quant_data_output()[-1]
  }
})

################
##  Functions ##
################

##############
## Get data ##
##############
fncYhatClassDf <- function(Fit, Y, Threshold, Censor=NULL, PredTime=NULL, RegType, DF, OffSetName)  {
  tm1 <- Fit
  atime <- PredTime
  tdf <- DF
  tY <- Y
  tcensor <- Censor
#  PREDrange <- range(predict(Fit), na.rm=T)  
  if(RegType == "Poisson" & length(OffSetName) == 0 ) {
    PREDrange <- range(predict(Fit), na.rm=T) 
  } else {
#    PREDrange <- range(exp(predict( Fit, newdata=DF) )* mean(DF[, OffSetName]), na.rm=T)
    PREDrange <- range(predict( Fit, newdata=DF), na.rm=T )
  } 
  # Need to make object for this so I can calculate the trapezoid AUC:
  threshLev <- Threshold
  #Get predictions for values at threshold
  #Make data for predict()
  newtdf1 <- switch(RegType,                
                    "Linear"   = tdf[ tdf[,  tY] >= threshLev, ], 
                    "Logistic" = tdf[ tdf[,  tY] == 1, ],
                    "Proportion Y Logistic" = tdf[ tdf[,  tY] == 1, ],
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
                    "Proportion Y Logistic" = tdf[ tdf[,  tY] == 0, ],
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
                                "Proportion Y Logistic" = plogis(threshLev),
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
  pr_table1 <- prop.table(table(factor(round(pm1, 10) >= round(threshLev, 10), levels=c("FALSE","TRUE") )))
  pr_table2 <- prop.table(table(factor(round(pm2, 10) >= round(threshLev, 10), levels=c("FALSE","TRUE") )))
  #Sensitivity and 1 - specificity
  propAbovMY1 <-  pr_table1["TRUE"]  #Sensitivity
  fls_Neg <-  pr_table1["FALSE"]  #FALSE negative
  propAbovMY0 <- pr_table2["TRUE"]  #1-specificity or false-positive
  specifity <-  pr_table2["FALSE"]  #Specificity
#Get frequencies
  f_table1 <- table(factor(round(pm1, 10) >= round(threshLev, 10), levels=c("FALSE","TRUE") ))
  f_table2 <- table(factor(round(pm2, 10) >= round(threshLev, 10), levels=c("FALSE","TRUE") ))
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
fncYhatClassPlt <- function(ClassDF, AUC, Brks, RegType, aspectRatio,
                            TBar, FBar, ThreshCol, ThreshSize)  {
  par(mfrow=c(2,1))
#  xlimMin <- Yhat[["extremes"]][1]
#  xlimMax <- Yhat[["extremes"]][10]
  xlimMin <- ClassDF$senspcXmin
  xlimMax <- ClassDF$senspcXmax
  #Sensitivity and 1-specificity
  Sens.Value <- switch(RegType,                
                        "Linear"   = round(ClassDF$propAbovMY1, 3), 
                       "Logistic" = round(ClassDF$propAbovMY1, 3),
                       "Proportion Y Logistic" = round(ClassDF$propAbovMY1, 3),
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
                       "Proportion Y Logistic" = round(ClassDF$propAbovMY0, 3),
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
                                "Linear"   = c(FBar, TBar), 
                                "Logistic" = c(FBar, TBar),
                        "Proportion Y Logistic" = c(FBar, TBar),
                        "Ordinal Logistic"  = c(FBar, TBar),
                                "Poisson"  = c(FBar, TBar),
                                "Quantile" = c(FBar, TBar),
                                "Cox PH"   = c(FBar, TBar),
                                "Cox PH with censoring"  = c(FBar, TBar),
                                "AFT"  = c(TBar, FBar),
                                "AFT with censoring"     = c(TBar, FBar),
                                "Generalized Least Squares" = c(FBar, TBar) )
  Bar.Colors2 <- switch(RegType,                
                        "Linear"   = c(TBar, FBar), 
                        "Logistic" = c(TBar, FBar),
                        "Proportion Y Logistic" = c(TBar, FBar),
                        "Ordinal Logistic"  = c(TBar, FBar),
                        "Poisson"  = c(TBar, FBar),
                        "Quantile" = c(TBar, FBar),
                        "Cox PH"   = c(TBar, FBar),
                        "Cox PH with censoring"  = c(TBar, FBar),
                        "AFT"  = c(FBar, TBar),
                        "AFT with censoring"     = c(FBar, TBar),
                        "Generalized Least Squares" = c(TBar, FBar) )
  #AUC value
  AUC_val <- switch(RegType,                
                    "Linear"   = round(AUC, 3), 
                    "Logistic" = round(AUC, 3),
                    "Proportion Y Logistic" = round(AUC, 3),
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
  abline(v=ClassDF$threshLev, lwd=ThreshSize, col=ThreshCol)
  plot(h2, col=Bar.Colors2[cuts2], xlim=c(xlimMin, xlimMax), ylim=c(0, tail(hyMAX, 1)),
       main=paste0("Outcome = No. (n = ", sum(ClassDF$N.specifity, ClassDF$N.AbovMY0),"). Proportion of predictions at or above cutoff: ", Spec.Value, ".  AUC = ", AUC_val, "."  ),
       xlab=paste0("1 - Specificity: False-Positives in red using a cutoff of ", ClassDF$threshLev, " (Transformed = ", round(ClassDF$Transform.Threshold, 3), ")." ))
  abline(v=ClassDF$threshLev, lwd=ThreshSize, col=ThreshCol)
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
                    "Proportion Y Logistic" = unname(ClassDF$propAbovMY1),
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
                      "Proportion Y Logistic" = unname(ClassDF$specifity),
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
                      "Proportion Y Logistic" = unname(ClassDF$propAbovMY0),
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
                    "Proportion Y Logistic" = unname(ClassDF$fls_Neg),
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
                      "Proportion Y Logistic" = unname(ClassDF$N.AbovMY1),
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
                      "Proportion Y Logistic" = unname(ClassDF$N.specifity),
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
                      "Proportion Y Logistic" = unname(ClassDF$N.AbovMY0),
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
                    "Proportion Y Logistic" = unname(ClassDF$N.fls_Neg),
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
                      "Proportion Y Logistic" = plogis(ClassDF$threshLev),
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
fncThreshQntl <- function(Fit, Y, Threshold, Censor=NULL, PredTime=NULL, RegType, DF, OffSetName) {
  #Get sensitivity and specificity for IQR of predicted values
  #Low 5: 5th lowest value
  yClass.L5 <-  try(fncYhatClassDf(Fit=Fit, Y=Y, Threshold=as.numeric(Threshold[["extremes"]][5]), 
                               Censor=Censor, PredTime=PredTime, RegType=RegType, DF=DF, OffSetName=OffSetName))
  yClass.05 <-  try(fncYhatClassDf(Fit=Fit, Y=Y, Threshold=as.numeric(Threshold[["counts"]][7]), 
                               Censor=Censor, PredTime=PredTime, RegType=RegType, DF=DF, OffSetName=OffSetName))
  yClass.10 <-  try(fncYhatClassDf(Fit=Fit, Y=Y, Threshold=as.numeric(Threshold[["counts"]][8]), 
                               Censor=Censor, PredTime=PredTime, RegType=RegType, DF=DF, OffSetName=OffSetName))
  yClass.25 <-  try(fncYhatClassDf(Fit=Fit, Y=Y, Threshold=as.numeric(Threshold[["counts"]][9]), 
                               Censor=Censor, PredTime=PredTime, RegType=RegType, DF=DF, OffSetName=OffSetName))
  yClass.50 <-  try(fncYhatClassDf(Fit=Fit, Y=Y, Threshold=as.numeric(Threshold[["counts"]][10]), 
                               Censor=Censor, PredTime=PredTime, RegType=RegType, DF=DF, OffSetName=OffSetName))
  yClass.75 <-  try(fncYhatClassDf(Fit=Fit, Y=Y, Threshold=as.numeric(Threshold[["counts"]][11]), 
                               Censor=Censor, PredTime=PredTime, RegType=RegType, DF=DF, OffSetName=OffSetName))
  yClass.90 <-  try(fncYhatClassDf(Fit=Fit, Y=Y, Threshold=as.numeric(Threshold[["counts"]][12]), 
                               Censor=Censor, PredTime=PredTime, RegType=RegType, DF=DF, OffSetName=OffSetName))
  yClass.95 <-  try(fncYhatClassDf(Fit=Fit, Y=Y, Threshold=as.numeric(Threshold[["counts"]][13]), 
                               Censor=Censor, PredTime=PredTime, RegType=RegType, DF=DF, OffSetName=OffSetName))
  #High 5: 5th highest value
  yClass.H5 <-  try(fncYhatClassDf(Fit=Fit, Y=Y, Threshold=as.numeric(Threshold[["extremes"]][6]), 
                               Censor=Censor, PredTime=PredTime, RegType=RegType, DF=DF, OffSetName=OffSetName))

  #Run yhat function when there are too few predictor values
  yclass_list <- list()
  if(length(Threshold$counts) != 13) {
#if(is.null(Threshold$extremes)) {
    for(i in 1:length(Threshold$values$value)) {
      yclass_list[[i]] <- fncYhatClassDf(Fit=Fit, Y=Y, Threshold=as.numeric(Threshold[["values"]][[1]][ i ]), 
                     Censor=Censor, PredTime=PredTime, RegType=RegType, DF=DF, OffSetName=OffSetName)
      names(yclass_list)[i] <- paste0("yClass.", i)
    }
  }
    
    #Group the output
  if(length(Threshold$counts) != 13) {
    YClass <- yclass_list
  } else {
    YClass <- list("yClass.05"=yClass.05, "yClass.10"=yClass.10, "yClass.25"=yClass.25,"yClass.50"=yClass.50,
                   "yClass.75"=yClass.75, "yClass.90"=yClass.90, "yClass.95"=yClass.95)
  }
  
#  YClass <- list("yClass.L5"=yClass.L5, "yClass.05"=yClass.05, "yClass.10"=yClass.10, "yClass.25"=yClass.25,"yClass.50"=yClass.50,
#                 "yClass.75"=yClass.75, "yClass.90"=yClass.90, "yClass.95"=yClass.95, "yClass.H5"=yClass.H5)
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
                      "Proportion Y Logistic" = plogis(YClass[[i]]$threshLev),
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
fncDcsnCrvPlt <- function(ThreshQntl, CType, xlim1,xlim2,ylim1,ylim2, Legend.Loc, LCol, LSize) {
  if(CType == "Net Benefit") {
    par(mar= c(5.1, 4.6, 4.1, 1.6))
    plot(ThreshQntl$Threshold.Level, ThreshQntl$Net.Benefit,type="n", xlim=c(xlim1,xlim2), ylim=c(ylim1,ylim2), 
         main="Decision curve plot of net benefit by threshold levels", xlab="Threshold", ylab="Net Benefit",
         cex.main=2, cex.lab=2)
    lines(ThreshQntl$Threshold.Level, ThreshQntl$Net.Benefit,cex=2, lwd=LSize, lty=1, col=LCol[1])
    lines(ThreshQntl$Threshold.Level, ThreshQntl$All.Treated,cex=2, lwd=LSize, lty=2, col=LCol[2])
    abline(h=0, col=LCol[3], lwd=LSize)
    legend(x=Legend.Loc, legend=c("Model", "All treated", "None treated"), 
           col=LCol, lty=c(1,2,1),
           lwd=2, cex=2)
  } else {
    par(mar= c(5.1, 4.6, 4.1, 1.6))
    plot(ThreshQntl$Threshold.Level, ThreshQntl$Interventions.Saved, 
         lwd=LSize, type="l", col=LCol[1], axes=F, xlim=c(xlim1,xlim2), ylim=c(ylim1,ylim2), 
         main="Decision curve plot of interventions avoided by threshold levels", xlab="Threshold",
         cex.main=2, cex.lab=2,
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

#####################################
## Offsets for Poisson regressions ##
#####################################
## This function determines if there is an offset in the model ##
fncMdlOffSetYesNo <- function(Model) {
  if(length(Model$offset) > 1) {
    OffsetYN <- "Yes"
  } else {
    OffsetYN <- "No"
  }
  return(OffsetYN=OffsetYN)
}

## This function gets the info I need about the offset ##
fncModelOffset <- function(Model, DF) {
  #Get offset
  offset_variable <- character()
  if(fncMdlOffSetYesNo(Model) == "Yes") {
    #if(length(Model$offset) > 1) {
    offset_variable <- names(Model[["model"]])[ grep("\\)\\)", names(Model[["model"]])) ]
  }
  #Get variable name, remove offset notation
  #offset_variable_name <- gsub("[[:punct:]]", "", gsub("offset|log", "", offset_variable))
  offset_variable_name <- strsplit(offset_variable, "\\(|\\)"  )[[1]][3]  #String split to get offset variable
  
  #Create function that goes into plot
  Offset_Exp_function <- function ( x) exp( x) * mean(DF[, offset_variable_name])
  
  if(length(offset_variable_name) == 0) {
    Offset_Exp_function <- NULL
  } else {
    Offset_Exp_function <- Offset_Exp_function
  }
  
  if(length(offset_variable_name) == 0) {
    offset_variable_name <- NULL
  } else {
    offset_variable_name <- offset_variable_name
  }
  
  #This creates the text for the offset value
  #offset_variable_val_text <- character()
  if(length(offset_variable_name) == 0) {
    offset_variable_val_text <- NULL
  } else {
    offset_variable_val_text <- paste0("list(", offset_variable_name, "=", mean(DF[,offset_variable_name,], na.rm=T),")" )
  }
  return(list(offset_variable_name=offset_variable_name, 
              Offset_Exp_function=Offset_Exp_function, offset_variable_val_text=offset_variable_val_text))
}

## This function creates an object of offset info or blank values if not relevant ##
fncMdlOffSetDF <- function(Model, DF) {
  if(fncMdlOffSetYesNo(Model) == "Yes") {
    Offset.List <- fncModelOffset(Model=Model, DF=DF)
  } else {
    Offset.List <- list(offset_variable_name=character(), 
                        Offset_Exp_function=expression(), offset_variable_val_text=numeric())
  }
  return(Offset.List)
}

## Reactive functions for offset functions above ##
#Yes/No on whether there is an offset
mdl_off_set_yes_no <- reactive({                 
  fncMdlOffSetYesNo(fit1() ) 
})

#This gets the offset data I need to plot the partial predictions below
mdl_off_set_output <- reactive({                 
  fncMdlOffSetDF(Model=fit1(), DF=df() ) 
})

##########################
##  Partial predictions ##
##########################
#This plots the predicted values    
    output$prt_prd <- renderPlot({
      if( mdl_off_set_yes_no() == "No") {
      if(input$pe_yes == "Yes") {
        plot(  do.call("Predict", list(fit1(), pe_x_var(),  fun=list("As is"=TRUE, 
              "Exponentiated"=exp, "Proportion"=plogis, "Mean time"= function(x) mean_time()(lp=x), 
              "Median time" =function(x) med_time()(lp=x))[[partial_effect_plot_fun()]] )   ), pch=15) 
      } else {
        plot(Predict(fit1(), fun=list("As is"=TRUE, "Exponentiated"=exp, "Proportion"=plogis,
              "Mean time"= function(x) mean_time()(lp=x), "Median time" =function(x) med_time()(lp=x))[[partial_effect_plot_fun()]] ), pch=15)
      }
      } else {
        if(input$pe_yes == "Yes") {
          plot(  do.call("Predict", list(fit1(), pe_x_var(),  fun= mdl_off_set_output()$Offset_Exp_function,
                        offset=eval(parse(text= mdl_off_set_output()$offset_variable_val_text ))) ), pch=15) 
        } else {
          plot(Predict(fit1(), fun= mdl_off_set_output()$Offset_Exp_function, 
                       offset=eval(parse(text= mdl_off_set_output()$offset_variable_val_text )) ), pch=15)
        }
      }
    }, height = 600)

    #1. Create yes/no box to determine plot single partial effect
    output$prt_one_yes <- renderUI({                                 #Same idea as output$vy
      selectInput("pe_yes", "1. Do you want to plot a single partial effect?", 
                  choices = c("No", "Yes"), multiple=FALSE, selected="No")     #Will make choices based on my reactive function.
    })
    #2, Select the variables that will get 
    output$prt_one_x <- renderUI({
      selectInput("pe_X", "2. Select a single predictor.", 
                  choices = predictor(), multiple=FALSE, selected=predictor()[1])     #Will make choices based on my reactive function.
    })

    #3. Select the type of function I will use in plot(predict, fun=?) 
    output$pePltFun <- renderUI({
      selectInput("pe_plot_fun", "3. Select the plot function.", choices = c("As is", "Exponentiated", 
                                "Proportion", "Mean time", "Median time"), multiple=FALSE, selected= "As is")     
    })
    #3A. Reactive function for the plot function above
    partial_effect_plot_fun <- reactive({                 
      input$pe_plot_fun 
    })
    #4. Select the type of function I will use in plot(predict, fun=?) 
    output$pePrtPred <- renderUI({
      selectInput("pe_prnt_prd", "4. Print the predicted values of 1 variable.", 
                  choices = c("No", "Yes"), multiple=FALSE, selected= "No")     
    })
    #4A. Print predicted scores for 1 variable
    output$pe_predict_var_smry <- renderPrint({
      if( mdl_off_set_yes_no() == "No") {
      
      if(input$pe_prnt_prd == "Yes") {
        do.call("Predict", list(fit1(), pe_x_var(), fun=list("As is"=TRUE, "Exponentiated"=exp, "Proportion"=plogis, "Mean time"= function(x) mean_time()(lp=x),
                "Median time" =function(x) med_time()(lp=x))[[partial_effect_plot_fun()]] )   )
      } 
      } else {
        if(input$pe_prnt_prd == "Yes") {
          do.call("Predict", list(fit1(), pe_x_var(), fun=mdl_off_set_output()$Offset_Exp_function,
                                  offset=eval(parse(text= mdl_off_set_output()$offset_variable_val_text )))   )
        } 
    }
    })
    #Creates the mean time function for survival models
    mean_time <- reactive({                 
      if(partial_effect_plot_fun() == "Mean time") {
        Mean(fit1() )
      } 
    })
    #Creates the median time function for survival models
    med_time <- reactive({                 
      if(partial_effect_plot_fun() == "Median time") {
        Quantile(fit1() )
      } 
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
      colors()[c(552, 498,652, 254, 26,547, 24, 152,32, 52:56, 66, 68, 85,97, 120, 128,139,
                 142, 147:151, 367,372,393, 448, 399,450,
                 485, 514, 529,562,568, 584, 589, 610, 615, 620, 625, 630, 635,640,646,651,657,
                 291, 294, 297, 300, 303, 306, 309, 312, 315, 318, 321, 324, 327, 
                 330, 333, 336, 339, 342, 345, 348, 351, 354,357)]    
    })
    #5. Select specific groups
    output$xyplot_grp_levs <- renderUI({                                 
      selectInput("xyplotGrpLvs", "5. Highlight specific groups?", 
                  choices = xyplot_groups(), multiple=TRUE)     
    })
    #Reactive function to get group levels
    xyplot_Group_Levels <- reactive({                 
      input$xyplotGrpLvs 
    })
    #6. Create yes/no box to make the XY plot
    output$xyplot_yes_no <- renderUI({                                 
      selectInput("XyplotYesNo", "6. Do you want to create the plot?", 
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
    #7. Extrapolate continuous X
    output$xyExtrapo_yes_no <- renderUI({                                 
      selectInput("xyExtrpYesNo", "7. Do you want to extrapolate on X?", 
                  choices = c("No", "Yes"), multiple=FALSE, selected="No")     
    })
    #7A. Reactive function for the variable
    xy_extrapolate <- reactive({
      input$xyExtrpYesNo
    })
    #8. Textbox to enter a formula
    output$xy_extrap_box <- renderUI({  
      textInput("xyExtrpBox", "8. Select the extrapolated range.", 
  value= paste0("do.call('Predict', list(fit1(),", '\'', XyplotX1(),'\'', "= ", ", ", '\'', XyplotZ1(),'\'', "))") )
    })
    #8A. Reactive function for textbox to enter a formula
    xy_extrap_box_Input <- reactive({
      input$xyExtrpBox
    }) 
    
    #9. Add a vertical line
    output$xyExtr_X_Val <- renderUI({
      textInput("xyExtrX", "9. Enter X-axis value for vertical line.",
      value = paste0('c( ', ')') )
    })
    #9A. Reactive function for the variable
    xyExtr_x_value <- reactive({
      as.numeric(eval(parse(text=input$xyExtrX )))
    })
    #9. Indicate lower limit of x-axis
    output$xyplot_Xlim1 <- renderUI({
      numericInput("xyplotLmX1", "10. Lower X-axis limit.",
                   value = xylm()$XMin, step = .1)
    })
    #7A. Reactive function for the variable
    xyplot_X_limit_1 <- reactive({
      input$xyplotLmX1
    })
    #10. Indicate upper limit of x-axis
    output$xyplot_Xlim2 <- renderUI({
      numericInput("xyplotLmX2", "11. Upper X-axis limit.",
                   value = xylm()$XMax, step = .1)
    })
    #11A. Reactive function for the variable
    xyplot_X_limit_2 <- reactive({
      input$xyplotLmX2
    })
    #11. Indicate lower limit of y-axis
    output$xyplot_Ylim1 <- renderUI({
      numericInput("xyplotLmY1", "12. Lower Y-axis limit.",
                   value = xylm()$YMin, step = .1)
    })
    #11A. Reactive function for the variable
    xyplot_Y_limit_1 <- reactive({
      input$xyplotLmY1
    })
    #12. Indicate upper limit of Y-axis
    output$xyplot_Ylim2 <- renderUI({
      numericInput("xyplotLmY2", "13. Upper Y-axis limit.",
                   value = xylm()$YMax, step = .1)
    })
    #12A. Reactive function for the variable
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
      if(xy_extrapolate() == "Yes") {
        eval(parse(text= xy_extrap_box_Input() ))
      } else {
        do.call("Predict", list(fit1(), XyplotX1(), XyplotZ1(),  fun= mdl_off_set_output()$Offset_Exp_function,
                                offset=eval(parse(text= mdl_off_set_output()$offset_variable_val_text )) )   ) 
      }
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
                    GroupLevs=xyplot_Group_Levels(), XYlims=xylm_all(), Clrs=xyplot_Line_Colors(), 
                    CIbands=xyplot_Bands_YesNo(), ABline= xyExtr_x_value()) 
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
    fncXYpltInt <- function(DF, xyplotDF, ContX, GroupX, GroupLevs, XYlims, Clrs, CIbands, ABline, ...) {
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
                  main= paste0("Partial prediction plot of ", ContX, " by levels of ", GroupX),
                  abline=list(v=ABline))
        } else {
          xYplot(xyplotDF[[which(names(xyplotDF) == "yhat")]] ~ xyplotDF[[which(names(xyplotDF) == ContX)]] ,  
                 groups= xyplotDF[[which(names(xyplotDF) == GroupX)]],
                 type= 'l', lty= 1:length(unique(DF[, GroupX ])), col= Clrs, 
                 lwd= 3, ylab= "Yhat", xlab= ContX, cex= 1.75, 
                 xlim=XLim, ylim=YLim,
                 main= paste0("Partial prediction plot of ", ContX, " by levels of ", GroupX),
                 abline=list(v=ABline) )
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
                  main= paste0("Partial prediction plot of ", ContX, " by levels of ", GroupX),
                  abline=list(v=ABline) )
        } else {
          xYplot(xyplotDF[xyplotDF[, GroupX ] %in% GroupLevs, which(names(xyplotDF) == "yhat")] ~ xyplotDF[xyplotDF[, GroupX ] %in% GroupLevs, which(names(xyplotDF) == ContX)] ,  
                 groups= xyplotDF[[which(names(xyplotDF) == GroupX)]][ xyplotDF[, GroupX ] %in% GroupLevs ],
                 type= 'l', lty= 1:length(unique(DF[, GroupX ])), col= Clrs, 
                 lwd= 3, ylab= "Yhat", xlab= ContX, cex= 1.75, 
                 xlim=XLim, ylim=YLim,
                 main= paste0("Partial prediction plot of ", ContX, " by levels of ", GroupX),
                 abline=list(v=ABline) )
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
      if(reg %in% c("Logistic","Proportion Y Logistic", "Ordinal Logistic") ) {
        xyplot_ylab <- paste0("Odds Ratio (",lev1, ":", lev2,")")
      }
      if(reg %in% c("Linear","Poisson","Quantile","Generalized Least Squares")) {
        xyplot_ylab <- paste0("Contrast (",lev1, ":", lev2,")")
    }
      ## Determine if it is an abline at 0 or 1 ##
      if(reg %in% c("Cox PH", "Cox PH with censoring","Logistic", "Proportion Y Logistic","Ordinal Logistic","AFT","AFT with censoring") ) {
        abline_01 <- 1
      }
      if(reg %in% c("Linear","Poisson","Quantile","Generalized Least Squares")) {
        abline_01 <- 0
    }
      #Put elements int a list
      a <- list(lev1, seq(x_min, x_max, length.out=250))
      names(a) <- c(group, X)
      b <- list(lev2, seq(x_min, x_max, length.out=250))
      names(b) <- c(group, X)
      #w <- do.call("contrast", list( fit=model, a=a, b=b) )
#      w <- contrast( fit=model, a=a, b=b)
      w <- contrast( fit=model, a=a, b=b, fun= mdl_off_set_output()$Offset_Exp_function)
      
      ## Exponentiate the data if needed ##
      #Contrast
      if(reg %in% c("Cox PH", "Cox PH with censoring","Logistic","Proportion Y Logistic", "Ordinal Logistic","AFT","AFT with censoring")) {
        w[["Contrast"]] <- exp(w[["Contrast"]])
      }
      #Lower
      if(reg %in% c("Cox PH", "Cox PH with censoring","Logistic", "Proportion Y Logistic","Ordinal Logistic","AFT","AFT with censoring")) {
        w[["Lower"]] <- exp(w[["Lower"]])
      }
      #Upper
      if(reg %in% c("Cox PH", "Cox PH with censoring","Logistic","Proportion Y Logistic", "Ordinal Logistic","AFT","AFT with censoring")) {
        w[["Upper"]] <- exp(w[["Upper"]])
      }
      ## Just Poisson ##
      #Contrast
      if(reg %in% "Poisson" & length(mdl_off_set_output()$offset_variable_name) == 0 ) {
        w[["Contrast"]] <- exp(w[["Contrast"]])
      } else {
        w[["Contrast"]] <- w[["Contrast"]]
      }
      #Lower
      if(reg %in% "Poisson" & length(mdl_off_set_output()$offset_variable_name) == 0 ) {
        w[["Lower"]] <- exp(w[["Lower"]])
      }
      #Upper
      if(reg %in% "Poisson" & length(mdl_off_set_output()$offset_variable_name) == 0 ) {
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
  textInput("nomoPrdSrvTmXaxis", "3. Mean and median survival X-axis times:", 
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

#6. Label size for nomogram variables
output$nomo_vr_lbl_sz <- renderUI({  
  numericInput("nomoVrLblSz", "6. Select variable label size",
               min=1, step=.1, value=1)
})
#6A. Object with multiplier value
nomo_var_label_size <- reactive({
  input$nomoVrLblSz
})
#7. Transition specific formulas
output$nomo_up_Fmla <- renderUI({
  textInput("nomoUpFmla", "7. Update nomogram formula", 
  #value= deparse(nomo_fmla_output(), width.cutoff=500 )  )     
  value= nomo_fmla_output()  )     
})
#7B. Make full regression call
#nomo_first_formula <- reactive({
#  paste0("coxph(list(",  deparse(ms_cph_mdl_fmla(), width.cutoff=500 ),  ",), data=", input$msDfInputName,", ", "id=", 
#         cph_time_fmla_id(), ", model=TRUE" , ")")
#})
#7A. Object with formula
nomo_update_formula <- reactive({
  parse(text=sub("expression", "", input$nomoUpFmla))
})

#8. Create yes/no box to determine plot single partial effect
output$nomo_yes <- renderUI({                                 #Same idea as output$vy
  selectInput("nomoYes", "8. Do you want to create a new nomogram?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     #Will make choices based on my reactive function.
})
#8A. Object with new layout
nomogram_yes <- reactive({
  input$nomoYes
})

#9. Run function to get nomogram formula
get_nomo_fmla <- reactive({
#  if (nomogram_yes() =="Yes") {
    fncNomo(RegType=input$regress_type)
#  }
})
#9A. Run the data function
nomo_fmla_output <- reactive({
#  if (nomogram_yes() =="Yes") {
    get_nomo_fmla()
#  }
})

#10. Get nomogram output
get_nomo_output <- reactive({
  if (nomogram_yes() =="Yes") {
    fncNomoOutput(Nom=nomo_update_formula(), Fit=fit1(), PrbSrvTm=nomogram_prob_survival_time(), TrnSrvTm=nomo_transformation_time_denom(), 
                MdMnSrvTm=nomo_pred_surv_time_xaxis(), Time_label=nomo_survival_time_prob_values(), 
                Time_label_period=nomo_survival_time_prob_periods(), RegType=input$regress_type)
  }
})


#11. Run plot
output$nomo_gram <- renderPlot({
  if(nomogram_yes() == "Yes") {
    plot(get_nomo_output(), cex.var=nomo_var_label_size() )
  } else {
    plot(nomogram(fit1()), cex.var=nomo_var_label_size())
  }
}, height = 800)

#12. Print nomogram results
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
                "Logistic" = expression(nomogram(Fit, fun=plogis, fun.at=c(.01, .025, .05, seq(.1,.9,.1), .95, .975, .99))),
                "Proportion Y Logistic" = expression(nomogram(Fit, fun=plogis, fun.at=c(.01, .025, .05, seq(.1,.9,.1), .95, .975, .99))),
                "Ordinal Logistic"  = expression(nomogram(Fit, fun=plogis, fun.at=c(.01, .025, .05, seq(.1,.9,.1), .95, .975, .99))),
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
                  "Proportion Y Logistic" = NA ,
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
                   "Proportion Y Logistic" = NA ,
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
                   "Proportion Y Logistic" = NA ,
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
                  "Proportion Y Logistic" = NA ,
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
                 "Proportion Y Logistic" = NA ,
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
                   "Proportion Y Logistic" = NA ,
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
                 "Proportion Y Logistic" = NA ,
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
         "Proportion Y Logistic" = eval(Nom) ,
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
  numericInput("calSrvTM", "3. Select a time for survival models", value= try(as.numeric(describeY()[["counts"]]["Mean"])), 
#               min=try(as.numeric(describeY()[["extremes"]][1])), max=try(as.numeric(describeY()[["extremes"]][10])), 
               step=1)
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

#6. Pick a time for survival models
output$calibrate_set_seed <- renderUI({
  numericInput("calSetNumSeed", "6. Set random number seed.", value= 1, min=1, step=1) 
})
#6A. Set random number seed
calibrate_number_seed <- reactive({
  input$calSetNumSeed
})

#Determine if we should begin the calibration.
output$BeginCalibrate <- renderUI({  
  selectInput("begin_cali", "7. Begin calibration?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     #Will make choices based on my reactive function.
})

#This creates a calibration curve    
output$cali_brate <- renderPlot({
  if (input$MI_for_cali == "No") {
    
  if (input$begin_cali == "Yes") {
  set.seed(calibrate_number_seed() )
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
      set.seed(calibrate_number_seed() )
      if (input$regress_type %in%  c("Cox PH","Cox PH with censoring","AFT","AFT with censoring")) {
        plot(calibrate(fit1(), B=input$cali_B_n, u=input$calSrvTM, m=floor(length(fit1()$linear.predictors) /calibrate_survival_quantile_n()), 
                       cmethod='KM'), add=TRUE)
      }
  }
  } 
  ######
  if (input$MI_for_cali == "Yes") {
    
    if (input$begin_cali == "Yes") {
      set.seed(calibrate_number_seed() )
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
      set.seed(calibrate_number_seed() )
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

#4. Pick a time for survival models
output$validate_set_seed <- renderUI({
  numericInput("valSetNumSeed", "4. Set random number seed.", value= 1, min=1, step=1) 
})
#4A. Random number seed
validate_number_seed <- reactive({
  input$valSetNumSeed
})

#Determine if we should begin the calibration.
output$BeginValidate <- renderUI({  
  selectInput("begin_vali", "5. Begin validation?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     #Will make choices based on my reactive function.
})

output$vali_date <- renderPrint({
  if (input$MI_for_vali == "No") {
    
    if (input$begin_vali == "Yes") {
      set.seed(validate_number_seed() )
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
      set.seed(validate_number_seed() )
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
#  xdf <- data.frame(df()[, predictor(), drop=FALSE])  #The drop argument allows me to show 1 predictor
  xdf <- data.frame(df()[, setdiff(predictor(), mdl_off_set_output()$offset_variable_name), drop=FALSE])  #Drops offset variables
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
"Proportion Y Logistic" = plot_yhat <- plogis(yhat),
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
  selectInput("topbottom", "1. Highlight top or bottom outcome scores.", 
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
  set.seed( 1 )
  transcan(cluster_anl_fmla(),
           imputed=TRUE, transformed=TRUE, trantab=TRUE, pl=FALSE,
           show.na=TRUE, data=df(), nk= input$SIknots, asis=input$asisx, iter.max=SI_max_iter(),
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
output$SI_set_maxIter <- renderUI({
  numericInput("SISetMaxIter", "3. Set max number of iterations.", value= 50, min=1, step=1) 
})
#4A. Random number seed
SI_max_iter <- reactive({
  input$SISetMaxIter
})
#Indicate if you want the transformation/imputed values
output$Transcan_Choice <- renderUI({ 
  radioButtons("TransChoice", "4. Would you like to run the simultaneous transformation and imputation?",
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
#               Proportion and t-tests and POWER ANALYSIS                      #
################################################################################

#####################
## Proportion test ##
#####################

#1. Y variable "Select the response variable"
output$prp_tst_y <- renderUI({                                
  selectInput("prpTstY", "1. Select the outcome",        
              choices = var(), multiple=FALSE, selected=var()[1] ) 
})
#1A. Reactive function for the Y variable
proportion_test_y <- reactive({
  input$prpTstY
})
#2. X formula  "Select the explanatory variable"
#Select the predictors.
output$prp_tst_x <- renderUI({                                 #Same idea as output$vy
  selectInput("prpTstX", "2. Select the group", 
              choices = setdiff(var(), proportion_test_y()), multiple=FALSE) 
})
#2A. Reactive function for the X variable
proportion_test_x <- reactive({
  input$prpTstX
})

#3. 1-sample test 
output$prp_tst_1_smpl <- renderUI({                                #
  selectInput("prpTst1s", "3. Is this a 1 sample test?",       #
              choices = c("No","Yes"), multiple=FALSE, selected= "No")
})
#3A. Reactive function for proportion level
proportion_test_one_sample <- reactive({
  input$prpTst1s
})

#4. Select 1-sample test probability
output$prp_tst_prp <- renderUI({                                #
  numericInput("prpTstPrp", "4. Select 1-sample test probability",       #
               value = 0.5, min=0, max=1, step=.01 )   #
})
#5A. Reactive function for proportion level
proportion_test_prop <- reactive({
  input$prpTstPrp
})
#5. Alternative hypothesis test
output$prp_tst_alt <- renderUI({                                
  selectInput("prpTstAlt", "5. Select a one- or two-sided test",        
              choices = c("two.sided", "less", "greater"), multiple=FALSE, selected="two.sided" ) 
})
#5A. Reactive function for alternative hypothesis test
proportion_test_alternative <- reactive({
  input$prpTstAlt
})
#6. 
output$prp_tst_CI <- renderUI({                               
  numericInput("prpTstCI", "6. Select the confidence interval level", 
               value = 0.95, min=0, max=1, step=.01 )   
})
#6A. Reactive function for confidence interval
proportion_test_Conf_Int <- reactive({
  input$prpTstCI
})
#7. Yates' correction
output$prp_tst_yts <- renderUI({  
  selectInput("prpTstYC", "7. Use the Yates' correction?", 
            choices = c(TRUE, FALSE), multiple=FALSE, selected=TRUE)
})
#7A. Reactive function for Exact method
proportion_test_Yates <- reactive({
  input$prpTstYC
})
#8. Exact method
output$prp_tst_YN <- renderUI({  
  selectInput("prpTstYN", "8. Run the proportion test?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     
})
#8A. Reactive function for Exact method
proportion_test_Yes_No <- reactive({
  input$prpTstYN
})
#9. Run the function below
proportion_test_run <- reactive({
  if(proportion_test_Yes_No() == "Yes") {    
    fncPrpTst(DF=df(), Y=proportion_test_y(), X=proportion_test_x(), P=proportion_test_prop(), 
              Alt=proportion_test_alternative(), CI=proportion_test_Conf_Int(), 
              Correct=proportion_test_Yates(), Samp1= proportion_test_one_sample())
  }  
})
#9A. proportion test output  
output$proportion_test_out <- renderPrint({
  if(proportion_test_Yes_No() == "Yes") {
    proportion_test_run()
  }
})

#############
## t-tests ##
#############
#1. Y variable "Select the response variable"
output$t_tst_y <- renderUI({                                
  selectInput("tTstY", "1. Select the outcome",        
              choices = var(), multiple=FALSE, selected=var()[1] ) 
})
#1A. Reactive function for the Y variable
t_test_y <- reactive({
  input$tTstY
})
#2. X formula  "Select the explanatory variable"
#Select the predictors.
output$t_tst_x <- renderUI({                                 #Same idea as output$vy
  selectInput("tTstX", "2. Select the group", 
              choices = setdiff(var(), t_test_y()), multiple=FALSE) 
})
#2A. Reactive function for the X variable
t_test_x <- reactive({
  input$tTstX
})

#3. 1-sample test 
output$t_tst_1_smpl <- renderUI({                                #
  selectInput("tTst1s", "3. Is this a 1 sample test?",       #
              choices = c("No","Yes"), multiple=FALSE, selected= "No")
})
#3A. Reactive function for proportion level
t_test_one_sample <- reactive({
  input$tTst1s
})

#4. Select 1-sample test mean
output$t_tst_mn <- renderUI({                                #
  numericInput("tTstMn", "4. Select 1-sample test mean",       #
               value = 1, step=1 )   #
})
#5A. Reactive function for proportion level
t_test_mean <- reactive({
  input$tTstMn
})
#5. Alternative hypothesis test
output$t_tst_alt <- renderUI({                                
  selectInput("tTstAlt", "5. Select a one- or two-sided test",        
              choices = c("two.sided", "less", "greater"), multiple=FALSE, selected="two.sided" ) 
})
#5A. Reactive function for alternative hypothesis test
t_test_alternative <- reactive({
  input$tTstAlt
})
#6. 
output$t_tst_CI <- renderUI({                               
  numericInput("tTstCI", "6. Select the confidence interval level", 
               value = 0.95, min=0, max=1, step=.01 )   
})
#6A. Reactive function for confidence interval
t_test_Conf_Int <- reactive({
  input$tTstCI
})
#7. Paired sample
output$t_tst_pr <- renderUI({  
  selectInput("tTstPr", "7. Is this a paired sample test?", 
              choices = c(TRUE, FALSE), multiple=FALSE, selected=FALSE)
})
#7A. Reactive function for Exact method
t_test_pair <- reactive({
  input$tTstPr
})
#8. Run the test
output$t_tst_YN <- renderUI({  
  selectInput("tTstYN", "8. Run the t-test?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     
})
#8A. Reactive function for above
t_test_Yes_No <- reactive({
  input$tTstYN
})
#9. Run the function below
t_test_run <- reactive({
  if(t_test_Yes_No() == "Yes") {    
    fncTTst(DF=df(), Y=t_test_y(), X=t_test_x(), M=t_test_mean(), 
            Pair=t_test_pair(), Alt=t_test_alternative(), CI=t_test_Conf_Int(), 
              Samp1= t_test_one_sample())
  }  
})
#9A. proportion test output  
output$t_test_out <- renderPrint({
  if(t_test_Yes_No() == "Yes") {
    t_test_run()
  }
})

#####################
## Power analysis  ##
#####################

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

########################################
## Function to create proportion test ##
########################################
fncPrpTst <- function(DF, Y, X, P, Alt, CI, Correct, Samp1) {
  #Determine if it is 1 or 2 sample test
  if(Samp1 == "Yes" ) {
    Tbl <- table(DF[, Y])
    #Conduct proportion test
    P.Test <- prop.test(Tbl, p=P, alternative = Alt,
                        conf.level = CI, correct = Correct)
  } else {
    Tbl <- table(DF[, X], DF[, Y])
    Tbl <- Tbl[, 2:1]
    #Conduct proportion test
    P.Test <- prop.test(Tbl, alternative = Alt,
                        conf.level = CI, correct = Correct)
  }
  return("Results"= P.Test)
}

####################################################
## Function to do proportion pairwise comparisons ##
####################################################
fncPrwsPrpTst <- function(DF, Y, X) {
  #Determine if it is 1 or 2 sample test
  Tbl <- table(DF[, X], DF[, Y])
  Tbl <- Tbl[, 2:1]
  #Conduct proportion test
  P.Test <- pairwise.prop.test(Tbl, alternative = "two.sided", p.adjust.method="bonferroni")
  return("Results"= P.Test)
}

#################################
## Function to create a t-test ##
#################################
fncTTst <- function(DF, Y, X, M, Pair, Alt, CI, Samp1) {
  #Determine if it is 1 or 2 sample test
  if(Pair ==FALSE)  {
    if(Samp1 == "Yes" ) {
      #Make formula
      FMLA <- as.formula( paste0(Y, "~1") )
      #Conduct proportion test
      T.Tst <- t.test(formula=FMLA, data=DF, mu=M, alternative = Alt, conf.level = CI)
    } else {
      #Make formula
      FMLA <- as.formula(paste(Y, "~", X))
      #Conduct proportion test
      T.Tst <- t.test(formula=FMLA, data=DF,  alternative = Alt, conf.level = CI)
    } 
  } else {
    FMLA <- as.formula( paste0("Pair(", Y, ",",X, ")", "~1") )
    #Conduct proportion test
    T.Tst <- t.test(formula=FMLA, data=DF, alternative = Alt, conf.level = CI)
  }
  return("Results"=T.Tst)
}

############################################
## Function to do group tests for 95% CIs ##
############################################
fncCnfGrpTst <- function(X, Y, DF, ci_type) {
  if (ci_type %in% c("Mean (t)", "Poisson (exact)") ) {
    Formula_1 <- as.formula(paste(paste0(Y, "~"),   
                                  paste( "as.factor(", X,")" )))
  }
  switch(ci_type,                
         "Mean (t)" =  summary(aov(formula= Formula_1, data=DF)), 
         "Proportion (binomial)" = fncPrpTst(DF, Y, X, P=NULL, Alt="two.sided", CI=.95, 
                                             Correct=TRUE, Samp1= "No"), 
         "Poisson (exact)" =  summary(aov(formula= Formula_1, data=DF)) 
  )
}

#############################################
## Function for post-hoc tests for 95% CIs ##
#############################################
fncCnfPstHc <- function(X, Y, DF, ci_type) {
  if (ci_type %in% c("Mean (t)", "Poisson (exact)") ) {
    Formula_1 <- as.formula(paste(paste0(Y, "~"),   
                                  paste( "as.factor(", X,")" )))
  }
  switch(ci_type,                
         "Mean (t)" =  TukeyHSD(aov(formula= Formula_1, data=DF)), 
         "Proportion (binomial)" = fncPrwsPrpTst(DF, Y, X), 
         "Poisson (exact)" =  TukeyHSD(aov(formula= Formula_1, data=DF)) 
  )
}

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
  all_m <- mean(dataf[, y], na.rm=T)
  all_sd <- sd(dataf[, y], na.rm=T)
  all_n <- length(na.omit(dataf[, y]))
  #By each X level
  agr_m <- aggregate(dataf[, y], list(dataf[, x]), FUN="mean", na.rm=T)
  agr_sd <- aggregate(dataf[, y], list(dataf[, x]), FUN="sd", na.rm=T)
  agr_n <- aggregate(dataf[ complete.cases(dataf[, y]) , y], list(dataf[ complete.cases(dataf[, y]) , x]), FUN="length")
  agr_df <- data.frame(x_lev=agr_m[, 1], agr_m=agr_m[, 2], agr_sd=agr_sd[, 2], agr_n=agr_n[, 2])
  
  #Calculates confidence intervals--Overall
  all_MOE <- qt((conf_lev/2)+.5, df=all_n - 1) * all_sd/sqrt(all_n)
  all_Lower <- all_m - all_MOE
  all_Upper <- all_m + all_MOE
  adf_all <- data.frame(cbind(PointEst=all_m, Lower=all_Lower, Upper=all_Upper))
  #Calculates confidence intervals--By Level
  MOE <- qt((conf_lev/2)+.5, df=agr_df$agr_n - 1) * agr_df$agr_sd/sqrt(agr_df$agr_n)
  Lower <- agr_df$agr_m - MOE
  Upper <- agr_df$agr_m + MOE
  adf_alpha <- data.frame(cbind(PointEst=agr_df$agr_m, Lower=Lower, Upper=Upper))
  rownames(adf_alpha) <- agr_df$x_lev
  #  alpha_o <- order(rownames(adf_alpha), decreasing = T) 
  alpha_o <- order(agr_df$x_lev, decreasing = T) 
  adf_alpha <- adf_alpha[alpha_o, ] 
  adf_o <- order(adf_alpha[, "PointEst"], decreasing = T) 
  adf_numeric <- adf_alpha[adf_o, ] 
  return(list(adf_alpha=adf_alpha, adf_numeric=adf_numeric, adf_all= adf_all) ) 
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
  
  adf_all <- binconf(x=sum(agr_df[,2], na.rm=TRUE), n=sum(agr_df[,3], na.rm=TRUE), alpha=1 - conf_lev)
  
  rownames(adf_alpha) <- agr_df$x_lev
  #  alpha_o <- order(rownames(adf_alpha), decreasing = T) 
  alpha_o <- order(agr_df$x_lev, decreasing = T) 
  adf_alpha <- adf_alpha[alpha_o, ] 
  adf_o <- order(adf_alpha[, "PointEst"], decreasing = T) 
  adf_numeric <- adf_alpha[adf_o, ] 
  return(list(adf_alpha=adf_alpha, adf_numeric=adf_numeric, adf_all=adf_all) ) 
}

##############

#Exact Poisson
pconf <- function(x, y, dataf, conf_lev) {
  #Aggregate outcomes for all
  all_sum <- sum(dataf[, y], na.rm=T)
  all_n <- length(na.omit(dataf[, y]))
  adf_all <- unlist(poisson.test(x=all_sum, T=all_n, conf.level= conf_lev)[c("estimate","conf.int")])
  adf_all <- data.frame(matrix(adf_all, ncol=3))
  colnames(adf_all) <- c("PointEst", "Lower", "Upper")
  #Aggregates outcome by factor 
  agr_sum <- aggregate(dataf[, y], list(dataf[, x]), FUN="sum", na.rm=T)
  agr_n <- aggregate(dataf[, y], list(dataf[, x]), FUN="length")
  agr_df <- data.frame(x_lev=agr_sum[, 1], agr_sum=agr_sum[, 2], agr_n=agr_n[, 2])
  #Calculates confidence intervals
  adf_alpha <- matrix(ncol= 3, nrow= nrow(agr_df), byrow = TRUE)
  for (i in 1:nrow(agr_df)) {
    adf_alpha[i, ] <- unlist(poisson.test(x=agr_df[i,2], T=agr_df[i,3], conf.level= conf_lev)[c("estimate","conf.int")])
  }
  adf_alpha <- data.frame(adf_alpha)
  colnames(adf_alpha) <- c("PointEst", "Lower", "Upper")
  rownames(adf_alpha) <- agr_df$x_lev
  #  alpha_o <- order(rownames(adf_alpha), decreasing = T) 
  alpha_o <- order(agr_df$x_lev, decreasing = T) 
  adf_alpha <- adf_alpha[alpha_o, ] 
  adf_o <- order(adf_alpha[, "PointEst"], decreasing = T) 
  adf_numeric <- adf_alpha[adf_o, ] 
  return(list(adf_alpha=adf_alpha, adf_numeric=adf_numeric, adf_all=adf_all) ) 
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
plot_ci_fnc <- function(xcivar, ycivar, ydf, cidf, ciconf_lev, alpha_num, Lcol, 
                        Pcol, tgt, Cbar, plyCol, labMulti=1, roundVal, XLim1, XLim2) {
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
       xlab= paste0("Value (grey vertical line = overall mean of ", round(mainYmn, roundVal), ", ", ciconf_lev * 100, "% ", "CI",
                    " [", round(cidf[["adf_all"]][,"Lower"], roundVal), ", ", round(cidf[["adf_all"]][,"Upper"], roundVal),"]",")"),
       #main=main_ttl, 
       axes=F,  cex.lab=1*labMulti, xlim=c(XLim1, XLim2))
  title(main_ttl, cex.main = 1*labMulti) 
  for (i in 1:nrow(adf)) {
    lines(c(adf[,'Lower'][i], adf[,'Upper'][i]), c(i,i), lwd=4, col=Lcol) 
    points(adf[,'PointEst'][i],i, pch=24, col=Pcol, lwd=1, bg=Pcol, cex=1.75*labMulti) 
  }
  #Mean line
  abline(v=mainYmn, lwd=3, col="grey", lty=3)
  #Target line
  abline(v=tgt, lwd=3, col="green", lty=1)
  axis(1) 
  axis(2,at=1:nrow(adf),labels=substr(rownames(adf), 1, 10), las=1, cex.axis=1*labMulti )
  #  axis(2,at=1:nrow(adf),labels=rownames(adf), las=1, cex.axis=1)
  axis(4,at=1:nrow(adf),labels=round(adf[, "PointEst"], roundVal), las=1, cex.axis= 1*labMulti*.75 )
  
  ## Add confidence bar ##
  #Create x and y data
  cidf[["adf_all"]][,"Lower"]
  Cbar_x <- c(rep(cidf[["adf_all"]][,"Lower"], nrow(adf)), rep(cidf[["adf_all"]][,"Upper"], nrow(adf)))
  Cbar_y <- c(1:nrow(adf), nrow(adf):1)
  #Create shading
  if(Cbar=="Yes") {
    polygon(Cbar_x, Cbar_y, col = adjustcolor(plyCol, alpha.f = 0.4), border= plyCol )
  }
  box()
}

#Confidence interval plot reactive function
plot_ci <- reactive({                  #This indicates the data frame I will use.
  if(input$CiCreate == "Yes") {
    plot_ci_fnc(xcivar=input$xcivar, ycivar=input$ycivar, ydf=df(), cidf=cidf(), 
                ciconf_lev=input$ciconf_lev, alpha_num=input$alpha_num, Lcol=ci_plot_Line_Colors(), 
                Pcol=ci_plot_Point_Colors(), tgt=ci_target_line(), Cbar= Ci_create_total_bar_interval(), 
                plyCol= ci_plot_Total_Bar_Colors(), labMulti=ci_plot_label_multiplier(),
                roundVal= ci_plot_round_decimals(), XLim1=ci_plot_Xlim_val1(), 
                XLim2=ci_plot_Xlim_val2() )
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
#Add a target line
output$Ci_Tgt_Line <- renderUI({                                 
  numericInput("ciTgtLn", "5. Add a target line.",
               value = NULL, step = .1)
})
#Reactive function for directly above
ci_target_line <- reactive({                 
  input$ciTgtLn 
})
#Select the sorting order.
output$Ci_Alpha_Num <- renderUI({                                #Creates a UI function here but it will
  radioButtons("alpha_num", "9. Sort by factor name or numerical value?",
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
    list("Alphabetical"=cidf()[["adf_alpha"]][nrow(cidf()[["adf_alpha"]]):1,], 
         "Numerical"=cidf()[["adf_numeric"]][nrow(cidf()[["adf_numeric"]]):1,],
         "Group.Tests"= conf_group_test(),
         "Pairwise.Comparisons"= conf_post_hoc())
  }
})

#Select whether to run the 95% confidence interval or not
output$Ci_create <- renderUI({                                #Creates a UI function here but it will
  radioButtons("CiCreate", "6. Create confidence intervals?",
               choices = c("No", "Yes"),
               selected="No")
})
#Select line colors
output$ci_plot_ln_clrs <- renderUI({                                 
  selectInput("ciPltLnClr", "7. Select line color.", 
              choices = xyplot_Line_Color_Names(), multiple=FALSE, selected= "blue")     
})
#Reactive function for directly above
ci_plot_Line_Colors <- reactive({                 
  input$ciPltLnClr 
})
#Select point colors
output$ci_plot_pt_clrs <- renderUI({                                 
  selectInput("ciPltPtClr", "8. Select point color.", 
              choices = xyplot_Line_Color_Names(), multiple=FALSE, selected="gold")     
})
#Reactive function for directly above
ci_plot_Point_Colors <- reactive({                 
  input$ciPltPtClr 
})
#Select whether to run the 95% confidence interval or not
output$Ci_create_tot_bar <- renderUI({                                #Creates a UI function here but it will
  selectInput("CiCreateTotBar", "10. Create overall Conf. Int. band?",
               choices = c("No", "Yes"),
               selected="No")
})
#Reactive function for directly above
Ci_create_total_bar_interval <- reactive({                 
  input$CiCreateTotBar 
})

#Select line colors
output$ci_plot_tot_bar_clrs <- renderUI({                                 
  selectInput("ciPltTotBarClr", "11. Select overall band color.", 
              choices = xyplot_Line_Color_Names(), multiple=FALSE, selected= "gray")     
})
#Reactive function for directly above
ci_plot_Total_Bar_Colors <- reactive({                 
  input$ciPltTotBarClr 
})
#Select line colors
output$ci_plot_lab_multi <- renderUI({                                 
  numericInput("ciPltLabMlt", "12. Increase label sizes.",
               value = 1, min=.01, step = .1)
})
#Reactive function for directly above
ci_plot_label_multiplier <- reactive({                 
  input$ciPltLabMlt 
})
#13. Indicate lower limit of x-axis
output$ci_plot_Xlim1 <- renderUI({
  numericInput("ciXLim1", "13. Lower X-axis limit.",
               value = round(min(cidf()$adf_alpha$Lower, na.rm=T), 2), step = .01)
})
#13a. Indicate lower limit of x-axis
ci_plot_Xlim_val1 <- reactive({
  input$ciXLim1
})
#14. Indicate upper limit of x-axis
output$ci_plot_Xlim2 <- renderUI({
  numericInput("ciXLim2", "14. Upper X-axis limit.",
               value = round(max(cidf()$adf_alpha$Upper, na.rm=T), 2) , step = .01)
})
#14. Indicate upper limit of x-axis
ci_plot_Xlim_val2 <- reactive({
  input$ciXLim2
})
#15. Select number of digits to round values by
output$ci_plot_rnd_decs <- renderUI({                                 
  numericInput("ciPltRndDec", "15. Round decimals by.",
               value = 2, step = 1)
})
#15a. Reactive function for directly above
ci_plot_round_decimals <- reactive({                 
  input$ciPltRndDec 
})

## Reactive function to do group tests ##
conf_group_test <- reactive({                  #This indicates the data frame I will use.
  if(input$CiCreate == "Yes") {
    fncCnfGrpTst(X=input$xcivar, Y=input$ycivar, DF=df(), ci_type= input$ci_type)
  }
})

## Reactive function to do pairwise tests ##
conf_post_hoc <- reactive({                  #This indicates the data frame I will use.
  if(input$CiCreate == "Yes") {
    fncCnfPstHc(X=input$xcivar, Y=input$ycivar, DF=df(), ci_type= input$ci_type)
  }
})

#######################################
# Graphs for trajectories by time     #
#######################################
#Binomial
fbconf <- function(x, xlev, y, z, dataf, conf_lev, Increment) {
  #Aggregates outcome by factor 
  if( is.null(xlev)) {
    dataf <- dataf
  } else {
    dataf <- dataf[ dataf[, x] %in% xlev,  ]
  }
  #Calculates confidence intervals for single units or in increments
  if(Increment == 1) {
    agr_sum <- aggregate(dataf[, y], list(dataf[, x] , dataf[, z]), FUN="sum", na.rm=TRUE)
    agr_n <- aggregate(dataf[, y], list(dataf[, x] , dataf[, z]), FUN="length")
  } else {
    agr_sum <- aggregate(dataf[, y], list(dataf[, x] , ceiling(dataf[, z]/Increment) ), FUN="sum", na.rm=TRUE)
    agr_n <- aggregate(dataf[, y], list(dataf[, x] , ceiling(dataf[, z]/Increment) ), FUN="length")
  }
  agr_df <- data.frame(x_lev=agr_sum[, 1], z_lev=as.integer(c(agr_sum[, 2])), agr_sum=agr_sum[, 3], agr_n=agr_n[, 3])
  agr_df <- cbind(agr_df, binconf(x=agr_df[,3], n=agr_df[,4], alpha=1 - conf_lev))
  rownames(agr_df) <- 1:nrow(agr_df)
  return(agr_df) 
}

#Continuous outcomes
ftconf <- function(x, xlev, y, z, dataf, conf_lev, Increment) {
  #Aggregates outcome by factor 
  if( is.null(xlev)) {
    dataf <- dataf
  } else {
    dataf <- dataf[ dataf[, x] %in% xlev,  ]
  }
  #Confidence interval data for increments
  if(Increment == 1) {
    agr_m <- aggregate(dataf[, y], list(dataf[, x] , dataf[, z]), FUN="mean", na.rm=TRUE)
    agr_sd <- aggregate(dataf[, y], list(dataf[, x] , dataf[, z]), FUN="sd", na.rm=TRUE)
    agr_n <- aggregate(dataf[, y], list(dataf[, x] , dataf[, z]), FUN="length")
    agr_df <- data.frame(x_lev=agr_m[, 1], z_lev=as.integer(c(agr_m[, 2])) , agr_m=agr_m[, 3], agr_sd=agr_sd[, 3], agr_n=agr_n[, 3])
  } else {
    agr_m <- aggregate(dataf[, y], list(dataf[, x] , ceiling(dataf[, z]/Increment) ), FUN="mean", na.rm=TRUE)
    agr_sd <- aggregate(dataf[, y], list(dataf[, x] , ceiling(dataf[, z]/Increment) ), FUN="sd", na.rm=TRUE)
    agr_n <- aggregate(dataf[, y], list(dataf[, x] , ceiling(dataf[, z]/Increment) ), FUN="length")
    agr_df <- data.frame(x_lev=agr_m[, 1], z_lev=as.integer(c(agr_m[, 2])), agr_m=agr_m[, 3], agr_sd=agr_sd[, 3], agr_n=agr_n[, 3])
  }
  #Calculates confidence intervals
  MOE <- qt((conf_lev/2)+.5, df=agr_df$agr_n - 1) * agr_df$agr_sd/sqrt(agr_df$agr_n)
  Lower <- agr_df$agr_m - MOE
  Upper <- agr_df$agr_m + MOE
  adf_alpha <- data.frame(cbind(PointEst=agr_df$agr_m, Lower=Lower, Upper=Upper))
  agr_df <- cbind(agr_df, adf_alpha)
  return(agr_df ) 
}

#Exact Poisson
fpconf <- function(x, xlev, y, z, dataf, conf_lev, Increment) {
  #Aggregates outcome by factor 
  if( is.null(xlev)) {
    dataf <- dataf
  } else {
    dataf <- dataf[ dataf[, x] %in% xlev,  ]
  }
  #Confidence interval data for increments
  if(Increment == 1) {
    agr_sum <- aggregate(dataf[, y] ~ dataf[, x]+ dataf[, z], FUN="sum", na.rm=TRUE)
    agr_n <- aggregate(dataf[, y] ~ dataf[, x]+ dataf[, z], FUN="length")
    agr_df <- data.frame(x_lev=agr_sum[, 1], z_lev=as.integer(c(agr_sum[, 2])), agr_sum=agr_sum[, 3], agr_n=agr_n[, 3])
  } else {
    agr_sum <- aggregate(dataf[, y] ~ dataf[, x]+ ceiling(dataf[, z]/Increment), FUN="sum", na.rm=TRUE)
    agr_n <- aggregate(dataf[, y] ~ dataf[, x]+ ceiling(dataf[, z]/Increment), FUN="length")
    agr_df <- data.frame(x_lev=agr_sum[, 1], z_lev=as.integer(c(agr_sum[, 2])), agr_sum=agr_sum[, 3], agr_n=agr_n[, 3])
  }
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

#Create levels that will appear in the aggregated only data
fncPltAgrLv <- function(All.Groups, Specific.Groups) {
  if(is.null(Specific.Groups) ) {
    fci_aggr_levs <- All.Groups
  } else {
    fci_aggr_levs <- Specific.Groups
  }
  return(fci_aggr_levs)
}

fci_plot_aggr_levs <- reactive({ 
  if(input$FCiCreate == "Yes") {
  fncPltAgrLv(All.Groups=fci_plot_groups(), Specific.Group=fci_plot_Group_Levels())
  }
})

fconf <- function(x=xcivar, xlev=xlev, y=ycivar, z=zcivar, dataf, conf_lev=ciconf_lev, 
                  Increment, Fci_type, Aggr, Aggr.Levs) {
  if(Aggr== "Yes: Aggregated data only") {
#    agr_df <- data.frame(x_lev=dataf[, x], z_lev=as.integer(c(dataf[, z])), agr_m=dataf[, y], agr_sd=0, agr_n=1)
    agr_df <- data.frame(x_lev=dataf[ dataf[, x] %in% Aggr.Levs, x], 
                         z_lev=as.integer(c( dataf[ dataf[, x] %in% Aggr.Levs, z] )), 
                         agr_m= dataf[ dataf[, x] %in% Aggr.Levs, y],
                         agr_sd=0, agr_n=1)
    adf_alpha <- data.frame(cbind(PointEst=agr_df$agr_m, Lower=agr_df$agr_m, Upper=agr_df$agr_m))
    agr_df <- cbind(agr_df, adf_alpha)
  } else {
    switch(Fci_type,                #"var" and can be used anywhere in server.r.
           "Mean (t)" =  ftconf(x, xlev, y, z, dataf, conf_lev, Increment), 
           "Proportion (binomial)" =  fbconf(x, xlev, y, z, dataf, conf_lev, Increment), 
           "Poisson (exact)" =  fpconf(x, xlev, y, z, dataf, conf_lev, Increment) 
    )
  }
}

#Reactive function that runs fconf above
fcidf <- reactive({                  #This indicates the data frame I will use.
  if(input$FCiCreate == "Yes") {
    fconf(x=input$fxcivar, xlev=fci_plot_Group_Levels(), y=input$fycivar, z=input$fzcivar, 
          dataf=df(), conf_lev=input$fciconf_lev, Increment=fci_Z_Increment(), 
          Fci_type=input$fci_type, Aggr=fCi_straight_line(), Aggr.Levs=fci_plot_aggr_levs() )
  }
})

################################################################################
#      Function to get overall trend confidence intervals, no grouping         # 
################################################################################
#Binomial
ftotBconf <- function(y, z, dataf, conf_lev, Increment) {
  #Calculates confidence intervals for single units or in increments
  if(Increment == 1) {
    agr_sum <- aggregate(dataf[, y], list(dataf[, z]), FUN="sum", na.rm=TRUE)
    agr_n <- aggregate(dataf[, y], list(dataf[, z]), FUN="length")
  } else {
    agr_sum <- aggregate(dataf[, y], list(ceiling(dataf[, z]/Increment) ), FUN="sum", na.rm=TRUE)
    agr_n <- aggregate(dataf[, y], list(ceiling(dataf[, z]/Increment) ), FUN="length")
  }
  agr_df <- data.frame(z_lev=as.integer(c(agr_sum[, 1])), agr_sum=agr_sum[, 2], agr_n=agr_n[, 2])
  agr_df <- cbind(agr_df, binconf(x=agr_df[,2], n=agr_df[,3], alpha=1 - conf_lev))
  rownames(agr_df) <- 1:nrow(agr_df)
  return(agr_df) 
}

#Continuous outcomes
ftotTconf <- function(y, z, dataf, conf_lev, Increment) {
  #Confidence interval data for increments
  if(Increment == 1) {
    agr_m <- aggregate(dataf[, y], list( dataf[, z]), FUN="mean", na.rm=TRUE)
    agr_sd <- aggregate(dataf[, y], list(dataf[, z]), FUN="sd", na.rm=TRUE)
    agr_n <- aggregate(dataf[, y], list(dataf[, z]), FUN="length")
    agr_df <- data.frame(z_lev=as.integer(c(agr_m[, 1])), agr_m=agr_m[, 2], agr_sd=agr_sd[, 2], agr_n=agr_n[, 2])
  } else {
    agr_m <- aggregate(dataf[, y], list(ceiling(dataf[, z]/Increment) ), FUN="mean", na.rm=TRUE)
    agr_sd <- aggregate(dataf[, y], list(ceiling(dataf[, z]/Increment) ), FUN="sd", na.rm=TRUE)
    agr_n <- aggregate(dataf[, y], list(ceiling(dataf[, z]/Increment) ), FUN="length")
    agr_df <- data.frame(z_lev= as.integer(c(agr_m[, 1])), agr_m=agr_m[, 2], agr_sd=agr_sd[, 2], agr_n=agr_n[, 2])
  }
  #Calculates confidence intervals
  MOE <- qt((conf_lev/2)+.5, df=agr_df$agr_n - 1) * agr_df$agr_sd/sqrt(agr_df$agr_n)
  Lower <- agr_df$agr_m - MOE
  Upper <- agr_df$agr_m + MOE
  adf_alpha <- data.frame(cbind(PointEst=agr_df$agr_m, Lower=Lower, Upper=Upper))
  agr_df <- cbind(agr_df, adf_alpha)
  return(agr_df ) 
}

#Exact Poisson
ftotPconf <- function(y, z, dataf, conf_lev, Increment) {
  #Confidence interval data for increments
  if(Increment == 1) {
    agr_sum <- aggregate(dataf[, y] ~ dataf[, z], FUN="sum", na.rm=TRUE)
    agr_n <- aggregate(dataf[, y] ~ dataf[, z], FUN="length")
    agr_df <- data.frame(z_lev= as.integer(c(agr_sum[, 1])), agr_sum=agr_sum[, 2], agr_n=agr_n[, 2])
  } else {
    agr_sum <- aggregate(dataf[, y] ~ ceiling(dataf[, z]/Increment), FUN="sum", na.rm=TRUE)
    agr_n <- aggregate(dataf[, y] ~ ceiling(dataf[, z]/Increment), FUN="length")
    agr_df <- data.frame(z_lev= as.integer(c(agr_sum[, 1])), agr_sum=agr_sum[, 2], agr_n=agr_n[, 2])
  }
  #Calculates confidence intervals
  adf_alpha <- matrix(ncol= 3, nrow= nrow(agr_df), byrow = TRUE)
  for (i in 1:nrow(agr_df)) {
    adf_alpha[i, ] <- unlist(poisson.test(x=agr_df[i,2], T=agr_df[i, 3], conf.level= conf_lev)[c("estimate","conf.int")])
  }
  adf_alpha <- data.frame(adf_alpha)
  colnames(adf_alpha) <- c("PointEst", "Lower", "Upper")
  agr_df <- cbind(agr_df, adf_alpha)
  return(agr_df=agr_df ) 
}

ftotconf <- function(y=ycivar, z=zcivar, dataf, conf_lev=ciconf_lev, Increment, Fci_type) {
  switch(Fci_type,                #"var" and can be used anywhere in server.r.
         "Mean (t)" =  ftotTconf(y, z, dataf, conf_lev, Increment), 
         "Proportion (binomial)" =  ftotBconf(y, z, dataf, conf_lev, Increment), 
         "Poisson (exact)" =  ftotPconf(y, z, dataf, conf_lev, Increment) 
  )
}

#Reactive function that runs ftotconf above
ftotCidf <- reactive({                  #This indicates the data frame I will use.
  if(input$FCiCreate == "Yes") {
    ftotconf(y=input$fycivar, z=input$fzcivar, dataf=df(), conf_lev=input$fciconf_lev, 
          Increment=fci_Z_Increment(), Fci_type=input$fci_type)
  }
})

###############################################################
## Function here for the point estimate, lower, upper bounds ##
###############################################################
ci_fac_fnc <- function(x_lev, z_lev, agr_df, NK, Straight.Line) {
  #ci_fac_fnc <- function(x_lev, z_lev, agr_df) {
  prmtrs <- c("PointEst", "Lower", "Upper")
  ctrs <- as.vector(unique(agr_df[, x_lev]))
  #Point est 
  ci_p <- list()
  if(Straight.Line== "No") {
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
  } else {
    for (i in 1:length(ctrs)) {
      x <- agr_df[agr_df[, x_lev] ==ctrs[i], z_lev]
      y <- agr_df[agr_df[, x_lev] ==ctrs[i], prmtrs[1]]
      ci_p[[i]] <- cbind(x, y_p=y, y)
    }
  }
  #Lower CI
  ci_l <- list()
  if(Straight.Line== "No") {
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
  } else {
    for (i in 1:length(ctrs)) {
      x <- agr_df[agr_df[, x_lev]==ctrs[i], z_lev]
      y <- agr_df[agr_df[, x_lev]==ctrs[i], prmtrs[2]]
      ci_l[[i]] <- cbind(x, y_l=y, y)
    }
  }
  #Upper CI
  ci_u <- list()
  if(Straight.Line== "No") {
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
  } else {
    for (i in 1:length(ctrs)) {
      x <- agr_df[agr_df[, x_lev]==ctrs[i], z_lev]
      y <- agr_df[agr_df[, x_lev]==ctrs[i], prmtrs[3]]
      ci_u[[i]] <- cbind(x, y_u=y, y)
    }
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
  ci_fac_fnc(x_lev="x_lev", z_lev="z_lev", agr_df=fcidf(), NK=input$FciNkKnots, Straight.Line= fCi_straight_line())
  }
})


############################################################
## Function for total point estimate, lower, upper bounds ##
############################################################ 
ci_tot_fac_fnc <- function(z_lev, agr_df, NK, Straight.Line) {
  #Parameters
  prmtrs <- c("PointEst", "Lower", "Upper")
  #Point est 
  ci_p <- list()
  if(Straight.Line== "No") {
    x <- agr_df[, z_lev]
    y <- agr_df[, prmtrs[1]]
    xx <- rcspline.eval(x, inclx=TRUE, nk=NK)
    knots <- attr(xx, "knots")
    coef <- lsfit(xx, y)$coef
    w <- rcspline.restate(knots, coef[-1], x="{\\rm BP}")
    xtrans <- eval(attr(w, "function"))
    ci_p <- cbind(x, y_p=coef[1] + xtrans(x), y)
  } else {
    x <- agr_df[, z_lev]
    y <- agr_df[, prmtrs[1]]
    ci_p <- cbind(x, y_p=y, y)
  }
  #Lower CI
  ci_l <- list()
  if(Straight.Line== "No") {
    x <- agr_df[, z_lev]
    y <- agr_df[, prmtrs[2]]
    xx <- rcspline.eval(x, inclx=TRUE, nk=NK)
    knots <- attr(xx, "knots")
    coef <- lsfit(xx, y)$coef
    w <- rcspline.restate(knots, coef[-1], x="{\\rm BP}")
    xtrans <- eval(attr(w, "function"))
    ci_l <- cbind(x, y_l=coef[1] + xtrans(x), y)
  } else {
    x <- agr_df[, z_lev]
    y <- agr_df[, prmtrs[2]]
    ci_l <- cbind(x, y_l=y, y)
  }
  #Upper CI
  ci_u <- list()
  if(Straight.Line== "No") {
    x <- agr_df[, z_lev]
    y <- agr_df[, prmtrs[3]]
    xx <- rcspline.eval(x, inclx=TRUE, nk=NK)
    knots <- attr(xx, "knots")
    coef <- lsfit(xx, y)$coef
    w <- rcspline.restate(knots, coef[-1], x="{\\rm BP}")
    xtrans <- eval(attr(w, "function"))
    ci_u <- cbind(x, y_u=coef[1] + xtrans(x), y)
  } else {
    x <- agr_df[, z_lev]
    y <- agr_df[, prmtrs[3]]
    ci_u <- cbind(x, y_u=y, y)
  }
  return(list(ci_p=ci_p, ci_l=ci_l, ci_u=ci_u #, 
              #max_pest=max_pest, min_pest=min_pest, max_ci=max_ci, min_ci=min_ci
  ))
}

#Reactive function that runs ci_fac_fnc() above
fci_tot_fac <- reactive({                  #This indicates the data frame I will use.
  if(input$FCiCreate == "Yes") {
    ci_tot_fac_fnc(z_lev="z_lev", agr_df=ftotCidf(), NK=input$FciNkKnots, 
               Straight.Line= fCi_straight_line())
  }
})


############################################################
##            Function to create the time plot            ##
############################################################
plot_fci_fnc <- function(x, y, z, xcivar, ycivar, zcivar, dataf, LCol, LWd, Fci.Fac, 
                         ci_p, ci_l, ci_u,max_pest, min_pest, max_ci, min_ci, ctrs, 
                         cibands, fCiXLim1, fCiXLim2, fCiYLim1, fCiYLim2, Tot.Line, FCI.Tot,
                         FCI.Tot.Straight, Conf.Intrv, Tgt.Line, Straight.Line, Time.Pt.Line,
                         ci_p_tot, ci_l_tot, ci_u_tot, Tot.Color, Tgt.Color, Tpt.Color, 
                         T3.Line.Width, Text.Size, LType, labMulti=1) {
  #Make text out of the confidence level
  ConINT <- paste0(as.character(Conf.Intrv*100), "%")
  #Main title
  if(cibands == "Yes") {
    Main.Title <- paste0( ycivar, " trajectories per ", xcivar,  " by ", zcivar, " with ", ConINT, " confidence bands")
  } else {
    Main.Title <- paste0( ycivar, " trajectories per ", xcivar,  " by ", zcivar)
  }
  #Line Type
  if(is.null( eval(parse(text=LType )) )) {
    line_type <- 1:length(ctrs)
  } else {
    #line_type <- LType
    line_type <- as.numeric(eval(parse(text=LType )))
  }
  #Set up colors
  my_clr <- LCol
  par(mar = c(6, 6, 3, 1) + 0.1)
  plot(unique(dataf[, z]), seq(min(min_ci, na.rm=T), max(max_ci, na.rm=T), 
                               length.out=length(unique(dataf[, z]))), type="n",  
       #cex.lab=1*labMulti, cex.main=1.35, cex.sub=1*labMulti, 
       axes=F, ylab="", xlab="", 
       xlim=c(fCiXLim1, fCiXLim2), ylim=c(fCiYLim1, fCiYLim2)
       )
  title(Main.Title, cex.main = 1.1*labMulti) 
  axis(1, las=1, cex.axis=1*labMulti )
  axis(2, las=3, cex.axis=1*labMulti )
  mtext(zcivar, side = 1, line = 4, cex=1.1*labMulti )
  mtext(ycivar, side = 2, line = 4, cex=1.1*labMulti )
  box()
  #Plot point estimate lines
  ci_time <- list() 
  l95 <- list() 
  u95 <- list() 
  xx_t <- list() 
  yy_t <- list() 
  #Confidence bands
  if(cibands == "Yes") {
    if(Straight.Line == "Yes") {
      for (i in 1:length(ctrs)) {
        ci_time[[i]] <- ci_l[[i]][,1]
        l95[[i]] <- ci_l[[i]][, "y"]
        u95[[i]] <- ci_u[[i]][, "y"]
        xx_t[[i]] <- c(ci_time[[i]], rev(ci_time[[i]]))
        yy_t[[i]] <- c(l95[[i]], rev(u95[[i]]))
        polygon(unlist(xx_t[[i]]), unlist(yy_t[[i]]), col = adjustcolor(my_clr[i], alpha.f = 0.4), 
                border=adjustcolor(my_clr[i], alpha.f = 0.4))
      }   
    } else {
      for (i in 1:length(ctrs)) {
        ci_time[[i]] <- ci_l[[i]][,1]
        l95[[i]] <- ci_l[[i]][, 2] #"y_p"
        u95[[i]] <- ci_u[[i]][, 2] #"y_p"
        xx_t[[i]] <- c(ci_time[[i]], rev(ci_time[[i]]))
        yy_t[[i]] <- c(l95[[i]], rev(u95[[i]]))
        polygon(unlist(xx_t[[i]]), unlist(yy_t[[i]]), col = adjustcolor(my_clr[i], alpha.f = 0.4), 
                border=adjustcolor(my_clr[i], alpha.f = 0.4))
      }
    } 
  }
  #Add text names
  if(Straight.Line == "Yes") {
    for (i in 1:length(ctrs)) {
      lines(ci_p[[i]][, "x"], ci_p[[i]][, "y"], lty= line_type[i], col= my_clr[i], lwd=LWd)
      text(ci_p[[i]][1, "x"], ci_p[[i]][1, "y"], ctrs[i], cex= Text.Size, col=my_clr[i])
      text(ci_p[[i]][nrow(ci_p[[i]]), "x"], ci_p[[i]][nrow(ci_p[[i]]), "y"], ctrs[i], cex= Text.Size, col=my_clr[i])
    }
  } else {
    for (i in 1:length(ctrs)) {
      lines(ci_p[[i]][, "x"], ci_p[[i]][, "y_p"], lty= line_type[i], col= my_clr[i], lwd=LWd)
      text(ci_p[[i]][1, "x"], ci_p[[i]][1, "y_p"], ctrs[i], cex= Text.Size, 
           col=my_clr[i])
      text(ci_p[[i]][nrow(ci_p[[i]]), "x"], ci_p[[i]][nrow(ci_p[[i]]), "y_p"], ctrs[i], cex= Text.Size, 
           col=my_clr[i])
    }
  }
  
  #Plot point estimate lines for the overall trend
  ci_time_tot <- list() 
  l95_tot <- list() 
  u95_tot <- list() 
  xx_t_tot <- list() 
  yy_t_tot <- list() 
  #Confidence bands
  if (Tot.Line == "Line with band") {
    if(Straight.Line == "Yes") {
      ci_time_tot <- ci_l_tot[,1]
      l95_tot <- ci_l_tot[, "y"]
      u95_tot <- ci_u_tot[, "y"]
      xx_t_tot <- c(ci_time_tot, rev(ci_time_tot))
      yy_t_tot <- c(l95_tot, rev(u95_tot))
      polygon(unlist(xx_t_tot), unlist(yy_t_tot), col = adjustcolor(Tot.Color, alpha.f = 0.4), 
              border=adjustcolor(Tot.Color, alpha.f = 0.4))
    } else {
      ci_time_tot <- ci_l_tot[,1]
      l95_tot <- ci_l_tot[, 2] #"y_p"
      u95_tot <- ci_u_tot[, 2] #"y_p"
      xx_t_tot <- c(ci_time_tot, rev(ci_time_tot))
      yy_t_tot <- c(l95_tot, rev(u95_tot))
      polygon(unlist(xx_t_tot), unlist(yy_t_tot), col = adjustcolor(Tot.Color, alpha.f = 0.4), 
              border=adjustcolor(Tot.Color, alpha.f = 0.4))
    } 
  }
  #Add overall lines
  if (Tot.Line %in% c("Line", "Line with band") ) {
    if(Straight.Line == "Yes") {
      lines(ci_p_tot[, "x"], ci_p_tot[, "y"], lty=1, col= Tot.Color, lwd=T3.Line.Width)
      text(ci_p_tot[1, "x"], ci_p_tot[1, "y"], labels= "ALL", cex= Text.Size, col=Tot.Color)
      text(ci_p_tot[nrow(ci_p_tot), "x"], ci_p_tot[nrow(ci_p_tot), "y"], labels= "ALL", cex= Text.Size, col=Tot.Color)
    } else {
      lines(ci_p_tot[, "x"], ci_p_tot[, "y_p"], lty=1, col= Tot.Color, lwd=T3.Line.Width)
      text(ci_p_tot[1, "x"], ci_p_tot[1, "y_p"], labels= "ALL", cex= Text.Size, col=Tot.Color)
      text(ci_p_tot[nrow(ci_p_tot), "x"], ci_p_tot[nrow(ci_p_tot), "y_p"], labels= "ALL", cex= Text.Size, col=Tot.Color)
    }
  }
  
  #Add target line
  for (i in 1:length(Tgt.Line)) {
    abline(h= as.numeric(eval(parse(text=Tgt.Line[i] )) ), 
           col=Tgt.Color, lty=3, lwd=T3.Line.Width)
  }
  #Add time point line
  for (i in 1:length(Time.Pt.Line)) {
    abline(v= as.numeric(eval(parse(text=Time.Pt.Line[i] )) ), 
           col=Tpt.Color, lty=1, lwd=T3.Line.Width)
  }
  
}

############################################################
##            Function to create the ITSA data            ##
############################################################
plot_itsa_df_fnc <- function(I.Model, I.df, I.dd, I.y, I.x, Model.x, I.treatment,
                             I.time, I.time.pt) {
  #The ITSA model
  itsa_mdl <- I.Model
  #ITSA data frame
  itsa_df_nm <- I.df
  #data distribution
  itsa_dd <- I.dd
  #ITSA outcome
  itsa_Y <- I.y
  #ITSA specific variables
  itsa_var_ls <- I.x
  #All variables
  itsa_all_var_ls <- Model.x
  #Covariates not used to construct dummy coding for time
  itsa_cov_ls <- setdiff(itsa_all_var_ls, itsa_var_ls)
  #ITSA treatment variable
  if (!is.null(I.treatment)) {
    itsa_trt_bin <- I.treatment
  } else {
    itsa_trt_bin <- NA
  }
  #ITSA time variable
  itsa_time_var <- I.time
  #ITSA intervention time points
  itsa_time_points <- I.time.pt
  #Get the unique years
  itsa_yr_df <- sort(unique(itsa_df_nm[, itsa_time_var]))
  
  #Get the intervention/treatment group value
  if (!is.na(itsa_trt_bin)) {
  itsa_trt_grp_val <- itsa_dd[["limits"]][which(rownames(itsa_dd[["limits"]]) =="High"),  
                                          which(colnames(itsa_df_nm) == itsa_trt_bin)]
  }
  #Get the control group value
  if (!is.na(itsa_trt_bin)) {
    itsa_ctl_grp_val <- itsa_dd[["limits"]][which(rownames(itsa_dd[["limits"]]) =="Low"),  
                                          which(colnames(itsa_df_nm) == itsa_trt_bin)]
  }
  ## Get aggregated outcome values for each year ##
  #Intervention
  if (!is.na(itsa_trt_bin)) {
    itsa_aggr_Y_int <- aggregate(itsa_df_nm[ itsa_df_nm[, itsa_trt_bin]==itsa_trt_grp_val, itsa_Y]  ~ itsa_df_nm[ itsa_df_nm[, itsa_trt_bin]==itsa_trt_grp_val, itsa_time_var]  , FUN="mean", na.rm=T)
  colnames(itsa_aggr_Y_int) <- c(itsa_time_var, itsa_Y)
  } else {
    itsa_aggr_Y_int <- aggregate(itsa_df_nm[ , itsa_Y]  ~ itsa_df_nm[ , itsa_time_var]  , FUN="mean", na.rm=T)
    colnames(itsa_aggr_Y_int) <- c(itsa_time_var, itsa_Y)
  }
  #Control
  if (!is.na(itsa_trt_bin)) {
    itsa_aggr_Y_ctl <- aggregate(itsa_df_nm[ itsa_df_nm[, itsa_trt_bin]==itsa_ctl_grp_val, itsa_Y]  ~ itsa_df_nm[ itsa_df_nm[, itsa_trt_bin]==itsa_ctl_grp_val, itsa_time_var]  , FUN="mean", na.rm=T)
  colnames(itsa_aggr_Y_ctl) <- c(itsa_time_var, itsa_Y)
  }
  
  #Get minimal data for the intervention group
  #--be careful about where I put new lines at because it will return too much
  if (!is.na(itsa_trt_bin)) {
    itsa_cov_trt_df <- itsa_df_nm[itsa_df_nm[,itsa_trt_bin] == itsa_trt_grp_val , 
                                which(colnames(itsa_df_nm) %in% itsa_all_var_ls) ][!duplicated( 
                                  itsa_df_nm[itsa_df_nm[,itsa_trt_bin] == itsa_trt_grp_val , ][itsa_time_var]), ]
  } else {
    itsa_cov_trt_df <- itsa_df_nm[, which(colnames(itsa_df_nm) %in% itsa_all_var_ls) ][!duplicated( 
                                    itsa_df_nm[, ][itsa_time_var]), ]
}
  #merge in average outcome by treatment group
  itsa_cov_trt_df <- merge(itsa_cov_trt_df, itsa_aggr_Y_int, by=itsa_time_var)

  #Get minimal data for the control group
  #--be careful about where I put new lines at because it will return too much
  if (!is.na(itsa_trt_bin)) {
    itsa_cov_ctl_df <- itsa_df_nm[itsa_df_nm[,itsa_trt_bin] == itsa_ctl_grp_val , 
                                which(colnames(itsa_df_nm) %in% itsa_all_var_ls) ][!duplicated( 
                                  itsa_df_nm[itsa_df_nm[,itsa_trt_bin] == itsa_ctl_grp_val , ][itsa_time_var]), ]
  } 
  #merge in average outcome by treatment group
  if (!is.na(itsa_trt_bin)) {
    itsa_cov_ctl_df <- merge(itsa_cov_ctl_df, itsa_aggr_Y_ctl, by=itsa_time_var)
}
  
    #Combine data into 1 dataset for plotting
  if (!is.na(itsa_trt_bin)) {
    itsa_pred_plot_df <- rbind(itsa_cov_trt_df, itsa_cov_ctl_df)
  #Get rownames in numerical order
  rownames(itsa_pred_plot_df) <- 1:nrow(itsa_pred_plot_df)
  } else {
    itsa_pred_plot_df <- itsa_cov_trt_df
    #Get rownames in numerical order
    rownames(itsa_pred_plot_df) <- 1:nrow(itsa_pred_plot_df)
}

  #For loop to change covariates to adjusted values
  for (i in 1:length(itsa_cov_ls)) {
    itsa_pred_plot_df[, which(colnames(itsa_pred_plot_df) == itsa_cov_ls[i]) ] <- 
      itsa_dd[[ "limits"]][rownames(itsa_dd[[ "limits"]]) == "Adjust to", which(colnames(itsa_dd[[ "limits"]]) == itsa_cov_ls[i])]
    
  }
  
  ## Get predicted scores for plotting data ##
  itsa_pred_plot_df$itsa_yhat <- predict(itsa_mdl, newdata= itsa_pred_plot_df)
  
  return("ITSA.DF"=itsa_pred_plot_df)
  ## End of function ##
}


############################################################
##            Function to create the ITSA plot            ##
############################################################
plot_itsa_fci_fnc <- function(xcivar, ycivar, zcivar, dataf, LCol,  LWd,
                              fCiXLim1, fCiXLim2, fCiYLim1, fCiYLim2, 
                              Tgt.Line, Time.Pt.Line, Tgt.Color, Tpt.Color, 
                              T3.Line.Width, Text.Size, labMulti, 
                              #From plot_itsa_df_fnc 
                               I.treatment, I.time.pt, Lgd.Loc, I.df, I.time) {
  itsa_df <- dataf
  #ITSA data frame
  itsa_df_nm <- I.df
  #ITSA time variable
  itsa_time_var <- I.time
  #Get the unique years
  itsa_yr_df <- sort(unique(itsa_df_nm[, itsa_time_var]))
  #ITSA treatment variable
  if (!is.null(I.treatment)) {
    itsa_trt_bin <- I.treatment
  } else {
    itsa_trt_bin <- NA
  }
  #ITSA intervention time points
  itsa_time_points <- I.time.pt
  #Set up colors, modify so a dummy color for "Treatment at..." gets added
  #my_clr <- LCol
  #Intervention group
  if (!is.na(itsa_trt_bin)) {
    my_clr <- c(LCol[1:4], "black", LCol[5:6])
  } else {
    my_clr <- c(LCol[1:2], "black", LCol[3])
  }
  
  #Length of my_clr so it is at least a minimum of 5
  if (!is.na(itsa_trt_bin)) {
    lth_my_clr <- max(5, sum(complete.cases(my_clr)), na.rm=TRUE)
  } else {
    lth_my_clr <- max(3, sum(complete.cases(my_clr)), na.rm=TRUE)
  }
  
  #Graph from plot_fci_fnc
  #Intervention group
  if (!is.na(itsa_trt_bin)) {
    itsa_period_values_trt <- cut(itsa_df[, zcivar][1:(nrow(itsa_df)/2)], 
                                   c(min(itsa_yr_df, na.rm=T) -1, itsa_time_points, max(itsa_yr_df, na.rm=T) +1), right=FALSE)
  } else {  
    itsa_period_values_trt <- cut(itsa_df[, zcivar], 
                                   c(min(itsa_yr_df, na.rm=T) -1, itsa_time_points, max(itsa_yr_df, na.rm=T) +1), right=FALSE)  }
  #Control group
  if (!is.na(itsa_trt_bin)) {
    itsa_period_values_ctl <- cut(itsa_df[, zcivar][((nrow(itsa_df)/2) + 1): nrow(itsa_df)], 
                                   c(min(itsa_yr_df, na.rm=T) -1, itsa_time_points, max(itsa_yr_df, na.rm=T) +1), right=FALSE)
  } else {  
    itsa_period_values_ctl <- NA
  }
  itsa_period_levels <- unique(itsa_period_values_trt)
  itsa_period_levels
  itsaPRED <- "itsa_yhat"
  #Main title
  if (!is.na(itsa_trt_bin)) {
    Main.Title <- paste0( ycivar, " trajectories per ", xcivar,  " by ", zcivar)
  } else {
    Main.Title <- paste0( ycivar, " trajectories", " by ", zcivar)
  }
  
  par(mar = c(6, 6, 3, 1) + 0.1)
  plot(itsa_df[, zcivar], itsa_df[, ycivar], type="n",  
       axes=F, ylab="", xlab="", xlim=c(fCiXLim1, fCiXLim2), ylim=c(fCiYLim1, fCiYLim2))
  #Intervention group
  if (!is.na(itsa_trt_bin)) {
    points(itsa_df[ itsa_df[, itsa_trt_bin] == max(itsa_df[, itsa_trt_bin], na.rm=T) , zcivar], 
           itsa_df[itsa_df[, itsa_trt_bin] == max(itsa_df[, itsa_trt_bin], na.rm=T), ycivar], 
           col=my_clr[1], cex=LWd*.5, lwd=LWd)
  } else {
    points(itsa_df[, zcivar], itsa_df[, ycivar], col=my_clr[1], cex=LWd*.5, lwd=LWd)
  }
  #Control group
  if (!is.na(itsa_trt_bin)) {
    points(itsa_df[itsa_df[, itsa_trt_bin] == min(itsa_df[, itsa_trt_bin], na.rm=T) , zcivar], 
           itsa_df[itsa_df[, itsa_trt_bin] == min(itsa_df[, itsa_trt_bin], na.rm=T), ycivar],
           col=my_clr[2], cex=LWd*.5, lwd=LWd)
  } 
  
  title(Main.Title, cex.main = 1.1*labMulti) 
  axis(1, las=1, cex.axis=1*labMulti )
  axis(2, las=3, cex.axis=1*labMulti )
  mtext(zcivar, side = 1, line = 4, cex=1.1*labMulti )
  mtext(ycivar, side = 2, line = 4, cex=1.1*labMulti )
  #Intervention
  for (i in 1:length(itsa_period_levels)) {
    if (!is.na(itsa_trt_bin)) {
    lines(itsa_df[itsa_period_values_trt == itsa_period_levels[i] & itsa_df[, itsa_trt_bin] == max(itsa_df[, itsa_trt_bin], na.rm=T) , zcivar], 
          itsa_df[itsa_period_values_trt == itsa_period_levels[i] & itsa_df[, itsa_trt_bin] == max(itsa_df[, itsa_trt_bin], na.rm=T), itsaPRED], 
          col=my_clr[3], lwd=LWd)
    } else {
      lines(itsa_df[itsa_period_values_trt == itsa_period_levels[i]  , zcivar], 
            itsa_df[itsa_period_values_trt == itsa_period_levels[i] , itsaPRED], 
            col=my_clr[2], lwd=LWd)
    }
  }
  #Control group
  if (!is.na(itsa_trt_bin)) {
    for (i in 1:length(itsa_period_levels)) {
      lines(itsa_df[itsa_period_values_trt == itsa_period_levels[i] & itsa_df[, itsa_trt_bin] == min(itsa_df[, itsa_trt_bin], na.rm=T) , zcivar], 
            itsa_df[itsa_period_values_trt == itsa_period_levels[i] & itsa_df[, itsa_trt_bin] == min(itsa_df[, itsa_trt_bin], na.rm=T), itsaPRED], 
            col=my_clr[4], lwd=LWd)
    }
  }
  #Standard regression line colors require their to be extra colors listed than the default number
  #Intervention
  if (!is.na(itsa_trt_bin)) {
    abline(lm(as.formula(paste(paste0(ycivar , "~", zcivar))), 
              data=itsa_df[itsa_df$treatment==1,]), lty=2, col=my_clr[6], lwd=LWd)  
  } else {
    abline(lm(as.formula(paste(paste0(ycivar , "~", zcivar))), 
              data=itsa_df), lty=2, col=my_clr[4], lwd=LWd)  
  }
  #Control
  if (!is.na(itsa_trt_bin)) {
    abline(lm(as.formula(paste(paste0(ycivar , "~", zcivar))), 
              data=itsa_df[itsa_df$treatment==0,]), lty=2, col=my_clr[7], lwd=LWd)  
  } 
  #Time and target lines
  #Add time point line
  for (i in 1:length(Time.Pt.Line)) {
    abline(v= as.numeric(eval(parse(text=Time.Pt.Line[i] )) ), 
           col=Tpt.Color, lty=1, lwd=T3.Line.Width)
  }
  #Add target line
  for (i in 1:length(Tgt.Line)) {
    abline(h= as.numeric(eval(parse(text=Tgt.Line[i] )) ), 
           col=Tgt.Color, lty=3, lwd=T3.Line.Width)
  }
  #Legend description
  #Intervention group
  if (!is.na(itsa_trt_bin)) {
    itsa_legend <- c("Treatment observed means","Control observed means",
                     "Treatment ITSA lines", "Control ITSA lines", 
                     paste0("Interruption at ", paste(sort(itsa_time_points), collapse=" & ") ),
                     "Treament regression line", "Control regression line")
  } else {
    itsa_legend <- c("Observed means",
                     "ITSA trend lines", 
                     paste0("Interruption at ", paste(sort(itsa_time_points), collapse=" & ") ),
                     "Regression line")
  }
  #Legend line types
  #Intervention group
  if (!is.na(itsa_trt_bin)) {
    itsa_legend_lty <- c(0,0,1,1,0,2,2) 
  } else {
    itsa_legend_lty <- c(0,1,0,2)
  }
  #Legend PCH type
  #Intervention group
  if (!is.na(itsa_trt_bin)) {
    itsa_legend_pch <- c(1,1,NA,NA,NA,NA,NA)
  } else {
    itsa_legend_pch <- c(1,NA,NA,NA)
  }     #lth_my_clr gives a minimum of 3 or 5 things in the legend or more if we use regression lines  
  legend(Lgd.Loc, legend=itsa_legend[1:lth_my_clr], lty=itsa_legend_lty[1:length(my_clr)], 
         lwd=(1+Text.Size), pch= itsa_legend_pch, col=my_clr, bty="n", cex=Text.Size)
  box()
  
}

#Confidence interval plot reactive function
plot_fci <- reactive({                  #This indicates the data frame I will use.
  if(input$FCiCreate == "Yes") {
    plot_fci_fnc(x="x_lev", y="PointEst", z="z_lev", xcivar=input$fxcivar, ycivar=input$fycivar, zcivar=input$fzcivar,
                 dataf=fcidf(), LCol= fci_plot_Line_Colors(), LWd=fci_plot_Line_Width(), ci_p=fci_fac()$ci_p, 
                 ci_l=fci_fac()$ci_l, ci_u=fci_fac()$ci_u,
    max_pest=fci_fac()$max_pest, min_pest=fci_fac()$min_pest, max_ci=fci_fac()$max_ci, min_ci=fci_fac()$min_ci, 
    ctrs=fci_fac()$ctrs, cibands=input$fcibands, fCiXLim1=input$fCiXLim1, fCiXLim2=input$fCiXLim2, 
    fCiYLim1=input$fCiYLim1, fCiYLim2=input$fCiYLim2, Tot.Line=fci_overall_line(), FCI.Tot=fci_all_line(), 
    FCI.Tot.Straight=fci_tot_group_aggr(), Conf.Intrv=input$fciconf_lev, 
    Tgt.Line=fCi_target_line(), Straight.Line= fCi_straight_line(), Time.Pt.Line= fCi_time_point_line(), 
    ci_p_tot=fci_tot_fac()$ci_p, ci_l_tot=fci_tot_fac()$ci_l, ci_u_tot=fci_tot_fac()$ci_u,
    Tot.Color=fci_plot_Overall_Line_Colors(), Tgt.Color=fci_plot_Target_Line_Colors(), 
    Tpt.Color=fci_plot_Time_Point_Line_Colors(), T3.Line.Width=fci_plot_targ_time_Line_Wd(), 
    Text.Size= fci_plot_text_label_size(), LType=fci_plot_group_line_type(), 
    labMulti=FCI_plot_label_multiplier() )
    }
})

#Get data from ITSA to plot the results
plot_itsa_DF <- reactive({
  if(itsa_yes_no() == "Yes") {
    plot_itsa_df_fnc(I.Model=fit1(), I.df=df(), I.dd=dd_df, 
                     I.y=input$fycivar, I.x=itsa_X_Variables(), 
                     Model.x=predictor(), I.treatment=itsa_treatment_indicator(),
                     I.time=input$fzcivar, I.time.pt=itsa_time_periods())
  }
})

#ITSA plot reactive function
plot_itsa <- reactive({                  #This indicates the data frame I will use.
  if(itsa_yes_no() == "Yes") {
    plot_itsa_fci_fnc(xcivar=input$fxcivar, ycivar=input$fycivar, zcivar=input$fzcivar, dataf=plot_itsa_DF(), 
                      LCol=fci_plot_Line_Colors(),  LWd=fci_plot_Line_Width(), 
                      fCiXLim1=input$fCiXLim1, fCiXLim2=input$fCiXLim2, 
                      fCiYLim1=input$fCiYLim1, fCiYLim2=input$fCiYLim2, 
                      Tgt.Line=fCi_target_line(), Time.Pt.Line=fCi_time_point_line(), 
                      Tgt.Color=fci_plot_Target_Line_Colors(), Tpt.Color=fci_plot_Time_Point_Line_Colors(), 
                      T3.Line.Width=fci_plot_targ_time_Line_Wd(), Text.Size=fci_plot_text_label_size(), 
                      labMulti=FCI_plot_label_multiplier(), I.treatment=itsa_treatment_indicator()[1], 
                      I.time.pt=itsa_time_periods(), Lgd.Loc=itsa_legend_location(), 
                      I.df=df(), I.time= input$fzcivar)
  }
})

## Get the overall group trend rates ##
fci_tot_group_aggr <- reactive({
  fncFciTotMn(y=input$fycivar, z=input$fzcivar, dataf=df(), Increment= fci_Z_Increment())
})
## Get the overall group trend line ##
fci_all_line <- reactive({
   fncAllTrndSpln(agr_df=fci_tot_group_aggr(), NK=input$FciNkKnots, Straight.Line= fCi_straight_line())
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
#4. Select the rolling time period 
output$FCIzInc <- renderUI({                                 
  numericInput("fciZinc", "4. Select time increments (3= 3 months).", 
               value = 1, step = 1, min=1)     
})
fci_Z_Increment <- reactive({                 
  input$fciZinc 
})
#Select specific groups
output$fciplot_grp_levs <- renderUI({                                 
  selectInput("fciPlotGrpLvs", "5. Highlight specific groups?", 
              choices = sort(fci_plot_groups()), multiple=TRUE)     
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
  selectInput("fci_type", "6. Select the type of confidence interval.",
              choices = c("Proportion (binomial)", "Mean (t)", "Poisson (exact)"),
              selected= "Mean (t)", multiple=FALSE)
})

#Select the confidence interval level
output$FCi_Conf_Lev <- renderUI({                                 
  numericInput("fciconf_lev", "7. Enter the confidence level.",
               value = .95, min=.01, max = .99, step = .01)
})
#Select the Confidence bands.
output$FCI_bands <- renderUI({                                 #Same idea as output$vy
  selectInput("fcibands", "8. Do you want confidence bands?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     #Will make choices based on my reactive function.
})
#Select line colors
output$fci_plot_ln_clrs <- renderUI({                                 
  selectInput("fciPltLnClr", "9. Select line colors.", 
              choices = xyplot_Line_Color_Names(), multiple=TRUE, 
              selected= xyplot_Line_Color_Names()[1:length(fci_plot_groups())] )     
})
#Reactive function for directly above
fci_plot_Line_Colors <- reactive({                 
  input$fciPltLnClr 
})

#Select line width
output$fci_plot_ln_wdth <- renderUI({                                 
  numericInput("fciPltLnWd", "11. Select the group line width.", 
               value = 2, min=0, step = 1)     
})
#Reactive function for directly above
fci_plot_Line_Width <- reactive({                 
  input$fciPltLnWd 
})

#Select how many knots I want
output$FCI_nk_knots <- renderUI({                                
      numericInput("FciNkKnots", "22. Select the number of spline knots.",
       value = 3, min=3, max = 10, step = 1)
})
#Select label size multiplier
output$FCI_plot_lab_multi <- renderUI({                                 
  numericInput("fciPltLabMlt", "23. Increase XY label sizes.",
               value = 1, min=.01, step = .1)
})
#Reactive function for directly above
FCI_plot_label_multiplier <- reactive({                 
  input$fciPltLabMlt 
})

#Select whether to run the 95% confidence interval or not
output$FCi_create <- renderUI({                                
  selectInput("FCiCreate", "20. Create the time plot?",
              choices = c("No", "Yes"),
              selected="No")
})
#Select whether to run the 95% confidence interval or not
output$FCi_ovral_line <- renderUI({
  selectInput("fciOvrLn", "13. Add the overall group line?",
              #              choices = c("No", "Yes"),
              choices = c("No", "Line", "Line with band"),
              selected="No")
})
#Reactive function for above
fci_overall_line <- reactive({ 
  input$fciOvrLn  
})
#Add a target line
output$FCi_Tgt_Line <- renderUI({                                 
  textInput("fciTgtLn", "14. Add a target line.",
            value = paste0('c( ', ')') )
})
#Reactive function for above
fCi_target_line <- reactive({ 
  input$fciTgtLn  
})
#Add a time point line
output$FCi_Tm_Pt_Line <- renderUI({                                 
  textInput("fciTmPtLn", "15. Add a time point line.",
            value = paste0('c( ', ')'))
})
#Reactive function for above
fCi_time_point_line <- reactive({ 
  input$fciTmPtLn  
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
  selectInput("fciStrtLn", "21. Use straight trend lines?",
              choices = c("No", "Yes", "Yes: Aggregated data only"),
              selected="No")
})
#13A. Reactive function for above
fCi_straight_line <- reactive({ 
  input$fciStrtLn  
})
#Select target and time line width
output$fci_plot_TgtTpt_ln_wdth <- renderUI({                                 
  numericInput("fciPlTgTpLnWd", "12. Select other line's width.", 
               value = 2, min=0, step = 1)     
})
#Reactive function for directly above
fci_plot_targ_time_Line_Wd <- reactive({                 
  input$fciPlTgTpLnWd 
})

#17. Select overall line color
output$fci_plot_ovral_ln_clrs <- renderUI({                                 
  selectInput("fciPltOLnClr", "17. Select 'overall' line color.", 
              choices = xyplot_Line_Color_Names(), multiple=FALSE, selected="black")     
})
#Reactive function for directly above
fci_plot_Overall_Line_Colors <- reactive({                 
  input$fciPltOLnClr 
})
#18. Select target line color
output$fci_plot_tgt_ln_clrs <- renderUI({                                 
  selectInput("fciPltTLnClr", "18. Select target line color.", 
              choices = xyplot_Line_Color_Names(), multiple=FALSE, selected="black")     
})
#Reactive function for directly above
fci_plot_Target_Line_Colors <- reactive({                 
  input$fciPltTLnClr 
})
#19. Select time point line color
output$fci_plot_time_pt_ln_clrs <- renderUI({                                 
  selectInput("fciPltTPLnClr", "19. Select time point line color.", 
              choices = xyplot_Line_Color_Names(), multiple=FALSE, selected="black")     
})
#Reactive function for directly above
fci_plot_Time_Point_Line_Colors <- reactive({                 
  input$fciPltTPLnClr 
})
#16. Select the line label size
output$fci_plot_txt_lbl_sz <- renderUI({                                 
  numericInput("fciPlTxtLblSz", "16. Select the line label size.", 
               value = 2, min=0, step = .1)     
})
#Reactive function for directly above
fci_plot_text_label_size <- reactive({                 
  input$fciPlTxtLblSz 
})
#24. Select whether to run the ITSA or not
output$ITSA_create <- renderUI({                                
  selectInput("itsaCreate", "24. Did you do an ITSA?",
              choices = c("No", "Yes"),
              selected="No")
})
#24A. Reactive function for creating the ITSA
itsa_yes_no <- reactive({
  input$itsaCreate
})
#25. Select ITSA variables
output$ITSA_I.x <- renderUI({ 
  selectInput("itsaXVar", "25. What are the ITSA variables?", 
              choices = predictor(), multiple=TRUE, selected=predictor()[1])     
})
#25A. Reactive function for legend location
itsa_X_Variables <- reactive({
  input$itsaXVar
})
#26. Select ITSA treatment indicator
output$ITSA_I.trt <- renderUI({ 
  selectInput("itsaTrtVar", "26. What is the treatment indicator?", 
              choices = itsa_X_Variables(), multiple=TRUE )
})
#26A. Reactive function for the ITSA treatment indicator
itsa_treatment_indicator <- reactive({
  input$itsaTrtVar
})
#27. Select the rolling time period 
output$ITSA_I.tm.pt <- renderUI({                                 
  selectInput("itsaTP", "27. When are the treatment periods?", 
              choices = seq(range_fzcivar()[1], range_fzcivar()[2], by =1), multiple=TRUE)     
})
#27A. Reactive function to get the intervention periods
itsa_time_periods <- reactive({                 
  input$itsaTP 
})
#28. Legend location
output$itsa_lgd_loc <- renderUI({                                
  selectInput("itsaLgdLoc", "28. Select the legend location.",        
              choices = c("bottomright","bottom","bottomleft","left","topleft","top","topright","right","center"), 
              multiple=FALSE, selected="topleft" ) 
})
#28A. Reactive function for legend location
itsa_legend_location <- reactive({
  input$itsaLgdLoc
})
#14. Indicate lower limit of x-axis
output$FCI__Xlim1 <- renderUI({
  numericInput("fCiXLim1", "29. Lower X-axis limit.",
               value = range_fzcivar()[1], step = 1)
})
#15. Indicate upper limit of x-axis
output$FCI__Xlim2 <- renderUI({
  numericInput("fCiXLim2", "30. Upper X-axis limit.",
               value = if(fci_Z_Increment() ==1) { range_fzcivar()[2] } else {ceiling(range_fzcivar()[2]/fci_Z_Increment() ) } , 
               step = 1)
})
#16. Indicate lower limit of y-axis
output$FCI__Ylim1 <- renderUI({
  numericInput("fCiYLim1", "31. Lower Y-axis limit.",
               value = range_fycivar()[1], step = .1)
})
#17. Indicate upper limit of x-axis
output$FCI__Ylim2 <- renderUI({
  numericInput("fCiYLim2", "32. Upper Y-axis limit.",
               value = range_fycivar()[2], step = .1)
})
#Add a target line
output$fci_plot_ln_typ <- renderUI({                                 
  textInput("fciPltLnTyp", "10. Change the group line type.",
            value = paste0('c( ', ')') )
})
#Reactive function for directly above
fci_plot_group_line_type <- reactive({                 
  input$fciPltLnTyp 
})

#Confidence interval plot for time
output$Plot_Fci_output <- renderPlot({ 
  #if(input$FCiCreate == "Yes") {
  if(itsa_yes_no() == "Yes") {
    plot_itsa()
  }  else {
    plot_fci()
  }
}, height = 800 )

#This prints the point estimates and confidence intervals
output$time_ci_out1 <- renderTable({
  if(input$FCiCreate == "Yes") {
    fcidf()
  }
}, rownames = TRUE, digits =3)
#This prints the point estimates and confidence intervals
output$all_time_ci_out1 <- renderTable({
  if(input$FCiCreate == "Yes") {
    ftotCidf()
  }
}, rownames = TRUE, digits =3)


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
#4. Pick a time for survival models
output$MI_set_seed <- renderUI({
  numericInput("MISetNumSeed", "4. Set random number seed.", value= 1, min=1, step=1) 
})
#4A. Random number seed
MI_number_seed <- reactive({
  input$MISetNumSeed
})
#Selects variables that won't get transformed or imputed
output$MIAsIs_x <- renderUI({                                 
  selectInput("MIasisx", "5. Variables not transformed nor splined.", 
              choices = predictor(), multiple=TRUE)     
})
#5. Determine if we should begin the multiple imputations.
output$MI_Begin <- renderUI({  
  selectInput("MIbegin", "6. Begin multiple imputation?", 
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
    set.seed(MI_number_seed() )
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
    set.seed(MI_number_seed() )
    transcan(mi_fmla(), imputed=TRUE, transformed=FALSE, trantab=TRUE, pl=FALSE, show.na=TRUE, data=df(), pr=FALSE, 
             nk=input$MIknots, 
             #n.impute=input$MInumber, 
             asis=input$MIasisx)
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
             ols(as.formula(input$up_fmla), x=TRUE, y=TRUE, data=new_imputed.si(), weights= eval(parse(text= weighting_regression_var())))
           } else {
             ols(mdl_fmla(), x=TRUE, y=TRUE, data=new_imputed.si(), weights= eval(parse(text= weighting_regression_var())))},
           "Logistic" = if(input$updy == "Yes") {
             lrm(as.formula(input$up_fmla), x=TRUE, y=TRUE, data=new_imputed.si(), tol=1e-100, maxit=20, 
                 weights= eval(parse(text= weighting_regression_var()))) #I added tol value so it can handle time predictor (YYMM)
           } else { #Added maxit to handle small samples. See https://stat.ethz.ch/pipermail/r-help/2017-September/449115.html  
             lrm(mdl_fmla(), x=TRUE, y=TRUE, data=new_imputed.si(), tol=1e-100, maxit=20, 
                 weights= eval(parse(text= weighting_regression_var())))},  #I added tol value so it can handle time predictor that causes "singularity"
           "Proportion Y Logistic" = if(input$updy == "Yes") {
             Glm(as.formula(input$up_fmla), x=TRUE, y=TRUE, data=new_imputed.si(), family=binomial(), weights= eval(parse(text= weighting_regression_var())))
           } else {
             Glm(mdl_fmla(), x=TRUE, y=TRUE, data=new_imputed.si(), family=binomial(), weights= eval(parse(text= weighting_regression_var())))},
           "Ordinal Logistic" = if(input$updy == "Yes") {
             orm(as.formula(input$up_fmla), x=TRUE, y=TRUE, data=new_imputed.si(), data=new_imputed.si())
           } else {
             orm(mdl_fmla(), x=TRUE, y=TRUE, data=new_imputed.si())},
           "Poisson" = if(input$updy == "Yes") {
             Glm(as.formula(input$up_fmla), x=TRUE, y=TRUE, data=new_imputed.si(), family=poisson(), weights= eval(parse(text= weighting_regression_var())))
           } else {
             Glm(mdl_fmla(), x=TRUE, y=TRUE, data=new_imputed.si(), family=poisson(), weights= eval(parse(text= weighting_regression_var())))},
           "Quantile" = if(input$updy == "Yes") {
             Rq(as.formula(input$up_fmla), x=TRUE, y=TRUE, data=new_imputed.si(), tau=as.numeric(rq_tau1()), weights= eval(parse(text= weighting_regression_var())))
           } else {
             Rq(mdl_fmla(), x=TRUE, y=TRUE, data=new_imputed.si(), tau=as.numeric(rq_tau1()), weights= eval(parse(text= weighting_regression_var())))},
           "Cox PH"   = if(input$updy == "Yes") {
             cph(cox_mdl_fmla1u(), x=TRUE, y=TRUE, data=new_imputed.si(), surv=TRUE, weights= eval(parse(text= weighting_regression_var())))
           } else {
             cph(cox_mdl_fmla1(), x=TRUE, y=TRUE, data=new_imputed.si(), surv=TRUE, weights= eval(parse(text= weighting_regression_var())))},
           "Cox PH with censoring" = if(input$updy == "Yes") {
             cph(cox_mdl_fmla2u(), x=TRUE, y=TRUE, data=new_imputed.si(), surv=TRUE, weights= eval(parse(text= weighting_regression_var())))
           } else {
             cph(cox_mdl_fmla2(), x=TRUE, y=TRUE, data=new_imputed.si(), surv=TRUE, weights= eval(parse(text= weighting_regression_var())))},
           #AFT models
           "AFT"   = if(input$updy == "Yes") {
             psm(aft_mdl_fmla1u(), data=new_imputed.si(), x=TRUE, y=TRUE, dist=AFT_PSM_Dist(), weights= eval(parse(text= weighting_regression_var())))
           } else {
             psm(aft_mdl_fmla1(), data=new_imputed.si(), x=TRUE, y=TRUE, dist=AFT_PSM_Dist(), weights= eval(parse(text= weighting_regression_var())))},
           "AFT with censoring" = if(input$updy == "Yes") {
             psm(aft_mdl_fmla2u(), data=new_imputed.si(), x=TRUE, y=TRUE, dist=AFT_PSM_Dist(), weights= eval(parse(text= weighting_regression_var())))
           } else {
             psm(aft_mdl_fmla2(), data=new_imputed.si(),  x=TRUE, y=TRUE, dist=AFT_PSM_Dist(), weights= eval(parse(text= weighting_regression_var())))},
           "Generalized Least Squares" = if(input$updy == "Yes") {
             Gls(as.formula(input$up_fmla), x=TRUE, data=new_imputed.si(), correlation=corCompSymm(form= gls_cor(), weights= eval(parse(text= weighting_regression_var()))))
           } else {
             Gls(mdl_fmla(), x=TRUE, data=new_imputed.si(),
                 correlation=corCompSymm(form= gls_cor()), weights= eval(parse(text= weighting_regression_var())))}
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
#8. Exact method
output$scatter_cor_regression_add_YN <- renderUI({  
  selectInput("sctrRgrLnYN", "8. Add the regression line?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     
})
#8A. Reactive function for Exact method
Scatter_Cor_Regression_Line_Yes_No <- reactive({
  input$sctrRgrLnYN
})
#9. Line color
output$sctr_crtst_clr <- renderUI({                                
  selectInput("sctrCrtstClr", "9. Select the line color(s).",        
              choices = xyplot_Line_Color_Names(), 
              multiple=TRUE, selected=xyplot_Line_Color_Names()[c(1,5)] ) 
})
#9A. Reactive function for alternative hypothesis test
scatter_cor_line_color <- reactive({
  input$sctrCrtstClr
})
output$scatter_cor_lgd_loc <- renderUI({                                
  selectInput("sctrCorLgdLoc", "10. Select the legend location.",        
              choices = c("bottomright","bottom","bottomleft","left","topleft","top","topright","right","center"), 
              multiple=FALSE, selected="topleft" ) 
})
#10A. Reactive function for legend location
scatter_cor_legend_location <- reactive({
  input$sctrCorLgdLoc
})

#11. Exact method
output$scatter_cor_test_run_YN <- renderUI({  
  selectInput("sctrCrtstYN", "11. Run the correlation and plot?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     
})
#11A. Reactive function for Exact method
Scatter_Cor_Test_Run_Yes_No <- reactive({
  input$sctrCrtstYN
})

#12. Run the function below
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
#12A.Correlation test output  
output$scatter_cor_test_cor_test_out <- renderPrint({
  if(Scatter_Cor_Test_Run_Yes_No() == "Yes") {
    scatter_cor_test_cor_run()
  }
})
#13. Run the function below
scatter_cor_test_plt_run <- reactive({
  if(Scatter_Cor_Test_Run_Yes_No() == "Yes") {    
    fncSctrPlt(DF=df(), X=scatter_cor_test_x(), Y=scatter_cor_test_y(), 
                 sct_plt_clr=scatter_cor_line_color(), CT=scatter_cor_test_cor_run(), 
               Add.Reg=Scatter_Cor_Regression_Line_Yes_No() , Leg.Loc= scatter_cor_legend_location()
    )
  }  
})
#13A.Scatter plot  
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
fncSctrPlt <- function(DF, X, Y, sct_plt_clr, CT, Add.Reg, Leg.Loc) {
  #Regression results
  if(Add.Reg =="Yes") {
    mod_fit_1 <- lm(DF[, Y] ~ DF[, X], data=DF)
  }
    #Title
    if(Add.Reg =="No") {
      Main.title <- paste0("Correlation of ", Y, " on ", X, " (R = ", round(as.numeric(CT["estimate"]), 3), 
                           ", ", "p-value= ", try(round(as.numeric(CT["p.value"]), 4)), ")")
    } else {
      Main.title <- paste0("Correlation of ", Y, " on ", X, " (R= ", round(as.numeric(CT["estimate"]), 3), 
                           ", ", "p-value= ", try(round(as.numeric(CT["p.value"]), 4)), 
                           ", Int.= ", try(round(as.numeric(mod_fit_1$coefficients[1]), 3)), 
                           ", Slope= ", try(round(as.numeric(mod_fit_1$coefficients[2]), 3)), ")" )
    }
    
  #Scatter plot
  scatter.smooth(DF[, X], DF[, Y] , main= Main.title, xlab=X, ylab=Y,
                 lpars =list(col = sct_plt_clr, lwd = 5, lty = 3))
  #Add regression line
  if(Add.Reg =="Yes") {
    abline(lm(DF[, Y] ~ DF[, X], data=DF), col= sct_plt_clr[2], lty=1, lwd= 5)
  }
  #Add Loess and regression trend lines
  if(Add.Reg =="Yes") {
    legend(Leg.Loc, legend=c("Loess trend","Regression trend"), col=sct_plt_clr[1:2],  #Leg.Loc sct_plt_clr
           lty= c(1,3), lwd= 2.5, cex = 2, bty="n", inset=c(0, .05))
  } else {
    legend(Leg.Loc, legend=c("Loess trend"), col=sct_plt_clr[1],  #Leg.Loc sct_plt_clr
           lty= 3, lwd= 2.5, cex = 2, bty="n", inset=c(0, .05))
  }
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
  selectInput("calcYN", "2. Calculate results or create a graph?", 
              choices = c("No", "Results", "Graph"), multiple=FALSE, selected="No")  
})
#2A. Reactive function for textbox to enter a formula
Calculator_YN <- reactive({
  input$calcYN
})                                                     
#Print the calculation 
output$prnt_calculation <- renderPrint({
  if (Calculator_YN() == "Results") {
    eval(parse(text=Calculator_Box_Input() ))
  }
})
#Plot the calculation 
  output$plt_calculation <- renderPlot({
  if (Calculator_YN() == "Graph") {
    eval(eval(parse(text=Calculator_Box_Input() )))
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
#2. Indicate if you want the histogram by factor
output$smry_var_hist_fac_yesno <- renderUI({                                 
  selectInput("smryVrHstFcYN", "2. Want graphs by factors?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     
})
#2A. Object for classification plot 
summary_var_hist_factor_yes_no <- reactive({
  input$smryVrHstFcYN
})
#3. Select the histogram variabe.
output$smry_var_hist_fac <- renderUI({
  selectInput("smryVrHstFct", "3. Select the factor name",
              choices = var(), multiple=FALSE, 
              selected= setdiff(var(), descriptive_summary_histogram_variable())[1] )
})
#3A. Reactive function for the variabe
descriptive_summary_histogram_factor <- reactive({
  input$smryVrHstFct
})
#4. Select the approximate number of histogram bars
output$smry_var_hist_bars <- renderUI({                                 
  numericInput("smryVrHstBrs", "4. Select approximate number of bars.", 
               value = 15, step = 1, min=2)     
})
#4A. Object for histogram bars 
summary_variable_histogram_bars <- reactive({
  input$smryVrHstBrs
})
#5. Bar color
output$smry_var_hist_bar_clr <- renderUI({                                
  selectInput("smryVrHstBrClr", "5. Select the bar colors.",        
              choices = xyplot_Line_Color_Names(), 
              multiple=FALSE, selected="blue" ) 
})
#5A. Reactive function for Bar color
summary_var_histogram_bar_color <- reactive({
  input$smryVrHstBrClr
})
#6. Select label size multiplier
output$smryVrHstLabMulti <- renderUI({                                 
  numericInput("smryVrHstLbMlt", "6. Increase XY label sizes.",
               value = 1.75, min=.01, step = .1)
})
#6a. Reactive function for directly above
summary_var_histogram_label_multiplier <- reactive({                 
  input$smryVrHstLbMlt
})
#7. Indicate if you want to show the mean and median
output$smry_hist_mn_med_yesno <- renderUI({                                 
  selectInput("smryHstMnMdYN", "7. Want to show the mean and median?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     
})
#7A. Object for the mean and median 
summary_hist_mean_median_yes_no <- reactive({
  input$smryHstMnMdYN
})
#8. Line colors
output$smry_var_hist_ln_clr <- renderUI({                                
  selectInput("smryVrHstLnClr", "8. Select Mean/Median line colors.",        
              choices = xyplot_Line_Color_Names(), 
              multiple=TRUE, selected="black" ) 
})
#8A. Reactive function for the line color
summary_var_histogram_line_color <- reactive({
  input$smryVrHstLnClr
})
#9. Indicate if you want the histogram
output$smry_var_hist_yesno <- renderUI({                                 
  selectInput("smryVrHstYN", "9. Do you want to run the histogram?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     
})
#9. Object for classification plot 
summary_var_hist_yes_no <- reactive({
  input$smryVrHstYN
})
#10. Run the histogram function below
summary_var_histogram_run <- reactive({
  if(summary_var_hist_yes_no() == "Yes") {    
    par(mar=c(4, 7, 4, 1))
    if(summary_var_hist_factor_yes_no() =="Yes") {
      #fncMltHstPlt <- function(DF, X, Y, BNS, CLR, LCLR, MN, MED, AddLine) {
        fncMltHstPlt(DF=df(), X=descriptive_summary_histogram_variable(), 
                     Y= descriptive_summary_histogram_factor(),
                  BNS=summary_variable_histogram_bars(), CLR=summary_var_histogram_bar_color(), 
                  LCLR=summary_var_histogram_line_color(), MN=smry_var_hist_mean(), 
                  MED=smry_var_hist_median(), AddLine=summary_hist_mean_median_yes_no(),
                  CEX.size= summary_var_histogram_label_multiplier() )
  }  else {
    fncSmryHist(DF=df(), X=descriptive_summary_histogram_variable(), 
                BNS=summary_variable_histogram_bars(), CLR=summary_var_histogram_bar_color(), 
                LCLR=summary_var_histogram_line_color(), MN=smry_var_hist_mean(), 
                MED=smry_var_hist_median(), AddLine=summary_hist_mean_median_yes_no(), 
                CEX.size= summary_var_histogram_label_multiplier())
  }
  }
})
#9A.histogram  
output$summary_var_histogram_out <- renderPlot({
  if(summary_var_hist_yes_no() == "Yes") {
    summary_var_histogram_run()
  }
})

###############################
## Function to get histogram ##
###############################
fncSmryHist <- function(DF, X, BNS, CLR, LCLR, MN, MED, AddLine, CEX.size) {
  hist(x=DF[, X], breaks=BNS, col=CLR, xlab= X,
       cex.lab=CEX.size, cex=CEX.size, cex.main=(CEX.size*.75), cex.axis= CEX.size,
       main= paste0("Histogram of ", X, " (Mean= ", round(MN, 3),", Median= ", round(MED, 3), ")"))
  #Add mean and median lines
  if (AddLine== "Yes") {
    abline(v=MN,  col= head(LCLR, 1), lwd=5, lty=1)
    abline(v=MED, col= tail(LCLR, 1), lwd=5, lty=2)
  }
}

###################################################
## Function to get histogram for multiple groups ##
###################################################
fncMltHstPlt <- function(DF, X, Y, BNS, CLR, LCLR, MN, MED, AddLine, CEX.size) {
  YLevs <- sort(unique(DF[, Y]))
  N_Levs <- length(YLevs)
  sqN <- sqrt(N_Levs)
  RowCol <- c(round(sqN), ceiling(sqN))
  par(mfrow=RowCol )
  
  MEAN <- list()
  MEDIAN <- list()
  for (i in 1:N_Levs) {
    MEAN[i] <- summary(DF[DF[ ,Y] == YLevs[i], X])["Mean"]
    MEDIAN[i] <- summary(DF[DF[ ,Y] == YLevs[i], X])["Median"]
  }
  #Get maximum X and Y dimensions for each level
  Xmax <- list()
  Xmin <- list()
  Ymax <- list()
  for (i in 1:N_Levs) {
    Xmin[i] <- min(hist(x=DF[DF[ ,Y] == YLevs[i], X], plot=FALSE, breaks=BNS)$breaks)
    Xmax[i] <- max(hist(x=DF[DF[ ,Y] == YLevs[i], X], plot=FALSE, breaks=BNS)$breaks)
    Ymax[i] <- max(hist(x=DF[DF[ ,Y] == YLevs[i], X], plot=FALSE, breaks=BNS)$counts)
  }
  #Create the histrograms  
  for (i in 1:N_Levs) {
    MN <- unlist(MEAN) 
    MED <- unlist(MEDIAN)
    hist(x=DF[DF[ ,Y] == YLevs[i], X], breaks=BNS, col=CLR, xlab= paste0(X, " for ", Y, ":", YLevs[i]),
         xlim= c(min(unlist(Xmin)), max(unlist(Xmax))), ylim= c(0, max(unlist(Ymax))),
         cex.lab=CEX.size, cex=CEX.size, cex.main=(CEX.size*.75), cex.axis= CEX.size,
         main= paste0(X, " (Mean= ", round(MN[i], 3),", Median= ", round(MED[i], 3), ")"))
    #Add mean and median lines
    if (AddLine== "Yes") {
      abline(v= MN[i],  col= head(LCLR, 1), lwd=5, lty=1)
      abline(v= MED[i], col= tail(LCLR, 1), lwd=5, lty=2)
    }
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

#3. Indicate if you want to run the trend function
output$dnsty_grp_bgn_yesno <- renderUI({                                 
  selectInput("dnsGrpBgnYN", "3. Begin grouping?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     
})
#3A. Object for yes/no running of the plot 
density_group_begin_yes_no <- reactive({
  input$dnsGrpBgnYN
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
#4. Select the specific groups.
output$dnsty_grp_trnd_Xlevs <- renderUI({
  if(density_group_begin_yes_no() == "Yes") {
    selectInput("dnsGrpTrXlev", "4. Select specific groups.",
                choices = sort(density_group_trend_grp_levels()), multiple=TRUE)
  } else {
    selectInput("dnsGrpTrXlev", "4. Select specific groups.",
                choices = "NA", multiple=TRUE, selected= "NA" )     
  }
})
#4A. Reactive function for the variable
density_group_trend_grp_X_levs <- reactive({
  input$dnsGrpTrXlev
})
#5. Select the time indicator.
output$dnsty_grp_trnd_Zvar <- renderUI({ 
  selectInput("dnsGrpTrnZ", "5. Select the time indicator.", 
              choices = setdiff(var(), c(density_group_trend_outcome(), density_group_trend_group() ) ), 
              multiple=FALSE, selected= setdiff(var(), c(density_group_trend_outcome(), density_group_trend_group() ) )[1])
})
#5A. Reactive function for the variable
density_group_trend_time <- reactive({
  input$dnsGrpTrnZ
})
#5B. Reactive function for the range of time
density_group_trend_range_time <- reactive({
  range(as.numeric(df()[, density_group_trend_time()]), na.rm=TRUE)
})
#6. Select the rolling time period 
output$dnsty_grp_trnd_Z_inc <- renderUI({                                 
  numericInput("dnsGrpTrnZInc", "6. Select time increments (3= 3 months).", 
               value = 1, step = 1)     
})
#6A. Reactive function for the variable
density_group_trend_Time_Increment <- reactive({
  input$dnsGrpTrnZInc
})
#7. Line color
output$dnsty_grp_trnd_ln_clr <- renderUI({                                
  selectInput("dnsGrpTrnLClr", "7. Select the line color.",        
              choices = xyplot_Line_Color_Names(), 
              multiple=FALSE, selected="red" ) 
})
#7A. Reactive function for line color
density_group_trend_line_color <- reactive({
  input$dnsGrpTrnLClr
})
#10. Select the rolling time period 
output$dnsty_grp_trnd_trgt <- renderUI({                                 
  numericInput("dnsGrpTrnTrgt", "10. Set a target.", 
               value = NULL, step = .01)     
})
#10A. Reactive function for the variable
density_group_trend_Target <- reactive({
  input$dnsGrpTrnTrgt
})
#11. Legend location
output$dnsty_grp_trnd_lgd_loc <- renderUI({                                
  selectInput("dnsGrpTrnLgdLoc", "11. Select the legend location.",        
              choices = c("bottomright","bottom","bottomleft","left","topleft","top","topright","right","center"), 
              multiple=FALSE, selected="topleft" ) 
})
#11A. Reactive function for legend location
density_group_trend_legend_location <- reactive({
  input$dnsGrpTrnLgdLoc
})
#12. Set the seed 
output$dnsty_grp_trnd_st_sed <- renderUI({                                 
  numericInput("dnsGrpTrnStSd", "12. Set (seed) group name randomness.", 
               value = 1, step = 1, min=1)     
})
#12A. Reactive function for the variable
density_group_trend_set_seed <- reactive({
  input$dnsGrpTrnStSd
})
#13. Indicate if you want to run the trend function
output$dnsty_grp_trnd_run_yesno <- renderUI({                                 
  selectInput("dnsGrpTrnRunYN", "13. Run the trend?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     
})
#13A. Object for yes/no running of the plot 
density_group_trend_run_yes_no <- reactive({
  input$dnsGrpTrnRunYN
})
#14. Speed of the animation play 
output$dnsty_grp_trnd_sec <- renderUI({                                 
  numericInput("dnsGrpTrnSec", "14. Play speed (1000= 1 second).", 
               value = 500, step = 100, min=0)     
})
#14A. Reactive function for the variable
density_group_trend_seconds <- reactive({
  input$dnsGrpTrnSec
})
#15. Play the trend graph
output$dnsty_grp_trnd_ply <- renderUI({
  if(density_group_begin_yes_no() == "Yes") {
    sliderInput("dnsGrpTrnPly", "15. Play the time trend.",   
                min= density_group_trend_range_time()[1], 
                max= density_group_trend_range_time()[2] - (density_group_trend_Time_Increment() - 1), 
                value =1, step=1, animate=list(interval= density_group_trend_seconds() ))  
  } else {
    sliderInput("dnsGrpTrnPly", "15. Play the time trend.",   
                min= 0, max= 1, value =0, step=1 )
  }
})
#15A. Reactive function for the variable
density_group_trend_play <- reactive({
  input$dnsGrpTrnPly
})
#16. Indicate lower limit of x-axis
output$dnsty_grp_trnd_Xlim1 <- renderUI({
  numericInput("dnsGrpTrnX1", "16. Lower X-axis limit.",
               value = min(density_group_trend_Y_density()[["x"]], na.rm=TRUE), step = .1)
})
#16A. Reactive function for the variable
density_group_trend_Xlim1 <- reactive({
  input$dnsGrpTrnX1
})
#17. Indicate upper limit of x-axis
output$dnsty_grp_trnd_Xlim2 <- renderUI({
  numericInput("dnsGrpTrnX2", "17. Upper X-axis limit.",
               value = max(density_group_trend_Y_density()[["x"]], na.rm=TRUE), step = .1)
})
#17A. Reactive function for the variable
density_group_trend_Xlim2 <- reactive({
  input$dnsGrpTrnX2
})
#18. Indicate lower limit of y-axis
output$dnsty_grp_trnd_Ylim1 <- renderUI({
  numericInput("dnsGrpTrnY1", "18. Lower Y-axis limit.",
               value = min(density_group_trend_Y_density()[["y"]], na.rm=TRUE), step = .1)
})
#18A. Reactive function for the variable
density_group_trend_Ylim1 <- reactive({
  input$dnsGrpTrnY1
})
#19. Indicate upper limit of Y-axis
output$dnsty_grp_trnd_Ylim2 <- renderUI({
  numericInput("dnsGrpTrnY2", "19. Upper Y-axis limit.",
               value = max(density_group_trend_Y_density()[["y"]], na.rm=TRUE), step = .1)
})
#19A. Reactive function for the variable
density_group_trend_Ylim2 <- reactive({
  input$dnsGrpTrnY2
})
#20. Run trend output function below  
density_group_trend_output <- reactive({
  if(density_group_trend_run_yes_no() == "Yes") {    
    fncTmDnsty(DF=df(), X= density_group_trend_group(), Y= density_group_trend_outcome(), 
               Z= density_group_trend_time(), Increment= density_group_trend_Time_Increment(), 
               Seed.Multiplier= density_group_trend_set_seed() )
  }  
})
#8. Select group label colors
output$dns_plot_lbl_clrs <- renderUI({                                 
  if(density_group_begin_yes_no() == "Yes") {
  selectInput("dnsPltLblClr", "8. Select group label colors.", 
              choices = xyplot_Line_Color_Names(), multiple=TRUE, 
              selected= xyplot_Line_Color_Names()[1:length(density_plot_groups())] )     
  } else {
    selectInput("dnsPltLblClr", "8. Select group label colors.", 
                choices = NA, multiple=TRUE, selected= NA )     
  }  
  })
#9. Select the line label size
output$dns_plot_txt_lbl_sz <- renderUI({                                 
  numericInput("dnsPlTxtLblSz", "9. Select the group label size.", 
               value = 2, min=0, step = .1)     
})
#9A. Reactive function for directly above
dns_plot_text_label_size <- reactive({                 
  input$dnsPlTxtLblSz 
})
#Reactive function for directly above
density_plot_Label_Colors <- reactive({                 
  input$dnsPltLblClr 
})
#Reactive function to get group levels
density_plot_groups <- reactive({                 
  unique(df()[, density_group_trend_group()]) 
})

#21. Plot function below  
density_group_trend_plot <- reactive({
  if(density_group_trend_run_yes_no() == "Yes") {    
    fncTmDnstPlot(TDList= density_group_trend_output(), X= density_group_trend_group(), 
                  Y= density_group_trend_outcome(), Z= density_group_trend_time(), 
                  Period= density_group_trend_play(), Lcol= density_group_trend_line_color(), 
                  Target= density_group_trend_Target(), Groups= density_group_trend_grp_X_levs(), 
                  Legend.Loc= density_group_trend_legend_location(), 
                  Xmin=density_group_trend_Xlim1(), Xmax=density_group_trend_Xlim2(), 
                  Ymin=density_group_trend_Ylim1(), Ymax= density_group_trend_Ylim2(),
                  GCol=density_plot_Label_Colors(), Text.Size=dns_plot_text_label_size(),
                  Period.Range=density_group_trend_range_time()) 
  }  
})
#21A. Trend plot  
output$dnsty_grp_trnd_plot <- renderPlot({
  if(density_group_trend_run_yes_no() == "Yes") {
    density_group_trend_plot()
  }
})
#22. Each period's output  
density_group_trend_by_time <- reactive({
  if(density_group_trend_run_yes_no() == "Yes") {    
    fncTmDnstOut(TDList= density_group_trend_output(), 
                 Period=density_group_trend_play(), Target= density_group_trend_Target(), Period.Range=density_group_trend_range_time())  
  }  
})
#22A. Each period's output  
output$dnsty_grp_trnd_out_by_tm <- renderPrint({
  if(density_group_trend_run_yes_no() == "Yes") {    
    density_group_trend_by_time()
  }
})

################################################################################
## Function to create aggregated values and density plot over time increments ##
################################################################################
fncTmDnsty <- function(DF, X, Y, Z, Increment, Seed.Multiplier) {
  #Get summary of values
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
  for (i in 1:length(Time.Periods )) {
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
      Dx.Cord[[j]][[i]] <- DxDY[[j]][[ 1]][which( min(abs(DxDY[[j]][[1]] - DYvals[[j]][i])) == abs(DxDY[[j]][[1]] - DYvals[[j]][i] ))]
      Dy.Cord[[j]][[i]] <- DxDY[[j]][[ 2]][which( min(abs(DxDY[[j]][[1]] - DYvals[[j]][i])) == abs(DxDY[[j]][[1]] - DYvals[[j]][i] ))]
      set.seed(i * Seed.Multiplier)
      Dy.Cord.Random[[j]][[i]] <- runif(1, min = 0.001, max = Dy.Cord[[j]][[i]])
      
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
                          Legend.Loc,Xmin, Xmax, Ymin, Ymax, GCol, Text.Size, Period.Range) {
  #Full range of periods to use in case I get a year variable
  Full.Range <- seq(Period.Range[1], Period.Range[2], by=1 )
  #Colors
  my_clr <- GCol
  Increment.Length <- TDList[["Increment.Length"]]
  #Title
  if(TDList[["Increment"]] == 1) {
    Main.Title <- paste(Y," rate by ", Z, ": ", 
                        TDList[["Time.Start.Label"]][[which(Full.Range == Period)]],  sep= "")
  } else {
    Main.Title <- paste(Y," rate by ", Z, ": ", 
                        TDList[["Time.Start.Label"]][[which(Full.Range == Period)]], " - ",                      
                        TDList[["Time.Stop.Label"]][[which(Full.Range == Period)]],  sep= "")
  }
  #Density plot
  plot(TDList[["D2"]][[ which(Full.Range == Period)]], xlim=c(Xmin,Xmax), ylim= c(Ymin, Ymax),  
       col=Lcol, lwd=8  ,
       main=Main.Title, xlab= "Rate" 
  )
  #Add vertical lines
  abline(v=Target, col="blue", lwd=2)           
  abline(v= mean(TDList[["AggrY"]][[1]][[2]]), col=colors()[102], lwd=2)
  abline(v= mean(TDList[["AggrY"]][[Increment.Length]][[2]]), col=colors()[102], lwd=2, lty=2)
  ###Rug and text to identify the med centers.###
  rug(TDList[["DYvals"]][[which(Full.Range == Period)]], side=1, col=Lcol)
  #Text for group values
  if ( is.null(Groups) ) {  
    text(TDList[["DYvals"]][[which(Full.Range == Period)]], unlist(TDList[["Dy.Cord.Random"]][which(Full.Range == Period)]),   
         labels= unlist(TDList[["DXname"]][[which(Full.Range == Period)]][X]),
         cex=Text.Size, col=my_clr )
  } else {
    non_groups <- setdiff(unlist(TDList[["DXname"]][[which(Full.Range == Period)]][[X]]) , Groups)   #Get excluded groups
    Groups.Temp <- as.character(unlist(TDList[["DXname"]][[which(Full.Range == Period)]][[X]]))      #Get all groups
    Groups.Temp[which(Groups.Temp %in% non_groups )] <- ""                      #Change non-groups to blanks
    text(TDList[["DYvals"]][[which(Full.Range == Period)]], unlist(TDList[["Dy.Cord.Random"]][which(Full.Range == Period)]),   
         labels= Groups.Temp, cex=Text.Size, col= my_clr)
  }
  ###Legend###
  #This creates a legend for the ablines with and without targets. TDList[["AggrY"]][[which(Full.Range == Period)]] YTmean
  if ( !is.numeric(Target) ) {  
    legend(x=Legend.Loc, legend=c(paste0("Starting pooled mean: ", round(mean(TDList[["AggrY"]][[1]][[2]], na.rm=TRUE), 3)),
                                  paste0("Ending pooled mean: ", round(mean(TDList[["AggrY"]][[Increment.Length]][[2]], na.rm=TRUE), 3) )),
           col=c(colors()[102], colors()[102], "blue"),
           lty= c(1,2,1), lwd= 1.5, cex = 1.5, bty="n", inset=c(0, .05))
  } else {  
    #legend(Legend.Loc, legend=c("Starting mean","Ending mean", "Target"),
    legend(Legend.Loc, legend=c(paste0("Starting pooled mean: ", round(mean(TDList[["AggrY"]][[1]][[2]], na.rm=TRUE), 3)),
                                paste0("Ending pooled mean: ", round(mean(TDList[["AggrY"]][[Increment.Length]][[2]], na.rm=TRUE), 3) ) , 
                                paste0("Target: ", Target)),
           col=c(colors()[102], colors()[102], "blue"),
           lty= c(1,2,1), lwd= 1.5, cex = 1.5, bty="n", inset=c(0, .05))
  }
}

################################################################################
##           Function to get aggregated values over time increments           ##
################################################################################
fncTmDnstOut <- function(TDList, Period, Target, Period.Range) {
  Full.Range <- seq(Period.Range[1], Period.Range[2], by=1 )
  out.by.period <- TDList[["AggrY"]][[ which(Full.Range == Period)]]
  
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
  return(list("Period Rate"= out.by.period, "High Target"= HT.table, "Low Target"= LT.table, 
              "Pooled.Mean"= mean(as.numeric(unlist(out.by.period[[2]])) ,na.rm=TRUE),
              "Pooled.Median"= median(as.numeric(unlist(out.by.period[[2]])) ,na.rm=TRUE)))
}

################################################################################
##                       Cost analysis with the Cox PH model                  ##
################################################################################

########################################################
#   Plot 1: Density plots on cost by intervention type #
########################################################
#Homemade functions
#Function to get density function for 1 group
cost_plot1_density <- function(y, df) {
  #Density function
  d_ctl <- density(df[, y], na.rm=T, adjust=2)
  d_ctl_x <- range(d_ctl$x)
  d_ctl_y <- range(d_ctl$y)
  return(list("Density.FUN"= d_ctl,
              "XLim"= d_ctl_x, 
              "YLim"= d_ctl_y))
}
#Reactive function for the function above
cost_plt_dens_fun_1_group <- reactive({
  if(dens_run_plot_yesno() == "Yes") {
    if(dens_strat_or_unstrat() != "Stratified") {
      cost_plot1_density(y= outcome(), df= df())
    }
  }  
})
  
#Function to get density function for 2 groups
cost_plot1_group_density <- function(y, x, x0, x1, df) {
  #Density function
#  d_ctl0 <- density(df[df[, x] == x0, y], na.rm=T, adjust=2)
#  d_ctl1 <- density(df[df[, x] == x1, y], na.rm=T, adjust=2)
  d_ctl0 <- density(df[as.numeric(df[, x]) == min(as.numeric(df[, x]), na.rm=T), y], na.rm=T, adjust=2)
  d_ctl1 <- density(df[as.numeric(df[, x]) == max(as.numeric(df[, x]), na.rm=T), y], na.rm=T, adjust=2)
  d_ctl_x <- range( c(d_ctl0$x, d_ctl1$x) )
  d_ctl_y <- range( c(d_ctl0$y, d_ctl1$y) )
  return(list("Density.FUN0"= d_ctl0,
              "Density.FUN1"= d_ctl1, 
              "XLim"= d_ctl_x, 
              "YLim"= d_ctl_y))
}

#Reactive function for the function above
cost_plt_dens_fun_2_group <- reactive({
  if(dens_run_plot_yesno() == "Yes") {
    if(dens_strat_or_unstrat() == "Stratified") {
      cost_plot1_group_density(y= outcome(), x= dens_stratified() , df= df(), 
                               x0=dens_strata_actual_values()[1], 
                               x1=dens_strata_actual_values()[2])
    }
  }  
})

#Reactive function that gets X-Y limits for the density plot
cost_plt_dens_fun_limits <- reactive({
  if(dens_run_plot_yesno() == "Yes") {
    
    if(dens_strat_or_unstrat() == "Stratified") {
      cost_plt_dens_fun_2_group()
    } else {
      cost_plt_dens_fun_1_group()
    }
  }  
})

#This plots the outcome for all patients
cost_plot1 <- function(y, x, df, d_ctl, dnsXLim1, dnsXLim2, dnsYLim1, dnsYLim2, 
                       LCol, Text.Size, labMulti=1, Lgd.Loc) {
  #####Color codes#####
  #Set up colors
  all_clrs <- c("Black", "Red", "Green","Blue","Turquoise","Purple","Yellow","Gray")
  my_clr <- LCol
  color_trnspt  <- adjustcolor(which(all_clrs %in% my_clr[1]), alpha.f = 0.4) 
  cst_tbl <- c(mean(df[, y], na.rm=T),
               median(df[, y], na.rm=T))
  names(cst_tbl) <- c("Mean", "Median" )
  #Set up plotting space
  par(mar = c(6, 6, 3, 1) + 0.1)
  plot(d_ctl, axes=F, type="n", ylab="", xlab="", main="",
       xlim=c(dnsXLim1, dnsXLim2), ylim=c(dnsYLim1, dnsYLim2))
  lines(d_ctl)
  title(paste0("Density plot of ", y), cex.main = 1.1*labMulti) 
  polygon(d_ctl, col=color_trnspt, border=color_trnspt) 
  
  abline(v=median(df[, y], na.rm=T),
         col=my_clr[1], lwd=Text.Size)
  abline(v=mean(df[, y], na.rm=T),
         col=my_clr[1], lty=3, lwd=Text.Size)
  axis(1, las=1, cex.axis=1*labMulti )
  axis(2, las=3, cex.axis=1*labMulti )
  mtext(y, side = 1, line = 4, cex=1.1*labMulti )
  mtext("Density", side = 2, line = 4, cex=1.1*labMulti )
  #Legend
  legend(Lgd.Loc, legend=c(paste0("Mean= ",round(cst_tbl["Mean"], 0)), 
                         paste0("Median= ", round(cst_tbl["Median"], 0))), 
         col=c(my_clr[1], my_clr[1]), lty=c(3,1), bty="n",
         lwd=(1+Text.Size), cex= Text.Size)
  
  box()
}

cost_plot1_grp <- function(y, x, df, Trt.NM, Ctl.NM, 
                           DENS, dnsXLim1, dnsXLim2, dnsYLim1, dnsYLim2, 
                           LCol, Text.Size, labMulti=1, Lgd.Loc) {
  #####Color codes#####
  #Set up colors
  all_clrs <- c("Black", "Red", "Green","Blue","Turquoise","Purple","Yellow","Gray")
  my_clr <- LCol
  color_trnspt0  <- adjustcolor(which(all_clrs %in% my_clr[1]), alpha.f = 0.4) 
  color_trnspt1  <- adjustcolor(which(all_clrs %in% my_clr[2]), alpha.f = 0.4) 
  
  #Density functions  
  d_ctl <- DENS$Density.FUN0
  d_trt <- DENS$Density.FUN1
  #Treatment/control group means/medians
  ctl_med <- median(df[, y][as.numeric(df[, x]) == min(as.numeric(df[, x]))], na.rm=T)
#  trt_med <- median(df[, y][as.numeric(df[, x]) == max(as.numeric(df[, x]))], na.rm=T)
  trt_med <- median(df[, y][as.numeric(df[, x]) != min(as.numeric(df[, x]))], na.rm=T)
  ctl_mn <- mean(df[, y][as.numeric(df[, x]) == min(as.numeric(df[, x]))], na.rm=T)
#  trt_mn <- mean(df[, y][as.numeric(df[, x]) == max(as.numeric(df[, x]))], na.rm=T)
  trt_mn <- mean(df[, y][as.numeric(df[, x]) != min(as.numeric(df[, x]))], na.rm=T)
  #The graph
  par(mar = c(6, 6, 3, 1) + 0.1)
  plot(d_ctl, axes=F, type="n", ylab="", xlab="", main="",
       xlim=c(dnsXLim1, dnsXLim2), ylim=c(dnsYLim1, dnsYLim2))
  lines(d_ctl)
  lines(d_trt)
  title(paste0("Density plot of ", y, " by ", x), cex.main = 1.1*labMulti) 
  
  polygon(d_ctl, col=color_trnspt0, border=color_trnspt0) 
  polygon(d_trt, col=color_trnspt1, border=color_trnspt1) 
  
  ###  
  abline(v=ctl_med, col= my_clr[1], lwd=Text.Size)
  abline(v=trt_med, col= my_clr[2], lwd=Text.Size)
  abline(v=ctl_mn, col= my_clr[1], lty=3, lwd=Text.Size)
  abline(v=trt_mn, col= my_clr[2], lty=3, lwd=Text.Size)
  axis(1, las=1, cex.axis=1*labMulti )
  axis(2, las=3, cex.axis=1*labMulti )
  mtext(y, side = 1, line = 4, cex=1.1*labMulti )
  mtext("Density", side = 2, line = 4, cex=1.1*labMulti )
  
  legend(Lgd.Loc, legend=c(paste0(Trt.NM, ": Mean= ",round(trt_mn, 0)), 
                           paste0(Trt.NM, ": Median= ", round(trt_med, 0)), 
                           paste0(Ctl.NM, ": Mean= ", round(ctl_mn, 0)), 
                           paste0(Ctl.NM, ": Median= ", round(ctl_med, 0))), 
         col=c(my_clr[2], my_clr[2], my_clr[1], my_clr[1]), lty=c(3,1,3,1),
         lwd=(1+Text.Size), cex=Text.Size, bty="n")
  box()
}

##################
# UPDATE START HERE #
##################

#This plots the outcome by a binary grouping variable
quant_plt1_fnc <- function(ests, Y, Trt.NM, Ctl.NM,
                           qntXLim1, qntXLim2, qntYLim1, qntYLim2, 
                           LCol, Text.Size, labMulti=1, Lgd.Loc) {
  est1 <- ests[["est1"]]
  est2 <- ests[["est2"]]
  #Set up colors
  all_clrs <- c("Black", "Red", "Green","Blue","Turquoise","Purple","Yellow","Gray")
  my_clr <- LCol
  color_trnspt0  <- adjustcolor(which(all_clrs %in% my_clr[1]), alpha.f = 0.2) 
  color_trnspt1  <- adjustcolor(which(all_clrs %in% my_clr[2]), alpha.f = 0.2) 
  #  c_red <- 2; c_blue <- 4
  #  red_trnspt2  <- adjustcolor(c_red, alpha.f = 0.2)                               #Set red color transparency.
  #  blue_trnspt2 <- adjustcolor(c_blue, alpha.f = 0.2)                              #Set blue color transparency.
  #  plot(1:5, est1[, 1], type="n",
  #       ylab=Y, xlab="Percentiles",
  #       ylim=c(0, max(est1, na.rm=T)*.9), xlim=c(1, 5),
  #       axes=F, main= paste0("Estimated ", Y, " over quantiles"))
  #Set up plotting space
  par(mar = c(6, 6, 3, 1) + 0.1)
  plot(1:5, est1[, 1], axes=F, type="n", ylab="", xlab="", main="",
       xlim=c(qntXLim1, qntXLim2), ylim=c(qntYLim1, qntYLim2))
  title(paste0("Estimated ", Y, " over quantiles"), cex.main = 1.1*labMulti) 
  lines(1:5, est1[, 1], col= my_clr[1], lwd=3)
  lines(1:5, est1[, 2], col= my_clr[2], lwd=3)
  #Set up coordinates for polygon function, NOTE: UPPER AND LOWER MEAN THE OPPOSITE 
  #BECAUSE A COX MODEL VIEWS COST IN THE OPPOSITE DIRECTION.
  #  axis(1, at=1:5, tick=F,  labels= c("p10", "p25", "p50", "p75", "p90")) #pos=1,
  #  axis(2)
  axis(1, at=1:5, tick=F,  labels= c("p10", "p25", "p50", "p75", "p90"),
       las=1, cex.axis=1*labMulti )
  axis(2, las=3, cex.axis=1*labMulti )
  mtext("Percentiles", side = 1, line = 4, cex=1.1*labMulti )
  mtext(Y, side = 2, line = 4, cex=1.1*labMulti )
  
  xxt <- c(1:5,5:1)
  yya <- c(est1$lower2, rev(est1$upper2))  #Treatment
  yyc <- c(est1$lower1, rev(est1$upper1))  #Control
  ##plot(xx,yyd, type="n", ylim=c(0,1))
  polygon(xxt, yya, col = color_trnspt1, border= color_trnspt1)
  polygon(xxt, yyc, col = color_trnspt0, border=color_trnspt0)
  legend(Lgd.Loc, legend=c(Trt.NM, Ctl.NM), 
         col=c(my_clr[2],my_clr[1]), lty=1,
         lwd=(1+Text.Size), cex=Text.Size, bty="n")
  box()
}

#Function that gets cost value quantiles between the treatment and control groups 
cost_quant <- function(y, x, df) {
  
  #Treatment/control group means/medians
  ctl_med <- median(df[, y][as.numeric(df[, x]) == min(as.numeric(df[, x]), na.rm=T)], na.rm=T)
#  trt_med <- median(df[, y][as.numeric(df[, x]) == max(as.numeric(df[, x]), na.rm=T)], na.rm=T)
  trt_med <- median(df[, y][as.numeric(df[, x]) != min(as.numeric(df[, x]), na.rm=T)], na.rm=T)
  ctl_mn <- mean(df[, y][as.numeric(df[, x]) == min(as.numeric(df[, x]), na.rm=T)], na.rm=T)
#  trt_mn <- mean(df[, y][as.numeric(df[, x]) == max(as.numeric(df[, x]), na.rm=T)], na.rm=T)
  trt_mn <- mean(df[, y][as.numeric(df[, x]) != min(as.numeric(df[, x]), na.rm=T)], na.rm=T)
  #### Get quantiles and bind with means. Partially used in the legend and the observed table in a later section
  qt1 <- quantile(df[, y][as.numeric(df[, x]) == min(as.numeric(df[, x]), na.rm=T)], probs=c(.1, .25, .5, .75, .9), na.rm=T)
#  qt2 <- quantile(df[, y][as.numeric(df[, x]) == max(as.numeric(df[, x]), na.rm=T)], probs=c(.1, .25, .5, .75, .9), na.rm=T)
  qt2 <- quantile(df[, y][as.numeric(df[, x]) != min(as.numeric(df[, x]), na.rm=T)], probs=c(.1, .25, .5, .75, .9), na.rm=T)
  odf_mqt <- as.data.frame(rbind(cbind(round(qt2, 2), round(qt1 ,2)),
                                 cbind(round(trt_mn, 2), round(ctl_mn, 2)) ))
  rownames(odf_mqt) <- c("p10", "p25", "p50", "p75", "p90", "Mean")
  colnames(odf_mqt) <- c(dens_strata_final_stata_names()[2], dens_strata_final_stata_names()[1])  #Observed data.frame of the means and quantiles
  ###  
  return(list("odf_mqt"=odf_mqt))
}

#1A. Select the type of density plot to run (full sample or by a group)
output$dens_plt1_typ <- renderUI({
  selectInput("DensPlt1Typ", "1. Choose the type of analysis.", 
              choices = c("Unstratified", "Stratified"), multiple=FALSE, selected="Unstratified")     
})
#1B. Reactive function for stratification above 
dens_strat_or_unstrat <- reactive({
  input$DensPlt1Typ
})

#2. Select the grouping variable for the density plot with 2 plots--value of 1=treatment group
output$dens_plt1_x <- renderUI({
  selectInput("DensPlt1X", "2. Select a binary stratified variable (optional).", 
              choices = predictor())     
})
#2A. Create a reactive function for the stratified variable in the Cost density plot
dens_stratified <- reactive({                  #Outcome is my reactive function name I will use below. 
  input$DensPlt1X                      #DensPlt1X comes from the UI file drop down box.
})
#3. Select the type of names I will use
output$dens_plt1_x_nms <- renderUI({
  selectInput("DensPlt1XNms", "3. Select strata names.", 
              choices = c("Actual values", "Custom names"), multiple=FALSE, selected="Values")     
})
#3A. Create a reactive function to select names based on actual values or custom
dens_strata_name_type <- reactive({                  #
  input$DensPlt1XNms
})
#3B. Get unique value names from the model specs
dens_strata_actual_values <- reactive({                  #
  if ( dens_strata_name_type() == "Actual values" ) {
    as.character(sort(unique(model_specs_long()[["limits"]][[ dens_stratified() ]])))
  } 
})
#4. Enter custom names
output$dens_plt1_x_cstm <- renderUI({                                 
  textInput("DensPlt1XCust", "4. Enter custom names.",
            value = paste0('c( ', "'",'Control',"'", ', ', "'",'Treatment',"'", ')'))
})
#4A. Reactive function for above
dens_strata_custom_names <- reactive({ 
  input$DensPlt1XCust 
})
#4B. Determine which stratification names to use
dens_strata_final_stata_names <- reactive({
  if ( dens_strata_name_type() == "Actual values" ) {
    dens_strata_actual_values()
  } else {
    eval(parse(text=dens_strata_custom_names() )) 
  }
})

#5. Create yes/no box to plot the density plot
output$run_dens_plt1 <- renderUI({ 
  selectInput("RunDensPlt1", "5. Do you want to run the plots?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     
})
#5A. Reactive function for  running plots
dens_run_plot_yesno <- reactive({
  input$RunDensPlt1
  })

#######################
## Cost Density Plot ##
#######################
#1. Select line colors
output$densPltLnClr <- renderUI({                                 
  selectInput("dn_ln_clr", "1. Select line colors.", 
              choices = c("Black", "Red", "Green","Blue","Turquoise","Purple","Yellow","Gray"), 
              multiple=TRUE, 
              selected= c("Black", "Red", "Green","Blue","Turquoise","Purple","Yellow","Gray")[c(2,4)] )    
})
#1A. Reactive function for directly above
Density_Line_Colors <- reactive({                 
  input$dn_ln_clr 
})
#2. Select the line label size
output$dens_plot_txt_lbl_sz <- renderUI({                                 
  numericInput("dnsPlTxtLblSz", "2. Select the legend size.", 
               value = 2, min=0, step = .1)     
})
#2A. Reactive function for directly above
dens_plot_text_label_size <- reactive({                 
  input$dnsPlTxtLblSz 
})
#3. Select label size multiplier
output$dns_plot_lab_multi <- renderUI({                                 
  numericInput("dnsPltLabMlt", "3. Increase XY label sizes.",
               value = 1, min=.01, step = .1)
})
#3A. Reactive function for directly above
dns_plot_label_multiplier <- reactive({                 
  input$dnsPltLabMlt 
})
#4. Legend location
output$dens_lgd_loc <- renderUI({                                
  selectInput("densLgdLoc", "4. Select the legend location.",        
              choices = c("bottomright","bottom","bottomleft","left","topleft","top","topright","right","center"), 
              multiple=FALSE, selected="topleft" ) 
})
#4A. Reactive function for legend location
density_legend_location <- reactive({
  input$densLgdLoc
})

#5. Indicate lower limit of x-axis
output$dns_Xlim1 <- renderUI({
  numericInput("dnsXLim1", "5. Lower X-axis limit.",
               value = cost_plt_dens_fun_limits()[["XLim"]][1], step = 1)
})
#6. Indicate upper limit of x-axis
output$dns_Xlim2 <- renderUI({
  numericInput("dnsXLim2", "6. Upper X-axis limit.",
               value = cost_plt_dens_fun_limits()[["XLim"]][2],
               step = 1)
})
#7. Indicate lower limit of y-axis
output$dns_Ylim1 <- renderUI({
  numericInput("dnsYLim1", "7. Lower Y-axis limit.",
  #value = cost_plt_dens_fun_limits()[["YLim"]][1], step = .1)
  value = 0, step = .1)
})
#8. Indicate upper limit of x-axis
output$dns_Ylim2 <- renderUI({
  numericInput("dnsYLim2", "8. Upper Y-axis limit.",
               value = cost_plt_dens_fun_limits()[["YLim"]][2], step = .1)
})

#6. This runs the cost_plot1() function above
dens_plt1 <- reactive({
  if(dens_run_plot_yesno() == "Yes") {
    
    if(dens_strat_or_unstrat() == "Stratified") {
      cost_plot1_grp(y= outcome(), x= dens_stratified() , df= df(), 
                     Trt.NM=dens_strata_final_stata_names()[2], 
                     Ctl.NM=dens_strata_final_stata_names()[1],
                     DENS=cost_plt_dens_fun_2_group(), 
                     dnsXLim1= input$dnsXLim1, 
                     dnsXLim2= input$dnsXLim2, 
                     dnsYLim1= input$dnsYLim1, 
                     dnsYLim2= input$dnsYLim2, 
                     LCol=Density_Line_Colors(), Text.Size=dens_plot_text_label_size(), 
                     labMulti=dns_plot_label_multiplier(), Lgd.Loc=density_legend_location())
    } else {
      cost_plot1(y= outcome(), df= df(), d_ctl= cost_plt_dens_fun_1_group()$Density.FUN, 
                 dnsXLim1= input$dnsXLim1, 
                 dnsXLim2= input$dnsXLim2, 
                 dnsYLim1= input$dnsYLim1, 
                 dnsYLim2= input$dnsYLim2, 
                 LCol=Density_Line_Colors(), Text.Size=dens_plot_text_label_size(), 
                 labMulti=dns_plot_label_multiplier(), Lgd.Loc=density_legend_location())
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

##########################
## Cost Percentile Plot ##
##########################
#1. Select line colors
output$qntPltLnClr <- renderUI({                                 
  selectInput("qnt_ln_clr", "1. Select line colors.", 
              choices = c("Black", "Red", "Green","Blue","Turquoise","Purple","Yellow","Gray"), 
              multiple=TRUE, 
              selected= c("Black", "Red", "Green","Blue","Turquoise","Purple","Yellow","Gray")[c(2,4)] )    
})
#1A. Reactive function for directly above
quantile_Line_Colors <- reactive({                 
  input$qnt_ln_clr 
})
#2. Select the line label size
output$qnt_plot_txt_lbl_sz <- renderUI({                                 
  numericInput("qntPlTxtLblSz", "2. Select the legend size.", 
               value = 2, min=0, step = .1)     
})
#2A. Reactive function for directly above
qnt_plot_text_label_size <- reactive({                 
  input$qntPlTxtLblSz 
})
#3. Select label size multiplier
output$qnt_plot_lab_multi <- renderUI({                                 
  numericInput("qntPltLabMlt", "3. Increase XY label sizes.",
               value = 1, min=.01, step = .1)
})
#3A. Reactive function for directly above
qnt_plot_label_multiplier <- reactive({                 
  input$qntPltLabMlt 
})
#4. Legend location
output$qnt_lgd_loc <- renderUI({                                
  selectInput("qntLgdLoc", "4. Select the legend location.",        
              choices = c("bottomright","bottom","bottomleft","left","topleft","top","topright","right","center"), 
              multiple=FALSE, selected="topleft" ) 
})
#4A. Reactive function for legend location
quantile_legend_location <- reactive({
  input$qntLgdLoc
})

#5. Indicate lower limit of x-axis
output$qnt_Xlim1 <- renderUI({
  numericInput("qntXLim1", "5. Lower X-axis limit.",
               value = 1, step = 1)
})
#6. Indicate upper limit of x-axis
output$qnt_Xlim2 <- renderUI({
  numericInput("qntXLim2", "6. Upper X-axis limit.",
               value = 5,
               step = 1)
})
#7. Indicate lower limit of y-axis
output$qnt_Ylim1 <- renderUI({
  numericInput("qntYLim1", "7. Lower Y-axis limit.",
               value = as.numeric(describeY()[["counts"]][".05"]), step = .1)
})
#8. Indicate upper limit of x-axis
output$qnt_Ylim2 <- renderUI({
  numericInput("qntYLim2", "8. Upper Y-axis limit.",
               value = as.numeric(describeY()[["counts"]][".90"]), step = .1)
})

############################################
#   Plot 2: Estimated cost over quantiles  #
############################################
quant_ests_fnc <- function(fit, Y, X, reg, Trt.NM, Ctl.NM) {
  #Creates function that will make correct value 
  MED_cox <<- Quantile(fit)                                                       #Creates function to compute quantiles

  #5. Cost--get predicted values at different quantiles
  if (reg %in% c("Cox PH", "Cox PH with censoring")) {
  p1.50 <- Predict(fit, fun=function(x) MED_cox(q=.50, lp=x))                           #MED_coxian
  p1.10 <- Predict(fit, fun=function(x) MED_cox(q=.90, lp=x))                    #10th percentile
  p1.25 <- Predict(fit, fun=function(x) MED_cox(q=.75, lp=x))                    #25th percentile
  p1.75 <- Predict(fit, fun=function(x) MED_cox(q=.25, lp=x))                    #75th percentile
  p1.90 <- Predict(fit, fun=function(x) MED_cox(q=.10, lp=x))                    #90th percentile
  }
  if (reg == "Ordinal Logistic") {
    p1.50 <- Predict(fit, fun=function(x) MED_cox(q=.50, lp=x))                           #MED_coxian
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
  colnames(est2) <- c(Trt.NM, "L95", "U95", Ctl.NM, "L95", "U95")
  rownames(est2) <- c("p10", "p25", "p50","p75", "p90")
  #Add cost difference between Treatment and controls 
  est2$Diff. <- abs(round(est2[, Ctl.NM ]) - round(est2[, Trt.NM]))
  ## Add in weighted average into the table
  #Make row of data
  wtot <- as.data.frame(matrix(nrow=1, ncol=7))
  colnames(wtot) <- c(Trt.NM, "L95", "U95", Ctl.NM, "L95", "U95", "Diff.")
  rownames(wtot) <- "Weighted"
  #Calculate weighted average
  wtot[, "Diff."] <- ((.25 * est2[rownames(est2) =="p10", "Diff."]) + (.25 * est2[rownames(est2) =="p25", "Diff."]) + 
           (.25 * est2[rownames(est2) =="p50", "Diff."]) + (.15 * est2[rownames(est2) =="p75", "Diff."]) + 
           (.1 * est2[rownames(est2) =="p90", "Diff."]))
  #Combine everything
  est2 <- rbind(est2, wtot)
  return(list(est1=est1, est2=est2))
}  

#This runs the function above
quant_ests <- reactive({
  if(input$RunDensPlt1 == "Yes") {
    
    if(input$DensPlt1Typ == "Stratified") {
    #  quant_ests_fnc(fit=fit1(), Y= outcome(), X= input$DensPlt1X, reg=input$regress_type)
      quant_ests_fnc(fit=fit1(), Y= outcome(), X= dens_stratified(), reg=input$regress_type,
                     Trt.NM=dens_strata_final_stata_names()[2], 
                     Ctl.NM=dens_strata_final_stata_names()[1])
    } 
  }  
})


#This runs the quant_plt1_fnc() function above
quant_plt1 <- reactive({
  if(input$RunDensPlt1 == "Yes") {
    
    if(input$DensPlt1Typ == "Stratified") {
      quant_plt1_fnc(ests=quant_ests(), Y= outcome(), 
                     Trt.NM=dens_strata_final_stata_names()[2], 
                     Ctl.NM=dens_strata_final_stata_names()[1],
                     qntXLim1=input$qntXLim1, qntXLim2=input$qntXLim2, 
                     qntYLim1=input$qntYLim1, qntYLim2=input$qntYLim2, 
                     LCol=quantile_Line_Colors(), Text.Size=qnt_plot_text_label_size(), 
                     labMulti=qnt_plot_label_multiplier(), Lgd.Loc=quantile_legend_location())
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
mean_ests_fnc <- function(fit, Y, X, reg, Trt.NM, Ctl.NM) {
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
  colnames(est2) <- c(Trt.NM, "L95", "U95", Ctl.NM, "L95", "U95")
  rownames(est2) <- "Mean"
  #Add cost difference between Treatment and controls 
  est2$Diff. <- abs(round(est2[, Ctl.NM]) - round(est2[, Trt.NM]))
  return(list(est1=est1, est2=est2))
}  

#This runs the function above
mean_ests <- reactive({
  if(input$RunDensPlt1 == "Yes") {
    
    if(input$DensPlt1Typ == "Stratified") {
      #mean_ests_fnc(fit=fit1(), Y= outcome(), X= input$DensPlt1X, reg=input$regress_type)
      mean_ests_fnc(fit=fit1(), Y= outcome(), X= dens_stratified(), reg=input$regress_type, 
                    Trt.NM=dens_strata_final_stata_names()[2], 
                    Ctl.NM=dens_strata_final_stata_names()[1])
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
  selectInput("CXyplotYesNo", "6. Do you want to create the plot?", 
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
  selectInput("CxyplotGrpLvs", "5. Highlight specific groups?", 
              choices = Cxyplot_groups(), multiple=TRUE)     
})
#Reactive function to get group levels
Cxyplot_Group_Levels <- reactive({                 
  input$CxyplotGrpLvs 
})
#7. Extrapolate continuous X
output$CxyExtrapo_yes_no <- renderUI({                                 
  selectInput("CxyExtrpYesNo", "7. Do you want to extrapolate on X?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     
})
#7A. Reactive function for the variable
Cxy_extrapolate <- reactive({
  input$CxyExtrpYesNo
})
#8. Textbox to enter a formula
output$Cxy_extrap_box <- renderUI({  
  textInput("CxyExtrpBox", "8. Select the extrapolated range.", 
            value= paste0("do.call('Predict', list(fit1(),", '\'', CXyplotX1(),'\'', "= ", ", ", '\'', CXyplotZ1(),'\'', "))") )
})
#8A. Reactive function for textbox to enter a formula
Cxy_extrap_box_Input <- reactive({
  input$CxyExtrpBox
}) 

#9. Add a vertical line
output$CxyExtr_X_Val <- renderUI({
  textInput("CxyExtrX", "9. Enter X-axis value for vertical line.",
            value = paste0('c( ', ')') )
})
#9A. Reactive function for the variable
CxyExtr_x_value <- reactive({
  as.numeric(eval(parse(text=input$CxyExtrX )))
})

#10. Indicate lower limit of x-axis
output$Cxyplot_Xlim1 <- renderUI({
  numericInput("CxyplotLmX1", "10. Lower X-axis limit.",
               value = Cxylm()$XMin, step = .1)
})
#10A. Reactive function for the variable
Cxyplot_X_limit_1 <- reactive({
  input$CxyplotLmX1
})
#11. Indicate upper limit of x-axis
output$Cxyplot_Xlim2 <- renderUI({
  numericInput("CxyplotLmX2", "11. Upper X-axis limit.",
               value = Cxylm()$XMax, step = .1)
})
#11A. Reactive function for the variable
Cxyplot_X_limit_2 <- reactive({
  input$CxyplotLmX2
})
#12. Indicate lower limit of y-axis
output$Cxyplot_Ylim1 <- renderUI({
  numericInput("CxyplotLmY1", "12. Lower Y-axis limit.",
               value = Cxylm()$YMin, step = .1)
})
#12A. Reactive function for the variable
Cxyplot_Y_limit_1 <- reactive({
  input$CxyplotLmY1
})
#13. Indicate upper limit of Y-axis
output$Cxyplot_Ylim2 <- renderUI({
  numericInput("CxyplotLmY2", "13. Upper Y-axis limit.",
               value = Cxylm()$YMax, step = .1)
})
#13A. Reactive function for the variable
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
                GroupLevs= Cxyplot_Group_Levels(), XYlims= Cxylm_all(), Clrs= Cxyplot_Line_Colors(), 
                CIbands=Cxyplot_Bands_YesNo(), ABline= CxyExtr_x_value()) 
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
  if(reg %in% c("Logistic", "Proportion Y Logistic","Ordinal Logistic") ) {
    xyplot_ylab <- paste0("Odds Ratio (",lev1, ":", lev2,")")
  }
  if(reg %in% c("Linear","Poisson","Quantile","Generalized Least
                Squares")) {
    xyplot_ylab <- paste0("Contrast (",lev1, ":", lev2,")")
}
  ## Determine if it is an abline at 0 or 1 ##
  if(reg %in% c("Cox PH", "Cox PH with censoring","Logistic", "Proportion Y Logistic", "Ordinal Logistic") ) {
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
pred_qnt_reg5_fnc <- function(qr.10, qr.25, qr.50, qr.75, qr.90, Dens1X, Trt.NM, Ctl.NM) {
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
  colnames(est2) <- c(Trt.NM,  "L95", "U95", Ctl.NM, "L95", "U95")
  rownames(est2) <- c("p10", "p25", "p50","p75", "p90")
  #Add cost difference between Treatment and controls 
  est2$Diff. <- abs(round(est2[, Ctl.NM]) - round(est2[, Trt.NM]))
  return(list(est1=est1, est2=est2))
}
#pred_qnt_reg5_fnc(qr.10=qr1, qr.25=qr1, qr.50=qr1, qr.75=qr1, qr.90=qr1, Dens1X="intervention")

#This runs the pred_qnt_reg5_fnc() function above
qr_ests <- reactive({
  if(input$RunDensPlt1 == "Yes") {
    
    if(input$DensPlt1Typ == "Stratified") {
      pred_qnt_reg5_fnc(qr.10=quant_reg.10(), qr.25=quant_reg.25(), qr.50=quant_reg.50(), 
                        #qr.75=quant_reg.75(), qr.90=quant_reg.90(), Dens1X=input$DensPlt1X)
      qr.75=quant_reg.75(), qr.90=quant_reg.90(), Dens1X=dens_stratified(), 
      Trt.NM=dens_strata_final_stata_names()[2], 
      Ctl.NM=dens_strata_final_stata_names()[1])
    } 
  }  
})
      
      ##############
      #Graph on differences between intervention groups on expected costs over percentiles 
      qr_plt1_fnc <- function(ests, Y, Trt.NM, Ctl.NM) {
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
        legend("topleft", legend=c(Trt.NM, Ctl.NM), 
               col=c(4,2), lty=1,
               lwd=2, cex=1.25)
        box()
      }
      
      #This runs the qr_plt1_fnc() function above
      qr_plt1 <- reactive({
        if(input$RunDensPlt1 == "Yes") {
          
          if(input$DensPlt1Typ == "Stratified") {
            qr_plt1_fnc(ests=qr_ests(), Y= outcome(), Trt.NM=dens_strata_final_stata_names()[2], 
                        Ctl.NM=dens_strata_final_stata_names()[1])
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
#9. Select line colors
output$SurvPltLnClr <- renderUI({                                 
  selectInput("sp_ln_clr", "9. Select line colors.", 
              choices = xyplot_Line_Color_Names(), multiple=TRUE, 
              selected= xyplot_Line_Color_Names()[1:length( unique(df()[, input$SrvPltX]))] )    
})
#9A. Reactive function for directly above
Surv_Line_Colors <- reactive({                 
  input$sp_ln_clr 
})
#10. Select line width
output$SurvPltLnWdt <- renderUI({                                 
  numericInput("sp_ln_wd", "10. Select the group line width.", 
               value = 2, min=0, step = .1)     
})
#10A. Reactive function for directly above
Surv_Line_Width <- reactive({                 
  input$sp_ln_wd 
})
#Create yes/no box to run survival plot
output$SpTimeInc <- renderUI({                                 
  numericInput("sp_timeinc", "11. Indicate the X-axis time increment.", 
               value = round((input$sp_Xlim2/5), 0), step = 1, min=0)     
})
#Indicate if you want the hazard function
output$HazardPlot <- renderUI({                                 
  selectInput("hazard_plot", "12. Do you want the hazard or survival?", 
              choices = c("Cumulative Hazard", "Cumulative Incidence", "Survival"), multiple=FALSE, selected="Survival")     
})
#Indicate if you want the log-minus-log plot
output$SrvLogLog <- renderUI({                                 
  selectInput("srv_log_log", "13. Want a log-minus-log plot (assess PH)?", 
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
           ylab= paste0(SrvHzrLbl()), fun=SrvHzr(), label.curves=list(col=Surv_Line_Colors(), cex=Surv_Line_Width()), 
           lwd=Surv_Line_Width(), col=Surv_Line_Colors(),
           xlab=paste0(SrvHzrLbl()," functions of time stratified by ", strsplit(names(srvft1()$strata)[1], "=")[[1]][1], 
                       " with ", input$SrvPltLvl*100, "% confidence intervals"), 
           mark.time=T, pch=LETTERS[1:length(names(srvft1()$strata))])
    } 
          if ( !"strata" %in% names(srvft1()) ) {
      plot(srvft1(), fun=SrvHzr(),
           xlim=c(input$sp_Xlim1, input$sp_Xlim2), ylim=c(input$sp_Ylim1,input$sp_Ylim2),
           ylab=paste0(SrvHzrLbl()), label.curves=list(col=Surv_Line_Colors(), cex=Surv_Line_Width()), 
           lwd=Surv_Line_Width(), col=Surv_Line_Colors(),
           xlab=paste0(SrvHzrLbl(), " function of time with ", input$SrvPltLvl*100, "% confidence intervals"))
    }
  } else {
    
    if(input$surv_plt_run == "Yes") {
      (do.call("survplot", list(fit1(), input$SrvPltX, conf.int=input$SrvPltLvl, conf=input$surv_plt_band, 
                                xlim=c(input$sp_Xlim1, input$sp_Xlim2), ylim=c(input$sp_Ylim1,input$sp_Ylim2),
                                xlab=paste0(SrvHzrLbl(), " time by ", input$SrvPltX), time.inc=input$sp_timeinc, 
                                fun=SrvHzr(), loglog= SrvLogLog(), label.curves=list(col=Surv_Line_Colors(), cex=Surv_Line_Width()), 
                                lwd=Surv_Line_Width(), col=Surv_Line_Colors() ))) 
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

#Reactive function to get group levels
KM_Surv_Plot_Groups <- reactive({                 
  unique(df()[, input$KMSrvPltX]) 
})
#9. Select line colors
output$KMSurvPltLnClr <- renderUI({                                 
  selectInput("km_sp_ln_clr", "9. Select line colors.", 
              choices = xyplot_Line_Color_Names(), multiple=TRUE, 
              selected= xyplot_Line_Color_Names()[1:length(KM_Surv_Plot_Groups())] )     
})
#9A. Reactive function for directly above
KM_Surv_Line_Colors <- reactive({                 
  input$km_sp_ln_clr 
})
#10. Select line width
output$KMSurvPltLnWdt <- renderUI({                                 
  numericInput("km_sp_ln_wd", "10. Select the group line width.", 
               value = 2, min=0, step = 1)     
})
#10A. Reactive function for directly above
KM_Surv_Line_Width <- reactive({                 
  input$km_sp_ln_wd 
})
#11. Legend location
output$KMSurvPltLgdLoc <- renderUI({                                
  selectInput("km_sp_lgd_loc", "11. Select the legend location.",        
              choices = c("bottomright","bottom","bottomleft","left","topleft","top","topright","right","center"), 
              multiple=FALSE, selected="topleft" ) 
})
#11A. Reactive function for legend location
KM_Surv_legend_location <- reactive({
  input$km_sp_lgd_loc
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

#Kaplan-Meier survival or hazard plots
kmSurvPltFnc <- function(KMsrvftFmla, df, Y, km_hazard_plot, KMSrvHzrLbl, km_sp_Xlim1, km_sp_Xlim2,
                         km_sp_Ylim1, km_sp_Ylim2, KMSrvPltX, 
                         LCOL, LWD, lgnd.loc) {
  pltType <- ifelse(km_hazard_plot == "No", "S", "F")
  plot(survfit(KMsrvftFmla, data= df), 
       xlim=c(km_sp_Xlim1, km_sp_Xlim2), ylim=c(km_sp_Ylim1, km_sp_Ylim2),
       ylab= paste0(KMSrvHzrLbl, " Probability"), 
       xlab=Y, mark.time=T, fun=pltType , lwd=LWD, 
       main= paste0("Kaplan-Meier plot of ", tolower(KMSrvHzrLbl), " by ", KMSrvPltX),
       pch=LETTERS[1:length(unique(df[, KMSrvPltX]))],
       col=LCOL, lty= 1:length(unique( df[, KMSrvPltX])))
  legend(lgnd.loc, legend=sort(unique( df[, KMSrvPltX])), col=LCOL, 
         lty=1:length(unique(df[, KMSrvPltX] )), bty="n", lwd=LWD, cex=1.5,
         title=KMSrvPltX)
}


#Survival fit object
KMsrvftPlot <- reactive({ 
  if (input$km_surv_plt_run == "Yes") {
    kmSurvPltFnc(KMsrvftFmla=KMsrvftFmla(), df=df(), Y=outcome(), km_hazard_plot=input$km_hazard_plot, 
                 KMSrvHzrLbl=KMSrvHzrLbl(), km_sp_Xlim1=input$km_sp_Xlim1, 
                 km_sp_Xlim2=input$km_sp_Xlim2, km_sp_Ylim1=input$km_sp_Ylim1, 
                 km_sp_Ylim2=input$km_sp_Ylim2, KMSrvPltX=input$KMSrvPltX, 
                 LCOL=KM_Surv_Line_Colors(), LWD=KM_Surv_Line_Width(), lgnd.loc=KM_Surv_legend_location() ) 
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
surv_quant_ests_fnc <- function(fit, Y, X, reg )
                                #, Trt.NM, Ctl.NM) 
                                {
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
  #colnames(est2) <- c(Trt.NM, "L95", "U95", Ctl.NM, "L95", "U95")
  colnames(est2) <- c("Treatment", "L95", "U95", "Control", "L95", "U95")
  rownames(est2) <- c("p10", "p25", "p50","p75", "p90")
  
  #Add cost difference between Treatment and controls 
#  est2$Diff. <- round(est2[, Trt.NM] - est2[, Ctl.NM], 2 )
  est2$Diff. <- round(est2[, "Treatment"] - est2[, "Control"], 2 )
  ## Add in weighted average into the table
  #Combine everything
  return(list(est1=est1, est2=est2))
}  

#This runs the function above
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
surv_mean_ests_fnc <- function(fit, Y, X, reg) 
                               #, Trt.NM, Ctl.NM) 
  {
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
#  colnames(est2) <- c(Trt.NM, "L95", "U95", Ctl.NM, "L95", "U95")
  colnames(est2) <- c("Treatment", "L95", "U95", "Control", "L95", "U95")
  rownames(est2) <- "Mean"
  #Add cost difference between Treatment and controls 
  est2$Diff. <- abs(round(est2[, "Control"]) - round(est2[, "Treatment"]))
#  est2$Diff. <- abs(round(est2[, Ctl.NM]) - round(est2[, Trt.NM]))
  return(list(est1=est1, est2=est2))
}  

#This runs the function above
surv_mean_ests <- reactive({
  if(input$survBinComp == "Yes") {
    surv_mean_ests_fnc(fit=fit1(), Y= outcome(), X= survival_var_X(), reg=input$regress_type
                       #, Trt.NM=dens_strata_final_stata_names()[2], 
                       #Ctl.NM=dens_strata_final_stata_names()[1]
                       )
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
surv_quant <- function(y, x, df)
                       #, Trt.NM, Ctl.NM 
  {
  
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
#  colnames(odf_mqt) <- c(Trt.NM, Ctl.NM)  #Observed data.frame of the means and quantiles
  colnames(odf_mqt) <- c("Treatment", "Control")  #Observed data.frame of the means and quantiles
  ###  
  return(list("odf_mqt"=odf_mqt))
}
#Run the function above
surv_quant_run <- reactive({
  if(input$survBinComp == "Yes") {
    surv_quant(y= outcome(), x= survival_var_X() , df= df()  
#               ,Trt.NM=dens_strata_final_stata_names()[2], 
#               Ctl.NM=dens_strata_final_stata_names()[1]
               )
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
         "AIC"= try(AIC( multi_state_model() )) )
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
    selectInput("msSumXLev2", "4. Select summary X levels", 
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
    selectInput("msSumTrnNm2", "5. Select specific transitions",
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
    numericInput("mSSumMultiVal2", "6. Select continuous X multiplier",
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
#             Additional function to run from the Describe tab                 #
################################################################################
## Calibration curve ##
Calibration.Curve <- function(model.fit=NULL, df.cal=NULL, outcome.Y=NULL, 
                              BINS=NULL, POS=NULL, CEX=NULL, Reg.Type=NULL,
                    VAL=FALSE, RND=NULL, PCol=NULL, LCol=NULL, LWD=NULL, YLim=NULL) {
  #Model fit name
  if(is.null(model.fit)) {
    model.fit <- fit1()
  } else {
    model.fit <- model.fit
  }
  #Data frame
  if(is.null(df.cal)) {
    df.cal <- df()
  } else {
    df.cal <- df.cal
  }
  #Outcome
  if(is.null(outcome.Y)) {
    outcome.Y <- outcome()
  } else {
    outcome.Y <- df.cal
  }
  #Regression type
  if(is.null(Reg.Type)) {
    Reg.Type <- input$regress_type
  } else {
    Reg.Type <- Reg.Type
  }
  
  #Create the number of quantiles
  if(is.null(BINS)) {
    quant.pick <- 10
  } else {
    quant.pick <- BINS
  }
  #Create the position object
  if(is.null(POS)) {
    POS.cal <- 3
  } else {
    POS.cal <- POS
  }
  #Create the CEX object
  if(is.null(CEX)) {
    CEX.cal <- 2
  } else {
    CEX.cal <- CEX
  }
  #Get unique predicted values 
  if (Reg.Type %in% c("Linear","Poisson", "Generalized Least Squares", "Quantile")) {
    unique.vals <- sort(unique(model.fit$fitted.values))    
  }
  if (Reg.Type == "Logistic") {
    unique.vals <- sort(unique(plogis(model.fit$linear.predictors))) 
  }
  #Select if the cuts are based on actual values
  if(VAL==FALSE) {
    VAL.cuts <- NULL
  }
    if(VAL==TRUE) {
      VAL.cuts <- c(unique.vals, max(unique.vals) + 1)
    } 
  #Create the rounding digit object
  if(is.null(RND)) {
    RND.VAL <- 2
  } else {
    RND.VAL <- RND
  }
  #Create the point color
  if(is.null(PCol)) {
    PCol.Val <- "blue"
  } else {
    PCol.Val <- PCol
  }
  #Create the point color
  if(is.null(LCol)) {
    LCol.Val <- "red"
  } else {
    LCol.Val <- LCol
  }
  #Create the point color
  if(is.null(LWD)) {
    LWD.Val <- 5
  } else {
    LWD.Val <- LWD
  }
  
  #Make cuts
    if (VAL==TRUE) {
    if (Reg.Type %in% c("Linear","Poisson", "Generalized Least Squares", "Quantile")) {
      cal.quants <- cut2(model.fit$fitted.values, cuts=VAL.cuts)    
    } else {
      cal.quants <- cut2(plogis(model.fit$linear.predictors), cuts=VAL.cuts)    
    }
  } else {
    if (Reg.Type %in% c("Linear","Poisson", "Generalized Least Squares", "Quantile")) {
      cal.quants <- cut2(model.fit$fitted.values, g=quant.pick)    
    } else {
      cal.quants <- cut2(plogis(model.fit$linear.predictors), g=quant.pick)    
    }
  }
  
  #Get quantile means  
  quant.ls <- list()
  if (Reg.Type %in% c("Linear","Poisson", "Generalized Least Squares", "Quantile")) {
    for (i in 1:quant.pick) {
      quant.ls[i] <- mean( model.fit$fitted.values[cal.quants == levels(cal.quants)[i] ] ,na.rm=TRUE)   
    }
  }
  if (Reg.Type == "Logistic") {
    for (i in 1:quant.pick) {
      quant.ls[i] <- mean( plogis(model.fit$linear.predictors)[cal.quants == levels(cal.quants)[i] ] ,na.rm=TRUE)  
    }
  }
  #Get frequency in each bin  
  Freq.Bin.ls <- list()
  for (i in 1:quant.pick) {
    Freq.Bin.ls[i] <- length( model.fit$y[cal.quants == levels(cal.quants)[i] ])
  }
  #Get observed means  
  obsY.ls <- list()
  for (i in 1:quant.pick) {
    obsY.ls[i] <- mean( model.fit$y[cal.quants == levels(cal.quants)[i] ] ,na.rm=TRUE)
  }
  
  #Get quantile counts  
  quant.ls <- list()
  if (Reg.Type %in% c("Linear","Poisson", "Generalized Least Squares", "Quantile")) {
    for (i in 1:quant.pick) {
      quant.ls[i] <- mean( model.fit$fitted.values[cal.quants == levels(cal.quants)[i] ] ,na.rm=TRUE)   
    }
  }
  if (Reg.Type == "Logistic") {
    for (i in 1:quant.pick) {
      quant.ls[i] <- mean( plogis(model.fit$linear.predictors)[cal.quants == levels(cal.quants)[i] ] ,na.rm=TRUE)  
    }
  }
  #Get observed counts  
  obsY.ls <- list()
  for (i in 1:quant.pick) {
    obsY.ls[i] <- mean( model.fit$y[cal.quants == levels(cal.quants)[i] ] ,na.rm=TRUE)
  }
  #Get quantile counts for the Hosmer-Lemeshow  
  quant.sum0.ls <- list()
  if (Reg.Type == "Logistic") {
    for (i in 1:quant.pick) {
      quant.sum0.ls[i] <- sum( plogis(model.fit$linear.predictors)[cal.quants == levels(cal.quants)[i] & 
                                                                     model.fit$y == as.numeric(names(table(model.fit$y)[1])) ] ,na.rm=TRUE)  
    }
  }
  quant.sum1.ls <- list()
  if (Reg.Type == "Logistic") {
    for (i in 1:quant.pick) {
      quant.sum1.ls[i] <- sum( plogis(model.fit$linear.predictors)[cal.quants == levels(cal.quants)[i] & 
                                                                     model.fit$y == as.numeric(names(table(model.fit$y)[2])) ] ,na.rm=TRUE)  
    }
  }
  obsY.sum0.ls <- list()
  if (Reg.Type == "Logistic") {
    for (i in 1:quant.pick) {
      obsY.sum0.ls[i] <- length( plogis(model.fit$linear.predictors)[cal.quants == levels(cal.quants)[i] & 
                                                                       model.fit$y == as.numeric(names(table(model.fit$y)[1])) ])  
    }
  }
  obsY.sum1.ls <- list()
  if (Reg.Type == "Logistic") {
    for (i in 1:quant.pick) {
      obsY.sum1.ls[i] <- length( plogis(model.fit$linear.predictors)[cal.quants == levels(cal.quants)[i] & 
                                                                       model.fit$y == as.numeric(names(table(model.fit$y)[2])) ] )  
    }
  }
  
  #Hosmer-Lemeshow X2 and contribution per group/bin
  #Calculate X2 values per Bin
  if (Reg.Type == "Logistic") {
    HLX2.Bin <- (( unlist(obsY.sum1.ls) - unlist(quant.sum1.ls) )^2)/ (unlist(obsY.sum0.ls)+unlist(obsY.sum1.ls))*unlist(quant.ls)*(1-unlist(quant.ls))
  } else {
    HLX2.Bin <- NA
  }
  #Calculate total X2
  if (Reg.Type == "Logistic") {
    HLX2 <- sum(HLX2.Bin)
  } else {
    HLX2 <- NA
  }
  #HL degrees of freedom
  if (Reg.Type == "Logistic") {
    HLX2.DF <- length(HLX2.Bin) - 2  
  } else {
    HLX2.DF <- NA
  }
  #HL p-value
  if (Reg.Type == "Logistic") {
    HLX2.p.val <- 1-pchisq(HLX2, df= HLX2.DF)  
  } else {
    HLX2.p.val <- NA
  }
  #Hosmer-Lemeshow test
  if (Reg.Type == "Logistic") {
    HLX2.test <- c("X2"= HLX2, "df"= HLX2.DF, "p-value"= HLX2.p.val)  
  } else {
    HLX2.test <- NA
  }
  
  #Y limits...needs to come here so it is calculated
  if(is.null(YLim)) {
    YLim.Val <- c(min(unlist(quant.ls), unlist(obsY.ls),na.rm=T )* .99, max(unlist(quant.ls), unlist(obsY.ls),na.rm=T) *1.01)
  } else {
    YLim.Val <- YLim
  }
  
  plot(1:quant.pick, quant.ls, type="n", ylim= YLim.Val,
       main="Calibration curve with observed means per bin", xlab="Means of predicted values in bins", 
       ylab= outcome.Y, axes=F, cex.lab=1.5, cex.main=2)
  axis(1, at=1:quant.pick, labels= round(unlist(quant.ls), RND.VAL), cex.axis=1.5)
  axis(2, cex.axis=1.5)
  lines(1:quant.pick, quant.ls, lwd=LWD.Val, col= LCol.Val)
  points(1:quant.pick, unlist(obsY.ls), cex=CEX.cal, pch=20, col= PCol.Val)
  text(1:quant.pick, unlist(obsY.ls), labels = round(unlist(obsY.ls), RND.VAL), cex=(CEX.cal* .5), pos=POS.cal)
  box()
  return( list("Quantiles"=levels(unlist(cal.quants)), 
               "Predicted Y means"=unlist(quant.ls), "Observed Y means"=unlist(obsY.ls), 
               "Sum of predicted outcomes"= unlist(quant.sum1.ls),  "Sum of observed outcomes"= unlist(obsY.sum1.ls),
               "Bin N"=unlist(Freq.Bin.ls), "Hosmer-Lemeshow X2 test"=HLX2.test, 
               "H-L X2 per bin"=HLX2.Bin
  ) )
}

## Function to plot a PNG picture ## Add jpeg and a real plot() in the future
fncPlotPng <- function(Image1=Null, Image2=Null) {
  library(jpeg)
  plot(1:10,ty="n", axes=F, xlab="", ylab="")
  rasterImage(Image1, .5,.5, 5,9.5)
  try(rasterImage(Image2, 5.5,.5, 10,9.5))
}

################################################################################
#        Function to calculate fixed rate loan information and Simple rate     #
################################################################################
loan.calculator <- function(Amount, Rate, Years, Extra=NA, Month=NA) {
  ################################################################################
  #Determine which type of analysis this is
  Analysis <- 1
  Analysis <- ifelse((is.na(head(Extra,1))) & is.na(head(Month)), 1, Analysis[1] ) 
  Analysis <- ifelse(!is.na(head(Extra,1)) & is.na(head(Month)), 2, Analysis[1] ) 
  Analysis <- ifelse(!is.na(head(Extra,1)) & !is.na(head(Month)), 3, Analysis[1] ) 
  Analysis <- ifelse(is.na(head(Extra,1)) & !is.na(head(Month)), 1, Analysis[1] ) 
  Analysis <- Analysis[1]
  ################################################################################
  
  #Change settings
  options(scipen=20)
  
  #Simple interest rate 
  # I= Interest
  # P= Principal
  # r = Interest rate per year
  # t= time in years
  # A = Amount
  
  P <- Amount
  r <- Rate
  t <- Years
  #Format principal using a comma for future printing
  format.Principal <- noquote(formatC(P, format="f", big.mark=",", digits = 2))
  
  #Total interest and total payments
  Interest <- P*r*t
  #Amount <- P(1 + (r*t))
  Loan.Amount <- P + Interest
  #Proportion of loan that is interest
  SIR.Interest.Proportion <- Interest/P
  
  ## Sentences ##
  SIR.Loan.Type.Intro <- "This is a summary of loan charges using a 'Simple Interest Rate'."
  SIR.Summary.Sentence <- paste0("Your principal loan amount is $", format.Principal, " with an interest rate of ", 
                                 r*100, "% for ", t, " years.")
  SIR.Payment.Sentence <- paste0("Your total loan payment is $", noquote(formatC(round(Loan.Amount, 2), format="f", big.mark=",", digits = 2)))
  SIR.Interest.Sentence <- paste0("Your total interest payment is $", noquote(formatC(round(Interest, 2), format="f", big.mark=",", digits = 2)) )
  SIR.Interest.Prop.Sentence <- paste0("The interest you pay is equal to ", round(100 * SIR.Interest.Proportion, 2),"% of the $", format.Principal, " you borrowed.")
  #Summarize and return results
  SIR.Summary <- data.frame(rbind(SIR.Loan.Type.Intro,
                                  SIR.Summary.Sentence,
                                  SIR.Payment.Sentence,
                                  SIR.Interest.Sentence,
                                  SIR.Interest.Prop.Sentence))
  row.names(SIR.Summary) <- 1:nrow((SIR.Summary))
  colnames(SIR.Summary) <- "Simple Interest Rate Summary"
  
  ################################################################################
  #                             Fixed rate formulas                              #
  ################################################################################
  years.pay <- Years
  Principal <- Amount 
  #Format principal using a comma for future printing
  format.Principal <- noquote(formatC(Principal, format="f", big.mark=",", digits = 2))
  rate <- Rate
  Monthly.rate <- (rate/12)
  Total.months <- round(years.pay * 12)
  Monthly.Payment <- (Principal * Monthly.rate* ( (1+ Monthly.rate)^Total.months)  )/ ((1+ Monthly.rate)^Total.months - 1)
  Total.Interest.Paid <- (Monthly.Payment*Total.months) - Principal
  Proportion.Loan.Interest <- ((Monthly.Payment*Total.months) - Principal) / Principal
  Total.Amount <- Total.months * Monthly.Payment
  
  ## Sentences ##
  ## Summary ##
  FIR.Loan.Type.Intro <- "This is a summary of loan charges using a 'Fixed Interest Rate'."
  FIR.Summary.Sentence <- paste0("Your principal loan amount is $", format.Principal, " with an interest rate of ",
                                 r*100, "% for ", t, " years.")
  monthly.pay.sentence <- paste0("Your monthly payment is $", 
                                 noquote(formatC(round(Monthly.Payment, 2), format="f", big.mark=",", digits = 2)))
  #Total amount owed in the loan (principal + interest)
  total.amount.owed.sentence <- paste0("Your total loan payment is $", 
                                       noquote(formatC(round(Total.Amount, 2), format="f", big.mark=",", digits = 2)))
  #The amount of interest paid
  Total.Int.Paid.sentence <- paste0("Your total interest payment is $", 
                                    noquote(formatC(round(Total.Interest.Paid, 2), format="f", big.mark=",", digits = 2)) )
  #Percentage of loan that is interest
  Percentage.Loan.Interest.sentence <- paste0("The interest you pay is equal to ", round(100 * Proportion.Loan.Interest, 2),"% of the $",format.Principal, " you borrowed.")
  
  ################################################################################
  #            Calculates original Amortization table, no extra paid amount     # 
  ################################################################################
  amt <- Monthly.Payment 
  i <- Principal
  Interest.Payment0 <- list()
  Balance0 <- list()
  Monthly.Principal0 <- list()
  Payment0 <- list()
  while(i > ceiling(-1*amt)) {
    Monthly.Principal0 <- append(Monthly.Principal0,  (amt - (i * Monthly.rate)) )
    Interest.Payment0 <- append(Interest.Payment0,  (i * Monthly.rate) )
    Payment0 <- append(Payment0,  (amt ) )
    #  i <- i - (amt - (i * Monthly.rate)) #original code
    i <- i - (amt - (i * Monthly.rate)) 
    Balance0 <- append(Balance0,  i)
    
  }
  #This summarizes the Amortization table
  Repayment.Summary0 <- data.frame(cbind(round(unlist(Payment0[1:(length(Monthly.Principal0)-1 )]), 2),
                                         round(cumsum(unlist(Payment0[1:(length(Monthly.Principal0)-1 )])), 2),
                                         round(unlist(Monthly.Principal0[1:(length(Monthly.Principal0)-1 )]), 2), 
                                         round(unlist(Interest.Payment0[1:(length(Monthly.Principal0)-1 )]), 2), 
                                         round(cumsum(unlist(Interest.Payment0[1:(length(Monthly.Principal0)-1 )])), 2), 
                                         round(unlist(Balance0[1:(length(Monthly.Principal0)-1 )]), 2) ))
  
  #Add variable names
  colnames(Repayment.Summary0) <- c("Payment","Total.Paid", "Principal", "Interest", "Total.Interest.Paid", "Remaining.Balance")
  #This calculates the Principal amount when the previous month was less than 1 payment
  Repayment.Summary0[, "Principal"][Repayment.Summary0[, "Remaining.Balance"] < 0]  <-  Repayment.Summary0[, "Payment"][Repayment.Summary0[, "Remaining.Balance"] < 0] + 
    Repayment.Summary0[, "Remaining.Balance"][Repayment.Summary0[, "Remaining.Balance"] < 0] - (Repayment.Summary0[, "Interest"][Repayment.Summary0[, "Remaining.Balance"] < 0] * 2)
  #This puts a 0 when the remaining balance amount when it is less than 0
  Repayment.Summary0[, "Remaining.Balance"][Repayment.Summary0[, "Remaining.Balance"] < 0]  <-  0
  #Adds in month indicator
  Repayment.Summary0$Month <- 1:nrow(Repayment.Summary0)
  
  
  #The month in which your payment is going more to the principal
  Principal.GT.Interest <- head(which(Repayment.Summary0[, "Principal"] > Repayment.Summary0[, "Interest"]), 1)
  Principal.GT.Int.sentence <- paste0("The month that your payment is going more towards the principal than interest is #",Principal.GT.Interest, ".")
  #The month that I have paid as much as I have originally paid
  Paid.Same.As.Loan0 <- head(which(Repayment.Summary0[, "Total.Paid"] >= Principal), 1)
  Paid.Same.As.Loan.sentence0 <- paste0("The month that you paid as much as your principal loan amount is #", Paid.Same.As.Loan0, ".")
  
  FIR.Summary <- data.frame(rbind(FIR.Loan.Type.Intro,
                                  FIR.Summary.Sentence,
                                  monthly.pay.sentence,
                                  total.amount.owed.sentence,
                                  Total.Int.Paid.sentence,
                                  Percentage.Loan.Interest.sentence,
                                  Principal.GT.Int.sentence,
                                  Paid.Same.As.Loan.sentence0))
  row.names(FIR.Summary) <- 1:nrow((FIR.Summary))
  colnames(FIR.Summary) <- "Fixed Interest Rate Summary"
  
  ################################################################################
  #            Calculates Amortization table when I pay extra $                  # 
  ################################################################################
  #This works through the formula to get the balance
  if (is.na(Extra[1])) {
    extra.amount <- 0
  } else {
    extra.amount <- Extra[1]
  }
  amt <- Monthly.Payment[1]  #Don't need to run these 2 commands since they were run above 
  i <- Principal
  Interest.Payment <- list()
  Balance <- list()
  Monthly.Principal <- list()
  Payment <- list()
  while(i > ceiling(-1*amt[1])) {
    Monthly.Principal <- append(Monthly.Principal,  (amt - (i * Monthly.rate)) )
    Interest.Payment <- append(Interest.Payment,  (i * Monthly.rate) )
    Payment <- append(Payment,  (amt + extra.amount) )
    i <- i - (amt - (i * Monthly.rate)) - extra.amount  #extra payment amount added
    Balance <- append(Balance,  i)
    
  }
  #This summarizes the amortization table
  Repayment.Summary <- data.frame(cbind(round(unlist(Payment[1:(length(Monthly.Principal)-1 )]), 2), 
                                        round(cumsum(unlist(Payment[1:(length(Monthly.Principal)-1 )])), 2),
                                        round(unlist(Monthly.Principal[1:(length(Monthly.Principal)-1 )]), 2), 
                                        round(unlist(Interest.Payment[1:(length(Monthly.Principal)-1 )]), 2), 
                                        round(cumsum(unlist(Interest.Payment[1:(length(Monthly.Principal)-1 )])), 2), 
                                        round(unlist(Balance[1:(length(Monthly.Principal)-1 )]), 2) ))
  #Add variable names
  colnames(Repayment.Summary) <- c("Payment","Total.Paid", "Principal", "Interest", "Total.Interest.Paid", "Remaining.Balance")
  #This calculates the Principal amount when the previous month was less than 1 payment
  Repayment.Summary[, "Principal"][Repayment.Summary[, "Remaining.Balance"] < 0]  <-  Repayment.Summary[, "Payment"][Repayment.Summary[, "Remaining.Balance"] < 0] + 
    Repayment.Summary[, "Remaining.Balance"][Repayment.Summary[, "Remaining.Balance"] < 0] - (Repayment.Summary[, "Interest"][Repayment.Summary[, "Remaining.Balance"] < 0] * 2)
  #This puts a 0 when the remaining balance amount when it is less than 0
  Repayment.Summary[, "Remaining.Balance"][Repayment.Summary[, "Remaining.Balance"] < 0]  <-  0
  #Adds in month indicator
  Repayment.Summary$Month <- 1:nrow(Repayment.Summary)
  
  
  #Calculates the proportion of your loan amount that is made up of interest
  Prop.Loan.Is.Interest <- tail(Repayment.Summary[,"Total.Interest.Paid"], 1) / Principal
  #Calculate the number of month to pay off the loan FOR THE ORIGINAL PAYMENT STRUCTURE
  Months.to.Pay.Loan <- nrow(Repayment.Summary0)
  #Calculate the number of month to pay off the loan with the extra payment
  Months.to.Pay.Loan.Extra <- nrow(Repayment.Summary)
  Years.to.Pay.Loan <- floor(Months.to.Pay.Loan.Extra/12)
  Remainder.Months <- Months.to.Pay.Loan.Extra %% 12
  #The month in which your payment is going more to the principal
  Principal.GT.Int.1 <- head(which(Repayment.Summary[, "Principal"] > Repayment.Summary[,"Interest"]), 1)
  #Proportion of payments that is reduced after paying extra
  Extra.Time.Saving <- Months.to.Pay.Loan - Months.to.Pay.Loan.Extra  
  #Proportion that is reduced after paying extra
  Extra.Interest.Saving <- (1 - round(Prop.Loan.Is.Interest, 4)/ round(Proportion.Loan.Interest, 4))  #extra amount/Original
  #Get original interest paid
  original.interest.paid <- round(tail(Repayment.Summary0[,"Total.Interest.Paid"], 1), 2)
  #Get new interest paid
  new.interest.paid <- round(tail(Repayment.Summary[,"Total.Interest.Paid"], 1), 2)
  #Dollars less paid in interest
  Dollar.Interest.Saving <-  original.interest.paid - new.interest.paid
  #Total amount paid for the loan
  Total.Amount.Extra <- sum(Repayment.Summary[, "Principal"]) + new.interest.paid
  #Total amount saved
  Total.Saved.Extra <- Total.Amount - Total.Amount.Extra  
  #Total amount of payments with extra amount
  Total.Payments.Extra <- (Months.to.Pay.Loan.Extra -1) * extra.amount
  #Actual saving from the loan payment
  Actual.Savings.Extra <- Total.Saved.Extra - Total.Payments.Extra
  #This includes the amount of principal, interest, and extra paid
  Main.Amount.Extra <- sum(Repayment.Summary[, "Principal"]) + new.interest.paid + Total.Payments.Extra
  
  ## Sentences ##
  #Create the time frame to payoff the loan
  if (Months.to.Pay.Loan.Extra > 12) {
    Repayment.Time <- paste0("It will take ", Years.to.Pay.Loan, " year(s) and ", Remainder.Months," months to pay off the loan.")
  } else {
    Repayment.Time <- paste0("It will take ",  Months.to.Pay.Loan.Extra," months to pay the loan.")
  }
  
  ## Sentence ##
  FIR.Plus.Summary.Sentence <- paste0("Your principal loan is $", format.Principal, ", interest rate of ",
                                      r*100, "% for ", t, " years. You pay an extra $", 
                                      noquote(formatC(round(Extra, 2), format="f", big.mark=",", digits = 2)), " each month." )
  #Savings in fewer months
  Extra.Time.Saving.Sentence <-  paste0("The extra $", noquote(formatC(round(extra.amount, 2), format="f", big.mark=",", digits = 2)), 
                                        " pays off your loan ", Extra.Time.Saving, " month(s) faster (", nrow(Repayment.Summary), 
                                        " months instead of ", Months.to.Pay.Loan," months).")
  #Savings in dollars
  Extra.Int.Saving.Sentence <-  paste0("You will pay $", noquote(formatC(round(round(Dollar.Interest.Saving, 2), 2), format="f", big.mark=",", digits = 2)), 
                                       " less in interest ($", noquote(formatC(round(new.interest.paid, 2), format="f", big.mark=",", digits = 2)), 
                                       " instead of $", noquote(formatC(round(original.interest.paid, 2), format="f", big.mark=",", digits = 2)),
                                       ") or ", round(Extra.Interest.Saving*100, 2), "% less in interest.")
  #The interest I pay as part of my loan
  Interest.Prop.Sentence <- paste0("The interest you now pay is equal to ", round(100 * Prop.Loan.Is.Interest, 2),"% of the $", format.Principal, " you borrowed.")
  #Month you pay more in principal
  Principal.GT.Int1.sentence <- paste0("The month that your payment is going more towards the principal than interest is #",Principal.GT.Int.1, ".")
  #Total amount owed in the loan (principal + interest)
  total.amount.owed.Extra.sentence <- paste0("Your loan total is now $", noquote(formatC(round(round(Main.Amount.Extra, 0), 2), format="f", big.mark=",", digits = 2)) , 
                                             ". ", "You saved $", noquote(formatC(round(round(Actual.Savings.Extra, 0), 2), format="f", big.mark=",", digits = 2)) , 
                                             " (loan reduced: $", noquote(formatC(round(round(Total.Saved.Extra, 0), 2), format="f", big.mark=",", digits = 2)) , 
                                             " - extra payments: $", noquote(formatC(round(round(Total.Payments.Extra, 0), 2), format="f", big.mark=",", digits = 2)) , ").")
  #The cost-savings ratio 
  Cost.Savings.Ratio <- paste0("Your Cost:Savings ratio is ", round(Total.Payments.Extra/Actual.Savings.Extra, 2), ". Lower is better.")
  #The month that I have paid as much as I have originally paid
  Paid.Same.As.Loan <- head(which(Repayment.Summary[, "Total.Paid"] >= Principal), 1)
  Paid.Same.As.Loan.sentence <- paste0("The month that you paid as much as your principal loan amount is #", Paid.Same.As.Loan, ".")
  
  FIR.Plus.Summary <- data.frame(rbind(FIR.Loan.Type.Intro,
                                       FIR.Plus.Summary.Sentence,
                                       total.amount.owed.Extra.sentence,
                                       Cost.Savings.Ratio,
                                       Repayment.Time,
                                       Extra.Time.Saving.Sentence,
                                       Extra.Int.Saving.Sentence,
                                       Interest.Prop.Sentence,
                                       Principal.GT.Int1.sentence,
                                       Paid.Same.As.Loan.sentence
  ))
  row.names(FIR.Plus.Summary) <- 1:nrow((FIR.Plus.Summary))
  colnames(FIR.Plus.Summary) <- "Fixed Interest Rate plus extra payment summary"
  
  ################################################################################
  #            Calculates Amortization table when I do a 1 time payment     # 
  ################################################################################
  fncL1Mth <- function(Amount, Rate, Years, Extra, Month, RS0) {
    #Make a vector of the months to pay extra
    mths <- Month
    #make a new vector that adds in time 1
    mth <- c(1,mths)  #Begins with the first month
    #Get difference between months
    tmt <- diff(mth)
    ##Sums up so that I can use it as the maximum months to subset, 1:mth_payment2
    mth_payment2 <- cumsum(tmt)
    #Show the exact months
    month_names <- paste0(mths, collapse=", ")
    
    years.pay <- Years
    Total.months <- round(years.pay * 12)
    Principal <- Amount 
    rate <- Rate
    Monthly.rate <- (rate/12)
    Monthly.Payment <- (Principal * Monthly.rate* ( (1+ Monthly.rate)^Total.months)  )/ ((1+ Monthly.rate)^Total.months - 1)
    amt2 <- Monthly.Payment 
    Total.Interest.Paid <- (Monthly.Payment*Total.months) - Principal
    Proportion.Loan.Interest <- ((Monthly.Payment*Total.months) - Principal) / Principal
    Total.Amount <- Total.months * Monthly.Payment
    extra.amount <- sum(Extra)
    
    P <- Amount
    r <- Rate
    t <- Years
    #Format principal using a comma for future printing
    format.Principal <- noquote(formatC(P, format="f", big.mark=",", digits = 2))
    FIR.Loan.Type.Intro <- "This is a summary of loan charges using a 'Fixed Interest Rate'."
    FIR.Summary.Sentence <- paste0("Your principal loan amount is $", format.Principal, " with an interest rate of ",
                                   r*100, "% for ", t, " years.")
    
    
    ## For loop to make tables 
    #Begin with the default payment schedule
    new_df <- list(RS0)
    rtm <- list()
    for (j in 1:length(mth_payment2)) {
      
      #Make lists to store data
      Interest.Payment2 <- list()
      Balance2 <- list()
      Monthly.Principal2 <- list()
      Payment2 <- list()
      #Begin while loop
      i <- new_df[[j]][["Remaining.Balance"]][ mth_payment2[j] ] - Extra[j]
      while(i > ceiling(-1*amt2)) {
        Monthly.Principal2 <- append(Monthly.Principal2,  (amt2 - (i * Monthly.rate)) )
        Interest.Payment2 <- append(Interest.Payment2,  (i * Monthly.rate) )
        Payment2 <- append(Payment2,  (amt2 ) )
        i <- i - (amt2 - (i * Monthly.rate)) 
        Balance2 <- append(Balance2,  i)
        
      }
      #This summarizes the Amortization table
      Repayment.Summary2 <- data.frame(cbind(round(unlist(Payment2[1:(length(Monthly.Principal2)-1 )]), 2), 
                                             round(cumsum(unlist(Payment2[1:(length(Monthly.Principal2)-1 )])), 2),                              round(unlist(Monthly.Principal2[1:(length(Monthly.Principal2)-1 )]), 2), 
                                             round(unlist(Interest.Payment2[1:(length(Monthly.Principal2)-1 )]), 2), 
                                             round(cumsum(unlist(Interest.Payment2[1:(length(Monthly.Principal2)-1 )]) ), 2), 
                                             round(unlist(Balance2[1:(length(Monthly.Principal2)-1 )]), 2) ))
      #Make sure this is a data frame
      Repayment.Summary2 <- data.frame(Repayment.Summary2)
      #Add variable names
      colnames(Repayment.Summary2) <- c("Payment","Total.Paid", "Principal", "Interest", "Total.Interest.Paid", "Remaining.Balance")
      #Adds in month indicator
      Repayment.Summary2$Month <- 1:nrow(Repayment.Summary2)
      
      ## Make a list where all data will go ##
      #Use the while loop to make data
      rtm[[j]] <- Repayment.Summary2
      
      new_df[[j+1]] <- rbind(new_df[[j]][1:mth_payment2[j], ], rtm[[j]])  #Add 1 so it will create the new data based on old
      #While loop will produce rtm
    }
    
    #Reduce it to just the last data
    Repayment.Summary2 <- new_df[[length(new_df)]]
    
    ##############################################################################
    #Finish data frame
    #This calculates the Principal amount when the previous month was less than 1 payment
    Repayment.Summary2[, "Principal"][Repayment.Summary2[, "Remaining.Balance"] < 0]  <-  Repayment.Summary2[, "Payment"][Repayment.Summary2[, "Remaining.Balance"] < 0] + 
      Repayment.Summary2[, "Remaining.Balance"][Repayment.Summary2[, "Remaining.Balance"] < 0] - (Repayment.Summary2[, "Interest"][Repayment.Summary2[, "Remaining.Balance"] < 0] * 2)
    #This puts a 0 when the remaining balance amount when it is less than 0
    Repayment.Summary2[, "Remaining.Balance"][Repayment.Summary2[, "Remaining.Balance"] < 0]  <-  0
    Repayment.Summary2$"Total.Interest.Paid" <- round(cumsum(Repayment.Summary2$"Interest"), 2) 
    #Add in row.names to show the payment number
    row.names(Repayment.Summary2) <- 1:nrow(Repayment.Summary2)
    #Adds in month indicator
    Repayment.Summary2$Month <- 1:nrow(Repayment.Summary2)
    
    #Calculates the proportion of your loan amount that is made up of interest
    Prop.Loan.Is.Interest2 <- tail(Repayment.Summary2[,"Total.Interest.Paid"], 1) / Principal
    #Calculate the number of month to pay off the loan FOR THE ORIGINAL PAYMENT STRUCTURE
    Months.to.Pay.Loan2 <- nrow(RS0)
    #Calculate the number of month to pay off the loan with the extra payment
    Mths.to.Pay.Loan.Extra <- nrow(Repayment.Summary2)
    Years.to.Pay.Loan2 <- floor(Mths.to.Pay.Loan.Extra/12)
    Remainder.Months2 <- Mths.to.Pay.Loan.Extra %% 12
    #The month in which your payment is going more to the principal
    Principal.GT.Int.2 <- head(which(Repayment.Summary2[, "Principal"] > Repayment.Summary2[,"Interest"]), 1)
    #Proportion of payments that is reduced after paying extra
    Extra.Time.Saving2 <- Months.to.Pay.Loan2 - Mths.to.Pay.Loan.Extra  
    #Proportion that is reduced after paying extra
    Extra.Interest.Saving2 <- (1 - round(Prop.Loan.Is.Interest2, 4)/ round(Proportion.Loan.Interest, 4))  #extra amount/Original
    #Get original interest paid
    original.interest.paid2 <- round(tail(RS0[,"Total.Interest.Paid"], 1), 2)
    #Get new interest paid
    new.interest.paid2 <- round(tail(Repayment.Summary2[,"Total.Interest.Paid"], 1), 2)
    #Dollars less paid in interest
    Dollar.Interest.Saving2 <-  original.interest.paid2 - new.interest.paid2
    #Total amount paid for the loan
    Total.Amount.1.Month <- sum(Repayment.Summary2[, "Principal"]) + new.interest.paid2
    #Total amount saved
    Total.Saved.1.Month <- Total.Amount - Total.Amount.1.Month  
    #Total amount of payments with extra amount
    Total.Payments.1.Month <- extra.amount
    #Actual saving from the loan payment
    Actual.Savings.1.Month <- Total.Saved.1.Month - Total.Payments.1.Month
    #This includes the amount of principal, interest, and extra paid
    Main.Amount.Extra2 <- sum(sum(Repayment.Summary2[, "Principal"]), new.interest.paid2, sum(Extra))
    
    ## Sentences ##
    #Create the time frame to payoff the loan
    if (Mths.to.Pay.Loan.Extra > 12) {
      Repayment.Time2 <- paste0("It will take ", Years.to.Pay.Loan2, " year(s) and ", Remainder.Months2," months to pay off the loan.")
    } else {
      Repayment.Time2 <- paste0("It will take ",  Mths.to.Pay.Loan.Extra," months to pay off the loan.")
    }
    
    ## Sentence ##
    FIR.Plus.Summary.Sentence2 <- paste0("Your principal loan is $", format.Principal, ", interest rate of ",
                                         r*100, "% for ", t, " year(s). You pay an extra $", noquote(formatC(round(extra.amount, 2), format="f", big.mark=",", digits = 2)), " at month(s) ", month_names, ".")
    #Savings in fewer months
    Extra.Time.Saving2.Sentence <-  paste0("The extra $", noquote(formatC(round(extra.amount, 2), format="f", big.mark=",", digits = 2)), " pays off your loan ", 
                                           Extra.Time.Saving2, " month(s) faster (", nrow(Repayment.Summary2), " months instead of ", Months.to.Pay.Loan2," months).")
    #Savings in dollars
    Extra.Int.Saving.Sentence2 <-  paste0("You will pay $", noquote(formatC(round(round(Dollar.Interest.Saving2, 2), 2), format="f", big.mark=",", digits = 2)), 
                                          " less in interest ($", noquote(formatC(round(new.interest.paid2, 2), format="f", big.mark=",", digits = 2)), 
                                          " instead of $", noquote(formatC(round(original.interest.paid2, 2), format="f", big.mark=",", digits = 2)),
                                          ") or ", round(Extra.Interest.Saving2*100, 2), "% less in interest.")
    #The interest I pay as part of my loan
    Interest.Prop.Sentence2 <- paste0("The interest you now pay is equal to ", round(100 * Prop.Loan.Is.Interest2, 2),
                                      "% of the $", format.Principal, " you borrowed.")
    #Month you pay more in principal
    Principal.GT.Int2.sentence <- paste0("The month that your payment is going more towards the principal than interest is #",Principal.GT.Int.2, ".")
    #Total amount owed in the loan (principal + interest)
    total.amount.owed.1.Month.sentence <- paste0("Your loan total is now $", noquote(formatC(round(round(Main.Amount.Extra2, 0), 2), format="f", big.mark=",", digits = 0)), 
                                                 ". ", "You saved about $", noquote(formatC(round(round(Actual.Savings.1.Month, 0), 2), format="f", big.mark=",", digits = 0)), 
                                                 " (loan reduced: $", noquote(formatC(round(round(Total.Saved.1.Month, 0), 2), format="f", big.mark=",", digits = 0)), " - extra payment: $", 
                                                 noquote(formatC(round(round(Total.Payments.1.Month, 2), 2), format="f", big.mark=",", digits = 0)), ").")
    #The cost-savings ratio 
    Cost.Savings.Ratio <- paste0("Your Cost:Savings ratio is ", round(Total.Payments.1.Month/Actual.Savings.1.Month, 2), ". Lower is better.")
    
    ################################################################################
    #                              corrections                                          #
    ################################################################################
    
    ## Add in correct payment amount in the right month ##
    Repayment.Summary2[, "Payment"][Month ] <-  Repayment.Summary2[, "Payment"][Month ] + Extra
    
    #Correct the last total.paid amount to match the remaining balance on the 2nd to last month
    Repayment.Summary2$"Payment"[nrow(Repayment.Summary2)] <- Repayment.Summary2$"Remaining.Balance"[nrow(Repayment.Summary2)-1 ]
    #Add in cumulative payments
    Repayment.Summary2$"Total.Paid" <- round(cumsum(Repayment.Summary2[, "Payment"]), 2)
    #The month that I have paid as much as I have originally paid
    Paid.Same.As.Loan2 <- head(which(Repayment.Summary2[, "Total.Paid"] >= Principal), 1)
    Paid.Same.As.Loan.sentence2 <- paste0("The month that you paid as much as your principal loan amount is #", Paid.Same.As.Loan2, ".")
    #Rearrange columns
    #Repayment.Summary2 <- Repayment.Summary2[, c("Payment","Total.Paid", "Principal", "Interest", "Total.Interest.Paid", "Remaining.Balance")]
    
    FIR.1.Month.Summary <- data.frame(rbind(FIR.Loan.Type.Intro,
                                            FIR.Plus.Summary.Sentence2,
                                            total.amount.owed.1.Month.sentence, #This needs work
                                            Cost.Savings.Ratio,
                                            Repayment.Time2,
                                            Extra.Time.Saving2.Sentence,
                                            Extra.Int.Saving.Sentence2,
                                            Interest.Prop.Sentence2,
                                            Principal.GT.Int2.sentence,
                                            Paid.Same.As.Loan.sentence2
    ))
    row.names(FIR.1.Month.Summary) <- 1:nrow((FIR.1.Month.Summary))
    colnames(FIR.1.Month.Summary) <- "Fixed Interest Rate plus extra payment summary"
    
    ##############################################################################
    
    return(list("Repayment.Summary2"=Repayment.Summary2,
                "FIR.1.Month.Summary"=FIR.1.Month.Summary))
  }
  
  if ( Analysis[1] == 3 ) {
    RS2 <- fncL1Mth(Amount=Amount, Rate=Rate, Years=Years, Extra=Extra, Month=Month, RS0=Repayment.Summary0)
  } 
  
  ################################################################################
  #                              Graphs                                          #
  ################################################################################
  
  # Shows how interest shoots up and then becomes less over time
  if (Analysis == 1) {
    plot(1:nrow(Repayment.Summary0), Repayment.Summary0[,"Total.Interest.Paid"], 
         col="red", main="Cumulative interest paid", xlab="Months", ylab="Cost",
         type="l", lwd=7)
  }
  if (Analysis == 2) {
    plot(1:nrow(Repayment.Summary0), Repayment.Summary0[,"Total.Interest.Paid"], 
         col="red", main="Cumulative interest paid", xlab="Months", ylab="Cost",
         type="l", lwd=7)
    lines(1:nrow(Repayment.Summary), Repayment.Summary[,"Total.Interest.Paid"], col="blue", lwd=7, lty=2)
    legend(x="topleft", legend=c("Original cumulative interest", "New cumulative interest"),
           col=c("red", "blue"), lty= c(1,2), lwd= 1.5, cex = 1.5, bty="n", inset=c(0, .05))
  }
  if (Analysis[1] == 3) {
    plot(1:nrow(Repayment.Summary0), Repayment.Summary0[,"Total.Interest.Paid"], 
         col="red", main="Cumulative interest paid", xlab="Months", ylab="Cost",
         type="l", lwd=7)
    lines(1:nrow(RS2$Repayment.Summary2), RS2$Repayment.Summary2[,"Total.Interest.Paid"], col="blue", lwd=7, lty=2)
    legend(x="topleft", legend=c("Original cumulative interest", "New cumulative interest"),
           col=c("red", "blue"), lty= c(1,2), lwd= 1.5, cex = 1.5, bty="n", inset=c(0, .05))
  }
  
  ################################################################################
  
  if (Analysis == 1) {
    return( list(
      "FIR.Summary"=FIR.Summary,
      "FIR.Repayment"=Repayment.Summary0,
      "SIR.Summary"=SIR.Summary,
      "Analysis"=Analysis))
  } 
  if (Analysis == 2) {
    return( list(
      "FIR.Summary"=FIR.Summary,
      "FIR.Repayment"=Repayment.Summary0,
      "FIR.Plus.Summary"=FIR.Plus.Summary,
      "FIR.Repayment.Plus"=Repayment.Summary,
      "SIR.Summary"=SIR.Summary,
      "Analysis"=Analysis))
  }
  if (Analysis[1] == 3) {
    return( list(
      "FIR.Summary"=FIR.Summary,
      "FIR.Repayment"=Repayment.Summary0,
      "FIR.1.Month.Summary"=RS2$FIR.1.Month.Summary,
      "FIR.Repayment.1.Month"=RS2$Repayment.Summary2,
      "SIR.Summary"=SIR.Summary ,
      "Analysis"= Analysis
    ))
  }
}

############################################################################################################
#                             Function to plot loan results                                                #
############################################################################################################

plot.loan <- function(Loan) {
  main_loan_DF <- Loan$FIR.Repayment
  Analysis <- Loan$Analysis
  if (Analysis == 1) {
    plot(1:nrow(main_loan_DF), main_loan_DF[,"Total.Interest.Paid"], 
         col="red", main="Cumulative interest paid", xlab="Months", ylab="Cost",
         type="l", lwd=7)
  }
  if (Analysis == 2) {
    new_loan_DF <- Loan$FIR.Repayment.Plus
    plot(1:nrow(main_loan_DF), main_loan_DF[,"Total.Interest.Paid"], 
         col="red", main="Cumulative interest paid", xlab="Months", ylab="Cost",
         type="l", lwd=7)
    lines(1:nrow(new_loan_DF), new_loan_DF[,"Total.Interest.Paid"], col="blue", lwd=7, lty=3)
    legend(x="topleft", legend=c("Original cumulative interest", "New cumulative interest"),
           col=c("red", "blue"), lty= c(1,3), lwd= 1.5, cex = 1.5, bty="n", inset=c(0, .05))
  }
  if (Analysis == 3) {
    new_loan_DF <- Loan$FIR.Repayment.1.Month
    plot(1:nrow(main_loan_DF), main_loan_DF[,"Total.Interest.Paid"], 
         col="red", main="Cumulative interest paid", xlab="Months", ylab="Cost",
         type="l", lwd=7)
    lines(1:nrow(new_loan_DF), new_loan_DF[,"Total.Interest.Paid"], col="blue", lwd=7, lty=3)
    legend(x="topleft", legend=c("Original cumulative interest", "New cumulative interest"),
           col=c("red", "blue"), lty= c(1,3), lwd= 1.5, cex = 1.5, bty="n", inset=c(0, .05))
  }
}

############################################################################################################
#                             Function to format loan tables                                               #
############################################################################################################
format.loan <- function(Loan) {
  #Creates main loan info
  main_loan_DF <- Loan$FIR.Repayment
  Analysis <- Loan$Analysis
  #Reformat all but the month column so that it stays an integer
  for (i in 1:(ncol(main_loan_DF) -1)) {
    main_loan_DF[, i] <- as.character(noquote(formatC(abs(main_loan_DF[, i]), width = 1, 
                                                      format = "f", big.mark=",", digits = 2)))
  }
  if (Analysis == 2) {
    new_loan_DF <- Loan$FIR.Repayment.Plus
    #Reformat all but the month column so that it stays an integer
    for (i in 1:(ncol(new_loan_DF) -1)) {
      new_loan_DF[, i] <- as.character(noquote(formatC(abs(new_loan_DF[, i]), width = 1, 
                                                       format = "f", big.mark=",", digits = 2)))
    }
  }
  if (Analysis[1] == 3) {
    new_loan_DF <- Loan$FIR.Repayment.1.Month
    #Reformat all but the month column so that it stays an integer
    for (i in 1:(ncol(new_loan_DF) -1)) {
      new_loan_DF[, i] <- as.character(noquote(formatC(abs(new_loan_DF[, i]), width = 1, 
                                                       format = "f", big.mark=",", digits = 2)))
    }
  }
  
  if (Analysis == 1) {
    return( "FIR.Repayment.Formatted"=main_loan_DF)
  } 
  if (Analysis == 2) {
    return( list("FIR.Summary.Formatted"= main_loan_DF, 
                 "FIR.Repayment.Plus.Formatted"= new_loan_DF))
  }
  if (Analysis == 3) {
    return( list("FIR.Summary.Formatted"= main_loan_DF,
                 "FIR.Repayment.1.Month"=new_loan_DF))
  }
}

#Examples
#shows I almost pay as much in interest as the loan, and when I begin to pay more in principal than interest.  
#loan.calculator(Amount=300000, Rate=.05, Years=30)$FIR.Summary
#This shows, by paying an extra $100 per month, that I save $39,942 and pay it off 44 months sooner 
#loan.calculator(Amount=300000, Rate=.05, Years=30, Extra=100)$FIR.Plus.Summary
#This shows, by paying an extra $20,000 at the 12th month, that I save $56,997 and pay it off 47 months sooner 
#loan.calculator(Amount=300000, Rate=.05, Years=30, Extra=20000, Month=12)$FIR.1.Month.Summary
#The $20,000 in the 3rd example was less than the $31,500 in the 2nd example because it reduced more interest, sooner. 
#plot.loan(loan.calculator(Amount=300000, Rate=.05, Years=30, Extra=20000, Month=12))
#format.loan(loan.calculator(Amount=300000, Rate=.05, Years=30))

#########################################
## AUC for cutoffs at each score level ##
#########################################
#This will give an AUC using each prediction as a threshold. It naturally doesn't
#calculate AUC/C-statistic values for the lowest prediction so it begins at the
#2nd prediction - highest prediction. The "Order" column indicates this and the
#first row begins with a value of 2 to indicate that it's the 2nd predicted value.

## Function for each cutoff ##
fncAucDcaClass <- function(Fit, Y, Threshold, Censor=NULL, PredTime=NULL, 
                           RegType, DF, OffSetName=NULL ) {
  
  #Adding way to get unique predictions to select the 
  if(RegType == "Poisson" & length(OffSetName) == 0 ) {
    PREDunique <- sort(unique(predict(Fit), na.rm=T)) 
  } else {
    PREDunique <- sort(unique(predict( Fit, newdata=DF), na.rm=T ))
  } 
  ####################
  # Model assessment #
  ####################
  #Make a for loop that goes through each unique prediction (excluding lowest value)
  #to get the AUC values for each prediction as a cutoff
  tmp_ls <- vector(mode = "list", length= length(PREDunique)-1 )
  for (i in 1:(length(PREDunique)-1)) {
    tmp_ls[[i]] <- fncYhatClassDf(Fit=Fit, Y=Y, Threshold=PREDunique[-1][i], Censor=Censor, PredTime=NULL, RegType=RegType, 
                                  DF=DF, OffSetName=OffSetName)
  }
  ##################
  # Get AUC values #
  ##################
  #For AUC levels
  tmp_auc_ls <- vector(mode = "list", length= length(PREDunique)-1 )
  #For the value indictor (e.g., 1st prediction = 2nd predicted value = '2')
  tmp_level_name <- vector(length= length(PREDunique)-1 )
  for (i in 1:length(tmp_ls) ) {
    tmp_auc_ls[[i]] <- fncThreshAUC(ClassDF= tmp_ls[[i]])
    tmp_level_name[i] <- i + 1
  }
  ############################
  ## Classification Results ##
  ############################
  #Make a for loop that goes through each unique prediction (excluding lowest value)
  #to get the classification results for each prediction as a cutoff
  tmp_class_ls <- vector(mode = "list", length= length(PREDunique)-1 )
  for (i in 1:(length(PREDunique)-1)) {
    tmp_class_ls[[i]] <- unlist(fncClassDfSmry(ClassDF= tmp_ls[[i]], RegType=RegType))
  }
  
  ##################
  # Final Results #
  ##################
  #Place the main results into a data frame to copy-and-paste later
  #Main result names from the classifaction
  #  Classification.Result.Names <- c( "propAbovMY1", "specifity", "fls_Neg","propAbovMY0",
  #                                    "N.AbovMY1", "N.specifity", "N.fls_Neg", "N.AbovMY0",
  #                                    "threshLev", "Transform.Threshold")
  Threshold.Names <- c( "threshLev", "Transform.Threshold")
  #Transpose of results data
  Thresh.Result <- data.frame(t(sapply(tmp_ls, function(x){as.numeric(x[Threshold.Names])})))
  #Make the Classification Results into a data frame
  Class.Result <- do.call(rbind.data.frame, tmp_class_ls)
  
  #Merge the results together with the predicted value order, thresholds, and classification results
  Results.Data1 <- data.frame("order"=tmp_level_name, Thresh.Result)
  Results.Data2 <- cbind(Results.Data1, "AUC"=unlist(tmp_auc_ls))
  Results.Data3 <- cbind(Results.Data2, Class.Result)
  #Give better column names
  colnames(Results.Data3) <- c("Order", "Threshhold", "Thresh.Trans", "AUC",
                               "Sensitivity","Specifity","False.Positives","False.Negatives","Accuracy.Rate",
                               "Error.Rate","Positive.Predictive.Value","Negative.Predictive.Value","N.Sensitivity",
                               "N.Specifity","N.False.Positives","N.False.Negatives", "N", "Net.Benefit","All.Treated",
                               "Interventions.Avoided")
  #Create ranks on main results
  Results.Data3$Rank.AUC <- rank(-Results.Data3$AUC)
  Results.Data3$Rank.Sensitivity <- rank(-Results.Data3$Sensitivity)
  Results.Data3$Rank.Specifity <- rank(-Results.Data3$Specifity)
  Results.Data3$Rank.Accuracy.Rate <- rank(-Results.Data3$Accuracy.Rate)
  Results.Data3$Rank.Net.Benefit <- rank(-Results.Data3$Net.Benefit)
  Results.Data3$Rank.Inter.Avoided <- rank(-Results.Data3$Interventions.Avoided)
  #Calculates proportion and N that were predicted to be positive of the outcome. This is how many we intervene on
  Results.Data3$Prop.Predicted.Pos <- (Results.Data3$N.Sensitivity + Results.Data3$N.False.Positives) / Results.Data3$N
  Results.Data3$N.Predicted.Pos <- Results.Data3$N.Sensitivity + Results.Data3$N.False.Positives
  #Re-arrange column order
  Results.Data3 <- Results.Data3[, c("Order","Threshhold","Thresh.Trans","AUC","Sensitivity","Specifity","False.Positives",
                                     "False.Negatives", "Accuracy.Rate","Error.Rate","Positive.Predictive.Value",
                                     "Negative.Predictive.Value","N.Sensitivity","N.Specifity","N.False.Positives",
                                     "N.False.Negatives", "N", "N.Predicted.Pos", "Prop.Predicted.Pos",
                                     "Net.Benefit","All.Treated","Interventions.Avoided","Rank.AUC","Rank.Sensitivity",
                                     "Rank.Specifity","Rank.Accuracy.Rate","Rank.Net.Benefit","Rank.Inter.Avoided")]
  return(list("Unique.Predictions"=PREDunique, "Results.Data"=Results.Data3))
}

################################################################################
#                           Bayesian Analysis                                  #
################################################################################

###################
# Get coda object #
###################
#1. Enter the name of the coda object that will be used for analysis
output$dbdaCodaObj <- renderUI({ 
  textInput("DBDAcoda", "1. Enter the Coda object name",   #Enter coda object
            value="")
})
#1A. Make coda object a reactive function
DBDA_coda_object_df <- reactive({      #Coda object for Bayesian analysis
  get(input$DBDAcoda)  
  })

#####################
## HDI Diagnostics ##
#####################
#Reactive function of Coda object parameter names
DBDA_parameter_Names <- reactive({
  if(input$DBDAcoda != "") {
    colnames(as.matrix(DBDA_coda_object_df(), chains=TRUE))[-1]
  }
})

#1. Select the parameters
output$select_dbda_diag_par <- renderUI({                                 
  selectInput("selDbPar", "1. Select the parameter.", 
              choices = DBDA_parameter_Names(), multiple=FALSE)     
})
#Reactive function for directly above
sel_DBDA_par_name <- reactive({                 
  input$selDbPar 
})
#2. Run the Diagnostics?
output$DBDA_Diag_YN <- renderUI({  
  selectInput("dbdaDiYn", "2. Run the Diagnostics?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")     
})
#2A. Reactive function for above
dbda_diagnostics_Yes_No <- reactive({
  input$dbdaDiYn
})
#Plot of DBDA diagnostics
#Confidence interval plot reactive function
plot_dbda_diagnostics <- reactive({                  #This indicates the data frame I will use.
  if(dbda_diagnostics_Yes_No() == "Yes") {
    diagMCMC( codaObject=DBDA_coda_object_df() , parName= sel_DBDA_par_name() ,  
              saveName=NULL , saveType=NULL )
  }
})
#Diagnostic plot for above
output$plotDbdaDiag <- renderPlot({ 
  if(dbda_diagnostics_Yes_No() == "Yes") {
    plot_dbda_diagnostics()
  }
}, height = 800)

#################################
## Posterior Distribution Plot ##
#################################

############
##   UI   ##
############
#1. Select the main parameter
output$dbdaPostPlot1 <- renderUI({                                
  selectInput("dbdaPP1", "1. Select the parameter.",       
              choices = DBDA_parameter_Names(), multiple=FALSE, selected=var()[1] )   
})
#1a. Reactive function for directly above
dbda_post_plot_par1 <- reactive({                 
  input$dbdaPP1 
})
#2. Do you want to compare parameter
output$dbdaPostCompareParYN <- renderUI({
  selectInput("dbdaPostComP12", "2. Compare parameters?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")
})
#2a. Reactive function for directly above
dbda_post_plot_compare_YN <- reactive({                 
  input$dbdaPostComP12 
})
#3. Select the 2nd parameter
output$dbdaPostPlot2 <- renderUI({                                
  selectInput("dbdaPP2", "3. Select 2nd parameter.",       
              choices = setdiff(DBDA_parameter_Names(), dbda_post_plot_par1()), 
              multiple=FALSE, selected=setdiff(DBDA_parameter_Names(), dbda_post_plot_par1())[1] )   
})
#3a. Reactive function for directly above
dbda_post_plot_par2 <- reactive({                 
  input$dbdaPP2 
})
#4. Do you want to run the function
output$dbdaPostCenTen <- renderUI({
  selectInput("dbdaPstCT", "4. Choose central tendency.", 
              choices = c("mode","median","mean"), multiple=FALSE, 
              selected=c("mode","median","mean")[1])
})
#4A. Reactive function for above
dbda_post_central_tendency <- reactive({
  input$dbdaPstCT
})
#5. Specify credible mass
output$dbdaPostCredibleMass <- renderUI({                                 
  numericInput("dbdaPstCrdMs", "5. Specify credible mass.",
               value = 0.95, min=0, max = 1, step = .01)
})
#5a. Reactive function for directly above
dbda_post_credible_mass <- reactive({                 
  input$dbdaPstCrdMs 
})
#6. Do you want to add a ROPE
output$dbdaPostROPEYN <- renderUI({
  selectInput("dbdaPstReYN", "6. Add ROPE?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")
})
#7. Lower ROPE value
output$dbdaPostRopeVal1 <- renderUI({                                 
  numericInput("dbdaPRpVl1", "7. Lower ROPE value.",
               value = 0, step = .01)
})
#7a. Reactive function for directly above
dbda_post_rope_val_1 <- reactive({
  if(input$dbdaPstReYN == "Yes") {
    input$dbdaPRpVl1 
  } else {
    NULL
  }
})
#8. Upper ROPE value
output$dbdaPostRopeVal2 <- renderUI({                                 
  numericInput("dbdaPRpVl2", "8. Upper ROPE value.",
               value = 0, step = .01)
})
#8a. Reactive function for directly above
dbda_post_rope_val_2 <- reactive({                 
  if(input$dbdaPstReYN == "Yes") {
    input$dbdaPRpVl2 
  } else {
    NULL
  }
})
#9. Select label size multiplier
output$dbdaPostLabMulti <- renderUI({                                 
  numericInput("dbdaPstLbMlt", "9. Increase XY label sizes.",
               value = 1.75, min=.01, step = .1)
})
#9a. Reactive function for directly above
dbda_post_label_multiplier <- reactive({                 
  input$dbdaPstLbMlt 
})
#10. Enter a weight variable.
output$dbdaPostMainTtl <- renderUI({                                 
  textInput("dbdaPstMnTtl", "10. Type main title.")     
})
#10A. Enter a weight variable.
dbda_post_main_title <- reactive({         
  input$dbdaPstMnTtl
})
#11. Enter a weight variable.
output$dbdaPostXlab <- renderUI({                                 
  textInput("dbdaPstXLb", "11. Type x-axis label.")     
})
#11A. Enter a weight variable.
dbda_post_x_label <- reactive({         
  input$dbdaPstXLb
})
#12. Enter a weight variable.
output$dbdaPostYlab <- renderUI({                                 
  textInput("dbdaPstYLb", "12. Type y-axis label.")     
})
#12A. Enter a weight variable.
dbda_post_y_label <- reactive({         
  input$dbdaPstYLb
})
#13. Do you want to add a ROPE
output$dbdaPostShowCur <- renderUI({
  selectInput("dbdaPstSC", "13. Show curve instead?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")
})
#13A. reactive function for above
dbda_post_show_curve <- reactive({         
  if(input$dbdaPstSC == "Yes") {
     TRUE
  } else {
    FALSE
  }
})
#14. Select bar colors
output$dbdaPlotLineCol <- renderUI({                                 
  selectInput("dbdaPLC", "14. Select bar color.", 
              choices = xyplot_Line_Color_Names(), multiple=FALSE, 
              selected= xyplot_Line_Color_Names()[1] )     
})
#14a. Reactive function for directly above
dbda_plot_line_colors <- reactive({                 
  input$dbdaPLC 
})
#15. X-axis limits
output$dbdaPostXaxisLims <- renderUI({                                 
  textInput("dbdaPstXLms", "15. List X-axis limits.",
            value = paste0('c( ', ')'))
})
#15a. Reactive function for directly above
dbda_post_x_axis_limits <- reactive({                 
  input$dbdaPstXLms 
})

#16. Place HDI text
output$dbdaPostPlaceHDIText <- renderUI({                                 
  numericInput("dbdaPPlHDITxt", "16. Place HDI text.",
               value = 0.7, step = .01)
})
#16a. Reactive function for directly above
dbda_post_place_hdi_text <- reactive({                 
  input$dbdaPPlHDITxt 
})
#17. Do you want to compare parameter
output$dbdaPostCompValYN <- renderUI({
  selectInput("dbdaPstCmpVl", "17. Add comparison value?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")
})
#18. Vertical line as a comparative value
output$dbdaPostCompVal <- renderUI({                                 
  numericInput("dbdaPCmpVl", "18. Plot comparative value.",
               value = 0, step = .01)
})
#18a. Reactive function for directly above
dbda_post_comparative_val <- reactive({
  if (input$dbdaPstCmpVl == "Yes") {
    input$dbdaPCmpVl 
  } else {
    NULL
  }
})
#19. Do you want to run the function
output$dbdaPostRun <- renderUI({
  selectInput("dbdaPstRn", "19. Run HDI plot?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")
}) 
#19A. Reactive function for above
dbda_post_run_Yes_No <- reactive({
  input$dbdaPstRn
})
#20. Do you want to run the function
output$dbdaPostEffSize <- renderUI({
  selectInput("dbdaPstES", "20. View effect size by distribution?", 
              choices = c("No", "Beta"), multiple=FALSE, selected="No")
})
#20A. Reactive function for above
dbda_post_effect_size <- reactive({
  input$dbdaPstES
})

# Reactive function to indicate if we compare 2 groups and we want an effect size
dbda_post_want_to_run_effect_size <- reactive({
  ifelse(dbda_post_plot_compare_YN()=="Yes" & dbda_post_run_Yes_No()== "Yes" & 
           dbda_post_effect_size() != "No", 1, 0 )
  })

# Reactive function that creates posterior for effect sizes
dbda_post_effect_size_posterior <- reactive({
  if (dbda_post_want_to_run_effect_size() == 1) {
    fncBayesEffectSize( Coda.Object=DBDA_coda_object_df(), 
                        Distribution= dbda_post_effect_size(), 
                        yVal1= dbda_post_plot_par1(), 
                        yVal2= dbda_post_plot_par2()) 
  }
})

## Plot DBDA Posterior HDI #1. ##
plot_dbda_posterior_distribution <- reactive({
  if(dbda_post_run_Yes_No() == "Yes") {
    par(mar=c(6, 7, 4, 2))
    if(dbda_post_plot_compare_YN() == "Yes") {
      plotPost( as.matrix(DBDA_coda_object_df())[, dbda_post_plot_par1()] - as.matrix(DBDA_coda_object_df())[, dbda_post_plot_par2()], 
                compVal= dbda_post_comparative_val(), cenTend= dbda_post_central_tendency(),
                ROPE=c(dbda_post_rope_val_1(),dbda_post_rope_val_2()), 
                credMass= dbda_post_credible_mass(), main= dbda_post_main_title(), 
                xlab= dbda_post_x_label(), ylab= dbda_post_y_label(), 
                showCurve= dbda_post_show_curve(), col= dbda_plot_line_colors(), 
                xlim= (eval(parse(text=dbda_post_x_axis_limits() )) ), 
                HDItextPlace= dbda_post_place_hdi_text(), cex=dbda_post_label_multiplier(), 
                cex.main= dbda_post_label_multiplier(), cex.lab= dbda_post_label_multiplier() )
    } else {
      plotPost( as.matrix(DBDA_coda_object_df())[, dbda_post_plot_par1()], 
                compVal= dbda_post_comparative_val(), cenTend= dbda_post_central_tendency(),
                ROPE=c(dbda_post_rope_val_1(),dbda_post_rope_val_2()), 
                credMass= dbda_post_credible_mass(), main= dbda_post_main_title(), 
                xlab= dbda_post_x_label(), ylab= dbda_post_y_label(), 
                showCurve= dbda_post_show_curve(), col= dbda_plot_line_colors(), 
                xlim= (eval(parse(text=dbda_post_x_axis_limits() )) ), 
                HDItextPlace= dbda_post_place_hdi_text(), cex=dbda_post_label_multiplier(), 
                cex.main= dbda_post_label_multiplier(), cex.lab= dbda_post_label_multiplier() )
    }
  }
})

## Plot DBDA Posterior HDI #2. ##
#Plot effect size
plot_dbda_post_distr_effect_size <- reactive({
  if(dbda_post_want_to_run_effect_size() == 1) {
    par(mar=c(6, 7, 4, 2))
      plotPost( dbda_post_effect_size_posterior(), 
                compVal= dbda_post_comparative_val(), cenTend= dbda_post_central_tendency(),
                ROPE=c(dbda_post_rope_val_1(),dbda_post_rope_val_2()), 
                credMass= dbda_post_credible_mass(), main= dbda_post_main_title(), 
                xlab= dbda_post_x_label(), ylab= dbda_post_y_label(), 
                showCurve= dbda_post_show_curve(), col= dbda_plot_line_colors(), 
                xlim= (eval(parse(text=dbda_post_x_axis_limits() )) ), 
                HDItextPlace= dbda_post_place_hdi_text(), cex=dbda_post_label_multiplier(), 
                cex.main= dbda_post_label_multiplier(), cex.lab= dbda_post_label_multiplier() )
    }
})

#Posterior distribution for above
output$plotDbdaPosteriorDistribution <- renderPlot({ 
  if(dbda_post_run_Yes_No() == "Yes") {
    if(dbda_post_want_to_run_effect_size() == 1) {
      plot_dbda_post_distr_effect_size()
    } else {
      plot_dbda_posterior_distribution()
    }
  }
}, height = 800)

##################################
## Hierarchical Estimation Plot ##
##################################

## Posterior summary ##
############
##   UI   ##
############
#1. Select the main parameter
output$dbdaPostSumLev <- renderUI({                                
  numericInput("dbdaPSL", "1. Hierarchical model level.",
               value = 1, min=1, max=3, step = 1)
})
#2. Select the outcome
output$dbdaPostSumY <- renderUI({
  selectInput("dbdaPSY", "2. Select the outcome.",
              choices = var(), multiple=FALSE, selected=var()[1] )
})
#3. Select hierarchical groupings...level-2
output$dbdaPostSumX1 <- renderUI({                                 
  selectInput("dbdaPSX1", "3. Select level-2 Group.", 
              choices = setdiff(var(), input$dbdaPSY), multiple=FALSE, 
              selected= setdiff(var(), input$dbdaPSY)[1])
})
#4. Select hierarchical groupings...level-3
output$dbdaPostSumX2 <- renderUI({                                 
  selectInput("dbdaPSX2", "4. Select level-3 Category.", 
              choices = setdiff(var(), try(c(input$dbdaPSY, input$dbdaPSX1))), multiple=FALSE, 
              selected= setdiff(var(), try(c(input$dbdaPSY, input$dbdaPSX1)))[1])
})
#5. Enter theta variable name.
output$dbdaPostSumTheta <- renderUI({                                 
  textInput("dbdaPstSmTht", "5. Type Theta name.")     
})
#6. Enter level-2 omega variable name.
output$dbdaPostSumOmega2 <- renderUI({                                 
  textInput("dbdaPstSmOmg2", "6. Type level-2 Omega.")     
})
#7. Enter level-2 omega variable name.
output$dbdaPostSumOmega3 <- renderUI({                                 
  textInput("dbdaPstSmOmg3", "7. Type level-3 Omega.")     
})
#8. Do you want to run the function
output$dbdaPostSumCenTen <- renderUI({
  selectInput("dbdaPstSmCT", "8. Choose central tendency.", 
              choices = c("Mode","Median","Mean"), multiple=FALSE, 
              selected= c("Mode","Median","Mean")[1])
})
#9. select the distribution type
output$dbdaPostSumDist <- renderUI({
  selectInput("dbdaPstSmDst", "9. Choose the distribution.", 
              choices = c("Beta", "Log-normal", "Normal", "t"), multiple=FALSE, 
              selected=c("Beta", "Log-normal", "Normal", "t")[1])
})
#9A. Reactive function for above
dbda_post_summary_distr <- reactive({
  input$dbdaPstSmDst
})

#10. Specify credible mass
output$dbdaPostSumCredibleMass <- renderUI({                                 
  numericInput("dbdaPstSmCrdMs", "10. Specify credible mass.",
               value = 0.95, min=0, max = 1, step = .01)
})
#10a. Reactive function for directly above
dbda_post_summary_credible_mass <- reactive({                 
  input$dbdaPstSmCrdMs 
})
#11. Is the data aggregated
output$dbdaPostSumAggrYN <- renderUI({
  selectInput("dbdaPstSmAgYN", "11. Is data aggregated?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")
})
#12. Pick the aggregated variable that represents the N.
output$dbdaPostSumAggrN <- renderUI({
  selectInput("dbdaPSAgN", "12. Select aggregated N value.",
              choices = var(), multiple=FALSE, selected=var()[1] )
})
#13. Do you want to run the function
output$dbdaPostSumRun <- renderUI({
  selectInput("dbdaPstSmRn", "13. Run posterior summary?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")
})
#13a. Reactive function for directly above
dbda_post_summary_run <- reactive({                 
  input$dbdaPstSmRn 
})
#14. Print the posterior structure
output$dbdaPostSumStructYN <- renderUI({
  selectInput("dbdaPstSmStrYN", "14. Print posterior structure?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")
})
#14a. Reactive function for directly above
dbda_post_summary_structure <- reactive({                 
  input$dbdaPstSmStrYN 
})
## Summary of DBDA Posterior ##
results_dbda_posterior_summary <- reactive({
  if(dbda_post_summary_run() == "Yes") {
    fncHdiBinSmry(MCmatrix=as.matrix(DBDA_coda_object_df(), chains=TRUE), mydf=df(), 
                  Level=input$dbdaPSL, Outcome=input$dbdaPSY, Group2=input$dbdaPSX1, 
                  Group3=input$dbdaPSX2, Theta=input$dbdaPstSmTht, Omega2=input$dbdaPstSmOmg2, 
                  Omega3=input$dbdaPstSmOmg3, Average=input$dbdaPstSmCT, 
                  AggrDF=input$dbdaPstSmAgYN, AggrN=input$dbdaPSAgN,
                  Distribution=dbda_post_summary_distr(), Cred.Mass=dbda_post_summary_credible_mass())
  }
})
#Print structure of posterior summary
output$structure_dbda_posterior_summary <- renderPrint({                                                 
  if(dbda_post_summary_structure() == "Yes") {
    str(results_dbda_posterior_summary())
  }
})

#############################
## Hierarchical Estimation ##
#############################
#1. Select the sorting order.
output$dbdaHierlphaNum <- renderUI({                                
  selectInput("dbdaHrAN", "1. Sort by name or numerical value?",
              choices = c("Alphabetical", "Numerical"),
              selected="Alphabetical")
})
#1a. Reactive function for directly above
dbda_hier_alpha_num <- reactive({                 
  input$dbdaHrAN 
})
#2. Select whether to view level-2 or level-3 group
output$dbdaHierViewGroup3 <- renderUI({
  selectInput("dbdaHrVwG3YN", "2. View level-3 groups?",
              choices = c("No", "Yes"),
              selected="No")
})
#2a. Reactive function for directly above
dbda_hier_view_group_level_3 <- reactive({                 
  input$dbdaHrVwG3YN 
})
#3. Select whether to view a subset
output$dbdaHierViewSub <- renderUI({
  selectInput("dbdaHrVwSb", "3. View a subset?",
              choices = c("No", "Yes"),
              selected="No")
})
#3a. Reactive function for directly above
dbda_hier_view_subset <- reactive({                 
  input$dbdaHrVwSb 
})
#4c. Get group names
dbda_hier_group_lev_names <- reactive({                 
  if (dbda_hier_view_subset() == "Yes") {
    if(dbda_hier_view_group_level_3() == "Yes") {
      results_dbda_posterior_summary()$Group3.Names
    } else {
      results_dbda_posterior_summary()$Group2.Names
    }
  } else {
    NULL
  } 
})
#4. Select specific groups
output$dbdaHierSpecGroup <- renderUI({                                 
  selectInput("dbdaHrGrpLvs", "4. Highlight specific groups?", 
              choices = dbda_hier_group_lev_names(), multiple=TRUE)     
})
#4a. Reactive function to get group levels
dbda_hier_group_levels <- reactive({                 
  input$dbdaHrGrpLvs 
})
#5. Select number of digits to round values by
output$dbdaHierRoundVals <- renderUI({                                 
  numericInput("dbdaHrRndVls", "5. Round decimals by.",
               value = 2, step = 1)
})
#5a. Reactive function for directly above
dbda_hier_round_decimals <- reactive({                 
  input$dbdaHrRndVls 
})
#6. Select line colors
output$dbdaHierLineCol <- renderUI({                                 
  selectInput("dbdaHrLnCl", "6. Select line color.", 
              choices = xyplot_Line_Color_Names(), multiple=FALSE, selected= "blue")     
})
#6a. Reactive function for directly above
dbda_hier_line_colors <- reactive({                 
  input$dbdaHrLnCl 
})
#7. Select point colors 
output$dbdaHierPointCol <- renderUI({                                 
  selectInput("dbdaHrPntCl", "7. Select point color.", 
              choices = xyplot_Line_Color_Names(), multiple=FALSE, selected= "red")     
})
#7a. Reactive function for directly above
dbda_hier_point_colors <- reactive({                 
  input$dbdaHrPntCl 
})
#8. Select observed rate point color for level-3 graph
output$dbdaHierObsRateCol <- renderUI({                                 
  selectInput("dbdaHrObsRtCl", "8. Level-3 observed '+' color.", 
              choices = xyplot_Line_Color_Names(), multiple=FALSE, selected= "sienna")     
})
#8a. Reactive function for directly above
dbda_hier_obs_rate_colors <- reactive({                 
  input$dbdaHrObsRtCl 
})
#9. Set target lines
output$dbdaHierTarLine <- renderUI({                                 
  textInput("dbdaHrTrLn", "9. Set target line(s).",
            value = paste0('c( ', ')'))
})
#9a. Reactive function for directly above
dbda_hier_target_line <- reactive({                 
  input$dbdaHrTrLn 
})

#10. Select target color
output$dbdaHierTarCol <- renderUI({                                 
  selectInput("dbdaHrTrCl", "10. Select target color.", 
              choices = xyplot_Line_Color_Names(), multiple=FALSE, selected= "gray")     
})
#10a. Reactive function for directly above
dbda_hier_target_color <- reactive({                 
  input$dbdaHrTrCl 
})
#11. Select whether to run overall 95% density bar
output$dbdaHierTotalBar <- renderUI({
  selectInput("dbdaHrTtlBr", "11. Create overall HDI band?",
              choices = c("No", "Yes"),
              selected="No")
})
#11a. Reactive function for directly above
dbda_hier_total_bar_interval <- reactive({                 
  input$dbdaHrTtlBr 
})
#12. Select overall band color
output$dbdaHierTotalBarCol <- renderUI({                                 
  selectInput("dbdaHrTtlBCol", "12. Select overall band color.", 
              choices = xyplot_Line_Color_Names(), multiple=FALSE, selected= "yellow")     
})
#12a. Reactive function for directly above
dbda_hier_total_bar_color <- reactive({                 
  input$dbdaHrTtlBCol 
})
## Lower and uppper x-axis limits ##
#Lower
dbda_hier_x_lim_1 <- reactive({                 
  if (dbda_post_summary_run() == "Yes") {
    min(results_dbda_posterior_summary()$Post[1:results_dbda_posterior_summary()$LTR, "Obs.Rate"], na.rm=TRUE)* 0.95
  } else {
    0
  } 
})
#Upper
dbda_hier_x_lim_2 <- reactive({                 
  if (dbda_post_summary_run() == "Yes") {
    max(results_dbda_posterior_summary()$Post[1:results_dbda_posterior_summary()$LTR, "Obs.Rate"], na.rm=TRUE)* 1.05
  } else {
    0
  } 
})
#13. Indicate lower limit of x-axis
output$dbdaHierXlim1 <- renderUI({
  numericInput("dbdaHrXLim1", "13. Lower X-axis limit.",
               value = round(dbda_hier_x_lim_1(), 2), step = .01)
})
#13a. Indicate lower limit of x-axis
dbda_hier_Xlim_val1 <- reactive({
    input$dbdaHrXLim1
})
#14. Indicate upper limit of x-axis
output$dbdaHierXlim2 <- renderUI({
  numericInput("dbdaHrXLim2", "14. Upper X-axis limit.",
               value = round(dbda_hier_x_lim_2(), 2), step = .01)
})
#14a. Indicate upper limit of x-axis
dbda_hier_Xlim_val2 <- reactive({
    input$dbdaHrXLim2
})
#15. Select whether to add a legend or not
output$dbdaHierAddLeg <- renderUI({
  selectInput("dbdaHrAdLgd", "15. Add the legend?",
              choices = c("No", "Yes"),
              selected="No")
})
#15a. Reactive function for directly above
dbda_hier_add_legend <- reactive({                 
  input$dbdaHrAdLgd 
})
#16. Legend location
output$dbdaHierLgdLoc <- renderUI({                                
  selectInput("dbdaHrLgdLc", "16. Select the legend location.",        
              choices = c("bottomright","bottom","bottomleft","left","topleft","top","topright","right","center"), 
              multiple=FALSE, selected="topright" ) 
})
#16A. Reactive function for legend location
dbda_hier_legend_location <- reactive({
  input$dbdaHrLgdLc
})
#17. Select label size multiplier
output$dbdaHierLabMulti <- renderUI({                                 
  numericInput("dbdaHrLbMlt", "17. Increase XY label sizes.",
               value = 1.75, min=.01, step = .1)
})
#17a. Reactive function for directly above
dbda_hier_label_multiplier <- reactive({                 
  input$dbdaHrLbMlt 
})
#18. Select label size multiplier
output$dbdaHierLineMulti <- renderUI({                                 
  numericInput("dbdaHrLnMlt", "18. Increase line width.",
               value = 1.75, min=.01, step = .1)
})
#18a. Reactive function for directly above
dbda_hier_line_multiplier <- reactive({                 
  input$dbdaHrLnMlt 
})

#19. Do you want to run the function
output$dbdaHierRun <- renderUI({
  selectInput("dbdaHrRn", "19. Run HDI plot?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")
})
#19A. Reactive function for above
dbda_hier_run_Yes_No <- reactive({
  input$dbdaHrRn
})
## Plot DBDA Posterior HDI ##
plot_dbda_hierarchical_estimation <- reactive({
  if(dbda_hier_run_Yes_No() == "Yes") {
    par(mar=c(2, 7, 4, 1))
    fncHdiBinP(MCmatrix=results_dbda_posterior_summary(), Level=input$dbdaPSL, 
               View.Order=dbda_hier_alpha_num(), View.Level=dbda_hier_view_group_level_3(),
               GroupX=dbda_hier_group_levels(), Lcol=dbda_hier_line_colors(), 
               Pcol=dbda_hier_point_colors(), P3.col=dbda_hier_obs_rate_colors(), 
               tgt=(eval(parse(text=dbda_hier_target_line() )) ), tgt.col=dbda_hier_target_color(),
               Cbar=dbda_hier_total_bar_interval(), plyCol=dbda_hier_total_bar_color(), 
               labMulti=dbda_hier_label_multiplier(), lineMulti= dbda_hier_line_multiplier(), 
               roundVal=dbda_hier_round_decimals(), 
               XLim1=dbda_hier_Xlim_val1(),XLim2=dbda_hier_Xlim_val2(), 
               Add.Lgd=dbda_hier_add_legend(), Leg.Loc=dbda_hier_legend_location())
  }
})
#Posterior distribution for above
output$plotDbdaHierEstimation <- renderPlot({ 
  if(dbda_hier_run_Yes_No() == "Yes") {
    plot_dbda_hierarchical_estimation()
  }
}, height = 800)


###########################################
## Posterior Predictive Check for groups ##
###########################################
#1. Select the outcome
output$dbdaPostCheckY <- renderUI({
  selectInput("dbdaPcgY", "1. Select the outcome.",
              choices = var(), multiple=FALSE, selected=var()[1] )
})
#1a. Reactive function for directly above
dbda_post_check_grp_Y <- reactive({                 
  input$dbdaPcgY 
})
#2. Select groups
output$dbdaPostCheckX <- renderUI({                                 
  selectInput("dbdaPcgX", "2. Select the group variable.", 
              choices = setdiff(var(), dbda_post_check_grp_Y()), multiple=FALSE, 
              selected= setdiff(var(), dbda_post_check_grp_Y())[1])
})
#2a. Reactive function for directly above
dbda_post_check_grp_X <- reactive({                 
  input$dbdaPcgX 
})
#3c. Get levels 
dbda_post_check_grp_X_all_levels <- reactive({                 
  if (dbda_post_check_grp_gen_YN() == "Yes") {
    sort(unique(df()[, dbda_post_check_grp_X()])) 
  }
})
#3. Select group levels
output$dbdaPostCheckLevX <- renderUI({                                 
    selectInput("dbdaPcgLX", "3. Select the group level.", 
                choices = dbda_post_check_grp_X_all_levels(), multiple=FALSE, 
              selected= dbda_post_check_grp_X_all_levels()[1])
})
#3a. Reactive function for directly above
dbda_post_check_grp_level_X <- reactive({
    input$dbdaPcgLX 
})
#4. Select the mean parameter
output$dbdaPostCheckParMn <- renderUI({                                
  selectInput("dbdaPcgPM", "4. Select mean parameter.",       
              choices = DBDA_parameter_Names(), multiple=FALSE, 
              selected=DBDA_parameter_Names()[1] )   
})
#4a. Reactive function for directly above
dbda_post_check_grp_pm <- reactive({                 
  input$dbdaPcgPM 
})
#5. Select the SD parameter
output$dbdaPostCheckParSD <- renderUI({                                
  selectInput("dbdaPcgPSD", "5. Select SD parameter.",       
              choices = setdiff(DBDA_parameter_Names(), dbda_post_check_grp_pm()), 
              multiple=FALSE, selected= setdiff(DBDA_parameter_Names(), dbda_post_check_grp_pm())[1] )   
})
#5a. Reactive function for directly above
dbda_post_check_grp_psd <- reactive({                 
  input$dbdaPcgPSD 
})
#6. select the distribution type
output$dbdaPostCheckDist <- renderUI({
  selectInput("dbdaPcgDst", "6. Choose the distribution.", 
              choices = c("Normal", "Log-normal", "t"), multiple=FALSE, 
              selected=c("Normal", "Log-normal", "t")[1])
})
#6A. Reactive function for above
dbda_post_check_grp_distr <- reactive({
  input$dbdaPcgDst
})
#7. Specify the number of posterior distribution lines
output$dbdaPostCheckNumPL <- renderUI({                                 
  numericInput("dbdaPcgNmPL", "7. Number of posterior lines.",
               value = 20, min=1, step = 1)
})
#7a. Reactive function for directly above
dbda_post_check_grp_number_lines <- reactive({                 
  input$dbdaPcgNmPL 
})
#8. Enter a weight variable.
output$dbdaPostCheckMainTtl <- renderUI({                                 
  textInput("dbdaPcgMnTtl", "8. Type main title.")     
})
#8A. Enter a weight variable.
dbda_post_check_grp_main_title <- reactive({         
  input$dbdaPcgMnTtl
})
#9. Enter a weight variable.
output$dbdaPostCheckXlab <- renderUI({                                 
  textInput("dbdaPcgXLb", "9. Type x-axis label.")     
})
#9A. Enter a weight variable.
dbda_post_check_grp_x_label <- reactive({         
  input$dbdaPcgXLb
})
#10. Select line colors
output$dbdaPostCheckBarCol <- renderUI({                                 
  selectInput("dbdaPcgBrCl", "10. Select bar color.", 
              choices = xyplot_Line_Color_Names(), multiple=FALSE, selected= "blue")     
})
#10a. Reactive function for directly above
dbda_post_check_grp_bar_colors <- reactive({                 
  input$dbdaPcgBrCl 
})
#11. Select line colors
output$dbdaPostCheckLineCol <- renderUI({                                 
  selectInput("dbdaPcgLnCl", "11. Select line color.", 
              choices = xyplot_Line_Color_Names(), multiple=FALSE, selected= "orange")     
})
#11a. Reactive function for directly above
dbda_post_check_grp_line_colors <- reactive({                 
  input$dbdaPcgLnCl 
})
#12. Specify the number of posterior distribution lines
output$dbdaPostCheckNumHB <- renderUI({                                 
  numericInput("dbdaPcgNmHB", "12. Number of histogram bars.",
               value = 30, min=1, step = 1)
})
#12a. Reactive function for directly above
dbda_post_check_grp_number_bars <- reactive({                 
  input$dbdaPcgNmHB 
})
#13. Select label size multiplier
output$dbdaPostCheckLabMulti <- renderUI({                                 
  numericInput("dbdaPcgLbMlt", "13. Increase XY label sizes.",
               value = 1.75, min=.01, step = .1)
})
#13a. Reactive function for directly above
dbda_post_check_grp_label_multiplier <- reactive({                 
  input$dbdaPcgLbMlt 
})
#14. X-axis limits
output$dbdaPostCheckXaxisLims <- renderUI({                                 
  textInput("dbdaPcgXLms", "14. List X-axis limits.",
            value = paste0('c( ', ')'))
})
#14a. Reactive function for directly above
dbda_post_check_grp_x_axis_limits <- reactive({                 
  input$dbdaPcgXLms 
})
#15. X-axis limits
output$dbdaPostCheckYaxisLims <- renderUI({                                 
  textInput("dbdaPcgYLms", "15. List Y-axis limits.",
            value = paste0('c( ', ')'))
})
#15a. Reactive function for directly above
dbda_post_check_grp_y_axis_limits <- reactive({                 
  input$dbdaPcgYLms 
})
#16b. Set the selected minimum value based on t-distribution or not
dbda_post_check_min_value_choice <- reactive({                 
  if (dbda_post_check_grp_distr() == "t") {
    95
  } else {
     0
  }
})
#16. Select label minimum value
output$dbdaPostCheckMinVal <- renderUI({                                 
  numericInput("dbdaPcgLbMV", "16. List minimum value.",
               value = dbda_post_check_min_value_choice(), step = 1)
})
#16a. Reactive function for directly above
dbda_post_check_grp_min_value <- reactive({                 
  input$dbdaPcgLbMV 
})
#17. X-axis points
output$dbdaPostCheckXaxisPoint <- renderUI({                                 
  textInput("dbdaPcgXPts", "17. Add X-axis point(s).",
            value = paste0('c( ', ')'))
})
#17a. Reactive function for directly above
dbda_post_check_grp_x_axis_points <- reactive({                 
  input$dbdaPcgXPts 
})
#18. Select point colors 
output$dbdaPostCheckPointCol <- renderUI({                                 
  selectInput("dbdaPcgPntCl", "18. Select point color.", 
              choices = xyplot_Line_Color_Names(), multiple=FALSE, selected= "red")     
})
#18a. Reactive function for directly above
dbda_post_check_point_colors <- reactive({                 
  input$dbdaPcgPntCl 
})

#19. Select whether to add a legend or not
output$dbdaPostCheckAddLeg <- renderUI({
  selectInput("dbdaPcgAdLgd", "19. Add the legend?",
              choices = c("No", "Yes"),
              selected="No")
})
#19a. Reactive function for directly above
dbda_post_check_add_legend <- reactive({                 
  input$dbdaPcgAdLgd 
})
#20. Legend location
output$dbdaPostCheckLgdLoc <- renderUI({                                
  selectInput("dbdaPcgLgdLc", "20. Select the legend location.",        
              choices = c("bottomright","bottom","bottomleft","left","topleft","top","topright","right","center"), 
              multiple=FALSE, selected="topright" ) 
})
#20A. Reactive function for legend location
dbda_post_check_legend_location <- reactive({
  input$dbdaPcgLgdLc
})
#21. Select label size multiplier
output$dbdaPostCheckRndPlc <- renderUI({                                 
  numericInput("dbdaPcgRP", "21. Round decimal places?",
               value = 1, step = 1)
})
#21a. Reactive function for directly above
dbda_post_check_grp_round_place <- reactive({                 
  input$dbdaPcgRP 
})
#22. Do you want to run the function
output$dbdaPostCheckGenGroups <- renderUI({
  selectInput("dbdaPcgGnGrp", "22. Generate group levels in #3?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")
})
#22A. Reactive function for above
dbda_post_check_grp_gen_YN <- reactive({
  input$dbdaPcgGnGrp
})
#23. Select the mean parameter
output$dbdaPostCheckParNu <- renderUI({                                
  selectInput("dbdaPcgPNu", "23. Select V (nu) d.f. parameter.",       
              choices = DBDA_parameter_Names(), multiple=FALSE, 
              selected=DBDA_parameter_Names()[1] )   
})
#23a. Reactive function for directly above
dbda_post_check_grp_pnu <- reactive({                 
  input$dbdaPcgPNu 
})
#24. Do you want to run the function
output$dbdaPostCheckRun <- renderUI({
  selectInput("dbdaPcgRn", "24. Run posterior plot?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")
})
#24A. Reactive function for above
dbda_post_check_grp_run_YN <- reactive({
  input$dbdaPcgRn
})
## Plot DBDA Posterior HDI ##
plot_dbda_posterior_group_check <- reactive({
  if(dbda_post_check_grp_run_YN() == "Yes") {     
    #    par( mar=c(4,2,2.5,.25) , mgp=c(2.5,0.5,0) , pty="m" )       
    switch(dbda_post_check_grp_distr() ,
           "Normal" =     fncGrpPostPredCheck(Coda.Object=DBDA_coda_object_df(), mydf=df(), 
                                              Outcome=dbda_post_check_grp_Y(), Group=dbda_post_check_grp_X(), 
                                              Group.Level=dbda_post_check_grp_level_X(), 
                                              Mean.Var=dbda_post_check_grp_pm(), 
                                              SD.Var=dbda_post_check_grp_psd(), Distribution=dbda_post_check_grp_distr(), 
                                              Num.Lines=dbda_post_check_grp_number_lines(), 
                                              Main.Title=dbda_post_check_grp_main_title(), 
                                              X.Lab=dbda_post_check_grp_x_label(), 
                                              Bar.Color=dbda_post_check_grp_bar_colors(), 
                                              Line.Color=dbda_post_check_grp_line_colors(), 
                                              Hist.Breaks=dbda_post_check_grp_number_bars(), 
                                              CEX.size=dbda_post_check_grp_label_multiplier(), 
                                              X.Lim=(eval(parse(text= dbda_post_check_grp_x_axis_limits() )) ),
                                              Y.Lim=(eval(parse(text= dbda_post_check_grp_y_axis_limits() )) ),
                                              Min.Val=dbda_post_check_grp_min_value(), 
                                              Round.Digits=dbda_post_check_grp_round_place(),
                                              Point.Loc= (eval(parse(text=dbda_post_check_grp_x_axis_points() )) ),
                                              PCol = dbda_post_check_point_colors(),
                                              Add.Lgd= dbda_post_check_add_legend(), 
                                              Leg.Loc=dbda_post_check_legend_location() ) , 
           "Log-normal" =     fncGrpPostPredCheck(Coda.Object=DBDA_coda_object_df(), mydf=df(), 
                                                  Outcome=dbda_post_check_grp_Y(), Group=dbda_post_check_grp_X(), 
                                                  Group.Level=dbda_post_check_grp_level_X(), 
                                                  Mean.Var=dbda_post_check_grp_pm(), 
                                                  SD.Var=dbda_post_check_grp_psd(), Distribution=dbda_post_check_grp_distr(), 
                                                  Num.Lines=dbda_post_check_grp_number_lines(), 
                                                  Main.Title=dbda_post_check_grp_main_title(), 
                                                  X.Lab=dbda_post_check_grp_x_label(), 
                                                  Bar.Color=dbda_post_check_grp_bar_colors(), 
                                                  Line.Color=dbda_post_check_grp_line_colors(), 
                                                  Hist.Breaks=dbda_post_check_grp_number_bars(), 
                                                  CEX.size=dbda_post_check_grp_label_multiplier(), 
                                                  X.Lim=(eval(parse(text= dbda_post_check_grp_x_axis_limits() )) ),
                                                  Y.Lim=(eval(parse(text= dbda_post_check_grp_y_axis_limits() )) ),
                                                  Min.Val=dbda_post_check_grp_min_value(), 
                                                  Round.Digits=dbda_post_check_grp_round_place(),
                                                  Point.Loc= (eval(parse(text=dbda_post_check_grp_x_axis_points() )) ),
                                                  PCol = dbda_post_check_point_colors(),
                                                  Add.Lgd= dbda_post_check_add_legend(), 
                                                  Leg.Loc=dbda_post_check_legend_location() ) ,
           "t" = fncPlotMcANOVA(codaSamples=DBDA_coda_object_df(), datFrm=df(), 
                                yName=dbda_post_check_grp_Y(), xName=dbda_post_check_grp_X(), 
                                MCmean=dbda_post_check_grp_pm(), 
                                MCsigma=dbda_post_check_grp_psd(), 
                                MCnu= dbda_post_check_grp_pnu(),
                                Num.Lines=dbda_post_check_grp_number_lines(), 
                                Main.Title=dbda_post_check_grp_main_title(), 
                                X.Lab=dbda_post_check_grp_x_label(), 
                                Line.Color=dbda_post_check_grp_line_colors(), 
                                CEX.size=dbda_post_check_grp_label_multiplier(), 
                                X.Lim=(eval(parse(text= dbda_post_check_grp_x_axis_limits() )) ),
                                Y.Lim=(eval(parse(text= dbda_post_check_grp_y_axis_limits() )) ),
                                PCol = dbda_post_check_point_colors(),
                                Add.Lgd= dbda_post_check_add_legend(), 
                                Leg.Loc=dbda_post_check_legend_location(),
                                T.Percentage=dbda_post_check_grp_min_value() ) )
  }
})
#Posterior distribution for above
output$plotDbdaPostCheckGroup <- renderPlot({ 
  if(dbda_post_check_grp_run_YN() == "Yes") {
    plot_dbda_posterior_group_check()
  }
}, height = 800)

#######################################
## Proportion above specific Y value ##
#######################################
#1. Select the main parameter
output$dbdaPropGtPar1 <- renderUI({                                
  selectInput("dbdaPgtP1", "1. Select 'Center' parameter.",       
              choices = DBDA_parameter_Names(), multiple=FALSE, 
              selected= DBDA_parameter_Names()[1] )   
})
#1a. Reactive function for directly above
dbda_prop_gr_than_par1 <- reactive({                 
  input$dbdaPgtP1 
})
#2. Select the 2nd parameter
output$dbdaPropGtPar2 <- renderUI({                                
  selectInput("dbdaPgtP2", "2. Select 'Spread' parameter.",       
              choices = setdiff(DBDA_parameter_Names(), dbda_prop_gr_than_par1()), 
              multiple=FALSE, selected=setdiff(DBDA_parameter_Names(), dbda_prop_gr_than_par1())[1] )   
})
#2a. Reactive function for directly above
dbda_prop_gr_than_par2 <- reactive({                 
  input$dbdaPgtP2 
})
#3. select the distribution type
output$dbdaPropGtDist <- renderUI({
  selectInput("dbdaPgtDst", "3. Choose the distribution.", 
              choices = c("Beta", "Log-normal", "Normal"), multiple=FALSE, 
              selected=c("Beta", "Log-normal", "Normal")[1])
})
#3A. Reactive function for above
dbda_prop_gr_than_distr <- reactive({
  input$dbdaPgtDst
})
#4. Do you want to run the function
output$dbdaPropGtCenTen <- renderUI({
  selectInput("dbdaPgtCT", "4. Choose central tendency.", 
              choices = c("Mode","Median","Mean"), multiple=FALSE, 
              selected=c("Mode","Median","Mean")[1])
})
#4A. Reactive function for above
dbda_prop_gr_than_central_tendency <- reactive({
  input$dbdaPgtCT
})

#5. X-axis limits
output$dbdaPropGtYval <- renderUI({                                 
  textInput("dbdaPgtYs", "5. List Y-values.",
            value = paste0('c( ', ')'))
})
#5a. Reactive function for directly above
dbda_prop_gr_than_y_values <- reactive({                 
  eval(parse(text= input$dbdaPgtYs))  
})
#6. X-axis limits
output$dbdaPropGtQval <- renderUI({                                 
  textInput("dbdaPgtQs", "6. List Y percentiles.",
            value = paste0('c( ', ')'))
})
#6a. Reactive function for directly above
dbda_prop_gr_than_q_values <- reactive({                 
  eval(parse(text=  input$dbdaPgtQs)) 
})
#7. Do you want to run the function
output$dbdaPropGtRun <- renderUI({
  selectInput("dbdaPgtRn", "7. Run probabilities?", 
              choices = c("No", "Yes"), multiple=FALSE, selected="No")
})
#7A. Reactive function for above
dbda_prop_gr_than_run_YN <- reactive({
  input$dbdaPgtRn
})

## Run the proportion greater than Y value function
dbda_prop_gr_than_func_run <- reactive({
  if(dbda_prop_gr_than_run_YN() == "Yes") {
    options(scipen=30)
    fncPropGtY( Coda.Object= DBDA_coda_object_df(), 
            Distribution= dbda_prop_gr_than_distr(), 
            yVal= dbda_prop_gr_than_y_values() , 
            qVal= dbda_prop_gr_than_q_values(), 
            Center= dbda_prop_gr_than_par1(), 
            Spread= dbda_prop_gr_than_par2(), 
            CenTend= dbda_prop_gr_than_central_tendency() )
  }
})

#proportion greater than Y value
output$dbdaPropGt_output <- renderPrint({ 
  if(dbda_prop_gr_than_run_YN() == "Yes") {
    dbda_prop_gr_than_func_run()
      }
})

#kermit
########################################
## My functions for Bayesian analysis ##
########################################

################################################################################
#                  1. Expand aggregated data into  full data                   #
################################################################################
#This function expands y ~ x1 + X2 BINARY aggregated data in multi-row data
#X1= lower level hierarchy (e.g., patients), X2= higher level hierarchy (e.g., States) 
#Z= Outcome, N= Total count of denominator (e.g., Z/N= rate)
fncExpAgr <- function(DF, X1, X2, Z, N, Level) {
  #Add variable that tracks number of 0s
  t_exp_df <- DF
  t_exp_df$Yzero <- t_exp_df[, N] - t_exp_df[, Z]
  #Total rows
  tot_rows <- nrow(DF)
  #For loop to get the vectors of 0s and 1s 
  z_ls <- vector(mode = "list", length = tot_rows)
  #Level 1 or 2 models
  if (Level <= 2) {
    for (i in 1:tot_rows) {
      z_ls[[i]] <- data.frame(Z=c(rep(0, t_exp_df[i, "Yzero"]), rep(1, t_exp_df[i, Z])), 
                              X1=rep(t_exp_df[i, X1], t_exp_df[i, N]))
    }
  }
  #Level 3 models
  if (Level == 3) {
    for (i in 1:tot_rows) {
      z_ls[[i]] <- data.frame(Z=c(rep(0, t_exp_df[i, "Yzero"]), rep(1, t_exp_df[i, Z])), 
                              X1=rep(t_exp_df[i, X1], t_exp_df[i, N]),
                              X2=rep(t_exp_df[i, X2], t_exp_df[i, N]))
    }
  }
  #Turn list into data frame
  ExpDF <- do.call(rbind.data.frame, z_ls)
  #Give column names
  if (Level <= 2) {
    colnames(ExpDF) <- c(Z, X1)
  }
  if (Level == 3) {
    colnames(ExpDF) <- c(Z, X1, X2)
  }
  return("ExpDF"=ExpDF)
}

################################################################################
# 2. Function to get the binary (non)hierarchical estimation posterior summary #
################################################################################
#Uses DBDA function below, summarizePost() 
#MCmatrix= MCMC matrix after: mcmcMat <- as.matrix(codaSamples, chains=TRUE) 
#mydf: Original data frame (i.e., not the data list used in JAGS) 
#Level= A 1, 2, or 3 level indicator for the type of model 
#Outcome= Model outcome 
#Group2 & Group3= level 2 and 3 group names 
fncHdiBinSmry <- function(MCmatrix, mydf, Level, Outcome, Group2, Group3=NULL, 
                          Theta=NULL, Omega2=NULL, Omega3=NULL, Average=NULL, 
                          AggrDF="No", AggrN=NULL, Distribution=NULL, Cred.Mass=0.95 ) {
  ###################################################################  
  ## These next 30 lines will exclude irrelevant parameters ##
  keep_theta_cols <- NULL
  keep_omega2_cols <- NULL
  keep_omega3_cols <- NULL
  #Level-1, non-hierarchical model
  if(Level== 1) {
    keep_theta_cols <- grep(Theta, colnames(MCmatrix))
  }
  #Level-2, hierarchical model
  if(Level== 2) {
    keep_theta_cols <- grep(Theta, colnames(MCmatrix))
  }
  if(Level== 2) {
    keep_omega2_cols <- grep(Omega2, colnames(MCmatrix))
  }
  #Level-3, hierarchical model
  if(Level== 3) {
    keep_theta_cols <- grep(Theta, colnames(MCmatrix))
  }
  if(Level== 3) {
    keep_omega3_cols <- grep(Omega3, colnames(MCmatrix))
  }
  if(Level== 3) {
    keep_omega2_cols <- setdiff(grep(Omega2, colnames(MCmatrix)), keep_omega3_cols)
  }
  #These are just the columns I need
  keep_these_cols <- c(which(colnames(MCmatrix) == "CHAIN"), keep_theta_cols, keep_omega2_cols, keep_omega3_cols)
  #Revise MCMC matrix to just the columns I need
  MCmatrix <- MCmatrix[, keep_these_cols]
  ###################################################################  
  #Get order of participants in rows of aggregated data
  if (AggrDF == "Yes") {
    group2_aggr_factor <- factor(mydf[, Group2], levels=c(mydf[, Group2])) 
  } else {
    group2_aggr_factor <- levels(factor(mydf[, Group2], levels=names(table(sort(mydf[, Group2]))) )) 
  }
  # Use the aggregated data if needed
  if (AggrDF == "Yes") {
    mydf <- fncExpAgr(DF=mydf, X1=Group2, X2=Group3, Z=Outcome, N=AggrN, Level=Level) 
  } else {
    mydf <- mydf
  }
  #Make a factor from aggregated data so that it follows the same order
  if (AggrDF == "Yes") {
    mydf[, Group2] <- factor(mydf[, Group2], levels=group2_aggr_factor)
  } else {
    mydf[, Group2] <- factor(mydf[, Group2], levels=group2_aggr_factor)
  }
  #Get the type of estimate 
  if (is.null(Average)) {
    average_type <- "Mode"
  } else {
    average_type <- Average
  }
  #Number of level-2 groups
  numGroups <- length(table(mydf[, Group2]))
  #Number of level-3 categories
  if(Level== 3) {
    numCats <- length(table(mydf[, Group3]))
  }
  #Get the level-2 group names
  Group2.Names <- sort(unique(mydf[, Group2]))
  #Get the level-3 group names
  if(Level== 3) {  #Get group-3 rates 
    Group3.Names <- sort(unique(mydf[, Group3]))
  } else {
    Group3.Names <- NULL
  }
  #Get the column numbers with the Theta and Omega in it
  #Level-1, non-hierarchical model
  if(Level== 1) {
    theta_cols <- grep(Theta, colnames(MCmatrix))
  } 
  #Level-2, hierarchical model
  if(Level== 2) {
    theta_cols <- grep(Theta, colnames(MCmatrix))
  }
  if(Level== 2) {
    omega2_cols <- grep(Omega2, colnames(MCmatrix))
  }
  #Level-3, hierarchical model
  if(Level== 3) {
    theta_cols <- grep(Theta, colnames(MCmatrix))
  }
  if(Level== 3) {
    omega3_cols <- grep(Omega3, colnames(MCmatrix))
  }
  if(Level== 3) {
    omega2_cols <- setdiff(grep(Omega2, colnames(MCmatrix)), omega3_cols)
  }
  
  #Get the column numbers with the Theta and Omega in it
  mat_cols <- 1:length(colnames(MCmatrix))      #Get range of matrix columns 
  #Level-1, non-hierarchical model
  if(Level== 1) {
    pName <- colnames(MCmatrix)
  } 
  #Level-2, hierarchical model
  if(Level== 2) {
    pName <- colnames(MCmatrix)[c(1, theta_cols, omega2_cols, setdiff(mat_cols, c(1, theta_cols, omega2_cols) ))]
  }
  #Level-3, hierarchical model
  if(Level== 3) {
    pName <- colnames(MCmatrix)[c(1, theta_cols, omega2_cols, omega3_cols, setdiff(mat_cols, c(1, theta_cols, omega2_cols, omega3_cols)))]
  }
  
  ## Create posterior chain summary ##
  # pName <- colnames(MCmatrix)   #Parameter names
  postDF <- list()
  #  for (i in 1:length(pName[-1]))  {
  for (i in 1:length(pName[which(pName != "CHAIN")]))  {
    #    postDF[[i]] <- summarizePost( MCmatrix[, pName[i] ] , compVal=NULL , ROPE=NULL )
    postDF[[i]] <- summarizePost( MCmatrix[, pName[which(pName != "CHAIN")[i]] ] , compVal=NULL , ROPE=NULL, credMass=Cred.Mass )
  }
  #Turn summary into data frame
  postDF <- data.frame(do.call( "rbind", postDF))
  #  rownames(postDF) <- pName[-1]
  rownames(postDF) <- pName[which(pName != "CHAIN")]
  ## Get number of parameters to create Group 2 variable
  m_param_tot <- length(colnames(MCmatrix)) - 1 #Total parameters from MCmatrix
  #Level-2, hierarchical model
  if(Level== 2) {
    param2_so_far <- length(c(theta_cols, omega2_cols))
    other_param2 <- m_param_tot - param2_so_far
    num_rep_Group2 <- other_param2 + 1
  }
  #Level-3, hierarchical model
  if(Level== 3) {
    param3_so_far <- length(c(theta_cols, omega2_cols, omega3_cols))
    other_param3 <- (m_param_tot - param3_so_far) / (numCats + 1)
    num_rep_Group3 <- other_param3 
  }
  #Enter Group/Cat into summary
  #Level-1, non-hierarchical model
  if(Level== 1) {
    row_name <- c(names(table(mydf[, Group2])) )
  }
  #Level-2, hierarchical model
  if(Level== 2) {
    row_name <- c(names(table(mydf[, Group2])), rep("Overall", num_rep_Group2) )
  }
  #Level-3, hierarchical model. try() used if "omega" is only param passed into JAGS function
  if(Level== 3) {
    row_name <- c(names(table(mydf[, Group2])), 
                  rep(names(table(mydf[, Group3])), 1), "Overall",
                  try(rep(names(table(mydf[, Group3])), num_rep_Group3)), try(rep("Overall", num_rep_Group3)) )
  }
  #Make a variable for the group 2 and 3 names
  postDF[, Group2] <- row_name
  
  #Enter Group/Cat counts
  #Level-1, non-hierarchical model
  if(Level== 1) {
    postDF[, Outcome] <- c(table(mydf[, Outcome], mydf[, Group2])[2,])
  }
  #Level-2, hierarchical model
  if(Level== 2) {
    postDF[, Outcome] <- c(table(mydf[, Outcome], mydf[, Group2])[2,], 
                           rep(0, nrow(postDF) - length(table(mydf[, Group2])) ))
  }
  #Level-3, hierarchical model
  if(Level== 3) {
    postDF[, Outcome] <- c(table(mydf[, Outcome], mydf[, Group2])[2,], 
                           table(mydf[, Outcome], mydf[, Group3])[2,],
                           rep(0, nrow(postDF) - 
                                 (length(table(mydf[, Group2])) + length(table(mydf[, Group3]))) ))
  }
  #Enter Group/Cat Ns
  #Level-1, non-hierarchical model
  if(Level== 1) {
    postDF[, "N"] <- c(colSums(table(mydf[, Outcome], mydf[, Group2])))
  }
  #Level-2, hierarchical model
  if(Level== 2) {
    postDF[, "N"] <- c( colSums(table(mydf[, Outcome], mydf[, Group2])), 
                        rep(0, nrow(postDF) - length(table(mydf[, Group2])) ))
  }
  #Level-3, hierarchical model
  if(Level== 3) {
    postDF[, "N"] <- c( colSums(table(mydf[, Outcome], mydf[, Group2])), 
                        colSums(table(mydf[, Outcome], mydf[, Group3])),
                        rep(0, nrow(postDF) - 
                              (length(table(mydf[, Group2])) + length(table(mydf[, Group3]))) ))
  }
  #Enter Group/Cat sums
  #Level-1, non-hierarchical model
  if(Level== 1) {
    cont_sum_ls1 <- by(mydf[, Outcome], mydf[, Group2], FUN=sum, na.rm = TRUE)
    class(cont_sum_ls1) <- "list"
    #cont_sum_ls1 <- factor(mydf[, Group2], levels=group2_aggr_factor)
    postDF[, "Sum"] <- unlist(cont_sum_ls1)
  }
  #Level-2, hierarchical model
  if(Level== 2) {
    cont_sum_ls1 <- by(mydf[, Outcome], mydf[, Group2], FUN=sum, na.rm = TRUE)
    class(cont_sum_ls1) <- "list"
    postDF[, "Sum"] <- c( unlist(cont_sum_ls1), 
                          rep(0, nrow(postDF) - length(table(mydf[, Group2])) ))
  }
  #Level-3, hierarchical model
  if(Level== 3) {
    cont_sum_ls1 <- by(mydf[, Outcome], mydf[, Group2], FUN=sum, na.rm = TRUE)
    class(cont_sum_ls1) <- "list"
    cont_sum_ls2 <- by(mydf[, Outcome], mydf[, Group3], FUN=sum, na.rm = TRUE)
    class(cont_sum_ls2) <- "list"
    postDF[, "Sum"] <- c( unlist(cont_sum_ls1), unlist(cont_sum_ls2) ,
                          rep(0, nrow(postDF) - 
                                (length(table(mydf[, Group2])) + length(table(mydf[, Group3]))) ))
  }
  
  #Observed rate
  if (Distribution == "Beta") {
    postDF$Obs.Rate <- postDF[, Outcome] / postDF[, "N"]
  } else {
    postDF$Obs.Rate <- postDF[, "Sum"] / postDF[, "N"]
  }
  
  #Get the row numbers with the Theta and Omega in it
  #Level-1, non-hierarchical model
  if(Level== 1) {
    theta_rows <- grep(Theta, rownames(postDF))
  } 
  #Level-2, hierarchical model
  if(Level== 2) {
    theta_rows <- grep(Theta, rownames(postDF))
  }
  if(Level== 2) {
    omega2_rows <- grep(Omega2, rownames(postDF))
  }
  #Level-3, hierarchical model
  if(Level== 3) {
    theta_rows <- grep(Theta, rownames(postDF))
  }
  if(Level== 3) {
    omega2_rows <- grep(Omega2, rownames(postDF))
  }
  if(Level== 3) {
    omega3_rows <- grep(Omega3, rownames(postDF))
  }
  #First make length of theta, omega2 and omega3 rows
  if(Level== 1) {   
    LTR <- length(theta_rows)
  }
  if(Level== 1) {
    LO2R<- NULL
  }
  if(Level== 1) {
    LO3R <- NULL
  }
  if(Level== 2) {   
    LTR <- length(theta_rows)
  }
  if(Level== 2) {   
    LO2R <- length(omega2_rows)
  }
  if(Level== 2) {
    LO3R <- NULL
  }
  if(Level== 3) {   
    LTR <- length(theta_rows)
  }
  if(Level== 3) {   
    LO2R <- length(setdiff(omega2_rows, omega3_rows))
  }
  if(Level== 3) {   
    LO3R <- length(omega3_rows)
  }
  #Create an order by parameter
  #Level-2, hierarchical model order of results
  if(Level== 1) {
    o6 <- c(order(postDF[, average_type][1:numGroups], decreasing = T)) 
  } 
  #ISSUE: first 2 elements are kappas and skipped in the order below colnames(mcmcMatTT)
  if(Level== 2) {
    o6 <- c(order(postDF[theta_rows, average_type], decreasing = T),  #Theta
            omega2_rows[order(postDF[omega2_rows, average_type], decreasing = T)], #Omega2
            setdiff(1:nrow(postDF), c(theta_rows, omega2_rows)))  #All others
  } 
  if(Level== 3) {
    o6 <- c(order(postDF[, average_type][theta_rows], decreasing = T), #Theta and then Omega2
            setdiff(omega2_rows, omega3_rows)[order(postDF[, average_type][setdiff(omega2_rows, omega3_rows)], decreasing = T)],
            omega3_rows[ order(postDF[, average_type][omega3_rows], decreasing = T)], #Omega3
            ((numGroups+numCats+LO3R ):nrow(postDF))[((numGroups+numCats+LO3R):nrow(postDF)) != omega3_rows] ) #All other
  } 
  #Re-order the rows now
  if(Level== 1) {
    postDFb <- postDF[o6, ]
  } 
  if(Level== 2) {
    postDFb <- rbind(postDF[o6[1:LTR], ], 
                     postDF[o6[(LTR + 1):(LTR + LO2R )], ],
                     postDF[ o6[( (LTR + LO2R) + 1):nrow(postDF)], ])
  } 
  if(Level== 3) {  #In this order: theta, Omega2, omega3 
    postDFb <- rbind(postDF[o6[1:LTR], ],                                   #Thetas
                     postDF[o6[(LTR + 1):(LTR + LO2R )], ],                 #omega2
                     postDF[o6[(LTR + LO2R + 1):(LTR + LO2R + 1)], ],       #omega3
                     postDF[ o6[( (LTR + LO2R + LO3R) + 1):nrow(postDF)], ])  #all others
  } 
  ## Put postDF in reverse order so that it will plot correctly
  if(Level== 1) {
    postDFa <- postDF[rev(1:LTR), ]
  } 
  if(Level== 2) {
    postDFa <- rbind(postDF[rev(1:LTR), ],
                     postDF[rev(omega2_rows), ],
                     postDF[ rev(( (LTR + LO2R) + 1):nrow(postDF)), ])
  } 
  if(Level== 3) {  #In this order: theta, Omega2, omega3 
    postDFa <- rbind(postDF[ rev(1:LTR), ],                                   #Thetas
                     postDF[ rev((LTR + 1):(LTR + LO2R )), ],                 #omega2
                     postDF[ omega3_rows, ],                                  #omega3
                     postDF[ rev(setdiff(1:(nrow(postDF)), c(1:(LTR + LO2R), omega3_rows))), ])  #all others
  } 
  
  ## Get the level-3 rates if doing a level-3 model for the plot points
  #I Changed Area into a 3 level factor to get a better 3rd level
  if(Level== 3) { 
    hspa <- aggregate(mydf[, Outcome] ~ mydf[, Group3] + mydf[, Group2] , data=mydf, FUN="sum")
  }
  if(Level== 3) { 
    hspb <- aggregate(mydf[, Outcome] ~ mydf[, Group3] + mydf[, Group2] , data=mydf, FUN="length")
  }
  #Merge
  if(Level== 3) { 
    a1hsp <- cbind(hspa, hspb[, 3])
  }
  if(Level== 3) { 
    colnames(a1hsp)[4] <- "Nsamp"
  }
  if(Level== 3) { 
    colnames(a1hsp)[1:3] <- c(Group3, Group2, Outcome)
  }
  ##(Add 1 to month and) make it a factor so it begins at 1
  if(Level== 3) { 
    a1hsp[, Group2] <- as.numeric(a1hsp[, Group2])
  }
  if(Level== 3) { 
    a1hsp[, Group3] <- as.numeric( as.factor(a1hsp[, Group3]) )  #Turning into factor to get numeric
  }
  #Make the rate
  if(Level== 3) { 
    a1hsp$Rate <- a1hsp[, (ncol(a1hsp)-1)] / a1hsp[, ncol(a1hsp)]
  }
  #For loop to create object
  if(Level== 3) {  #Get group-3 rates 
    Group3.Obs <- list()
    for (i in 1:LO2R) {
      Group3.Obs[[i]] <- a1hsp[a1hsp[, Group3] == i, "Rate"]
    }
  } else {
    Group3.Obs <- NULL
  }
  #Reverse order to match Post1
  if(Level== 3) {  #Get group-3 rates 
    Group3.Obs1 <- rev(Group3.Obs)
  } else {
    Group3.Obs1 <- NULL
  }
  #Get numerical order to match with Post2
  if(Level== 3) {  #Get group-3 rates 
    g3_order <- as.numeric(gsub("[^0-9.-]", "", rownames(postDFb[ ((LTR + 1):(LTR + LO2R )), ]) ))
  } else {
    g3_order <- NULL
  }
  #Get level-3 estimates in numerical order
  if(Level== 3) {  #Get group-3 rates 
    Group3.Obs2 <- Group3.Obs[g3_order]
  } else {
    Group3.Obs2 <- NULL
  }
  return(list("Post"=postDF,"Post1"=postDFa, "Post2"=postDFb, "Level"=Level, "Outcome"=Outcome,
              "Group2.Names"=Group2.Names, "Group3.Names"=Group3.Names, 
              "Group2"=Group2, "Group3"=Group3,
              "Theta"=Theta, "Omega2"=Omega2, "Omega3"=Omega3, "Average"=Average,
              "Order"=o6, "LTR"=LTR, "LO2R"=LO2R, "LO3R"=LO3R,
              "Lower"= intersect("HDIlow", colnames(postDF)),
              "Upper"= intersect("HDIhigh",colnames(postDF)), 
              "ciconf_lev"= unique(postDF$HDImass), "g3_order"=g3_order,
              "Group3.Obs1"=Group3.Obs1, "Group3.Obs2"=Group3.Obs2 ))
}

################################################################################
#           3. Function to plot HDIs for hierarchical estimation               #
################################################################################
fncHdiBinP <- function(MCmatrix, Level, View.Order="Alphabetical", View.Level="No",  #View.Order= alpha/numerical, View.Level=Yes/No "View 3rd level?"
                       GroupX=NULL, Lcol, Pcol, P3.col, tgt=NULL, tgt.col, 
                       Cbar, plyCol, labMulti=1, lineMulti=1, 
                       roundVal, XLim1, XLim2,Add.Lgd, Leg.Loc) {
  # Assign objects from MCMC matrix objects  
  Group2 <- MCmatrix$Group2 
  Group3 <- MCmatrix$Group3 
  Outcome <- MCmatrix$Outcome 
  ciconf_lev <- MCmatrix$ciconf_lev 
  Average <- MCmatrix$Average 
  Theta <- MCmatrix$Theta 
  Omega2 <- MCmatrix$Omega2 
  Omega3 <- MCmatrix$Omega3 
  LTR <- MCmatrix$LTR
  LO2R <- MCmatrix$LO2R
  LO3R <- MCmatrix$LO3R
  Lower <- MCmatrix$Lower 
  Upper <- MCmatrix$Upper 
  Group3.Obs1 <- MCmatrix$Group3.Obs1
  Group3.Obs2 <- MCmatrix$Group3.Obs2
  
  #Select the group levels that determine which rows go into the data frame
  if(Level== 1) {
    if (View.Level == "No") {
      row_numbers <- 1:LTR
    }
  }
  #Level-2, hierarchical model
  if(Level== 2) {
    if (View.Level == "No") {
      row_numbers <- 1:(LTR + LO2R)
    }
  }
  #Level-3, hierarchical model
  if(Level== 3) {
    if (View.Level == "Yes") {
      row_numbers <- setdiff(1:(LTR + LO2R + LO3R), 1:LTR)
    } else {
      row_numbers <- setdiff(1:(LTR + LO2R + LO3R), (LTR + 1):(LTR + LO2R))
    }
  }
  #Create hdidf table of Bayesian estimates
  if(View.Order == "Alphabetical") {                                  #Post1 
    hdidf <- MCmatrix$Post1[row_numbers , c(Group2, Average, Lower, Upper, "Obs.Rate")]
  } 
  if(View.Order == "Numerical") {                                  #Post2 
    hdidf <- MCmatrix$Post2[row_numbers , c(Group2, Average, Lower, Upper, "Obs.Rate")]
  }
  #Create adf table of observed values
  if(View.Order == "Alphabetical") {                                  #Post1 
    adf <- MCmatrix$Post1[row_numbers , c(Group2, "Obs.Rate")]
  } 
  if(View.Order == "Numerical") {                                  #Post2 
    adf <- MCmatrix$Post2[row_numbers , c(Group2, "Obs.Rate")]
  }
  #Hierarchical average for the highest level (e.g., Omega)  
  if(Level >= 2) {
    mainYmn <- hdidf[nrow(hdidf), which(colnames(hdidf)== Average)]
  } else {
    mainYmn <- NA
  }
  #Select which 3-level category data gets reported
  if(View.Order == "Alphabetical") {                                  #Post1 
    Group3.Obs <- Group3.Obs1
  } 
  if(View.Order == "Numerical") {                                  #Post2 
    Group3.Obs <- Group3.Obs2
  }
  #Main X labels
  if(Level >= 2) {
    X_Label <- paste0("Dashed line= Overall hierarchical est. of ", round(mainYmn, roundVal), ", ", ciconf_lev * 100, "% ", "HDI",
                      " [", round(hdidf[nrow(hdidf), which(colnames(hdidf)== "HDIlow")], roundVal), ", ", 
                      round(hdidf[nrow(hdidf), which(colnames(hdidf)== "HDIhigh")], roundVal),"]")
  } else {
    X_Label <- paste0(Group2, " posterior estimates")
  }
  #Main title
  #Level-3, hierarchical model
  if(Level== 3) {
    if (View.Level == "Yes") {
      main_ttl <- paste0(ciconf_lev * 100, "% ", "Highest Density Intervals of ", Outcome, " by ", Group3)
    } 
  }
  if(Level== 3) {
    if (View.Level == "No") {
      main_ttl <- paste0(ciconf_lev * 100, "% ", "Highest Density Intervals of ", Outcome, " by ", Group2)
    } 
  }
  if(Level < 3) {
    main_ttl <- paste0(ciconf_lev * 100, "% ", "Highest Density Intervals of ", Outcome, " by ", Group2)
  }
  #Legend
  #Level-3, hierarchical model
  if(Level== 3) {
    if (View.Level == "Yes") {
      legend_text <- c(paste0("Observed ", Group2), paste0("Observed ", Group3), "Hierarchical Estimate")
      legend_type <- c(3, 2, 24)
      pcol_vector <- c(P3.col, Pcol, Pcol)
    } 
  } 
  if(Level== 3) {
    if (View.Level == "No") {
      legend_text <- c("Observed Rate", "Hierarchical Estimate")
      legend_type <- c(2, 24)
      pcol_vector <- Pcol
    } 
  } 
  if(Level == 2) {
    legend_text <- c("Observed Rate", "Hierarchical Estimate")
    legend_type <- c(2, 24)
    pcol_vector <- Pcol
  }
  if(Level == 1) {
    legend_text <- c("Estimate")
    legend_type <- c(24)
    pcol_vector <- Pcol
  }
  
  #Get names of level 1:3 or just overall level-3 groups
  if(Level== 3) {
    if (View.Level == "Yes") {
      group_names <- hdidf[-nrow(hdidf), Group2]
    } else {
      group_names <- hdidf[1:LTR, Group2]
    }
  } else {
    group_names <- hdidf[1:LTR, Group2]
  }
  #Determine which group names to plot
  if (is.null(GroupX)) {
    plot_group_names <- 1:length(group_names)
  } else {
    plot_group_names <- which(group_names %in% GroupX)
  }
  
  #Get rows to use for plots, level 1 print everything, other levels print everything - last row
  if(Level== 1) {
    plot_row_numbers <- (1:length(row_numbers))[plot_group_names]
  } else {
    plot_row_numbers <- (1:(length(row_numbers) - 1))[plot_group_names]
  }
  ## Create plot
  rng <- seq(min(adf[, "Obs.Rate"], na.rm=TRUE)* 0.95, max(adf[, "Obs.Rate"], na.rm=TRUE)* 1.05, length.out=nrow(adf[plot_row_numbers,]))
  par(mar=c(5,7,4,6))
  plot(rng, 1:length(rng), type="n", ylab="", 
       xlab= X_Label,
       axes=F,  cex.lab=1*labMulti, xlim=c(XLim1, XLim2))
  #axes=F,  cex.lab=1*labMulti)
  title(main_ttl, cex.main = 1*labMulti) 
  #Merge 2 tables so I can get points in correct order
  for (i in 1:(length(plot_row_numbers)) ) {
    lines(c(hdidf[plot_row_numbers, Lower][i], hdidf[plot_row_numbers, Upper][i]), 
          c((1:length(plot_row_numbers))[i], (1:length(plot_row_numbers))[i]), 
          lwd=1*lineMulti, col=Lcol) 
    #Points for observed rates and Bayesian estimates
    points(hdidf[plot_row_numbers, Average][i ], i, pch=24, col=Pcol, lwd=1, bg=Pcol, cex=1.75*labMulti) 
    if(Level >= 2) {
      points(hdidf[plot_row_numbers, "Obs.Rate"][i ], (1:length(plot_row_numbers))[i], pch=2, col=Pcol, lwd=1, bg=Pcol, cex=1.75*labMulti)
    }
  }
  #Add points for the level-3 category for each group per category
  if(Level== 3) {
    if (View.Level == "Yes") {
      for (i in 1:LO2R) {
        points( Group3.Obs[[plot_row_numbers[i]]], rep( (1:LO2R)[i], length(Group3.Obs[[plot_row_numbers[i]]])), pch=3, 
                col=P3.col, lwd=1)
      }
    } 
  }
  #Mean line
  if(Level >= 2) {
    abline(v=mainYmn, lwd=1*lineMulti, col="grey", lty=3)
  }
  axis(1) 
  axis(2, at=1:length(plot_row_numbers), labels= substr(hdidf[plot_row_numbers, Group2], 1, 10), las=1, cex.axis=1*labMulti )
  axis(4, at=1:length(plot_row_numbers), labels= round(hdidf[plot_row_numbers, Average], roundVal), las=1, cex.axis= 1*labMulti*.75 )
  ## Add overall confidence bar ##
  #Create x and y data
  Cbar_x <- c(rep(hdidf[nrow(hdidf), Lower], length(plot_row_numbers)), rep(hdidf[nrow(hdidf), Upper], length(plot_row_numbers) ))
  Cbar_y <- c(1:length(plot_row_numbers), length(plot_row_numbers):1)
  #Create shading
  if(Cbar=="Yes") {
    polygon(Cbar_x, Cbar_y, col = adjustcolor(plyCol, alpha.f = 0.4), border= plyCol )
  }
  #Add legend
  if(Add.Lgd =="Yes") {
    legend(Leg.Loc, legend=legend_text, col=pcol_vector, 
           pch=legend_type, pt.bg=pcol_vector, cex = 2, bty="n", inset=c(0, .05))
  } 
  #Target line
  abline(v=tgt, lwd=1*lineMulti, col=tgt.col, lty=1)
  box()
}

################################################################################
#                  4. Posterior Predictive Check for groups                    #
################################################################################
#Use the coda object and dataset. Works for normal and log-normal distributions.
fncGrpPostPredCheck <- function(Coda.Object, mydf, Outcome, Group, Group.Level,
                                Mean.Var, SD.Var, Distribution, Num.Lines=NULL, 
                                Main.Title=NULL, X.Lab=NULL, Bar.Color=NULL, 
                                Line.Color=NULL, Hist.Breaks=NULL, CEX.size=NULL, 
                                X.Lim=NULL, Y.Lim=NULL, Min.Val=NULL, Round.Digits=NULL,
                                Point.Loc= NULL, PCol=NULL, Add.Lgd= NULL, Leg.Loc= NULL) {
  #Make coda into as.matrix  
  MC.Chain <- as.matrix( Coda.Object )
  chainLength <- NROW(MC.Chain)  #Chain length
#  par( mar=c(4,2,2.5,.25) , mgp=c(2.5,0.5,0) , pty="m" )
  
  #Get a number of pseudo-random chains
  pltIdx <- floor(seq(1, chainLength, length= Num.Lines)) 
  #Get spread in outcome variable values
  xComb <- seq( Min.Val , max(mydf[, Outcome], na.rm=TRUE) , length=501 )
  #Make X limit values, I can set my minimum value
  if (is.null(X.Lim)) {
    X.Lim <- c(Min.Val, round(max(mydf[, Outcome], na.rm=TRUE), digits=Round.Digits))
  }
  ## Graph ##
  par( mar=c(4,2,2.5,.25) , mgp=c(2.5,0.5,0) , pty="m" )
  #Allows me to run if I only have 1 group by leaving "generate levels =="No"
  if (nchar(Group.Level) == 0 ) { 
    hist( mydf[, Outcome], xlab= X.Lab, ylab=NULL, 
          main= Main.Title, breaks=Hist.Breaks, col= Bar.Color, border="white", 
          prob=TRUE, cex.lab=CEX.size, cex=CEX.size, cex.main=CEX.size, 
          xlim=X.Lim, ylim=Y.Lim, lab=NULL, axes=FALSE)
  } else {
    hist( mydf[, Outcome][mydf[, Group] == Group.Level] , xlab= X.Lab, ylab=NULL, 
          main= Main.Title, breaks=Hist.Breaks, col= Bar.Color, border="white", 
          prob=TRUE, cex.lab=CEX.size, cex=CEX.size, cex.main=CEX.size, 
          xlim=X.Lim, ylim=Y.Lim, lab=NULL, axes=FALSE)
  }
  
  
  axis(1)  #Put values in labels
  #This adds in minimum value in case it isn't in range (e.g., show negatve range of normal distribution)
  axis(1, at=X.Lim[1]) 
 # box()   #Dropping this for now because it looks better without
  #Add in posterior estimate lines
  for ( chnIdx in pltIdx ) {
    #Normal Distribution
    if (Distribution == "Normal") {
      lines( xComb ,
             dnorm( xComb, MC.Chain[chnIdx, Mean.Var], MC.Chain[chnIdx, SD.Var] ),
             col= Line.Color )
    }
    #Log Normal Distribution
    if (Distribution == "Log-normal") {
      lines( xComb ,
             dlnorm( xComb, MC.Chain[chnIdx, Mean.Var], MC.Chain[chnIdx, SD.Var] ),
             col= Line.Color )
    }
    #Add points 
    if (!is.null(Point.Loc)) {
      for (i in 1:length(Point.Loc)) {
        points(x=Point.Loc[i], y=0, pch=3, lwd=3, cex=CEX.size, col=PCol)
      }
    }
    #Add legend
    if(Add.Lgd =="Yes") {
      legend_text <- c(paste0("Observed ", abbreviate(Group.Level, 8)), "Posterior Estimate")
      legend_type <- c(1, 1)
      pcol_vector <- c(Bar.Color, Line.Color)
      legend(Leg.Loc, legend=legend_text, col=pcol_vector, 
             lty=legend_type, pt.bg=pcol_vector, cex = 2, bty="n", inset=c(0, .05))
    }
  }
  
} #End of function

################################################################################
#             5. Posterior predictive check for ANOVA                          #
################################################################################
#This is a modified function from PlotMCmeanC: Jags-Ymet-Xnom1fac-MrobustHet.r. 
#The original plot produces a generic plot, then it shifts the X and Y axes to
#a completely different area to produce the graphs. Old code embedded below
#from what looks like normal distributions.
fncPlotMcANOVA <- function( codaSamples=NULL, datFrm=NULL , yName=NULL , xName=NULL,  
                            MCmean=NULL, MCsigma=NULL, MCnu=NULL, Num.Lines=NULL, 
                            Main.Title=NULL, X.Lab=NULL, Line.Color=NULL, 
                            CEX.size=NULL, X.Lim=NULL, Y.Lim=NULL, PCol = NULL,
                            Add.Lgd= NULL, Leg.Loc=NULL, T.Percentage=NULL ) {
  mcmcMat <- as.matrix(codaSamples, chains=TRUE)
  chainLength <- NROW( mcmcMat )
  y <- datFrm[, yName]
  x <- as.numeric(as.factor(datFrm[, xName]))
  xlevels <- levels(as.factor(datFrm[, xName]))
  #Make x-limits
  if (is.null(X.Lim)) {
    X.Limits <- c(0.1,length(xlevels) + 0.1)
  } else {
    X.Limits <- X.Lim
  }
  #Make y-limits
  if (is.null(Y.Lim)) {
    Y.Limits <- c(min(y) - 0.2 * (max(y) - min(y)), max(y) + 0.2*(max(y) - min(y)))
  } else {
    Y.Limits <- Y.Lim
  }
  #Get generic mean parameter name to use for graphing
  mean_par <- strsplit(MCmean, "[", fixed=TRUE)[[1]][1]
  #Get generic sigma (SD) parameter name to use for graphing
  sigma_par <- strsplit(MCsigma, "[", fixed=TRUE)[[1]][1]
  # Display data with posterior predictive distributions
  par( mar=c(5,6,2.5,.25))
  plot(-1,0, 
       #       xlim=c(0.1,length(xlevels) + 0.1) , 
       #       ylim=c(min(y) - 0.2 * (max(y) - min(y)), max(y) + 0.2*(max(y) - min(y))) , 
       xlim= X.Limits, xlab=X.Lab , xaxt="n" , ylab= yName ,
       ylim= Y.Limits, main=Main.Title, 
       cex.lab=CEX.size, cex=CEX.size, cex.main=CEX.size )
  axis( 1 , at=1:length(xlevels) , tick=FALSE , lab=xlevels )
  for ( xidx in 1:length(xlevels) ) {
    xPlotVal = xidx 
    yVals = y[ x == xidx ]
    points( rep(xPlotVal, length(yVals)) + runif(length(yVals), -0.05, 0.05) , 
            yVals , pch=1 , cex=CEX.size , col= PCol ) #COLOR
    chainSub = round(seq(1, chainLength, length= Num.Lines)) #20
    for ( chnIdx in chainSub ) {
      #      m = mcmcMat[chnIdx, paste("m[", xidx, "]", sep="")]
      # m = mcmcMat[chnIdx,paste("b[",xidx,"]",sep="")]
      #      s = mcmcMat[chnIdx, paste("ySigma[", xidx,"]", sep="")]
      m = mcmcMat[chnIdx, paste(mean_par, "[", xidx, "]", sep="")]
      s = mcmcMat[chnIdx, paste(sigma_par, "[", xidx,"]", sep="")]
      nu = mcmcMat[chnIdx, MCnu]
      #This controls tails of t distribution. Coverage "*.01" to get proportion
      tlim= qt( c((0.5 - (T.Percentage*0.01)/2), (0.5 + (T.Percentage*0.01)/2)) , df= nu )  
      #This controls tails of t distribution
      yl = m + tlim[1]*s
      yh = m + tlim[2]*s
      ycomb=seq(yl, yh, length=501) ##201
      #ynorm = dnorm(ycomb,mean=m,sd=s)
      #ynorm = 0.67*ynorm/max(ynorm)
      yt = dt( (ycomb - m) / s , df= nu )
      yt = 0.67 * yt / max(yt)           #This controls heighth of curve peaks
      lines( xPlotVal - yt , ycomb , col= Line.Color ) #COLOR
    }
  }
  #Add legend
  if(Add.Lgd =="Yes") {
    legend_text <- c("Observed Value", "Posterior Estimate")
    legend_type <- c(0, 1)
    pch_type <- c(1, -1)
    pcol_vector <- c(PCol, Line.Color)
    legend(Leg.Loc, legend=legend_text, col=pcol_vector, 
           lty=legend_type, pt.bg=pcol_vector, cex = 2, pch=pch_type, 
           bty="n", inset=c(0, .05))
  }
}

################################################################################
#           6. Get proportions above/below specific values                     #
################################################################################
#This function calculates the proportion above specific values.
fncPropGtY <- function( Coda.Object=NULL, Distribution=NULL, yVal=NULL, qVal=NULL, 
                        Center=NULL, Spread=NULL, CenTend=NULL ) {
  #Convert into a matrix
  MC.Matrix <- as.matrix(Coda.Object, chains=TRUE)    
  
  #Shape and rate parameters using the mode values of omega and kappa
  a_shape <- MC.Matrix[, Center] * (MC.Matrix[, Spread] - 2) + 1
  b_shape <- (1 - MC.Matrix[, Center]) * (MC.Matrix[, Spread] - 2) + 1
  
  ###################
  ## Mean for Beta ##
  ###################
  if(!is.null(yVal)) {
    if(Distribution == "Beta") {
      mean_val <- a_shape/MC.Matrix[, Spread]
    }
  }
  if(!is.null(yVal)) {
    if(Distribution == "Beta") {
      mean_val_dist <- summarizePost(mean_val)[c(c("Mode","Median",
                                                   "Mean")[which(c("Mode","Median","Mean")== CenTend)],"HDIlow","HDIhigh")]
    }
  }
  #Make NA if not from a beta distribution
  if (Distribution == "Beta") {
    mean_val_dist <- mean_val_dist
  } else {
    mean_val_dist <- NA
  }
  
  #######################
  ## Beta distribution ##
  #######################
  ## Get summary ##
  # Proportion greater than Y
  PbetaGtY <- list()
  if(!is.null(yVal)) {
    if(Distribution == "Beta") {
      for (i in 1:length(yVal)) {
        PbetaGtY[[i]] <- summarizePost( 1- pbeta(yVal[i], a_shape, 
                                                 b_shape) )[c(c("Mode","Median","Mean")[which(c("Mode","Median","Mean") == CenTend)], "HDIlow", "HDIhigh")]
        names(PbetaGtY)[i] <- paste0("Y_", yVal[i])
      }
    }
  }
  # Quantiles of Y
  QbetaGtY <- list()
  if(!is.null(qVal)) {
    if(Distribution == "Beta") {
      for (i in 1:length(qVal)) {
        QbetaGtY[[i]] <- summarizePost( qbeta(qVal[i], a_shape, 
                                              b_shape) )[c(c("Mode","Median","Mean")[which(c("Mode","Median","Mean") == CenTend)], "HDIlow", "HDIhigh")]
        names(QbetaGtY)[i] <- paste0("Percentile_", qVal[i])
      }
    }
  }
  #Effect size for 2 values, in "yVal"
  betaEffSize2Y <- list()
  if(length(yVal) == 2) {
    if(Distribution == "Beta") {
      es1 <- (asin(sign(1- pbeta(yVal[1], a_shape, b_shape) ) * sqrt(abs(1- pbeta(yVal[1], a_shape, b_shape) ))))*2  
      es2 <- (asin(sign(1- pbeta(yVal[2], a_shape, b_shape)  ) * sqrt(abs(1- pbeta(yVal[2], a_shape, b_shape) ))))*2  
      #Get the posterior summary on effect size between the 2 Y-values
      betaEffSize2Y <- summarizePost(abs(es1 - es2) )[c(c("Mode","Median","Mean")[which(c("Mode","Median","Mean") == CenTend)], "HDIlow", "HDIhigh")]
    }
  }
  #Return NAs for NULL objects
  #probability
  if (length(PbetaGtY)==0 ) {
    PbetaGtY <- NA
  } else {
    PbetaGtY <- PbetaGtY
  } 
  #quantile
  if (length(QbetaGtY)==0 ) {
    QbetaGtY <- NA
  } else {
    QbetaGtY <- QbetaGtY
  } 
  #Effect size
  if (length(betaEffSize2Y)== 0 ) {
    betaEffSize2Y <- NA
  } else {
    betaEffSize2Y <- betaEffSize2Y
  } 
  
  #############################
  ## Log-normal distribution ##
  #############################
  ## Get summary ##
  # Proportion greater than Y
  PlogGtY <- list()
  if(!is.null(yVal)) {
    if(Distribution == "Log-normal") {
      for (i in 1:length(yVal)) {
        PlogGtY[[i]] <- summarizePost( plnorm(q=yVal[i], meanlog= MC.Matrix[, Center], 
                                              sdlog= MC.Matrix[, Spread], lower.tail=FALSE) )[c(c("Mode","Median","Mean")[which(c("Mode","Median","Mean") == CenTend)], "HDIlow", "HDIhigh")]
        names(PlogGtY)[i] <- paste0("Y_", yVal[i])
      }
    }
  }
  # Quantiles of Y
  QlogGtY <- list()
  if(!is.null(qVal)) {
    if(Distribution == "Log-normal") {
      for (i in 1:length(qVal)) {
        QlogGtY[[i]] <- summarizePost( qlnorm(p=qVal[i], meanlog= MC.Matrix[, Center], 
                                              sdlog= MC.Matrix[, Spread]) )[c(c("Mode","Median","Mean")[which(c("Mode","Median","Mean") == CenTend)], "HDIlow", "HDIhigh")]
        names(QlogGtY)[i] <- paste0("Percentile_", qVal[i])
      }
    }
  }
  #Return NAs for NULL objects
  #probability
  if (length(PlogGtY)==0 ) {
    PlogGtY <- NA
  } else {
    PlogGtY <- PlogGtY
  } 
  #quantile
  if (length(QlogGtY)==0 ) {
    QlogGtY <- NA
  } else {
    QlogGtY <- QlogGtY
  } 
  
  #########################
  ## Normal distribution ##
  #########################
  ## Get summary ##
  # Proportion greater than Y
  PnormGtY <- list()
  if(!is.null(yVal)) {
    if(Distribution == "Normal") {
      for (i in 1:length(yVal)) {
        PnormGtY[[i]] <- summarizePost( pnorm(q=yVal[i], mean= MC.Matrix[, Center], 
                                              sd= MC.Matrix[, Spread], lower.tail=FALSE) )[c(c("Mode","Median","Mean")[which(c("Mode","Median","Mean") == CenTend)], "HDIlow", "HDIhigh")]
        names(PnormGtY)[i] <- paste0("Y_", yVal[i])
      }
    }
  }
  # Quantiles of Y
  QnormGtY <- list()
  if(!is.null(qVal)) {
    if(Distribution == "Normal") {
      for (i in 1:length(qVal)) {
        QnormGtY[[i]] <- summarizePost( qnorm(p=qVal[i], mean= MC.Matrix[, Center], 
                                              sd= MC.Matrix[, Spread]) )[c(c("Mode","Median","Mean")[which(c("Mode","Median","Mean") == CenTend)], "HDIlow", "HDIhigh")]
        names(QnormGtY)[i] <- paste0("Percentile_", qVal[i])
      }
    }
  }
  #Return NAs for NULL objects
  #probability
  if (length(PnormGtY)==0 ) {
    PnormGtY <- NA
  } else {
    PnormGtY <- PnormGtY
  } 
  #quantile
  if (length(QnormGtY)==0 ) {
    QnormGtY <- NA
  } else {
    QnormGtY <- QnormGtY
  } 
  
  ######################################
  ## Create final distribution values ##
  ######################################
  ## 1. Probability ##
  if (Distribution == "Beta") {
    PdisGtY <- PbetaGtY
  } 
  if (Distribution == "Log-normal") {
    PdisGtY <- PlogGtY
  } 
  if (Distribution == "Normal") {
    PdisGtY <- PnormGtY
  } 
  #Make NA if the above weren't selected
  if (is.null(PdisGtY)) {
    PdisGtY <- NA
  } 
  ## 2. Quantile ##
  if (Distribution == "Beta") {
    QdisGtY <- QbetaGtY
  } 
  if (Distribution == "Log-normal") {
    QdisGtY <- QlogGtY
  } 
  if (Distribution == "Normal") {
    QdisGtY <- QnormGtY
  } 
  #Make NA if the above weren't selected
  if (is.null(QdisGtY)) {
    QdisGtY <- NA
  } 
  ## 3. Effect size ##
  if (Distribution == "Beta") {
    disEsY <- betaEffSize2Y
  } 
  ##################################################
  ## Gets effect sizes for non-Beta distributions ##
  ##################################################
  nonBetaEffSize2Y <- vector()
  if(length(yVal) == 2) {
    if(Distribution != "Beta") {
      oes1 <- (asin(sign( PdisGtY[[1]][1] ) * sqrt(abs(PdisGtY[[1]][1] ))))*2  
      oes2 <- (asin(sign( PdisGtY[[2]][1]) * sqrt(abs( PdisGtY[[2]][1]))))*2 
      #Get the posterior summary on effect size between the 2 Y-values
      nonBetaEffSize2Y <- abs(oes1 - oes2)
    }
  }
  #Effect size
  if (length(nonBetaEffSize2Y)== 0 ) {
    nonBetaEffSize2Y <- NA
  } else {
    nonBetaEffSize2Y <- nonBetaEffSize2Y
  } 
  
  #This is temp code for log-normal that makes it NA for now
  if (Distribution != "Beta") {
    disEsY <- nonBetaEffSize2Y
  } 
  
  #Make NA if the above weren't selected
  if (is.null(disEsY)) {
    disEsY <- NA
  } 
  
  return(list("Est.Prop.GT.Y"= PdisGtY, 
              "Est.Effect.Size.2Y"= disEsY, 
              "Est.Quantile.Y"= QdisGtY,
              "Est.Mean.Beta"=mean_val_dist) )
}

################################################################################
#                 7. R2 for models with metric only predictors                 #
################################################################################
#This produces R2 for normal or t-distribution models with only metric predictors.
#The formula is used in the bottom example, just needs the correlation matrix
#from the actual observed dataset and some matrix algebra of the MCMC object's 
#beta coefficients to get the R2. This works on standardized data but
#it should work on regular data. And it might work with nominal predictors or 
#maybe factors. The code for an older Kruschke version is below the function.

fncXmetR2 <- function( codaSamples=NULL , data=NULL , 
                       Z.Beta=NULL, xName=NULL, yName=NULL) {
  y = data[, yName]
  x = as.matrix(data[, xName])
  MCMC_Mat = as.matrix(codaSamples, chains=TRUE)
  #  zbeta  = MCMC_Mat[,grep("^zbeta$|^zbeta\\[", colnames(MCMC_Mat))]
  zbeta  = MCMC_Mat[,grep(paste0("^", Z.Beta,"$|^", Z.Beta, "\\[" ),colnames(MCMC_Mat))]
  if ( ncol(x)==1 ) { 
    zbeta = matrix( zbeta , ncol=1 ) 
  }
  #-----------------------------------------------------------------------------
  # Compute R^2 for credible parameters:
  YcorX = cor( y , x ) # correlation of y with each x predictor
  R2 = zbeta %*% matrix( YcorX , ncol=1 )
  #-----------------------------------------------------------------------------
  return( "R2"=R2)
}

################################################################################
#                    8. Bayesian Effect sizes                                  #
################################################################################
#This function calculates the proportion above specific values.
fncBayesEffectSize <- function( Coda.Object=NULL, Distribution=NULL, 
                                yVal1=NULL, yVal2=NULL, CenTend=NULL ) {
  #Convert into a matrix
  MC.Matrix <- as.matrix(Coda.Object, chains=TRUE)    
  
  ###########################
  ## Calculate effect size ##
  ###########################
  
  ##########
  ## Beta ##
  ##########
  if(Distribution == "Beta") {
    as1 <- (asin(sign(MC.Matrix[, yVal1]) * sqrt(abs(MC.Matrix[, yVal1]))))*2  
    as2 <- (asin(sign(MC.Matrix[, yVal2]) * sqrt(abs(MC.Matrix[, yVal2]))))*2  
    Effect.Size.Output <- abs(as1 - as2 )
  }
  
  return("Effect.Size.Posterior"=Effect.Size.Output )
}

##########################################
## DBDA Functions for Bayesian analysis ##
##########################################

## Need summarizePost and HDIofMCMC
########################
## DBDA function code ##
########################
summarizePost = function( paramSampleVec , 
                          compVal=NULL , ROPE=NULL , credMass=0.95 ) {
  meanParam = mean( paramSampleVec )
  medianParam = median( paramSampleVec )
  dres = density( paramSampleVec )
  modeParam = dres$x[which.max(dres$y)]
  mcmcEffSz = round( effectiveSize( paramSampleVec ) , 1 )
  names(mcmcEffSz) = NULL
  hdiLim = HDIofMCMC( paramSampleVec , credMass=credMass )
  if ( !is.null(compVal) ) {
    pcgtCompVal = ( 100 * sum( paramSampleVec > compVal ) 
                    / length( paramSampleVec ) )
  } else {
    compVal=NA
    pcgtCompVal=NA
  }
  if ( !is.null(ROPE) ) {
    pcltRope = ( 100 * sum( paramSampleVec < ROPE[1] ) 
                 / length( paramSampleVec ) )
    pcgtRope = ( 100 * sum( paramSampleVec > ROPE[2] ) 
                 / length( paramSampleVec ) )
    pcinRope = 100-(pcltRope+pcgtRope)
  } else { 
    ROPE = c(NA,NA)
    pcltRope=NA 
    pcgtRope=NA 
    pcinRope=NA 
  }  
  return( c( Mean=meanParam , Median=medianParam , Mode=modeParam , 
             ESS=mcmcEffSz ,
             HDImass=credMass , HDIlow=hdiLim[1] , HDIhigh=hdiLim[2] , 
             CompVal=compVal , PcntGtCompVal=pcgtCompVal , 
             ROPElow=ROPE[1] , ROPEhigh=ROPE[2] ,
             PcntLtROPE=pcltRope , PcntInROPE=pcinRope , PcntGtROPE=pcgtRope ) )
}

########################
## DBDA function code ##
########################
HDIofMCMC = function( sampleVec , credMass=0.95 ) {
  # Computes highest density interval from a sample of representative values,
  #   estimated as shortest credible interval.
  # Arguments:
  #   sampleVec
  #     is a vector of representative values from a probability distribution.
  #   credMass
  #     is a scalar between 0 and 1, indicating the mass within the credible
  #     interval that is to be estimated.
  # Value:
  #   HDIlim is a vector containing the limits of the HDI
  sortedPts = sort( sampleVec )
  ciIdxInc = ceiling( credMass * length( sortedPts ) )
  nCIs = length( sortedPts ) - ciIdxInc
  ciWidth = rep( 0 , nCIs )
  for ( i in 1:nCIs ) {
    ciWidth[ i ] = sortedPts[ i + ciIdxInc ] - sortedPts[ i ]
  }
  HDImin = sortedPts[ which.min( ciWidth ) ]
  HDImax = sortedPts[ which.min( ciWidth ) + ciIdxInc ]
  HDIlim = c( HDImin , HDImax )
  return( HDIlim )
}

#######################
## Chain Diagnostics ##
#######################
diagMCMC <- function( codaObject , parName=varnames(codaObject)[1] ,
                     saveName=NULL , saveType="jpg" ) {
  DBDAplColors = c("skyblue","black","royalblue","steelblue")
#  openGraph(height=5,width=7)
  par( mar=0.5+c(3,4,1,0) , oma=0.1+c(0,0,2,0) , mgp=c(2.25,0.7,0) , 
       cex.lab=1.75 )
  layout(matrix(1:4,nrow=2))
  # traceplot and gelman.plot are from CODA package:
  require(coda)
  coda::traceplot( codaObject[,c(parName)] , main="" , ylab="Param. Value" ,
                   col=DBDAplColors ) 
  tryVal = try(
    coda::gelman.plot( codaObject[,c(parName)] , main="" , auto.layout=FALSE , 
                       col=DBDAplColors, autoburnin=FALSE )  #This is the fix so I will always get shrink factor
  )  
  # if it runs, gelman.plot returns a list with finite shrink values:
  if ( class(tryVal)=="try-error" ) {
    plot.new() 
    print(paste0("Warning: coda::gelman.plot fails for ",parName))
  } else { 
    if ( class(tryVal)=="list" & !is.finite(tryVal$shrink[1]) ) {
      plot.new() 
      print(paste0("Warning: coda::gelman.plot fails for ",parName))
    }
  }
  DbdaAcfPlot(codaObject,parName,plColors=DBDAplColors)
  DbdaDensPlot(codaObject,parName,plColors=DBDAplColors)
  mtext( text=parName , outer=TRUE , adj=c(0.5,0.5) , cex=2.0 )
  if ( !is.null(saveName) ) {
    saveGraph( file=paste0(saveName,"Diag",parName), type=saveType)
  }
}

# Function(s) for plotting properties of mcmc coda objects.
DbdaAcfPlot <- function( codaObject , parName=varnames(codaObject)[1] , plColors=NULL ) {
  if ( all( parName != varnames(codaObject) ) ) { 
    stop("parName must be a column name of coda object")
  }
  nChain = length(codaObject)
  if ( is.null(plColors) ) plColors=1:nChain
  xMat = NULL
  yMat = NULL
  for ( cIdx in 1:nChain ) {
    acfInfo = acf(codaObject[,c(parName)][[cIdx]],plot=FALSE) 
    xMat = cbind(xMat,acfInfo$lag)
    yMat = cbind(yMat,acfInfo$acf)
  }
  matplot( xMat , yMat , type="o" , pch=20 , col=plColors , ylim=c(0,1) ,
           main="" , xlab="Lag" , ylab="Autocorrelation" )
  abline(h=0,lty="dashed")
  EffChnLngth = effectiveSize(codaObject[,c(parName)])
  text( x=max(xMat) , y=max(yMat) , adj=c(1.0,1.0) , cex=1.5 ,
        labels=paste("ESS =",round(EffChnLngth,1)) )
}

DbdaDensPlot <- function( codaObject , parName=varnames(codaObject)[1] , plColors=NULL ) {
  if ( all( parName != varnames(codaObject) ) ) { 
    stop("parName must be a column name of coda object")
  }
  nChain = length(codaObject) # or nchain(codaObject)
  if ( is.null(plColors) ) plColors=1:nChain
  xMat = NULL
  yMat = NULL
  hdiLims = NULL
  for ( cIdx in 1:nChain ) {
    densInfo = density(codaObject[,c(parName)][[cIdx]]) 
    xMat = cbind(xMat,densInfo$x)
    yMat = cbind(yMat,densInfo$y)
    hdiLims = cbind(hdiLims,HDIofMCMC(codaObject[,c(parName)][[cIdx]]))
  }
  matplot( xMat , yMat , type="l" , col=plColors , 
           main="" , xlab="Param. Value" , ylab="Density" )
  abline(h=0)
  points( hdiLims[1,] , rep(0,nChain) , col=plColors , pch="|" )
  points( hdiLims[2,] , rep(0,nChain) , col=plColors , pch="|" )
  text( mean(hdiLims) , 0 , "95% HDI" , adj=c(0.5,-0.2) )
  EffChnLngth = effectiveSize(codaObject[,c(parName)])
  MCSE = sd(as.matrix(codaObject[,c(parName)]))/sqrt(EffChnLngth) 
  text( max(xMat) , max(yMat) , adj=c(1.0,1.0) , cex=1.5 ,
        paste("MCSE =\n",signif(MCSE,3)) )
}

#############################
## Posterior distributions ##
#############################
plotPost = function( paramSampleVec , cenTend=c("mode","median","mean")[1] , 
                     compVal=NULL, ROPE=NULL, credMass=0.95, HDItextPlace=0.7, 
                     xlab=NULL , xlim=NULL , yaxt=NULL , ylab=NULL , 
                     main=NULL , cex=NULL , cex.lab=NULL ,
                     col=NULL , border=NULL , showCurve=FALSE , breaks=NULL , 
                     ... ) {
  # Override defaults of hist function, if not specified by user:
  # (additional arguments "..." are passed to the hist function)
  if ( is.null(xlab) ) xlab="Param. Val."
  if ( is.null(cex.lab) ) cex.lab=1.5
  if ( is.null(cex) ) cex=1.4
  if ( is.null(xlim) ) xlim=range( c( compVal , ROPE , paramSampleVec ) )
  if ( is.null(main) ) main=""
  if ( is.null(yaxt) ) yaxt="n"
  if ( is.null(ylab) ) ylab=""
  if ( is.null(col) ) col="skyblue"
  if ( is.null(border) ) border="white"
  
  # convert coda object to matrix:
  if ( class(paramSampleVec) == "mcmc.list" ) {
    paramSampleVec = as.matrix(paramSampleVec)
  }
  
  summaryColNames = c("ESS","mean","median","mode",
                      "hdiMass","hdiLow","hdiHigh",
                      "compVal","pGtCompVal",
                      "ROPElow","ROPEhigh","pLtROPE","pInROPE","pGtROPE")
  postSummary = matrix( NA , nrow=1 , ncol=length(summaryColNames) , 
                        dimnames=list( c( xlab ) , summaryColNames ) )
  
  # require(coda) # for effectiveSize function
  postSummary[,"ESS"] = effectiveSize(paramSampleVec)
  
  postSummary[,"mean"] = mean(paramSampleVec)
  postSummary[,"median"] = median(paramSampleVec)
  mcmcDensity = density(paramSampleVec)
  postSummary[,"mode"] = mcmcDensity$x[which.max(mcmcDensity$y)]
  
  HDI = HDIofMCMC( paramSampleVec , credMass )
  postSummary[,"hdiMass"]=credMass
  postSummary[,"hdiLow"]=HDI[1]
  postSummary[,"hdiHigh"]=HDI[2]
  
  # Plot histogram.
  cvCol = "darkgreen"
  ropeCol = "darkred"
  if ( is.null(breaks) ) {
    if ( max(paramSampleVec) > min(paramSampleVec) ) {
      breaks = c( seq( from=min(paramSampleVec) , to=max(paramSampleVec) ,
                       by=(HDI[2]-HDI[1])/18 ) , max(paramSampleVec) )
    } else {
      breaks=c(min(paramSampleVec)-1.0E-6,max(paramSampleVec)+1.0E-6)
      border="skyblue"
    }
  }
  if ( !showCurve ) {
    par(xpd=NA)
    histinfo = hist( paramSampleVec , xlab=xlab , yaxt=yaxt , ylab=ylab ,
                     freq=F , border=border , col=col ,
                     xlim=xlim , main=main , cex=cex , cex.lab=cex.lab ,
                     breaks=breaks , ... )
  }
  if ( showCurve ) {
    par(xpd=NA)
    histinfo = hist( paramSampleVec , plot=F )
    densCurve = density( paramSampleVec , adjust=2 )
    plot( densCurve$x , densCurve$y , type="l" , lwd=5 , col=col , bty="n" ,
          xlim=xlim , xlab=xlab , yaxt=yaxt , ylab=ylab ,
          main=main , cex=cex , cex.lab=cex.lab , ... )
  }
  cenTendHt = 0.9*max(histinfo$density)
  cvHt = 0.7*max(histinfo$density)
  ROPEtextHt = 0.55*max(histinfo$density)
  # Display central tendency:
  mn = mean(paramSampleVec)
  med = median(paramSampleVec)
  mcmcDensity = density(paramSampleVec)
  mo = mcmcDensity$x[which.max(mcmcDensity$y)]
  if ( cenTend=="mode" ){ 
    text( mo , cenTendHt ,
          bquote(mode==.(signif(mo,3))) , adj=c(.5,0) , cex=cex )
  }
  if ( cenTend=="median" ){ 
    text( med , cenTendHt ,
          bquote(median==.(signif(med,3))) , adj=c(.5,0) , cex=cex , col=cvCol )
  }
  if ( cenTend=="mean" ){ 
    text( mn , cenTendHt ,
          bquote(mean==.(signif(mn,3))) , adj=c(.5,0) , cex=cex )
  }
  # Display the comparison value.
  if ( !is.null( compVal ) ) {
    pGtCompVal = sum( paramSampleVec > compVal ) / length( paramSampleVec ) 
    pLtCompVal = 1 - pGtCompVal
    lines( c(compVal,compVal) , c(0.96*cvHt,0) , 
           lty="dotted" , lwd=2 , col=cvCol )
    text( compVal , cvHt ,
          bquote( .(round(100*pLtCompVal,1)) * "% < " *
                    .(signif(compVal,3)) * " < " * 
                    .(round(100*pGtCompVal,1)) * "%" ) ,
          adj=c(pLtCompVal,0) , cex=0.8*cex , col=cvCol )
    postSummary[,"compVal"] = compVal
    postSummary[,"pGtCompVal"] = pGtCompVal
  }
  # Display the ROPE.
  if ( !is.null( ROPE ) ) {
    pInROPE = ( sum( paramSampleVec > ROPE[1] & paramSampleVec < ROPE[2] )
                / length( paramSampleVec ) )
    pGtROPE = ( sum( paramSampleVec >= ROPE[2] ) / length( paramSampleVec ) )
    pLtROPE = ( sum( paramSampleVec <= ROPE[1] ) / length( paramSampleVec ) )
    lines( c(ROPE[1],ROPE[1]) , c(0.96*ROPEtextHt,0) , lty="dotted" , lwd=2 ,
           col=ropeCol )
    lines( c(ROPE[2],ROPE[2]) , c(0.96*ROPEtextHt,0) , lty="dotted" , lwd=2 ,
           col=ropeCol)
    text( mean(ROPE) , ROPEtextHt ,
          bquote( .(round(100*pLtROPE,1)) * "% < " * .(ROPE[1]) * " < " * 
                    .(round(100*pInROPE,1)) * "% < " * .(ROPE[2]) * " < " * 
                    .(round(100*pGtROPE,1)) * "%" ) ,
          adj=c(pLtROPE+.5*pInROPE,0) , cex=1 , col=ropeCol )
    
    postSummary[,"ROPElow"]=ROPE[1] 
    postSummary[,"ROPEhigh"]=ROPE[2] 
    postSummary[,"pLtROPE"]=pLtROPE
    postSummary[,"pInROPE"]=pInROPE
    postSummary[,"pGtROPE"]=pGtROPE
  }
  # Display the HDI.
  lines( HDI , c(0,0) , lwd=4 , lend=1 )
  text( mean(HDI) , 0 , bquote(.(100*credMass) * "% HDI" ) ,
        adj=c(.5,-1.7) , cex=cex )
  text( HDI[1] , 0 , bquote(.(signif(HDI[1],3))) ,
        adj=c(HDItextPlace,-0.5) , cex=cex )
  text( HDI[2] , 0 , bquote(.(signif(HDI[2],3))) ,
        adj=c(1.0-HDItextPlace,-0.5) , cex=cex )
  par(xpd=F)
  #
  return( postSummary )
}

#------------------------------------------------------------------------------

## End of Bayesian section ##
################################################################################

################################################################################
## Testing section: Begin  ##
################################################################################
#output$testplot1 <- renderPlot({ 
##  plot(values$a, values$b)
##} )

#output$test1 <- renderPrint({
#list(  mdl_off_set_output() )
#  }) 
 
################################################################################
## Testing section: End ##
################################################################################

  })   #This is the last line of code that closes out the entire server file
  
 