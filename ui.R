library(shiny)
library(jsonlite)


#The code below allows me to print 2 plots in the same panel.   
  shinyUI(
    fluidPage( 
      
        titlePanel(title= "Predictive Modeling and Global Sensitivity Analysis"),

        sidebarLayout(position= "right",
                      sidebarPanel(
                      ),       
      
      mainPanel(
        tabsetPanel(                                      #Creates multiple tabs.
          tabPanel("Data", 
                   h4("Download, Upload and Save Data."),
                   br(),
                   h5("Upload R Data."),
                   ###
          uiOutput("upload_r_df"),
          verbatimTextOutput("datastr"),
          br(),
          h5("Upload a text file."),
          uiOutput("text_file_type"),
          uiOutput("upload_df"),
          verbatimTextOutput("datastr_txt"),
          h6("Determines if the text file is loaded as model builder data. If yes, text file data is automatically loaded to the model builder tab although mtcars is listed."),
          uiOutput("use_txt"),
          #uiOutput("data"),
          br(),
          h5("Save the uploaded data frame into an R data file"),
          h6("This is the new name given to the uploaded text or R data frame you will 'save as' in the new Rdata file"),
          uiOutput("new_df_name"),
          h6("Answering yes will download the uploaded data into an RData file"),
          uiOutput("new_df"),
#          h6("Dataset object name that I'm requesting is saved in the Rdata file"),
#          uiOutput("df_save_nm"),
          #uiOutput("downloadSave_txt"),
          h6("This is the download button text...click on this to save the new file into Rdata"),
          downloadLink('downloadSave', 'Download RData file'),
          h6("Newly created data summary"),
          verbatimTextOutput("new_smry_df"),
br(),
  ############### Begin here
h4("Modify the Data Frame"),
br(),
h5("Change the data type"),
h6("Convert a variable to a character, factor, or numeric type."),

fluidRow(
  column(3,
         uiOutput("modify_character")),  
  column(3, offset=1,
         uiOutput("modify_factor")),  
  column(3, offset=1,
         uiOutput("modify_numeric"))  
  ),
br(),
h6("Subset the data you need. In #5, 'subset' indicates rows, 'select' indicates columns (e.g., subset= gender==male, select= 1:7)."),
fluidRow(
  column(3,
         uiOutput("subset_df_yes_no")),  
  column(3, offset=1,
         uiOutput("subset_args")),
  column(3, offset=1,
         uiOutput("modify_df_yes_no")) 
),
br(),
fluidRow(
  column(3,
         uiOutput("modified_df_save")),  
  column(3, offset=1,
         uiOutput("modified_df_name")),  
  column(3, offset=1,
         downloadLink('download_modified_df', '9. Click to download data.'))
),

################# End here

          ## Transformed/Imputed ##
          br(),
          br(),
          h4("Transformed and Imputed data"),
          br(),
          h6("DF name for the new Rdata with transformed/imputed data"),
          uiOutput("new_df_name_all"),
#          h6("Dataset object name of transformed/imputed data used to save as in Rdata file"),
#          uiOutput("df_save_nm_all"),
          h6("Answering yes will merge the original data with transformed/imputed/factor data"),
          uiOutput("new_df_all"),
          downloadLink('downloadSaveAll', 'Download transformed/imputed RData'),
          
          ## Factor scores ##
          br(),
          br(),
          h4("Factor Scores (with transformed and imputed data)"),
          br(),
          h6("DF name for the new Rdata with transformed/imputed/factor data"),
          uiOutput("new_df_name_all_fs"),
 #         h6("Dataset object name of transformed/imputed/factor data that to save as in Rdata file"),
#          uiOutput("df_save_nm_all_fs"),
          h6("Answering yes will merge the original data with transformed/imputed/factor data"),
          uiOutput("new_df_all_fs"),
          downloadLink('downloadSaveAllFs', 'Download factor scores (trans/imputed) RData'),
          h4(" ")
          ),
          ###
                    
          tabPanel("Model builder" ,
                   fluidRow(
                     column(3,
                   textInput("dataframe", "1. Enter the data frame name",   #Creates input "varY"
                             value="mtcars"),                            #Default is mtcars data.
                   br(),                                                 
                   
                   uiOutput("BeginModel"),  
                   br(),                   

                   uiOutput("vy"),  #vy is the created drop down box coming from renderUI in server.r.
                   br(),                   

                   uiOutput("vx"),  #vx is the created drop down box coming from renderUI in server.r.
                   br()
                   
                     ),
                   
                   column(3,  offset=1,
                   uiOutput("reg_typ"),  #vx is the created drop down box coming from renderUI in server.r.
                   br(),
                   
                   uiOutput("quant_tau"),  #Selects the percentile to use in quantile regression.
                   br(),
                   
                   uiOutput("censoring"),  #vx is the created drop down box coming from renderUI in server.r.
                   br(),

                   uiOutput("clustering"),  #vx is the created drop down box coming from renderUI in server.r.
                   br()
                   ),
                   
                   column(4, offset=1, 
                          uiOutput("rcs_yes"),  #rcs_yes is the created drop down box coming from renderUI in server.r.
                          br(),                   
                          
                          uiOutput("rx"),  #vx is the created drop down box coming from renderUI in server.r.
                          br(),
                          
                          uiOutput("update_yes"),  #vx is the created drop down box coming from renderUI in server.r.
                          br(),
                          
                          uiOutput("uf"),  #vx is the created drop down box coming from renderUI in server.r.
                          br() 
                          #, #Taking out these 3 lines because I will have a PRED tab
#                           selectInput("dmdfyhat", "13. Do you want to download the predictions (yhat: raw/logit)?", 
#                                      choices = c("No", "Yes"), multiple=FALSE, selected="No")     
                   )
                   ),
                   
                   

                   h4("Regression results"),
                   verbatimTextOutput("regress"),  #"regress" calls output$regress from server.r 
                   br(),
                   h4("Regression equation"),
                   h6("Note: No equation given for a null multistate model (e.g., time ~ strat(transition)."),
                   verbatimTextOutput("regress_equation"),  #"Regression formula 
                   br(),
                   h4("Model specs"),
                   verbatimTextOutput("specifications"),  #"regress" calls output$regress from server.r 
                   h6("Specs gives these values:"),
                   h6("1. Low:effect = 25%, 2. Adjust to = Median, 3. High:effect = 75%, 4. Low:prediction = 10%"),
                   h6("5. High:prediction = 90%, 6. Low=lowest value, and 7. High = Highest value."),                 
                   br(),
                   h4("Describe the outcome variable"),
                   verbatimTextOutput("desc_Y"),  #"regress" calls output$regress from server.r
                   plotOutput("outcome_hist"),
                   h5("Histogram of numeric variables (i.e., not factors)"),
                   br(),
h4("Describe the predicted values"),
verbatimTextOutput("desc_YhatHistRslt"),  #
                   plotOutput("y_hat_hist"),
                   h5("Linear predicted values (logit, exp(logit), or response level) and probabilities (Logistic regression).")

                   ),    

############## PREDs SECTION #############################
tabPanel("PREDs",                                #Creates a new panel named "Test"
         fluidRow(                           #Wrapping them in a fluidRow provides easy control over  
           h4("Download, Upload, Save and make Predictions from model fits."),
           br(),
           h5("Save model."),
           h5("Note: Save ANY file name with '.RData' at the end."),
           column(3,
           uiOutput("SaveModelFit"),
           br(),
           h5("Upload a model fit.")
           ),
           column(3, offset=1,
           uiOutput("mdl_fit_name")
           ),
           column(4, 
                  downloadLink('model', '3. Click to download the model.')
                  ),
           uiOutput("upload_model_fit"),
           verbatimTextOutput("mdl_print"),
           br(),
           h5("Upload a data frame for predictions."),
           uiOutput("upload_PRED_df"),
           verbatimTextOutput("PREDdatastr"),
           br(),
           h5("Get predicitions using the current model."),
           column(3,
                  uiOutput("curr_fit_df"),
                  br(),
                  h5("Get predicitions using a previous model.")
                  ), #1
           column(3,
                  uiOutput("fit_curr_mdl")
                  ), #2
           column(3,
                  uiOutput("curr_mdl_pred_df")
                  ), #3
           column(3,
                  downloadLink('pred_curr', '4. Click to download the PREDs.')
                  ),
           uiOutput("primary_fit"),
           column(3, 
                  uiOutput("new_fit_df")
                  ), #2
           column(3, 
                  uiOutput("fit_new_mdl")
                  ), #3
           column(3, 
                  uiOutput("new_mdl_pred_df") #4
           ), #1
           column(3, 
                  downloadLink('pred_new', '5. Click to download the PREDs.')
           ), #4
           
           column(12, 
                  br(),
                  h5("Summary of the selected model fit"),
                  verbatimTextOutput("prime_mdl_smry")
           ) 
           
         )),

############## End PREDs #############################

############## Describe and Missing #############################
tabPanel("Describe",
         fluidRow(   
           h4("Descriptives on means and missing values. Plot the target variable, stratifying by factors."),
           br(),
           h4("Explore the mean values of an outcome variable by factor levels."),
           column(3, 
                uiOutput("desc_y")),
           column(3, offset=1,
                  uiOutput("desc_x")),
           column(3, offset=1,
                  uiOutput("desc_choice"))
         ),
         plotOutput("DescSmryPlt", height = 800, width = 1200),          
         #plotOutput("DescSmryPlt", height = 700),          
           br(),
         fluidRow(
           h4("Explore the missingness of a variable by factor levels."),
           column(3, 
                  uiOutput("miss_y")),
           column(3, offset=1,
                  uiOutput("miss_x")),
           column(3, offset=1,
                  uiOutput("miss_choice"))
           ),
         br(),
         br(),
         h5("Proportion of missng values stratified by factors."),
         plotOutput("MissSmryPlt", height = 700), 
         br(),
         h5("Total proportion of missng values for each factor."),
         plotOutput("naPlt", height = 700), 
         h5("Missng values clustered by factors."),
         plotOutput("naDendo", height = 700), 
         h5("Recursive partitioning to predict missing values. When condition met, move to the left of the split. Otherwise, move right."),
         plotOutput("naTree", height = 700),
         h5("Logistic regression to predict missing values."),
         verbatimTextOutput("smry_lrm_miss"),
         br(),
         h5("ANOVA summary table of the logistic regression."),
         verbatimTextOutput("anova_lrm_miss"),
         br()                   
),    

          tabPanel("Reduce", 
                   h4("Data reduction: Redundancy, Cluster, and Principal Components Analysis"),
                   h4("with optional use of transformed and imputed predictor values."),
                   br(),
                   h4("Redundancy Analysis."),
                   uiOutput("redun_r2_lev"),
                   uiOutput("redun_choice"),
                   h6("Redundancy analysis results."),
                   h6("Note: Continuous variables with narrow distributions and few levels may prevent the analysis. Consider converting to a factor."),
                   verbatimTextOutput("redun_smry"),
                   h4("Hierarchical Cluster Analysis of predictors."),
                   uiOutput("clust_sim_matrix"),
                   h6("spearman= Squared Spearman correlation; pearson=pearson correlation; hoeffding= Hoeffding D statistic; bothpos= proportion of observations for which two variables are positive; ccbothpos=  chance-corrected bothpos."),
                   uiOutput("clust_choice"),
                   h6("Note: Higher values indicate stronger associations amongst predictors."),
                   h6("Speed: Pearson= fastest, Spearman= middle, Hoeffding D= slowest (caution in N > 25,000)."),
                   plotOutput("cluster_plot"),
                   h4("Transformation and Imputation of predictors."),
                   uiOutput("SIks"),
                   uiOutput("AsIs_x"),
                   h6("Select the variables that won't be transformed or won't have knots (e.g., continuous with < 5 unique values)."),
                   uiOutput("Transcan_Choice"),
                   h6("Summary of the trasnformation and imputation from the transcan() function."),
                   verbatimTextOutput("trans_smry"),
                   h6("Simultaneous transformation and single imputation of all predictors using transcan(). Imputed values are shown as red \"+\".  Transformed values are scaled to [0,1]."),
                   plotOutput("ptrans_plot"),
                   h4("Principal Components Analysis."),
                   uiOutput("mm_var_ls"),
                   uiOutput("pca_choice"), 
                   br(),
                   h5("Scree plot: Proportion of variance explained by each component using all predictors in the model."),
                   h5("Raw predictors (black) and transcan-transformed variables (red)."),
                   plotOutput("screeplot"),
                   h5("Build your factors: Select the predictors in each factor, consult the cluster analysis."),
                   uiOutput("factor_nmbr"),
                   uiOutput("sm_fac_ls1"),
                   uiOutput("sm_fac_ls2"),
                   uiOutput("sm_fac_ls3"),
                   uiOutput("sm_fac_ls4"),
                   uiOutput("sm_fac_ls5"),
                   uiOutput("sm_fac_ls6"),
                   uiOutput("sm_fac_ls7"),
                   uiOutput("sm_fac_ls8"),
                   uiOutput("sm_fac_ls9"),
                   uiOutput("sm_fac_ls10"),
                   h5("Summary of the Factor(s) (e.g., \"PC1\" = \"factor1\")."),
                   verbatimTextOutput("factor_score_output")
          ),    #Creates a new panel named "Summary"

tabPanel("Impute",  
         fluidRow(   
           h4("Modeling with Multiple Imputation (MI)."),
           h6("Note: After starting the multiple imputation, all graphs and tables will include MI results."),
           h6("However, the 'Calibration' and 'Validation' tab results use single imputation."),
           h6("To calculate Calibration/Validation stats, first go to \"Reduce\" tab and answer #1 & #2 under \"Transformation and Imputation of predictors\"."),
           h6("Single imputation is used because of the issue of random sampling of random sampling from multiple imputed datasets. Consider selecting all model X and Y when imputing."),
           h6("To update the 'Approximate' tab, answer 'Yes' to the '2. Begin modeling?' in the 'Model builder' after running the MI."),
           column(6, 
                  uiOutput("MIx"),
                  uiOutput("MIks")),
           column(6, 
                  uiOutput("MIn"),
                  uiOutput("MI_Begin")
                  )),
         h6("Select the number imputations equal to the percentage missing (e.g., 20% missing = 20 imputations)."),
         h6("Set the number of knots = 0 for a linear fit or if there are < 5 unique values in continous variables."),
         h4("Multiple Imputation summary"),
           verbatimTextOutput("MI_smry")
         ),

          tabPanel("Modify", 
                   h4("Fast backwards step-down regression, AIC based suggestions"),
                   verbatimTextOutput("mod_ify")),    #Creates a new panel named "Summary"
          tabPanel("Approximate",                       
                   h4("Approximate the model:"),
                   h4("Create a parsimonious model with an equivalent level of prediction as the \"Full Model\" ."  ),
                   uiOutput("MIForAprx"),
                   h5("If you will answer \"Yes\", also click \"Yes\" to #2 in the 'Model builder' tab before imputing and approximating if you want to compare the imputed/non-imputed results."  ),
                   plotOutput("approximate_plot"),                  #Creates a new panel named "Summary"
                   br(),
                   h5("Proportion of \"Full Model\" R2 remaining after deletion of each predictor. Select a stopping rule that makes approximation inadequate (e.g., 0.95)."),
                   h6("Note: There is nothing gained in parsimony when removing nonlinear terms, gains come from removing predictors."),
                   h6("Caution: Low N categories can bias a factor's rank due to R2's sensitivity to outliers, refer to the \"Importance\" tab when there are discrepancies."),
                   dataTableOutput("approximate_print"),
                   column(6, 
                   uiOutput("aprx_uf"),
                   uiOutput("aprx_mdl_yes")
                   ),
                   column(5, offset=1, 
                          uiOutput("aprx_mdl_fit_name"),
                          downloadLink('aprox_model', '4. Click to download the model.')
                          ),
                   column(12,
                          h5("Summary of the approximate model"),
                          verbatimTextOutput("apprx_fit_print")
                   )

          ),    
          tabPanel("Summary",                       
                   h4("Interpret effects"),
                   h4("Shows changes between the 25th and 75th percentiles, 0-to-1, or mode to other categories."  ),
                   plotOutput("dis", height=600),                  #Creates a new panel named "Summary"
                   br(),
                   h6("Warning: Factors with 4 or more categories can't be plotted in a single plot."),
                   h6("Consider using \"Partial PREDS\" tab to assess factors with many levels."),
                   uiOutput("sum_one_yes"),  #vx is the created drop down box coming from renderUI in server.r.
                   br(),
                   uiOutput("sum_one_x"),  #vx is the created drop down box coming from renderUI in server.r.
                   br(),
                   h4("Summary of model effects"),                   
                   h6("Note: Values use quantiles from the predictor's distribution:"),
                   h6("1. Low: 25th percentile, 2. High: 75th percentile, 3. Diff = 75th - 25th, 
                      4. Effect = Diff * coefficient"),
                   verbatimTextOutput("predictor_smry")
          ),    
          
          tabPanel("Importance", 
                   h4("Plot of predictor importance"),
                   plotOutput("p_anova", height=700, width="100%"),
                   br(),
                   h4("ANOVA summary of model"),
                   verbatimTextOutput("anova_smry")
                   ),    #Creates a new panel named "Summary"
          tabPanel("Partial PREDS", 
                   h4("Partial predictions plot"),
                   h4("The model can be described using partial effect plots by plotting each X against Y holding other predictions constant (e.g., Median)."),                   
                   plotOutput("prt_prd", height=600),
                   br(),
                   
                   fluidRow(
                     column(3, 
                            uiOutput("prt_one_yes")),
                     column(3, offset=1, 
                            uiOutput("prt_one_x"))
                   ),
                   br(),
                   
                   h4("Partial prediction of a continuous predictor by a factor."),
                    h5("This plot shows the expected trend line by multiple levels with 95% confidence intervals. Especially helpful in seeing the interaction effect and where lines intersect or diverge."),  
                   br(),
                   fluidRow(
                     column(3, 
                            uiOutput("xyplot_x")),
                     column(3, 
                            uiOutput("xyplot_z")),
                     column(3, 
                            uiOutput("xyplot_bands")),
                     column(3, 
                            uiOutput("xyplot_yes_no"))
                   ),
                   br(),
                   plotOutput("xYplot_interaction", height=700, width="100%"),
                   br(),
h4("Contrast plots"),
h5("This graph shows differences between groups (linear differences, odds ratios, hazard ratios) with 95% confidence intervals. Compare 2 groups on predicted values, especially useful for interactions."),                   
h5("This plot compliments the partial prediction plot above. You must run the plot directly above first."),                   
fluidRow(
  column(3,
         uiOutput("xyplot_con_lev1")),
  column(3,
         uiOutput("xyplot_con_lev2"))
),
fluidRow(
  column(3,
         uiOutput("xyplot_con_ylim0")),
  column(3,
         uiOutput("xyplot_con_ylim1")),
  column(3,
         uiOutput("xyp_yes_no"))
),
h5("Contrasts require 'factor' data types for groups. Convert variables into factors in the \"Data\" tab."),                   
br(),
plotOutput("xyplot_contrast_plot", height=700, width="100%"),
h6("The portions of the red horizontal line (e.g., at 0 for linear regression, at 1 for logistic regression and Cox PH) corresponds to no significant predictor effect when contained within the 95% CI."),
h6("The blue line represents the contrast level you selected (e.g., 'Female')."),
h6("For example, women may have higher rates of death before age 58, equal with men from 58-88, and have lower rates after 88 years."),
br(),
h5("Contrasts and 95% confidence intervals from the plot above at various percentiles of the continuous predictor. Non-interaction contrasts will be constant."),
tableOutput("contrast_quant_table"),
br()
                   ),    #Creates a new panel named "Summary"
          tabPanel("Nomogram",
                   h4("Hand calculate probabilities with a nomogram"),
                   h4("We use a nomogram that converts each effect in the model to a 0 to 100 scale."),
                   plotOutput("nomo_gram", height = 600),
                   br(),
                   uiOutput("nomo_one_yes"),  #vx is the created drop down box coming from renderUI in server.r.
                   br(),
                   uiOutput("nomo_one_x"),  #vx is the created drop down box coming from renderUI in server.r.
                   br()                   
          ),    #Creates a new panel named "Nomogram"                           
          
          tabPanel("Calibration",
                   fluidRow(
                   column(3, 
                   uiOutput("calibrate_type"),
                   uiOutput("MIForCali")
                   ),  
                   column(3, offset=1,
                   uiOutput("calibrate_B_arg_n"),
                   uiOutput("BeginCalibrate")
                   )),
                   h5("Note: Prior to validation, create the single imputed data under 'Transformation and Imputation of predictors' in the 'Reduce' tab for multiple imputation purposes."),
                   #h6("Note: Confirm the single imputed or original dataframe is loaded in the 'Model builder' tab, depending on your purpose."),
                   br(),
                   h4("Model calibration (e.g, bootstrapped, cross-validated--repeated 100 times)"),
                   h4("The reliability of a model, meaning the ability of the model to predict future observations as well as it appeared to predict the responses at hand."),
                   plotOutput("cali_brate", height = 600)
                   ),    #Creates a new panel named "Summary"
                       
          tabPanel("Validation", 
                   fluidRow(
                     column(3, 
                            uiOutput("validate_type"),
                            uiOutput("MIForVali")
                     ),  
                     column(3, offset=1,
                            uiOutput("validate_B_arg_n"),
                            uiOutput("BeginValidate")
                     )),
                   h5("Note: Prior to validation, create the single imputed data under 'Transformation and Imputation of predictors' in the 'Reduce' tab for multiple imputation purposes."),
                   #h6("Note: Confirm the single imputed or original dataframe is loaded in the 'Model builder' tab, depending on your purpose."),
                   br(),
                   h4("Calibration and discrimination indexes (from bootstrapped methods, cross-validated--100 repeats)"),
                   h4("The worth of a model can be judged by how far it goes out on a limb while 
                      still maintaining good calibration. If one model assigns .02 of 
                      the sample to a risk of dying above a predicted value of .9 while 
                      the other model assigns .08 of the sample to the high risk group, 
                      the second model is more discriminating."),
                   h6("Note: Use the \"boot\" method to view the \"Factors Retained in Backwards Elimination\". These are factors found statistically significant in random draws. First 50 bootstrapped results shown."), 
                   verbatimTextOutput("vali_date")),    #Creates a new panel named "Summary"

#############
tabPanel("Survival", 
         h4("Survival analysis plots and residuals"),
         h5("Note: Use splines fits and interactions to test linearity and additivity. Use splines in place of Martingales when considering continuous X tranfsormations."),                   
         br(),
         h4("Survival plots"),                   
         h5("Note: Stratified survival function curves don't include time increment and confidence bands/bars options."),
         h5("The log-minus-log plot is not applicable to the survival and hazard function plots. The log-minus-log plot can have negative values, adjust y limit values accordingly."),
         br(),
         h5("Set up the survival plot."),
         fluidRow(
           column(3, 
                  uiOutput("srv_plt_one_x")),
           column(3,
                  uiOutput("srv_plt_lvl")),
           column(3,
                  uiOutput("SurvPltBands")),
         column(3,
                uiOutput("SurvPltRun"))
),
h5("Modify the survival plot display."),
fluidRow(
  column(3, 
         uiOutput("SurvPltXlim1")),
  column(3,
         uiOutput("SurvPltXlim2")),
  column(3,
         uiOutput("SurvPltYlim1")),
  column(3,
         uiOutput("SurvPltYlim2"))
),
fluidRow(
  column(3, 
         uiOutput("SpTimeInc")),
  column(3,
         uiOutput("HazardPlot")),
  column(3,
         uiOutput("SrvLogLog"))
),
br(),
         plotOutput("surv_plot1", height = 800, width="100%"),
         br(),
############################## Begin here
h4("Kaplan-Meier survival and hazard plots (cumulative probability)"),                   
br(),
h5("Set up the Kaplan-Meier plot. Default is survival, select hazard option below."),
fluidRow(
  column(3, 
         uiOutput("km_srv_plt_one_x")),
  column(3,
         uiOutput("KMHazardPlot")),
  column(3,
         uiOutput("KMSurvPltRun")),
  column(3,
         uiOutput("KmRestrictMean"))
),
h5("Modify the plot display."),
fluidRow(
  column(3, 
         uiOutput("KMSurvPltXlim1")),
  column(3,
         uiOutput("KMSurvPltXlim2"))
),
fluidRow(
  column(3, 
         uiOutput("KMSurvPltYlim1")),
  column(3,
         uiOutput("KMSurvPltYlim2"))
),
br(),
plotOutput("km_plot", height = 700, width="100%"),
br(),
h5("Unconditional survival function. Associated with default survival plot, first plot at the top. Modify restricted mean with KM button #4 directly above."),
verbatimTextOutput("KM_SF_Output_UC"), 
br(),
h5("Stratified survival function. Kaplan-Meier plot above. Modify restricted mean with KM button #4 directly above."),
verbatimTextOutput("KM_SF_Output_C"), 
br(),
############################## End here

         h4("Schoenfeld residuals"),
         h5("Assess the Cox model's proportional hazards assumption."),
         br(),
         h5("Schoenfeld residuals test."),
         tableOutput("schoenfeld_test"),
         h5("Schoenfeld residuals plot."),
h6("Check for a pattern in time. An upwards line may suggest an increasing effect in X over time. For binary predictors (with no interaction term), the top band of dots is failures when X=1, lower band is when X=0"),
uiOutput("Schoenfeld_X"),
         plotOutput("schoenfeld_plt", height = 800, width="100%"),
br(),
## Creating time-dependent datasets ##
h4("Time-dependent dataset creation for a time-dependent coefficient model."),
br(),
h5("This creates a time-dependent dataset and interaction term (X*Time) to model the proportional hazard assumption."),
h5("When specifying the time-dependent model later in the 'Model builder' tab, use Outcome=tstart, Censor= tstop,event."),
h5("The default time period cutoff is 1 unit periods, 1:max(Y), e.g., 1:365. Enter interval/step periods as a vector, e.g., c(30,60)."),
h5("We may want to add a value to the time value to emphasize parts of the period (e.g., log(time+20) ). If so, set the value below."),
fluidRow(
         column(3, 
                uiOutput("time_dependent_predictors")) ,
         column(3,
                uiOutput("td_cut_lev")) ,
         column(3,
                uiOutput("time_dependent_var")) ,
         column(3, 
                uiOutput("td_log_increment")) 
), 
br(),
fluidRow(
  column(3, 
         uiOutput("td_data_yes_no")) ,
  column(3, offset=1,
         uiOutput("td_data_download_name")) ,
    column(3, offset=1,
         downloadLink('td_data_download', '7. Click to download data.'))
),
br(),
h5("Remember to save R data frames with the extension '.RData'."),

############################################### END HERE

br(),
h4("DFBETAS residuals"),
h5("DFBETAS allows us to identify influential observations. The cutoff level is an absolute value and indicates a change in the coefficient by standard error (e.g., 0.2)."),
fluidRow(
  column(5, 
         uiOutput("U_Cutoff_Lvl")),
  column(5,
         uiOutput("DFBETASYesNo"))
),
         uiOutput("dfbetas_influence"),
         br(),
         h5("These are the observations that have any influential values. * indicates influential values."),

         verbatimTextOutput("InfluenceDFBETAS"), 
         br(),
h5("The DFBETAS, influential cases, Schoenfeld and Martingale residuals are saved in the download."),
fluidRow(
  column(3, 
         uiOutput("SaveDFBETAS")),
  column(3, offset=1,
         uiOutput("dfbetas_fit_name")),
  column(3, offset=1,
  downloadLink('dfbetas_influential_residuals', '3. Click to download residuals.'))
),
h5("Remember to save R data frames with the extension '.RData'."),
br(),
         h4("Mixed effects Cox proportional hazards model"),
         h5("Answer 'Yes' for the cost analysis to get the frailties and graph in the correct direction."),
         h5("Note: If you updated the model formula, the mixed effects model won't run when there are spline terms (e.g., rcs(X,5))."),

         fluidRow(
           column(5, 
                  uiOutput("CoxLev2")),
           column(5, offset=1,
                  uiOutput("coxme_yes")),
           column(5, offset=1,
                  uiOutput("coxme_cost_yes"))
           
         ),
h4("Regression results"),
         verbatimTextOutput("efit1_out"),
br(),
h4("Assess between-group variance"),
verbatimTextOutput("efit1_tests"),
h6("1. Random effects SD and additional risk: The square root of the between group variance. Groups at 1 SD have an additional risk of exp(sd). E.g., exp(0.644)= 1.90= 90% more risk."),
h6("2. Random effects LRT: This tests the significance of adding a between-group term to the fixed effects model."),
h6("3. Cox & Snell pseudo R2: A discrimination index."),
h6("4. Intraclass correlation: The level of similarity within groups or the level of differences between groups."),
h6("5. Median hazard ratio: Additional median risk associated with observing the outcome for two patients from different groups."), 
h6("E.g., 96% increased median risk of getting an infection due to the variation of care across medical centers."), 
h6("Magnitude of median differences attributed to group differences. E.g., when 2 patients with"),
h6("the same covariate values but in different hospitals are randomly sampled, the hazard ratio comparing the patient"),
h6("with the larger hazard to the patient with the smaller hazard ratio will exceed 1.51 in 50% of the samples. If the"),
h6("MHR has the biggest effect, that means the center is the most important thing in the model for understanding the outcome."),
h6("6. Reduction in the between group variance: The proportion of the random effects variance from a null model (i.e., no covariates)"),
h6("reduced after fitting full model (i.e., has covariates). Includes the null model variance and SD."),
h6("Note: The reduction in variance can be negative if the impact of level 1 predictors was relatively higher than level 2 predictors."),
         br(),
         fluidRow(                           #Wrapping them in a fluidRow provides easy control over  
           h4("Download mixed effects cox model fit"),
           h5("Note: Save ANY file name with '.RData' at the end."),
           column(3,
                  uiOutput("SaveModelFitCme")
           ),
           column(3, offset=1,
                  uiOutput("cme_mdl_fit_name")
           ),
           column(3, offset=1,
                  downloadLink('cme_model', '3. Click to download the model.')
           )),
         br(),
h4("Random effects frailties"),
plotOutput("frail_plot1", height = 700, width="100%"),
#This gives the option of selecting abbrviation length
fluidRow(                           
  column(10, 
       uiOutput("abbr_length")
)),
h5("Random effects frailties sorted alphabetically by cluster name and numerically by frailty value."),
verbatimTextOutput("frail_output"),
br(),

),    

########################
## Multi-state models ##
########################
tabPanel("Multi-State", 
         h4("Multi-state survival analysis"),
         br(),
         h5("This tab allows one to create 1) a multi-state dataset, 2) Aalen-Johansen survival estimates, 3) probability-in-state-curves, 4) a restricted mean time-in-state Z test for 2 groups, 5) Cox type-specific regressions, and 6) state space diagrams."),
         h5("For an introduction to multi-state modeling, please see Terry Therneau's 2018 article: 'Application of multi-state models in cancer clinical trials'. Clinical Trials, 15(5), 489-498."),
         br(),
         ## Multi-state model ##
         h4("Create a multi-state model data frame."),
         h5("This section converts wide format data into long format and produces multiple variables such as start and stop times for various states."),
         h5("Important: One time variable (e.g., #3) that covers the full range is required (i.e., values of 1:max(Time)). The ID variable in #5 needs to be in sequential order (i.e., no gaps). The time variable created from #2, will take the same name."),
         fluidRow(
           column(3, 
                  uiOutput("st_Time_Trans")),
           column(3, offset=1,
                  uiOutput("st_Bin_Term")),
           column(3, offset=1,
                  uiOutput("st_Time_Term"))
         ),
         h5("#4 below calculates cumulative values for events. #6 merges in non-state variables that remain constant over time such as birth year and location."),
         fluidRow(
           column(3, 
                  uiOutput("st_Prior_Event")),
           column(3, offset=1,
                  uiOutput("st_Time_ID")),
           column(3, offset=1,
                  uiOutput("st_Time_I_V"))
         ),
         h5("Create and save multi-state data."),
         fluidRow(
           column(3,
                  uiOutput("make_multi_state_df")),
           column(3, offset=1,
                  uiOutput("ms_data_download_name")) ,
           column(3, offset=1,
                  downloadLink('ms_data_download', '9. Click to download data.'))
         ),
         br(),
         ## Load data  
         h4("All sections in steps 1-24 are dependent on preceeding steps. Complete relevant steps prior to subsequent steps (e.g., restricted time is optional)."),
         br(),
         h4("Load multi-state data frame and check build"),
         fluidRow(
           column(3, 
                  uiOutput("ms_df_input_name")),
           column(3, offset=1,
                  uiOutput("st_Time_Start")),
           column(3, offset=1,
                  uiOutput("st_Time_Stop"))
         ),
         fluidRow(
           column(3, 
                  uiOutput("st_Time_Event")),
           column(3, offset=1, 
                  uiOutput("st_Time_Srvchck_ID")),
           column(3, offset=1,
                  uiOutput("check_multi_state_df"))
         ),
         ## Survival check output ##
         verbatimTextOutput("survival_check_attr"),
         br(),
         h4("Set up 'Current Probability-in-State' Aalen-Johansen Survival Estimates"),
         fluidRow(
           column(3, 
                  uiOutput("prob_in_state_strata")),
           column(3, 
                  uiOutput("prob_in_state_strata_factor")),
           column(3, 
                  uiOutput("prob_in_state_rmean")),
           column(3, 
                  uiOutput("run_probability_in_state"))
         ),
         br(),
         ## Aalen-Johansen survival estimates of probability in state ##
         h5("Summary of restricted mean time in state. Default time = max(time) when not specified by user."), 
         verbatimTextOutput("print_prob_in_state"),
         h5("Added 95% CIs to survival package summary using the Normal Approximation method. More conservative than Z test, see discussion on influence matrix in package documentation."),
         br(),
         h4("Test the difference in mean Time-in-State of a 2 group strata (e.g., intervention and control)"),
         h5("Conducts a Z test between groups' mean time-in-state."),
         fluidRow(
           column(3, 
                  uiOutput("rmean_time_state_prime_level")),
           column(3, offset=1,
                  uiOutput("rmean_time_in_state_rmean")),
           column(3, offset=1,
                  uiOutput("run_time_in_state_z_test"))
         ),
         br(),
         verbatimTextOutput("print_prob_in_state_Z_test"),
         br(),
h4("Probability-in-State curves"),
h5("Uses the Aalen-Johansen survival estimates for unconditional and conditional (stratified) displays. Colors assigned to factors in the order they are listed in various output above."),
fluidRow(
  column(3, 
         uiOutput("prob_state_strata_plot_yesno")),
  column(3, 
         uiOutput("prob_state_curves_colors")),
  column(3, 
         uiOutput("prob_state_exclude_state")),
  column(3, 
         uiOutput("prob_state_time_legend"))
),
br(),
#X and Y limits
fluidRow(
  column(3, 
         uiOutput("prob_state_curve_Xlim1")),
  column(3, 
         uiOutput("prob_state_curve_Xlim2")),
  column(3, 
         uiOutput("prob_state_curve_Ylim1")),
  column(3, 
         uiOutput("prob_state_curve_Ylim2"))
),
br(),
#Colors
fluidRow(
  column(3, 
         uiOutput("prob_state_time_x_axis_vals")),
  column(3, 
         uiOutput("prob_state_time_x_axis_text")),
  column(3, 
         uiOutput("run_prob_state_curves"))
),
br(),
plotOutput("probability_in_state_plot", height = 800, width="100%"),
br(),
## Cox PH multi-state model ##
h4("Cox type specific multi-state model"),
h6("Conduct baseline models with consistent X across all transitions or transition specific models. Transition models allow for different, clinically relevant predictors in each transition."),
h5("First, enter the data name in the 'Load multi-state data frame and review build' section above."),
fluidRow(
  column(3, 
         uiOutput("cph_Time_Start")),
  column(3, offset=1,
         uiOutput("cph_Time_Stop")),
  column(3, offset=1,
         uiOutput("cph_Time_Event"))
),
br(),
h5("Update the baseline model formula in #6. Use 'strata' rather than 'strat' to stratify."),
fluidRow(
  column(3, 
         uiOutput("Cph_Time_Fmla_ID")),
  column(3, offset=1,
         uiOutput("M_S_v_x")),
  column(3, offset=1,
         uiOutput("MS_CPH_Uf"))
),
br(),
h5("Create transition specific models (e.g., '1:2') in #8."),
h6("Example code: coxph(list(Surv(start, stop, event) ~ age, 1:2 + 2:3 ~ age + sex), data=mgus1, id=id, model=TRUE)"),
fluidRow(
  column(3, 
         uiOutput("Update_MS_CPH_Yes")),
  column(3, offset=1,
         uiOutput("ms_Trn_Spc_Fmla")),
  column(3, offset=1,
         uiOutput("MS_Trns_Use_Yes"))
),
br(),
h5("Create and save a multi-state model."),
fluidRow(
  column(3,
         uiOutput("Begin_MS_Mdl")),
  column(3, offset=1,
         uiOutput("ms_Cph_Download_Nm")) ,
  column(3, offset=1,
         downloadLink('ms_model_download', '12. Click to download model.'))
),
br(),
h4("Regression results"),
h5("Positive logits indicate faster transition times into new states. Negative logits indicate slower transitions."),
verbatimTextOutput("ms_model_fit_out"),
br(),
## Schoenfeld residauls ##
h4("Schoenfeld residuals"),
h5("Assess the Cox model's proportional hazards assumption."),
br(),
fluidRow(
  column(3,
         uiOutput("Begin_MS_Schoenfeld_Res")),
  column(3, offset=1,
         uiOutput("MSSchoenfeld_X")) 
),
h5("Schoenfeld residuals test"),
h6("Schoenfeld residuals not run on a transition specific model entered into #8 above."),
tableOutput("MSschoenfeld_test"),
h5("Schoenfeld residuals plot"),
h6("Check for a pattern in time. An upwards line may suggest an increasing effect in X over time. For binary predictors (with no interaction term), the top band of dots is failures when X=1, lower band is when X=0"),
plotOutput("MSschoenfeld_plt", height = 800, width="100%"),
br(),
#Model summary for state space diagram
h4("Summarize output"),
h5("Create the model summary to retrieve the state space diagram values"),
h6("Select options for the coefficient of primary interest. Leaving #2 blank returns all transitions."),
fluidRow(
  column(3, 
         uiOutput("MS_Sum_X_Levs1")),
  column(3, offset=1,
         uiOutput("MS_Sum_Trns_Nms1")),
  column(3, offset=1,
         uiOutput("MS_Sum_Multi_Val1"))
),
h6("Select options for the coefficient of secondary interest. Leaving #5 blank returns all transitions."),
fluidRow(
  column(3, 
         uiOutput("MS_Sum_X_Levs2")),
  column(3, offset=1,
         uiOutput("MS_Sum_Trns_Nms2")),
  column(3, offset=1,
         uiOutput("MS_Sum_Multi_Val2"))
),
fluidRow(
  column(3, 
         uiOutput("Sum_2_X_Lev_Yes")),
  column(3, offset=1,
         uiOutput("Begin_MS_Sum_Yes"))
),
br(),
h4("Model summary of key transitions"),
verbatimTextOutput("ms_model_summary_out"),
br(),
## State space diagram, text values, and legend ##
h4("State space diagram"),
h5("After the state space diagram is created, point and left-click on the plot to automatically add transition coefficients to that spot."),
fluidRow(
  column(3, 
         uiOutput("State_Spc_Color")),
  column(3, offset=1,
         uiOutput("ms_St_Spc_Layout")),
  column(3, offset=1,
         uiOutput("MS_Mod_St_Spc_Layout"))
),
h5("After the first click, you will need to repeatedly close the pop-up matrix to add each text label. Edit the matrix to add curvature by entering values above/below 1."),
fluidRow(
  column(3, 
         uiOutput("St_Sp_Legend")),
  column(3, offset=1,
         uiOutput("Make_St_Spc_Diag"))
  ),
plotOutput("ms_state_space_plot", height = 800, width="100%", click="plot_click"),
  br()

#End of tab
),    


#############
tabPanel("Cost", 
         h4("Cost and continuous outcomes"),
         h4("The Cox PH, Ordinal Logistic, and Quantile regression models can be used on continuous Y. This section is primarily for cost models but can be used for any continuous Y. Includes stratification for treatments."),
         br(),
         h5("Dudley, R. & Harrell, F. (1993). 'Comparison of analytic models for estimating the effect of clinical factors on the cost of coronary artery bypass graft surgery'. Journal of Clinical Epidemiology, 46(3)."),
         br(),
         h5("The following 3 questions will set up the types of plots and tabled results."),                   
         h6("Note: If using the Cox PH model with censoring, censor a non-event (e.g., death = 0)."),                   
         h6("Note: Not all tables and plots are applicable to a quantile regression (e.g., means)."), 
         fluidRow(
           column(3, 
                  uiOutput("dens_plt1_typ")),
           column(3, offset=1,
                  uiOutput("dens_plt1_x")),
           column(3, offset=1,
                  uiOutput("run_dens_plt1"))
         ),
         br(),
         h5("Partial effects plots: Plot the mean or various quantile effects. For quantile regression, get individual plots in the 'Partial PREDs' tab."),
         plotOutput("cox_prt_prd", height = 700, width="100%"),
         br(),
fluidRow(
  h5("The following questions are to modify the partial effects plots above."),
  column(3, 
         uiOutput("CoxMnMed"),
         uiOutput("prt_one_cox_x")
         ),
  column(3, offset=1,
         uiOutput("cox_pct_lvl"),
         uiOutput("OneCoxXYes"))
  ),
         h5("Density plot on the outcome, with or without stratification."),
         plotOutput("plot_dens_plt1", height = 700, width="100%"),
         br(),
         h4("The plot and tables below are for stratified results."),
         br(),
         h5("We often estimate effects for the 'middle' of the data (e.g., mean), we can also estimate other parts of the data."),
         h5("These are 5 effects at various quantiles for the stratified variable (e.g., we estimate for most and least expensive costing patients)."),
         plotOutput("plot_quant_plt1", height = 700, width="100%"),
         br(),
         h5("Quantiles: Point estimates and confidence intervals of the effects from the 10th/25th/50th/75th/95th percentiles."),
         h5("Weighted average = ((0.25 * p10 Diff) + (0.25 * p25 Diff) + (0.25 * p50 Diff) + (0.15 * p75 Diff) + (0.10 * p90 Diff))."),
         tableOutput("quant_out1"),
         br(),
         h5("Mean: Point estimates and confidence intervals for the mean effect."),
         tableOutput("mean_out1"),
         br(),
         h5("Observed outcome means and quantiles."),
         tableOutput("obsdfmqt_out1"),
###################################### Begin here
#Cost plots for contrasts and interactions: 
#The letter C is added to the beginning of everything used in the partial PREDs tab
br(),
h4("Partial prediction of a continuous predictor by a factor."),
h5("This plot shows the expected trend line by multiple levels with 95% confidence intervals. Especially helpful in seeing the interaction effect and where lines intersect or diverge."),  
br(),
fluidRow(
  column(3, 
         uiOutput("Cxyplot_x")),
  column(3, 
         uiOutput("Cxyplot_z")),
  column(3, 
         uiOutput("Cxyplot_bands")),
  column(3, 
         uiOutput("Cxyplot_yes_no"))
),
br(),
plotOutput("CxYplot_interaction", height=600, width="100%"),
br(),
h4("Contrast plots"),
h5("This graph shows differences between groups (linear differences, odds ratios, hazard ratios) with 95% confidence intervals. Compare 2 groups on predicted values, especially useful for interactions."),                   
h5("This plot compliments the partial prediction plot above. You must run the plot directly above first."),                   
fluidRow(
  column(3,
         uiOutput("Cxyplot_con_lev1")),
  column(3,
         uiOutput("Cxyplot_con_lev2"))
),
fluidRow(
  column(3,
         uiOutput("Cxyplot_con_ylim0")),
  column(3,
         uiOutput("Cxyplot_con_ylim1")),
  column(3,
         uiOutput("Cxyp_yes_no"))
),
h5("Contrasts require 'factor' data types for groups. Convert variables into factors in the \"Data\" tab."),                   
br(),
plotOutput("Cxyplot_contrast_plot", height=600, width="100%"),
h6("The portions of the red horizontal line at 1 corresponds to no significant predictor effect when contained within the 95% CI."),
h6("The blue line represents the contrast level you selected (e.g., 'Female')."),
h6("For example, women may have higher rates of death before age 58, equal with men from 58-88, and have lower rates after 88 years."),
br(),
h5("Contrasts and 95% confidence intervals from the plot above at various percentiles of the continuous predictor. Non-interaction contrasts will be constant."),
tableOutput("Ccontrast_quant_table"), 
br()

###################################### End here

),    #Creates a new panel named "Summary"



tabPanel("Monte Carlo",                                #Creates a new panel named "Test"
         fluidRow(                           #Wrapping them in a fluidRow provides easy control over  
           h4(" Monte Carlo Simulation"),
           h6(" Note: Open this tab first to unlock the plots in the following tabs."),
           column(3,
           uiOutput("s_seed"),  #Sets the seed for the random number.
           uiOutput("up_mn_var"),
           uiOutput("up_mn_val"),
           uiOutput("update_mc_mn")  
           ),
           
           column(3,  offset=1,
           uiOutput("nsim"),  #These are all model predictors.
           uiOutput("up_sd_var"),
           uiOutput("up_sd_val"),
           uiOutput("update_mc_sd")
           ),
                      
           column(4, offset=1, 
           #Download the simulated data
           selectInput("dmcdf", "Do you want to download the simulated data?", 
                       choices = c("No", "Yes"), multiple=FALSE, selected="No"),     
           uiOutput("up_pdf_var"),
           uiOutput("up_pdf_val"),
           uiOutput("update_mc_pdf")
           )
           ),
         column(12,  
                #Update the model formula so I can modify it or do a brand new simulation
                uiOutput("update_rms_fnc")
         ),
         
         h5("Distribution types assigned to predictors. Modify: binomial (mean), normal (mean, SD), uniform (PDF)"),
         verbatimTextOutput("name_dist_type"),
         h4("Morris One-at-a-Time Sensitivity Analysis"),
           verbatimTextOutput("morris_oat"),
           h4("Monte Carlo Global Sensitivity Analysis"),
           verbatimTextOutput("mc_gsa")
         ),
         
tabPanel("OAT M/SD plot",                                #Creates a new panel named "Test Plot"
         fluidRow(                           #Wrapping them in a fluidRow provides easy control over  
           h4("Morris One-at-a-Time plot of Yhat means by Yhat SDs for each predictor (holding other predictors constant)"),
           h5("High means indicate high Elementary Effects. High SDs may indicate non-linearity and/or interactions."),
           h5("Red line= sample mean; Blue line= sample median. Lines not always visible."),
           plotOutput("oat_mn_sd", height = 600)   
         )),              

tabPanel("Tornado plot",                                #Creates a new panel named "Test Plot"
         fluidRow(                           #Wrapping them in a fluidRow provides easy control over  
           h4("Tornado plot of absolute value standardized regression coefficients."),
           h5("Wider bands indicate that the outcome is more sensitive to the predictor (i.e., it's more important)."),
           plotOutput("tornadoplot")   
         )),              

tabPanel("Cutoff plot",
         h4("This plot allows us to see the proportion of expected values above/below a cutoff"),
         fluidRow(
           column(5,
         uiOutput("cutoff_yes")
         ),
         column(5, 
                uiOutput("cutoff_val")
                )),
         plotOutput("cutoffplot"),
         verbatimTextOutput("cutoff_smry"),
         h5("Descibe the simulated values"),
         verbatimTextOutput("desc_YhatPlotRslt")
),    

tabPanel("Cobweb plot",                                #Creates a new panel named "Test Plot"
         fluidRow(                           #Wrapping them in a fluidRow provides easy control over  
           h4("Cobeweb plot of best 1% or 5% of predicted outcome scores (\"best\" can be top or bottom values.)"),
           h5("Shows responses of predictors with best predicted outcome values. Narrower spread indicates greater predictor importance."),
           h6("Note: Requires at least 2 predictors, 1 of which needs to be continuous. If not, produces error message."),
           uiOutput("top_bottom_5"),
           plotOutput("cobwebplot", height=600),
           h5("Response level names and \"approximate\" associated percentiles for binomial and categorical factors (in order)."),
           h5("Binomial and categorical percentiles are 1/N *100 (e.g., 3 levels= 1/3, 2/3, 3/3= 33, 68, 100)."),
           verbatimTextOutput("cobweb_lev_nm")
         )),              


#### META ANALYSIS ##########
tabPanel("Meta" ,
         fluidRow(
           h4("Meta analysis for binary or continuous outcomes"),
           h6("Demo values are used as defaults. For a continuous outcome example, data(Fleiss93Cont)."),
           column(4,
                  textInput("meta_dataframe", "A. Enter the data frame name",   #Meta analysis data frame"
                            value="lungcancer"),                            #Default is lungcancer data.
                  br(),                   
                  h4("Binary outcomes"),
                  uiOutput("event.e_bin"),  #Number of events in the treatment group.
                  uiOutput("event.c_bin"),  #Number of events in the control group.
                  br(),                   
                  h4("Continuous outcomes"),
                  uiOutput("n.e_con"),  #
                  uiOutput("n.c_con")  #
           ),
           
           column(3,  offset=1,
                  uiOutput("bin_or_con"),  #Indicates whether this is a binary or continuous outcome.
                  br(),
                  h4(" "),
                  br(),                   
                  uiOutput("n.e_bin"),  #Number of observations in the treatment group.
                  uiOutput("n.c_bin"),  #Number of observations in the treatment group.
                  br(),                   
                  br(),                   
                  h4(" "),
                  uiOutput("mean.e_con"),  #
                  uiOutput("mean.c_con")  #
           ),
           
           column(3, offset=1, 
                  br(),                   
                  br(),
                  br(),
                  h4(" "),
                  br(),                   
                  br(),
                  br(),
                  h4(" "),
                  br(),                   
                  br(),
                  br(),
                  h4(" "),
                  br(),                   
                  br(),
                  br(),
                  h4(" "),
                  br(),                   
                  br(),
                  br(),
                  br(),
                  br(),                   
                  uiOutput("sd.e_con"),  #
                  uiOutput("sd.c_con")  #
           )
         ),
         h4("Overall treatment effects (Odds Ratios) with 95% confidence intervals"),
         verbatimTextOutput("meta_summary"),  #Meta analysis results 
         br(),
         h4("A Forest Plot of study and overall treatment effects"),
         plotOutput("forestplot")
),    

################################################################################
#                       Power analysis                                         #
################################################################################

tabPanel("Power" ,
         fluidRow(
           h4("Power analysis using a binomial proportion or t-test"),
           h6("Demo values are used as defaults, including harmonic mean sample sizes for uneven group Ns."),
           column(4,
                  h4("Binary outcomes"),
                  uiOutput("power_bin"),  #Number of events in the treatment group.
                  uiOutput("n_bin"),  #Number of events in the control group.
                  br(),
                  h4("Continuous outcomes"),
                  uiOutput("power_con"),  #
                  uiOutput("n_con"),  #
                  uiOutput("pwr_smp_con"),  #
                  br(),                   
                  h4("Do you have uneven group sizes?"),
                  uiOutput("grp1_n")  #
           ),
           
           column(3,  #offset=1,
                  br(),                   
                  br(),                   
                  uiOutput("p1_bin"),  #Number of observations in the treatment group.
                  uiOutput("p2_bin"),  #Number of observations in the treatment group.
                  br(),                   
                  uiOutput("delta_con"),  #
                  uiOutput("sd_Con"),  #
                  br(),                   
                  br(),                   
                  br(),                   
                  br(),                   
                  br(),                   
                  br(),                   
                  br(),                   
                  h4(" "),
                  uiOutput("grp2_n")  #
           ),

           column(3, offset=1, 
                  br(),                   
                  br(),                   
                  uiOutput("sig_bin"),  #Number of observations in the treatment group.
                  uiOutput("pwr_smp_bin"),           
                  br(),
                  br(),
                  uiOutput("sig_con"),  #
                  uiOutput("type_con"),  #
                  br(),                   
                  br(),                   
                  br(),                   
                  br(),                   
                  br(),                   
                  br(),                   
                  br(),                   
                  h4(" "),
                  uiOutput("harmonic_n")  #
           )
         ),
         
         h4("Power analysis summary"),
         verbatimTextOutput("power_summary"),
         h5("'Delta' represents the difference between the two group's mean."),
         h5("The pooled standard deviation (SD) is calculated as the average of each group's SD (Cohen, 1988)."),
         br(),
         h4("Effect size summary"),
         verbatimTextOutput("effect_size_summary"),
         h5("Effect sizes are standardized values that represent the magnitude of differences between the groups."),
         h5("Binary outcomes: Values of 0.20, 0.50, and 0.80 represent small, medium, and large effects (Cohen, 1988)."),
         h5("Continuous outcomes: Values of 0.20, 0.50, and 0.80 represent small, medium, and large effects. Consult Cohen, 1988, about paired t-tests.")
), 

###
tabPanel("95% CIs",
         h4("This plot produces unadjusted confidence intervals for each level of a factor."),
         fluidRow(
           column(3, 
                  uiOutput("CIy"),
                  br(),                                                 
                  uiOutput("CIx")
           ),
           column(3, offset=1,
                  uiOutput("Ci_Choice_Type"),
                  br(),                                                 
                  uiOutput("Ci_Conf_Lev")
           ),
           column(3, offset=1,
                  uiOutput("Ci_Alpha_Num"),
                  uiOutput("Ci_create")
           )),
         plotOutput("Plot_Ci_output", height = 700, width="100%"),
         h6("Note: You can sort alphabetically by the factor level name or numerically by the point estimate. Left side = factor level, right side = point estimate."),
         verbatimTextOutput("Cidf_output"),
         h6("Note: The values above are point estimates and confidence limits that are sorted alphabetically and numerically."),
         br(),
         h4("Performance of groups over time (need >= 6 time points)"),
         h5("This plot has smoothed spline trajectories, with or without confidence bands. \"Trend\" lines may not have cooridnate values that equal rates."),
         br(),
         fluidRow(
           column(3, 
                  uiOutput("FCIy"),
                  br(),                                                 
                  uiOutput("FCi_Choice_Type")
                  ),
           column(3, 
                  uiOutput("FCIx"),
                  br(),                                                 
                  uiOutput("FCi_Conf_Lev")
           ),
           column(3, 
                  uiOutput("FCIz"),
                  br(),                                                 
                  uiOutput("FCi_create")
           ),
           column(3, 
                  uiOutput("FCI_bands"),
                  br(),                                                 
                  uiOutput("FCI_nk_knots")
           )),
         h6("Hint: To zoom in on the lines only, select a lower confidence level (e.g., .01) and don't use confidence bands."),
         br(),
         plotOutput("Plot_Fci_output", height = 800, width="100%"),
         h5("These are point estimates and confidence intervals. These may not match up with smoothed lines."),
         tableOutput("time_ci_out1")
)
###


############## TEST SECTION #############################
# , #THIS COMMA IS COMMENTED OUT IN CASE I EVER NEED THE TEST FUNCTION BELOW    

#tabPanel("Test it",                                #Creates a new panel named "Test"
#         fluidRow(                           #Wrapping them in a fluidRow provides easy control over  
#           verbatimTextOutput("test1")
#           plotOutput("testplot1")
#         ))
############## TEST SECTION #############################


        )             ####From this point down, this closes the main sections at the top
      )
    )
  )
)
