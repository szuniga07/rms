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
                   h4("Model specs"),
                   verbatimTextOutput("specifications"),  #"regress" calls output$regress from server.r 
                   h6("Specs gives these values:"),
                   h6("1. Low:effect = 25%, 2. Adjust to = Median, 3. High:effect = 75%, 4. Low:prediction = 10%"),
                   h6("5. High:prediction = 90%, 6. Low=lowest value, and 7. High = Highest value."),                 
                   br(),
                   h4("Describe the outcome variable"),
                   verbatimTextOutput("desc_Y"),  #"regress" calls output$regress from server.r
                   plotOutput("outcome_hist"),
                   h5("Histogram of numeric variables (i.e., not factors)")
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
                  h5("Get predicitions using an existing model.")
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
           h4("Descriptives on means and missing values. Plot the target variable, stratifying by facctors."),
           br(),
           h4("Explore the mean values of a variable by factor levels."),
           column(3, 
                uiOutput("desc_y")),
           column(3, offset=1,
                  uiOutput("desc_x")),
           column(3, offset=1,
                  uiOutput("desc_choice"))
         ),
           plotOutput("DescSmryPlt", height = 700),          
           br(),
         fluidRow(   
           column(3, 
                  uiOutput("miss_y")),
           column(3, offset=1,
                  uiOutput("miss_x")),
           column(3, offset=1,
                  uiOutput("miss_choice"))
           ),
         br(),
         h4("Explore the missingness of a variable by factor levels."),
         br(),
         h5("Proportion of missng values stratified by factors."),
         plotOutput("MissSmryPlt", height = 700), 
         br(),
         h5("Total proportion of missng values for each factor."),
         plotOutput("naPlt", height = 400), 
         h5("Missng values clustered by factors."),
         plotOutput("naDendo", height = 400), 
         h5("Recursive partitioning to predict missing values. When condition met, move to the left of the split. Otherwise, move right."),
         plotOutput("naTree", height = 400),
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
           h6("Single imputation is used because of the issue of random sampling of random sampling from multiple imputed datasets."),
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
                   plotOutput("dis"),                  #Creates a new panel named "Summary"
                   br(),
                   h6("Warning: Factors with 4 or more categories can't be plotted in a single plot."),
                   h6("Consider using \"Partial PREDS\" tab to assess factors with many levels."),
                   uiOutput("sum_one_yes"),  #vx is the created drop down box coming from renderUI in server.r.
                   br(),
                   uiOutput("sum_one_x"),  #vx is the created drop down box coming from renderUI in server.r.
                   br()                   
          ),    
          
          tabPanel("Importance", 
                   h4("Plot of predictor importance"),
                   plotOutput("p_anova"),
                   br(),
                   h4("ANOVA summary of model"),
                   verbatimTextOutput("anova_smry")
                   ),    #Creates a new panel named "Summary"
          tabPanel("Partial PREDS", 
                   h4("Partial predictions plot"),
                   h4("The model can be described using partial effect plots by plotting each X against Y holding other predictions constant (e.g., Median)."),                   
                   plotOutput("prt_prd"),
                   br(),
                   uiOutput("prt_one_yes"),  #vx is the created drop down box coming from renderUI in server.r.
                   br(),
                   uiOutput("prt_one_x"),  #vx is the created drop down box coming from renderUI in server.r.
                   br()                   
                   ),    #Creates a new panel named "Summary"
          tabPanel("Nomogram",
                   h4("Hand calculate probabilities with a nomogram"),
                   h4("We use a nomogram that converts each effect in the model to a 0 to 100 scale."),
                   plotOutput("nomo_gram"),
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
                   h6("Note: Confirm the single imputed or original dataframe is loaded in the 'Model builder' tab, depending on your purpose."),
                   br(),
                   h4("Model calibration (e.g, bootstrapped, cross-validated--repeated 100 times)"),
                   h4("The reliability of a model, meaning the ability of the model to predict future observations as well as it appeared to predict the responses at hand."),
                   plotOutput("cali_brate")
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
                   h6("Note: Confirm the single imputed or original dataframe is loaded in the 'Model builder' tab, depending on your purpose."),
                   br(),
                   h4("Calibration and discrimination indexes (from bootstrapped methods, cross-validated--100 repeats)"),
                   h4("The worth of a model can be judged by how far it goes out on a limb while 
                      still maintaining good calibration. If one model assigns .02 of 
                      the sample to a risk of dying above a predicted value of .9 while 
                      the other model assigns .08 of the sample to the high risk group, 
                      the second model is more discriminating."),
                   h6("Note: Use the \"boot\" method to view the \"Factors Retained in Backwards Elimination\". These are factors found statistically significant in random draws. First 50 bootstrapped results shown."), 
                   verbatimTextOutput("vali_date")),    #Creates a new panel named "Summary"


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
           plotOutput("oat_mn_sd")   
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
         verbatimTextOutput("cutoff_smry")
),    

tabPanel("Cobweb plot",                                #Creates a new panel named "Test Plot"
         fluidRow(                           #Wrapping them in a fluidRow provides easy control over  
           h4("Cobeweb plot of best 1% or 5% of predicted outcome scores (\"best\" can be top or bottom values.)"),
           h5("Shows responses of predictors with best predicted outcome values. Narrower spread indicates greater predictor importance."),
           h6("Note: Requires at least 2 predictors, 1 of which needs to be continuous. If not, produces error message."),
           uiOutput("top_bottom_5"),
           plotOutput("cobwebplot"),
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
         verbatimTextOutput("power_summary")
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
         plotOutput("Plot_Ci_output"),
         h6("Note: You can sort alphabetically by the factor level name or numerically by the point estimate. Left side = factor level, right side = point estimate."),
         verbatimTextOutput("Cidf_output"),
         h6("Note: The values above are point estimates and confidence limits that are sorted alphabetically and numerically.")
)    
###

# , #THIS COMMA IS COMMENTED OUT IN CASE I EVER NEED THE TEST FUNCTION BELOW    

############## TEST SECTION #############################
##tabPanel("Test it",                                #Creates a new panel named "Test"
         ##         fluidRow(                           #Wrapping them in a fluidRow provides easy control over  
##           h4("Download, Upload, Save and make Predictions from model fits."),
##           verbatimTextOutput("test1")
##           plotOutput("testplot1")
##           h4("Modeling with Multiple Imputation."),
##           column(6, 
##uiOutput("MIx"),
##uiOutput("MIks")),
##column(6, 
##uiOutput("MIn"),
##uiOutput("MI_Begin")),
##h5("Evaluation of Multiple Imputation"),
##verbatimTextOutput("test1")
##         ))
############## TEST SECTION #############################


        )             ####From this point down, this closes the main sections at the top
      )
    )
  )
)
