                                  
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
                   h5("Whenever saving R data file names, use the '.RData' extension in the name (e.g., myData.RData)."),
                   fluidRow(
                     column(3, 
                            uiOutput("View_main_df"))  
                   ),
                   tableOutput("view_main_data_out"),
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
          h6("Create date formats. For #4, group variables by format type if multiple types. For #5, format examples for JAN 1, 2021. For #6, indicate how many X per format (e.g., c(3,1))."),
          fluidRow(
            column(3,
                   uiOutput("modify_time_X")),  
            column(3, offset=1,
                   uiOutput("modify_time_format")),  
            column(3, offset=1,
                   uiOutput("modify_tm_fmt_rep"))  
          ),
          br(),
          h6("Subset the data you need. In #8, 'subset' indicates rows, 'select' indicates columns (e.g., subset= gender==male, select= 1:7)."),
          fluidRow(
            column(3,
                   uiOutput("subset_df_yes_no")),  
            column(3, offset=1,
                   uiOutput("subset_args")),
            column(3, offset=1,
                   uiOutput("modify_df_yes_no")) 
          ),
          br(),
          h6("Create 'Time' as the difference between 2 dates, select 'Time1', 'Time2'. If none, 'No.Time.Var=NA'. Create numeric 'YYMM' as month + year: 202101. And 'Month': ordered(YYMM)."),
          fluidRow(
            column(3,
                   uiOutput("modify_2_var_Time")),  
            column(3, offset=1,
                   uiOutput("modify_Time_add_month")),
            column(3, offset=1,
                   uiOutput("modify_add_time_YN")) 
          ),
          br(),
          fluidRow(
            column(3,
                   uiOutput("modified_df_save")),  
            column(3, offset=1,
                   uiOutput("modified_df_name")),  
            column(3, offset=1,
                   downloadLink('download_modified_df', '15. Click to download data.'))
          ),
          
          ################# End here
          
          ## Transformed/Imputed ##
          br(),
          h5("Once data is downloaded, it can be uploaded and entered at the top for analysis. Make sure #3 at top is 'No'."),
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
                            br(),
                            uiOutput("reg_typ"),  #
                            br()
                     ),
                     
                     column(3,  offset=1,
                            uiOutput("censoring"),  #Survival censoring variable
                            br(),
                            uiOutput("aft_dist"),  #AFT distribution choice.
                            br(),
                            uiOutput("clustering"),  #GLS clustering variable.
                            br(),
                            uiOutput("quant_tau"),  #Selects the percentile to use in quantile regression.
                            br(),
                            uiOutput("weighting_reg"),  #Selects the weight variable in the regression.
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
                     )
                   ),
                   h5("Note: To model using Multiple Imputation, set #2 to 'No' after completing imputations. 'Proportion Y Logistic' requires a weight in #10 equal to the proportion's denominator."),
                   h5("For Poisson regression using incidence rate, enter the offset function in #14, e.g., Y ~ x1 + x2 + offset(log(N)) ."),
                   br(), 
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
                   br(),
                   h4("Describe the predicted values"), 
                   verbatimTextOutput("desc_YhatHistRslt"),   
                   h5("Transformed.Yhat: Cox and logistic model predictions are transformed to probabilities. Poisson model predictions are exponentiated."),
                   plotOutput("y_hat_hist"),
                   h5("Linear predicted values (logit, exp(logit), or response level) and probabilities (Logistic regression)."),
                   br(),
                   h4("Plot and compare observed and predicted values, with or without factor groupings."),
                   h5("Add a second color in #5 for predicted scores when #2 is 'No', if preferred."),
                   br(),
                   fluidRow(   
                     column(3, 
                            uiOutput("opyx")),
                     column(3, 
                            uiOutput("opy_strat_fac")),
                     column(3, 
                            uiOutput("opyz")),
                     column(3, 
                            uiOutput("opyplot_grp_levs"))
                   ),
                   fluidRow(   
                     column(3, 
                            uiOutput("opy_plot_ln_clrs")),
                     column(3, 
                            uiOutput("opy_plot_symbl_sz")),
                     column(3, 
                            uiOutput("opy_Hor_Line")),
                     column(3, 
                            uiOutput("opy_Vert_Line"))
                   ),
                   fluidRow(   
                     column(3, 
                            uiOutput("opy_plot_tgt_ln_clrs")),
                     column(3, 
                            uiOutput("opy_plot_lab_multi")),
                     column(3, 
                            uiOutput("opy_lgd_loc")),
                     column(3, 
                            uiOutput("opy_create"))
                   ),
                   fluidRow(   
                     column(3, 
                            uiOutput("opy__Xlim1")),
                     column(3, 
                            uiOutput("opy__Xlim2")),
                     column(3, 
                            uiOutput("opy__Ylim1")),
                     column(3, 
                            uiOutput("opy__Ylim2"))
                   ), 
                   br(),
                   plotOutput("observed_pred_scatter", height = 800, width="100%"),
                   br(),
                   h5("If selecting a factor to show predictions by group members, consider factors with fewer levels to easier examine results or limit it to specific cases (#4 above). 'RES MN' = Residual Mean."),
                   br(),
                   h5("Residual means and medians"),
                   verbatimTextOutput("residuals_by_group"),  #"regress" calls output$regress from server.r 
                   br(),
                   h4("Examine sensitivity and specificity values from a threshold on predicted values."),
                   br(),
                   h5("Set a prediction threshold and examine sensitivity and 1-specificity (false-positives) below. Find the best cutoff level. AUC calculated using the trapezoidal rule (Rosner, 2006) at the cutoff level."),
                   h5("The threshold that minimizes misclassification is where the sum of the number of false-positive and false-negative results (error rate) is lowest."),
                   h5("The appropriate threshold is based on the clinical context (e.g., we consider any risk over 20% as being too high to go without an intervention)."),
                   h5("A common option is to select a threshold equal to the prevalence rate (mean). A predicted score above prevalence indicates high risk, a value below prevalence indicates low risk."),
                   h5("The threshold range can be set by using sensible upper and lower bounds on the maximum number of false positives one would tolerate to find one true positive. E.g., if a detected cancer is worth 16 unnecessary surgical interventions, an appropriate risk threshold for surgery would be 1/(1 + 16) = 6%."),
                   h5("Someone may not do more than 10 biopsies to find one high-grade cancer in patients with similar health and who think about the risks and benefits of biopsy vs. finding cancer in the same way. So if a patient's risk was above 10% I do a biopsy, otherwise not. The risk of 10% is odds of 1:9. Missing a high-grade cancer is 9 times worse than doing an unnecessary biopsy."),
                   fluidRow(
                     column(3, 
                            uiOutput("pred_class_thresh")),
                     column(3,
                            uiOutput("pred_class_time")),
                     column(3, 
                            uiOutput("class_pri_mdl_nm")),
                     column(3,
                            uiOutput("use_pred_cls_pri_mdl_yesno"))
                   ),
                   br(),
                   h5("Get classifcation results from a prior model on new data. Upload prior model in 'Data' or 'PREDs' tabs, set up model above, enter prior model fit name in #3, and answer 'Yes' in #4 below."),
                   fluidRow(
                     column(3, 
                            uiOutput("pred_class_t_br_clrs")),
                     column(3, 
                            uiOutput("pred_class_f_br_clrs")),
                     column(3, 
                            uiOutput("pred_class_ln_clrs")),
                     column(3, 
                            uiOutput("pred_class_ln_wdth"))
                   ),
                   br(),
                   fluidRow(
                     column(3, 
                            uiOutput("pred_class_hist_bars")),
                     column(3, offset=1,
                            uiOutput("class_hist_asp_ratio")),
                     column(3, offset=1,
                            uiOutput("pred_class_hist_yesno"))
                   ), 
                   br(),
                   h5("Outcome = Yes = max(Outcome). Outcome = No = min(Outcome). In a 'Cox PH with censoring' model, Outcome = Yes = 'Dead', Outcome = Yes = 'Alive'. For non-censored 'Cox PH' and 'AFT', Outcome = Yes = <Time-cutoff and Outcome = No = >=Time-cutoff because it represents higher risk/faster outcomes."),
                   h5("X-axis values are linear predictions. For AFT models, sensitivty and 1-specificity values are in reverse because predictions are in survival times (i.e., use values below cutoff)."),
                   h5("For Poisson regressions with offset(), the actual range of predictions differs from the histogram of predicted Y above. Offsets need to be multiplied by mean(offset) to be similar."),
                   h5("Use built-in function, fncAucDcaClass(Fit= fit1(), Y= outcome(), RegType= input$regress_type, DF= df()) , in 'Describe' tab to get AUC/Decision Curve/Classification results for each unique prediction as a threshold. See server.r for documentation."),
                   plotOutput("plot_binary_class_run", height = 800, width="100%"),
                   br(),
                   h5("Get sensitivity, specificity, false-positive, false-negative, and positive and negative predictive values associated using your prediction threshold value. Includes Decision Curve Analysis."),
                   verbatimTextOutput("get_bin_class_sens_spc_out"),
                   h5("Note: The Decision Curve Analysis above is weighted using normal approximation in formulas for continuous outcomes."),
                   h5("Positive Predictive Value: Proportion of all positive classifications that were true-positive."),
                   h5("Negative Predictive Value: Proportion of all negative classifications that were true-negative."),
                   br(),
                   h4("Decision Curve Analysis (Vickers, 2006, doi:10.1177/0272989X06295361)"),
                   br(),
                   h5("The Net Benefit is driven by true-positives, a higher value is better. The NB plot compares what provides a better strategy along various thresholds (only consider thresholds within the range of predicted values) . Two valid comparison strategies are to 'treat all' or 'treat none'."),
                   h5("For example, a net benefit of 0.0973 indicates that the difference between our true-positives and false-positives will be finding about 10 more true-positives than false-positives per 100 patients using a cutoff of FEV= 3."),
                   h5("The 'All treated' line tends to intersect the 'None treated' line at the prevalence line for binary outcomes. No Benefit when all interventions withheld."),
                   h5("A model can reduce unnecessary interventions. For example, a risk threshold of 10% may reduce the number of unnecessary interventions by 40 per 100 without missing treatment for any patients with cancer."),
                   h5("Net Benefit = Frequency of True-Positives/N - False-Positives/N * (Pt/1-Pt) at a specific threshold (Pt)."),
                   h5("All Treated = (true-positive + false-negative)/N - (false positive + true-negative)/N * (Pt/1-Pt)."),
                   h5("Interventions Avoided = Frequency of True-Negatives/N - False-Negatives/N * (1-Pt/Pt) at a specific threshold (Pt)."),
                   h5("Weighting: As Pt goes up, true- and false-positives goes down but the weight goes up. With a bigger weight, net benefit tends to get smaller because [false-positives * weight] is larger."),
                   br(),
                   fluidRow(
                     column(3, 
                            uiOutput("net_or_intervention")),
                     column(3, 
                            uiOutput("dca_lgd_loc")),
                     column(3, 
                            uiOutput("dca_plot_ln_clrs")),
                     column(3, 
                            uiOutput("dca_ln_wdth"))
                   ),
                   fluidRow(
                     column(3, 
                            uiOutput("descionCrvPltXlim1")),
                     column(3,
                            uiOutput("descionCrvPltXlim2")),
                     column(3,
                            uiOutput("descionCrvPltYlim1")),
                     column(3,
                            uiOutput("descionCrvPltYlim2"))
                   ),
                   fluidRow(
                     column(3, 
                            uiOutput("decison_curve_anly_yesno"))
                   ),
                   br(),
                   plotOutput("plot_thresh_quant_run", height = 800, width="100%"),
                   h5("The decision curve's weights (e.g., Pt/1-Pt) for 7 predicted values use quantiles (0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95)."),
                   h5("The X-axis also represents the binary categories and values when there are < 20 unique values."),
                   br(),
                   h4("Decision Curve Analysis output for various thresholds along quantiles."),
                   verbatimTextOutput("get_decision_curve_out"),
                   h5("The DCA output is for each threshold level listed above."),
                   br()
          ),    
          
          ############## PREDs SECTION #############################
          tabPanel("PREDs",                                #Creates a new panel named "Test"
                   h4("Download, Upload, Save and make Predictions from model fits."),
                   br(),
                   h5("Save model."),
                   h5("Note: Save ANY file name with '.RData' at the end. Poisson regressions with offsets require exp(lpred)*offset mean"),
                   fluidRow(                           #Wrapping them in a fluidRow provides easy control over  
                     column(3,
                            uiOutput("SaveModelFit")),
                     column(3, offset=1,
                            uiOutput("mdl_fit_name")),
                     column(4, 
                            downloadLink('model', '3. Click to download the model.')
                     )),
                   h5("Upload a model fit."),
                   uiOutput("upload_model_fit"),
                   verbatimTextOutput("mdl_print"),
                   br(),
                   h5("Upload a data frame for predictions."),
                   uiOutput("upload_PRED_df"),
                   verbatimTextOutput("PREDdatastr"),
                   br(),
                   h5("Get predicitions using the current model."),
                   fluidRow(
                     column(3,
                            uiOutput("curr_fit_df")),
                     column(3,
                            uiOutput("fit_curr_mdl")),
                     column(3,
                            uiOutput("curr_mdl_pred_df")),
                     column(3, 
                            downloadLink('pred_curr', '4. Click to download the PREDs.'))
                   ),
                   br(),
                   h5("Get predicitions using a previous model."),
                   fluidRow(
                     column(3,
                            uiOutput("primary_fit")),
                     column(3,
                            uiOutput("new_fit_df")),
                     column(4,
                            uiOutput("fit_new_mdl"))
                   ),
                   fluidRow(
                     column(3,
                            uiOutput("new_mdl_pred_df")),
                     column(3,
                            downloadLink('pred_new', '5. Click to download the PREDs.'))
                   ),
                   h5("Summary of the selected model fit"),
                   verbatimTextOutput("prime_mdl_smry")
          ),
          
          ############## End PREDs #############################
          
          ############## Describe and Missing #############################
          tabPanel("Describe",
                   h4("Calculator/Grapher"),
                   fluidRow(
                     column(4, 
                            uiOutput("calculator_box")),
                     column(4, offset=1,
                            uiOutput("calculator_yesno"))
                   ),
                   h5("Basic mathematical functions (+, -, *, /) and runs other commands (e.g., sqrt(16) + 6 = 10, sd(mtcars$mpg) )."),         
                   h5("To load an object to the global environment, use assign(), e.g., assign(\"my_data\", my_data[1:5000, ], envir=globalenv() )"),
                   h5("Add leading 0s to a numeric X: assign(\"airquality\", within(airquality, {Temp <- sprintf(\"%012d\", Temp) }), envir=globalenv() )"),
                   h5("When using an offset in Poisson regression, plot as ' plot(Predict(fit1(), fun=function(x) exp(x)*mean(nonmel$n), offset=list(n=mean(nonmel$n)))) '."),
                   h5("Example code for contrasts: contrast(model, list(week =c(2 ,16), treat='New'), list(week= c(2 ,16), treat= 'Old') )"),
                   h5("Calibration Curve, type into #1 above: Calibration.Curve() . Select 'Graph' in #2 (or 'Results' for statistics). Change the number of bins, text location, colors, digits, and size."),
                   h5("Defaults are: Calibration.Curve(BINS=10, POS=3, CEX=2, VAL=FALSE, RND=2, PCol='blue', LCol='red', LWD=5, YLim). VAL=TRUE bins data by unique predictions."),
                   h5("Works for Linear, Poisson, Quantile, GLS, and Logistic models. Hosmer-Lemeshow X2 test for logistic models."),
                   verbatimTextOutput("prnt_calculation"),
                   plotOutput("plt_calculation", height = 500, width = "100%"),
                   br(),
                   h4("Descriptives on means and missing values. Plot the target variable, stratifying by factors."),
                   br(),
                   h4("Summarize the data's central tendency and dispersion."),
                   fluidRow(   
                     column(3, 
                            uiOutput("desc_summ_vars")),
                     column(3, offset=1,
                            uiOutput("des_summ_yesno"))
                   ),
                   verbatimTextOutput("prnt_desc_summ"),
                   h5("Coefficient of variation = Standard Deviation / Mean. COV returns errors ('NA') for non-numerical variables."),
                   br(),
                   #Histogram
                   h4("Histogram of a key variable"),
                   fluidRow(   
                     column(3, 
                            uiOutput("smry_var_hist_var")),
                     column(3, offset=1,
                            uiOutput("smry_var_hist_fac_yesno")),
                     column(3, offset=1,
                            uiOutput("smry_var_hist_fac"))
                   ),
                   fluidRow(   
                     column(3,
                            uiOutput("smry_var_hist_bars")),
                     column(3, offset=1,
                            uiOutput("smry_var_hist_bar_clr")),
                     column(3, offset=1,
                            uiOutput("smryVrHstLabMulti"))
                   ),
                   fluidRow(   
                     column(3, 
                            uiOutput("smry_hist_mn_med_yesno")),
                     column(3, offset=1,
                            uiOutput("smry_var_hist_ln_clr")),
                     column(3, offset=1,
                            uiOutput("smry_var_hist_yesno"))
                   ),
                   plotOutput("summary_var_histogram_out", height = 800, width = "100%"),
                   h5("Note: Mean= solid vertical line, Median= dashed vertical line."),
                   br(),
                   #Means by factors
                   h4("Explore the mean values of an outcome variable by factor levels."),
                   fluidRow(   
                     column(3, 
                            uiOutput("desc_y")),
                     column(3, offset=1,
                            uiOutput("desc_x")),
                     column(3, offset=1,
                            uiOutput("desc_choice"))
                   ),
                   plotOutput("DescSmryPlt", height = 800, width = 1200), 
                   ## scatter plot with correlation ## 
                   h4("Scatterplot with a loess smoothed line, OLS regression line, and a correlation test."),
                   br(),
                   h5("Method indicates which correlation coefficient is used for the test."),
                   h5("'pearson'= both continuous variables; 'kendall' tau= continuous, ordinal, and binary scales. 'spearman' rho= continuous, ordinal, and binary scales; nonparametric method will detect not only non-linear relationships but non-monotonic ones."),
                   h5("Exact indicates whether an exact p-value should be computed. Used for 'kendall' and 'spearman'. Default is to leave #7 blank."),
                   h5("Continuity correction used for 'kendall' and 'spearman' when not computed exactly."),
                   h5("When adding a regression line, the intercept and slope estimates are added to the title. The p-value is idential to a Pearson correlation. Pearson R^2 is the proportion of variance in Y explained by X."),
                   br(),
                   fluidRow(   
                     column(3, 
                            uiOutput("sctr_crtst_y")),
                     column(3, offset=1,
                            uiOutput("sctr_crtst_x")),
                     column(3, offset=1,
                            uiOutput("sctr_crtst_meth"))
                   ),
                   fluidRow(   
                     column(3, 
                            uiOutput("sctr_crtst_alt")),
                     column(3, offset=1,
                            uiOutput("scatter_cor_test_CI")),
                     column(3, offset=1,
                            uiOutput("scatter_cor_test_exct"))
                   ),
                   fluidRow(   
                     column(3, 
                            uiOutput("scatter_cor_test_cnt")),
                     column(3, offset=1,
                            uiOutput("scatter_cor_regression_add_YN")),
                     column(3, offset=1,
                            uiOutput("sctr_crtst_clr"))
                   ),
                   fluidRow(   
                     column(3, 
                            uiOutput("scatter_cor_lgd_loc")),
                     column(3, offset=1,
                            uiOutput("scatter_cor_test_run_YN"))
                   ),
                   plotOutput("scatter_cor_test_plt_out", height = 800, width = 1200),
                   br(),
                   h5("Correlation test results"),
                   verbatimTextOutput("scatter_cor_test_cor_test_out"),
                   
                   ## summaryRc plot ## 
                   br(),
                   h4("Graphical Summarization of Continuous Variables Against a Continuous Response"),
                   fluidRow(
                     br(),
                     column(3, 
                            uiOutput("smryRc_y")),
                     column(3, offset=1,
                            uiOutput("smryRc_x"))
                   ),
                   br(),
                   fluidRow(
                     column(3, 
                            uiOutput("smryRc_strat_yes_no")),
                     column(3, offset=1,
                            uiOutput("smryRc_z")),
                     column(3, offset=1,
                            uiOutput("smryrc_choice"))
                   ),
                   h5("Summary of lowess smoothed X against Y by stratification levels. Percentiles and tick marks below indicate X's data density."),
                   plotOutput("summaryRC_plot_function_out", height = 800, width = "100%"),          
                   br(),
                   ## Density plot of trend over time by groups ## 
                   h4("Trend over time by groups"),
                   br(), 
                   h5("A density plot per time period. Requires complete data for all time periods."),
                   h5("Press the play button to see the trend. Highlight specific groups. Names are above group rates, randomly stacked. Select >1 time increments for rolling average."),
                   fluidRow(   
                     column(3, 
                            uiOutput("dnsty_grp_trnd_Yvar")),
                     column(3, 
                            uiOutput("dnsty_grp_trnd_Xvar")),
                     column(3, 
                            uiOutput("dnsty_grp_bgn_yesno")),
                     column(3, 
                            uiOutput("dnsty_grp_trnd_Xlevs"))
                   ),
                   fluidRow(   
                     column(3, 
                            uiOutput("dnsty_grp_trnd_Zvar")),
                     column(3, 
                            uiOutput("dnsty_grp_trnd_Z_inc")),
                     column(3, 
                            uiOutput("dnsty_grp_trnd_ln_clr")),
                     column(3, 
                            uiOutput("dns_plot_lbl_clrs"))
                   ),
                   fluidRow(   
                     column(3, 
                            uiOutput("dns_plot_txt_lbl_sz")),
                     column(3, 
                            uiOutput("dnsty_grp_trnd_trgt")),
                     column(3, 
                            uiOutput("dnsty_grp_trnd_lgd_loc"))
                   ),
                   
                   fluidRow(   
                     column(3, 
                            uiOutput("dnsty_grp_trnd_st_sed")),
                     column(3, 
                            uiOutput("dnsty_grp_trnd_run_yesno")),
                     column(3, 
                            uiOutput("dnsty_grp_trnd_sec")),
                     column(3, 
                            uiOutput("dnsty_grp_trnd_ply"))
                   ),
                   fluidRow(   
                     column(3, 
                            uiOutput("dnsty_grp_trnd_Xlim1")),
                     column(3, 
                            uiOutput("dnsty_grp_trnd_Xlim2")),
                     column(3, 
                            uiOutput("dnsty_grp_trnd_Ylim1")),
                     column(3, 
                            uiOutput("dnsty_grp_trnd_Ylim2"))
                   ),
                   br(),
                   plotOutput("dnsty_grp_trnd_plot", height = 800, width = "100%"),          
                   br(),
                   h5("Rates by period from the density plot above"),
                   br(),
                   verbatimTextOutput("dnsty_grp_trnd_out_by_tm"),
                   br(),
                   h4("Explore the missingness of a variable by factor levels."),
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
                   fluidRow(   
                     column(3,  
                            uiOutput("SIks"),
                            uiOutput("SI_set_maxIter")),
                     column(3, offset=1,
                            uiOutput("AsIs_x"))
                   ),
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
                   h4("Modeling with Multiple Imputation (MI)."),
                   h6("Note: After starting the multiple imputation, all graphs and tables will include MI results."),
                   h6("However, the 'Calibration' and 'Validation' tab results use single imputation."),
                   h6("Single imputation is used because of the issue of random sampling of random sampling from multiple imputed datasets. Consider selecting all model X and Y when imputing."),
                   h6("To update the 'Approximate' tab, answer 'Yes' to the '2. Begin modeling?' in the 'Model builder' after running the MI."),
                   br(),
                   fluidRow(   
                     column(3,  
                            uiOutput("MIx"),
                            uiOutput("MI_set_seed")),
                     column(3, offset=1, 
                            uiOutput("MIn"),
                            uiOutput("MIAsIs_x")),
                     column(3, offset=1,
                            uiOutput("MIks"),
                            uiOutput("MI_Begin"))
                   ),
                   h6("Select the number imputations equal to the percentage missing (e.g., 20% missing = 20 imputations)."),
                   h6("Set the number of knots = 0 for a linear fit or if there are < 5 unique values in continous variables."),
                   h4("Multiple Imputation summary"),
                   verbatimTextOutput("MI_smry")
          ),
          
          tabPanel("Modify", 
                   h4("Fast backwards step-down regression, AIC based suggestions"),
                   verbatimTextOutput("mod_ify"),
                   br(),
                   h3("Crude Analysis"),
                   br(),
                   h4("Get variable importance statistics of single predictors for linear or logistic models."),
                   h4("'Min.Coef' & 'Max.Coef' gives the most extreme coefficient when there is a perfect correlation of Y and X (SD of Y/ SD of X)."),
                   h4("'Min.Slope.95' & 'Max.Slope.95' gives the product of min/max coefficient * 95th percentile of X."),
                   h4("Keep or Drop specific variables but don't select both."),
                   h4("Select the predictors with a threshold on the proportion missing and/or select the top number of predictors."),
                   br(),
                   fluidRow(   
                     column(3, 
                            uiOutput("imprtnt_y")),
                     column(3, 
                            uiOutput("imprtnt_kp_x")),
                     column(3, 
                            uiOutput("imprtnt_dp_x")),
                     column(3, 
                            uiOutput("imprtnt_reg_typ"))
                   ),
                   fluidRow(   
                     column(3, 
                            uiOutput("imprtnt_Prop_Miss")),
                     column(3, 
                            uiOutput("imprtnt_Top_N")),
                     column(3, 
                            uiOutput("imprtntXVarRun"))
                   ),
                   verbatimTextOutput("imprtnt_X_Output"),
                   br(),
                   h4("Type imprtnt_X_function() in the describe tab to run the fncImportantX function."),
                   br(),
                   ),    #Creates a new panel named "Modify"
          tabPanel("Approximate",                       
                   h4("Approximate the model:"),
                   h4("Create a parsimonious model with an equivalent level of prediction as the \"Full Model\" ."  ),
                   uiOutput("MIForAprx"),
                   h5("If you will answer \"Yes\", also click \"Yes\" to #2 in the 'Model builder' tab before imputing and approximating if you want to compare the imputed/non-imputed results."  ),
                   plotOutput("approximate_plot", height=500, width = "100%"),                  #Creates a new panel named "Summary"
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
                   fluidRow(
                     column(3, 
                            uiOutput("pePltFun")),
                     column(3, offset=1, 
                            uiOutput("pePrtPred")) 
                   ),
                   br(),
                   h4("Partial predictions of a single variable"),
                   h5("Select the variable in #2 above."),
                   verbatimTextOutput("pe_predict_var_smry"),
                   br(),
                   h4("Partial prediction of a continuous predictor by a factor."),
                   h5("This plot shows the expected trend line by multiple levels with 95% confidence intervals. Especially helpful in seeing the interaction effect and where lines intersect or diverge."),  
                   h5("For Poisson regression offset(), include the function in #8, e.g., 'do.call('Predict', list(fit1(),'year2'= 1:7, 'procedure', fun=function(x) exp(x)*mean(df()[, 'Pt.Days'']), offset= list(Pt.Days=mean(df()[, 'Pt.Days''])) ))'."),  
                   br(),
                   fluidRow(
                     column(3, 
                            uiOutput("xyplot_x")),
                     column(3, offset=1,
                            uiOutput("xyplot_z")),
                     column(3, offset=1,
                            uiOutput("xyplot_bands"))
                   ),
                   fluidRow(
                     column(3, 
                            uiOutput("xyplot_line_clrs")),
                     column(3, offset=1,
                            uiOutput("xyplot_grp_levs")),
                     column(3, offset=1,
                            uiOutput("xyplot_yes_no"))
                   ),
                   fluidRow(
                     column(3, 
                            uiOutput("xyExtrapo_yes_no")),
                     column(3, offset=1,
                            uiOutput("xy_extrap_box")),
                     column(3, offset=1,
                            uiOutput("xyExtr_X_Val"))
                   ),
                   fluidRow(
                     column(3, 
                            uiOutput("xyplot_Xlim1")),
                     column(3, 
                            uiOutput("xyplot_Xlim2")),
                     column(3, 
                            uiOutput("xyplot_Ylim1")),
                     column(3, 
                            uiOutput("xyplot_Ylim2"))
                   ),
                   br(),
                   plotOutput("xYplot_interaction", height=800, width="100%"),
                   br(),
                   h4("For extrapolated values in a Poisson regression with an offset, specify like this: do.call('Predict', list(fit1(),'year2'= 1:7, 'procedure', fun=function(x) exp(x)*mean(df()[, 'Pt.Days']), offset= list(Pt.Days=mean(df()[, 'Pt.Days'])) ))"),
                   br(),
                   h4("Contrast plots"),
                   h5("This graph shows differences between groups (linear differences, odds ratios, hazard ratios) with 95% confidence intervals. Compare 2 groups on predicted values, especially useful for interactions."),                   
                   h5("This plot compliments the partial prediction plot above. You must run the plot directly above first."),                   
                   fluidRow(
                     column(3,
                            uiOutput("xyplot_con_lev1")),
                     column(3,
                            uiOutput("xyplot_con_lev2")),
                     column(3,
                            uiOutput("xyp_yes_no"))
                   ),
                   fluidRow(
                     column(3,
                            uiOutput("xyplot_con_xlim0")),
                     column(3,
                            uiOutput("xyplot_con_xlim1")),
                     column(3,
                            uiOutput("xyplot_con_ylim0")),
                     column(3,
                            uiOutput("xyplot_con_ylim1"))
                   ),
                   h5("Contrasts require 'factor' data types for groups. Convert variables into factors in the \"Data\" tab."),                   
                   br(),
                   plotOutput("xyplot_contrast_plot", height=700, width="100%"),
                   h6("The portions of the red horizontal line (e.g., at 0 for linear regression, at 1 for logistic regression and Cox PH) corresponds to no significant predictor effect when contained within the 95% CI."),
                   h6("The blue line represents the contrast level you selected (e.g., 'Female')."),
                   h6("For example, women may have higher rates of death before age 58, equal with men from 58-88, and have lower rates after 88 years."),
                   h6("For Poisson regressions with offset(), contrasts are of general rate ratios. To contrast using an average offset, use Predict() in Describe tab."),
                   br(),
                   h5("Contrasts and 95% confidence intervals from the plot above at various percentiles of the continuous predictor. Non-interaction contrasts will be constant."),
                   tableOutput("contrast_quant_table"),
                   br()
          ),    #Creates a new panel named "Summary"
          tabPanel("Nomogram",
                   h4("Hand calculate probabilities with a nomogram"),
                   h4("We use a nomogram that converts each effect in the model to a 0 to 100 scale (e.g., probability for logistic model) or time scale."),
                   br(),
                   h5("Survival. #1: Select 2 times for survival probabilities, #2: Divide to convert into new time periods or keep as 1, #3: Specify time range (e.g., 1:5) for Mean/Median predictions, if #2 > 1, transform."),
                   fluidRow(
                     column(4,
                            uiOutput("nomo_prob_surv_time")),
                     column(4,
                            uiOutput("nomo_trans_time_denom")),
                     column(4,
                            uiOutput("nomo_pred_surv_time_xaxis"))
                   ),
                   br(),
                   h5("Survival model labeling features. #4: Time values listed in predictions, #5: Time periods such as 'days', #6: Change the size of variable names. "),
                   fluidRow(
                     column(4,
                            uiOutput("nomo_surv_time_prob_vals")),
                     column(4,
                            uiOutput("nomo_surv_time_prob_pers")),
                     column(4,
                            uiOutput("nomo_vr_lbl_sz"))
                   ),
                   h5("Update the nomogram for all models. #7: Modify the nomogram or revise X values not able to plot...ap=c(.1,.5,1:5,10,20,30,40), #8: Update various models."),
                   fluidRow(
                     column(4,
                            uiOutput("nomo_up_Fmla")),
                     column(4,
                            uiOutput("nomo_yes"))
                   ),
                   plotOutput("nomo_gram", height = 800, width="100%"),
                   br(),
                   h4("Nomogram output"),
                   verbatimTextOutput("nomogram_smry")
          ),    #Creates a new panel named "Nomogram"                           
          
          tabPanel("Calibration",
                   h4("The reliability of a model, meaning the ability of the model to predict future observations as well as it appeared to predict the responses at hand."),
                   br(),
                   h5("Note: When using multiple imputation a single imputed dataset is created. First indicate which variables are neither transformed nor splined in the 'Impute' tab."),
                   h5("For #3, mean time shown. For #4, select the number of groups that survival models calibration is validated on (see note below). Default is 5 (e.g., sample size/5)."),
                   br(),
                   fluidRow(
                     column(3, 
                            uiOutput("calibrate_type"), 
                            uiOutput("calibrate_surv_quan_n"),
                            uiOutput("BeginCalibrate")
                     ),  
                     column(3, offset=1,
                            uiOutput("calibrate_B_arg_n"),
                            uiOutput("MIForCali") 
                     ),
                     column(3, offset=1,
                            uiOutput("calibrate_surv_time"),
                            uiOutput("calibrate_set_seed")
                     )
                   ),
                   br(),
                   h5("Validation of survival model calibration: Black dots are means of predicted values with 95% CIs. The blue Xs represent bootstrap bias-corrected Kaplan-Meier estimates."),
                   h5("Apparent calibration accuracy obtained by stratifying intervals and plotting the mean predicted value within the interval by the stratum's Kaplan-Meier estimate."),
                   plotOutput("cali_brate", height = 800, width= "100%")
          ),    #Creates a new panel named "Summary"
          
          tabPanel("Validation", 
                   fluidRow(
                     column(3, 
                            uiOutput("validate_type"),
                            uiOutput("validate_set_seed")
                     ),  
                     column(3, offset=1,
                            uiOutput("validate_B_arg_n"),
                            uiOutput("BeginValidate")
                     ),  
                     column(3, offset=1,
                            uiOutput("MIForVali")
                     )),
                   h5("Note: When using multiple imputation a single imputed dataset is created. First indicate which variables are neither transformed nor splined in the 'Impute' tab."),
                   #h6("Note: Confirm the single imputed or original dataframe is loaded in the 'Model builder' tab, depending on your purpose."),
                   br(),
                   h4("Calibration and discrimination indexes (from bootstrapped methods, cross-validated--100 repeats)"),
                   h4("The worth of a model can be judged by how far it goes out on a limb while 
                      still maintaining good calibration. If one model assigns .02 of 
                      the sample to a risk of dying above a predicted value of .9 while 
                      the other model assigns .08 of the sample to the high risk group, 
                      the second model is more discriminating."),
                   h6("Note: Use the \"boot\" method to view the \"Factors Retained in Backwards Elimination\". These are factors found statistically significant in random draws. First 50 bootstrapped results shown."),
                   h6("A Slope much smaller than 1 indicates that the range of observed risks is much smaller than the range of predicted risks. And vice vera."),
                   h6("The formula to convert Dxy to the C-statistic: Dxy/2 + 0.5 (e.g., 0.60/2 + 0.5 = 0.80.)"),
                   verbatimTextOutput("vali_date")),    #Creates a new panel named "Summary"
          
          #############
          tabPanel("Survival", 
                   h4("Survival analysis plots and residuals"),
                   h5("Note: Use splines fits and interactions to test linearity and additivity. Use splines in place of Martingales when considering continuous X tranfsormations."),                   
                   br(),
                   h4("Survival plots"),                   
                   h5("Note: Stratified survival function curves don't include time increment and confidence bands/bars options."),
                   h5("The log-minus-log plot is not applicable to the survival and cumulative hazard function plots. The log-minus-log plot can have negative values, adjust y limit values accordingly."),
                   br(),
                   h5("Set up the survival plot. The default plot (#4 == 'No') is the Kaplan-Meier survival function (#14 == 'Survival')."),
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
                            uiOutput("SurvPltLnClr")),
                     column(3,
                            uiOutput("SurvPltLnWdt")),
                     column(3,
                            uiOutput("SurvPltHrzLn")),
                     column(3,
                            uiOutput("SurvPltVrtLn"))
                   ),
                   h5("For model survival plots, select either 'Survival' or 'Cumulative Incidence' for #14 (i.e., plot not run for 'Cumulative Hazard')."),
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
                   h5("Make up to 5 legends. Select '-1' in #11 to create the legend for a subset."),
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
                            uiOutput("KMSurvPltXlim2")),
                     column(3, 
                            uiOutput("KMSurvPltYlim1")),
                     column(3,
                            uiOutput("KMSurvPltYlim2"))
                   ),
                   fluidRow(
                     column(3, 
                            uiOutput("KMSurvPltLnClr")),
                     column(3,
                            uiOutput("KMSurvPltLnWdt")),
                     column(3,
                            uiOutput("KMSurvPltLgdN")),
                     column(3,
                            uiOutput("KMSurvPltLgdLoc"))
                   ),
                   fluidRow(
                     column(3,
                            uiOutput("KMSurvPltCex")),
                     column(3,
                            uiOutput("Km_Tgt_Line")),
                     column(3,
                            uiOutput("Km_Tm_Line")),
                     column(3,
                            uiOutput("KMSurvPlt_group_levs"))
                   ),
                   
                   br(),
                   plotOutput("km_plot", height = 800, width="100%"),
                   br(),
                   h5("Unconditional survival function. Associated with default survival plot, first plot at the top. Modify restricted mean with KM button #4 directly above."),
                   verbatimTextOutput("KM_SF_Output_UC"), 
                   br(),
                   h5("Stratified survival function. Kaplan-Meier plot above. Modify restricted mean with KM button #4 directly above."),
                   verbatimTextOutput("KM_SF_Output_C"), 
                   br(),
                   ############################## End here
                   
                   h4("Estimated Survival Times"),
                   br(),
                   ## Estimated mean and quantiles in values of Y ##
                   fluidRow(
                     column(3, 
                            uiOutput("surv_binary_X")),
                     column(3,
                            uiOutput("surv_binary_compare"))
                   ),
                   br(),
                   h5("Quantiles: Point estimates and confidence intervals of the effects from the 10th/25th/50th/75th/95th percentiles."),
                   tableOutput("surv_quant_out1"),
                   br(),
                   h5("Mean: Point estimates and confidence intervals for the mean effect."),
                   tableOutput("surv_mean_out1"),
                   br(),
                   h5("Observed outcome means and quantiles."),
                   tableOutput("surv_obsdfmqt_out1"),
                   br(),
                   h4("Probability of survival for specific times"),
                   h5("Includes option to get hazard function for AFT models."),
                   br(),
                   h5("Enter the X values when the data box opens and then close to get the probability. Note: Close the data box to run new models. Defaults are adjusted model values."),
                   fluidRow(
                     column(3, 
                            uiOutput("surv_probability_time")),
                     column(3, 
                            uiOutput("surv_time_probability_hazard_survival")),
                     column(3,
                            uiOutput("surv_time_probability_yesno"))
                   ),
                   br(),
                   h5("Results"),
                   verbatimTextOutput("surv_hazard_survival_function_out"), 
                   br(),
                   
                   ## Download Survival Estimates ##
                   h4("Download Survival Estimates"),
                   br(),
                   fluidRow(
                     column(3, 
                            uiOutput("survival_estimate_time")),
                     column(3, offset=1,
                            uiOutput("survival_estimate_hazard_survival"))
                   ),
                   br(),
                   fluidRow(
                     column(3,
                            uiOutput("survival_estimate_yesno")),
                     column(3, offset=1,
                            uiOutput("surv_est_data_download_name")) ,
                     column(3, offset=1,
                            downloadLink('se_data_download', '5. Click to download estimates.'))
                   ),
                   br(),
                   
                   ###############################
                   h3("Residuals"),
                   br(),
                   h4("Schoenfeld residuals"),
                   h5("Assess the Cox model's proportional hazards assumption."),
                   br(),
                   h5("Schoenfeld residuals test."),
                   tableOutput("schoenfeld_test"),
                   h5("Schoenfeld residuals plot."),
                   h6("Check for a pattern in time. An upwards line may suggest an increasing effect in X over time. For binary predictors (with no interaction term), the top band of dots is failures when X=1, lower band is when X=0"),
                   h6("The dotted green line represents X's coefficient or mean of residuals. You can see how the proportional hazards varies around the average, over time."),
                   uiOutput("Schoenfeld_X"),
                   plotOutput("schoenfeld_plt", height = 800, width="100%"),
                   
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
                   h4("Deviance residuals"),
                   br(),
                   h5("Plot deviance residuals to examine outliers."),
                   h5("In Cox PH models, positive values indicate failure times are sooner than expected and negative values indicate that failure times are longer than expected (or were censored)."),
                   h5("For AFT models, vice versa is true. Deviance are the only residuals available for AFT models."),
                   fluidRow(
                     column(6,
                            uiOutput("dev_res_yesno"))
                   ),
                   plotOutput("prediction_deviance_outlier_run", height = 800, width="100%"),
                   br(),
                   h5("Deviance scatterplot to assess 1) the association between a covariate and unexplained variation, 2) whether to add new covariates, 3) and non-linearity. Plot has a loess smoothed line (in red)."),
                   fluidRow(
                     column(5, 
                            uiOutput("deviance_cov_plot_x")) ,
                     column(5, offset=1,
                            uiOutput("dev_cov_yesno"))
                   ),
                   plotOutput("prediction_deviance_covariate_run", height = 800, width="100%"),
                   
                   br(),
                   h5("Residuals: DFBETAS, Deviance, influential cases, Schoenfeld and Martingale residuals are saved in the download. AFT models only have Deviance residuals."),
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
                   
                   br(),
                   ## Creating time-dependent datasets ##
                   h4("Time-dependent/varying dataset creation for a time-dependent/varying coefficient model."),
                   h5("To create a time-dependent/varying covariate dataset, go to the 'Multi-State' tab."),
                   br(),
                   h5("This creates a time-dependent dataset with an optional interaction term (continuous X*Time) to assess and model accordingly for the proportional hazards assumption."),
                   h5("In #1, enter the variables you want in the dataset"),
                   h5("For categorical X (e.g., 'Intervention'), select 'No' for #3 and skip #5. For continuous X, select 'Yes' for #3."),
                   h5("For step 4, enter the covariate of primary interest. For example, there are 3 'Intervention' coefficients for 0-30, 30-60, 60+ day periods."),
                   h5("When specifying the time-dependent model later in the 'Model builder' tab, use Outcome=tstart, Censor= tstop,event."),
                   h5("For a model using categorical X (as binary 0/1), specify the Intervention*Time period as 'Intervention*strat(tgroup)'. To interpret the 2nd time period (tgroup=2), multiply exp(coefficients): Intervention * 'Intervention * tgroup=2'. The 'Intervention' coefficient is Intervention*tgroup=1. Interpret the coefficient 'as is'."),
                   h5("We may want to add a value to the time value to emphasize parts of the period (e.g., log(time+20) ). If so, set the value in #5 below."),
                   br(),
                   fluidRow(
                     column(3, 
                            uiOutput("time_dependent_predictors")) ,
                     column(3,
                            uiOutput("td_cut_lev")) ,
                     column(3,
                            uiOutput("td_X_by_time_yesno")) ,
                     column(3, 
                            uiOutput("time_dependent_var")) 
                   ), 
                   br(),
                   fluidRow(
                     column(3, 
                            uiOutput("td_log_increment")) ,
                     column(3, 
                            uiOutput("td_data_yes_no")) ,
                     column(3, 
                            uiOutput("td_data_download_name")) ,
                     column(3, 
                            downloadLink('td_data_download', '8. Click to download data.'))
                   ),
                   br(),
                   h5("Remember to save R data frames with the extension '.RData'."),
                   br(),
                   
                   ## Multi-level Cox PH model ##
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
                   h4("Download mixed effects cox model fit"),
                   h5("Note: Save ANY file name with '.RData' at the end."),
                   fluidRow(                           #Wrapping them in a fluidRow provides easy control over  
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
                   h4("Create a multi-state (or time-dependent/varying covariate) model data frame."),
                   br(),
                   h5("This section converts wide format data into long format and produces multiple variables such as start and stop times for various states."),
                   h5("Important: One time variable (e.g., #3) that covers the full range is required (i.e., values of 1:max(Time)). The ID variable in #5 needs to be in sequential order (i.e., no gaps). The time variable created from #2, will take the same name."),
                   h5("If running a time-dependent covariate model, specify the model as 'Surv(tstart, tstop, event) ~ Intervention'. "),
                   h5("'Transient state'means a subject can transition out of that state. 'Terminal state' means once a subject enters that state, departure is not possible (e.g., death)."),
                   h5("Make sure to reverse code (use 'enum') after time-dependent data creation, 'Intervention' at time 1=1, time 2=0 if patients start with the intervention then stop. No code change needed if patients do not start on the intervention (i.e., begin as Intervention==0)."),
                   h5("To create a time-dependent/varying coefficient dataset, go to the 'Survival' tab."),
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
                   h5("Added 95% CIs to survival package summary using the Normal Approximation method. More conservative than the Z test below, see discussion on influence matrix in package documentation."),
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
                   h4("Create and save a multi-state model."),
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
                   h6("Check for a pattern in time. An upwards line may suggest an increasing effect in X over time. For binary predictors (with no interaction term), the top band of dots is failures when X=1, lower band is when X=0."),
                   plotOutput("MSschoenfeld_plt", height = 800, width="100%"),
                   br(),
                   #Model summary for state space figure
                   h4("Summarize output"),
                   h5("Create the model summary to retrieve the state space figure values. Complete this section to get the diagram below."),
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
                   h5("Dudley, R. & Harrell, F. (1993). 'Comparison of analytic models for estimating the effect of clinical factors on the cost of coronary artery bypass graft surgery'. Journal of Clinical Epidemiology, 46(3), 261-271."),
                   br(),
                   h5("The following 5 questions will set up the types of plots and tabled results. Stratification is by 0 vs. >= 1 for 3+ levels."),                   
                   h6("Note: If using the Cox PH model with censoring, censor a non-event (e.g., death = 0)."),                   
                   h6("Note: Not all tables and plots are applicable to a quantile regression (e.g., means)."), 
                   fluidRow(
                     column(3, 
                            uiOutput("dens_plt1_typ")),
                     column(3, offset=1,
                            uiOutput("dens_plt1_x"))
                   ),
                   fluidRow(
                     column(3, 
                            uiOutput("dens_plt1_x_nms")),
                     column(3, offset=1,
                            uiOutput("dens_plt1_x_cstm")),
                     column(3, offset=1,
                            uiOutput("run_dens_plt1"))
                   ),
                   br(),
                   h5("Partial effects plots: Plot the mean or various quantile effects. For quantile regression, get individual plots in the 'Partial PREDs' tab."),
                   plotOutput("cox_prt_prd", height = 700, width="100%"),
                   br(),
                   h5("The following questions are to modify the partial effects plots above."),
                   fluidRow(
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
                   h5("Modify the plot space in #5-8. The colors of the lines are assigned according to the value/group order of the stratification variable (e.g., 0, 1 or Control, Treatment)."),
                   #Cost Density plot
                   fluidRow(
                     column(3, uiOutput("densPltLnClr")),
                     column(3, uiOutput("dens_plot_txt_lbl_sz")),
                     column(3, uiOutput("dns_plot_lab_multi")),
                     column(3, uiOutput("dens_lgd_loc"))
                   ),
                   fluidRow(
                     column(3, uiOutput("dns_Xlim1")),
                     column(3, uiOutput("dns_Xlim2")),
                     column(3, uiOutput("dns_Ylim1")),
                     column(3, uiOutput("dns_Ylim2"))
                   ),
                   br(),
                   h4("The plot and tables below are for stratified results."),
                   br(),
                   h5("We often estimate effects for the 'middle' of the data (e.g., mean), we can also estimate other parts of the data."),
                   h5("These are 5 effects at various quantiles for the stratified variable (e.g., we estimate for most and least expensive costing patients)."),
                   br(),
                   h5("Modify the plot space in #5-8. The colors of the lines are assigned according to the value/group order of the stratification variable (e.g., 0, 1 or Control, Treatment)."),
                   #Cost Density plot
                   fluidRow(
                     column(3, uiOutput("qntPltLnClr")),
                     column(3, uiOutput("qnt_plot_txt_lbl_sz")),
                     column(3, uiOutput("qnt_plot_lab_multi")),
                     column(3, uiOutput("qnt_lgd_loc"))
                   ),
                   fluidRow(
                     column(3, uiOutput("qnt_Xlim1")),
                     column(3, uiOutput("qnt_Xlim2")),
                     column(3, uiOutput("qnt_Ylim1")),
                     column(3, uiOutput("qnt_Ylim2"))
                   ),
                   br(),
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
                            uiOutput("Cxyplot_bands"))
                   ),
                   fluidRow(
                     column(3, 
                            uiOutput("Cxyplot_line_clrs")),
                     column(3, 
                            uiOutput("Cxyplot_grp_levs")),
                     column(3, 
                            uiOutput("Cxyplot_yes_no"))
                   ),
                   fluidRow(
                     column(3, 
                            uiOutput("CxyExtrapo_yes_no")),
                     column(3, offset=1,
                            uiOutput("Cxy_extrap_box")),
                     column(3, offset=1,
                            uiOutput("CxyExtr_X_Val"))
                   ),
                   fluidRow(
                     column(3, 
                            uiOutput("Cxyplot_Xlim1")),
                     column(3, 
                            uiOutput("Cxyplot_Xlim2")),
                     column(3, 
                            uiOutput("Cxyplot_Ylim1")),
                     column(3, 
                            uiOutput("Cxyplot_Ylim2"))
                   ),
                   br(),
                   plotOutput("CxYplot_interaction", height=800, width="100%"),
                   br(),
                   h4("Contrast plots"),
                   h5("This graph shows differences between groups (linear differences, odds ratios, hazard ratios) with 95% confidence intervals. Compare 2 groups on predicted values, especially useful for interactions."),                   
                   h5("This plot compliments the partial prediction plot above. You must run the plot directly above first."),                   
                   fluidRow(
                     column(3,
                            uiOutput("Cxyplot_con_lev1")),
                     column(3,
                            uiOutput("Cxyplot_con_lev2")),
                     column(3,
                            uiOutput("Cxyp_yes_no"))
                   ),
                   fluidRow(
                     column(3,
                            uiOutput("Cxyplot_con_xlim0")),
                     column(3,
                            uiOutput("Cxyplot_con_xlim1")),
                     column(3,
                            uiOutput("Cxyplot_con_ylim0")),
                     column(3,
                            uiOutput("Cxyplot_con_ylim1"))
                   ),
                   h5("Contrasts require 'factor' data types for groups. Convert variables into factors in the \"Data\" tab."),                   
                   br(),
                   plotOutput("Cxyplot_contrast_plot", height=700, width="100%"),
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
                   h4(" Monte Carlo Simulation"),
                   h6(" Note: Open this tab first to unlock the plots in the following tabs. For #7, select a categorical variable."),
                   fluidRow(                           #Wrapping them in a fluidRow provides easy control over  
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
                   h4("Monte Carlo Global Uncertainty and Sensitivity Analysis"),
                   verbatimTextOutput("mc_gsa")
          ),
          
          tabPanel("OAT M/SD plot",                                #Creates a new panel named "Test Plot"
                   h4("Morris One-at-a-Time plot of Yhat means by Yhat SDs for each predictor (holding other predictors constant)"),
                   h5("High means indicate high Elementary Effects. High SDs may indicate non-linearity and/or interactions."),
                   h5("Red line= sample mean; Blue line= sample median. Lines not always visible."),
                   #         fluidRow(                           #Wrapping them in a fluidRow provides easy control over  
                   plotOutput("oat_mn_sd", height = 600, width="100%")   
                   #         )
          ),              
          
          tabPanel("Tornado plot",                                #Creates a new panel named "Test Plot"
                   h4("Tornado plot of absolute value standardized regression coefficients."),
                   h5("Wider bands indicate that the outcome is more sensitive to the predictor (i.e., it's more important)."),
                   #         fluidRow(                           #Wrapping them in a fluidRow provides easy control over  
                   plotOutput("tornadoplot", height = 800, width="100%")   
                   #         )
          ),              
          
          tabPanel("Cutoff plot",
                   h4("This plot allows us to see the proportion of expected values above/below a cutoff"),
                   fluidRow(
                     column(5,
                            uiOutput("cutoff_yes")
                     ),
                     column(5, 
                            uiOutput("cutoff_val")
                     )),
                   plotOutput("cutoffplot", height = 600, width="100%"),
                   verbatimTextOutput("cutoff_smry"),
                   h5("Descibe the simulated values"),
                   verbatimTextOutput("desc_YhatPlotRslt")
          ),    
          
          tabPanel("Cobweb plot",                                #Creates a new panel named "Test Plot"
                   h4("Cobeweb plot of best 1% or 5% of predicted outcome scores (\"best\" can be top or bottom values.)"),
                   h5("Shows responses of predictors with best predicted outcome values. Narrower spread indicates greater predictor importance."),
                   h5("Note: Requires at least 2 predictors."),
                   h6("For all categorical predictor models, increase #2 until lines appear. Lines won't reach 100 on y-axis because of limited number of unique Y predictions. Select bottom 1% or 5%, gray lines will indicate highest values."),
                   br(),
                   fluidRow(                           #Wrapping them in a fluidRow provides easy control over  
                     column(3, 
                            uiOutput("top_bottom_5")),
                     column(3, 
                            uiOutput("cobweb_seq"))
                   ),
                   plotOutput("cobwebplot", height=800, width="100%"),
                   h5("Response level names and \"approximate\" associated percentiles for binomial and categorical factors (in order)."),
                   h5("Binomial and categorical percentiles are 1/N *100 (e.g., 3 levels= 1/3, 2/3, 3/3= 33, 68, 100)."),
                   verbatimTextOutput("cobweb_lev_nm")
          ),              
          
          
          #### META ANALYSIS ##########
          tabPanel("Meta" ,
                   h4("Meta analysis for binary or continuous outcomes"),
                   h6("Demo values are used as defaults. For a continuous outcome example, data(Fleiss93Cont)."),
                   fluidRow(
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
                   h4("One- and two-sample binomial proportion tests or a one- and two-sample and paired t-tests"),
                   h5("One-sample tests compare data with a hypothetical value (e.g., heads from a coin toss vs. 0.50 rate). Two-sample tests compare independent groups (e.g., group 1's age vs. group 2's age). Paired t-test compares dependent samples (e.g., patient's blood pressure time 1 vs time 2)."),
                   br(),
                   h4("Proportion tests"),
                   fluidRow(
                     column(3,
                            uiOutput("prp_tst_y")),
                     column(3, 
                            uiOutput("prp_tst_x")),
                     column(3, 
                            uiOutput("prp_tst_1_smpl")),
                     column(3, 
                            uiOutput("prp_tst_prp"))
                   ),
                   fluidRow(  
                     column(3, 
                            uiOutput("prp_tst_alt")),
                     column(3,
                            uiOutput("prp_tst_CI")),
                     column(3, 
                            uiOutput("prp_tst_yts")),
                     column(3, 
                            uiOutput("prp_tst_YN"))
                   ),
                   verbatimTextOutput("proportion_test_out"),
                   br(),
                   h4("t-tests"),
                   fluidRow(
                     column(3,
                            uiOutput("t_tst_y")),
                     column(3, 
                            uiOutput("t_tst_x")),
                     column(3, 
                            uiOutput("t_tst_1_smpl")),
                     column(3, 
                            uiOutput("t_tst_mn"))
                   ),
                   fluidRow(  
                     column(3, 
                            uiOutput("t_tst_alt")),
                     column(3, 
                            uiOutput("t_tst_CI")),
                     column(3,
                            uiOutput("t_tst_pr")),
                     column(3, 
                            uiOutput("t_tst_YN")) 
                   ),
                   verbatimTextOutput("t_test_out"),
                   br(),
                   ##
                   br(),
                   h4("Power analysis using a two-sample binomial proportion test or a one- or two-sample t-test"),
                   h5("Demo values are used as defaults, including harmonic mean sample sizes for uneven group Ns."),
                   h4("Binary outcomes"),
                   fluidRow(
                     column(3,
                            uiOutput("power_bin")),
                     column(3, offset=1,
                            uiOutput("p1_bin")),
                     column(3, offset=1,
                            uiOutput("sig_bin"))
                   ),
                   fluidRow(
                     column(3,
                            uiOutput("n_bin")),
                     column(3, offset=1,
                            uiOutput("p2_bin")),
                     column(3, offset=1,
                            uiOutput("one_two_side_bin"))
                   ),
                   fluidRow(
                     column(3, 
                            uiOutput("pwr_smp_bin"))
                   ),
                   br(),
                   h4("Continuous outcomes"),
                   fluidRow(
                     column(3,
                            uiOutput("power_con")),
                     column(3, offset=1,
                            uiOutput("delta_con")),
                     column(3, offset=1,
                            uiOutput("sig_con"))
                   ),
                   fluidRow(
                     column(3,
                            uiOutput("n_con")),
                     column(3, offset=1,
                            uiOutput("sd_Con")),
                     column(3, offset=1,
                            uiOutput("type_con"))
                   ),
                   fluidRow(
                     column(3, 
                            uiOutput("one_two_side_con")),
                     column(3, offset=1,
                            uiOutput("pwr_smp_con"))
                   ),
                   br(),
                   h4("Do you have uneven group sizes?"),
                   fluidRow(
                     column(3,
                            uiOutput("grp1_n")),
                     column(3, offset=1,
                            uiOutput("grp2_n")),
                     column(3, offset=1,
                            uiOutput("harmonic_n"))
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
                            uiOutput("CIy")),
                     column(3, offset=1,
                            uiOutput("CIx")),
                     column(3, offset=1,
                            uiOutput("Ci_Choice_Type")),
                   ),
                   fluidRow(   
                     column(3, 
                            uiOutput("Ci_Conf_Lev")),
                     column(3, offset=1,
                            uiOutput("Ci_Tgt_Line")),
                     column(3, offset=1,
                            uiOutput("Ci_create"))
                   ),
                   fluidRow(   
                     column(3, 
                            uiOutput("ci_plot_ln_clrs")),
                     column(3, offset=1,
                            uiOutput("ci_plot_pt_clrs")),
                     column(3, offset=1,
                            uiOutput("Ci_Alpha_Num"))
                   ),
                   fluidRow(   
                     column(3, 
                            uiOutput("Ci_create_tot_bar")),
                     column(3, offset=1,
                            uiOutput("ci_plot_tot_bar_clrs")),
                     column(3, offset=1,
                            uiOutput("ci_plot_lab_multi"))
                   ),
                   fluidRow(   
                     column(3, 
                            uiOutput("ci_plot_Xlim1")),
                     column(3, offset=1,
                            uiOutput("ci_plot_Xlim2")),
                     column(3, offset=1, 
                            uiOutput("ci_plot_rnd_decs"))
                   ),
                   plotOutput("Plot_Ci_output", height = 800, width="100%"),
                   h6("Note: You can sort alphabetically by the factor level name or numerically by the point estimate. Left side = factor level, right side = point estimate."),
                   verbatimTextOutput("Cidf_output"),
                   h6("Note: The values above are point estimates and confidence limits that are sorted alphabetically and numerically. Poisson group and pairwise comparisons assume normal approximation."),
                   br(),
                   h4("Performance of groups over time (need >= 6 time points for spline knots, use 'straight trend lines' when < 6)"),
                   h5("This plot has straight or smoothed spline trajectories, with or without confidence bands. Smoothed \"trend\" lines may not have cooridnate values that equal rates."),
                   br(),
                   fluidRow(   
                     column(3, 
                            uiOutput("FCIy")),
                     column(3, 
                            uiOutput("FCIx")),
                     column(3, 
                            uiOutput("FCIz")),
                     column(3, 
                            uiOutput("FCIzInc"))
                   ),
                   fluidRow(   
                     column(3, 
                            uiOutput("fciplot_grp_levs")),
                     column(3, 
                            uiOutput("FCi_Choice_Type")),
                     column(3, 
                            uiOutput("FCi_Conf_Lev")),
                     column(3, 
                            uiOutput("FCI_bands"))
                   ),
                   fluidRow(   
                     column(3, 
                            uiOutput("fci_plot_ln_clrs")),
                     column(3, 
                            uiOutput("fci_plot_ln_typ")),
                     column(3, 
                            uiOutput("fci_plot_ln_wdth")),
                     column(3, 
                            uiOutput("fci_plot_TgtTpt_ln_wdth"))
                   ),
                   fluidRow(   
                     column(3, 
                            uiOutput("FCi_ovral_line")),
                     column(3, 
                            uiOutput("FCi_Tgt_Line")),
                     column(3, 
                            uiOutput("FCi_Tm_Pt_Line")),
                     column(3, 
                            uiOutput("fci_plot_txt_lbl_sz"))
                   ),
                   fluidRow(   
                     column(3, 
                            uiOutput("fci_plot_ovral_ln_clrs")),
                     column(3, 
                            uiOutput("fci_plot_tgt_ln_clrs")),
                     column(3, 
                            uiOutput("fci_plot_time_pt_ln_clrs")),
                     column(3, 
                            uiOutput("FCi_create"))
                   ),
                   fluidRow(   
                     column(3, 
                            uiOutput("FCi_strght_ln")),
                     column(3, 
                            uiOutput("FCI_nk_knots")),
                     column(3, 
                            uiOutput("FCI_plot_lab_multi")),
                     column(3, 
                            uiOutput("ITSA_create"))
                   ),
                   fluidRow(   
                     column(3, 
                            uiOutput("ITSA_I.x")),
                     column(3, 
                            uiOutput("ITSA_I.trt")),
                     column(3, 
                            uiOutput("ITSA_I.tm.pt")),
                     column(3, 
                            uiOutput("itsa_lgd_loc"))
                   ),
                   fluidRow(   
                     column(3, 
                            uiOutput("FCI__Xlim1")),
                     column(3, 
                            uiOutput("FCI__Xlim2")),
                     column(3, 
                            uiOutput("FCI__Ylim1")),
                     column(3, 
                            uiOutput("FCI__Ylim2"))
                   ),
                   h5("Modify the plot space in #29-32. Enter values into 'c()' for #14 and #15 when needed, separate values with ',' (e.g., c(1, 2) ). For single time point data, use 'Yes: Aggregated data only' in #21."),
                   h5("For an Interrupted Time Series Analysis, run the model and modify #1-3 & #24-28 and other options (e.g., leave #26 blank if there is no control group, add colors to #9 for lines to appear). "),
                   h5("To see the ITSA graph's data, use this command in the 'Describe' tab: plot_itsa_DF(). Use 'contrast(fit1(), list(zt=14, zxt89=1), list(zt=13, zxt89=0))' to see fewer smoked cigs in the intervention period."),
                   h5("Note: The colors of the lines are assigned according to the group order of the 'point estimate and confidence interval' output below."),
                   br(),
                   plotOutput("Plot_Fci_output", height = 800, width="100%"),
                   h5("These are point estimates and confidence intervals. These may not match up with smoothed lines. x_lev and z_lev match with #2 and #3 above."),
                   tableOutput("time_ci_out1"),
                   h5("Overall rates. Uses all data and used for the 'overall group trend line'."),
                   tableOutput("all_time_ci_out1")
          ),
          
          ## Bayesian Analysis ##
          tabPanel("Bayesian",
                   h3("Check diagnostics, examine parameters, and graph the hierarchical estimation of Bayesian models."),
                   br(),
                   h4("Some functions are from Kruschke, J. (2015). 'Doing Bayesian Data Analysis: A Tutorial with R, JAGS, and Stan (2nd Ed.)'."),
                   br(),
                   h5("Distribution choices below refer to the likelihood's distribution. Enter the data object name in #1 of the 'Model Builder' tab for some sections."),
                   br(),
                   h4("Enter the name of the Coda object with your MCMC simulations.'"),
                   fluidRow(   
                     column(3, 
                            uiOutput("dbdaCodaObj"))
                   ),
                   br(),
                   # Diagnostics #
                   h4("Run diagnostics on the parameters from the MCMC simulations."),
                   fluidRow(   
                     column(3, 
                            uiOutput("select_dbda_diag_par")),
                     column(3, offset=1,
                            uiOutput("DBDA_Diag_YN"))
                   ),
                   br(),
                   plotOutput("plotDbdaDiag", height = 800, width="100%"),
                   h3("Bayesian posterior marginal distributions with Highest Density Intervals"),
                   br(),
                   h4("Select parameters, comparative values, ROPEs (regions of practical equivalence), and display setings."),
                   h5("Make contrasts between single or multiple groups by selecting multiple parameters in #1 and/or #3."),
                   br(),
                   fluidRow(   
                     column(3, 
                            uiOutput("dbdaPostPlot1")),
                     column(3, 
                            uiOutput("dbdaPostCompareParYN")),
                     column(3, 
                            uiOutput("dbdaPostPlot2")),
                     column(3, 
                            uiOutput("dbdaPostCenTen"))
                   ),
                   fluidRow(   
                     column(3, 
                            uiOutput("dbdaPostCredibleMass")),
                     column(3, 
                            uiOutput("dbdaPostROPEYN")),
                     column(3, 
                            uiOutput("dbdaPostRopeVal1")),
                     column(3, 
                            uiOutput("dbdaPostRopeVal2"))
                   ),
                   fluidRow(   
                     column(3, 
                            uiOutput("dbdaPostLabMulti")),
                     column(3, 
                            uiOutput("dbdaPostMainTtl")),
                     column(3, 
                            uiOutput("dbdaPostXlab")),
                     column(3, 
                            uiOutput("dbdaPostYlab"))
                   ),
                   fluidRow(   
                     column(3, 
                            uiOutput("dbdaPostShowCur")),
                     column(3, 
                            uiOutput("dbdaPlotLineCol")),
                     column(3, 
                            uiOutput("dbdaPostXaxisLims")),
                     column(3, 
                            uiOutput("dbdaPostPlaceHDIText"))
                   ),
                   fluidRow(   
                     column(3, 
                            uiOutput("dbdaPostCompValYN")),
                     column(3, 
                            uiOutput("dbdaPostCompVal")),
                     column(3, 
                            uiOutput("dbdaPostRun")),
                     column(3, 
                            uiOutput("dbdaPostEffSize"))
                   ),
                   br(),
                   plotOutput("plotDbdaPosteriorDistribution", height = 800, width="100%"),
                   h5("Select 2 parameters to compare their difference (e.g., Par. 1 - Par. 2). View 'effect size' by selecting the distribution in #20. Proportions: 0.20= small, 0.50= medium, 0.80= large (Cohen, 1988)."),
                   br(), 
                   h3("Posterior Predictive Check for groups"),
                   br(),
                   h4("Check how posterior lines fit data from Normal, Log-normal, Skew-normal, t, Weibull, or Gamma distributions."),
                   h4("Load dataset in 'Builder' tab and MCMC simumlations above."),
                   h5("Questions 6-8 are for estimating the outcome's distribution (ignore 9-11). Estimated lines are placed over histograms of Y. Use 'scale' in #7 for Weibull."),
                   h5("Questions 9-11 are for trend lines (ignore 6-8), denoted analogously to 'Ordinary Least Squares' and Logistic models with polynomial terms ('Linear', 'Quadratic', 'Cubic')."),
                   h5("For quadratic and cubic polynomial models, enter the variables/parameters of #9 & #10 in this order: Linear, quadratic, cubic, all other numerical variables/paramters in the same order."),
                   h5("The Difference-in-Difference (DID) graph requires variable/parameter entry of #9 & #10 in this order: (Intercept), Time, Intervention, Time*Intervention, all other numerical variables/paramters in the same order."),
                   br(),
                   fluidRow(   
                     column(3, 
                            uiOutput("dbdaPostCheckDist")),
                     column(3, 
                            uiOutput("dbdaPostCheckY")),
                     column(3, 
                            uiOutput("dbdaPostCheckX")),
                     column(3, 
                            uiOutput("dbdaPostCheckGenGroups"))
                   ),
                   fluidRow(   
                     column(3, 
                            uiOutput("dbdaPostCheckLevX")),
                     column(3, 
                            uiOutput("dbdaPostCheckParMn")),
                     column(3, 
                            uiOutput("dbdaPostCheckParSD")),
                     column(3, 
                            uiOutput("dbdaPostCheckParNu"))
                   ),
                   fluidRow(   
                     column(3, 
                            uiOutput("dbdaPartPredX")),
                     column(3, 
                            uiOutput("dbdaPartPredPars")),
                     column(3, 
                            uiOutput("dbdaPartPredData")),
                     column(3, 
                            uiOutput("dbdaPostCheckNumPL"))
                   ),
                   fluidRow(   
                     column(3, 
                            uiOutput("dbdaPostCheckMainTtl")),
                     column(3, 
                            uiOutput("dbdaPostCheckXlab")),
                     column(3, 
                            uiOutput("dbdaPostCheckBarCol")),
                     column(3, 
                            uiOutput("dbdaPostCheckLineCol")),
                   ),
                   fluidRow(   
                     column(3, 
                            uiOutput("dbdaPostCheckNumHB")),
                     column(3, 
                            uiOutput("dbdaPostCheckLabMulti")),
                     column(3, 
                            uiOutput("dbdaPostCheckXaxisLims")),
                     column(3, 
                            uiOutput("dbdaPostCheckYaxisLims"))
                   ),
                   fluidRow(   
                     column(3, 
                            uiOutput("dbdaPostCheckMinVal")),
                     column(3, 
                            uiOutput("dbdaPostCheckXaxisPoint")),
                     column(3, 
                            uiOutput("dbdaPostCheckPointCol")),
                     column(3, 
                            uiOutput("dbdaPostCheckAddLeg"))
                   ),
                   fluidRow(   
                     column(3, 
                            uiOutput("dbdaPostCheckLgdLoc")),
                     column(3, 
                            uiOutput("dbdaPostCheckRndPlc")),
                     column(3, 
                            uiOutput("dbdaPostCheckRun"))
                   ),
                   br(),
                   plotOutput("plotDbdaPostCheckGroup", height = 800, width="100%"),
                   br(),
                   h5("Idenitfy mean and SD parameters for each group (e.g., log-normal model mean= muOfLogY[1], SD= sigmaOfLogY[1])."),
                   h5("X-limits can remain blank if the minimum value is specified. Select 'No' for #22 when there is only 1 group for (log-)normal distributions."),
                   h5("For single sample t-distribution estimation, use 't: 1 group', for multiple, use 't: ANOVA'. #8's V parameter is a t-distribution's normality/degrees of freedom. Expand or narrow t's tails by changing the percentage coverage in #21 (e.g., 95= 0.025 - 0.975)."),
                   br(),
                   h3("Hierarchical model: Create a summary of the posterior distribution."),
                   br(), 
                   h4("List the outcome, group, category, and parameter names for (non)hierarchical models. "),
                   h4("Model level refers to the hierarchy: Level-1 = non-hierarhical, Level-2 = group hierarchy (e.g., baseball player), level-3 = categorical hierarchy (e.g., baseball position)."),
                   h4("First, load the source data (aggregated or not) in the 'Model Builder' tab. Make sure factors remain constant in raw and aggregated data."),
                   h5("Make distinct spelling of Theta (low-level 'average' name) and Omega (higher-level 'average' name) names because of character matching (e.g., 'muOfY' & 'muOfOY' instead of 'muOfY' & 'muOfYO')."),
                   br(),
                   fluidRow(   
                     column(3, 
                            uiOutput("dbdaPostSumLev")),
                     column(3, 
                            uiOutput("dbdaPostSumY")),
                     column(3, 
                            uiOutput("dbdaPostSumX1")),
                     column(3, 
                            uiOutput("dbdaPostSumX2"))
                   ),
                   fluidRow(   
                     column(3, 
                            uiOutput("dbdaPostSumTheta")),
                     column(3, 
                            uiOutput("dbdaPostSumOmega2")),
                     column(3, 
                            uiOutput("dbdaPostSumOmega3")),
                     column(3, 
                            uiOutput("dbdaPostSumCenTen"))
                   ),
                   fluidRow(   
                     column(3, 
                            uiOutput("dbdaPostSumDist")),
                     column(3, 
                            uiOutput("dbdaPostSumCredibleMass")),
                     column(3, 
                            uiOutput("dbdaPostSumAggrYN")),
                     column(3, 
                            uiOutput("dbdaPostSumAggrN"))
                   ),
                   fluidRow(   
                     column(3, 
                            uiOutput("dbdaPostSumRun")),
                     column(3, 
                            uiOutput("dbdaPostSumStructYN"))
                   ),
                   br(),
                   h4("Structure of posterior summary"),
                   verbatimTextOutput("structure_dbda_posterior_summary"),  
                   br(),
                   h5("To view the result, go to the 'Describe' tab and run this command: results_dbda_posterior_summary() ."),
                   br(), 
                   h3("Graph the (non-)Hierarchical Estimation of 1, 2, and 3 level models"),
                   br(),
                   h4("View different level parameters, select subsets, and modify the graph."),
                   fluidRow(   
                     column(3, 
                            uiOutput("dbdaHierlphaNum")),
                     column(3, 
                            uiOutput("dbdaHierViewGroup3")),
                     column(3, 
                            uiOutput("dbdaHierViewSub")),
                     column(3, 
                            uiOutput("dbdaHierSpecGroup"))
                   ),
                   fluidRow(   
                     column(3, 
                            uiOutput("dbdaHierRoundVals")),
                     column(3, 
                            uiOutput("dbdaHierLineCol")),
                     column(3, 
                            uiOutput("dbdaHierPointCol")),
                     column(3, 
                            uiOutput("dbdaHierObsRateCol"))
                   ),
                   fluidRow(   
                     column(3, 
                            uiOutput("dbdaHierTarLine")),
                     column(3, 
                            uiOutput("dbdaHierTarCol")),
                     column(3, 
                            uiOutput("dbdaHierTotalBar")),
                     column(3, 
                            uiOutput("dbdaHierTotalBarCol"))
                   ),
                   fluidRow(   
                     column(3, 
                            uiOutput("dbdaHierXlim1")),
                     column(3, 
                            uiOutput("dbdaHierXlim2")),
                     column(3, 
                            uiOutput("dbdaHierAddLeg")),
                     column(3, 
                            uiOutput("dbdaHierLgdLoc"))
                   ),
                   fluidRow(   
                     column(3, 
                            uiOutput("dbdaHierLabMulti")),
                     column(3, 
                            uiOutput("dbdaHierLineMulti")),
                     column(3, 
                            uiOutput("dbdaHierRun"))
                   ),
                   br(),
                   h4("Hierarchical Estimation"),
                   plotOutput("plotDbdaHierEstimation", height = 800, width="100%"),
                   br(),
                   h5("Choose between level-2 groups (e.g., baseball players) and level-3 categories (e.g., positions)."),
                   br(),
                   h4("Predicted Probability Greater than a Specific Value"),
                   br(),
                   h5("Select values from a Beta, Normal, Log-normal, Skew-normal, t, Weibull, or Gamma distribution to calculate the predicted probabilities above that value."),
                   h5("Select quantile values from a Beta, Normal, Log-normal, Skew-normal, t, Weibull, or Gamma distribution to calculate the predicted percentile for that value."),
                   h5("Skew-normal calculations are slow. Use the 'scale' instead of the lambda parameter for Weibull."),
                   br(),
                   fluidRow(   
                     column(3, 
                            uiOutput("dbdaPropGtPar1")),
                     column(3, 
                            uiOutput("dbdaPropGtPar2")),
                     column(3, 
                            uiOutput("dbdaPropGtPar3")),
                     column(3, 
                            uiOutput("dbdaPropGtDist"))
                   ),
                   fluidRow(   
                     column(3, 
                            uiOutput("dbdaPropGtCenTen")),
                     column(3, 
                            uiOutput("dbdaPropGtYval")),
                     column(3, 
                            uiOutput("dbdaPropGtQval")),
                     column(3, 
                            uiOutput("dbdaPropGtRun"))
                   ),
                   br(),
                   h5("'Est.Prop.GT.Y'= Probability of value > Y; 'Est.Quantile.Y'= Percentile associated with your selected value; 'Est.Mean.Beta'= Mean from Beta distr. shape parameters."),
                   h5("'Est.Effect.Size.2Y'= Posterior effect size between probabilities of 2 Y values from the Beta distribution (e.g., if Beta Prob > Y when 0.61=0.69 and 0.70=0.37, then effect size= 0.57). Or simply the effect sizes between the 2 proportions from non Beta distributions (i.e., not from a posterior)."),
                   h5("Consider using the median central tendency if probabilities appear unlikely because modes can randomly be extreme values."),
                   verbatimTextOutput("dbdaPropGt_output"),
                   br(),
                   h5("If using the Beta distribution, the predicted mean is provided. Beta values require posteriors to calculate shape values (e.g., 'theta' and 'omega' from a hierarchical estimation)."),
                   br()
                   
          ) #End of Bayesian panel  
          ###
          
          ############## TEST SECTION #############################
          #, #THIS COMMA IS COMMENTED OUT IN CASE I EVER NEED THE TEST FUNCTION BELOW    
          
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
  