files.sources <- list.files("R/", full.names = TRUE)
sapply(files.sources, source)

options(marginaleffects_safe = FALSE)

# Define server logic
server <- function(input, output, session) {
  
  # Trigger modal helper dialogs
  observe_helpers(help_dir = "help_mds", withMathJax = TRUE)
  
  # Example data table in tutorial
  
  ex_dt <- reactive({
    read.xlsx("www/eg_grwth_data.xlsx")
  })
  
  # Render the example data table
  output$ex_dt <- renderDT({
    req(ex_dt())
    datatable(ex_dt(), caption = "Example dataset in long format. The uploaded file
              must have at least the four columns with the sample IDs, time points,
              treatments, and tumor measurements, but additional columns can be
              present in the table and which will be ignored for the analysis.")
  })
  
  # Synergy results in tutorial
  
  ex_syn <- reactive({
    read.xlsx("www/synergy_results_2025-01-24.xlsx")
  })
  
  # Render example synergy results
  
  output$ex_syn <- renderDT({
    req(ex_syn())
    datatable(ex_syn(), caption = "Synergy results table from the example data. 'Model' column indicates the synergy reference model used.
              'Metric' refers to the synergy metric used to report the results, either the combination index (CI), or the synergy score (SS).
              'Estimate', 'lwr', and 'upr' columns indicate the estimated value of the corresponding CI or SS, and the lower and upper limit of the 95% confidence interval, respectively.
              'pval' indicates the associated p-value for the drug combination effect and 'Time' indicates the time point associated to the results.")
  })
  
  # Synergy results in tutorial
  
  ex_syn <- reactive({
    read.xlsx("www/synergy_results_2025-01-24.xlsx")
  })
  
  # Render example synergy results
  
  output$ex_syn <- renderDT({
    req(ex_syn())
    datatable(ex_syn(), caption = "Synergy results table from the example data. 'Model' column indicates the synergy reference model used.
              'Metric' refers to the synergy metric used to report the results, either the combination index (CI), or the synergy score (SS).
              'Estimate', 'lwr', and 'upr' columns indicate the estimated value of the corresponding CI or SS, and the lower and upper limit of the 95% confidence interval, respectively.
              'pval' indicates the associated p-value for the drug combination effect and 'Time' indicates the time point associated to the results.")
  })
  
  # Outliers for tutorial
  
  ex_outlier <- reactive({
    read.csv("www/residual_outliers_2025-01-24.csv", row.names = 1)
  })
  
  # Render outliers table
  
  output$ex_outlier <- renderDT({
    req(ex_outlier())
    datatable(ex_outlier(), caption = "Table with the identified potential outlier observations.")
  })

  # First tab: data upload and model fitting ----
  # Reactive expression to read the data
  data <- reactive({
    req(input$file)
    file <- input$file
    ext <- tools::file_ext(file$name)
    
    # Read the file based on extension
    switch(ext,
           txt = read.delim(file$datapath, sep = "\t"),
           csv = read.csv(file$datapath),
           xlsx = read_excel(file$datapath),
           validate("Invalid file; Please upload a .txt, .csv, or .xlsx file")
    )
  })
  
  # Dynamically generate UI for selecting the "Sample" column
  output$sample_selector <- renderUI({
    req(data())
    selectInput("sample_col", "Select 'Sample' Column", selected = "Mouse",choices = names(data()), multiple = FALSE)
  })
  
  # Dynamically generate UI for selecting the "Time" column
  output$time_selector <- renderUI({
    req(data())
    selectInput("time_col", "Select 'Time' Column", selected = "Day",choices = names(data()), multiple = FALSE)
  })
  
  # Dynamically generate UI for selecting the "Treatment" column
  output$treatment_selector <- renderUI({
    req(data())
    selectInput("treatment_col", "Select 'Treatment' Column", selected = "Treatment",choices = names(data()), multiple = FALSE)
  })
  
  # Dynamically generate UI for selecting the "TV" column
  output$tv_selector <- renderUI({
    req(data())
    selectInput("tv_col", "Select 'Tumor Measurements' Column", selected = "TumVol", choices = names(data()), multiple = FALSE)
  })
  
  # Update the value of `time_start` and `time_end` based on the maximum value of the Time column
  observeEvent(input$time_col, {
    req(input$time_col)
    
    # Extract the time column values
    time_values <- data()[[input$time_col]]
    
    # Update `time_start` numeric input with the max value from the selected time column
    updateNumericInput(session, "time_start", value = min(time_values, na.rm = TRUE))
    
    # Update `time_end` numeric input with the max value from the selected time column
    updateNumericInput(session, "time_end", value = max(time_values, na.rm = TRUE))
  })
  
  # Dynamically generate UI for selecting Control treatment name
  
  output$control_group <- renderUI({
    req(input$treatment_col)
    req(data())
    # Extract treatment names
    trt_names <- data()[[input$treatment_col]]
    # Select input
    selectInput("control_group",
                "Control Group Name:",
                choices = unique(trt_names),
                multiple = FALSE)
  })
  
  # Dynamically generate UI for selecting Drug A treatment name
  
  output$drug_a <- renderUI({
    req(input$treatment_col)
    req(data())
    # Extract treatment names
    trt_names <- data()[[input$treatment_col]]
    # Select input
    selectInput("drug_a",
                "Drug A Group Name:",
                choices = unique(trt_names),
                multiple = FALSE)
  })
  
  # Dynamically generate UI for selecting Drug B treatment name
  
  output$drug_b <- renderUI({
    req(input$treatment_col)
    req(data())
    # Extract treatment names
    trt_names <- data()[[input$treatment_col]]
    # Select input
    selectInput("drug_b",
                "Drug B Group Name:",
                choices = unique(trt_names),
                multiple = FALSE)
  })
  
  # Dynamically generate UI for selecting Drug C treatment name (if present)
  
  output$drug_c <- renderUI({
    req(input$treatment_col)
    req(data())
    # Extract treatment names
    trt_names <- data()[[input$treatment_col]]
    # Select input
    selectInput("drug_c",
                "Drug C Group Name (if present; leave empty otherwise):",
                choices = c(unique(trt_names), ""), 
                selected = "",
                multiple = FALSE)
  })
  
  # Dynamically generate UI for selecting Control treatment name
  
  output$combination <- renderUI({
    req(input$treatment_col)
    req(data())
    # Extract treatment names
    trt_names <- data()[[input$treatment_col]]
    # Select input
    selectInput("combination",
                "Combination Group Name (Drug A + Drug B [+ Drug C, if present]):",
                choices = unique(trt_names),
                multiple = FALSE)
  })
  
  # Reactive to run the analysis when the button is clicked
  model_results <- eventReactive(input$run_analysis, {
    req(
      data(),
      input$sample_col,
      input$time_col,
      input$treatment_col,
      input$tv_col
      )
    
    # Get user inputs
    sample_id <- input$sample_col
    time <- input$time_col
    treatment <- input$treatment_col
    tumor_vol <- input$tv_col
    trt_control <- input$control_group
    drug_a <- input$drug_a
    drug_b <- input$drug_b
    drug_c <- input$drug_c
    if (drug_c == "") {
      drug_c <- NA
    }
    combination <- input$combination
    time_start <- input$time_start
    time_end <- input$time_end
    min_observations <- input$min_observations
    show_plot <- input$show_plot
    tum_vol_0 <- input$tum_vol_0
    maxIter <- input$maxIter
    msMaxIter <- input$msMaxIter
    niterEM <- input$niterEM
    msMaxEval <- input$msMaxEval
    opt <- input$opt
    
    weights <- NULL
    
    if (input$model_heteroscedasticity == TRUE &&
        input$varIdent == "SampleID") {
      weights <- nlme::varIdent(form = ~1|SampleID)
    }
    
    if (input$model_heteroscedasticity == TRUE &&
        input$varIdent == "Time") {
      weights <- nlme::varIdent(form = ~1|Time)
    }
    
    if (input$model_heteroscedasticity == TRUE &&
        input$varIdent == "Treatment") {
      weights <- nlme::varIdent(form = ~1|Treatment)
    }
    

    # Fit the model using getRTV and lmmModel
    withCallingHandlers({
      result <- tryCatch({
        lmmModel(
          data = data(),
          sample_id = sample_id,
          time = time,
          treatment = treatment,
          tumor_vol = tumor_vol,
          trt_control = trt_control,
          drug_a = drug_a,
          drug_b = drug_b,
          drug_c = drug_c,
          combination = combination,
          time_start = time_start,
          time_end = time_end,
          min_observations = min_observations,
          show_plot = show_plot,
          tum_vol_0 = tum_vol_0,
          maxIter = maxIter,
          msMaxIter = msMaxIter,
          niterEM = niterEM,
          msMaxEval = msMaxEval,
          opt = opt,
          weights = weights
        )
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error", duration = 30)
        NULL
      })
    }, warning = function(w) {
      showNotification(paste("Warning:", w$message, collapse = "."), type = "warning", duration = 30)
      invokeRestart("muffleWarning")
    })
  })
  
  # Render the data table
  output$dataTable <- renderDT({
    req(data())
    datatable(data())
  })
  
  # Obtain model estimates
  
  modelEstimates <- reactive({
    req(model_results())
    if (length(nlme::fixef(model_results())) == 5){
      lmmModel_estimates(model_results()) %>% rename(
        "SD random effects" = "sd_ranef",
        "SD residuals" = "sd_resid"
      )
    } else {
      lmmModel_estimates(model_results()) %>% rename(
        "SD random effects" = "sd_ranef",
        "SD residuals" = "sd_resid"
      )
    }
    
  })
  
  # Render the model estimates
  output$model_estimates <- renderTable({
    req(modelEstimates())
  }, striped = TRUE, digits = 5, caption = "Estimates calculated for the model. The
  'Control', ' Drug A', 'Drug B' and, 'Combination' columns give the information about
  the coefficients for each group (tumor growth rates), while the 'SD random effects' and
  'SD residuals' indicate the standard deviation of random effects (between-subject variance),
  and standard deviation of the residuals (within-subject variance), respectively.", rownames = TRUE)
  
  output$model_estimates_header <- renderText({
    req(input$run_analysis)
    "Model Estimates"
  })
  
  observeEvent(input$run_analysis, {shinyjs::delay(500,shinyjs::show("estimates_file_type"))})
  observeEvent(input$run_analysis, {shinyjs::delay(500,shinyjs::show("downloadEstimates"))})
  
  # Download of model estimates table
  output$downloadEstimates <- downloadHandler(
    filename = function() {
      paste("model_estimates_", Sys.Date(), ".", input$estimates_file_type, sep = "")
    },
    content = function(file) {
      df <- modelEstimates()
      if (input$estimates_file_type == "csv") {
        write.csv(df, file, row.names = TRUE)
      } else if (input$estimates_file_type == "txt") {
        write.table(df, file, row.names = FALSE, sep = "\t")
      } else if (input$estimates_file_type == "xlsx") {
        write.xlsx(df, file)
      }
    }
      )
  
  # Generate plot
  modelPlot <- reactive({
    req(model_results())
    drug_c <- input$drug_c
    if (drug_c == "") {
      drug_c <- NA
    }
    plot_lmmModel(
      model = model_results(),
      trt_control = input$control_group,
      drug_a = input$drug_a,
      drug_b = input$drug_b,
      drug_c = drug_c,
      combination = input$combination
    )
  })

  # Render the plot if show_plot is TRUE
  output$model_plot <- renderPlot({
    req(modelPlot(), input$show_plot)
  })
  
  # Download plot
  
  observeEvent(input$run_analysis, {shinyjs::delay(500,shinyjs::show("model_plot_file"))})
  observeEvent(input$run_analysis, {shinyjs::delay(500,shinyjs::show("downloadModelPlot"))})

  output$downloadModelPlot <- downloadHandler(
    filename = function() {
      paste("model_plot_", Sys.Date(), ".", input$model_plot_file, sep = "")
    },
    content = function(file) {
      model_plot <- modelPlot()
      ggsave(
        file,
        plot = model_plot,
        dpi = 300,
        height = 10,
        width = 14,
        units = "in"
      )
    }
  )

  output$model_plot_header <- renderText({
    req(input$run_analysis)
    "Tumor Growth Plots"
  })
  
  # Second tab: Synergy calculation ----
  
  # Synergy calculation reactive expression
  synergy_results <- eventReactive(input$syn_calc, {
    req(model_results())
    
    if(input$setseed_ra){
      set.seed(123)
    }
    
    withCallingHandlers({
      tryCatch({
        result <- lmmSynergy(
          model = model_results(),
          method = input$synergy_method,
          min_time = input$min_time,
          robust = input$robustSE,
          type = input$se_type,
          ra_nsim = input$ra_nsim,
          show_plot = FALSE
        )
        return(result)
      }, error = function(e) {
        showNotification(paste("Error: ", 
                               e$message, ". To solve it, you can try clicking on 'Show Advanced Options' 
                             and increasing the 'Minimum Time for Synergy Calculation'",
                               collapse = ".", sep = ""), type = "error", duration = NULL)
        NULL
      })
    }, warning = function(w) {
      showNotification(paste("Warning:", w$message, collapse = "."), type = "warning", duration = 30)
      invokeRestart("muffleWarning")
    })
  })
  
  synergyPlot <- reactive({
    req(synergy_results())
    plot_lmmSynergy(syn_data = synergy_results())
  })
  
  # Render the synergy plot
  output$synergy_plot <- renderPlot({
    req(synergyPlot())
  })
  
  # Download synergy plot
  observeEvent(input$syn_calc, {shinyjs::delay(500, shinyjs::show("syn_plot_file"))})
  observeEvent(input$syn_calc, {shinyjs::delay(500, shinyjs::show("downloadSynPlot"))})
  
  output$downloadSynPlot <- downloadHandler(
    filename = function() {
      paste("syn_plot_", Sys.Date(), ".", input$syn_plot_file, sep = "")
    },
    content = function(file) {
      syn_plot <- synergyPlot()
      ggsave(
        file,
        plot = syn_plot,
        dpi = 300,
        width = 14,
        height = 10,
        units = "in"
      )
    }
  )
  
  output$synergy_plot_header <- renderText({
    req(input$syn_calc)
    "Plots for Synergy Results"
  })
  
  # Render the synergy results table
  output$synergy_results_table <- renderDT({
    req(synergy_results())
    datatable(synergy_results()$Synergy, 
              caption = "Results of synergy calculation for each time point.
              'Model': Reference model used for synergy evaluation.
              'CI': Combination Index. 
              'SS': Synergy Score. 
              'Estimate': point estimate of the metric for the synergy evaluation. 
              'lwr' and 'upr': lower and upper-limit of the 95% confidence interval
              for the synergy calculation, respectively.
              'pval': p-value associated with the synergy evaluation.
              'Time': time point associated with the results.")
  })
  
  # Download synergy results
  observeEvent(input$syn_calc, {shinyjs::delay(1000, shinyjs::show("synergy_file_type"))})
  observeEvent(input$syn_calc, {shinyjs::delay(1000, shinyjs::show("downloadSynergy"))})
  
  output$downloadSynergy <- downloadHandler(
    filename = function() {
      paste("synergy_results_", Sys.Date(), ".", input$synergy_file_type, sep = "")
    },
    content = function(file) {
      df <- synergy_results()$Synergy
      if (input$synergy_file_type == "csv") {
        write.csv(df, file, row.names = TRUE)
      } else if (input$synergy_file_type == "txt") {
        write.table(df, file, row.names = FALSE, sep = "\t")
      } else if (input$synergy_file_type == "xlsx") {
        write.xlsx(df, file)
      }
    }
  )
  
  output$synergy_results_table_header <- renderText({
    req(input$syn_calc)
    "Synergy Results"
  })
  
  
  
  # Third tab: Diagnostics of the Model ----
  
  # Diagnostics Plots
  
  modelDiagPlot <- reactive({
    req(model_results(), input$run_analysis)
    ranef.d <- plot_ranefDiagnostics(model = model_results()) # Plot of random effect diagnostics
    resid.d <- plot_residDiagnostics(model = model_results()) # Plot of residual diagnostics
    cowplot::plot_grid(ranef.d[[1]], resid.d[[4]],
                       resid.d[[1]], resid.d[[5]], byrow = F)
  })
  
  # Render plot
  output$model_diag_plot <- renderPlot({
    req(modelDiagPlot(), input$run_analysis)
  })
  
  # Download plot
  
  observeEvent(input$run_analysis, {shinyjs::delay(500, shinyjs::show("diag_plot_file"))})
  observeEvent(input$run_analysis, {shinyjs::delay(500, shinyjs::show("downloadDiagPlot"))})
  
  output$downloadDiagPlot <- downloadHandler(
    filename = function() {
      paste("diag_plot_", Sys.Date(), ".", input$diag_plot_file, sep = "")
    },
    content = function(file) {
      diag_plot <- modelDiagPlot()
      ggsave(
        file,
        plot = diag_plot,
        dpi = 300,
        width = 14,
        height = 14,
        units = "in"
      )
    }
  )
  
  output$model_diagnostics_header <- renderText({
    req(input$run_analysis)
    "Random Effects and Residual Diagnostics Plots"
  })
  # Output the Normality Tests
  output$ranef_norm <- renderPrint({
    req(model_results(), input$run_analysis)
    fBasics::shapiroTest(nlme::ranef(model_results())$Time, 
                         title = "Shapiro - Wilk Normality Test of random effects")
  })
  output$normality_ranef_header <- renderText({
    req(input$run_analysis)
    "Shapiro-Wilk's Normality Test for Random Effects"
  })
  output$resid_norm <- renderPrint({
    req(model_results(), input$run_analysis)
    fBasics::shapiroTest(resid(model_results(), type = "normalized"), 
                         title = "Shapiro - Wilk Normality Test of normalized residuals")
  })
  output$normality_resid_header <- renderText({
    req(input$run_analysis)
    "Shapiro-Wilk's Normality Test for Normalized Residuals"
  })
  
  # Outliers
  output$resid_outliers <- renderDT({
    req(model_results(), input$run_analysis)
    datatable(residDiagnostics(model = model_results())$Outliers)
  })
  
  # Download outliers
  observeEvent(input$run_analysis, {shinyjs::delay(1000, shinyjs::show("outliers_file_type"))})
  observeEvent(input$run_analysis, {shinyjs::delay(1000, shinyjs::show("downloadOutliers"))})
  
  output$downloadOutliers <- downloadHandler(
    filename = function() {
      paste("residual_outliers_", Sys.Date(), ".", input$outliers_file_type, sep = "")
    },
    content = function(file) {
      df <- residDiagnostics(model = model_results())$Outliers
      if (input$outliers_file_type == "csv") {
        write.csv(df, file, row.names = TRUE)
      } else if (input$outliers_file_type == "txt") {
        write.table(df, file, row.names = FALSE, sep = "\t")
      } else if (input$outliers_file_type == "xlsx") {
        write.xlsx(df, file)
      }
    }
  )
  
  output$resid_outliers_header <- renderText({
    req(input$model_diagnostic)
    "Potential Outlier Observations"
  })
  
  ## Influential Diagnostics ----
  ### log-likelihood displacements ----
  
  logLikDisp <- reactive({
    req(model_results(), input$run_analysis)
    withProgress(message = "Calculating log-likelihood displacements", value = 0.25, {
      var_name <- NULL
      if (input$model_heteroscedasticity == TRUE &&
          input$varIdent == "SampleID") {
        var_name <- "SampleID"
      } else if (input$model_heteroscedasticity == TRUE &&
                 input$varIdent == "Time") {
        var_name <- "Time"
      } else if (input$model_heteroscedasticity == TRUE &&
                 input$varIdent == "Treatment") {
        var_name <- "Treatment"
      }
      
      logLikSubjectDisplacements(
        model = model_results(),
        disp_thrh = input$disp_thrh,
        var_name = var_name
      )
    })
  })
  
  # Render the Diagnostics Plots
  output$loglik_disp <- renderPlot({
    req(logLikDisp(), input$run_analysis)
  })
  
  # Render console results
  output$loglik_disp_sub <- renderPrint({
    req(model_results(), input$run_analysis)
    withProgress(message = "Calculating log-likelihood displacements", value = 0.5, {
      var_name <- NULL
      if (input$model_heteroscedasticity == TRUE &&
          input$varIdent == "SampleID") {
        var_name <- "SampleID"
      } else if (input$model_heteroscedasticity == TRUE &&
                 input$varIdent == "Time") {
        var_name <- "Time"
      } else if (input$model_heteroscedasticity == TRUE &&
                 input$varIdent == "Treatment") {
        var_name <- "Treatment"
      }
      
      logLikSubjectDisplacements(
        model = model_results(),
        disp_thrh = input$disp_thrh,
        var_name = var_name
      )
    })
  })
  
  # Download loglik displacements
  observeEvent(input$run_analysis, {shinyjs::delay(1000, shinyjs::show("downloadloglikDisp"))})
  
  output$downloadloglikDisp <- downloadHandler(
    filename = function() {
      paste("logLik_Disp_", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      df <- logLikDisp()
      write.table(df, file, row.names = TRUE, sep = "\t", col.names = "logLikDisp")
    }
  )
  
  
  output$loglik_disp_header <- renderText({
    req(input$run_analysis)
    "Plot of the log-likelihood displacements by subject"
  })
  
  ### Cook's Distances ----
  
  CookDist <- reactive({
    req(model_results(), input$run_analysis)
    withProgress(message = "Calculating Cook's Distances", value = 0.75, {
      CookDistance(model = model_results(), cook_thr = input$cook_thr)
    })
  })
  
  # Render plot
  output$cook_dist <- renderPlot({
    req(CookDist(), input$run_analysis)
  })
  
  # Render console results
  output$cook_dist_sub <- renderPrint({
    req(model_results(), input$run_analysis)
    withProgress(message = "Calculating Cook's Distances", value = 0.9, {
      CookDistance(model = model_results(), cook_thr = input$cook_thr)
    })
  })
  
  output$cook_dist_header <- renderText({
    req(input$run_analysis)
    "Plot of Cook's distances by subject"
  })
  
  # Download Cook's Distances
  observeEvent(input$run_analysis, {shinyjs::delay(1000, shinyjs::show("downloadCooksDist"))})
  
  output$downloadCooksDist <- downloadHandler(
    filename = function() {
      paste("CooksDist_", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      df <- CookDist()
      write.table(df, file, row.names = TRUE, sep = "\t", col.names = "CooksDist")
    }
  )
  
  # Fourth tab: Performance of the Model ----
  
  # Diagnostics plots
  ObsPred <- reactive({
    req(model_results(), input$run_analysis)
    plot_ObsvsPred(model = model_results(), nrow = input$nrow, ncol = input$ncol)
  })
  
  # Render the Diagnostics Plots
  output$obs_vs_plot <- renderPlot({
    req(ObsPred(), input$run_analysis)
  })
  
  # Download Diagnostics Plots
  observeEvent(input$run_analysis, {shinyjs::delay(500, shinyjs::show("obs_pred_file"))})
  observeEvent(input$run_analysis, {shinyjs::delay(500, shinyjs::show("downloadObsPredPlot"))})
  
  output$downloadObsPredPlot <- downloadHandler(
    filename = function() {
      paste("ObsvsPred_plot_", Sys.Date(), ".", input$obs_pred_file, sep = "")
    },
    content = function(file) {
      ObsPred_plot <- ObsPred()
      if(input$obs_pred_file == "png") {
        png(file, res = 300, width = 12, height = 12, units = "in")
        print(ObsPred_plot)
        dev.off()
      } else if (input$obs_pred_file == "pdf") {
        pdf(file, width = 12, height = 12)
        print(ObsPred_plot)
        dev.off()
      } else if (input$obs_pred_file == "svg") {
        svg(file, width = 12, height = 12)
        print(ObsPred_plot)
        dev.off()
      }
    }
  )
  
  output$obs_vs_plot_header <- renderText({
    req(input$run_analysis)
    "Plots of Observed vs Predicted Values"
  })
  output$model_performance <- renderTable({
    req(model_results(), input$run_analysis)
    performance::model_performance(model = model_results(), metrics = c("AIC", "AICc", "BIC", "R2", "RMSE", "SIGMA"))
  }, striped = TRUE, digits = 5, caption = "Metrics of model performance: 
  AIC: Akaike's Information Criterion. 
  AICc: Second-order (or small sample) AIC with a correction for small sample sizes.
  BIC: Bayesian Information Criterion.
  R2: Nakagawa's marginal and conditional r-squared value for the mixed effects model.
  RMSE: root mean squared error.
  Sigma: residual standard deviation.", rownames = FALSE)
  output$performance_header <- renderText({
    req(input$model_performance)
    "Model Performance"
  })
  
  
  
  
  
  
  
 
  
  # Fifth tab: Power Analysis ----
  
  ## Post Hoc Power Analysis ----
  
  # Slider for selecting time
  output$posthocpwr_time <- renderUI({
    req(model_results())
    sliderTextInput("posthoc_dynamic_slider", "Select the time point for which to calculate the statistical power:", 
                    choices = sort(unique(model_results()$data$Time)), 
                    selected = max(model_results()$data$Time), 
                    grid = TRUE
    )
  })
  
  
  post_hoc_pwr_result <- eventReactive(input$post_hoc_pwr, {
    req(model_results())
    
    if(input$setseed){
      set.seed(123)
    }
    PostHocPwr(
      model = model_results(),
      nsim = input$pwr_nsim ,
      method = input$pwr_method,
      pvalue = input$pwr_pval,
      time = input$posthoc_dynamic_slider
    )
  })
  
  output$post_hoc_pwr <- renderText({
    req(post_hoc_pwr_result())
    paste("Post Hoc Power:", post_hoc_pwr_result())
  })
  
  output$post_hoc_pwr_header <- renderText({
    req(input$post_hoc_pwr)
    paste("Post Hoc Power for ", input$pwr_method," Reference Model for Time ",input$posthoc_dynamic_slider, sep = "")
  })
  
  ## A Priori Power Analysis ----
  # Sample Size
  
  SampleSizePwr <- reactive({
    req(model_results(), input$sample_size_pwr)
    model_estimates <- round(lmmModel_estimates(model = model_results()), 4)
    PwrSampleSize(npg = seq(from = min(input$sample_n), to = max(input$sample_n), by = 1),
                  time = unique(model_results()$dt1$Time), 
                  grwrControl = model_estimates[,1],
                  grwrA = model_estimates[,2],
                  grwrB = model_estimates[,3],
                  grwrComb = model_estimates$Combination, 
                  sd_ranef = model_estimates$sd_ranef, 
                  sgma = model_estimates$sd_resid,
                  method = input$sample_size_pwr_method)
  })
  
  ## Render plot
  output$sample_size_pwr <- renderPlot({
    req(SampleSizePwr(), input$sample_size_pwr)
  })
  
  ## Download plot
  observeEvent(input$sample_size_pwr, {shinyjs::delay(500, shinyjs::show("size_pwr_plot_file"))})
  observeEvent(input$sample_size_pwr, {shinyjs::delay(500, shinyjs::show("downloadSampleSizePwr"))})
  
  output$downloadSampleSizePwr <- downloadHandler(
    filename = function() {
      paste("size_pwr_plot_", Sys.Date(), ".", input$size_pwr_plot_file, sep = "")
    },
    content = function(file) {
      pwr_plot <- SampleSizePwr()
      ggsave(
        file,
        plot = pwr_plot,
        dpi = 300,
        width = 14,
        height = 6,
        units = "in"
      )
    }
  )
  
  ## Header
  output$sample_size_pwr_header <- renderText({
    req(input$sample_size_pwr)
    paste("A Priori Sample Size Power Analysis for ", input$sample_size_pwr_method," Reference Model", sep = "")
  })
  
  # Time 
  
  TimePwr <- reactive({
    req(model_results(), input$time_pwr)
    model_estimates <- round(lmmModel_estimates(model = model_results()), 4)
    
    if (input$max_freq == "max") {
      i <- 2
      time_list <- list()
      
      time_pwr_min_t <- input$time_pwr_min_t
      time_pwr_max_t <- input$time_pwr_max_t
      time_pwr_step <- input$time_pwr_step
      
      time_list[[1]] <- seq(0, time_pwr_min_t, time_pwr_step)
      while (max(time_list[[i - 1]]) <= time_pwr_max_t) {
        time_pwr_min_t <- time_pwr_min_t + time_pwr_step
        time_list[[i]] <- seq(0, time_pwr_min_t, time_pwr_step)
        i <- i + 1
      }
      
      PwrTime(
        npg = input$time_pwr_npg,
        time = time_list,
        type = "max",
        grwrControl = model_estimates[,1],
        grwrA = model_estimates[,2],
        grwrB = model_estimates[,3],
        grwrComb = model_estimates$Combination,
        sd_ranef = model_estimates$sd_ranef,
        sgma = model_estimates$sd_resid,
        method = input$time_pwr_method
      )
    } else if (input$max_freq == "freq"){
      
      time_list <- list()
      
      time_pwr_max_t <- input$time_pwr_max_t
      time_pwr_min_freq <- input$time_pwr_min_freq
      time_pwr_max_freq <- input$time_pwr_max_freq
      
      if(time_pwr_max_freq > time_pwr_max_t){
        stop("The maximum number of measurements cannot be larger than the time of follow-up")
      }
      
      i <- 1
      while (time_pwr_min_freq <= time_pwr_max_freq) {
        time_list[[i]] <- round(seq(0, time_pwr_max_t, length.out = time_pwr_min_freq))
        time_pwr_min_freq <- time_pwr_min_freq + 1
        i <- i + 1
      }
      
      PwrTime(
        npg = input$time_pwr_npg,
        time = time_list,
        type = "freq",
        grwrControl = model_estimates[,1],
        grwrA = model_estimates[,2],
        grwrB = model_estimates[,3],
        grwrComb = model_estimates$Combination,
        sd_ranef = model_estimates$sd_ranef,
        sgma = model_estimates$sd_resid,
        method = input$time_pwr_method
      )
    }
  })
  
  ## Render plot
  output$time_pwr <- renderPlot({
    req(TimePwr(), input$time_pwr)
    
  })
  
  ## Download plot
  observeEvent(input$time_pwr, {shinyjs::delay(500, shinyjs::show("time_pwr_plot_file"))})
  observeEvent(input$time_pwr, {shinyjs::delay(500, shinyjs::show("downloadTimePwr"))})
  
  output$downloadTimePwr <- downloadHandler(
    filename = function() {
      paste("time_pwr_plot_", Sys.Date(), ".", input$time_pwr_plot_file, sep = "")
    },
    content = function(file) {
      pwr_plot <- TimePwr()
      ggsave(
        file,
        plot = pwr_plot,
        dpi = 300,
        width = 14,
        height = 6,
        units = "in"
      )
    }
  )
  
  output$time_pwr_header <- renderText({
    req(input$time_pwr)
    paste("A Priori Time Power Analysis for ", input$time_pwr_method," Reference Model", sep = "")
  })
  
  # Variability
  
  VarPwr <- reactive({
    req(model_results(), input$var_pwr)
    model_estimates <- round(lmmModel_estimates(model = model_results()), 4)
    
    grwrControl_pwr_var <- input$grwrControl_pwr_var
    if(is.na(grwrControl_pwr_var)){
      grwrControl_pwr_var <- model_estimates[,1]
    }
    
    grwrA_pwr_var <- input$grwrA_pwr_var
    if(is.na(grwrA_pwr_var)){
      grwrA_pwr_var <- model_estimates[,2]
    }
    
    grwrB_pwr_var <- input$grwrB_pwr_var
    if(is.na(grwrB_pwr_var)){
      grwrB_pwr_var <- model_estimates[,3]
    }
    
    grwrComb_pwr_var <- input$grwrComb_pwr_var
    if(is.na(grwrComb_pwr_var)){
      grwrComb_pwr_var <- model_estimates$Combination
    }
    
    sd_ranef <- input$sd_ranef
    if(is.na(sd_ranef)){
      sd_ranef <- model_estimates$sd_ranef
    }
    
    sgma <- input$sgma
    if(is.na(sgma)){
      sgma <- model_estimates$sd_resid
    }
    
    sd_eval_min <- input$sd_eval_min
    if(is.na(sd_eval_min)){
      sd_eval_min <- 0.1*model_estimates$sd_ranef
    }
    
    sd_eval_max <- input$sd_eval_max
    if(is.na(sd_eval_max)){
      sd_eval_max <- 2*model_estimates$sd_ranef
    }
    
    sgma_min <- input$sgma_min
    if(is.na(sgma_min)){
      sgma_min <- 0.1*model_estimates$sd_resid
    }
    
    sgma_max <- input$sgma_max
    if(is.na(sgma_max)){
      sgma_max <- 2*model_estimates$sd_resid
    }
    
    grwrComb_min <- input$grwrComb_min
    if(is.na(grwrComb_min)){
      grwrComb_min <- 0.1*model_estimates$Combination
    }
    
    grwrComb_max <- input$grwrComb_max
    if(is.na(grwrComb_max)){
      grwrComb_max <- 2*model_estimates$Combination
    }
    
    APrioriPwr(
      npg = input$var_pwr_npg,
      time = unique(model_results()$dt1$Time),
      grwrControl = grwrControl_pwr_var,
      grwrA = grwrA_pwr_var,
      grwrB = grwrB_pwr_var,
      grwrComb = grwrComb_pwr_var,
      sd_ranef = sd_ranef,
      sgma = sgma,
      sd_eval = seq(sd_eval_min, sd_eval_max, sd_eval_min),
      sgma_eval = seq(sgma_min, sgma_max, sgma_min),
      grwrComb_eval = seq(grwrComb_min, grwrComb_max, abs(grwrComb_min)),
      method = input$var_pwr_method
    )
  })
  
  ## Render plot
  output$var_pwr <- renderPlot({
    req(VarPwr(), input$var_pwr)
  })
  
  ## Download plot
  observeEvent(input$var_pwr, {shinyjs::delay(1000, shinyjs::show("var_pwr_plot_file"))})
  observeEvent(input$var_pwr, {shinyjs::delay(1000, shinyjs::show("downloadVarPwr"))})
  
  output$downloadVarPwr <- downloadHandler(
    filename = function() {
      paste("var_pwr_plot_", Sys.Date(), ".", input$var_pwr_plot_file, sep = "")
    },
    content = function(file) {
      pwr_plot <- VarPwr()
      ggsave(
        file,
        plot = pwr_plot,
        dpi = 300,
        width = 16,
        height = 8,
        units = "in"
      )
    }
  )
  
  ## Header
  output$var_pwr_header <- renderText({
    req(input$var_pwr)
    paste("A Priori Variability Power Analysis for ", input$var_pwr_method," Reference Model", sep = "")
  })
  
}