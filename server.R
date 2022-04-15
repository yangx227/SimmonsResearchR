#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(imputeTS)
library(tidymodels)
library(timetk)
library(modeltime)
library(modelsummary)

library(lubridate)
library(htmltools)
library(DTedit)
library(ggstatsplot)
library(cowplot)

model_base <- data.table::fread("C:/Users/hu.yang/Projects/SN Time Series/Brand_Awareness_TS_Models_Demo/Brand_Awareness_TS_Models_Demo_V3/brand_awareness_base.csv") %>%
  mutate(date = lubridate::mdy(date))


source("C:/Users/hu.yang/Projects/SN Time Series/Brand_Awareness_TS_Models_Demo/Brand_Awareness_TS_Models_Demo_V3/ts_funcs.R")

varlist_dict <- tibble::tibble(
  desc = c("Awareness", "Consideration", "Familiarity", "Favorability", "Recommend", "Brand Digital GCM Impressions", "NonBrand Digital GCM Impressions", "Print Impressions", "TV impressions", "Events", "3rd party content PVs", "3rd party social impressions", "3rd Party Social Shares"),
  name = c("awareness_percent", "consideration_t2b_percent", "familiarity_t1b_percent", "favorability_percent", "recommend_t3b_percent", "branded_digital_impressions", "non_brand_digital_impressions", "print_impression", "tv_impressions", "event", "x3rd_party_content_pvs", "x3rd_party_social_impressions", "x3rd_party_social_shares")
)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  
  sel_base <- reactiveVal(model_base)  
  
  selectedVarName <- reactive({
    return(input$varSelect)
  })
  
  models_results_table <-  reactiveVal(NULL)
  
  models_results_fit <-  reactiveVal(NULL)
  
  models_results_predict <-  reactiveVal(NULL)
  
  models_fitted_fig <-  reactiveVal(NULL)
  
  models_coefs_df <- reactiveVal(NULL)
  
  models_stats_info <- reactiveVal(NULL)
  
  splitsData <- reactiveVal(NULL)
  
  simulation_lift <- reactiveVal(NULL)
  
  vif_table <- reactiveVal(NULL)
  
  selModelDate <- reactiveVal(
    ifelse(!is.null(model_base), model_base$date[1], NULL)
  )
  
  selectedTSData <- reactive({
    
    req(input$varSelect)
    
    var_df <- tibble::tibble(desc = input$varSelect)
    sel_var <- var_df %>%
      inner_join(varlist_dict) %>%
      pull(name)
    
    dat <- sel_base() %>%
      select(date, sel_var)
    
    if (input$varSelect %in% input$varsTransSelect) {
      
      return(applyVarsTransformation(dat, sel_var, input$varTransformation))
    } else {
      
      return(dat)
    }
    
    
    
  })
  
  selectedTSDataImputed <- reactive({
    
    if (sum(is.na(selectedTSData())) == 0) {
      return(selectedTSData())
    }
    
    if (input$missingImputationMethod == "None") {
      
      return(selectedTSData())
      
    } else if (input$missingImputationMethod == "Linear") {
      
      
      selData <- ts(selectedTSData(), frequency = 12, start = c(2020,3))
      selDataImputed <- tibble::as_tibble(na_interpolation(selData, option = "linear"))
      
      selDataImputed$date <- selectedTSData()$date
      
      return(selDataImputed)
      
    } else if (input$missingImputationMethod == "Spline") {
      
      selData <- ts(selectedTSData(), frequency = 12, start = c(2020,3))
      selDataImputed <- tibble::as_tibble(na_interpolation(selData, option = "spline"))
      
      selDataImputed$date <- selectedTSData()$date
      
      return(selDataImputed)
      
      
    } else if (input$missingImputationMethod == "Stine") {
      
      selData <- ts(selectedTSData(), frequency = 12, start = c(2020,3))
      selDataImputed <- tibble::as_tibble(na_interpolation(selData, option = "stine"))
      
      selDataImputed$date <- selectedTSData()$date
      
      return(selDataImputed)
      
    } else if (input$missingImputationMethod == "All 0") {
      
      selDataImputed <- selectedTSData()
      
      selDataImputed[is.na(selDataImputed)] <- 0.000001
      
      return(selDataImputed)
      
    }
    
  })
  
  
  
  output$tsPlot <- renderPlotly({
    
    data_tmp <- selectedTSData()
    names(data_tmp) <- c("date", "value") 
    
    data_tmp_imputed <- selectedTSDataImputed()
    names(data_tmp_imputed) <- c("date", "value")
    
    fig <- data_tmp_imputed %>%
      plot_time_series(.date_var = date,
                       .value = value, 
                       .line_color = "#2c3e50",
                       .smooth = input$trendLine,
                       .smooth_size = 0.6,
                       .smooth_alpha = 0.6,
                       .smooth_degree = input$trendSmoothDegree
      )
    
    data_tmp <- data_tmp %>%
      filter(is.na(value)) %>%
      select(-value) %>%
      left_join(data_tmp_imputed)
    
    if (nrow(data_tmp) > 0) {
      
      fig <- fig %>%
        add_trace(x = ~data_tmp$date, y = ~data_tmp$value, name = 'Imputed', marker = list(color = 'rgb(153, 0, 51)', size = 6), type = 'scatter', mode = 'markers')
    }
    
    fig <- fig %>% layout(title = paste0("Monthly Time Series Values: ", selectedVarName()),
                          xaxis = list(title = "Date"),
                          yaxis = list (title = selectedVarName()),
                          hovermode = "x unified",
                          legend = list(orientation = 'h'))
    
    
  })
  
  output$tsHistPlots <- renderPlot({
    
    req(input$varSelect)
    
    dep_df <- tibble::tibble(desc = input$varSelect)
    dep_df <- dep_df %>%
      inner_join(varlist_dict)
    
    dat <- sel_base() %>% select(-date)
    
    dep <- dep_df$name[1]
    indep <- c("branded_digital_impressions", "non_brand_digital_impressions", "print_impression", "tv_impressions", "event", "x3rd_party_content_pvs", "x3rd_party_social_impressions", "x3rd_party_social_shares")
    
    dat <- dat %>%
      select(c(dep, indep)) %>%
      na.omit()
    
    figs_list <- list()
    
    varlist <- c(dep, indep)
    
    
    for (i in 1:length(varlist)) {
      
      figs_list[[i]] <- ggplot(dat, aes(x=!!sym(varlist[i]))) + 
        geom_histogram(color="black", alpha=.2, fill="#FF6666") +
        geom_vline(aes(xintercept=mean(!!sym(varlist[i]))),
                   color="blue", linetype="dashed", size=1) +
        xlab("") +
        ggtitle(varlist[i]) +
        theme_cowplot(12) +
        theme(legend.position = "none",
              plot.title = element_text(hjust = 0.5)) 
      
      
    }    
    
    return(plot_grid(figs_list[[1]], figs_list[[2]], figs_list[[3]], figs_list[[4]], figs_list[[5]], figs_list[[6]], figs_list[[7]], figs_list[[8]], figs_list[[9]], cols = 3))
    
  })
  
  observeEvent(input$varsTransSelect, {
    
    if (length(input$varsTransSelect) > 0) {
      shinyjs::enable("varTransformation")
    }
    
    
  })
  
  
  output$tsScatterPlot <- renderPlot({
    
    req(input$varSelect)
    
    dep_df <- tibble::tibble(desc = input$varSelect)
    dep_df <- dep_df %>%
      inner_join(varlist_dict)
    
    dat <- sel_base() %>% select(-date)
    
    dep <- dep_df$name[1]
    indep <- c("branded_digital_impressions", "non_brand_digital_impressions", "print_impression", "tv_impressions", "event", "x3rd_party_content_pvs", "x3rd_party_social_impressions", "x3rd_party_social_shares")
    
    dat <- dat %>%
      select(c(dep, indep)) %>%
      na.omit()
    
    figs_list <- list()
    
    for (i in 1:length(indep)) {
      cat(indep[i], "\n")
      
      cor_val <- cor(dat[[dep[1]]], dat[[indep[i]]])
      
      figs_list[[i]] <- ggplot(dat, aes(x=!!sym(indep[i]), y=!!sym(dep[1]),  alpha = 0.5)) +
        geom_point(color = 'blue', size = 4) +
        geom_smooth(method=lm, se=FALSE, linetype = "dashed", color = "darkred") +
        ggtitle(paste0(indep[i], " (R2=",round(cor_val^2,3),")")) + 
        theme_cowplot(12) +
        theme(legend.position = "none",
              plot.title = element_text(hjust = 0.5),
              strip.background = element_blank()) 
      
    }    
    
    return(plot_grid(figs_list[[1]], figs_list[[2]], figs_list[[3]], figs_list[[4]], figs_list[[5]], figs_list[[6]], figs_list[[7]], figs_list[[8]], cols = 3))
    
  })
  
  
  output$tsCorrPlot <- renderPlot({
    
    req(input$varSelect)
    
    dep_df <- tibble::tibble(desc = input$varSelect)
    dep_df <- dep_df %>%
      inner_join(varlist_dict)
    
    dat <- sel_base() %>% select(-date)
    
    dep <- dep_df$name[1]
    indep <- c("branded_digital_impressions", "non_brand_digital_impressions", "print_impression", "tv_impressions", "event", "x3rd_party_content_pvs", "x3rd_party_social_impressions", "x3rd_party_social_shares")
    
    dat <- dat %>%
      select(c(dep, indep)) %>%
      na.omit()
    
    fig <- ggstatsplot::ggcorrmat(
      data = dat,
      type = "parametric", # parametric for Pearson, nonparametric for Spearman's correlation
      colors = c("darkred", "white", "steelblue") # change default colors
    )
    

    return(fig)
    
  })
  
  output$tsSTLPlot <- renderPlotly({
    
    data_tmp <- selectedTSData()
    names(data_tmp) <- c("date", "value") 
    
    
    fig <- data_tmp %>% plot_stl_diagnostics(.date_var = date,
                                             .value = value)
    
    
    fig <- fig %>% layout(title = paste0('STL Diagnostics - ', selectedVarName()), 
                          hovermode = "x unified",
                          legend = list(orientation = 'h'))
    
  })
  
  
  output$tsSeasonalDiagPlot <- renderPlotly({
    
    data_tmp <- selectedTSData()
    names(data_tmp) <- c("date", "value") 
    
    
    fig <- data_tmp %>% plot_seasonal_diagnostics (.date_var = date,
                                                   .value = value)
    
    
    fig <- fig %>% layout(title = paste0('Seasonal Diagnostics - ', selectedVarName()), 
                          hovermode = "x unified",
                          legend = list(orientation = 'h'))
    
  })
  
  
  output$tsACSPACFPlot <- renderPlotly({
    
    data_tmp <- selectedTSData()
    names(data_tmp) <- c("date", "value") 
    
    fig <- data_tmp %>% plot_acf_diagnostics(.date_var = date,
                                             .value = value,
                                             .lags = 12)
    
    
    fig <- fig %>% layout(title = paste0('ACF/PACF Plots - ',  selectedVarName()), 
                          hovermode = "x unified")
    
    
  })
  
  output$tsAnomalyPlot <- renderPlotly({
    
    data_tmp <- selectedTSData()
    names(data_tmp) <- c("date", "value") 
    
    fig <- data_tmp %>% plot_anomaly_diagnostics(.date_var = date,
                                                 .value = value,
                                                 .alpha = anomalyCL(),
                                                 .max_anomalies = input$anomalyMaxium)
    
    fig <- fig %>% layout(title = paste0('Anamaly Diagnostics Plots - ',  selectedVarName(), ' (CI: ',input$anomalyCL,')'), 
                          hovermode = "x unified",
                          legend = list(orientation = 'h'))
    
  })
  
  
  anomalyCL <-  reactive({
    
    if (input$anomalyCL == "95%") {
      return(0.05)
    } else  if (input$anomalyCL == "90%") {
      return(0.10)
    } else  if (input$anomalyCL == "85%") {
      return(0.15)
    } else  if (input$anomalyCL == "80%") {
      return(0.20)
    } else {
      return(0.05)
    }
    
  })
  
  output$data_summary_table  <- renderDT({
    
    req(sel_base())
    
    
    results <- ts_data_summary(sel_base() %>% select(-date)) %>%
      mutate(across(`Complete Rate`:P100, ~ round(.x, 3)))
    
    return(datatable(results,
                     caption = paste0("Data Summary Table"),
                     editable = FALSE,
                     options = list(dom = 't',
                                    pageLength = 50,
                                    ordering = F)) %>%
             formatPercentage("Complete Rate", 1)
           
    )
    
  })
  
  
  observeEvent(input$apply2Data, {
    
    updateProgressBar(
      session = session,
      id = "pb1",
      value = 0
    )
    
    dat <- sel_base()
    
    modified_flag <- 0
    
    updateProgressBar(
      session = session,
      id = "pb1",
      value = 10
    )
    

    if (length(input$varsTransSelect) > 0) {
      
      var_df <- tibble::tibble(desc = input$varsTransSelect)
      varlist <- var_df %>%
        inner_join(varlist_dict) %>%
        pull(name)
      
      if (input$missingImputationMethod != "None") {

        dat1 <- purrr::map(varlist, ~applyMissingImputation(sel_base(), .x, input$missingImputationMethod)) %>%
          purrr::reduce(left_join)
        
        dat <- dat %>%
          select(-varlist) %>%
          left_join(dat1) %>%
          select(names(sel_base()))
        
      }
      
      updateProgressBar(
        session = session,
        id = "pb1",
        value = 50
      )
      
      
      dat1 <- dat %>%
        select(setdiff(names(dat), varlist))
      
      dat2 <- dat %>%
        select(c('date', varlist))
      
      dat2 <- purrr::map(varlist, ~applyVarsTransformation(dat2, .x, input$varTransformation)) %>%
        purrr::reduce(left_join) %>%
        select(-date)
      
      dat <- bind_cols(dat1, dat2) %>%
        select(names(dat))
      
      modified_flag <- 1
    }
    
    updateProgressBar(
      session = session,
      id = "pb1",
      value = 90
    )
    
    if (modified_flag == 1){
      
      sel_base(dat)
    }
    
    updateAwesomeRadio(session, "missingImputationMethod",
                       label = "Missing Value Imputation by Interpolation:",
                       choices = c("None", "Linear", "Spline", "Stine", "All 0"),
                       selected = "None",
                       inline = TRUE)
    
    updateAwesomeRadio(session,
                       inputId = "varTransformation",
                       label = "Transformations Options:",
                       choices = c("None", "Log", "Box-Cox after log", "Normalization after log (Min:0, Max:1)", "Standardization after log (Mean:0, SD:1)"),
                       selected = "None"
    )
    
    updateProgressBar(
      session = session,
      id = "pb1",
      value = 100
    )
    
  })
  
  observeEvent(input$resetData, {
    
    sel_base(model_base)
    
    updateProgressBar(
      session = session,
      id = "pb1",
      value = 0
    )
    
    updateAwesomeRadio(session, "missingImputationMethod",
                       label = "Missing Value Imputation by Interpolation:",
                       choices = c("None", "Linear", "Spline", "Stine", "All 0"),
                       selected = "None",
                       inline = TRUE)
    
    updateAwesomeRadio(session,
                       inputId = "varTransformation",
                       label = "Transformations Options:",
                       choices = c("None", "Log", "Box-Cox after log", "Normalization after log (Min:0, Max:1)", "Standardization after log (Mean:0, SD:1)"),
                       selected = "None"
    )
    
  })
  
  
  observeEvent(input$indepVarSelect, {
    
    updatePickerInput(session, 
                      inputId = "indepVarForced",
                      choices = input$indepVarSelect)
    
  })
  
  
  observeEvent(input$buildModels, {
    
    req(input$depVarSelect)
    req(input$indepVarSelect)
    
    dep_df <- tibble::tibble(desc = input$depVarSelect)
    dep_df <- dep_df %>%
      inner_join(varlist_dict)
    
    indep_df <- tibble::tibble(desc = input$indepVarSelect)
    indep_df <- indep_df %>%
      inner_join(varlist_dict)
    
    depVarSelect <- dep_df %>% pull(name)
    indepVarSelect <- indep_df %>% pull(name)
    
    if (length(input$indepVarForced) > 0) {
      
      forced_indep_df <- tibble::tibble(desc = input$indepVarForced)
      forced_indep_df <- forced_indep_df %>%
        inner_join(varlist_dict)

      forcedIndepVarSelect <- forced_indep_df %>% pull(name)
    } else {
      
      forcedIndepVarSelect <- NULL
    }
    

    updateProgressBar(
      session = session,
      id = "pb2",
      value = 0
    )
    
    dat <- sel_base() %>%
      select(c(depVarSelect, indepVarSelect))
    
    if (sum(is.na(dat)) > 0) {
      
      sendSweetAlert(
        session = session,
        title = "Error...",
        text = "Missing values in the data, please check!",
        type = "error"
      )
      
      return(NULL)
    }
    
    dat <- bind_rows(sel_base(), future_frame(sel_base(), .date_var = date, .length_out = 6))
    
    
    dat1 <- tail(dat, 12) %>%
      na.omit()
    
    for (i in 1:length(indepVarSelect)) {
      dat[[indepVarSelect[i]]][(nrow(dat)-5):nrow(dat)] <- dat1[[indepVarSelect[i]]]
    }
    
    dat_signature_tbl <- dat %>%
      tk_augment_timeseries_signature() %>%
      select(-diff, -starts_with('year'), -ends_with('iso'), -contains('week'), 
             -contains('day'), -contains('hour'), -contains('minute'), -contains('second'), -contains('am'), -ends_with('xts')) %>%
      mutate(month.lbl = as.factor(month))
    
    dat_signature_tbl$date.num <- 1:nrow(dat_signature_tbl)
    
    splits <- dat_signature_tbl %>%
      time_series_split(
        date_var = date,
        assess = "6 months",
        cumulative = TRUE
      )
    
    dat_signature_tbl <- slice(dat_signature_tbl, 1:(nrow(dat_signature_tbl)-6))
    
    formula_list <- gen_models_formulas(dat_signature_tbl, depVarSelect, indepVarSelect, forcedIndepVarSelect, session)
    
    updateProgressBar(
      session = session,
      id = "pb2",
      value = 55
    )
    
    if (length(formula_list) == 2) {
      
      models <- list(
        "Model1" = lm(formula_list[[1]], data = dat_signature_tbl),
        "Model2" = lm(formula_list[[2]], data = dat_signature_tbl)  
      )
      
      updatePickerInput(session = session, 
                        inputId = "modelsSelect",
                        choices = c('Model1',
                                    'Model2'))
      
    } else if (length(formula_list) == 3) {
      models <- list(
        "Model1" = lm(formula_list[[1]], data = dat_signature_tbl),
        "Model2" = lm(formula_list[[2]], data = dat_signature_tbl),  
        "Model3" = lm(formula_list[[3]], data = dat_signature_tbl)  
      )
      
      updatePickerInput(session = session, 
                        inputId = "modelsSelect",
                        choices = c('Model1',
                                    'Model2',
                                    'Model3'))
      
    } else if (length(formula_list) == 4) {
      models <- list(
        "Model1" = lm(formula_list[[1]], data = dat_signature_tbl),
        "Model2" = lm(formula_list[[2]], data = dat_signature_tbl),  
        "Model3" = lm(formula_list[[3]], data = dat_signature_tbl),  
        "Model4" = lm(formula_list[[4]], data = dat_signature_tbl)  
      )
      
      updatePickerInput(session = session, 
                        inputId = "modelsSelect",
                        choices = c('Model1',
                                    'Model2',
                                    'Model3',
                                    'Model4'))
      
    } else if (length(formula_list) == 5) {
      models <- list(
        "Model1" = lm(formula_list[[1]], data = dat_signature_tbl),
        "Model2" = lm(formula_list[[2]], data = dat_signature_tbl),  
        "Model3" = lm(formula_list[[3]], data = dat_signature_tbl),  
        "Model4" = lm(formula_list[[4]], data = dat_signature_tbl),  
        "Model5" = lm(formula_list[[5]], data = dat_signature_tbl)  
      )
      
      updatePickerInput(session = session, 
                        inputId = "modelsSelect",
                        choices = c('Model1',
                                    'Model2',
                                    'Model3',
                                    'Model4',
                                    'Model5'))
      
    } else if (length(formula_list) == 6) {
      models <- list(
        "Model1" = lm(formula_list[[1]], data = dat_signature_tbl),
        "Model2" = lm(formula_list[[2]], data = dat_signature_tbl),  
        "Model3" = lm(formula_list[[3]], data = dat_signature_tbl),  
        "Model4" = lm(formula_list[[4]], data = dat_signature_tbl),  
        "Model5" = lm(formula_list[[5]], data = dat_signature_tbl),  
        "Model6" = lm(formula_list[[6]], data = dat_signature_tbl)  
      )
      
      
      updatePickerInput(session = session, 
                        inputId = "modelsSelect",
                        choices = c('Model1',
                                    'Model2',
                                    'Model3',
                                    'Model4',
                                    'Model5',
                                    'Model6'))
      
    } else if (length(formula_list) == 7) {
      models <- list(
        "Model1" = lm(formula_list[[1]], data = dat_signature_tbl),
        "Model2" = lm(formula_list[[2]], data = dat_signature_tbl),  
        "Model3" = lm(formula_list[[3]], data = dat_signature_tbl),  
        "Model4" = lm(formula_list[[4]], data = dat_signature_tbl),  
        "Model5" = lm(formula_list[[5]], data = dat_signature_tbl),  
        "Model6" = lm(formula_list[[6]], data = dat_signature_tbl),  
        "Model7" = lm(formula_list[[7]], data = dat_signature_tbl) 
      )
      
      updatePickerInput(session = session, 
                        inputId = "modelsSelect",
                        choices = c('Model1',
                                    'Model2',
                                    'Model3',
                                    'Model4',
                                    'Model5',
                                    'Model6',
                                    'Model7'))
      
    } else if (length(formula_list) == 8) {
      models <- list(
        "Model1" = lm(formula_list[[1]], data = dat_signature_tbl),
        "Model2" = lm(formula_list[[2]], data = dat_signature_tbl),  
        "Model3" = lm(formula_list[[3]], data = dat_signature_tbl),  
        "Model4" = lm(formula_list[[4]], data = dat_signature_tbl),  
        "Model5" = lm(formula_list[[5]], data = dat_signature_tbl),  
        "Model6" = lm(formula_list[[6]], data = dat_signature_tbl),  
        "Model7" = lm(formula_list[[7]], data = dat_signature_tbl),  
        "Model8" = lm(formula_list[[8]], data = dat_signature_tbl)  
      )
      
      updatePickerInput(session = session, 
                        inputId = "modelsSelect",
                        choices = c('Model1',
                                    'Model2',
                                    'Model3',
                                    'Model4',
                                    'Model5',
                                    'Model6',
                                    'Model7',
                                    'Model8'))
      
    } else if (length(formula_list) == 9) {
      models <- list(
        "Model1" = lm(formula_list[[1]], data = dat_signature_tbl),
        "Model2" = lm(formula_list[[2]], data = dat_signature_tbl),  
        "Model3" = lm(formula_list[[3]], data = dat_signature_tbl),  
        "Model4" = lm(formula_list[[4]], data = dat_signature_tbl),  
        "Model5" = lm(formula_list[[5]], data = dat_signature_tbl),  
        "Model6" = lm(formula_list[[6]], data = dat_signature_tbl),  
        "Model7" = lm(formula_list[[7]], data = dat_signature_tbl),  
        "Model8" = lm(formula_list[[8]], data = dat_signature_tbl),  
        "Model9" = lm(formula_list[[9]], data = dat_signature_tbl) 
      )
      
      
      updatePickerInput(session = session, 
                        inputId = "modelsSelect",
                        choices = c('Model1',
                                    'Model2',
                                    'Model3',
                                    'Model4',
                                    'Model5',
                                    'Model6',
                                    'Model7',
                                    'Model8',
                                    'Model9'))
    }  else if (length(formula_list) == 10) {
      models <- list(
        "Model1" = lm(formula_list[[1]], data = dat_signature_tbl),
        "Model2" = lm(formula_list[[2]], data = dat_signature_tbl),  
        "Model3" = lm(formula_list[[3]], data = dat_signature_tbl),  
        "Model4" = lm(formula_list[[4]], data = dat_signature_tbl),  
        "Model5" = lm(formula_list[[5]], data = dat_signature_tbl),  
        "Model6" = lm(formula_list[[6]], data = dat_signature_tbl),  
        "Model7" = lm(formula_list[[7]], data = dat_signature_tbl),  
        "Model8" = lm(formula_list[[8]], data = dat_signature_tbl),  
        "Model9" = lm(formula_list[[9]], data = dat_signature_tbl),  
        "Model10" = lm(formula_list[[10]], data = dat_signature_tbl) 
      )
      
      
      updatePickerInput(session = session, 
                        inputId = "modelsSelect",
                        choices = c('Model1',
                                    'Model2',
                                    'Model3',
                                    'Model4',
                                    'Model5',
                                    'Model6',
                                    'Model7',
                                    'Model8',
                                    'Model9',
                                    'Model10'))
    }
    
    updateProgressBar(
      session = session,
      id = "pb2",
      value = 60
    )
    
    models_coefs_list <- purrr::map(1:length(models), ~ tidy(models[[.x]]) %>% select(term, estimate))
    names(models_coefs_list) <- paste0("Model",1:length(models_coefs_list))
    
    models_coefs_df(models_coefs_list)
    
    models_table <- modelsummary(models,
                                 fmt = 4,
                                 gof_omit = 'Log.Lik.|F',
                                 statistic = NULL,
                                 escape = FALSE,
                                 title = 'Time Series Linear Models Comparison',
                                 stars = c('+' = 0.2, '*' = 0.1, '**' = 0.05, '***' = 0.01))
    
    models_results_table(models_table)
    
    models_stats_info(purrr::map_dfr(1:length(models), ~ get_model_info(models[[.x]], .x)))
    
    vif_df <- purrr::map_dfr(models, ~ rms::vif(.x)) %>%
      mutate(across(everything(), ~ round(.x, 2))) %>%
      select(!(starts_with('date'))) %>%
      select(!(starts_with('month')))
    
    vif_df$model <- paste0("Model",1:nrow(vif_df))
    
    vif_table(vif_df %>% select(model, everything()))
    
    updateProgressBar(
      session = session,
      id = "pb2",
      value = 70
    )
    
    train_data <- training(splits)
    test_data <- testing(splits)
    
    splitsData(bind_rows(train_data, test_data))
    
    model_tbl_linear <- modeltime_table()
    
    for (i in 1:length(formula_list)) {
      
      model_fit <- linear_reg() %>%
        set_engine("lm") %>%
        fit(as.formula(formula_list[[i]]), data = train_data)
      
      model_tbl_linear <- model_tbl_linear %>%
        add_modeltime_model(model_fit)
      
      model_tbl_linear <- model_tbl_linear %>%
        update_modeltime_description(i, paste0("Model",i))
    }
    
    updateProgressBar(
      session = session,
      id = "pb2",
      value = 80
    )
    
    calibration_tbl_training <- model_tbl_linear %>%
      modeltime_calibrate(train_data) %>%
      mutate(.type = 'Training')
    
    
    calibration_tbl_linear1 <- calibration_tbl_training %>%
      
      combine_cis(splits)
    
    updateProgressBar(
      session = session,
      id = "pb2",
      value = 85
    )
    
    calibration_tbl_linear2 <- calibration_tbl_training %>%
      
      modeltime_calibrate(
        new_data = training(splits)
      ) %>%
      unnest(cols = c(.calibration_data))
    
    models_results_fit(calibration_tbl_linear2)
    models_results_predict(calibration_tbl_linear1)
    
    updateProgressBar(
      session = session,
      id = "pb2",
      value = 90
    )
    
    models_fig <- model_tbl_linear %>%
      modeltime_forecast(new_data = train_data,
                         actual_data = dat_signature_tbl) %>%
      plot_modeltime_forecast()
    
    models_fitted_fig(models_fig)  
    
    updateProgressBar(
      session = session,
      id = "pb2",
      value = 100
    )
    
    
    
  })
  
  output$models_table <- function(){ 
    
    req(models_results_table())
    
    models_results_table() %>%
      kable_styling(font_size = 14)
  }
  
  output$models_vif_table <- function(){ 
    
    req(vif_table())
    
    kable(vif_table(), caption = "Models VIFs") %>%
      kable_styling(font_size = 14)
  }
  
  
  output$tsModelR2Plot <- renderPlotly({
    
    req(models_stats_info())
    
    fig <- plot_ly(models_stats_info(), x = ~name, y = ~adj.r.squared, type = 'scatter', mode = 'lines', line = list(color = 'rgb(44, 62, 80)'))
    
    fig <- fig %>% layout(title = "Models Fittings: ",
                          xaxis = list(title = ""),
                          yaxis = list (title = "Adjusted R Squared"),
                          hovermode = "x unified",
                          legend = list(orientation = 'h'))
    
    
  })
  
  output$tsModelAICPlot <- renderPlotly({
    
    req(models_stats_info())
    
    fig <- plot_ly(models_stats_info(), x = ~name, y = ~aic, type = 'scatter', mode = 'lines', line = list(color = 'rgb(44, 62, 80)'))
    
    fig <- fig %>% layout(title = "Models Predictions: ",
                          xaxis = list(title = ""),
                          yaxis = list (title = "AIC"),
                          hovermode = "x unified",
                          legend = list(orientation = 'h'))
    
    
  })
  
  
  
  
  output$tsModelsFittingPlot <- renderPlotly({
    
    req(models_fitted_fig())
    
    fig <- models_fitted_fig()
    
    fig <- fig %>% layout(title = paste0("Models Fitting Comparison Plot: ", input$depVarSelect),
                          xaxis = list(title = "Date"),
                          yaxis = list (title = input$depVarSelect)
    )
    
  })
  
  
  
  output$tsModelsSensitivityPlot <- renderPlotly({
    
    req(models_results_fit())
    req(models_results_predict())
    
    
    data_actual <- models_results_fit() %>%  
      select(date, .actual, .model_desc) %>%
      rename(actual = .actual) %>%
      filter(.model_desc == input$modelsSelect)
    
    data_fitted <- models_results_fit() %>% 
      select(date, .prediction, .model_desc) %>%
      rename(fitted = .prediction) %>%
      filter(.model_desc == input$modelsSelect)
    
    
    data_predict <- models_results_predict() %>% 
      select(.index, .value, .model_desc, .conf_lo_95, .conf_hi_95, .conf_lo_90, .conf_hi_90, .conf_lo_85, .conf_hi_85, .conf_lo_80, .conf_hi_80) %>%
      rename(prediction = .value,
             date = .index) %>%
      filter(.model_desc == input$modelsSelect)
    
    
    data_predict_head1 <- head(data_predict, 1) %>%
      select(date, prediction, .model_desc) %>%
      rename(fitted = prediction)
    
    data_fitted <- bind_rows(data_fitted, data_predict_head1)
    
    data <- data_actual %>%
      full_join(data_fitted) %>%
      full_join(data_predict)
    
    fig <- plot_ly(data, x = ~date, y = ~actual, name = 'Actual', type = 'scatter', mode = 'lines', line = list(color = 'rgb(153, 0, 51)'))
    
    fig <- fig %>%
      add_trace(y = ~fitted, name = 'Fitted', line = list(color = 'rgb(44, 62, 80)', dash = 'dash')) 
    
    
    if ("95%" %in% input$forecastCI) {
      
      fig <- fig %>%
        add_trace(y = ~.conf_hi_95, name = 'High Conf', line = list(color = 'transparent'), type = 'scatter', mode = 'lines')
      
      fig <- fig %>% 
        add_trace(y = ~.conf_lo_95, type = 'scatter', mode = 'lines',
                  fill = 'tonexty', fillcolor='rgb(204,204,254)', line = list(color = 'transparent'),
                  showlegend = FALSE, name = 'Low Conf') 
      
    }
    
    if ("90%" %in% input$forecastCI) {
      
      fig <- fig %>%
        add_trace(y = ~.conf_hi_90, name = 'High Conf', line = list(color = 'transparent'), type = 'scatter', mode = 'lines')
      
      fig <- fig %>% 
        add_trace(y = ~.conf_lo_90, type = 'scatter', mode = 'lines',
                  fill = 'tonexty', fillcolor='rgb(165,165,237)', line = list(color = 'transparent'),
                  showlegend = FALSE, name = 'Low Conf') 
      
    }
    
    if ("85%" %in% input$forecastCI) {
      
      fig <- fig %>%
        add_trace(y = ~.conf_hi_85, name = 'High Conf', line = list(color = 'transparent'), type = 'scatter', mode = 'lines')
      
      fig <- fig %>% 
        add_trace(y = ~.conf_lo_85, type = 'scatter', mode = 'lines',
                  fill = 'tonexty', fillcolor='rgb(153,153,237)', line = list(color = 'transparent'),
                  showlegend = FALSE, name = 'Low Conf') 
      
    }
    
    if ("80%" %in% input$forecastCI) {
      
      fig <- fig %>%
        add_trace(y = ~.conf_hi_80, name = 'High Conf', line = list(color = 'transparent'), type = 'scatter', mode = 'lines')
      
      fig <- fig %>% 
        add_trace(y = ~.conf_lo_80, type = 'scatter', mode = 'lines',
                  fill = 'tonexty', fillcolor='rgb(138,138,237)', line = list(color = 'transparent'),
                  showlegend = FALSE, name = 'Low Conf') 
      
    }
    
    fig <- fig %>%
      add_trace(y = ~prediction, name = 'Prediction', mode = 'lines+markers', line = list(color = 'rgb(44, 62, 80)'), 
                marker = list(
                  color = 'rgb(44, 62, 80)',
                  size = 4,
                  opacity = 0.8,
                  line = list(
                    color = 'rgb(44, 62, 80)',
                    width = 1
                  )
                )) 
    
    fig <- fig %>% layout(title = paste0("Model Fitting and Forecasting: ", input$depVarSelect, " (",input$modelsSelect,")"),
                          xaxis = list(title = "Date"),
                          yaxis = list (title = input$depVarSelect),
                          hovermode = "x unified",
                          legend = list(orientation = 'h'),
                          dragmode = "select") %>%
      event_register("plotly_selecting")
  })
  
  output$tsModelsResidualsPlot <- renderPlotly({
    
    req(models_results_fit())
    
    data_residuals <- models_results_fit() %>% 
      select(date, .residuals, .model_desc) %>%
      rename(residuals = .residuals) %>%
      filter(.model_desc == input$modelsSelect)
    
    
    fig <- plot_ly(data_residuals, x = ~date, y = ~residuals, name = 'Residuals', type = 'scatter', mode = 'lines', line = list(color = 'rgb(44, 62, 80)'))
    
    fig <- fig %>% layout(title = paste0("Model Residuals: ", input$depVarSelect, " (",input$modelsSelect,")"),
                          xaxis = list(title = "Date"),
                          yaxis = list (title = input$depVarSelect),
                          hovermode = "x unified",
                          legend = list(orientation = 'h'))
    
  })
  
  
  output$modelInfo <- renderDT({
    
    req(splitsData())
    req(input$refDateSelect)
    req(selModelDate())
    
    colfunc <- colorRampPalette(c("grey", "white"))
    
    d <- event_data("plotly_click")
    
    if (!is.null(d)) {
      selModelDate(d$x[1])
    }
    
    
    datSel <- dplyr::filter(splitsData(), date == selModelDate()) %>%
      select(-month.lbl)
    
    current_month <- datSel$month[1]
    
    datSel <- datSel%>%
      tidyr::pivot_longer(cols = names(datSel)[-1], names_to = 'term', values_to = "actual_values") 
    
    model_coefs <- models_coefs_df()[[input$modelsSelect]] %>%
      left_join(datSel) %>%
      select(-date)
    
    model_coefs$actual_values[1] <- 1
    
    if (current_month != 1)
      model_coefs$actual_values[1+current_month] <- 1
    
    model_coefs[is.na(model_coefs)] <- 0
    
    model_coefs <- model_coefs %>%
      mutate(Lift = estimate * actual_values)
    
    total_lift <- sum(model_coefs$Lift)
    
    model_coefs <- model_coefs %>%
      add_row(term = 'Total', estimate = NA, actual_values = NA, Lift = total_lift) %>%
      mutate(estimate = round(estimate, 4),
             `Actual Values` = round(actual_values, 2),
             Lift = round(Lift, 4),
             `Lift Percent` = round(Lift / total_lift, 3),
             `Simulation Values` = round(actual_values, 2),
             `Simulation Lift` = Lift
      ) 
    
    simulationData(model_coefs %>%
                     mutate(date = selModelDate()) %>%
                     select(date, term, estimate, "Actual Values", Lift))
    
    model_coefs <- model_coefs %>%
      select(term, estimate, "Actual Values", Lift)
    
    
    
    if (input$refDateSelect != "Not Selected") {
      
      ref_date = ymd(input$refDateSelect)
      
      datSel <- dplyr::filter(splitsData(), date == ref_date) %>%
        select(-month.lbl)
      
      current_month <- datSel$month[1]
      
      datSel <- datSel%>%
        tidyr::pivot_longer(cols = names(datSel)[-1], names_to = 'term', values_to = "actual_values") 
      
      model_coefs_ref <- models_coefs_df()[[input$modelsSelect]] %>%
        left_join(datSel) %>%
        select(-date)
      
      model_coefs_ref$actual_values[1] <- 1
      
      if (current_month != 1)
        model_coefs_ref$actual_values[1+current_month] <- 1
      
      model_coefs_ref[is.na(model_coefs_ref)] <- 0
      
      model_coefs_ref <- model_coefs_ref %>%
        mutate(Lift = estimate * actual_values)
      
      total_lift <- sum(model_coefs_ref$Lift)
      
      model_coefs_ref <- model_coefs_ref %>%
        add_row(term = 'Total', estimate = NA, actual_values = NA, Lift = total_lift) %>%
        mutate(estimate = round(estimate, 4),
               actual_values = round(actual_values, 2),
               Lift = round(Lift, 4),
               lift_pct = round(Lift / total_lift, 3)) %>%
        rename(`Reference Values` = actual_values,
               `Reference Lift` = Lift,
               `Ref Lift Percent` = lift_pct) %>%
        select(term, `Reference Values`, `Reference Lift`, `Ref Lift Percent`)
      
      model_coefs <- model_coefs %>%
        inner_join(model_coefs_ref) %>%
        select(term, estimate, `Reference Values`, `Reference Lift`, `Actual Values`, Lift) 
      
      range1 <- unique(c(model_coefs$Lift, model_coefs$`Reference Lift`))
      
      return(datatable(model_coefs,
                       caption = paste0("Lift Table:  ", as.character(selModelDate()), " (Reference Date - ", input$refDateSelect,")"),
                       editable = FALSE,
                       options = list(dom = 't',
                                      pageLength = 50,
                                      ordering = F)) %>% 
               formatStyle("Lift", 
                           backgroundColor = styleEqual(sort(range1, 
                                                             decreasing = TRUE),
                                                        colfunc(length(range1)))
               )  %>% 
               formatStyle("Reference Lift", 
                           backgroundColor = styleEqual(sort(range1, 
                                                             decreasing = TRUE),
                                                        colfunc(length(range1)))
               )
             
      )
      
      
    } else {
      
      range1 <- unique(model_coefs$Lift)
      
      return(datatable(model_coefs,
                       caption = paste0("Lift Table:  ", as.character(selModelDate()), " (Reference Date - ", input$refDateSelect,")"),
                       editable = FALSE,
                       options = list(dom = 't',
                                      pageLength = 50,
                                      ordering = F,
                                      columnDefs = list(list(className = 'dt-center', targets = 2:4)))) %>% 
               formatStyle("Lift", 
                           backgroundColor = styleEqual(sort(range1, 
                                                             decreasing = TRUE),
                                                        colfunc(length(unique(model_coefs$Lift))))
               )  
             
      )
      
    }
    
  })
  
  
  
  simulationData <- reactiveVal(tibble::tibble(date = as.character('2020-3-1'), term='Total', estimate = 0, "Actual Values" = 0, Lift = 0))
  
  simulationTableResults <- dtedit(
    input, output,
    name = 'simulationTable',
    thedata = simulationData,
    callback.update = simulationTable.update.callback,
    #title.edit = "Simulation Edit",
    show.delete = FALSE,
    show.insert = FALSE,
    show.copy = FALSE,
    icon.edit = shiny::icon("edit"),
    datatable.options = list(
      dom = 't',
      pageLength = 20,
      ordering = F,
      columnDefs = list(list(className = 'dt-center', targets = 1:3))),
    datatable.caption = ("Simulation Table: ")
  ) 
  
  simulationTable.update.callback <- function(data, olddata, row) {
    # 'data' contains the dataframe *after* the row has been updated
    # 'row' is the row number where data has been updated
    # 'olddata' is the previous version of the data
    
    data <- data %>%
      mutate(Lift = estimate * `Actual Values`)
    
    total_lift <- sum(data$Lift, na.rm = TRUE)
    data$Lift[nrow(data)] <- total_lift
    
    data$Lift <- round(data$Lift, 4)
    
    simulation_lift(total_lift)
    
    return(data)
  }
  
  
  output$tsModelsSimulationPlot <- renderPlotly({
    
    req(models_results_fit())
    req(models_results_predict())
    
    
    data_actual <- models_results_fit() %>%  
      select(date, .actual, .model_desc) %>%
      rename(actual = .actual) %>%
      filter(.model_desc == input$modelsSelect)
    
    data_fitted <- models_results_fit() %>% 
      select(date, .prediction, .model_desc) %>%
      rename(fitted = .prediction) %>%
      filter(.model_desc == input$modelsSelect)
    
    
    data_predict <- models_results_predict() %>% 
      select(.index, .value, .model_desc, .conf_lo_95, .conf_hi_95, .conf_lo_90, .conf_hi_90, .conf_lo_85, .conf_hi_85, .conf_lo_80, .conf_hi_80) %>%
      rename(prediction = .value,
             date = .index) %>%
      filter(.model_desc == input$modelsSelect)
    
    
    data_predict_head1 <- head(data_predict, 1) %>%
      select(date, prediction, .model_desc) %>%
      rename(fitted = prediction)
    
    data_fitted <- bind_rows(data_fitted, data_predict_head1)
    
    data <- data_actual %>%
      full_join(data_fitted) %>%
      full_join(data_predict)
    
    if (simulation_lift()) {
      
      data <- data %>%
        mutate(simulation = ifelse(date == selModelDate(), simulation_lift(), NA))
    }
    
    fig <- plot_ly(data, x = ~date, y = ~actual, name = 'Actual', type = 'scatter', mode = 'lines', line = list(color = 'rgb(153, 0, 51)'))
    
    fig <- fig %>%
      add_trace(y = ~fitted, name = 'Fitted', line = list(color = 'rgb(44, 62, 80)', dash = 'dash')) 
    
    
    if ("95%" %in% input$forecastCI) {
      
      fig <- fig %>%
        add_trace(y = ~.conf_hi_95, name = 'High Conf', line = list(color = 'transparent'), type = 'scatter', mode = 'lines')
      
      fig <- fig %>% 
        add_trace(y = ~.conf_lo_95, type = 'scatter', mode = 'lines',
                  fill = 'tonexty', fillcolor='rgb(204,204,254)', line = list(color = 'transparent'),
                  showlegend = FALSE, name = 'Low Conf') 
      
    }
    
    if ("90%" %in% input$forecastCI) {
      
      fig <- fig %>%
        add_trace(y = ~.conf_hi_90, name = 'High Conf', line = list(color = 'transparent'), type = 'scatter', mode = 'lines')
      
      fig <- fig %>% 
        add_trace(y = ~.conf_lo_90, type = 'scatter', mode = 'lines',
                  fill = 'tonexty', fillcolor='rgb(165,165,237)', line = list(color = 'transparent'),
                  showlegend = FALSE, name = 'Low Conf') 
      
    }
    
    if ("85%" %in% input$forecastCI) {
      
      fig <- fig %>%
        add_trace(y = ~.conf_hi_85, name = 'High Conf', line = list(color = 'transparent'), type = 'scatter', mode = 'lines')
      
      fig <- fig %>% 
        add_trace(y = ~.conf_lo_85, type = 'scatter', mode = 'lines',
                  fill = 'tonexty', fillcolor='rgb(153,153,237)', line = list(color = 'transparent'),
                  showlegend = FALSE, name = 'Low Conf') 
      
    }
    
    if ("80%" %in% input$forecastCI) {
      
      fig <- fig %>%
        add_trace(y = ~.conf_hi_80, name = 'High Conf', line = list(color = 'transparent'), type = 'scatter', mode = 'lines')
      
      fig <- fig %>% 
        add_trace(y = ~.conf_lo_80, type = 'scatter', mode = 'lines',
                  fill = 'tonexty', fillcolor='rgb(138,138,237)', line = list(color = 'transparent'),
                  showlegend = FALSE, name = 'Low Conf') 
      
    }
    
    fig <- fig %>%
      add_trace(y = ~prediction, name = 'Prediction', mode = 'lines+markers', line = list(color = 'rgb(44, 62, 80)'), 
                marker = list(
                  color = 'rgb(44, 62, 80)',
                  size = 4,
                  opacity = 0.8,
                  line = list(
                    color = 'rgb(44, 62, 80)',
                    width = 1
                  )
                )) 
    
    # add simulation point
    if (simulation_lift()) {
      
      fig <- fig %>%
        add_trace(y = ~simulation, name = 'Simulation', line = list(color = 'transparent'), type = 'scatter', mode = 'markers', 
                  marker = list(size = 10,
                                color = 'rgba(112, 48, 160, .9)',
                                line = list(color = 'rgba(152, 0, 0, .8)',
                                            width = 2)))
    }
    
    fig <- fig %>% layout(title = paste0("Model Fitting and Forecasting: ", input$depVarSelect, " (",input$modelsSelect,")"),
                          xaxis = list(title = "Date"),
                          yaxis = list (title = input$depVarSelect),
                          hovermode = "x unified",
                          legend = list(orientation = 'h'),
                          dragmode = "select") %>%
      event_register("plotly_selecting")
  })
  
  lift_table_df <- reactiveVal(NULL)
  
  observeEvent(input$modelsSelect, {
    
    req(models_coefs_df())
    
    model_coefs <- models_coefs_df()[[input$modelsSelect]]
    
    lift_table <- gen_lift_table(model_coefs, sel_base())
    
    if (!is.null(lift_table)) {
      
      if (nrow(lift_table) > 0) {
        
        lift_table_df(lift_table)
        
        varlist <- setdiff(names(lift_table), "date")
        
        var_df <- tibble::tibble(name = varlist)
        sel_desc <- var_df %>%
          inner_join(varlist_dict) %>%
          pull(desc)
        
        updatePickerInput(session, "varsLiftSelect",
                          label = "Independent Variable for Lift Chart:",
                          choices = c(sel_desc),
                          selected = sel_desc[1])
        
      } else {
        
        updatePickerInput(session, "varsLiftSelect",
                          label = "Independent Variable for Lift Chart:",
                          choices = c(""),
                          selected ="None")
      }
    } else {
      
      updatePickerInput(session, "varsLiftSelect",
                        label = "Independent Variable for Lift Chart:",
                        choices = c(""),
                        selected ="None")
    }
    
    
    
  })
  
  
  output$tsModelsLiftsPlot <- renderPlotly({
    
    req(input$modelsSelect)
    req(input$varsLiftSelect)
    req(lift_table_df())
    
    var_df <- tibble::tibble(desc = input$varsLiftSelect)
    sel_var <- var_df %>%
      inner_join(varlist_dict) %>%
      pull(name)
    
    lift_table <- lift_table_df()
    
    fig <- plot_ly(x =~ lift_table[["date"]], y =~ lift_table[[sel_var]], type = 'scatter', mode = 'lines', line = list(color = 'rgb(44, 62, 80)'))
    fig <- fig %>% layout(title = paste0("Model Lift: ", input$varsLiftSelect, " (", input$modelsSelect, ")"),
                          xaxis = list(title = ""),
                          yaxis = list (title = "Lift"),
                          hovermode = "x unified")
    
  })
  
  output$blankTest <- renderUI({
    
    req(splitsData())
    req(selModelDate())
    
    colfunc <- colorRampPalette(c("grey", "white"))
    
    d <- event_data("plotly_click")
    
    if (!is.null(d)) {
      selModelDate(d$x[1])
    }
    
    
    datSel <- dplyr::filter(splitsData(), date == selModelDate()) %>%
      select(-month.lbl)
    
    current_month <- datSel$month[1]
    
    datSel <- datSel%>%
      tidyr::pivot_longer(cols = names(datSel)[-1], names_to = 'term', values_to = "actual_values") 
    
    model_coefs <- models_coefs_df()[[input$modelsSelect]] %>%
      left_join(datSel) %>%
      select(-date)
    
    model_coefs$actual_values[1] <- 1
    
    if (current_month != 1)
      model_coefs$actual_values[1+current_month] <- 1
    
    model_coefs[is.na(model_coefs)] <- 0
    
    model_coefs <- model_coefs %>%
      mutate(Lift = estimate * actual_values)
    
    total_lift <- sum(model_coefs$Lift)
    
    model_coefs <- model_coefs %>%
      add_row(term = 'Total', estimate = NA, actual_values = NA, Lift = total_lift) %>%
      mutate(estimate = round(estimate, 4),
             `Actual Values` = round(actual_values, 2),
             Lift = round(Lift, 4),
             `Lift Percent` = round(Lift / total_lift, 3),
             `Simulation Values` = round(actual_values, 2),
             `Simulation Lift` = Lift
      ) 
    
    simulationData(model_coefs %>%
                     mutate(date = selModelDate()) %>%
                     select(date, term, estimate, "Actual Values", Lift))
    
    simulation_lift(total_lift)
    
    HTML("")
    
  })
  
  

})
