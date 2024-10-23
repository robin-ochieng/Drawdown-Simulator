list(
# Reactive Functions -----------------------------------------------------
  drawdown_inputs <- eventReactive({input$drawdown_resim; input$drawdown_submit %% (num.quest + 2) > num.quest}, {
    return(reactiveValuesToList(input))
  }, ignoreNULL = FALSE),
  
  drawdown_simulations_reactive <- eventReactive({input$drawdown_resim; input$drawdown_submit %% (num.quest + 2) > num.quest}, {
    drawdown_inputs = drawdown_inputs()
    return(Drawdown_Simulations(retire_age = drawdown_inputs$drawdown_retire_age, 
                                start_capital = drawdown_inputs$drawdown_start_capital, 
                                withdraw_freq = drawdown_inputs$drawdown_withdraw_freq, 
                                annual_mean_return = drawdown_inputs$drawdown_annual_mean_return, 
                                annual_ret_std_dev = drawdown_inputs$drawdown_annual_ret_std_dev, 
                                annual_inflation = drawdown_inputs$drawdown_annual_inflation, 
                                annual_inf_std_dev = drawdown_inputs$drawdown_annual_inf_std_dev, 
                                n_sim = 10000, 
                                withdraw_type = drawdown_inputs$drawdown_withdraw_type, 
                                annual_withdrawals = drawdown_inputs$drawdown_annual_withdrawals, 
                                percent_withdrawal = drawdown_inputs$drawdown_percent_withdrawal))
  }, ignoreNULL = FALSE),
  
  drawdown_paths_reactive <- eventReactive({input$drawdown_resim; input$drawdown_submit %% (num.quest + 2) > num.quest}, {
    return(Drawdown_Paths(drawdown_simulations_reactive()))
  }, ignoreNULL = FALSE),

  drawdown_withdrawals_reactive <- eventReactive({input$drawdown_resim; input$drawdown_submit %% (num.quest + 2) > num.quest}, {
    return(Drawdown_Withdrawals(drawdown_simulations_reactive()))
  }, ignoreNULL = FALSE),

  drawdown_life_ex_reactive <- eventReactive({input$drawdown_resim; input$drawdown_submit %% (num.quest + 2) > num.quest}, {
    drawdown_inputs = drawdown_inputs()
    return(round_to_fraction(exn(ILT15_female_reduced, drawdown_inputs$drawdown_retire_age), p_list[match(drawdown_inputs$drawdown_withdraw_freq, freq_list_drawdown)]))
  }, ignoreNULL = FALSE),

# Output Functions --------------------------------------------------------
  output$drawdown_text_life_ex <- renderText({
    drawdown_inputs = drawdown_inputs()
    return(c(round_2d(drawdown_life_ex_reactive()), " Years"))
  }),
  
  output$drawdown_text_median_fund_life_ex <- renderText({
    drawdown_inputs = drawdown_inputs()
    median = Drawdown_Percentile_Life_Ex(Drawdown_Paths = drawdown_paths_reactive(),
                                         freq = drawdown_inputs$drawdown_withdraw_freq,
                                         age = drawdown_inputs$drawdown_retire_age)
    return(c("€", round_2d(median, T)))
  }),
  
  output$drawdown_text_ruin_prob_life_ex <- renderText({
    drawdown_inputs = drawdown_inputs()
    ruin = 100 * Drawdown_Ruin_Life_Ex(Drawdown_Paths = drawdown_paths_reactive(),
                                       freq = drawdown_inputs$drawdown_withdraw_freq,
                                       age = drawdown_inputs$drawdown_retire_age)
    return(c(round_2d(ruin), "%"))
  }),
  
  output$drawdown_table <- renderDataTable({
    drawdown_inputs = drawdown_inputs()
    freq = p_list[match(drawdown_inputs$drawdown_withdraw_freq, freq_list_drawdown)]
    series = list(1, (freq * seq(5, (length(drawdown_paths_reactive()[1, ]) / freq), 5) + 1))
    points = list(drawdown_life_ex_reactive())
    colour = list('yellow')
    return(Drawdown_Table(Drawdown_Paths = drawdown_paths_reactive(),
                          Drawdown_Withdrawals = drawdown_withdrawals_reactive(),
                          freq = drawdown_inputs$drawdown_withdraw_freq,
                          series = series,
                          points = points,
                          colour = colour))
  }),
  
  output$drawdown_plot_sims <- renderPlot({
    drawdown_inputs = drawdown_inputs()
    return(Drawdown_Plot_Sims(Drawdown_Paths = drawdown_paths_reactive(), 
                              freq = drawdown_inputs$drawdown_withdraw_freq, 
                              n_sims = 25, 
                              points = list(drawdown_life_ex_reactive()),
                              colour = list('black')))
  }),

  output$drawdown_plot_percentiles <- renderPlotly({
    drawdown_inputs = drawdown_inputs()
    return(Drawdown_Plot_Percentile(Drawdown_Paths = drawdown_paths_reactive(),
                                    Drawdown_Withdrawals = drawdown_withdrawals_reactive(),
                                    freq = drawdown_inputs$drawdown_withdraw_freq,
                                    lower = 0.05, upper = 0.95,
                                    points = list(drawdown_life_ex_reactive()),
                                    colour = list('black')))
  }),

# Observe Event Functions -------------------------------------------------
  observeEvent(input$drawdown_withdraw_type,{
    if(input$drawdown_withdraw_type == T) {
      shinyjs::hide("drawdown_annual_withdrawals_input")
      shinyjs::show("drawdown_percent_withdrawal_input")
      updateNumericInputIcon(session, "drawdown_percent_withdrawal", value = 4)
      enable("drawdown_percent_withdrawal")
      disable("drawdown_annual_withdrawals")
      updateNumericInputIcon(session, "drawdown_annual_withdrawals", value = NA)
    } else {
      shinyjs::show("drawdown_annual_withdrawals_input")
      shinyjs::hide("drawdown_percent_withdrawal_input")
      updateNumericInputIcon(session, "drawdown_annual_withdrawals", value = 12000)
      enable("drawdown_annual_withdrawals")
      disable("drawdown_percent_withdrawal")
      updateNumericInputIcon(session, "drawdown_percent_withdrawal", value = NA)
    }
  }),

  observe({
    shinyjs::hide("drawdown_submit")
  
    if((input$drawdown_surveydisplay %% 2 == 1 & input$drawdown_surveydisplay != 0))
      shinyjs::show("drawdown_submit")
  }),

# UI Functions ------------------------------------------------------------
  drawdown_ui_reactive <- reactive({
    mainui = list(
      column(12,
    fluidRow(
      box(
        title = "Life Expectancy", status = 'primary', solidHeader = T, width = 4,
        h3(textOutput('drawdown_text_life_ex'))
      ),
      box(
        title = "Median Fund Value", status = "primary", solidHeader = T, width = 4,
        h3(textOutput("drawdown_text_median_fund_life_ex"))
      ),
      box(
        title = "Probability of Ruin", status = "primary", solidHeader = T, width = 4,
        h3(textOutput("drawdown_text_ruin_prob_life_ex"))
      )
    ),
      tabBox(type = "tabs", width = 12,
             tabPanel("Summary", plotlyOutput("drawdown_plot_percentiles")),
             tabPanel("Simulations", plotOutput("drawdown_plot_sims")),
             tabPanel("Table", DT::dataTableOutput("drawdown_table"), rownames = FALSE)
             )
      )
    )
    return(riskprofilerui(session = session,
                          surveydisplay = input$drawdown_surveydisplay,
                          submit = input$drawdown_submit, 
                          page = "drawdown",
                          input_mean_return = "drawdown_annual_mean_return", 
                          input_ret_std_dev = "drawdown_annual_ret_std_dev",
                          mainui = mainui))
  }),

  output$drawdown_save_results_text <- renderText({
    if(input$drawdown_submit %% (num.quest + 2) > 0 && (input$drawdown_submit %% (num.quest + 2) <= num.quest)){
      save_results(session, input$drawdown_submit, input$drawdown_survey)
    }
  }),

  output$drawdown_ui <- renderUI({
    drawdown_ui_reactive()
  })
)