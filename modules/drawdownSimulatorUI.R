list(
  sidebarLayout(
    sidebarPanel(actionButton(inputId = "drawdown_resim", label = "Re-Run Simulation", style = "background-color: white; float:right", icon("random")),
                 br(), br(),
                 numericInputIcon(inputId = "drawdown_retire_age", label = "Age at Retirement:", value = 66, min = 55, max = getOmega(ILT15_female_reduced), icon = list(NULL, "Years")),
                 numericInputIcon(inputId = "drawdown_start_capital", label = "Starting Capital:", value = 300000, min = 0, icon = icon("euro")),
                 selectInput("drawdown_withdraw_freq", "Withdrawal Frequency:", freq_list_drawdown),
                 awesomeRadio("drawdown_withdraw_type", "Withdrawal Type:", choices = list("Fixed" = F, "Percentage" = T), inline = TRUE),
                 div(id = "drawdown_annual_withdrawals_input", numericInputIcon(inputId = "drawdown_annual_withdrawals", label = "Total Withdrawals per Annum:", value = 12000, min = 0, icon = icon("euro"))),
                 div(id = "drawdown_percent_withdrawal_input", numericInputIcon(inputId = "drawdown_percent_withdrawal", label = "Percentage Withdrawn per Annum:", value = 4, min = 0, max = 100, icon = list(NULL, icon("percent")))),
                 h5(strong("Portfolio Suggestion Tool:")),
                 actionButton(inputId = "drawdown_surveydisplay", label = "Risk Profiler", style = "background-color: white", icon("poll")),
                 bsTooltip("drawdown_surveydisplay", "Complete the survey to identify a suitable level of risk",
                           "right", options = list(container = "body")),
                 h6(textOutput("drawdown_save_results_text")),
                 numericInputIcon(inputId = "drawdown_annual_mean_return", label = "Mean Annual Return:", value = 3, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                 numericInputIcon(inputId = "drawdown_annual_ret_std_dev", label = "Standard Deviation of Annual Return:", value = 4, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                 numericInputIcon(inputId = "drawdown_annual_inflation", label = "Mean Annual Inflation:", value = 1.5, min = 0, max = 100, icon = list(NULL, icon("percent"))),
                 numericInputIcon(inputId = "drawdown_annual_inf_std_dev", label = "Standard Deviation of Annual Inflation:", value = 1, min = 0, max = 100, icon = list(NULL, icon("percent")))
    ),
    
    mainPanel(
      uiOutput("drawdown_ui"),
      column(1, actionButton("drawdown_submit", "Next"))
      )
  )
)