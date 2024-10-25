  list(


# Drawdown Functions ------------------------------------------------------
  Drawdown_Simulations <- function(retire_age, start_capital, withdraw_freq, annual_mean_return, annual_ret_std_dev, annual_inflation, annual_inf_std_dev, n_sim = 10000, withdraw_type = F, annual_withdrawals = 15000, percent_withdrawal = 4, life_table = ILT15_female_reduced, end_age = NA){
    
    p = p_list[match(withdraw_freq, freq_list_drawdown)]
    
    # Investment
    annual_mean_return = annual_mean_return / 100
    annual_ret_std_dev = annual_ret_std_dev / 100
    
    # Inflation
    annual_inflation = annual_inflation / 100
    annual_inf_std_dev = annual_inf_std_dev / 100
    
    # Withdrawals
    if (withdraw_type == F) {
      periodic_withdrawals = annual_withdrawals / p
    } else {
      periodic_withdrawals = start_capital * effective2Convertible(i = percent_withdrawal / 100, k = p) / p
    }
    
    # Time to Run
    if(is.na(end_age)){
      n_years = getOmega(life_table) - retire_age
    } else {
      n_years = end_age - retire_age
    }
    
    # number of periods to simulate
    n_obs = p * n_years
    
    # periodic Investment and Inflation assumptions
    periodic_mean_return = effective2Convertible(i=annual_mean_return,k=p)/p
    periodic_ret_std_dev = annual_ret_std_dev / sqrt(p)
    
    periodic_inflation = effective2Convertible(i=annual_inflation,k=p)/p
    periodic_inf_std_dev = annual_inf_std_dev / sqrt(p)
    
    #-------------------------------------
    # Simulation
    #-------------------------------------
    
    paths_matrix = matrix(0, n_sim, n_obs)
    withdrawals_matrix = matrix(0, n_sim, n_obs)
    
    periodic_invest_returns = numeric(n_obs - 1)
    periodic_inflation_returns = numeric(n_obs - 1)
    
    if(periodic_withdrawals > start_capital){
      paths_matrix[, 1] = 0
      withdrawals_matrix[, 1] = start_capital
      mylist = list(paths_matrix, withdrawals_matrix)
      return(mylist)
    }
    
    paths_matrix[, 1] = start_capital - periodic_withdrawals
    withdrawals_matrix[, 1] = periodic_withdrawals
    
    for(i in 1:n_sim){
      periodic_invest_returns = rnorm(n_obs - 1, mean = periodic_mean_return, sd = periodic_ret_std_dev)
      # To Not Adjust for Inflation on Fixed Withdrawals:
      # if (withdraw_type == F) {
      #   periodic_inflation_returns = numeric(n_obs - 1)
      # } else {
      #   periodic_inflation_returns = rnorm(n_obs - 1, mean = periodic_inflation, sd = periodic_inf_std_dev)
      # }
      periodic_inflation_returns = rnorm(n_obs - 1, mean = periodic_inflation, sd = periodic_inf_std_dev)
      
      next_fund_value = paths_matrix[i, 1] * (1 + periodic_invest_returns[1]) - ((1 + periodic_inflation_returns[1]) * periodic_withdrawals)
      if (next_fund_value <= 0) {
        withdrawals_matrix[i, 2] = paths_matrix[i, 1] * (1 + periodic_invest_returns[1])
        next
      }
      paths_matrix[i, 2] = next_fund_value
      withdrawals_matrix[i, 2] = ((1 + periodic_inflation_returns[1]) * periodic_withdrawals)
      
      for(j in 3:n_obs){
        next_fund_value = paths_matrix[i, j - 1] * (1 + periodic_invest_returns[j - 1]) - (prod(1 + periodic_inflation_returns[1:j - 1]) * periodic_withdrawals)
        if(next_fund_value <= 0){
          withdrawals_matrix[i, j] = paths_matrix[i, j - 1] * (1 + periodic_invest_returns[j - 1])
          break
        } else {
          paths_matrix[i, j] = next_fund_value
          withdrawals_matrix[i, j] = (prod(1 + periodic_inflation_returns[1:j - 1]) * periodic_withdrawals)
        }
      }
    }
    mylist = list(paths_matrix, withdrawals_matrix)
    return(mylist)
  },

  Drawdown_Paths <- function(Drawdown_Simulations){
    return(Drawdown_Simulations[[1]])
  },

  Drawdown_Withdrawals <- function(Drawdown_Simulations){
    return(Drawdown_Simulations[[2]])
  },
  
  Drawdown_Mean <- function(Drawdown_Paths, time = length(Drawdown_Paths[1, ])){
      return(mean(Drawdown_Paths[, time]))
  },

  Drawdown_Mean_Life_Ex <- function(Drawdown_Paths, freq, age = NULL, ex = NULL, lifetable = ILT15_female_reduced){
    freq = p_list[match(freq, freq_list_drawdown)]
    if(is.null(ex)){
      ex = round_to_fraction(exn(lifetable, age), freq)
    }
    time = round((freq * ex) + 1)
    return(Drawdown_Mean(Drawdown_Paths, time))
  },

  Drawdown_Percentile <- function(Drawdown_Paths, time = length(Drawdown_Paths[1, ]), percentile = 0.50){
    return(quantile(Drawdown_Paths[, time], probs = percentile))
  },

  Drawdown_Percentile_Life_Ex <- function(Drawdown_Paths, freq, age = NULL, ex = NULL, percentile = 0.50, lifetable = ILT15_female_reduced){
    freq = p_list[match(freq, freq_list_drawdown)]
    if(is.null(ex)){
      ex = round_to_fraction(exn(lifetable, age), freq)
    }
    time = round((freq * ex) + 1)
    return(Drawdown_Percentile(Drawdown_Paths, time, percentile))
  },

  Drawdown_Ruin_Prob <- function(Drawdown_Paths, time = length(Drawdown_Paths[1, ]), ruin_value = 0){
    return(length(which(Drawdown_Paths[, time] <= ruin_value)) / length(Drawdown_Paths[, time]))
  },

  Drawdown_Ruin_Life_Ex <- function(Drawdown_Paths, freq, age = NULL, ex = NULL, lifetable = ILT15_female_reduced, ruin_value = 0){
    freq = p_list[match(freq, freq_list_drawdown)]
    if(is.null(ex)){
      ex = round_to_fraction(exn(lifetable, age), freq)
    }
    time = round((freq * ex) + 1)
    return(Drawdown_Ruin_Prob(Drawdown_Paths, time, ruin_value))
  },

  Drawdown_Mean_Withdrawals <- function(Drawdown_Withdrawals, time = length(Drawdown_Withdrawals[1, ])){
    if(time == 1){
      return(mean(Drawdown_Withdrawals[, time]))
    } else {
      return(mean(rowSums(Drawdown_Withdrawals[, 1:time])))
    }
  },

  Drawdown_Mean_Withdrawals_Life_Ex <- function(Drawdown_Withdrawals, freq, age = NULL, ex = NULL, lifetable = ILT15_female_reduced){
    freq = p_list[match(freq, freq_list_drawdown)]
    if(is.null(ex)){
      ex = round_to_fraction(exn(lifetable, age), freq)
    }
    time = round((freq * ex) + 1)
    return(Drawdown_Mean_Withdrawals(Drawdown_Withdrawals, time))
  },

  Drawdown_Percentile_Withdrawals <- function(Drawdown_Withdrawals, time = length(Drawdown_Withdrawals[1, ]), percentile = 0.50){
    if(time == 1){
      return(quantile(Drawdown_Withdrawals[, time], probs = percentile))
    } else {
      return(quantile(rowSums(Drawdown_Withdrawals[, 1:time]), probs = percentile))
    }
  },

  Drawdown_Percentile_Withdrawals_Life_Ex <- function(Drawdown_Withdrawals, freq, percentile = 0.50, age = NULL, ex = NULL, lifetable = ILT15_female_reduced){
    freq = p_list[match(freq, freq_list_drawdown)]
    if(is.null(ex)){
      ex = round_to_fraction(exn(lifetable, age), freq)
    }
    time = round((freq * ex) + 1)
    return(Drawdown_Percentile_Withdrawals(Drawdown_Withdrawals, time, percentile))
  },

  Drawdown_DataFrame <- function(Drawdown_Paths, Drawdown_Withdrawals, freq, lower = 0.05, upper = 0.95, ruin_value = 0){
    freq = p_list[match(freq, freq_list_drawdown)]
  
    total_withdrawn = numeric(length(Drawdown_Withdrawals[1, ]))
    average = numeric(length(Drawdown_Paths[1, ]))
    prob_ruin = numeric(length(Drawdown_Paths[1, ]))
    lower_bound = numeric(length(Drawdown_Paths[1, ]))
    median = numeric(length(Drawdown_Paths[1, ]))
    upper_bound = numeric(length(Drawdown_Paths[1, ]))
    years = c(seq(0, ((length(Drawdown_Paths[1, ]) - 1) / freq), by = 1/freq))
  
    for(i in 1:length(Drawdown_Paths[1, ])){
      total_withdrawn[i] = Drawdown_Mean_Withdrawals(Drawdown_Withdrawals, i)
      average[i] = Drawdown_Mean(Drawdown_Paths, i)
      prob_ruin[i] = Drawdown_Ruin_Prob(Drawdown_Paths, i, ruin_value)
      lower_bound[i] = Drawdown_Percentile(Drawdown_Paths, i, percentile = lower)
      median[i] = Drawdown_Percentile(Drawdown_Paths, i)
      upper_bound[i] = Drawdown_Percentile(Drawdown_Paths, i, percentile = upper)
    }
    dataframe = data.frame(years, total_withdrawn, average, prob_ruin, lower_bound, median, upper_bound)
    return(dataframe)
  },

  Drawdown_Table <- function(Drawdown_Paths, Drawdown_Withdrawals, freq, series = NULL, points = NULL, colour = NULL, lower = 0.05, upper = 0.95, ruin_value = 0){
    dataframe = Drawdown_DataFrame(Drawdown_Paths = Drawdown_Paths, Drawdown_Withdrawals = Drawdown_Withdrawals, freq = freq, lower = lower, upper = upper, ruin_value = ruin_value)
    freq = p_list[match(freq, freq_list_drawdown)]

    if(!is.null(points)){
      points = unlist(points)
      series = list(sort(unique(c(unlist(series), ((freq * points)) + 1))))
      points = lapply(points, function(x) round(x, digits = 0))
    }
    if(!is.null(series)){
      dataframe = dataframe[unlist(series), ]
    }
    dataframe$years = lapply(dataframe$years, round, digits = 0)
    
    colnames(dataframe) = c("Years", "Mean Withdrawals", "Mean Fund Value", "Probability of Ruin", paste(toOrdinal(100*lower), 'Percentile'), "Median", paste(toOrdinal(100*upper), 'Percentile'))
    table <- datatable(dataframe, options = list(paging = FALSE, searching = FALSE, info = FALSE, columnDefs = list(list(className = 'dt-center', targets = "_all"))), rownames = FALSE)
    table <- formatCurrency(table, columns = c("Mean Withdrawals", "Mean Fund Value", paste(toOrdinal(100*lower), 'Percentile'), "Median", paste(toOrdinal(100*upper), 'Percentile')), currency = "KES ")
    table <- formatPercentage(table, columns = "Probability of Ruin", digits = 0)
    table <- formatStyle(table, c(1, 4), `border-right` = "solid 1px")
    if(!is.null(points) && !is.null(colour)){
      for(i in 1:length(points)){
        table <- formatStyle(table, "Years", target = 'row', backgroundColor = styleEqual(points[i], colour[i]))
      }
    }
    return(table)
  },

  Drawdown_Plot_Sims <- function(Drawdown_Paths, freq, n_sims = 25, points = NULL, colour = NULL){
    freq = p_list[match(freq, freq_list_drawdown)]
    dat <- vector("list", n_sims)
    p <- ggplot()
    for (i in seq(n_sims)){
      dat[[i]] <- data.frame(years = c(seq(0, ((length(Drawdown_Paths[1, ]) - 1) / freq), by = 1/freq)), capital = Drawdown_Paths[i, ])
      p <- p + geom_line(data = dat[[i]], mapping = aes(x = years, y = capital), col = i)
    }
    if (!is.null(points) && !is.null(colour)){
      points = unlist(points)
      for (i in 1:length(points)){
        p <- p + geom_vline(xintercept = points[i], color = colour[i], linetype = 'longdash', width = 3)
      }
    }
    p <- p + 
      labs(x = "Years", y = "Fund Value") +
      scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(labels = scales::dollar_format(prefix = "KES "), expand = c(0, 0)) +
      theme(legend.position = "none", 
            axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10),
            axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
            axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            # panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_line(colour = "grey"),
            panel.background = element_blank(), axis.line = element_line(colour = "black"))
    return(p)
  },

  Drawdown_Plot_Percentile <- function(Drawdown_Paths, Drawdown_Withdrawals, freq, lower = 0.05, upper = 0.95, ruin_value = 0, points = NULL, colour = NULL){
    dataframe = Drawdown_DataFrame(Drawdown_Paths = Drawdown_Paths, Drawdown_Withdrawals = Drawdown_Withdrawals, freq = freq, lower = lower, upper = upper, ruin_value = ruin_value)
    freq = p_list[match(freq, freq_list_drawdown)]
    fig <- plot_ly(dataframe, x = ~round(dataframe$years, 2), y = ~round(dataframe$median), name = "Median", type = "scatter", mode = "lines", line = list(color = '#00A7FA', width = 4))
    fig <- fig %>% add_trace(y = ~round(dataframe$lower_bound), name = paste(toOrdinal(100*lower), 'Percentile'), line = list(color = '#FF3B28', width = 4))
    fig <- fig %>% add_trace(y = ~round(dataframe$upper_bound), name = paste(toOrdinal(100*upper), 'Percentile'), line = list(color = '#48EB12', width = 4))
    if (!is.null(points) && !is.null(colour)){
      points = unlist(points)
      colour = unlist(colour)
      vline_list = list()
      for (i in 1:length(points)){
        vline_list[[i]] = plotly_vline(x = points[i], colour = colour[i])
      }
      fig <- fig %>% layout(shapes = vline_list)
    }
    fig <- fig %>% layout(xaxis = list(title = "Years"), yaxis = list(title = list(text = "Fund Value", standoff = 15), tickprefix = 'KES ', tickformat = ",.", ticklabelposition = "inside"))  
    fig <- fig %>% layout(legend = list(orientation = "h", xanchor = "center", x = 0.5, y = 1.05))
    fig <- fig %>% layout(hovermode = "x unified")
  },


  riskprofilerui <- function(session, surveydisplay, submit, page, input_mean_return, input_ret_std_dev, mainui, means = c(2.5, 3, 3.5), std_devs = c(2, 4, 7)){
    if(surveydisplay %% 2 == 1 && surveydisplay != 0){
      if(submit %% (num.quest + 2) == 0){
        return(list(
          column(12,
                 img(src='stockmarket.jpeg', height="300px", width="100%")
          ),
          br(),
          br(),
          box(status = "primary", width = 12, solidHeader = F,
              h5("The following questions are intended to assess your capacity for loss and help you to choose \n
                      a suitable portfolio and level of risk."),
              h5("Click next to begin the survey."))
        ))
      } else if ((submit %% (num.quest + 2) > 0) && (submit %% (num.quest + 2) <= num.quest)){
        return(list(
          box(status = "warning", width = 12, solidHeader = T,
              h5("Disclaimer: This risk profiler tool is for illustrative purposes only.
                     It is designed to assist you in understanding your attitude towards risk.")
          ),
          box(status = "primary", width = 12, solidHeader = T,
              h5(paste0(
                "Q", submit %% (num.quest + 2), ": ",
                Qlist[submit %% (num.quest + 2), 2]
              )),
              radioButtons(paste0(page,"_survey"), "Please Select:", myLists[[submit %% (num.quest + 2)]])
          )
        ))
      } else {
        return(list(
          box(status = "warning", width = 12, solidHeader = T,
              h5("Disclaimer: This risk profiler tool is for illustrative purposes only.
                     It is designed to assist you in understanding your attitude towards risk.")
          ),
          box(status = "primary", width = 12, solidHeader = T,
              h5(portfolio(session, input_mean_return, input_ret_std_dev, means, std_devs)),
              hr(),
              h4("Suitable changes have been applied to the mean return and standard deviation of returns."),
              h4("These changes are intended to reflect your risk appetite, but may be changed at any time."),
              hr(),
              h4("Click the risk profiler button (in the sidebar) to return to the summary page."),
              h4("Click next to repeat the survey.")
          )
        ))
      }
    } else {
      return(mainui)
    }
  },

    portfolio_update <- function(session, input_mean_return, new_mean_return, input_ret_std_dev, new_ret_std_dev){
    updateNumericInputIcon(session, input_mean_return, value = new_mean_return)
    updateNumericInputIcon(session, input_ret_std_dev, value = new_ret_std_dev)
  },

  portfolio <- function(session, input_mean_return, input_ret_std_dev, means = c(2.5, 3, 3.5), std_devs = c(2, 4, 7)){
    if(is.character(results)){
      results <<- parse_vector(results, col_integer()) 
    }
    x = mean(results)
    if(x < 7/3){
      portfolio_update(session, input_mean_return, means[1], input_ret_std_dev, std_devs[1])
      h1("Low Risk Appetite")
    } else if (x <= 11/3){
      portfolio_update(session, input_mean_return, means[2], input_ret_std_dev, std_devs[2])
      h1("Moderate Risk Appetite")
    } else{
      portfolio_update(session, input_mean_return, means[3], input_ret_std_dev, std_devs[3])
      h1("High Risk Appetite")
    }
  },



# Plotly - Horizontal & Vertical Lines ------------------------------------
plotly_hline <- function(y = 0, colour = "blue") {
  list(type = "line", x0 = 0, x1 = 1, xref = "paper", y0 = y, y1 = y, line = list(color = colour, width = 3, dash = 'dash'))
},

plotly_vline <- function(x = 0, colour = "red") {
  list(type = "line", y0 = 0, y1 = 1, yref = "paper", x0 = x, x1 = x, line = list(color = colour, width = 3, dash = 'dash'))
},

# Risk Profiler Functions -------------------------------------------------
  save_results <- function(session, submit, survey){
    if(submit %% (num.quest + 2) > 0 && (submit %% (num.quest + 2) <= num.quest)){
      try(results[submit %% (num.quest + 2)] <<- survey, silent = T)
    }
    return("")
  }

  )

