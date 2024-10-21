  list(

  riskprofilerui <- function(session, surveydisplay, submit, page, input_mean_return, input_ret_std_dev, mainui, means = c(2.5, 3, 3.5), std_devs = c(2, 4, 7)){
    if(surveydisplay %% 2 == 1 && surveydisplay != 0){
      if(submit %% (num.quest + 2) == 0){
        return(list(
          column(12,
                 img(src='stockmarket.jpeg', height="200px", width="100%")
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
  }






  )

