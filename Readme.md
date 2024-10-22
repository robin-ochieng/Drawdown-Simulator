# Drawdown Simulator

## Overview

The Drawdown Simulator is a Shiny application designed to help individuals make informed decisions about whether to buy an annuity or draw down their pension at retirement. In Ireland, this process is known as an Approved Retirement Fund (ARF) [34]. 

## What is Drawdown?

Drawdown involves keeping one’s pension savings invested with their pension provider upon reaching retirement. Each year, the retiree decides how much money to withdraw from their pension pot to live on. The remaining pension fund stays invested, typically in the stock market.

## Features

- **Interactive Simulations**: Visualize different drawdown scenarios and their potential outcomes.
- **Data-Driven Insights**: Utilize real-world data to make informed decisions.
- **User-Friendly Interface**: Easy to navigate and understand, even for those with limited financial knowledge.

## Getting Started

To run the Drawdown Simulator, you will need to have R and the Shiny package installed. Follow these steps to get started:

1. **Install R**: Download and install R from [CRAN](https://cran.r-project.org/).
2. **Install Shiny**: Open R and run the following command to install Shiny:
    ```R
    install.packages("shiny")
    ```
3. **Run the App**: Open `app.R` or `app2.R` in RStudio or your preferred R environment and click "Run App".

## File Structure

- `app.R`, `app2.R`: Main application files.
- `data/`: Contains data files such as `ILT15.xlsx` and `Qlist.csv`.
- `modules/`: Contains server and UI modules for the drawdown simulator.
- `www/`: Contains static assets like custom styles and images.

## Contributing

We welcome contributions! 

## License

This project is licensed.

## Acknowledgements

Special thanks to all contributors and the open-source community for their invaluable support.


