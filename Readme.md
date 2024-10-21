# SORP Calculator Shiny App

This Shiny application provides a user-friendly interface to calculate SORP repayments and visualize SORP schedules. The app includes features to display periodic repayment amounts, SORP balance over time, and the proportions of interest vs. capital in each repayment.

## Project Structure

### Key Files and Directories

- **app.R**: The main entry point for the Shiny application.
- **modules/**: Contains modularized server and UI components.
  - **functions.R**: Utility functions used across the application.
  - **SORPCalculatorServer.R**: Server-side logic for the SORP calculator.
  - **SORPCalculatorUI.R**: UI components for the SORP calculator.
- **rsconnect/**: Configuration files for deploying the app to ShinyApps.io.
- **www/**: Static assets such as custom CSS, favicon, and images.

### Features

- **Periodic Repayment Amount**: Displays the periodic repayment amount calculated based on the SORP parameters.
- **SORP Schedule**: Shows a detailed SORP schedule in a data table format.
- **SORP Balance Over Time**: Visualizes the outstanding SORP balance over time.
- **Interest vs Capital Proportions**: Plots the proportions of interest and capital in each repayment.

### How to Run

1. Ensure you have R and Shiny installed on your system.
2. Clone this repository.
3. Open `app.R` in RStudio or your preferred R environment.
4. Run the application by clicking the "Run App" button or using the command:
   ```r
   shiny::runApp()

# Deployment
- To deploy this application to ShinyApps.io, follow these steps:
1. Install the rsconnect package if you haven't already:

```r
install.packages("rsconnect")

2. Configure your account using:

```r 
rsconnect::setAccountInfo(name='<ACCOUNT_NAME>', token='<TOKEN>',secret='<SECRET>')
 

3. Deploy the app:
```r
rsconnect::deployApp()

