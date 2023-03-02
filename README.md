# oxygenTransientFit


This app is a web-based tool that allows researchers to analyze leaf photosynthetic responses transitioning from low to high oxygen. The app allows you to upload a CSV file and explore fitting exponential decay models to your data. You can adjust the range of time values to fit, and select the number of phases for the exponential decay model. The app is built on the R programming language and the Shiny web framework.

The app is hosted at https://flux.shinyapps.io/oxygenTransientFit with 25 active hours per month.

## Getting Started

### Prerequisites

To run the app locally, you need to have R (version 4.0.0 or higher) and RStudio installed on your computer. You also need to install the following packages:
- Shiny - The web framework used
- tidyverse - A collection of R packages used for data wrangling and visualization
- shinythemes - A package used for customizing the appearance of the app
- DT - A package used for generating data tables
- nls.multstart - A package used for Non-Linear Regression using multiple starting values

You can install these packages by running the following command in R:

```
install.packages(c("shiny", "tidyverse", "shinythemes", "DT", "nls.multstart"))
```

### Installing

To install the app, you can download the code from the GitHub repository:

```
git clone https://github.com/codexf/oxygenTransientFit.git

```

### Running the app

To run the app, open the `app.R` file in RStudio and click the "Run App" button in the top right corner of the script editor window. This will launch the app in a new window.

Alternatively, you can run the app from the command line by navigating to the directory containing the `app.R` file and running the following command:

```
shiny::runApp('app.R')
```


## Using the app

### Upload Data tab

- In the "Upload Data" tab, you can upload your CSV file and select the columns containing the time and net assimilation rate data. You can also view a table of the selected data and a plot of the original data.

- If you do not have your own data to upload, you can use the provided demo data by clicking on the "Load Demo Data" checkbox.

- After checking your selected data, go to the "Fitting Exponential Models to Data" tab. 

### Fitting Exponential Models to Data tab

- You can select a range of time values by adjusting the "Start" and "End" sliders. If the selected range looks good, click on the "Subset and Reset X-Axis to Zero" button to reset the x-axis offset of the data to zero. 

- You can fit a one-phase or two-phase exponential decay model to the subsetted data by clicking on the "Fit One-Phase" or "Fit Two-Phase" button. If the fitting fails, try selecting another range of the data and click the "Subset and Reset X-Axis to Zero" button again and redo the fit.

- If the fitting is successful, the plot is updated with the fitted curve and equation, and a table of fit parameters and SSR is displayed.

## Modifying the app

If the fitting fails, you can adjust the lower_bounds and upper_bounds parameters in the functions `fit_exponential_decay_one_phase.R` and `fit_exponential_decay_two_phase.R`. These bounds define the lower and upper limits of the values that the optimizer can search for the model parameters. By adjusting these bounds,  you can explore a wider range of potential parameter values and increase the likelihood of finding a good fit for their data. 

## Credits

The App was developed by Xinyu Fu under the supervision of Berkley Walker at Michigan State University.