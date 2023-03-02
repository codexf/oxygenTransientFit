##' Function to fit two-phase exponential decay function to data
#'
#' @param data Dataframe should have at least columns named 'y', 'x'
#'
#' @return a list of model, equation, table_data(parameter fits)
#' @export
#'
#' @examples
fit_exponential_decay_two_phase <- function(data) {
  # Set bounds and starting values for model parameters
  # Random start parameter values are picked from a uniform distribution between 
  # start_lower() and start_upper() for each parameter. 
  start_lower <- c(alpha = 0, beta = 0.001, gamma = 0, delta = 0.001, theta = 0)
  start_upper <- c(alpha = 30, beta = 0.01, gamma = 30, delta = 0.01, theta = 30)
  lower <- c(alpha = 0, beta = 0.001, gamma = 0, delta = 0.001, theta = 0)
  
  # Fit the model with nls_multstart and handle errors using tryCatch
  tryCatch({
    model <- nls_multstart(y ~ alpha * exp(-beta * x) + gamma * exp(-delta * x) + theta,
                           data = data,
                           iter = 200,
                           start_lower = start_lower,
                           start_upper = start_upper,
                           lower = lower,
                           supp_errors = 'Y')
    
    
    # Determine the term with a larger decay constant and assign values to corresponding variables
    coef_vals <- coef(model)
    if (coef_vals['beta'] > coef_vals['delta']) {
      span_fast <- coef_vals['alpha']
      k_fast <- coef_vals['beta']
      span_slow <- coef_vals['gamma']
      k_slow <- coef_vals['delta']
    } else {
      span_fast <- coef_vals['gamma']
      k_fast <- coef_vals['delta']
      span_slow <- coef_vals['alpha']
      k_slow <- coef_vals['beta']
    }
    plateau <- coef_vals['theta']
    
    # Calculate sum of squared residuals (SSR)
    ssr <- sum(resid(model)^2)
    
    # Create equation of two-phase exponential decay function
    equation <- paste0("y = ", round(span_fast, 3), "*exp(-", round(k_fast, 3), "*x) + ",
                       round(span_slow, 3), "*exp(-", round(k_slow, 3), "*x) + ", round(plateau, 3))
    
    # Create table of coefficients and SSR
    table_data <- data.frame(
      term = c("span_fast", "k_fast", "span_slow", "k_slow", "plateau", "SSR"),
      value = format(c(span_fast, k_fast, span_slow, k_slow, plateau, ssr), digits = 3),
      stringsAsFactors = FALSE
    )
    
    # Remove the row names
    rownames(table_data) <- NULL
    
    # print(table_data)
    
    # Return results as a list
    list(model = model, equation = equation, table_data = table_data)
  }, error = function(e) {
    # Return an empty list if fitting fails
    message('The nonlinear fitting using nls.multstart failed')
    list(model = NULL, equation = "Fitting failed", table_data = data.frame(term = character(), value = character()))
  })
}