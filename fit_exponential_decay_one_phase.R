#' Function to fit one-phase exponential decay function to data
#'
#' @param data Dataframe should have at least columns named 'y', 'x'
#'
#' @return a list of model, equation, table_data(parameter fits)
#' @export
#'
#' @examples
fit_exponential_decay_one_phase <- function(data) {
  # Try fitting one-phase exponential decay function using nls_multstart()
  tryCatch({
    lower_bounds <- c(span = 0, k = 0.001, plateau = 0)
    upper_bounds <- c(span = 30, k = 0.01, plateau = 30)
    lower <- c(span = 0, k = 0.001, plateau = 0)
    
    # Define the model using nls_multstart
    model <- nls_multstart(
      formula = y ~ span * exp(-k * x) + plateau,
      data = data,
      iter = 100,
      start_lower = lower_bounds,
      start_upper = upper_bounds,
      supp_errors = 'Y',
      lower = lower
    )
    
    # Extract coefficients from model
    span <- coef(model)["span"]
    k <- coef(model)["k"]
    plateau <- coef(model)["plateau"]
    
    # Calculate sum of squared residuals (SSR)
    ssr <- sum(resid(model)^2)
    
    # Create equation of exponential decay function
    equation <- paste0("y = ", round(span, 3),"*exp(-",round(k, 3),"*x)+" ,round(plateau, 3))
    
    # Create table of coefficients and SSR
    table_data <- data.frame(
      term = c("span", "k", "plateau", "SSR"),
      # Format numeric columns as character
      value = format(c(span, k, plateau, ssr), digits = 3),
      stringsAsFactors = FALSE
    )
    
    # Remove the row names
    rownames(table_data) <- NULL
    
    # Return results as a list
    list(model = model, equation = equation, table_data = table_data)
  }, error = function(e) {
    # Return an empty list if fitting fails
    list(model = NULL, equation = "Fitting failed", table_data = data.frame(term = character(), value = character()))
  })
}