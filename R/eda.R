#' Perform Exploratory Data Analysis
#'
#' This function performs exploratory data analysis on the given dataframe.
#' It includes summarizing data types, basic statistics, and missing values.
#' @param df A data frame to analyze.
#' @param plots A character vector of plot types to create (e.g., "histogram", "boxplot").
#' @param dep_var Dependent variable for analysis.
#' @return A list containing summary statistics, str output, missing values, and plots.
#' @examples
#' eda_analysis(mtcars, c("histogram", "boxplot"), "mpg")
#' @export
eda_analysis <- function(df, plots = c("histogram", "boxplot"), dep_var = NULL) {
  library(ggplot2)
  library(dplyr)

  # Convert character columns to factors
  df <- df %>% mutate_if(is.character, as.factor)

  # Convert factors to have valid names
  df <- df %>% mutate_if(is.factor, ~factor(.x, levels = make.names(levels(.x))))

  # Summary of data types and basic statistics
  data_summary <- summary(df)

  # Capture the output of the str() function
  data_str <- capture.output(str(df))

  # Missing values
  missing_values <- sapply(df, function(x) sum(is.na(x)))

  # Plot generation
  plot_list <- list()

  if ("histogram" %in% plots) {
    for (col in names(df)) {
      if (is.numeric(df[[col]])) {
        p <- ggplot(df, aes_string(x = col)) +
          geom_histogram(bins = 30, fill = "blue", color = "white") +
          ggtitle(paste("Histogram of", col))
        plot_list[[paste("histogram", col)]] <- p
      }
    }
  }

  if ("boxplot" %in% plots && !is.null(dep_var)) {
    if (dep_var %in% names(df) && is.numeric(df[[dep_var]])) {
      p <- ggplot(df, aes_string(x = "1", y = dep_var, group = "1")) +
        geom_boxplot(fill = "red", color = "black") +
        ggtitle(paste("Boxplot of", dep_var))
      plot_list[["boxplot"]] <- p
    } else {
      warning(paste("No numeric variable found for", dep_var, "or invalid dep_var"))
    }
  }

  return(list(summary = data_summary, str = data_str, missing = missing_values, plots = plot_list))
}
