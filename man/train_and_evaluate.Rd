% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model.R
\name{train_and_evaluate}
\alias{train_and_evaluate}
\title{Train and Evaluate Models}
\usage{
train_and_evaluate(df, dep_var, model_type)
}
\arguments{
\item{df}{A data frame to use for training.}

\item{dep_var}{The dependent variable.}

\item{model_type}{The type of model ("regression" or "classification").}
}
\value{
A list containing models and evaluation metrics.
}
\description{
This function trains and evaluates multiple models based on the given dependent variable.
}
\examples{
train_and_evaluate(mtcars, "mpg", "regression")
}
