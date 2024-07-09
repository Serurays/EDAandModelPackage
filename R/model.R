#' Train and Evaluate Models
#'
#' This function trains and evaluates multiple models based on the given dependent variable.
#' @param df A data frame to use for training.
#' @param dep_var The dependent variable.
#' @param model_type The type of model ("regression" or "classification").
#' @return A list containing models and evaluation metrics.
#' @examples
#' train_and_evaluate(mtcars, "mpg", "regression")
#' @export
train_and_evaluate <- function(df, dep_var, model_type) {
  library(caret)
  set.seed(123)
  results <- list()

  if (model_type == "regression") {
    models <- c("lm", "rf", "ridge", "lasso")
    train_control <- trainControl(method = "cv", number = 10)

    for (model in models) {
      set.seed(123)
      fit <- train(as.formula(paste(dep_var, "~ .")), data = df, method = model, trControl = train_control)
      results[[model]] <- list(model = fit, metrics = fit$results)
    }
  } else if (model_type == "classification") {
    # Ensure dep_var remains unchanged as factor
    df[[dep_var]] <- as.factor(df[[dep_var]])

    models <- c("rf", "glm", "knn", "svmRadial")
    train_control <- trainControl(method = "cv", number = 10, classProbs = TRUE, summaryFunction = twoClassSummary)

    for (model in models) {
      set.seed(123)
      fit <- train(as.formula(paste(dep_var, "~ .")), data = df, method = model, trControl = train_control, metric = "ROC")
      results[[model]] <- list(model = fit, confusion_matrix = confusionMatrix(predict(fit, df), df[[dep_var]]))
    }
  }

  return(results)
}
