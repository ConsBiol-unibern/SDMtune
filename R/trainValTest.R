#' Train, Validation and Test datasets
#'
#' Split a dataset randomly in train, validation and test datasets. If val parameter is set to zero then
#' the function returns only the train and the test datasets.
#'
#' @param df The data frame that has to be splitted in train, validation and test datasets.
#' @param val The percentage of data withhold for validation.
#' @param test The percentage of data withhold for testing.
#' @param seed The value used to set the seed in order to have consistent results, default is NULL.
#'
#' @return A list with the train, validation and test data sets or train and test sets accordingly.
#'
#' @examples
#' \dontrun{
#' sets <- trainValTest(presence, val = 0.3, test = 0.1, set_seed = 25)}
#'
#' @author Sergio Vignali
trainValTest <- function(df, val, test, seed = NULL) {

  if (!is.null(seed)) set.seed(seed)

  permutation <- sample(nrow(df))

  if (val > 0) {
    start_val <- round(nrow(df) * (1 - val - test), 0) + 1
    start_test <- round(nrow(df) * val, 0) + start_val
    train <- df[permutation[1:(start_val - 1)], ]
    val <- df[permutation[start_val:(start_test - 1)], ]
    test <- df[permutation[start_test:nrow(df)], ]

    return(list(train, val, test))

  } else {
    start_test <- round(nrow(df) * (1 - test), 0) + 1
    train <- df[permutation[1:(start_test - 1)], ]
    test <- df[permutation[start_test:nrow(df)], ]

    return(list(train, test))
  }
}
