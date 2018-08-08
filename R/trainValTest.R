#' Train, Validation and Test datasets
#'
#' Split a dataset randomly in train and test datasets or train, validation and test datasets.
#'
#' @param df data.frame containing the data that have to be splitted in train, validation and test datasets.
#' @param test numeric. The percentage of data withhold for testing.
#' @param val numeric. The percentage of data withhold for validation, default is 0.
#' @param seed numeric. The value used to set the seed in order to have consistent results, default is NULL.
#'
#' @return A list with the train, validation and test data sets or train and test sets accordingly.
#' @export
#' @import zeallot
#'
#' @examples
#' \dontrun{
#' sets <- trainValTest(presence, val = 0.3, test = 0.1, set_seed = 25)}
#'
#' @author Sergio Vignali
trainValTest <- function(df, test, val = 0, seed = NULL) {

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
