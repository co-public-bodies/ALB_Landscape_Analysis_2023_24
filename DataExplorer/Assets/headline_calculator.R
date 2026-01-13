
#' Calculate and Format Headline Totals
#'
#' Calculates a summed total for a specific column within a dataset, filtered
#' for the most recent financial year. The result is then formatted with a
#' prefix (e.g., currency symbol) and comma separators for display in UI bslib boxes.
#'
#' @param input_dataset A data frame or tibble. Must contain a column named
#'   `Financial Year` and the numeric column specified in `input_column_name`.
#' @param input_column_name A character string. The name of the numeric column
#'   to be summed (e.g., "Total FTE Employed on 31st March").
#' @param prefix A character string. The symbol to prepend to the final output.
#'   Defaults to "£". Set to "" if no prefix is required.
#' @param round_digits An integer or NULL. The number of decimal places to
#'   round the result to using `janitor::round_half_up`. If NULL (default),
#'   no rounding is performed before formatting.
#'
#' @return A character string representing the formatted total (e.g., "£1,500,000").
#'
#' @importFrom dplyr filter pull
#' @importFrom janitor round_half_up
#' @importFrom scales comma
#'
#' @export


headline_calculator <- function(input_dataset, input_column_name, 
                                prefix = "£", round_digits = NULL) {
    
    # get the summed value for the variable to generate headline totals         
    temp_value <- input_dataset |>
        dplyr::filter(`Financial Year` == max(`Financial Year`)) |>
        dplyr::pull(.data[[input_column_name]]) |> 
        sum(na.rm = TRUE)
    
    # apply rounding for headline if not NULL
    if (!is.null(round_digits)){
        temp_value <- janitor::round_half_up(temp_value, digits = round_digits)
    } 
    
    # apply formatting
    temp_value <- paste0(prefix, scales::comma(temp_value))
    
    return(temp_value)
}