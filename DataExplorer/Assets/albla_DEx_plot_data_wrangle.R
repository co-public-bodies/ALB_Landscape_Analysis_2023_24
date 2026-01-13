#' Pre-process Data for ALB Bar Plots
#'
#' @description
#' Prepares raw data for the plotting function by calculating label formatting and positioning, which is passed to the barplot function. 
#' Key features include:
#' * **Dynamic Label Positioning:** Calculates `label_pos` based on screen width (`current_width_inches`). It applies a "pull-back" logic to ensure labels inside bars don't spill over on small screens.
#' * **Rich Text Formatting:** Generates HTML strings for labels (bolding, currency symbols, caveats).
#' * **Caveat Handling:** Replaces missing or caveat-flagged data points with explanatory text (e.g., "-- Data not available --").
#'
#' @param input_dataset A data frame. The raw filtered dataset from the Shiny dashboard inputs.
#' @param input_numeric_column_x String. The variable being plotted.
#' @param input_unique_financial_years_count Integer. Number of years selected. If >1, years are appended to label text for clarity.
#' @param input_unique_albs_count Integer. Total count of ALBs (used for scaling logic).
#' @param input_highlighted_albs Vector. Names of ALBs selected for highlighting. Affects `label_color` logic (dims non-selected labels).
#' @param input_columns_with_caveats_22_23 Vector. List of column names that have specific data quality caveats for the 22/23 financial year.
#' @param input_columns_with_caveats_23_24 Vector. List of column names that have specific data quality caveats for the 23/24 financial year.
#' @param current_width_inches Numeric. The real-time screen width. Critical for determining the `base_pullback` buffer to keep labels legible on mobile devices.
#'
#' @return A processed data frame with new columns: `Label` (HTML string), `label_pos` (numeric x-coordinate), `label_color`, and `label_size`.
#' @import dplyr
#' @export

albla_DEx_plot_data_wrangle <- function(input_dataset, 
                                        input_numeric_column_x, 
                                        input_unique_financial_years_count, 
                                        input_unique_albs_count, 
                                        input_highlighted_albs,
                                        input_columns_with_caveats_22_23, 
                                        input_columns_with_caveats_23_24, 
                                        current_width_inches = 10
                                        ){
    
    # make sure the width of the plot within user screen is numeric
    width_val <- as.numeric(current_width_inches)
    # for debugging plot sizes for responsiveness
    # print(paste("Wrangling Width:", width_val)) 
    
    # define label position buffers
    # We calculate this once, outside the dataframe pipe.
    # This avoids any vector-recycling issues inside case_when(). 
    # This tells the label where to position itself within the largest bar(s), whilst factoring in the user's screen size. 
    if (width_val < 4) {
        base_pullback <- 0.75
    } else if (width_val >= 4 & width_val < 5) {
        base_pullback <- 0.65
    } else if (width_val >= 5 & width_val < 6) {
        base_pullback <- 0.55
    } else if (width_val >= 6 & width_val < 7) {
        base_pullback <- 0.45
    } else if (width_val >= 7 & width_val < 9) {
        base_pullback <- 0.25
    } else {
        base_pullback <- 0.15
    }
    
    # for debug
    # print(paste("Calculated Pullback:", base_pullback))
    
    
    # For labels OUTSIDE the bar (<60% of max range):
    # On mobile, space is tight. We reduce the gap slightly to keep text closer to the bar end.
    outside_buffer_mult <- case_when(
        current_width_inches <= 6 ~ 0.02,  # Mobile: Tiny gap (2%)
        .default = 0.05                    # Desktop: Standard gap (5%)
    )
    
    # wrangle plot data to make specific adjustments required prior to plotting
    temp_data <- input_dataset |>
        # chair pay data was not in numeric format for 2022/23
        mutate(`Chair Pay` = as.numeric(case_when(
            .default = `Chair Pay`,
            `Financial Year` == "2022/23" ~ NA
        ))) |>
        arrange(get(input_numeric_column_x)) |>
        # turn ALB names into factor, descending by numeric variable chosen
        mutate(`Plot Name` = factor(`Plot Name`, levels = unique(`Plot Name`))) |>
        ungroup() |> 
        # calculate how much the bar represents as a percent of the total size
        mutate(pct_of_total = get(input_numeric_column_x) / max(get(input_numeric_column_x), na.rm = TRUE) * 100) |>
        # generate bar labels based on size of screen and data selected
        # flag for data where data is not collected
        mutate(
            Label = case_when(
                .default = paste0("<b>£", format_bignum(get(
                    input_numeric_column_x
                )), "</b>"),
                input_numeric_column_x %in% input_columns_with_caveats_22_23 & 
                    `Financial Year` == "2022/23" ~
                    "-- Data not available for financial year 2022/23 --", 
                input_numeric_column_x %in% input_columns_with_caveats_23_24 & 
                    `Financial Year` == "2023/24" ~
                    "-- Data not available for financial year 2023/24 --", 
                is.na(get(input_numeric_column_x)) ~ "-- No Data Provided Yet --",
                grepl(pattern = "FTE |Staff ", x = input_numeric_column_x) ~ paste0("<b>", comma(round(
                    get(input_numeric_column_x)
                )), " FTE</b>"),
                
                get(input_numeric_column_x) == 0 ~ "<b>£0</b>"
            )
        ) |>
        # if showing multiple years of data, append year of data for clarity
        mutate(Label = case_when(
            .default = Label,
            input_unique_financial_years_count > 1 ~ paste0(Label, 
                                                            " (", `Financial Year`, ")")
        )) |> 
        ## calculate position of label based on data selected and size of bars
        mutate(
            label_pos = case_when(
                .default = get(input_numeric_column_x),
                # show NAs in centre
                is.na(get(input_numeric_column_x)) ~ max(get(input_numeric_column_x), na.rm = TRUE) / 2,
                
                # --- OUTSIDE LABELS (Using outside_buffer_mult) ---
                # labels for small bars go outside
                get(input_numeric_column_x) != 0 &
                    pct_of_total <= 60 ~ get(input_numeric_column_x) + 
                    (outside_buffer_mult * max(get(input_numeric_column_x), na.rm = TRUE)),
                
                # --- INSIDE LABELS ---
                # labels for biggest bar(s) go inside
                get(input_numeric_column_x) != 0 & pct_of_total > 60 ~ {
                    
                    # Calculate the raw position first
                    raw_pos <- get(input_numeric_column_x)
                    max_val <- max(get(input_numeric_column_x), na.rm = TRUE)
                    
                    # Dynamic adjustment: 
                    # If the bar is "medium" sized (60-80% of total), we need MORE pullback 
                    # because there's less space to the right of the text.
                    # If the bar is "huge" (90-100%), we need LESS pullback.
                    # This helps the label float towards the top end of the bars
                    
                    pct_factor <- get(input_numeric_column_x) / max_val
                    
                    # Formula: 
                    # Start with max value * base_pullback.
                    # Divide by pct_factor to penalize smaller bars more heavily.
                    # (e.g., a 60% bar gets pulled back more than a 100% bar)
                    
                    buffer <- (max_val * base_pullback) / (pct_factor ^ 0.5) 
                    
                    raw_pos - buffer
                },
                
                # --- ZERO HANDLING ---
                # treat 0s similar to NAs by positioning in the middle
                get(input_numeric_column_x) == 0 &
                    max(get(input_numeric_column_x), na.rm = TRUE) > 0 ~ (max(get(input_numeric_column_x), na.rm = TRUE) / 2)
            )
        ) |>
        ungroup() |>
        # color of labels is based on the position of the label - 
        # labels inside the bar are white, labels outside the bar are black, for contrast
        mutate(label_color = case_when(
            .default = "black",
            get(input_numeric_column_x) != 0 &
                pct_of_total > 60 ~ "white", 
        )) |>
        # further color modifications based on whether or not the bars are highlighted by users
        mutate(label_color = case_when(
            .default = label_color,
            length(input_highlighted_albs) > 0 & !`Plot Name` %in% input_highlighted_albs  ~ "black", 
        )) |>
        # label sizing varies based on whether there is multiple years of data shown (more years = more data = less space)
        mutate(label_size = case_when(
            .default = 10, 
            input_unique_financial_years_count == 2 ~ 8
        ))
    
    return(temp_data)
}