## Landscape Analysis Table Formatting

# Purpose: For applying consistent formatting for tables in 
# ALB Landscape analysis publication. 

#' @param input_dataset A dataset with ALBs/Departments and their values.
#' @param heatmap_column Name of column to add a heatmap color palette over the values
#' @param enable_search Option to include search bar at top of tables - only used for big tables

# GT version - has minor issues with header color formatting not 
# behaving itself, so we have to keep it as white for the time being
# there's also currently no way to remove pagination details, even when there 
# is no pages required in the table. Otherwise, this is the preferred format

landscape_output_table <- function(input_dataset,
                                   heatmap_column = FALSE,
                                   enable_search = FALSE){

    table_dims <- dim(input_dataset)[1]
    
    if (table_dims > 6){ 
        
        table_height <- 600
        
    }
    else {
        table_height <- table_dims * 110
    }
    
    # table setup
    table_output <- input_dataset |>
        mutate(across(any_of(c("ALB", "Financial Year",
                               "Department", 
                               "Classification", 
                               "Category",
                               "Purpose")), \(x) as.character(x))) |> 
        gt()|>
        cols_label_with(
            fn = function(x) {
                gt::html(paste0("<span style ='font-weight:bold'>", x, "</span>"))
            }
        ) |>
        opt_interactive(use_compact_mode = FALSE,
                        active = TRUE,
                        use_search = enable_search, 
                        use_pagination = FALSE,
                        use_pagination_info = FALSE,
                        use_page_size_select = FALSE,
                        pagination_type = 'simple', 
                        use_highlight = TRUE) |> # make sortable
        # tab_style(style = list(cell_text(color = '#00344A', weight = "bold")), # colors - currently don't work
                  # locations = cells_column_labels(everything())) |>
        tab_style(
            style = cell_borders(
                sides = c("top", "bottom"),
                color = "grey90",
                weight = px(0.05),
                style = "solid"
            ),
            locations = cells_body()
        ) |>
        fmt_number(columns = c(contains("FTE ", ignore.case = FALSE)),# format FTE columns
                   suffixing = FALSE, decimals = 1, use_seps = TRUE, sep_mark = ",") |>
        fmt_currency( # format Spend/Income columns
            columns = c(contains("Spend"),
                        contains("Income"),
                        contains("Funding"),
                        contains("Budget"),
                        contains("TME"),
                        contains("RDEL"),
                        contains("CDEL"),
                        contains("AME")),
            currency = "GBP",
            suffixing = TRUE) |> 
        tab_options(container.height = table_height, 
                    container.overflow.y = TRUE) |> 
        cols_width(
            contains("Classification") ~ px(150))

    # apply heatmap to column(s) of choice
    if (heatmap_column != FALSE){
        table_output <- table_output |>
            data_color(columns = contains(heatmap_column),
                       method = "numeric", 
                       palette = 
                           as.character(PrettyCols::prettycols("RedBlues",
                                                        direction = -1)))
    }

    # output table
    return(table_output)
}