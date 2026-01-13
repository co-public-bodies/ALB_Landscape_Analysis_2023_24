#' Generate Interactive and Responsive ALB Bar Plots
#'
#' @description
#' Creates a dynamic `ggiraph` bar chart designed for the ALB Landscape Data Explorer Dashboard.
#' This function handles:
#' * **Responsiveness:** Adapts font sizes, margins, and container height based on the user's screen width and number of selected ALBs.
#' * **Theming:** Toggles color palettes based on whether financial data is inflation-adjusted.
#' * **Interactivity:** Adds HTML tooltips, hover effects, and download capabilities.
#'
#' @param input_dataset A data frame containing the pre-processed data to be plotted. This is generated in the albla_DEx_plot_data_wrangle() function.
#' @param input_numeric_column_x String. The name of the numeric column to map to the x-axis (e.g., "Total FTE", "Income").
#' @param input_inflation_switch_status String. The state of the inflation toggle (e.g., "Original data"). Used to determine bar colors (Blue for original, Orange/Red for adjusted), and is reflected in the title.
#' @param input_unique_albs_count Integer. The number of unique Arms-Length Bodies selected. Used to calculate the dynamic height of the plot container.
#' @param current_width_inches Numeric. The real-time width of the plot container in inches, passed from the client session. Controls font scaling and margin expansion for responsive design on mobile/tablet. Default is 10.
#'
#' @return A `girafe` interactive plot object ready for rendering in Shiny.
#' @import ggplot2
#' @import ggiraph
#' @import ggtext
#' @export


albla_DEx_barplot <- function(input_dataset,
                                     input_numeric_column_x,
                                     input_inflation_switch_status,
                                     input_unique_albs_count, 
                              current_width_inches = 10) {
    
    # --- Logic for Colors and Labels ---
    # sets colors based on whether data shown is adjusted for inflation
    plot_color_value <- case_when(
        input_inflation_switch_status == "Original data" ~ "#00344A",
        input_inflation_switch_status != "Original data" & 
            !grepl(input_numeric_column_x, pattern = "Income|Spend|Expenditure|Costs") ~ "#00344A",
        input_inflation_switch_status != "Original data" &
            grepl(input_numeric_column_x, pattern = "Income|Spend|Expenditure|Costs") ~ "#e37b0b"
    )    
    
    #### X-Axis settings ####
    # set prefix, suffix and axis title for X-Axis
    
    xlab_prefix <- case_when(
        .default = "", 
        grepl(input_numeric_column_x, pattern = "Income|Spend|Pay|Expenditure|Costs") ~ "£"
    )
    
    xlab_suffix <- case_when(
        .default = "", 
        !grepl(input_numeric_column_x, pattern = "Income|Spend|Pay|Expenditure|Costs") ~ " FTE"
    )
    
    xlab_text <- case_when(
        .default = input_numeric_column_x,
        input_inflation_switch_status != "Original data" & grepl(input_numeric_column_x, pattern = "Income|Spend|Expenditure") ~
            paste0(input_numeric_column_x, " (", input_inflation_switch_status, ")")
    )
    
    # --- Calculate Plot Dynamic Font Sizes ---
    # base size of theme font on the size of the plot within viewer's screen
    dynamic_font_size <- case_when(
        current_width_inches < 4  ~ 7.5,   # Mobile: Compact text (7.5pt)
        between(current_width_inches, 4, 5) ~ 8.5, # larger mobile / small tablets
        between(current_width_inches, 5, 6) ~ 10, 
        between(current_width_inches, 6, 8) ~ 12, # Tablet: Standard (12pt)
        .default = 14                     # Desktop: Larger, readable text (14pt)
    )
    
    # --- Calculate Plot Container Dynamic Height ---

    # Expand height of barplot container based on how many ALBs are selected
    # Plot grows with an increase in ALBs selected to prevent crushing 
    
    calculated_height_inches <- case_when(
        .default = 5,
        # set sizes for smaller ALB selections 
        between(input_unique_albs_count, 1, 10) ~ 5,
        # grow plot as more ALBs are added
        between(input_unique_albs_count, 10, 15) ~ 7.5,
        between(input_unique_albs_count, 15, 20) ~ 10,
        # dynamically grow as more than 20 ALBs are selected
        input_unique_albs_count > 20 ~ 10 + (0.2 * input_unique_albs_count) 
    )
    
    
    # Calculate right-sided breathing room in margins for plot - preventing truncation
    
    right_expansion <- case_when(
        current_width_inches <= 6 ~ 0.2,  # Mobile: lots of room for labels
        current_width_inches <= 10 ~ 0.15, # Tablet: moderate room
        .default = 0.1                # Desktop: standard room
    )
    
    # --- Static Plot Creation ---
    
    # build barplot using ggplot and ggiraph syntax
    # previously used plotly
    static <- suppressWarnings(
        input_dataset |> 
            # remove apostrophes which break ggiraph tooltips
            mutate(ALB = str_remove_all(
                ALB, pattern = "\\'"
            )) |> 
            ggplot(aes(
                x = get(input_numeric_column_x),
                y = `Plot Name`,
                group = `Financial Year`
            )) +
            geom_col_interactive(
                position = "dodge", 
                stat = "identity",
                aes(
                    fill = barColour,
                    alpha = `Financial Year`,
                    data_id = paste0(ALB, `Financial Year`), 
                    tooltip = paste0("<b>", ALB, " (", `Financial Year`, ")</b>", 
                                     "<br>Sponsored by: ", Department,
                                     "<br>", xlab_text, ": ", xlab_prefix, 
                                     comma(janitor::round_half_up(
                                         get(input_numeric_column_x))), xlab_suffix
                    )
                )
            ) +
            # for multiple years of data, set alpha to help user distinguish
            # between different years
            scale_alpha_manual(
                values = c("2022/23" = 0.75, "2023/24" = 1)
            ) +
            # if an ALB is highlighted change colours so they stand out
            # logic is that all bars are highlighted by default
            # but if highlighted ALBs are selected, everything else becomes 
            # 'not highlighted' and turns to grey
            scale_fill_manual(values = c(
                "Highlighted" = plot_color_value,
                "Not highlighted" = "#e5e5e5"
            )) +
            # labels need to be geom_richtext for html tags to work
            ggtext::geom_richtext(
                aes(
                    x = label_pos,
                    label = Label, 
                    group = `Financial Year`, 
                    size = label_size / ggplot2::.pt,
                    color = label_color
                ),
                fill = NA,         
                label.color = NA,  
                position = position_dodge(width = 0.9),
                hjust = 0
            ) +
            # themeing and styling
            scale_size_identity() + 
            scale_color_manual(values = c("black" = "black", "white" = "white")) +
            theme_classic(base_size = dynamic_font_size, base_family = "roboto") +
            theme(legend.position = "none") +
            labs(y = NULL, x = xlab_text) +
            scale_y_discrete(expand = c(0,0)) +
            scale_x_continuous(position = "top", expand = expansion(mult = c(0, right_expansion)),
                               labels = label_number(scale_cut = cut_short_scale(), 
                                                     prefix = xlab_prefix, 
                                                     suffix = xlab_suffix)) + 
            theme(panel.grid.major.x = element_line(colour = "grey85"), 
                  axis.title.x = element_text(face = "bold", size = rel(1.1)))
    )
    
    # --- Turn into Girafe Object - Dynamic Plot ---
    dynamic <- girafe(
        ggobj = static,
        width_svg = current_width_inches,
        height_svg = calculated_height_inches,
        options = list(
            opts_sizing(rescale = TRUE),
            opts_hover(css = "fill:#B53737;cursor:pointer;"),
            opts_selection(type = "none"),
            opts_toolbar(saveaspng = TRUE, 
                         pngname = paste0("ALB-Data-Explorer-", format(Sys.Date(), "%Y-%m-%d"))
                         ), 
            opts_tooltip(css = paste0(
                "background-color: #00344A;",  # background matches dashboard theme
                "color: white;",               # White text
                "font-family: sans-serif;",    # Clean font
                "font-size: 0.9rem;",          # Readable size
                "padding: 10px;",              # Breathing room inside the box
                "border-radius: 5px;",         # Rounded corners
                "box-shadow: 3px 3px 5px rgba(0,0,0,0.3);", # Drop shadow for depth
                "border: 1px solid white;",    # Thin white border for high contrast
                "z-index: 9999;"               # Ensure tooltip sits on top of everything
            ),
            # have tooltip follow cursor
            use_cursor_pos = TRUE)
        )
    )
    
    return(dynamic)
}