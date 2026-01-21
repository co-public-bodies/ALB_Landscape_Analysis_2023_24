#' @title ALB Landscape Data Explorer
#' @version 2.1
#' @author Martin Ingram, Mark Swan
#'
#' @description
#' A Shiny dashboard designed by the Cabinet Office, for exploring the landscape of UK Government
#' Arm's Length Bodies (ALBs). This tool provides a user-friendly interface for
#' the public and policy professionals to analyse and explore data on ALB
#' # staffing (FTE), financial data (Funding, RDEL, CDEL, AME),
#' and accountability information. Data can be explored for ALBs 
#' across different government departments and financial years (2022/23 - 2023/24).
#' 
#' @details
#' **Application Architecture:**
#' This file (`app.R`) serves as the primary entry point and controller. It handles
#' UI generation and Server logic but relies on helper functions all stored in the
#' `Assets/` directory. These assets are sourced dynamically at runtime to ensure
#' the main script remains clean and maintainable.
#'
#' **Key Features:**
#' * **Dynamic Filtering:** Users can filter by Sponsoring Department, specific ALBs,
#'     and Financial Year.
#' * **Inflation Adjustment:** Integrated toggle to adjust financial figures to a
#'     2023/24 baserate using GDP deflators.
#' * **Plot Builder:** Interactive bar charts (`ggiraph`) with drill-down capabilities
#'     and contextual tooltips. Handles data caveats (e.g., missing metrics in specific years)
#'     via dynamic notifications.
#' * **Headline Totals:** High-level summary cards (`bslib::value_box`) showing
#'     aggregated metrics for selected bodies.
#' * **Dataset Builder:** A custom table generator (made in `gt`) allowing users to select
#'     specific variables (Staffing, Finance, Accountability) and export the custom
#'     slice as CSV or formatted RTF.
#'
#' **Data Sources:**
#' Data is sourced from the Cabinet Office Public Bodies Directory publications.
#' The app imports local Excel datasets and provides links to full GitHub repositories
#' for raw data access.
#'
#' @note
#' Ensure the `Assets/` folder contains all necessary helper scripts (including
#' `headline_calculator.R`, `albla_DEx_barplot.R`, etc.) before launching.
#'
#' @seealso
#' \url{https://www.gov.uk/guidance/public-bodies-reform}
#'

# import packages
suppressPackageStartupMessages({
  library(tidyverse)
  library(gt)
  library(gtExtras)
  library(downloadthis)
  library(scales)
  library(shiny)
  library(shinycssloaders)
  library(shinyWidgets)
  library(shinydashboard)
  library(shinyjs)
  library(shinyBS)
  library(bslib)
  library(bsicons)
  library(ggiraph)
  library(ggtext)
  library(htmltools)
  library(httr)
  library(showtext)
})

# add fonts
font_add_google(name = "Roboto", family = "roboto")
showtext_auto()


# Load all functions we use within the app
# Loads helper functions for plotting (albla_DEx_barplot) and data wrangling.
assets_scripts <- list.files(path = "Assets/",
                             pattern = ".R$",
                             full.names = TRUE)
sapply(assets_scripts, source)

# disable scientific notation in outputs
options(scipen = 999)


# Import Text -------------------------------------------------------------
# Load explainer text for the DEX
DEX_overall_text <- read_tsv("Assets/Text Descriptions/DEX Overall Text.txt",
                             col_names = FALSE,
                             show_col_types = FALSE) |> 
  pull(1) |> 
  paste0(collapse = "<br>")


DEX_staff_text <- read_tsv("Assets/Text Descriptions/DEX Staff Text.txt",
                           col_names = FALSE, 
                           show_col_types = FALSE) |> 
  pull(1) |> 
  paste0(collapse = "<br>")

DEX_finance_text <- read_tsv("Assets/Text Descriptions/DEX Finance Text.txt", 
                             col_names = FALSE,
                             show_col_types = FALSE) |> 
  pull(1) |> 
  paste0(collapse = "<br>")

# Import data -----------------------------------------------------------

# import data
# data is published once a year by Cabinet Office, capturing the previous financial year
# as new years of data are collected, we will integrate them into this dataset
dashboard_data <-
  readxl::read_xlsx("Data/2025-04-17 Combined_Landscape_Data_2223_2324.xlsx")

# Variable Lists ----------------------------------------------------------
# variable name translations
combined_variable_list <-
  readxl::read_xlsx("Data/2025-04-17 Combined_Dataset_Variables.xlsx")

# apply user-friendly column names to variables from dictionary
colnames(dashboard_data) <- c(combined_variable_list$`Short Name`)

# flag columns that can't be plotted using 2022/23 data
cols_not_available_to_plot_2023 <- c("Chair Pay", 
                                     "Contingent Labour Spend",
                                     "Consultancy Spend", 
                                     "Contingent FTE Employed on 31st March")

# flag columns that can't be plotted using 2023/24 data
cols_not_available_to_plot_2024 <- c("FTE In London on 31st March")


# Plot builder options ----------------------------------------------------

# get options for plotting drop-down - only shows numerical variables
numeric_columns_for_plotting <- combined_variable_list |>
  filter(Format == "Number (Decimal)") |>
  mutate(`Short Name` = factor(
    `Short Name`,
    levels = c(
      "Total FTE Employed on 31st March",
      "Contingent FTE Employed on 31st March",
      "FTE In London on 31st March",
      "Chair Pay",
      "Government-funded Income",
      "Other Income",
      "Total Income",
      "Total Managed Expenditure",
      "AME Spend",
      "CDEL Spend",
      "RDEL Admin Spend",
      "RDEL Programme Spend",
      "Grants Spend",
      "Contingent Labour Spend",
      "Consultancy Spend", 
      "Total RDEL Spend",
      "Total DEL Spend"
    )
  )) |>
  arrange(`Short Name`) |>
  pull(`Short Name`)

### Plot Builder Options - HTML Tag Styling ####

# Create a vector of HTML strings that display on top of the plot builder drop down options
# this creates badges for the variable options, and is intended to make them appear more visually distinct
html_content_for_dropdown <- sapply(numeric_columns_for_plotting, function(name) {
  
  name_char <- as.character(name) # Ensure variable name is treated as character
  
  # Set the Bootstrap badge class based on the name -
  # splitting into variables with Funding, Spending, Chair Pay, and FTE data
  
  # Default color/class is danger primary (blue)
  badge_class <- "text-bg-primary" 
  
  # Default icon is the tolley cart, reflecting that most variables capture ALB spending
  icon_tag_html <- as.character(shiny::icon("cart-shopping", lib = "font-awesome")) 
  
  # style FTE columns differently - different icon and color
  if (grepl("FTE", name_char, ignore.case = TRUE)) {
    
    # danger theme (red) for FTE variables - to make FTE columns stand out
    badge_class <- "text-bg-danger"
    
    # add different icon for FTE to make them stand out - users == people
    icon_tag_html <- as.character(shiny::icon("users", lib = "font-awesome"))
  }
  
  # add styling for government funding tags - makes them distinct from spending
  if (grepl("Income|Funding", name_char, ignore.case = TRUE)) {
    
    # success theme (green) for funding / income based variables
    badge_class <- "text-bg-success"
    
    # add different icon for funding columns
    icon_tag_html <- as.character(shiny::icon("building-columns", lib = "font-awesome"))
  }
  
  # styling for Chair Pay column - styled to stand out from other columns above
  if (grepl("Chair Pay", name_char, ignore.case = TRUE)) {
  
    badge_class <- "text-bg-warning"
    
    icon_tag_html <- as.character(shiny::icon("user-tie", lib = "font-awesome"))
  }
  
  # Use sprintf to create the final HTML string
  # This combines the class, icon, and text into a single styled badge
  sprintf("<span class='badge %s' style='width: 100%%; text-align: left;'>%s %s</span>",
          badge_class,
          icon_tag_html,
          name_char)
  
}, USE.NAMES = FALSE)


#### Inflation Adjustment Columns ####

# get columns with finance information for inflation adjustments
# this tells the app which columns are in scope for inflation adjustment

finance_columns_for_adjustments <- numeric_columns_for_plotting |>
  as.character() |> 
  tibble(fin_cols = _) |> 
  filter(!grepl("FTE", x = fin_cols)) |> 
  # remove chair pay from adjustments 
  # as 2022/23 data is not numeric format and
  # is therefore not directly comparable with 2023/24
  filter(fin_cols != "Chair Pay") |> 
  pull(fin_cols) # extract as vector


# UI Setup --------------------------------------------------------------

# drop down options for Financial Years within the data
fy_timepoint_choices <- dashboard_data |> 
  select(`Financial Year`) |> 
  unique() |> 
  pull()


# drop-down options for each sponsoring department in data
parent_dept_choices <- dashboard_data |>
  select(`Department`) |>
  unique() |>
  pull() |> 
  sort()

# Create the nested list with ALBs under departments for choices
initial_alb_choices <- split(dashboard_data$ALB, dashboard_data$Department) %>%
  lapply(unique)

# select a vector of ALBs from a random department to show on startup
# previously used to default to showing AGO bodies, but randomising helps show
# the wider landscape to audiences when visiting
random_albs_for_startup <-
  unlist(initial_alb_choices[sample(1:length(unlist(parent_dept_choices)), 1)])


# container for all different sets of variable options
variable_options <- list()

# drop-down options for columns with overall ALB information
variable_options$overall <- combined_variable_list |>
  filter(
    variable_group == "overall" |
      `Variable Name (Dataset)` == "staffing_overall_hq_region"
  ) |>
  filter(!`Short Name` %in% c("ALB", # not required for selection
                              "Department", "Number of Bodies",
                              "Sponsor", 
                              "Plot Name")) |>
  select(`Variable Name (Dataset)`, `Short Name`)


# drop-down options for columns with ALB accountability information
variable_options$accountability <- combined_variable_list |>
  filter(variable_group == "accountability") |>
  select(`Variable Name (Dataset)`, `Short Name`) 

# drop-down options for columns with ALB staffing (FTE) information
variable_options$staffing <- combined_variable_list |>
  filter(variable_group == "staffing") |>
  filter(`Variable Name (Dataset)` != "staffing_overall_hq_region") |>
  select(`Variable Name (Dataset)`, `Short Name`) |>
  mutate(`Short Name` = factor(
    `Short Name`,
    levels = c(
      "Total FTE Employed on 31st March",
      "Contingent FTE Employed on 31st March",
      "FTE In London on 31st March", 
      "ALB Employs Staff"
    )
  )) |>
  arrange(`Short Name`)

# drop-down options for columns with ALB finance information
variable_options$finance <- combined_variable_list |>
  filter(variable_group == "finance") |>
  select(`Variable Name (Dataset)`, `Short Name`) |>
  mutate(`Short Name` = factor(
    `Short Name`,
    levels = c(
      "Total Income",
      "Government-funded Income",
      "Other Income",
      "Total Managed Expenditure",
      "Total DEL Spend",
      "Total RDEL Spend",
      "RDEL Programme Spend",
      "RDEL Admin Spend",
      "CDEL Spend",
      "AME Spend",
      "Grants Spend",
      "Contingent Labour Spend", 
      "Consultancy Spend",
      "ALB Distributes Grants",
      "Spending Control Levels",
      "Spending Control Details",
      "Finance Supporting Comments"
    )
  )) |>
  arrange(`Short Name`)

# Headline Card Themes --------------------------------------------------

# Blue Card Box
blue_theme_box <- value_box_theme(name = "dx_palette_blue",
                                  bg = "#00344A",
                                  fg = "#FFFFFF")

# White Card Box
white_theme_box <- value_box_theme(name = "dx_palette_white",
                                   bg = "#F5F5F5",
                                   fg = "black")

# Red Card Box
red_theme_box <- value_box_theme(name = "dx_palette_red",
                                 bg = "#B53737",
                                 fg = "#FFFFFF")

# # signal session start
cat(paste0(
  "||| 0",
  " === ",
  Sys.time(),
  " || ",
  "=== ",
  "Session Started.\n"
))


# Full Dataset Downloads  ----------------------------------------------

# Define the URL of the full xlsx file for 2022/23
file_url_22_23 <- "https://github.com/co-public-bodies/ALB_Landscape_Analysis_2022_23/raw/refs/heads/main/Data/2024-12-17%20Public%20Bodies%20Directory%202022_23.xlsx"

# Define the URL of the full xlsx file for 2023/24
file_url_23_24 <- "https://github.com/co-public-bodies/ALB_Landscape_Analysis_2023_24/raw/refs/heads/main/Data/2025-05-29%20Public%20Bodies%20Directory%202023_24.xlsx"

# Define a suggested filename for the download of 2022/23
suggested_download_filename_22_23 <- "Public_Bodies_Directory_2022_23.xlsx"

# Define a suggested filename for the download of 2023/24
suggested_download_filename_23_24 <- "Public_Bodies_Directory_2023_24.xlsx"


# User Interface ------------------------------------------------------

# setup UI
ui <- bslib::page_sidebar(
  
  theme = bs_theme(version = 5, # bslib themeing
                   primary = "#00344A",
                   secondary = "#B53737"
                   ),
  
  # setting to enable Shinyjs functions within app
  useShinyjs(),

# UI: Sidebar --------------------------------------------------------------

    sidebar = sidebar(open = TRUE, width = '25%', id = "sidebar_collapse",
                      
                      h5("Data Filters:"),

                      # selection drop down for sponsor depts
                      pickerInput(
                        "selected_department",
                        width = '100%',
                        label = HTML("<b>Select Sponsor(s):</b>"), 
                        choices = parent_dept_choices,
                        selected = parent_dept_choices,
                        options = list(
                          container = "body",
                          `live-search` = TRUE,
                          `actions-box` = TRUE,
                          size = 10,
                          `selected-text-format` = "count > 1"
                        ),
                        multiple = TRUE
                      ),
                      
                      # selection drop down for ALBs under sponsors
                      pickerInput(
                        "selected_albs",
                        width = '100%',
                        label = HTML("<b>Select Arm's Length Bodies:</b>"), 
                        choices = initial_alb_choices,
                        selected = random_albs_for_startup,
                        multiple = TRUE,
                        options = list(
                          container = "body",
                          `live-search` = TRUE,
                          `actions-box` = TRUE,
                          size = 10,
                          `selected-text-format` = "count > 1"
                        ),
                        
                      ), 
                      
                      # selection drop down for ALBs to highlight
                      pickerInput(
                        "highlighted_albs",
                        width = '100%',
                        label = HTML("<b>Select ALBs To Highlight:</b>"),
                        choices = NULL,
                        multiple = TRUE,
                        options = list(
                          container = "body",
                          `live-search` = TRUE,
                          `actions-box` = TRUE,
                          size = 10,
                          `selected-text-format` = "count > 1"
                        ),
                        selected = 0
                      ),
                      
                     # selection drop-down for variable options for plot builder
                     # uses html-styled tags to stand out in the UI
                      pickerInput(
                        inputId = "visualise_column",
                        label = HTML("<b>Plot Builder Variable: (X-Axis):</b>"),
                        choices = numeric_columns_for_plotting,
                        selected = numeric_columns_for_plotting[1],
                        multiple = FALSE,
                        width = "100%",
                        options = pickerOptions(
                          liveSearch = TRUE, 
                          container = "body"
                        ),
                        # 'choicesOpt' controls the display of the options in the dropdown
                        choicesOpt = list(
                          content = html_content_for_dropdown
                        )
                      ),
                      
                      # selection drop down for financial years
                      pickerInput(
                        "selected_fy_picker",
                        width = '100%',
                        label = HTML("<b>Select Financial Year(s):</b>"), 
                        choices = fy_timepoint_choices,
                        selected = max(fy_timepoint_choices),
                        options = list(
                          container = "body",
                          `live-search` = TRUE,
                          `actions-box` = TRUE,
                          size = 10,
                          `selected-text-format` = "count > 1"
                        ),
                        multiple = TRUE
                      ),
                      
                      # inflation adjustment widget - radio button for baserate
                     conditionalPanel(condition = "/Income|Spend|Expenditure|Costs/.test(input.visualise_column)",
                     
                      prettyRadioButtons(inputId = "switch_inflation_adjustment", 
                                         label = HTML("<b>Inflation adjustment:</b>"), 
                                         choices = c("Original data", "Adjusted to 2023/24 baserate"), 
                                         selected = "Original data", shape = "round", 
                                         outline = TRUE, thick = TRUE, icon = icon("check"),
                                         status = "danger",
                                         fill = TRUE, animation = "pulse", 
                      )
                     )
                    ), #### End of sidebar ====

    
    
# UI: CSS -----------------------------------------------------------------

# Meta data for improving visibility on Google searches
tags$head(
  # Title for browser tab and Google searches
  tags$title("ALB Landscape Analysis Data Explorer | Cabinet Office"),
  
  # Text for description under URL on Google
  tags$meta(name = "description", 
            content = "Interactive explorer for the UK Government's Arm's Length Bodies (ALB) data in 2023/24. Filter by department, budget, spending, and staff counts."),
  
  # Keywords 
  tags$meta(name = "keywords", content = "ALBs, Arms-Length Bodies, Public Bodies, Quangos, Cabinet Office, UK Government, Data, Shiny")
),

  # Add CSS to make the dropdown float on top of everything else
  tags$head(# logo - places onto the right of the dashboard
    tags$script(
      HTML(
        '$(document).ready(function() {
            $(".navbar .container-fluid")
            .append("<img id = \'CO_Logo\' src=\'Cabinet_Office_WHITE_AW.png\' align=\'right\' height = \'57.5px\'>"  );
            });'
      )
    ), tags$style(
      HTML(
        '@media (max-width:992px) { #Cabinet_Office_WHITE_AW.png { position: fixed; right: 10%; top: 0.5%; }}'
      )
    )),
  
  
  # import css/scss stylesheets
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/scss", href = "scss_style.scss")
  ),
  
  tags$head(tags$style(HTML("a {color: #B53737}"))),
  
# Favicon formatting
  tags$head(
    tags$link(rel = "shortcut icon", href = "favicon.ico"),
    tags$link(rel = "apple-touch-icon", sizes = "180x180", href = "favicon.ico"),
    tags$link(
      rel = "icon",
      type = "image/png",
      sizes = "32x32",
      href = "/favicon-32x32.png"
    ),
    tags$link(
      rel = "icon",
      type = "image/png",
      sizes = "16x16",
      href = "/favicon-16x16.png"
    )
  ),

  # UI: Header Panel -------------------------------------------------
  
  fluidRow(box(
    width = 12,
    ## App title
    titlePanel(
      windowTitle = "ALB Landscape Explorer",
      title =
        fluidRow(
          #### Header Bar Styling ####
          # Use display:flex and align-items:center for vertical centering
          style = "background-color:#00344A;
               height:80px;
               margin:10px;
               border-radius:8px;
               display:flex;
               align-items:center;",
          column(
            width = 2,
            style = "text-align: center;",
            img(
              src = "Cabinet Office_WHITE_AW.png",
              height = "50px"
            )
          ),
          column(
            width = 9,
            # margin:0 in the h3 style removes default browser spacing
            h3(style = "text-align:center;color:white;margin:0;",
               span(
                 "ALB Landscape Data Explorer",
                 span(
                   "V2.1",
                   style = "background-color: #B53737; color: white; padding: 3px 8px; border-radius: 4px; font-size: 0.8em; margin-left: 8px; font-weight: bold;"
                 )
               )
            )
          ),
          # github url / icon
          column(width = 1,
                 style = "text-align: center; display: flex; justify-content: center;",
                 
                 # hyperlink to repository
                 tags$a(
                   href = "https://github.com/co-public-bodies/ALB_Landscape_Analysis_2023_24", 
                   target = "_blank", # Opens link in a new tab
                   style = "color: white; text-decoration: none;", 
                   
                   # github icon
                   bs_icon("github", size = "2.5rem"),

                 ) |> 
                   # tooltip
                   tooltip("View Source Code")
          )
        )
    ), #### End of Headerbar Styling ####
    
    tags$br(), 
    
    # setup container space for tabset with different DEx options/views
    div(style = "padding-left: 10px;",
    tabsetPanel(id = "main_box_tabset", 
                  selected = "Plot Builder",
                # selected = "About This Data",
                  type = "pills",
              

  # UI: Plot Panel ----------------------------------------------------------
    
  tabPanel(
    title = "Plot Builder",
    icon = icon("palette"),
    
    tags$hr(),
    
    # Dynamic text above the plot to explain caveats
    htmlOutput(outputId = "variable_note_text"),
    
    bslib::card(
      id = "plot_card_full_screen",
      full_screen = TRUE,
      withSpinner(
        girafeOutput("plot_variables", width = "100%", height = "100%"),
        color = "#B53737"
      )
    )
  ), 
  
  ## UI: Headline boxes ====
  
  tabPanel(title = "Headline Totals", icon = icon("newspaper"),
           
           tags$hr(),
           
           fluidRow(
             id = "vb_row",
          
             #### Headlines 1 ####
             
             # provides headlines for entire department(s) set of ALBs in most recent year of data
             
             h4(textOutput(outputId = "ALBs_Selected_Summary_text"), align = "center",
                style = "padding-left: 20px; padding-top: 25px;"),

             # ALB Staff employed box
             column(
               width = 4,
               style = "padding-left: 25px;
                             padding-right: 10px;",
               value_box(max_height = "180px",
                         id = 'headline_FTE', 
                         title = "Staff Employed (FTE):",
                         value = textOutput("Dept_ALB_FTE_text"),
                         theme = blue_theme_box,
                         showcase = bs_icon("person-workspace")
               )%>% 
                 tooltip("Staff employed is measured as the full-time equivalent (FTE) number of staff as at the 31st of March, which is the end of the financial year.", 
                         id = "headline_staff_tooltip") 
             ),
             
             # ALB Government Funding box
             column(
               width = 4,
               style = "padding-left: 10px;
                             padding-right: 10px;",
               value_box(max_height = "180px",
                         id = 'headline_Govt_Budget', 
                         title = "Direct Government Funding:",
                         value = textOutput("Dept_ALB_govt_funding_text"),
                         theme = white_theme_box,
                         showcase = bs_icon("bank")
               ) %>% 
                 tooltip("Direct government funding captures the amount of public funding allocated to the ALB by HM Treasury.",
                         id = "headline_government_funding_tooltip") 
             ),
             
             
             # ALB Other Funding
             column(
               width = 4,
               style = "padding-left: 10px;
                             padding-right: 10px;",
               
               
               value_box(max_height = "180px",
                         id = 'headline_other_funding', 
                         title = "Other Income:",
                         value = textOutput("Dept_ALB_other_funding_text"),
                         theme = red_theme_box,
                         showcase = bs_icon("cash")
               ) %>% 
                 tooltip("Other income captures any additional funding that supports the ALB's operations outwith their direct funding allocated by HM Treasury. This includes
                         income or revenue generated by the ALB through levies, fees, cost recovery, lottery funding, or commercial income.",
                         id = "headline_other_funding_tooltip") 
             ),
 
           tags$hr(),
 
           #### Headlines 2 ####
           fluidRow(
             id = "vb_row2",
             
             column(
               width = 4,
               style = "padding-left: 25px;
                             padding-right: 10px;",
               
               # ALB RDEL Spending
               value_box(max_height = "180px",
                         id = 'headline_RDEL',
                         title = "RDEL Spending:",
                         value = textOutput("ALB_RDEL_Expenditure_text"),
                         theme = red_theme_box,
                         showcase = bs_icon("person-add")
               )  %>% 
                 tooltip("Departmental Expenditure Limits (DEL) is the amount that government departments have been allocated by the Treasury to spend each year. 
                         DEL budgets are split into two additional categories: Resource spending (RDEL) – which covers what the government spends on its day-to-day running and administration costs. These are generally goods and services, like nurses’ pay or medicines.", 
                         id = "headline_RDEL_tooltip")
             ),
            
             #ALB CDEL expenditure box
             column(
               width = 4,
               style = "padding-left: 10px;
                             padding-right: 25px;",
               value_box(max_height = "180px",
                         id = "headline_CDEL",
                         title = "CDEL Spending:",
                         value = textOutput("ALB_CDEL_Expenditure_text"),
                         theme = blue_theme_box,
                         showcase = bs_icon("motherboard")
               ) %>% 
                 tooltip("Departmental Expenditure Limits (DEL) is the amount that government departments have been allocated by the Treasury to spend each year. 
                         DEL budgets are split into two additional categories: Capital spending (CDEL) – which is funding for investment to improve the UK’s infrastructure and public services. For example, new roads, hospitals and military equipment.",  
                         id = "headline_CDEL_tooltip")
               
               ),
             
             # ALB AME Spending
             column(
               width = 4,
               style = "padding-left: 25px;
                             padding-right: 10px;",
               value_box(max_height = "180px",
                         id = 'headline_AME',
                         title = "AME Spending:",
                         value = textOutput("ALB_AME_Expenditure_text"),
                         theme = white_theme_box,
                         showcase = bs_icon("graph-up-arrow")
               ) %>% 
                 tooltip("Annually Managed Expenditure (AME) is money spent on things that are harder to plan for, usually because demand for them varies, so budgets are not fixed in advance. This includes welfare, pensions and debt interest payments.", id = "headline_AME_tooltip")
             )
           )
           )
           
  ), # end of totals panel

# Table Builder panel -----------------------------------------------------

  tabPanel(title = "Dataset Builder", icon = icon("table"), 
           
           tags$hr(),
           
           p(
             "Using the dropdown menus below you can create a dataset for the ALBs
              you have chosen and add or remove data variables as you wish. The dataset
              can then be downloaded either as a .csv file or a table to be put into
              other documents."
           ),
           
           fluidRow(

             #### DB column 1 ####
             column(
               width = 5,
               offset = 1,
               
               # Drop-down for Overall Columns
               pickerInput(
                 "selected_overall",
                 width = '100%', 
                 label = HTML("<b>ALB Overall Information:</b>"),
                 choices = variable_options$overall$`Short Name`,
                 multiple = TRUE,
                 selected = NULL,
                 options = list(
                   `live-search` = TRUE, `actions-box` = TRUE, size = 10,
                   `selected-text-format` = "count > 1"
                 )
               ),
               
               # Drop-down for Accountability Columns
               pickerInput(
                 "selected_accountability",
                 width = '100%', 
                 label = HTML("<b>ALB Accountability:</b>"),
                 choices = variable_options$accountability$`Short Name`,
                 multiple = TRUE,
                 selected = NULL,
                 options = list(
                   `live-search` = TRUE, `actions-box` = TRUE, size = 10,
                   `selected-text-format` = "count > 1"
                 )
               )
             ),
             
             #### DB column 2 ####
             column(
               width = 5,
               
               # Drop-down for Staffing Columns
               pickerInput(
                 "selected_staffing",
                 width = '100%',
                 label = HTML("<b>ALB Staff (FTE):</b>"),
                 choices = variable_options$staffing$`Short Name`,
                 multiple = TRUE,
                 selected = NULL,
                 options = list(
                   `live-search` = TRUE, `actions-box` = TRUE, size = 10,
                   `selected-text-format` = "count > 1"
                 )
               ),
               
               # Drop-down for Finance Columns
               pickerInput(
                 "selected_finance",
                 width = '100%',
                 label = HTML("<b>ALB Finance:</b>"),
                 choices = variable_options$finance$`Short Name`,
                 multiple = TRUE,
                 selected = NULL,
                 options = list(
                   `live-search` = TRUE, `actions-box` = TRUE, size = 10,
                   `selected-text-format` = "count > 1"
                 )
               )
             )
           ),
           
          #### Table Builder: Table ####
            gt_output("table_with_data"),
          
          # tags$hr(), 
          
           fluidRow(
            
             column(width = 3,  #### Download dynamic table builder data as csv #### 
               actionBttn(
                 inputId = "visible_downloadtabledata_csv",
                 label = "Save table data as csv",
                 icon = bs_icon("filetype-csv"),  
                 style = "unite",
                 color = "primary", 
                 size = "xs"
               ), 
               #  hidden download button that down the work for the action button
               tags$div(
                 style = "position: absolute; width: 1px; height: 1px; padding: 0; margin: -1px; overflow: hidden; clip: rect(0, 0, 0, 0); border: 0;",
                 downloadButton(
                   outputId = "hidden_downloadtabledata_csv",
                   label = "Hidden Download Trigger" # This label won't be seen
                 )
               )
               
             ),
             column(
               width = 3,   #### Download dynamic table builder data as RTF #### 
               actionBttn(
                 inputId = "visible_downloadtabledata_rtf",
                 label = "Save as formatted table",
                 icon = bs_icon("table"),  
                 style = "unite",
                 color = "primary", 
                 size = "xs"
               ), 
               #  hidden download button that down the work for the action button
               tags$div(
                 style = "position: absolute; width: 1px; height: 1px; padding: 0; margin: -1px; overflow: hidden; clip: rect(0, 0, 0, 0); border: 0;",
                 downloadButton(
                   outputId = "hidden_downloadtabledata_rtf",
                   label = "Hidden Download Trigger"
                 )
               )
               
             ),
          # fluidRow(
             column(width = 3, 
                    #### Download all data button for 2022/23 #### 
                    actionBttn(
                      inputId = "download_full_data_button_22_23",
                      label = "Download all 22/23 data",
                      icon = bs_icon("file-earmark-excel"),  
                      style = "unite",
                      color = "royal", 
                      size = "xs"
                    )
             ),
             
             column(width = 3,
                    #### Download all data button for 2023/24 #### 
                    actionBttn(
                      inputId = "download_full_data_button_23_24",
                      label = "Download all 23/24 data",
                      icon = bs_icon("file-earmark-excel-fill"),  
                      style = "unite",
                      color = "success", 
                      size = "xs"
                    )
             )
           )
           ), 
  

# UI: About this data panel ---------------------------------------------------

  tabPanel(title = "About This Data", icon = icon("circle-info"), 
           
           tags$hr(),
           
           accordion(id = "accordion_about_data",
                     open = "accordion_overall",
                     multiple = FALSE, 
                     
                     accordion_panel(title = "Overview",
                                     icon = icon("circle-info"), 
                                     HTML(DEX_overall_text)
                                     ), 
                     accordion_panel(title = "Staff Data",
                                     icon = icon("users"), 
                                     HTML(DEX_staff_text)),
                     
                     accordion_panel(title = "Finance Data",
                                     icon = icon("gbp"), 
                                     HTML(DEX_finance_text))
           ) # end of accordion
  )

    )
    )
  )
  )
)#### End of UI ####


# Server ------------------------------------------------------------------
server <- function(input, output, session) {

# Server: Data Wrangling ---------------------------------------------

  # filter data to parent department of choice
  user_filtered_data_dept <- reactive({
    temp <- dashboard_data |>
      filter(`Department` %in%
               input$selected_department)
    
    
    #### Inflation Adjustment ####
    # apply inflation adjustments to finance data if user selects switch
    # only applies to finance-related columns
    if (input$switch_inflation_adjustment == "Adjusted to 2023/24 baserate"){
      temp <- temp |>
        mutate(across(
          all_of(finance_columns_for_adjustments),
          \(current_col_value) current_col_value * (fy_base_year_deflator / fy_deflator)
        ))
      
    }
    
    temp
  })


  observeEvent(input$selected_department, {
   
    # fetch user's current selection before updating ALB list
    current_selection <- input$selected_albs
    
    # Ensure we don't proceed with an empty department selection
    if (is.null(input$selected_department)) {
      # If no department is selected, clear the ALB picker
      updatePickerInput(
        session = session,
        inputId = "selected_albs",
        choices = "Select a department first",
        selected = NULL
      )
      return()
    }
    
    # Filter the main dataframe based on the selected department(s)
    filtered_data <- user_filtered_data_dept() %>%
      filter(Department %in% input$selected_department)
    
    # Create the nested list with ALBs under departments for choices
    nested_choices <- split(filtered_data$ALB, filtered_data$Department) %>%
      lapply(unique)
    
    # Determine which of the old selections are valid in the new list of choices
    # - unlist() flattens the nested list into a simple vector of all available ALBs.
    # - intersect() finds the items that are in both 'current_selection' and the new list.
    new_selection <- intersect(current_selection, unlist(nested_choices, use.names = FALSE))
    
    # Update the picker input with the nested choices and the preserved selection
    updatePickerInput(
      session = session, # Always good practice to pass the session object
      inputId = "selected_albs",
      choices = nested_choices,         # Use the nested list for the UI
      selected = new_selection,         # Use the preserved list for the selection
      options = list(`actions-box` = TRUE)
    )
  }, ignoreNULL = FALSE) # Use ignoreNULL = FALSE to handle when the user de-selects all departments
  
  
  #update pickers options for highlighting to only show ALBs which are selected above
  observeEvent(input$selected_albs, {
    updatePickerInput(
      inputId = "highlighted_albs",
      options = list(`actions-box` = TRUE),
      selected = 0,
      choices = user_filtered_data_alb() |>
        pull(ALB) |>
        as.character() |>
        unique() |>
        sort()
    )
    
  })
  
  
  # make secondary dataset with departments data that now shows
  # only ALBs selected from second picker 
  user_filtered_data_alb <- reactive({
    
    user_filtered_data_dept() |>
      filter(ALB %in% input$selected_albs) %>%
      filter(`Financial Year` %in% input$selected_fy_picker) |> 
      mutate(barColour = case_when(
        .default = "Highlighted",
        length(input$highlighted_albs) >= 1 ~ case_when(.default = "Not highlighted", ALB %in% input$highlighted_albs ~ "Highlighted")
      ))
    
  })
  
  # Server: Plot Output --------------------------------------------------
  
  # Calculate plot width dynamically based on user screen size
  # Returns width in inches, clamped between 4 and 20, which defines how 
  # wide to draw the plot. Clamps help prevent the plot from scaling infinitely
  # on ultra large and very small screens

  current_plot_width <- reactive({
    
    # Get width in pixels from the client
    plot_width_px <- session$clientData$output_plot_variables_width
    
    # Handle initial load (width might be NULL briefly)
    if (is.null(plot_width_px)) plot_width_px <- 800
    
    # Convert pixels to inches (Standard web DPI is 96)
    width_in <- plot_width_px / 96
    
    # Clamp: Minimum 4 inches (phones), Max 20 (ultrawides)
    max(4, min(width_in, 20))
  })
  
  # make up data for plot - dynamically reacts to various user inputs
  bar_plot_data <- reactive({

    # build bar plot data using
    # "Assets/alba_DEx_plot_data_wrangle.R"
      user_filtered_data_alb() |> 
      albla_DEx_plot_data_wrangle(input_numeric_column_x = input$visualise_column, 
                                  input_unique_financial_years_count = length(input$selected_fy_picker), 
                                  input_unique_albs_count = length(input$selected_albs), 
                                  input_highlighted_albs = input$highlighted_albs,
                                  
                                  input_columns_with_caveats = cols_not_available_to_plot_2023,
                                  input_columns_with_caveats_23_24 = cols_not_available_to_plot_2024, 
                                  current_width_inches = current_plot_width()
                                  )
  })
  
  # with dynamic data above - build bar plot based on user inputs
  output$plot_variables <- renderGirafe({
    
    req(input$selected_fy_picker, input$selected_department, input$selected_albs)
  
    # Data availability validation checks
    
    # Check if the selected variable is invalid for the selected year
    # This prevents the red error message by stopping execution cleanly
    
    # # Check 2022/23 conflicts
    if (input$visualise_column %in% cols_not_available_to_plot_2023) {
      validate(
        need(!identical(input$selected_fy_picker, "2022/23"), message = "")
      )
    }

    # Check 2023/24 conflicts
    if (input$visualise_column %in% cols_not_available_to_plot_2024) {
      validate(
        need(!identical(input$selected_fy_picker, "2023/24"), message = "")
      )
    }
    
    
    # build bar plot using data from bar_plot_data() built using
    # "Assets/alba_DEx_plot_data_wrangle.R"
    # and plot using the barplot function: 
    # "Assets/alba_DEx_barplot.R"
    bar_plot_data() |>  
      albla_DEx_barplot(input_numeric_column_x = input$visualise_column, 
                        input_inflation_switch_status = input$switch_inflation_adjustment, 
                        input_unique_albs_count = length(input$selected_albs), 
                        current_width_inches = current_plot_width()
      ) 
  }) 
  
  
  # Server: Table ---------------------------------------------------------
  
  # create dataset to show within table
  user_filtered_data_alb_table <- reactive({
    # find active variable name pairs
    # by default show ALB and department, and number of ALBs under name
    # and any variables selected from 4 picker drop-downs
    active_variables <- combined_variable_list |>
      filter(
        `Short Name` %in% c(
          "ALB",
          "Department",
          "Number of Bodies",
          "Plot Name",
          input$selected_overall,
          input$selected_accountability,
          input$selected_staffing,
          input$selected_finance,
          input$visualise_column
        )
      )
    
    # use active variables to select requested columns from data
    user_filtered_data_alb() |>
      select(`Financial Year`, any_of(active_variables$`Short Name`)) |>
      arrange(desc(get(input$visualise_column)))
  })
  
  # render interative GT table
  output$table_with_data <- render_gt({
    user_filtered_data_alb_table() |>
      select(-contains("Plot Name")) |>
      landscape_output_table()
  })
  
  # Server: Headline Cards Text ---------------------------------------------
  
  # Text with headlines for summary boxes
  output$ALBs_Selected_Summary_text <- renderText({
    
    req(input$selected_albs)
  
    
    paste0(
      "Headlines for ", 
      user_filtered_data_alb() |>
        filter(`Financial Year` == max(`Financial Year`)) |> 
        pull(`Number of Bodies`) |> 
        sum(), 
      " selected ALB(s) in ", 
      user_filtered_data_alb() |>
        filter(`Financial Year` == max(`Financial Year`)) |> 
        pull(`Financial Year`) |> 
        unique(), 
      
      
      # add note explaining that data is adjusted for inflation
      if (input$switch_inflation_adjustment == "Adjusted to 2023/24 baserate"){
        " (adjusted to 2023/24 baserate)"
      }
      
    )
  })
  
  # Headline Text for FTE under Department selected
  output$Dept_ALB_FTE_text <- renderText({
    headline_calculator(user_filtered_data_alb(), "Total FTE Employed on 31st March", prefix = "")
  })
  
  # Headline Text for Government Budget of ALBs under Department selected
  output$Dept_ALB_govt_funding_text <- renderText({
    headline_calculator(user_filtered_data_alb(), "Government-funded Income")
  })
  

  # Headline Text for RDEL of ALBs Sponsored by Department selected
  output$ALB_RDEL_Expenditure_text <- renderText({
    headline_calculator(user_filtered_data_alb(), 'Total RDEL Spend')
  })
  
  
  # Headline Text for AME under Department selected
  output$ALB_AME_Expenditure_text <- renderText({
    headline_calculator(user_filtered_data_alb(), 'AME Spend')
  })
  
  # Headline Text for Other Funding of ALBs under Department selected
  output$Dept_ALB_other_funding_text <- renderText({
    headline_calculator(user_filtered_data_alb(), 'Other Income')
  })
  
  
  # Headline Text for Number of ALBs Sponsored by Department selected
  output$ALB_CDEL_Expenditure_text <- renderText({
    headline_calculator(user_filtered_data_alb(), 'CDEL Spend')
  })
  

  
  
  # Server: Table Builder Dynamic Downloads Button Outputs -----------------
  
  # Buttons for downloading the dynamic dataset from the table builder
  # use a 2 button system: the more aesthetic actionBttn() triggers a hidden
  # download button that the user cant see, which triggers the download. 
  # we couldnt find an easy way to make the actionBttn() trigger the download 
  # from the table builder, so this is a workaround
  
  #### Dynamic CSV ####
  
  # trigger downloader for csv file from action button
  observeEvent(input$visible_downloadtabledata_csv, {
    
    # Trigger the hidden downloadButton
    shinyjs::runjs("document.getElementById('hidden_downloadtabledata_csv').click();")

  })
  
  # option to download table data as .csv
  output$hidden_downloadtabledata_csv <- downloadHandler(
    
    filename = function() {
      paste0(Sys.Date(), " - ", "ALB_Landscape_DEX_custom.csv")
    },
    content = function(file) {
      write.csv(
        user_filtered_data_alb_table() |>
          # remove internal dashboard names for ALB from user download, 
          # helps to keep downloads cleaner
          select(-contains("Plot Name")),
        file
        

      )
    }
  )
  
  #### Dynamic RTF ####
  
  # trigger downloader for csv file from action button
  observeEvent(input$visible_downloadtabledata_rtf, {
    
    # Trigger the hidden downloadButton
    shinyjs::runjs("document.getElementById('hidden_downloadtabledata_rtf').click();")
    
  })
  
  
  # option to download table data as .rtf table
  output$hidden_downloadtabledata_rtf <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(), " - ", "ALB_Landscape_DEX_custom.rtf")
    },
    content = function(file) {
      gtsave(
        user_filtered_data_alb_table() |>
          # remove internal dashboard names for ALB from user download, 
          # helps to keep downloads cleaner
          select(-contains("Plot Name")) |>
          gt(),
        file
        
      )
      
    }
  )
  
  

  
# Server: TB Button Downloads - Full Datasets -----------------------------

# Download buttons for the full datasets get the data from external sources
# they are currently hosted on github, but we will look to get the .xlsx files
# stored on Gov.UK for greater stability in the long term
  
# Observe the actionBttn click for 22_23 download
observeEvent(input$download_full_data_button_22_23, {

    # --- Trigger Download via JavaScript ---
    js_download_code <- sprintf(
      "var link = document.createElement('a');
       link.href = '%s';
       link.download = '%s';
       link.style.display = 'none';
       document.body.appendChild(link);
       link.click();
       document.body.removeChild(link);",
      file_url_22_23,
      suggested_download_filename_22_23 
    )
    
    # Run the JavaScript code in the user's browser
    runjs(js_download_code)
    
  }) # End observeEvent - download 22/23
  
# Observe the actionBttn click for 23_24 download
observeEvent(input$download_full_data_button_23_24, {
    
    # --- Trigger Download via JavaScript ---
    js_download_code <- sprintf(
      "var link = document.createElement('a');
       link.href = '%s';
       link.download = '%s';
       link.style.display = 'none';
       document.body.appendChild(link);
       link.click();
       document.body.removeChild(link);",
      file_url_23_24,
      suggested_download_filename_23_24 
    )
    
    # Run the JavaScript code in the user's browser
    runjs(js_download_code)
    
  }) # End observeEvent - download 23/24
  
  
# Server Variable Note Text -----------------------------------------------

# Dynamic text for displaying caveats about variables selected by the user. 
# Mostly these are changes in methodology or the availability of variables 
# between different data collection windows. 
# e.g. FTE in London was only captured in the 22/23 publication. 

  output$variable_note_text <- renderUI({
    
    note_string <- case_when(
      .default = "",
      input$visualise_column == "Chair Pay" ~ "<p><b>NOTE</b>: Data for chair salaries during 2022/23 and 2023/24 are not directly comparable due to a change in methodology. Chair Pay data for 2022/23 is therefore not shown in the plot builder.</p>",
      input$visualise_column == "FTE In London on 31st March" ~ "<p><b>NOTE</b>: Data for the variable for 'FTE In London on 31st March' was not collected during 2023/24.</p>",
      input$visualise_column == "Contingent Labour Spend" ~ "<p><b>NOTE</b>: Data for the variable 'Contingent Labour Spend' was not collected during 2022/23.</p>",
      input$visualise_column == "Consultancy Spend" ~ "<p><b>NOTE</b>: Data for the variable 'Consultancy Spend' was not collected during 2022/23</p>",
      input$visualise_column == "Contingent FTE Employed on 31st March" ~ "<p><b>NOTE</b>: Data for the variable 'Contingent FTE Employed on 31st March' was not collected during 2022/23.</p>"
    )
    
    HTML(note_string)
  })

} #### End of Server ####

# Launch App
shinyApp(ui, server)
