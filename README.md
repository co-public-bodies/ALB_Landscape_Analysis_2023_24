# ALB Landscape Analysis 2023/24 ⛰️

Publication of the UK Government's ALB Landscape Analysis covering the 2023/24 Financial Year.

## Website 🌐

[Quarto](https://quarto.org/) documents used to build the [ALB Landscape Analysis Website for 2023/24](https://co-public-bodies.github.io/ALB_Landscape_Analysis_2023_24/). This is a high-level analysis of all ALBs captured in 2023/24, aggregated across all ALBs, and disaggregated by classification, sponsoring department, and ALB purpose categories.

## Data Explorer 🗺️

The [ALB Landscape Data Explorer](https://civil-service-analysis.shinyapps.io/ALB_Landscape_Analysis_Explorer/) is an interactive dashboard built in [R Shiny](http://shiny.posit.co/) that enables granular, organisation-level analysis of the Arm's Length Body Landscape. While the [Website](https://co-public-bodies.github.io/ALB_Landscape_Analysis_2023_24/) provides high-level aggregate trends for ALBs, the data explorer empowers users to drill down into data for specific bodies, compare funding, spending and staffing data, and to build custom datasets for further analyses.

#### Key Features:

**Dynamic Plot Builder:** Visualise key metrics (FTE, RDEL, Income) using interactive bar charts powered by [ggiraph](https://davidgohel.github.io/ggiraph/).

**Inflation Adjustment:** Integrated toggle to adjust financial data to a 2023/24 baserate using [GDP deflators](https://www.gov.uk/government/statistics/gdp-deflators-at-market-prices-and-money-gdp-march-2025-spring-statement-quarterly-national-accounts), allowing for accurate real-terms comparisons across financial years.

**Custom Dataset Builder:** Users can select specific variables (e.g., "Staffing", "Accountability", "Finance") to generate bespoke tables and export them as .csv or formatted .rtf documents.

**Transparency:** Caveats and notes are dynamically rendered based on user selection to ensure data is interpreted correctly.

#### Technical Stack 🛠️

The application is built using the [R Shiny](http://shiny.posit.co/) framework and leverages modern UI/UX packages for a responsive and accessible tool for exploring UK Public Sector data:

**UI Framework:** [bslib](https://rstudio.github.io/bslib/) (Bootstrap 5) for a responsive and accessible layout.

**Visualisation:** [ggiraph](https://davidgohel.github.io/ggiraph/) for interactive graphics and [gt](https://gt.rstudio.com/) for publication-ready tables.

**Data Handling:** [tidyverse](https://tidyverse.org/) for reactive data wrangling.

#### 🚀 How to Run Locally

To run this application on your own machine:

-   Clone this repository.

-   Open app.R in RStudio.

-   Ensure all dependencies are installed (see the library() calls at the top of the app.R script).

-   Run the application

*Note: The app relies on helper functions stored in the `Assets/` directory. Ensure your working directory is set to the project root so these can be sourced correctly.*
