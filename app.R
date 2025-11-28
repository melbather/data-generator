# Read in files
source("global.R")

ui <- fluidPage(
  tags$head(
    includeCSS("www/style.css")
  ),
  titlePanel("Fake Data Generator"),
  sidebarLayout(
    sidebarPanel(
      numericInput("num_rows", 
                    "Number of rows",
                    min = 1, 
                    max = MAX_ROWS, 
                    step = 1, 
                    value = 100),
      actionButton("add_col", "Add new column"),
),
    mainPanel(
      div(id = "columns_container")
    )
)
)

server <- function(input, output) {
  col_counter <- reactiveVal(0)

  observeEvent(input$add_col, {
    n <- col_counter()

    # prevent more than MAX_COLS
    if (n >= MAX_COLS) {
      showNotification("Maximum columns reached", type = "warning")
      return()
    }

    n <- n + 1
    col_counter(n)

    # create UI block
    insertUI(
      selector = "#columns_container",
      where = "beforeEnd",
      ui = div(
        id = paste0("col_block_", n),
        style = "border:1px solid #ccc; padding:10px; margin-top:10px;",
        h4(paste("Column", n)),

        textInput(paste0("col_name", n), "Column name"),

        selectInput(
          paste0("dtype_", n), "Data type",
          choices = c("Float", "Integer", "Categorical")
        ),

        uiOutput(paste0("options_", n)),

        actionButton(paste0("remove_", n), "Remove this column")
      )
    )

    # Capture this block's index in a local environment
    local({
      i <- n

      # When data type changes
      observeEvent(input[[paste0("dtype_", i)]], {
        dtype <- input[[paste0("dtype_", i)]]

        output[[paste0("options_", i)]] <- renderUI({
          if (is.null(dtype)) return(NULL)

          if (dtype == "Float") {
            tagList(
              selectInput(
                paste0("dist_", i), "Distribution",
                choices = c("Normal", "Uniform", "Exponential")
              ),
              uiOutput(paste0("dist_params_", i))
            )

          } else if (dtype == "Integer") {
            tagList(
              selectInput(
                paste0("dist_", i), "Distribution",
                choices = c("Poisson", "Zero-inflated Poisson", "Binomial")
              ),
              uiOutput(paste0("dist_params_", i))
            )

          } else if (dtype == "Categorical") {
            textInput(
              paste0("cat_values_", i),
              "Categories (comma-separated)",
              "A,B,C"
            )
          }
        })
      })

      # When distribution changes
      observeEvent(input[[paste0("dist_", i)]], {
        dist <- input[[paste0("dist_", i)]]

        output[[paste0("dist_params_", i)]] <- renderUI({
          if (is.null(dist)) return(NULL)

          switch(dist,
            "Normal" = tagList(
              numericInput(paste0("mean_", i), "Mean", 0),
              numericInput(paste0("sd_", i),   "Std Dev", 1)
            ),
            "Uniform" = tagList(
              numericInput(paste0("min_", i), "Min", 0),
              numericInput(paste0("max_", i), "Max", 1)
            ),
            "Exponential" = numericInput(
              paste0("rate_", i), "Rate", 1
            ),
            "Poisson" = numericInput(
              paste0("lambda_", i), "Lambda", 1
            ),
            "Zero-inflated Poisson" = tagList(
              numericInput(paste0("mu_", i), "Mu", 1),
              numericInput(paste0("pi_", i), "Pi (zero-inflation prob)", 0.5)
            ),
            "Binomial" = tagList(
              numericInput(paste0("n_", i), "n", 10),
              numericInput(paste0("p_", i), "p", 0.5)
            )
          )
        })
      })

    }) # end local()

    # remove handler
    observeEvent(input[[paste0("remove_", n)]], {
      removeUI(selector = paste0("#col_block_", n))
    })

  }) # end observeEvent(input$add_col)

} # end server

  shinyApp(ui = ui, server = server)