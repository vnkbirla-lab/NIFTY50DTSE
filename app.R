# app.R
# NIFTY 50 stock viewer with dual-axis "Stock vs Nifty" tab
# + auto-install of dependencies
# + README.md generation, preview, and download
# ------------------------------------------------------------

# -------- [1] Minimal package bootstrap --------
req_pkgs <- c("shiny", "quantmod", "xts", "DT", "ggplot2")
missing  <- req_pkgs[!vapply(req_pkgs, function(p) requireNamespace(p, quietly = TRUE), logical(1))]

if (length(missing)) {
  # Set a CRAN mirror if none present (use Posit CRAN by default)
  if (is.null(getOption("repos")) || getOption("repos")["CRAN"] %in% c("@CRAN@", NULL, "")) {
    options(repos = c(CRAN = "https://cloud.r-project.org"))
  }
  install.packages(missing, dependencies = TRUE)
}

# Now load (quietly)
suppressPackageStartupMessages({
  library(shiny)
  library(quantmod)
  library(xts)
  library(DT)
  library(ggplot2)
})

# -------- Helper: README generator --------
generate_readme <- function() {
  glue <- function(...) paste0(..., collapse = "")
  date_str <- as.character(Sys.Date())
  glue(
    "# NIFTY 50 Shiny App (quantmod)\n\n",
    "**Last generated:** ", date_str, "\n\n",
    "An R Shiny app to fetch and visualize **historical** stock prices for NIFTY 50 companies and the **NIFTY 50 index** from Yahoo Finance via `{quantmod}`.\n\n",
    "## Features\n",
    "- Company picker (NSE Yahoo tickers with `.NS`)\n",
    "- Date range selection\n",
    "- **Price (Candles)** tab with optional SMA(20/50) and EMA(20)\n",
    "- **Stock vs Nifty** tab with **dual Y-axes**: left = NIFTY 50 index, right = focal stock (blue)\n",
    "- **Data** tab with recent OHLCV (last 100 rows) and CSV download\n",
    "- Auto-installs required packages on first run\n\n",
    "## Data Source & Notes\n",
    "- Data pulled from **Yahoo Finance** via `{quantmod}` (`getSymbols()`), which is typically **delayed**.\n",
    "- For true real-time streaming or commercial use, swap in a licensed market data provider.\n\n",
    "## How to Run\n",
    "```r\n",
    "shiny::runApp()  # from the folder containing app.R\n",
    "```\n\n",
    "## Dependencies\n",
    "`shiny`, `quantmod`, `xts`, `DT`, `ggplot2`. The app auto-checks and installs missing packages.\n\n",
    "## Dual-Axis Method\n",
    "The 'Stock vs Nifty' plot overlays two lines using a **linear transformation** so both series share the canvas. The **right axis** shows the focal stock's **true price scale** via inverse transform.\n\n",
    "## License & Disclaimer\n",
    "This app is provided as-is for educational/analytical purposes. Verify data before making decisions.\n"
  )
}

# Write (or overwrite) README.md at launch so it's present for GitHub
try({
  writeLines(generate_readme(), con = "README.md", useBytes = TRUE)
}, silent = TRUE)

# -------- NIFTY 50: simple mapping (edit over time as membership changes) --------
nifty50 <- c(
  "RELIANCE"              = "RELIANCE.NS",
  "TCS"                   = "TCS.NS",
  "Infosys"               = "INFY.NS",
  "HDFC Bank"             = "HDFCBANK.NS",
  "ICICI Bank"            = "ICICIBANK.NS",
  "ITC"                   = "ITC.NS",
  "Bharti Airtel"         = "BHARTIARTL.NS",
  "Hindustan Unilever"    = "HINDUNILVR.NS",
  "Larsen & Toubro"       = "LT.NS",
  "State Bank of India"   = "SBIN.NS",
  "Kotak Mahindra Bank"   = "KOTAKBANK.NS",
  "Axis Bank"             = "AXISBANK.NS",
  "Bajaj Finance"         = "BAJFINANCE.NS",
  "Asian Paints"          = "ASIANPAINT.NS",
  "Maruti Suzuki"         = "MARUTI.NS",
  "Sun Pharma"            = "SUNPHARMA.NS",
  "Titan"                 = "TITAN.NS",
  "NTPC"                  = "NTPC.NS",
  "ONGC"                  = "ONGC.NS",
  "UltraTech Cement"      = "ULTRACEMCO.NS",
  "Nestlé India"          = "NESTLEIND.NS",
  "Mahindra & Mahindra"   = "M&M.NS",
  "Power Grid"            = "POWERGRID.NS",
  "Tata Steel"            = "TATASTEEL.NS",
  "Coal India"            = "COALINDIA.NS",
  "HCL Tech"              = "HCLTECH.NS",
  "Tata Motors"           = "TATAMOTORS.NS",
  "Wipro"                 = "WIPRO.NS",
  "Adani Enterprises"     = "ADANIENT.NS",
  "Adani Ports"           = "ADANIPORTS.NS",
  "JSW Steel"             = "JSWSTEEL.NS",
  "Bajaj Finserv"         = "BAJAJFINSV.NS",
  "HDFC Life"             = "HDFCLIFE.NS",
  "Tech Mahindra"         = "TECHM.NS",
  "Grasim"                = "GRASIM.NS",
  "Cipla"                 = "CIPLA.NS",
  "LTIMindtree"           = "LTIM.NS",
  "Britannia"             = "BRITANNIA.NS",
  "Dr. Reddy's"           = "DRREDDY.NS",
  "Eicher Motors"         = "EICHERMOT.NS",
  "Hero MotoCorp"         = "HEROMOTOCO.NS",
  "Apollo Hospitals"      = "APOLLOHOSP.NS",
  "Tata Consumer"         = "TATACONSUM.NS",
  "BPCL"                  = "BPCL.NS",
  "Divi's Labs"           = "DIVISLAB.NS",
  "Shree Cement"          = "SHREECEM.NS",
  "Hindalco"              = "HINDALCO.NS",
  "SBI Life"              = "SBILIFE.NS",
  "ICICI Prudential"      = "ICICIPRULI.NS"
)

# -------- UI --------
ui <- fluidPage(
  tags$head(tags$style(HTML("
    .metric-box { display:flex; gap:24px; flex-wrap:wrap; margin-bottom:12px; }
    .metric {
      border:1px solid #444; border-radius:10px; padding:10px 14px;
      min-width:180px; background:#1a1a1a; color:#eaeaea;
    }
    .metric .label { font-size:12px; opacity:0.8; }
    .metric .value { font-size:20px; font-weight:600; margin-top:4px; }
    .good { color:#33c24d; } .bad { color:#ff5a5f; }
    .shiny-download-link { margin-top: 8px; }
    .codebox { white-space: pre-wrap; font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace; }
  "))),
  titlePanel("NIFTY 50 — Stock Viewer with Index Comparison"),
  sidebarLayout(
    sidebarPanel(
      selectInput("ticker", "Select Company", choices = nifty50, selected = "RELIANCE.NS"),
      dateRangeInput("dates", "Date Range",
                     start = Sys.Date() - 365, end = Sys.Date(),
                     min = "2000-01-01", max = Sys.Date()),
      checkboxInput("add_sma", "Add SMA (20, 50)", value = TRUE),
      checkboxInput("add_ema", "Add EMA (20)", value = FALSE),
      actionButton("go", "Fetch / Refresh", class = "btn-primary"),
      hr(),
      helpText("Yahoo via quantmod is typically delayed. For true real-time, plug in a paid market data feed.")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Price (Candles)",
          plotOutput("price_plot", height = "520px")
        ),
        tabPanel(
          "Stock vs Nifty",
          br(),
          helpText("Left axis: NIFTY 50 (^NSEI). Right axis (blue): selected stock price."),
          plotOutput("dual_plot", height = "520px")
        ),
        tabPanel(
          "Data",
          fluidRow(
            column(6, h4("Recent OHLCV (last 100 rows)")),
            column(6, align = "right", downloadButton("dl_csv", "Download CSV"))
          ),
          DTOutput("table")
        ),
        tabPanel(
          "README",
          h4("README.md (auto-generated)"),
          div(class = "codebox", verbatimTextOutput("readme_text")),
          downloadButton("dl_readme", "Download README.md")
        )
      )
    )
  )
)

# -------- Server --------
server <- function(input, output, session) {
  
  # Historical data fetch (button-driven)
  histData <- eventReactive(input$go, {
    validate(
      need(!is.null(input$dates[1]) && !is.null(input$dates[2]), "Please pick a start and end date.")
    )
    withProgress(message = "Fetching Yahoo data…", value = 0, {
      incProgress(0.2)
      sym  <- input$ticker
      from <- as.Date(input$dates[1])
      to   <- as.Date(input$dates[2])
      
      stock_xts <- tryCatch(
        getSymbols(sym, src = "yahoo", from = from, to = to, auto.assign = FALSE),
        error = function(e) {
          showNotification(paste("Error fetching stock:", e$message), type = "error")
          return(NULL)
        }
      )
      incProgress(0.5)
      nse_xts <- tryCatch(
        getSymbols("^NSEI", src = "yahoo", from = from, to = to, auto.assign = FALSE),
        error = function(e) {
          showNotification(paste("Error fetching NIFTY:", e$message), type = "error")
          return(NULL)
        }
      )
      incProgress(0.9)
      list(stock = stock_xts, index = nse_xts)
    })
  }, ignoreInit = TRUE)
  
  # --- Candlestick plot (quantmod) ---
  output$price_plot <- renderPlot({
    d <- histData()
    req(d, d$stock)
    x <- d$stock
    chart_Series(x, name = paste0(input$ticker, "  (", input$dates[1], " → ", input$dates[2], ")"))
    
    if (isTRUE(input$add_sma)) {
      add_TA(SMA(Cl(x), n = 20), on = 1, col = "steelblue")
      add_TA(SMA(Cl(x), n = 50), on = 1, col = "darkorange")
    }
    if (isTRUE(input$add_ema)) {
      add_TA(EMA(Cl(x), n = 20), on = 1, col = "forestgreen")
    }
  })
  
  # --- Dual-axis line plot: Stock (right, blue) vs NIFTY (left) ---
  output$dual_plot <- renderPlot({
    d <- histData()
    req(d, d$stock, d$index)
    
    st_close <- Cl(d$stock)
    in_close <- Cl(d$index)
    
    merged <- na.omit(merge(in_close, st_close))
    req(nrow(merged) > 1)
    
    df <- data.frame(
      date  = index(merged),
      nifty = as.numeric(merged[, 1]),
      stock = as.numeric(merged[, 2])
    )
    
    smin <- min(df$stock, na.rm = TRUE)
    smax <- max(df$stock, na.rm = TRUE)
    nmin <- min(df$nifty, na.rm = TRUE)
    nmax <- max(df$nifty, na.rm = TRUE)
    
    if (isTRUE(all.equal(smin, smax))) {
      b <- 1
      a <- nmin - b * smin
    } else {
      b <- (nmax - nmin) / (smax - smin)
      a <- nmin - b * smin
    }
    
    df$stock_scaled <- a + b * df$stock
    
    ggplot(df, aes(x = date)) +
      geom_line(aes(y = nifty, color = "NIFTY 50"), linewidth = 0.8) +
      geom_line(aes(y = stock_scaled, color = "Stock"), linewidth = 0.8) +
      scale_color_manual(
        name = NULL,
        values = c("NIFTY 50" = "gray20", "Stock" = "blue")
      ) +
      scale_y_continuous(
        name = "NIFTY 50 (^NSEI)",
        sec.axis = sec_axis(~ (.-a) / b, name = paste0(names(nifty50)[nifty50 == input$ticker], " (", input$ticker, ")"))
      ) +
      labs(
        title = "Stock vs NIFTY (dual axes)",
        subtitle = paste(format(input$dates[1]), "to", format(input$dates[2])),
        x = NULL
      ) +
      theme_minimal(base_size = 12) +
      theme(
        legend.position = "top",
        panel.grid.minor = element_blank()
      )
  })
  
  # --- Data table + download ---
  output$table <- renderDT({
    d <- histData()
    req(d, d$stock)
    x <- d$stock
    df <- data.frame(date = index(x), coredata(x), row.names = NULL, check.names = FALSE)
    if (nrow(df) > 100) df <- tail(df, 100)
    datatable(
      df,
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  output$dl_csv <- downloadHandler(
    filename = function() {
      paste0(gsub("\\.NS$", "", input$ticker), "_", input$dates[1], "_", input$dates[2], ".csv")
    },
    content = function(file) {
      d <- histData()
      req(d, d$stock)
      x <- d$stock
      df <- data.frame(date = index(x), coredata(x), row.names = NULL, check.names = FALSE)
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  # -------- [2] README preview + download --------
  output$readme_text <- renderText({
    generate_readme()
  })
  
  output$dl_readme <- downloadHandler(
    filename = function() "README.md",
    content  = function(file) {
      writeLines(generate_readme(), con = file, useBytes = TRUE)
    }
  )
  
  # Initialize with a first fetch so plots aren't empty
  observeEvent(TRUE, {
    if (is.null(isolate(histData()))) {
      isolate(updateDateRangeInput(session, "dates", start = Sys.Date() - 365, end = Sys.Date()))
      # Simulate a click
      session$sendCustomMessage("click", "go")
    }
  }, once = TRUE)
}

# ---- Run app ----
shinyApp(ui, server)
