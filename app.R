# ========================
# MVAM Data Quality Dashboard
# Full App: UI + Server with Dummy Data
# ========================

# ---- Load Packages ----
library(shiny)
library(bslib)
library(dplyr)
library(plotly)
library(echarts4r)
library(reactable)
library(shinyWidgets)
library(lubridate)
library(tidyr)
library(hms)
library(heatmaply)
# ---- Parameters & Theme ----
wfp_colors <- list(
  blue = "#0072BC",
  light_blue = "#6ECFF6",
  green = "#78BE20",
  orange = "#F58220",
  gray = "#58595B"
)

# ---- Helper Function ----
fmt_team <- function(x) sprintf("%02d", as.integer(x))

# ---- Dummy Data ----
set.seed(123)
dummy_data <- data.frame(
  region = sample(c("Northern", "Upper East", "Ashanti", "Eastern", "Western"), 500, TRUE),
  enumerator = sample(paste0("ENUM", 1:10), 500, TRUE),
  date = sample(seq(Sys.Date() - 30, Sys.Date(), by = "day"), 500, TRUE),
  completed = sample(c(TRUE, FALSE), 500, TRUE, prob = c(0.8, 0.2)),
  start = sample(seq.POSIXt(Sys.time() - 86400*30, Sys.time(), by = "hour"), 500, TRUE),
  end = sample(seq.POSIXt(Sys.time() - 86400*30, Sys.time(), by = "hour"), 500, TRUE),
  hhid = sample(1000:1999, 500, TRUE),
  rm_call_dispo = sample(c(1, 0), 500, TRUE, prob = c(0.8, 0.2)),
  call_back = sample(c(NA, 1), 500, TRUE, prob = c(0.2, 0.8)),
  duration = round(rnorm(500, mean = 45, sd = 10), 1),
  district = sample(paste("District", 1:20), 500, TRUE)
)

# ---- UI ----
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#003087"),
  
  # ---- Header ----
  div(
    class = "header d-flex justify-content-between align-items-center bg-white p-3 mb-3 shadow-sm rounded",
    h1("Data Quality Monitoring Dashboard", style = "color:#003087; font-weight:700; font-size:28px;"),
    div(class = "subtitle text-muted small", textOutput("last_updated"))
  ),
  
  # ---- Layout ----
  sidebarLayout(
    sidebarPanel(
      width = 3,
      style = paste0("background-color: ", wfp_colors$light_blue, "10; border-radius:12px; padding:1rem;"),
      h5("Filters", style = "font-weight:600;"),
      selectInput("region", "Region", choices = c("All"), selected = "All"),
      selectInput("enumerator", "Operator", choices = c("All"), selected = "All"),
      dateRangeInput("date_range", "Interview Date Range", start = Sys.Date() - 30, end = Sys.Date()),
      actionBttn("refresh", "Refresh", style = "fill", color = "primary", block = TRUE),
      hr()
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Overview",
                 fluidRow(
                   column(3,
                          div(class = "card shadow-sm p-3 mb-3 bg-white rounded text-center",
                              h6("Sample frame", class = "text-muted fw-bold"),
                              h3(textOutput("sample_frame"), class = "fw-bold text-primary")
                          )
                   ),
                   column(3,
                          div(class = "card shadow-sm p-3 mb-3 bg-white rounded text-center",
                              h6("Total interviews started", class = "text-muted fw-bold"),
                              h3(textOutput("total_ints"), class = "fw-bold text-primary")
                          )
                   ),
                   column(3,
                          div(class = "card shadow-sm p-3 mb-3 bg-white rounded text-center",
                              h6("Expected interviews", class = "text-muted fw-bold"),
                              h3(textOutput("expected_ints"), class = "fw-bold text-primary")
                          )
                   ),
                   column(3,
                          div(class = "card shadow-sm p-3 mb-3 bg-white rounded text-center",
                              h6("Completed interviews", class = "text-muted fw-bold"),
                              h3(textOutput("completed_ints"), class = "fw-bold text-primary")
                          )
                   ),
                   column(3,
                          div(class = "card shadow-sm p-3 mb-3 bg-white rounded text-center",
                              h6("Completion rate", class = "text-muted fw-bold"),
                              h3(textOutput("comp_rate"), class = "fw-bold text-success")
                          )
                   ),
                   column(3,
                          div(class = "card shadow-sm p-3 mb-3 bg-white rounded text-center",
                              h6("Regions in data", class = "text-muted fw-bold"),
                              h3(textOutput("total_regions"), class = "fw-bold text-primary")
                          )
                   )
                 ),
                 hr(),
        layout_column_wrap(
          widths = 1/2,
          card(
            full_screen = TRUE,
            card_header("Completed Surveys", class = "text-success", style = "text-align: left;"),  # Added left alignment
            card_body_fill(echarts4rOutput("completion_by_region"))
          ),
          card(
            full_screen = TRUE,
            card_header("Completed Surveys per Day", class = "text-success", style = "text-align: left;"),  # Added left alignment
            card_body_fill(plotlyOutput("ints_by_day"))
          )
        )
      ),
        
        tabPanel("Operators Check",
                 fluidRow(
                   column(
                     3,
                     div(
                       class = "card shadow-sm p-3 mb-3 bg-white rounded text-center",
                       h6("Operators in Data", class = "text-muted fw-bold"),
                       h2(textOutput("operators_cov"), class = "fw-bold text-primary")
                     )
                   ),
                   column(
                     3,
                     div(
                       class = "card shadow-sm p-3 mb-3 bg-white rounded text-center",
                       h6("Median Duration (mins)", class = "text-muted fw-bold"),
                       h2(textOutput("med_dur"), class = "fw-bold text-success")
                     )
                   )
                 ),
                 hr(),
                 # =========================
                 # Operator Performance
                 # =========================
                 layout_column_wrap(
                   widths = 1/2,
                   card(
                     full_screen = TRUE,
                     card_header("Completed Surveys by Operator", class = "fw-bold text-success", style = "text-align:left;"),
                     card_body_fill(echarts4rOutput("completion_by_region_operator", height = "400px"))
                   ),
                   card(
                     full_screen = TRUE,
                     card_header("Completed Surveys by Operator per Day", class = "fw-bold text-success", style = "text-align:left;"),
                     card_body_fill(plotlyOutput("ints_by_day_operator", height = "400px"))
                   )
                 ),
                 br(),
                 div(class = "card shadow-sm p-3 mb-3 bg-white rounded",
                     h6("Interview Duration by Operator", class = "fw-bold text-success"),
                     reactableOutput("data_table")
                 )
        ),
        
        tabPanel("Interview Oddities",
                 tags$style(HTML("
           .radio label { text-align: left !important; display: block; }
           .radio { margin-left: 0; }
         ")),
                 div(class = "card shadow-sm p-3 mb-3 bg-white rounded",
                     h6("Please select the type of oddities", style = "text-align:left;"),
                     radioButtons(
                       inputId = "statistic",
                       label = NULL,
                       choices = c("Interviewers with Wrong Dates" = "logical1",
                                   "Interviews at Odd Times" = "gps1",
                                   "Household Gaps" = "logical2",
                                   "Duplicates" = "gps2",
                                   "Number of days since last data" = "log2",
                                   "Itinerary progress by Operator" = "gs2"),
                       selected = "logical1"
                     )
                 ),
                 uiOutput("oddities_content")
        ),
        
        tabPanel("Preliminary Results",
                 div(class = "card shadow-sm p-3 mb-3 bg-white rounded",
                     h6("Preliminary Results", class = "fw-bold"),
                     uiOutput("pre_result")
                 )
        )
      )
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  
  # --- Update Timestamp ---
  output$last_updated <- renderText({
    paste("Last updated on:", format(Sys.time(), "%B %d, %Y %H:%M"))
  })
  
  # --- Update Select Inputs ---
  observe({
    updateSelectInput(session, "region", choices = c("All", unique(dummy_data$region)))
    updateSelectInput(session, "enumerator", choices = c("All", unique(dummy_data$enumerator)))
  })
  
  # --- Reactive Filtered Data ---
  filtered_data <- reactive({
    df <- dummy_data
    if (input$region != "All") df <- df %>% filter(region == input$region)
    if (input$enumerator != "All") df <- df %>% filter(enumerator == input$enumerator)
    df <- df %>% filter(date >= input$date_range[1] & date <= input$date_range[2])
    df
  })
  
  # --- Overview KPIs ---
  output$sample_frame <- renderText({ "500" })
  output$total_ints <- renderText({ nrow(filtered_data()) })
  output$expected_ints <- renderText({ 550 })
  output$completed_ints <- renderText({ sum(filtered_data()$completed) })
  output$comp_rate <- renderText({
    round(sum(filtered_data()$completed)/nrow(filtered_data())*100,1) %>% paste0("%")
  })
  output$total_regions <- renderText({ length(unique(filtered_data()$region)) })
  
  # --- Overview Charts ---
  output$completion_by_region <- renderEcharts4r({
    filtered_data() %>%
      group_by(region) %>%
      summarise(Completed = sum(completed), .groups = "drop") %>%
      arrange(Completed) %>%  # sort descending
      e_charts(region) %>%
      e_bar(Completed, color = wfp_colors$green) %>%
      e_flip_coords() %>%   # horizontal bars
      e_tooltip(trigger = "item")
  })
  
  
  
  output$ints_by_day <- renderPlotly({
    df <- filtered_data()
    req(df)
    
    df_summary <- df %>%
      filter(rm_call_dispo == 1) %>% 
      filter(!is.na(call_back)) %>% 
      count(region, date) %>%
      mutate(date = as.Date(date)) %>%   # keep as Date for ordering
      arrange(date) %>%
      pivot_wider(
        names_from = date,
        values_from = n,
        values_fill = 0
      ) %>%
      as.data.frame()
    
    rownames(df_summary) <- df_summary$region
    df_summary <- df_summary[, -1]
    
    # Format column names for display
    colnames(df_summary) <- format(as.Date(colnames(df_summary)), "%b\n%d")
    
    wfp_colors <- c("#E6F2FA", "#6ECFF6", "#0072BC", "#003366")
    
    heatmaply(
      df_summary,
      dendrogram = "none",
      plot_method = "plotly",
      draw_cellnote = TRUE,
      colors = colorRampPalette(wfp_colors)(256),
      column_text_angle = 0,
      colorbar_len = 0.95,
      na.rm = FALSE,
      ylab = "Region",
      xlab = "Date",
      limits = c(0, max(df_summary, na.rm = TRUE))
    ) %>%
      layout(
        xaxis = list(
          tickangle = 0,
          tickfont = list(size = 10),
          ticklabelposition = "outside bottom",
          ticklabelmode = "instant"
        )
      )
  })
  
  
  # --- Operators Check ---
  output$operators_cov <- renderText({ length(unique(filtered_data()$enumerator)) })
  output$med_dur <- renderText({ round(median(filtered_data()$duration), 1) })
  
  output$completion_by_region_operator <- renderEcharts4r({
    filtered_data() %>%
      group_by(enumerator) %>%
      summarise(Completed = sum(completed)) %>%
      arrange(Completed)%>%  # sort descending
      e_charts(enumerator) %>%
      e_bar(Completed, color = wfp_colors$orange) %>%
      e_flip_coords() %>%   # this makes the bars horizontal
      e_tooltip(trigger = "item")
  })
  
  output$ints_by_day_operator <- renderPlotly({
    df <- filtered_data()
    req(df)
    
    df_summary <- df %>%
      filter(rm_call_dispo == 1) %>%  
      filter(!is.na(call_back)) %>% 
      count(enumerator, date) %>%
      mutate(date = as.Date(date)) %>%   # keep as Date
      arrange(date) %>%
      pivot_wider(
        names_from = date,
        values_from = n,
        values_fill = 0
      ) %>%
      as.data.frame()
    
    rownames(df_summary) <- df_summary$enumerator
    df_summary <- df_summary[, -1]
    
    # Convert column names to formatted labels for display
    colnames(df_summary) <- format(as.Date(colnames(df_summary)), "%b\n%d")
    
    wfp_colors <- c("#E6F2FA", "#6ECFF6", "#0072BC", "#003366")
    
    heatmaply(
      df_summary,
      dendrogram = "none",
      plot_method = "plotly",
      draw_cellnote = TRUE,
      colors = colorRampPalette(wfp_colors)(256),
      column_text_angle = 0,
      colorbar_len = 0.95,
      na.rm = FALSE,
      ylab = "Operator",
      xlab = "Date",
      limits = c(0, max(df_summary, na.rm = TRUE))
    ) %>%
      layout(
        xaxis = list(
          tickangle = 0,
          tickfont = list(size = 10),
          ticklabelposition = "outside bottom",
          ticklabelmode = "instant"
        )
      )
  })
  
  
  output$data_table <- renderReactable({
    filtered_data() %>%
      group_by(enumerator) %>%
      summarise(Interviews = n(), Completed = sum(completed), Median_Duration = median(duration)) %>%
      reactable(bordered = TRUE, highlight = TRUE, striped = TRUE, defaultPageSize = 8)
  })
  
  # --- Interview Oddities Dynamic UI ---
  output$oddities_content <- renderUI({
    indicator_title <- switch(input$statistic,
                              "logical1" = "Interviewers with Wrong Dates",
                              "logical2" = "Household Gaps",
                              "gps1" = "Interviews at Odd Times",
                              "gps2" = "Duplicates",
                              "log2" = "Number of days since last data",
                              "gs2" = "Itinerary progress by Operator")
    if (input$statistic %in% c("logical1", "logical2", "gps1", "gps2", "log2")) {
      tagList(
        h4(indicator_title, style = "color: #003087; font-weight: 600; text-align: left;"),
        reactableOutput("logical_checks")
      )
    } else if (input$statistic == "gs2") {
      tagList(
        h4(indicator_title, style = "color: #003087; font-weight: 600; text-align: left;"),
        plotlyOutput("itinerary", height = "400px")
      )
    }
  })
  
  # --- Logical Checks Table ---
  output$logical_checks <- renderReactable({
    data <- switch(input$statistic,
                   "logical1" = filtered_data() %>%
                     select(region, enumerator, start, end) %>%
                     mutate(start = lubridate::ymd_hms(start),
                            end = lubridate::ymd_hms(end)) %>%
                     filter(as.Date(start) > Sys.Date() | as.Date(start) < Sys.Date() - 30 |
                              as.Date(end) > Sys.Date() | as.Date(end) < Sys.Date() - 30),
                   
                   "logical2" = filtered_data() %>%
                     select(enumerator, hhid, rm_call_dispo, call_back) %>%
                     group_by(enumerator) %>%
                     summarise(n_hh_int_started = n_distinct(hhid),
                               completed_hh = sum(rm_call_dispo == 1 & !is.na(call_back), na.rm = TRUE),
                               .groups = "drop") %>%
                     mutate(expected_household = 150,
                            gap = expected_household - completed_hh) %>%
                     select(enumerator, expected_household, n_hh_int_started, completed_hh, gap),
                   
                   "gps1" = filtered_data() %>%
                     mutate(start = lubridate::ymd_hms(as.character(start)),
                            start_time = hms::as_hms(start)) %>%
                     filter(start_time < hms::as_hms("05:00:00") | start_time > hms::as_hms("22:00:00")) %>%
                     select(region, enumerator, start_time),
                   
                   "gps2" = filtered_data() %>%
                     filter(rm_call_dispo == 1 & !is.na(call_back)) %>%
                     select(region, enumerator, hhid) %>%
                     count(region, enumerator, hhid) %>% filter(n > 1),
                   
                   "log2" = filtered_data() %>%
                     select(region, enumerator, start, end) %>%
                     mutate(start_dt = as.POSIXct(start), end_dt = as.POSIXct(end),
                            last_activity = pmax(start_dt, end_dt, na.rm = TRUE)) %>%
                     group_by(region, enumerator) %>%
                     summarise(last_sync = max(last_activity, na.rm = TRUE),
                               days_since_last_data = round(as.numeric(difftime(Sys.Date(), as.Date(last_sync), units = "days"))),
                               .groups = "drop"),
                   
                   data.frame(Check = c("Check 1", "Check 2"), Status = c("Pass", "Fail"))
    )
    reactable(data)
  })
  
  # --- Itinerary Progress Heatmap ---
  output$itinerary <- renderPlotly({
    heatmap_data <- filtered_data() %>%
      group_by(enumerator, district) %>%
      summarise(visits = n(), .groups = "drop") %>%
      tidyr::pivot_wider(names_from = district, values_from = visits, values_fill = 0)
    
    plot_ly(
      x = colnames(heatmap_data)[-1],
      y = heatmap_data$enumerator,
      z = as.matrix(heatmap_data[,-1]),
      type = "heatmap",
      colorscale = list(c(0,1), c("lightblue","darkblue")),
      reversescale = FALSE
    ) %>%
      layout(xaxis = list(title = "District"),
             yaxis = list(title = "Operator"))
  })
  
  # --- Preliminary Results Dummy ---
  output$pre_result <- renderUI({
    tagList(
      h5("Dummy Preliminary Results Table"),
      reactable::reactable(
        data.frame(
          Indicator = c("FCS", "LCSI", "rCSI"),
          Value = c(65, 72, 58)
        ),
        bordered = TRUE,
        highlight = TRUE,
        striped = TRUE
      )
    )
  })
}

# ---- Run App ----
shinyApp(ui, server)
