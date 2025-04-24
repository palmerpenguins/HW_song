library(shiny)
library(bslib)
library(plotly)
library(DT)
library(tidyverse)
library(lubridate)
library(dataRetrieval)
library(broom)

# === Get and Clean Data ===

siteNumber <- "01646500"
startDate <- "2015-01-01"
endDate <- "2024-12-31"

# Discharge
discharge_data <- readNWISdv(siteNumber, "00060", startDate, endDate)

# Temperature
temperature_data <- readNWISdv(siteNumber, "00010", startDate, endDate)

# Clean discharge
flow_data <- discharge_data %>%
  rename(date = Date, discharge_cfs = X_00060_00003) %>%
  mutate(
    year = year(date),
    month = month(date),
    season = case_when(
      month %in% c(12, 1, 2) ~ "Winter",
      month %in% c(3, 4, 5) ~ "Spring",
      month %in% c(6, 7, 8) ~ "Summer",
      month %in% c(9, 10, 11) ~ "Fall"
    )
  ) %>%
  filter(!is.na(discharge_cfs))

# Clean temperature
temperature_data_cleaned <- temperature_data |>
  rename(
    "4_1_ft_C" = "X_4.1.ft.from.riverbed..middle....Discontinued._00010_00003",
    "1_ft_C" = "X_1.0.ft.from.riverbed..bottom....Discontinued._00010_00003",
    "7_1_ft_C" = "X_7.1.ft.from.riverbed..top....Discontinued._00010_00003",
    "old_multiparameter_C" = "X_From.multiparameter.sonde...Discontinued._00010_00003",
    "current_multiparameter_C" = "X_From.multiparameter.sonde_00010_00003",
    "date" = "Date"
  ) |>
  select(date, "4_1_ft_C", "1_ft_C", "7_1_ft_C", "old_multiparameter_C", "current_multiparameter_C")

avg_temp <- transform(temperature_data_cleaned,
                      avg_temp_C = rowMeans(temperature_data_cleaned[,-1], na.rm = TRUE))

temperature_data_final <- avg_temp |>
  select(date, avg_temp_C)

# Combine discharge + temperature
temp_and_discharge <- left_join(temperature_data_final, flow_data, by = "date") %>%
  filter(!is.na(avg_temp_C))

# === UI ===

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "cosmo"),
  titlePanel("🌊 Potomac River Discharge – USGS Data Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("xvar", "X-axis Variable:", choices = c("Year" = "year", "Average Temperature (°C)" = "avg_temp_C")),
      selectInput("yvar", "Y-axis Variable:", choices = c("Discharge (cfs)" = "discharge_cfs")),
      selectInput("seasonFilter", "Filter by Season:", choices = c("All", unique(flow_data$season)), selected = "All"),
      dateRangeInput("dateRange", "Select Date Range:", start = min(flow_data$date), end = max(flow_data$date)),
      checkboxInput("logY", "Log-transform Y-axis", value = FALSE),
      checkboxInput("runAnova", "Show ANOVA Summary", value = TRUE),
      checkboxInput("runRegression", "Show Custom Regression", value = TRUE),
      checkboxInput("runTTest", "Show T-Test (Summer vs Other)", value = TRUE)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Scatterplot (Custom X vs Y)", plotlyOutput("scatterPlot")),
        tabPanel("Discharge by Season (Boxplot)", plotlyOutput("boxPlot")),
        tabPanel("Yearly Trend", plotlyOutput("trendPlot")),
        tabPanel("Simple Regression: Temp vs Discharge", plotlyOutput("tempOnlyPlot")),
        tabPanel("Multiple Regression: Temp + Season", plotlyOutput("multiRegPlot")),
        tabPanel("Data Table", DTOutput("dataTable")),
        tabPanel("Model Summaries",
                 h4("Custom X vs Y Regression"),
                 verbatimTextOutput("modelSummary"),
                 h4("ANOVA: Discharge ~ Season"),
                 verbatimTextOutput("anovaSummary"),
                 h4("T-Test: Summer vs Other Seasons"),
                 verbatimTextOutput("ttestSummary"),
                 h4("Simple Regression: Discharge ~ Temperature"),
                 verbatimTextOutput("tempOnlyModelSummary"),
                 h4("Multiple Regression: Discharge ~ Temperature + Season"),
                 verbatimTextOutput("multiModelSummary")
        )
      )
    )
  )
)

# === SERVER ===

server <- function(input, output, session) {
  
  # Filtered data
  filteredData <- reactive({
    req(input$dateRange)
    df <- temp_and_discharge %>%
      filter(date >= input$dateRange[1] & date <= input$dateRange[2])
    if (input$seasonFilter != "All") {
      df <- df %>% filter(season == input$seasonFilter)
    }
    df
  })
  
  # Scatterplot (Custom X vs Y)
  output$scatterPlot <- renderPlotly({
    df <- filteredData()
    p <- ggplot(df, aes_string(x = input$xvar, y = input$yvar)) +
      geom_point(alpha = 0.5) +
      geom_smooth(method = "lm", se = TRUE, color = "blue") +
      scale_y_continuous(trans = ifelse(input$logY, "log10", "identity")) +
      labs(title = "Scatterplot with Regression Line")
    ggplotly(p)
  })
  
  # Boxplot: Discharge by Season
  output$boxPlot <- renderPlotly({
    df <- filteredData()
    p <- ggplot(df, aes(x = season, y = discharge_cfs)) +
      geom_boxplot(fill = "steelblue") +
      labs(title = "Discharge by Season", x = "Season", y = "Discharge (cfs)")
    ggplotly(p)
  })
  
  # Yearly Trend Plot
  output$trendPlot <- renderPlotly({
    df <- filteredData() %>%
      group_by(year) %>%
      summarize(mean_discharge = mean(discharge_cfs, na.rm = TRUE))
    p <- ggplot(df, aes(x = year, y = mean_discharge)) +
      geom_point() +
      geom_smooth(method = "lm", se = TRUE) +
      labs(title = "Yearly Average Discharge", x = "Year", y = "Mean Discharge (cfs)")
    ggplotly(p)
  })
  
  # Simple Regression: Discharge ~ Temperature
  output$tempOnlyPlot <- renderPlotly({
    df <- filteredData()
    p <- ggplot(df, aes(x = avg_temp_C, y = discharge_cfs)) +
      geom_point(alpha = 0.5) +
      geom_smooth(method = "lm", se = TRUE, color = "blue") +
      labs(title = "Discharge vs Temperature (Simple Regression)",
           x = "Average Temperature (°C)", y = "Discharge (cfs)") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$tempOnlyModelSummary <- renderPrint({
    df <- filteredData()
    model <- lm(discharge_cfs ~ avg_temp_C, data = df)
    summary(model)
  })
  
  # Multiple Regression: Discharge ~ Temp + Season
  output$multiRegPlot <- renderPlotly({
    df <- filteredData()
    p <- ggplot(df, aes(x = avg_temp_C, y = discharge_cfs, color = season)) +
      geom_point(alpha = 0.5) +
      geom_smooth(method = "lm", se = FALSE) +
      labs(title = "Discharge vs Temperature by Season (Multiple Regression)",
           x = "Average Temperature (°C)", y = "Discharge (cfs)") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$multiModelSummary <- renderPrint({
    df <- filteredData()
    model <- lm(discharge_cfs ~ avg_temp_C + season, data = df)
    summary(model)
  })
  
  # ANOVA: Discharge by Season
  output$anovaSummary <- renderPrint({
    req(input$runAnova)
    df <- filteredData()
    model <- aov(discharge_cfs ~ season, data = df)
    summary(model)
  })
  
  # T-Test: Summer vs Other
  output$ttestSummary <- renderPrint({
    req(input$runTTest)
    df <- filteredData() %>%
      mutate(season_group = if_else(season == "Summer", "Summer", "Other"))
    t.test(discharge_cfs ~ season_group, data = df)
  })
  
  # Regression summary for custom x/y
  output$modelSummary <- renderPrint({
    req(input$runRegression)
    df <- filteredData()
    model <- lm(as.formula(paste(input$yvar, "~", input$xvar)), data = df)
    summary(model)
  })
  
  # Data Table
  output$dataTable <- renderDT({
    df <- filteredData()
    datatable(df, options = list(pageLength = 10), filter = "top")
  })
}

# Run the app
shinyApp(ui = ui, server = server)
