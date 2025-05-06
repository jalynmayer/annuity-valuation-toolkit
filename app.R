# packages
if (!requireNamespace("shiny", quietly = TRUE))   install.packages("shiny")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")

library(shiny)
library(ggplot2)

# functions
annuity_cashflows <- function(type = c("Level", "Increasing", "Perpetuity"),
                             payment, n, i, timing = c("immediate", "due"),
                             step = 0) {
  type   <- match.arg(type)
  timing <- match.arg(timing)

  # payment times: t=1..n (annuity immediate)  or  t=0..n-1 (annuity due)
  t_vec <- if (timing == "immediate") 1:n else 0:(n - 1L)

  if (type == "Perpetuity") {
    
    n     <- 200L
    t_vec <- if (timing == "immediate") 1:n else 0:(n - 1L)
  }

  cf <- switch(type,
               Level       = rep(payment, length(t_vec)),
               Increasing  = payment + step * (seq_along(t_vec) - 1L),
               Perpetuity  = rep(payment, length(t_vec)))

  data.frame(t = t_vec, cf = cf)
}

pv_generic <- function(cf_tbl, i) {
  with(cf_tbl, sum(cf / (1 + i)^t))
}

fv_generic <- function(cf_tbl, i, horizon_n) {
  with(cf_tbl, sum(cf * (1 + i)^(horizon_n - t)))
}

duration_macaulay <- function(cf_tbl, i, PV) {
  with(cf_tbl, sum(t * cf / (1 + i)^t)) / PV
}

convexity <- function(cf_tbl, i, PV) {
  with(cf_tbl, sum(t * (t + 1) * cf / (1 + i)^(t + 2))) / PV
}

# ui
ui <- fluidPage(
  titlePanel("Annuity Valuation Toolkit"),
  sidebarLayout(
    sidebarPanel(
      selectInput("annuity_type", "Annuity type:",
                  choices = c("Level", "Increasing", "Perpetuity")),
      selectInput("timing", "Payment timing:",
                  choices = c("Immediate (end of period)" = "immediate",
                              "Due (beginning of period)" = "due")),
      numericInput("payment", "First payment amount (P):", value = 100, min = 0, step = 1),
      conditionalPanel(
        "input.annuity_type == 'Increasing'",
        numericInput("step", "Increment per payment (ΔP):", value = 10, min = 0, step = 1)
      ),
      conditionalPanel(
        "input.annuity_type != 'Perpetuity'",
        numericInput("n", "Number of payments (n):", value = 20, min = 1, step = 1)
      ),
      sliderInput("i", "Effective annual interest rate (i):",
                  min = 0.001, max = 0.15, value = 0.05, step = 0.001),
      actionButton("go", "Calculate")
    ),
    mainPanel(
      fluidRow(
        column(4, verbatimTextOutput("pv_txt")),
        column(4, verbatimTextOutput("fv_txt")),
        column(4, verbatimTextOutput("dur_conv_txt"))
      ),
      plotOutput("priceCurve", height = "400px")
    )
  )
)

# server
server <- function(input, output, session) {

  vals <- eventReactive(input$go, {
    req(input$payment, input$i)
    n_input <- if (input$annuity_type == "Perpetuity") 200L else input$n

    cf_tbl <- annuity_cashflows(type   = input$annuity_type,
                                payment= input$payment,
                                n      = n_input,
                                i      = input$i,
                                timing = input$timing,
                                step   = ifelse(is.null(input$step), 0, input$step))

    PV  <- pv_generic(cf_tbl, input$i)
    FV  <- if (input$annuity_type == "Perpetuity") NA else fv_generic(cf_tbl, input$i, input$n)
    dur <- duration_macaulay(cf_tbl, input$i, PV)
    conv<- convexity(cf_tbl, input$i, PV)

    list(PV = PV, FV = FV, dur = dur, conv = conv, cf = cf_tbl)
  })

  output$pv_txt <- renderText({
    v <- vals(); paste0("Present Value: $", formatC(v$PV, format = "f", digits = 2))
  })

  output$fv_txt <- renderText({
    v <- vals()
    if (is.na(v$FV)) "Future Value: — (Perpetuity)" else
      paste0("Future Value: $", formatC(v$FV, format = "f", digits = 2))
  })

  output$dur_conv_txt <- renderText({
    v <- vals(); paste0("Duration: ", round(v$dur, 2), " yrs\nConvexity: ", round(v$conv, 2))
  })

  output$priceCurve <- renderPlot({
    v <- vals()
    i_seq <- seq(0.005, 0.15, length.out = 200)
    prices <- sapply(i_seq, function(r) pv_generic(v$cf, r))
    ggplot(data.frame(i = i_seq, Price = prices), aes(i, Price)) +
      geom_line() +
      geom_point(aes(x = input$i, y = v$PV), size = 3) +
      labs(x = "Yield (i)", y = "Present Value", title = "Price–Yield Curve") +
      theme_minimal()
  })
}

#run 
shinyApp(ui, server)

