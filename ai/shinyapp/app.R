
library(shiny)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(ggplot2)
library(plotly)
library(tidyverse)
library(igraph)
library(ggraph)
library(tidygraph)
library(visNetwork)
library(scales)
library(viridisLite)
library(echarts4r)
library(BradleyTerry2)
library(bslib)
library(tibble)
library(htmlwidgets)
library(DT)
library(rlang)

# load data
data <- readRDS("data.rds")
data <- data %>%
  mutate(
    # Convert weird values to logicals / numeric flags
    cw_flag   = creative_writing_v0.1_creative_writing %in% c(TRUE, "TRUE", 1),
    cw_score  = suppressWarnings(as.numeric(creative_writing_v0.1_score)),
    
    math_flag = math_v0.1_math %in% c(TRUE, "TRUE", 1),
    
    code_flag = is_code %in% c(TRUE, "TRUE", 1),
    
    if_flag   = if_v0.1_if %in% c(TRUE, "TRUE", 1),
    if_score  = suppressWarnings(as.numeric(if_v0.1_score)),
    
    # Build category
    category = case_when(
      code_flag ~ "code",
      math_flag ~ "math",
      if_flag | if_score > 0 ~ "reasoning",
      cw_flag | cw_score > 0 ~ "creative_writing",
      TRUE ~ "other"
    )
  )

stopifnot(all(c("model_a","model_b","winner","category") %in% names(data)))

make_df2_leader <- function(df) {
  df %>%
    mutate(
      a_win = as.integer(winner == "model_a"),
      b_win = as.integer(winner == "model_b"),
      a_tie = as.integer(winner == "tie"),
      b_tie = a_tie
    ) %>%
    pivot_longer(c(model_a, model_b), names_to = "side", values_to = "model") %>%
    mutate(
      win = if_else(side == "model_a", a_win, b_win),
      tie = if_else(side == "model_a", a_tie, b_tie)
    )
}

wilson_ci <- function(successes, n, z = 1.96) {
  ifelse(n > 0, {
    phat <- successes / n
    denom <- 1 + (z^2)/n
    center <- (phat + (z^2)/(2*n)) / denom
    half   <- (z * sqrt((phat*(1-phat) + (z^2)/(4*n))/n)) / denom
    tibble(lower = pmax(0, center - half), upper = pmin(1, center + half))
  }, tibble(lower = NA_real_, upper = NA_real_))
}

all_categories <- sort(unique(data$category))

ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  br(),
  titlePanel(div("Chatbot Performance and Ratings",
                 style = "padding-bottom: 20px;"
  )),
  br(),
  sidebarLayout(
    sidebarPanel(
      class = "sticky-top",
      h4("Global Filters"),
      selectInput(
        "view_mode", "Category:",
        choices = c("Overall", all_categories),
        selected = "Overall"
      ),
      conditionalPanel(
        condition = "input.view_mode == 'Overall'",
        selectizeInput(
          "cat", "Filter Categories:",
          choices = all_categories, selected = all_categories,
          multiple = TRUE, options = list(plugins = list("remove_button"))
        )
      ),
      sliderInput("min_matches", "Minimum Matches", min = 1, max = 500, value = 30, step = 1)
    ),
    mainPanel(
      
      h3("Best Chatbots by Prompt Category"),
      div(
        style = "padding:20px; font-size:15px; line-height:1.5;",
        p("The following leaderboard summarizes model performance across categories by combining wins, ties, and losses (including 'both bad') into a composite success metric (Win rate + 0.5×Tie rate). Each model’s win rate and success rate are calculated from the full dataset of pairwise comparisons, filtering out models that participated in fewer than the minimum number of matches. Models are then ranked according to either win or success rate. To change the leaderboard view, use the section view expander on the right side of the chart.")
      ),
      bslib::card(
        full_screen = TRUE, collapsible = TRUE, collapsed = FALSE,
        bslib::card_header(
          div(style="display:flex; align-items:center; justify-content:space-between; gap:16px; flex-wrap:wrap;",
              div(style="font-weight:600;", textOutput("main_title")),
              div(
                selectInput(
                  "sort_by", "Sort leaderboard by:",
                  choices = c("Success rate", "Win rate"),
                  selected = "Success rate", width = "220px"
                )
              )
          )
        ),
        bslib::card_body(
          div(style = "font-weight:600; font-size:16px; margin-bottom:8px;", 
              "Sorted Bar Chart"),
          echarts4rOutput("main_chart", height = "600px"),
          tags$hr(),
          div(style = "font-weight:600; margin:8px 0;", "Table of Rates"),
          DTOutput("main_table")
        )
      ),
      
      br(),
      h3("Chatbot Strength & Weaknesses by Category"),
      div(
        style = "padding:20px; font-size:15px; line-height:1.5;",
        p("The category profile section measures how each chatbot performs within individual categories relative to the category average. For the selected model, the bar chart shows per-category success rates against the mean of all models. This provides an in-depth comparative view of how a single model’s capabilities vary across task types.")
      ),
      bslib::card(
        full_screen = TRUE, collapsible = TRUE, collapsed = FALSE,
        bslib::card_header(
          div(style="display:flex; align-items:center; justify-content:space-between; gap:16px; flex-wrap:wrap;",
              div(style="font-weight:600;", textOutput("prof_title_bar")),
              div(
                selectInput("prof_model", "Selected model:", choices = NULL, width = "280px")
              )
          )
        ),
        bslib::card_body(
          div(style="display:flex; gap:20px; flex-wrap:wrap; align-items:center; margin-bottom:10px;",
              checkboxInput("prof_use_success", "Use Success Rate (Win + 0.5×Tie)", value = TRUE)
          ),
          tags$div(style = "font-weight:600; margin:8px 0;", "Sorted Bar Chart"),
          echarts4rOutput("prof_bar", height = "520px"),
          tags$hr(),
          tags$div(style = "font-weight:600; margin:8px 0;", "Table of Rates by Category"),
          DTOutput("prof_detail_table")
        )
      ),
      
      br(),
      h3("Estimated Match-Up Results by Category"),
      div(
        style = "padding:20px; font-size:15px; line-height:1.5;",
        p("In this section we use the Bradley–Terry model to estimate the latent ability of each chatbot based on head-to-head outcomes. The Bradley-Terry model assigns a positive score or 'ability' to each item, and the probability of one model beating another is proportional to the ratio of their scores. The heatmap summarizes pairwise win probabilities across all models. Below, for a selected model, the stacked bar chart and table show the probability that one model defeats another, including the empirical rate of ties or 'both-bad' outcomes.")
      ),
      bslib::card(
        full_screen = TRUE, collapsible = TRUE, collapsed = FALSE,
        bslib::card_header(
          div(style="font-weight:600;", "Bradley–Terry Estimated Match-Ups")
        ),
        bslib::card_body(
          div(style = "font-weight:600; margin:8px 0;", "Head-to-Head Win Probabilities (Heatmap)"),
          plotlyOutput("bt_heatmap", height = "680px"),
          tags$hr(),
          div(
            style="display:flex; align-items:center; gap:16px; flex-wrap:wrap; margin-bottom:8px;",
            div(style="font-weight:600;", "Detailed Match-Ups for Selected Model"),
            selectInput("bt_model", "Selected model:", choices = NULL, width = "320px")
          ),
          div(style = "font-weight:600; margin:8px 0;", "Head-to-Head Win Probabilities (Bradley–Terry)"),
          echarts4rOutput("bt_chart", height = "720px"),
          tags$hr(),
          div(style = "font-weight:600; margin:8px 0;", "Table of Predicted Rates"),
          DTOutput("bt_table")
        )
      )
    )
  )
)

server <- function(input, output, session){
  
  selected_df <- reactive({
    if (identical(input$view_mode, "Overall")) {
      df <- data
      if (!is.null(input$cat) && length(input$cat) > 0) {
        df <- df %>% filter(category %in% input$cat)
      }
      df
    } else {
      data %>% filter(category == input$view_mode)
    }
  })
  
  models_available <- reactive({
    df <- selected_df()
    df %>%
      select(model_a, model_b) %>%
      pivot_longer(everything(), values_to = "model") %>%
      distinct(model) %>%
      arrange(model) %>%
      pull(model)
  })
  
  observeEvent(models_available(), {
    ms <- models_available()
    sel_prof <- if (length(ms) && input$prof_model %in% ms) input$prof_model else dplyr::first(ms)
    sel_bt   <- if (length(ms) && input$bt_model   %in% ms) input$bt_model   else dplyr::first(ms)
    updateSelectInput(session, "prof_model", choices = ms, selected = sel_prof)
    updateSelectInput(session, "bt_model",   choices = ms, selected = sel_bt)
  }, ignoreInit = FALSE)
  
  leaderboard <- reactive({
    df2 <- make_df2_leader(selected_df())
    sort_col <- if (identical(input$sort_by, "Win rate")) "win_rate" else "success_rate"
    
    df2 %>%
      group_by(model) %>%
      summarise(
        wins = sum(win),
        ties = sum(tie),
        matches = n(),
        win_rate = wins / matches,
        tie_rate = ties / matches,
        success_rate = (wins + 0.5 * ties) / matches,
        .groups = "drop"
      ) %>%
      filter(matches >= input$min_matches) %>%
      arrange(desc(.data[[sort_col]])) %>%
      mutate(
        rank     = row_number(),
        label    = sprintf("#%d  %s", rank, model),
        win_comp = round(win_rate, 3),
        tie_half = round(0.5 * tie_rate, 3)
      )
  })
  
  output$main_title <- renderText({
    "Chatbots Leaderboard (Success Rate = Win Rate + 0.5×Tie Rate)"
  })
  
  output$main_chart <- renderEcharts4r({
    lb <- leaderboard()
    validate(need(nrow(lb) > 0, "No models meet the minimum matches threshold."))
    
    metric_title <- if (identical(input$sort_by, "Win rate")) "Win Rate" else "Success Rate (Win + 0.5×Tie)"
    
    start_lab <- lb$label[1]
    end_lab   <- lb$label[min(10, nrow(lb))]
    
    lb |>
      e_charts(label) |>
      e_bar(win_comp, name = "Win Rate", stack = "rate",
            label = list(show = TRUE, position = "insideRight", color = "#fff", fontSize = 10)) |>
      e_bar(tie_half, name = "0.5*Tie Rate", stack = "rate",
            label = list(show = TRUE, position = "right", color = "#000", fontSize = 10)) |>
      e_flip_coords() |>
      e_y_axis(inverse = TRUE,
               axisLabel = list(fontSize = 11)) |>
      e_legend(right = 10, top = 30) |>
      e_title(paste0("Chatbots Ranked by ", metric_title)) |>
      e_datazoom(y_index = 0, type = "slider", startValue = start_lab, endValue = end_lab) |>
      e_datazoom(y_index = 0, type = "inside", startValue = start_lab, endValue = end_lab) |>
      e_x_axis(max = 1,
               axisLabel = list(formatter = htmlwidgets::JS("function(x){return (x*100).toFixed(0)+'%';}"))) |>
      e_grid(left = 260) |>   # more room for long model names
      e_tooltip(show = FALSE) |>
      e_theme("infographic") |>
      e_toolbox_feature("saveAsImage")
  })
  
  output$main_table <- renderDT({
    lb <- leaderboard()
    validate(need(nrow(lb) > 0, "No models meet the minimum matches threshold."))
    tbl <- lb %>% select(model, wins, ties, matches, win_rate, tie_rate, success_rate)
    
    order_col <- if (identical(input$sort_by, "Win rate")) 4L else 6L
    
    tbl <- tibble(
      Model = tbl$model,
      Wins = tbl$wins,
      Ties = tbl$ties,
      Matches = tbl$matches,
      `Win Rate` = tbl$win_rate,
      `Tie Rate` = tbl$tie_rate,
      `Success Rate` = tbl$success_rate
    ) 
    
    datatable(
      tbl,
      rownames = FALSE,
      extensions = "Buttons",
      options = list(
        pageLength = 10,
        lengthMenu = c(5, 10, 25, 50, 100),
        order = list(list(order_col, "desc")),
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel")
      )
    ) %>%
      formatPercentage(columns = c("Win Rate", "Tie Rate", "Success Rate"), digits = 1)
  })
  
  df2_profile <- reactive({
    make_df2_leader(selected_df())
  })
  
  model_cat_summary <- reactive({
    df2_profile() %>%
      group_by(model, category) %>%
      summarise(
        wins = sum(win),
        ties = sum(tie),
        matches = n(),
        win_rate = wins / matches,
        tie_rate = ties / matches,
        success_rate = (wins + 0.5 * ties) / matches,
        .groups = "drop"
      )
  })
  
  sel_profile <- reactive({
    metric <- if (isTRUE(input$prof_use_success)) "success_rate" else "win_rate"
    mcs <- model_cat_summary()
    
    me <- mcs %>%
      filter(model == input$prof_model) %>%
      rename(model_matches = matches)
    
    bench <- mcs %>%
      group_by(category) %>%
      summarise(
        bench = weighted.mean(.data[[metric]], w = .data$matches),
        total_matches = sum(.data$matches),
        .groups = "drop"
      )
    
    me %>%
      inner_join(bench, by = "category") %>%
      mutate(
        metric = .data[[metric]],
        lift = metric - bench
      ) %>%
      filter(model_matches >= input$min_matches) %>%
      arrange(desc(metric))
  })
  
  output$prof_title_bar <- renderText({
    paste0("Category Profile for ", input$prof_model)
  })
  
  output$prof_bar <- renderEcharts4r({
    validate(
      need(length(models_available()) > 0, "No models in the current view/filter."),
      need(!is.null(input$prof_model) && nzchar(input$prof_model), "Select a model to view the profile.")
    )
    
    prof <- sel_profile() %>%
      filter(is.finite(metric), is.finite(bench)) %>%
      mutate(
        metric = ifelse(is.na(metric), 0, metric),
        bench  = ifelse(is.na(bench),  0, bench)
      )
    
    validate(need(nrow(prof) > 0, "No categories meet the minimum matches threshold."))
    
    chart_df <- prof %>%
      arrange(desc(metric)) %>%
      mutate(
        cat_lab = sprintf("%s  (n=%d)", category, model_matches),
        my_val  = round(metric, 6),
        bench_r = round(bench,  6)
      )
    
    chart_df |>
      e_charts(cat_lab) |>
      e_bar(
        my_val, name = input$prof_model,
        label = list(
          show = TRUE, position = "insideLeft",
          color = "#fff", fontSize = 10
        )
      ) |>
      e_line(
        bench_r, 
        name = "Category Avg", 
        y_index = 0, 
        symbol = "circle", 
        symbolSize = 6,
        label = list(
          show = TRUE, position = 'right',
          color = '#000',fontSize = 10
        )) |>
      e_flip_coords() |>
      e_y_axis(inverse = TRUE,
               axisLabel = list(fontSize = 11)) |>
      e_title("Model vs Category Average") |>
      e_x_axis(
        max = 1,
        axisLabel = list(formatter = htmlwidgets::JS("function(x){ return (x*100).toFixed(0)+'%'; }"))
      ) |>
      e_grid(left = 260) |>   # more room for longer category labels
      e_legend(right = 10, top = 30) |>
      e_datazoom(y_index = 0, type = "slider") |>
      e_theme("infographic") |>
      e_toolbox_feature("saveAsImage")
  })
  
  output$prof_detail_table <- DT::renderDT({
    validate(
      need(length(models_available()) > 0, "No models in the current view/filter."),
      need(!is.null(input$prof_model) && nzchar(input$prof_model), "Select a model to view the profile.")
    )
    
    mcs <- model_cat_summary()
    metric_name <- if (isTRUE(input$prof_use_success)) "success_rate" else "win_rate"
    
    prof <- mcs %>%
      filter(model == input$prof_model) %>%
      rename(model_matches = matches)
    
    bench <- mcs %>%
      group_by(category) %>%
      summarise(
        bench = weighted.mean(.data[[metric_name]], w = .data$matches),
        total_matches = sum(.data$matches),
        .groups = "drop"
      )
    
    prof <- prof %>%
      inner_join(bench, by = "category") %>%
      mutate(
        metric = .data[[metric_name]],
        lift = metric - bench
      ) %>%
      filter(model_matches >= input$min_matches) %>%
      arrange(desc(metric))
    
    validate(need(nrow(prof) > 0, "No categories meet the minimum matches threshold."))
    
    if (isTRUE(input$prof_use_success)) {
      ci <- wilson_ci(successes = prof$wins + 0.5 * prof$ties, n = prof$model_matches)
    } else {
      ci <- wilson_ci(successes = prof$wins, n = prof$model_matches)
    }
    
    tbl <- tibble(
      Category = prof$category,
      `Matches (model)` = prof$model_matches,
      Wins = prof$wins,
      Ties = prof$ties,
      Rate = prof$metric,
      `Category Avg` = prof$bench,
      `Lift (pts)` = round(100 * prof$lift, 1),
      `Matches (category total)` = prof$total_matches
    )
    
    DT::datatable(
      tbl,
      rownames = FALSE,
      extensions = "Buttons",
      options = list(
        pageLength = 10,
        lengthMenu = c(5, 10, 25, 50, 100),
        order = list(list(4, "desc")),
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel")
      )
    ) %>%
      DT::formatPercentage(columns = c("Rate", "Category Avg"), digits = 1)
  })
  
  bt_pair_counts <- reactive({
    selected_df() %>%
      transmute(
        A = pmin(model_a, model_b),
        B = pmax(model_a, model_b),
        is_tie_or_bb = as.integer(winner %in% c("tie","both_bad"))
      ) %>%
      filter(A != B) %>%
      summarise(
        matches = dplyr::n(),
        ties_or_bb = sum(is_tie_or_bb),
        .by = c(A, B)
      )
  })
  
  bt_fit <- reactive({
    df <- selected_df() %>%
      filter(winner %in% c("model_a", "model_b")) %>%
      transmute(
        player1 = model_a,
        player2 = model_b,
        r1 = as.numeric(winner == "model_a"),
        r2 = as.numeric(winner == "model_b")
      )
    
    validate(need(nrow(df) > 0, "No decisive matches in the selected filter."))
    
    players <- sort(unique(c(df$player1, df$player2)))
    df <- df %>% mutate(
      player1 = factor(player1, levels = players),
      player2 = factor(player2, levels = players)
    )
    
    fit <- tryCatch(
      BTm(cbind(r1, r2), player1, player2, data = df, id = "player"),
      error = function(e) NULL
    )
    validate(need(!is.null(fit), "BT model failed to fit for the selected filter (disconnected or separable)."))
    fit
  })
  
  bt_abilities <- reactive({
    fit <- bt_fit()
    ab <- BTabilities(fit) %>% as.data.frame() %>% tibble::rownames_to_column("model") %>% select(model, ability)
    setNames(ab$ability, ab$model)
  })
  
  output$bt_heatmap <- renderPlotly({
    ab_map   <- bt_abilities()
    choices  <- models_available()
    
    validate(need(length(choices) > 1, "Need at least two models to draw the heatmap."))
    
    abilities <- tibble(model = names(ab_map), ability = as.numeric(ab_map))
    
    lb <- leaderboard()
    validate(need(nrow(lb) > 0, "No leaderboard rows to drive model ordering."))
    
    model_order <- intersect(lb$model, abilities$model)
    validate(need(length(model_order) > 1, "Not enough overlap between BT models and leaderboard models."))
    
    grid <- expand.grid(
      row_model = model_order,
      col_model = model_order,
      stringsAsFactors = FALSE
    ) %>%
      mutate(
        prob = ifelse(
          row_model == col_model, NA_real_,
          plogis(ab_map[row_model] - ab_map[col_model])
        )
      )
    
    grid$row_model <- factor(grid$row_model, levels = rev(model_order))
    grid$col_model <- factor(grid$col_model, levels = model_order)
    
    grid <- grid %>% mutate(
      hover = sprintf("Row: %s<br>Col: %s<br>P(Row beats Col): %s",
                      row_model, col_model,
                      ifelse(is.na(prob), "—", percent(prob, 0.1)))
    )
    
    p <- ggplot(grid, aes(x = col_model, y = row_model, fill = prob, text = hover)) +
      geom_tile(na.rm = FALSE) +
      scale_fill_viridis_c(
        option = "D", limits = c(0, 1),
        labels = percent_format(accuracy = 1),
        name = "P(Row beats Col)"
      ) +
      coord_equal() +
      labs(
        title = "Head-to-Head Win Probabilities (Bradley–Terry)",
        x = "Column model",
        y = "Row model"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title.position = "plot",
        axis.title.x = element_text(margin = margin(t = 0)),
        axis.title.y = element_text(margin = margin(r = 0)),
        axis.text.x = element_text(size = 3, angle = 45, hjust = 1, vjust = 1),
        axis.text.y = element_text(size = 4),
        panel.grid = element_blank()
      )
    
    ggplotly(p, tooltip = "text")
  })
  
  bt_make_vs <- function(sel, model_choices, ab_map, pair_counts) {
    opps <- setdiff(model_choices, sel)
    if (!(sel %in% names(ab_map)) || !length(opps)) {
      return(tibble(opponent=character(), matches_vs_selected=integer(),
                    p_sel_vs_row=numeric(), p_row_vs_sel=numeric(), p_tie_or_both_bad=numeric()))
    }
    
    base <- tibble(opponent = opps) %>%
      mutate(
        p_sel_vs_row = ifelse(opponent %in% names(ab_map),
                              plogis(ab_map[sel] - ab_map[opponent]), NA_real_),
        p_row_vs_sel = ifelse(opponent %in% names(ab_map),
                              plogis(ab_map[opponent] - ab_map[sel]), NA_real_),
        A = pmin(sel, opponent), B = pmax(sel, opponent)
      ) %>%
      left_join(pair_counts, by = c("A","B")) %>%
      mutate(
        matches_vs_selected = replace_na(matches, 0L),
        p_tie_or_both_bad = replace_na(ties_or_bb / pmax(matches, 1), 0),
        p_sel_vs_row = replace_na(p_sel_vs_row, 0),
        p_row_vs_sel = replace_na(p_row_vs_sel, 0)
      ) %>%
      select(opponent, matches_vs_selected, p_sel_vs_row, p_row_vs_sel, p_tie_or_both_bad)
    
    base %>%
      filter(matches_vs_selected >= input$min_matches) %>%
      arrange(desc(p_sel_vs_row))
  }
  
  output$bt_chart <- renderEcharts4r({
    ab_map   <- bt_abilities()
    pair_cnt <- bt_pair_counts()
    choices  <- models_available()
    validate(need(length(choices) > 0, "No models available for the selected filter."))
    
    df_all <- bt_make_vs(input$bt_model, choices, ab_map, pair_cnt)
    validate(need(nrow(df_all) > 0, "No head-to-head predictions meet the min-matches threshold."))
    
    df <- df_all %>%
      mutate(rank = row_number(), label = sprintf("#%d  %s", rank, opponent))
    start_lab <- df$label[1]
    end_lab   <- df$label[min(10, nrow(df))]
    
    df |>
      e_charts(label) |>
      e_bar(p_sel_vs_row, name = "P(Selected Model Wins)", stack = "prob",
            label = list(show = TRUE, position = "insideRight", color = "#fff", fontSize = 10
            )) |>
      e_bar(p_tie_or_both_bad, name = "Empirical % Tie or Both Bad", stack = "prob",
            label = list(show = TRUE, position = "right", color = "#000", fontSize = 10
            )) |>
      e_flip_coords() |>
      e_y_axis(inverse = TRUE,
               axisLabel = list(fontSize = 11)) |>
      e_legend(right = 10, top = 30) |>
      e_title(paste0("Predicted Matchup Outcomes vs ", input$bt_model)) |>
      e_datazoom(y_index = 0, type = "slider", startValue = start_lab, endValue = end_lab) |>
      e_datazoom(y_index = 0, type = "inside", startValue = start_lab, endValue = end_lab) |>
      e_x_axis(max = 1,
               axisLabel = list(formatter = htmlwidgets::JS("function(x){return (x*100).toFixed(0)+'%';}"))) |>
      e_grid(left = 260) |>   # more room for long opponent names
      e_tooltip(show = FALSE) |>
      e_theme("infographic") |>
      e_toolbox_feature("saveAsImage")
  })
  
  output$bt_table <- DT::renderDT({
    ab_map   <- bt_abilities()
    pair_cnt <- bt_pair_counts()
    choices  <- models_available()
    validate(need(length(choices) > 0, "No models available for the selected filter."))
    
    tbl <- bt_make_vs(input$bt_model, choices, ab_map, pair_cnt)
    validate(need(nrow(tbl) > 0, "No head-to-head rows meet the min-matches threshold."))
    
    tbl <- tibble(
      Opponent = tbl$opponent,
      `# Matches` = tbl$matches_vs_selected,
      `P(Selected Model Wins)` = tbl$p_sel_vs_row,
      `P(Opponent Wins)` = tbl$p_row_vs_sel,
      `Empirical % Tie or Both Bad` = tbl$p_tie_or_both_bad
    ) 
    
    DT::datatable(
      tbl,
      rownames = FALSE,
      extensions = "Buttons",
      options = list(
        pageLength = 10,
        lengthMenu = c(5, 10, 25, 50, 100),
        order = list(list(3, "desc")),
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel")
      )
    ) %>%
      DT::formatPercentage(columns = c("P(Selected Model Wins)", "P(Opponent Wins)", "Empirical % Tie or Both Bad"), digits = 1)
  })
}

shinyApp(ui, server)