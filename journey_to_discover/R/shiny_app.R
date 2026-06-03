# ============================================================
# Journey To Discover — Shiny App
# Hidden Gem Explorer for Indian Destinations
# Trio Explorers · CSE DS
# Run with: shiny::runApp("R/shiny_app.R")
# ============================================================

library(shiny)
library(bslib)
library(tidyverse)
library(jsonlite)
library(janitor)
library(DT)
library(plotly)
library(scales)

# ── Load & prepare data ──────────────────────────────────────
travel_raw <- read_csv(here::here("data", "travel_dataset.csv"),
                       show_col_types = FALSE) %>% clean_names()

# Read JSON as a raw list — most reliable approach across all R versions
tourism_list <- jsonlite::read_json(
  here::here("data", "india_tourism_dataset.json")
)

travel <- travel_raw %>%
  rename(
    reviews  = number_of_google_review_in_lakhs,
    rating   = google_review_rating,
    fee      = entrance_fee_in_inr,
    visit_hr = time_needed_to_visit_in_hrs
  ) %>%
  mutate(
    reviews = as.numeric(reviews),
    rating  = as.numeric(rating),
    fee     = as.numeric(fee),
    popularity_tier = case_when(
      reviews < 0.5  ~ "Low (<0.5L)",
      reviews < 1.5  ~ "Moderate (0.5-1.5L)",
      reviews < 3.0  ~ "High (1.5-3L)",
      TRUE           ~ "Very High (>3L)"
    ),
    popularity_tier = factor(popularity_tier,
      levels = c("Low (<0.5L)", "Moderate (0.5-1.5L)",
                 "High (1.5-3L)", "Very High (>3L)"))
  ) %>%
  filter(!is.na(reviews), !is.na(rating))

# Safe scalar extractors from a list entry
get_chr  <- function(x, f) { v <- x[[f]]; if (is.null(v)) NA_character_ else as.character(v) }
get_dbl  <- function(x, f) { v <- x[[f]]; if (is.null(v)) NA_real_      else as.numeric(v)  }
get_lgl  <- function(x, f) { v <- x[[f]]; if (is.null(v)) NA            else as.logical(v)  }
get_lst  <- function(x, f) { v <- x[[f]]; if (is.null(v)) list()        else v               }
get_range <- function(x, idx) {
  v <- x[["budget_category"]][["total_daily_range"]]
  if (is.null(v) || length(v) < idx) NA_real_ else as.numeric(v[[idx]])
}

# Build tourism tibble row by row from the raw list
tourism <- tibble(
  destination   = map_chr(tourism_list, get_chr,  "destination_name"),
  state         = map_chr(tourism_list, get_chr,  "state"),
  region        = map_chr(tourism_list, get_chr,  "region"),
  lat           = map_dbl(tourism_list, ~ get_dbl(.x[["coordinates"]], "latitude")),
  lng           = map_dbl(tourism_list, ~ get_dbl(.x[["coordinates"]], "longitude")),
  popularity_sc = map_dbl(tourism_list, get_dbl,  "popularity_score"),
  safety_rating = map_dbl(tourism_list, get_dbl,  "safety_rating"),
  accessibility = map_chr(tourism_list, get_chr,  "accessibility"),
  hidden_gems   = map(tourism_list,     get_lst,  "hidden_gems"),
  hidden_gem_n  = map_int(tourism_list, ~ length(get_lst(.x, "hidden_gems"))),
  trip_types    = map(tourism_list,     get_lst,  "trip_types"),
  best_seasons  = map(tourism_list,     get_lst,  "best_seasons"),
  budget_low    = map_dbl(tourism_list, get_range, 1),
  budget_high   = map_dbl(tourism_list, get_range, 2),
  ideal_days    = map_dbl(tourism_list, get_dbl,  "ideal_days"),
  min_days      = map_dbl(tourism_list, get_dbl,  "minimum_days"),
  experience    = map_chr(tourism_list, get_chr,  "unique_experiences"),
  user_review   = map_chr(tourism_list, ~ { v <- .x[["user_reviews_summary"]]; if (is.null(v)) "" else as.character(v) }),
  ideal_for     = map(tourism_list,     get_lst,  "ideal_for"),
  activities    = map(tourism_list,     get_lst,  "activities_available"),
  permits       = map_lgl(tourism_list, get_lgl,  "permits_required")
) %>%
  mutate(
    hidden_gem_score = round((hidden_gem_n * safety_rating) /
                               pmax(popularity_sc, 1), 2),
    region_simple = case_when(
      str_detect(region, "North East|Northeast") ~ "North East",
      str_detect(region, "North")    ~ "North",
      str_detect(region, "South")    ~ "South",
      str_detect(region, "East")     ~ "East",
      str_detect(region, "West")     ~ "West",
      str_detect(region, "Central")  ~ "Central",
      TRUE                           ~ "Islands"
    ),
    # Flatten seasons to simple labels
    season_label = map_chr(best_seasons, function(s) {
      sl <- tolower(paste(s, collapse = " "))
      tags <- c()
      if (str_detect(sl, "winter|november|december|january|february"))
        tags <- c(tags, "Winter")
      if (str_detect(sl, "summer|april|may|june"))
        tags <- c(tags, "Summer")
      if (str_detect(sl, "monsoon|july|august|september"))
        tags <- c(tags, "Monsoon")
      if (str_detect(sl, "autumn|post|october"))
        tags <- c(tags, "Autumn")
      if (str_detect(sl, "spring|march"))
        tags <- c(tags, "Spring")
      if (length(tags) == 0) tags <- "Year-round"
      paste(tags, collapse = ", ")
    })
  )

# State-level join back to travel dataset
state_summary <- tourism %>%
  group_by(state) %>%
  summarise(
    avg_popularity  = mean(popularity_sc),
    avg_safety      = mean(safety_rating),
    total_hidden_n  = sum(hidden_gem_n),
    avg_budget_low  = mean(budget_low),
    avg_budget_high = mean(budget_high),
    .groups = "drop"
  )

df <- travel %>%
  left_join(state_summary, by = "state")

# Unique values for filters
all_zones   <- sort(unique(na.omit(travel$zone)))
all_seasons <- c("Winter", "Summer", "Monsoon", "Autumn", "Spring")
all_access  <- c("Easy", "Moderate", "Difficult")
all_regions <- sort(unique(tourism$region_simple))
all_trips   <- sort(unique(unlist(tourism$trip_types)))

# ── Colour helpers ───────────────────────────────────────────
gem_pal <- c(
  "Very High (>3L)"    = "#01579B",
  "High (1.5-3L)"      = "#0288D1",
  "Moderate (0.5-1.5L)"= "#4FC3F7",
  "Low (<0.5L)"        = "#B3E5FC"
)

score_color <- function(s) {
  if (s >= 4) "#0F6E56"
  else if (s >= 3) "#3B6D11"
  else if (s >= 2) "#BA7517"
  else "#5F5E5A"
}

# ── UI ───────────────────────────────────────────────────────
ui <- page_navbar(
  title = "Journey to Discover",
  theme = bs_theme(
    version   = 5,
    bootswatch = "flatly",
    primary    = "#1D9E75",
    font_scale = 0.9
  ),
  bg = "#1a2e4a",
  inverse = TRUE,

  # ── Tab 1: Hidden Gem Explorer ──────────────────────────
  nav_panel("Hidden Gem Explorer",
    layout_sidebar(
      sidebar = sidebar(
        width = 260,
        bg = "#f8f9fa",

        h6("Destination Filters", class = "text-muted fw-bold mt-2 mb-2"),

        selectInput("region_f", "Region",
          choices = c("All" = "", all_regions), selected = ""),

        selectInput("access_f", "Accessibility",
          choices = c("Any" = "", all_access), selected = ""),

        selectInput("season_f", "Best Season",
          choices = c("Any" = "", all_seasons), selected = ""),

        selectInput("permits_f", "Permits",
          choices = c("Any" = "any", "Not required" = "no",
                      "Required" = "yes"), selected = "any"),

        hr(),
        h6("Score & Budget", class = "text-muted fw-bold mb-2"),

        sliderInput("gem_score_f", "Min hidden gem score",
          min = 0, max = 5, value = 0, step = 0.5),

        sliderInput("budget_f", "Max daily budget (₹)",
          min = 1000, max = 12000, value = 12000,
          step = 500, pre = "₹", sep = ","),

        hr(),
        h6("Sort by", class = "text-muted fw-bold mb-2"),
        selectInput("sort_f", NULL,
          choices = c(
            "Gem score (high first)"  = "gem_score",
            "Popularity (low first)"  = "pop_asc",
            "Budget (low first)"      = "budget_asc",
            "Hidden gems count"       = "gem_count",
            "Safety rating"           = "safety"
          ))
      ),

      # Main panel
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        value_box("Destinations",  textOutput("n_dest",  inline=T), showcase = icon("map-pin"),   theme = "primary"),
        value_box("Hidden Gems",   textOutput("n_gems",  inline=T), showcase = icon("gem"),       theme = "success"),
        value_box("Avg Gem Score", textOutput("avg_sc",  inline=T), showcase = icon("star"),      theme = "info"),
        value_box("Avg Budget/day",textOutput("avg_bud", inline=T), showcase = icon("wallet"),    theme = "secondary")
      ),

      br(),

      # Trip type chips
      div(
        style = "margin-bottom:12px",
        uiOutput("trip_chips")
      ),

      # Cards
      uiOutput("dest_cards")
    )
  ),

  # ── Tab 2: Review Distribution ──────────────────────────
  nav_panel("Review Analysis",
    layout_columns(
      col_widths = c(7, 5),
      card(
        card_header("Histogram of Google Reviews"),
        plotlyOutput("hist_plot", height = "320px")
      ),
      card(
        card_header("Box Plot of Google Reviews"),
        plotlyOutput("box_plot", height = "320px")
      )
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Popularity Tier Breakdown"),
        plotlyOutput("tier_plot", height = "300px")
      ),
      card(
        card_header("Reviews vs Rating"),
        plotlyOutput("scatter_plot", height = "300px")
      )
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Review Distribution by Zone"),
        plotlyOutput("zone_plot", height = "300px")
      ),
      card(
        card_header("Top 15 Most-Reviewed Destinations"),
        plotlyOutput("top15_plot", height = "300px")
      )
    )
  ),

  # ── Tab 3: Hidden Gem Deep Dive ─────────────────────────
  nav_panel("Hidden Gem Deep Dive",
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Hidden Gem Count by State"),
        plotlyOutput("gem_state_plot", height = "340px")
      ),
      card(
        card_header("Budget vs Popularity Score"),
        plotlyOutput("budget_pop_plot", height = "340px")
      )
    ),
    card(
      card_header("Full Destination Table with Gem Scores"),
      DTOutput("dest_table")
    )
  ),

  # ── Tab 4: Key Insights ──────────────────────────────────
  nav_panel("Key Insights",
    layout_columns(
      col_widths = c(12),
      card(
        card_header("Project — Journey to Discover"),
        p("Team: Trio Explorers · CSE DS"),
        tags$table(
          class = "table table-sm table-bordered",
          tags$thead(tags$tr(
            tags$th("Name"), tags$th("Roll No"), tags$th("Autonomy No")
          )),
          tags$tbody(
            tags$tr(tags$td("Sanchayan Adhya"),  tags$td("2462048"), tags$td("12624019043")),
            tags$tr(tags$td("Aryan Kumar"),       tags$td("2462065"), tags$td("12624019016")),
            tags$tr(tags$td("Aditi Shambhavi"),   tags$td("2462045"), tags$td("12624019001")),
            tags$tr(tags$td("Rahul Mahato"),      tags$td("2462077"), tags$td("12625019080"))
          )
        )
      )
    ),
    layout_columns(
      col_widths = c(4, 4, 4),
      card(
        card_header(icon("chart-bar"), " Finding 1: Low-review majority"),
        p("Most destinations have fewer than 0.5L reviews. Visibility is concentrated at a handful of iconic locations, not evenly distributed across the country.")
      ),
      card(
        card_header(icon("gem"), " Finding 2: Hidden gems link"),
        p("Dataset 2 lists validated hidden gems per destination. Low-review destinations often carry the highest gem scores — underrated does not mean inferior.")
      ),
      card(
        card_header(icon("map"), " Finding 3: Recommendation value"),
        p("High-rated destinations span all review tiers. Popularity alone is a poor guide for itinerary planning. The gem score combines safety, gem count, and inverse popularity to surface overlooked spots.")
      )
    ),
    card(
      card_header("Hidden Gem Score Formula"),
      tags$pre(
        class = "bg-light p-3 rounded",
        "hidden_gem_score = (hidden_gem_count × safety_rating) ÷ popularity_score\n\nHigh score = many validated gems + safe + low mainstream popularity\nExample: Ziro Valley → (3 × 8) ÷ 5 = 4.8  (top scorer)\nExample: Agra       → (2 × 8) ÷ 10 = 1.6  (iconic but low gem score)"
      )
    )
  )
)

# ── Server ───────────────────────────────────────────────────
server <- function(input, output, session) {

  # Trip type selection state
  selected_trips <- reactiveVal(character(0))

  observeEvent(input$trip_toggle, {
    cur <- selected_trips()
    t   <- input$trip_toggle
    if (t %in% cur) selected_trips(setdiff(cur, t))
    else            selected_trips(union(cur, t))
  })

  # ── Filtered tourism data ──────────────────────────────
  filt_tourism <- reactive({
    d <- tourism
    if (nzchar(input$region_f))  d <- filter(d, region_simple == input$region_f)
    if (nzchar(input$access_f))  d <- filter(d, accessibility  == input$access_f)
    if (nzchar(input$season_f)) {
      se <- input$season_f
      d  <- filter(d, map_lgl(best_seasons, ~ any(str_detect(tolower(.x), tolower(se)))))
    }
    if (input$permits_f == "no")  d <- filter(d, !permits)
    if (input$permits_f == "yes") d <- filter(d,  permits)
    d <- filter(d, hidden_gem_score >= input$gem_score_f,
                   budget_low       <= input$budget_f)
    if (length(selected_trips()) > 0) {
      d <- filter(d, map_lgl(trip_types,
                             ~ any(.x %in% selected_trips())))
    }
    # Sort
    d <- switch(input$sort_f,
      gem_score  = arrange(d, desc(hidden_gem_score)),
      pop_asc    = arrange(d, popularity_sc),
      budget_asc = arrange(d, budget_low),
      gem_count  = arrange(d, desc(hidden_gem_n)),
      safety     = arrange(d, desc(safety_rating)),
      d
    )
    d
  })

  # ── Value boxes ────────────────────────────────────────
  output$n_dest  <- renderText(nrow(filt_tourism()))
  output$n_gems  <- renderText(sum(filt_tourism()$hidden_gem_n))
  output$avg_sc  <- renderText(
    if (nrow(filt_tourism()) > 0)
      round(mean(filt_tourism()$hidden_gem_score), 2) else "–"
  )
  output$avg_bud <- renderText(
    if (nrow(filt_tourism()) > 0)
      paste0("₹", format(round(mean(filt_tourism()$budget_low)),
                         big.mark = ",")) else "–"
  )

  # ── Trip type chips ────────────────────────────────────
  output$trip_chips <- renderUI({
    chips <- lapply(all_trips, function(t) {
      active <- t %in% selected_trips()
      actionButton(
        inputId  = "trip_toggle",
        label    = t,
        value    = t,
        class    = paste("btn btn-sm me-1 mb-1",
                         if (active) "btn-success" else "btn-outline-secondary"),
        onclick  = sprintf(
          "Shiny.setInputValue('trip_toggle', '%s', {priority: 'event'})", t)
      )
    })
    div(chips)
  })

  # ── Destination cards ──────────────────────────────────
  output$dest_cards <- renderUI({
    d <- filt_tourism()
    if (nrow(d) == 0) {
      return(div(class = "text-center text-muted py-5",
                 icon("search"), " No destinations match your filters."))
    }
    cards <- lapply(seq_len(nrow(d)), function(i) {
      row   <- d[i, ]
      sc    <- row$hidden_gem_score
      gems  <- unlist(row$hidden_gems)
      trips <- paste(unlist(row$trip_types)[1:min(4, length(unlist(row$trip_types)))],
                     collapse = " · ")
      sc_col <- score_color(sc)

      div(
        class = "card mb-2 shadow-sm",
        div(
          class = "card-body py-2 px-3",
          div(
            class = "d-flex align-items-start gap-3",
            # Score badge
            div(
              class = "text-center rounded p-2 flex-shrink-0",
              style = sprintf("background:#E1F5EE;min-width:54px;color:%s", sc_col),
              div(style = "font-size:1.3rem;font-weight:500;line-height:1",
                  round(sc, 1)),
              div(style = "font-size:0.65rem", "gem score")
            ),
            # Main info
            div(
              class = "flex-grow-1",
              div(
                class = "d-flex align-items-center gap-2",
                strong(row$destination),
                if (row$permits) span(class = "badge bg-warning text-dark",
                                      "Permit needed") else NULL,
                span(class = "badge bg-light text-dark border",
                     row$accessibility)
              ),
              div(class = "text-muted small",
                  row$state, " · ", row$region_simple,
                  " · ", row$hidden_gem_n, " hidden gems",
                  " · ", row$ideal_days, " ideal days"),
              div(class = "text-muted small mt-1", trips)
            ),
            # Budget
            div(
              class = "text-end flex-shrink-0",
              div(class = "small fw-bold",
                  paste0("₹", format(row$budget_low, big.mark = ","),
                         "–", format(row$budget_high, big.mark = ","))),
              div(class = "text-muted", style = "font-size:0.7rem", "per day")
            )
          ),

          # Hidden gems list
          if (length(gems) > 0) {
            div(
              class = "mt-2",
              div(class = "small text-muted fw-bold mb-1",
                  icon("gem"), " HIDDEN GEMS"),
              div(
                class = "d-flex flex-wrap gap-1",
                lapply(gems, function(g) {
                  span(class = "badge",
                       style = "background:#E1F5EE;color:#085041;font-weight:400",
                       g)
                })
              )
            )
          },

          # Review
          if (nzchar(row$user_review)) {
            div(
              class = "small text-muted fst-italic mt-2 p-2 rounded",
              style = "background:#f8f9fa",
              paste0('"', row$user_review, '"')
            )
          }
        )
      )
    })

    div(cards)
  })

  # ── Review Analysis plots ──────────────────────────────
  output$hist_plot <- renderPlotly({
    mn <- mean(df$reviews, na.rm = TRUE)
    md <- median(df$reviews, na.rm = TRUE)
    plot_ly(df, x = ~reviews, type = "histogram",
            xbins = list(size = 0.3),
            marker = list(color = "#2979FF", line = list(color = "white", width = 0.5))) %>%
      add_segments(x = mn, xend = mn, y = 0, yend = 80,
                   line = list(color = "#E53935", dash = "dash"), name = "Mean") %>%
      add_segments(x = md, xend = md, y = 0, yend = 70,
                   line = list(color = "#FF9800", dash = "dash"), name = "Median") %>%
      layout(xaxis = list(title = "Reviews (lakhs)"),
             yaxis = list(title = "Frequency"),
             showlegend = TRUE,
             plot_bgcolor  = "white",
             paper_bgcolor = "white")
  })

  output$box_plot <- renderPlotly({
    plot_ly(df, y = ~reviews, type = "box",
            fillcolor = "#90CAF9",
            line      = list(color = "#1a2e4a"),
            marker    = list(color = "#E53935", size = 6)) %>%
      layout(yaxis = list(title = "Reviews (lakhs)"),
             xaxis = list(title = ""),
             plot_bgcolor  = "white",
             paper_bgcolor = "white")
  })

  output$tier_plot <- renderPlotly({
    tc <- df %>% count(popularity_tier)
    plot_ly(tc, x = ~popularity_tier, y = ~n,
            type   = "bar",
            marker = list(color = c("#B3E5FC","#4FC3F7","#0288D1","#01579B")),
            text   = ~n, textposition = "outside") %>%
      layout(xaxis = list(title = ""),
             yaxis = list(title = "Count"),
             plot_bgcolor  = "white",
             paper_bgcolor = "white")
  })

  output$scatter_plot <- renderPlotly({
    plot_ly(df, x = ~reviews, y = ~rating,
            color = ~popularity_tier,
            colors = gem_pal,
            type = "scatter", mode = "markers",
            text = ~paste0("<b>", name, "</b><br>",
                           state, "<br>Reviews: ", reviews, "L<br>Rating: ", rating),
            hoverinfo = "text",
            marker = list(size = 7, opacity = 0.75)) %>%
      layout(xaxis = list(title = "Reviews (lakhs)"),
             yaxis = list(title = "Rating"),
             legend = list(title = list(text = "Tier")),
             plot_bgcolor  = "white",
             paper_bgcolor = "white")
  })

  output$zone_plot <- renderPlotly({
    df_z <- df %>% filter(!is.na(zone))
    plot_ly(df_z, y = ~reviews, color = ~zone,
            type = "box", boxpoints = "outliers") %>%
      layout(xaxis = list(title = ""),
             yaxis = list(title = "Reviews (lakhs)"),
             plot_bgcolor  = "white",
             paper_bgcolor = "white")
  })

  output$top15_plot <- renderPlotly({
    t15 <- df %>% arrange(desc(reviews)) %>% slice_head(n = 15)
    plot_ly(t15,
            x    = ~reviews,
            y    = ~reorder(name, reviews),
            type = "bar", orientation = "h",
            marker = list(color = "#2979FF"),
            text = ~paste0(reviews, "L"), textposition = "outside") %>%
      layout(xaxis = list(title = "Reviews (lakhs)"),
             yaxis = list(title = ""),
             plot_bgcolor  = "white",
             paper_bgcolor = "white",
             margin = list(l = 180))
  })

  # ── Hidden Gem Deep Dive plots ─────────────────────────
  output$gem_state_plot <- renderPlotly({
    ss <- state_summary %>%
      arrange(desc(total_hidden_n)) %>%
      slice_head(n = 15)
    plot_ly(ss,
            x    = ~total_hidden_n,
            y    = ~reorder(state, total_hidden_n),
            type = "bar", orientation = "h",
            marker = list(color = ~avg_popularity,
                          colorscale = "Viridis",
                          showscale  = TRUE,
                          colorbar   = list(title = "Avg popularity")),
            text = ~total_hidden_n, textposition = "outside") %>%
      layout(xaxis = list(title = "Hidden gems listed"),
             yaxis = list(title = ""),
             plot_bgcolor  = "white",
             paper_bgcolor = "white",
             margin = list(l = 160))
  })

  output$budget_pop_plot <- renderPlotly({
    plot_ly(tourism,
            x    = ~popularity_sc,
            y    = ~budget_low,
            size = ~hidden_gem_n,
            color = ~accessibility,
            colors = c("Easy" = "#1D9E75", "Moderate" = "#FF9800", "Difficult" = "#E53935"),
            type = "scatter", mode = "markers",
            text = ~paste0("<b>", destination, "</b><br>",
                           state, "<br>",
                           "Popularity: ", popularity_sc, "/10<br>",
                           "Budget: ₹", format(budget_low, big.mark=","), "/day<br>",
                           "Hidden gems: ", hidden_gem_n, "<br>",
                           "Gem score: ", round(hidden_gem_score, 2)),
            hoverinfo = "text",
            marker = list(opacity = 0.75)) %>%
      layout(xaxis = list(title = "Popularity score (out of 10)"),
             yaxis = list(title = "Min daily budget (₹)"),
             legend = list(title = list(text = "Accessibility")),
             plot_bgcolor  = "white",
             paper_bgcolor = "white")
  })

  output$dest_table <- renderDT({
    tourism %>%
      mutate(
        hidden_gems_list = map_chr(hidden_gems, ~ paste(.x, collapse = "; ")),
        trip_types_list  = map_chr(trip_types,  ~ paste(.x, collapse = ", ")),
        seasons_list     = season_label
      ) %>%
      select(
        Destination   = destination,
        State         = state,
        Region        = region_simple,
        `Gem Score`   = hidden_gem_score,
        `Gem Count`   = hidden_gem_n,
        Popularity    = popularity_sc,
        Safety        = safety_rating,
        Accessibility = accessibility,
        `Budget Low`  = budget_low,
        `Ideal Days`  = ideal_days,
        Permits       = permits,
        Seasons       = seasons_list,
        `Trip Types`  = trip_types_list,
        `Hidden Gems` = hidden_gems_list
      ) %>%
      arrange(desc(`Gem Score`)) %>%
      datatable(
        filter    = "top",
        rownames  = FALSE,
        options   = list(pageLength = 15, scrollX = TRUE,
                         columnDefs = list(list(width = "220px",
                                                targets = c(12, 13)))),
        class     = "table table-sm table-striped"
      ) %>%
      formatRound("`Gem Score`", digits = 2) %>%
      formatCurrency("`Budget Low`", currency = "₹", digits = 0)
  })
}

shinyApp(ui, server)
