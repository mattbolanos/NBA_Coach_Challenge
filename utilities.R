# --- utilities --- #

# ----------------- #
# --- Functions --- #
# ----------------- #

# Tooltip function
with_tooltip <- function(val, tooltip_val) {
  
  div(
    tippy(text = val, tooltip = tooltip_val, placement = "top", theme = "translucent", animation = "scale", duration = 500)
  )
  
}

# Color palette function
make_color_pal <- function(colors, bias = 1) {
  get_color <- colorRamp(colors, bias = bias)
  function(x) rgb(get_color(x), maxColorValue = 255)
}

# Normalize colors
normalize_col_vec <- function(dataframe, col, value) {
  x <- dataframe[[col]]
  min <- x %>% min()
  max <- x %>% max()
  res <- (value - min) / (max - min)
  res
}

# Choose palette
pal_react <- viridis::magma(n = 5, begin = 0.65, end = 1) %>% make_color_pal(bias = 2)

# Create table
create_chall_table <- function(table_data) {
  
  table_data %>% 
    reactable(
      filterable = TRUE,
      defaultPageSize = 25,
      striped = FALSE,
      borderless = TRUE,
      showSortIcon = TRUE,
      resizable = TRUE,
      bordered = FALSE,
      searchable = FALSE,
      defaultColDef = colDef(align = "left"),
      style = list(fontFamily = "Karla", fontWeight = 500),
      theme = reactableTheme(
        headerStyle = list(
          "&:hover[aria-sort]" = list(background = "hsl(0, 0%, 96%)"),
          "&[aria-sort='ascending'], &[aria-sort='descending']" = list(background = "hsl(0, 0%, 96%)"),
          borderColor = "#555"
        ),
      ),
      columns = list(
        official_nba_com_video = colDef(
          cell = function(value) {
            htmltools::tags$a(href = value, target = "_blank", "Video")
          },
          name = "Video Link",
          width = 95,
          filterable = FALSE,
          sortable = FALSE
        ),
        challenging_team = colDef(
          html = TRUE,
          name = "Challenger",
          width = 125
        ),
        date_game = colDef(
          name = "Game Date", 
          width = 115
        ),
        opponent = colDef(
            html = TRUE,
            name = "Opponent",
            width = 125
        ),
        initial_call = colDef(
          name = "Initial Call",
          width = 155,
          html = TRUE,
          header = with_tooltip("Initial Call", "Call on the floor by the ref"),
          cell = function(value) {
            if (value %like% 'Possession') {
              
              value <- gsub("Possession", "Poss", value)
              
              class <- paste0("tag status-posses")
              div(class = class, value)
              
            } else if (value %like% 'Foul' & value != "Offensive Foul") {
              
              class <- paste0("tag status-foul")
              div(class = class, value)
              
            } else if (value %like% 'Basket' | value %like% 'Goal') {
              
              class <- paste0("tag status-goal")
              div(class = class, value)
              
            } else if (value %like% 'Jump') {
              
              class <- paste0("tag status-jump")
              div(class = class, value)
              
            } else if (value == "Offensive Foul") {
              
              class <- paste0("tag status-off")
              div(class = class, value)
              
            } else
              value
          }
        ),
        challenge_outcome = colDef(
          name = "Outcome",
          width = 135,
          html = TRUE,
          header = with_tooltip("Outcome", "Was the challenge successful?"),
          cell = function(value) {
            class <-
              paste0("tag status-", tolower(substr(
                value, nchar(value) - 2, nchar(value)
              )))
            div(class = class, value)
          }
        ),
        quarter = colDef(name = "Period", width = 84),
        cl = colDef(name = "Game Clock", width = 115),
        score_margin = colDef(
          name = "Margin",
          width = 85,
          cell = function(value) {
            if (value > 0) {
              value <- paste0("+", value)
              class <-
                paste0("tag status-pos")
              div(class = class, value)
            } else if (value < 0) {
              class <- paste0("tag status-neg")
              div(class = class, value)
            } else {
              class <- paste0("tag status-even")
              div(class = class, value)
            }
            
          }
        ),
        challenger_pbp_wp = colDef(
          name = "C.T. WP",
          width = 80,
          filterable = F,
          header = with_tooltip("C.T. WP", "Challenging team's win probability at time of challenge"),
          style = function(value) {
            value_normal <-
              normalize_col_vec(table_data, 'challenger_pbp_wp', value)
            pal <-
              pal_react(value_normal)
            list(color = '#111', background = pal)
          },
          class = "border-left",
          cell = function(value) {
            paste0(percent(value, accuracy = .1))
          }
        ),
        score_change_poss = colDef(
          name = "S.C.P.",
          width = 80,
          header = with_tooltip("S.C.P.", "Whether the possession could have resulted in a score change or not"),
          cell = function(value) {
            class <- paste0("tag status-", tolower(value))
            div(class = class, value)
            
          }
        ),
        raw_diff = colDef(
          name = html("&Delta;"),
          html = T,
          width = 75,
          filterable = F,
          header = with_tooltip("&Delta;", "Potential change in WP un-adjusted for team strength"),
          align = "center",
          style = function(value) {
            value_normal <- normalize_col_vec(table_data, 'raw_diff', value)
            pal <- pal_react(value_normal)
            list(color = '#111', background = pal)
          },
          cell = function(value) {
            paste0(percent(value, accuracy = .1))
          },
          class = "border-left"
        ),
        adj_diff = colDef(
          name = html("Adj &Delta;"),
          html = T,
          width = 75,
          header = with_tooltip("Adj &Delta;", "Potential change in WP adjusted for team strength"),
          filterable = F,
          style = function(value) {
            value_normal <- normalize_col_vec(table_data, 'adj_diff', value)
            pal <- pal_react(value_normal)
            list(color = '#111', background = pal)
          },
          cell = function(value) {
            paste0(percent(value, accuracy = .1))
            
          },
          class = "border-left"
        ),
        garb = colDef(show = F)
      )
    )
  
}

# --------------- #
# --- Helpers --- #
# --------------- #

# Read in team logos
logos <- read_csv(
  "data/nba_team_logo_urls.csv"
)

# Read in challenges
challs <- read_csv(
  "data/challenges.csv", 
  guess_max = 0
) %>% 
  mutate(
    # Adjust some columns
    cl = substr(cl, 1, nchar(cl) - 3),
    opponent = ifelse(
      challenging_team == home_team,
      visit_team,
      home_team
    ),
    score_margin = ifelse(
      challenging_team == home_team,
      as.numeric(hs) - as.numeric(vs),
      as.numeric(vs) - as.numeric(hs)
    ),
    raw_diff = round(as.numeric(raw_diff), 3),
    adj_diff = round(as.numeric(adj_diff), 3),
    challenger_pbp_wp = round(as.numeric(challenger_pbp_wp), 3),
    date_game = mdy(date_game)
  ) %>%
  # Join logos
  left_join(
    logos %>% select(challenging_team = team_abr, chall_logo = logo_url),
    by = "challenging_team"
  ) %>% 
  left_join(
    logos %>% select(opponent = team_abr, opp_logo = logo_url),
    by = "opponent"
  ) %>% 
  mutate(
    challenging_team = paste0(
      "<img src='", chall_logo,"' height='35'>", challenging_team
    ),
    opponent = paste0(
      "<img src='", opp_logo,"' height='35'>", opponent
    ),
  ) %>% 
  select(
    official_nba_com_video, date_game, challenging_team, opponent,
    initial_call, challenge_outcome, quarter, cl, score_margin,
    challenger_pbp_wp, score_change_poss, raw_diff, adj_diff, garb
  ) %>%
  arrange(
    desc(adj_diff)
  )

# CSV export data
csv_export_dat <- challs %>% 
  mutate(
    challenging_team = gsub(".*>", "", challenging_team),
    opponent = gsub(".*>", "", opponent)
  ) %>% 
  rename(
    garbage_time = garb,
    game_clock = cl,
    video_link = official_nba_com_video
  )
