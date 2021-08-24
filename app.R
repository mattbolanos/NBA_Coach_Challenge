library(tidyverse)
library(extrafont)
library(shinyWidgets)
library(shiny)
library(htmltools)
library(future)
library(furrr)
library(RPostgres)
library(DBI)
library(shinycssloaders)
library(viridis)
library(lubridate)
library(reactable)
library(gt)
library(scales)
library(data.table)
library(paletteer)
library(shinythemes)

plan('multicore')

#
db <- ''

host_db <- ''
#
db_port <- ''
#
db_user <- ''
#
db_password <- '!'
#
con <-
  dbConnect(
    RPostgres::Postgres(),
    dbname = db,
    host = host_db,
    port = db_port,
    user = db_user,
    password = db_password
  )



challs <- dbGetQuery(con, "SELECT * FROM nba_2021_challenges")



ui <- navbarPage(
  theme = shinytheme("yeti"),
  "NBA Coach's Challenges",
  #WRITING
  tabPanel(
    "Value of a Challenge",
    strong("What is the Value of a NBA Coach's Challenge?",
           style = "font-size:35px; font-family:Georgia; color:black"),
    br(),
    p(
      "By Matt Bolaños | matthew.a.bolanos@gmail.com |",
      a("Portfolio", href = "https://www.matthewabolanos.com/", target = "_blank"),
      style = "font-size:16px;font-family:Georgia; color:black"
    ),
    strong("Introduction & Method", style = "font-size:24px;font-family:Georgia; color:black; font-style: italic"),
    br(),
    p("As the title suggests, this application looks into the quantitative 
      aspects of NBA coaching challenges. I approached this problem by 
      conceptualizing the value of a challenge as the delta, or difference, 
      between the true challenge outcome and the “counterfactual.” For example, 
      suppose Team X scores, is called for an offensive foul, uses their challenge, 
      and the ruling on the floor stands.  What is the delta between Team X 
      picking up an offensive foul, and Team X having the call overturned and 
      receiving the field goal and free throw for an and-one? I used in-game win 
      probability (WP) as a proxy to measure the probabilities for these two 
      potential outcomes.  I joined each play-by-play event from the 2020-2021 
      regular season with its corresponding WP via nbastatR, and then cross-referenced 
      the NBA's public coach's challenge review data to retrieve more information 
      and video clips for each challenge. I replaced any missing video clips 
      using the NBA's API, and I filled in any discrepancies with Synergy. Once 
      the data was complete and all in one place, I hand-tracked all 649 
      challenges to identify potential score changes, who was shooting the 
      ball (if applicable), and what the counterfactual would have been had 
      the challenge gone the other way. Inpredictable's WP calculator allowed 
      me to calculate the challenging team's WP, based on whether the call stood 
      or was overturned. This tool takes into account the time remaining in 
      the game, the score margin, and which team has possession. For example, 
      suppose, in a 5 point game in the 2nd quarter, Player X drives, makes a 
      layup, and gets called for an offensive foul in a block/charge context 
      which prompts his coach to challenge. The “ruling stands WP” in this case 
      is based on Player X's team losing the ball at this point in the game 
      (00:52 left in 2nd quarter, -5 margin, does not have possession). 
      The “ruling overturned WP” is based on Player X being awarded an 
      and-one opportunity (00:52 left in 2nd quarter, -2 margin, does not have possession).", 
      HTML("&#8239;"),"Another factor I baked into this step was the expected points of the offensive 
      player being fouled (when applicable). In the above example, if Player X 
      shot 50% or better from the free-throw line during the 20-21 season, then 
      on average he would make the one free throw and cut the deficit by another 
      point. I extended this method to all challenges involving a potential score.  
      If Player Y gets fouled on a three-pointer and shoots 80% from the line, 
      we can expect him to make 2 out of 3 (.8 * 3 = 2.4, or 2 after rounding). 
      It is also important to point out here that for challenges made by the 
      team on defense where an overturned call would lead to a jump ball, 
      I calculated the “overturn WP” as if the challenging team had possession. 
      This maximizes the potential delta in WP, which gives us a better idea of 
      the magnitude that an overturned call COULD have if the team also wins the 
      jump ball. One last thing of note is that the WP from nbastatR took into 
      account the “Vegas Spread”, or the projected difference in team strength 
      for that particular game. Inpredictable's tool assumes that both teams 
      are perfectly matched and playing at a neutral site. By “matching” the WP 
      tool to the current game situation (factoring in which team actually has 
      possession), I computed the spread variable. This allowed me to not only 
      calculate the raw delta between the call standing and being overturned, 
      but to also adjust for in-game team strength differences.",
      style = "font-size:19px; font-family:Gill Sans;color:black"
    ),
    p("Coaching challenges are a relatively new addition to the NBA rulebook, 
      and there is still a lot of debate regarding when teams should use them. 
      This project does not attempt to answer that question, for numerous reasons. 
      One, the eye test is the dominating perspective when deciding whether to 
      challenge or not. Most every call is different. There is really no way to 
      quantify what makes up a “bad call”, and the initial motivation to challenge 
      should not be driven by the potential swing in WP. It is also important to consider the other 
      incentives for coaches to challenge. If it is a close game in the 4th 
      quarter and Team X's best player picks up their 6th foul, some coaches 
      would be quick to use their challenge in the hope that it will overturn 
      the call, so that player may stay in the game, regardless of the quality 
      of the call.",HTML("&#8239;"),"Also, there is no guarantee that the “optimal” time to 
      challenge will even occur in any given game. Not every NBA game features 
      a one-point score margin with a questionable call in the final 30 
      seconds, for example. This relates to game theory in some ways, as 
      coaches must make the decision to either challenge on a medium-sized 
      delta WP call in the 2nd quarter (e.g. fouling a 90% free throw shooter 
      on a three) or hold out until the 4th quarter on the chance that there 
      will be a high leverage scenario in which to use their challenge. If a 
      coach does decide to use that challenge in the 2nd quarter, and is 
      unable to challenge a close foul call in the 4th, that coach would 
      almost certainly face scrutiny for not saving the challenge.",HTML("&#8239;"),
      "Analyzing 
      coaching challenges requires removing hindsight bias from the equation, 
      which can be easy to apply when evaluating in-game decision-making. 
      With this topic, I hoped to inch toward answering the question at the top 
      of the screen: 'What is the Value of a Coach's Challenge?' Below are some 
      tables detailing a few insights I drew from the data.",
      style = "font-size:19px; font-family:Gill Sans;color:black"
    ),
    HTML(
      '<center><img src="avg.by.type.png"; width="75%"></center>'
    ),
    br(),
    p("I determined if challenges occurred in garbage time using Ben Falk's definition 
      of garbage time at Cleaning the Glass, explained",
      a("here.", href = "https://cleaningtheglass.com/stats/guide/garbage_time", target =
          "_blank"),
      "Filtering out garbage time only excluded a mere 4 out of the total 
      649 challenges. With 649 being a small sample size to begin with, 
      I did not wish to further reduce the data set too much. The first thing 
      of note is how small most of these bins are, with only defensive and 
      offensive fouls having a decent sample. It makes intuitive sense that 
      offensive fouls had a slightly larger average unadjusted WP change 
      (as opposed to defensive fouls) since offensive fouls that are challenged 
      often can turn into free throws or and-one opportunities if overturned. 
      Moreover, while 99 still leaves a lot to be desired in terms of a population 
      size from which concrete insights may be drawn, a",
      span("+5.5%", style = "font-style: italic; font-weight:bold"),
      "increase in overturn rate from offensive to defensive fouls stood out to 
      me. This relatively high overturn rate paired with strong average deltas 
      results in offensive fouls nabbing the highest expected value of 
      challengeable calls.",HTML("&#8239;"),"As mentioned, the overturn rate bump could be due 
      to the 'bang-bang' nature of block/charge calls, and we might expect 
      more of these calls to be overturned when the officials are given the 
      opportunity to use replay review. Speaking of the danger of reading too 
      much into small samples, challenges used on team possession calls had an 
      astounding overturn rate of",
      span("63.3% ", style = "font-style: italic; font-weight:bold"),
      "over the regular season. This high success rate suggests that teams 
      only challenged possession calls that they were almost certain were 
      miscalled. Regardless, it would appear that at least most of the time, 
      the coach on the bench with the tablet can review the play and accurately 
      determine who should have possession. Compare this to the far more subjective 
      criteria applied to foul calls, in particular, and it makes sense why 
      possession call overturn rates are as high as they are. The below table 
      splits up challenges by game period.",
      style = "font-size:19px; font-family:Gill Sans;color:black"
    ),
    HTML(
      '<center><img src="avg.by.period.png"; width="75%"></center>'
    ),
    br(),
    p("As expected, challenges were most frequent in the 3rd and 4th quarters. 
      One interesting point here is that 56 games during the 20-21 regular 
      season went to at least one overtime. Of those 56 games,",HTML("&#8239;"),
      "ALL of them 
      featured at least one coach's challenge. However, it appears that most of 
      those challenges were used in the preceding quarters, since there were 
      only 15 instances of teams holding out until extended play.", 
      HTML("&#8239;"),"Yet, these 
      15 instances clearly had the largest implications, since they have the 
      highest deltas in WP, more than double the averages from the second-best 
      4th quarter. Since overtime periods are only 5 minutes long and their 
      existence hinges on a tie game after regulation, it makes sense why these 
      averages are so high.", 
      HTML("&#8239;"),"A very interesting trend in this table is the steady 
      decline in challenges used on potential score-changing possessions, 
      even as the average unadjusted delta in WP increases. This passes the 
      smell test given that simply having possession becomes much more important 
      as the game winds down, and coaches are more incentivized to use challenges 
      on non-scoring plays in the late stages. The 4th quarter having the smallest 
      overturn rate potentially suggests a “desparation mode” effect, in which 
      coaches are quicker to challenge calls that are less likely overturned, 
      but do so with the hope of keeping their team's chances in that particular 
      game alive. This hypothesis is supported by the fact that the overall 
      overturn rate on challenges is",
      span("48%,", style = "font-style: italic; font-weight:bold"),
      "leaving the 4th quarter at a",
      span("-5%", style = "font-style: italic; font-weight:bold"),
      "disadvantage. Keeping with the game situation theme, the below table groups challenges by
                     absolute score margin.",
      style = "font-size:19px; font-family:Gill Sans;color:black"
    ),
    HTML(
      '<center><img src="avg.by.margin.png"; width="75%"></center>'
    ),
    br(),
    p("One point that jumps out here is how second half challenge frequency 
      increases as the score margin grows. Upon further review, this is likely 
      due to the notion that leads in the 15-20+ range typically take a decent 
      amount of time to build, such that challenges, when the score difference 
      is large, tend to occur mostly in the second half. On the flip side, it 
      was interesting to see that most score-changing possession challenges 
      occurred in the 0-5 margin bin.", 
      HTML("&#8239;"),"As discussed above, possession-based 
      challenges become more valuable and prevalent as the game progresses 
      into the later quarters. That being said, challenges made late in games 
      do not perfectly correlate with the game being close, which provides some 
      explanation for the high S.C.P. rate. In terms of overall bin size, one 
      can see that coaches opted to save challenges for close contests on average, 
      with only 53 instances happening in 20+ blowout situations. Perhaps due to 
      my psychology undergraduate study, I have always been curious about the 
      cognitive effects that home-court advantage can have on NBA referees. 
      The below graphic explores that concept.",
      style = "font-size:19px; color:black; font-family:Gill Sans"
    ),
    HTML('<center><img src="home.png"; width="48%"></center>'),
    br(),
    p("The overall challenge totals between the home and visiting team were 
      nearly dead even, with the home team and visitor challenging 324 and 325 
      times, respectively.", 
      HTML("&#8239;"),"There was a slight difference, however, in overturn 
      frequency, with the visiting team winning",
      span("46.2% ", style = "font-style: italic; font-weight:bold"),
      " of challenges to the home team's",
      span("50.9% ", style = "font-style: italic; font-weight:bold"),
      "success rate. This is not a large disparity by any means, 
      but it is worth acknowledging. Contrary to the “home cooking” theory, 
      visiting teams had a higher overturn rate in the 4th quarter by a slim margin of",
      span("1.9%.", style = "font-style: italic; font-weight:bold"),
      "Perhaps NBA referees are less home team centric than at first thought. 
      When grouping the challenges by half, the home team regains a slight 
      advantage with an overturn rate",
      span("0.4% ", style = "font-style: italic; font-weight:bold"),
      "higher than the visiting team in the second half.",
      style = "font-size:19px; color:black; font-family:Gill Sans"
    ),
    p("I would like to thank Mike Beuoy at",
      a("Inpredictable ", href = "http://stats.inpredictable.com/nba/wpCalc.php",
        target = "_blank"),
      "for his great work in the WP space and its handy WP calculator.  
      The package nbastatR has always made it very easy to work with NBA data in R. 
      The NBA's data regarding coaching challenges can be found",
      a("here.", href = "https://official.nba.com/2019-20-nba-coachs-challenge-results/",
        target = "_blank"),
      "My interactive coach's challenge database can be found on the next tab 
      of this application. I will leave you with the below graphic splitting 
      challenges by team, so you can see how your favorite team used their 
      challenges this past season.",
      style = "font-size:19px; color:black; font-family:Gill Sans"
    ),
    HTML('<center><img src="team.png"; width="60%"></center>')
    
    
    
    
  ),
  tabPanel(
    "Coach's Challenge DB",
    strong("NBA 2020-21 Regular Season Coach's Challenge Database", style = "font-size:30px;font-family:Futura; color:black"),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
      tags$style(type = "text/css", ".label-default {background-color: #ff8c00;}"),
    ),
    br(),
    strong("Version: 2021-08-23", style = "font-size:15px;font-family:Georgia; color:red"),
    p(
      "Table: Matt Bolaños | Data: nbastatR, inpredicatable, nba.com |",
      a("Spinner", href = "https://gifimage.net/wp-content/uploads/2018/04/loading-gif-orange-4.gif", target =
          "_blank"),
      style = "font-size:15px;font-family:Arial; color:black; font-style: italic;"
    ),
    strong("Glossary", style = "font-size:23px;font-family:Futura; color:black"),
    p(
      "C.T. WP - Challenging team's win probability (WP) at time of challenge" ,
      style = "font-size:15px;font-family:Arial; color:black"
    ),
    p(
      "S.C.P. - Flag of whether the possession could have resulted in a score change or not",
      style = "font-size:15px;font-family:Arial; color:black"
    ),
    p(
      HTML("&Delta;"),
      "- Potential change in WP unadjusted for in-game team strength differences",
      style = "font-size:15px;font-family:Arial; color:black"
    ),
    p(
      HTML("Adj &Delta;"),
      "- Potential change in WP adjusted for team strength",
      style = "font-size:15px;font-family:Arial; color:black"
    ),
    
    div(
      materialSwitch(
        inputId = "garbage",
        label = "Filter out Garbage Time",
        value = T,
        inline = T
      ),
      style = "font-size:16px;font-family:Futura; color:black; font-weight:bold"
    ),
    
    withSpinner(
      reactableOutput("table"),
      image = "https://gifimage.net/wp-content/uploads/2018/04/loading-gif-orange-4.gif",
      image.width = "100px",
      image.height = "100px"
    ),
    tags$script(
      'document.getElementById("table").addEventListener("click", function(event){
                             setTimeout(function(){
                             console.log("rerender")
                                        HTMLWidgets.staticRender()
                             }, 10);
                          })
                           '
    )
  )
)

server <- function(input, output) {
  autoInvalidate <- reactiveTimer(10000)
  observe({
    autoInvalidate()
    cat(".")
  })
  
  
  
  
  output$table <- renderReactable({
    table_data <- challs %>%
      mutate(
        cl = substr(cl, 1, nchar(cl) - 3),
        opponent = case_when(challenging_team == home_team ~ visit_team,
                             T ~ home_team),
        score_margin = case_when(challenging_team == home_team ~ hs -
                                   vs,
                                 T ~ vs - hs),
        across(raw_diff, ~ round(., 3)),
        across(adj_diff, ~ round(., 3)),
        across(challenger_pbp_wp, ~ round(., 3)),
        date_game = mdy(date_game)
      ) %>%
      select(
        official_nba_com_video,
        date_game,
        challenging_team,
        opponent,
        initial_call,
        challenge_outcome,
        quarter,
        cl,
        score_margin,
        challenger_pbp_wp,
        score_change_poss,
        raw_diff,
        adj_diff,
        garb
      ) %>%
      arrange(desc(adj_diff))
    
    if ((input$garbage) == T) {
      table_data <- table_data %>%
        filter(!(garb == T & quarter == 4))
      
    }
    
    
    
    
    make_color_pal <- function(colors, bias = 1) {
      get_color <- colorRamp(colors, bias = bias)
      function(x)
        rgb(get_color(x), maxColorValue = 255)
    }
    normalize_col_vec <- function(dataframe, col, value) {
      x <- dataframe[[col]]
      min <- x %>% min()
      max <- x %>% max()
      res <- (value - min) / (max - min)
      res
    }
    pal_react <-
      viridis::magma(n = 5, begin = 0.65, end = 1) %>% make_color_pal(bias = 2)
    
    reactable(
      table_data,
      filterable = TRUE,
      defaultPageSize = 25,
      compact = F,
      
      borderless = FALSE,
      defaultColDef = colDef(align = "left"),
      style = list(fontFamily = "Tahoma", fontWeight = 600),
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
          name = "Video",
          width = 75,
          filterable = F
        ),
        challenging_team = colDef(
          cell = function(value) {
            image <-
              img(
                src = paste0(
                  "https://stats.nba.com/media/img/teams/logos/",
                  value,
                  "_logo.svg"
                ),
                height = "55px",
                alt = value
              )
            tagList(div(style = list(
              display = "inline-block", width = "65px"
            ), image), value)
          },
          name = "Challenger",
          width = 125,
          JS(
            "function(cellInfo) {
      return '<b>' + cellInfo.value + '</b>'
                }"
          )
        ),
        date_game = colDef(name = "Date", width = 110),
        
        opponent = colDef(
          cell = function(value) {
            image <-
              img(
                src = paste0(
                  "https://stats.nba.com/media/img/teams/logos/",
                  value,
                  "_logo.svg"
                ),
                height = "55px",
                alt = value
              )
            tagList(div(style = list(
              display = "inline-block", width = "65px"
            ), image),
            value)
          },
          name = "Opponent",
          width = 125,
          JS(
            "function(cellInfo) {
      return '<b>' + cellInfo.value + '</b>'
                }"
          )
        ),
        initial_call = colDef(
          name = "Initial Call",
          width = 140,
          html = T,
          cell = function(value) {
            if (value %like% 'Possession') {
              class <- paste0("tag status-posses")
              div(class = class, value)
            } else if (value %like% 'Foul' &
                       value != "Offensive Foul") {
              class <- paste0("tag status-foul")
              div(class = class, value)
            } else if (value %like% 'Basket' |
                       value %like% 'Goal') {
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
          html = T,
          cell = function(value) {
            class <-
              paste0("tag status-", tolower(substr(
                value, nchar(value) - 2, nchar(value)
              )))
            div(class = class, value)
          }
        ),
        quarter = colDef(name = "Period", width = 84),
        cl = colDef(name = "Game Clock", width = 110),
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
    
    
    
    
  })
  
  
  
  
  
  
}

shinyApp(ui = ui, server = server)
