This tutorial scrapes and cleans the NBA’s Coaching Challenge data from
the 2020-2021 regular season at
<a href="https://ak-static.cms.nba.com/wp-content/uploads/sites/4/2021/05/Coachs-Challenge-reviews-1.pdf" class="uri">https://ak-static.cms.nba.com/wp-content/uploads/sites/4/2021/05/Coachs-Challenge-reviews-1.pdf</a>

Load in necessary packages

    library(tidyverse)
    library(tabulizer)
    library(janitor)
    library(gtools)
    library(data.table)

Extract the tables from the pdf. The NBA unfortunately formats this data
as a pdf of an excel sheet, so there is work to be done to scrape and
clean it.

    dat <- extract_tables("https://ak-static.cms.nba.com/wp-content/uploads/sites/4/2021/05/Coachs-Challenge-reviews-1.pdf",
                          method = "stream", guess = T, columns = list(c(11)))

Here we are binding all of the 14 pages of the pdf, adding column names,
and cleaning up the date column.

    df <- smartbind(as.data.frame(dat[1]), as.data.frame(dat[2]),
                    as.data.frame(dat[3]), as.data.frame(dat[4]),
                    as.data.frame(dat[5]), as.data.frame(dat[6]),
                    as.data.frame(dat[7]), as.data.frame(dat[8]),
                    as.data.frame(dat[9]), as.data.frame(dat[10]),
                    as.data.frame(dat[11]), as.data.frame(dat[12]),
                    as.data.frame(dat[13]), as.data.frame(dat[14]))

    df <- df %>% 
      row_to_names(row_number = 1) %>%
      clean_names() 

    ## Warning in row_to_names(., row_number = 1): Row 1 does not provide unique names.
    ## Consider running clean_names() after row_to_names().

    df$date <- as.Date(df$date, format =  "%m/%d/%Y")

The first page required additional parsing methods, so we will save it
as a separate dataframe before combining it with the rest.

    first_page <- df %>%
      filter(date < "2021-01-01") %>%
      mutate(
        away_team = trimws(
          substr(away_t_heoame_t_ineiatmial_call_challenged, 1, 4),
          "right"
        ),
        home_team = substr(away_t_heoame_t_ineiatmial_call_challenged, 5, 7),
        initial_call = substr(
          away_t_heoame_t_ineiatmial_call_challenged,
          9,
          nchar(away_t_heoame_t_ineiatmial_call_challenged) - 4
        ),
        challenging_team = substr(
          away_t_heoame_t_ineiatmial_call_challenged,
          nchar(away_t_heoame_t_ineiatmial_call_challenged) - 2,
          nchar(away_t_heoame_t_ineiatmial_call_challenged)
        ),
        challenging_team = case_when(
          away_t_heoame_t_ineiatmial_call_challenged == "NYK CLE Home Team PosseNssYioKn" ~ "NYK",
          away_t_heoame_t_ineiatmial_call_challenged == "CHA DAL Home Team PosseCssHioAn" ~ "CHA",
          away_t_heoame_t_ineiatmial_call_challenged == "CLE IND Home Team PosseCssLiEon" ~ "CLE",
          TRUE ~ challenging_team
        )
      ) %>%
      mutate(
        initial_call = case_when(
          initial_call %like% "Home Team" ~ "Home Team Possession",
          TRUE ~ initial_call
        )
      ) %>%
      select(
        date,
        away_team,
        home_team,
        initial_call,
        challenging_team,
        challenge_outcome,
        period,
        game_clock,
        official_nba_com_video
      )

WARNING: Very large case\_when statement ahead. Due to the format of the
data, as you may have gathered from the previous chunk, there are a lot
of typos and discrepancies when parsing this pdf file. Below creates the
final clean data frame. Enjoy!

    final_df <- df %>%
      filter(date >= "2021-01-01") %>%
      mutate(
        challenging_team = case_when(
          period != "" ~ period,
          TRUE ~ substr(
            challenge_outcome,
            nchar(challenge_outcome) - 2,
            nchar(challenge_outcome)
          )
        ),
        challenging_team = case_when(
          challenging_team == "eET" ~ "DET",
          challenging_team == "eOS" ~ "BOS",
          challenging_team == "eYK" ~ "NYK",
          challenging_team == "Lon" ~ "LAL",
          challenging_team == "oAn" ~ "CHA",
          challenging_team == "oLn" ~ "LAL",
          challenging_team == "oSn" ~ "BOS",
          challenging_team == "oTn" ~ "DET",
          challenging_team == "oXn" ~ "PHX",
          TRUE ~ challenging_team
        ),
        initial_call = substr(challenge_outcome, 1, nchar(challenge_outcome) -
                                4),
        period = official_nba_com_video,
        challenge_outcome = game_clock,
        game_clock = na,
        official_nba_com_video = na_2,
        away_team = away_t_heoame_t_ineiatmial_call_challenged,
        home_team = x
      ) %>%
      select(
        date,
        away_team,
        home_team,
        initial_call,
        challenging_team,
        challenge_outcome,
        period,
        game_clock,
        official_nba_com_video
      ) %>%
      bind_rows(first_page) %>%
      mutate(
        initial_call = case_when(
          initial_call %like% "Home Team Poss" ~ "Home Team Possession",
          initial_call %like% "Away Team Poss" ~ "Away Team Possession",
          initial_call %like% "Basket Interferenc" ~ "Basket Interference",
          initial_call %like% "Goalten" ~ "Goaltending",
          initial_call %like% "Jump" ~ "Jump Ball",
          initial_call %like% "Loose Ball" ~ "Loose Ball Foul",
          initial_call %like% "Offensive" ~ "Offensive Foul",
          initial_call %like% "Shooting" ~ "Shooting Foul",
          initial_call %like% "Personal" ~ "Personal Foul",
          initial_call %like% "Defensive" ~ "Defensive Foul",
          initial_call %like% "Loose Ball" ~ "Loose Ball Foul",
          initial_call %like% "Home Team" ~ "Home Team Possession",
          initial_call %like% "Away Team" ~ "Away Team Possession",
          TRUE ~ initial_call
        ),
        period = case_when(
          date == "2021-02-12" &
            away_team == "DET" & challenging_team == "BOS" ~ "4",
          TRUE ~ period
        )
      )

    head(final_df)

    ##           date away_team home_team   initial_call challenging_team
    ## 2:1 2021-01-01       BOS       DET Offensive Foul              BOS
    ## 2:2 2021-01-01       MIA       DAL  Personal Foul              MIA
    ## 2:3 2021-01-01       MEM       CHA  Personal Foul              CHA
    ## 2:4 2021-01-01       PHX       DEN Offensive Foul              DEN
    ## 2:5 2021-01-01       LAC       UTA  Personal Foul              LAC
    ## 2:6 2021-01-01       PHX       DEN  Personal Foul              PHX
    ##     challenge_outcome period game_clock
    ## 2:1    Challenge Lost      3    03:33.0
    ## 2:2    Challenge Lost      3    02:07.0
    ## 2:3     Challenge Won      4    10:19.0
    ## 2:4    Challenge Lost      3    02:01.0
    ## 2:5    Challenge Lost      4    00:20.4
    ## 2:6     Challenge Won      4    00:31.8
    ##                                                                official_nba_com_video
    ## 2:1 https://dmmx2.nba.com/videourlredirect?&project=/opsrep/trigger16_bosdet_1_010121
    ## 2:2 https://dmmx2.nba.com/videourlredirect?&project=/opsrep/trigger16_miadal_1_010121
    ## 2:3 https://dmmx2.nba.com/videourlredirect?&project=/opsrep/trigger16_memcha_1_010121
    ## 2:4 https://dmmx2.nba.com/videourlredirect?&project=/opsrep/trigger16_phxden_1_010121
    ## 2:5 https://dmmx2.nba.com/videourlredirect?&project=/opsrep/trigger16_lacuta_1_010121
    ## 2:6 https://dmmx2.nba.com/videourlredirect?&project=/opsrep/trigger16_phxden_2_010121
