library(tidyverse)
library(tabulizer)
library(janitor)
library(gtools)
library(data.table)

dat <- extract_tables("https://ak-static.cms.nba.com/wp-content/uploads/sites/4/2021/05/Coachs-Challenge-reviews-1.pdf",
                      method = "stream", guess = T, columns = list(c(11)))

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

df$date <- as.Date(df$date, format =  "%m/%d/%Y")


first_page <- df %>% 
  filter(date < "2021-01-01") %>% 
  mutate(away_team = trimws(substr(away_t_heoame_t_ineiatmial_call_challenged, 1, 4),"right"),
         home_team = substr(away_t_heoame_t_ineiatmial_call_challenged, 5, 7),
         initial_call = substr(away_t_heoame_t_ineiatmial_call_challenged, 9, nchar(away_t_heoame_t_ineiatmial_call_challenged)-4),
         challenging_team = substr(away_t_heoame_t_ineiatmial_call_challenged, nchar(away_t_heoame_t_ineiatmial_call_challenged)-2, nchar(away_t_heoame_t_ineiatmial_call_challenged)),
         challenging_team = case_when(away_t_heoame_t_ineiatmial_call_challenged == "NYK CLE Home Team PosseNssYioKn" ~ "NYK",
                                      away_t_heoame_t_ineiatmial_call_challenged == "CHA DAL Home Team PosseCssHioAn" ~ "CHA",
                                      away_t_heoame_t_ineiatmial_call_challenged == "CLE IND Home Team PosseCssLiEon" ~ "CLE",
                                      TRUE ~ challenging_team)) %>% 
  mutate(initial_call = case_when(initial_call %like% "Home Team" ~ "Home Team Possession",
                                  TRUE ~ initial_call)) %>% 
  select(date, away_team, home_team, initial_call, challenging_team,
                challenge_outcome, period, game_clock, official_nba_com_video)


final_df <- df %>% 
  filter(date >= "2021-01-01") %>% 
  mutate(challenging_team = case_when(period != "" ~ period,
                                      TRUE ~ substr(challenge_outcome, nchar(challenge_outcome)-2, nchar(challenge_outcome))),
         challenging_team = case_when(challenging_team == "eET" ~ "DET",
                                      challenging_team == "eOS" ~ "BOS",
                                      challenging_team == "eYK" ~ "NYK",
                                      challenging_team == "Lon" ~ "LAL",
                                      challenging_team == "oAn" ~ "CHA",
                                      challenging_team == "oLn" ~ "LAL",
                                      challenging_team == "oSn" ~ "BOS",
                                      challenging_team == "oTn" ~ "DET",
                                      challenging_team == "oXn" ~ "PHX",
                                      TRUE ~ challenging_team),
         initial_call = substr(challenge_outcome, 1, nchar(challenge_outcome)-4),
         period = official_nba_com_video,
         challenge_outcome = game_clock,
         game_clock = na,
         official_nba_com_video = na_2,
         away_team = away_t_heoame_t_ineiatmial_call_challenged,
         home_team = x
         ) %>% 
  select(date, away_team, home_team, initial_call, challenging_team,
         challenge_outcome, period, game_clock, official_nba_com_video) %>% 
  bind_rows(first_page) %>% 
  mutate(initial_call = case_when(initial_call %like% "Home Team Poss" ~ "Home Team Possession",
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
                                  TRUE ~ initial_call),
         period = case_when(date == "2021-02-12" & away_team == "DET" & challenging_team == "BOS" ~ "4",
                            TRUE ~ period))




# testing w pbp ---------------------------------------------------------

ids <- unique(pbp %>% pull(game_id))

team_slugs <- game_logs(seasons=2021, result_types = 'team') %>% 
  group_by(idGame) %>% 
  slice(n()) %>% 
  mutate(home_team = slugTeam,
         visit_team = slugOpponent) %>% 
  select(idGame, home_team, visit_team)

pbp <- pbp %>% 
  left_join(team_slugs, by = c("game_id" = "idGame")) %>% 
  mutate(cl = case_when(substr(cl, 1,2) == "00" ~ substr(cl, 1, nchar(cl) -2),
                        TRUE ~ cl))


clean_pbp <- pbp %>% 
  mutate(de = case_when(date_game == "2021-01-04" & visit_team == "BOS" & evt == 684 ~ "Instant Replay - Challenge - Ruling Stands",
                        date_game == "2021-01-06" & visit_team == "CHA" & evt == 598 ~ "Instant Replay - Challenge - Ruling Stands",
                        date_game == "2021-02-05" & visit_team == "CHI" & evt == 517 ~ "Instant Replay - Challenge - Ruling Stands",
                        date_game == "2021-02-27" & visit_team == "UTA" & evt == 575 ~ "Instant Replay - Challenge - Ruling Stands",
                        date_game == "2021-02-17" & visit_team == "HOU" & evt == 664 ~ "Instant Replay - Challenge - Overturn Ruling",
                        
                        TRUE ~ de))


all_challenges <- clean_pbp %>% 
  filter(de %like% 'Instant Replay - Challenge - Overturn Ruling' |
           de %like% 'Instant Replay - Challenge - Ruling Stands' |
           de %like% 'Instant Replay - Challenge - Support Ruling')
  



clean_df <- final_df %>% 
  filter(!(date == "2021-02-23" & challenging_team == "NYK")) %>% 
  mutate(game_clock = case_when(date == "2021-01-07" & away_team == "DAL" ~ "00:03.0",
                                date == "2021-01-16" & away_team == "DET" ~ "03:41.0",
                                date == "2021-01-18" & away_team == "GSW" & challenging_team == "GSW" ~ "11:33.0",
                                date == "2021-01-18" & away_team == "GSW" & challenging_team == "LAL" ~ "04:31.0",
                                date == "2021-01-20" & away_team == "PHX" & challenging_team == "HOU" ~ "08:47.0",
                                date == "2021-01-21" & away_team == "LAL" ~ "10:17.0",
                                date == "2021-01-25" & away_team == "CHA" ~ "01:45.0",
                                date == "2021-01-25" & away_team == "TOR" & challenging_team == "TOR" ~ "03:02.0",
                                date == "2021-01-25" & away_team == "LAL" & challenging_team == "CLE" ~ "07:01.0",
                                date == "2021-01-28" & away_team == "GSW" & challenging_team == "GSW" ~ "07:52.0",
                                date == "2021-01-29" & away_team == "IND" ~ "07:31.0",
                                date == "2021-01-29" & away_team == "DAL" ~ "04:15.0",
                                date == "2021-01-30" & away_team == "MIL" ~ "06:25.0",
                                date == "2021-01-31" & away_team == "LAC" ~ "04:43.0",
                                date == "2021-01-31" & away_team == "BKN" ~ "08:21.0",
                                date == "2021-02-05" & away_team == "WAS" ~ "09:10.0",
                                date == "2021-02-05" & away_team == "CHI" ~ "08:32.0",
                                date == "2021-02-06" & away_team == "TOR" ~ "09:31.0",
                                date == "2021-02-08" & away_team == "WAS" ~ "10:17.0",
                                date == "2021-02-08" & away_team == "GSW" ~ "02:10.0",
                                date == "2021-02-10" & away_team == "NOP" ~ "06:04.0",
                                date == "2021-02-11" & away_team == "IND" & challenging_team == "DET" ~ "08:42.0",
                                date == "2021-02-11" & away_team == "MIA" ~ "01:02.0",
                                date == "2021-02-12" & away_team == "DET" & challenging_team == "DET" ~ "10:50.0",
                                date == "2021-02-13" & away_team == "PHI" ~ "07:55.0",
                                date == "2021-02-14" & away_team == "MIL" ~ "06:44.0",
                                date == "2021-02-17" & away_team == "NYK" & challenging_team == "NYK" ~ "04:30.0",
                                date == "2021-02-17" & away_team == "DEN" ~ "07:02.0",
                                date == "2021-02-18" & away_team == "TOR" & challenging_team == "MIL" ~ "04:24.0",
                                date == "2021-02-18" & away_team == "MIA" ~ "08:44.0",
                                date == "2021-02-20" & away_team == "WAS" ~ "03:37.0",
                                date == "2021-02-21" & away_team == "BOS" & period == 2 ~ "06:02.0",
                                date == "2021-02-21" & away_team == "BOS" & period == 5 ~ "02:20.0",
                                date == "2021-02-24" & away_team == "TOR" & challenging_team == "MIA" ~ "01:18.0",
                                date == "2021-02-24" & away_team == "GSW" ~ "00:19.0",
                                date == "2021-02-24" & away_team == "CHA" & challenging_team == "CHA" ~ "00:32.0",
                                date == "2021-02-25" & away_team == "SAC" & challenging_team == "SAC" ~ "09:06.0",
                                date == "2021-02-27" & away_team == "DEN" ~ "07:34.0",
                                date == "2021-02-27" & away_team == "UTA" ~ "06:05.0",
                                date == "2021-02-27" & away_team == "NOP" ~ "02:40.0",
                                date == "2021-02-28" & away_team == "NYK" ~ "00:36.0",
                                date == "2021-02-28" & away_team == "CHA" ~ "00:25.0",
                                date == "2021-03-02" & away_team == "LAC" & challenging_team == "BOS" ~ "00:28.0",
                                date == "2021-03-03" & away_team == "UTA" ~ "02:16.0",
                                date == "2021-03-03" & away_team == "LAL" ~ "04:04.0",
                                date == "2021-03-04" & away_team == "TOR" ~ "03:25.0",
                                date == "2021-03-11" & away_team == "ORL" ~ "05:40.0",
                                date == "2021-03-11" & away_team == "DAL"  & challenging_team == "DAL" ~ "03:33.0",
                                date == "2021-03-11" & away_team == "PHX" ~ "01:10.0",
                                date == "2021-03-13" & away_team == "POR" & challenging_team == "MIN" ~ "03:02.0",
                                date == "2021-03-14" & away_team == "POR" ~ "01:06.0",
                                date == "2021-03-15" & away_team == "MIL" ~ "01:58.0",
                                date == "2021-03-16" & away_team == "OKC" ~ "08:55.0",
                                date == "2021-03-16" & away_team == "MIN" ~ "06:21.0",
                                date == "2021-03-19" & away_team == "SAC" ~ "01:41.0",
                                date == "2021-03-19" & away_team == "UTA" ~ "01:25.0",
                                date == "2021-03-20" & away_team == "GSW" ~ "08:24.0",
                                date == "2021-03-22" & away_team == "CHA" & challenging_team == "SAS" ~ "04:52.0", 
                                date == "2021-03-22" & away_team == "TOR" & challenging_team == "HOU" ~ "03:57.0",
                                date == "2021-03-22" & away_team == "TOR" & challenging_team == "TOR" ~ "03:47.0",
                                date == "2021-03-23" & away_team == "LAL" ~ "09:29.0",
                                date == "2021-03-24" & away_team == "BOS" & challenging_team == "BOS" ~ "00:10.0",
                                date == "2021-03-24" & away_team == "BOS" & challenging_team == "MIL" ~ "01:21.0",
                                date == "2021-03-24" & away_team == "DET" ~ "00:02.0",
                                date == "2021-03-24" & away_team == "DEN" ~ "08:34.0",
                                date == "2021-03-25" & away_team == "WAS" ~ "07:30.0",
                                date == "2021-03-26" & away_team == "BKN" & challenging_team == "DET" ~ "05:06.0",
                                date == "2021-03-26" & away_team == "BKN" & challenging_team == "BKN" ~ "06:14.0",
                                date == "2021-03-26" & away_team == "BOS" ~ "08:44.0",
                                date == "2021-03-27" & away_team == "CHI" ~ "07:47.0",
                                date == "2021-03-27" & away_team == "BOS" & challenging_team == "OKC" ~ "05:26.0",
                                date == "2021-03-27" & away_team == "PHI" ~ "05:27.0",
                                date == "2021-03-29" & away_team == "DAL" ~ "10:59.0",
                                date == "2021-03-30" & away_team == "CHA" ~ "07:29.0",
                                date == "2021-03-31" & away_team == "DAL" ~ "05:11.0",
                                date == "2021-03-31" & away_team == "POR" ~ "03:09.0",
                                date == "2021-03-31" & away_team == "MIL" ~ "10:29.0",
                                date == "2021-04-01" & away_team == "ATL" & challenging_team == "ATL" ~ "05:20.0",
                                date == "2021-04-02" & away_team == "DAL" ~ "11:04.0",
                                date == "2021-04-02" & away_team == "MIN" & challenging_team == "MIN" ~ "04:37.0",
                                date == "2021-04-02" & away_team == "MIN" & challenging_team == "MEM" ~ "04:27.0",
                                date == "2021-04-02" & away_team == "ATL" ~ "09:07.0",
                                date == "2021-04-04" & away_team == "LAL" ~ "06:17.0",
                                date == "2021-04-04" & away_team == "GSW" ~ "00:38.0",
                                date == "2021-04-04" & away_team == "ORL" ~ "02:37.0",
                                date == "2021-04-05" & away_team == "SAC" & challenging_team == "SAC" ~ "04:05.0",
                                date == "2021-04-05" & away_team == "SAC" & challenging_team == "MIN" ~ "01:47.0",
                                date == "2021-04-06" & away_team == "LAL" & challenging_team == "LAL" ~ "07:10.0",
                                date == "2021-04-06" & away_team == "MIL" ~ "02:08.0",
                                date == "2021-04-07" & away_team == "DAL" ~ "03:35.0",
                                date == "2021-04-08" & away_team == "MIL" & challenging_team == "MIL" ~ "05:44.0",
                                date == "2021-04-08" & away_team == "POR" & challenging_team == "POR" ~ "05:59.0",
                                date == "2021-04-08" & away_team == "PHX" ~ "03:17.0",
                                date == "2021-04-09" & away_team == "PHI" ~ "01:32.0",
                                date == "2021-04-09" & away_team == "MEM" ~ "09:03.0",
                                date == "2021-04-11" & away_team == "CHI" ~ "03:24.0",
                                date == "2021-04-12" & away_team == "LAL" ~ "04:38.0",
                                date == "2021-04-12" & away_team == "SAC" ~ "02:56.0",
                                date == "2021-04-13" & away_team == "MIA" ~ "10:26.0",
                                date == "2021-04-14" & away_team == "BKN" ~ "01:04.0",
                                date == "2021-04-16" & away_team == "OKC" & challenging_team == "DET" ~ "03:10.0",
                                date == "2021-04-16" & away_team == "NOP" & challenging_team == "WAS" ~ "03:43.0",
                                date == "2021-04-17" & away_team == "GSW" ~ "03:30.0", 
                                date == "2021-04-18" & away_team == "NOP" ~ "00:30.0",
                                date == "2021-04-18" & away_team == "HOU" ~ "10:00.0",
                                date == "2021-04-18" & away_team == "POR" ~ "03:33.0",
                                date == "2021-04-19" & away_team == "SAS" ~ "07:36.0",
                                date == "2021-04-19" & away_team == "CLE" ~ "05:42.0",
                                date == "2021-04-19" & away_team == "MEM" ~ "01:40.0",
                                date == "2021-04-21" & away_team == "MIA" ~ "00:47.0",
                                date == "2021-04-23" & away_team == "CLE" ~ "10:47.0",
                                date == "2021-04-25" & away_team == "CLE" & challenging_team == "CLE" ~ "10:28.0",
                                date == "2021-04-25" & away_team == "CLE" & challenging_team == "WAS" ~ "00:38.0",
                                date == "2021-04-26" & away_team == "LAC" ~ "04:44.0",
                                date == "2021-04-27" & away_team == "BKN" ~ "01:42.0",
                                date == "2021-04-28" & away_team == "SAS" & challenging_team == "MIA" ~ "07:53.0",
                                date == "2021-04-29" & away_team == "GSW" ~ "04:21.0",
                                date == "2021-04-29" & away_team == "NOP" ~ "01:09.0",
                                date == "2021-05-01" & away_team == "GSW" ~ "02:26.0",
                                date == "2021-05-02" & away_team == "SAC" & challenging_team == "SAC" ~ '01:36.0',
                                date == "2021-05-02" & away_team == "SAC" & challenging_team == "DAL" ~ "07:48.0",
                                date == "2021-05-02" & away_team == "PHX" & challenging_team == "OKC" ~ "05:47.0",
                                date == "2021-05-02" & away_team == "PHI" & challenging_team == "PHI" ~ "02:18.0",
                                date == "2021-05-02" & away_team == "PHI" & challenging_team == "SAS" ~ "01:08.0",
                                date == "2021-05-04" & away_team == "CHA" ~ "00:09.0",
                                date == "2021-05-05" & away_team == "WAS" ~ "03:47.0",
                                date == "2021-05-06" & away_team == "BKN" ~ "00:06.0",
                                date == "2021-05-06" & away_team == "OKC" ~ "03:04.0",
                                date == "2021-05-10" & away_team == "IND" ~ "00:33.0",
                                date == "2021-05-10" & away_team == "HOU" ~ "03:47.0",
                                date == "2021-05-11" & away_team == "ORL" ~ "03:57.0",
                                date == "2021-05-12" & away_team == "POR" & challenging_team == "UTA" ~ "00:27.0",
                                date == "2021-05-12" & away_team == "POR" & challenging_team == "POR" ~ "05:26.0",
                                date == "2021-05-13" & away_team == "LAC" ~ "08:40.0",
                                date == "2021-05-13" & away_team == "PHI" ~ "04:50.0",
                                date == "2021-05-14" & away_team == "NOP" ~ "04:32.0",
                                date == "2021-05-14" & away_team == "LAC" ~ "01:40.0",
                                date == "2021-05-15" & away_team == "CHA" ~ "07:28.0",
                                date == "2021-05-16" & away_team == "MEM" ~ "02:56.0",
                                date == "2020-12-22" & away_team == "LAC" & challenging_team == "LAL" ~ "00:52.0",
                                date == "2020-12-22" & away_team == "LAC" & challenging_team == "LAC" ~ "04:32.0",
                                date == "2020-12-25" & away_team == "BKN" ~ "07:08.0",
                                date == "2020-12-27" & away_team == "ORL" & challenging_team == "ORL" ~ "02:56.0",
                                date == "2020-12-27" & away_team == "ORL" & challenging_team == "WAS" ~ "00:13.0",
                                date == "2020-12-31" & away_team == "NOP" ~ "11:07.0",
                                
                                
                                TRUE ~ game_clock),
         period = case_when(date == "2021-02-12" & away_team == "DET" & challenging_team == "BOS" ~ "4",
                            TRUE ~ period))



nba_df <- clean_df %>% 
  mutate(game_clock = substr(game_clock, 1, nchar(game_clock) -2)) %>% 
  left_join(all_challenges, by = c("date" = "date_game",
                            "away_team" = "visit_team",
                            "home_team" = "home_team", 
                            "period" = "quarter",
                            "game_clock" = "cl")) %>% 
  mutate(challenge_outcome = case_when(date == "2021-03-26" & challenging_team == "BKN" ~ "Challenge Won",
                                       date == "2021-02-28" & challenging_team == "HOU" ~ "Challenge Lost",
                                       date == "2021-03-26" & challenging_team == "MIL" ~ "Challenge Lost",
                                       date == "2020-12-26" & challenging_team == "TOR" ~ "Challenge Lost",
                                       date == "2021-01-22" & challenging_team == "PHX" ~ "Challenge Lost",
                                       date == "2021-03-04" & challenging_team == "TOR" ~ "Challenge Lost",
                                       date == "2021-05-05" & challenging_team == "MIL" ~ "Challenge Lost",
                                       T ~ challenge_outcome))




