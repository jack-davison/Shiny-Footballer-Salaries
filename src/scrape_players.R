library(rvest)
library(dplyr)

all_links <-
  read_html("https://salarysport.com/football") |>
  html_elements("a") |>
  html_attr("href")

leagueone_links <- all_links[grepl("/league-one/|/premier-league/|/championship/|/league-two/", all_links)]

read_players <- function(link) {
  league <- strsplit(link, split = "/")[[1]][3]
  city <- strsplit(link, split = "/")[[1]][4]
  read_html(paste("https://salarysport.com", link[1], sep = "")) |>
    html_table() |>
    bind_rows() |>
    mutate(league = league,
           city = city,
           .before = everything())
}

players <-
  purrr::map(leagueone_links, purrr::possibly(read_players), .progress = TRUE) |>
  bind_rows() |>
  janitor::clean_names() |>
  filter(is.na(team),
         player_name != "") |>
  janitor::remove_empty("cols") |>
  mutate(across(weekly_wage:yearly_salary, readr::parse_number))

readr::write_csv(distinct(players), "data/players.csv")
