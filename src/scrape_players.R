library(rvest)
library(dplyr)

all_links <-
  read_html("https://salarysport.com/football") |>
  html_elements("a") |>
  html_attr("href")

leagueone_links <- all_links[grepl("league-one", all_links)]

read_players <- function(link) {
  city <-
    gsub("/football/league-one/|/", "", link) |> snakecase::to_title_case()
  read_html(paste("https://salarysport.com", link[1], sep = "")) |>
    html_table() |>
    bind_rows() |>
    mutate(league = "league-one", city = city, .before = everything())
}

players <-
  purrr::map(leagueone_links, read_players, .progress = TRUE) |>
  bind_rows() |>
  janitor::clean_names() |>
  filter(
    is.na(team),
    player_name != ""
  ) |>
  janitor::remove_empty("cols") |>
  mutate(across(weekly_wage:yearly_salary, readr::parse_number))

readr::write_csv(players, "data/players.csv")
