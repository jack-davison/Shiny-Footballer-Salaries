

# Prep --------------------------------------------------------------------

# read in players data
players <- readr::read_csv("data/players.csv")

# fix countries to join w/ the shapefile
players <-
  dplyr::mutate(
    players,
    name = dplyr::case_match(
      nationality,
      "Antigua & Barbuda" ~ "Antigua and Barb.",
      "Czech Republic" ~ "Czech Rep.",
      "DR Congo" ~ "Dem. Rep. Congo",
      "England" ~ "United Kingdom",
      "Gibraltar" ~ "Spain",
      "Holland" ~ "Netherlands",
      "Ivory Coast" ~ "Côte d'Ivoire",
      "N.Ireland" ~ "United Kingdom",
      "Scotland" ~ "United Kingdom",
      "St Kitts & Nevis" ~ "St. Kitts and Nevis",
      "St Lucia" ~ "Saint Lucia",
      "The Gambia" ~ "Gambia",
      "Wales" ~ "United Kingdom",
      "U.S.A." ~ "United States",
      "North Macedonia" ~ "Macedonia",
      "South Korea" ~ "Korea",
      "Bosnia & Herzegovina" ~ "Bosnia and Herz.",
      "Trinidad & Tobago" ~ "Trinidad and Tobago",
      "U.A.E." ~ "United Arab Emirates",
      "Guadeloupe" ~ "France",
      .default = nationality
    )
  )

# read in shapefile
earth <- sf::read_sf("data/earth.shp")

# set ggplot theme
ggplot2::theme_set(ggplot2::theme_classic(base_size = 14))

# User Interface ----------------------------------------------------------

ui <- bslib::page_sidebar(
  # theme
  theme = bslib::bs_theme(
    bootswatch = "lux",
    bg = "#f2f2f2",
    fg = "#002B36"
  ),
  title = "League One Footballer Salaries",
  
  # sidebar
  sidebar = bslib::sidebar(
    shiny::p("Please select your team of choice below."),
    shiny::selectInput(
      inputId = "selectLeague",
      label = "League",
      choices = c(unique(players$league)),
      selected = ""
    ),
    shiny::selectInput(
      inputId = "selectTeam",
      label = "Team",
      choices = c(unique(players$city[players$league == "premier-league"])),
      selected = "Oxford United"
    ),
    shiny::sliderInput(
      inputId = "sliderAge",
      label = "Age Range",
      min = min(players$age),
      max = max(players$age),
      value = range(players$age)
    ),
    shiny::HTML(
      "<p>Some footballers get paid <i>a lot</i>, which can really skew the distribution. Click below to use a log scale on the graphs and map.</p>"
    ),
    shiny::checkboxInput(
      inputId = "checkLog",
      label = "Use log scale",
      value = FALSE
    ),
    shiny::hr(),
    shiny::HTML(
      "Data Source: <a href='https://salarysport.com/football'>salarysport.com</a>"
    ),
    shiny::HTML("Created by Jack Davison")
  ),
  
  # main body
  bslib::layout_columns(
    height = 250,
    bslib::value_box(
      "Median Annual Income",
      showcase = bsicons::bs_icon("currency-pound"),
      value = shiny::textOutput("salaryMedian")
    ),
    bslib::value_box(
      "Mean Annual Income",
      showcase = bsicons::bs_icon("currency-pound"),
      value = shiny::textOutput("salaryMean")
    ),
    bslib::value_box(
      "Max Annual Income",
      showcase = bsicons::bs_icon("currency-pound"),
      value = shiny::textOutput("salaryMax")
    )
  ),
  bslib::layout_column_wrap(
    width = 0.5,
    bslib::card(
      bslib::card_header(
        "Raw Data",
        bslib::tooltip(
          bsicons::bs_icon("info-circle"),
          "This table can be sorted by clicking the column headers."
        )
      ),
      bslib::card_body(gt::gt_output("table"), padding = 0),
      full_screen = TRUE
    ),
    bslib::card(
      bslib::card_header(
        "Team Nationalities",
        bslib::tooltip(
          bsicons::bs_icon("info-circle"),
          "This map shows where team members are from, and the median salary for each country of origin."
        )
      ),
      bslib::card_body(leaflet::leafletOutput("map"), padding = 0),
      full_screen = TRUE,
    ),
    bslib::card(
      bslib::card_header(
        "Salary Distribution",
        bslib::tooltip(
          bsicons::bs_icon("info-circle"),
          "This shows the distribution of salaries for the team in GBP. If it appears skewed, try using a log scale instead."
        )
      ),
      bslib::card_body(plotOutput("histogram"), padding = 0),
      full_screen = TRUE
    ),
    bslib::card(
      bslib::card_header(
        "Salary vs. Age",
        bslib::tooltip(
          bsicons::bs_icon("info-circle"),
          "This shows how salary varies with age. Typically the younger players get paid less, but things tend to level out in their 20s."
        )
      ),
      bslib::card_body(plotOutput("boxplot"), padding = 0),
      full_screen = TRUE
    ),
  )
)


# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  # update teamdata by team selection
  teamdata <- reactive({
    dplyr::filter(
      players,
      city == input$selectTeam,
      dplyr::between(age, input$sliderAge[1], input$sliderAge[2])
    )
  })
  
  # update team options by league
  shiny::observeEvent(input$selectLeague, {
    newteams <- unique(players$city[players$league == input$selectLeague])
    shiny::updateSelectInput(session, "selectTeam",
                             choices = newteams,
                             selected = newteams[1])
  })
  
  shiny::observeEvent(input$selectTeam, {
    newteam <- players[players$city == input$selectTeam,]
    shiny::updateSliderInput(session, "sliderAge",
                             min = min(newteam$age),
                             max = max(newteam$age),
                             value = range(newteam$age))
  })

  # render raw data table
  output$table <- gt::render_gt({
    teamdata() |>
      dplyr::select(-league, -city, -name) |>
      dplyr::rename_with(snakecase::to_title_case) |>
      gt::gt() |>
      gt::fmt_number(columns = 2:3, decimals = 2) |>
      gt::tab_options(    table.background.color = "#FFFFFF00") |>
      gt::opt_interactive(use_filters = TRUE, use_search = TRUE)
  })

  # render histogram
  output$histogram <- renderPlot({
    if (input$checkLog) {
      fun <- ggplot2::scale_x_log10
      lab <- "Annual Salary (log scale)"
    } else {
      fun <- ggplot2::scale_x_continuous
      lab <- "Annual Salary"
    }
    teamdata() |>
      dplyr::filter(yearly_salary > 0) |>
      ggplot2::ggplot(ggplot2::aes(x = yearly_salary)) +
      ggplot2::geom_histogram() +
      fun(labels = scales::label_dollar(prefix = "£")) +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(c(0, .1))) +
      ggplot2::labs(x = lab)
  })
  
  # render boxplots
  output$boxplot <- renderPlot({
    if (input$checkLog) {
      fun <- ggplot2::scale_y_log10
      lab <- "Annual Salary (log scale)"
    } else {
      fun <- ggplot2::scale_y_continuous
      lab <- "Annual Salary"
    }
    teamdata() |>
      ggplot2::ggplot(ggplot2::aes(x = age, y = yearly_salary)) +
      ggplot2::geom_boxplot(ggplot2::aes(group = age)) +
      fun(
        labels = scales::label_dollar(prefix = "£"),
        expand = ggplot2::expansion(c(0, .1))
      ) +
      ggplot2::labs(
        y = lab,
        x = "Player Age"
      )
  })

  # render leaflet map
  output$map <- leaflet::renderLeaflet({
    counts <-
      teamdata() |>
      dplyr::summarise(
        salary = median(yearly_salary),
        players = dplyr::n_distinct(player_name),
        .by = name
      ) |>
      dplyr::left_join(dplyr::select(earth, name, geometry)) |>
      sf::st_as_sf()

    counts$plot_salary <- counts$salary

    if (input$checkLog) {
      counts$plot_salary <- log(counts$plot_salary)
    }

    pal <-
      scales::col_numeric(
        domain = counts$plot_salary,
        palette = c("black", "gold")
      )

    map <-
      leaflet::leaflet(data = counts) |>
      leaflet::addProviderTiles(leaflet::providers$CartoDB.PositronNoLabels) |>
      leaflet::addPolygons(
        popup = ~paste0(name, "<br>Total Players: ", players),
        stroke = F,
        fillOpacity = 0.75,
        fillColor = ~ pal(plot_salary)
      ) |>
      leaflet::addProviderTiles(leaflet::providers$CartoDB.PositronOnlyLabels)

    if (input$checkLog) {
      trans_func <- function(x) {
        10^x
      }
    } else {
      trans_func <- function(x) {
        x
      }
    }

    map <-
      leaflet::addLegend(
        map = map,
        pal = pal,
        values = counts$plot_salary,
        title = "Median Annual Salary",
        position = "bottomleft",
        labFormat = leaflet::labelFormat(prefix = "£", transform = trans_func)
      )

    map
  })

  # calc stats
  output$salaryMean <- renderText({
    scales::label_dollar(prefix = "£")(mean(teamdata()$yearly_salary))
  })

  output$salaryMedian <- renderText({
    scales::label_dollar(prefix = "£")(median(teamdata()$yearly_salary))
  })

  output$salaryMax <- renderText({
    scales::label_dollar(prefix = "£")(max(teamdata()$yearly_salary))
  })
}

# run app
shiny::shinyApp(ui, server)
