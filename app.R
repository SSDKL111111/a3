library(here)
library(shiny)
library(shinydashboard)
library(bslib)
library(leaflet)
library(DT)
library(sf)
library(treemapify)
library(plotly)
library(forcats)
library(tidyverse)


# Load Data
members_elected <- read_csv(
  here("data", "HouseMembersElectedDownload-31496.csv"),
  skip = 1
)

first_preferences_house <- read_csv(
  here("data", "HouseFirstPrefsByCandidateByVoteTypeDownload-31496.csv"),
  skip = 1
)

tcp_house <- read_csv(
  here("data", "HouseTcpByCandidateByVoteTypeDownload-31496.csv"),
  skip = 1
)

first_preferences_senate <- read_csv(
  here("data", "SenateFirstPrefsByDivisionByVoteTypeDownload-31496.csv"),
  skip = 1
)

# Load Map
division_map <- st_read(here("data", "AUS_ELB_region.shp")) |>
  st_transform(crs = 4326) # Transform CRS to WGS84 (EPSG:4326) to resolve datum warning


house_status <- members_elected |>
  select(DivisionID, DivisionNm, PartyAb) |>
  mutate(
    # Combine LP, NP, LNP, LPNP
    PartyAb = case_when(
      PartyAb %in% c("LP", "NP", "LNP", "LPNP") ~ "LP_NP",
      TRUE ~ PartyAb
    ),
    Status = "Declared"
  )


# Identify undecided seats (not in members_elected)
undecided_divisions <- setdiff(
  unique(tcp_house$DivisionID),
  unique(members_elected$DivisionID)
)

# Process leading seats from TCP data for undecided divisions
leading_seats <- tcp_house |>
  filter(DivisionID %in% undecided_divisions) |>
  group_by(DivisionID) |>
  slice(which.max(TotalVotes)) |> # Select candidate with highest TCP votes
  select(DivisionID, DivisionNm, PartyAb) |>
  mutate(
    # Combine LP, NP, LNP, LPNP
    PartyAb = case_when(
      PartyAb %in% c("LP", "NP", "LNP", "LPNP") ~ "LP_NP",
      TRUE ~ PartyAb
    ),
    Status = "Leading"
  )

# Combine declared and leading seats
house_status <- bind_rows(house_status, leading_seats)

# Ensure division names match between shapefile and house_status
# Replace 'Elect_div' with the actual column name in the .shp file
division_map <- division_map |>
  rename(DivisionNm = Elect_div) |>
  mutate(
    # fix division name in map
    DivisionNm = case_when(
      DivisionNm == "Mcewen" ~ "McEwen",
      DivisionNm == "Eden-monaro" ~ "Eden-Monaro",
      DivisionNm == "Mcmahon" ~ "McMahon",
      DivisionNm == "O'connor" ~ "O'Connor",
      TRUE ~ DivisionNm
    )
  ) |>
  left_join(house_status, by = "DivisionNm") |>
  mutate(
    PartyAb = case_when(
      is.na(PartyAb) ~ "OTHER",
      PartyAb %in% c("LP", "NP", "LNP", "LPNP") ~ "LP_NP",
      TRUE ~ PartyAb
    ),
  )


# Senate: Aggregate first preference votes
senate_summary <- first_preferences_senate |>
  group_by(StateAb, PartyAb) |>
  drop_na() |>
  mutate(
    # Combine LP, NP, LNP, LPNP
    PartyAb = case_when(
      PartyAb %in% c("LP", "NP", "LNP", "LPNP") ~ "LP_NP",
      TRUE ~ PartyAb
    )
  ) |>
  summarise(
    TotalVotes = sum(TotalVotes),
    .groups = "drop"
  )


# Define color palette as a named list
party_colors <- list(
  LP_NP = "#1F77B4",
  ALP = "#FA4962",
  GRN = "#2CA02C",
  IND = "#FFBF33",
  XEN = "#F71EF6",
  KAP = "#881EF7",
  OTHER = "#7F7F7F"
)

# Create a colorFactor palette for Leaflet
palette <- colorFactor(
  palette = unlist(party_colors),
  levels = names(party_colors),
  na.color = "#7F7F7F" # Neutral gray
)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Australian Federal Election 2025 Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overall Status", tabName = "overall"),
      menuItem("House of Representatives", tabName = "house"),
      menuItem("Senate", tabName = "senate")
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML("
      .content-wrapper, .main-sidebar {
        height: 1080px !important;
        overflow-y: hidden !important;
      }
      .viz-note {
        font-size: 12px;
        font-style: italic;
        color: #555555;
        margin-top: 5px;
        margin-bottom: 5px;
      }
      .box-header h3 {
        font-weight: bold;
      }
    "))),
    tabItems(
      tabItem(
        tabName = "overall",
        fluidRow(
          box(
            title = "House of Distribution",
            plotlyOutput("house_seat_pie", height = "600px"),
            p("Shows declared/leading House seats and national Senate vote shares by party.", class = "viz-note"),
            width = 6
          ),
          box(
            title = "Senate First Preference Votes by State",
            plotlyOutput("senate_projection", height = "600px"),
            p("Displays cumulative Senate vote shares by party across states.", class = "viz-note"),
            width = 6
          )
        ),
        p("Data last updated: May 27, 2025. Note: Results are preliminary and subject to change.")
      ),
      tabItem(
        tabName = "house",
        fluidRow(
          box(
            title = "Electorate Map",
            leafletOutput("house_map", height = "700px"),
            p("Maps House electorate status (Declared/Leading) by party,
              with grouping LNP, LP, NP, and LPNP as LP_NP.", class = "viz-note"),
            width = 6
          ),
          box(
            title = "Leading Seats",
            DTOutput("key_seats_table"),
            p("Lists House electorates with leading candidates based on
              two-candidate-preferred votes.", class = "viz-note"),
            width = 6
          ),
          box(
            title = "Leading Seats Treemap",
            plotOutput("leading_treemap", height = "300px"),
            p("Visualizes the number of leading House seats by party, proportional to area.",
              class = "viz-note"
            ),
            width = 6
          )
        )
      ),
      tabItem(
        tabName = "senate",
        fluidRow(
          box(
            title = "Senate National Vote Share",
            plotlyOutput("senate_lollipop", height = "600px"),
            p("Illustrates national Senate vote share percentages by party,
              with grouping LNP, LP, NP, and LPNP as LP_NP.", class = "viz-note"),
            width = 6
          ),
          box(
            title = "Senate Votes by State",
            plotlyOutput("senate_facet_bar", height = "600px"),
            p("Breaks down Senate vote counts by party for each state,
              using AEC first preference data.", class = "viz-note"),
            width = 6
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output) {
  # House seat pie chart
  output$house_seat_pie <- renderPlotly({
    house_summary <- house_status |>
      group_by(PartyAb, Status) |>
      summarise(Seats = n(), .groups = "drop")

    p1 <- plot_ly(
      house_summary |>
        filter(Status == "Declared") |>
        mutate(Color = palette(PartyAb)),
      labels = ~PartyAb,
      values = ~Seats,
      type = "pie",
      marker = list(colors = ~Color),
      domain = list(x = c(0, 0.45), y = c(0, 1))
    ) |> layout(annotations = list(
      list(
        x = mean(c(0, 0.45)), y = 0.9,
        text = "Declared", showarrow = FALSE,
        font = list(size = 14), xanchor = "center"
      )
    ))

    p2 <- plot_ly(
      house_summary |>
        filter(Status == "Leading") |>
        mutate(Color = palette(PartyAb)),
      labels = ~PartyAb,
      values = ~Seats,
      type = "pie",
      marker = list(colors = ~Color),
      domain = list(x = c(0.55, 1), y = c(0, 1))
    ) |> layout(annotations = list(
      list(
        x = mean(c(0.55, 1)), y = 0.9,
        text = "Leading", showarrow = FALSE,
        font = list(size = 14), xanchor = "center"
      )
    ))

    p <- subplot(p1, p2, nrows = 1, margin = 0.1) |>
      layout(
        title = "<b>Declared vs. Leading Seats</b>",
        legend = list(
          title = list(text = "Party")
        )
      )

    p
  })

  # Senate first preference votes plot
  output$senate_projection <- renderPlotly({
    senate_votes <- senate_summary |>
      group_by(PartyAb) |>
      summarise(
        TotalVotes = sum(TotalVotes),
        .groups = "drop"
      )
    votes <- c(senate_votes$TotalVotes)
    names(votes) <- senate_votes$PartyAb


    p <- ggplot(
      senate_summary,
      aes(x = reorder(PartyAb, -votes[PartyAb]), y = TotalVotes, fill = StateAb)
    ) +
      geom_bar(
        stat = "identity",
      ) +
      theme_minimal() +
      labs(
        # title = "Senate First Preference Votes",
        x = "Party",
        y = "Total Votes",
        fill = "State"
      ) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
      )

    ggplotly(p)
  })

  # House map
  output$house_map <- renderLeaflet({
    map_data <- division_map |>
      mutate(
        PartyAb = case_when(
          PartyAb %in% names(party_colors) ~ PartyAb,
          TRUE ~ "OTHER"
        )
      ) |>
      st_zm()


    leaflet(map_data) |>
      addTiles() |>
      addPolygons(
        fillColor = ~ palette(PartyAb),
        weight = 1,
        opacity = 1,
        color = "white",
        fillOpacity = 0.7,
        label = ~ paste(DivisionNm, "-", Status),
        layerId = ~DivisionNm
      ) |>
      addLegend(
        position = "bottomright",
        colors = unlist(party_colors),
        labels = names(party_colors),
        title = "Party",
        opacity = 0.7
      ) |>
      setView(
        lng = 134.4896,
        lat = -25.7344,
        zoom = 4
      )
  })

  # Key seats table
  output$key_seats_table <- renderDT({
    datatable(
      house_status |>
        filter(Status == "Leading"),
      options = list(pageLength = 5, autoWidth = TRUE)
    )
  })


  # Leading seats treemap
  output$leading_treemap <- renderPlot({
    leading_summary <- house_status |>
      filter(Status == "Leading") |>
      group_by(PartyAb) |>
      summarise(Seats = n(), .groups = "drop")

    p <- ggplot(
      leading_summary,
      aes(area = Seats, fill = PartyAb, label = PartyAb)
    ) +
      geom_treemap() +
      geom_treemap_text(colour = "white", place = "centre", size = 12) +
      theme_minimal() +
      labs(fill = "Party") +
      scale_fill_manual(values = party_colors)

    p
  })

  # Senate lollipop chart
  output$senate_lollipop <- renderPlotly({
    plot_data <- senate_summary |>
      mutate(
        PartyAb = case_when(
          PartyAb %in% names(party_colors) ~ PartyAb,
          TRUE ~ "OTHER"
        )
      ) |>
      group_by(PartyAb) |>
      summarise(
        TotalVotes = sum(TotalVotes),
        .groups = "drop"
      ) |>
      mutate(
        Share = round(TotalVotes / sum(TotalVotes) * 100, digits = 2) # Percentages
      ) |>
      filter(Share > 0)


    p <- ggplot(
      plot_data,
      aes(x = reorder(PartyAb, -TotalVotes), y = Share)
    ) +
      geom_segment(
        aes(x = PartyAb, xend = PartyAb, y = 0, yend = Share, colour = PartyAb),
        size = 1
      ) +
      geom_point(
        aes(color = PartyAb),
        size = 5
      ) +
      geom_text(
        aes(label = sprintf("%.2f%%", Share), y = Share + 2),
        vjust = -1,
        size = 4
      ) +
      theme_minimal() +
      labs(
        # title = "Senate National Vote Share",
        x = "Party",
        y = "Vote Share (%)"
      ) +
      scale_color_manual(values = unlist(party_colors)) +
      theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank()
      )

    ggplotly(p)
  })

  # Senate votes by state (faceted bar chart)
  output$senate_facet_bar <- renderPlotly({
    plot_data <- senate_summary |>
      mutate(
        PartyAb = case_when(
          PartyAb %in% names(party_colors) ~ PartyAb,
          TRUE ~ "OTHER"
        )
      )

    votes <- plot_data |>
      group_by(StateAb, PartyAb) |>
      summarise(
        Votes = sum(TotalVotes),
        .groups = "drop"
      )

    plot_data <- plot_data |>
      left_join(votes, by = c("StateAb", "PartyAb")) |>
      mutate(
        PartyAb = fct_reorder(PartyAb, -Votes)
      )

    p <- ggplot(
      plot_data,
      aes(x = PartyAb, y = TotalVotes, fill = PartyAb)
    ) +
      facet_wrap(~StateAb, ncol = 4, scales = "free_y") +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(
        # title = "Senate Votes by State",
        x = "Party",
        y = "Votes",
        fill = "Party"
      ) +
      scale_fill_manual(values = unlist(party_colors)) +
      theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "bottom",
        strip.text = element_text(size = 10)
      )

    ggplotly(p) |>
      layout(legend = list(
        x = 0,
        xanchor = "left",
        yanchor = "bottom",
        orientation = "h"
      ))
  })
}

# Run the app
shinyApp(ui, server)
