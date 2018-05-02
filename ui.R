library(leaflet)

# Choices for drop-downs

vars2 <- c(
  "GLOBAL" = "global",
  "USA" = "domestic"
)
state_vars <- statenames_df$State
country_var <- countrynames_df$Country

stat_vars <- append(state_vars, "1-ALL")
stat_vars <- stat_vars[order(stat_vars)]

country_var <- append(country_var, "1-ALL")
country_var <- country_var[order(country_var)]

navbarPage("Starbucks Around The World", id="nav",

  tabPanel("Interactive map",
    div(class="outer",

      tags$head(
        # Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),

      # If not using custom CSS, set height of leafletOutput to a number instead of percent
      leafletOutput("map", width="100%", height="100%"),
      
      # Shiny versions prior to 0.11 should use class = "modal" instead.
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
        width = 350, height = "auto",

        h2("STARBUCKS"),

        selectInput("option", "WHERE", vars2, selected="global"),
        conditionalPanel("input.option == 'domestic'",
                         selectInput("state", "STATE", stat_vars, selected = "1-ALL")),
        conditionalPanel("input.option == 'global'",
                         selectInput("country", "Country", country_var, selected = "1-ALL")
        )
      )
    )
  ),

tabPanel("Data explorer",
         fluidRow(
           column(3,
                  selectInput("dt_option", 
                              "Option", 
                              c("Global","USA"))
           ),
           column(3,
                  conditionalPanel("input.dt_option",
                                   selectInput("dt_country", 
                                               "Country", 
                                               c("All"=""), multiple=TRUE)
                  )
           ),
           column(3,
                  conditionalPanel("input.dt_option",
                                   selectInput("dt_state", 
                                               "State", 
                                               c("All"=""), multiple=TRUE)
                  )
           )
         ),
         hr(),
         DT::dataTableOutput("starbuckstb")
),

conditionalPanel("false", icon("crosshair"))
)
