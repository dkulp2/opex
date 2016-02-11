library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("FTTH Outsourced Regional Costs"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(type="tabs",
                  tabPanel("Parameters",
                           sliderInput("mlp.fee",
                                       "MLP fee:",
                                       min = 10,
                                       max = 200,
                                       step = 1,
                                       pre = "$",
                                       value = 45),
                           sliderInput("take.rate", "Take Rate", min=0, max=100, step=1, post='%', value=75),
                           selectInput("service.fee", "Minimum Service Fee", c("WiredWest 25Mb/s at $49/mo"=49, "Crocker 1000Mb/s at $25/mo"=25), selected=25),
                           numericInput("fiber.plant.depreciation", "Fiber Plant Depreciation (per mile)", 1395), # per mile
                           numericInput("insurance", "Insurance (per mile)", 442),                 # per mile
                           numericInput("bond.fees", "Bond fees (per pole)", 3),                   # per pole
                           numericInput("pole.rental", "Pole Rental (per pole)", 13),              # per pole
                           numericInput("electronics.depreciation", "Electronics Depreciation (per unit)", 63),   # per premise
                           numericInput("routine.mtnce", "Routine Maintenance (per unit)", 39),              # per premise
                           numericInput("network.operator.base", "Network Operator Base (per town) NEEDS WORK", 16800),   # per town(!)
                           numericInput("network.operator", "Network Operator (per sub) NEEDS WORK", 36),           # per subscriber
                           numericInput("purma.dues", "PURMA Dues (per town)", 1200),               # per town
                           numericInput("accountant", "Accountant (per town)", 3000),               # per town
                           numericInput("bookkeeping.etc", "Bookkeeping, etc. (per town)", 5000),          # per town
                           numericInput("legal", "Legal", 10000),                   # per town
                           numericInput("return.pct", "Contingency (% of opex)", .05)                # percent of opex (contingency fund)
                  ),
                  tabPanel("Towns",
                           # maybe a pull down indicating different service costs
                           sliderInput("interest.rate", "Interest Rate", min=3, max=6, step=0.5, post='%', value=4),
                           sliderInput("years", "Years of Finance", min=10, max=30, step=1, value=20),
                           checkboxGroupInput("town.names", "Towns:", 
                                              choices = c('Alford', 'Ashfield', 'Becket', 'Blandford', 'Charlemont', 'Chesterfield', 
                                                          'Colrain', 'Cummington', 'Egremont', 'Goshen', 'Hancock', 'Hawley', 'Heath', 
                                                          'Hinsdale', 'Lanesborough', 'Leyden', 'Middlefield', 'Monroe', 'Monterey', 
                                                          'Montgomery', 'Mount Washington', 'New Ashford', 'New Braintree', 'New Marlborough', 
                                                          'New Salem', 'Otis', 'Peru', 'Petersham', 'Plainfield', 'Rowe', 'Royalston', 
                                                          'Sandisfield', 'Savoy', 'Shutesbury', 'Tolland', 'Tyringham', 'Warwick', 
                                                          'Washington', 'Wendell', 'West Stockbridge', 'Windsor', 'Worthington'),
                                              selected = c('Becket','Blandford','Chesterfield','Goshen','Heath','Hinsdale',
                                                           'Monterey','New Ashford','New Salem','Peru','Plainfield',
                                                           'Rowe','Sandisfield','Shutesbury','Tolland','Washington',
                                                           'West Stockbridge','Windsor','Ashfield','Charlemont','Cummington',
                                                           'New Marlborough','Worthington','Colrain','Egremont','Hawley'))
                  )
      )
    ),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type="tabs",
                  tabPanel("Plots", plotOutput("cash.flow"), plotOutput('reqd.mlp.fee'), plotOutput('subscriber.fees')),
                  tabPanel("Town Stats", tableOutput("basic.town.data")),
                  tabPanel("Standalone Details", tableOutput("town.costs")),
                  tabPanel("Cumulative Regional Details", tableOutput("regional.costs"))
      )
    )
  )
))
