library(shiny)

all.towns <- c('Alford', 'Ashfield', 'Becket', 'Blandford', 'Charlemont', 'Chesterfield', 
               'Colrain', 'Cummington', 'Egremont', 'Goshen', 'Hancock', 'Hawley', 'Heath', 
               'Hinsdale', 'Lanesborough', 'Leyden', 'Middlefield', 'Monroe', 'Monterey', 
               'Montgomery', 'Mount Washington', 'New Ashford', 'New Braintree', 'New Marlborough', 
               'New Salem', 'Otis', 'Peru', 'Petersham', 'Plainfield', 'Rowe', 'Royalston', 
               'Sandisfield', 'Savoy', 'Shutesbury', 'Tolland', 'Tyringham', 'Warwick', 
               'Washington', 'Wendell', 'West Stockbridge', 'Windsor', 'Worthington')

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("FTTH Outsourced Regional Costs"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      
      tabsetPanel(type="tabs",
                  tabPanel("Basic Settings",
                           sliderInput("mlp.fee",
                                       "MLP fee:",
                                       min = 10,
                                       max = 200,
                                       step = 1,
                                       pre = "$",
                                       value = 45),
                           sliderInput("take.rate", "Take Rate", min=0, max=100, step=1, post='%', value=75),
                           selectInput(
                             "towngroups", "Towns",
                             choices=c("All 43" = 1,
                                       "All WiredWest" = 2,
                                       "Strong & Medium WiredWest" = 3,
                                       "West County (Crocker Cluster 3)" = 4,
                                       "Quabbin (Crocker Cluster 2)" = 5,
                                       "None" = 6,
                                       "Custom" = "custom"),
                             selected=3),
                           
                           # Only show this panel if Custom is selected
                           conditionalPanel(
                             condition = "input.town == 'custom'", 
                             selectizeInput("townnames", "Choose Individual Towns", 
                                            choices = all.towns,
                                            selected = 'Ashfield',
                                            multiple = TRUE
                             )
                           ),
                           selectInput("service.fee", "Minimum Service Fee", c("WiredWest 25Mb/s at $49/mo"=49, "Crocker 1000Mb/s at $25/mo"=25), selected=25)
                  ),
                  tabPanel("Opex Parameters",
                           numericInput("return.pct", "Contingency (% of opex)", .05),            # percent of opex (contingency fund)
                           h3('Network Operator'),
                           numericInput("network.operator.base", "Base (per town) - NEEDS WORK", 16800),   # per town(!)
                           numericInput("network.operator", "Per subscriber - NEEDS WORK", 36),           # per subscriber
                           h3('Per Mile'),
                           numericInput("fiber.plant.depreciation", "Fiber Plant Depreciation", 1395), # per mile
                           numericInput("insurance", "Insurance", 442),                 # per mile
                           h3('Per Pole'),
                           numericInput("bond.fees", "Bond fees", 3),                   # per pole
                           numericInput("pole.rental", "Pole Rental", 13),              # per pole
                           h3('Per Unit'),
                           numericInput("electronics.depreciation", "Electronics Depreciation", 63),   # per premise
                           numericInput("routine.mtnce", "Routine Maintenance", 39),              # per premise
                           h3('Per Town'),
                           numericInput("purma.dues", "PURMA Dues", 1200),               # per town
                           numericInput("accountant", "Accountant", 3000),               # per town
                           numericInput("bookkeeping.etc", "Bookkeeping, etc.", 5000),          # per town
                           numericInput("legal", "Legal", 10000)                  # per town
                  ),
                  tabPanel("Financing",
                           # maybe a pull down indicating different service costs
                           sliderInput("interest.rate", "Interest Rate", min=3, max=6, step=0.5, post='%', value=4),
                           sliderInput("years", "Years of Finance", min=10, max=30, step=1, value=20),
                           sliderInput("debt.responsibility", "Fraction of Debt Covered By Subscribers", min=0, max=100, step=5, value=0, post='%')
                  )
      )
    ),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type="tabs",
                  tabPanel("Plots",
                           conditionalPanel(
                             condition = "input.townnames.length > 0",
                             plotOutput("cash.flow"), plotOutput('reqd.mlp.fee'), plotOutput('subscriber.fees')
                           )
                  ),
                  tabPanel("Town Stats", DT::dataTableOutput("basic.town.data")),
                  tabPanel("Standalone Details", DT::dataTableOutput("town.costs")),
                  tabPanel("Cumulative Regional Details", DT::dataTableOutput("regional.costs"))
      )
    )
  )
))

