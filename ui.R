library(shiny)
library(googleCharts)

source('docs.R')

all.towns <- c('Alford', 'Ashfield', 'Becket', 'Blandford', 'Charlemont', 'Chesterfield', 
               'Colrain', 'Cummington', 'Egremont', 'Florida', 'Goshen', 'Hancock', 'Hawley', 'Heath', 
               'Hinsdale', 'Lanesborough', 'Leyden', 'Middlefield', 'Monroe', 'Monterey', 
               'Montgomery', 'Mount Washington', 'New Ashford', 'New Braintree', 'New Marlborough', 
               'New Salem', 'Otis', 'Peru', 'Petersham', 'Plainfield', 'Rowe', 'Royalston', 
               'Sandisfield', 'Savoy', 'Shutesbury', 'Tolland', 'Tyringham', 'Warwick', 
               'Washington', 'Wendell', 'West Stockbridge', 'Windsor', 'Worthington')

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  googleChartsInit(),
  # tags$link(
  #   href=paste0("http://fonts.googleapis.com/css?",
  #               "family=Source+Sans+Pro:300,600,300italic"),
  #   rel="stylesheet", type="text/css"),
  # tags$style(type="text/css",
  #            "body {font-family: 'Source Sans Pro'}"
  #),
  
  # Application title
  titlePanel("FTTH Outsourced Regional Costs"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(type="tabs",
                  tabPanel("Basic Settings",
                           sliderInput("take.rate", "Take Rate", min=5, max=100, step=5, post='%', value=75),
                           sliderInput("debt.responsibility", "Fraction of Debt Covered By Subscribers", min=0, max=100, step=5, value=50, post='%'),
                           selectInput(
                             "towngroups", "Towns",
                             choices=c("All Towns" = 1,
                                       "All WiredWest" = 2,
                                       "Strong & Medium WiredWest" = 3,
                                       "West County" = 4,
                                       "Midtowns" = 7,
                                       "Northwest Hampshire" = 8,
                                       "Quabbin" = 5,
                                       "None" = 6,
                                       "Ashfield" = 9,
                                       "Custom" = "custom"),
                             selected=1),
                           
                           # Only show this panel if Custom is selected
                           conditionalPanel(
                             condition = "input.towngroups == 'custom'", 
                             selectizeInput("townnames", "Choose Individual Towns", 
                                            choices = all.towns,
                                            selected = all.towns,
                                            multiple = TRUE
                             )
                           ),
                           selectInput("service.fee", 
                                       span("Internet Service",style=debatable), 
                                       c("$25 (Crocker offers 1Gb/s at $25)"=25, "$20"=20, "$30"=30,"$0"=0), selected=25),
                           
                           selectInput("tiers","Tiers", c("Single Tier"=1,"Two Tier"=2), selected=1),
                           
                           # show only if custom 2-tier
                           conditionalPanel(
                             condition = "input.tiers == 2", 
                             sliderInput("tier1delta", "Lower Tier Price Reduction", -50, 0, -15, step=1, round=TRUE, pre='$'),
                             sliderInput('tier1takerate', 'Lower Tier Take Rate', min=5, max=95, step=5, post='%', value=30)
                             # p('Tier 1: $', textOutput('tier1price',inline=TRUE)),
                             # p('Tier 2: $', textOutput('tier2price',inline=TRUE))
                           ),
                           sliderInput("seasonal.month", "Number of Seasonal Months", 1, 12, 7, step=1, round=TRUE),
                           selectInput("data.source", "Source of Units, Miles, and Pole Count Data", c("Cartesian"='Cartesian',"MBI / Crocker"='Crocker'), selected='Crocker')
                  ),
                  tabPanel("Opex Parameters",
                           googlePieChart("opex.pie",
                                          width="auto", height = "auto",
                                          # Set the default options for this chart; they can be
                                          # overridden in server.R on a per-update basis. See
                                          # https://developers.google.com/chart/interactive/docs/gallery/bubblechart
                                          # for option documentation.
                                          options = list(
                                            fontName = "Source Sans Pro",
                                            fontSize = 13,
                                            title = "Regional Opex",
                                            # The default padding is a little too spaced out
                                            chartArea = list(
                                              top = 5, left = 5,
                                              height = "100%", width = "100%"
                                            ),
                                            # Set fonts
                                            titleTextStyle = list(
                                              fontSize = 16
                                            ),
                                            tooltip = list(
                                              textStyle = list(
                                                fontSize = 12
                                              )
                                            )
                                          )
                           ),
                           hr(),
                           tabsetPanel(type="tabs",position='right',
                                       tabPanel("Plant", p(),
                                                numericInput("insurance", 'Insurance (per mile)', 442),                 # per mile
                                                numericInput("bond.fees", "Bond fees (per pole)", 3),                   # per pole
                                                numericInput("pole.rental", "Rental (per pole)", 13),              # per pole
                                                numericInput("routine.mtnce", 'Routine Maintenance (per installed unit)', 39),     # per premise
                                                sliderInput("install.percent", "Percent of Premises With Drops", min=0, max=100, value=80, step=1, post='%'),
                                                numericInput("electricity.per.hut", "Electricity (per enclosure)", 2500),
                                                numericInput("avg.huts.per.town", "Average Huts Per Town", 1)),
                                       tabPanel('Net Op', p(),
                                                numericInput("network.operator.base", span("Base (per town)",style=debatable), 16800),   # per town(!)
                                                numericInput("network.operator", span("Per subscriber",style=debatable), 36),           # per subscriber
                                                selectInput("backhaul.connections", span("Backhaul Connections",style=debatable), c("Aggregated","Per Town")),
                                                numericInput("units.per.gb","Subscribers per Gbps Backhaul", 600),
                                                numericInput("backhaul.gb.price", span("Backhaul Price Per Gbps",style=debatable), 18000)),
                                       tabPanel('Admin', p(),
                                                numericInput("purma.dues", "PURMA Dues", 1200),               # per town
                                                numericInput("accountant", "Accountant", 3000),               # per town
                                                numericInput("bookkeeping.etc", "Bookkeeping, etc.", 5000),          # per town
                                                numericInput("legal", "Legal", 10000)),                  # per town
                                       tabPanel('Contigency', p(),
                                                selectInput('contingency_method', span('Method', style=debatable),
                                                            choices=c('A percentage of opex'='percentage',
                                                                      'A fixed "profit" per town'='fixed',
                                                                      'A fixed "profit", capped at a maximum value'='capped'),
                                                            selected='percentage'),
                                                conditionalPanel(
                                                  condition = "input.contingency_method == 'percentage'",
                                                  sliderInput("return.pct", "Percent of opex", min=0, max=50, value=5, step=1, post='%')             # percent of opex (contingency fund)
                                                ),
                                                conditionalPanel(
                                                  condition = "input.contingency_method != 'percentage'",
                                                  sliderInput("return.amt", "Per Town Profit", min=0, max=100000, value=50000, step=10000, pre='$')             # $50k based on WiPro recommendation
                                                ),
                                                conditionalPanel(
                                                  condition = "input.contingency_method == 'capped'",
                                                  sliderInput("return.max", "Maximum Regional Profit", min=0, max=3000000, value=500000, step=100000, pre='$')            # $500,000 for a large region of 10 or more towns
                                                )
                                       ),
                                       tabPanel('Depr Reserve', p(),
                                                selectInput('depreciation_method',span('Method', style=debatable),
                                                            choices=c('Scaled Leverett by Road Miles and Unit Count'='scaled',
                                                                      'Based on 3% of MBI'='mbi','None (Towns budget separately)'='none'),
                                                            selected='mbi'),
                                                conditionalPanel(
                                                  condition = "input.depreciation_method == 'scaled'",
                                                  numericInput("fiber.plant.depreciation", "Fiber Plant Depreciation Per Mile", 1395), # per mile
                                                  numericInput("electronics.depreciation", "Electronics Depreciation Per Unit", 63)   # per premise
                                                ),
                                                conditionalPanel(
                                                  condition = "input.depreciation_method == 'mbi'",
                                                  checkboxInput('exclude_makeready', 'Exclude Make Ready', value=TRUE),
                                                  conditionalPanel(
                                                    condition = 'input.exclude_makeready',
                                                    numericInput("make.ready", "Make Ready (per pole)", 470)
                                                  ),
                                                  checkboxInput('exclude_electronics', 'Exclude Electronics (from both CapEx and Depreciation)', value=FALSE),
                                                  conditionalPanel(
                                                    condition = 'input.exclude_electronics',
                                                    numericInput("electronics", "Electronics (per unit)", 1000)
                                                  )
                                                ))
                           )
                  ),
                  tabPanel("Financing",
                           # maybe a pull down indicating different service costs
                           sliderInput("interest.rate", "Interest Rate", min=3, max=6, step=0.5, post='%', value=4),
                           sliderInput("years", "Years of Finance", min=10, max=30, step=1, value=20)
                  )
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type="tabs",
                  tabPanel("Subscriber Costs",
                           selectInput("regional.standalone.display", "", c('Regional','Standalone')),
                           plotOutput('subscriber.fees'),
                           plotOutput('reqd.mlp.fee'),
                           DT::dataTableOutput("town.costs2"),
                           h3("Explanation"),
                           docs['Subscriber Costs']
                  ),
                  tabPanel("Net Income",
                           conditionalPanel(
                             condition = "input.townnames.length > 0",
                             sliderInput("mlp.fee",
                                         "MLP fee",
                                         min = 0,
                                         max = 100,
                                         step = 0.5,
                                         pre = "$",
                                         value = 25),
                             p('Regional break-even MLP Fee: $', textOutput('opt.mlp.fee',inline=TRUE)),
                             plotOutput("net.income"),
                             h3('Explanation'),
                             docs['Net Income'])
                  ),
                  tabPanel("Cost vs Take Rate",
                           plotOutput("cost.vs.take.rate"),
                           h3('Explanation'),
                           docs['Cost vs Take Rate']
                  ),
                  tabPanel("Town Stats", DT::dataTableOutput("basic.town.data"),
                           h3('Explanation'),
                           docs['Town Stats']),
                  tabPanel("Standalone Opex", DT::dataTableOutput("town.costs"),
                           h3("Explanation"),
                           docs['Standalone Opex']),
                  tabPanel("Cumulative Regional Opex",
                           DT::dataTableOutput("regional.costs"),
                           h3("Explanation"),
                           docs['Regional Opex']),
                  tabPanel("Discussion",
                           docs['Discussion'])
      )
    )
  )
))
