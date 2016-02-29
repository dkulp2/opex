library(shiny)

all.towns <- c('Alford', 'Ashfield', 'Becket', 'Blandford', 'Charlemont', 'Chesterfield', 
               'Colrain', 'Cummington', 'Egremont', 'Florida', 'Goshen', 'Hancock', 'Hawley', 'Heath', 
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
                           sliderInput("take.rate", "Take Rate", min=0, max=100, step=1, post='%', value=75),
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
                                       "Custom" = "custom"),
                             selected=3),
                           
                           # Only show this panel if Custom is selected
                           conditionalPanel(
                             condition = "input.towngroups == 'custom'", 
                             selectizeInput("townnames", "Choose Individual Towns", 
                                            choices = all.towns,
                                            selected = 'Ashfield',
                                            multiple = TRUE
                             )
                           ),
                           selectInput("service.fee", "Minimum Service Fee", c("Crocker 1000Mb/s at $25/mo"=25), selected=25),
                           sliderInput("seasonal.month", "Number of Seasonal Months", 1, 12, 7, step=1, round=TRUE)
                  ),
                  tabPanel("Opex Parameters",
                           numericInput("return.pct", "Contingency (% of opex)", .05),            # percent of opex (contingency fund)
                           h3('Depreciation'),
                           selectInput('depreciation_method','Method',
                                       choices=c('Scaled Leverett by Road Miles and Unit Count'='scaled',
                                                 'Based on 3% of MBI Not-To-Exceed Minus Make Ready'='mbi'),
                                       selected='mbi'),
                           conditionalPanel(
                             condition = "input.depreciation_method == 'scaled'",
                             numericInput("fiber.plant.depreciation", "Fiber Plant Depreciation Per Mile", 1395), # per mile
                             numericInput("electronics.depreciation", "Electronics Depreciation Per Unit", 63)   # per premise
                           ),
                           conditionalPanel(
                             condition = "input.depreciation_method == 'mbi'",
                             numericInput("make.ready", "Make Ready (per pole)", 470)
                           ),
                           h3('Network Operator'),
                           numericInput("network.operator.base", "Base (per town) - NEEDS WORK", 16800),   # per town(!)
                           numericInput("network.operator", "Per subscriber - NEEDS WORK", 36),           # per subscriber
                           h3("Bandwidth"),
                           numericInput("units.per.gb","Subscribers per Gb/s Backhaul", 600),
                           numericInput("backhaul.gb.price", "Backhaul Price Per Gb/s", 18000),
                           h3('Insurance (per mile)'),
                           numericInput("insurance", NULL, 442),                 # per mile
                           h3('Utility Poles (per pole)'),
                           numericInput("bond.fees", "Bond fees", 3),                   # per pole
                           numericInput("pole.rental", "Pole Rental", 13),              # per pole
                           h3('Routine Maintenance (per installed unit)'),
                           numericInput("routine.mtnce", NULL, 39),     # per premise
                           sliderInput("install.percent", "Percent of Premises With Drops", min=0, max=100, value=80, step=1, post='%'),
                           h3('Enclosures'),
                           numericInput("electricity.per.hut", "Electricity Per Hut", 2500),
                           numericInput("avg.huts.per.town", "Average Huts Per Town", 1),
                           h3('Administrative (per town) (needs work)'),
                           numericInput("purma.dues", "PURMA Dues", 1200),               # per town
                           numericInput("accountant", "Accountant", 3000),               # per town
                           numericInput("bookkeeping.etc", "Bookkeeping, etc.", 5000),          # per town
                           numericInput("legal", "Legal", 10000)                  # per town
                  ),
                  tabPanel("Financing",
                           # maybe a pull down indicating different service costs
                           sliderInput("interest.rate", "Interest Rate", min=3, max=6, step=0.5, post='%', value=4),
                           sliderInput("years", "Years of Finance", min=10, max=30, step=1, value=20),
                           sliderInput("debt.responsibility", "Fraction of Debt Covered By Subscribers", min=0, max=100, step=5, value=100, post='%')
                  )
      )
    ),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type="tabs",
                  tabPanel("Subscriber Costs",
                           plotOutput('subscriber.fees'),
                           plotOutput('reqd.mlp.fee'),
                           h3("Explanation"),p("Subscriber costs here are for minimal service. Costs are computed as the sum of a Debt Service Fee (if any), a minimum Internet Service fee and an MLP Fee, representing the breakeven cost per subscriber to pay for sustaining the network asset. (See the 'Standalone Opex' panel for more information.) The horizontal line represents the cost if all towns participated in a regional network, jointly sharing costs. ",
                                              "The MLP Fee plot (lower plot) shows how the opex increases gradually in a regional approach, while the standalone costs for individual towns can be very high. Each blue dot is the MLP Fee that that particularly town would pay on its own. Each orange dot corresponds to a break-even fee if that town ", strong("and all towns listed above it")," joined together. The point size corresponds to the total number of subscribers. Note that the highest cost towns are some of the least populated, but in aggregate the region can absorb those higher costs with minimal impact on the individual subscriber.")
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
                                         value = 45),
                             p('Regional break-even MLP Fee: $', textOutput('opt.mlp.fee',inline=TRUE)),
                             plotOutput("net.income"),
                             h3('Explanation'),
                             p("This plot shows the net income per user realized by a town or regional entity given a fixed MLP Fee. A town or region should operate under sustainable conditions such that net income is positive. The blue lines indicate the net income for individual towns when operated independently. The orange lines indicate net income for a region of towns that includes that town ",strong("and all towns listed above it."), " As the MLP Fee increases, more towns have positive cash flow if they were to operate independently. The MLP Fee necessary for a region to operate in the black is lower than the MLP Fee required for all towns to be cash flow positive independently. Of course this also means that some towns are paying a larger MLP Fee to sustain less profitable towns, but the cost impact is relatively small (typically less than $10 per month). In addition, towns realize the benefit of cooperative work instead of suffering the burden of administration replicated in each town."))
                  ),
                  tabPanel("Town Stats", DT::dataTableOutput("basic.town.data"),
                           h3("Explanation:"),p("Units refers to the number of possible drops; there can be multiple units per premise. ",
                                                "This number comes from MBI premise counts published September 2015. ",
                                                "Vacancy rate is calculated from the 2010 census. If a town reported to the MBI its non-primary ",
                                                "premise count, then the seasonal rate is the fraction defined as 'non-primary premises' / 'non-vacant units', assuming that non-primary premises are single-unit. ",
                                                "Otherwise the seasonal rate is computed from the 2010 census. ",
                                                "Subscribers is computed from the non-vacant units where the seasonal percentage is pro-rated based on the seasonal months parameter. ",
                                                "Miles and Poles are from MBI. 'Avg Single Family' and 'Total Assessed' are the assessed values from the DLS Data Bank (Jim Drawe). Capex refers to the town's borrowed amount requested by MBI. ",
                                                "'Capex w/ Interest' is the total debt using equal annual payments. 'Tax/Home/Month' and 'Capex Fee/Sub/Mo' split the debt across the tax base and the subscribers according to the user parameter 'Fraction of Debt Covered by Subscribers' under the 'Financing' tab.")),
                  tabPanel("Standalone Opex", DT::dataTableOutput("town.costs"),
                           h3("Explanation"),
                           p("Per unit costs in the 'Opex Parameters' panel are initially based on Leverett. Plant Opex is computed as the sum of insurance, pole fees, pole bond, routine maintenance, and depreciation costs. Insurance is currently crudely computed on a per mile basis; insurance would likely be lower if purchased over a large number of towns. ",
                             "Routine maintenance is estimated as a function of the number of drops. Depreciation is computed as one of two methods defined in the 'Finance' tab, namely, either as a scaled depreciation based on Leverett or as 3% of the total capital cost without make ready. ",
                             "Network operator is currently a flat per town cost plus a per drop cost from Crocker's estimate of an integrated NO/ISP. ",
                             "Admin costs are detailed in the 'Opex Parameters' panel.")),
                  tabPanel("Cumulative Regional Opex", DT::dataTableOutput("regional.costs"),
                           h3("Explanation"),
                           p("Regional costs are identical to standalone, but the total costs are computed cumulatively starting with the town with the highest return per subscriber. The only economy of scale is currently a minor administrative costs savings in which it is assumed, crudely, that administrative costs would be halved if shared among two or more towns. Admin represents less than 5% of costs, so is not significant. There is likely to be significant costs savings in a large, multi-town contract for network operator and ISP, but those costs are currently based on Crocker's integrated NO/ISP estimates per town. "),
                             "Another area of possible savings is insurance, which probably does not scale linearly with road miles as is modeled here. At the default scale factor, annual insurance for 32 WiredWest towns would be almost $700,000, which is far higher than anticipated.")
      )
    )
  )
))

