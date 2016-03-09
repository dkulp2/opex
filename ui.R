library(shiny)
debatable = "color:orange"

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
                           selectInput("service.fee", span("Minimum Service Fee",style=debatable), c("Crocker 1000Mb/s at $25/mo"=25), selected=25),
                           sliderInput("seasonal.month", "Number of Seasonal Months", 1, 12, 7, step=1, round=TRUE)
                  ),
                  tabPanel("Opex Parameters",
                           numericInput("return.pct", "Contingency (% of opex)", .05),            # percent of opex (contingency fund)
                           h3('Depreciation'),
                           selectInput('depreciation_method',span('Method', style=debatable),
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
                           h3('Network Operator', style=debatable),
                           numericInput("network.operator.base", "Base (per town)", 16800),   # per town(!)
                           numericInput("network.operator", "Per subscriber", 36),           # per subscriber
                           h3("Bandwidth"),
                           selectInput("backhaul.connections", span("Backhaul Connections",style=debatable), c("Aggregated","Per Town")),
                           numericInput("units.per.gb","Subscribers per Gb/s Backhaul", 600),
                           numericInput("backhaul.gb.price", span("Backhaul Price Per Gb/s",style=debatable), 18000),
                           h3('Insurance (per mile)', style=debatable),
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
                           h3('Administrative (per town)',style=debatable),
                           numericInput("purma.dues", "PURMA Dues", 1200),               # per town
                           numericInput("accountant", "Accountant", 3000),               # per town
                           numericInput("bookkeeping.etc", "Bookkeeping, etc.", 5000),          # per town
                           numericInput("legal", "Legal", 10000)                  # per town
                  ),
                  tabPanel("Financing",
                           # maybe a pull down indicating different service costs
                           sliderInput("interest.rate", "Interest Rate", min=3, max=6, step=0.5, post='%', value=4),
                           sliderInput("years", "Years of Finance", min=10, max=30, step=1, value=20)
                  )
      ),
      hr(),h4("Items marked in",span("orange",style=debatable), "represent currently unresolved issues or possible regional savings. Further information can be found in the 'Discussion' tab."),
      hr(),h4('Please send feedback to ',a('David Kulp.',href='mailto:dkulp@dizz.org'),' Thanks!')
      
    ),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type="tabs",
                  tabPanel("Subscriber Costs",
                           selectInput("regional.standalone.display", "", c('Regional','Standalone')),
                           plotOutput('subscriber.fees'),
                           plotOutput('reqd.mlp.fee'),
                           h3("Explanation"),p("Subscriber costs here are for minimal service. Costs are computed as the sum of a Debt Service Fee (if any), a minimum Internet Service Fee and an MLP Fee, representing the breakeven cost per subscriber to pay for sustaining the network asset. (See the 'Standalone Opex' panel for more information.)"),
                           p("If 'Regional' is selected, then the MLP Fee is the same for all towns because opex is computed over the entire region and distributed uniformly to all subscribers. The horizontal line represents the average cost per subscriber."),
                           p("The MLP Fee plot (lower plot) shows how the opex increases gradually in a regional approach, while the standalone costs for individual towns can be very high. Each blue dot is the MLP Fee that that particularly town would pay on its own. Each orange dot corresponds to a break-even fee if that town ", strong("and all towns listed above it")," joined together. The point size corresponds to the total number of subscribers. Note that the highest cost towns are some of the least populated, but in aggregate the region can absorb those higher costs with minimal impact on the individual subscriber.")
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
                  tabPanel("Cost vs Take Rate",
                           plotOutput("cost.vs.take.rate"),
                           h3('Explanation'),
                           p("The plot displays the total cost per subscriber per month given the selected town or towns (regional), selected share of debt service paid by subscribers, and other parameters.")),
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
                           p("Per unit costs in the 'Opex Parameters' panel are initially based on Leverett. Plant Opex is computed as the sum of insurance, pole fees, pole bond, routine maintenance, and electricity. Insurance is currently crudely computed on a per mile basis; insurance would likely be lower if purchased over a large number of towns. Routine maintenance is estimated as a function of the number of drops."),
                           p("Depreciation is computed as one of two methods defined in the 'Finance' tab, namely, either as a scaled depreciation based on Leverett or as 3% of the total capital cost without make ready. See the 'Discussion' tab."),
                           p("Network operator is currently a flat per town cost plus a per drop cost from Crocker's estimate of an integrated NO/ISP. See the 'Discussion' tab for more information."),
                           p("Admin costs are detailed in the 'Opex Parameters' panel.")),
                  tabPanel("Cumulative Regional Opex", DT::dataTableOutput("regional.costs"),
                           h3("Explanation"),
                           p("Regional costs are identical to standalone, but the total costs are computed cumulatively starting with the town with the highest return per subscriber. The only economy of scale is currently a minor administrative costs savings in which it is assumed, crudely, that administrative costs would be halved if shared among two or more towns. Admin represents less than 5% of costs, so is not significant. There is likely to be significant costs savings in a large, multi-town contract for network operator and ISP, but those costs are currently based on Crocker's integrated NO/ISP estimates per town. "),
                           p("Another area of possible savings is insurance, which probably does not scale linearly with road miles as is modeled here. At the default scale factor, annual insurance for 32 WiredWest towns would be almost $700,000, which is far higher than anticipated.")),
                  tabPanel("Discussion",
                           h1("Unresolved model parameters and areas for regional savings"),
                           p("The following discussion identifies open issues corresponding with parameters marked in",span("orange.",style=debatable),"Several of these issues are areas for potential regional savings. Other issues should be resolved now because they represent uncertainties that affect costs significantly. "),
                           h3("ISP Service Fee and Backhaul",style=debatable),
                           p("The model uses an ISP cost for minimum service based on Crocker's current offering in Leverett. This could go down in a competitive bid for more customers. In any case, I would like to include the cost of backhaul in the ISP cost, however currently the owner/operator pays for backhaul, since the model is based on Leverett."),
                           p("I think that it makes more sense to include backhaul as part of the ISP because this allows the ISP to have almost full control of customer satisfaction and can choose the appropriate oversubscription rate. Moreover, backhaul demand increases with subscribers and the introduction of tiered service would seem to require the ISP to manage backhaul bandwidth. However, requiring the ISP to pay for backhaul may raise rates above those shown here. The impact is roughly +/-$5 per month."),
                           h3("Backhaul connections",style=debatable),
                           p("When multiple towns are selected, then backhaul is assumed to be provided as needed on a per town basis. A proposed alternative is to minimize the number of middle mile connections by aggregating backhaul for multiple towns. This solution would require an unknown cost for dark fiber leases, but the savings is typically only $1-2 per sub per month. This can be understood trivially by doubling backhaul costs, which typically adds about $3 to a subscriber's bill."),
                           h3("Depreciation Methods",style=debatable),
                           p("There are two depreciation methods here. The first is a scaled depreciation based on Leverett's road miles and unit counts. This is likely an over-estimate, but it does include separate depreciation for fiber and electronics."),
                           p("The other method is simpler and cheaper. The depreciation is computed as an annual set aside of 3% of the cost of the plant — the minimum suggested, not required, by MLP law — which can be calculated by subtracting the make ready from the total capital cost. However, this approach does not include a shorter depreciation schedule for electronics because those costs are currently unknown."),
                           h3("Network Operator",style=debatable),
                           p("Additional comparable network operator costs are needed. The NO represents a significant portion of the operating budget that could benefit from multiple towns working together. However, the current NO costs are based on an informal Crocker proposal document that budgets the network operator as a flat cost per town plus a cost per drop. There is no savings from multiple towns, although this seems like a prime opportunity for economies of scale."),
                           h3("Insurance",style=debatable),
                           p("Insurance is intuitively an area that is ripe for costs savings for a larger plant. No guidance from PURMA, yet."),
                           h3("Administration",style=debatable),
                           p("Administration costs are only about 7% of the total expenses. However, there are opportunities for costs savings and efficiencies here."),
                           h2("Potential Regional Savings"),
                           p("A question of particular interest is what savings are available to subscribers when towns are part of a regional network. There are three kinds of savings: non-economic or intangible benefits, cost sharing, and economies of scale. For many of our towns, the administration and management of a telecommunications network is a new burden for town employees who are already over-committed. Sharing information, knowledge, and experience are common practice in our region. Sharing the required administrative work, perhaps by having it done by one or two trained people, is efficient and makes sense for many of our towns."),
                           p("The second kind of savings is based on averaging costs over multiple towns. Operating a town network tends to be more expensive for the less populated towns. When small towns combine with larger towns, the subscribers in the smaller towns tend to realize substantial cost savings. From the perspective of the larger towns (and the bulk of the subscribers), the cost to subscribers may rise, but only marginally, because that extra cost is spread over many subscribers. (See the MLP Fee plot.)"),
                           p("The third opportunity for savings is realized through economies of scale. Practically speaking, the main sources of such savings are probably in insurance, network operator, administration, and backhaul aggregation. Savings in each area may be modest, but the total could easily be $5 per subscriber per month. These savings accrue to all of the towns and help improve the sustainability of the network. For the larger towns these savings help offset any increases due to cost sharing."),
                           p("In short, there are significant monetary savings for small towns; there are probably offsetting additional costs and potential additional savings for large towns. But there is an intangible advantage to all towns to team together to provide mutual support and improve the chance of a successful implementation. Overall, the towns benefit from a regional approach, even in a fully outsourced model.")
                           )
      )
    )
  )
))

