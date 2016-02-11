library(shiny)

library(dplyr)
library(ggplot2)
library(reshape)

# The plant.opex are fixed costs.
# I assume these costs are linear in the counts (miles, poles, etc.)
# That is, no economy of scale as increase size
plant.opex <- function(miles, poles, units, subscribers, input) {
  (input$fiber.plant.depreciation+input$insurance)*miles +
    (input$bond.fees+input$pole.rental)*poles +
    (input$electronics.depreciation+input$routine.mtnce)*units
}

# The netop.opex is the cost of the Network Operator subcontract.
# Crocker estimates a base cost per town and a cost per subscriber.
# THIS NEEDS WORK
netop.opex <- function(n_towns, subscribers, input) {
  input$network.operator.base * n_towns + input$network.operator*subscribers
}

# The admin.opex are the costs for bookkeeping, etc., which Crocker
# estimates as potentially a 50% savings when regionalized.
# This needs work.
admin.opex <- function(n_towns, input) {
  x <- (input$purma.dues+input$accountant+input$bookkeeping.etc+input$legal)*n_towns
  return(ifelse(n_towns>1, x/2, x)) # half admin costs if sharing (FIXME!)
}

# A contingency fund for paying deductibles or other issues 
# is based on a percent of total opex.
contingency <- function(tot.opex, input) {
  tot.opex * input$return.pct
}

# units miles poles         town   capex avg_sf_home total_assessed
# 1   357    26   785       Alford  830000      714950      271040108
# 2   934    72  1743     Ashfield 2300000      237407      224035671
# 3  1862   107  3560       Becket 3750000      231784      500323071
# 4   612    50  1385    Blandford 1760000      210653      165829451
# 5   671    55  1783   Charlemont 1710000      184317      125982938
# 6   618    48  1320 Chesterfield 1510000      212902      140105660
# ...
town.data <- read.table('towndata.txt', header=T)
rownames(town.data) <- town.data$town


town.data <- arrange(town.data, desc(town))

# return principal compounded with interest over years
compound <- function(principal, years, interest.rate) {
  for (i in 2:years) { principal <- principal * (1+interest.rate) }
  return(principal)
}


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  town.subset <- reactive({
    ss <- filter(town.data, town %in% input$town.names) # use only user-selected towns
    ss$debt <- compound(ss$capex, input$years, input$interest.rate/100)
    ss$'tax/sub/mo' <- ss$debt / input$years / ss$total_assessed * ss$avg_sf_home / 12
    return(ss)
  })

  
  town.derived <- reactive({
    # Compute costs, revenue, cash flow per town
    
    ts <- town.subset()
    take.rate <- input$take.rate/100
    
    per.town.data <-
      mutate(ts,
             n_towns=1,
             plant.opex=plant.opex(miles, poles, units, units*take.rate, input),
             netop.opex=netop.opex(1,units*take.rate, input),
             admin.opex=admin.opex(1, input),
             contingency=contingency(plant.opex+netop.opex+admin.opex, input),
             total.opex=plant.opex+netop.opex+admin.opex+contingency,
             revenue=input$mlp.fee*units*take.rate*12,
             cash.flow=revenue-total.opex,
             opex.per.sub.per.mo=total.opex/(units*take.rate)/12,
             net.per.sub.per.mo=input$mlp.fee-opex.per.sub.per.mo)
    
    # For computing cumulative results and plotting purposes, reorder town names by cash.flow
    per.town.data$town <- factor(per.town.data$town, levels=per.town.data$town[order(per.town.data$cash.flow)])
    per.town.data <- arrange(per.town.data, desc(town))
    
    # Now generate cumulative values in order of most affordable first
    cum.town.data <-
      mutate(per.town.data, 
             miles=cumsum(miles), 
             poles=cumsum(poles), 
             units=cumsum(units),
             n_towns=1:nrow(per.town.data),
             plant.opex=plant.opex(miles, poles, units, units*take.rate, input),
             netop.opex=netop.opex(n_towns, units*take.rate, input),
             admin.opex=admin.opex(n_towns, input),
             contingency=contingency(plant.opex+netop.opex+admin.opex, input),
             total.opex=plant.opex+netop.opex+admin.opex+contingency,
             revenue=input$mlp.fee*units*take.rate*12,
             cash.flow=revenue-total.opex,
             opex.per.sub.per.mo=total.opex/(units*take.rate)/12,
             net.per.sub.per.mo=input$mlp.fee-opex.per.sub.per.mo)
    
    # combine the data.frames
    costs <- cbind(rbind(per.town.data, cum.town.data), method=factor(rep(c('standalone','regional') , each=nrow(per.town.data))))
  })  
  
  output$basic.town.data <- renderTable(arrange(town.subset()[,c('town','units','miles','poles',
                                                                 'avg_sf_home','total_assessed',
                                                                 'capex','debt','tax/sub/mo')]
                                                ,town))
  
  output$town.costs <- renderTable({ 
    x <- arrange(town.derived(),as.character(town));
    z <- filter(x, method=='standalone')[,c('town','plant.opex','netop.opex','admin.opex',
                                          'contingency','total.opex','revenue','opex.per.sub.per.mo',
                                          'net.per.sub.per.mo')]
    return(z)
  })
  
  output$regional.costs <- renderTable({
    filter(town.derived(), method=='regional')[,c('town','plant.opex','netop.opex','admin.opex',
                                                  'contingency','total.opex','revenue','opex.per.sub.per.mo',
                                                  'net.per.sub.per.mo')]
  })
  
  output$cash.flow <- renderPlot({
      ggplot(town.derived(), aes(x=town,y=cash.flow/n_towns,fill=method)) + geom_bar(stat='identity',position='dodge') + coord_flip() +   ggtitle("Cash Flow Per Town") + ylab("$ / town")
  })
  
  output$reqd.mlp.fee <- renderPlot({
    ggplot(town.derived(), aes(x=town,y=opex.per.sub.per.mo,fill=method)) + geom_bar(stat='identity',position = "dodge") + coord_flip() + ggtitle("OpEx Per Subscriber") + ylab("$/month") 
    
  })
  
  output$subscriber.fees <- renderPlot({
    # stacked plot of debt, mlp fee, service fees
    z <- filter(town.derived(), method=='regional')
    z$min.service <- input$service.fee
    z2 <- melt(z, id="town", measure.vars=c('opex.per.sub.per.mo','min.service','tax/sub/mo'), variable_name='cost')
    z2$value <- as.numeric(z2$value)
    ggplot(z2, aes(x=town,y=value,fill=cost)) + geom_bar(stat='identity',position = "stack") + ggtitle("Monthly Subscriber Costs") + ylab("$/month") + theme(axis.text.x  = element_text(angle=45, vjust=1, hjust=1))
    
  })
    
})
