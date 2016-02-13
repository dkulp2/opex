library(shiny)

library(dplyr)
library(ggplot2)
library(reshape)

# There are two depreciation strategies:
# Crocker/Leverett: scale the Leverett plant by road miles and Leverett electronics by units,
# MLP/MBI: use MBI's not to exceed number as the cost of the total plant and take 3%.
# In the latter case, I don't separate plant and electronics
depreciation <- function(miles, units, cost, input) {
  if (input$depreciation_method == 'mbi') {
    return(cost*(1-input$make.ready.pct/100)*0.03)  # treat fiber and electonics differently?
  } else {
    return(input$fiber.plant.depreciation*miles + input$electronics.depreciation*units)
  }
}

# The plant.opex are fixed costs.
# I assume these costs are linear in the counts (miles, poles, etc.)
# That is, no economy of scale as increase size
plant.opex <- function(miles, poles, units, subscribers, cost, input) {
  input$insurance*miles +
    (input$bond.fees+input$pole.rental)*poles +
    input$routine.mtnce*units +
    depreciation(miles, units, cost, input)
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


# calculate annual payments and then total
# http://mathforum.org/library/drmath/view/67057.html
debt.svc <- function(principal, years, interest.rate) {
  return( years * (principal*interest.rate) / (1-(1+interest.rate)^(-years)) )
}

subscribers.fnc <- function(units, input) {
  as.integer(((1-input$seasonal.pct/100)*units + input$seasonal.pct/100*units*input$seasonal.month/12) *input$take.rate/100)
}

# returns a JavaScript function for formatting a numeric field as money
JSmoney <- function() {
  DT::JS('function(data, type, row, meta) { return type=="display"?"$" + data.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ","):data }')
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

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  observe({
    if (input$towngroups == 1) {
      updateSelectizeInput(session, "townnames", selected=town.data$town)
    } else if (input$towngroups == 2) {
      updateSelectizeInput(session, "townnames", selected=
                             town.data[town.data$WiredWest_support != 'No','town'])
    } else if (input$towngroups == 3) {
      updateSelectizeInput(session, "townnames", selected=
                             town.data[town.data$WiredWest_support %in% c('Strong','Medium'),'town'])
    } else if (input$towngroups == 4) {
      updateSelectizeInput(session, "townnames", selected=
                             town.data[town.data$crocker_cluster == '3','town'])
    } else if (input$towngroups == 5) {
      updateSelectizeInput(session, "townnames", selected=
                             town.data[town.data$crocker_cluster == '2','town'])
    } else if (input$towngroups == 6) {
      updateSelectizeInput(session, "townnames", selected='')
    }
  })
  
  town.subset <- reactive({
    ss <- filter(town.data, town %in% input$townnames) # use only user-selected towns
    ss <- mutate(ss,
                 subscribers=subscribers.fnc(units,input),
                 debt=as.integer(debt.svc(capex, input$years, input$interest.rate/100)),
                 proptax.per.mo=round((debt * (1-input$debt.responsibility/100)) / input$years / total_assessed * avg_sf_home / 12, 2),
                 capex.fee.per.mo=round(debt*input$debt.responsibility/100 / input$years / subscribers / 12,2))
    return(ss)
  })

  
  town.derived <- reactive({
    # Compute costs, revenue, net income per town
    
    ts <- town.subset()

    per.town.data <-
      mutate(ts,
             n_towns=1,
             plant.opex=plant.opex(miles, poles, units, subscribers, total_cost, input),
             netop.opex=netop.opex(1,subscribers, input),
             admin.opex=admin.opex(1, input),
             contingency=as.integer(contingency(plant.opex+netop.opex+admin.opex, input)),
             total.opex=plant.opex+netop.opex+admin.opex+contingency,
             revenue=input$mlp.fee*subscribers*12,
             net.income=revenue-total.opex,
             opex.per.sub.per.mo=round(total.opex/subscribers/12,2),
             net.per.sub.per.mo=input$mlp.fee-opex.per.sub.per.mo)
    
    # For computing cumulative results and plotting purposes, reorder town names by net income
    per.town.data$town <- factor(per.town.data$town, levels=per.town.data$town[order(per.town.data$net.per.sub.per.mo)])
    per.town.data <- arrange(per.town.data, desc(town))
    per.town.data$cum.total.opex <- cumsum(per.town.data$total.opex)
    
    # Now generate cumulative values in order of most affordable first
    cum.town.data <-
      mutate(per.town.data, 
             miles=cumsum(miles), 
             poles=cumsum(poles), 
             units=cumsum(units),
             total_cost=cumsum(total_cost),
             n_towns=1:nrow(per.town.data),
             subscribers=subscribers.fnc(units, input),
             plant.opex=plant.opex(miles, poles, units, subscribers, total_cost, input),
             netop.opex=netop.opex(n_towns, subscribers, input),
             admin.opex=admin.opex(n_towns, input),
             contingency=as.integer(contingency(plant.opex+netop.opex+admin.opex, input)),
             total.opex=plant.opex+netop.opex+admin.opex+contingency,
             revenue=input$mlp.fee*subscribers*12,
             net.income=revenue-total.opex,
             opex.per.sub.per.mo=round(total.opex/subscribers/12,2),
             net.per.sub.per.mo=input$mlp.fee-opex.per.sub.per.mo)

    # for debug purposes, cum.total.opex is the addition of the standalone cost of town i to the cumulative cost.
    # By comparison, cum.town.data$total.opex is the cost with economies of scale, if any. Comparing these
    # two values helps debug whether such economies are properly computed.
    cum.town.data$cum.total.opex <- cum.town.data$total.opex
    if (nrow(cum.town.data)>1) {
      for (i in 2:nrow(cum.town.data)) {
        cum.town.data$cum.total.opex[i] <- cum.town.data$total.opex[i-1] + per.town.data$total.opex[i]
      }
    }
    
    # combine the data.frames
    costs <- cbind(rbind(per.town.data, cum.town.data), method=factor(rep(c('standalone','regional') , each=nrow(per.town.data))))

    # for debug purposes, keep a copy of all the core computed data and save a copy of input as a list instead of a reactive object.
    if (!file.exists("z.Rdata")) {
      INP <- list()
      for (i in names(input)) { INP[[i]] <- input[[i]] }
      save(ts, INP, per.town.data, cum.town.data, costs, file='z.Rdata')
    }
    
    return(costs)
  })  
  
  opt.mlp.fee <- reactive({
    z <- town.derived()
    mlp.fee <- z[z$method=='regional','opex.per.sub.per.mo']
    mlp.fee[length(mlp.fee)]
  })
  output$opt.mlp.fee <- renderText(as.integer(opt.mlp.fee()))
  
  # See http://rstudio.github.io/DT/options.html for DT::renderDataTable options
  # there is supposed to be an export extension, but I cannot get it to work. http://rstudio.github.io/DT/extensions.html
  
  output$basic.town.data <- DT::renderDataTable(
    { 
      x <- arrange(town.subset()[,c('town','units','subscribers','miles','poles',
                                    'avg_sf_home','total_assessed',
                                    'capex','debt','proptax.per.mo','capex.fee.per.mo')]
                   ,town)
      return(x)
    }, 
    rownames=FALSE, 
    colnames=c('Town','Units','Subscribers','Miles','Poles','Avg Single Family',
               'Total Assessed', 'Capex', 'Capex w/ Interest', 'Tax / Home / Month','Capex Fee / Sub / Month'),
    options=list(dom='t',paging=FALSE, columnDefs=list(list(targets=4:8,render=JSmoney())))
  )
  
  output$town.costs <- DT::renderDataTable(
    { 
      x <- town.derived()
      x$town <- as.character(x$town)
      x <- arrange(x,town)
      z <- filter(x, method=='standalone')[,c('town','plant.opex','netop.opex','admin.opex',
                                              'contingency','total.opex','opex.per.sub.per.mo')]
      return(z)
    }, 
    rownames=FALSE,
    colnames=c('Town','Plant Opex','Network Operator','Admin Opex',
               'Contingency','Total Opex',
               'Opex/Sub/Month'),
    options=list(dom='t',paging=FALSE,columnDefs=list(list(targets=1:6, render=JSmoney())))
  )
  
  output$regional.costs <- DT::renderDataTable(
    {
      x <- arrange(town.derived(),desc(town))
      x$town <- as.character(x$town)
      z <- filter(x, method=='regional')[,c('town','plant.opex','netop.opex','admin.opex',
                                            'contingency','total.opex','opex.per.sub.per.mo')]
      return(z)
    }, 
    rownames=FALSE,
    colnames=c('Town','Plant Opex','Network Operator','Admin Opex',
               'Contingency','Total Opex','Opex/Sub/Month'),
    options=list(dom='t',paging=FALSE,columnDefs=list(list(targets=1:6, render=JSmoney())))
  )
  
  output$net.income.tbl <- renderTable({
    z <- town.derived()[,c('town','method','total.opex','revenue','net.income','opex.per.sub.per.mo','net.per.sub.per.mo')]
    z2 <- cbind(z[z$method=='regional',-2],z[z$method=='standalone',c(-1,-2)])
    colnames(z2) <- c('name',paste(colnames(z2)[2:6],rep(c('reg','stnd'),each=5),sep='.'))
    return(z2)
  })

    output$net.income <- renderPlot({
  #    ggplot(town.derived(), aes(x=town,y=net.income/n_towns,fill=method)) + geom_bar(stat='identity',position='dodge') + coord_flip() +   ggtitle("Net Income Per Town") + ylab("$ / town")
    ggplot(town.derived(), aes(x=town,y=net.per.sub.per.mo,fill=method)) + geom_bar(stat='identity',position='dodge') + coord_flip() +   ggtitle("Net Income Per User") + ylab("$ / subscriber / month")
  })
  
  # output$reqd.mlp.fee <- renderPlot({
  #   z<-town.derived()
  #   ggplot(z, aes(x=town,y=opex.per.sub.per.mo,fill=method)) + geom_bar(stat='identity',position = "dodge") + coord_flip() + ggtitle("Required Fee Per Subscriber to Cover Opex (MLP Fee)") + ylab("$/month") 
  # })
  
  output$reqd.mlp.fee <- renderPlot({
    z<-town.derived()
    ggplot(z, aes(x=town,y=opex.per.sub.per.mo,color=method)) + geom_point(aes(size=subscribers)) + coord_flip() +
      ggtitle("Required Fee Per Subscriber to Cover Opex (MLP Fee)") + ylab("$/month") 
  })

  output$subscriber.fees <- renderPlot({
      # stacked plot of debt, mlp fee, service fees
      z <- filter(town.derived(), method=='regional')
      z$min.service <- as.numeric(input$service.fee)
      mean.cost <- weighted.mean(z$opex.per.sub.per.mo+z$min.service+z$capex.fee.per.mo, z$units)
      cost.measures <- c('opex.per.sub.per.mo','min.service','capex.fee.per.mo')
      z2 <- melt(z, id="town", measure.vars=cost.measures, variable_name='cost')
      z2$value <- as.numeric(z2$value)
      z2$town <- as.character(z2$town)
      z2$cost <- factor(z2$cost, levels=c('min.service','opex.per.sub.per.mo','capex.fee.per.mo'))
      z2 <- arrange(z2, cost)
      ggplot(z2, aes(x=town,y=value,fill=cost,order=cost)) + geom_bar(stat='identity',position = "stack") + 
        ggtitle("Monthly Subscriber Costs (Regional)") + ylab("$/month") + 
        theme(axis.text.x  = element_text(angle=45, vjust=1, hjust=1)) + 
        geom_hline(aes(yintercept=mean.cost))+geom_text(aes(0,mean.cost,label = sprintf("$%.0f",mean.cost), vjust = -1, hjust=-1)) +
        scale_fill_discrete(name  ="Costs",
                             breaks=c('capex.fee.per.mo','opex.per.sub.per.mo','min.service'),
                             labels=c("CapEx Fee","MLP Fee","Internet Service"))
  })

})
