library(shiny)

library(plyr)
library(dplyr)
library(ggplot2)
library(scales)
library(reshape)
library(googleCharts)
library(RColorBrewer)

# There are two depreciation strategies:
# Crocker/Leverett: scale the Leverett plant by road miles and Leverett electronics by units,
# MLP/MBI: use MBI's not to exceed number as the cost of the total plant and take 3%.
# In the latter case, I don't separate plant and electronics
depreciation <- function(miles, units, cost, poles, input) {
  
  if (input$depreciation_method == 'mbi') {
    depr.cost <- cost
    if (input$exclude_makeready) {
      depr.cost <- depr.cost - poles*input$make.ready
    }
    if (input$exclude_electronics) {
      depr.cost <- depr.cost - input$install.percent/100*units*input$electronics
    }
    depr.cost <- depr.cost*0.03
  } else if (input$depreciation_method == 'none') {
    depr.cost <- 0
  } else if (input$depreciation_method == 'schedule') {
    depr.cost <- cost
    if (input$exclude_makeready) {
      depr.cost <- depr.cost - poles*input$make.ready
    }
    depr.cost <- (depr.cost * 0.2 / input$electronics.life) + (depr.cost * 0.8 / input$fiber.life)
  } else {
    depr.cost <- input$fiber.plant.depreciation*miles + input$electronics.depreciation*units*input$install.percent/100
  }
  return(depr.cost)
}

# The plant.opex are fixed costs.
# I assume these costs are linear in the counts (miles, poles, etc.)
# That is, no economy of scale as increase size
plant.opex <- function(n_towns, miles, poles, units, input) {
  as.integer(input$insurance*miles +
               (input$bond.fees+input$pole.rental)*poles +
               input$routine.mtnce*units*input$install.percent/100 +
               input$electricity.per.hut * n_towns * input$avg.huts.per.town)
}

# The netop.opex is the cost of the Network Operator subcontract.
# Crocker estimates a base cost per town and a cost per subscriber.
# THIS NEEDS WORK
netop.opex <- function(n_towns, subscribers, input) {
  if (input$backhaul.connections == 'Per Town' && any(n_towns>1)) {
    # reconstruct non-cumulative town subscribers
    per.town.subscribers <- subscribers - head(c(0,subscribers),length(subscribers))
    backhaul.cost <- ceiling(per.town.subscribers/input$units.per.gb) * input$backhaul.gb.price
    backhaul.cost <- cumsum(backhaul.cost)
  } else {
    backhaul.cost <- (ceiling(subscribers/input$units.per.gb) * input$backhaul.gb.price) 
  }
  input$network.operator.base * n_towns + input$network.operator*subscribers + backhaul.cost
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
contingency <- function(tot.opex, n_towns, input) {
  if (input$contingency_method == 'percentage') {
    return(tot.opex * input$return.pct/100)
  } else if (input$contingency_method == 'capped') {
    return(pmin(input$return.max, input$return.amt*n_towns))
  } else {
    return(input$return.amt*n_towns)
  }
}


# calculate annual payments and then total
# http://mathforum.org/library/drmath/view/67057.html
debt.svc <- function(principal, years, interest.rate) {
  return( years * (principal*interest.rate) / (1-(1+interest.rate)^(-years)) )
}

subscribers.fnc <- function(units, vacancy, seasonal, input, take.rate) {
  non.vacant <- (1-vacancy)*units
  as.integer(((1-seasonal)*non.vacant + input$seasonal.month/12*seasonal*non.vacant)*take.rate/100)
}

# given a set of town data in ts and user inputs, compute the opex for each town individually and regionally.
town.region.costs <- function(ts, input) {
  if (nrow(ts) == 0) {
    return(data.frame())
  }

  per.town.data <-
    mutate(ts,
           n_towns=1,
           plant.opex=plant.opex(1, miles, poles, units, input),
           netop.opex=netop.opex(1,subscribers, input),
           admin.opex=admin.opex(1, input),
           contingency=as.integer(contingency(plant.opex+netop.opex+admin.opex, 1, input)),
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
           subscribers=cumsum(subscribers),
           plant.opex=plant.opex(n_towns, miles, poles, units, input),
           netop.opex=netop.opex(n_towns, subscribers, input),
           admin.opex=admin.opex(n_towns, input),
           contingency=as.integer(contingency(plant.opex+netop.opex+admin.opex, n_towns, input)),
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
  return(costs)
}

# set units, miles, and poles based on data set selection
# set seasonal based on units selected
# compute subscribers and debt-related info per town based on take.rate selected
update.town.data <- function(td, input, take.rate=input$take.rate) {
  td$units <- td[[paste0(input$data.source,'_Units')]]
  td$miles <- td[[paste0(input$data.source,'_Miles')]]
  td$poles <- td[[paste0(input$data.source,'_Poles')]]
  
  # Jim uses 2010 census. Towns also provided MBI with count of non-primary *premises* (not units). Most towns reported zero.
  # For those towns that reported non-zero, we don't know whether each non-primary premise corresponds to
  # one or more units, so I assume that all of the non-primary premises are single-unit.
  # If there is a non-zero MBI number, then I use that to compute the seasonal percentage. Otherwise,
  # I use Jim's census data, i.e. (seasonal / (premises - vacant))
  td$seasonal <- with(td, ifelse(mbi_non_primary>0,mbi_non_primary/((1-vacancy)*units),census_seasonal))
  
  return(mutate(td,
                electronics=ifelse(rep(input$exclude_electronics,nrow(td)),units*input$electronics*input$install.percent/100,0),
                capex=capex-electronics,
                subscribers=subscribers.fnc(units,vacancy, seasonal, input, take.rate),
                depreciation=as.integer(depreciation(miles, units, total_cost, poles, input)),
                debt=as.integer(debt.svc(capex, input$years, input$interest.rate/100)),
                proptax.per.mo=round((debt * (1-input$debt.responsibility/100)) / input$years / total_assessed * avg_sf_home / 12, 2),
                capex.fee.per.mo=round(debt*input$debt.responsibility/100 / input$years / subscribers / 12,2),
                depr.resv.per.mo=round(depreciation/subscribers/12,2))
  )  
}

# returns a JavaScript function for formatting a numeric field as money
JSmoney <- function() {
  DT::JS('function(data, type, row, meta) { return type=="display"?"$" + data.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ","):data }')
}

#####################################################################################

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

shinyServer(function(input, output, session) {

  # sets the town selection based on the grouping (All, WiredWest, etc.)
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
    } else if (input$towngroups == 7) {
      updateSelectizeInput(session, "townnames", selected=c('Ashfield','Plainfield','Cummington','Goshen'))
    } else if (input$towngroups == 8) {
      updateSelectizeInput(session, "townnames", selected=c('Plainfield','Cummington','Goshen','Chesterfield','Worthington'))
    } else if (input$towngroups == 9) {
      updateSelectizeInput(session, "townnames", selected=c('Ashfield'))
    }
  })
  

  tier2.delta <- reactive({
    # fee = (fee+a) * r_a + (fee+b) * r_b
    # b = ((fee - (fee+a)*r_a)/r_b - fee)
    mean.fee <- as.numeric(input$service.fee)
    tier1.delta <- as.numeric(input$tier1delta)
    tier1.takerate <- as.numeric(input$tier1takerate)/100
    return( ( ( mean.fee - (mean.fee+tier1.delta)*tier1.takerate ) / (1-tier1.takerate) ) - mean.fee )
  })
  output$tier1price <- renderText(round(as.numeric(input$service.fee)+as.numeric(input$tier1delta),2))
  output$tier2price <- renderText(round(as.numeric(input$service.fee)+tier2.delta(),2))

    
  # returns just the town data for the towns specified by user. Also computes subscriber, debt-related costs given the user parameters
  town.subset <- reactive({
    ss <- filter(town.data, town %in% input$townnames) # use only user-selected towns
    ss <- update.town.data(ss, input)  # add subscriber and debt-related data using user-selected take.rate
    return(ss)
  })
  
  
  town.derived <- reactive({
    # Compute costs, revenue, net income per town
    
    td <- town.region.costs(town.subset(), input)

    # for debug purposes, keep a copy of all the core computed data and save a copy of input as a list instead of a reactive object.
    # if (!file.exists("z.Rdata")) {
    #   INP <- list()
    #   for (i in names(input)) { INP[[i]] <- input[[i]] }
    #   save(td, INP, file='z.Rdata')
    # }

    return(td)
    
  })  
  
  # The optimum MLP fee is the MLP fee that is in the last row of the cumulative calculations,
  # since the last row includes *all* selected towns.
  opt.mlp.fee <- reactive({
    z <- town.derived()
    mlp.fee <- z[z$method=='regional','opex.per.sub.per.mo']
    mlp.fee[length(mlp.fee)]
  })
  output$opt.mlp.fee <- renderText(round(opt.mlp.fee(),2))
  
  # See http://rstudio.github.io/DT/options.html for DT::renderDataTable options
  # there is supposed to be an export extension, but I cannot get it to work. http://rstudio.github.io/DT/extensions.html

  # Returns the table displayed as "Town Stats"
  output$basic.town.data <- DT::renderDataTable(
    { 
      x <- arrange(town.subset()[,c('town','units','vacancy','seasonal', 'subscribers','miles','poles',
                                    'avg_sf_home','total_assessed',
                                    'capex','debt','proptax.per.mo','capex.fee.per.mo','depr.resv.per.mo')]
                   ,town)
      x$seasonal <- sprintf("%d%%", as.integer(100*x$seasonal))
      x$vacancy <- sprintf("%d%%",as.integer(100*x$vacancy))
      return(x)
    }, 
    rownames=FALSE, 
    colnames=c('Town','Units','Vacancy Rate','Seasonal Rate','Subs','Miles','Poles','Avg Single Family',
               'Total Assessed', 'Town Capex', 'Town Capex w/ Interest', 'Tax / Home / Month','Debt Service / Sub / Month', 'Depr Resv / Month'),
    options=list(dom='t',paging=FALSE, columnDefs=list(list(targets=2:3,class="dt-right"),list(targets=7:13,render=JSmoney())))
  )
  
  # Returns the breakdown of costs per town in the 4 categories
  output$town.costs2 <- DT::renderDataTable(
    {
      z <- town.derived()
      z$town <- as.character(z$town)
 
      sa <- filter(z, method=='standalone')
      rg <- filter(z, method=='regional')
      if (input$regional.standalone.display=='Regional') {
        opex.per.sub.per.mo <- tail(rg,1)$opex.per.sub.per.mo  # single cost in regional for all
      } else {
        opex.per.sub.per.mo <- sa$opex.per.sub.per.mo
      }
      capex.fee.per.mo <- sa$capex.fee.per.mo
      depr.resv.per.mo <- sa$depr.resv.per.mo
      data.frame('Town'=sa$town,
                 'Internet Service'=as.integer(input$service.fee), 
                 'MLP Fee'=opex.per.sub.per.mo,
                 'Depr Resv Fee'=depr.resv.per.mo,
                 'Debt Service Fee'=capex.fee.per.mo,
                 'Total Monthly Cost'=as.integer(input$service.fee)+opex.per.sub.per.mo+depr.resv.per.mo+capex.fee.per.mo)
    },
    rownames=FALSE,
    colnames=c('Town','Internet Service','MLP Fee','Depreciation Reserve Fee','Debt Service Fee','Total Monthly Cost Per Subscriber'),
    options=list(dom='t',paging=FALSE,columnDefs=list(list(targets=1:5, render=JSmoney())))
    
  )
  
  # Returns the table displayed as "Standalone" costs per town
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
    colnames=c('Town','Plant','Network Oper','Admin',
               'Contingency','Total Annual Opex',
               'Opex/Sub/Month'),
    options=list(dom='t',paging=FALSE,columnDefs=list(list(targets=1:6, render=JSmoney())))
  )

  # Returns the table displayed as "Regional" costs, i.e. displays the totals for a cumulative
  # scenario in which the "region" corresponding to row N includes town in row 1 to N.
  output$regional.costs <- DT::renderDataTable(
    {
      x <- arrange(town.derived(),desc(town))
      x$town <- as.character(x$town)
      z <- filter(x, method=='regional')[,c('town','plant.opex','netop.opex','admin.opex',
                                            'contingency','total.opex','opex.per.sub.per.mo')]
      return(z)
    }, 
    rownames=FALSE,
    colnames=c('Town','Plant','Network Operator','Administration',
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
    ggplot(town.derived(), aes(x=town,y=net.per.sub.per.mo,fill=method)) + geom_bar(stat='identity',position='dodge') + coord_flip() +   ggtitle("Net Income Per User") + ylab("$ / subscriber / month")
  })
  
  output$reqd.mlp.fee <- renderPlot({
    z<-arrange(town.derived(), method)
    if (nrow(z) == 0) {
      return(ggplot()+geom_blank())
    }
    max.regional.mlp <- tail(filter(z,method=='regional'),1)$opex.per.sub.per.mo
    ggplot(z, aes(x=town,y=opex.per.sub.per.mo,color=method)) + geom_point(aes(size=subscribers)) + 
      geom_hline(aes(yintercept=max.regional.mlp)) +
      coord_flip() +
      ggtitle("Required Fee Per Subscriber to Cover Opex (MLP Fee)") + ylab("$/month") +  
      scale_y_continuous(breaks=pretty_breaks(10))  + 
      scale_color_discrete(breaks=c('standalone','regional'), labels=c("Standalone","Regional"))
  })
  
  output$subscriber.fees <- renderPlot({
    # stacked plot of debt, depr resv, mlp fee, service fees
    td <- town.derived()
    if (nrow(td)==0) {
      return(ggplot()+geom_blank())
    }
    regional.c <- tail(filter(td, method=='regional'),1)$opex.per.sub.per.mo
    z <- filter(td, method=='standalone')
    z$min.service <- as.numeric(input$service.fee)

    mean.cost.per.town.1 <- mean(z$opex.per.sub.per.mo+z$min.service+z$capex.fee.per.mo+z$depr.resv.per.mo)
    if (input$regional.standalone.display == 'Regional') {
      # replace the cost for opex with the cost if shared across all towns
      z$opex.per.sub.per.mo <- regional.c
    }
    # average cost per town
    mean.cost.per.town <- mean(z$opex.per.sub.per.mo+z$min.service+z$capex.fee.per.mo+z$depr.resv.per.mo)

    # average cost per subscriber
    mean.cost.per.user <- weighted.mean(z$opex.per.sub.per.mo+z$min.service+z$capex.fee.per.mo+z$depr.resv.per.mo, z$subscribers)
    
    cost.measures <- c('opex.per.sub.per.mo','min.service','capex.fee.per.mo','depr.resv.per.mo')
    z2 <- melt(z, id="town", measure.vars=cost.measures, variable_name='cost')
    z2$value <- as.numeric(z2$value)
    z2$town <- as.character(z2$town)
    z2$cost <- factor(z2$cost, levels=c('min.service','opex.per.sub.per.mo','capex.fee.per.mo','depr.resv.per.mo'))
    z2 <- arrange(z2, cost)
    
    base.plot <- ggplot(z2, aes(x=town,y=value,fill=cost,order=cost)) + 
      geom_bar(stat='identity',position = "stack") + 
      ggtitle(sprintf("Monthly Subscriber Costs (%s)",input$regional.standalone.display)) + 
      ylab("$/month") + 
      theme(axis.text.x  = element_text(angle=45, vjust=1, hjust=1)) + 
      scale_fill_brewer(name  ="Costs",
                        breaks=c('capex.fee.per.mo','depr.resv.per.mo','opex.per.sub.per.mo','min.service'),
                        labels=c("Debt Service Fee","Depr Resv Fee","MLP Fee","Internet Service"),
                        palette = 'Set3', direction = -1)
    
    if (input$tiers == 1) {
      return(base.plot 
             + geom_hline(aes(yintercept=mean.cost.per.user))+geom_text(aes(0,mean.cost.per.user,label = sprintf("$%.0f",mean.cost.per.user), vjust = 1, hjust=-1), size=10))  
    } else {
      tier1.price <- round(mean.cost.per.user + as.numeric(input$tier1delta),2)
      tier2.price <- round(mean.cost.per.user + tier2.delta(),2)
      return(base.plot 
             + geom_hline(aes(yintercept=tier1.price), color='yellow', size=2)+geom_text(aes(0,tier1.price,label = sprintf("$%.0f",tier1.price), vjust = 1, hjust=-1), size=10)
             + geom_hline(aes(yintercept=tier2.price), color='purple', size=2)+geom_text(aes(0,tier2.price,label = sprintf("$%.0f",tier2.price), vjust = 1, hjust=-1), size=10)
             )
    }
        
  })
  
  # This is moderately compute intensive because the mean.cost is computed over a range of take rates instead of just one
  cost.vs.take.rate.plot.data <- reactive({
    plot.data <-
      ldply(seq(5,100,5),function(take.rate) { 
        ss <- filter(town.data, town %in% input$townnames) # use only user-selected towns
        ss <- update.town.data(ss, input, take.rate)

        z <- town.region.costs(ss, input)
        sa <- filter(z, method=='standalone')
        rg <- filter(z, method=='regional')
        mean.opex.per.sub.per.mo <- tail(rg,1)$opex.per.sub.per.mo  # single cost in regional for all
        mean.capex.fee.per.mo <- weighted.mean(sa$capex.fee.per.mo, sa$subscribers)  # varies per town
        mean.depr.resv.per.mo <- weighted.mean(sa$depr.resv.per.mo, sa$subscribers)  # varies per town
        return(c(take.rate=take.rate,
                 'Internet Service'=as.integer(input$service.fee), 
                 'MLP Fee'=mean.opex.per.sub.per.mo,
                 'Depr Resv Fee'=mean.depr.resv.per.mo,
                 'Debt Service Fee'=mean.capex.fee.per.mo))
      })
    plot.data <- melt(plot.data, id='take.rate', measure.vars=2:5, variable_name='Cost')
    return(plot.data)
  })
  
  output$cost.vs.take.rate <- renderPlot({
    plot.data <- cost.vs.take.rate.plot.data()
    current.cost <- sum(plot.data[plot.data$take.rate==input$take.rate,'value'])
    if (input$take.rate >= 40) { take.rate.xlim <- xlim(35,105) } else { take.rate.xlim <- xlim(0,105)}
    ggplot(plot.data, aes(x=take.rate, y=value, fill=Cost)) + geom_bar(stat='identity',position = "stack") +
      geom_hline(aes(yintercept=current.cost), size=1.5)+geom_text(aes(max(plot.data$take.rate),current.cost,label = sprintf("$%.0f",current.cost), vjust =-1, hjust=1), size=10) +
      geom_vline(aes(xintercept=input$take.rate), size=1.5) + xlab("Take Rate (%)") + ylab("Subscriber Cost Per Month ($)") +
      ggtitle("Monthly Subscriber Costs (Regional) vs Take Rate") + take.rate.xlim + scale_fill_brewer(palette = 'Set3', direction=-1)
  })
  
  output$opex.pie <- reactive({
    td <- town.derived()
    regional.c <- tail(filter(td, method=='regional'),1)
    
    pie.data <- data.frame(name=c('Plant','Network Operator','Administration',
                                  'Contingency'),
                           cost=c(regional.c$plant.opex,regional.c$netop.opex,
                                  regional.c$admin.opex,regional.c$contingency))
    
    # Return the data and options
    list(
      data = googleDataTable(pie.data),
      options = list(
      )
    )
  })
    

})
  