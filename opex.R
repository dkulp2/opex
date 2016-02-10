library(dplyr)
library(ggplot2)

fiber.plant.depreciation <- 1395 # per mile
insurance <- 442                 # per mile
bond.fees <- 3                   # per pole
pole.rental <- 13                # per pole
electronics.depreciation <- 63   # per premise
routine.mtnce <- 39              # per premise
network.operator.base <- 16800   # per town(!)
network.operator <- 36           # per subscriber
purma.dues <- 1200               # per town
accountant <- 3000               # per town
bookkeeping.etc <- 5000          # per town
legal <- 10000                   # per town
return.pct <- .05                # percent of opex (contingency fund)
take.rate <- 0.75
avg.price.per.sub <- 70          # average revenue per subscriber given product mix



# The plant.opex are fixed costs.
# I assume these costs are linear in the counts (miles, poles, etc.)
# That is, no economy of scale as increase size
plant.opex <- function(miles, poles, premises, subscribers) {
  (fiber.plant.depreciation+insurance)*miles +
    (bond.fees+pole.rental)*poles +
    (electronics.depreciation+routine.mtnce)*premises
}

# The netop.opex is the cost of the Network Operator subcontract.
# Crocker estimates a base cost per town and a cost per subscriber.
# This needs work.
netop.opex <- function(subscribers) {
  network.operator.base +
  network.operator*subscribers
}

# The admin.opex are the costs for bookkeeping, etc., which Crocker
# estimates as potentially a 50% savings when regionalized.
# This needs work.
admin.opex <- function() {
  purma.dues+accountant+bookkeeping.etc+legal
}

# A contingency fund for paying deductibles or other issues 
# is based on a percent of total opex.
contingency <- function(tot.opex) {
  tot.opex * return.pct
}

# premises, miles and poles per town, e.g.
#                  premises miles poles
# Alford                357    26   785
# Egremont             1034    56  1837
# Hancock               755    26   758
# Lanesborough         1848    52  1423
# ...
town.data <- read.table('towndata.txt', header=T, row.names = 1)
town.data$town <- row.names(town.data)

# Compute costs, revenue, cash flow per town
town.data <-
  mutate(town.data,
         n_towns=1,
         plant.opex=plant.opex(miles, poles, premises, premises*take.rate),
         netop.opex=netop.opex(premises*take.rate),
         admin.opex=admin.opex(),
         isp.opex=0, # fix me
         contingency=contingency(plant.opex+netop.opex+admin.opex+isp.opex),
         total.opex=plant.opex+netop.opex+admin.opex+isp.opex+contingency,
         revenue=avg.price.per.sub*premises*take.rate*12,
         cash.flow=revenue-total.opex,
         monthly.cost.per.sub=total.opex/(premises*take.rate)/12,
         monthly.net.per.sub=avg.price.per.sub-monthly.cost.per.sub)

# For plotting purposes, reorder town names by cash.flow
town.data$town <- factor(town.data$town, levels=town.data$town[order(town.data$cash.flow)])
town.data <- arrange(town.data, desc(town))

ggplot(town.data, aes(x=town,y=cash.flow)) + geom_bar(stat='identity') + coord_flip() + ggtitle("Cash Flow Per Town") + ylab("$ / town")

ggplot(town.data, aes(x=town,y=monthly.cost.per.sub)) + geom_bar(stat='identity') + coord_flip() + ggtitle("OpEx Per Subscriber (MLP Fee)") + ylab("$/month")

# Now generate cumulative values in order of most affordable first
town.data.cum <-
  mutate(town.data, 
         miles=cumsum(miles), 
         poles=cumsum(poles), 
         premises=cumsum(premises),
         n_towns=1:nrow(town.data),
         plant.opex=plant.opex(miles, poles, premises, premises*take.rate),
         netop.opex=netop.opex(premises*take.rate),
         admin.opex=cumsum(admin.opex)/2,
         isp.opex=0, # fix me
         contingency=contingency(plant.opex+netop.opex+admin.opex+isp.opex),
         total.opex=plant.opex+netop.opex+admin.opex+isp.opex+contingency,
         revenue=avg.price.per.sub*premises*take.rate*12,
         cash.flow=revenue-total.opex,
         monthly.cost.per.sub=total.opex/(premises*take.rate)/12,
         monthly.net.per.sub=avg.price.per.sub-monthly.cost.per.sub)

ggplot(town.data.cum, aes(x=town,y=cash.flow/n_towns)) + geom_bar(stat='identity') + coord_flip() +
  ggtitle("Cash Flow Per Town REGIONAL CUMULATIVE") + ylab("$ / town")

ggplot(town.data.cum, aes(x=town,y=monthly.cost.per.sub)) + geom_bar(stat='identity') + coord_flip() + ggtitle("OpEx Per Subscriber REGIONAL CUMULATIVE") + ylab("$/month")

# combine the data.frames
costs <- cbind(rbind(town.data, town.data.cum), method=factor(rep(c('standalone','regional') , each=nrow(town.data))))

ggplot(costs, aes(x=town,y=cash.flow/n_towns,fill=method)) + geom_bar(stat='identity',position='dodge') + coord_flip() +   ggtitle("Cash Flow Per Town") + ylab("$ / town")
ggplot(costs, aes(x=town,y=monthly.cost.per.sub,fill=method)) + geom_bar(stat='identity',position = "dodge") + coord_flip() + ggtitle("OpEx Per Subscriber") + ylab("$/month") 
