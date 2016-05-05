#debatable = "color:orange"
debatable = ''

docs <- 
  list('Standalone Opex' = 
         div(
           p("Per unit costs in the 'Opex Parameters' panel are initially based on Leverett. 
             Plant Opex is computed as the sum of insurance, pole fees, pole bond, routine maintenance, 
             and electricity. Insurance is currently crudely computed on a per mile basis; insurance would
             likely be lower if purchased over a large number of towns. Routine maintenance is estimated
             as a function of the number of drops."),
           p("Depreciation is computed as one of two methods defined in the 'Finance' tab, namely, 
             either as a scaled depreciation based on Leverett or as 3% of the total capital cost. Make ready and electronics are optionally excluded. See the 'Discussion' tab."),
           p("Network operator is currently a flat per town cost plus a per drop cost from Crocker's 
             estimate of an integrated NO/ISP. See the 'Discussion' tab for more information."),
           p("Admin costs are detailed in the 'Opex Parameters' panel.")),
       'Regional Opex' =
         div(
           p("Regional costs are identical to standalone, but the total costs are computed 
             cumulatively starting with the town with the highest return per subscriber. The only 
             economies of scale are currently aggregated bandwidth (if selected in model) and a 
             minor administrative costs savings in which it is assumed, crudely, that administrative 
             costs would be halved if shared among two or more towns. Admin represents usually about 
             5% of costs, so is not significant. There is likely to be significant costs savings in a 
             large, multi-town contract for network operator and ISP, but those costs are currently based 
             on Crocker's integrated NO/ISP estimates per town. "),
           p("Another area of possible savings is insurance, which probably does not scale linearly with 
             road miles as is modeled here. At the default scale factor, annual insurance for 32 WiredWest 
             towns would be almost $700,000, which is far higher than anticipated.")
         ),
       'Town Stats' =
         div(
           p("Units refers to the number of possible drops; there can be multiple units per premise. 
             This number comes from MBI premise counts published September 2015. 
             Vacancy rate is calculated from the 2010 census. If a town reported to the MBI its 
             non-primary premise count, then the seasonal rate is the fraction defined as 
             'non-primary premises' divided by 'non-vacant units', assuming that non-primary premises
             are single-unit. Otherwise the seasonal rate is computed from the 2010 census. 
             Subscribers is computed from the non-vacant units where the seasonal percentage is 
             pro-rated based on the seasonal months parameter. 
             Miles and Poles are from MBI. 'Avg Single Family' and 'Total Assessed' are the assessed
             values from the DLS Data Bank (Jim Drawe). Capex refers to the town's borrowed amount 
             requested by MBI. 
             'Capex w/ Interest' is the total debt using equal annual payments. 'Tax/Home/Month' 
             and 'Capex Fee/Sub/Mo' split the debt across the tax base and the subscribers according
             to the user parameter 'Fraction of Debt Covered by Subscribers' under the
             'Financing' tab.")
         ),
       'Discussion' =
         div(
           h1("Model parameters and areas for regional savings"),
           h3("Contingency / Profit"),
           p("Leverett budgets a contingency based on a percentage of operating expense. 
MBI/WiPro published estimates requiring a $50,000/yr 'profit' per town, which I interpret as a contingency.
Under a regional model one can argue that sharing costs should also imply sharing of contingency among towns. 
After all, if 40 towns participated, then a total required profit of $2M per year seems excessively high,
particularly considering that it acrues year after year. Thus, it seems reasonable to also have a regional 'cap'
on profit. My default is a 5% contingency, but there are options to set a fixed profit and also a regional cap."),
           h3("ISP Service Fee and Backhaul",style=debatable),
           p("The model uses an ISP cost for minimum service based on Crocker's current offering in Leverett. I would like to include the cost of backhaul in the ISP cost, however in the current model the owner/operator pays for backhaul as part of network operator expenses."),
           p("Including backhaul as part of the ISP better aligns the interests of the ISP and owner; it allows the ISP to have almost full control of customer satisfaction and the ISP can choose the appropriate oversubscription rate. Moreover, since backhaul demand increases with subscribers and is partially dependent on subscribed speeds (if tiered service is offered) then the ISP seems to be best positioned to manage backhaul bandwidth allocation. The net impact should be a wash with backhaul averaging about $5 per month as either an operator cost or a subsciber cost."),
           h3("Backhaul connections",style=debatable),
           p("When multiple towns are selected, then the total required backhaul is summed
and a single aggregate cost is computed, by default. This assumes that there is leased 
dark fiber among the towns and a single aggregated backhaul point. The cost of the additional
dark fiber is unknown and not included. There is also an option to compute the
cost per town, i.e. each town has its own connection to the MB123. The savings in aggregating
             backhaul is typically less than $3/sub/mo."),
           p("This model does not currently include any economy of scale that could be realized
             by purchasing a large amount of backhaul. Currently Axia does not have discounted
             rates for larger amounts, but this may change in the future. The impact, though,
             can be determined by trying different amounts for backhaul in the 'Net Op' parameters."),
           h3("Depreciation Reserve Fund",style=debatable),
           p("The cost to fund a required depreciation reserve (different from depreciation accounting) 
             is a major operating expense - typically representing at least 1/3 of costs - that is
uniquely required by MLP law. There are two 
             options here for computing the depreciation reserve. The first is a scaled depreciation 
             based on Leverett's road miles and unit counts. This is likely an over-estimate and includes 
             separate depreciation schedules for fiber and electronics, which are not required for the 
             depreciation reserve fund."),
           p("The other method is simpler and cheaper. The depreciation is computed as an annual set 
             aside of 3% of the cost of the plant - the minimum suggested, not required, by MLP law - which
             can be calculated by subtracting the make ready from the total capital cost. This is still 
             probably an overestimate since much of the remaining capital cost is engineering, which may  
             not to be repeated when replacing aging plant equipment. In addition, the model allows for
             the exclusion of electronics. The capital cost of electronics is estimated very roughly at $1000
per unit. This includes both the unit cost itself, plus installation and I assume core electronics. (This could use some work.) 
The model assumes that an ISP partner owns all electronics,
             which reduces depreciation reserves and borrowing. Third party ownership of electronics also
             would reduce operating expense (maintenance, service contracts, etc.), but the cost would 
             likely be compensated in ISP cost, so I don't attempt to model that change."),
           p("Independent of the legally required depreciation reserve fund amount, 
             there is the question of what is a prudent set aside. 
             Fiber optic cable is designed to last 20-25 years, but typically lasts much longer under
             normal conditions. There may be a desire in the future to upgrade electronics and possibly cable to handle newer optical
             technology, but it is also reasonable to minimize outlay to ensure
             affordability."),
           h3("Network Operator",style=debatable),
           p("Additional comparable network operator costs are needed. The NO represents a significant portion of the operating budget that could benefit from multiple towns working together. However, the current NO costs are based on an informal Crocker proposal document that budgets the network operator as a flat cost per town plus a cost per drop. There is no savings from multiple towns, although this seems like a prime opportunity for economies of scale."),
           h3("Insurance",style=debatable),
           p("Insurance is intuitively an area that is ripe for costs savings for a larger plant.
             I'm looking for guidance from PURMA to improve this part, which in practice is not 
             actually based on per-mile cost."),
           h3("Administration",style=debatable),
           p("Administration costs are a small fraction of the total expenses. However, there are opportunities for costs savings and efficiencies here."),
           h2("Potential Regional Savings"),
           p("A question of particular interest is what savings are available to subscribers when towns are part of a regional network. There are three kinds of savings: non-economic or intangible benefits, cost sharing, and economies of scale. For many of our towns, the administration and management of a telecommunications network is a new burden for town employees who are already over-committed. Sharing information, knowledge, and experience are common practice in our region. Sharing the required administrative work, perhaps by having it done by one or two trained people, is efficient and makes sense for many of our towns."),
           p("The second kind of savings is based on averaging costs over multiple towns. Operating a town network tends to be more expensive for the less populated towns. When small towns combine with larger towns, the subscribers in the smaller towns tend to realize substantial cost savings. From the perspective of the larger towns (and the bulk of the subscribers), the cost to subscribers may rise, but only marginally, because that extra cost is spread over many subscribers. (See the MLP Fee plot.)"),
           p("The third opportunity for savings is realized through economies of scale. Practically speaking, the main sources of such savings are probably in insurance, network operator, administration, and backhaul aggregation. Savings in each area may be modest, but the total could easily be $5 per subscriber per month. These savings accrue to all of the towns and help improve the sustainability of the network. For the larger towns these savings help offset any increases due to cost sharing."),
           p("In short, there are significant monetary savings for small towns; there are probably offsetting additional costs and potential additional savings for large towns. But there is an intangible advantage to all towns to team together to provide mutual support and improve the chance of a successful implementation. Overall, the towns benefit from a regional approach, even in a fully outsourced model."),
           hr(),h4('Feedback welcome. Email ',a('David Kulp.',href='mailto:dkulp@dizz.org'),' Thanks!'),
           h6("(",a('Source Code',href="https://github.com/dkulp2/opex"),")")
         ),
       'Subscriber Costs' =
         div(
           p("Subscriber costs here are for minimal service under the 'Single Tier' model. Costs are computed as the sum of a Debt Service Fee (if any), a minimum Internet Service Fee
             and an MLP Fee, representing the breakeven cost per subscriber to pay for sustaining the network asset. (See the 'Standalone Opex' panel for more information.)"),
           p("If 'Regional' is selected, then the MLP Fee is the same for all towns because opex is computed over the entire region and distributed uniformly to all subscribers. The horizontal line represents the same shared cost per subscriber across the network."),
           p("If 'Standalone' is selected, then the MLP Fee varies based on the cost per town. The horizontal line represents the average subscriber costs across different towns."),
           p("If 'Two-tier' is selected, then the purple horizontal line corresponds to the necessary cost for the higher tiered service and the yellow line corresponds to the manually adjusted lower tier."),
           p("The MLP Fee plot (lower plot) shows how the opex increases gradually in a regional
             approach, while the standalone costs for individual towns can be very high. 
             Each blue dot is the MLP Fee that that particularly town would pay on its own. 
             Each red dot corresponds to a break-even fee if that town ", 
             strong("and all towns listed above it"), 
             " joined together. The point size 
             corresponds to the total number of subscribers. Note that the highest cost towns
             are some of the least populated, but in aggregate the region can absorb those 
             higher costs with minimal impact on the individual subscriber.
             The vertical line corresponds to the maximum MLP Fee if all displayed towns 
             participated in a regional network and distributed costs. Those blue dots to the
             left of the vertical black line are standalone costs that are less than the regional
             cost; these towns 'subsidize' the regional network through slightly higher fees. 
             Those blue dots to the right of the black line pay less than they would individually.")
         ),
       'Net Income' =
         div(
           p("This plot shows the net income per user realized by a town or regional entity 
             given a fixed MLP Fee. A town or region should operate under sustainable conditions 
             such that net income is positive. The blue lines indicate the net income for individual 
             towns when operated independently. The orange lines indicate net income for a region of 
             towns that includes that town ",strong("and all towns listed above it."), " As the MLP Fee
             increases, more towns have positive cash flow if they were to operate independently. 
             The MLP Fee necessary for a region to operate in the black is lower than the MLP Fee 
             required for all towns to be cash flow positive independently. Of course this also means 
             that some towns are paying a larger MLP Fee to sustain less profitable towns, but the 
             cost impact is relatively small (typically less than $10 per month). In addition, towns 
             realize the benefit of cooperative work instead of suffering the burden of administration 
             replicated in each town.")
         ),
       'Cost vs Take Rate' =
         div(
           p("The plot displays the total cost per subscriber per month given the selected town
             or towns (regional), selected share of debt service paid by subscribers, and other 
             parameters.")
         )
       )

