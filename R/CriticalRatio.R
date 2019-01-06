
#' Criticalratio
#'
#' Calculating critical ratio of a news vendor model under any distribution.this critical ratio maxmizes profit.
#'
#'@param  sellingprice  numeric,selling price of the SKU
#' @param  cost  numeric,cost of the SKU
#' @param  salvage  numeric,,salvage or discounted value if sold after season,if there is no salvage , zero is placed in the argument.
#' @param  penality numeric, peanlity cost of not satisfying demand if any, if not, zero is placed in the argument.
#' @param  na.rm A logical indicating whether missing values should be removed
#'
#'
#' @return the critical ratio.
#'
#'@import stats

#' @export
#'
#' @example
#'
#'CriticalRatio(sellingprice=80,cost=60,salvage=45,penality=25,na.rm=TRUE)
#'


CriticalRatio<- function(sellingprice,cost,salvage,penality,na.rm=TRUE){
  (sellingprice-cost+penality)/(sellingprice-cost+penality+cost-salvage)
}











