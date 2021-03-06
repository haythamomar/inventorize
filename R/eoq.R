
#' eoq
#'
#' economic order quantity.
#'
#' .
#'
#'
#' @param annualdemand numeric,annual demand of the SKU.
#' @param  orderingcost, numeric ordeing cost of the SKU
#' @param  purchasecost ,numeric, purchase cost per item
#' @param holdingrate    numeric holding rate per item per year.
#' @param na.rm A logical indicating whether missing values should be removed
#'
#' @return the eoq.cycle stock time in years and cycle stock time in weeks.
#'
#' @export
#'
#' @example
#'
#' eoq(annualdemand=5000,orderingcost=400,purchasecost=140,holdingrate=0.2,na.rm=TRUE)
#'
#' @author "haytham omar  email: <h.omar5942@gmail.com>"
#' @note this is the first version of the inventorize package, all the fucntions are basic knowlege for supply chain without
#' any contribution from my side, the aim is to facilitate and ease much of the bookkeeping that is endured during analysis of stock.
#'





eoq<- function(annualdemand,orderingcost,purchasecost,holdingrate,na.rm=TRUE){

  eoq<-sqrt((annualdemand*2*orderingcost)/(purchasecost*holdingrate))
  T_years<- eoq/annualdemand
  T_weeks<- T_years*52
  return(data.frame(EOQ=eoq,T_years=T_years,T_weeks=T_weeks))
}


