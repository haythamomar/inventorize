#' EUSnorm_singleperiod
#'
#' Calculating expected unit short based on an assumed normal distribution.
#'
#' Calculating expected unit short based on an assumed normal distribution for a newsvendor model.
#'
#' @param  quantity, numeric,quantity replinished every cycle.
#' @param demand numeric,annual Expected  demand of the SKU .
#' @param standerddeviation numeric,  standard  deviation of the SKU during season.
#'@param na.rm  logical,TRUE
#'
#'
#'
#' @return a dataframe that contains Expected unit short,k and g(k).
#'
#'@import stats
#' @export
#'
#' @example
#'\dontrun{
#' EUSnorm_singleperiod(quantity=35000,demand=32000,standerddeviation=12000,na.rm=TRUE)
#'}
#' @author "haytham omar  email: <h.omar5942@gmail.com>"
#' @note this is the first version of the inventorize package, all the fucntions are common knowlege for supply chain without
#' any contribution from my side, the aim is to facilitate and ease much of the bookkeeping that is endured during stock analysis.
#'




EUSnorm_singleperiod<-function(quantity,demand,standerddeviation,na.rm=TRUE){
  k<- (quantity-demand)/standerddeviation
  gk<- dnorm(k,0,1)-(k*(1-pnorm(k)))
  eus<- gk*standerddeviation
  return(data.frame(k=k,gk=gk,eus=eus))
}



