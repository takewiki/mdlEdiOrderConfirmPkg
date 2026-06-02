#' EDI销售订单更新
#'
#' @param FMessageNumber
#' @param FLineItemNumber
#' @param FCommittedQuantity
#' @param FCommittedQuantityUOM
#' @param FCommittedQuantityConfirmedDate
#' @param dms_token
#'
#' @return 两个数的和
#' @export
#'
#' @examples
#' EdiOrderConfirm_view()
EdiOrderConfirm_update <- function(dms_token, FMessageNumber, FLineItemNumber, FCommittedQuantity, FCommittedQuantityUOM, FCommittedQuantityConfirmedDate) {


  sql=paste0("
             exec rds_proc_UpdateORDRSPSchedule '",FMessageNumber,"', '",FLineItemNumber,"', '",FCommittedQuantity,"', '",FCommittedQuantityUOM,"', '",FCommittedQuantityConfirmedDate,"'
             ")

  res = tsda::sql_update2(token =dms_token ,sql_str = sql)



  return(res)

}
