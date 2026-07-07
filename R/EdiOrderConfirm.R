#' EDI销售订单更新
#'
#' @param FMessageNumber
#' @param FLineItemNumber
#' @param FCommittedQuantity
#' @param FCommittedQuantityUOM
#' @param FCommittedQuantityConfirmedDate
#' @param erp_token
#'
#' @return 两个数的和
#' @export
#'
#' @examples
#' EdiOrderConfirm_update()
EdiOrderConfirm_update <- function(erp_token, FMessageNumber, FLineItemNumber, FCommittedQuantity, FCommittedQuantityUOM, FCommittedQuantityConfirmedDate,FErpDeliveryDate) {


  sql=paste0("
exec rds_proc_UpdateORDRSPSchedule '",FMessageNumber,"', '",FLineItemNumber,"', '",FCommittedQuantity,"', '",FCommittedQuantityUOM,"', '",FCommittedQuantityConfirmedDate,"','",FErpDeliveryDate,"'
             ")

  res = tsda::sql_update2(token =erp_token ,sql_str = sql)



  return(res)

}



#' EDI销售订单查询
#'
#' @param FMessageNumber
#' @param FLineItemNumber
#' @param erp_token
#'
#' @return 两个数的和
#' @export
#'
#' @examples
#' EdiOrderConfirm_view()
EdiOrderConfirm_view <- function(erp_token, FMessageNumber, FLineItemNumber) {


  sql=paste0("
             exec rds_proc_Edi_GetORDRSPSchedule '",FMessageNumber,"','",FLineItemNumber,"'
             ")

  res = tsda::sql_select2(token =erp_token ,sql = sql)



  return(res)

}


#' EDI销售订单同步
#'
#' @param FMessageNumber
#' @param erp_token
#'
#' @return 两个数的和
#' @export
#'
#' @examples
#' EdiOrderConfirm_sync()
EdiOrderConfirm_sync <- function(erp_token, FMessageNumber) {


  sql=paste0("
             exec rds_proc_ORDRSPSsync '",FMessageNumber,"'
             ")

  res = tsda::sql_update2(token =erp_token ,sql_str = sql)



  return(res)

}




#' EDI销售订单表头查询
#'
#' @param FMessageNumber
#' @param erp_token
#'
#' @return 两个数的和
#' @export
#'
#' @examples
#' EdiOrderHeader_view()
EdiOrderHeader_view <- function(erp_token, FMessageNumber) {


  sql=paste0("
          select  distinct
      'XSDD-102-'+[MessageNumber] AS [FBillNo]
      , CONVERT(varchar(10), CONVERT(date, OrderDate, 112), 120) AS [FDate],
	  'XSDD01_SYS' as [FBillTypeID],
	  '102' as [FSaleOrgId],
	  b.FNUMBER as [FCustId],
	   '02.01' as [FSaleDeptId]
       ,'10208' as [FSaleGroupId]
       ,'0021_020103_100436' as [FSalerId] ,
	   c.fnumber as [F_nlj_country],
	   'A' as [F_kd_Assistant],
	   d.FNUMBER as [FSettleCurrId],
	   e.FNUMBER as [FRecConditionId],
	  sum( CAST(PriceDetailCalculationNetPrice as decimal(28,10))/ CAST(PriceDetailUnitPriceBasis as decimal(28,10))*CAST(OrderedQuantity as decimal(28,10))) over(partition by MessageNumber) as [FTotalRecAmount],
       0 as  [FIsDo]
           ,'待处理' as [FLogMessage]
           ,getdate() as [FUpdateTime]
           ,0 as [FErrorTimes]
           ,0 as [FNeedUpdate]
		   from rds_edi_ORDERSHeader a
		   left join T_BD_CUSTOMER b on b.FUSEORGID=100436 and b.F_EDI_CUSTOMERNUMBER<>'' and a.BuyerCode=b.F_EDI_CUSTOMERNUMBER
		   left join rds_vw_country c on b.FCOUNTRY=c.FID
		   left join T_BD_CURRENCY d on b.FTRADINGCURRID =d.FCURRENCYID
		   left join T_BD_RECCONDITION e on b.FRECCONDITIONID = e.FID
		   inner join rds_edi_ORDERSItem f on f.HeaderId=a.Id
      where A.FIsDo = 0  and [MessageNumber] = '",FMessageNumber,"'
             ")

  res = tsda::sql_select2(token =erp_token ,sql = sql)



  return(res)

}

#' EDI销售订单明细查询
#'
#' @param FMessageNumber
#' @param erp_token
#'
#' @return 两个数的和
#' @export
#'
#' @examples
#' EdiOrderItem_view()
EdiOrderItem_view <- function(erp_token, FMessageNumber) {


  sql=paste0("

select
      'XSDD-102-'+[MessageNumber] AS [FBillNo],
	   F.LineItemNumber AS FSeq,
	   F.ProductBuyerItemNumber as FMapId,
	   j.FNUMBER as FMaterialId,
	   'kg' as FUnitID,
	   cast(f.OrderedQuantity as [decimal](18, 4)) as FQty,
	   CAST(F.PriceDetailCalculationNetPrice as decimal(28,10))/ CAST(F.PriceDetailUnitPriceBasis as decimal(28,10)) as FPrice,
	   CAST(F.PriceDetailCalculationNetPrice as decimal(28,10))/ CAST(F.PriceDetailUnitPriceBasis as decimal(28,10)) as FTaxPrice,
	   'FL01' as F_RD_FANLI,
	   0 as FIsFree,
	   0 as FEntryTaxRate,
	   CAST(F.PriceDetailCalculationNetPrice as decimal(28,10))/ CAST(F.PriceDetailUnitPriceBasis as decimal(28,10))  as F_rds_productTaxPriceGL,
	   CAST(F.PriceDetailCalculationNetPrice as decimal(28,10))/ CAST(F.PriceDetailUnitPriceBasis as decimal(28,10))*CAST(F.OrderedQuantity as decimal(28,10)) as F_rds_productAmountGL,
	    CONVERT(varchar(10), CONVERT(date, FErpDeliveryDate, 112), 120) AS FDeliveryDate,
		'' as FBomId,
		'002' as F_QH_Industry,
		'0000' as F_kd_APPLICATION,
       0 as  [FIsDo]
           ,'待处理' as [FLogMessage]
           ,getdate() as [FUpdateTime]
           ,0 as [FErrorTimes]
           ,0 as [FNeedUpdate]
		   from rds_edi_ORDERSHeader a
		   left join T_BD_CUSTOMER b on b.FUSEORGID=100436 and b.F_EDI_CUSTOMERNUMBER<>'' and a.BuyerCode=b.F_EDI_CUSTOMERNUMBER
		   left join rds_vw_country c on b.FCOUNTRY=c.FID
		   left join T_BD_CURRENCY d on b.FTRADINGCURRID =d.FCURRENCYID
		   left join T_BD_RECCONDITION e on b.FRECCONDITIONID = e.FID
		   inner join rds_edi_ORDERSItem f on f.HeaderId=a.Id
		   inner join rds_edi_ORDERSSchedule_update g on g.ItemId=f.Id
		   LEFT join t_Sal_CustMatMapping h on h.FCUSTOMERID=b.FCUSTID
		   left join t_Sal_CustMatMappingEntry i on h.FID=i.FID and i.FCUSTMATNO =f.ProductBuyerItemNumber
		   left join T_BD_MATERIAL j on i.FMATERIALID=j.FMATERIALID
      where A.FIsDo = 0   and [MessageNumber] = '",FMessageNumber,"'
             ")

  res = tsda::sql_select2(token =erp_token ,sql = sql)



  return(res)

}


#' EDI销售订单状态更新
#'
#' @param FMessageNumber
#' @param erp_token
#'
#' @return 两个数的和
#' @export
#'
#' @examples
#' EdiOrderFIsDo_update()
EdiOrderFIsDo_update <- function(erp_token, FMessageNumber) {


  sql=paste0("
  update a set  FIsDo=1,FLogMessage='已同步' from  rds_edi_ORDERSHeader a where
		   FIsDo = 0 and [MessageNumber]=  '",FMessageNumber,"'

             ")

  res = tsda::sql_update2(token =erp_token ,sql_str = sql)



  return(res)

}


#' EDI销售订单确认删除
#'
#' @param FMessageNumber
#' @param FDate
#' @param erp_token
#'
#' @return 两个数的和
#' @export
#'
#' @examples
#' EdiOrderConfirm_delete()
EdiOrderConfirm_delete <- function(erp_token, FMessageNumber,FDate) {


  sql=paste0("
  exec rds_proc_Edi_OrderConfirm_delete  '",FMessageNumber,"','",FDate,"'

             ")

  res = tsda::sql_delete2(token =erp_token ,sql_str = sql)



  return(res)

}




#' EDI数据中台销售订单确认删除
#'
#' @param dms_token
#' @param FBillNo
#' @param FDate
#'
#' @return 两个数的和
#' @export
#'
#' @examples
#' dms_EdiOrderConfirm_delete()
dms_EdiOrderConfirm_delete <- function(dms_token, FBillNo,FDate) {


  sql=paste0("
  exec rds_proc_Edi_salesOrder_delete  '",FBillNo,"','",FDate,"'

             ")

  res = tsda::sql_delete2(token =dms_token ,sql_str = sql)



  return(res)

}
