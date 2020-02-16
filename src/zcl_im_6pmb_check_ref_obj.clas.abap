class ZCL_IM_6PMB_CHECK_REF_OBJ definition
  public
  final
  create public .

*"* public components of class ZCL_IM_6PMB_CHECK_REF_OBJ
*"* do not include other source files here!!!
public section.

  interfaces IF_EX_IWO1_ORDER_BADI .
protected section.
*"* protected components of class ZCL_IM_6PMB_CHECK_REF_OBJ
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_IM_6PMB_CHECK_REF_OBJ
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_IM_6PMB_CHECK_REF_OBJ IMPLEMENTATION.


method IF_EX_IWO1_ORDER_BADI~AUDISP_FOR_REFURB_ORDER.
endmethod.


method IF_EX_IWO1_ORDER_BADI~AUTHORITY_CHECK_AUART_ACTIVIT.
endmethod.


method IF_EX_IWO1_ORDER_BADI~CHANGE_COSTRELEVNCY.
endmethod.


method IF_EX_IWO1_ORDER_BADI~CONFIRMATIONDATE_CHANGEABLE.
endmethod.


method IF_EX_IWO1_ORDER_BADI~CONTRACT_PRICE_GET.
endmethod.


method IF_EX_IWO1_ORDER_BADI~CREATE_BANF_FOR_IND_STOCK.
endmethod.


method IF_EX_IWO1_ORDER_BADI~INVEST_ORDER_XRAIST01_ALTER.
endmethod.


method IF_EX_IWO1_ORDER_BADI~NO_CONTRACT_DATA_GET.
endmethod.


method IF_EX_IWO1_ORDER_BADI~NO_RES_FOR_IND_STOCK.
endmethod.


method IF_EX_IWO1_ORDER_BADI~ORDER_SCHEDULE.
endmethod.


method IF_EX_IWO1_ORDER_BADI~REFERENCE_ORDER_CHK.

  if not refnr is initial.
    if i_caufvd-tplnr is initial and i_caufvd-equnr is initial.

      message e000(zpm01) with 'Either of Functional Loc.or Equipment No is Mandatory'.


    endif.
  endif.
endmethod.


method IF_EX_IWO1_ORDER_BADI~RKPF_MODIFY.
endmethod.
ENDCLASS.
