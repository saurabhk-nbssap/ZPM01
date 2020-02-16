*&---------------------------------------------------------------------*
*& Report  ZPM_TASK_LIST_UPLOAD
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZPM_TASK_LIST_UPLOAD.


types : begin of ls_file_data,
**************  header  **************
          group             type char8,       " group
          tsk_lst_type      type c,         " task list type
***          grup_counter      type char2,     " group counter
***          valid_frm_date    type char10,    " valid date
          tsk_lst_usage     type char3,     " Usage
          plan_plant        type char4,     " Planning plant
          work_center       type char10,
***          wrk_cntr_plant    type char4,
          header_text       type char40,
          planner_grup      type char3,
          status            type char3,
          system_cond       type c,
          maintain_cat      type char6,

*****************  operations  ****************

***          tsk_lst_node      type char8,
          opt_act_no        type char4,
          cntrl_key         type char4,
          stand_text        type char40,

***********    maintainance package  ***************

          packg_no          type char2,       " check no.

        end of ls_file_data.


types : begin of ls_error,
          row_no type sy-tabix,
          msg1 type string,
          msg2 type string,
        end of ls_error.

types : begin of ls_final,
          equi_no type equnr,
          grp_cntr  type plnal,
          msg type string,
        end of ls_final.

data : gt_head like EAM_S_HDR_INS,    " header

       gt_opt like TABLE OF EAM_S_TL_OPR,      " operations
       gw_opt like eam_s_tl_opr,

       gt_packt like TABLE OF EAM_S_TL_MPACK,
       gw_packt like EAM_S_TL_MPACK,  " maintainance package

       it_bapi_ret type TABLE OF BAPIRET2,
       wa_bapi_ret type bapiret2,

       lt_cmt_ret type bapiret2,

        lt_error type table of ls_error,
        lw_error type ls_error,

        lt_final type table of ls_final,
        lw_final type ls_final.

data : I_TAB_RAW_DATA TYPE  TRUXS_T_TEXT_DATA.

data : lt_file_Data type table of ls_file_data,
       lw_file_data type ls_file_data,

       lt_header type table of ls_file_data,
       lw_header type ls_file_data,

       lt_opt type table of ls_file_data,
       lw_opt type ls_file_data.

 DATA : mo_eam_tasklist            TYPE REF TO cl_eam_tasklist.

 data : gv_plnnr type plnnr,
        gv_plnal type plnal,
        lt_return       TYPE bapirettab,
        ls_tl_id        TYPE eam_s_tl_key.

SELECTION-SCREEN BEGIN OF BLOCK b1 with frame title text-001.

  parameters : p_file type rlgrap-filename.

selection-screen end of block b1.

at selection-screen on value-request for p_file.

CALL FUNCTION 'F4_FILENAME'
EXPORTING
  PROGRAM_NAME        = SYST-CPROG
  DYNPRO_NUMBER       = SYST-DYNNR
***  FIELD_NAME          = lv_field_name
IMPORTING
  FILE_NAME           = p_file
  .


start-of-selection.

IF p_file is initial.
  message 'Select file path' type 'I'.
else.
  perform convert_excel_to_itab.
  perform assign_data.        " assigning data to bapi tables.

  delete adjacent duplicates from lt_final comparing all fields.
    write : /  'Task list has been created with their Group Counter : '.
    LOOP AT lt_final into lw_final.
      write : / lw_final-equi_no, '---->', lw_final-grp_cntr.
    ENDLOOP.
ENDIF.

*&---------------------------------------------------------------------*
*&      Form  CONVERT_EXCEL_TO_ITAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM convert_excel_to_itab .

  break abap01.

  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
  EXPORTING
*     I_FIELD_SEPERATOR          =
    I_LINE_HEADER              = 'X'
    i_tab_raw_data             = I_TAB_RAW_DATA
    i_filename                 = p_file
  TABLES
    i_tab_converted_data       = lt_file_data
  EXCEPTIONS
    CONVERSION_FAILED          = 1
    OTHERS                     = 2
    .
  IF sy-subrc <> 0.
  message 'File Error' type 'E' display like 'I'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ASSIGN_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM assign_data .

break abap01.


lt_header[] = lt_file_data[].
lt_opt[] = lt_file_data[].

delete adjacent duplicates from lt_header comparing group.
****delete adjacent duplicates from lt_opt comparing group opt_act_no.

  LOOP AT lt_header into lw_header.

************************************************        header section

      gt_head-plnty = 'A'.        " genral task list

      gt_head-verwe = lw_header-tsk_lst_usage.

      gt_head-werks = lw_header-plan_plant.    " plant

      gt_head-iwerk = lw_header-plan_plant.    " Plant

      gt_head-arbpl = lw_header-work_center.   " work center

      gt_head-arbpl_werk = lw_header-plan_plant.   " planning plant

      gt_head-ktext = lw_header-header_text.         " description

      gt_head-vagrp = lw_header-planner_grup.      " planner group

      gt_head-statu = lw_header-status.      " Status

      gt_head-anlzu = lw_header-system_cond.   " system condition

      gt_head-strat = lw_header-maintain_cat.      " maintainance


**************************************************    operation section

  LOOP AT lt_opt into lw_opt where group = lw_header-group.

***      gw_opt-plnkn = lw_opt-tsk_lst_node.

      gw_opt-plnty = 'A'.           "gt_head-plnty.       "task list type

    CALL FUNCTION 'CONVERSION_EXIT_NUMCV_INPUT'
    EXPORTING
      input  = lw_opt-opt_act_no
    IMPORTING
      output = gw_opt-vornr.
****      gw_opt-vornr = lw_file_data-opt_Act_no.

      gw_opt-steus  = lw_opt-cntrl_key.       " control key

****      gw_opt-plnal  = lw_opt-plnal.

      gw_opt-ltxa1 = lw_opt-stand_text.     " standard text


****      gw_opt-steus = lw_opt-status.

      append gw_opt to gt_opt.
      clear : gw_opt.


      gw_packt-plnty = 'A'.         "gt_head-plnty.

***      gw_packt-plnal = gt_head-plnal.

***      gw_packt-plnkn = gw_opt-plnkn.

      gw_packt-paket = lw_opt-packg_no.

    CALL FUNCTION 'CONVERSION_EXIT_NUMCV_INPUT'
    EXPORTING
      input  = lw_opt-opt_act_no
    IMPORTING
      output = GW_PACKT-vornr.
****      gw_packt-vornr = lw_opt-opt_Act_no.

      gw_packt-strat = lw_opt-maintain_cat.

      append gw_packt to gt_packt.
      clear gw_packt.

  ENDLOOP.    " item

  perform process_bapi.
  clear : gt_head, gt_opt, gt_packt.
endloop.    " header


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PROCESS_BAPI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_bapi .

  break abap01.

  CALL FUNCTION 'EAM_TASKLIST_CREATE'
    EXPORTING
      is_header                      = gt_head
*     IV_DATE                        = SY-DATUM
*     IV_PROFILE                     = ' '
*     IV_CLEAR_BUFFER_IF_ERROR       = 'X'
*     IV_UPDATE_SHORTTEXT            = ABAP_TRUE
   IMPORTING
     EV_PLNNR                       = gv_plnnr
     EV_PLNAL                       = gv_plnal
   TABLES
     IT_OPERATIONS                  = gt_opt
*     IT_COMPONENTS                  =
*     IT_PRTS                        =
*     IT_RELATIONS                   =
*     IT_SPACK_LINES                 =
*     IT_SPACK_OUTLINES              =
*     IT_SPACK_LIMITS                =
*     IT_SPACK_CONTR_LIMITS          =
     IT_MPACKAGES                   = gt_packt
*     IT_TEXT                        =
*     IT_TEXT_LINES                  =
     ET_RETURN                      = it_bapi_ret
            .

  break abap01.

  READ TABLE IT_BAPI_RET INTO WA_BAPI_RET WITH KEY TYPE = 'E'.
  IF SY-SUBRC = 0.
    LOOP AT it_bapi_ret into wa_bapi_ret.

****    lw_error-row_no = sy-tabix.
    lw_error-msg1 = wa_bapi_ret-message.
    lw_error-msg2 = wa_bapi_ret-message_v1.

    append lw_error to lt_error.

    ENDLOOP.

    write : / 'Error while BAPI execution : '.
     skip 2.
    LOOP AT lt_error into LW_ERROR.
      write : / lw_error-msg1, lw_error-msg2.
    ENDLOOP.
  ELSE.

    clear : it_bapi_ret.
    CALL FUNCTION 'EAM_TASKLIST_POST'
    EXPORTING
      iv_plnty        = 'A'
      iv_plnnr        = gv_plnnr
    IMPORTING
      ET_RETURN       = it_bapi_ret
      .


      read table it_bapi_ret into wa_bapi_ret with key type = 'E'.

        IF wa_bapi_ret-type = 'E'.

          LOOP AT it_bapi_ret into wa_bapi_ret.

            lw_error-row_no = sy-tabix.
            lw_error-msg1 = wa_bapi_ret-message.
            lw_error-msg2 = wa_bapi_ret-message_v1.

            append lw_error to lt_error.

          ENDLOOP.

      else.     " if no error occured while posting.

        lw_final-equi_no  = gv_plnnr.
        lw_final-grp_cntr = gv_plnal.
        lw_final-msg  = 'Task List has been created.'.
        append lw_final to lt_final.


        break abap01.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          WAIT          = 'X'
        IMPORTING
          RETURN        = lt_cmt_ret
          .

      ENDIF.

    clear: it_bapi_ret, lt_cmt_ret, lw_error, lw_final.
  ENDIF.



ENDFORM.
