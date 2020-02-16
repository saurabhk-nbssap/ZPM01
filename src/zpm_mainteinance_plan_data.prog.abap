*&---------------------------------------------------------------------*
*& Report  ZPM_MAINTEINANCE_PLAN_DATA
*& Date : 02.05.2018
*&---------------------------------------------------------------------*
*& Requested By : Kamalakar Varma
*& Functional Consultant : Pranav Patvardhan
*& Developed By : Siddharth Pardeshi from Adroit Infotech, Pune
*&
*&---------------------------------------------------------------------*
REPORT ZPM_MAINTEINANCE_PLAN_DATA.

TYPES : BEGIN OF ls_file_data,
          main_plan_cat       type char2,
          maint_stgr          type char6,
          plain_text          type char40,
          equip_no            type char18,
          order_type          type char4,
          main_act_typ        type char3,
          work_center         type char8,
          plant               type char4,
          tsk_lst_typ         type c,
          tsk_lst_grup        type char8,
          grup_counter        type char2,
          schedule_period     type char3,
          unit_sched_intv     type char3,
          call_hor_main_call  type char3,
          comp_prces_check    type c,       " check box
          cycle_start_Date    type char10,
          plan_sort           type char50,
        end of ls_file_Data.

TYPES : BEGIN OF TY_ERROR,
          ERROR(60) TYPE c,
        END OF TY_ERROR.

data : lv_field_name type dynfnam,
       lv_mode type c.

data : lt_file_Data type table of ls_file_data,
        lw_file_data type ls_file_data,
        I_TAB_RAW_DATA TYPE  TRUXS_T_TEXT_DATA,
        lv_date type char10.

DATA : BDCDATA LIKE TABLE OF BDCDATA WITH HEADER LINE.

DATA : IT_MSG TYPE TABLE OF BDCMSGCOLL,  " ERROR HANDLING
       WA_MSG TYPE BDCMSGCOLL,
       MSG(51),
       IT_OUTPUT  TYPE TABLE OF TY_ERROR,
       WA_OUTPUT  LIKE LINE OF IT_OUTPUT,
       WA_STRING(10) TYPE C,
       WA_PATH    TYPE STRING ,
       WA_ERROR   TYPE STRING,
       WA_CNT     TYPE I,
       W_MODE     TYPE C,
       WA_CNT1(2) TYPE N.


SELECTION-SCREEN BEGIN OF BLOCK b1 with frame title text-001.

parameters : p_file type IBIPPARMS-PATH obligatory,
             p_error type IBIPPARMS-PATH OBLIGATORY DEFAULT 'C:\Users\Siddharth\Desktop\EQUIPMENT_CREATION_ERROR.TXT',
             p_mode type c obligatory default 'A'.

selection-screen end of block b1.

at selection-screen on value-request for p_file.

CALL FUNCTION 'F4_FILENAME'
EXPORTING
  PROGRAM_NAME        = SYST-CPROG
  DYNPRO_NUMBER       = SYST-DYNNR
  FIELD_NAME          = 'P_FILE' "lv_field_name
IMPORTING
  FILE_NAME           = p_file
  .

at selection-screen on value-request for p_error.

CALL FUNCTION 'F4_FILENAME'
EXPORTING
  PROGRAM_NAME        = SYST-CPROG
  DYNPRO_NUMBER       = SYST-DYNNR
  FIELD_NAME          = lv_field_name
IMPORTING
  FILE_NAME           = p_error
  .

lv_field_name = p_file.


at selection-screen.
****  break abap01.
if p_mode = 'A' or p_mode = 'E' or p_mode = 'N'.

  lv_mode = p_mode.

else.

  message 'Enter correct mode "A" for foreground or "E" for background or "N" processing with error display' TYPE 'E' display like 'I'.

ENDIF.


start-of-selection.

perform convert_excel_to_itab.
*  **include bdcrecx1.
*  **perform open_group.

break abap01.

LOOP AT lt_file_data into lw_file_data.

refresh bdcdata.
perform mp_rec.
call transaction 'IP01' using bdcdata mode lv_mode update 'S' messages into it_msg.

IF SY-SUBRC EQ 0.
read table it_msg into wa_msg with key msgtyp = 'S'.

IF sy-subrc = 0.
WRITE :/ wa_msg-msgv1,'DATA CREATED SUCCESSFULLY' .
ENDIF.

ELSE.
LOOP AT IT_MSG INTO WA_MSG WHERE MSGTYP EQ 'E'. " Error Handling

CALL FUNCTION 'MESSAGE_TEXT_BUILD'
EXPORTING
  MSGID               = WA_MSG-MSGID
  MSGNR               = WA_MSG-MSGNR
  MSGV1               = WA_MSG-MSGV1
  MSGV2               = WA_MSG-MSGV2
  MSGV3               = WA_MSG-MSGV3
  MSGV4               = WA_MSG-MSGV4
IMPORTING
  MESSAGE_TEXT_OUTPUT = MSG.

WA_OUTPUT-ERROR = MSG.

WA_STRING       = sy-tabix.

CONCATENATE WA_STRING WA_OUTPUT-ERROR INTO WA_OUTPUT-ERROR SEPARATED BY SPACE.

APPEND WA_OUTPUT-ERROR TO IT_OUTPUT.

WA_ERROR = p_error.

CALL FUNCTION 'GUI_DOWNLOAD'                            " Download Error file
EXPORTING
  FILENAME                        = WA_ERROR
  WRITE_FIELD_SEPARATOR           = 'X'
TABLES
  DATA_TAB                        = IT_OUTPUT.

IF SY-SUBRC <> 0.
MESSAGE ID SY-MSGID TYPE SY-MSGTY
NUMBER SY-MSGNO  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
ENDIF.

ENDLOOP.

ENDIF.

clear : it_msg.

ENDLOOP.

FORM MP_REC.
perform bdc_dynpro      using 'SAPLIWP3' '0100'.
perform bdc_field       using 'BDC_CURSOR'
      'RMIPM-WARPL'.
perform bdc_field       using 'BDC_OKCODE'
      '/00'.
perform bdc_field       using 'RMIPM-MPTYP'
      lw_file_data-main_plan_cat."'PM'.
perform bdc_field       using 'RMIPM-WSTRA'
      lw_file_data-maint_stgr."'ICC'.
perform bdc_dynpro      using 'SAPLIWP3' '0201'.
perform bdc_field       using 'BDC_OKCODE'
      '/00'.
perform bdc_field       using 'RMIPM-WPTXT'
      lw_file_data-plain_text."'testing'.
perform bdc_field       using 'RMIPM-PSTXT'
      lw_file_data-plain_text."'testing'.
***  perform bdc_field       using 'RIWO1-TPLNR'
***                                'T-ZNB-SPDY'.
perform bdc_field       using 'RIWO1-EQUNR'
      lw_file_data-equip_no."'10004508'.
perform bdc_field       using 'BDC_CURSOR'
      'RMIPM-PLNAL'.
perform bdc_field       using 'RMIPM-IWERK'
      lw_file_data-plant."'1101'.
perform bdc_field       using 'RMIPM-WPGRP'
      lw_file_data-main_act_typ."'002'.
perform bdc_field       using 'RMIPM-AUART'
      lw_file_data-order_type."'PM01'.
***  perform bdc_field       using 'RMIPM-ILART'
***                                '001'.
perform bdc_field       using 'RMIPM-GEWERK'
      lw_file_data-work_center."'TMECH'.
perform bdc_field       using 'RMIPM-WERGW'
      lw_file_data-plant."'1101'.
perform bdc_field       using 'RMIPM-PLNTY'
      lw_file_data-tsk_lst_typ."'A'.
perform bdc_field       using 'RMIPM-PLNNR'
      lw_file_data-tsk_lst_grup."'46'.
perform bdc_field       using 'RMIPM-PLNAL'
      lw_file_data-grup_counter."'1'.
perform bdc_dynpro      using 'SAPLIWP3' '0201'.
perform bdc_field       using 'BDC_OKCODE'
      '=T\02'.
********************************************************************************
perform bdc_field       using 'RMIPM-WPTXT'
      lw_file_data-plain_text."
perform bdc_field       using 'RMIPM-PSTXT'
      lw_file_data-plain_text."      'testing'.
***  perform bdc_field       using 'RIWO1-TPLNR'
***                                'T-ZNB-SPDY'.
perform bdc_field       using 'RIWO1-EQUNR'
      lw_file_data-equip_no."'10004508'.
perform bdc_field       using 'BDC_CURSOR'
      'RMIPM-PLNAL'.
perform bdc_field       using 'RMIPM-IWERK'
      lw_file_data-plant."'1101'.
perform bdc_field       using 'RMIPM-WPGRP'
      lw_file_data-main_act_typ."'002'.
perform bdc_field       using 'RMIPM-AUART'
      lw_file_data-order_type."'PM01'.
***  perform bdc_field       using 'RMIPM-ILART'
***                                '001'.
perform bdc_field       using 'RMIPM-GEWERK'
      lw_file_data-work_center."'TMECH'.
perform bdc_field       using 'RMIPM-WERGW'
      lw_file_data-plant."'1101'.
perform bdc_field       using 'RMIPM-PLNTY'
      lw_file_data-tsk_lst_typ."'A'.
perform bdc_field       using 'RMIPM-PLNNR'
      lw_file_data-tsk_lst_grup."'46'.
perform bdc_field       using 'RMIPM-PLNAL'
      lw_file_data-grup_counter."'1'.
perform bdc_dynpro      using 'SAPLIWP3' '0201'.
perform bdc_field       using 'BDC_OKCODE'
      '/00'.
perform bdc_field       using 'RMIPM-WPTXT'
      lw_file_data-plain_text."'testing'.
perform bdc_field       using 'RMIPM-PSTXT'
      lw_file_data-plain_text."'testing'.
***  perform bdc_field       using 'RIWO1-TPLNR'
***                                'T-ZNB-SPDY'.
perform bdc_field       using 'RIWO1-EQUNR'
      lw_file_data-equip_no."'10004508'.
perform bdc_field       using 'RMIPM-IWERK'
      lw_file_data-plant."'1101'.
perform bdc_field       using 'RMIPM-WPGRP'
      lw_file_data-main_act_typ."'002'.
perform bdc_field       using 'RMIPM-AUART'
      lw_file_data-order_type."'PM01'.
***  perform bdc_field       using 'RMIPM-ILART'
***                                '001'.
perform bdc_field       using 'RMIPM-GEWERK'
      lw_file_data-work_center."'TMECH'.
perform bdc_field       using 'RMIPM-WERGW'
      lw_file_data-plant."'1101'.
perform bdc_field       using 'RMIPM-PLNTY'
      lw_file_data-tsk_lst_typ."'A'.
perform bdc_field       using 'RMIPM-PLNNR'
      lw_file_data-tsk_lst_grup."'46'.
perform bdc_field       using 'RMIPM-PLNAL'
      lw_file_data-grup_counter."'1'.
********************************************************************************
perform bdc_field       using 'BDC_CURSOR'
      'RMIPM-STADT'.
perform bdc_field       using 'RMIPM-HORIZ'
      lw_file_data-call_hor_main_call."'90'.
perform bdc_field       using 'RMIPM-ZEIT'
      'X'.
perform bdc_field       using 'RMIPM-ABRHO'
      lw_file_data-schedule_period."'365'.
perform bdc_field       using 'RMIPM-HUNIT'
      lw_file_data-unit_sched_intv."'DAY'.
perform bdc_field       using 'RMIPM-CALL_CONFIRM'
      lw_file_data-comp_prces_check."'X'.

perform bdc_field       using 'RMIPM-STICH'
                              'X'.

perform bdc_field       using 'RMIPM-SFAKT'
      '1.00'.
perform bdc_field       using 'RMIPM-STADT'
      lw_file_data-cycle_start_date."'18.04.2018'.
*********************************************************************************************
***perform bdc_dynpro      using 'SAPLIWP3' '0201'.
***perform bdc_field       using 'BDC_OKCODE'
***      '=BU'.
***perform bdc_field       using 'RMIPM-WPTXT'
***      lw_file_data-plain_text."'testing'.
***perform bdc_field       using 'RMIPM-PSTXT'
***      lw_file_data-plain_text."'testing'.
******  perform bdc_field       using 'RIWO1-TPLNR'
******                                'T-ZNB-SPDY'.
***perform bdc_field       using 'RIWO1-EQUNR'
***      lw_file_data-equip_no."'10004508'.
***perform bdc_field       using 'RMIPM-IWERK'
***      lw_file_data-plant."'1101'.
***perform bdc_field       using 'RMIPM-WPGRP'
***      lw_file_data-main_act_typ."'002'.
***perform bdc_field       using 'RMIPM-AUART'
***      lw_file_data-order_type."'PM01'.
******  perform bdc_field       using 'RMIPM-ILART'
******                                '001'.
***perform bdc_field       using 'RMIPM-GEWERK'
***      lw_file_data-work_center."'TMECH'.
***perform bdc_field       using 'RMIPM-WERGW'
***      lw_file_data-plant."'1101'.
***perform bdc_field       using 'RMIPM-PLNTY'
***      lw_file_data-tsk_lst_typ."'A'.
***perform bdc_field       using 'RMIPM-PLNNR'
***      lw_file_data-tsk_lst_grup."'46'.
***perform bdc_field       using 'RMIPM-PLNAL'
***      lw_file_data-grup_counter."'1'.
***perform bdc_field       using 'BDC_CURSOR'
***      'RMIPM-STADT'.
***perform bdc_field       using 'RMIPM-HORIZ'
***      lw_file_data-call_hor_main_call."'90'.
***perform bdc_field       using 'RMIPM-ZEIT'
***      'X'.
***perform bdc_field       using 'RMIPM-ABRHO'
***      lw_file_data-schedule_period."'365'.
***perform bdc_field       using 'RMIPM-HUNIT'
***      lw_file_data-unit_sched_intv."'DAY'.
***perform bdc_field       using 'RMIPM-CALL_CONFIRM'
***      lw_file_data-comp_prces_check."'X'.
***
***perform bdc_field       using 'RMIPM-STICH'
***                              'X'.
***
***perform bdc_field       using 'RMIPM-SFAKT'
***      '1.00'.
***perform bdc_field       using 'RMIPM-STADT'
***      lw_file_data-cycle_start_date."'18.04.2018'.
****************************************************************************

*****************************   additional change for sort key field on 24.05.2018 mady by siddharth pardeshi, Adroit Infotech, Pune

  perform bdc_dynpro      using 'SAPLIWP3' '0201'.
  perform bdc_field       using 'BDC_OKCODE'
                                '=T\03'.
  perform bdc_field       using 'RMIPM-WPTXT'
                                lw_file_data-plain_text."'Test plan 2'.
  perform bdc_field       using 'RMIPM-PSTXT'
                                lw_file_data-plain_text."'Test plan 2'.
***  perform bdc_field       using 'RIWO1-TPLNR'
***                                'T-ZNB-SPDY'.
  perform bdc_field       using 'RIWO1-EQUNR'
                                lw_file_data-equip_no."'10004508'.
  perform bdc_field       using 'RMIPM-IWERK'
                                lw_file_data-plant."'1101'.
  perform bdc_field       using 'RMIPM-WPGRP'
                                lw_file_data-main_act_typ."'002'.
  perform bdc_field       using 'RMIPM-AUART'
                                lw_file_data-order_type."'PM01'.
***  perform bdc_field       using 'RMIPM-ILART'
***                                '001'.
  perform bdc_field       using 'RMIPM-GEWERK'
                                lw_file_data-work_center."'TMECH'.
  perform bdc_field       using 'RMIPM-WERGW'
                                lw_file_data-plant."'1101'.
  perform bdc_field       using 'RMIPM-PLNTY'
                                lw_file_data-tsk_lst_typ."'A'.
  perform bdc_field       using 'RMIPM-PLNNR'
                                lw_file_data-tsk_lst_grup."'46'.
  perform bdc_field       using 'RMIPM-PLNAL'
                                lw_file_data-grup_counter."'1'.
  perform bdc_field       using 'BDC_CURSOR'
                                'RMIPM-STADT'.
  perform bdc_field       using 'RMIPM-HUNIT'
                                lw_file_data-unit_sched_intv."'DAY'.
  perform bdc_field       using 'RMIPM-STICH'
                                 lw_file_data-comp_prces_check."'X'.
  perform bdc_field       using 'RMIPM-SFAKT'
                                '1.00'.
  perform bdc_field       using 'RMIPM-FABKL'
                                'I1'.
  perform bdc_field       using 'RMIPM-STADT'
                                lw_file_data-cycle_start_date."'18.04.2018'.
  perform bdc_dynpro      using 'SAPLIWP3' '0201'.
  perform bdc_field       using 'BDC_OKCODE'
                                '=BU'.
  perform bdc_field       using 'RMIPM-WPTXT'
                                lw_file_data-plain_text."'Test plan 2'.
  perform bdc_field       using 'RMIPM-PSTXT'
                                lw_file_data-plain_text."'Test plan 2'.
***  perform bdc_field       using 'RIWO1-TPLNR'
***                                'T-ZNB-SPDY'.
  perform bdc_field       using 'RIWO1-EQUNR'
                                lw_file_data-equip_no."'10004508'.
  perform bdc_field       using 'RMIPM-IWERK'
                                lw_file_data-plant."'1101'.
  perform bdc_field       using 'RMIPM-WPGRP'
                                lw_file_data-main_act_typ."'002'.
  perform bdc_field       using 'RMIPM-AUART'
                                lw_file_data-order_type."'PM01'.
***  perform bdc_field       using 'RMIPM-ILART'
***                                 '001'.
  perform bdc_field       using 'RMIPM-GEWERK'
                                lw_file_data-work_center."'TMECH'.
  perform bdc_field       using 'RMIPM-WERGW'
                                lw_file_data-plant."'1101'.
  perform bdc_field       using 'RMIPM-PLNTY'
                                lw_file_data-tsk_lst_typ."'A'.
  perform bdc_field       using 'RMIPM-PLNNR'
                                lw_file_data-tsk_lst_grup."'46'.
  perform bdc_field       using 'RMIPM-PLNAL'
                                '1'.
  perform bdc_field       using 'BDC_CURSOR'
                                'RMIPM-PLAN_SORT'.
  perform bdc_field       using 'RMIPM-PLAN_SORT'
                                lw_file_data-plan_sort."'IBJ1-INDO BAIJIN'.

  concatenate sy-datum+6(2) sy-datum+4(2) sy-datum(4) into lv_date separated by '.'.
  perform bdc_field       using 'RMIPM-STTAG'
                                lv_date."'24.05.2018'.


***********************************************************************************************************************************
*  **perform bdc_transaction using 'IP01'.
*  **
*  **perform close_group.

ENDFORM.


*----------------------------------------------------------------------*
*        Start new screen                                              *
*----------------------------------------------------------------------*
FORM BDC_DYNPRO USING PROGRAM DYNPRO.
CLEAR BDCDATA.
BDCDATA-PROGRAM  = PROGRAM.
BDCDATA-DYNPRO   = DYNPRO.
BDCDATA-DYNBEGIN = 'X'.
APPEND BDCDATA.
ENDFORM.

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
FORM BDC_FIELD USING FNAM FVAL.
***IF FVAL <> NODATA.
CLEAR BDCDATA.
BDCDATA-FNAM = FNAM.
BDCDATA-FVAL = FVAL.
APPEND BDCDATA.
***ENDIF.
ENDFORM.

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
*   I_FIELD_SEPERATOR          =
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
