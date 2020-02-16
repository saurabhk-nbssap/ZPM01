*&*********************************************************************************************************&*
*& Report  ZPNWP                                                                                           &*
*&*********************************************************************************************************&*
*& OBJECT NAME          : ZPNWP                                                                            &*
*& TECHNICAL CONSULTANT : PRADEEP KODINAGULA                                                               &*
*& MODULE NAME          : PM                                                                               &*
*& CREATE DATE          : 04.05.2016                                                                       &*
*& TRANSPORT NO         : IRDK923537                                                                       &*
*& DESCRIPTION          : PM Work Permit Printing.                                                         &*
*&                                                                                                         &*
* REVISION HISTORY------------------------------------------------------------------------------------------*
*   CHANGED BY:          6010859 - SaurabhK(Requested by: Mr. Kamalakar Varma)                              *
*   CHANGE ON:           Monday, June 04, 2018 14:16:59                                                     *
*   REASON FOR CHANGE:   IRDK932363, IRDK932361, IRDK932359, IRDK932357: PM: S_K: ZNWP: Restrict print. to notif typ: ZA ZB ZD ZS ZT            *
*************************************************************************************************************

report  zpnwp.

data a type qmel-qmnum.
data fm_name type  rs38l_fnam.


data : doc_type type char02,
       c_type   type char01.

data  output_options type ssfcompop.
data: control_parameters type ssfctrlop,
      w_cnt              type i,
      w_cnt2             type i.

data : begin of wa_noti,
         qmnum type qmel-qmnum,
         qmart type qmel-qmart,
       end of wa_noti.

data it_noti like table of wa_noti.

*-----------------------------------------------------------------------SELECTION SCREEN
selection-screen begin of block a with frame.

selection-screen begin of block main with frame title text-000.
select-options : so_qmnum for a obligatory.
selection-screen end of block main.

selection-screen begin of block print with frame title text-001.

selection-screen begin of line.
selection-screen position 2.
parameters p_radio1 radiobutton group g1 modif id pk user-command xxx default 'X'.
selection-screen comment 6(33) text-200  modif id pk.
selection-screen end of line.

selection-screen begin of line.
selection-screen position 2.
parameters p_radio2 radiobutton group g1 modif id pk.
selection-screen comment 6(50) text-201  modif id pk.
selection-screen end of line.

selection-screen begin of line.
selection-screen position 2.
parameters p_radio3 radiobutton group g1 modif id pk.
selection-screen comment 6(55) text-202  modif id pk.
selection-screen end of line.

selection-screen begin of line.
selection-screen position 2.
parameters p_radio4 radiobutton group g1 modif id pk.
selection-screen comment 6(55) text-203  modif id pk.
selection-screen end of line.

selection-screen end of block print.

selection-screen begin of block copies with frame title text-003.

selection-screen begin of line.
selection-screen position 2.
parameters p_check1 as checkbox modif id pr default 'X'.
selection-screen comment 6(30) text-204  modif id pr.

selection-screen position 40.
parameters p_check2 as checkbox modif id pr default 'X'.
selection-screen comment 46(20) text-205  modif id pr.
selection-screen end of line.

selection-screen end of block copies.

selection-screen end of block a.

*----------------------------------------------------------------------- AT SELECTION SCREEN
at selection-screen.

  if p_check1 = '' and p_check2 = ''.
    message e000(zmessage) with 'Please select either issuer copy or receiver copy'.
  endif.

*----------------------------------------------------------------------- START OF SELECTION.
start-of-selection.

  perform set_output_options.
  perform fetch_notification_details.
  perform print_notifications.

end-of-selection.
  leave to list-processing.

*&---------------------------------------------------------------------*
*&      Form  SET_OUTPUT_PARAMETERS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_output_options .
  if p_check1 = 'X' and p_check2 = ''.
    c_type = 'I'.
  elseif p_check1 = '' and p_check2 = 'X'.
    c_type = 'R'.
  elseif p_check1 = 'X' and p_check2 = 'X'.
    output_options-tdcopies = 2.
  endif.
endform.
*&---------------------------------------------------------------------*
*&      Form  FETCH_NOTIFICATION_DETAILS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form fetch_notification_details .
  select qmnum qmart from qmel into table it_noti where qmnum in so_qmnum.
  describe table it_noti lines w_cnt.

  loop at it_noti into wa_noti.

    if wa_noti-qmart ne 'ZA' and wa_noti-qmart ne 'ZB' and wa_noti-qmart ne 'ZD' and wa_noti-qmart ne 'ZS' and wa_noti-qmart ne 'ZT'.
      write: / wa_noti-qmnum, ': Notification type', wa_noti-qmart ,'is not supported by this print program.'.
      delete it_noti.
    endif.

    clear wa_noti.
  endloop.

  if it_noti is initial.
    message 'No notifications found.' type 'S' display like 'E'.
    stop.
  endif.
endform.
*&---------------------------------------------------------------------*
*&      Form  PRINT_NOTIFICATIONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form print_notifications .
  loop at it_noti into wa_noti.

    if p_radio1 = 'X' or p_radio2 = 'X'.

      call function 'SSF_FUNCTION_MODULE_NAME'
        exporting
          formname           = 'ZPM001PK_WORK_PERMIT_FORM'
        importing
          fm_name            = fm_name
        exceptions
          no_form            = 1
          no_function_module = 2
          others             = 3.
      if sy-subrc <> 0.
        message id sy-msgid type sy-msgty number sy-msgno
        with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      endif.


      if p_radio1 = 'X'.
        doc_type = 'R1'.
      elseif p_radio2 = 'X'.
        doc_type = 'R2'.
      endif.


      call function fm_name
        exporting
          control_parameters = control_parameters
          output_options     = output_options
          user_settings      = 'X'
          p_qmnum            = wa_noti-qmnum
          c_type             = c_type
          doc_type           = doc_type
        exceptions
          formatting_error   = 1
          internal_error     = 2
          send_error         = 3
          user_canceled      = 4
          others             = 5.
      if sy-subrc <> 0.
        message id sy-msgid type sy-msgty number sy-msgno
        with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      endif.

    elseif p_radio3 = 'X' or p_radio4 = 'X'.

      call function 'SSF_FUNCTION_MODULE_NAME'
        exporting
          formname           = 'ZPM001PK_WORK_PERMIT_FORM_1'
        importing
          fm_name            = fm_name
        exceptions
          no_form            = 1
          no_function_module = 2
          others             = 3.
      if sy-subrc <> 0.
        message id sy-msgid type sy-msgty number sy-msgno
        with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      endif.


      if p_radio3 = 'X'.
        doc_type = 'R3'.
      elseif p_radio4 = 'X'.
        doc_type = 'R4'.
      endif.

      call function fm_name
        exporting
          control_parameters = control_parameters
          output_options     = output_options
          user_settings      = 'X'
          p_qmnum            = wa_noti-qmnum
          c_type             = c_type
          doc_type           = doc_type
        exceptions
          formatting_error   = 1
          internal_error     = 2
          send_error         = 3
          user_canceled      = 4
          others             = 5.
      if sy-subrc <> 0.
        message id sy-msgid type sy-msgty number sy-msgno
        with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      endif.

    endif.

    clear wa_noti.
  endloop.
endform.
