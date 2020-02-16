*&---------------------------------------------------------------------*
*&  Include           ZPM_CLASS_B_PERMIT_FORMS
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_SMARTFORM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form display_smartform .

  call function 'SSF_FUNCTION_MODULE_NAME'
    exporting
      formname           = gv_f_name
      variant            = ' '
      direct_call        = ' '
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
  call function fm_name
    exporting
*     OUTPUT_OPTIONS   =
*     USER_SETTINGS    = 'X'
      gv_aufnr         = p_ornum
    exceptions
      formatting_error = 1
      internal_error   = 2
      send_error       = 3
      user_canceled    = 4
      others           = 5.

  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
    with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

endform.                    " DISPLAY_SMARTFORM
