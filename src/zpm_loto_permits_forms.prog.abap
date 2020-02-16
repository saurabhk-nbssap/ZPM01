*&---------------------------------------------------------------------*
*&  Include           ZPM_LOTO_PERMITS_FORMS
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_SMARTFORM
*&---------------------------------------------------------------------*
*  To print smartform
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
FORM display_smartform .

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = gv_f_name
      variant            = ' '
      direct_call        = ' '
    IMPORTING
      fm_name            = fm_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  CALL FUNCTION fm_name
    EXPORTING
      gv_aufnr         = p_ornum
    EXCEPTIONS
      formatting_error = 1
      internal_error   = 2
      send_error       = 3
      user_canceled    = 4
      OTHERS           = 5.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " DISPLAY_SMARTFORM
