FUNCTION Z6XX_AMOUNT_IN_WORDS_INR.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_AMOUNT) DEFAULT 0
*"     REFERENCE(I_CURRENCY) TYPE  WAERS
*"     REFERENCE(I_FILLER) OPTIONAL
*"     REFERENCE(I_LANGUAGE) TYPE  SY-LANGU OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_IN_WORDS) TYPE  SPELL
*"----------------------------------------------------------------------
*   Global data declarations

  DATA : v_amount TYPE  pc207-betrg.
  IF I_currency = 'INR' OR I_currency = ''.
    v_amount = I_amount.
    CALL FUNCTION 'HR_IN_CHG_INR_WRDS'
      EXPORTING
        amt_in_num         = v_amount
      IMPORTING
        amt_in_words       = E_in_words-word
      EXCEPTIONS
        data_type_mismatch = 1
        OTHERS             = 2.
    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ELSE.
    CONCATENATE E_in_words-word 'ONLY' INTO E_in_words-word SEPARATED BY SPACE.
    TRANSLATE  E_in_words-word TO UPPER CASE.

    ENDIF.

  ELSE.
    CALL FUNCTION 'SPELL_AMOUNT'
      EXPORTING
        amount    = I_amount
        currency  = I_currency
        filler    = I_filler
        language  = I_language
      IMPORTING
        in_words  = E_in_words
      EXCEPTIONS
        not_found = 1
        too_large = 2
        OTHERS    = 3.
    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
    IF NOT E_in_words-decword IS INITIAL.
      CONCATENATE E_in_words-word 'AND' E_in_words-decword INTO E_in_words-word SEPARATED BY space.
      TRANSLATE   E_in_words-word TO UPPER CASE.

    ENDIF.
  ENDIF.








ENDFUNCTION.
