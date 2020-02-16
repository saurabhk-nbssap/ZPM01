*----------------------------------------------------------------------*
*   INCLUDE RIPRIF03                                                   *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*       FORM NOTIFICATION_DETAIL                                       *
*----------------------------------------------------------------------*
*       Notification Details are printed here.                         *
*       Text ELEMENT used    NOTIF_DETAIL                              *
*----------------------------------------------------------------------*
FORM NOTIFICATION_DETAIL.

  PERFORM READ_WQMFE_TABLES.           " get text tables for WQMFE

*... Texts are already available in WQMFE and WQMUR, see
*... Function READ_NOTIFICATION
  CALL FUNCTION 'WRITE_FORM'           " Print pos detail.
       EXPORTING
            ELEMENT   = 'NOTIF_DETAIL'
            WINDOW    = 'MAIN'.

  PERFORM PRINT_POSITION_LONGTEXT.     " Print the longtext to posit.

ENDFORM.
*----------------------------------------------------------------------*
*       FORM PRINT_TEMPLATE                                            *
*----------------------------------------------------------------------*
*                                                                      *
*----------------------------------------------------------------------*
FORM PRINT_TEMPLATE USING POS_NR LIKE WQMFE-FENUM.
  DATA: FIRST_CODE.                    " flag for first code in a group

*... Set column heading for codes
  CALL FUNCTION 'WRITE_FORM'           " catalog category line
       EXPORTING
           ELEMENT   = 'CODE_HEADER'
           WINDOW    = 'MAIN'.


*... We have now the complete set of valid activities,  now print
*... the activity template.
  CLEAR IQKAT.
  SORT IQKAT BY  KATALOGART CODEGRUPPE CODE.
  LOOP AT IQKAT.
    QKAT = IQKAT.
    AT NEW CODEGRUPPE.
*... for each new group write the group title and the top of the
*... Template box.  However, we must first check that the group
*... has codes. If the group does not have codes then dont prepare
*... the top of the first box because no boxes are needed.
      FIRST_CODE = YES.
      CALL FUNCTION 'WRITE_FORM'       " Code group
           EXPORTING
               ELEMENT   = 'CODE_GROUP'
               WINDOW    = 'MAIN'.
    ENDAT.

    IF QKAT-CODE <> SPACE.
*... a code line is being processed
      IF FIRST_CODE = YES.  " first code for the current group
        WIPRT-BOX = C_PART_BOX.        " for top of first box
        CALL FUNCTION 'WRITE_FORM'
             EXPORTING                 " write top of box
                  ELEMENT = 'PART_BOX' " for new code group
                  WINDOW  = 'MAIN'.
        FIRST_CODE = SPACE.
      ENDIF.

      WIPRT-BOX = C_BOX.               " underneath part of box
      FOUND = SPACE.
      LOOP AT IVIQMMA WHERE            " is this code already
              QMNUM = VIQMEL-QMNUM     " already entered  ?
          AND FENUM = POS_NR           " when found, mark
          AND MNGRP = QKAT-CODEGRUPPE  " mark template with
          AND MNCOD = QKAT-CODE        " with X
          AND MNVER = QKAT-VERSIONCD
          AND KZLOESCH IS INITIAL.
        FOUND = YES.                                        "
        WQMMA = IVIQMMA.                                    "
      ENDLOOP.
*... check if the activity has already been registered
      IF FOUND = YES.                  " act. registered
        CALL FUNCTION 'WRITE_FORM'     " code
             EXPORTING
                 ELEMENT   = 'CODE'    " '|_X_|'  is in
                 WINDOW    = 'MAIN'.   " sapscript form itself
      ELSE.
        CALL FUNCTION 'WRITE_FORM'     " code
             EXPORTING
                 ELEMENT   = 'CODE_BOX'" '|___|'  from PROGRAm
                 WINDOW    = 'MAIN'.   " passed into sapscript
      ENDIF.

    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CAUSE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM CAUSE USING P_QMNUM LIKE WQMFE-QMNUM
                 P_FENUM LIKE WQMFE-FENUM.

  LOOP AT IVIQMUR WHERE
           QMNUM = P_QMNUM             " current Notif number
       AND FENUM = P_FENUM.            " current position number
    WQMUR = IVIQMUR.                   " Set workarea for SAPSCRIPT
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              ELEMENT = 'NOTIF_CAUSE'
              WINDOW  = 'MAIN'.
    PERFORM PRINT_CAUSE_LONGTEXT.
  ENDLOOP.

ENDFORM.                               " CAUSE
*&---------------------------------------------------------------------*
*&      Form  TASK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM TASK  USING QMNUM LIKE  WQMFE-QMNUM
                 FENUM LIKE  WQMFE-FENUM.

  LOOP AT IVIQMSM WHERE
           QMNUM = QMNUM               " current Notif number
       AND FENUM = FENUM.              " current position number

    WQMSM = IVIQMSM.                   " Set workarea for SAPSCRIPT

    CALL FUNCTION 'PM_PARTNER_READ'
         EXPORTING
              PARVW                = WQMSM-PARVW
              PARNR                = WQMSM-PARNR
         IMPORTING
              DIADR_WA             = DIADR
         EXCEPTIONS
              NO_VALID_PARNR       = 1
              NO_VALID_PARNR_TODAY = 2
              NO_AUTHORITY         = 3
              OTHERS               = 4.

    PERFORM READ_STATUS USING WQMSM-OBJNR
                              BSVZ-STEXT.

*// the address is in DIADR, the task in WQMSM
*// print now possible

    CALL FUNCTION 'WRITE_FORM'         " task
         EXPORTING
              ELEMENT   = 'TASK'
              WINDOW    = 'MAIN'.

  ENDLOOP.

ENDFORM.
