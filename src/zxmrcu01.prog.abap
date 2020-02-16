*&---------------------------------------------------------------------*
*&  Include           ZXMRCU01
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* OBJECT DESCRIPTION: Triggering notification for exceeding set limit in characteristics,
*                     used for capturing measurement recording.
*.
*          DEVELOPER: Ramakrishna
*      CREATION DATE: 09.06.2010
*        DEV REQUEST: IRDK900060
*----------------------------------------------------------------------*

* REVISION HISTORY-----------------------------------------------------*
*
*        REVISION NO:   R***
*          DEVELOPER:                        DATE:   DD.MM.YYYY
*        DESCRIPTION:
*----------------------------------------------------------------------*


*DATA: ST_IMRG_INS LIKE IMRG.
*DATA : ST_IMPT TYPE IMPT.
*data : v_qmnum type RIQS1-N_QMNUM,
*       v_message type char255.
*DATA : WA_BAPI2078_NOTHDRI TYPE BAPI2078_NOTHDRI.
*DATA : WA_BAPI2078_NOTHDRE TYPE BAPI2078_NOTHDRE.
*DATA : I_BAPIRET2 TYPE TABLE OF BAPIRET2.
*IF NOT IMRG_INS[] IS INITIAL.
*LOOP AT IMRG_INS INTO ST_IMRG_INS.
*    CALL FUNCTION 'MEASUREM_POINT_RFC_SINGLE_002'
*     EXPORTING
*       MEASUREMENT_POINT        = ST_IMRG_INS-POINT
**       SECONDARY_INDEX          = ' '
*       WITH_DIALOG_SCREEN       = ' '
**       EDIT_MODE                = ' '
**       COMMIT_WORK              = 'X'
**       WAIT_AFTER_COMMIT        = 'X'
*     IMPORTING
*       IMPT_BA                  = ST_IMPT
**       UPDATE_SUCCESS           =
*     EXCEPTIONS
*       NO_AUTHORITY             = 1
*       POINT_NOT_FOUND          = 2
*       INDEX_NOT_UNIQUE         = 3
*       TYPE_NOT_FOUND           = 4
*       UPDATE_FAILED            = 5
*       OTHERS                   = 6
*              .
*    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    ELSE.
*         IF ST_IMRG_INS-RECDV GT ST_IMPT-MRMAX.
*
*           WA_BAPI2078_NOTHDRI-SHORT_TEXT = 'Measurement Point value exceeded'.
*
**           CALL FUNCTION 'BAPI_QUALNOT_CREATE'
**             EXPORTING
***              EXTERNAL_NUMBER          =
**               NOTIF_TYPE               = 'ZQ'
**               NOTIFHEADER              = WA_BAPI2078_NOTHDRI
***              TASK_DETERMINATION       = ' '
***              SENDER                   =
**             IMPORTING
**               NOTIFHEADER_EXPORT       = WA_BAPI2078_NOTHDRE
**             TABLES
***              NOTITEM                  =
***              NOTIFCAUS                =
***              NOTIFACTV                =
***              NOTIFTASK                =
***              NOTIFPARTNR              =
***              LONGTEXTS                =
***              KEY_RELATIONSHIPS        =
**               RETURN                   = I_BAPIRET2
**                     .
**
**
**            CALL FUNCTION 'BAPI_QUALNOT_SAVE'
**              EXPORTING
**                NUMBER            = WA_BAPI2078_NOTHDRE-NOTIF_NO
***             IMPORTING
***               NOTIFHEADER       =
***             TABLES
***               RETURN            =
**                      .
**            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
***             EXPORTING
***               WAIT          =
***             IMPORTING
***               RETURN        =
**                      .
*
**           CALL FUNCTION 'ZQM05_CREATE_NOTIFICATION'
**             EXPORTING
***              MATNR                  =
**               DESCRIPTION            = 'Measurement Point Value Exceeded'
***              COORDINATOR            =
***              SOLDTOPARTY            =
**               NOTIFICATIONTYPE       = 'ZQ'
**               AUTHOR                 = SY-UNAME
**             IMPORTING
**               NOTIFICATION           = v_qmnum
**               MESSAGE                = v_message.
**                     .
*
*
*         ENDIF.
*
*    ENDIF.
*
*
*ENDLOOP.
*ENDIF.
DATA:
    L_IMPT     LIKE IMPT,             "Measuring point
    L_MPOBI    LIKE RIMR0-MPOBK,      "Transparent object key
    L_PRIOK    LIKE QMEL-PRIOK        "Notification priority: (optional)
      VALUE '2',                      "  2 = Medium
    L_QMART    LIKE QMEL-QMART        "Notification type: (mandatory)
      VALUE 'ZM',                     "  M2 = Malfunction report
    L_TQ80     LIKE TQ80,             "Customizing: Notification types
    L_T356     LIKE T356,             "Customizing: Priorities
    L_T_MESG   LIKE MESG              "Messages             "P4BK102530
      OCCURS 0 WITH HEADER LINE,
    L_T_VIQMEL LIKE VIQMEL            "Notification header
      OCCURS 0 WITH HEADER LINE,      "  as an internal table
    V_MIN      TYPE RIMR0-TOTAC,
    V_MAX      LIKE RIMR0-TOTAC,
    V_VAL      LIKE RIMR0-TOTAC,
    L_VIQMEL   LIKE RFC_VIQMEL,       "Notification header
    L_VIQMFE   LIKE RFC_VIQMFE.       "Notification item



LOOP AT IMRG_INS.

  CALL FUNCTION 'MEASUREM_POINT_RFC_SINGLE_002'
   EXPORTING
     MEASUREMENT_POINT        = IMRG_INS-POINT
*       SECONDARY_INDEX          = ' '
     WITH_DIALOG_SCREEN       = ' '
*       EDIT_MODE                = ' '
*       COMMIT_WORK              = 'X'
*       WAIT_AFTER_COMMIT        = 'X'
   IMPORTING
     IMPT_BA                  = l_IMPT
*       UPDATE_SUCCESS           =
   EXCEPTIONS
     NO_AUTHORITY             = 1
     POINT_NOT_FOUND          = 2
     INDEX_NOT_UNIQUE         = 3
     TYPE_NOT_FOUND           = 4
     UPDATE_FAILED            = 5
     OTHERS                   = 6
            .
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  endif.
  check l_impt-mptyp eq 'Z'.
*                                      Sample condition: ...............
*                                      New measurement document is not
*                                      assigned to a notification and a
*                                      valuation code has been given and
*                                      the last digit of the Code is not
*                                      '0'. -> Create malfunction report

*                                      Check notification type ........
  IF L_QMART NE L_TQ80-QMART.
    SELECT SINGLE * INTO L_TQ80 FROM TQ80
           WHERE QMART =  L_QMART.
    IF SY-SUBRC NE 0.
      MESSAGE E421(IM) WITH L_QMART.
    ENDIF.
  ENDIF.

  IF NOT ( L_PRIOK IS INITIAL ).
*                                      Priority given --> check! ......
    IF L_PRIOK NE L_T356-PRIOK.
      SELECT SINGLE * INTO L_T356 FROM T356
             WHERE ARTPR =  L_TQ80-ARTPR
             AND   PRIOK =  L_PRIOK.
      IF SY-SUBRC NE 0.
        MESSAGE E001(I0) WITH 'T356' L_TQ80-ARTPR L_PRIOK SPACE.
      ENDIF.
    ENDIF.
  ENDIF.

*                                      Measurement point changed in the
*                                      same transaction? ...............
  LOOP AT IMPT_UPD
       WHERE POINT =  IMRG_INS-POINT.
    MOVE IMPT_UPD TO L_IMPT.
    EXIT.
  ENDLOOP.

  IF SY-SUBRC NE 0.
*                                      No -> Get from buffer or DB. ....
    CALL FUNCTION 'MEASUREM_POINT_READ'
      EXPORTING
        POINT   = IMRG_INS-POINT
      IMPORTING
        IMPT_WA = L_IMPT
      EXCEPTIONS
        OTHERS  = 1.
  ENDIF.

  CHECK SY-SUBRC           =  0             AND
        L_IMPT-POINT       = IMRG_INS-POINT AND
        NOT ( L_IMPT-MPOBJ IS INITIAL ).

  IF not IMRG_INS-RECDV between
     L_IMPT-MRmin AND l_impt-mrmax.

    CALL FUNCTION 'FLTP_CHAR_CONVERSION_FROM_SI'
      EXPORTING
        CHAR_UNIT        = L_IMPT-MRNGU
        UNIT_IS_OPTIONAL = 'X'
        DECIMALS         = L_IMPT-DECIM
        EXPONENT         = L_IMPT-EXPON
        FLTP_VALUE_SI    = L_IMPT-MRMAX
        INDICATOR_VALUE  = l_impt-MRMAXI
        MASC_SYMBOL      = '_'
      IMPORTING
        CHAR_VALUE       = V_MAX.

    CALL FUNCTION 'FLTP_CHAR_CONVERSION_FROM_SI'
      EXPORTING
        CHAR_UNIT        = IMRG_INS-RECDU
        UNIT_IS_OPTIONAL = 'X'
        DECIMALS         = L_IMPT-DECIM
        EXPONENT         = L_IMPT-EXPON
        FLTP_VALUE_SI    = IMRG_INS-READG
        INDICATOR_VALUE  = IMRG_INS-READGI
        MASC_SYMBOL      = '_'
      IMPORTING
        CHAR_VALUE       = V_VAL.

    CALL FUNCTION 'FLTP_CHAR_CONVERSION_FROM_SI'
      EXPORTING
        CHAR_UNIT        = L_IMPT-MRNGU
        UNIT_IS_OPTIONAL = 'X'
        DECIMALS         = L_IMPT-DECIM
        EXPONENT         = L_IMPT-EXPON
        FLTP_VALUE_SI    = L_IMPT-MRMIN
        INDICATOR_VALUE  = l_impt-MRMINI
        MASC_SYMBOL      = '_'
      IMPORTING
        CHAR_VALUE       = V_MIN.
*                                      Get the transparent key of the ..
*                                      measuring point's object. .......
    CALL FUNCTION 'OBJECT_IDENTIFICATION_GET'
      EXPORTING
        OBJNR  = L_IMPT-MPOBJ
      IMPORTING
        E_KEY  = L_MPOBI
      EXCEPTIONS
        OTHERS = 1.
    CHECK SY-SUBRC =  0.

    CASE L_IMPT-MPOBJ(2).
      WHEN 'IE'.
        MOVE L_MPOBI TO L_VIQMEL-EQUNR.
      WHEN 'IF'.
        MOVE L_MPOBI TO L_VIQMEL-TPLNR.
      WHEN OTHERS.
*                                      Measuring point's object is not .
*                                      allowed as reference object of a
*                                      a notification. -> Next loop! ...
        CONTINUE.
    ENDCASE.

*                                      Fill data to notification header
    MOVE: L_QMART        TO L_VIQMEL-QMART,
          L_T356-PRIOK   TO L_VIQMEL-PRIOK,
          IMRG_INS-IDATE TO L_VIQMEL-AUSVN,
          IMRG_INS-ITIME TO L_VIQMEL-AUZTV,
          IMRG_INS-READR TO L_VIQMEL-QMNAM,
          IMPT_OLD-LOCAS TO L_VIQMEL-BAUTL.
    CONDENSE V_MAX NO-GAPS.
    CONDENSE V_MIN NO-GAPS.
    CONDENSE V_VAL NO-GAPS.
    CONCATENATE 'Value' V_VAL 'not in bet. lts.' V_MIN '-' V_MAX INTO L_VIQMEL-QMTXT SEPARATED BY space.


    IF IMRG_INS-MDTXT       IS INITIAL   AND
       NOT ( IMRG_INS-VLCOD IS INITIAL ).
*                                      Measurement document has no text
*                                      to load up to notification. .....
*                                      --> Take text of valuation code!
      CALL FUNCTION 'QPK1_CODE_TEXT'
        EXPORTING
          I_KATALOGART = IMRG_INS-CODCT
          I_CODEGRUPPE = IMRG_INS-CODGR
          I_CODE       = IMRG_INS-VLCOD
        IMPORTING
          E_TEXT       = L_VIQMEL-QMTXT
        EXCEPTIONS
          OTHERS       = 1.
    ENDIF.

*                                      Create notification header. .....
    CALL FUNCTION 'IQS0_CREATE_VIQMEL'
      EXPORTING
        R_VIQMEL = L_VIQMEL
        I_NO_MSG = 'X'
      TABLES
        E_VIQMEL = L_T_VIQMEL
      EXCEPTIONS
        OTHERS   = 1.

    IF SY-SUBRC <> 0.                  "                    "P4BK102530
      CALL FUNCTION 'MESSAGES_GIVE'
        TABLES
          T_MESG = L_T_MESG.

      LOOP AT L_T_MESG
           WHERE MSGTY = 'E' OR
                 MSGTY = 'A'.
        EXIT.
      ENDLOOP.

      IF SY-SUBRC <> 0.
        MESSAGE E897(IR) WITH 'IQS0_CREATE_VIQMEL'.
      ENDIF.

      MESSAGE ID     L_T_MESG-ARBGB
              TYPE   L_T_MESG-MSGTY
              NUMBER L_T_MESG-TXTNR
              WITH   L_T_MESG-MSGV1
                     L_T_MESG-MSGV2
                     L_T_MESG-MSGV3
                     L_T_MESG-MSGV4.
    ENDIF.

    IF NOT ( IMRG_INS-VLCOD IS INITIAL ) AND
       IMRG_INS-CODCT       =  L_TQ80-FEKAT.
*                                      Measurement document contains a .
*                                      valuation code and the code .....
*                                      catalog type matches with the ...
*                                      customized one at the ...........
*                                      notification type. ..............
*                                      --> Create a notification item ..
      MOVE: IMRG_INS-CODCT TO L_VIQMFE-FEKAT,
            IMRG_INS-CODGR TO L_VIQMFE-FEGRP,
            IMRG_INS-VLCOD TO L_VIQMFE-FECOD,
            IMPT_OLD-LOCAS TO L_VIQMFE-BAUTL,
            IMRG_INS-MDTXT TO L_VIQMFE-FETXT.

      CALL FUNCTION 'IQS0_ADD_ITEM'
        EXPORTING
          I_QMNUM  = L_T_VIQMEL-QMNUM
          R_VIQMFE = L_VIQMFE
          I_NO_MSG = 'X'
        EXCEPTIONS
          OTHERS   = 1.
    ENDIF.

*                                      Prepare posting of notification.
    CALL FUNCTION 'IQS1_POST_NOTIFICATION'
      EXPORTING
        I_COMMIT = ' '
      TABLES
        E_VIQMEL = L_T_VIQMEL
      EXCEPTIONS
        OTHERS   = 1.

    IF SY-SUBRC =  0.

      PERFORM STATUS_UPDATE in PROGRAM SAPLBSVA.
      MESSAGE ID     '8I'
              TYPE   'S'
              NUMBER '000'
              WITH   L_T_VIQMEL-QMNUM.

*                                      Assign the measurement document .
*                                      to the notification. ............
      MOVE-CORRESPONDING IMRG_INS TO IMRG_INS_USR.
      MOVE L_T_VIQMEL-OBJNR TO IMRG_INS_USR-WOOBJ.
      APPEND IMRG_INS_USR.
    ENDIF.
  endif.
ENDLOOP.
