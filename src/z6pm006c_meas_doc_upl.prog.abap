*&---------------------------------------------------------------------*
*& Report  Z6PM006C_MEAS_DOC_UPL
REPORT  Z6PM006C_MEAS_DOC_UPL.
*----------------------------------------------------------------------*
* OBJECT DESCRIPTION: Measuring Document Upload
* OBJECT TYPE       : BDC                FUNC. CONSULTANT  : Sanjay
*          DEVELOPER: Ramakrishna
*      CREATION DATE: 01.07.2010
*        DEV REQUEST: IRDK900247
*             TCODE : ZPM007
*----------------------------------------------------------------------*
* REVISION HISTORY-----------------------------------------------------*
*
*        REVISION NO:   R***
*          DEVELOPER:                        DATE:   DD.MM.YYYY
*        DESCRIPTION:
*----------------------------------------------------------------------*
INCLUDE  z6bdcrecxx .
DATA: BEGIN OF itab OCCURS 0,
        POINT LIKE IMRG-POINT,                   " Measuring Point
        dfdat(10),                               " Date
        DFTIM(8),                                " Time
        DFRDR type RIMR0-DFRDR,                  " User Name
        READC(15),                               " Reading Value

      END OF itab.
DATA: count(2)  TYPE   n ,
      bdc_fld(30)  TYPE c ,
      vgw01(15) ,
      zeinh01(3),
      tran_mode ,
      msg01(50) ,    msg02(50) ,    msg03(50) ,   msg04(50) ,
      msg_txt(50) .
DATA : V_FILENAME TYPE STRING.
*&---------------------------------------------------------------------*
*           SELECTION-SCREEN
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK s01 WITH FRAME TITLE TEXT-T01.
PARAMETERS:  p_fore     RADIOBUTTON   GROUP rad DEFAULT 'X' ,
             p_back     RADIOBUTTON   GROUP rad ,
             p_noerr    RADIOBUTTON   GROUP rad .
SELECTION-SCREEN END OF BLOCK S01.

SELECTION-SCREEN BEGIN OF BLOCK s2 WITH FRAME TITLE text-002 .
PARAMETERS: p_file  type IBIPPARMS-PATH  .

SELECTION-SCREEN END OF BLOCK s2 .

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.

     CALL FUNCTION 'F4_FILENAME'
       EXPORTING
         PROGRAM_NAME        = SYST-CPROG
         DYNPRO_NUMBER       = SYST-DYNNR
         FIELD_NAME          = 'P_FILE '
       IMPORTING
         FILE_NAME           = P_FILE
               .

     IF NOT P_FILE IS INITIAL.

       V_FILENAME = P_FILE.
     ENDIF.
*&---------------------------------------------------------------------*
*           START-OF-SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM upload.

  IF p_fore EQ 'X' .
    tran_mode = 'A' .
  ELSEIF p_back EQ 'X' .
    tran_mode = 'E' .
  ELSEIF p_noerr EQ 'X' .
    tran_mode = 'N' .
  ENDIF .

  LOOP AT iTAB .
    PERFORM  bdc_ik11.
  ENDLOOP .

  END-OF-SELECTION .

*&---------------------------------------------------------------------*
*&      Form  upload
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM upload.
*  CALL FUNCTION 'UPLOAD'
*       EXPORTING
*            filename                = 'C:\REC.TXT'
*            filetype                = 'DAT'
*       TABLES
*            data_tab                = itab
*       EXCEPTIONS
*            conversion_error        = 1
*            invalid_table_width     = 2
*            invalid_type            = 3
*            no_batch                = 4
*            unknown_error           = 5
*            gui_refuse_filetransfer = 6
*            OTHERS                  = 7.

CALL FUNCTION 'GUI_UPLOAD'
  EXPORTING
    FILENAME                      = V_FILENAME
    FILETYPE                      = 'DAT'
*   HAS_FIELD_SEPARATOR           = ' '
*   HEADER_LENGTH                 = 0
*   READ_BY_LINE                  = 'X'
*   DAT_MODE                      = ' '
*   CODEPAGE                      = ' '
*   IGNORE_CERR                   = ABAP_TRUE
*   REPLACEMENT                   = '#'
*   CHECK_BOM                     = ' '
*   VIRUS_SCAN_PROFILE            =
*   NO_AUTH_CHECK                 = ' '
* IMPORTING
*   FILELENGTH                    =
*   HEADER                        =
  TABLES
    DATA_TAB                      = ITAB
* EXCEPTIONS
*   FILE_OPEN_ERROR               = 1
*   FILE_READ_ERROR               = 2
*   NO_BATCH                      = 3
*   GUI_REFUSE_FILETRANSFER       = 4
*   INVALID_TYPE                  = 5
*   NO_AUTHORITY                  = 6
*   UNKNOWN_ERROR                 = 7
*   BAD_DATA_FORMAT               = 8
*   HEADER_NOT_ALLOWED            = 9
*   SEPARATOR_NOT_ALLOWED         = 10
*   HEADER_TOO_LONG               = 11
*   UNKNOWN_DP_ERROR              = 12
*   ACCESS_DENIED                 = 13
*   DP_OUT_OF_MEMORY              = 14
*   DISK_FULL                     = 15
*   DP_TIMEOUT                    = 16
*   OTHERS                        = 17
          .
IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
ENDIF.

ENDFORM.                    " upload
*&---------------------------------------------------------------------*
*&      Form  bdc_c201
*&---------------------------------------------------------------------*
FORM bdc_IK11.

  data: zero   type   p decimals 2 .

* First Screen of C201
  PERFORM bdc_dynpro      USING 'SAPLIMR0' '1210'.

  PERFORM bdc_field       USING 'BDC_CURSOR'     'RIMR0-DFRDR'.
  PERFORM bdc_field       USING 'BDC_OKCODE'     '/00'.

  PERFORM bdc_field       USING 'IMRG-POINT'    itAB-POINT.
  PERFORM bdc_field       USING 'RIMR0-DFTIM'   ITAB-DFTIM .
  PERFORM bdc_field       USING 'RIMR0-DFDAT'   ITAB-DFDAT.
  PERFORM bdc_field       USING 'RIMR0-DFRDR'   ITAB-DFRDR .

* Second Screen after Enter
  PERFORM bdc_dynpro      USING 'SAPLIMR0' '5210'.
  PERFORM bdc_field       USING 'BDC_CURSOR' 'RIMR0-READC'.
  PERFORM bdc_field       USING 'BDC_OKCODE' '/00'.
  PERFORM bdc_field       USING 'RIMR0-READC' ITAB-READC.

* Second Screen after Enter
  PERFORM bdc_dynpro      USING 'SAPLIMR0' '5210'.
  PERFORM bdc_field       USING 'BDC_CURSOR' 'RIMR0-READC'.
  PERFORM bdc_field       USING 'BDC_OKCODE' '=BU'.




  CALL TRANSACTION 'IK11' USING bdcdata MODE tran_mode

                                         MESSAGES INTO messtab .

*************************************************************************
** BREAK-POINT .
* Check the LOG
*  LOOP AT messtab .
*    msg01 = messtab-msgv1 .
*    msg02 = messtab-msgv2 .
*    msg03 = messtab-msgv3 .
*    msg04 = messtab-msgv4 .
*    CALL FUNCTION 'MESSAGE_PREPARE'
*         EXPORTING
*              language = sy-langu
*              msg_id   = messtab-msgid
*              msg_no   = messtab-msgnr
*              msg_var1 = msg01
*              msg_var2 = msg02
*              msg_var3 = msg03
*              msg_var4 = msg04
*         IMPORTING
*              msg_text = msg_txt.
*    message_gui msg_txt .
*  ENDLOOP .
***********************************************************************
  CLEAR: bdcdata , bdcdata[] .

ENDFORM.
