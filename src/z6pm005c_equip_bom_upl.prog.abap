*&---------------------------------------------------------------------*
*& Report  Z6PM005C_EQUIP_BOM_UPL
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  Z6PM005C_EQUIP_BOM_UPL.
*----------------------------------------------------------------------*
* OBJECT DESCRIPTION: Equipment BOM Upload - IB01
* OBJECT TYPE       : BDC                FUNC. CONSULTANT : Sanjay
*          DEVELOPER: Ramakrishna
*      CREATION DATE: 30.06.2010
*        DEV REQUEST: IRDK900227
*             TCODE : ZPM005
*----------------------------------------------------------------------*
* REVISION HISTORY-----------------------------------------------------*
*
*        REVISION NO:   R***
*          DEVELOPER:                        DATE:   DD.MM.YYYY
*        DESCRIPTION:
*----------------------------------------------------------------------*

INCLUDE  z6bdcrecXX .
DATA  bomexist.

TABLES: equi,eqst,marc,stpo,MARA.

DATA  flag.
DATA  v_werks LIKE t001w-werks.
DATA  v_matnr LIKE mara-matnr.
DATA  cntr TYPE i.
DATA:  BEGIN OF  upload OCCURS 1 ,
         equnr LIKE equi-equnr,
         ict   LIKE rc29p-postp,
         werks LIKE rc29n-werks,
         matnr LIKE rc29p-idnrk,
         meins LIKE rc29p-meins,
         menge(10), " LIKE RC29P-MENGE,
         usage LIKE rc29p-postp,
       END OF upload.
DATA : BEGIN OF iequnr OCCURS 1,
         equnr LIKE equi-equnr,
         werks LIKE rc29n-werks,
         usage LIKE rc29p-postp,
       END OF iequnr.
DATA : BEGIN OF download OCCURS 0 .
        INCLUDE STRUCTURE upload.
DATA   desc(40).
DATA   END OF download.
*DATA : download2 LIKE iequnr OCCURS 0 WITH HEADER LINE.
*DATA : download1 LIKE  upload OCCURS 0 WITH HEADER LINE.
DATA v_mode.
DATA  filename(128).
DATA: fn_matnr(30), fn_ict(30) ,fn_menge(30), fn_meins(30),
      ln_no(2) TYPE n.
DATA: BEGIN OF messagetab OCCURS 1.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA END OF messagetab.
DATA : V_FILE TYPE STRING.
SELECTION-SCREEN BEGIN OF BLOCK s1 WITH FRAME TITLE text-001 .
PARAMETERS: p-fore   RADIOBUTTON GROUP rad1  ,
            p-back   RADIOBUTTON GROUP rad1 DEFAULT 'X' .
SELECTION-SCREEN END   OF BLOCK s1 .
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


START-OF-SELECTION.

  IF p-fore EQ 'X' .
    v_mode = 'A' .
  ELSE .
    v_mode = 'E' .
  ENDIF .
  PERFORM  upload.
  PERFORM  run_bdc.
  PERFORM  download.
*---------------------------------------------------------------------*
*       FORM upload                                                   *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM upload.

*  CALL FUNCTION 'UPLOAD'
*    EXPORTING
**     CODEPAGE                      = ' '
*      filename                      = 'c:\doc1.txt'
*      filetype                      = 'DAT'
*    TABLES
*      data_tab                      = upload
*    EXCEPTIONS
*      conversion_error              = 1
*      invalid_table_width           = 2
*      invalid_type                  = 3
*      no_batch                      = 4
*      unknown_error                 = 5
*      gui_refuse_filetransfer       = 6
*      OTHERS                        = 7 .

V_FILE = P_FILE.
CALL FUNCTION 'GUI_UPLOAD'
  EXPORTING
    FILENAME                      = V_FILE
    FILETYPE                      = 'DAT'
    HAS_FIELD_SEPARATOR           = 'X'
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
    DATA_TAB                      =  UPLOAD
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
*&      Form  RUN_BDC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM run_bdc.

  LOOP AT upload.
    UNPACK upload-equnr TO upload-equnr.
    MODIFY upload.
    MOVE-CORRESPONDING upload TO iequnr.
    COLLECT iequnr.
  ENDLOOP.
  SORT iequnr BY equnr.
  SORT upload BY equnr.
  LOOP AT iequnr.

    CALL FUNCTION 'CHECK_EQUI_BOM'
      EXPORTING
        datum                = sy-datum
        equnr                = iequnr-equnr
*        FLG_LVORM_EXC        = 'X'
     IMPORTING
*        SHORT_TEXT           =
       werk                 = v_werks
*        EEQUI                =
     EXCEPTIONS
       equi_not_found       = 1
       equi_to_delete       = 2
       OTHERS               = 3
              .
    IF  sy-subrc = 1.
      MOVE-CORRESPONDING iequnr TO download .
      download-desc = 'Equipment not found'.
      APPEND download.
      CONTINUE.
    ENDIF.
    IF v_werks NE iequnr-werks .
      MOVE-CORRESPONDING iequnr TO download .
      download-desc = 'Equipment not found for plant'.
      APPEND download.
      CONTINUE.
    ENDIF.
*    select single * from equi where equnr eq iequnr-equnr.
*    if sy-subrc ne 0.
*       continue.
*    endif.
    SELECT SINGLE * FROM eqst WHERE equnr EQ iequnr-equnr
                    AND werks EQ iequnr-werks
                    AND stlan EQ iequnr-usage .
    IF sy-subrc EQ 0.
      bomexist = 1.
    ENDIF.
*    if sy-subrc eq 0.
*    move-corresponding iequnr to download2.
*    append download2.
*       continue.
*    endif.



    CLEAR cntr.
    PERFORM bdc_dynpro      USING 'SAPLCSDI' '0200'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'RC29N-STLAN'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '/00'.
    PERFORM bdc_field       USING 'RC29N-EQUNR'
                                  iequnr-equnr .
    PERFORM bdc_field       USING 'RC29N-WERKS'
                                  iequnr-werks .
    PERFORM bdc_field       USING 'RC29N-STLAN'
                                  iequnr-usage .

    if bomexist = 1.
      PERFORM bdc_dynpro      USING 'SAPLCSDI' '0150'.

      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=FCNP'.
    else.
      PERFORM bdc_dynpro      USING 'SAPLCSDI' '0110'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'RC29K-EXSTL'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '/00'.
      PERFORM bdc_dynpro      USING 'SAPLCSDI' '0111'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'RC29K-LABOR'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '/00'.

    endif.


    CLEAR flag.
    LOOP AT upload WHERE equnr EQ iequnr-equnr AND werks EQ iequnr-werks
                       AND   usage EQ iequnr-usage.
      AT NEW equnr.
        flag = 1.
      ENDAT.
      v_matnr = upload-matnr.
      UNPACK v_matnr TO v_matnr.
      SELECT SINGLE * FROM marC WHERE ( matnr EQ v_matnr
                                       or matnr eq upload-matnr )
                                and   werks eq upload-werks.
      IF sy-subrc NE 0.
        MOVE-CORRESPONDING upload TO download.
        download-desc = 'Material/Plant not found'.
        APPEND download.
        CONTINUE.
      ENDIF.
      IF bomexist = 1.
        SELECT SINGLE * FROM stpo
                        WHERE stlty EQ 'E'
                        AND   stlnr EQ eqst-stlnr
                        AND  ( idnrk EQ upload-matnr
                               or idnrk eq v_matnr ).
        IF sy-subrc = 0.
          MOVE-CORRESPONDING upload TO download.
          download-desc = 'Material Already Exist'.
          APPEND download.
          CONTINUE.
        ENDIF.

      ENDIF.
      PERFORM bdc_dynpro      USING 'SAPLCSDI' '0140'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'RC29P-POSTP(02)'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=FCNP'.

*   CONCATENATE 'RC29P-IDNRK(' LN_no ')' into fn_MATNR.
*   CONCATENATE 'RC29P-POSTP(' LN_no ')' into fn_ict.
*   CONCATENATE 'RC29P-MEINS(' LN_no ')' into fn_MEINS.
*   CONCATENATE 'RC29P-MENGE(' LN_no ')' into fn_MENGE.
      IF flag = 1 and bomexist ne 1.
        PERFORM bdc_field       USING 'RC29P-IDNRK(01)'
                                      upload-matnr .
        PERFORM bdc_field       USING 'RC29P-POSTP(01)'
                                      upload-ict.
        PERFORM bdc_field       USING 'RC29P-MEINS(01)'
                                      upload-meins.
        PERFORM bdc_field       USING 'RC29P-MENGE(01)'
                                      upload-menge.
      ELSE.
        PERFORM bdc_field       USING 'RC29P-IDNRK(02)'
                                      upload-matnr .
        PERFORM bdc_field       USING 'RC29P-POSTP(02)'
                                      upload-ict.
        PERFORM bdc_field       USING 'RC29P-MEINS(02)'
                                      upload-meins.
        PERFORM bdc_field       USING 'RC29P-MENGE(02)'
                                      upload-menge.

      ENDIF.
      PERFORM bdc_dynpro      USING 'SAPLCSDI' '0130'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '/00'.

      PERFORM bdc_dynpro      USING 'SAPLCSDI' '0131'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '/00'.

      CLEAR flag.

    ENDLOOP.
    PERFORM bdc_dynpro      USING 'SAPLCSDI' '0140'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'RC29P-POSTP(02)'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '=FCBU'.
    IF bomexist = 1.
      CALL TRANSACTION 'IB02'  USING bdcdata   MODE  v_mode.
    ELSE.
      CALL TRANSACTION 'IB01'  USING bdcdata   MODE  v_mode.
    ENDIF.
*    messages into messagetab.
*    read table messagetab with key MSGTYP = 'E'.
*    if sy-subrc = 0.
*       CALL FUNCTION 'BDC_OPEN_GROUP'
*        EXPORTING
*          CLIENT                    = SY-MANDT
**          DEST                      = FILLER8
*          GROUP                     = 'ZTEST2'
**          HOLDDATE                  = FILLER8
**          KEEP                      = FILLER1
*          USER                      = SY-UNAME
**          RECORD                    = FILLER1
**        IMPORTING
**          QID                       =
**        EXCEPTIONS
**          CLIENT_INVALID            = 1
**          DESTINATION_INVALID       = 2
**          GROUP_INVALID             = 3
**          GROUP_IS_LOCKED           = 4
**          HOLDDATE_INVALID          = 5
**          INTERNAL_ERROR            = 6
**          QUEUE_ERROR               = 7
**          RUNNING                   = 8
**          SYSTEM_LOCK_ERROR         = 9
**          USER_INVALID              = 10
**          OTHERS                    = 11
*                 .
*       IF sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*       ENDIF.
*
*
*    CALL FUNCTION 'BDC_INSERT'
*     EXPORTING
*       TCODE                  = 'IB01'
**       POST_LOCAL             = NOVBLOCAL
**       PRINTING               = NOPRINT
*        TABLES
*        dynprotab              = BDCDATA
**     EXCEPTIONS
**       INTERNAL_ERROR         = 1
**       NOT_OPEN               = 2
**       QUEUE_ERROR            = 3
**       TCODE_INVALID          = 4
**       PRINTING_INVALID       = 5
**       POSTING_INVALID        = 6
**       OTHERS                 = 7
*              .
*    IF sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    ENDIF.
*
*CALL FUNCTION 'BDC_CLOSE_GROUP'
** EXCEPTIONS
**   NOT_OPEN          = 1
**   QUEUE_ERROR       = 2
**   OTHERS            = 3
*          .
*IF sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*ENDIF.
*ENDIF.
    CLEAR: bdcdata , bdcdata[],bomexist.

  ENDLOOP.

ENDFORM.                    " RUN_BDC

*&---------------------------------------------------------------------*
*&      Form  download
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM download.
  IF NOT download[] IS INITIAL.
    CONCATENATE 'C:\BOM_errlog_'
                             sy-datum '_' sy-uzeit '.xls' INTO filename.
    CONDENSE filename NO-GAPS.
    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        filename                = filename
        filetype                = 'DAT'
      TABLES
        data_tab                = download
      EXCEPTIONS
        file_open_error         = 1
        file_write_error        = 2
        invalid_filesize        = 3
        invalid_type            = 4
        no_batch                = 5
        unknown_error           = 6
        invalid_table_width     = 7
        gui_refuse_filetransfer = 8
        customer_error          = 9
        OTHERS                  = 10.
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDIF.

ENDFORM.                    "download
