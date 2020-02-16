*&---------------------------------------------------------------------*
*& Report  Z6PM007C_EQUIP_TL_UPL
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  Z6PM007C_EQUIP_TL_UPL.
*----------------------------------------------------------------------*
* OBJECT DESCRIPTION: Equipment Task List Upload
* OBJECT TYPE       : BDC                FUNC. CONSULTANT  : Sanjay
*          DEVELOPER: Ramakrishna
*      CREATION DATE: 01.07.2010
*        DEV REQUEST: IRDK900245
*             TCODE : ZPM007
*----------------------------------------------------------------------*
* REVISION HISTORY-----------------------------------------------------*
*
*        REVISION NO:   R***
*          DEVELOPER:                        DATE:   DD.MM.YYYY
*        DESCRIPTION:
*----------------------------------------------------------------------*
TABLES: AFKO,CRHD,EQUZ.
DATA  V_MODE.
INCLUDE  Z6BDCRECXX .
DATA : FILENAME(128) TYPE C.
DATA :  LTXT(20),CKEY(20),UOM(20),UO1(20),VOR(20),FL(20),WDUR(20)
                                                                 .
DATA : WCENT(20),SCON(20),NO(20),DUR(20),ODESC(20),FLP(20),MARK1(20),
            Mark7(20),MARK2(20),MARK3(20),MARK4(20),MARK5(20),MARK6(20).
DATA :  OPNOC(2), DSNOC(2),OPNOC1(2).
DATA :  OPNO TYPE I,DSNO TYPE I.
DATA  CNT TYPE I.

DATA:  FLAG ,FLAG1.

SELECTION-SCREEN BEGIN OF BLOCK S1 WITH FRAME TITLE TEXT-001 .
PARAMETERS: P-FORE   RADIOBUTTON GROUP RAD1 DEFAULT 'X' ,
            P-BACK   RADIOBUTTON GROUP RAD1 .
SELECTION-SCREEN END   OF BLOCK S1 .

DATA:  BEGIN OF  UPLOAD OCCURS 1 ,
         EQUNR LIKE EQUI-EQUNR,
         KTEXT LIKE PLKOD-KTEXT,
         USAGE LIKE PLKOD-VERWE,
         STATUS LIKE PLKOD-STATU,
         SCOND LIKE PLKOD-ANLZU,
         MAINTS1 LIKE PLKOD-STRAT,
         OPERATION LIKE PLPOD-VORNR,
         WCENT LIKE PLPOD-ARBPL,
         CNTKEY LIKE PLPOD-STEUS,
         OPDESC LIKE PLKOD-KTEXT,
         WKDUR(5),  " LIKE plpod-arbei,
         UOM  LIKE PLPOD-ARBEH,
         NO(8), "  LIKE plpod-anzzl,
         DURA(5), " LIKE plpod-dauno,
         UOM1 LIKE PLPOD-DAUNE,
         LTXT(70)  , "LIKE rstxt-txline,
         MAINTS LIKE PLKOD-STRAT,
         MAINTS2 LIKE PLKOD-STRAT,
       END OF UPLOAD.
DATA  UPLOAD1 LIKE UPLOAD OCCURS 0 WITH HEADER LINE.
DATA  UPLOAD3 LIKE UPLOAD OCCURS 0 WITH HEADER LINE.
DATA  DOWNLOAD LIKE UPLOAD OCCURS 0 WITH HEADER LINE.

START-OF-SELECTION .


  IF P-FORE EQ 'X' .
    V_MODE = 'A' .
  ELSE .
    V_MODE = 'E' .
  ENDIF .
  PERFORM  UPLOAD  .
  PERFORM  BDC_PR .

END-OF-SELECTION .

*&---------------------------------------------------------------------*
*&      Form  upload
*&---------------------------------------------------------------------*
FORM UPLOAD.

  CALL FUNCTION 'UPLOAD'
    EXPORTING
*     CODEPAGE                      = ' '
      FILENAME                      = 'c:\PM.TXT'
      FILETYPE                      = 'DAT'
    TABLES
      DATA_TAB                      = UPLOAD
    EXCEPTIONS
      CONVERSION_ERROR              = 1
      INVALID_TABLE_WIDTH           = 2
      INVALID_TYPE                  = 3
      NO_BATCH                      = 4
      UNKNOWN_ERROR                 = 5
      GUI_REFUSE_FILETRANSFER       = 6
      OTHERS                        = 7 .
  REFRESH : UPLOAD1,UPLOAD3.
  LOOP AT UPLOAD.
    CONDENSE UPLOAD-OPDESC.
    UNPACK UPLOAD-EQUNR TO UPLOAD-EQUNR.
    MODIFY UPLOAD.
  ENDLOOP.


ENDFORM.                    " upload
*&---------------------------------------------------------------------*
*&      Form  bdc_cv01n
*&---------------------------------------------------------------------*
FORM BDC_PR.
  DELETE ADJACENT DUPLICATES FROM UPLOAD1 COMPARING EQUNR OPDESC .
  CLEAR FLAG.
  LOOP AT UPLOAD.
    CLEAR: EQUZ,CRHD.
*    unpack upload-equnr to upload-equnr.
*   select single * from zafko_aufk where equnr eq upload-equnr.
    SELECT SINGLE * FROM EQUZ WHERE EQUNR EQ UPLOAD-EQUNR
                                     AND DATBI GT SY-DATUM  .
    SELECT SINGLE * FROM CRHD WHERE OBJID EQ EQUZ-GEWRK.
    IF SY-SUBRC = 0.
      UPLOAD-WCENT  = CRHD-ARBPL .
      MODIFY UPLOAD.
    ENDIF.
  ENDLOOP.
  REFRESH: DOWNLOAD,UPLOAD1,UPLOAD3.
  LOOP AT UPLOAD.
    IF UPLOAD-WCENT IS INITIAL.
      MOVE-CORRESPONDING UPLOAD TO DOWNLOAD.
      APPEND DOWNLOAD.
      DELETE UPLOAD.
    ENDIF.
  ENDLOOP.
  SORT UPLOAD BY EQUNR.
  APPEND LINES OF UPLOAD TO : UPLOAD1 , UPLOAD3.
  PERFORM DOWNLOAD.
  LOOP AT UPLOAD .
    AT NEW EQUNR.
      FLAG = 1.
    ENDAT.
    IF FLAG = 1 .
      PERFORM BDC_DYNPRO      USING 'SAPLCPDI' '3010'.
      PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                    'RC27E-EQUNR'.
      PERFORM BDC_FIELD       USING  'RC27E-EQUNR'
                                      UPLOAD-EQUNR .
      PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                    '/00'.
      PERFORM BDC_DYNPRO      USING 'SAPLCPDA' '3010'.
      PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                    '=VOUE'.
      PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                    'PLKOD-STRAT  '.
      PERFORM BDC_FIELD       USING 'PLKOD-KTEXT'
                                      UPLOAD-KTEXT .
      PERFORM BDC_FIELD       USING  'RCR01-ARBPL'
                                      UPLOAD-WCENT.
      PERFORM BDC_FIELD       USING 'PLKOD-VERWE'
                                      '4' .
      PERFORM BDC_FIELD       USING 'PLKOD-STATU'
                                      '4'.
      PERFORM BDC_FIELD       USING 'PLKOD-ANLZU'
                                      UPLOAD-SCOND .

      PERFORM BDC_FIELD       USING 'PLKOD-STRAT'
                                         UPLOAD-MAINTS .

      PERFORM FOROPERATION.

      PERFORM BDC_DYNPRO      USING 'SAPLCPDI' '3400'.
      PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                    '=BU'.
      PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                    'PLPOD-VORNR(02)'.

*    PERFORM bdc_field       USING 'PLPOD-DAUNE(01)'
*                                    upload-uom1

      CALL TRANSACTION 'IA01'  USING BDCDATA   MODE  V_MODE .
      CLEAR: BDCDATA , BDCDATA[] .
    ENDIF.
    CLEAR FLAG.
  ENDLOOP .

ENDFORM.                                                    " bdc_cv01n

*---------------------------------------------------------------------*
*       FORM foroperation                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM FOROPERATION.
  SORT UPLOAD1 BY EQUNR OPDESC.
*  LOOP AT upload1 WHERE equnr EQ upload-equnr.
*    AT NEW opdesc.
*      cnt = cnt + 1.
*    ENDAT.
*  ENDLOOP.
  CLEAR: FLAG1,OPNO,OPNOC.
  LOOP AT UPLOAD1 WHERE EQUNR EQ UPLOAD-EQUNR.
    AT NEW OPDESC.
      FLAG1 = 1.
    ENDAT.
    IF FLAG1 = 1.
      OPNOC1 = OPNO.
      OPNO = OPNO + 1.
      OPNOC = OPNO.
      CONCATENATE 'PLPOD-ARBPL(' OPNOC ')' INTO WCENT.
      CONCATENATE 'PLPOD-STEUS(' OPNOC ')' INTO CKEY.
      CONCATENATE 'PLPOD-LTXA1(' OPNOC ')' INTO ODESC.
      CONCATENATE 'PLPOD-ARBEI(' OPNOC ')' INTO WDUR.
      CONCATENATE 'PLPOD-ARBEH(' OPNOC ')' INTO UOM.
      CONCATENATE 'PLPOD-ANZZL(' OPNOC ')' INTO NO.
      CONCATENATE 'PLPOD-DAUNO(' OPNOC ')' INTO DUR.
      CONCATENATE 'PLPOD-DAUNE(' OPNOC ')' INTO UO1.
      CONCATENATE 'PLPOD-VORNR(' OPNOC ')' INTO VOR.
      CONCATENATE 'RC27X-FLG_SEL(' OPNOC ')' INTO FL.
      CONCATENATE 'RC27X-FLG_SEL(' OPNOC1 ')' INTO FLP.
      PERFORM BDC_DYNPRO      USING 'SAPLCPDI' '3400'.
      PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                    '/00'.
      PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                    'PLPOD-DAUNE(01)'.
*      PERFORM bdc_field       USING wcent
*                                      upload1-wcent .
      PERFORM BDC_FIELD       USING CKEY
                                      UPLOAD1-CNTKEY .
      PERFORM BDC_FIELD       USING ODESC
                                      UPLOAD1-OPDESC.
      PERFORM BDC_FIELD       USING SCON
                                      UPLOAD1-SCOND .
      PERFORM BDC_FIELD       USING WDUR
                                      UPLOAD1-WKDUR.
      PERFORM BDC_FIELD       USING UOM
                                      UPLOAD1-UOM .
      PERFORM BDC_FIELD       USING NO
                                      UPLOAD1-NO .
      PERFORM BDC_FIELD       USING DUR
                                      UPLOAD1-DURA.
      PERFORM BDC_FIELD       USING UO1
                                      UPLOAD1-UOM1.
      PERFORM BDC_DYNPRO      USING 'SAPLCPDI' '3400'.
      PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                    '=LTXT'.
      PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                    VOR.
      IF OPNOC1 NE 0.
        PERFORM BDC_FIELD       USING FLP  ' '.
      ENDIF.

      PERFORM BDC_FIELD       USING FL
                                    'X'.
      PERFORM FORLTXT.

      PERFORM BDC_DYNPRO      USING 'SAPLCPDI' '3400'.
      PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                    '=WPLT'.
      PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                    VOR.

      PERFORM FORMS.
    ENDIF.
    CLEAR FLAG1.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FORLTXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FORLTXT.
  DATA: FLAGL(2),CUR TYPE I , CURC(2).
  DATA: CLTXT(20).
  CLEAR: DSNO, DSNOC.


  CUR = 2.
  DSNO = 2.
  LOOP AT UPLOAD3 WHERE EQUNR EQ UPLOAD-EQUNR
                  AND   OPDESC EQ UPLOAD1-OPDESC.
    IF DSNO <= 15.
      DSNO = DSNO + 1.

    ENDIF.
    IF CUR <= 16.
      CUR  = CUR + 1.
    ENDIF.

    DSNOC = DSNO.
    CURC = CUR.
    CONCATENATE 'RSTXT-TXLINE(' DSNOC ')' INTO LTXT.

    CONCATENATE 'RSTXT-TXLINE(' CURC ')' INTO CLTXT.
*      unpack upload3-ltxt to upload3-ltxt.
    PERFORM BDC_DYNPRO      USING 'SAPLSTXX' '1100'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  CLTXT.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                         '=EDNP'.
    PERFORM BDC_DYNPRO      USING 'SAPLSTXX' '1100'.


    PERFORM BDC_FIELD       USING LTXT
                                    UPLOAD3-LTXT(40) .
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  CLTXT.
    IF UPLOAD3-LTXT+40(30) NE SPACE.
        DSNO = DSNO + 1.
        CUR = CUR + 1.
      CURC = CUR.
      DSNOC = DSNO.
      CONCATENATE 'RSTXT-TXLINE(' DSNOC ')' INTO LTXT.


      PERFORM BDC_FIELD       USING LTXT
                                      UPLOAD3-LTXT+40(30) .
      PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  CLTXT.
      IF DSNO > 16.

        DSNO = DSNO - 1.
      ENDIF.
      IF DSNO > 17.

        CUR = CUR - 1.
      ENDIF.



    ENDIF.
*    AT END OF OPDESC.
*      FLAGL = 1.
*    ENDAT.
*    IF FLAGL = 1.
*      PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                           '=TXBA'.
*    ELSE.
      PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                           '=E'.
*    ENDIF.
    CLEAR FLAGL.
  ENDLOOP.
PERFORM BDC_DYNPRO      USING 'SAPLSTXX' '1100'.
      PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                           '=TXBA'.
ENDFORM.                    " FORLTXT
*&---------------------------------------------------------------------*
*&      Form  forms
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FORMS.
  CONCATENATE 'RIHSTRAT-MARK01(' OPNOC ')' INTO MARK1.
  CONCATENATE 'RIHSTRAT-MARK02(' OPNOC ')' INTO MARK2.
  CONCATENATE 'RIHSTRAT-MARK03(' OPNOC ')' INTO MARK3.
  CONCATENATE 'RIHSTRAT-MARK04(' OPNOC ')' INTO MARK4.
  CONCATENATE 'RIHSTRAT-MARK05(' OPNOC ')' INTO MARK5.
  CONCATENATE 'RIHSTRAT-MARK06(' OPNOC ')' INTO MARK6.
  CONCATENATE 'RIHSTRAT-MARK07(' OPNOC ')' INTO MARK7.
  PERFORM BDC_DYNPRO      USING 'SAPLCPDI' '3600'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=BACK'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'RIHSTRAT-MARK02(01)'.
  IF UPLOAD1-MAINTS EQ 'B1'.
    IF UPLOAD1-MAINTS2 EQ '1D'.
      PERFORM BDC_FIELD       USING
                                    MARK1   'X'.
    ELSEIF UPLOAD1-MAINTS2 EQ '1W'.
      PERFORM BDC_FIELD       USING
                                    MARK2   'X'.
    ELSEIF UPLOAD1-MAINTS2 EQ '1M'.
      PERFORM BDC_FIELD       USING
                                    MARK3   'X'.
    ELSEIF UPLOAD1-MAINTS2 EQ '3M'.
      PERFORM BDC_FIELD       USING
                                    MARK4   'X'.
    ELSEIF UPLOAD1-MAINTS2 EQ '6M'.
      PERFORM BDC_FIELD       USING
                                    MARK5   'X'.
    ELSEIF UPLOAD1-MAINTS2 EQ '1Y'.
      PERFORM BDC_FIELD       USING
                                    MARK6   'X'.

    ENDIF.
  ENDIF.
  IF UPLOAD1-MAINTS EQ 'C1'.
    IF UPLOAD1-MAINTS2 EQ '1W'.
      PERFORM BDC_FIELD       USING
                                    MARK1   'X'.
    ELSEIF UPLOAD1-MAINTS2 EQ '2W'.
      PERFORM BDC_FIELD       USING
                                    MARK2   'X'.
    ELSEIF UPLOAD1-MAINTS2 EQ '1M'.
      PERFORM BDC_FIELD       USING
                                    MARK3   'X'.
    ELSEIF UPLOAD1-MAINTS2 EQ '3M'.
      PERFORM BDC_FIELD       USING
                                    MARK4   'X'.
    ELSEIF UPLOAD1-MAINTS2 EQ '6M'.
      PERFORM BDC_FIELD       USING
                                    MARK5   'X'.
    ELSEIF UPLOAD1-MAINTS2 EQ '1Y'.
      PERFORM BDC_FIELD       USING
                                    MARK6   'X'.
    ELSEIF UPLOAD1-MAINTS2 EQ '2M'.
      PERFORM BDC_FIELD       USING
                                    MARK7   'X'.

    ENDIF.
  ENDIF.
ENDFORM.                    " forms
*&---------------------------------------------------------------------*
*&      Form  download
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DOWNLOAD.
  IF NOT DOWNLOAD[] IS INITIAL.
    CONCATENATE 'C:\ERRLOG' SY-DATUM '_' SY-UZEIT '.xls' INTO FILENAME.
    CONDENSE FILENAME NO-GAPS.
    CALL FUNCTION 'WS_DOWNLOAD'
         EXPORTING
              FILENAME                = FILENAME
              FILETYPE                = 'DAT'
         TABLES
              DATA_TAB                = DOWNLOAD
         EXCEPTIONS
              FILE_OPEN_ERROR         = 1
              FILE_WRITE_ERROR        = 2
              INVALID_FILESIZE        = 3
              INVALID_TYPE            = 4
              NO_BATCH                = 5
              UNKNOWN_ERROR           = 6
              INVALID_TABLE_WIDTH     = 7
              GUI_REFUSE_FILETRANSFER = 8
              CUSTOMER_ERROR          = 9
              OTHERS                  = 10.
  ENDIF.

ENDFORM.                    " download
