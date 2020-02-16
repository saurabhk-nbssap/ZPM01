*&---------------------------------------------------------------------*
*& Report  Z6PM008C_MAINTPLAN_IP42_UPL
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  Z6PM008C_MAINTPLAN_IP42_UPL.
*----------------------------------------------------------------------*
* OBJECT DESCRIPTION: Maintenance Plan Upload
* OBJECT TYPE       : BDC                FUNC. CONSULTANT  : Sanjay
*          DEVELOPER: Ramakrishna
*      CREATION DATE: 07.07.2010
*        DEV REQUEST: IRDK900261 ,IRDK900332
*             TCODE : ZPM008
*----------------------------------------------------------------------*
* REVISION HISTORY-----------------------------------------------------*
*
*        REVISION NO:   R001
*          DEVELOPER:   Ramakrishna             DATE:   13.07.2010
*        DESCRIPTION:   Fine Tuning of Program
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*     TABLES
*----------------------------------------------------------------------*
TABLES: t351t      ,
        t351x      ,
        mpos       ,
        t351       ,
        t430t      ,
        t024a      ,
        crhd       ,
        t399w      ,
        t350       ,
        sscrfields ,
        eapl       ,
        equz       .



*----------------------------------------------------------------------*
*     INCLUDE
*----------------------------------------------------------------------*
INCLUDE  z6bdcrecxx .

*----------------------------------------------------------------------*
*     SELECTION SCREEN
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK s1 WITH FRAME TITLE text-001 .
PARAMETERS: p_fore   RADIOBUTTON GROUP rad1 DEFAULT 'X' ,
            p_back   RADIOBUTTON GROUP rad1 .
SELECTION-SCREEN END   OF BLOCK s1 .
SELECTION-SCREEN BEGIN OF BLOCK s2 WITH FRAME TITLE text-002 .
PARAMETERS: p_file  type IBIPPARMS-PATH  .

SELECTION-SCREEN END OF BLOCK s2 .
*
SELECTION-SCREEN : BEGIN OF BLOCK s3  WITH FRAME TITLE text-s03.
SELECTION-SCREEN PUSHBUTTON  /1(7) help USER-COMMAND info.
SELECTION-SCREEN PUSHBUTTON  15(17) down USER-COMMAND down.
SELECTION-SCREEN : END OF BLOCK s3.

INCLUDE Z6XX003I_BDC_NOTE.

INITIALIZATION.
*----------------------------------------------------------------------*
*  MOVE 'Document Upload Mode' TO  sscrfields-functxt_01.
  MOVE '@0S@' TO help.
  MOVE '@49@ Download'(003) TO down.
  PERFORM f_fill_infotext.
*----------------------------------------------------------------------*
*     GLOBLE DATA
*----------------------------------------------------------------------*
  DATA  v_mode.
  DATA : V_FILENAME TYPE STRING.





*----------------------------------------------------------------------*
*     INTERNAL TABLES
*----------------------------------------------------------------------*


  TYPES : BEGIN OF S_UPLOAD  ,
           mptyp  LIKE v_t399w_i-mptyp ,  "Maintenance plan Category
           strat  LIKE t351-strat ,       "Strategy
           plntxt(40) ,                   "Plan Text
*        equnr  LIKE equi-equnr ,       "Equipment
           equnr(18) TYPE n ,             "Equipment
           auart  LIKE v_t003o_i-auart ,  "Order Type
           wcent  LIKE plpod-arbpl ,      "WORK CENTER
           plnty  LIKE eapl-plnty ,       "Task list type
*        plnnr  LIKE eapl-plnnr ,       "Task list group
           plnnr(8) TYPE n ,              "Task list group
           plnal(2) TYPE n ,              "Group counter
           vspos(3) ,                     "SF later confirmation
           topos(3) ,                     "Tolerance (+)
           vsneg(3) ,                     "SF earlier confirmation
           toneg(3) ,                     "Tolerance (-)
           sfakt(4) ,                     "Cycle modification factor
           fabkl(2) ,                     "Factory calendar
           horiz(3) ,                     "Call horizon
           abrho(3) ,                     "Scheduling period
           stadt(10) ,                    "Start of cycle
         END   OF s_UPLOAD .


  DATA : I_UPLOAD TYPE TABLE OF S_UPLOAD.
  DATA  : ST_UPLOAD TYPE S_UPLOAD.
  DATA : BEGIN OF err_data OCCURS 0 ,
           mptyp  LIKE v_t399w_i-mptyp ,
           strat  LIKE t351-strat ,
           equnr  LIKE  equi-equnr,
           err(50) ,
         END   OF err_data .

  DATA : g_answer              TYPE c,
         g_lines_tab           TYPE popuptext OCCURS 0
                               WITH HEADER LINE.
  DATA : IDWN TYPE ZZTT_FILEFORMAT.
  DATA : ST_DWN TYPE ZZLT_FILEFORMAT.
  DATA : wa_DWN TYPE ZZLT_FILEFORMAT.


*DATA : BEGIN OF idwn OCCURS 0,
*              text1(25),
*              text2(25),
*              text3(25),
*              text4(25),
*              text5(25),
*              text6(25),
*              text7(25),
*              text8(25),
*              text9(25),
*              text10(25),
*              text11(25),
*              text12(25),
*              text13(25),
*              text14(25),
*              text15(25),
*              text16(25),
*              text17(25),
*              text18(25),
*              text19(15),
*              text20(15),
*              text21(15),
*              text22(15),
*              text23(15),
*              text24(15),
*              text25(15),
*              text26(15),
*              text27(15),
*              text28(15),
*              text29(15),
*              text30(15),
*              text31(15),
****kdamle-09062005-start
*              text32(15),
*              text33(15),
*              text34(15),
*              text35(15),
*              text36(15),
*              text37(15),
*              text38(15),
*              text39(15),
*              text40(15),
****kdamle-09062005-end
****kdamle-03012006-start
*              text41(8),
****kdamle-03012006-end
*   END OF idwn.


AT SELECTION-SCREEN.
  IF sscrfields-ucomm = 'INFO'.
    CALL FUNCTION 'DD_POPUP_WITH_INFOTEXT'
      EXPORTING
        titel        = 'Text File Format '(020)
        start_column = 10
        start_row    = 10
        end_column   = 85
        end_row      = 27
        infoflag     = ' '
      IMPORTING
        answer       = g_answer
      TABLES
        lines        = g_lines_tab.
  ELSEIF sscrfields-ucomm = 'DOWN'.
    PERFORM f_fill_idwn.
    CALL FUNCTION 'DOWNLOAD'
      EXPORTING
        filename = 'c:\FIDocumentfile format.txt'
        filetype = 'DAT'
      TABLES
        data_tab = idwn.

  ENDIF.
*

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.

  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      PROGRAM_NAME  = SYST-CPROG
      DYNPRO_NUMBER = SYST-DYNNR
      FIELD_NAME    = 'P_FILE '
    IMPORTING
      FILE_NAME     = P_FILE.

  IF NOT P_FILE IS INITIAL.

    V_FILENAME = P_FILE.
  ENDIF.
*----------------------------------------------------------------------*
*     START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION .
  IF p_fore EQ 'X' .
    v_mode = 'A' .
  ELSE .
    v_mode = 'E' .
  ENDIF .
  PERFORM  F_UPLOAD  .
  PERFORM  check_data .
  PERFORM  run_bdc .
  PERFORM  disp_err .

*&---------------------------------------------------------------------*
*&      Form  I_UPLOAD
*&---------------------------------------------------------------------*
FORM F_UPLOAD.

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
    DATA_TAB                      =  I_UPLOAD
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

*  CALL FUNCTION 'I_UPLOAD'
*       EXPORTING
*            filename                = 'C:\IA42.TXT'
*            filetype                = 'DAT'
*       TABLES
*            data_tab                = I_UPLOAD
*       EXCEPTIONS
*            conversion_error        = 1
*            invalid_table_width     = 2
*            invalid_type            = 3
*            no_batch                = 4
*            unknown_error           = 5
*            gui_refuse_filetransfer = 6
*            OTHERS                  = 7.
ENDFORM.                    " I_UPLOAD

*&---------------------------------------------------------------------*
*&      Form  check_data
*&---------------------------------------------------------------------*
FORM check_data.
  LOOP AT I_UPLOAD INTO ST_UPLOAD.
**-- CHECK ALREAD PROCESSED
    SELECT SINGLE * FROM  mpos
                    WHERE equnr  EQ  ST_UPLOAD-equnr
                    AND   plnnr  EQ  ST_UPLOAD-plnnr
                    AND   plnal  EQ  ST_UPLOAD-plnal .
    IF sy-subrc EQ 0 .
      MOVE-CORRESPONDING ST_UPLOAD TO err_data .
      err_data-err = 'Strategy Plan already Created' .
      APPEND err_data .
      DELETE I_UPLOAD WHERE mptyp EQ ST_UPLOAD-mptyp
                    AND   strat EQ ST_UPLOAD-strat
                    AND   equnr EQ ST_UPLOAD-equnr
                    AND   plnnr EQ ST_UPLOAD-plnnr
                    AND   plnal EQ ST_UPLOAD-plnal .
    ENDIF .

**-- CHECK PLAN CATEGORY
    SELECT SINGLE * FROM t399w
                    WHERE mptyp EQ ST_UPLOAD-mptyp .
    IF sy-subrc NE 0 .
      MOVE-CORRESPONDING ST_UPLOAD TO err_data .
      err_data-err = 'Invalid Plan Category' .
      APPEND err_data .
      DELETE I_UPLOAD WHERE mptyp EQ ST_UPLOAD-mptyp
                    AND   strat EQ ST_UPLOAD-strat
                    AND   equnr EQ ST_UPLOAD-equnr .
    ENDIF .

**-- CHECK STRATEGY
    SELECT SINGLE * FROM t351
                    WHERE strat EQ ST_UPLOAD-strat .
    IF sy-subrc NE 0 .
      MOVE-CORRESPONDING ST_UPLOAD TO err_data .
      err_data-err = 'Invalid Plan Strategy' .
      APPEND err_data .
      DELETE I_UPLOAD WHERE mptyp EQ ST_UPLOAD-mptyp
                    AND   strat EQ ST_UPLOAD-strat
                    AND   equnr EQ ST_UPLOAD-equnr .
    ENDIF .

**-- CHECK EQUIPMENT (cat,TL group,grp cnt)
    SELECT SINGLE * FROM eapl
                    WHERE equnr EQ ST_UPLOAD-equnr .
    IF sy-subrc NE 0 .
      MOVE-CORRESPONDING ST_UPLOAD TO err_data .
      err_data-err = 'Invalid Equipment No.' .
      APPEND err_data .
      DELETE I_UPLOAD WHERE mptyp EQ ST_UPLOAD-mptyp
                    AND   strat EQ ST_UPLOAD-strat
                    AND   equnr EQ ST_UPLOAD-equnr .
    ELSEIF eapl-plnty  NE  ST_UPLOAD-plnty .
      MOVE-CORRESPONDING ST_UPLOAD TO err_data .
      err_data-err = 'Invalid Task List Type' .
      APPEND err_data .
      DELETE I_UPLOAD WHERE mptyp EQ ST_UPLOAD-mptyp
                    AND   strat EQ ST_UPLOAD-strat
                    AND   equnr EQ ST_UPLOAD-equnr .
    ELSEIF eapl-plnnr  NE  ST_UPLOAD-plnnr .
      UNPACK ST_UPLOAD-plnnr TO ST_UPLOAD-plnnr .
      MOVE-CORRESPONDING ST_UPLOAD TO err_data .
      err_data-err = 'Invalid Task List Group' .
      APPEND err_data .
      DELETE I_UPLOAD WHERE mptyp EQ ST_UPLOAD-mptyp
                    AND   strat EQ ST_UPLOAD-strat
                    AND   equnr EQ ST_UPLOAD-equnr .
    ENDIF .

    SELECT SINGLE * FROM eapl
                    WHERE equnr EQ ST_UPLOAD-equnr
                    AND   plnal EQ ST_UPLOAD-plnal .

    IF sy-subrc NE 0 .
      MOVE-CORRESPONDING ST_UPLOAD TO err_data .
      err_data-err = 'Invalid Group Counter' .
      APPEND err_data .
      DELETE I_UPLOAD WHERE mptyp EQ ST_UPLOAD-mptyp
                    AND   strat EQ ST_UPLOAD-strat
                    AND   equnr EQ ST_UPLOAD-equnr .
    ENDIF .

**-- CHECK ORDER TYPE
    SELECT SINGLE * FROM t350
                    WHERE auart EQ ST_UPLOAD-auart .
    IF sy-subrc NE 0 .
      MOVE-CORRESPONDING ST_UPLOAD TO err_data .
      err_data-err = 'Invalid Order Type' .
      APPEND err_data .
      DELETE I_UPLOAD WHERE mptyp EQ ST_UPLOAD-mptyp
                    AND   strat EQ ST_UPLOAD-strat
                    AND   equnr EQ ST_UPLOAD-equnr .
    ENDIF .

**-- CHECK WORK CENTER
    SELECT SINGLE * FROM   crhd
                    WHERE  arbpl  EQ  ST_UPLOAD-wcent .
    IF sy-subrc NE 0 .
      MOVE-CORRESPONDING ST_UPLOAD TO err_data .
      err_data-err = 'Invalid Work Center' .
      APPEND err_data .
      DELETE I_UPLOAD WHERE mptyp EQ ST_UPLOAD-mptyp
                    AND   strat EQ ST_UPLOAD-strat
                    AND   equnr EQ ST_UPLOAD-equnr .
    ENDIF .
  ENDLOOP .
ENDFORM.                    " check_data

*&---------------------------------------------------------------------*
*&      Form  run_bdc
*&---------------------------------------------------------------------*
FORM run_bdc.
  LOOP AT I_UPLOAD INTO ST_UPLOAD .
    CLEAR : bdcdata[], bdcdata .

    PERFORM bdc_dynpro      USING 'SAPLIWP3'        '0100'.
    PERFORM bdc_field       USING 'BDC_CURSOR'      'RMIPM-MPTYP'.
    PERFORM bdc_field       USING 'BDC_OKCODE'      '/00'.
    PERFORM bdc_field       USING 'RMIPM-MPTYP'     ST_UPLOAD-mptyp .
    PERFORM bdc_field       USING 'RMIPM-WSTRA'     ST_UPLOAD-strat .

    PERFORM bdc_dynpro      USING 'SAPLIWP3'        '0201'.
    PERFORM bdc_field       USING 'BDC_OKCODE'      '/00'.
    PERFORM bdc_field       USING 'RMIPM-WPTXT'     ST_UPLOAD-plntxt .
    PERFORM bdc_field       USING 'RIWO1-EQUNR'     ST_UPLOAD-equnr .
    PERFORM bdc_field       USING 'RMIPM-AUART'     sT_UPLOAD-auart .
    PERFORM bdc_field       USING 'RMIPM-GEWERK'    sT_UPLOAD-wcent .
    PERFORM bdc_field       USING 'RMIPM-PLNTY'     sT_UPLOAD-plnty .
    PERFORM bdc_field       USING 'RMIPM-PLNNR'     sT_UPLOAD-plnnr .
    PERFORM bdc_field       USING 'RMIPM-PLNAL'     sT_UPLOAD-plnal .

    PERFORM bdc_dynpro      USING 'SAPLIWP3'        '0201'.
    PERFORM bdc_field       USING 'BDC_OKCODE'      '=T\02'.

    PERFORM bdc_dynpro      USING 'SAPLIWP3'        '0201'.
    PERFORM bdc_field       USING 'BDC_OKCODE'      '=BU'.
*    PERFORM bdc_field       USING 'BDC_CURSOR'      'RMIPM-STADT'.
    PERFORM bdc_field       USING 'RMIPM-VSPOS'     ST_UPLOAD-vspos .
    PERFORM bdc_field       USING 'RMIPM-HORIZ'     sT_UPLOAD-horiz .
    PERFORM bdc_field       USING 'RMIPM-TOPOS'     ST_UPLOAD-topos .
    PERFORM bdc_field       USING 'RMIPM-ABRHO'     ST_UPLOAD-abrho .
    PERFORM bdc_field       USING 'RMIPM-VSNEG'     ST_UPLOAD-vsneg .
    PERFORM bdc_field       USING 'RMIPM-TONEG'     ST_UPLOAD-toneg .
    PERFORM bdc_field       USING 'RMIPM-SFAKT'     ST_UPLOAD-sfakt .
    PERFORM bdc_field       USING 'RMIPM-FABKL'     sT_UPLOAD-fabkl .
    PERFORM bdc_field       USING 'RMIPM-STADT'     sT_UPLOAD-stadt .

    CALL TRANSACTION 'IP42' USING bdcdata MODE v_mode .
  ENDLOOP .
ENDFORM.                    " run_bdc

*&---------------------------------------------------------------------*
*&      Form  disp_err
*&---------------------------------------------------------------------*
FORM disp_err.
  LOOP AT err_data .
    AT FIRST .
      WRITE : / sy-uline(85) .
      WRITE : / '|' NO-GAP , (6)  'Cat.'    NO-GAP COLOR COL_HEADING ,
                '|' NO-GAP , (6)  'Strat.'  NO-GAP COLOR COL_HEADING ,
                '|' NO-GAP , (18) 'Eq. No.' NO-GAP COLOR COL_HEADING ,
                '|' NO-GAP , (50) 'ERROR'   NO-GAP COLOR COL_NEGATIVE ,
                '|' NO-GAP .
      WRITE : / sy-uline(85) .
    ENDAT .

    WRITE : / '|' NO-GAP , (6) err_data-mptyp NO-GAP COLOR COL_NORMAL ,
              '|' NO-GAP , err_data-strat NO-GAP COLOR COL_NORMAL ,
              '|' NO-GAP , err_data-equnr NO-GAP COLOR COL_NORMAL ,
              '|' NO-GAP , err_data-err   NO-GAP COLOR COL_NORMAL ,
              '|' NO-GAP .

    AT LAST .
      WRITE : / sy-uline(85) .
    ENDAT .
  ENDLOOP .
ENDFORM.                    " disp_err



*R        BDC For Create Strategy Plan (IP42)                                                            35
*SP_BACK          Back Ground                                                                            19
*SP_FORE          Fore Ground                                                                            19
*&---------------------------------------------------------------------*
*&      Form  F_FILL_INFOTEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_FILL_INFOTEXT .
  MOVE: 'X' TO g_lines_tab-hell,
        'X' TO g_lines_tab-topofpage,
        'Format for Upload Data'(005)  TO g_lines_tab-text.
  APPEND g_lines_tab.

  MOVE:  'X' TO g_lines_tab-hell,
          ' ' TO g_lines_tab-topofpage,
          'Field        Type      Width  Dec  Remarks'(006)
          TO g_lines_tab-text.
  APPEND g_lines_tab.
  MOVE ' ' TO g_lines_tab-text.
  APPEND g_lines_tab.


  PERFORM append_fields USING :
'mptyp' 'Char' '2' '' 'Maitntenance Plan Category',
*'     ' '    ' '  ' '' 'all line items for a JV).',
'strat' 'Char' '6' '' 'Strategy',
'plntx' 'Char' '40' '' 'Plan Text',
'equnr' 'Numc' '18' '' 'Equipment',
'auart' 'Char' '4' '' 'Order Type',
'wcent' 'Char' '8' '' 'WORK CENTER',
'plnty' 'Char' '1' '' 'Task list type',
'plnnr' 'Numc' '8' '' 'Task list group',
'plnal' 'Numc' '2' ' ' 'Group counter',
*'     ' '    ' '  ' '' 'foreign currency',
'vspos' 'Char' '3' '' 'SF later confirmation',
'topos' 'Char' '3' '' 'Tolerance (+)',
'vsneg' 'Char' '3' '' 'SF earlier confirmation',
'toneg'  'Char'  '3'  ''  'Tolerance (-)',
'sfakt'  'Char'  '4'  ''  'Cycle modification factor',
'fabkl' 'Char' '2' '' 'Factory calendar',
'horiz' 'Char' '3' '' 'Call horizon',
'abrho' 'Char' '3' '' 'Scheduling period',
'stadt' 'Char' '10' '' 'Start of cycle'.

***kdamle-03012006-end
ENDFORM.                    " F_FILL_INFOTEXT
*&---------------------------------------------------------------------*
*&      Form  F_FILL_IDWN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_FILL_IDWN .
  type-pools : abap.
  DATA : it_details TYPE abap_compdescr_tab,
          wa_comp TYPE abap_compdescr.
  FIELD-SYMBOLS : <G1>,<F1>.
  data : v_text type string.
  data : v_fname type DDOBJNAME.
  DATA : ref_descr TYPE REF TO cl_abap_structdescr.

  ref_descr ?= cl_abap_typedescr=>describe_by_data( st_upload ).
  it_details[] = ref_descr->components[].



  loop at it_details into wa_comp.
    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE wa_comp TO <G1>.
      IF SY-SUBRC NE 0.
        EXIT.
      ELSE.
        case sy-index.
          when 4.
            ASSIGN COMPONENT sy-tabix OF STRUCTURE ST_DWN TO <F1>.
            if sy-subrc ne 0.
              exit.
            endif.
            <f1> = <g1>.

        endcase.

      ENDIF.
    ENDDO.

  endloop.
  APPEND ST_DWN TO IDWN.
  CLEAR  ST_DWN.
  UNASSIGN : <G1>,<F1>.
  loop at idwn into st_dwn.
    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE st_dwn TO <G1>.
      IF SY-SUBRC NE 0.
        EXIT.
      ELSE.
        ASSIGN COMPONENT sy-index OF STRUCTURE wa_DWN TO <F1>.
        if sy-subrc ne 0.
          exit.
        endif.
        v_FNAME = <G1>.
        SELECT SINGLE SCRTEXT_M FROM DD04T INTO V_TEXT
                   WHERE ROLLNAME = V_FNAME
                     and ddlanguage = sy-langu.
        if v_text is initial.
          v_text = <g1>.
        endif.
        <f1> = v_text.

      endif.
    enddo.
  endloop.
  APPEND WA_DWN TO IDWN.
  CLEAR  WA_DWN.
  UNASSIGN : <G1>,<F1>.
*  CLEAR idwn.
*  REFRESH idwn.
*
*  idwn-text1(25) =  'Maintenance plan Category'.
*  idwn-text2(25) =  'Strategy'.
*  idwn-text3(25) =  'Plan Text'.
*  idwn-text4(25) =  'Equipment'.
*  idwn-text5(25) =  'Order Type'.
*  idwn-text6(25) =  'WORK CENTER'.
*  idwn-text7(25) =  'Task list type'.
*  idwn-text8(25) =  'Task list group'.
*  idwn-text9(25) =  'Group counter'.
*  idwn-text10(25) =  'SF later confirmation'.
*  idwn-text11(25) =  'Tolerance (+)'.
*  idwn-text12(25) =  'SF earlier confirmation'.
*  idwn-text13(25) =  'Tolerance (-)'.
*  idwn-text14(25) =  'Cycle modification fact'.
*  idwn-text15(25) =  'Factory calendar'.
*  idwn-text16(25) =  'Call horizon'.
*  idwn-text17(25) =  'Scheduling period'.
*  idwn-text18(25) =  'Start of cycle'.
*  APPEND idwn.
*  clear idwn.
*  idwn-text1(15) =  'mptyp'.
*  idwn-text2(15) =  'strat'.
*  idwn-text3(15) =  'plntx'.
*  idwn-text4(15) =  'equnr'.
*  idwn-text5(15) =  'auart'.
*  idwn-text6(15) =  'wcent'.
*  idwn-text7(15) =  'plnty'.
*  idwn-text8(15) =  'plnnr'.
*  idwn-text9(15) =  'plnal'.
*  idwn-text10(15) =  'vspos'.
*  idwn-text11(15) =  'topos'.
*  idwn-text12(15) =  'vsneg'.
*  idwn-text13(15) =  'toneg'.
*  idwn-text14(15) =  'sfakt'.
*  idwn-text15(15) =  'fabkl'.
*  idwn-text16(15) =  'horiz'.
*  idwn-text17(15) =  'abrho'.
*  idwn-text18(15) =  'stadt'.
*  APPEND idwn.
*  clear idwn.
ENDFORM.                    " F_FILL_IDWN


*&---------------------------------------------------------------------*
*&      Form  APPEND_FIELDS
*&---------------------------------------------------------------------*
FORM append_fields USING field typ width decm rem.

  DATA : text(140).
  text = field.
  text+13(10)  = typ.
  text+24(6)  = width.
  text+30(5)  = decm.
  text+35(80)  = rem.

  MOVE:  ' ' TO g_lines_tab-hell,
         ' ' TO g_lines_tab-topofpage,
         text  TO g_lines_tab-text.
  APPEND g_lines_tab.



ENDFORM.                    " APPEND_FIELDS
