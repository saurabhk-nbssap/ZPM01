*&---------------------------------------------------------------------*
*&  Include           RIPRIF1A
*&---------------------------------------------------------------------*

DATA:   ls_lines          TYPE tline,
        lt_lines          TYPE STANDARD TABLE OF tline,
        ls_lines1         TYPE tline,
        lt_lines1         TYPE STANDARD TABLE OF tline.
CONSTANTS: lc_template(1)      VALUE '>'.

DATA: gv_outputparams     TYPE sfpoutputparams, " Output parameters
      gv_fp_docparams     TYPE sfpdocparams,
      gv_fm_name          TYPE rs38l_fnam,      " Function Name
      gv_interface_type   TYPE fpinterfacetype, " interface name
      gv_w_cx_root        TYPE REF TO cx_root,  " Exception class
      gv_form             TYPE fpwbformname,    " Form name
      gv_fpformoutput     TYPE fpformoutput,    " Form Output
      gv_formroutine      TYPE fpname,          "value for FormRoutine
      gi_err              TYPE i VALUE 0.

* Declaration of structures and Internal table for PDF
DATA: gs_wiprt        TYPE wiprt,                                   "PM internal print work area - for use in SAPSCRIPT print
      gs_viqmel       TYPE viqmel,                                  "Notification Header
      gs_tq80_t       TYPE tq80_t,                                  "Notification type texts
      gs_to24i        TYPE t024i,                                   "Maintenance planner groups
      gs_makt         TYPE makt,                                    "Material Descriptions
      gs_wqmfe        TYPE wqmfe,                                   "Work table for notification item
      gs_qkat_head    TYPE ops_riprat00_qkat_pdf,                   "Strucure for QM inspection catalog online module
      gt_qkat_head    TYPE STANDARD TABLE OF ops_riprat00_qkat_pdf, "Table Type for QM inspection catalog online module
      gs_qkat         TYPE ops_riprat00_qkat_pdf,                   "Strucure for QM inspection catalog online module
      gt_qkat         TYPE STANDARD TABLE OF ops_riprat00_qkat_pdf, "Table Type for QM inspection catalog online module
      gs_qkat_box     TYPE ops_riprat00_qkat_pdf,                   "Strucure for QM inspection catalog online module
      gt_qkat_box     TYPE STANDARD TABLE OF ops_riprat00_qkat_pdf, "Table Type for QM inspection catalog online module
      gs_caufvd1      TYPE caufvd,                                  "Global structure  Order Headers and Items
      gs_riwo1        TYPE riwo1,                                   "Global structure object connection
      gs_afvgd        TYPE afvgd,                                   "Global structure order operation
      gs_rcr011       TYPE rcr01,                                   "Global structure Work Center from Task Lists
      gs_resbd        TYPE material_detail,                         "Global Structure for Material Details
      gt_resbd        TYPE tt_resbd,                                "Global Internal Table for Material Details
      gs_kbedp        TYPE kbedp,                                   "Global structure printing individual capacity commitments
      gt_kbedp        TYPE STANDARD TABLE OF kbedp,
      gs_tline1       TYPE tline,                                   "Global Structure for Long Text
      gt_tline1       TYPE STANDARD TABLE OF tline,                 "Global Internal Table for Long Text
      gs_tline2       TYPE tline,                                   "Global Structure for Long Text
      gt_tline2       TYPE STANDARD TABLE OF tline,                 "Global Internal Table for Long Text
      gt_afvgd        TYPE STANDARD TABLE OF afvgd,                 "Global table for order operation
      gs_ihpad1       TYPE ops_riprct00_ihpad_pdf,                  "Global Structure for Patner Info
      gt_ihpad1       TYPE ops_riprct00_t_ihpad_pdf,                "Global Internal Table for Patner Info
      gs_api_val      TYPE ops_riprct00_api_val_r_pdf,                                   "Global structure of Class Value
      gt_api_val      TYPE STANDARD TABLE OF ops_riprct00_api_val_r_pdf,             "Global Table for Class value
      gs_opr_text     TYPE material_longtext,
      gt_opr_text     TYPE STANDARD TABLE OF material_longtext,     "OPS_TABLE_FOR_MATLONGTXT,
      gs_mat_text     TYPE material_longtext,                       "Global Structure for Material Long text
      gt_mat_text     TYPE STANDARD TABLE OF material_longtext,     "Global Internal Table for Material Long text
      gs_ripw0        TYPE ripw0,
      gt_ripw0        TYPE STANDARD TABLE OF ripw0,
      gs_ihgns        TYPE ihgns,
      gt_ihgns        TYPE STANDARD TABLE OF ihgns,
      gs_rcr01        TYPE ops_table_for_wrkcenter_dtl,                "Global Structure for Workcenter details
      gt_rcr01        TYPE STANDARD TABLE OF ops_table_for_wrkcenter_dtl,"Global Internal Table for Workcenter details
      gs_api_kssk     TYPE api_kssk,
      gt_api_kssk     TYPE STANDARD TABLE OF api_kssk,
      gs_iloa         TYPE iloa,
      gs_t357g        TYPE ops_riprct00_s_v_t357g_pdf,
      gt_t357g        TYPE STANDARD TABLE OF ops_riprct00_s_v_t357g_pdf,
      c_code(4)       TYPE c,
      gs_adrs         TYPE adrs,
      cn_country      TYPE ad_line_s.

DATA : gs_srvtyp_stlvpos  TYPE mmpur_print_srvtyp,
       gt_srvtyp_stlpos   TYPE STANDARD TABLE OF mmpur_print_srvtyp,
       gs_srvtyp_kt       TYPE mmpur_print_srvtyp,
       gt_srvtyp_kt       TYPE STANDARD TABLE OF mmpur_print_srvtyp,
       gs_srvtyp_pn       TYPE mmpur_print_srvtyp,
       gt_srvtyp_pn       TYPE STANDARD TABLE OF mmpur_print_srvtyp,
       gs_srv_time        TYPE mmpur_print_time,
       gt_srv_time        TYPE STANDARD TABLE OF mmpur_print_time,
       gs_srv_text        TYPE mmpur_print_t166p,
       gt_srv_text        TYPE STANDARD TABLE OF mmpur_print_t166p,
       gs_formula_body    TYPE mmpur_print_variablen,
       gt_formula_body    TYPE STANDARD TABLE OF mmpur_print_variablen,
       gs_formulahdrs     TYPE mmpur_print_formel,
       gt_formulahdrs     TYPE STANDARD TABLE OF mmpur_print_formel,
       gs_srv_lines       TYPE mmpur_print_srvline,
       gt_srv_lines       TYPE STANDARD TABLE OF mmpur_print_srvline,
       gs_srhdrs          TYPE mmpur_print_srvhdr,
       gt_srhdrs          TYPE STANDARD TABLE OF mmpur_print_srvhdr,
       gs_service         TYPE mmpur_print_ml_esll,
       gt_service         TYPE STANDARD TABLE OF mmpur_print_ml_esll.


TABLES: essr,              "Erfassungsblatt
        ekpo,              "Einkaufsposition
        ekko,              "Bestellkopf
        t166p.
*- INTERNE TABELLEN ---------------------------------------------------*

*- Tabelle der Positionstexte -----------------------------------------*
DATA: BEGIN OF it166p OCCURS 10.
        INCLUDE STRUCTURE t166p.
DATA: END OF it166p.

DATA: BEGIN OF xt166p OCCURS 10.
        INCLUDE STRUCTURE t166p.
DATA: END OF xt166p.

*- Tabelle der Textheader ---------------------------------------------*
DATA: BEGIN OF xtheadkey,
         tdobject LIKE thead-tdobject,
         tdname LIKE thead-tdname,
         tdid LIKE thead-tdid,
      END OF xtheadkey.

*- Tabelle der Nachrichten alt/neu ------------------------------------*
DATA: BEGIN OF xnast OCCURS 10.
        INCLUDE STRUCTURE nast.
DATA: END OF xnast.

DATA: BEGIN OF ynast OCCURS 10.
        INCLUDE STRUCTURE nast.
DATA: END OF ynast.

*- Hilfsfelder --------------------------------------------------------*
DATA: hadrnr(8),                       "Key TSADR
      elementn(30),                    "Name des Elements
      retco LIKE sy-subrc,             "Returncode Druck
      xdrflg LIKE t166p-drflg.         "Hilfsfeld Textdruck

DATA: gs_affhd TYPE affhd,
      gt_affhd TYPE STANDARD TABLE OF affhd,
      gs_mat_longtext TYPE material_longtext,
      gt_mat_longtext TYPE STANDARD TABLE OF material_longtext,
      gs_prt_longtext TYPE material_longtext,
      gt_prt_longtext TYPE STANDARD TABLE OF material_longtext.

DATA : gs_picklist    TYPE ops_riprmp00_picklist_pdf,
       gt_picklist    TYPE ops_riprmp00_t_tbl_picklst_pdf.

DATA: gt_t357z_t TYPE ops_riprmp00_t_t357z_t_pdf.

DATA: gs_fact_txtcdgr    TYPE wqmfe-txtcdgr,
      gs_t357z_t         TYPE t357z_t,
      gs_t356_t          TYPE t356_t,
      gs_pmpl            TYPE pmpl.


DATA : gs_material TYPE material_detail,
       gt_material TYPE  STANDARD TABLE OF material_detail.

DATA: gs_tline       TYPE tline,                                   "Global Structure for Long Text
      gt_tline       TYPE STANDARD TABLE OF tline,                 "Global Internal Table for Long Text
      gs_tline_for_tbl TYPE ops_str_for_longtxt,
      gt_tline_for_tbl TYPE STANDARD TABLE OF ops_str_for_longtxt,
      gt_tline3       TYPE STANDARD TABLE OF ops_str_for_longtxt,  "Global Internal Table for Long Text
      gs_tline3       TYPE ops_str_for_longtxt,                    "Global Structure for Long Text
      gs_wqmur        TYPE wqmur,                                  "Notification type texts
      gt_wqmur        TYPE STANDARD TABLE OF wqmur,
      gs_wqmsm        TYPE wqmsm,                                  "Notification type texts
      gt_wqmsm        TYPE STANDARD TABLE OF wqmsm,
      gt_wqmfe        TYPE STANDARD TABLE OF wqmfe,
      gt_makt         TYPE STANDARD TABLE OF makt,
      gs_diadr        TYPE ops_address,
      gt_diadr TYPE STANDARD TABLE OF ops_address,
      gs_bsvz         TYPE ops_status_task,
      gt_bsvz TYPE STANDARD TABLE OF ops_status_task,
      gs_ihpad        TYPE ops_ihpad,
      gt_ihpad      TYPE STANDARD TABLE OF ops_ihpad.

*&---------------------------------------------------------------------*
*&      Form  open_form_pdf
*&---------------------------------------------------------------------*
FORM open_form_pdf USING archive_type TYPE clike "Order or Notification type
                     object_id    TYPE clike " Order nr. or Notif.nr
                     alternate_dest TYPE clike.

*... Set the print options
  form_open_flag = yes.                " set flag so close is performed
*... did an alternate printer come form print data ?
  IF alternate_dest <> space.          " normally from operation
    itcpo-tddest           = alternate_dest.  " from Operation
  ELSE.   " stay with standard dest for user from T390_u
    itcpo-tddest           = wworkpaper-tddest.   " Where to print
  ENDIF.

  IF wworkpaper-tdcopies IS INITIAL.
    wworkpaper-tdcopies = 1.
  ENDIF.
  IF wworkpaper-tdarmod  IS INITIAL.
    wworkpaper-tdarmod  = '1'.
  ENDIF.

  itcpo-tdcopies         = wworkpaper-tdcopies.    " copies
  itcpo-tdnewid          = wworkpaper-tdnewid.     " new spool entry
  itcpo-tdimmed          = wworkpaper-tdimmed.     " immediately
  itcpo-tddelete         = wworkpaper-tddelete.    " delete after
  itcpo-tdcover          = wworkpaper-tdcover.     " cover page
  itcpo-tdcovtitle       = wworkpaper-tdcovtitle.  " title for cover
  itcpo-tdreceiver       = wworkpaper-tdreceiver.  " report to ->
  itcpo-tdarmod          = wworkpaper-tdarmod.     " Archive mode
  itcpo-tdtelenum        = wworkpaper-tdtelenum.
  itcpo-tdteleland       = wworkpaper-tdteleland.

  IF NOT wworkpaper-print_lang IS INITIAL.
*... reset the default print_language with a specific language
*... if it was set in t390_u or set on the paper selection screen
    print_language =  wworkpaper-print_lang.
  ELSE.
    print_language =  original_print_language.
  ENDIF.

*... important is to set the program so variables to be used by
*... SAPSCRIPT should be read form the driving abap and not the
*... the original calling program.
  itcpo-tdprogram        = syst-repid. " This abap
*... should a print record be written later.
  IF device = c_screen
  OR device = c_preview.
    dont_log = yes.
  ELSE.
    dont_log = space.
  ENDIF.

*... Perform possible print redirection
  PERFORM printer_redirection.

  IF device = c_preview.               " special print preview option
    itcpo-tdpreview = yes.
    itcpo-tdnoprint = ' '.   " no sneaky printing from SAPSCRIPT
  ELSE.
    itcpo-tdpreview = space.   " make sure preview is off otherwise
  ENDIF.

*$*$ ARCHIVE LINK

  g_toa_dara_tab-function   = c_dara.  "archive function
  g_toa_dara_tab-sap_object = archive_type. "Order or Notification
  g_toa_dara_tab-ar_object  = t390-ar_object.  "arch object
  g_toa_dara_tab-object_id  = object_id.    " order or notif number
  g_toa_dara_tab-mandant = t390-mandt.

  g_arc_params_tab-sap_object  = archive_type. "Order or Notification
  g_arc_params_tab-ar_object   = t390-ar_object.
  g_arc_params_tab-mandant = t390-mandt.
  g_arc_params_tab-report = t390-abapname.
*-> SY fields not available in update task
  g_arc_params_tab-arcuser = sy_uname.
  g_arc_params_tab-datum   = sy_datum.

*... Now determine what device based on destination type.
  PERFORM determine_device USING itcpo dest_device.
  gv_formroutine = t390-abapform.
*******print parameters

  gv_outputparams-device     = dest_device.
  gv_outputparams-nodialog   = 'X'.
  gv_outputparams-preview    = itcpo-tdpreview.
  gv_outputparams-dest       = itcpo-tddest.
  gv_outputparams-reqnew     = itcpo-tdnewid.
  gv_outputparams-reqimm     = itcpo-tdimmed.
  gv_outputparams-reqdel     = itcpo-tddelete.
  gv_outputparams-reqfinal   = itcpo-tdfinal.
  gv_outputparams-senddate   = itcpo-tdsenddate.
  gv_outputparams-sendtime   = itcpo-tdsendtime.
  gv_outputparams-schedule   = itcpo-tdschedule.
  gv_outputparams-copies     = itcpo-tdcopies.
  gv_outputparams-dataset    = itcpo-tddataset.
  gv_outputparams-suffix1    = itcpo-tdsuffix1.
  gv_outputparams-suffix2    = itcpo-tdsuffix2.
  gv_outputparams-covtitle   = itcpo-tdcovtitle.
  gv_outputparams-cover      = itcpo-tdcover.
  gv_outputparams-receiver   = itcpo-tdreceiver.
  gv_outputparams-division   = itcpo-tddivision.
  gv_outputparams-lifetime   = itcpo-tdlifetime.
  gv_outputparams-authority  = itcpo-tdautority.
  gv_outputparams-rqposname  = itcpo-rqposname.
  gv_outputparams-arcmode    = itcpo-tdarmod.
  gv_outputparams-noarmch    = itcpo-tdnoarmch.
  gv_outputparams-title      = itcpo-tdtitle.
  gv_outputparams-nopreview  = itcpo-tdnoprev.
  gv_outputparams-noprint    = itcpo-tdnoprint.

  try.
      get badi gb_riprif01_pdf_paper_print.
      if gb_riprif01_pdf_paper_print is bound.
        call badi gb_riprif01_pdf_paper_print->get_pdf_paper_selected
            receiving
              rv_pdf_paper_selected = gv_pdf_paper_selected.
      endif.
    catch cx_badi.                                  "#EC NO_HANDLER
  endtry.
  if gv_pdf_paper_selected is initial.
    CLEAR gv_outputparams-getpdf.
  else.
    gv_outputparams-getpdf = 'X'.
  endif.

**open form
  CALL FUNCTION 'FP_JOB_OPEN'
    CHANGING
      ie_outputparams = gv_outputparams
    EXCEPTIONS
      cancel          = 1
      usage_error     = 2
      system_error    = 3
      internal_error  = 4
      OTHERS          = 5.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  gv_form = t390-pdf_form.

  TRY.

      CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
        EXPORTING
          i_name           = gv_form
        IMPORTING
          e_funcname       = gv_fm_name
          e_interface_type = gv_interface_type.

    CATCH cx_root INTO gv_w_cx_root.
      gi_err = 1.

  ENDTRY.

ENDFORM.                    "OPEN_FORM_PDF

*&---------------------------------------------------------------------*
*&      Form  POPULATE_DATA_PDF
*&---------------------------------------------------------------------*
FORM populate_data_pdf .
  MOVE-CORRESPONDING caufvd TO gs_picklist.

  gs_picklist-title          = wiprt-title.

  gs_picklist-ktext           =   caufvd-ktext.
  gs_picklist-equnr           =   riwo1-equnr.
  gs_picklist-eqtxt           =   riwo1-eqtxt.
  gs_picklist-bautl           =   riwo1-bautl.
  gs_picklist-bautx           =   riwo1-bautx.
  gs_picklist-tplnr           =   riwo1-tplnr.
  gs_picklist-pltxt           =   riwo1-pltxt.
  gs_picklist-serialnr        =   riwo1-serialnr.
  gs_picklist-matnr           =   riwo1-matnr.
  gs_picklist-matnr_m         =   resbd-matnr.
  gs_picklist-matktx          =   riwo1-matktx.
  gs_picklist-deviceid        =   riwo1-deviceid.
  gs_picklist-vornr           =  afvgd-vornr.
  gs_picklist-uvorn           =  afvgd-uvorn.
  gs_picklist-ltxa1           =  afvgd-ltxa1 .
  gs_picklist-arbpl           =  rcr01-arbpl.
  gs_picklist-werks           =  rcr01-werks.
  gs_picklist-werks_p         =  resbd-werks.
  gs_picklist-ktext_wrk       =  rcr01-ktext.
  gs_picklist-menge            = resbd-menge.
  gs_picklist-einheit          = resbd-einheit.
  gs_picklist-matxt            = resbd-matxt.
  gs_picklist-banfnr           = resbd-banfnr.
  gs_picklist-sttxt            = resbd-sttxt.
  gs_picklist-rsnum            = resbd-rsnum.
  gs_picklist-lgort            = resbd-lgort.
  gs_picklist-ablad            = resbd-ablad.
  gs_picklist-wempf            = resbd-wempf.


ENDFORM.                    " POPULATE_DATA_PDF
*&---------------------------------------------------------------------*
*&      Form  CLOSE_FORM_PDF
*&---------------------------------------------------------------------*
FORM close_form_pdf .
  CALL FUNCTION 'FP_JOB_CLOSE'
    EXCEPTIONS
      usage_error    = 1
      system_error   = 2
      internal_error = 3
      OTHERS         = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " CLOSE_FORM_PDF

*----------------------------------------------------------------------*
*       FORM NEW_PAPER_TEST_PDF.                                           *
*----------------------------------------------------------------------*
*    If the Printer defined in the workcenter of the order operation   *
*    changes or printer redirection is on the we must call open form   *
*    again.                                                            *
*    The variable LAST_PRINTER is controlled globally in report.       *
*    It is only important that it contains an initial value that       *
*    can not possible be a valid destination.                          *
*----------------------------------------------------------------------*
FORM new_paper_test_pdf.                   " do we need to open a new form ?

  DATA: redirection_on,
        crhd_tmp LIKE crhd.

  PERFORM check_redirection USING redirection_on.
*... get the printer of the workcenter
  CALL FUNCTION 'CR_WORKSTATION_READ'
    EXPORTING
      id        = afvgd-arbid
    IMPORTING
      ecrhd     = crhd_tmp
    EXCEPTIONS
      not_found = 1
      OTHERS    = 2.
  afvgd-pdest = crhd_tmp-pdest.
*... test if a new form must be started
  IF afvgd-pdest <> last_printer       " operation destination
  OR redirection_on = yes.
    IF form_open_flag = yes.
      PERFORM close_form_pdf.              " first close existing form
    ENDIF.

    PERFORM set_gv_arc_type_aufk USING caufvd-auart.        "n766146
    PERFORM open_form_pdf  USING gv_arc_type_aufk           "n766146
                               caufvd-aufnr
                               afvgd-pdest. " alternate printer sent
  ENDIF.
  last_printer = afvgd-pdest.          " save last printer
ENDFORM.                    "NEW_PAPER_TEST
**----------------------------------------------------------------------*
**       FORM TITLE_PAGE_PDF                                                *
**----------------------------------------------------------------------*
**       Prints title for every new page                                *
**----------------------------------------------------------------------*
FORM title_page_pdf.
  MOVE wiprt-title TO gs_wiprt-title.
  IF pmpl-copy_nr = '  1'.
    IF wworkpaper-pm_delta_p = 'X'.
      MOVE text-505 TO gs_wiprt-copy_text.
    ELSE.
      MOVE text-502 TO gs_wiprt-copy_text.
    ENDIF.
  ELSE.
    IF wworkpaper-pm_delta_p = 'X'.
      MOVE text-504 TO gs_wiprt-copy_text.
    ELSE.
      MOVE text-503 TO gs_wiprt-copy_text.
    ENDIF.
    MOVE pmpl-copy_nr   TO gs_wiprt-timeunit.
    MOVE pmpl-copy_nr   TO gs_pmpl-copy_nr.
  ENDIF.
ENDFORM.                    "TITLE_PAGE
*
*----------------------------------------------------------------------*
*       FORM TITLE_BLOCK_PDF                                               *
*----------------------------------------------------------------------*
*       Prints title for every new block (material, split, ...)        *
*----------------------------------------------------------------------*
FORM title_block_pdf.
  MOVE wiprt-title TO gs_wiprt-title.
  IF pmpl-copy_nr = '  1'.
    IF wworkpaper-pm_delta_p = 'X'.
      MOVE text-505 TO gs_wiprt-copy_text.
    ELSE.
      MOVE text-502 TO gs_wiprt-copy_text.
    ENDIF.
  ELSE.
    IF wworkpaper-pm_delta_p = 'X'.
      MOVE text-504 TO gs_wiprt-copy_text.
    ELSE.
      MOVE text-503 TO gs_wiprt-copy_text.
    ENDIF.
    MOVE pmpl-copy_nr TO gs_wiprt-timeunit.
  ENDIF.
ENDFORM.                    "TITLE_BLOCK
**----------------------------------------------------------------------*
**       FORM NOTIFICATION_HEADER_PDF.                                      *
**----------------------------------------------------------------------*
**       Print the Notification header. Read texts, Issue Element       *
**----------------------------------------------------------------------*
FORM notification_header_pdf.
  DATA save_riwo1 LIKE riwo1.

  CLEAR save_riwo1.
*-> get RIWO1 of notif (could be necessary within order printing)
  IF riwo1-objnr <> viqmel-objnr.
    save_riwo1 = riwo1.
    READ TABLE iriwo1 WITH KEY objnr = viqmel-objnr.
    riwo1 = iriwo1.
  ENDIF.
*... Print the Notification Header Window, short version.
  CLEAR : gs_viqmel,gs_tq80_t.

  MOVE-CORRESPONDING viqmel TO gs_viqmel.
  MOVE-CORRESPONDING t024i TO gs_to24i.
  MOVE : tq80_t-qmartx TO gs_tq80_t-qmartx.
  MOVE : t356_t-priokx TO gs_t356_t-priokx.
  MOVE : fact_txtcdgr  TO gs_fact_txtcdgr.

*-> restore RIWO1 if saved above
  IF NOT save_riwo1 IS INITIAL.
    riwo1 = save_riwo1.
  ENDIF.
*-> print longtext

  PERFORM print_notification_longtxt_pdf.
  gt_tline1[] = lt_lines1[].
ENDFORM.                    "NOTIFICATION_HEADER_PDF
*&---------------------------------------------------------------------*
*&      Form  PRINT_LONGTEXT_PDF
*&---------------------------------------------------------------------*
FORM print_longtext_pdf USING object          TYPE clike
                          object_nr       TYPE clike
                          spras           TYPE clike
                          txid            TYPE clike
                          window          TYPE clike
                          start_line_nr   TYPE numeric
                          last_line_nr    TYPE numeric
                          underline_flag  TYPE clike.


  DATA lv_subrc LIKE sy-subrc.



*... convert parameters to correct size
  text_object  =   object.             " Move to special fields
  text_name    =   object_nr.          " for sizes to call
  text_id      =   txid.               " functions

*... First read the text.
  PERFORM read_text USING  text_id
                           spras
                           text_name
                           text_object
                           rc.

  MOVE rc TO lv_subrc.
  IF lv_subrc NE 0.
* Wenn Druck im Dialog aktiviert ist, und die Meldung noch nicht
* gesichert war, kann der Text über %00000000001 aus dem Memory
* gelesen werden.
*--- T399J lesen
    DATA: w_t399j TYPE t399j.
    SELECT SINGLE * FROM  t399j CLIENT SPECIFIED INTO w_t399j
           WHERE  mandt       = syst-mandt.
    IF sy-subrc EQ 0.
      IF w_t399j-dialog_prt NE space.         " Druck im Dialog?
* Positions- und Ursachennummer folgt nach der Meldungsnummer
        MOVE  '%00000000001' TO text_name(12).
        PERFORM read_text USING  text_id
                                 spras
                                 text_name
                                 text_object
                                 rc.
        MOVE rc TO lv_subrc.
      ENDIF. "Print in dialog
    ENDIF.   "No Entry in t399j
  ENDIF.     "Text not found.
  IF lv_subrc = 0.                   "When text is found
*... with something to print.
*... Remove unwanted text lines from end of table
    LOOP AT table_lines.
      IF syst-tabix > last_line_nr.
        DELETE table_lines.
      ENDIF.
    ENDLOOP.
*... Remove unwanted text lines from start of table
    IF start_line_nr > 1.
      del_lines = start_line_nr - 1.
      DO del_lines TIMES.
        DELETE table_lines INDEX 1.
      ENDDO.
    ENDIF.
    lt_lines[] = table_lines[].

    PERFORM tx_shift_template_lines TABLES lt_lines.

    lt_lines1[] = lt_lines[].
    REFRESH lt_lines.
  ENDIF.                               " text was found

ENDFORM.                    "PRINT_LONGTEXT

*&---------------------------------------------------------------------*
*&      Form  tx_shift_template_lines
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LINES    text
*----------------------------------------------------------------------*
FORM tx_shift_template_lines TABLES p_lines STRUCTURE tline.
  LOOP AT p_lines WHERE tdformat(1) = lc_template.
    p_lines-tdformat = p_lines-tdline(2).
    SHIFT p_lines LEFT BY 2 PLACES.
    MODIFY p_lines.
  ENDLOOP.
ENDFORM.                    "tx_shift_template_lines
*&---------------------------------------------------------------------*
*&      Form  PRINT_NOTIFICATION_LONGTXT_PDF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM print_notification_longtxt_pdf.

*... LONGTEXT for NOTIFICATIOn Header
  IF NOT viqmel-indtx IS INITIAL.      "Is there a long text for Notif
    text_object_name =  'QMEL'.        " temp number
    PERFORM read_text USING ltxt_id    " text for create
                            viqmel-kzmla       " mode text
                            text_object_name   " with temp number
                            c_qmel                          "
                            rc.

*... now build the text name
    IF rc <> 0.
      text_object_name = viqmel-qmnum.
    ENDIF.
*... print the text
    PERFORM print_longtext_pdf USING c_qmel
                                 text_object_name
                                 viqmel-kzmla
                                 ltxt_id
                                 c_main
                                 c_start_line_nr
                                 c_last_line_nr
                                 yes.  "with underline around text
  ENDIF.
ENDFORM.                    "PRINT_NOTIFICATION_LONGTXT_PDF

*&---------------------------------------------------------------------*
*&      Form  PRINT_POSITION_LONGTEXT_PDF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM print_position_longtext_pdf.
*... LONGTEXT for Position
  IF NOT wqmfe-indtx IS INITIAL.       "Is there a long text for POS
    text_object_name =  'QMFE'.        " temp number
    PERFORM read_text USING ltxt_id    " text for create
                            wqmfe-kzmla" mode text
                            text_object_name   " with temp number
                            c_qmel                          "
                            rc.
    IF rc <> 0.
*... now build the text name                 see MIWO0f50
      text_object_name+0(12) = wqmfe-qmnum.
      text_object_name+12(4) = wqmfe-fenum.
      CONDENSE text_object_name NO-GAPS.
    ENDIF.
*... print the text
    PERFORM print_longtext_pdf USING c_qmfe
                                 text_object_name
                                 wqmfe-kzmla
                                 ltxt_id
                                 c_main
                                 c_start_line_nr
                                 c_last_line_nr
                                 yes.  "with underline around text
  ENDIF.
ENDFORM.                    "PRINT_POSITION_LONGTEXT_PDF

*&---------------------------------------------------------------------*
*&      Form  NOTIFICATION_DETAIL_PDF
*&---------------------------------------------------------------------*
*       Notification Details are printed here.                         *
*       Text ELEMENT used    NOTIF_DETAIL                              *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
FORM notification_detail_pdf .

  PERFORM read_wqmfe_tables.           " get text tables for WQMFE
  MOVE-CORRESPONDING wqmfe TO gs_wqmfe.
  MOVE-CORRESPONDING makt  TO gs_makt.
  CLEAR: lt_lines1.
  PERFORM print_position_longtext_pdf. " Print the longtext to posit.
  gt_tline2[] = lt_lines1[].

*For getting the above details in internal table
  LOOP AT lt_lines1 INTO gs_tline.
    gs_tline_for_tbl-tdline   =  gs_tline-tdline.
    gs_tline_for_tbl-fenum =  gs_wqmfe-fenum.
    APPEND gs_tline_for_tbl TO gt_tline_for_tbl.
    CLEAR gs_tline_for_tbl.
  ENDLOOP.
  APPEND gs_wqmfe TO gt_wqmfe.
  APPEND gs_makt  TO gt_makt.

ENDFORM.                    " NOTIFICATION_DETAIL_PDF
*&---------------------------------------------------------------------*
*&      Form  PRINT_TEMPLATE_PDF
*&---------------------------------------------------------------------*
FORM print_template_pdf USING pos_nr LIKE wqmfe-fenum.
  DATA: first_code.                    " flag for first code in a group
*... We have now the complete set of valid activities,  now print
*... the activity template.
  CLEAR iqkat.
  SORT iqkat BY  katalogart codegruppe code.
  LOOP AT iqkat.
    qkat = iqkat.
    AT NEW codegruppe.
*... for each new group write the group title and the top of the
*... Template box.  However, we must first check that the group
*... has codes. If the group does not have codes then dont prepare
*... the top of the first box because no boxes are needed.
      first_code = yes.
      MOVE qkat-codegruppe TO gs_qkat_head-codegruppe.
      MOVE qkat-kurztextgr TO gs_qkat_head-kurztextgr.
      APPEND gs_qkat_head TO gt_qkat_head.
      CLEAR gs_qkat_head.
    ENDAT.

    IF qkat-code <> space.
*... a code line is being processed
      IF first_code = yes.  " first code for the current group
        wiprt-box = c_part_box.        " for top of first box
        first_code = space.
      ENDIF.

      wiprt-box = c_box.               " underneath part of box
      found = space.
      LOOP AT iviqmma WHERE            " is this code already
              qmnum = viqmel-qmnum     " already entered  ?
          AND fenum = pos_nr           " when found, mark
          AND mngrp = qkat-codegruppe  " mark template with
          AND mncod = qkat-code        " with X
          AND mnver = qkat-versioncd
          AND kzloesch IS INITIAL.
        found = yes.                                        "
        wqmma = iviqmma.                                    "
      ENDLOOP.
*... check if the activity has already been registered
      IF found = yes.                  " act. registered
        MOVE qkat-codegruppe TO gs_qkat-codegruppe.
        MOVE qkat-kurztextgr TO gs_qkat-kurztextgr.
        MOVE 'X' TO gs_qkat-box.
        MOVE qkat-code TO gs_qkat-code.
        MOVE qkat-kurztextcd TO gs_qkat-kurztextcd.
        MOVE wqmma-mngfa TO gs_qkat-mngfa.
        APPEND gs_qkat TO gt_qkat.
        CLEAR gs_qkat.
      ELSE.
        MOVE qkat-codegruppe TO gs_qkat-codegruppe.
        MOVE qkat-kurztextgr TO gs_qkat-kurztextgr.
        MOVE qkat-code TO gs_qkat-code.
        MOVE qkat-kurztextcd TO gs_qkat-kurztextcd.
        MOVE wiprt-box TO gs_qkat-mngfa.
        APPEND gs_qkat TO gt_qkat.
        CLEAR gs_qkat.
      ENDIF.
    ENDIF.
  ENDLOOP.


ENDFORM.                    " PRINT_TEMPLATE_PDF
*&---------------------------------------------------------------------*
*&      Form  PRINT_MAT_LONGTEXT_PDF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM print_mat_longtext_pdf .
*... LONGTEXT for Material reservations.
  IF NOT resbd-ltext IS INITIAL.       "Is there a long text for mat .
    CALL FUNCTION 'CO_ZK_TEXTKEY_RESB'
      EXPORTING
        rsnum = resbd-rsnum
        rspos = resbd-rspos
        rsart = resbd-rsart
      IMPORTING
        ltsch = text_object_name.
    stxh-tdname =   text_object_name.
*... print the text
    PERFORM print_longtext_pdf USING tco09-objec        "OBJECT
                                 text_object_name   "NAME
                                 resbd-ltxsp        "LANGUAGE
                                 tco09-idkop        "TYPE   (material)
                                 c_main"which window
                                 c_start_line_nr
                                 c_last_line_nr
                                 yes.  "with underline around text
  ENDIF.


ENDFORM.                    " PRINT_MAT_LONGTEXT_PDF


** Start of PDF Coding C5076209,28/06/06
*----------------------------------------------------------------------*
*       FORM ORDER_HEADER_SHORT                                        *
*----------------------------------------------------------------------*
*       Process order header details, PDF        element               *
*----------------------------------------------------------------------*
FORM order_header_short_pdf.
  DATA save_riwo1 LIKE riwo1.

  CLEAR save_riwo1.
*-> get RIWO1 of order (could be necessary within notif printing)
  IF riwo1-objnr <> caufvd-objnr.
    save_riwo1 = riwo1.
    READ TABLE iriwo1 WITH KEY objnr = caufvd-objnr.
    riwo1 = iriwo1.
  ENDIF.
  MOVE-CORRESPONDING caufvd TO gs_caufvd1.
  MOVE-CORRESPONDING riwo1 TO gs_riwo1.
*-> restore RIWO1 if saved above
  IF NOT save_riwo1 IS INITIAL.
    riwo1 = save_riwo1.
  ENDIF.


*-> print longtext
  PERFORM print_order_longtext_pdf.
  LOOP AT lt_lines1 INTO ls_lines1.
    gs_tline1 = ls_lines1.
    APPEND gs_tline1 TO gt_tline1.
    CLEAR gs_tline1.
  ENDLOOP.
  REFRESH lt_lines1.

ENDFORM.                    "ORDER_HEADER_SHORT_PDF

** End of PDF Coding C5076209,28/06/06

*****************************************************************************
******************** start of PDF Coding C5079349 on 04/07/2006 *************
*****************************************************************************
*&---------------------------------------------------------------------*
*&      Form  ORDER_HEADER_DETAIL_PDF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM order_header_detail_pdf .
  DATA save_riwo1 LIKE riwo1.

  CLEAR save_riwo1.
*-> get RIWO1 of order (could be necessary within notif printing)
  IF riwo1-objnr <> caufvd-objnr.
    save_riwo1 = riwo1.
    READ TABLE iriwo1 WITH KEY objnr = caufvd-objnr.
    riwo1 = iriwo1.
  ENDIF.
  gs_iloa = iloa.
  IF NOT save_riwo1 IS INITIAL.
    riwo1 = save_riwo1.
  ENDIF.

*-> print longtext
  PERFORM print_order_longtext_pdf.

ENDFORM.                    " ORDER_HEADER_DETAIL_PDF


*---------------------------------------------------------------------*
*       FORM PRINT_ORDER_LONGTEXT                                     *
*---------------------------------------------------------------------*
FORM print_order_longtext_pdf.
*... LONGTEXT for Order
  IF NOT caufvd-ltext IS INITIAL.      "Is there a long text for ORDEr
    CALL FUNCTION 'CO_ZK_TEXTKEY_CAUFV'
      EXPORTING
        aufnr = caufvd-aufnr
      IMPORTING
        ltsch = text_object_name.
    stxh-tdname =  text_object_name.
*... print the text
    PERFORM print_longtext_pdf USING tco09-objec        "OBJECT
                                 text_object_name   "NAME
                                 caufvd-ltext       "LANGUAGE
                                 tco09-idord        "TYPE
                                 c_main"which window
                                 c_start_line_nr
                                 c_last_line_nr
                                 yes.  "with underline around text
  ENDIF.
ENDFORM.                    "PRINT_ORDER_LONGTEXT_PDF

*---------------------------------------------------------------------*
*       FORM PERMITS_PDF                                              *
*---------------------------------------------------------------------*
FORM permits_pdf USING object_nr LIKE ihsg-objnr.
*... Permits must be found in IHSG_tab and IHGNS_TAB
*... print all permissions relevant for printing (See indicator)
  LOOP  AT ihsg_tab WHERE objnr = object_nr
                 AND   k_druck = 'X'.  "permission to be printed
    ihsg = ihsg_tab.                   " set DDIC table workarea
    PERFORM read_permit_texts          " v_t357g is set globally
            USING ihsg-pmsog.
*... for all permission check to see if permission has already been
*... granted.  If so, print who granted permission and when and for
*... how long.
    gs_t357g-sogen  =  v_t357g-sogen.
    gs_t357g-gntyp  =  v_t357g-gntyp.
    gs_t357g-gntxt  =  v_t357g-gntxt.
    gs_t357g-gttxt  =  v_t357g-gttxt.
    gs_t357g-counter = ihsg_tab-counter.

    APPEND gs_t357g TO gt_t357g .
    CLEAR gs_t357g .

    LOOP AT ihgns_tab WHERE counter = ihsg_tab-counter
                      AND   geniakt NE 'X'.
*... if they exist and not inactive then they are granted.
      ihgns =  ihgns_tab.
      gs_ihgns = ihgns.
      APPEND gs_ihgns TO gt_ihgns.
      CLEAR gs_ihgns.
    ENDLOOP.
  ENDLOOP.
ENDFORM.                    "PERMITS_PDF

*---------------------------------------------------------------------*
*       FORM PARTNER_DETAILS_PDF                                              *
*---------------------------------------------------------------------*
FORM partner_details_pdf TABLES ihpad_loc STRUCTURE ihpad.
  DATA lv_nrart TYPE nrart.
  DATA: lt_adrs_print       TYPE STANDARD TABLE OF adrs_tab,
         l_adrs_print        TYPE adrs_tab,
         i_adr_data          TYPE fpcompadr,
         l_address_value     TYPE string,
         ls_landx50          TYPE t005t,
         ls_adrswa_in        TYPE adrs,
         index               TYPE i,
         l_address           TYPE char80.

*... print the partner info in window main from IHPAD- record
  LOOP AT ihpad_loc.
    ihpad = ihpad_loc.        " only can pass ddic fields to SAPSCRIPT
*-> read VTEXT in print language
    CALL FUNCTION 'PM_PARTNER_ROLL_TEXT'
      EXPORTING
        parvw    = ihpad-parvw
        language = print_language
      IMPORTING
        vtext    = ihpad-vtext.

    MOVE-CORRESPONDING ihpad TO gs_ihpad1.

    gs_adrs-land1 = gs_ihpad1-country.



    MOVE :  ihpad-name1 TO ls_adrswa_in-name1,
             ihpad-name2 TO ls_adrswa_in-name2,
             ihpad-name3 TO ls_adrswa_in-name3,
             ihpad-name4 TO ls_adrswa_in-name4,
             ihpad-street TO ls_adrswa_in-stras,
             ihpad-city1 TO ls_adrswa_in-ort01,
             ihpad-city2 TO ls_adrswa_in-ort02,
             ihpad-country TO ls_adrswa_in-land1,
             ihpad-po_box TO ls_adrswa_in-pfach,
             ihpad-post_code2 TO ls_adrswa_in-pstl2,
             ihpad-region TO ls_adrswa_in-regio.

    ls_adrswa_in-land1 = gs_ihpad1-country.

* Do a conversion of the partner representation for personal numbers.
* This is done here to impact only the printing of shop papers

    SELECT SINGLE nrart FROM tpar INTO lv_nrart
    WHERE parvw EQ gs_ihpad1-parvw.
    IF sy-subrc EQ 0 AND lv_nrart EQ 'PE'.
* The NAME1,     NAME2,        NAME3 fields hold:
*     last name, Display name, First name
* Map NAME2 to NAME1 and clear the other fields.
      IF ls_adrswa_in-name2 IS NOT INITIAL.
        MOVE ls_adrswa_in-name2 TO ls_adrswa_in-name1.
        CLEAR ls_adrswa_in-name2.
        CLEAR ls_adrswa_in-name3.
      ENDIF.
    ENDIF.

    CALL FUNCTION 'ADDRESS_INTO_PRINTFORM'
      EXPORTING
        adrswa_in               = ls_adrswa_in
        number_of_lines         = 10
      IMPORTING
        address_printform_table = lt_adrs_print.



    DESCRIBE TABLE lt_adrs_print LINES index.

    IF index GT 0.
      READ TABLE lt_adrs_print INTO  l_adrs_print INDEX 1.
      IF NOT  l_adrs_print-line IS INITIAL.
        gs_ihpad1-line1 = l_adrs_print-line.
      ENDIF.
    ENDIF.


    IF index GT 1.
      READ TABLE lt_adrs_print INTO  l_adrs_print INDEX 2.
      IF NOT  l_adrs_print-line IS INITIAL.
        gs_ihpad1-line2 = l_adrs_print-line.
      ENDIF.
    ENDIF.
    IF index GT 2.
      READ TABLE lt_adrs_print INTO  l_adrs_print INDEX 3.
      IF NOT  l_adrs_print-line IS INITIAL.
        gs_ihpad1-line3 = l_adrs_print-line.
      ENDIF.
    ENDIF.
    IF index GT 3.
      READ TABLE lt_adrs_print INTO  l_adrs_print INDEX 4.
      IF NOT  l_adrs_print-line IS INITIAL.
        gs_ihpad1-line4 = l_adrs_print-line.
      ENDIF.
    ENDIF.
    IF index GT 4.
      READ TABLE lt_adrs_print INTO  l_adrs_print INDEX 5.
      IF NOT  l_adrs_print-line IS INITIAL.
        gs_ihpad1-line5 = l_adrs_print-line.
      ENDIF.
    ENDIF.
    IF index GT 5.
      READ TABLE lt_adrs_print INTO  l_adrs_print INDEX 6.
      IF NOT  l_adrs_print-line IS INITIAL.
        gs_ihpad1-line6 = l_adrs_print-line.
      ENDIF.
    ENDIF.
    IF index GT 6.
      READ TABLE lt_adrs_print INTO  l_adrs_print INDEX 7.
      IF NOT  l_adrs_print-line IS INITIAL.
        gs_ihpad1-line7 = l_adrs_print-line.
      ENDIF.
    ENDIF.
    IF index GT 7.
      READ TABLE lt_adrs_print INTO  l_adrs_print INDEX 8.
      IF NOT  l_adrs_print-line IS INITIAL.
        gs_ihpad1-line8 = l_adrs_print-line.
      ENDIF.
    ENDIF.
    IF index GT 8.
      READ TABLE lt_adrs_print INTO  l_adrs_print INDEX 9.
      IF NOT  l_adrs_print-line IS INITIAL.
        gs_ihpad1-line9 = l_adrs_print-line.
      ENDIF.
    ENDIF.

    IF index GT 9.
      READ TABLE lt_adrs_print INTO  l_adrs_print INDEX 10.
      IF NOT  l_adrs_print-line IS INITIAL.
        gs_ihpad1-line10 = l_adrs_print-line.
      ENDIF.
    ENDIF.

    APPEND gs_ihpad1 TO gt_ihpad1.
    CLEAR gs_ihpad1.

  ENDLOOP.
ENDFORM.                    "PARTNER_DETAILS



*----------------------------------------------------------------------*
*       FORM PRINT_OPERATION_TEXT.                                     *
*----------------------------------------------------------------------*
*       Print the long text for the notification Operation.            *
*       It will be printed exactly as found in text file STXH          *
*       inside the current form in window MAIN.                        *
*----------------------------------------------------------------------*
*  -->  AFVGD  Globally for key           must be available            *
*----------------------------------------------------------------------*
FORM print_operation_text_pdf.
*... LONGTEXT for cause
  IF NOT afvgd-txtsp IS INITIAL.       "Is there a long text for OPER.
*... now build the text name
    CALL FUNCTION 'CO_ZK_TEXTKEY_AFVG'
      EXPORTING
        aplzl = afvgd-aplzl
        aufpl = afvgd-aufpl
      IMPORTING
        ltsch = text_object_name.
    stxh-tdname = text_object_name.
*... print the text
    PERFORM print_longtext_opr_pdf USING tco09-objec        "OBJECT
                                 text_object_name   "NAME
                                 afvgd-txtsp        "LANGUAGE
                                 tco09-idpos        "TYPE
                                 c_main"which window
                                 c_start_line_nr    "from line
                                 c_last_line_nr     "to line
                                 yes.  "with underline around text
  ENDIF.
ENDFORM.                    "PRINT_OPERATION_TEXT_PDF


*&---------------------------------------------------------------------*
*&      Form  PRINT_LONGTEXT_OPR_PDF
*&---------------------------------------------------------------------*
FORM print_longtext_opr_pdf USING object          TYPE clike
                          object_nr       TYPE clike
                          spras           TYPE clike
                          txid            TYPE clike
                          window          TYPE clike
                          start_line_nr   TYPE numeric
                          last_line_nr    TYPE numeric
                          underline_flag  TYPE clike.


  DATA lv_subrc LIKE sy-subrc.



*... convert parameters to correct size
  text_object  =   object.             " Move to special fields
  text_name    =   object_nr.          " for sizes to call
  text_id      =   txid.               " functions

*... First read the text.
  PERFORM read_text USING  text_id
                           spras
                           text_name
                           text_object
                           rc.

  MOVE rc TO lv_subrc.
  IF lv_subrc NE 0.
* Wenn Druck im Dialog aktiviert ist, und die Meldung noch nicht
* gesichert war, kann der Text über %00000000001 aus dem Memory
* gelesen werden.
*--- T399J lesen
    DATA: w_t399j TYPE t399j.
    SELECT SINGLE * FROM  t399j CLIENT SPECIFIED INTO w_t399j
           WHERE  mandt       = syst-mandt.
    IF sy-subrc EQ 0.
      IF w_t399j-dialog_prt NE space.         " Druck im Dialog?
* Positions- und Ursachennummer folgt nach der Meldungsnummer
        MOVE  '%00000000001' TO text_name(12).
        PERFORM read_text USING  text_id
                                 spras
                                 text_name
                                 text_object
                                 rc.
        MOVE rc TO lv_subrc.
      ENDIF. "Print in dialog
    ENDIF.   "No Entry in t399j
  ENDIF.     "Text not found.
  IF lv_subrc = 0.                   "When text is found
*... with something to print.
*... Remove unwanted text lines from end of table
    LOOP AT table_lines.
      IF syst-tabix > last_line_nr.
        DELETE table_lines.
      ENDIF.
    ENDLOOP.
*... Remove unwanted text lines from start of table
    IF start_line_nr > 1.
      del_lines = start_line_nr - 1.
      DO del_lines TIMES.
        DELETE table_lines INDEX 1.
      ENDDO.
    ENDIF.
    lt_lines[] = table_lines[].

    PERFORM tx_shift_template_lines TABLES lt_lines.
    LOOP AT lt_lines INTO ls_lines1.
      gs_opr_text-vornr = afvgd-vornr.
      gs_opr_text-uvorn = afvgd-uvorn.
      gs_opr_text-tdline = ls_lines1-tdline.
      APPEND gs_opr_text TO gt_opr_text.
      CLEAR gs_opr_text.
    ENDLOOP.
    REFRESH lt_lines.
  ENDIF.                               " text was found

ENDFORM.                    "PRINT_LONGTEXT


*&---------------------------------------------------------------------*
*&      Form  PRINT_CLASSIFICATION_PDF
*&---------------------------------------------------------------------*
*-- Klassen drucken
FORM print_classification_pdf USING object     TYPE clike
                                object_tab TYPE clike
                                fieldname  TYPE clike.


  DATA: local_object LIKE kssk-objek.
  DATA: local_object_tab LIKE tclt-obtab.
  DATA: local_fieldname LIKE api_ob_key-field.

  DATA: BEGIN OF intklassen OCCURS 0.
          INCLUDE STRUCTURE api_kssk.
  DATA: END OF intklassen.
  DATA: BEGIN OF object_identification OCCURS 0.
          INCLUDE STRUCTURE api_ob_key.
  DATA: END OF object_identification.

  DATA: BEGIN OF charact_values OCCURS 0.
          INCLUDE STRUCTURE api_val_r.
  DATA: END OF charact_values.

  local_object = object.
  CHECK NOT local_object IS INITIAL.
  local_object_tab = object_tab.
  REFRESH intklassen.
  CALL FUNCTION 'CLAP_DDB_GET_CLASSIFICATION'
    EXPORTING
      object                 = local_object
      obtab                  = local_object_tab
      spras                  = print_language
    TABLES
      allocations            = intklassen
    EXCEPTIONS
      no_allocation          = 1
      set_aennr              = 2
      change_nr_not_exist    = 3
      date_in_past           = 4
      error_class            = 5
      error_date_restriction = 6
      error_status           = 7
      OTHERS                 = 8.

  local_fieldname = fieldname.
  REFRESH object_identification.
*... set which type of class info is reguired, eg EQUI, EQUNR
  object_identification-field = local_fieldname.
  object_identification-value = local_object.
  APPEND object_identification.

  LOOP AT intklassen.
    gs_api_kssk = intklassen. "note current class in ddic structure
    APPEND gs_api_kssk TO gt_api_kssk.
    REFRESH  charact_values.
    CALL FUNCTION 'CACL_OBJECT_READ_VALIDATION'
      EXPORTING
        object_type              = local_object_tab
        class_type               = intklassen-klart
        class                    = intklassen-class
        with_unassigned_characts = ' '
        with_inherited_characts  = ' '
        language                 = print_language
      TABLES
        object_identification    = object_identification
        charact_values           = charact_values
      EXCEPTIONS
        error                    = 1
        warning                  = 2
        OTHERS                   = 3.
    LOOP AT charact_values.
      IF charact_values-val_assign = 'X'.
        gs_api_val-klart = gs_api_kssk-klart.
        gs_api_val-class = gs_api_kssk-class.
        gs_api_val-charact = charact_values-charact.
        gs_api_val-value = charact_values-value.
        gs_api_val-val_assign = charact_values-val_assign.

      ELSE.
        gs_api_val-klart = gs_api_kssk-klart.
        gs_api_val-class = gs_api_kssk-class.
        gs_api_val-charact = charact_values-charact.
        gs_api_val-value = charact_values-value.
        gs_api_val-val_assign = charact_values-val_assign.
      ENDIF.
      APPEND gs_api_val TO gt_api_val.
      CLEAR: gs_api_val, gs_api_kssk.

    ENDLOOP.
  ENDLOOP.
ENDFORM.                    "PRINT_CLASSIFICATION_PDF


*----------------------------------------------------------------------*
*       FORM OBJECT_LIST_PDF.                                          *
*----------------------------------------------------------------------*
*       List objects fro the order                                     *
*----------------------------------------------------------------------*
*  -->  GT_RIPW0    Global internal table with RIPW0 records           *
*----------------------------------------------------------------------*
FORM object_list_pdf USING new_page_flag TYPE clike.
  DESCRIBE TABLE iripw0 LINES entries.
  CHECK entries > 0.                   " something to print

  iripw0 = space.

  SORT iripw0 BY sortf obzae.
  LOOP AT iripw0 WHERE  loknz = space. " not deleted objects
    ripw0 = iripw0.                    " set workarea for SAPSCRIPT
    gs_ripw0 = ripw0.
    APPEND gs_ripw0 TO gt_ripw0.
    CLEAR gs_ripw0.
  ENDLOOP.

ENDFORM.                    "OBJECT_LIST_PDF

*&---------------------------------------------------------------------*
*&      Form  SERVICE_PACKAGE_PDF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_AFVGD_PACKNO  text
*      -->P_AFVGD_FLG_FRD  text
*----------------------------------------------------------------------*
FORM service_package_pdf USING packno LIKE afvgd-packno
                               flg_frd LIKE afvgd-flg_frd.

  REFRESH: gliederung, leistung.

  CHECK NOT packno IS INITIAL.



  CALL FUNCTION 'MS_SUBDIVISION_FOR_PRINT'
    EXPORTING
      packno           = packno
      online_data      = 'X'
    TABLES
      gliederung       = gliederung
    EXCEPTIONS
      packno_not_exist = 1
      OTHERS           = 2.

  IF sy-subrc EQ 0.
    LOOP AT gliederung.
      PERFORM print_gliederung_pdf.
      CALL FUNCTION 'MS_SERVICES_FOR_PRINT'
        EXPORTING
          packno            = gliederung-sub_packno
          online_data       = 'X'
        TABLES
          leistung          = leistung
        EXCEPTIONS
          no_services_found = 01.
      PERFORM print_leistung_pdf USING flg_frd.


    ENDLOOP.

  ENDIF.

ENDFORM.                    " SERVICE_PACKAGE_PDF


*&---------------------------------------------------------------------*
*&      Form  PRINT_LEISTUNG_PDF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FLG_FRD  text
*----------------------------------------------------------------------*
FORM print_leistung_pdf USING flg_frd LIKE afvgd-flg_frd.
  DATA: return.

  DATA: ls_services TYPE mmpur_print_ml_esll,
        ls_rm11p TYPE mmpur_print_srvtyp.



  LOOP AT leistung.
    MOVE space TO leistung-extgroup.
    MOVE '0'   TO leistung-rang.
    MOVE leistung TO ml_esll.
    IF flg_frd = '+'.
      CLEAR ml_esll-int_work.
    ENDIF.

    MOVE-CORRESPONDING ml_esll TO gs_service.
    MOVE afvgd-packno TO gs_service-packno.
    APPEND gs_service      TO gt_service.
    PERFORM fill_services_params USING gs_service
                                       ls_rm11p.


    PERFORM texte_pdf USING 'ESLL' leistung-packno leistung-introw 'LLTX'

                        return.
    IF return = 4.
      PERFORM texte_pdf USING 'ASMD' leistung-srvpos ' ' 'LTXT' return.
    ENDIF.
    PERFORM texte_pdf USING 'ESLL' leistung-packno leistung-introw 'LTXT'
                         return.

    PERFORM print_time_pdf.
    PERFORM print_formel_pdf.

    CLEAR ml_esll.

    CLEAR gs_service.



  ENDLOOP.
ENDFORM.                    " PRINT_LEISTUNG_PDF

*&---------------------------------------------------------------------*
*&      Form  TEXTE_PDF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2137   text
*      -->P_LEISTUNG_PACKNO  text
*      -->P_LEISTUNG_INTROW  text
*      -->P_2140   text
*      -->P_RETURN  text
*----------------------------------------------------------------------*
FORM texte_pdf USING  object TYPE clike
                      packno TYPE simple "NUMC
                      introw TYPE simple "NUMC
                      id     TYPE clike
                      return TYPE clike.
  CLEAR thead.
* Textheader lesen
  thead-tdobject  = object.
  thead-tdspras   = print_language.
  thead-tdname    = packno.
  IF object = 'ESLL'.
    thead-tdname+10 = introw.
  ENDIF.
  thead-tdid      = id.

  CALL FUNCTION 'SELECT_TEXT'
    EXPORTING
      id         = thead-tdid
      language   = thead-tdspras
      name       = thead-tdname
      object     = thead-tdobject
    IMPORTING
      entries    = entries
    TABLES
      selections = xthead.
  IF entries NE 0.
    return = 0.
    SORT xthead BY tdid.
* Text lesen
    LOOP AT xthead.
      MOVE-CORRESPONDING xthead TO thead.
      PERFORM lesen_ttxit USING xthead-tdobject xthead-tdid.

      MOVE: thead-tdid     TO gs_srv_text-tdid,
            thead-tdname   TO gs_srv_text-txnam,
            thead-tdobject TO gs_srv_text-tdobject,
            ttxit-tdtext   TO gs_srv_text-tdtext.

      MOVE: ml_esll-introw TO gs_srv_text-introw.
      APPEND gs_srv_text TO gt_srv_text.
      CLEAR gs_srv_text.
      CLEAR sy-subrc.

    ENDLOOP.

  ELSE.
    return = 4.
  ENDIF.

ENDFORM.                    " TEXTE_PDF

*&---------------------------------------------------------------------*
*&      Form  PRINT_FORMEL_PDF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM print_formel_pdf.
  DATA: feld(15).
  FIELD-SYMBOLS: <value> TYPE ml_esll-frmval1.
  CHECK NOT ml_esll-formelnr IS INITIAL.

  CALL FUNCTION 'MS_READ_AND_CHECK_FORMULA'
    EXPORTING
      i_formelnr = ml_esll-formelnr
      no_errors  = 'X'
    IMPORTING
      e_formel   = formel
    TABLES
      variablen  = variablen
    EXCEPTIONS
      OTHERS     = 0.

  MOVE :  formel-formelbez TO gs_formulahdrs-formelbez,
          formel-formel    TO gs_formulahdrs-formel,
          formel-formelnr  TO gs_formulahdrs-formelnr,
          formel-meins     TO gs_formulahdrs-meins,
          ml_esll-introw   TO gs_formulahdrs-introw,
          ml_esll-frmval1  TO gs_formulahdrs-frmval1.

  APPEND  gs_formulahdrs    TO gt_formulahdrs.
  CLEAR gs_formulahdrs.
  MOVE 'ML_ESLL-FRMVAL1' TO feld.
  LOOP AT variablen.
    MOVE sy-tabix TO feld+14(1).
    ASSIGN (feld) TO <value>.
    MOVE <value> TO ml_esll-frmval1.
    MOVE : ml_esll-frmval1      TO gs_formula_body-frmval1,
           variablen-meins      TO gs_formula_body-meins,
           ml_esll-formelnr     TO gs_formula_body-formelnr,
           variablen-varbez     TO gs_formula_body-varbez,
           variablen-value      TO gs_formula_body-value,
           ml_esll-introw       TO gs_formula_body-introw,
           variablen-varno      TO gs_formula_body-varno.

    APPEND gs_formula_body       TO gt_formula_body.
    CLEAR : gs_formula_body.

  ENDLOOP.

ENDFORM.                    " PRINT_FORMEL



*&---------------------------------------------------------------------*
*&      Form  PRINT_GLIEDERUNG_PDF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM print_gliederung_pdf .
  DATA: return.

  DATA: ls_services TYPE mmpur_print_ml_esll,
          ls_rm11p TYPE mmpur_print_srvtyp.

  CHECK gliederung-rang NE 0.
  MOVE gliederung TO ml_esll.
  MOVE-CORRESPONDING ml_esll TO gs_service.
  MOVE ekpo-ebelp TO gs_service-ebelp.
  PERFORM fill_services_params USING gs_service
                                     ls_rm11p.
  MOVE afvgd-packno TO gs_service-packno.

  APPEND gs_service         TO gt_service.
  CLEAR  gs_service.

  PERFORM texte_pdf USING 'ESLL' gliederung-packno gliederung-introw 'LTXT'
                     return.

  CLEAR ml_esll.

ENDFORM.                    " PRINT_GLIEDERUNG_PDF


*&---------------------------------------------------------------------*
*&      Form  TEXTE_JOBT_PDF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PACKNO     text
*      -->INTROW     text
*      -->ID         text
*----------------------------------------------------------------------*
FORM texte_jobt_pdf USING packno introw id.

  DATA: xname LIKE thead-tdname.

  xname  = packno.
  xname+10 = introw.
  CLEAR xtheadkey.

*..lesen Textsteuerungstabelle
  REFRESH xt166p.
  CLEAR xt166p.
  LOOP AT it166p.
    MOVE it166p TO xt166p.
    CASE xt166p-tdobject.
      WHEN 'ASMD'.    "Activity Master
        IF ml_esll-srvpos NE space.
          PERFORM text_select USING 'ASMD' ml_esll-srvpos
                                    xt166p-tdid.
          READ TABLE xthead INDEX 1.
          IF sy-subrc EQ 0.
            xt166p-txnam = ml_esll-srvpos.
            APPEND xt166p.
          ENDIF.
        ENDIF.
      WHEN 'ESLL'.
**.....Text zur Leistung ESLL
        IF xtheadkey-tdobject NE 'ESLL'.
          PERFORM text_select USING 'ESLL'  xname id.
        ENDIF.
        xtheadkey-tdid = xt166p-tdid.
        READ TABLE xthead WITH KEY
                         tdobject      = xtheadkey-tdobject
                         tdname        = xtheadkey-tdname
                   BINARY SEARCH.
        IF sy-subrc EQ 0.
          xt166p-txnam = xname.
          APPEND xt166p.
        ENDIF.
      WHEN OTHERS.
        APPEND xt166p.
    ENDCASE.

  ENDLOOP.
  SORT xt166p BY drflg drpri.

*..kein weiterer Text mit gleicher Reihenfolge erlauben


  xdrflg = '#'.
  LOOP AT xt166p.
    IF xt166p-drflg EQ xdrflg.
      DELETE xt166p.
    ELSE.
      xdrflg = xt166p-drflg.
    ENDIF.
  ENDLOOP.



*  Langtexte ausgeben
  LOOP AT xt166p.
    MOVE xt166p TO t166p.
*..Langtextbezeichnung
    PERFORM lesen_ttxit USING xthead-tdobject xthead-tdid.
    MOVE-CORRESPONDING t166p TO gs_srv_text.
    MOVE: ml_esll-introw TO gs_srv_text-introw.
    APPEND gs_srv_text TO gt_srv_text.
    CLEAR gs_srv_text.
    CLEAR sy-subrc.
  ENDLOOP.

ENDFORM.                    "TEXTE_PDF

*&---------------------------------------------------------------------*
*&      Form  FILL_SERVICES_PARAMS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IS_SERVICES  text
*      -->IS_RM11P     text
*----------------------------------------------------------------------*
FORM fill_services_params  USING  is_services TYPE mmpur_print_ml_esll
                                  is_rm11p    TYPE mmpur_print_srvtyp.

  DATA: lv_rang TYPE string.

  lv_rang = is_services-rang.
  CONDENSE lv_rang.

  IF is_services-extgroup NE ' '.

    MOVE-CORRESPONDING is_services TO gs_srhdrs.

    CASE  lv_rang.
      WHEN '1'.
        gs_srhdrs-prnstr = '*'.
        APPEND  gs_srhdrs TO gt_srhdrs.
      WHEN '2'.
        gs_srhdrs-prnstr = '**'.
        APPEND  gs_srhdrs TO gt_srhdrs.
      WHEN '3'.
        gs_srhdrs-prnstr = '***'.
        APPEND  gs_srhdrs TO gt_srhdrs.
      WHEN '4'.
        gs_srhdrs-prnstr = '****'.
        APPEND  gs_srhdrs TO gt_srhdrs.
      WHEN OTHERS.
    ENDCASE.

  ELSE.

    IF lv_rang EQ '0'.

      MOVE-CORRESPONDING is_services TO gs_srv_lines.
      SHIFT gs_srv_lines-extrow LEFT DELETING LEADING '0'.

      SHIFT is_rm11p-pln_extrow LEFT DELETING LEADING '0'.
      SHIFT is_rm11p-knt_extrow LEFT DELETING LEADING '0'.
      CONDENSE is_services-extsrvno.
      CONDENSE is_services-stlvpos.

      MOVE gs_srv_lines-introw TO gs_srvtyp_pn-introw.

      IF is_rm11p-pln_extrow NE '0'.
        MOVE: is_rm11p-pln_extrow TO gs_srvtyp_pn-pln_extrow,
              is_rm11p-pln_grp    TO gs_srvtyp_pn-pln_grp.
        APPEND gs_srvtyp_pn TO gt_srvtyp_pn .
        CLEAR gs_srvtyp_pn.
      ENDIF.

      IF is_rm11p-knt_extrow NE '0'.
        MOVE gs_srv_lines-introw TO gs_srvtyp_kt-introw.
        SHIFT is_rm11p-knt_ebeln LEFT DELETING LEADING '0'.
        SHIFT is_rm11p-knt_ebelp LEFT DELETING LEADING '0'.

        MOVE: is_rm11p-knt_extrow   TO gs_srvtyp_kt-knt_extrow,
              is_rm11p-knt_group    TO gs_srvtyp_kt-knt_group,
              is_rm11p-knt_ebeln    TO gs_srvtyp_kt-knt_ebeln.

        APPEND gs_srvtyp_kt TO gt_srvtyp_kt .
        CLEAR gs_srvtyp_kt.
      ENDIF.

      IF is_services-extsrvno NE ' '.
        MOVE: is_services-extsrvno TO gs_srv_lines-extsrvno.
      ENDIF.

      IF is_services-stlvpos NE ' '.
        MOVE gs_srv_lines-introw TO gs_srvtyp_stlvpos-introw.
        MOVE: is_services-stlvpos TO gs_srvtyp_stlvpos-stlvpos,
              is_services-ausgb   TO gs_srvtyp_stlvpos-ausgb,
              is_services-lbnum   TO gs_srvtyp_stlvpos-lbnum.

        APPEND gs_srvtyp_stlvpos TO gt_srvtyp_stlpos .
        CLEAR gs_srvtyp_stlvpos.
      ENDIF.

      APPEND  gs_srv_lines TO gt_srv_lines.

    ENDIF.
  ENDIF.

ENDFORM.                    " FILL_SERVICES_PARAMS
*&---------------------------------------------------------------------*
*&      Form  PRINT_TIME_PDF
*&---------------------------------------------------------------------*
FORM print_time_pdf.
  CHECK NOT ml_esll-pernr IS INITIAL OR
        NOT ml_esll-persext IS INITIAL OR
        NOT ml_esll-sdate IS INITIAL OR
        NOT ml_esll-begtime IS INITIAL OR
        NOT ml_esll-endtime IS INITIAL.
  MOVE-CORRESPONDING ml_esll TO gs_srv_time.
  APPEND gs_srv_time      TO gt_srv_time.
  CLEAR gs_srv_time.
ENDFORM.                    " PRINT_TIME


*---------------------------------------------------------------------*
FORM text_select  USING object name id.
*&---------------------------------------------------------------------*
*&      Form  TEXT_SELECT
*&---------------------------------------------------------------------*
*       Text-Header selektieren                                        *
*----------------------------------------------------------------------*

* Textheader lesen
  thead-tdobject  = object.
  thead-tdspras   = ekko-spras.
  thead-tdname    = name.
  thead-tdid      = id.
  MOVE-CORRESPONDING thead TO xtheadkey.

  CALL FUNCTION 'SELECT_TEXT'
    EXPORTING
      id         = thead-tdid
      language   = thead-tdspras
      name       = thead-tdname
      object     = thead-tdobject
    IMPORTING
      entries    = entries
    TABLES
      selections = xthead.
  SORT xthead BY tdid.

ENDFORM.                    "TEXT_SELECT


*&---------------------------------------------------------------------*
*&      Form  PRINT_MAT_LONGTEXT1_PDF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
FORM print_mat_longtext1_pdf .
*... LONGTEXT for Material reservations.
  IF NOT resbd-ltext IS INITIAL.       "Is there a long text for mat .
    CALL FUNCTION 'CO_ZK_TEXTKEY_RESB'
      EXPORTING
        rsnum = resbd-rsnum
        rspos = resbd-rspos
        rsart = resbd-rsart
      IMPORTING
        ltsch = text_object_name.
    stxh-tdname =   text_object_name.
*... print the text
    PERFORM print_longtext_mat_pdf USING tco09-objec        "OBJECT
                                 text_object_name   "NAME
                                 resbd-ltxsp        "LANGUAGE
                                 tco09-idkop        "TYPE   (material)
                                 c_main"which window
                                 c_start_line_nr
                                 c_last_line_nr
                                 yes.  "with underline around text
  ENDIF.


ENDFORM.                    " PRINT_MAT_LONGTEXT1_PDF

*&---------------------------------------------------------------------*
*&      Form  PRINT_LONGTEXT_MAT_PDF
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
FORM print_longtext_mat_pdf USING object          TYPE clike
                          object_nr       TYPE clike
                          spras           TYPE clike
                          txid            TYPE clike
                          window          TYPE clike
                          start_line_nr   TYPE numeric
                          last_line_nr    TYPE numeric
                          underline_flag  TYPE clike.

  DATA lv_subrc LIKE sy-subrc.



*... convert parameters to correct size
  text_object  =   object.             " Move to special fields
  text_name    =   object_nr.          " for sizes to call
  text_id      =   txid.               " functions

*... First read the text.
  PERFORM read_text USING  text_id
                           spras
                           text_name
                           text_object
                           rc.

  MOVE rc TO lv_subrc.
  IF lv_subrc NE 0.
* Wenn Druck im Dialog aktiviert ist, und die Meldung noch nicht
* gesichert war, kann der Text über %00000000001 aus dem Memory
* gelesen werden.
*--- T399J lesen
    DATA: w_t399j TYPE t399j.
    SELECT SINGLE * FROM  t399j CLIENT SPECIFIED INTO w_t399j
           WHERE  mandt       = syst-mandt.
    IF sy-subrc EQ 0.
      IF w_t399j-dialog_prt NE space.         " Druck im Dialog?
* Positions- und Ursachennummer folgt nach der Meldungsnummer
        MOVE  '%00000000001' TO text_name(12).
        PERFORM read_text USING  text_id
                                 spras
                                 text_name
                                 text_object
                                 rc.
        MOVE rc TO lv_subrc.
      ENDIF. "Print in dialog
    ENDIF.   "No Entry in t399j
  ENDIF.     "Text not found.
  IF lv_subrc = 0.                   "When text is found
*... with something to print.
*... Remove unwanted text lines from end of table
    LOOP AT table_lines.
      IF syst-tabix > last_line_nr.
        DELETE table_lines.
      ENDIF.
    ENDLOOP.
*... Remove unwanted text lines from start of table
    IF start_line_nr > 1.
      del_lines = start_line_nr - 1.
      DO del_lines TIMES.
        DELETE table_lines INDEX 1.
      ENDDO.
    ENDIF.
    lt_lines[] = table_lines[].

    PERFORM tx_shift_template_lines TABLES lt_lines.
    LOOP AT lt_lines INTO ls_lines1.
      gs_mat_text-vornr = afvgd-vornr.
      gs_mat_text-uvorn = afvgd-uvorn.
      gs_mat_text-matnr = resbd-matnr.
      gs_mat_text-rspos = resbd-rspos.
      gs_mat_text-tdline = ls_lines1-tdline.
      APPEND gs_mat_text TO gt_mat_text.
      CLEAR gs_mat_text.
    ENDLOOP.
    REFRESH lt_lines.
  ENDIF.                               " text was found

ENDFORM.                    " PRINT_LONGTEXT_MAT_PDF

*----------------------------------------------------------------------*
*       FORM NOTIFICATION_HEADER_DETAIL_PDF                                *
*----------------------------------------------------------------------*
*       Print the Notification header. Read texts, Issue Element       *
*----------------------------------------------------------------------*
FORM notification_header_detail_pdf.
  DATA save_riwo1 LIKE riwo1.

  CLEAR save_riwo1.
*-> get RIWO1 of notif (could be necessary within order printing)
  IF riwo1-objnr <> viqmel-objnr.
    save_riwo1 = riwo1.
    READ TABLE iriwo1 WITH KEY objnr = viqmel-objnr.
    riwo1 = iriwo1.
  ENDIF.

  MOVE-CORRESPONDING viqmel TO gs_viqmel.
  MOVE : tq80_t-qmartx TO gs_tq80_t-qmartx.
  MOVE-CORRESPONDING t024i TO gs_to24i.
  MOVE : t356_t-priokx TO gs_t356_t-priokx.
  MOVE : fact_txtcdgr  TO gs_fact_txtcdgr.
  IF NOT save_riwo1 IS INITIAL.
    riwo1 = save_riwo1.
  ENDIF.
*-> print longtext
  PERFORM print_notification_longtxt_pdf.
  gt_tline1[] = lt_lines1[].
ENDFORM.                    "NOTIFICATION_HEADER_DETAIL_PDF

*&---------------------------------------------------------------------*
*&      Form  READ_EQUIPMENT_PARTNER_PDF
*&---------------------------------------------------------------------*
FORM tech_object_partner_pdf USING equipment_nr LIKE viqmel-equnr
                               functional_location LIKE riwo1-tplnr.
  DATA: ihpad_cop LIKE ihpad OCCURS   0 WITH HEADER LINE.

  IF  NOT equipment_nr IS INITIAL.
    PERFORM get_equipment_data USING equipment_nr.
    PERFORM get_partner_for_object TABLES ihpad_cop
                                   USING equi-objnr.
*-> deletes double partners
    PERFORM only_new_partners TABLES ihpad_cop.
    PERFORM partner_details_pdf  TABLES ihpad_cop.              "
  ELSE.
    IF NOT functional_location IS INITIAL.
      PERFORM get_func_loc_data USING functional_location.
      PERFORM get_partner_for_object TABLES ihpad_cop
                                   USING iflo-objnr.
*-> deletes double partners
      PERFORM only_new_partners TABLES ihpad_cop.
      PERFORM partner_details_pdf TABLES  ihpad_cop.            "
    ENDIF.
  ENDIF.
ENDFORM.           "READ_EQUIPMENT_PARTNER_PDF




*&---------------------------------------------------------------------*
*&      Form  TASK_PDF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM task_pdf  USING qmnum LIKE  wqmfe-qmnum
                 fenum LIKE  wqmfe-fenum.

  LOOP AT iviqmsm WHERE
           qmnum = qmnum               " current Notif number
       AND fenum = fenum.              " current position number

    wqmsm = iviqmsm.                   " Set workarea for SAPSCRIPT
    MOVE-CORRESPONDING wqmsm TO gs_wqmsm.

    APPEND:  gs_wqmsm         TO    gt_wqmsm.
    CALL FUNCTION 'PM_PARTNER_READ'
      EXPORTING
        parvw                = wqmsm-parvw
        parnr                = wqmsm-parnr
      IMPORTING
        diadr_wa             = diadr
      EXCEPTIONS
        no_valid_parnr       = 1
        no_valid_parnr_today = 2
        no_authority         = 3
        OTHERS               = 4.
    MOVE-CORRESPONDING diadr TO gs_diadr.
    MOVE fenum TO gs_diadr-fenum.
    APPEND gs_diadr TO gt_diadr.

    PERFORM read_status USING wqmsm-objnr
                              bsvz-stext.
    MOVE-CORRESPONDING bsvz TO gs_bsvz.
    MOVE fenum TO gs_bsvz-fenum.
    APPEND gs_bsvz TO gt_bsvz.

  ENDLOOP.
ENDFORM.                    "TASK_PDF

*&---------------------------------------------------------------------*
*&      Form  CAUSE_PDF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM cause_pdf USING p_qmnum LIKE wqmfe-qmnum
                 p_fenum LIKE wqmfe-fenum.
  LOOP AT iviqmur WHERE
           qmnum = p_qmnum             " current Notif number
       AND fenum = p_fenum.            " current position number
    wqmur = iviqmur.                   " Set workarea for SAPSCRIPT
    MOVE-CORRESPONDING wqmur TO gs_wqmur.

    APPEND:  gs_wqmur         TO    gt_wqmur.
    CLEAR: lt_lines1.
    PERFORM print_cause_longtext_pdf.

    LOOP AT lt_lines1 INTO gs_tline.
      gs_tline3-tdline   =  gs_tline-tdline.
      gs_tline3-qmnum =  gs_wqmur-qmnum.
      gs_tline3-fenum =  gs_wqmur-fenum.
      gs_tline3-kzloesch =  gs_wqmur-kzloesch.
      gs_tline3-urnum = gs_wqmur-urnum.
      APPEND gs_tline3 TO gt_tline3.
      CLEAR gs_tline3.
    ENDLOOP.
    REFRESH gt_tline.
    REFRESH lt_lines1.
    CLEAR:   gs_wqmur.
  ENDLOOP.

ENDFORM.                               " CAUSE_PDF

*&---------------------------------------------------------------------*
*&      Form  PRINT_CAUSE_LONGTEXT_PDF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM print_cause_longtext_pdf.

*... LONGTEXT for cause
  IF NOT wqmur-indtx IS INITIAL.       "Is there a long text for cause
    text_object_name =  'QMUR'.        " temp number
    PERFORM read_text USING ltxt_id    " text for create
                            wqmur-kzmla" mode text
                            text_object_name   " with temp number
                            c_qmel                          "
                            rc.

*... now build the text name                 see MIWO0f50
    IF rc <> 0.
      text_object_name+0(12) = wqmur-qmnum.
      text_object_name+12(4) = wqmur-fenum.
      text_object_name+16(4) = wqmur-urnum.
      CONDENSE text_object_name NO-GAPS.
    ENDIF.
*... print the text
    PERFORM print_longtext_pdf USING c_qmur
                                 text_object_name
                                 wqmur-kzmla
                                 ltxt_id
                                 c_main
                                 c_start_line_nr
                                 c_last_line_nr
                                 yes.  "with underline around text
    CLEAR text_object_name.
  ENDIF.
ENDFORM.                    "PRINT_CAUSE_LONGTEXT


*----------------------------------------------------------------------*
*       FORM  PRINT_PRT_LONGTEXT_PDF.                                      *
*----------------------------------------------------------------------*
*       Print the longtext for a Production resource or Tool (PRT)     *
*----------------------------------------------------------------------*
*  -->  AFFHD     PRT Dialog table must be available globally          *
*----------------------------------------------------------------------*
FORM print_prt_longtext_pdf.
*... LONGTEXT for PRT.
  IF NOT affhd-txtkz IS INITIAL.       "Is there a long text for PRT .
    CALL FUNCTION 'CO_ZK_TEXTKEY_AFFH'
      EXPORTING
        aufpl = affhd-aufpl
        mandt = syst-mandt
        pzlfh = affhd-pzlfh
      IMPORTING
        ltsch = text_object_name.
    stxh-tdname =   text_object_name.
*... print the text
    PERFORM print_longtext_pdf USING tco09-objec        "OBJECT
                                 text_object_name   "NAME
                                 affhd-txtsp        "LANGUAGE
                                 tco09-idfhm        "TYPE
                                 c_main"which window
                                 c_start_line_nr
                                 c_last_line_nr
                                 yes.  "with underline around text
  ENDIF.

ENDFORM.                    " PRINT_PRT_LONGTEXT_PDF
