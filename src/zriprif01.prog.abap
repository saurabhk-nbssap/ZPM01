***INCLUDE RIPRIF01 .
INCLUDE liprtf03.                      " common routines included
*$*$ General Routines used inside print ABAPS RIPRxxnn ----------------
*----------------------------------------------------------------------*
*       FORM UNDER_line                                                *
*----------------------------------------------------------------------*
*       Print an underline to help separate texts                      *
*----------------------------------------------------------------------*
FORM under_line.
  WRITE c_underscore_str  TO wiprt-colhd.    " Underline
  CALL FUNCTION 'WRITE_FORM'
    EXPORTING                       " write an underline
      element = 'UNDERLINE'
      window  = 'MAIN'.
ENDFORM.                    "UNDER_LINE



*----------------------------------------------------------------------*
*       FORM DATA_IMPORT.                                              *
*----------------------------------------------------------------------*
*       Import data to be used in Print ABAPS RIPRxxnn                 *
*----------------------------------------------------------------------*
*  -->  p1  Export made in LIPRTF02, Function                          *
*           PM_NOTIFICATION_PRINT_CONTROL                              *
*----------------------------------------------------------------------*
FORM data_import.
  DATA h_t024i LIKE t024i.

  PERFORM notification_data_import
    TABLES
         iviqmfe                       " ... positions
         iviqmma                       " ... codes
         iviqmsm                       " ... codes
         iviqmur                       " ... codes
         iqkat                         " ... Catalogue
         ihpad_tab                     " ... partner table
    CHANGING
         iviqmel                       " ... Notification
         riwo1                         " ... Texts from Objects
         riwo00                 " ... Dialog structure with status info
         rqm00.


  viqmel = iviqmel.         " Both workareas set. View needed in
  "      SAPSCRIPT
*-> clear header line to avoid error, if print redir. is called
*   before loop at iviqmfe
  CLEAR wqmfe.

  PERFORM general_data_import.
  IF original_print_language <> print_language.
    PERFORM read_object_texts USING riwo1 print_language.
  ENDIF.
*... special fields already used in forms are supported
*... The following lines are for upwards compatibilty defined
*... The fileds can be addressed directly via RIWO1- now
  makt-maktx =  riwo1-bautx.
  eqkt-eqktx =  riwo1-eqtxt.
  iflo-pltxt =  riwo1-pltxt.

  REFRESH notif_ihpad_tab.
*... move the passed notification partner details to special table
*... The  IHPAD_TAB is used generally for printing partner addresses
*... Not just Notifcation partner but also equipment partner.
  LOOP AT ihpad_tab.
    notif_ihpad_tab = ihpad_tab.
    APPEND notif_ihpad_tab.
  ENDLOOP.
  PERFORM read_catalogue_tables        " READ Quality control tables
          USING viqmel-qmart.          "     for catalog categories
  PERFORM read_catalogue_texts.
  IF NOT viqmel-aufnr IS INITIAL. " header order exists
*-> save RIWO1 from notif in WRIWO1
    REFRESH iriwo1.
    iriwo1 = riwo1. " from notif
    APPEND iriwo1.
*-> DO the same like FORM order_data_import in RIPRIF02
*... get the order data from memory, not there ABEND !!!!!!
    PERFORM only_order_data_import     " see  LIPRTF03.
      TABLES
       op_print_tab
       kbedp_tab
       ihpad_tab
       ihsg_tab
       ihgns_tab
       iafvgd
       iripw0
       iresbd
       iaffhd
      CHANGING
       caufvd
       riwo1
       iloa.
    IF syst-subrc <> 0.
      MESSAGE a650(id).                  " import failed
    ENDIF.
    IF original_print_language <> print_language.
      PERFORM read_object_texts USING riwo1 print_language.
    ENDIF.
*-> save RIWO1 from order in WRIWO1 as well
    iriwo1 = riwo1. " from order
    APPEND iriwo1.
*-> restore RIWO1 from notif
    READ TABLE iriwo1 WITH KEY objnr = viqmel-objnr.
    riwo1 = iriwo1.

*-> clear header lines to avoid error, if print redir. is called
*   before loop at iafvgd or iresbd
    CLEAR afvgd.
    CLEAR resbd.
*-> read CAUFVD-INNAM because it is sometimes not filled
    IF NOT caufvd-ingpr IS INITIAL AND
       caufvd-innam IS INITIAL.
*-> read text
      CALL FUNCTION 'T024I_READ'
        EXPORTING
          ingrp    = caufvd-ingpr
          iwerk    = caufvd-iwerk
        IMPORTING
          struct   = h_t024i
        EXCEPTIONS
          no_entry = 1
          OTHERS   = 2.
      IF sy-subrc = 0.
        caufvd-innam = h_t024i-innam.
      ENDIF.
    ENDIF.

    DESCRIBE TABLE op_print_tab LINES op_entries.
* PERFORM LEFT_JUSTIFY_MAT USING CAUFVD-BAUTL.
    REFRESH order_ihpad_tab.
*... move the passed notification partner details to special table
*... The  IHPAD_TAB is used generally for printing partner addresses
*... Not just Order partner but also equipment partner.
    LOOP AT ihpad_tab.
      order_ihpad_tab = ihpad_tab.
      APPEND order_ihpad_tab.
    ENDLOOP.
*-> restore ihpad_tab from notif
    REFRESH ihpad_tab.
    LOOP AT notif_ihpad_tab.
      ihpad_tab = notif_ihpad_tab.
      APPEND ihpad_tab.
    ENDLOOP.
  ENDIF.

*---added for company code
  SELECT SINGLE * FROM  T001
   WHERE  BUKRS       = viqmel-bukrs.
ENDFORM.                    "DATA_IMPORT
*----------------------------------------------------------------------*
*       FORM READ_CATALOGUE_TEXTS                                      *
*----------------------------------------------------------------------*
* read the texts of the catalogues with print-language again
*----------------------------------------------------------------------*
FORM read_catalogue_texts.
*-> in order to ensure that also documents in foreign languages
*-> will have the correct catalogue texts
  LOOP AT iviqmfe WHERE prtkz = yes.
*-> wqmfe texts
    CALL FUNCTION 'QPK1_CODE_TEXT'
      EXPORTING
        i_katalogart      = iviqmfe-fekat
        i_codegruppe      = iviqmfe-fegrp
        i_code            = iviqmfe-fecod
        i_sprache         = print_language
      IMPORTING
        e_text            = iviqmfe-txtcdgr
        e_grouptext       = iviqmfe-txtgr
      EXCEPTIONS
        no_match_in_range = 1
        OTHERS            = 2.
    IF sy-subrc <> 0.
      iviqmfe-txtcd   = space.
      iviqmfe-txtgr   = space.
      iviqmfe-txtcdgr = space.
    ELSE.
*-> txtcd is often used instead of txtcdgr
      iviqmfe-txtcd   = iviqmfe-txtcdgr.
    ENDIF.
    CALL FUNCTION 'QPK1_CODE_TEXT'
      EXPORTING
        i_katalogart      = iviqmfe-otkat
        i_codegruppe      = iviqmfe-otgrp
        i_code            = iviqmfe-oteil
        i_sprache         = print_language
      IMPORTING
        e_text            = iviqmfe-txtcdgrot
        e_grouptext       = iviqmfe-txtgrot
      EXCEPTIONS
        no_match_in_range = 1
        OTHERS            = 2.
    IF sy-subrc <> 0.
      iviqmfe-txtcdot   = space.
      iviqmfe-txtgrot   = space.
      iviqmfe-txtcdgrot = space.
    ELSE.
*-> txtcdot is often used instead of txtcdgrot
      iviqmfe-txtcdot  = iviqmfe-txtcdgrot.
    ENDIF.
    MODIFY iviqmfe.
  ENDLOOP.
*-> wqmma texts
  LOOP AT iviqmma.
    CALL FUNCTION 'QPK1_CODE_TEXT'
      EXPORTING
        i_katalogart      = iviqmma-mnkat
        i_codegruppe      = iviqmma-mngrp
        i_code            = iviqmma-mncod
        i_sprache         = print_language
      IMPORTING
        e_text            = iviqmma-txtcdgr
        e_grouptext       = iviqmma-txtgr
      EXCEPTIONS
        no_match_in_range = 1
        OTHERS            = 2.
    IF sy-subrc <> 0.
      iviqmma-txtgr   = space.
      iviqmma-txtcd   = space.
      iviqmma-txtcdgr = space.
    ELSE.
*-> txtcd is often used instead of txtcdgr
      iviqmma-txtcd   = iviqmma-txtcdgr.
    ENDIF.
    MODIFY iviqmma.
  ENDLOOP.
*-> wqmsm texts
  LOOP AT iviqmsm.
    CALL FUNCTION 'QPK1_CODE_TEXT'
      EXPORTING
        i_katalogart      = iviqmsm-mnkat
        i_codegruppe      = iviqmsm-mngrp
        i_code            = iviqmsm-mncod
        i_sprache         = print_language
      IMPORTING
        e_text            = iviqmsm-txtcdgr
        e_grouptext       = iviqmsm-txtgr
      EXCEPTIONS
        no_match_in_range = 1
        OTHERS            = 2.
    IF sy-subrc <> 0.
      iviqmsm-txtgr   = space.
      iviqmsm-txtcd   = space.
      iviqmsm-txtcdgr = space.
    ELSE.
*-> txtcd is often used instead of txtcdgr
      iviqmsm-txtcd   = iviqmsm-txtcdgr.
    ENDIF.
    MODIFY iviqmsm.
  ENDLOOP.
*-> wqmur texts
  LOOP AT iviqmur.
    CALL FUNCTION 'QPK1_CODE_TEXT'
      EXPORTING
        i_katalogart      = iviqmur-urkat
        i_codegruppe      = iviqmur-urgrp
        i_code            = iviqmur-urcod
        i_sprache         = print_language
      IMPORTING
        e_text            = iviqmur-txtcdgr
        e_grouptext       = iviqmur-txtgr
      EXCEPTIONS
        no_match_in_range = 1
        OTHERS            = 2.
    IF sy-subrc <> 0.
      iviqmur-txtgr   = space.
      iviqmur-txtcd   = space.
      iviqmur-txtcdgr = space.
    ELSE.
*-> txtcd is often used instead of txtcdgr
      iviqmur-txtcd   = iviqmur-txtcdgr.
    ENDIF.
    MODIFY iviqmur.
  ENDLOOP.
*-> qkat texts
  LOOP AT iqkat WHERE sprache <> print_language.
    CALL FUNCTION 'QPK1_CODE_TEXT'
      EXPORTING
        i_katalogart      = iqkat-katalogart
        i_codegruppe      = iqkat-codegruppe
        i_code            = iqkat-code
        i_sprache         = print_language
      IMPORTING
        e_text            = iqkat-kurztextcd
        e_grouptext       = iqkat-kurztextgr
      EXCEPTIONS
        no_match_in_range = 1
        OTHERS            = 2.
    IF sy-subrc <> 0.
      iqkat-kurztextcd = space.
      iqkat-kurztextgr = space.
    ENDIF.
    MODIFY iqkat.
  ENDLOOP.
*-> fact text
  IF NOT viqmel-qmkat IS INITIAL.
    CALL FUNCTION 'QPK1_CODE_TEXT'
      EXPORTING
        i_katalogart      = viqmel-qmkat
        i_codegruppe      = viqmel-qmgrp
        i_code            = viqmel-qmcod
        i_sprache         = print_language
      IMPORTING
        e_text            = fact_txtcdgr
        e_grouptext       = fact_txtgr
      EXCEPTIONS
        no_match_in_range = 1
        OTHERS            = 2.
    IF sy-subrc <> 0.
      fact_txtcdgr = space.
      fact_txtgr   = space.
    ENDIF.
  ENDIF.
ENDFORM.                    "READ_CATALOGUE_TEXTS

*---------------------------------------------------------------------*
*       FORM GENERAL_DATA_IMPORT                                      *
*---------------------------------------------------------------------*
FORM general_data_import.
*... the general information used needed regardless of whether
*... Orders or notifications are being printed.
*... This information MUST be supplied. See export in
*... LIPRTF02 form   PRINT_PAPERS
  IMPORT wworkpaper                    " Control information
         t390                          " ...
         device                        " ...
         print_language                " default language
         sy_uname
         sy_datum
         FROM MEMORY ID id_iprt_options.

  IF syst-subrc <> 0.
    MESSAGE a650(id).
  ENDIF.

  original_print_language =  print_language.   "note original setting
*... if the user set a specific langauge for a paper, set print_lang
  IF NOT wworkpaper-print_lang IS INITIAL. "specific lang for paper
*... reset the default print_language with a specific language
*... if it was set in t390_u or set on the paper selection screen
    print_language =  wworkpaper-print_lang.
  ENDIF.
  IF caufvd-ernam IS INITIAL.
    caufvd-ernam = sy_uname.
  ENDIF.
  IF caufvd-erdat IS INITIAL.
    caufvd-erdat = sy_datum.
  ENDIF.
ENDFORM.                    "GENERAL_DATA_IMPORT

*----------------------------------------------------------------------*
*       FORM READ_CATALOGUE_TABLES                                     *
*----------------------------------------------------------------------*
*       Read Tables tq80 and tq8t. The Notif type and category tables  *
*----------------------------------------------------------------------*
*  -->  NOTIF_TYPE    Usually from VIQMEL, the notification type.      *
*                     Database field QMEL-QMART.                  *
*----------------------------------------------------------------------*
FORM  read_catalogue_tables USING notif_type TYPE viqmel-qmart.
*... see structure tq8t   All category types are defined here.

  SELECT SINGLE * FROM  tq80           "notif type
         WHERE  qmart       = notif_type.

  SELECT SINGLE * FROM  tq80_t
         WHERE  spras       = print_language
         AND    qmart       = notif_type.

  SELECT SINGLE * FROM  tq8t           "Notif category
         WHERE  qmtyp       = tq80-qmtyp.
*... the longtext ID variable LTXT_ID must be set based on on the
*... Notification type.    The QM notifications have Longtext id
*... LTQM, PM and Service Notifications have ID LTXT.
  CASE tq8t-qmtyp.
    WHEN c_qm_02.
* QM Notification
      ltxt_id = c_ltqm.
      g_arc_type = c_arc_type_qm.
    WHEN c_pm_01.
* PM Notification
      ltxt_id = c_ltxt.
      g_arc_type = c_arc_type_pm.
    WHEN c_sm_03.
* CS Notification
      ltxt_id = c_ltxt.
      g_arc_type = c_arc_type_sm.
    WHEN c_g0_05.
* General Notification
      ltxt_id = c_ltxt.
      g_arc_type = c_arc_type_g0.
  ENDCASE.
  SELECT SINGLE * FROM  t352b          " Report schema
         WHERE  rbnr        = tq80-rbnr.    " for generic
  " catalogue  reads
  SELECT SINGLE * FROM  tq15t
         WHERE  sprache     = print_language
         AND    katalogart  = tq80-mfkat.
ENDFORM.                    "READ_CATALOGUE_TABLES

*----------------------------------------------------------------------*
*       FORM READ_CODE_TEXT                                            *
*----------------------------------------------------------------------*
*       This form can be used to read the text for any code type.      *
*       If you do not know what Catalogue_type to use see T352B.       *
*       This record is already read and available in program           *
*----------------------------------------------------------------------*
*  -->  CATALOG_TYPE See Tq15                                          *
*  -->  CODE_GROUP   See T352b  Eg VIQMFE-FEGRP                        *
*  -->  CODE         See Any code from file.  Eg VIQMFE-FECOD          *
*  -->  Version                               EG VIQMFE-FEVER          *
*                                                                      *
*  <--  TEXT         The text from QPCT                                *
*----------------------------------------------------------------------*
FORM read_code_text USING catalog_type
                          code_group
                          code
                          version
                 CHANGING text.

  CALL FUNCTION 'QPK1_CODE_TEXT'
       EXPORTING
            i_katalogart      = catalog_type
            i_codegruppe      = code_group
            i_code            = code
            i_sprache         = print_language
       IMPORTING
            e_text            = text
*          e_grouptext       =
       EXCEPTIONS
            no_match_in_range = 1
            OTHERS            = 2.
ENDFORM.                    "READ_CODE_TEXT

*----------------------------------------------------------------------*
*       FORM READ_VIEW_TEXT_TABLES.                                    *
*----------------------------------------------------------------------*
*       " Read tables for VIQMEL                                       *
*----------------------------------------------------------------------*
FORM read_view_text_tables.            " Read tables for VIQMEL

  SELECT SINGLE * FROM  t024i
         WHERE  iwerk       = viqmel-iwerk
         AND    ingrp       = viqmel-ingrp.

  SELECT SINGLE * FROM  t356_t
         WHERE  spras       = print_language
         AND    artpr       = viqmel-artpr
         AND    priok       = viqmel-priok.
  IF NOT print_language IS INITIAL.
*--    Materialtext lesen
    IF NOT viqmel-matnr IS INITIAL.
      PERFORM read_material.
    ENDIF.
    PERFORM read_status USING    viqmel-objnr
                        CHANGING riwo00-sttxt.
  ENDIF.
*-> read text for workcenter with print-language
  CALL FUNCTION 'CR_WORKSTATION_READ'
    EXPORTING
      id        = viqmel-arbpl
      msgty     = 'W'
    IMPORTING
      ecrhd     = crhd
    EXCEPTIONS
      not_found = 01.
  IF sy-subrc = 0.
    riwo00-gewrk = crhd-arbpl.
    IF crhd-objty <> crtx-objty OR
       crhd-objid <> crtx-objid OR
       print_language <> crtx-spras.
      SELECT SINGLE * FROM  crtx
             WHERE  objty       = crhd-objty
             AND    objid       = crhd-objid
             AND    spras       = print_language.
    ENDIF.
    IF sy-subrc = 0.
      riwo00-ktext = crtx-ktext.
    ELSE.
      CLEAR riwo00-ktext.
    ENDIF.
  ENDIF.
ENDFORM.                    "READ_VIEW_TEXT_TABLES

*----------------------------------------------------------------------*
*       FORM  READ_WQMFE_TABLES.                                       *
*----------------------------------------------------------------------*
*       Read text tables for WQMFE Notification position record        *
*----------------------------------------------------------------------*
FORM read_wqmfe_tables.
  SELECT SINGLE * FROM  makt
         WHERE  matnr       = wqmfe-bautl
         AND    spras       = print_language.
  IF syst-subrc <> 0
  OR makt-matnr IS INITIAL.
    CLEAR makt.
  ENDIF.
ENDFORM.                    "READ_WQMFE_TABLES

*----------------------------------------------------------------------*
*       FORM OPEN_FORM                                                 *
*----------------------------------------------------------------------*
*       Set print options and Open form                                *
*  ---> Alternative destination.   From Operation if it is available   *
*       Note that this alternative destination could also be over      *
*       written by printer redirection.                                *
*----------------------------------------------------------------------*
FORM open_form USING archive_type TYPE clike "Order or Notification type
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

  try.
      get badi gb_riprif01_pdf_paper_print.
      if gb_riprif01_pdf_paper_print is bound.
        call badi gb_riprif01_pdf_paper_print->get_pdf_paper_selected
          RECEIVING
            rv_pdf_paper_selected = gv_pdf_paper_selected.
      endif.
    catch cx_badi.                                      "#EC NO_HANDLER
  endtry.
  if gv_pdf_paper_selected is initial.
    CLEAR itcpo-tdgetotf.
  else.
    itcpo-tdgetotf = 'X'.
  endif.

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
    itcpo-tdnoprint = yes.   " no sneaky printing from SAPSCRIPT
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
*... Open the SAPSCRIPT FORM.
  CALL FUNCTION 'OPEN_FORM'
    EXPORTING
      archive_index  = g_toa_dara_tab
      archive_params = g_arc_params_tab
      dialog         = space     " SAPSCRIPT diaglog
      device         = dest_device
      form           = t390-form " the form to print
      language       = print_language                 "
      OPTIONS        = itcpo.

*... Any problems noted in SAPSCRIPT   , no exceptions used here

ENDFORM.                    "OPEN_FORM

*---------------------------------------------------------------------*
*       FORM DETERMINE_DEVICE                                         *
*---------------------------------------------------------------------*
*  -->  ITCPO                                                         *
*  -->  RET_DEVICE                                                    *
*---------------------------------------------------------------------*
FORM determine_device USING itcpo STRUCTURE itcpo
                            ret_device TYPE clike. "ITCPP-TDDEVICE
  CALL FUNCTION 'PM_DETERMINE_OUTPUT_DEVICE'
    EXPORTING
      in_itcpo   = itcpo
    IMPORTING
      out_device = ret_device.
ENDFORM.                    "DETERMINE_DEVICE

*----------------------------------------------------------------------*
*       FORM PRINTER_REDIRECTION                                       *
*----------------------------------------------------------------------*
*       Check the T392 / T392_s Tables for print redirection         *
*       The ITCPO (Print options are approriately set if valid         *
*       redirection records are found.                                 *
*----------------------------------------------------------------------*
*  <->  ITCPO     Print options used in CALL OPEN_FORM                 *
*   --> WWORKPAPER The current workpaper details                       *
*   <-- ITCPO-TDDEST is set globally here plus other  ITCPO vars       *
*----------------------------------------------------------------------*
FORM printer_redirection.
  DATA: on.
  PERFORM check_redirection USING on.
  CHECK on = yes.        " did we find a valid redirection ?
*... that means after check we have a matching value for the right field
*... We can now alter the PRINTER OPTIONS
  itcpo-tddest           = t392_v-tddest.      " Where to print
  itcpo-tdcopies         = t392_v-tdcopies.    " copies
  itcpo-tdnewid          = t392_v-tdnewid.     " new spool entry
  itcpo-tdimmed          = t392_v-tdimmed.     " immediately
  itcpo-tddelete         = t392_v-tddelete.    " delete after
  itcpo-tdcover          = t392_v-tdcover.     " cover page
  itcpo-tdcovtitle       = t392_v-tdcovtitle.  " title for cover
  itcpo-tdreceiver       = t392_v-tdreceiver.  " report to ->
  itcpo-tdprogram        = syst-repid. " driving ABAP
  itcpo-tdarmod          = t392_v-tdarmod.     " Archive mode
  itcpo-tdtelenum        = t392_v-tdtelenum.                "
  itcpo-tdteleland       = t392_v-tdteleland.               "

  IF NOT t392_v-print_lang IS INITIAL.
*... reset the default print_language with a specific language
*... if it was set in t392_V or set on the paper selection screen
    print_language =  t392_v-print_lang.
  ENDIF.
*...
ENDFORM.                    "PRINTER_REDIRECTION


*----------------------------------------------------------------------*
*       FORM CHECK_REDIRECTION                                         *
*----------------------------------------------------------------------*
*       Test for redirection.                                          *
*       If printer redirection is active and a valid value found
*       the T392_V record is set and available globally
*----------------------------------------------------------------------*
*  -->  REDIRECTION_ON   'X' = yes  ' ' = no redirection               *
*  <--  T392_V    Globally                                             *
*----------------------------------------------------------------------*
FORM check_redirection USING redirection_on TYPE clike.

*... Now test for Field Specific Printer redirection
*... T392 has defined for each paper if Redirection is active and
*... when it is active on what field redirection is based.
*... Check to see if paper redirection is active

  SELECT         * FROM t392
         WHERE pm_appl      = wworkpaper-pm_appl
         AND   workpaper    = wworkpaper-workpaper
         AND   opt_active   = 'X'.
    EXIT.           " only one record. array fetch not needed
  ENDSELECT.

  IF syst-dbcnt = 0.          " only go on when redirection is set on
    redirection_on = space.  " in table t392 for this paper type
    EXIT.
  ENDIF.

  "  redirection is active, we found 1
  CLEAR field_known.                 " default  field not in ABAP
  CLEAR plant_field_known.           " plant field in <FIELD_WERK>

  PERFORM special_field_test.

  IF field_known = space.              " not a workcenter so test
    PERFORM test_other_fields.         " other field names
  ENDIF.

  IF field_known = space.              " field NOT  availabe in ABAP
    redirection_on = space.            " field can not be used in ABAP
    EXIT.
  ENDIF.

*... so far things are looking,
*... we now have active field for redirection that exists in this ABAP
*... We must now check if the current value in this field is defined
*... in T392_V.  Our last test for redirection.

  SELECT       * FROM t392_v INTO TABLE temp_t392_v
         WHERE pm_appl     = wworkpaper-pm_appl
         AND   workpaper   = wworkpaper-workpaper
         AND   ddic_struc  = t392-ddic_struc
         AND   pm_fld_nam  = t392-pm_fld_nam.

  temp_t392_v = space.
  LOOP AT temp_t392_v.                 " loop on possible fields
    t392_v = temp_t392_v.              " set T392_V
*... compare T392_V value against current field contents
    CHECK <field_cont> CP t392_v-fieldvalue.
*... and if the plant field is set then check it aswell.
    IF  NOT t392_v-werks IS INITIAL AND plant_field_known = yes.
      CHECK <field_werk> CP t392_v-werks.
    ENDIF.
    redirection_on = yes.              " H I T !!!!!!!!!
    EXIT.                " the first match is as good as any
  ENDLOOP.                             " select values from t392_s
ENDFORM.                    "CHECK_REDIRECTION

*---------------------------------------------------------------------*
*       FORM SET_PLANT_FIELD                                          *
*---------------------------------------------------------------------*
FORM set_plant_field.
*... first test if how the Plant field should be decided ?
*... only necessary for some fields but we set it just in case
  CASE field_cont_name.
    WHEN 'VIQMEL-BEBER'.  field_werk_name = 'VIQMEL-SWERK'.
    WHEN 'VIQMEL-INGRP'.  field_werk_name = 'VIQMEL-IWERK'.
    WHEN 'VIQMEL-STORT'.  field_werk_name = 'VIQMEL-SWERK'.
    WHEN 'CAUFVD-APGPR'.  field_werk_name = 'CAUFVD-IWERK'.
    WHEN 'CAUFVD-STORT'.  field_werk_name = 'CAUFVD-WERKS'.
*... catch all plant for other cases
    WHEN OTHERS.  " just assume WERKS, this is most common
      field_werk_name       = t392-ddic_struc.
      field_werk_name+10(1) = '-'.
      field_werk_name+11(10) = 'WERKS'.
      CONDENSE field_werk_name NO-GAPS.
  ENDCASE.
ENDFORM.                    "SET_PLANT_FIELD

*---------------------------------------------------------------------*
*       FORM SPECIAL_FIELD_TEST                                       *
*---------------------------------------------------------------------*
FORM special_field_test.
*... check now for workcenters we can convert internal field
*... to external field plus get the correct plant here
  IF t392-pm_fld_nam = 'PPSID'         " is it a work center
  OR t392-pm_fld_nam = 'GEWRK'         " under one of many names
*..  OR T392-PM_FLD_NAM = 'VAPLZ'   " NO already converted !!!!!!!!!!
  OR t392-pm_fld_nam = 'ARBID'
* Workcenter from notification is different
  OR ( t392-pm_fld_nam = 'ARBPL' AND   "Work center in notification
       t392-ddic_struc = 'VIQMEL' ).

*... convert it from internal workstation to external

    PERFORM build_redirect_field.      " Workstation is a known field

    CALL FUNCTION 'CR_WORKSTATION_READ'
      EXPORTING
        id        = <field_cont>
      IMPORTING
        arbpl     = rcr01-arbpl
        ecrhd     = crhd
        ktext     = rcr01-ktext
        werks     = rcr01-werks
      EXCEPTIONS
        not_found = 01.          " ignore this !!!!
* Unicode: Test is only possible on runtime, therfore no typing
    ASSIGN rcr01-arbpl TO <field_cont>." set to external now
    ASSIGN rcr01-werks TO <field_werk>." set to external now
    plant_field_known = yes.
  ENDIF.
ENDFORM.                    "SPECIAL_FIELD_TEST

*----------------------------------------------------------------------*
*       FORM TEST_OTHER_FIELDS.                                        *
*----------------------------------------------------------------------*
*       Test non Workcenter fields to see if they are LIVE in ABAP     *
*       FIELD_KNOWN set globally is found in abap                      *
*----------------------------------------------------------------------*
FORM test_other_fields.
*.. special check now for workstation fields
  PERFORM build_redirect_field. "Build the field name for <FIELD_CONT>
  PERFORM set_plant_field.             " decide matching plant field
  ASSIGN (field_werk_name) TO <field_werk>.    " assign plant
  IF syst-subrc =  0.
    plant_field_known = yes.
  ELSE.
    plant_field_known = space.
  ENDIF.
ENDFORM.                    "TEST_OTHER_FIELDS

*----------------------------------------------------------------------*
*       FORM BUILD_REDIRECT_FIELD                                      *
*----------------------------------------------------------------------*
*       Build the field name for <FIELD_CONT> and set FIELD_KNOWN flag *
*----------------------------------------------------------------------*
*  <--  p2        FIELD_CONT_NAME    <FIELD_CONT>     GLOBAL           *
*----------------------------------------------------------------------*
FORM build_redirect_field.
*...     Field DDIC_STRUC and PM_FLD_NAM determine the field
*...  build the ABAP fieldname as found in t392
  field_cont_name       = t392-ddic_struc.
  field_cont_name+10(1) = '-'.
  field_cont_name+11(10) = t392-pm_fld_nam.
  CONDENSE field_cont_name NO-GAPS.
*... we can now try and retrieve the contents of the field on which
*... print redirection is based.
*... now test main field, is it in ABAP
  ASSIGN (field_cont_name) TO <field_cont>.    " if field is in abap
  IF syst-subrc = 0.                   " main field in ABAP ?
    field_known = yes.                 " follow the main field
  ELSE.
    field_known = space.
  ENDIF.
ENDFORM.                    "BUILD_REDIRECT_FIELD

*----------------------------------------------------------------------*
*       FORM NEW_PAPER_TEST.                                           *
*----------------------------------------------------------------------*
*    If the Printer defined in the workcenter of the order operation   *
*    changes or printer redirection is on the we must call open form   *
*    again.                                                            *
*    The variable LAST_PRINTER is controlled globally in report.       *
*    It is only important that it contains an initial value that       *
*    can not possible be a valid destination.                          *
*----------------------------------------------------------------------*
FORM new_paper_test.                   " do we need to open a new form ?

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
      PERFORM close_form.              " first close existing form
    ENDIF.

    PERFORM set_gv_arc_type_aufk USING caufvd-auart.        "n766146

* PERFORM OPEN_FORM  USING C_ARC_TYPE_AUFK                     "n766146
    PERFORM open_form  USING gv_arc_type_aufk               "n766146
                               caufvd-aufnr
                               afvgd-pdest. " alternate printer sent
  ENDIF.
  last_printer = afvgd-pdest.          " save last printer
ENDFORM.                    "NEW_PAPER_TEST

*----------------------------------------------------------------------*
*       FORM CLOSE_FORM.                                               *
*----------------------------------------------------------------------*
*       Close current open SAPSCRIPT form.                             *
*----------------------------------------------------------------------*
FORM close_form.
  DATA lt_otfdata type table of ITCOO.
  if gv_pdf_paper_selected is initial.
    CALL FUNCTION 'CLOSE_FORM'
      IMPORTING
        RESULT   = itcpp           " Information can be useful
      EXCEPTIONS                      " eg dest, spool nr etc
        unopened = 1.
  else.
    CALL FUNCTION 'CLOSE_FORM'
      TABLES
        OTFDATA  = lt_otfdata
      EXCEPTIONS
        unopened = 1.
    call badi gb_riprif01_pdf_paper_print->set_sapscript_paper_result
      EXPORTING
        it_otfdata   = lt_otfdata
        iv_form_name = t390_t-papertext.
  endif.

ENDFORM.                    "CLOSE_FORM

*----------------------------------------------------------------------*
*       FORM TITLE.                                                    *
*----------------------------------------------------------------------*
*       Issue a standard title                                         *
*       Assumes TEXT ELEMENT and WINDOW are both called 'TITLE'        *
*----------------------------------------------------------------------*
FORM title.
  PERFORM set_title.
*print the title
*  CALL FUNCTION 'WRITE_FORM'
*    EXPORTING
*      element = 'TITLE'
*      window  = 'TITLE'.         " Constant window in FORM

  CALL FUNCTION 'WRITE_FORM'
    EXPORTING
      element = 'TITLE'
      window  = 'HEADER'.         " Constant window in FORM




ENDFORM.                    "TITLE

*----------------------------------------------------------------------*
*       FORM TITLE_PAGE                                                *
*----------------------------------------------------------------------*
*       Prints title for every new page                                *
*----------------------------------------------------------------------*
FORM title_page.
  CALL FUNCTION 'WRITE_FORM'
    EXPORTING
      element = 'TITLE_PAGE'
      window  = 'MAIN'.
ENDFORM.                    "TITLE_PAGE

*----------------------------------------------------------------------*
*       FORM TITLE_BLOCK                                               *
*----------------------------------------------------------------------*
*       Prints title for every new block (material, split, ...)        *
*----------------------------------------------------------------------*
FORM title_block.
  CALL FUNCTION 'WRITE_FORM'
    EXPORTING
      element = 'TITLE_BLOCK'
      window  = 'MAIN'.
ENDFORM.                    "TITLE_BLOCK

*----------------------------------------------------------------------*
*       FORM SET_TITLE.                                                *
*----------------------------------------------------------------------*
*       WIPRT-TITLE is set from text table .   From  T390_T            *
*       Use in all formulars as the title field                        *
*----------------------------------------------------------------------*
FORM set_title.
  CLEAR t390_t.
  SELECT SINGLE * FROM  t390_t         " Workpaper texts
         WHERE  spras       = print_language
         AND    pm_appl     = t390-pm_appl
         AND    workpaper   = t390-workpaper.
  wiprt-title = t390_t-papertext.      " Set title of form
ENDFORM.                    "SET_TITLE

*----------------------------------------------------------------------*
*       FORM NOTIFICATION_HEADER.                                      *
*----------------------------------------------------------------------*
*       Print the Notification header. Read texts, Issue Element       *
*----------------------------------------------------------------------*
FORM notification_header.
  DATA save_riwo1 LIKE riwo1.

  CLEAR save_riwo1.
*-> get RIWO1 of notif (could be necessary within order printing)
  IF riwo1-objnr <> viqmel-objnr.
    save_riwo1 = riwo1.
    READ TABLE iriwo1 WITH KEY objnr = viqmel-objnr.
    riwo1 = iriwo1.
  ENDIF.
*... Print the Notification Header Window, short version.
  CALL FUNCTION 'WRITE_FORM'
    EXPORTING
      element = 'NOTIF_HEADER_SHORT'
      window  = 'MAIN'.
*-> restore RIWO1 if saved above
  IF NOT save_riwo1 IS INITIAL.
    riwo1 = save_riwo1.
  ENDIF.
*-> print longtext
  PERFORM print_notification_longtext.
ENDFORM.                    "NOTIFICATION_HEADER

*----------------------------------------------------------------------*
*       FORM NOTIFICATION_HEADER_DETAIL                                *
*----------------------------------------------------------------------*
*       Print the Notification header. Read texts, Issue Element       *
*----------------------------------------------------------------------*
FORM notification_header_detail.
  DATA save_riwo1 LIKE riwo1.

  CLEAR save_riwo1.
*-> get RIWO1 of notif (could be necessary within order printing)
  IF riwo1-objnr <> viqmel-objnr.
    save_riwo1 = riwo1.
    READ TABLE iriwo1 WITH KEY objnr = viqmel-objnr.
    riwo1 = iriwo1.
  ENDIF.
*... Print the Notification Header Window, DETAIL version
  CALL FUNCTION 'WRITE_FORM'
    EXPORTING
      element = 'NOTIF_HEADER_DETAIL'
      window  = 'MAIN'.
*-> restore RIWO1 if saved above
  IF NOT save_riwo1 IS INITIAL.
    riwo1 = save_riwo1.
  ENDIF.
*-> print longtext
  PERFORM print_notification_longtext.
ENDFORM.                    "NOTIFICATION_HEADER_DETAIL
*----------------------------------------------------------------------*
*       FORM END_OF_REPORT                                             *
*----------------------------------------------------------------------*
*       Issue Line 'END OF REPORT' to indicate last page.              *
*       So user can be sure he has all pages                           *
*----------------------------------------------------------------------*
FORM end_of_report.

  CALL FUNCTION 'WRITE_FORM'
    EXPORTING
      element = 'END_OF_REPORT'
      window  = 'MAIN'.

ENDFORM.                    "END_OF_REPORT

*----------------------------------------------------------------------*
*       FORM PRINT_LONGTEXT                                            *
*----------------------------------------------------------------------*
*       The longtext will be read, using OBJECT                        *
*                                        OBJECT_NR                     *
*                                        SPRAS                         *
*                                        TXID                          *
*                                                                      *
*       And printed in WINDOW 'WINDOW' in the current FORM.            *
*                                                                      *
*----------------------------------------------------------------------*
*  -->  OBJECT    LONGTEXT OBJECT                                      *
*  -->  OBJECT_NR Key to longtext                                      *
*  -->  SPRAS     Language                                             *
*  -->  TXID      Text id                                              *
*  -->  WINDOW    FORM WINDOW                                          *
*  -->  START_LINE  Print longtext starting line nr                    *
*  -->  END_LINE  Last line number                                     *
*  -->  UNDERLINE_FLAG :   'X' = With Uline before and after text      *
*----------------------------------------------------------------------*
FORM print_longtext USING object          TYPE clike
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
*... PRINT THE TEXT NOW
    PERFORM text_output TABLES table_lines
                         USING  table_header
                                window
                                underline_flag.

  ENDIF.                               " text was found

ENDFORM.                    "PRINT_LONGTEXT

*---------------------------------------------------------------------*
*       FORM TEXT_OUTPUT                                              *
*---------------------------------------------------------------------*
*  -->  TABLE_LINES                                                   *
*  -->  TABLE_HEADER                                                  *
*  -->  WINDOW                                                        *
*  -->  UNDERLINE_FLAG                                                *
*---------------------------------------------------------------------*
FORM text_output TABLES table_lines    STRUCTURE tline
                 USING  table_header   LIKE thead
                        window         TYPE clike
                        underline_flag TYPE clike.

*... to protect longtexts from page breaks use the PROTECT
*... ENDPROTECT command in SAPSCRIPT
  PERFORM sapscript_command USING  'PROTECT'.  "DO NOT TRANSLATE CMD

  IF underline_flag = yes.
    PERFORM under_line.                " underscore line
  ENDIF.

  CALL FUNCTION 'WRITE_FORM_LINES'
    EXPORTING
      header    = table_header
      window    = window
    IMPORTING
      frompage  = frompage       " not interested ?
    TABLES
      lines     = table_lines
    EXCEPTIONS
      function  = 01
      type      = 02
      unopened  = 03
      unstarted = 04
      window    = 05.
*... leave SYST-SUBRC set.  We dont mind if text was bad, nothing
*... we can do about here.

  IF underline_flag = yes.
    PERFORM under_line.                " underscore line
  ENDIF.

*... page break protect off.
  PERFORM sapscript_command USING  'ENDPROTECT'.            "
ENDFORM.                    "TEXT_OUTPUT

*----------------------------------------------------------------------*
*       FORM PRINT_NOTIFICATION_LONTEXT.                               *
*----------------------------------------------------------------------*
*       Print the long text for the notification header.               *
*       It will be printed exactly as found in text file STXH          *
*       inside the current form in window MAIN.                        *
*----------------------------------------------------------------------*
*  -->  p1        VIQMEL    Globally the VIEW must be available        *
*----------------------------------------------------------------------*
FORM print_notification_longtext.

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
    PERFORM print_longtext USING c_qmel
                                 text_object_name
                                 viqmel-kzmla
                                 ltxt_id
                                 c_main
                                 c_start_line_nr
                                 c_last_line_nr
                                 yes.  "with underline around text
  ENDIF.
ENDFORM.                    "PRINT_NOTIFICATION_LONGTEXT


*----------------------------------------------------------------------*
*       FORM PRINT_position_LONTEXT.                                   *
*----------------------------------------------------------------------*
*       Print the long text for the notification POSITION              *
*       It will be printed exactly as found in text file STXH          *
*       inside the current form in window MAIN.                        *
*----------------------------------------------------------------------*
*  -->  WQMFE  Globally Position work record must be available         *
*----------------------------------------------------------------------*
FORM print_position_longtext.
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
    PERFORM print_longtext USING c_qmfe
                                 text_object_name
                                 wqmfe-kzmla
                                 ltxt_id
                                 c_main
                                 c_start_line_nr
                                 c_last_line_nr
                                 yes.  "with underline around text
  ENDIF.
ENDFORM.                    "PRINT_POSITION_LONGTEXT

*----------------------------------------------------------------------*
*       FORM PRINT_CAUSE_LONGTEXT.                                     *
*----------------------------------------------------------------------*
*       Print the long text for the notification DAMAGE CAUSE.         *
*       It will be printed exactly as found in text file STXH          *
*       inside the current form in window MAIN.                        *
*----------------------------------------------------------------------*
*  -->  WQMUR  Globally CAUSE work record must be available            *
*----------------------------------------------------------------------*
FORM print_cause_longtext.

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
    PERFORM print_longtext USING c_qmur
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
*       FORM LOCK_AND_SET.                                             *
*----------------------------------------------------------------------*
*       Enqueue print record   and determine copy number.              *
*       IF enqueue fails set COPY to 'NOT LOGGED'                      *
*----------------------------------------------------------------------*
FORM lock_and_set USING depth LIKE iprt_wa-prt_depth.
*... PMPL key is set, copy text (ORIGINAL/COPY) is set, LOG record
*... Locked.  All copy numbers are locked !

  no_log = space.                      " Clear lock flag
  CLEAR pmpl.                          " Clear log record.

  CALL FUNCTION 'PM_BUILD_PRINT_KEY'   " Build PMPL print key
       EXPORTING                       " based on for
            pm_appl   =  t390-pm_appl                       "
            prt_depth =  depth         " which paper depth
            viqmel    =  viqmel
            wqmfe     =  wqmfe
            caufvd    =  caufvd
            afvgd     =  afvgd
            resbd     =  resbd
       IMPORTING
            pmpl      =  pmpl.

  pmpl-pm_paper  = t390-workpaper.     " Now set the Workpaper
  pmpl-pm_appl   = t390-pm_appl.                            "
*  in display only mode do not lock pmpl
  IF device = c_screen OR device = c_preview.
    PERFORM set_pmpl_record.           " complete PMPL record
    " and copy indicator
  ELSE.
    CALL FUNCTION 'ENQUEUE_EPMPL'
      EXPORTING
        pm_appl        = pmpl-pm_appl  "Lock
        print_key      = pmpl-print_key
        pm_paper       = pmpl-pm_paper
        _wait          = yes
      EXCEPTIONS
        foreign_lock   = 01
        system_failure = 02.

    IF syst-subrc <> 0.
      no_log = yes.                    " The Document log is locked
      wiprt-copy_text = 'LOG ERR'(501)." for exactly this document.
      " Print it without log.
    ELSE.
      PERFORM set_pmpl_record.         " complete PMPL record
      " and copy indicator
    ENDIF.
  ENDIF.
ENDFORM.                    "LOCK_AND_SET

*----------------------------------------------------------------------*
*       FORM SET_PMPL_RECORD.                                          *
*----------------------------------------------------------------------*
*       The PMPL print log record is set based on the current key      *
*       and current runtime variables.                                 *
*       The idea is to decide whether it is a copy or not              *
*       and set the complete PMPL for writing after form is completely *
*       output.                                                        *
*----------------------------------------------------------------------*
*  -->  p1        PMPL record                                          *
*  <--  p2        PMPL record completed plus WIPRT copy text           *
*----------------------------------------------------------------------*
FORM set_pmpl_record.
*... Now work out how many copies have been printed a set COPY TEXT
*... for priting on the FORM.

  last_copy_nr = 0.
  SELECT MAX( copy_nr ) INTO last_copy_nr  FROM pmpl
         WHERE  pm_appl     = pmpl-pm_appl
         AND    print_key   = pmpl-print_key
         AND    pm_paper    = pmpl-pm_paper.
  pmpl-copy_nr = last_copy_nr + 1.

  IF pmpl-copy_nr = 1.                 " First_one
    IF wworkpaper-pm_delta_p = 'X'.
      wiprt-copy_text = 'Original/Delta'(505).
    ELSE.
      wiprt-copy_text = 'Original'(502).
    ENDIF.
    g_repeat = pmpl-copy_nr.           "für QM Reaklamation
  ELSE.
    IF wworkpaper-pm_delta_p = 'X'.
      wiprt-copy_text = 'Delta   '(504).
      WRITE pmpl-copy_nr TO wiprt-copy_text+8(3) NO-ZERO.
    ELSE.
      wiprt-copy_text = 'Kopie   '(503).
      WRITE pmpl-copy_nr TO wiprt-copy_text+8(3) NO-ZERO.
    ENDIF.
    g_repeat = pmpl-copy_nr.           "für QM Reklamation
  ENDIF.

*... complete the PMPL Log record for later.
  pmpl-abapname   = ''.   "This field is not supported from 4.0 onward.
  pmpl-form       = t390-form.
  pmpl-uname      = syst-uname.
  pmpl-datum      = syst-datum.
  pmpl-uzeit      = syst-uzeit.
  pmpl-tddest     = itcpo-tddest.
  pmpl-pm_delta_p = wworkpaper-pm_delta_p.
  pmpl-tdtelenum  = itcpo-tdtelenum.
  pmpl-tdteleland = itcpo-tdteleland.
ENDFORM.                    "SET_PMPL_RECORD

*----------------------------------------------------------------------*
*       FORM UNLOCK_AND_LOG.                                           *
*----------------------------------------------------------------------*
*       Dequeue the Print log record PMPL and write the log record     *
*       not in that order. !                                           *
*----------------------------------------------------------------------*
*  -->  PMPL      Globally set (or should be !!!)                      *
*----------------------------------------------------------------------*
FORM unlock_and_log.
  if gv_pdf_paper_selected is not initial.
    exit.
  endif.
*... update LOG file PMPL  DEQUEUE MADE BY SYSTEM !!!!!!!!!!
*// no_log is set in lock_and_set
  CHECK no_log = space.                "enqueue worked and copy no. set
  IF dont_log = space.                 " see open_form
    IF syst-oncom = space AND syst-batch = space. " not in update task
      CALL FUNCTION 'PM_UPDATE_PMPL' IN UPDATE TASK
        EXPORTING
          indupd = c_insert
          wpmpl  = pmpl.
      COMMIT WORK.
    ELSE.                              " we are already in update task
      CALL FUNCTION 'PM_UPDATE_PMPL'
        EXPORTING
          indupd = c_insert
          wpmpl  = pmpl.
    ENDIF.
*... unlock print record / for workpaper
*... DEQUEUE ON EPMPL MADE BY SYSTEM WHEN PM_UPDATE_PMPL IS FINSIHED
  ENDIF.
ENDFORM.                    "UNLOCK_AND_LOG

*---------------------------------------------------------------------*
*       FORM SAPSCRIPT_COMMAND                                        *
*---------------------------------------------------------------------*
*  -->  CMD                                                           *
*---------------------------------------------------------------------*
FORM sapscript_command USING cmd.
  CALL FUNCTION 'CONTROL_FORM'
    EXPORTING
      command   = cmd
    EXCEPTIONS
      unopened  = 01
      unstarted = 02.
ENDFORM.                    "SAPSCRIPT_COMMAND

*---------------------------------------------------------------------*
*       FORM PARTNER_DETAILS                                          *
*---------------------------------------------------------------------*
*  -->  IHPAD_LOC                                                     *
*---------------------------------------------------------------------*
FORM partner_details TABLES ihpad_loc STRUCTURE ihpad.

  DATA lv_nrart TYPE nrart.

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
* Do a conversion of the partner representation for personal numbers.
* This is done here to impact only the printing of shop papers
    SELECT SINGLE nrart FROM tpar INTO lv_nrart
      WHERE parvw EQ ihpad-parvw.
    IF sy-subrc EQ 0 AND lv_nrart EQ 'PE'.
* The NAME1,     NAME2,        NAME3 fields hold:
*     last name, Display name, First name
* Map NAME2 to NAME1 and clear the other fields.
      IF ihpad-name2 IS NOT INITIAL.
        MOVE ihpad-name2 TO ihpad-name1.
        CLEAR ihpad-name2.
        CLEAR ihpad-name3.
      ENDIF.
    ENDIF.
    CALL FUNCTION 'WRITE_FORM'
      EXPORTING
        element = 'PARTNER_INFO'
        window  = 'MAIN'.
  ENDLOOP.
ENDFORM.                    "PARTNER_DETAILS

*&---------------------------------------------------------------------*
*&      Form  READ_EQUIPMENT_PARTNER
*&---------------------------------------------------------------------*
FORM tech_object_partner USING equipment_nr LIKE viqmel-equnr
                               functional_location LIKE riwo1-tplnr.
  DATA: ihpad_cop LIKE ihpad OCCURS   0 WITH HEADER LINE.

  IF  NOT equipment_nr IS INITIAL.
    PERFORM get_equipment_data USING equipment_nr.
    PERFORM get_partner_for_object TABLES ihpad_cop
                                   USING equi-objnr.
*-> deletes double partners
    PERFORM only_new_partners TABLES ihpad_cop.
    PERFORM partner_details  TABLES ihpad_cop.              "
  ELSE.
    IF NOT functional_location IS INITIAL.
      PERFORM get_func_loc_data USING functional_location.
      PERFORM get_partner_for_object TABLES ihpad_cop
                                   USING iflo-objnr.
*-> deletes double partners
      PERFORM only_new_partners TABLES ihpad_cop.
      PERFORM partner_details TABLES  ihpad_cop.            "
    ENDIF.
  ENDIF.
ENDFORM.                               " READ_EQUIPMENT_PARTNER

*---------------------------------------------------------------------*
*       FORM GET_FUNC_LOC_DATA                                        *
*---------------------------------------------------------------------*
*  -->  FUNCTIONAL_LOCATION                                           *
*---------------------------------------------------------------------*
FORM get_func_loc_data USING functional_location LIKE riwo1-tplnr.
  CALL FUNCTION 'FUNC_LOCATION_READ'
    EXPORTING
      tplnr           = functional_location
    IMPORTING
      iflo_wa         = iflo
    EXCEPTIONS
      iflot_not_found = 01
      iloa_not_found  = 02.
ENDFORM.                    "GET_FUNC_LOC_DATA

*---------------------------------------------------------------------*
*       FORM GET_EQUIPMENT_DATA                                       *
*---------------------------------------------------------------------*
*  -->  EQUIPMENT_NR                                                  *
*---------------------------------------------------------------------*
FORM get_equipment_data USING equipment_nr  LIKE viqmel-equnr.
  CALL FUNCTION 'EQUIPMENT_READ'
    EXPORTING
      equi_no        = equipment_nr
    IMPORTING
      efhm           = efhm
      eqkt           = eqkt
      equi           = equi
      equz           = equz
      iloa           = iloa
    EXCEPTIONS
      auth_no_begrp  = 01
      auth_no_iwerk  = 02
      auth_no_swerk  = 03
      eqkt_not_found = 04
      equi_not_found = 05
      equz_not_found = 06
      iloa_not_found = 07.
ENDFORM.                    "GET_EQUIPMENT_DATA

*---------------------------------------------------------------------*
*       FORM GET_PARTNER_FOR_OBJECT                                   *
*---------------------------------------------------------------------*
*  -->  IHPAD_COP                                                     *
*  -->  OBJECT_NR                                                     *
*---------------------------------------------------------------------*
FORM get_partner_for_object TABLES ihpad_cop STRUCTURE ihpad
                            USING object_nr LIKE equi-objnr.
*... get the partners for the equipment
  CALL FUNCTION 'PM_GET_PARTNER_INFO'
    EXPORTING
      object_number = object_nr
    TABLES
      ihpad_tab_exp = ihpad_cop.

ENDFORM.                    "GET_PARTNER_FOR_OBJECT

*---------------------------------------------------------------------*
*       FORM WRITE_LONGTEXT                                           *
*---------------------------------------------------------------------*
*... ABAP write version of Long text print
FORM write_longtext USING object         TYPE clike
                          object_nr      TYPE clike
                          spras          TYPE clike
                          txid           TYPE clike
                          start_line_nr  TYPE numeric
                          last_line_nr   TYPE numeric
                          underline_flag TYPE clike.


*... convert parameters to correct size
  text_object  =   object.             " Move to special fields
  text_name    =   object_nr.          " for sizes to call
  text_id      =   txid.               " functions

*... First read the text.

  PERFORM read_text USING  text_id
                           print_language
                           text_name
                           text_object
                           rc.

  IF rc = 0.                           "When text is found
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

    IF underline_flag = yes.
      ULINE.
    ENDIF.
*... text unformatted print   FUNCTION: Print_text is an option here
*...                         if you really need formated texts
*...                         Use a SAPSCRIPT print example is even
*...                         better.
    LOOP AT table_lines.
      WRITE: / table_lines-tdline.
    ENDLOOP.

    IF underline_flag = yes.
      ULINE.
    ENDIF.
  ENDIF.                               " text was found
ENDFORM.                    "WRITE_LONGTEXT


*---------------------------------------------------------------------*
*       FORM READ_TEXT                                                *
*---------------------------------------------------------------------*
*  -->  TEXT_ID                                                       *
*  -->  PRINT_LANGUAGE                                                *
*  -->  TEXT_NAME                                                     *
*  -->  TEXT_OBJECT                                                   *
*  -->  LRC                                                           *
*---------------------------------------------------------------------*
FORM read_text USING  text_id          LIKE   thead-tdid
                      print_language   LIKE   thead-tdspras
                      text_name        LIKE   thead-tdname
                      text_object      LIKE   thead-tdobject
                      lrc              LIKE    rc.
  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      id              = text_id
      language        = print_language
      name            = text_name
      object          = text_object
    IMPORTING
      header          = table_header
    TABLES
      lines           = table_lines
    EXCEPTIONS
      id              = 01
      language        = 02
      name            = 03
      not_found       = 04
      object          = 05
      reference_check = 06.

  lrc = syst-subrc.
ENDFORM.                    "READ_TEXT
*&---------------------------------------------------------------------*
*&      Form  READ_MATERIAL
*&---------------------------------------------------------------------*
*   Materailtext lesen                                                 *
*----------------------------------------------------------------------*
FORM read_material.

  TABLES: mtcom,
          mtqss.
  DATA :  BEGIN OF dummy_tab OCCURS 0,
            dummy(1),
          END   OF dummy_tab.

  MOVE  'MTQSS'          TO mtcom-kenng.
  MOVE  viqmel-matnr     TO mtcom-matnr.
  IF NOT wworkpaper-print_lang IS INITIAL.
    MOVE  wworkpaper-print_lang  TO mtcom-spras.
  ELSE.
    MOVE  print_language   TO mtcom-spras.
  ENDIF.

  CALL FUNCTION 'MATERIAL_READ_MAQM'
    EXPORTING
      schluessel         = mtcom
    IMPORTING
      matdaten           = mtqss
    TABLES
      seqmat01           = dummy_tab
    EXCEPTIONS
      material_not_found = 04.
  IF sy-subrc EQ 0.
    MOVE mtqss-maktx TO riwo00-matktx.
  ENDIF.
ENDFORM.                               " READ_MATERIAL
*&---------------------------------------------------------------------*
*&      Form  READ_STATUS
*&---------------------------------------------------------------------*
* Statustext lesen                                                     *
*----------------------------------------------------------------------*
*  <--  sttxt     Text
*----------------------------------------------------------------------*
FORM read_status USING p_objnr
                       p_sttxt.

*-- statusleiste füllen                                          -----*
  CALL FUNCTION 'STATUS_TEXT_EDIT'
    EXPORTING
      flg_user_stat    = yes
      objnr            = p_objnr
      spras            = print_language
    IMPORTING
      line             = p_sttxt
    EXCEPTIONS
      object_not_found = 01.
ENDFORM.                               " READ_STATUS

*&---------------------------------------------------------------------*
*&      Form  PRINT_CLASSIFICATION
*&---------------------------------------------------------------------*
*-- Klassen drucken
FORM print_classification USING object     TYPE clike
                                object_tab TYPE clike
                                fieldname  TYPE clike.

  TABLES: api_kssk,                    "class header
          api_val_r.                   "values for each class
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
    api_kssk = intklassen. "note current class in ddic structure
    CALL FUNCTION 'WRITE_FORM'
      EXPORTING
        element = 'CLASS'.
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
      api_val_r = charact_values.      "move to ddic struc for sapscript
      IF charact_values-val_assign = 'X'.
        CALL FUNCTION 'WRITE_FORM'
          EXPORTING
            element = 'CLASS_FEATURE'.
      ELSE.
        CALL FUNCTION 'WRITE_FORM'
          EXPORTING
            element = 'CLASS_FEATURE_INHERITED'.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
ENDFORM.                    "PRINT_CLASSIFICATION
*&---------------------------------------------------------------------*
*&      Form  ONLY_NEW_PARTNERS
*&---------------------------------------------------------------------*
* deletes with equal PARNR and PARVW
*----------------------------------------------------------------------*
*      -->P_IHPAD_COP  text                                            *
*----------------------------------------------------------------------*
FORM only_new_partners TABLES ihpad_cop STRUCTURE ihpad.
*-> check if a partner is already in the order_ihpad_tab
  LOOP AT ihpad_cop. " delete all partners already printed with
    " order_ihpad_tab
*-> check order table
    READ TABLE order_ihpad_tab WITH KEY parnr   = ihpad_cop-parnr
                                        parvw   = ihpad_cop-parvw.
    IF sy-subrc = 0.
*-> entry found - we don't need it in ihpad_cop any longer
      DELETE ihpad_cop.
      CONTINUE.
    ENDIF.
*-> check notification table
    READ TABLE notif_ihpad_tab WITH KEY parnr   = ihpad_cop-parnr
                                        parvw   = ihpad_cop-parvw.
    IF sy-subrc = 0.
*-> entry found - we don't need it in ihpad_cop any longer
      DELETE ihpad_cop.
      CONTINUE.
    ENDIF.
*-> check common table
    READ TABLE ihpad_tab WITH KEY parnr   = ihpad_cop-parnr
                                  parvw   = ihpad_cop-parvw.
    IF sy-subrc = 0.
*-> entry found - we don't need it in ihpad_cop any longer
      DELETE ihpad_cop.
      CONTINUE.
    ENDIF.
  ENDLOOP.
ENDFORM.                               " ONLY_NEW_PARTNERS
*&---------------------------------------------------------------------*
*&      Form  READ_OBJECT_TEXTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_RIWO1  text
*      -->P_PRINT_LANGUAGE  text
*----------------------------------------------------------------------*
FORM read_object_texts USING h_riwo1 LIKE riwo1 h_langu LIKE sy-langu.
  DATA: old_makt LIKE makt,
        old_eqkt LIKE eqkt,
        old_iflo LIKE iflo.

*-> store old value
  old_makt = makt.
  old_eqkt = eqkt.
  old_iflo = iflo.
*-> assembly
  IF NOT h_riwo1-bautl IS INITIAL.
    CLEAR makt.
    SELECT SINGLE * FROM  makt
           WHERE  matnr  = h_riwo1-bautl
           AND    spras  = h_langu.
    IF NOT makt-maktx IS INITIAL.
      h_riwo1-bautx = makt-maktx.
    ENDIF.
    makt = old_makt.
  ENDIF.
*-> material number for serial number (with 4.0A)
  IF NOT h_riwo1-matnr IS INITIAL.
    CLEAR makt.
    SELECT SINGLE * FROM  makt
           WHERE  matnr  = h_riwo1-matnr
           AND    spras  = h_langu.
    IF NOT makt-maktx IS INITIAL.
      h_riwo1-matktx = makt-maktx.
    ENDIF.
    makt = old_makt.
  ENDIF.
*-> equipment
  IF NOT h_riwo1-equnr IS INITIAL.
    CLEAR eqkt.
    SELECT SINGLE * FROM  eqkt
           WHERE  equnr  = h_riwo1-equnr
           AND    spras  = h_langu.
    IF NOT eqkt-eqktx IS INITIAL.
      h_riwo1-eqtxt = eqkt-eqktx.
    ENDIF.
    eqkt = old_eqkt.
  ENDIF.
*-> funct. loc.
  IF NOT h_riwo1-tplnr IS INITIAL.
    CLEAR iflo.
    SELECT SINGLE * FROM  iflo
           WHERE  tplnr  = h_riwo1-tplnr
           AND    spras  = h_langu.
    IF NOT iflo-pltxt IS INITIAL.
      h_riwo1-pltxt = iflo-pltxt.
    ENDIF.
    iflo = old_iflo.
  ENDIF.
ENDFORM.                    " READ_OBJECT_TEXT

*&---------------------------------------------------------------------*
*&      Form  IMPORT_TIME_ZONE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LV_SESS_TZONE  text
*----------------------------------------------------------------------*
FORM IMPORT_TIME_ZONE CHANGING lv_sess_tzone type TTZZ-Tzone.
  IMPORT IV_SESS_TZONE TO LV_SESS_TZONE FROM MEMORY ID GV_TZONE.
ENDFORM.                    "IMPORT_TIME_ZONE
*&---------------------------------------------------------------------*
*&      Form  NOTIFICATION_DATA_IMPORT
*&---------------------------------------------------------------------*
*       Imports the notification data
*----------------------------------------------------------------------*
*      --> Values from memory.
*          see list below
*----------------------------------------------------------------------*
FORM notification_data_import
  TABLES   i_iviqmfe   TYPE t_iviqmfe
           i_iviqmma   TYPE t_iviqmma
           i_iviqmsm   TYPE t_iviqmsm
           i_iviqmur   TYPE t_iviqmur
           i_iqkat     TYPE t_iqkat
           i_ihpad_tab TYPE t_ihpad_tab
  CHANGING i_iviqmel   LIKE iviqmel
           i_riwo1     LIKE riwo1
           i_riwo00    LIKE riwo00
           i_rqm00     LIKE rqm00.

*--------------------------------------------------------------------*
*--- Assigned Objects
  DATA: lb_qnao_data_import TYPE REF TO badi_qnao_data_import.
*--- Assigned Objects
*--------------------------------------------------------------------*
  IMPORT iviqmel   TO i_iviqmel           " ... Notification
         iviqmfe   TO i_iviqmfe           " ... positions
         iviqmma   TO i_iviqmma           " ... codes
         iviqmsm   TO i_iviqmsm           " ... codes
         iviqmur   TO i_iviqmur           " ... codes
         iqkat     TO i_iqkat             " ... Catalogue
         ihpad_tab TO i_ihpad_tab         " ... partner table
         riwo1     TO i_riwo1             " ... Texts from Objects
         riwo00    TO i_riwo00   " ... Dialog structure with status info
         FROM MEMORY ID id_iprt_struct.

  IF syst-subrc <> 0.
    MESSAGE a650(id).                  " import failed
  ELSE.
    IMPORT rqm00                       " QM-QN special data
    FROM MEMORY ID id_iprt_rqm00.
    IF syst-subrc <> 0.
      MESSAGE i650(id).                " only import QM-QN failed
    ENDIF.

*--------------------------------------------------------------------*
*--- Assigned Objects
    TRY.
        GET BADI lb_qnao_data_import.
      CATCH cx_badi.                                    "#EC NO_HANDLER
    ENDTRY.
    IF lb_qnao_data_import IS BOUND.
      CALL BADI lb_qnao_data_import->import
        IMPORTING
          et_result = iqnao_tab.
    ENDIF.
*--- Assigned Objects
*--------------------------------------------------------------------*
  ENDIF.


ENDFORM.                    " NOTIFICATION_DATA_IMPORT
*&---------------------------------------------------------------------*
*&      Form  SET_GV_ARC_TYPE_AUFK
*&---------------------------------------------------------------------*
*  set global variable gv_arc_type_aufk to check for PM or CS
*----------------------------------------------------------------------*
*  --> new with note 766146
*  --> iv_auart       relevant oder type
*----------------------------------------------------------------------*
FORM set_gv_arc_type_aufk USING iv_auart LIKE t350-auart.

*check if object type for order 'CS' or 'PM'
  IF gs_t350-auart <> iv_auart.
    CLEAR gs_t350.
    SELECT SINGLE * FROM t350 INTO gs_t350
      WHERE auart = iv_auart.
  ENDIF.

  IF gs_t350-service = 'X'.
    gv_arc_type_aufk = c_arc_type_a_cs.
  ELSE.
    gv_arc_type_aufk = c_arc_type_a_pm.
  ENDIF.

ENDFORM.                    " SET_GV_ARC_TYPE_AUFK
*&---------------------------------------------------------------------*
*&      Form  LOGO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LOGO .
  CALL FUNCTION 'WRITE_FORM'
    EXPORTING
      element = 'LOGO'
      window  = 'LOGO'.         " Constant window in FORM
ENDFORM.                    " LOGO
*&---------------------------------------------------------------------*
*&      Form  STATUS_READ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM STATUS_READ .

  select single * from  jsto where OBJNR = viqmel-objnr.

  select OBJNR
         STAT
         INACT
         CHGNR from jest into table ijest
         where OBJNR = viqmel-objnr.

  ijest1[] = ijest[].

  select STSMA
         ESTAT
         SPRAS
         TXT04
         TXT30 from  TJ30T into table iTJ30T
         for all entries in ijest
         where STSMA  = jsto-STSMA
         and   ESTAT  = ijest-stat
         and   SPRAs = 'EN'.

  select OBJNR
         STAT
         CHGNR
         USNAM
         UDATE
         UTIME
         INACT
         CHIND from  JCDS into table iJCDS
         for all entries in ijest
         where OBJNR  = ijest-objnr
         and   STAT   = ijest-stat
         and   CHGNR  = ijest-CHGNR.

  SORT iTJ30T BY STSMA ESTAT .
  SORT IJCDS BY OBJNR STAT CHGNR.
*
  LOOP AT IJEST1 INTO IJEST_WA1.
    CLEAR: iTJ30T_WA,IJCDS_WA.
    READ TABLE iTJ30T INTO iTJ30T_WA WITH KEY STSMA = jsto-STSMA
                                              ESTAT = IJEST_WA1-STAT
                                              BINARY SEARCH.
    IF SY-SUBRC = 0.
      IJEST_WA1-TXT30 = iTJ30T_WA-TXT30.
    ENDIF.

    READ TABLE IJCDS INTO IJCDS_WA WITH KEY OBJNR  = ijest_WA1-objnr
                                            STAT   = ijest_WA1-stat
                                            CHGNR  = ijest_WA1-CHGNR
                                             BINARY SEARCH.
    IF SY-SUBRC = 0.
      IJEST_WA1-USNAM = IJCDS_WA-USNAM.
      IJEST_WA1-UDATE = IJCDS_WA-UDATE.
    ENDIF.

    IJEST_WA1-INDI  = IJEST_WA1-STAT+0(1).
    MODIFY IJEST1 FROM IJEST_WA1.
  ENDLOOP.

  DELETE IJEST1 WHERE INDI NE 'E'.
ENDFORM.                    " STATUS_READ
*&---------------------------------------------------------------------*
*&      Form  STATUS_PRINT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  STATUS_PRINT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM STATUS_PRINT .
CLEAR IJEST_WA1.
LOOP AT IJEST1 INTO IJEST_WA1.
  CALL FUNCTION 'WRITE_FORM'
    EXPORTING
      element = 'STATUS'
      window  = 'MAIN'.

ENDLOOP.
ENDFORM.                    " STATUS_PRINT
*&---------------------------------------------------------------------*
*&      Form  REVIEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM REVIEW .

CALL FUNCTION 'WRITE_FORM'
    EXPORTING
      element = 'REVIEW'
      window  = 'MAIN'.

ENDFORM.                    " REVIEW
*&---------------------------------------------------------------------*
*&      Form  BORDER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BORDER .
CALL FUNCTION 'WRITE_FORM'
    EXPORTING
      element = 'BORDER'
      window  = 'MAIN'.
ENDFORM.                    " BORDER
