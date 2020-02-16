REPORT zriprct00 .
************************************************************************
* Print Drive ABAP for CONTROL TICKET                                  *
*                                                                      *
* Standard FORM is  PM_COMMOM                                          *
*                                                                      *
*----------------------------------------------------------------------*
* ABAP STEPS:                                                          *
*    1: IMPORT data to print.                                          *
*                                                                      *
*    2: Parse of Data to print FORM.                                   *
*       The external text tables will be read as necessary.            *
*                                                                      *
*    3: Save Print-Protocol records in PMPL                            *
************************************************************************
*$*$  D A T A    S E C T I O N    I N C L U D E S ---------------------*
INCLUDE ZRIPRID01.
*INCLUDE riprid01.                      " General DATA and TABLE struct.
*------------------*
START-OF-SELECTION.
*------------------*
  PERFORM print_paper.  "can be started via SUBMIT or PERFORM PRINT_PAPER
  PERFORM print_paper_pdf.  "can be started via SUBMIT or PERFORM PRINT_PAPER_PDF

*$*$ ................ M A I N     F O R M .............................*
*... DATA STRUCTURE: ..................................................*
*...                                                                   *
*...    CAUFVD (AFIH AUFK AFKO plus other dialog fields: ORDER HEADER) *
*...     |                                                             *
*...     |-- AFVGD       (AFVC AFVV plus dialog fields) Order operatns *
*...     |   |                                                         *
*...     |   |-- The sub operations also stored AFVGD and are pre      *
*...     |       sorted. The SUMNR fields distinguishes Main operaitons*
*...     |       and sub operations                                    *
*...     |                                                             *
*...     |-- RESBD                 Materials                           *
*...     |-- RIPW0                 Object list dialog area
*             ------VIQMEL         First Notification from Object list
*...     |-- IHPAD                 Partners to Orders                  *
*...
*......................................................................*

*----------------------------------------------------------------------*
*       FORM PRINT_PAPER                                               *
*----------------------------------------------------------------------*
*       Main driving Form behind the Printing of Papers                *
*       All information is imported from MEMORY                        *
*----------------------------------------------------------------------*
*  -->  FORM        Name of SAPSCRIPT form to use.                     *
*  -->  WWORKPAPER  Print options for SAPSCRIPT.                       *
*                   Structure command to define wworkpaper so the      *
*                   individual fields can be addressed.                *
*  -->  DATA STRUCTURES    See form DATA_IMPORT INCLUDE RIPRID01       *
*----------------------------------------------------------------------*
*$*$ -   P  R  I  N   T       P  A  P  E  R
FORM print_paper.                      " This form name must be used !!!
*$*$ -  STARTED BY EXTERNAL PERFORM
  PERFORM order_data_import.           " See INCLUDE RIPRIf02
  PERFORM main_print.                  " Print the PAPER now
ENDFORM.                    "PRINT_PAPER

*----------------------------------------------------------------------*
*       FORM PRINT_PAPER_PDF                                           *
*----------------------------------------------------------------------*
*       Main driving Form behind the Printing of Papers                *
*       All information is imported from MEMORY                        *
*----------------------------------------------------------------------*
*  -->  FORM        Name of SAPSCRIPT form to use.                     *
*  -->  WWORKPAPER  Print options for SAPSCRIPT.                       *
*                   Structure command to define wworkpaper so the      *
*                   individual fields can be addressed.                *
*  -->  DATA STRUCTURES    See form ORDER_DATA_IMPORT INCLUDE RIPRIF02 *
*----------------------------------------------------------------------*
FORM print_paper_pdf.

  PERFORM order_data_import.                 " See INCLUDE RIPRIF02
  PERFORM main_print_pdf.                  " Print the PAPER now
ENDFORM.                    "PRINT_PAPER_PDF

*$*$ MAIN PRINT SECTION CONTROLLED HERE................................
*... If you are making changes to Print ABAPS, (Naturally a copied
*... version) here is the place you can alter the logic and
*... and data supplied to the form.   You should not alter logic
*... before this point if you wish it to operate successfully
*... with the standard transactions. Form PRINT_PAPER must exist !!
*... However if you wish the PRINT LOG to work you must take
*... care to make sure the LOG records are written to PMPL.
*......................................................................

FORM main_print.
*... Workpaper is controlled at a HEADER LEVEL  (ORDERS)
*... ONLY THOSE OPERATIONS WITH A VALID CONTROL KEY FOR PRINT will
*... be LISTED.

  PERFORM set_gv_arc_type_aufk USING caufvd-auart.          "n766146

* start of node 766146:
* PERFORM OPEN_FORM  USING C_ARC_TYPE_AUFK "Archive link for order
  PERFORM open_form  USING gv_arc_type_aufk  "Archive link for order
* end of node 766146
                           caufvd-aufnr"order number as key
                           ' '.        "New form for each Order
  PERFORM lock_and_set                 " Enque and determine copy number
          USING c_header_order.        " open for Header level
  PERFORM set_title.
  perform logo.
  perform title.       "added
  PERFORM title_page.
  PERFORM read_order_text_tables.      " Read tables for CAUFVD
  PERFORM order_header_detail. " Now print the order header see f02
  perform border.
  PERFORM permits USING caufvd-objnr.  "special permits
  PERFORM partner_details              " prints partner details
          TABLES order_ihpad_tab.                           "
  PERFORM tech_object_partner          " partner address equi / F.Locat
          USING caufvd-equnr  caufvd-tplnr.
  PERFORM print_classification USING caufvd-equnr 'EQUI' 'EQUNR'.
  PERFORM print_classification USING caufvd-tplnr 'IFLOT' 'TPLNR'.
  PERFORM order_operations_heading.  " Operation details and reservations heading
  PERFORM order_operations.  " Operation details and reservations
*  PERFORM object_list USING yes.       " object list with new page true
  PERFORM end_of_report.               " Print end of report line
  PERFORM close_form.                  " Close the form.
  PERFORM unlock_and_log.              " Dequeue and Log print
ENDFORM.                    "MAIN_PRINT

*$*$ MAIN PRINT PDF SECTION CONTROLLED HERE................................
*... If you are making changes to Print ABAPS, (Naturally a copied
*... version) here is the place you can alter the logic and
*... and data supplied to the form.   You should not alter logic
*... before this point if you wish it to operate successfully
*... with the standard transactions
*... However if you wish the PRINT LOG to work you must take
*... care to make sure the LOG records are written to PMPP.
*......................................................................
FORM main_print_pdf.

  PERFORM set_gv_arc_type_aufk USING caufvd-auart.          "n766146

  PERFORM open_form_pdf  USING gv_arc_type_aufk  "Archive link for order

                           caufvd-aufnr"order number as key
                           ' '.        "New form for each Order
  PERFORM lock_and_set                 " Enque and determine copy number
          USING c_header_order.        " open for Header level
  PERFORM set_title.
*  perform logo.
*  perform title.       "added
  PERFORM title_page_pdf..
  PERFORM read_order_text_tables.      " Read tables for CAUFVD
  PERFORM order_header_detail_pdf. " Now print the order header see f02
  PERFORM permits_pdf USING caufvd-objnr.  "special permits
  PERFORM partner_details_pdf              " prints partner details
          TABLES order_ihpad_tab.                           "
  PERFORM tech_object_partner_pdf          " partner address equi / F.Locat
          USING caufvd-equnr  caufvd-tplnr.
  PERFORM print_classification_pdf USING caufvd-equnr 'EQUI' 'EQUNR'.
  PERFORM print_classification_pdf USING caufvd-tplnr 'IFLOT' 'TPLNR'.
  PERFORM order_operations_pdf.  " Operation details and reservations
  PERFORM object_list_pdf USING yes.       " object list with new page true
  PERFORM print_pdf.
  PERFORM close_form_pdf.                  " Close the form.
  PERFORM unlock_and_log.              " Dequeue and Log print

ENDFORM.                    "MAIN_PRINT_PDF
*$*$   F O R M    R O U T I N E S -------------------------------------*
*...   Includes for General and Sepcific form routines
INCLUDE ZRIPRIF01.
*INCLUDE riprif01.                      " General PRINT routines
INCLUDE ZRIPRIF02.
*INCLUDE riprif02.                      " General PRINT routines ORDERS
INCLUDE ZRIPRIF1A.
*INCLUDE riprif1a.

*&---------------------------------------------------------------------*
*&      Form  ORDER_OPERATIONS_PDF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*       Notification Details are printed here.                         *
*       Text ELEMENT used    OPERATION                                 *
*----------------------------------------------------------------------*
FORM order_operations_pdf.
  iafvgd = space.
  LOOP AT iafvgd WHERE aufpl = caufvd-aufpl. "loop on operations
    afvgd = iafvgd.                    " Set workarea for SAPSCRIPT
*... for each operation
    PERFORM check_print_status USING afvgd-objnr
                                     wworkpaper-pm_delta_p
                                     rc.
    CHECK rc = 0.
    IF op_entries > 0.                 " single operation print active
      LOOP AT op_print_tab WHERE
              flg_sel = 'X'
         AND  vornr   = afvgd-vornr    " was the operation selected
         AND  uvorn   = afvgd-uvorn.   " for print ???
      ENDLOOP.
      CHECK syst-subrc = 0.            " should this op be printed
    ENDIF.
    PERFORM read_op_text_tables.       "operation text tables
    " T430 is now available
    iafvgd-flg_frd = afvgd-flg_frd.
    CHECK t430-vrgd = yes.             " jump to next operation when oper not marked for print
    gs_afvgd = iafvgd.
    APPEND gs_afvgd TO gt_afvgd.
    gs_rcr01-arbid = afvgd-arbid.
    gs_rcr01-arbpl = rcr01-arbpl.
    gs_rcr01-ktext = rcr01-ktext.
    gs_rcr01-werks = rcr01-werks.
    APPEND gs_rcr01 TO gt_rcr01.

    CLEAR gs_rcr01.
*... check that the operation should be printed based on the
*... control key.
    PERFORM print_operation_text_pdf.      " Longtext to operation
  ENDLOOP.
ENDFORM.                    "ORDER_OPERATIONS_PDF


*&---------------------------------------------------------------------*
*&      Form  PRINT_PDF
*&---------------------------------------------------------------------*
FORM print_pdf .
  DATA: lv_sess_tzone  TYPE  ttzz-tzone.

*--- Get time zone value for the user
  PERFORM get_timezone CHANGING lv_sess_tzone.

  IF NOT gv_fm_name IS INITIAL       .
    CALL FUNCTION gv_fm_name
      EXPORTING
        order_header               = caufvd
        object_connection          = riwo1
        order_operation            = gt_afvgd
        task_list                  = rcr01
        title_page                 = gs_wiprt
        order_type                 = t003p
        priority                   = t356_t
        local_account              = gs_iloa
        object_list                = gt_ripw0
        revision                   = t352r
        permits                    = gt_t357g
        permit_segment             = gt_ihgns
        partner_info               = gt_ihpad1
        class_object               = gt_api_kssk
        class_value                = gt_api_val
        notification_head_longtext = lt_lines1
        material_longtext          = gt_opr_text
        work_center                = gt_rcr01
        sess_tzone                 = lv_sess_tzone
      IMPORTING
        /1bcdwb/formoutput         = gv_fpformoutput
      EXCEPTIONS
        usage_error                = 1
        system_error               = 2
        internal_error             = 3
        OTHERS                     = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.
  REFRESH : gt_tline1,gt_qkat_head, gt_qkat, lt_lines1,gt_afvgd,gt_ihpad1,gt_opr_text,gt_api_val,gt_ihgns,gt_ripw0,gt_api_kssk,gt_rcr01,gt_t357g.
  CLEAR : gs_wiprt, gs_viqmel, gs_tq80_t, gs_to24i,gs_wqmfe, gs_makt,gs_iloa.

  IF gv_pdf_paper_selected IS NOT INITIAL.
    CALL BADI gb_riprif01_pdf_paper_print->set_pdf_paper_result
      EXPORTING
        iv_pdfdata   = gv_fpformoutput-pdf
        iv_form_name = t390_t-papertext.
  ENDIF.

ENDFORM.                    " PRINT_PDF


*&---------------------------------------------------------------------*
*&      Form  get_timezone
*&---------------------------------------------------------------------*
*  Check the time zone switch and customization status and get the reference time zone
*----------------------------------------------------------------------*
*      -->CV_SESS_TZONE  reference time zone for the user
*----------------------------------------------------------------------*
FORM get_timezone CHANGING cv_sess_tzone type ttzz-tzone.
*--- declaration for time zone support
  TYPE-POOLS: tzs1.
  DATA: gb_badi_time_zone    TYPE REF TO badi_eam_tz_generic_core,
        gv_time_zone_active  TYPE tz_d_active.

  BREAK-POINT ID eam_tzs_gen.
*--- at first check if BAdI is implemented
  TRY.
      GET BADI gb_badi_time_zone.

    CATCH cx_badi_not_implemented.
      gv_time_zone_active = abap_false.
      RETURN.
  ENDTRY.

  IF cl_badi_query=>number_of_implementations( gb_badi_time_zone ) = 0.
    gv_time_zone_active = abap_false.
    RETURN.
  ENDIF.

**--- check if customizing is active
  CALL FUNCTION 'TZS1_CHECK_CUSTOMIZING'
    EXPORTING
      is_application        = tzs1_appl-pm_woc
    IMPORTING
      es_activated          = gv_time_zone_active
    EXCEPTIONS
      application_not_valid = 1
      OTHERS                = 2.
  IF sy-subrc <> 0.
    gv_time_zone_active = abap_false.
  ENDIF.

*--- For time zone support, get the reference time zone of the user
  IF gv_time_zone_active = abap_true.
    CALL FUNCTION 'TZS1_GET_SESSION_TZON'
      IMPORTING
        e_sess_tzone = cv_sess_tzone.
  ENDIF.

ENDFORM.                    "get_timezone
*.......................................................................
*$*$ G E N E R A L     F O R M     R O U T I N E S ....................*
