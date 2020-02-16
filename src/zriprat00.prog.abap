REPORT zriprat00.
************************************************************************
* Print Drive ABAP for NOTIFICATION ACTIVITY TEMPLATE                  *
*                                                                      *
* Standard FORM is  PM_COMMON                                          *
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

INCLUDE zriprid01.                      " General DATA and TABLE struct.
*------------------*
START-OF-SELECTION.
*------------------*

  PERFORM print_paper. " for SAPScript
  PERFORM print_paper_pdf. " for PDF

*$*$ ................ M A I N     F O R M .............................*
*... DATA STRUCTURE: ..................................................*
*...                                                                   *
*...    VIQMEL (QMEL ILOA and QMIH together in View) Notif. Header     *
*...     !                                                             *
*...     !-- WQMFE                 Notification Position               *
*...     !   !                                                         *
*...     !   !-- WQMMA             Activities for a position           *
*...     !   !                                                         *
*...     !   !-- WQMUR             Damages per position.               *
*...     !                                                             *
*...     !-- WQMSM                 Procedures / Permissions for Notif  *
*...                                                                   *
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
FORM print_paper.
  PERFORM data_import.                 " See INCLUDE RIPRID01
  PERFORM read_view_text_tables.       " Read tables for VIQMEL
  PERFORM main_print.                  " Print the PAPER now

ENDFORM.                    "PRINT_PAPER

*----------------------------------------------------------------------*
*       FORM PRINT_PAPER_PDF                                               *
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
FORM print_paper_pdf.

  PERFORM data_import.                 " See INCLUDE RIPRID01
  PERFORM read_view_text_tables.       " Read tables for VIQMEL
  PERFORM main_print_pdf.              " Print the PAPER now
ENDFORM.                    "PRINT_PAPER_PDF

*$*$ MAIN PRINT SECTION CONTROLLED HERE................................
*... If you are making changes to Print ABAPS, (Naturally a copied
*... version) here is the place you can alter the logic and
*... and data supplied to the form.   You should not alter logic
*... before this point if you wish it to operate successfully
*... with the standard transactions
*... However if you wish the PRINT LOG to work you must take
*... care to make sure the LOG records are written to PMPP.
*......................................................................
FORM main_print.
*... Workpaper is controlled at a position level.
*... Each position a new Title/page reset is necessary.
*... For all positions marked for print.
  PERFORM open_form USING g_arc_type   "archive type
                          viqmel-qmnum "notif number as key
                              ' '.     " New form for each position
  PERFORM lock_and_set                 " Enque and determine copy number
              USING c_header_notif.    " Log to HEADER !!!!!!!!
  PERFORM set_title.
  perform logo.
  perform title.       "added
  PERFORM title_page.
  PERFORM notification_header.         " Header info each time
  perform status_read.
  perform status_PRINT.
  perform review.
  perform border.
  PERFORM header_detail.
  PERFORM end_of_report.               " Print end of report line
  PERFORM close_form.                  " Close the form.
  PERFORM unlock_and_log.              " Dequeue and Log print
  LOOP AT iviqmfe WHERE prtkz = yes.
    wqmfe = iviqmfe.                   " set work area as in FORM
*... the work area for QMFE is now set (WQMFE)
    PERFORM open_form USING g_arc_type "archive type
                            viqmel-qmnum     "notif number as key
                            ' '.       " New form for each position
    PERFORM lock_and_set               " Enque and determine copy number
            USING c_pos.               " Log to position level
    PERFORM title_page.
    PERFORM notification_header.       " Header info each time
    PERFORM notification_detail.       " Print the details
    PERFORM print_template USING wqmfe-fenum.
    PERFORM end_of_report.             " Print end of report line
    PERFORM close_form.                " Close the form.
    PERFORM unlock_and_log.            " Dequeue and Log print
  ENDLOOP.   " loop at position marked for print.
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

*... Workpaper is controlled at a position level.
*... Each position a new Title/page reset is necessary.
*... For all positions marked for print.
  PERFORM open_form_pdf USING g_arc_type   "archive type
                          viqmel-qmnum "notif number as key
                              ' '.     " New form for each position
  PERFORM lock_and_set                 " Enque and determine copy number
              USING c_header_notif.    " Log to HEADER !!!!!!!!
  PERFORM set_title.
  PERFORM title_page_pdf.
  PERFORM notification_header_pdf.         " Header info each time
  PERFORM header_detail_pdf.
  PERFORM print_pdf.
  PERFORM close_form_pdf.                  " Close the form.
  PERFORM unlock_and_log.              " Dequeue and Log print
  LOOP AT iviqmfe WHERE prtkz = yes.
    wqmfe = iviqmfe.                   " set work area as in FORM
*... the work area for QMFE is now set (WQMFE)
    PERFORM open_form_pdf USING g_arc_type "archive type
                            viqmel-qmnum     "notif number as key
                            ' '.       " New form for each position
    PERFORM lock_and_set               " Enque and determine copy number
            USING c_pos.               " Log to position level
    PERFORM title_page_pdf.
    PERFORM notification_header_pdf.       " Header info each time
    PERFORM notification_detail_pdf.       " Print the details
    PERFORM print_template_pdf USING wqmfe-fenum.
    PERFORM print_pdf.
    PERFORM close_form_pdf.                " Close the form.
    PERFORM unlock_and_log.            " Dequeue and Log print

  ENDLOOP.   " loop at position marked for print.
ENDFORM.                    "MAIN_PRINT_PDF
*$*$   F O R M    R O U T I N E S -------------------------------------*
*...   Includes for General and Sepcific form routines

INCLUDE zriprif01.                      " General PRINT routines
INCLUDE zriprif02.
INCLUDE ZRIPRIF03.
*INCLUDE riprif03.
INCLUDE Zriprif1a.
*$*$ G E N E R A L     F O R M     R O U T I N E S ....................*
FORM header_detail.
  CLEAR : wqmur, wqmfe.
  PERFORM print_template USING '0000'.
ENDFORM.                    "HEADER_DETAIL

*&---------------------------------------------------------------------*
*&      Form  HEADER_DETAIL_PDF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM header_detail_pdf.
  CLEAR : wqmur, wqmfe.
  PERFORM print_template_pdf USING '0000'.
ENDFORM.                    "HEADER_DETAIL_PDF

*&---------------------------------------------------------------------*
*&      Form  PRINT_PDF
*&---------------------------------------------------------------------*
FORM print_pdf .

  DATA: lv_sess_tzone  TYPE  ttzz-tzone.

*--- Get time zone value for the user
  PERFORM get_timezone CHANGING lv_sess_tzone.

  IF NOT gv_fm_name IS INITIAL.
    CALL FUNCTION gv_fm_name
      EXPORTING
        object_connection            = riwo1
        title_page                   = gs_wiprt
        notification_header          = gs_viqmel
        notification_type_text       = gs_tq80_t
        maintenance_panner_groups    = gs_to24i
        notification_detail          = gs_wqmfe
        material_descriptions        = gs_makt
        notification_head_longtext   = gt_tline1
        notification_detail_longtext = gt_tline2
        inspection_catalog_header    = gt_qkat_head
        fact_txtcdgr                 = gs_fact_txtcdgr
        inspection_catalog_item      = gt_qkat
        sess_tzone                   = lv_sess_tzone
      IMPORTING
        /1bcdwb/formoutput           = gv_fpformoutput
      EXCEPTIONS
        usage_error                  = 1
        system_error                 = 2
        internal_error               = 3
        OTHERS                       = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.

  REFRESH : gt_tline1,gt_tline2,gt_qkat_head, gt_qkat.
  CLEAR : gs_wiprt, gs_viqmel , gs_tq80_t, gs_to24i,gs_wqmfe, gs_makt.

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
