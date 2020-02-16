*&---------------------------------------------------------------------*
*& Report ZPM_PRG_DRV_IBJ_WORK_PERMIT
*&---------------------------------------------------------------------*
*& IHDK904154 : ZPM001 : 6010859 : Friday, December 6, 2019 5:22:10 PM
*&---------------------------------------------------------------------*
program zpm_prg_drv_ibj_work_permit.

start-of-selection.
  data lv_formname    type tdsfname value 'ZPM_SF_PRMIT_WORK'.
  data lv_fm_name     type rs38l_fnam.

  clear lv_fm_name.
  call function 'SSF_FUNCTION_MODULE_NAME'
    exporting
      formname           = lv_formname
    importing
      fm_name            = lv_fm_name
    exceptions
      no_form            = 1
      no_function_module = 2
      others             = 3.
  if sy-subrc <> 0.
* Implement suitable error handling here
  endif.

  check lv_fm_name is not initial.

  data(ls_output) = value ssfcompop( tddest  = 'LP01'       " Spool: Output device
                                     tdimmed = abap_true    " Immediate Spool Print
                                     tdnewid = abap_true ). " New Spool Request

  data(ls_control) = value ssfctrlop( device    = 'PRINTER'     " Output device
                                      preview   = abap_false    " SAP Smart Forms: General Indicator
                                      no_dialog = abap_true     " Print preview
                                      getotf    = abap_true ).

  data: ls_job_op_info type ssfcrescl.
  clear ls_job_op_info.
  call function lv_fm_name
    exporting
      control_parameters = ls_control
      output_options     = ls_output
      user_settings      = abap_false   " supress user settings; to set defaults as above
    importing
      job_output_info    = ls_job_op_info
    exceptions
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      others             = 5.
  if sy-subrc <> 0.
* Implement suitable error handling here
  endif.

  check ls_job_op_info-otfdata[] is not initial.
  message 'Use Ctrl + P to print.' type 'S'.

  call function 'SSFCOMP_PDF_PREVIEW'
    exporting
      i_otf                    = ls_job_op_info-otfdata[]
    exceptions
      convert_otf_to_pdf_error = 1
      cntl_error               = 2
      others                   = 3.
  if sy-subrc <> 0.
* Implement suitable error handling here
  endif.
