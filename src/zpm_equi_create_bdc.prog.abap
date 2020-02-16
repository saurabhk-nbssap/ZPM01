" ------------------------------------------------------
" Change History
" ------------------------------------------------------
" Changed by - 6010859; SaurabhK
" Date/time - Monday, June 25, 2018 09:44:15
" Approved by - Kamalakar Varma
" TR - IRDK932608
" Desc - PM: S_K: ZPM009: Changes for DMS doc: 25.06.18; Improve error handling and logging
" ------------------------------------------------------
report zpm_equi_bdc
       no standard page heading line-size 255.

***include bdcrecx1.
***
***start-of-selection.
***
***perform open_group.

data : lv_field_name type dynfnam.

***types : begin of ls_file_data,
***          valid_date    type datsl,
***          equi_cat      type eqtyp,
***          tech_grp      type begru,
***          weight_obj    type brgew, "
***          unit_weight   type gewei, "
***          size          type groes, "
***          inv_no        type invnr, "
***          str_dt_tech   type inbdt, "
***          typ_tech_obj  type eqart, "
***          acq_value     type answt, "
***          currency_key  type waers, "
***          acq_date      type ansdt, "
***          manfc_aset    type herst, "
***          cntry_manfc   type herld, "
***          manfc_mdl_no  type typbz, "
***          yer_constrct  type baujj, "
***          mnth_cnstrct  type baumm, "
***          manfc_prt_no  type mapar, "
***          manfc_sr_no   type serge, "
***          desc_tech_obj type KTX01, "
***          valid_frm_dt  type datab,
***          maintn_plnt   type swerk, "
***          loc_maintn    type stort,
***          room          type RAUMNR,  "
***          plant_sec     type beber, "
***          work_center   type arbpl,
***          indic_tech    type abckz, "
***          sort_field    type eqfnr, "
***          company_code  type bukrs, "
***          busins_area   type gsber, "
***          main_ast_no   type anlnr, "
***          aset_sub_no   type ANLN2, "
***          cost_center   type kostl, "
***          wbs_element   type PS_PSP_PNR,  "
***          std_ord_no    type daufn, "
***          seltlmnt_ord  type aufnr, "
***          maint_plnt    type iwerk, "
***          plan_grp_cust type ingrp, "
***          main_wrk_cntr type gewrk,
***          plnt_wrk_ctr  type wergw, "
***          catalog_pf    type rbnr,  "
***          func_loc      type tplnr,   "-
***          time          type uzeit,   "-
***          pos_sup_int   type posnr,   "-
***          tech_id_no    type tidnr, "
***          document_typ  type dokar, "==
***          doc_no        type doknr, "==
***          doc_part      type doktl, "==
***          doc_ver       type dokvr, "==
***      end of ls_file_data.

types : begin of ls_file_data,
          equi_code     type char30,
          desc_tech_obj type char40, "
          valid_date    type char10, "datsl,
          equi_cat      type c,
          tech_grp      type char4,
          weight_obj    type char11, "
          unit_weight   type char3, "
          size          type char32, "
          inv_no        type char25, "
          str_dt_tech   type char10, "inbdt, "
          typ_tech_obj  type char10, "
          acq_value     type char10, "
          currency_key  type char5, "
          acq_date      type char10, "ansdt, "
          manfc_aset    type char30, "
          cntry_manfc   type char3, "
          manfc_mdl_no  type char20, "
          yer_constrct  type char4, "
          mnth_cnstrct  type char2, "
          manfc_prt_no  type char30, "
          manfc_sr_no   type char30, "
          valid_frm_dt  type char10, "datab,
          maintn_plnt   type char4, "
          loc_maintn    type char10,
          room          type char8,  "
          plant_sec     type char3, "
          work_center   type char8,
          indic_tech    type c, "
          sort_field    type char30, "
          company_code  type char4, "
          busins_area   type char4, "
          main_ast_no   type char20, "
          aset_sub_no   type char4, "
          cost_center   type char10, "
          wbs_element   type char8,  "
          std_ord_no    type char12, "
          seltlmnt_ord  type char12, "
          maint_plnt    type char4, "
          plan_grp_cust type char3, "
          main_wrk_cntr type char8,
          plnt_wrk_ctr  type char4, "
          catalog_pf    type char9,  "
          func_loc      type char30,   "-
          time          type char8, "uzeit,   "-
          pos_sup_int   type char6, "posnr,   "-
          tech_id_no    type char25, "
*          document_typ  type char3, "==
          doc_no        type char25, "==
*          doc_part      type char3, "==
*          doc_ver       type char2, "==
        end of ls_file_data.


data : it_msg  type table of bdcmsgcoll,  " ERROR HANDLING
       wa_msg  type bdcmsgcoll,
       msg(51).

types : begin of ty_log,
          status(1) type c.
          include   type ls_file_data.
types:  msg type bapi_msg,
        end of ty_log.

data : bdcdata       like table of bdcdata with header line,
       wa_path       type string,
       wa_error      type string,
       wa_cnt        type i,
       w_mode        type c,
       wa_cnt1(2)    type n,
       wa_string(10) type c,
       it_output     type table of ty_log,
       wa_output     like line of it_output.

data : lt_file_data type table of ls_file_data,
       lw_file_data type ls_file_data.

data : i_tab_raw_data type  truxs_t_text_data.

data : lt_data_gen like bapi_itob,
       lt_ext_no   like bapi_itob_parms,
       lt_data_spe like bapi_itob_eq_only,
       lt_return   like bapiret2,
       lt_cmt_ret  type bapiret2.

data : lv_equi_no type equnr,
       lv_mode    type c.

selection-screen begin of block b1 with frame title text-001.

parameters : p_file  type ibipparms-path obligatory,
             p_error type ibipparms-path obligatory,
             p_mode  type c obligatory default 'A'.

selection-screen end of block b1.

at selection-screen on value-request for p_file.

  call function 'F4_FILENAME'
    exporting
      program_name  = syst-cprog
      dynpro_number = syst-dynnr
      field_name    = 'P_FILE' "lv_field_name
    importing
      file_name     = p_file.

at selection-screen on value-request for p_error.

  call function 'F4_FILENAME'
    exporting
      program_name  = syst-cprog
      dynpro_number = syst-dynnr
      field_name    = lv_field_name
    importing
      file_name     = p_error.

  lv_field_name = p_file.


at selection-screen.
****  break abap01.
  if p_mode = 'A' or p_mode = 'E'.
    lv_mode = p_mode.
  else.
    message 'Enter correct mode "A" for foreground or "E" for background processing with error display' type 'E' display like 'I'.
  endif.


start-of-selection.

  perform convert_excel_to_itab.

  break abap01.

  refresh it_output.
  clear lw_file_data.
  loop at lt_file_data into lw_file_data.

    " prepare output file
    clear wa_output.
    move-corresponding lw_file_data to wa_output.

    refresh bdcdata.
    refresh it_msg.

    " check dms doc
    data: doc_no type draw-doknr,
          dok_vr type draw-dokvr,
          dok_pt type draw-doktl.
    clear: doc_no, dok_vr, dok_pt.

    if lw_file_data-doc_no is not initial.
      doc_no = lw_file_data-doc_no.
      doc_no = |{ doc_no alpha = in }|.

      select single max( dokvr ) doktl
        from draw
        into ( dok_vr, dok_pt )
        where doknr = doc_no
        and   dokar = 'Z14'
        and   dokst = 'FR' "Released
        group by doktl.

      if sy-subrc <> 0.
        clear: doc_no, dok_vr, dok_pt.
        wa_output-status = 'E'.
        wa_output-msg = 'Invalid DMS document no. OR latest version of DMS document is not released'.
        append wa_output to it_output.
        clear lw_file_data.
        continue.
      endif.
    endif.

    perform ie01_new.
    call transaction 'IE01' using bdcdata mode lv_mode update 'S' messages into it_msg.

    clear wa_msg.
    read table it_msg into wa_msg with key msgid = 'IS' msgnr = '816' msgtyp = 'S'.
    if sy-subrc = 0.
      wa_output-status = 'S'.
      wa_output-equi_code = wa_msg-msgv1.
      wa_output-msg = 'Equipment created with the number' && ` ` && condense( wa_msg-msgv1 ).
    else.
      read table it_msg into wa_msg index 1.  " any kind of non-sucess message will be logged
      if sy-subrc = 0.
        wa_output-status = 'E'.
        clear msg.
        call function 'MESSAGE_TEXT_BUILD'
          exporting
            msgid               = wa_msg-msgid
            msgnr               = wa_msg-msgnr
            msgv1               = wa_msg-msgv1
            msgv2               = wa_msg-msgv2
            msgv3               = wa_msg-msgv3
            msgv4               = wa_msg-msgv4
          importing
            message_text_output = msg.

        wa_output-msg = msg.
      endif.
    endif.

    append wa_output to it_output.
    clear lw_file_data.
  endloop.

  wa_error = p_error.
  call function 'GUI_DOWNLOAD'                            " Download Error file
    exporting
      filename              = wa_error
      write_field_separator = 'X'
    tables
      data_tab              = it_output.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty
    number sy-msgno  with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.


*&---------------------------------------------------------------------*
*&      Form  CONVERT_EXCEL_TO_ITAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form convert_excel_to_itab .

  break abap01.

  call function 'TEXT_CONVERT_XLS_TO_SAP'
    exporting
*     I_FIELD_SEPERATOR    =
      i_line_header        = 'X'
      i_tab_raw_data       = i_tab_raw_data
      i_filename           = p_file
    tables
      i_tab_converted_data = lt_file_data
    exceptions
      conversion_failed    = 1
      others               = 2.
  if sy-subrc <> 0.
    message 'File Error' type 'E' display like 'I'.
  endif.

endform.


*&---------------------------------------------------------------------*
*&      Form  IE01_NEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form ie01_new .

  perform bdc_dynpro      using 'SAPMIEQ0' '0100'.
  perform bdc_field       using 'BDC_CURSOR'
        'RM63E-EQTYP'.
  perform bdc_field       using 'BDC_OKCODE'
        '/00'.
***  concatenate lw_file_data-valid_date+6(2) lw_file_data-valid_date+4(2) lw_file_data-valid_date(4) into lw_file_data-valid_date." separated by '.'.
  perform bdc_field       using 'RM63E-DATSL'
        lw_file_data-valid_date. "'11042018'.
  translate lw_file_data-equi_cat to upper case.
  perform bdc_field       using 'RM63E-EQTYP'
        lw_file_data-equi_cat. "'Q'.

  perform bdc_dynpro      using 'SAPMIEQ0' '0101'.
  perform bdc_field       using 'BDC_OKCODE'
        '=T\02'.
  perform bdc_field       using 'ITOB-BEGRU'
        lw_file_data-tech_grp."'0001'.
  perform bdc_field       using 'ITOB-BRGEW'
        lw_file_data-weight_obj."'10'.
  translate lw_file_data-unit_weight to upper case.
  perform bdc_field       using 'ITOB-GEWEI'
        lw_file_data-unit_weight."'KG'.
  perform bdc_field       using 'ITOB-GROES'
        lw_file_data-size."'10'.
  perform bdc_field       using 'ITOB-INVNR'
        lw_file_data-inv_no."'1234'.
***  concatenate lw_file_data-str_dt_tech+6(2) lw_file_data-str_dt_tech+4(2) lw_file_data-str_dt_tech(4) into lw_file_data-str_dt_tech." separated by '.'.
  perform bdc_field       using 'ITOB-INBDT'
        lw_file_data-str_dt_tech."'11042018'.
  perform bdc_field       using 'ITOB-EQART'
        lw_file_data-typ_tech_obj."'1000'.
  perform bdc_field       using 'ITOB-ANSWT'
        lw_file_data-acq_value."'1234'.
  translate lw_file_data-currency_key to upper case.
  perform bdc_field       using 'ITOB-WAERS'
        lw_file_data-currency_key."'INR'.
***  concatenate lw_file_data-acq_date+6(2) lw_file_data-acq_date+4(2) lw_file_data-acq_date(4) into lw_file_data-acq_date." separated by '.'.
  perform bdc_field       using 'ITOB-ANSDT'
        lw_file_data-acq_date."'11042018'.
  perform bdc_field       using 'BDC_CURSOR'
        'ITOB-SERGE'.
  translate lw_file_data-manfc_aset to upper case.
  perform bdc_field       using 'ITOB-HERST'
        lw_file_data-manfc_aset."'in'.
  translate lw_file_data-cntry_manfc to upper case.
  perform bdc_field       using 'ITOB-HERLD'
        lw_file_data-cntry_manfc."'IN'.
  perform bdc_field       using 'ITOB-TYPBZ'
        lw_file_data-manfc_mdl_no."'987'.
  perform bdc_field       using 'ITOB-BAUJJ'
        lw_file_data-yer_constrct."'2018'.
  perform bdc_field       using 'ITOB-BAUMM'
        lw_file_data-mnth_cnstrct."'03'.
  perform bdc_field       using 'ITOB-MAPAR'
        lw_file_data-manfc_prt_no."'2345'.
  perform bdc_field       using 'ITOB-SERGE'
        lw_file_data-manfc_sr_no."'4567'.
  perform bdc_field       using 'ITOB-SHTXT'
        lw_file_data-desc_tech_obj."'Test equipment'.
***  concatenate lw_file_data-valid_frm_dt+6(2) lw_file_data-valid_frm_dt+4(2) lw_file_data-valid_frm_dt(4) into lw_file_data-valid_frm_dt." separated by '.'.
  perform bdc_field       using 'ITOB-DATAB'
        lw_file_data-valid_frm_dt."'11042018'.

  perform bdc_dynpro      using 'SAPMIEQ0' '0101'.
  perform bdc_field       using 'BDC_OKCODE'
        '/00'.
  perform bdc_field       using 'BDC_CURSOR'
        'ITOB-STORT'.
  perform bdc_field       using 'ITOB-SWERK'
        lw_file_data-maintn_plnt."'1101'.
  perform bdc_field       using 'ITOB-SHTXT'
        lw_file_data-desc_tech_obj."'Test equipment'.
  perform bdc_field       using 'ITOB-DATAB'
        lw_file_data-valid_frm_dt."'11.04.2018'.

  perform bdc_dynpro      using 'SAPMIEQ0' '0101'.
  perform bdc_field       using 'BDC_OKCODE'
        '=T\03'.
  perform bdc_field       using 'BDC_CURSOR'
        'ITOB-EQFNR'.
  perform bdc_field       using 'ITOB-SWERK'
        lw_file_data-maintn_plnt.                           "''1101'.
  perform bdc_field       using 'ITOB-STORT'
        lw_file_data-loc_maintn."'0001'.
  perform bdc_field       using 'ITOB-MSGRP'
        lw_file_data-room."'abcd'.
  translate lw_file_data-plant_sec to upper case.
  perform bdc_field       using 'ITOB-BEBER'
        lw_file_data-plant_sec."'SYN'.
  translate lw_file_data-work_center to upper case.
  perform bdc_field       using 'ITOBATTR-ARBPL'
        lw_file_data-work_center."'RVD-AGR'.
  perform bdc_field       using 'ITOB-ABCKZ'
        lw_file_data-indic_tech."'A'.
  perform bdc_field       using 'ITOB-EQFNR'
        lw_file_data-sort_field."'test'.
  perform bdc_field       using 'ITOB-SHTXT'
        lw_file_data-desc_tech_obj."'Test equipment'.
  perform bdc_field       using 'ITOB-DATAB'
        lw_file_data-valid_frm_dt."'11.04.2018'.

  perform bdc_dynpro      using 'SAPMIEQ0' '0101'.
  perform bdc_field       using 'BDC_OKCODE'
        '=T\04'.
  perform bdc_field       using 'ITOB-BUKRS'
        lw_file_data-company_code."'1000'.
  perform bdc_field       using 'ITOB-GSBER'
        lw_file_data-busins_area."'0001'.
  perform bdc_field       using 'ITOB-ANLNR'
        lw_file_data-main_ast_no.                           "'1100000'.
  perform bdc_field       using 'ITOB-ANLUN'
        lw_file_data-aset_sub_no."'0'.
  perform bdc_field       using 'ITOB-KOSTL'
        lw_file_data-cost_center."'100101008'.
  translate lw_file_data-wbs_element to upper case.
  perform bdc_field       using 'ITOB-PROID'
        lw_file_data-wbs_element."'IMAB-NI12-01.1.05'.
  perform bdc_field       using 'ITOB-DAUFN'
        lw_file_data-std_ord_no."'200005032'.
  perform bdc_field       using 'ITOB-AUFNR'
        lw_file_data-seltlmnt_ord."'70000333'.
  perform bdc_field       using 'ITOB-IWERK'
        lw_file_data-maint_plnt."'1101'.
  perform bdc_field       using 'ITOB-INGRP'
        lw_file_data-plan_grp_cust."'001'.
  translate lw_file_data-main_wrk_cntr to upper case.
  perform bdc_field       using 'ITOBATTR-GEWRK'
        lw_file_data-main_wrk_cntr."'TELEC'.
  perform bdc_field       using 'ITOBATTR-WERGW'
        lw_file_data-plnt_wrk_ctr."'1101'.
  perform bdc_field       using 'ITOB-RBNR'
        lw_file_data-catalog_pf."'000000001'.
  perform bdc_field       using 'ITOB-SHTXT'
        lw_file_data-desc_tech_obj."'Test equipment'.
  perform bdc_field       using 'ITOB-DATAB'
        lw_file_data-valid_frm_dt."'11.04.2018'.

  perform bdc_dynpro      using 'SAPMIEQ0' '0101'.
  perform bdc_field       using 'BDC_OKCODE'
        '=CIPL'.
  perform bdc_field       using 'BDC_CURSOR'
        'ITOB-SHTXT'.
  perform bdc_field       using 'ITOB-SHTXT'
        lw_file_data-desc_tech_obj."'Test equipment'.
  perform bdc_field       using 'ITOB-DATAB'
        lw_file_data-valid_frm_dt."'11.04.2018'.

  perform bdc_dynpro      using 'SAPLIEL2' '0100'.
  perform bdc_field       using 'BDC_CURSOR'
        'IEQINSTALL-TPLNR'.
  perform bdc_field       using 'BDC_OKCODE'
        '=EXIT'.
  perform bdc_field       using 'IEQINSTALL-TPLNR'
        lw_file_data-func_loc."'T-DRY-1RVD'.
  perform bdc_field       using 'IEQINSTALL-DATUM'
        lw_file_data-valid_frm_dt."'11.04.2018'.
  perform bdc_field       using 'IEQINSTALL-UZEIT'
        lw_file_data-time."'00:00:00'.

  perform bdc_dynpro      using 'SAPMIEQ0' '0101'.
  perform bdc_field       using 'BDC_OKCODE'
        '=T\05'.
  perform bdc_field       using 'BDC_CURSOR'
        'ITOB-TIDNR'.
  perform bdc_field       using 'ITOB-TIDNR'
        lw_file_data-tech_id_no."'1234'.
  perform bdc_field       using 'ITOB-SHTXT'
        lw_file_data-desc_tech_obj."'Test equipment'.
  perform bdc_field       using 'ITOB-DATAB'
        lw_file_data-valid_frm_dt."'11.04.2018'.

  perform bdc_dynpro      using 'SAPMIEQ0' '0101'.
  perform bdc_field       using 'BDC_OKCODE'
        '=BU'.
  perform bdc_field       using 'GF_ALLE'
        'X'.
  if doc_no is not initial.
    perform bdc_field       using 'BDC_CURSOR'
          'DRAW-DOKVR(01)'.
*  translate lw_file_data-document_typ to upper case.
    perform bdc_field       using 'DRAW-DOKAR(01)'
          'Z14'.  " lw_file_data-document_typ."'DRW'.
    perform bdc_field       using 'DRAW-DOKNR(01)'
          doc_no. " lw_file_data-doc_no."'20000000000'.
    perform bdc_field       using 'DRAW-DOKTL(01)'
          dok_pt. " lw_file_data-doc_part."'000'.
    perform bdc_field       using 'DRAW-DOKVR(01)'
          dok_vr. " lw_file_data-doc_ver."'00'.
  endif.
  perform bdc_field       using 'ITOB-SHTXT'
        lw_file_data-desc_tech_obj."'Test equipment'.
  perform bdc_field       using 'ITOB-DATAB'
        lw_file_data-valid_frm_dt."'11.04.2018'.
endform.


*----------------------------------------------------------------------*
*        Start new screen                                              *
*----------------------------------------------------------------------*
form bdc_dynpro using program dynpro.
  clear bdcdata.
  bdcdata-program  = program.
  bdcdata-dynpro   = dynpro.
  bdcdata-dynbegin = 'X'.
  append bdcdata.
endform.

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
form bdc_field using fnam fval.
***IF FVAL <> space.
  clear bdcdata.
  bdcdata-fnam = fnam.
  bdcdata-fval = fval.
  append bdcdata.
***ENDIF.
endform.
