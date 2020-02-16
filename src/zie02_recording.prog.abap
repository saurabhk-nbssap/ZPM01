*&---------------------------------------------------------------------*
*& Report  ZIE02_RECORDING                                             *
*&                                                                     *
*&---------------------------------------------------------------------*
*& Program Name        :  ZIE02_RECORDING                              *
*& Creation Date       :  Aug 08, 2016                                 *
*& Author              :  Naren Karra                                  *
*& Functional          :  K Varma  (Indofil)                           *
*& Application Area    :  PM                                           *
*& Development Spec ID :                                               *
*&---------------------------------------------------------------------*
*& Description:        :  Equipment Master BDC                         *
*& Inputs              :                                               *
*& Outputs             :                                               *
*& Scheduling          :  Back ground and foreground.                  *
*& External Routines   :  N/A                                          *
*& Assumptions/Restriction:                                            *
*& Change History:                                                     *
*&=====================================================================*
*& Date          | Change #              | Changed By | Description    *
*& Aug 08, 2016  | IRDK924943            | IBM_AMS    | Initial        *
*&                                                      Development    *
*----------------------------------------------------------------------*

REPORT zie02_recording NO STANDARD PAGE HEADING LINE-SIZE 255.
TYPE-POOLS: truxs.
*----------------------------------------------------------------------*
*  I N C L U D E S                                                     *
*----------------------------------------------------------------------*
INCLUDE zbdcrecx1_nk.
*----------------------------------------------------------------------*
*  T Y P E S    D E C L A R A T I O N S                                *
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_file,
        equnr    TYPE equi-equnr,
        equnr1   TYPE equi-equnr,
        equnr2   TYPE equi-equnr,
        equnr3   TYPE equi-equnr,
        equnr4   TYPE equi-equnr,
        equnr5   TYPE equi-equnr,
        equnr6   TYPE equi-equnr,
        equnr7   TYPE equi-equnr,
        equnr8   TYPE equi-equnr,
        equnr9   TYPE equi-equnr,
        equnr10   TYPE equi-equnr,
        equnr11   TYPE equi-equnr,
        equnr12   TYPE equi-equnr,
        equnr13   TYPE equi-equnr,
        equnr14   TYPE equi-equnr,
        equnr15   TYPE equi-equnr,
        equnr16   TYPE equi-equnr,
        equnr17   TYPE equi-equnr,
        equnr18   TYPE equi-equnr,
        equnr19   TYPE equi-equnr,
        equnr20   TYPE equi-equnr,
        equnr21   TYPE equi-equnr,
        equnr22   TYPE equi-equnr,
        equnr23   TYPE equi-equnr,
        equnr24   TYPE equi-equnr,
        equnr25   TYPE equi-equnr,
        equnr26   TYPE equi-equnr,
        equnr27   TYPE equi-equnr,
        equnr28   TYPE equi-equnr,
        equnr29   TYPE equi-equnr,
        equnr30   TYPE equi-equnr,
       END OF ty_file.
*----------------------------------------------------------------------*
*  D A T A    D E C L A R A T I O N S                                  *
*----------------------------------------------------------------------*
DATA: wa TYPE ty_file,
      it TYPE TABLE OF ty_file,
      it_type TYPE truxs_t_text_data.

DATA: lv_file TYPE string.
DATA: lv_file1 TYPE rlgrap-filename.

DATA:
      n TYPE c,
      n1 TYPE i,
      lv_index TYPE sy-index,
      lv_equnr TYPE equi-equnr,
      lv_date(10).

DATA:
      ls_equipment    LIKE bapi_itob_parms-equipment,
      ls_general_exp  LIKE bapi_itob,
      ls_specific_exp LIKE bapi_itob_eq_only,
      ls_fleet_exp    LIKE bapi_fleet,
      ls_return       LIKE bapiret2.
*----------------------------------------------------------------------*
*   A T   S E L E C T I O N    S C R E E N                             *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR file.
  CLEAR file.
  CALL FUNCTION 'F4_FILENAME'
    IMPORTING
      file_name = file.

  lv_file  = file.
  lv_file1 = file.    " For Excel upload file type.
*----------------------------------------------------------------------*
*   S T A R T   O F   S E L E C T I O N                                *
*----------------------------------------------------------------------*
**********************************************************************  ~ nk
* These will be useful in future when dealing wit complex requirements !!
*BAPI_EQUI_GETDETAIL
*BAPI_EQUI_GETLIST
*BAPI_EQUI_GETSTATUS
*BAPI_EQUI_INSTALL
START-OF-SELECTION.
  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
    EXPORTING
      i_tab_raw_data       = it_type
      i_filename           = lv_file1
    TABLES
      i_tab_converted_data = it
    EXCEPTIONS
      conversion_failed    = 1
      OTHERS               = 2.

  DESCRIBE FIELD it: LENGTH n1 IN BYTE MODE.
  n = n1.

  PERFORM open_group.
  LOOP AT it INTO wa.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa-equnr
      IMPORTING
        output = wa-equnr.

    ls_equipment          = wa-equnr.

    CALL FUNCTION 'BAPI_EQUI_GETDETAIL'
      EXPORTING
        equipment         = ls_equipment
      IMPORTING
        data_general_exp  = ls_general_exp
        data_specific_exp = ls_specific_exp
        data_fleet_exp    = ls_fleet_exp
        return            = ls_return.

    CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
      EXPORTING
        date_internal            = ls_general_exp-start_from"SY-DATUM
      IMPORTING
        date_external            = lv_date"ls_general_exp-start_from
      EXCEPTIONS
        date_internal_is_invalid = 1
        OTHERS                   = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.


    PERFORM bdc_dynpro      USING 'SAPMIEQ0' '0100'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'RM63E-EQUNR'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '/00'.
    PERFORM bdc_field       USING 'RM63E-EQUNR'
                                  wa-equnr."'10000001'.
    PERFORM bdc_dynpro      USING 'SAPMIEQ0' '0101'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '=T\04'.
    PERFORM bdc_field       USING 'ITOB-INBDT'
                                  lv_date.
*                                  ls_general_exp-start_from."itob-inbdt."'01.06.2010'.
    PERFORM bdc_field       USING 'ITOB-EQART'
                                  ls_general_exp-objecttype."itob-eqart."'1030'.
    PERFORM bdc_field       USING 'ITOB-HERST'
                                  ls_general_exp-manfacture."itob-herst."'shroff'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'ITOB-SHTXT'.
    PERFORM bdc_field       USING 'ITOB-SHTXT'
*                                  'D14 reactor'.
                                  ls_general_exp-descript."itob-shtxt."'D14 reactor'.
    PERFORM bdc_dynpro      USING 'SAPMIEQ0' '0101'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '=UEQU'.
    PERFORM bdc_field       USING 'ITOB-TIDNR'
                                  ls_specific_exp-techid."itob-tidnr."'R-101'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'ITOB-SHTXT'.
    PERFORM bdc_field       USING 'ITOB-SHTXT'
*                                  'D14 reactor'.
                                  ls_general_exp-descript."itob-shtxt."'D14 reactor'.
    PERFORM bdc_dynpro      USING 'SAPLIEL2' '0101'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '=SEDF'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'IEQINSTALL-HSTPS(01)'.
    PERFORM bdc_dynpro      USING 'SAPLIEL2' '0101'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '/00'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'IEQINSTALL-EQUNR(04)'.
    PERFORM bdc_field       USING 'IEQINSTALL-EQUNR(02)'
                                  wa-equnr1."'10000012'.
    PERFORM bdc_field       USING 'IEQINSTALL-EQUNR(03)'
                                  wa-equnr2."'10000012'.
    PERFORM bdc_field       USING 'IEQINSTALL-EQUNR(04)'
                                  wa-equnr3."'10000013'.
    PERFORM bdc_field       USING 'IEQINSTALL-EQUNR(05)'
                                  wa-equnr4."'10000014'.
    PERFORM bdc_field       USING 'IEQINSTALL-EQUNR(06)'
                                  wa-equnr5."'10000015'.
    PERFORM bdc_field       USING 'IEQINSTALL-EQUNR(07)'
                                  wa-equnr6."'10000017'.
    PERFORM bdc_field       USING 'IEQINSTALL-EQUNR(08)'
                                  wa-equnr7."'10000018'.
    PERFORM bdc_field       USING 'IEQINSTALL-EQUNR(09)'
                                  wa-equnr8."'10000018'.
    PERFORM bdc_field       USING 'IEQINSTALL-EQUNR(10)'
                                  wa-equnr9."'10000018'.
    PERFORM bdc_field       USING 'IEQINSTALL-EQUNR(11)'
                                  wa-equnr10."'10000018'.
    PERFORM bdc_field       USING 'IEQINSTALL-EQUNR(12)'
                                  wa-equnr11."'10000018'.
    PERFORM bdc_field       USING 'IEQINSTALL-EQUNR(13)'
                                  wa-equnr12."'10000018'.
    PERFORM bdc_field       USING 'IEQINSTALL-EQUNR(14)'
                                  wa-equnr13."'10000018'.
    PERFORM bdc_field       USING 'IEQINSTALL-EQUNR(15)'
                                  wa-equnr14.               "'10000018'
    PERFORM bdc_field       USING 'IEQINSTALL-EQUNR(16)'
                                  wa-equnr15.               "'10000018'
    PERFORM bdc_field       USING 'IEQINSTALL-EQUNR(17)'
                                  wa-equnr16.               "'10000018'
    PERFORM bdc_field       USING 'IEQINSTALL-EQUNR(18)'
                                  wa-equnr17.               "'10000018'
    PERFORM bdc_field       USING 'IEQINSTALL-EQUNR(19)'
                                  wa-equnr18.               "'10000018'
    PERFORM bdc_field       USING 'IEQINSTALL-EQUNR(20)'
                                  wa-equnr19.               "'10000018'
    PERFORM bdc_field       USING 'IEQINSTALL-EQUNR(21)'
                                  wa-equnr20.               "'10000018'
    PERFORM bdc_field       USING 'IEQINSTALL-EQUNR(22)'
                                  wa-equnr21.               "'10000018'
    PERFORM bdc_field       USING 'IEQINSTALL-EQUNR(23)'
                                  wa-equnr22.               "'10000018'
    PERFORM bdc_field       USING 'IEQINSTALL-EQUNR(24)'
                                  wa-equnr23.               "'10000018'
    PERFORM bdc_field       USING 'IEQINSTALL-EQUNR(25)'
                                  wa-equnr24.               "'10000018'
    PERFORM bdc_field       USING 'IEQINSTALL-EQUNR(26)'
                                  wa-equnr25.               "'10000018'
    PERFORM bdc_field       USING 'IEQINSTALL-EQUNR(27)'
                                  wa-equnr26.               "'10000018'
    PERFORM bdc_field       USING 'IEQINSTALL-EQUNR(28)'
                                  wa-equnr27.               "'10000018'
    PERFORM bdc_field       USING 'IEQINSTALL-EQUNR(29)'
                                  wa-equnr28.               "'10000018'
    PERFORM bdc_field       USING 'IEQINSTALL-EQUNR(30)'
                                  wa-equnr29.
    PERFORM bdc_field       USING 'IEQINSTALL-EQUNR(31)'
                                  wa-equnr30.
    PERFORM bdc_dynpro      USING 'SAPLIEL2' '0101'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '/ERW'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'IEQINSTALL-HSTPS(02)'.
    PERFORM bdc_dynpro      USING 'SAPMIEQ0' '0101'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '=BU'.
    PERFORM bdc_field       USING 'ITOB-TIDNR'
                                  ls_specific_exp-techid."itob-tidnr."'R-101'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'ITOB-SHTXT'.
    PERFORM bdc_field       USING 'ITOB-SHTXT'
*                                  'D14 reactor'.
                                  ls_general_exp-descript."itob-shtxt."'D14 reactor'.
    PERFORM bdc_transaction USING 'IE02'.
    CLEAR wa-equnr.
  ENDLOOP.
  PERFORM close_group.
