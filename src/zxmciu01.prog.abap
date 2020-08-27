*&---------------------------------------------------------------------*
*&  Include           ZXMCIU01
*&---------------------------------------------------------------------*
*& Purpose: Automatically call print program ZNWP(t-code) when saving/creating notification type ZA, ZB, ZD, ZS, ZT via IW21
*&---------------------------------------------------------------------*
break: 6010859, 10106.
" IRDK932559: MM: S_K: IW21: ZPM_IW21: Allow print for ZA, ZB, ZD, ZS - Wednesday, June 20, 2018 16:28:10
if sy-tcode = 'IW21'.
  commit work.
  if n_viqmel-qmart = 'ZT' or n_viqmel-qmart = 'ZA' or n_viqmel-qmart = 'ZB' or n_viqmel-qmart = 'ZD' or n_viqmel-qmart = 'ZS'.
    " tcode - znwp
    submit zpnwp via selection-screen
    with so_qmnum eq n_viqmel-qmnum and return.
  endif.
endif.

*<< Below code is related to fund managment enhancement - inserting record in deviration rule for Order to Funds Center.
*** FS given by KPMG consultant Gasia ; INDOFIL Developer 10106 - TR: IHDK907939 , Date : 27.08.2020

IF ( sy-tcode = 'IW21' or sy-tcode = 'IW22' or sy-tcode = 'IW23')
    AND sy-ucomm = 'BU'
    AND N_VIQMEL-aufnr is NOT INITIAL
    AND N_VIQMEL-KOSTL is NOT INITIAL.

 DATA: ZFUND_CENTER TYPE FISTL.
 DATA: lv_table1 TYPE string,
       lv_table2 TYPE string.
 DATA: lv_exists TYPE REF TO data,
       lv_exists2 TYPE REF TO data.
 CREATE DATA lv_exists TYPE fmfmoaihd3000017.
 CREATE DATA lv_exists2 TYPE FMFMOAIHD3000022.
 ASSIGN lv_exists2->* TO FIELD-SYMBOL(<fs>).

*FMFMOAIHP3000016 in PRD (The same table will be varying in
*DEV- FMFMOAIHD3000022 &
*QAS - FMFMOAIHQ3000016).
*Refer tables for  Derivation rule: Cost Centre to Fund Centre
*DEV : FMFMOAIHD3000017
*QA: FMFMOAIHQ3000014
*PRD: FMFMOAIHP3000015


 IF sy-sysid EQ 'IHD'.
    lv_table1 = 'FMFMOAIHD3000017'.

  ELSEIF sy-sysid EQ 'IHQ'.
    lv_table1 = 'FMFMOAIHQ3000014'.

  ELSEIF sy-sysid EQ 'IHP'.
    lv_table1 = 'FMFMOAIHP3000015'.

  ENDIF.

 SELECT  SINGLE target1 FROM (lv_table1)
    INTO ZFUND_CENTER
    WHERE ( SOUR1_FROM <= N_VIQMEL-KOSTL AND SOUR1_TO >=  N_VIQMEL-KOSTL ).
  IF ZFUND_CENTER IS NOT INITIAL.
**    insert row in derivative rule .

*SOUR1_FROM - AUFNR
*SOUR1_TO  - AUFNR
*VALID_FROM - SY-datum
*
*TARGET1  - ZFUND_CENTER
*
*ADDED_BY - Sy-uname
*ADDED_ON - Sy-datum

*insert INTO
  ENDIF.

ENDIF.
