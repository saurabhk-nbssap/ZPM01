*&---------------------------------------------------------------------*
*&  Include           ZXMCIU01
*&---------------------------------------------------------------------*
*& Purpose: Automatically call print program ZNWP(t-code) when saving/creating notification type ZA, ZB, ZD, ZS, ZT via IW21
*&---------------------------------------------------------------------*

break: 6010859, 10106.
* IRDK932559: MM: S_K: IW21: ZPM_IW21: Allow print for ZA, ZB, ZD, ZS - Wednesday, June 20, 2018 16:28:10
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

IF ( ( sy-tcode = 'IW21' or sy-tcode = 'IW22' or sy-tcode = 'IW23')
    AND sy-ucomm = 'BU'
    AND N_VIQMEL-aufnr is NOT INITIAL
    AND N_VIQMEL-KOSTL is NOT INITIAL )
    or ( ( sy-tcode = 'IW33' or sy-tcode = 'IW32' or sy-tcode = 'IW31')
    AND sy-ucomm = 'BU'
    AND I_MCIPMB-aufnr is NOT INITIAL
    AND I_MCIPMB-KOSTL is NOT INITIAL ) .

 DATA: ZFUND_CENTER TYPE FISTL.
 DATA: lv_table1 TYPE string,
       lv_table2 TYPE string.
 DATA: lv_exists TYPE REF TO data,
       lv_exists2 TYPE REF TO data.

*I_MCIPMB-KOSTL
*FMFMOAIHP3000016 in PRD (The same table will be varying in
*DEV- FMFMOAIHD3000022 &
*QAS - FMFMOAIHQ3000016).
*Refer tables for  Derivation rule: Cost Centre to Fund Centre
*DEV : FMFMOAIHD3000017
*QA: FMFMOAIHQ3000014
*PRD: FMFMOAIHP3000015

 IF sy-sysid EQ 'IHD'.
    lv_table1 = 'FMFMOAIHD3000017'.
    lv_table2 = 'FMFMOAIHD3000022'.
  ELSEIF sy-sysid EQ 'IHQ'.
    lv_table1 = 'FMFMOAIHQ3000014'.
    lv_table2 = 'FMFMOAIHQ3000016'.
  ELSEIF sy-sysid EQ 'IHP'.
    lv_table1 = 'FMFMOAIHP3000015'.
    lv_table2 = 'FMFMOAIHP3000016'.
  ENDIF.

 CREATE DATA lv_exists2 TYPE (lv_table2).
 ASSIGN lv_exists2->* TO FIELD-SYMBOL(<fs>).

 IF ( sy-tcode = 'IW21' or sy-tcode = 'IW22' or sy-tcode = 'IW23') .
   SELECT  SINGLE target1 FROM (lv_table1)
    INTO ZFUND_CENTER
    WHERE ( SOUR1_FROM <= N_VIQMEL-KOSTL AND SOUR1_TO >=  N_VIQMEL-KOSTL ).
 ELSEIF ( sy-tcode = 'IW31' or sy-tcode = 'IW32' or sy-tcode = 'IW33') .
   SELECT  SINGLE target1 FROM (lv_table1)
    INTO ZFUND_CENTER
    WHERE ( SOUR1_FROM <= I_MCIPMB-KOSTL AND SOUR1_TO >=  I_MCIPMB-KOSTL ).
 ENDIF.


  IF ZFUND_CENTER IS NOT INITIAL.
**    insert row in derivative rule .

  ASSIGN COMPONENT 'SOUR1_FROM' OF STRUCTURE <fs> TO FIELD-SYMBOL(<fs_value>).
  IF sy-subrc eq 0.
    IF N_VIQMEL-aufnr IS NOT INITIAL .
     <fs_value> = N_VIQMEL-aufnr.
    ENDIF.
    IF I_MCIPMB-aufnr IS NOT INITIAL .
     <fs_value> = I_MCIPMB-aufnr.
    ENDIF.
  ENDIF.

  UNASSIGN <fs_value>.
  ASSIGN COMPONENT 'SOUR1_TO' OF STRUCTURE <fs> TO <fs_value>.
  IF sy-subrc eq 0.
    if N_VIQMEL-aufnr IS NOT INITIAL .
    <fs_value> = N_VIQMEL-aufnr.
    endif.
    if I_MCIPMB-aufnr is NOT INITIAL.
    <fs_value> = I_MCIPMB-aufnr.
    endif.

  ENDIF.

  UNASSIGN <fs_value>.
  ASSIGN COMPONENT 'VALID_FROM' OF STRUCTURE <fs> TO <fs_value>.
  IF sy-subrc eq 0.
*    <fs_value> = sy-datum." if we entered System cdate incosistancy error in comming hence we are inserting date as 01.01.0001.
     <fs_value> = '00010101'.
  ENDIF.

  UNASSIGN <fs_value>.
  ASSIGN COMPONENT 'TARGET1' OF STRUCTURE <fs> TO <fs_value>.
  IF sy-subrc eq 0.
    <fs_value> = zfund_center.
  ENDIF.

  UNASSIGN <fs_value>.
  ASSIGN COMPONENT 'ADDED_BY' OF STRUCTURE <fs> TO <fs_value>.
  IF sy-subrc eq 0.
    <fs_value> = sy-uname.
  ENDIF.

  UNASSIGN <fs_value>.
  ASSIGN COMPONENT 'ADDED_ON' OF STRUCTURE <fs> TO <fs_value>.
  IF sy-subrc eq 0.
    <fs_value> = sy-datum.
  ENDIF.

  modify (lv_table2) FROM <fs>.
*  IF sy-subrc eq 0.
*    COMMIT WORK.
*  ELSE.
*    ROLLBACK WORK.
*  ENDIF.
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
