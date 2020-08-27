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

 SELECT  SINGLE target1 FROM FMFMOAIHD3000017 INTO ZFUND_CENTER
    WHERE ( SOUR1_FROM <= N_VIQMEL-KOSTL AND SOUR1_TO >=  N_VIQMEL-KOSTL ).
  IF ZFUND_CENTER IS NOT INITIAL.
**    insert row in derivative rule .


  ENDIF.

ENDIF.
