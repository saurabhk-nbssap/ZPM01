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
