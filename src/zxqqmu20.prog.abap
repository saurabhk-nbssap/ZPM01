*&---------------------------------------------------------------------*
*&  Include           ZXQQMU20
*&---------------------------------------------------------------------*
** Validations
** Either Functional Location is necessary or Equipment Number Necessary for creating Maintenance Order

if i_viqmel-qmart EQ 'M1' OR i_viqmel-qmart EQ 'M2' OR i_viqmel-qmart EQ 'M3'.
if i_viqmel-tplnr is initial and i_viqmel-equnr is initial.

  message e000(zpm01) with 'Either of Func. Loc.or Equipment No is Mandatory'.


endif.
ENDIF.
