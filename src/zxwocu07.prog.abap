*&---------------------------------------------------------------------*
*&  Include           ZXWOCU07
*&---------------------------------------------------------------------*
** Validations
** Either Functional Location is necessary or Equipment Number Necessary for creating Maintenance Order
if caufvd_imp-tplnr is initial and caufvd_imp-equnr is initial.

  message e000(zpm01) with 'Either of Funct. Loc.or Equipment No is Mandatory'.


endif.
