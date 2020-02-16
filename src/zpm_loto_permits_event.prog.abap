*&---------------------------------------------------------------------*
*&  Include           ZPM_LOTO_PERMITS_EVENT
*&---------------------------------------------------------------------*
**&---------------------------------------------------------------------*
AT SELECTION-SCREEN.
**&---------------------------------------------------------------------*
*checking values are available
  ok_code = sy-ucomm.
  IF ok_code = gv_onli.
    SELECT SINGLE aufnr
                  iwerk
      FROM afih INTO gwa_afih
      WHERE aufnr = p_ornum
      AND   iwerk = p_plant.
    IF sy-subrc <> 0.
      MESSAGE e002(zpm).
    ENDIF.
  ENDIF.
**&---------------------------------------------------------------------*
AT SELECTION-SCREEN ON p_plant.
**&---------------------------------------------------------------------*
*For checking plant exsist
  SELECT SINGLE werks FROM t001w INTO gv_iwerk WHERE werks = p_plant.
  IF sy-subrc <> 0.
    MESSAGE e000(zpm).
  ENDIF.
**&---------------------------------------------------------------------*
AT SELECTION-SCREEN ON p_ornum.
**&---------------------------------------------------------------------*
*For checking maintanence order number exsist
  SELECT SINGLE aufnr FROM afih INTO gv_aufnr WHERE aufnr = p_ornum.
  IF sy-subrc <> 0.
    MESSAGE e001(zpm).
  ENDIF.
**&---------------------------------------------------------------------*
START-OF-SELECTION.
**&---------------------------------------------------------------------*
  PERFORM display_smartform.
