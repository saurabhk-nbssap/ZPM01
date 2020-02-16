*&---------------------------------------------------------------------*
*&  Include           ZPM_LOTO_PERMITS_TOP
*&---------------------------------------------------------------------*
*Structure for checking values are available
TYPES: BEGIN OF ty_afih,
        aufnr TYPE afih-aufnr,
        iwerk TYPE afih-iwerk,
      END OF ty_afih.

DATA: fm_name   TYPE rs38l_fnam,  "Function module name for SF
      gv_aufnr  TYPE afih-aufnr,
      gv_iwerk  TYPE afih-iwerk,
      gwa_afih  TYPE ty_afih,     "Work area for checking values are available
      ok_code   TYPE sy-ucomm.

CONSTANTS : gv_onli   TYPE sy-ucomm VALUE 'ONLI',
            gv_f_name TYPE tdsfname VALUE 'ZPM_SF_LOTO'.

**&=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--------*
**  Selection Screen (INPUT)
**&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS : p_plant TYPE afih-iwerk OBLIGATORY,       "Plant
             p_ornum TYPE afih-aufnr OBLIGATORY.       "Maintanence order number
SELECTION-SCREEN END OF BLOCK b1.
**&---------------------------------------------------------------------*
