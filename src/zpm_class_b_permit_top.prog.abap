*&---------------------------------------------------------------------*
*&  Include           ZPM_CLASS_B_PERMIT_TOP
*&---------------------------------------------------------------------*
*Structure for checking values are available
types: begin of ty_afih,
        aufnr type afih-aufnr,
        iwerk type afih-iwerk,
      end of ty_afih.

data: fm_name   type rs38l_fnam,    "Function module name for SF
      gv_aufnr  type afih-aufnr,
      gv_iwerk  type afih-iwerk,
      gwa_afih   type ty_afih,       "Work area for checking values are available
      ok_code   type sy-ucomm.

constants : gv_onli   type sy-ucomm value 'ONLI',
            gv_f_name type tdsfname value 'ZPM_SF_CLASS_B'.

**&=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=----------*
**  Selection Screen (INPUT)
**&---------------------------------------------------------------------*
selection-screen begin of block b1 with frame title text-001.
parameters : p_plant type afih-iwerk obligatory,       "Plant
             p_ornum type afih-aufnr obligatory.       "Maintanence order number
selection-screen end of block b1.
