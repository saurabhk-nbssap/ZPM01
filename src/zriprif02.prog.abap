***INCLUDE RIPRIF02 .
*INCLUDE liprtf03.                      " common routines included
*----------------------------------------------------------------------*
*       FORM ORDER_DATA_IMPORT                                         *
*----------------------------------------------------------------------*
*       Import data to be used in Print ABAPS RIPRxxnn (ORDER PRINTING)*
*----------------------------------------------------------------------*
*  -->  p1  Export made in LIPRTF02, Function                          *
*           PM_ORDER_PRINT_CONTROL                                     *
*----------------------------------------------------------------------*
FORM ORDER_DATA_IMPORT.
*... get the order data from memory, not there ABEND !!!!!!
  PERFORM ONLY_ORDER_DATA_IMPORT       " see  LIPRTF03.
      tables
       OP_PRINT_TAB
       KBEDP_TAB
       IHPAD_TAB
       IHSG_TAB
       IHGNS_TAB
       IAFVGD
       IRIPW0
       IRESBD
       IAFFHD
      changing
       CAUFVD
       RIWO1
       ILOA.
  IF SYST-SUBRC <> 0.
    MESSAGE A650(ID).                  " import failed
  ENDIF.
*-> clear header lines to avoid error, if print redir. is called
*   before loop at iafvgd or iresbd
  CLEAR AFVGD.
  CLEAR RESBD.
*-> read CAUFVD-INNAM because it is sometimes not filled
  IF NOT CAUFVD-INGPR IS INITIAL AND
     CAUFVD-INNAM IS INITIAL.
*-> read text
    CALL FUNCTION 'T024I_READ'
      EXPORTING
        INGRP    = CAUFVD-INGPR
        IWERK    = CAUFVD-IWERK
      IMPORTING
        STRUCT   = T024I
      EXCEPTIONS
        NO_ENTRY = 1
        OTHERS   = 2.
    IF SY-SUBRC = 0.
      CAUFVD-INNAM = T024I-INNAM.
    ENDIF.
  ENDIF.

  DESCRIBE TABLE OP_PRINT_TAB LINES OP_ENTRIES.
*... now get the general information, Paper name, language device etc
  PERFORM GENERAL_DATA_IMPORT.
  IF ORIGINAL_PRINT_LANGUAGE <> PRINT_LANGUAGE.
    PERFORM READ_OBJECT_TEXTS USING RIWO1 PRINT_LANGUAGE.
  ENDIF.
* PERFORM LEFT_JUSTIFY_MAT USING CAUFVD-BAUTL.
  REFRESH ORDER_IHPAD_TAB.
*... move the passed notification partner details to special table
*... The  IHPAD_TAB is used generally for printing partner addresses
*... Not just Order partner but also equipment partner.
  LOOP AT IHPAD_TAB.
    ORDER_IHPAD_TAB = IHPAD_TAB.
    APPEND ORDER_IHPAD_TAB.
  ENDLOOP.
*... If the order has a notification read it
*... The structure viqmel can then be prepared for the customer
*... so he can print details from Notification on order papers.
  CLEAR VIQMEL.
  IF NOT CAUFVD-QMNUM IS INITIAL. " header notification exists
*-> save RIWO1 from order in WRIWO1
    REFRESH IRIWO1.
    IRIWO1 = RIWO1. " from order
    APPEND IRIWO1.
*-> DO the same like FORM data_import in RIPRIF01
    IMPORT IVIQMEL                       " ... Notification
         IVIQMFE                       " ... positions
         IVIQMMA                       " ... codes
         IVIQMSM                       " ... codes
         IVIQMUR                       " ... codes
         IQKAT                         " ... Catalogue
         IHPAD_TAB                     " ... partner table
         RIWO1                         " ... Texts from Objects
         RIWO00             " ... Dialog structure with status info
         FROM MEMORY ID ID_IPRT_STRUCT.
    IF SYST-SUBRC <> 0.
      MESSAGE A650(ID).                  " import failed
    ENDIF.
    IF ORIGINAL_PRINT_LANGUAGE <> PRINT_LANGUAGE.
      PERFORM READ_OBJECT_TEXTS USING RIWO1 PRINT_LANGUAGE.
    ENDIF.
*... special fields already used in forms are supported
*... The following lines are for upwards compatibilty defined
*... The fileds can be addressed directly via RIWO1- now
    MAKT-MAKTX =  RIWO1-BAUTX.
    EQKT-EQKTX =  RIWO1-EQTXT.
    IFLO-PLTXT =  RIWO1-PLTXT.
*-> save RIWO1 from notif in WRIWO1 as well
    IRIWO1 = RIWO1. " from notif
    APPEND IRIWO1.
*-> restore RIWO1 from order
    READ TABLE IRIWO1 WITH KEY OBJNR = CAUFVD-OBJNR.
    RIWO1 = IRIWO1.

    VIQMEL = IVIQMEL.         " Both workareas set. View needed in
    "      SAPSCRIPT
*-> clear header line to avoid error, if print redir. is called
*   before loop at iviqmfe
    CLEAR WQMFE.
    REFRESH NOTIF_IHPAD_TAB.
*... move the passed notification partner details to special table
*... The  IHPAD_TAB is used generally for printing partner addresses
*... Not just Notifcation partner but also equipment partner.
    LOOP AT IHPAD_TAB.
      NOTIF_IHPAD_TAB = IHPAD_TAB.
      APPEND NOTIF_IHPAD_TAB.
    ENDLOOP.
*-> restore ihpad_tab from order
    REFRESH IHPAD_TAB.
    LOOP AT ORDER_IHPAD_TAB.
      IHPAD_TAB = ORDER_IHPAD_TAB.
      APPEND IHPAD_TAB.
    ENDLOOP.
    PERFORM READ_CATALOGUE_TABLES        " READ Quality control tables
            USING VIQMEL-QMART.          "     for catalog categories
    PERFORM READ_CATALOGUE_TEXTS.

    " Reported by
    if viqmel-qmnam is initial.
      select single qmnam from viqmel into viqmel-qmnam where qmnum = caufvd-qmnum.
    endif.

    select single name_first, name_last from v_usr_name into ( @data(first), @data(last) ) where bname = @viqmel-qmnam.

    clear username.
    username = first && ` ` && last.

    " Person responsible: IRDK933219
    select single objnr from qmel into @data(objnr) where qmnum = @caufvd-qmnum.

    if objnr is not initial.
      select single parnr from ihpa into @data(parnr) where objnr = @objnr and parvw = 'VW'.

      if parnr is not initial.
        clear: first, last.
        shift parnr left deleting leading '0'.
        select single name_first name_last from v_usr_name into ( first, last ) where bname = parnr.

        clear userresp.
        userresp = first && ` ` && last.
      endif.
    endif.
  ENDIF.
ENDFORM.                    "ORDER_DATA_IMPORT
*---------------------------------------------------------------------*
*       FORM READ_ORDER_TEXT_TABLES                                   *
*---------------------------------------------------------------------*
*...    MUST BE PERFORMED BEFORE USING THE ORDER HEADER               *
*       Read ATAB text tables for use in ORDER Printing               *
*       Single singles for texts at order header level                *
*       Also set the ILOA if it is not available                      *
*       Text keys from TCO09 are also read for long texts             *
*       Other general workarea for the order header are set here      *
*---------------------------------------------------------------------*
FORM READ_ORDER_TEXT_TABLES.
  SELECT SINGLE * FROM  T003P          " order type text
         WHERE  SPRAS       = PRINT_LANGUAGE
         AND    AUART       = CAUFVD-AUART.
  SELECT SINGLE * FROM  T356_T         " priority texts
         WHERE  SPRAS       = PRINT_LANGUAGE
         AND    ARTPR       = CAUFVD-ARTPR
         AND    PRIOK       = CAUFVD-PRIOK.
  SELECT SINGLE * FROM  T352R
         WHERE  IWERK       = CAUFVD-IWERK    " revision numbers
         AND    REVNR       = CAUFVD-REVNR.

*    ---added for company name
  SELECT SINGLE * FROM  T001
     WHERE  BUKRS       = CAUFVD-BUKRS.
*-> read text for workcenter with print-language
  CALL FUNCTION 'CR_WORKSTATION_READ'
    EXPORTING
      ID        = CAUFVD-GEWRK
      MSGTY     = 'W'
    IMPORTING
      ECRHD     = CRHD
    EXCEPTIONS
      NOT_FOUND = 01.
  IF SY-SUBRC = 0.
    IF CRHD-OBJTY <> CRTX-OBJTY OR
       CRHD-OBJID <> CRTX-OBJID OR
       PRINT_LANGUAGE <> CRTX-SPRAS.
      SELECT SINGLE * FROM  CRTX
             WHERE  OBJTY       = CRHD-OBJTY
             AND    OBJID       = CRHD-OBJID
             AND    SPRAS       = PRINT_LANGUAGE.
    ENDIF.
    IF SY-SUBRC = 0.
      CAUFVD-VATXT = CRTX-KTEXT.
    ELSE.
      CLEAR CAUFVD-VATXT.
    ENDIF.
  ENDIF.
*... If the ILOA (Accounting and location info) is not available
*... but the Order has ILOA number then read it directly
  IF  NOT CAUFVD-ILOAN IS INITIAL AND ILOA IS INITIAL.
    SELECT SINGLE * FROM  ILOA WHERE ILOAN = CAUFVD-ILOAN.
  ENDIF.
*... Read the STXH long text keys for print long texts
  SELECT SINGLE * FROM  TCO09
         WHERE  AUFTY       = C_PM_ORDER_TYPE.
*... build the TEXTNAME for the order header longtext
  CALL FUNCTION 'CO_ZK_TEXTKEY_CAUFV'
    EXPORTING
      AUFNR = CAUFVD-AUFNR
    IMPORTING
      LTSCH = STXH-TDNAME.
*-> reread order status text in print language
  CALL FUNCTION 'STATUS_TEXT_EDIT'
    EXPORTING
      FLG_USER_STAT    = YES
      OBJNR            = CAUFVD-OBJNR
      ONLY_ACTIVE      = YES
      SPRAS            = PRINT_LANGUAGE
    IMPORTING
      LINE             = CAUFVD-STTXT
      USER_LINE        = CAUFVD-ASTTX
    EXCEPTIONS
      OBJECT_NOT_FOUND = 1
      OTHERS           = 2.
ENDFORM.                    "READ_ORDER_TEXT_TABLES

*----------------------------------------------------------------------*
*       FORM READ_OP_TEXT_TABLES.                                      *
*----------------------------------------------------------------------*
*       Read text tables / Get text elements for operations            *
*----------------------------------------------------------------------*
*  -->  AFVGD     Global                                               *
*----------------------------------------------------------------------*
FORM READ_OP_TEXT_TABLES.
  CALL FUNCTION 'CR_WORKSTATION_READ'  "  Read the workstation
       EXPORTING                       "  for texts to print
            ID    = AFVGD-ARBID                             "
            MSGTY = 'W'                                     "
       IMPORTING                       "  RCR01 work area is
            ARBPL = RCR01-ARBPL        "  used in FORMS
            ECRHD = CRHD                                    "
            KTEXT = RCR01-KTEXT                             "
            WERKS = RCR01-WERKS                             "
       EXCEPTIONS                                           "
            NOT_FOUND = 01.
*-> read text for workcenter with print-language
  IF SY-SUBRC = 0.
    IF CRHD-OBJTY <> CRTX-OBJTY OR
       CRHD-OBJID <> CRTX-OBJID OR
       PRINT_LANGUAGE <> CRTX-SPRAS.
      SELECT SINGLE * FROM  CRTX
             WHERE  OBJTY       = CRHD-OBJTY
             AND    OBJID       = CRHD-OBJID
             AND    SPRAS       = PRINT_LANGUAGE.
    ENDIF.
    IF SY-SUBRC = 0.
      RCR01-KTEXT = CRTX-KTEXT.
    ELSE.
      CLEAR RCR01-KTEXT.
    ENDIF.
  ENDIF.
  CALL FUNCTION 'CO_TA_T430_READ'      "  Read control key
       EXPORTING                       "  to text for print flags
            PLNAW = '*'                                     "
            SPRAS = PRINT_LANGUAGE     "  The controls are checked
            STEUS = AFVGD-STEUS        "  before printing certain
       IMPORTING                       "  operations types
            STRUCT = T430                                   "
            TEXT   = T430T-TXT                              "
       EXCEPTIONS                                           "
            NO_ENTRY = 01.                                  "
  AFVGD-FLG_FRD = T430-LIEF.           "ExtProc flag from cont key
*-> reread operation status text in print language
  CALL FUNCTION 'STATUS_TEXT_EDIT'
    EXPORTING
      OBJNR            = AFVGD-OBJNR
      ONLY_ACTIVE      = YES
      SPRAS            = PRINT_LANGUAGE
    IMPORTING
      LINE             = AFVGD-VSTTXT
    EXCEPTIONS
      OBJECT_NOT_FOUND = 1
      OTHERS           = 2.
*-> ...and all related materials
  IRESBD = SPACE.
  LOOP AT IRESBD WHERE VORNR = AFVGD-VORNR.
*-> reread order status text in print language
    CALL FUNCTION 'STATUS_TEXT_EDIT'
      EXPORTING
        OBJNR            = IRESBD-OBJNR
        ONLY_ACTIVE      = YES
        SPRAS            = PRINT_LANGUAGE
      IMPORTING
        LINE             = IRESBD-STTXT
      EXCEPTIONS
        OBJECT_NOT_FOUND = 1
        OTHERS           = 2.
    MODIFY IRESBD.
  ENDLOOP.
ENDFORM.                    "READ_OP_TEXT_TABLES

*----------------------------------------------------------------------*
*       FORM MAINT_PLAN_DETAIL.                                        *
*----------------------------------------------------------------------*
*       Print the maintenance plan header and longtext                 *
*----------------------------------------------------------------------*
FORM MAINT_PLAN_DETAIL.
  CALL FUNCTION 'WRITE_FORM'
    EXPORTING
      ELEMENT = 'MAINT_PLAN_DETAIL'
      WINDOW  = 'MAIN'.
  PERFORM PRINT_MAINT_PLAN_LONGTEXT.
ENDFORM.                    "MAINT_PLAN_DETAIL

*----------------------------------------------------------------------*
*       FORM ORDER_HEADER_DETAIL.                                      *
*----------------------------------------------------------------------*
*       Process order header details, SAPSCRIPT element                *
*----------------------------------------------------------------------*
FORM ORDER_HEADER_DETAIL.
  DATA SAVE_RIWO1 LIKE RIWO1.

  CLEAR SAVE_RIWO1.
*-> get RIWO1 of order (could be necessary within notif printing)
  IF RIWO1-OBJNR <> CAUFVD-OBJNR.
    SAVE_RIWO1 = RIWO1.
    READ TABLE IRIWO1 WITH KEY OBJNR = CAUFVD-OBJNR.
    RIWO1 = IRIWO1.
  ENDIF.
*... set by default order header to Detail
  CALL FUNCTION 'WRITE_FORM'           " Print pos detail.
       EXPORTING
            ELEMENT   = 'ORDER_HEADER_DETAIL'
            WINDOW    = 'MAIN'.
*-> restore RIWO1 if saved above
  IF NOT SAVE_RIWO1 IS INITIAL.
    RIWO1 = SAVE_RIWO1.
  ENDIF.
*-> print longtext
  PERFORM PRINT_ORDER_LONGTEXT.
ENDFORM.                    "ORDER_HEADER_DETAIL

*----------------------------------------------------------------------*
*       FORM ORDER_HEADER_SHORT                                        *
*----------------------------------------------------------------------*
*       Process order header details, SAPSCRIPT element                *
*----------------------------------------------------------------------*
FORM ORDER_HEADER_SHORT.
  DATA SAVE_RIWO1 LIKE RIWO1.

  CLEAR SAVE_RIWO1.
*-> get RIWO1 of order (could be necessary within notif printing)
  IF RIWO1-OBJNR <> CAUFVD-OBJNR.
    SAVE_RIWO1 = RIWO1.
    READ TABLE IRIWO1 WITH KEY OBJNR = CAUFVD-OBJNR.
    RIWO1 = IRIWO1.
  ENDIF.
*... set by default order header to Detail
  CALL FUNCTION 'WRITE_FORM'           " Print pos detail.
       EXPORTING
            ELEMENT   = 'ORDER_HEADER_SHORT'
            WINDOW    = 'MAIN'.
*-> restore RIWO1 if saved above
  IF NOT SAVE_RIWO1 IS INITIAL.
    RIWO1 = SAVE_RIWO1.
  ENDIF.
*-> print longtext
  PERFORM PRINT_ORDER_LONGTEXT.
ENDFORM.                    "ORDER_HEADER_SHORT

*----------------------------------------------------------------------*
*       FORM ORDER_OPERATIONS.                                         *
*----------------------------------------------------------------------*
*       Notification Details are printed here.                         *
*       Text ELEMENT used    OPERATION                                 *
*----------------------------------------------------------------------*
FORM ORDER_OPERATIONS.
  IAFVGD = SPACE.
  LOOP AT IAFVGD WHERE AUFPL = CAUFVD-AUFPL. "loop on operations
    AFVGD = IAFVGD.                    " Set workarea for SAPSCRIPT
*... for each operation
    PERFORM CHECK_PRINT_STATUS USING AFVGD-OBJNR
                                     WWORKPAPER-PM_DELTA_P
                                     RC.
    CHECK RC = 0.
    IF OP_ENTRIES > 0.                 " single operation print active
      LOOP AT OP_PRINT_TAB WHERE
              FLG_SEL = 'X'
         AND  VORNR   = AFVGD-VORNR    " was the operation selected
         AND  UVORN   = AFVGD-UVORN.   " for print ???
      ENDLOOP.
      CHECK SYST-SUBRC = 0.            " should this op be printed
    ENDIF.
    PERFORM READ_OP_TEXT_TABLES.       "operation text tables
    " T430 is now available
*... check that the operation should be printed based on the
*... control key.
    CHECK T430-VRGD = YES.             " jump to next operation

    " when oper not marked for print
    CALL FUNCTION 'WRITE_FORM'                              "
         EXPORTING
           ELEMENT   = 'OPERATION'     " main operation details
           WINDOW    = 'MAIN'.
*... now print either the interal or external operation details
*... FLAG AFVGD-FLG_FRD    Set based on Control key  see read_op_texts
    data : wa_ven_name like lfa1-name1.

    CASE AFVGD-FLG_FRD.
      WHEN '+'.
        clear lfa1.
    select single * from lfa1
     where lifnr = AFVGD-lifnr.
        CALL FUNCTION 'WRITE_FORM'
          EXPORTING
            ELEMENT = 'EXTERNAL_WORK'
            WINDOW  = 'MAIN'.
      WHEN SPACE.
        CALL FUNCTION 'WRITE_FORM'
          EXPORTING
            ELEMENT = 'INTERNAL_WORK'  " main operation details
            WINDOW  = 'MAIN'.
      WHEN OTHERS.
        CALL FUNCTION 'WRITE_FORM'
          EXPORTING
            ELEMENT = 'INTERNAL_WORK'  " main operation details
            WINDOW  = 'MAIN'.
        CALL FUNCTION 'WRITE_FORM'
          EXPORTING
            ELEMENT = 'EXTERNAL_WORK'
            WINDOW  = 'MAIN'.
    ENDCASE.
    PERFORM PRINT_OPERATION_TEXT.      " Longtext to operation
  ENDLOOP.
ENDFORM.                    "ORDER_OPERATIONS

*----------------------------------------------------------------------*
*       FORM OBJECT_LIST.                                              *
*----------------------------------------------------------------------*
*       List objects fro the order                                     *
*----------------------------------------------------------------------*
*  -->  IRIPW0    Global internal table with RIPW0 records             *
*----------------------------------------------------------------------*
FORM OBJECT_LIST USING NEW_PAGE_FLAG TYPE CLIKE.
  DESCRIBE TABLE IRIPW0 LINES ENTRIES.
  CHECK ENTRIES > 0.                   " something to print
  IF NEW_PAGE_FLAG = YES.
    CALL FUNCTION 'CONTROL_FORM'       " start on new page
         EXPORTING
              COMMAND = 'NEW-PAGE'.
    CALL FUNCTION 'WRITE_FORM'
      EXPORTING
        ELEMENT = 'OBJECT_TITLE' " Order object title
        WINDOW  = 'MAIN'.
  ENDIF.
  IRIPW0 = SPACE.
  SORT IRIPW0 BY SORTF OBZAE.
  LOOP AT IRIPW0 WHERE  LOKNZ = SPACE. " not deleted objects
    RIPW0 = IRIPW0.                    " set workarea for SAPSCRIPT
    CALL FUNCTION 'WRITE_FORM'
      EXPORTING
        ELEMENT = 'OBJECT_LIST'  " Order object list
        WINDOW  = 'MAIN'.
  ENDLOOP.
ENDFORM.                    "OBJECT_LIST

*----------------------------------------------------------------------*
*       FORM PRINT_maint_plan_LONGTEXT.                                *
*----------------------------------------------------------------------*
*       Longtext for the maintenance plan header                       *
*----------------------------------------------------------------------*
FORM PRINT_MAINT_PLAN_LONGTEXT.
*... print the text
ENDFORM.                    "PRINT_MAINT_PLAN_LONGTEXT

*---------------------------------------------------------------------*
*       FORM PRINT_ORDER_LONGTEXT                                     *
*---------------------------------------------------------------------*
FORM PRINT_ORDER_LONGTEXT.
*... LONGTEXT for Order
  IF NOT CAUFVD-LTEXT IS INITIAL.      "Is there a long text for ORDEr
    CALL FUNCTION 'CO_ZK_TEXTKEY_CAUFV'
      EXPORTING
        AUFNR = CAUFVD-AUFNR
      IMPORTING
        LTSCH = TEXT_OBJECT_NAME.
    STXH-TDNAME =  TEXT_OBJECT_NAME.
*... print the text
    PERFORM PRINT_LONGTEXT USING TCO09-OBJEC        "OBJECT
                                 TEXT_OBJECT_NAME   "NAME
                                 CAUFVD-LTEXT       "LANGUAGE
                                 TCO09-IDORD        "TYPE
                                 C_MAIN"which window
                                 C_START_LINE_NR
                                 C_LAST_LINE_NR
                                 YES.  "with underline around text
  ENDIF.
ENDFORM.                    "PRINT_ORDER_LONGTEXT

*----------------------------------------------------------------------*
*       FORM PRINT_OPERATION_TEXT.                                     *
*----------------------------------------------------------------------*
*       Print the long text for the notification Operation.            *
*       It will be printed exactly as found in text file STXH          *
*       inside the current form in window MAIN.                        *
*----------------------------------------------------------------------*
*  -->  AFVGD  Globally for key           must be available            *
*----------------------------------------------------------------------*
FORM PRINT_OPERATION_TEXT.
*... LONGTEXT for cause
  IF NOT AFVGD-TXTSP IS INITIAL.       "Is there a long text for OPER.
*... now build the text name
    CALL FUNCTION 'CO_ZK_TEXTKEY_AFVG'
      EXPORTING
        APLZL = AFVGD-APLZL
        AUFPL = AFVGD-AUFPL
      IMPORTING
        LTSCH = TEXT_OBJECT_NAME.
    STXH-TDNAME = TEXT_OBJECT_NAME.
*... print the text
    PERFORM PRINT_LONGTEXT USING TCO09-OBJEC        "OBJECT
                                 TEXT_OBJECT_NAME   "NAME
                                 AFVGD-TXTSP        "LANGUAGE
                                 TCO09-IDPOS        "TYPE
                                 C_MAIN"which window
                                 C_START_LINE_NR    "from line
                                 C_LAST_LINE_NR     "to line
                                 YES.  "with underline around text
  ENDIF.
ENDFORM.                    "PRINT_OPERATION_TEXT

*----------------------------------------------------------------------*
*       FORM SET_OP_PRINT_STATUS.                                      *
*----------------------------------------------------------------------*
*       Check the status print for the current operation               *
*       If it is not set then set it.  We just printed something       *
*::::   Potential status problem.                                      *
*       The update is perform over the order operations directly.      *
*       If since the last commit a total new status situation occurs   *
*       and this late setting of operation status causes problem
*
*----------------------------------------------------------------------*
FORM SET_OP_PRINT_STATUS.
  CALL FUNCTION 'STATUS_CHECK'
    EXPORTING
      OBJNR             = AFVGD-OBJNR  " check opertion
      STATUS            = STATUS_PRINT                      " for I0007
    EXCEPTIONS
      OBJECT_NOT_FOUND  = 01
      STATUS_NOT_ACTIVE = 02.
  IF SYST-SUBRC = 2.   " status is not yet active, so lets set now
*... only when we managed to find the operation status object
*... and the status printed is not yet set, it is ok to set
*... status printed
    REFRESH STAT_TAB.                  " build table for
    STAT_TAB-STAT = STATUS_PRINT.      " setting status
    APPEND STAT_TAB.
    CALL FUNCTION 'STATUS_CHANGE_INTERN'
         EXPORTING
              OBJNR               = AFVGD-OBJNR
         TABLES
              STATUS              = STAT_TAB
         EXCEPTIONS
              OBJECT_NOT_FOUND
              STATUS_INCONSISTENT
              STATUS_NOT_ALLOWED.
  ENDIF.
ENDFORM.                    "SET_OP_PRINT_STATUS

*---------------------------------------------------------------------*
*       FORM SET_RESB_PRINT_STATUS                                    *
*---------------------------------------------------------------------*
FORM SET_RESB_PRINT_STATUS.
  CALL FUNCTION 'STATUS_CHECK'
    EXPORTING
      OBJNR             = RESBD-OBJNR  " check Material comp
      STATUS            = STATUS_PRINT                      " for I0007
    EXCEPTIONS
      OBJECT_NOT_FOUND  = 01
      STATUS_NOT_ACTIVE = 02.
  IF SYST-SUBRC = 2.   " status is not yet active, so lets set now
    REFRESH STAT_TAB.                  " build table for
    STAT_TAB-STAT = STATUS_PRINT.      " setting status  I0007
    APPEND STAT_TAB.
    CALL FUNCTION 'STATUS_CHANGE_INTERN'
         EXPORTING
              OBJNR               = RESBD-OBJNR  "set material comp
         TABLES
              STATUS              = STAT_TAB  " as printed
         EXCEPTIONS
              OBJECT_NOT_FOUND
              STATUS_INCONSISTENT
              STATUS_NOT_ALLOWED.
  ENDIF.
*... must be posted separately viua status_update_dialog function
ENDFORM.                    "SET_RESB_PRINT_STATUS

*---------------------------------------------------------------------*
*       FORM STATUS_UPDATE                                            *
*---------------------------------------------------------------------*
FORM STATUS_UPDATE.
  CHECK DONT_LOG = SPACE.              "only change status if logging on
  CALL FUNCTION 'STATUS_UPDATE_DIALOG'."post status changes now
ENDFORM.                    "STATUS_UPDATE

*----------------------------------------------------------------------*
*       FORM Left_justify_mat                                          *
*----------------------------------------------------------------------*
*       If the material number is numeric then it should be left       *
*       justified to the form output.                                  *
*----------------------------------------------------------------------*
* -->  MAT    Material number to left justify / or part number         *
*----------------------------------------------------------------------*
FORM LEFT_JUSTIFY_MAT USING MATERIAL TYPE CLIKE.
  DATA: MAT LIKE RESBD-MATNR,
        MAT_NUM(18) TYPE N.
  MAT = MATERIAL.
  IF MAT CO '0123456789'.              " is matierial numeric
    MAT_NUM = MAT.
    WRITE MAT_NUM  TO MAT NO-ZERO.
    SHIFT MAT LEFT DELETING LEADING ' '.
  ENDIF.
  MATERIAL = MAT.
ENDFORM.                    "LEFT_JUSTIFY_MAT

*----------------------------------------------------------------------*
*       FORM PRT_PRINT                                                 *
*----------------------------------------------------------------------*
*       Print the PRT (production resource or Tools                    *
*----------------------------------------------------------------------*
*  -->  PLAN_NUMBER  the plan number defines for which operation       *
*                    PRTs should be printed
*----------------------------------------------------------------------*
FORM  PRT_PRINT USING AUFPL LIKE AFVGD-AUFPL   " PLAN_NUMBER
                      APLZL LIKE AFVGD-APLZL.  " plan_cnt.

  CONSTANTS: lc_measpoint TYPE FHMAR VALUE 'P'.

  DATA: LV_MEASPOINT_AS_PRT_ACTIVE TYPE FLAG.

* check if measuring point as PRT is active
  CALL FUNCTION 'CF_UT_MP_AS_PRT_ACTIVE_CHECK'
    IMPORTING
      EV_MEASPOINT_AS_PRT_ACTIVE = LV_MEASPOINT_AS_PRT_ACTIVE.

  LOOP AT IAFFHD WHERE AUFPL = AUFPL   "loop at PRTs for operation
                 AND   APLZL = APLZL.                       "
    AFFHD = IAFFHD.
    SELECT SINGLE * FROM  TCF10
           WHERE  STEUF       = AFFHD-STEUF.
    IF TCF10-XDRUCK = 'X'.             " FHM is to be printed
      CASE AFFHD-FHMAR.
        WHEN C_PRT_E.                  " Equipment as PRT   "30c
          CALL FUNCTION 'WRITE_FORM'
            EXPORTING
              ELEMENT = 'PRT_EQUI'
              WINDOW  = 'MAIN'.
        WHEN C_PRT_M.                  " Material as PRT
          CALL FUNCTION 'WRITE_FORM'
            EXPORTING
              ELEMENT = 'PRT_MAT'
              WINDOW  = 'MAIN'.
        WHEN C_PRT_S.                  " Standard PRT from PRT file
          CALL FUNCTION 'WRITE_FORM'
            EXPORTING
              ELEMENT = 'PRT_OTHER'
              WINDOW  = 'MAIN'.
        WHEN C_PRT_D.    " document from Doc Management system as PRT
          CALL FUNCTION 'WRITE_FORM'
            EXPORTING
              ELEMENT = 'PRT_DOCUMENT'
              WINDOW  = 'MAIN'.
*... print the EPS form from a DMS document for a drawing if it exists
          DOKOB = 'DRAW'.              " see table tdwo   for OBJECTS
          OBJKY = AFFHD-DOKNR.
          PERFORM DMS_DOCUMENT_EPS_OUTPUT USING AFFHD-DOKNR
                                                AFFHD-DOKAR
                                                AFFHD-DOKTL
                                                AFFHD-DOKVR.
        WHEN lc_measpoint.               " measuring point as PRT
          IF NOT LV_MEASPOINT_AS_PRT_ACTIVE IS INITIAL.
            CALL FUNCTION 'WRITE_FORM'
              EXPORTING
                ELEMENT = 'PRT_MPO'
                WINDOW  = 'MAIN'.
          ENDIF.
      ENDCASE.
*... Longtext for PRT
      PERFORM PRINT_PRT_LONGTEXT.
    ENDIF.
  ENDLOOP.                             " loop on PRTS
ENDFORM.                    "PRT_PRINT

*----------------------------------------------------------------------*
*       FORM  PRINT_PRT_LONGTEXT.                                      *
*----------------------------------------------------------------------*
*       Print the longtext for a Production resource or Tool (PRT)     *
*----------------------------------------------------------------------*
*  -->  AFFHD     PRT Dialog table must be available globally          *
*----------------------------------------------------------------------*
FORM PRINT_PRT_LONGTEXT.
*... LONGTEXT for PRT.
  IF NOT AFFHD-TXTKZ IS INITIAL.       "Is there a long text for PRT .
    CALL FUNCTION 'CO_ZK_TEXTKEY_AFFH'
      EXPORTING
        AUFPL = AFFHD-AUFPL
        MANDT = SYST-MANDT
        PZLFH = AFFHD-PZLFH
      IMPORTING
        LTSCH = TEXT_OBJECT_NAME.
    STXH-TDNAME =   TEXT_OBJECT_NAME.
*... print the text
    PERFORM PRINT_LONGTEXT USING TCO09-OBJEC        "OBJECT
                                 TEXT_OBJECT_NAME   "NAME
                                 AFFHD-TXTSP        "LANGUAGE
                                 TCO09-IDFHM        "TYPE
                                 C_MAIN"which window
                                 C_START_LINE_NR
                                 C_LAST_LINE_NR
                                 YES.  "with underline around text
  ENDIF.
ENDFORM.                    "PRINT_PRT_LONGTEXT

*---------------------------------------------------------------------*
*       FORM PRINT_MAT_LONGTEXT                                       *
*---------------------------------------------------------------------*
FORM PRINT_MAT_LONGTEXT.
*... LONGTEXT for Material reservations.
  IF NOT RESBD-LTEXT IS INITIAL.       "Is there a long text for mat .
    CALL FUNCTION 'CO_ZK_TEXTKEY_RESB'
      EXPORTING
        RSNUM = RESBD-RSNUM
        RSPOS = RESBD-RSPOS
        RSART = RESBD-RSART
      IMPORTING
        LTSCH = TEXT_OBJECT_NAME.
    STXH-TDNAME =   TEXT_OBJECT_NAME.
*... print the text
    PERFORM PRINT_LONGTEXT USING TCO09-OBJEC        "OBJECT
                                 TEXT_OBJECT_NAME   "NAME
                                 RESBD-LTXSP        "LANGUAGE
                                 TCO09-IDKOP        "TYPE   (material)
                                 C_MAIN"which window
                                 C_START_LINE_NR
                                 C_LAST_LINE_NR
                                 YES.  "with underline around text
  ENDIF.
ENDFORM.                    "PRINT_MAT_LONGTEXT

*---------------------------------------------------------------------*
*       FORM HR_ORDER_OPERATIONS                                      *
*---------------------------------------------------------------------*
FORM HR_ORDER_OPERATIONS.
  IAFVGD = SPACE.
  LOOP AT IAFVGD.                      "loop on the operations
    AFVGD = IAFVGD.                    " Set workarea for SAPSCRIPT
*... for each operation
    IF OP_ENTRIES > 0.                 " single operation print active
      LOOP AT OP_PRINT_TAB WHERE
              FLG_SEL = 'X'
         AND  VORNR   = AFVGD-VORNR    " was the operation selected
         AND  UVORN   = AFVGD-UVORN.   " for print ???
      ENDLOOP.
      CHECK SYST-SUBRC = 0.            " should this op be printed
    ENDIF.
    PERFORM READ_OP_TEXT_TABLES.       "operation text tables
    " T430 is now available
*... the Logistik Workcenter MUST be converted into the HR Workcenter
*... NOW for Print purposes
    CLEAR PDWORK.
    PDWORK-ARBID = AFVGD-ARBID.
    PDWORK-WERKS = AFVGD-WERKS.
    MOVE-CORRESPONDING AFVGD TO PDAVOEA.
    CALL FUNCTION 'READ_PD_WORKCENTER'
      EXPORTING
        PDWORK_IMP           = PDWORK
      IMPORTING
        PDWORK_EXP           = PDWORK
      EXCEPTIONS
        NO_INTEGRATION       = 01
        INVALID_ARBID        = 02
        WORKCENTER_NOT_FOUND = 03.
    IF SY-SUBRC <> 0.
      CLEAR PDWORK.
    ENDIF.
*... check that the operation should be printed based on the
*... control key.
    CHECK T430-VRGD = YES.             " jump to next operation
    " when oper not marked for print
    CALL FUNCTION 'WRITE_FORM'                              "
         EXPORTING
           ELEMENT   = 'OPERATION'     " main operation details
           WINDOW    = 'MAIN'.
*... now print either the interal or external operation details
*... FLAG AFVGD-FLG_FRD    Set based on Control key  see read_op_texts
    IF AFVGD-FLG_FRD = YES.            " it is a external operation
      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          ELEMENT = 'EXTERNAL_WORK'                   "
          WINDOW  = 'MAIN'.
    ELSE.
      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          ELEMENT = 'INTERNAL_WORK'  " main operation details
          WINDOW  = 'MAIN'.
    ENDIF.
    PERFORM PRINT_OPERATION_TEXT.      " Longtext to operation
  ENDLOOP.
ENDFORM.                    "HR_ORDER_OPERATIONS

* Print special permissions for an object.
FORM PERMITS USING OBJECT_NR LIKE IHSG-OBJNR.
*... Permits must be found in IHSG_tab and IHGNS_TAB
*... print all permissions relevant for printing (See indicator)
  LOOP  AT IHSG_TAB WHERE OBJNR = OBJECT_NR
                 AND   K_DRUCK = 'X'.  "permission to be printed
    IHSG = IHSG_TAB.                   " set DDIC table workarea
    PERFORM READ_PERMIT_TEXTS          " v_t357g is set globally
            USING IHSG-PMSOG.
    CALL FUNCTION 'WRITE_FORM'
      EXPORTING
        ELEMENT = 'PERMIT'
        WINDOW  = 'MAIN'.
*... for all permission check to see if permission has already been
*... granted.  If so, print who granted permission and when and for
*... how long.
    LOOP AT IHGNS_TAB WHERE COUNTER = IHSG_TAB-COUNTER
                      AND   GENIAKT ne 'X'.
*... if they exist and not inactive then they are granted.
      IHGNS =  IHGNS_TAB.
      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          ELEMENT = 'GRANT'
          WINDOW  = 'MAIN'.
    ENDLOOP.
*... print special permits texts for current order and counter
*...
    PERFORM PERMIT_LONGTEXT USING OBJECT_NR
                                  IHSG_TAB-COUNTER.
  ENDLOOP.
ENDFORM.                    "PERMITS

*---------------------------------------------------------------------*
*       FORM READ_PERMIT_TEXTS                                        *
*---------------------------------------------------------------------*
FORM READ_PERMIT_TEXTS USING SOGEN LIKE V_T357G-SOGEN.
*... get the texts for a permission.
*    (permission text and permission type text.)
  CLEAR V_T357G.
  SELECT SINGLE * FROM  T357G
         WHERE  SOGEN       = SOGEN.
  SELECT SINGLE * FROM  T357G_T
     WHERE  SPRAS       = PRINT_LANGUAGE
     AND    PMSOG       = SOGEN.
  IF SYST-SUBRC <> 0.
    CLEAR T357G_T.
  ENDIF.
  SELECT SINGLE * FROM  T352T_T
         WHERE  SPRAS       = PRINT_LANGUAGE
         AND    GNTYP       = T357G-GNTYP.
  IF SYST-SUBRC <> 0.
    CLEAR T352T_T.
  ENDIF.
*... set the view tables for access in SAPSCRIPT
  V_T357G-SOGEN  =  T357G-SOGEN.
  V_T357G-GNTYP  =  T357G-GNTYP.
  V_T357G-GNTXT  =  T357G_T-GNTXT.
  V_T357G-GTTXT  =  T352T_T-GNTXT.
ENDFORM.                    "READ_PERMIT_TEXTS

*---------------------------------------------------------------------*
*       FORM PERMIT_LONGTEXT                                          *
*---------------------------------------------------------------------*
*  -->  OBJECT_NR                                                     *
*  -->  COUNTER                                                       *
*---------------------------------------------------------------------*
FORM PERMIT_LONGTEXT USING OBJECT_NR LIKE CAUFVD-OBJNR
                           COUNTER LIKE IHSG-COUNTER.
  DATA: HKEY(30).
*... build objectName  (key to to text)
  CLEAR HKEY.
  HKEY =  OBJECT_NR.
  HKEY+22(6) = COUNTER.
  PERFORM PRINT_LONGTEXT USING 'IHSG'  "OBJECT
                               HKEY    "NAME
                               PRINT_LANGUAGE     "LANGUAGE
                               'LTXT'  "TYPE
                               C_MAIN  "which window
                               C_START_LINE_NR
                               C_LAST_LINE_NR
                               YES.    "with underline around text
ENDFORM.                    "PERMIT_LONGTEXT

*... check the kbedp_tab for splits (sub kbed records)
FORM SPLIT_COUNT USING BEDID LIKE AFVGD-BEDID
                       BEDZL LIKE AFVGD-BEDZL
                       CNT   TYPE P.
  CNT = 0.
  LOOP AT KBEDP_TAB WHERE BEDID =  BEDID
                    AND   BEDZL =  BEDZL.
*...splits point to parent KBED record, when set then it is a split
    CHECK NOT KBEDP_TAB-CANUMF IS INITIAL.
*...also check that the splitt number is set.If not it is a rest record
    CHECK NOT KBEDP_TAB-SPLIT  IS INITIAL.  "dont print REST info
    CNT = CNT + 1.
  ENDLOOP.
*... CANUM is the capacity number
*... canumf is the pointer to main capacity record if it a split exits
ENDFORM.                    "SPLIT_COUNT

*---------------------------------------------------------------------*
*       FORM DMS_OBJECT_PRINT                                         *
*---------------------------------------------------------------------*
*  -->  EQUIPMENT                                                     *
*  -->  FUNC_LOC                                                      *
*---------------------------------------------------------------------*
FORM DMS_OBJECT_PRINT USING EQUIPMENT LIKE EQUI-EQUNR
                            FUNC_LOC  LIKE IFLOT-TPLNR.
*... print the EPS form from a DMS document for technical objects.
  DOKOB = 'EQUI'.                      " see table tdwo   for OBJECTS
  OBJKY = EQUIPMENT.
  PERFORM PRINT_EPS_TEXTS_FOR_OBJECT USING DOKOB OBJKY.
  DOKOB = 'IFLOT'.                     " see table tdwo   for OBJECTS
  OBJKY = FUNC_LOC.
  PERFORM PRINT_EPS_TEXTS_FOR_OBJECT USING DOKOB OBJKY.
ENDFORM.                    "DMS_OBJECT_PRINT

*----------------------------------------------------------------------*
*       FORM PRINT_EPS_TEXTS_FOR_OBJECT                                *
*----------------------------------------------------------------------*
*       Prints a EPS text from SAPSCRIPT for a given Document
*       from Document manag System.                                    *
*----------------------------------------------------------------------*
*  -->  OBJECT    SAP object    See table TDwo for Objects  (EG EQUI)  *
*  -->  Key       Object Key    Eg Equipment number                    *
*----------------------------------------------------------------------*
FORM PRINT_EPS_TEXTS_FOR_OBJECT USING OBJECT LIKE DRAD-DOKOB
                                      KEY    LIKE DRAD-OBJKY.

*... Get all documents  for object
  SELECT        * FROM  DRAD  INTO TABLE DRAD_TAB
         WHERE  DOKOB       = OBJECT
         AND    OBJKY       = KEY.
  CHECK SYST-SUBRC = 0.                "go on if we found something
  LOOP AT DRAD_TAB.
    DRAD = DRAD_TAB.
    PERFORM DMS_DOCUMENT_EPS_OUTPUT USING DRAD-DOKNR
                                          DRAD-DOKAR
                                          DRAD-DOKTL
                                          DRAD-DOKVR.
  ENDLOOP.
ENDFORM.                    "PRINT_EPS_TEXTS_FOR_OBJECT

*---------------------------------------------------------------------*
*       FORM DMS_DOCUMENT_EPS_OUTPUT                                  *
*---------------------------------------------------------------------*
*  -->  DOKNR                                                         *
*  -->  DOKAR                                                         *
*  -->  DOKTL                                                         *
*  -->  DOKVR                                                         *
*---------------------------------------------------------------------*
FORM DMS_DOCUMENT_EPS_OUTPUT USING DOKNR  LIKE DRAD-DOKNR
                                   DOKAR  LIKE DRAD-DOKAR
                                   DOKTL  LIKE DRAD-DOKTL
                                   DOKVR  LIKE DRAD-DOKVR.

  CALL FUNCTION 'READ_DMS_POSTSCRIPT_TEXT'
    EXPORTING
      DOCUMENT           = DOKNR
      DOCUMENT_TYPE      = DOKAR
      DOCUMENT_PART      = DOKTL
      DOCUMENT_VERS      = DOKVR
      LANGUAGE           = PRINT_LANGUAGE
    IMPORTING
      EPS_TEXT_HEADER    = TABLE_HEADER
    TABLES
      EPS_TEXT_LINES     = TABLE_LINES
    EXCEPTIONS
      EPS_TEXT_NOT_FOUND = 01.
  IF SYST-SUBRC = 0.                   "We have something to print.
    PERFORM TEXT_OUTPUT TABLES TABLE_LINES
                        USING TABLE_HEADER
                              'MAIN'   "In window main
                              NO.      "no under line
  ENDIF.
ENDFORM.                    "DMS_DOCUMENT_EPS_OUTPUT
*---------------------------------------------------------------------*
*       FORM CHECK_PRINT_STATUS                                       *
*---------------------------------------------------------------------*
*  -->  OBJNR                                                         *
*  -->  DELTA_MODE                                                    *
*  -->  RC                                                            *
*---------------------------------------------------------------------*
FORM CHECK_PRINT_STATUS USING OBJNR LIKE AFVGD-OBJNR
                              DELTA_MODE LIKE WWORKPAPER-PM_DELTA_P
                              RC LIKE SYST-SUBRC.
  RC = 0.
  CHECK DELTA_MODE = 'X'.
  CALL FUNCTION 'STATUS_CHECK'
    EXPORTING
      OBJNR             = OBJNR
      STATUS            = STATUS_PRINT
    EXCEPTIONS
      OBJECT_NOT_FOUND  = 01
      STATUS_NOT_ACTIVE = 02.
  IF SYST-SUBRC = 0.                   " the segment is already printed
    RC = 8.
  ENDIF.
ENDFORM.                    "CHECK_PRINT_STATUS
*&---------------------------------------------------------------------*
*&      Form  SERVICE_PACKAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_AFVGD_PACKNO  text
*----------------------------------------------------------------------*
FORM SERVICE_PACKAGE USING PACKNO LIKE AFVGD-PACKNO
                           FLG_FRD LIKE AFVGD-FLG_FRD.

  REFRESH: GLIEDERUNG, LEISTUNG.

  CHECK NOT PACKNO IS INITIAL.

  CALL FUNCTION 'WRITE_FORM'
    EXPORTING
      ELEMENT = 'SRV'
    EXCEPTIONS
      OTHERS  = 01.

  CALL FUNCTION 'MS_SUBDIVISION_FOR_PRINT'
    EXPORTING
      PACKNO           = PACKNO
      ONLINE_DATA      = 'X'
    TABLES
      GLIEDERUNG       = GLIEDERUNG
    EXCEPTIONS
      PACKNO_NOT_EXIST = 1
      OTHERS           = 2.

  IF SY-SUBRC EQ 0.
    LOOP AT GLIEDERUNG.
      PERFORM PRINT_GLIEDERUNG.
      CALL FUNCTION 'MS_SERVICES_FOR_PRINT'
        EXPORTING
          PACKNO            = GLIEDERUNG-SUB_PACKNO
          ONLINE_DATA       = 'X'
        TABLES
          LEISTUNG          = LEISTUNG
        EXCEPTIONS
          NO_SERVICES_FOUND = 01.
      PERFORM PRINT_LEISTUNG USING FLG_FRD.


    ENDLOOP.

  ENDIF.

ENDFORM.                    " SERVICE_PACKAGE
*&---------------------------------------------------------------------*
*&      Form  PRINT_GLIEDERUNG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM PRINT_GLIEDERUNG.
  DATA: RETURN.

  CHECK GLIEDERUNG-RANG NE 0.
  MOVE GLIEDERUNG TO ML_ESLL.

  CALL FUNCTION 'WRITE_FORM'
    EXPORTING
      ELEMENT = 'SERVICES'
    EXCEPTIONS
      OTHERS  = 01.

  CLEAR ML_ESLL.
  PERFORM TEXTE USING 'ESLL' GLIEDERUNG-PACKNO GLIEDERUNG-INTROW 'LTXT'
                       RETURN.
  CLEAR ML_ESLL.
ENDFORM.                    " PRINT_GLIEDERUNG

*&---------------------------------------------------------------------*
*&      Form  PRINT_LEISTUNG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM PRINT_LEISTUNG USING FLG_FRD LIKE AFVGD-FLG_FRD.
  DATA: RETURN.

  LOOP AT LEISTUNG.
    MOVE SPACE TO LEISTUNG-EXTGROUP.
    MOVE '0'   TO LEISTUNG-RANG.
    MOVE LEISTUNG TO ML_ESLL.
    IF FLG_FRD = '+'.
      CLEAR ML_ESLL-INT_WORK.
    ENDIF.
    CALL FUNCTION 'WRITE_FORM'
      EXPORTING
        ELEMENT = 'SERVICES'
      EXCEPTIONS
        OTHERS  = 01.

    PERFORM TEXTE USING 'ESLL' LEISTUNG-PACKNO LEISTUNG-INTROW 'LLTX'
                        RETURN.
    IF RETURN = 4.
      PERFORM TEXTE USING 'ASMD' LEISTUNG-SRVPOS ' ' 'LTXT' RETURN.
    ENDIF.
    PERFORM TEXTE USING 'ESLL' LEISTUNG-PACKNO LEISTUNG-INTROW 'LTXT'
                         RETURN.

    PERFORM PRINT_TIME.
    PERFORM PRINT_FORMEL.
    CLEAR ML_ESLL.
  ENDLOOP.
ENDFORM.                    " PRINT_LEISTUNG

*&---------------------------------------------------------------------*
*&      Form  TEXTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM TEXTE USING  OBJECT TYPE CLIKE
                  PACKNO TYPE SIMPLE "NUMC
                  INTROW TYPE SIMPLE "NUMC
                  ID     TYPE CLIKE
                  RETURN TYPE CLIKE.
  CLEAR THEAD.
* Textheader lesen
  THEAD-TDOBJECT  = OBJECT.
  THEAD-TDSPRAS   = PRINT_LANGUAGE.
  THEAD-TDNAME    = PACKNO.
  IF OBJECT = 'ESLL'.
    THEAD-TDNAME+10 = INTROW.
  ENDIF.
  THEAD-TDID      = ID.

  CALL FUNCTION 'SELECT_TEXT'
    EXPORTING
      ID         = THEAD-TDID
      LANGUAGE   = THEAD-TDSPRAS
      NAME       = THEAD-TDNAME
      OBJECT     = THEAD-TDOBJECT
    IMPORTING
      ENTRIES    = ENTRIES
    TABLES
      SELECTIONS = XTHEAD.
  IF ENTRIES NE 0.
    RETURN = 0.
    SORT XTHEAD BY TDID.
* Text lesen
    LOOP AT XTHEAD.
      MOVE-CORRESPONDING XTHEAD TO THEAD.
      PERFORM LESEN_TTXIT USING XTHEAD-TDOBJECT XTHEAD-TDID.
      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          ELEMENT = 'ITEM_TEXT'
        EXCEPTIONS
          OTHERS  = 01.
      CLEAR SY-SUBRC.
    ENDLOOP.
  ELSE.
    RETURN = 4.
  ENDIF.

ENDFORM.                    " TEXTE

*&---------------------------------------------------------------------*
*&      Form  LESEN_TTXIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM LESEN_TTXIT USING OBJECT TYPE CLIKE  "THEAD-TDOBJECT
                       ID     TYPE CLIKE. "THEAD-TDID

  CLEAR TTXIT.
  SELECT SINGLE * FROM TTXIT WHERE TDSPRAS  EQ PRINT_LANGUAGE
                             AND   TDOBJECT EQ OBJECT
                             AND   TDID     EQ ID.

ENDFORM.                    " LESEN_TTXIT
*&---------------------------------------------------------------------*
*&      Form  PRINT_TIME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM PRINT_TIME.

  CHECK NOT ML_ESLL-PERNR IS INITIAL OR
       NOT ML_ESLL-PERSEXT IS INITIAL OR
       NOT ML_ESLL-SDATE IS INITIAL OR
       NOT ML_ESLL-BEGTIME IS INITIAL OR
       NOT ML_ESLL-ENDTIME IS INITIAL.

  CALL FUNCTION 'WRITE_FORM'
    EXPORTING
      ELEMENT = 'TIME_DATA'
    EXCEPTIONS
      OTHERS  = 1.


ENDFORM.                    " PRINT_TIME

*&---------------------------------------------------------------------*
*&      Form  PRINT_FORMEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM PRINT_FORMEL.
  DATA: FELD(15).
  FIELD-SYMBOLS: <VALUE> TYPE ML_ESLL-FRMVAL1.
  CHECK NOT ML_ESLL-FORMELNR IS INITIAL.

  CALL FUNCTION 'MS_READ_AND_CHECK_FORMULA'
    EXPORTING
      I_FORMELNR = ML_ESLL-FORMELNR
      NO_ERRORS  = 'X'
    IMPORTING
      E_FORMEL   = FORMEL
    TABLES
      VARIABLEN  = VARIABLEN
    EXCEPTIONS
      OTHERS     = 0.

  CALL FUNCTION 'WRITE_FORM'
    EXPORTING
      ELEMENT = 'FORMEL_KOPF'
    EXCEPTIONS
      OTHERS  = 1.
  MOVE 'ML_ESLL-FRMVAL1' TO FELD.
  LOOP AT VARIABLEN.
    MOVE SY-TABIX TO FELD+14(1).
    ASSIGN (FELD) TO <VALUE>.
    MOVE <VALUE> TO ML_ESLL-FRMVAL1.
    CALL FUNCTION 'WRITE_FORM'
      EXPORTING
        ELEMENT = 'FORMEL_BODY'
      EXCEPTIONS
        OTHERS  = 1.
  ENDLOOP.

ENDFORM.                    " PRINT_FORMEL
*&---------------------------------------------------------------------*
*&      Form  ORDER_OPERATIONS_HEADING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ORDER_OPERATIONS_HEADING .

*     CHECK T430-VRGD = YES.             " jump to next operation
  " when oper not marked for print
  CALL FUNCTION 'WRITE_FORM'                              "
       EXPORTING
         ELEMENT   = 'OPERATION_HEAD'     " main operation details heading
         WINDOW    = 'MAIN'.

ENDFORM.                    " ORDER_OPERATIONS_HEADING
