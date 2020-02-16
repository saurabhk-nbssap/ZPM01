*&---------------------------------------------------------------------*
*& Report  Z6PM002_FILL_TPMP_USERNAME
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  Z6PM002_FILL_TPMP_USERNAME.


TABLES TPMP.

SELECT * FROM TPMP WHERE USERN = SPACE.

  SELECT SINGLE * FROM TPMP WHERE AUART = TPMP-AUART AND
                                  WERKS = TPMP-WERKS AND
                                  USERN <> SPACE.

  IF SY-SUBRC IS INITIAL.
     DELETE FROM TPMP WHERE AUART = TPMP-AUART AND
                            WERKS = TPMP-WERKS AND
                            USERN = SPACE.
  ELSE.
     UPDATE TPMP SET   USERN = '************'
                 WHERE AUART = TPMP-AUART AND
                       WERKS = TPMP-WERKS AND
                       USERN = SPACE.
  ENDIF.

ENDSELECT.
