*&---------------------------------------------------------------------*
*& Include          LZFG_BM_CSF01
*&---------------------------------------------------------------------*

*---------------------------------------------------------------------*
*       FORM GET_CURRENCY_DECIMALS                                    *
*---------------------------------------------------------------------*
*       get decimals of a selected currency (table TCURX)             *
*---------------------------------------------------------------------*
FORM GET_CURRENCY_DECIMALS USING    VALUE(I_WAERS)  TYPE WAERS
                           CHANGING C_CURR_DECIMALS TYPE CURRDEC.
  STATICS: L_WRK_CURR   TYPE BAPI1090_1.
  DATA:    L_WRK_RETURN TYPE BAPIRETURN.
  IF I_WAERS IS INITIAL.
    C_CURR_DECIMALS = 2.
    EXIT.
  ENDIF.
  IF I_WAERS <> L_WRK_CURR-CURRENCY.
    L_WRK_CURR-CURRENCY = I_WAERS.
    SELECT SINGLE CURRDEC FROM TCURX
           INTO L_WRK_CURR-CURDECIMALS
           WHERE  CURRKEY     = I_WAERS.
    IF SY-SUBRC <> 0.
*     default: 2 decimals
      L_WRK_CURR-CURDECIMALS = 2.
    ENDIF.
  ENDIF.
  C_CURR_DECIMALS = L_WRK_CURR-CURDECIMALS.
ENDFORM.
