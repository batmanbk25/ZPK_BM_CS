FUNCTION ZFM_BM_CS_SALES_ORG_SD101.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_DATA) TYPE  ZST_BM_SALES_SD101
*"----------------------------------------------------------------------
DATA: LSS_TPAR        TYPE TPAR.

  GS_DATASD101 = I_DATA.
  SELECT SINGLE
    *
      FROM TPAR
       INTO CORRESPONDING FIELDS OF  GT_SD101
      WHERE PARVW = GS_DATASD101-PARVW .
  IF SY-SUBRC IS INITIAL.
    GS_SD101-PARVW = GS_DATASD101-PARVW.
    GS_SD101-FEHGR = GS_DATASD101-FEHGR.

    MOVE-CORRESPONDING GS_SD101 TO LSS_TPAR.
    UPDATE  TPAR FROM LSS_TPAR.

  ELSE.
    GS_SD101-PARVW = GS_DATASD101-PARVW.
    GS_SD101-FEHGR = GS_DATASD101-FEHGR.

    MOVE-CORRESPONDING GS_SD101 TO LSS_TPAR.
    INSERT  TPAR FROM LSS_TPAR.
  ENDIF.

ENDFUNCTION.
