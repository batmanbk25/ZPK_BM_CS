FUNCTION ZFM_BM_CS_SALES_ORG_SD33.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_DATA) TYPE  ZST_BM_SALES_SD33
*"----------------------------------------------------------------------
  DATA: LSS_TVAK          TYPE TVAK.

  GS_DATASD33 = I_DATA.
  SELECT SINGLE
    *
      FROM TVAK
       INTO CORRESPONDING FIELDS OF  GT_SD33
      WHERE AUART = GS_DATASD33-AUART.
  IF SY-SUBRC IS INITIAL.
    GS_SD33-AUART = GS_DATASD33-AUART.
    GS_SD33-PARGR = GS_DATASD33-PARGR.
    MOVE-CORRESPONDING GS_SD33 TO LSS_TVAK.
    UPDATE  TVAK FROM LSS_TVAK.

  ELSE.
    GS_SD33-AUART = GS_DATASD33-AUART.
    GS_SD33-PARGR = GS_DATASD33-PARGR.
    MOVE-CORRESPONDING GS_SD33 TO LSS_TVAK.
    INSERT  TVAK FROM LSS_TVAK.
  ENDIF.
ENDFUNCTION.