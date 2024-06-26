FUNCTION ZFM_BM_CS_SALES_ORG_SD99.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_DATA) TYPE  ZST_BM_SALES_SD99
*"----------------------------------------------------------------------

DATA: LSS_TVAK          TYPE TVAK.

  GS_DATASD99 = I_DATA.
  SELECT SINGLE
    *
      FROM TVAK
       INTO CORRESPONDING FIELDS OF  GT_SD99
      WHERE AUART = GS_DATASD99-AUART .
  IF SY-SUBRC IS INITIAL.
    GS_SD99-AUART = GS_DATASD99-AUART.
    GS_SD99-FEHGR = GS_DATASD99-FEHGR.
    GS_SD99-DIAFE = GS_DATASD99-DIAFE.
    MOVE-CORRESPONDING GS_SD99 TO LSS_TVAK.
    UPDATE  TVAK FROM LSS_TVAK.

  ELSE.
    GS_SD99-AUART = GS_DATASD99-AUART.
    GS_SD99-FEHGR = GS_DATASD99-FEHGR.
    GS_SD99-DIAFE = GS_DATASD99-DIAFE.
    MOVE-CORRESPONDING GS_SD99 TO LSS_TVAK.
    INSERT  TVAK FROM LSS_TVAK.
  ENDIF.
ENDFUNCTION.
