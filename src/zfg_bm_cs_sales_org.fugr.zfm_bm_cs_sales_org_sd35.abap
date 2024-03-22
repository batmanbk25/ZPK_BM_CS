FUNCTION ZFM_BM_CS_SALES_ORG_SD35.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_DATA) TYPE  ZST_BM_SALES_SD35
*"----------------------------------------------------------------------

DATA: LSS_TVPG           TYPE TVPG,
        LS_TVPAG TYPE TVPAG.

  GS_DATASD35 = I_DATA.
  SELECT SINGLE
    TVPG~PARGR,
    TVPAG~VTEXT
      FROM TVPG
    INNER JOIN TVPAG
    ON TVPAG~PARGR = TVPG~PARGR
       INTO CORRESPONDING FIELDS OF  @GT_SD35
      WHERE TVPG~PARGR = @GS_DATASD35-PARGR.
  IF SY-SUBRC IS INITIAL.
    GS_SD35-PARGR = GS_DATASD35-PARGR.
    GS_SD35-VTEXT = GS_DATASD35-VTEXT.
    GS_SD35-PAOBJ = 'C'.
    GS_SD35-SPRAS = SY-LANGU.

    MOVE-CORRESPONDING GS_SD35 TO LSS_TVPG.
    MOVE-CORRESPONDING GS_SD35 TO LS_TVPAG.

    UPDATE  TVPG FROM LSS_TVPG.
    UPDATE  TVPAG FROM LS_TVPAG.
  ELSE.
     GS_SD35-PARGR = GS_DATASD35-PARGR.
    GS_SD35-VTEXT = GS_DATASD35-VTEXT.
    GS_SD35-PAOBJ = 'A'.
    GS_SD35-SPRAS = SY-LANGU.

    MOVE-CORRESPONDING GS_SD35 TO LSS_TVPG.
    MOVE-CORRESPONDING GS_SD35 TO LS_TVPAG.

    INSERT  TVPG FROM LSS_TVPG.
    INSERT  TVPAG FROM LS_TVPAG.
  ENDIF.
ENDFUNCTION.