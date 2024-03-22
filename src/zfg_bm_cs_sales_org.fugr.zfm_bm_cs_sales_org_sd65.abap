FUNCTION ZFM_BM_CS_SALES_ORG_SD65.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_DATA) TYPE  ZST_BM_SALES_SD65
*"----------------------------------------------------------------------
  DATA: LSS_T682 TYPE T682,
        LS_T682T TYPE T682T.

  GS_DATASD65 = I_DATA.
  SELECT SINGLE
    T682~KVEWE,
    T682~KAPPL,
    T682~KOZGF,
    T682~KOZGV,
    T682~SYPSY,
    T682~SYSID,
    T682~SAPRL,
    T682~EXT_COUNTER_USED,
    T682T~VTXTM,
    T682T~SPRAS
      FROM T682
    INNER JOIN T682T
    ON T682T~KVEWE = T682~KVEWE
    AND T682T~KAPPL = T682~KAPPL
    AND T682T~KOZGF = T682~KOZGF
       INTO CORRESPONDING FIELDS OF  @GT_SD65
      WHERE T682~KVEWE = @GS_DATASD65-KVEWE
    AND T682~KAPPL = @GS_DATASD65-KAPPL
    AND T682~KOZGF = @GS_DATASD65-KOZGF.
  IF SY-SUBRC IS INITIAL.
*    PERFORM CHECKSD26.
    GS_SD65-KVEWE = 'C'.
    GS_SD65-KAPPL = 'V'.
    GS_SD65-KOZGF = GS_DATASD65-KOZGF.
    GS_SD65-VTXTM = GS_DATASD65-VTXTM.
    GS_SD65-KOZGV = GS_DATASD65-KOZGV.
    GS_SD65-SYPSY = GS_DATASD65-SYPSY.
    GS_SD65-SYSID = GS_DATASD65-SYSID.
    GS_SD65-SAPRL = GS_DATASD65-SAPRL.
    GS_SD65-EXT_COUNTER_USED = 'X'.
    GS_SD65-SPRAS = SY-LANGU.

    MOVE-CORRESPONDING GS_SD65 TO LSS_T682.
    MOVE-CORRESPONDING GS_SD65 TO LS_T682T.

    UPDATE  T682 FROM LSS_T682.
    UPDATE  T682T FROM LS_T682T.
  ELSE.
    GS_SD65-KVEWE = 'C'.
    GS_SD65-KAPPL = 'V'.
    GS_SD65-KOZGF = GS_DATASD65-KOZGF.
    GS_SD65-VTXTM = GS_DATASD65-VTXTM.
    GS_SD65-KOZGV = GS_DATASD65-KOZGV.
    GS_SD65-SYPSY = GS_DATASD65-SYPSY.
    GS_SD65-SYSID = GS_DATASD65-SYSID.
    GS_SD65-SAPRL = GS_DATASD65-SAPRL.
    GS_SD65-EXT_COUNTER_USED = 'X'.
    GS_SD65-SPRAS = SY-LANGU.

    MOVE-CORRESPONDING GS_SD65 TO LSS_T682.
    MOVE-CORRESPONDING GS_SD65 TO LS_T682T.

    INSERT  T682 FROM LSS_T682.
    INSERT  T682T FROM LS_T682T.
  ENDIF.
ENDFUNCTION.
