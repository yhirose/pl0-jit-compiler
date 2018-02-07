VAR x;
  PROCEDURE func0;
    VAR x0;
    PROCEDURE func1;
      VAR x1;
    BEGIN
      x1 := 789;
      out x;
      out x0;
      out x1;
    END;
  BEGIN
    x0 := 456;
    CALL func1
  END;
BEGIN
  x := 123;
  CALL func0
END.
