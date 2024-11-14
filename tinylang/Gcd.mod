MODULE Gcd;
# FROM A IMPORT A1, A2;
# IMPORT B1, B2, B3;
    VAR x: INTEGER;
    VAR y,z: INTEGER;
    VAR m: BOOLEAN;
    CONST consta = TRUE;
    CONST constD = NOT TRUE;
    CONST constC = NOT consta;
    CONST constb = constC AND consta;
    CONST constE = consta AND (consta OR consta);

    PROCEDURE GCD(a, b: INTEGER; c, d: INTEGER) : INTEGER;
    VAR t: INTEGER;
    VAR y: INTEGER;
    BEGIN
      IF b = 0 THEN
        RETURN a;
      ELSE
        b := AH;
        IF (b + 1) * (2+a) = a + 4 THEN
          RETURN a;
        END;
        b := AH;
      END;
    #  WHILE b # 0 DO
    #    t := a MOD b;
    #    a := b;
    #    b := t;
    #  END;
    RETURN a;
    END GCD;

END Gcd.
