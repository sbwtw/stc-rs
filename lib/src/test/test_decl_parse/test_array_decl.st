FUNCTION F: INT

VAR
    a: ARRAY [ 1 .. 2 ]  OF INT;
    b:ARRAY[1..2]OF INT;
    c: ARRAY[1.2..2.3]OF DWORD;
    c2: ARRAY[1..3, 2..4]OF DWORD;
    b:ARRAY[a..b]OF INT;
    b2:ARRAY[a..b, c..d]OF INT;
    b3:ARRAY[a..b, c..d]OF ARRAY[0..10] OF INT;
END_VAR

END_FUNCTION