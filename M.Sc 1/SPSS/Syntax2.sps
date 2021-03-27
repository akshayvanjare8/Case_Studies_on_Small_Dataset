
RECODE dept_name (1=3) (2=2) (3=3) (4=1) (5=1) (6=3) (7=2) (8=2) (9=3) (10=1) (11=1) (12=1) (13=2) 
    (14=2) (15=3) (16=1) (17=3) (18=1) (19=3) (20=3) (21=1) (22=3) (23=1) INTO dept_name.
VARIABLE LABELS  dept_name 'dept_name'.
EXECUTE.

RECODE dept_name (1=3) (2=2) (3=3) (4=1) (5=1) (6=3) (7=2) (8=2) (9=3) (10=1) (11=1) (12=1) (13=2) 
    (14=2) (15=3) (16=1) (17=3) (18=1) (19=3) (20=3) (21=1) (22=3) (23=1) INTO dept_name.
VARIABLE LABELS  dept_name 'dept_name_1'.
EXECUTE.
