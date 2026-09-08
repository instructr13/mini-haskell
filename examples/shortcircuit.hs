data Bool = False | True

infixr 3 &&

False && y = False
True  && y = y

loop = loop

main = False && loop
