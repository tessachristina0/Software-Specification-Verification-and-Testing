data Boy = Matthew | Peter | Jack | Arnold | Carl
    deriving (Eq,Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]

accuses ::  Boy -> Boy -> Bool
accuses Matthew _ = False
accuses Peter Matthew = True
accuses Peter Jack = True
accuses Matthew Peter = True


-- accusers ::  Boy -> [Boy]


-- guilty, honest ::  [Boy]