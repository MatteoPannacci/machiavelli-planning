(define (problem pr_mid) (:domain machiavelli)

    (:objects
        nA n2 n3 n4 n5 n6 n7 n8 n9 n10 nJ nQ nK - num
        D C H S - seed
        cAD c2D c3D c4D c5D c6D c7D c8D c9D c10D cJD cQD cKD - card
        cAC c2C c3C c4C c5C c6C c7C c8C c9C c10C cJC cQC cKC - card
        cAH c2H c3H c4H c5H c6H c7H c8H c9H c10H cJH cQH cKH - card
        cAS c2S c3S c4S c5S c6S c7S c8S c9S c10S cJS cQS cKS - card
    )

    (:init
        
        ; number ordering ;
        (succ nA n2)
        (succ n2 n3)
        (succ n3 n4)
        (succ n4 n5)
        (succ n5 n6)
        (succ n6 n7)
        (succ n7 n8)
        (succ n8 n9)
        (succ n9 n10)
        (succ n10 nJ)
        (succ nJ nQ)
        (succ nQ nK)
        (succ nK nA) ; variant: can combine lower and higher straights beyond Ace ;

        ; number and seed association for diamonds cards ;
        (has_num cAD nA) (has_seed cAD D)
        (has_num c2D n2) (has_seed c2D D)
        (has_num c3D n3) (has_seed c3D D)
        (has_num c4D n4) (has_seed c4D D)
        (has_num c5D n5) (has_seed c5D D)
        (has_num c6D n6) (has_seed c6D D)
        (has_num c7D n7) (has_seed c7D D)
        (has_num c8D n8) (has_seed c8D D)
        (has_num c9D n9) (has_seed c9D D)
        (has_num c10D n10) (has_seed c10D D)
        (has_num cJD nJ) (has_seed cJD D)
        (has_num cQD nQ) (has_seed cQD D)
        (has_num cKD nK) (has_seed cKD D)

        ; number and seed association for clubs cards ;
        (has_num cAC nA) (has_seed cAC C)
        (has_num c2C n2) (has_seed c2C C)
        (has_num c3C n3) (has_seed c3C C)
        (has_num c4C n4) (has_seed c4C C)
        (has_num c5C n5) (has_seed c5C C)
        (has_num c6C n6) (has_seed c6C C)
        (has_num c7C n7) (has_seed c7C C)
        (has_num c8C n8) (has_seed c8C C)
        (has_num c9C n9) (has_seed c9C C)
        (has_num c10C n10) (has_seed c10C C)
        (has_num cJC nJ) (has_seed cJC C)
        (has_num cQC nQ) (has_seed cQC C)
        (has_num cKC nK) (has_seed cKC C)

        ; number and seed association for hearts cards ;
        (has_num cAH nA) (has_seed cAH H)
        (has_num c2H n2) (has_seed c2H H)
        (has_num c3H n3) (has_seed c3H H)
        (has_num c4H n4) (has_seed c4H H)
        (has_num c5H n5) (has_seed c5H H)
        (has_num c6H n6) (has_seed c6H H)
        (has_num c7H n7) (has_seed c7H H)
        (has_num c8H n8) (has_seed c8H H)
        (has_num c9H n9) (has_seed c9H H)
        (has_num c10H n10) (has_seed c10H H)
        (has_num cJH nJ) (has_seed cJH H)
        (has_num cQH nQ) (has_seed cQH H)
        (has_num cKH nK) (has_seed cKH H)

        ; number and seed association for spades cards ;
        (has_num cAS nA) (has_seed cAS S)
        (has_num c2S n2) (has_seed c2S S)
        (has_num c3S n3) (has_seed c3S S)
        (has_num c4S n4) (has_seed c4S S)
        (has_num c5S n5) (has_seed c5S S)
        (has_num c6S n6) (has_seed c6S S)
        (has_num c7S n7) (has_seed c7S S)
        (has_num c8S n8) (has_seed c8S S)
        (has_num c9S n9) (has_seed c9S S)
        (has_num c10S n10) (has_seed c10S S)
        (has_num cJS nJ) (has_seed cJS S)
        (has_num cQS nQ) (has_seed cQS S)
        (has_num cKS nK) (has_seed cKS S)

        ; current hand ;
        (free c7D)
        (free c8D)
        (free c9D)
        (free c10H)
        (free cJH)
        (free cQC)
        (free c5C)
        (free c7C)

        ; current board ;
        (in_numpile_of c4C c4C)
        (in_numpile_of c4H c4C)
        (in_numpile_of c4S c4C)
        
        (in_numpile_of c6S c6S)
        (in_numpile_of c6H c6S)
        (in_numpile_of c6C c6S)
        (in_numpile_of c6D c6S)
        
        (in_numpile_of cQS cQS)
        (in_numpile_of cQD cQS)
        (in_numpile_of cQH cQS)
        
        (in_seedpile_of cAC cAC)
        (in_seedpile_of c2C cAC)
        (in_seedpile_of c3C cAC)
        (in_seedpile_of c4C cAC)

        ; initiale costs ;
        (= (build-cost) 3)
        (= (add-cost) 1)
        (= (dismantle-cost) 3)
        (= (total-cost) 0)

    )


    (:goal (and
        (not (exists (?c - card)
            (free ?c)
        ))
    ))

    (:metric minimize (total-cost))

)
