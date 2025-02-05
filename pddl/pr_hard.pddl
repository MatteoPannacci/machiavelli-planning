(define (problem pr1) (:domain machiavelli)

    (:objects
        nA n2 n3 n4 n5 n6 n7 n8 n9 n10 nJ nQ nK - num
        D C H S - seed

        cADr c2Dr c3Dr c4Dr c5Dr c6Dr c7Dr c8Dr c9Dr c10Dr cJDr cQDr cKDr - card
        cACr c2Cr c3Cr c4Cr c5Cr c6Cr c7Cr c8Cr c9Cr c10Cr cJCr cQCr cKCr - card
        cAHr c2Hr c3Hr c4Hr c5Hr c6Hr c7Hr c8Hr c9Hr c10Hr cJHr cQHr cKHr - card
        cASr c2Sr c3Sr c4Sr c5Sr c6Sr c7Sr c8Sr c9Sr c10Sr cJSr cQSr cKSr - card

        cADb c2Db c3Db c4Db c5Db c6Db c7Db c8Db c9Db c10Db cJDb cQDb cKDb - card
        cACb c2Cb c3Cb c4Cb c5Cb c6Cb c7Cb c8Cb c9Cb c10Cb cJCb cQCb cKCb - card
        cAHb c2Hb c3Hb c4Hb c5Hb c6Hb c7Hb c8Hb c9Hb c10Hb cJHb cQHb cKHb - card
        cASb c2Sb c3Sb c4Sb c5Sb c6Sb c7Sb c8Sb c9Sb c10Sb cJSb cQSb cKSb - card
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

        ; number and seed association for red diamonds cards ;
        (has_num cADr nA) (has_seed cADr D)
        (has_num c2Dr n2) (has_seed c2Dr D)
        (has_num c3Dr n3) (has_seed c3Dr D)
        (has_num c4Dr n4) (has_seed c4Dr D)
        (has_num c5Dr n5) (has_seed c5Dr D)
        (has_num c6Dr n6) (has_seed c6Dr D)
        (has_num c7Dr n7) (has_seed c7Dr D)
        (has_num c8Dr n8) (has_seed c8Dr D)
        (has_num c9Dr n9) (has_seed c9Dr D)
        (has_num c10Dr n10) (has_seed c10Dr D)
        (has_num cJDr nJ) (has_seed cJDr D)
        (has_num cQDr nQ) (has_seed cQDr D)
        (has_num cKDr nK) (has_seed cKDr D)

        ; number and seed association for red clubs cards ;
        (has_num cACr nA) (has_seed cACr C)
        (has_num c2Cr n2) (has_seed c2Cr C)
        (has_num c3Cr n3) (has_seed c3Cr C)
        (has_num c4Cr n4) (has_seed c4Cr C)
        (has_num c5Cr n5) (has_seed c5Cr C)
        (has_num c6Cr n6) (has_seed c6Cr C)
        (has_num c7Cr n7) (has_seed c7Cr C)
        (has_num c8Cr n8) (has_seed c8Cr C)
        (has_num c9Cr n9) (has_seed c9Cr C)
        (has_num c10Cr n10) (has_seed c10Cr C)
        (has_num cJCr nJ) (has_seed cJCr C)
        (has_num cQCr nQ) (has_seed cQCr C)
        (has_num cKCr nK) (has_seed cKCr C)

        ; number and seed association for red hearts cards ;
        (has_num cAHr nA) (has_seed cAHr H)
        (has_num c2Hr n2) (has_seed c2Hr H)
        (has_num c3Hr n3) (has_seed c3Hr H)
        (has_num c4Hr n4) (has_seed c4Hr H)
        (has_num c5Hr n5) (has_seed c5Hr H)
        (has_num c6Hr n6) (has_seed c6Hr H)
        (has_num c7Hr n7) (has_seed c7Hr H)
        (has_num c8Hr n8) (has_seed c8Hr H)
        (has_num c9Hr n9) (has_seed c9Hr H)
        (has_num c10Hr n10) (has_seed c10Hr H)
        (has_num cJHr nJ) (has_seed cJHr H)
        (has_num cQHr nQ) (has_seed cQHr H)
        (has_num cKHr nK) (has_seed cKHr H)

        ; number and seed association for red spades cards ;
        (has_num cASr nA) (has_seed cASr S)
        (has_num c2Sr n2) (has_seed c2Sr S)
        (has_num c3Sr n3) (has_seed c3Sr S)
        (has_num c4Sr n4) (has_seed c4Sr S)
        (has_num c5Sr n5) (has_seed c5Sr S)
        (has_num c6Sr n6) (has_seed c6Sr S)
        (has_num c7Sr n7) (has_seed c7Sr S)
        (has_num c8Sr n8) (has_seed c8Sr S)
        (has_num c9Sr n9) (has_seed c9Sr S)
        (has_num c10Sr n10) (has_seed c10Sr S)
        (has_num cJSr nJ) (has_seed cJSr S)
        (has_num cQSr nQ) (has_seed cQSr S)
        (has_num cKSr nK) (has_seed cKSr S)

        ; number and seed association for blue diamonds cards ;
        (has_num cADb nA) (has_seed cADb D)
        (has_num c2Db n2) (has_seed c2Db D)
        (has_num c3Db n3) (has_seed c3Db D)
        (has_num c4Db n4) (has_seed c4Db D)
        (has_num c5Db n5) (has_seed c5Db D)
        (has_num c6Db n6) (has_seed c6Db D)
        (has_num c7Db n7) (has_seed c7Db D)
        (has_num c8Db n8) (has_seed c8Db D)
        (has_num c9Db n9) (has_seed c9Db D)
        (has_num c10Db n10) (has_seed c10Db D)
        (has_num cJDb nJ) (has_seed cJDb D)
        (has_num cQDb nQ) (has_seed cQDb D)
        (has_num cKDb nK) (has_seed cKDb D)

        ; number and seed association for blue clubs cards ;
        (has_num cACb nA) (has_seed cACb C)
        (has_num c2Cb n2) (has_seed c2Cb C)
        (has_num c3Cb n3) (has_seed c3Cb C)
        (has_num c4Cb n4) (has_seed c4Cb C)
        (has_num c5Cb n5) (has_seed c5Cb C)
        (has_num c6Cb n6) (has_seed c6Cb C)
        (has_num c7Cb n7) (has_seed c7Cb C)
        (has_num c8Cb n8) (has_seed c8Cb C)
        (has_num c9Cb n9) (has_seed c9Cb C)
        (has_num c10Cb n10) (has_seed c10Cb C)
        (has_num cJCb nJ) (has_seed cJCb C)
        (has_num cQCb nQ) (has_seed cQCb C)
        (has_num cKCb nK) (has_seed cKCb C)

        ; number and seed association for blue hearts cards ;
        (has_num cAHb nA) (has_seed cAHb H)
        (has_num c2Hb n2) (has_seed c2Hb H)
        (has_num c3Hb n3) (has_seed c3Hb H)
        (has_num c4Hb n4) (has_seed c4Hb H)
        (has_num c5Hb n5) (has_seed c5Hb H)
        (has_num c6Hb n6) (has_seed c6Hb H)
        (has_num c7Hb n7) (has_seed c7Hb H)
        (has_num c8Hb n8) (has_seed c8Hb H)
        (has_num c9Hb n9) (has_seed c9Hb H)
        (has_num c10Hb n10) (has_seed c10Hb H)
        (has_num cJHb nJ) (has_seed cJHb H)
        (has_num cQHb nQ) (has_seed cQHb H)
        (has_num cKHb nK) (has_seed cKHb H)

        ; number and seed association for blue spades cards ;
        (has_num cASb nA) (has_seed cASb S)
        (has_num c2Sb n2) (has_seed c2Sb S)
        (has_num c3Sb n3) (has_seed c3Sb S)
        (has_num c4Sb n4) (has_seed c4Sb S)
        (has_num c5Sb n5) (has_seed c5Sb S)
        (has_num c6Sb n6) (has_seed c6Sb S)
        (has_num c7Sb n7) (has_seed c7Sb S)
        (has_num c8Sb n8) (has_seed c8Sb S)
        (has_num c9Sb n9) (has_seed c9Sb S)
        (has_num c10Sb n10) (has_seed c10Sb S)
        (has_num cJSb nJ) (has_seed cJSb S)
        (has_num cQSb nQ) (has_seed cQSb S)
        (has_num cKSb nK) (has_seed cKSb S)


        ; current hand ;
        (free c3Hb)
        (free c5Cb)
        (free c7Cr)
        (free c7Dr)
        (free c8Db)
        (free c9Dr)
        (free c10Hr)
        (free cJHr)
        

        ; current board ;

        (in_numpile_of cQSr, CQSr)
        (in_numpile_of cQHr, CQSr)
        (in_numpile_of cQDb, CQSr)
        (in_numpile_of cQCr, CQSr)

        (in_seedpile_of cACr, CACr)
        (in_seedpile_of c2Cb, CACr)
        (in_seedpile_of c3Cr, CACr)
        (in_seedpile_of c4Cb, CACr)

        (in_seedpile_of cAHb, CAHb)
        (in_seedpile_of c2Hb, CAHb)
        (in_seedpile_of c3Hr, CAHb)
        (in_seedpile_of c4Hb, CAHb)
        (in_seedpile_of c5Hr, CAHb)
        (in_seedpile_of c6Hb, CAHb)

        (in_numpile_of c6Sb, C6Sb)
        (in_numpile_of c6Db, C6Sb)
        (in_numpile_of c6Cr, C6Sb)

        (in_numpile_of c6Dr, C6Dr)
        (in_numpile_of c6Cb, C6Dr)
        (in_numpile_of c6Hr, C6Dr)


        ; initiale costs ;
        (= (build-cost) 5)
        (= (add-cost) 1)
        (= (dismantle-cost) 7)
        (= (total-cost) 0)

    )


    (:goal (and
        (not (exists (?c - card)
            (free ?c)
        ))
    ))

    (:metric minimize (total-cost))

)
