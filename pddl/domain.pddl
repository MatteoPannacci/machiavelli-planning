(define (domain machiavelli)

    (:requirements :typing :adl :action-costs)


    (:types
        card - object
        num - object
        seed - object
    )


    (:predicates
        (has_num ?c - card ?n - num)
        (has_seed ?c - card ?s - seed)
        (in_numpile_of ?c - card ?ref - card)
        (in_seedpile_of ?c - card ?ref - card)
        (free ?c - card)
        (succ ?n1 ?n2 - num)
    )


    (:functions
        (total-cost)
        (build-cost)
        (add-cost)
        (dismantle-cost)
    )


    (:action build_numpile
        :parameters (?c1 - card ?c2 - card ?c3 - card)
        :precondition (and
            (not (= ?c1 ?c2))
            (not (= ?c1 ?c3))
            (not (= ?c2 ?c3))
            (free ?c1)
            (free ?c2)
            (free ?c3)
            (exists (?n - num) (and 
                (has_num ?c1 ?n)
                (has_num ?c2 ?n)
                (has_num ?c3 ?n)
            ))
            ; can't use many cards with same seed and number ;
            (not (exists (?s - seed) (or
                (and (has_seed ?c1 ?s) (has_seed ?c2 ?s))
                (and (has_seed ?c1 ?s) (has_seed ?c3 ?s))
                (and (has_seed ?c2 ?s) (has_seed ?c3 ?s))
            )))
        )
        :effect (and
            ; a pile is represented by a 'reference' card ;
            ; which is part of the pile itself and it's the first parameter ?c1;
            (in_numpile_of ?c1 ?c1)
            (in_numpile_of ?c2 ?c1)
            (in_numpile_of ?c3 ?c1)
            (not (free ?c1))
            (not (free ?c2))
            (not (free ?c3))
            (increase (total-cost) (build-cost))
        )
    )


    (:action add_to_numpile
        :parameters (?c - card ?ref - card)
        :precondition (and 
            (free ?c)
            (in_numpile_of ?ref ?ref) ; meaning it's the reference to a numpile;
            (exists (?n - num) (and
                (has_num ?ref ?n)
                (has_num ?c ?n)
            ))
            ; can't have many cards with the same seed in the numpile;
            (not (exists (?s - seed ?c2 - card) (and
                (in_numpile_of ?c2 ?ref)
                (has_seed ?c ?s)
                (has_seed ?c2 ?s)
            )))
        )
        :effect (and 
            (in_numpile_of ?c ?ref)
            (not (free ?c))
            (increase (total-cost) (add-cost))
        )
    )


    (:action dismantle_numpile
        :parameters (?ref - card)
        :precondition (and 
            (in_numpile_of ?ref ?ref)
        )
        :effect (and 
            (forall (?c - card) 
                (when (in_numpile_of ?c ?ref) ; ?c can be ?ref itself ;
                    (and 
                        (free ?c)
                        (not (in_numpile_of ?c ?ref))
                    )
                )
            )
            (increase (total-cost) (dismantle-cost))
        )
    )


    ; assumption: c1 c2 c3 are given in order ;
    ; no loss of generality;
    (:action build_seedpile
        :parameters (?c1 - card ?c2 - card ?c3 - card)
        :precondition (and 
            (free ?c1)
            (free ?c2)
            (free ?c3)
            (exists (?s - seed) (and
                (has_seed ?c1 ?s)
                (has_seed ?c2 ?s)
                (has_seed ?c3 ?s)
            ))
            (exists (?n1 ?n2 ?n3 - num) (and
                (has_num ?c1 ?n1)
                (has_num ?c2 ?n2)
                (has_num ?c3 ?n3)
                (succ ?n1 ?n2)
                (succ ?n2 ?n3)
            ))
        )
        :effect (and 
            ; the reference of the pile is the first parameter;
            (in_seedpile_of ?c1 ?c1)
            (in_seedpile_of ?c2 ?c1)
            (in_seedpile_of ?c3 ?c1)
            (not (free ?c1))
            (not (free ?c2))
            (not (free ?c3))
            (increase (total-cost) (build-cost))
        )
    )


    (:action add_to_seedpile
        :parameters (?c - card ?ref - card)
        :precondition (and 
            (in_seedpile_of ?ref ?ref)
            (free ?c)
            (exists (?s - seed) (and
                (has_seed ?c ?s)
                (has_seed ?ref ?s)
            ))
            ; we are asking for a card to be 'adjacent' to one currently in ;
            ; the pile. this will be possible only at the extremes of the pile ;
            ; if we have only one deck. ;
            (exists (?n ?n2 - num ?c2 - card) (and
                (has_num ?c ?n)
                (in_seedpile_of ?c2 ?ref)
                (has_num ?c2 ?n2)
                (or (succ ?n ?n2) (succ ?n2 ?n))
            ))
            ; to generalize to many decks (so with possible duplicates of cards) ; 
            ; we check if there exist a card with the same number (and seed) ;
            (not (exists (?n - num ?c2 - card) (and
                (has_num ?c ?n)
                (in_seedpile_of ?c2 ?ref)
                (has_num ?c2 ?n)
            )))

        )
        :effect (and 
            (in_seedpile_of ?c ?ref)
            (not (free ?c))
            (increase (total-cost) (add-cost))
        )
    )


    (:action dismantle_seedpile
        :parameters (?ref - card)
        :precondition (and 
            (in_seedpile_of ?ref ?ref)
        )
        :effect (and 
            (forall (?c - card) 
                (when (in_seedpile_of ?c ?ref) 
                    (and
                        (free ?c)
                        (not (in_seedpile_of ?c ?ref))
                    )
                )
            )
            (increase (total-cost) (dismantle-cost))
        )
    )


)