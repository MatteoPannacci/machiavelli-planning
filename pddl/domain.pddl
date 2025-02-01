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
            ; we avoid to use the same card many times ;
            ; only case where this can happen ;
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
        )
        :effect (and
            ; should c1 be in pile with itself? ;
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
            ; meaning it is in a numpile and it's its reference ;
            (in_numpile_of ?ref ?ref)
            (exists (?n - num) (and
                (has_num ?ref ?n)
                (has_num ?c ?n)
            ))
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
                (when (in_numpile_of ?c ?ref) ; this include ref itself ;
                    (and 
                        (free ?c)
                        (not (in_numpile_of ?c ?ref))
                    )
                )
            )
            (increase (total-cost) (dismantle-cost))
        )
    )


    ; requires c1 c2 c3 to be ordered ;
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
            ; we are assuming only 1 deck ;
            ; meaning only 1 card with that seed-num combination ;
            (exists (?n ?n2 - num ?c2 - card) (and
                (has_num ?c ?n)
                (in_seedpile_of ?c2 ?ref)
                (has_num ?c2 ?n2)
                (or (succ ?n ?n2) (succ ?n2 ?n))
            ))

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