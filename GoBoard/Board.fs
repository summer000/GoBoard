namespace GoBoard

module Board =
    
    type StoneColor = Black | White

    type StonePosition = {X: int; Y: int}

    type Stone = {
        Position: StonePosition
        Color: StoneColor
    }

    type Position = list<Stone>


