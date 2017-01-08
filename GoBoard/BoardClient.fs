namespace GoBoard

open WebSharper
open WebSharper.JavaScript
open WebSharper.UI.Next
open WebSharper.UI.Next.Client
open WebSharper.UI.Next.Html

module SVGE = WebSharper.UI.Next.Html.SvgElements
module SVGA = WebSharper.UI.Next.Html.SvgAttributes

[<JavaScript>]
module BoardClient =

    type Grid(size: int, width: int, margin: int) = 
        member this.Size = size
        member this.Width = width
        member this.Step = (width - 2 * margin) / size
        member this.Margin = (width - this.Step * size) / 2
        member this.Coords = 
            seq {for i in 0 .. size-1 -> this.Margin + this.Step * i + this.Step / 2} 
            |> Seq.map string

    type CursorType = CursorStone | CursorErase

    let colorString = function
        | Board.Black -> "black"
        | Board.White -> "white"

    type Cursor = {
        Position: Option<Board.StonePosition>
        Type: CursorType
        Color: Board.StoneColor
    }

    type BoardPosition = Map<Board.StonePosition, Board.Stone>

    let boardSize = 19
    let boardWidth = 500
    let boardMargin = 30
    let grid = Grid (boardSize, boardWidth, boardMargin)

    let cursor = Var.Create {Cursor.Position = None; Cursor.Type = CursorStone; Cursor.Color = Board.Black}

    let boardPosition = Var.Create (BoardPosition [])

    let onCursorClick (c:Var<Cursor>) = 
        match c.Value.Position with
        | Some(p) -> 
            match c.Value.Type with
            | CursorStone ->
                if not (boardPosition.Value.ContainsKey p) then
                    boardPosition.Value <- boardPosition.Value.Add(p, {Board.Stone.Position = p; Board.Stone.Color = c.Value.Color})
                    let newColor = if c.Value.Color = Board.Black then Board.White else Board.Black
                    c.Value <- {c.Value with Color = newColor}
            | CursorErase ->
                boardPosition.Value <- boardPosition.Value.Remove(p)
        | None -> ()

    let svgGrid (grid: Grid) =  
        let line x1 x2 y1 y2 = 
            SVGE.line 
                [SVGA.x1 x1; SVGA.x2 x2; SVGA.y1 y1; SVGA.y2 y2; SVGA.strokeWidth "1"; SVGA.stroke "black"]
                [] :> Doc
            
        let c0 = grid.Coords |> Seq.nth 0
        let c1 = grid.Coords |> Seq.nth (grid.Size-1)
        
        let lines = Seq.map (fun a -> [(line c0 c1 a a); (line a a c0 c1)]) grid.Coords |> Seq.concat
     
        let letters =
            let text i =
                let letter = "ABCDEFGHJKLOMNPQRST".Substring(i, 1)
                let x = Seq.nth i grid.Coords
                let y1 = string grid.Margin
                let y2 = string (grid.Width - grid.Margin)
                [
                    SVGE.text [
                        SVGA.x x; SVGA.y y1
                        SVGA.fill "black"
                        SVGA.textAnchor "middle"
                        SVGA.dominantBaseline "text-after-edge"
                    ] [Doc.TextNode letter] :> Doc
                    SVGE.text [
                        SVGA.x x; SVGA.y y2
                        SVGA.fill "black"
                        SVGA.textAnchor "middle"
                        SVGA.dominantBaseline "text-before-edge"
                    ] [Doc.TextNode letter] :> Doc
                ]
            seq {0 .. grid.Size - 1} |> Seq.map text |> Seq.concat

        let numbers =
            let text i =
                let number = string (grid.Size - i)
                let y = Seq.nth i grid.Coords
                let x1 = string grid.Margin
                let x2 = string (grid.Width - grid.Margin)
                [
                    SVGE.text [
                        SVGA.x x1
                        SVGA.y y
                        SVGA.fill "black"
                        SVGA.textAnchor "end"
                        SVGA.dominantBaseline "central"
                    ] [Doc.TextNode number] :> Doc
                    SVGE.text [
                        SVGA.x x2
                        SVGA.y y
                        SVGA.fill "black"
                        SVGA.textAnchor "start"
                        SVGA.dominantBaseline "central"
                    ] [Doc.TextNode number] :> Doc
                ]
            seq {0 .. grid.Size - 1} |> Seq.map text |> Seq.concat

        let shapes =
            seq {for x in {0 .. grid.Size - 1} do for y in {0 .. grid.Size - 1} -> (x, y)}
            |> Seq.map (fun i ->
                SVGE.circle [
                    SVGA.cx (Seq.nth (fst i) grid.Coords)
                    SVGA.cy (Seq.nth (snd i) grid.Coords)
                    SVGA.r (grid.Step / 2 |> string)
                    SVGA.fillOpacity "0.0"
                    on.mouseEnter (fun _ _ -> cursor.Value <- {cursor.Value with Position = Some {X = fst i; Y = snd i}})
                    on.mouseLeave (fun _ _ -> cursor.Value <- {cursor.Value with Position = None})
                    on.click (fun _ _ -> onCursorClick cursor)
                ] [] :> Doc
            )

        SVGE.g [] (Seq.concat [lines; letters; numbers; shapes])


    let renderCursor (c:Cursor) =
        match c.Position with
        | Some stonePosition ->
            match c.Type with
            | CursorStone ->
                if boardPosition.Value.ContainsKey stonePosition then Doc.Empty else
                    SVGE.circle [
                        SVGA.cx (Seq.nth stonePosition.X grid.Coords)
                        SVGA.cy (Seq.nth stonePosition.Y grid.Coords)
                        SVGA.r (grid.Step / 2 |> string)
                        SVGA.fill (colorString c.Color) 
                        SVGA.stroke "black"
                        SVGA.fillOpacity "0.5"
                        SVGA.pointerEvents "none"
                    ] [] :> Doc
            | CursorErase -> Doc.Empty
        | None -> Doc.Empty

    let renderBoardPosition (b:BoardPosition) =
        let stones = 
            Map.toSeq b |> Seq.map snd |> Seq.map (fun stone -> 
                SVGE.circle [
                    SVGA.cx (Seq.nth stone.Position.X grid.Coords)
                    SVGA.cy (Seq.nth stone.Position.Y grid.Coords)
                    SVGA.r (grid.Step / 2 |> string)
                    SVGA.fill (colorString stone.Color)
                    SVGA.stroke "black"
                    SVGA.fillOpacity "1"
                    SVGA.pointerEvents "none"
                ] [] :> Doc
            )
        SVGE.g [] stones


    let svgBoard = 
        let sBoardWidth = (string boardWidth)
        SVGE.svg [
            SVGA.width (sBoardWidth)
            SVGA.height (sBoardWidth)
            SVGA.style "fill:#d8ba61"
        ] [
            SVGE.g [] [
                SVGE.rect [SVGA.width (sBoardWidth); SVGA.height (sBoardWidth)] []
            ]
            svgGrid grid
            Doc.BindView renderBoardPosition boardPosition.View
            Doc.BindView renderCursor cursor.View
        ]

    let abc = Var.Create "xxx"
    let abcd txt = span [Doc.TextNode txt]

    let Main () =
        div [
            svgBoard
            Doc.Button "Black" [] (fun () -> cursor.Value <- {cursor.Value with Type = CursorStone; Color = Board.Black})
            Doc.Button "White" [] (fun () -> cursor.Value <- {cursor.Value with Type = CursorStone; Color = Board.White})
            Doc.Button "Erase" [] (fun () -> cursor.Value <- {cursor.Value with Type = CursorErase})
            Doc.Button "Search" [] (fun () -> 
                async {
                    let! x = Server.Search <| (Map.toList boardPosition.Value |> List.map snd)
                    abc.Value <- x
                }
                |> Async.Start
            )
            Doc.BindView abcd abc.View
        ]


